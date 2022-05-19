const std = @import("std");

const Expression = struct {
    token: Token,
    @"error": ParseError = ParseError.None,
    next: ?*Expression = null,
};

const ParseError = error{
    None,
    UnexpectedToken,
};

const Token = struct {
    position: usize,
    len: usize,
    tag: Tag,

    const Tag = enum {
        identifier,
        string,
        number,
        paren_open,
        paren_close,
        list_open,
        list_close,
        tuple_open,
        tuple_close,
        equal,
        plus,
        minus,
        times,
        divide,
        keyword_using,
        keyword_let,
        keyword_in,
    };

    const map = std.ComptimeStringMap(Tag, .{
        .{ "(", .paren_open },
        .{ ")", .paren_close },
        .{ "[", .list_open },
        .{ "]", .list_close },
        .{ "{", .tuple_open },
        .{ "}", .tuple_close },
        .{ "=", .equal },
        .{ "+", .plus },
        .{ "-", .minus },
        .{ "*", .times },
        .{ "/", .divide },
        .{ "using", .keyword_using },
        .{ "let", .keyword_let },
        .{ "in", .keyword_in },
    });

    pub fn precedence(self: @This()) u32 {
        return switch (self.tag) {
            .plus, .minus => 10,
            .times, .divide => 11,
            else => 0,
        };
    }
};

const Tokeniser = struct {
    bytes: []const u8,
    cursor: usize,

    pub fn next(self: *@This()) ?Token {
        while (self.cursor < self.bytes.len) {
            if (std.ascii.isSpace(self.bytes[self.cursor])) {
                self.cursor += 1;
            } else break;
        }

        if (self.cursor >= self.bytes.len) {
            return null;
        }

        if (Token.map.get(&.{self.bytes[self.cursor]})) |tag| {
            defer self.cursor += 1;
            return Token{
                .position = self.cursor,
                .len = 1,
                .tag = tag,
            };
        }
       
        return if (std.ascii.isAlpha(self.bytes[self.cursor])) self.identifier()
        else if (self.bytes[self.cursor] == '\"') self.string()
        else if (std.ascii.isDigit(self.bytes[self.cursor])) self.number()
        else null; // TODO(hazeycode): return error, invalid character in stream
    }
    
    fn identifier(self: *@This()) Token {
        var forward_cursor = self.cursor + 1;
        
        while (forward_cursor < self.bytes.len) : (forward_cursor += 1) {
            const char = self.bytes[forward_cursor];
            
            if (std.ascii.isSpace(char)) {
                break;
            }
            if (Token.map.get(&.{char})) |_| {
                break;
            }
        }
        
        defer self.cursor = forward_cursor;
        
        return Token {
            .position = self.cursor,
            .len = forward_cursor - self.cursor,
            .tag = .identifier,
        };
    }
    
    fn string(self: *@This()) Token {
        var forward_cursor = self.cursor + 1;
        var escape_next = false;
        
        while (forward_cursor < self.bytes.len) : (forward_cursor += 1) {
            const char = self.bytes[forward_cursor];
            
            if (char == '\\') {
                escape_next = true;
            }
            else if (escape_next == false and self.bytes[forward_cursor] == '"') {
                break;
            }
            else if (escape_next) {
                escape_next = false;
            }
        }
        
        defer self.cursor = forward_cursor + 1;
        
        const start = self.cursor + 1;
        const end = forward_cursor;
        
        return Token {
            .position = start,
            .len = end - start,
            .tag = .string,
        };
    }
    
    fn number(self: *@This()) Token {
        var forward_cursor = self.cursor + 1;
        var decimal_point_encountered = false;
        
        while (forward_cursor < self.bytes.len) : (forward_cursor += 1) {
            const char = self.bytes[forward_cursor];
            
            if (char == '.') {
                if (decimal_point_encountered) {
                    break;
                }
                decimal_point_encountered = true;
            }
            else if (std.ascii.isDigit(char) == false) {
                break;
            }
        }
        
        defer self.cursor = forward_cursor;
        
        return Token {
            .position = self.cursor,
            .len = forward_cursor - self.cursor,
            .tag = .number,
        };
    }
};

fn tokenise(bytes: []const u8) Tokeniser {
    return .{
        .bytes = bytes,
        .cursor = 0,
    };
}

fn expression(allocator: std.mem.Allocator, tokeniser: *Tokeniser) anyerror!?Expression {
    if (tokeniser.next()) |token| {
        switch (token.tag) {
            .minus => {
                var res = Expression{
                    .token = token,
                };
                if (try expression(allocator, tokeniser)) |expr| {
                    res.next = try allocator.create(Expression);
                    res.next.?.* = expr;
                }
                return res;
            },
            .number => {
                var expr0 = Expression{
                    .token = token,
                };

                if (tokeniser.next()) |next_token| {
                    var expr1 = Expression{
                        .token = next_token,
                    };

                    switch (next_token.tag) {
                        .equal, .plus, .minus, .times, .divide => {
                            if (try expression(allocator, tokeniser)) |expr2| {
                                switch (expr2.token.tag) {
                                    .equal, .plus, .minus, .times, .divide => {
                                        if (expr2.token.precedence() < expr1.token.precedence()) {
                                            const rhs = expr2.next.?.next;
                                            const temp_lhs_token = expr2.next.?.token;

                                            expr1.next = try allocator.create(Expression);
                                            expr0.next = try allocator.create(Expression);

                                            expr0.next.?.* = .{
                                                .token = temp_lhs_token,
                                            };
                                            expr0.next.?.next = rhs;

                                            expr1.next.?.* = expr0;
                                            expr2.next.?.* = expr1;

                                            return expr2;
                                        }
                                    },
                                    else => {},
                                }
                                expr0.next = try allocator.create(Expression);
                                expr0.next.?.* = expr2;
                            }

                            expr1.next = try allocator.create(Expression);
                            expr1.next.?.* = expr0;

                            return expr1;
                        },
                        else => {
                            expr1.@"error" = ParseError.UnexpectedToken;

                            expr0.next = try allocator.create(Expression);
                            expr0.next.?.* = expr1;

                            return expr0;
                        },
                    }
                } else {
                    return expr0;
                }
            },
            else => {
                return Expression{
                    .token = token,
                    .@"error" = ParseError.UnexpectedToken,
                };
            },
        }
    } else {
        return null;
    }
}

/// Parses source bytes
/// Caller should call `deinit` on the returned Expression to free allocated memory
pub fn parse(allocator: std.mem.Allocator, bytes: []const u8) !?Expression {
    var tokeniser = tokenise(bytes);
    return try expression(allocator, &tokeniser);
}

pub fn evaluate(_: Expression, _: anytype) Expression {}

pub fn free(allocator: std.mem.Allocator, expr: Expression) void {
    var maybe_cur = expr.next;
    while (maybe_cur) |cur| {
        maybe_cur = cur.next;
        allocator.destroy(cur);
    }
}

const testing = std.testing;

fn testTokenise(input: []const u8, expected: []const Token) !void {
    var tokeniser = tokenise(input);

    var tokens = std.ArrayList(Token).init(testing.allocator);

    defer tokens.deinit();

    while (tokeniser.next()) |token| {
        try tokens.append(token);
    }
    
    try testing.expectEqualSlices(Token, expected, tokens.items);
}

test {
    testing.refAllDecls(@This());
}

test "tokenise arithmetic" {
    const test_input: []const u8 = "2 + 15 * -(100 - 1472) / 2";

    const expected_result = .{
        Token{ .position = 0, .len = 1, .tag = .number },
        Token{ .position = 2, .len = 1, .tag = .plus },
        Token{ .position = 4, .len = 2, .tag = .number },
        Token{ .position = 7, .len = 1, .tag = .times },
        Token{ .position = 9, .len = 1, .tag = .minus },
        Token{ .position = 10, .len = 1, .tag = .paren_open },
        Token{ .position = 11, .len = 3, .tag = .number },
        Token{ .position = 15, .len = 1, .tag = .minus },
        Token{ .position = 17, .len = 4, .tag = .number },
        Token{ .position = 21, .len = 1, .tag = .paren_close },
        Token{ .position = 23, .len = 1, .tag = .divide },
        Token{ .position = 25, .len = 1, .tag = .number },
    };

    try testTokenise(test_input, &expected_result);
}

test "tokenise string literals" {
    const test_input: []const u8 =
        \\"Behold! I am a \"string\"!"
    ;
    
    const expected_result = .{
        Token{ .position = 1, .len = 26, .tag = .string },
    };
    
    try testTokenise(test_input, &expected_result);
}

test "parse" {
    const test_input: []const u8 = "2 + 15 * 100 - 17";

    var expected_17 = Expression{ .token = .{ .position = 15, .len = 2, .tag = .number } };

    var expected_100 = Expression{
        .token = .{ .position = 9, .len = 3, .tag = .number },
        .next = &expected_17,
    };

    var expected_15 = Expression{
        .token = .{ .position = 4, .len = 2, .tag = .number },
        .next = &expected_100,
    };

    var expected_times = Expression{
        .token = .{ .position = 7, .len = 1, .tag = .times },
        .next = &expected_15,
    };

    var expected_minus = Expression{
        .token = .{ .position = 13, .len = 1, .tag = .minus },
        .next = &expected_times,
    };

    var expected_2 = Expression{
        .token = .{ .position = 0, .len = 1, .tag = .number },
        .next = &expected_minus,
    };

    var expected = Expression{
        .token = .{ .position = 2, .len = 1, .tag = .plus },
        .next = &expected_2,
    };

    var expr = try parse(testing.allocator, test_input);
    try testing.expect(expr != null);

    defer free(testing.allocator, expr.?);

    var maybe_expected_cur: ?*Expression = &expected;
    var maybe_cur: ?*Expression = &expr.?;
    while (maybe_expected_cur) |expected_cur| {
        try testing.expectEqual(expected_cur.token, maybe_cur.?.token);
        maybe_expected_cur = expected_cur.next;
        maybe_cur = maybe_cur.?.next;
    }
}
