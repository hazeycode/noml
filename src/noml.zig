const std = @import("std");

const builtin_fns = .{
    "using",
    "let",
    "in",
};

const Expression = struct {
    token: Token,
    @"error": ParseError = ParseError.None,
    next: ?*Expression = null,
};

const ParseError = error {
    None,
    UnexpectedToken,
};

const Token = struct {
    position: usize,
    len: usize,
    type: Type,

    const Type = enum {
        symbol,
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
    };

    const type_map = std.ComptimeStringMap(Type, .{
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
    });
    
    pub fn associativity(self: *@This()) enum { left, right, none } {
        return switch(self.type) {
            .plus, .minus .times, .divide => .left,
            else => .none,
        };
    }
    
    pub fn precedence(self: @This()) u32 {
        return switch (self.type) {
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
        std.debug.assert(self.cursor <= self.bytes.len);

        while (self.cursor < self.bytes.len) {
            if (std.ascii.isSpace(self.bytes[self.cursor])) {
                self.cursor += 1;
            } else break;
        }

        if (self.cursor >= self.bytes.len) {
            return null;
        }

        if (Token.type_map.get(&.{self.bytes[self.cursor]})) |token_type| {
            defer self.cursor += 1;
            return Token{
                .position = self.cursor,
                .len = 1,
                .type = token_type,
            };
        }

        const State = enum {
            identity,
            string,
            number,
        };

        var state = State.identity;

        if (std.ascii.isDigit(self.bytes[self.cursor])) {
            state = .number;
        }

        var forward_cursor = self.cursor + 1;

        while (forward_cursor < self.bytes.len) {
            if (std.ascii.isSpace(self.bytes[forward_cursor])) {
                break;
            }
            if (Token.type_map.get(&.{self.bytes[forward_cursor]})) |_| {
                break;
            }
            forward_cursor += 1;
        }

        defer self.cursor = forward_cursor;

        return Token{
            .position = self.cursor,
            .len = forward_cursor - self.cursor,
            .type = switch (state) {
                .number => .number,
                else => unreachable,
            },
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
        switch (token.type) {
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
                if (tokeniser.next()) |operator_token| {
                    var expr0 = Expression{
                        .token = operator_token,
                        .next = try allocator.create(Expression),
                    };
                    expr0.next.?.token = token;
                    
                    switch (operator_token.type) {
                        .equal, .plus, .minus, .times, .divide => {                         
                            if (try expression(allocator, tokeniser)) |expr1| {
                                switch (expr1.token.type) {
                                    .number => {
                                        expr0.next.?.next.? = try allocator.create(Expression);
                                        expr0.next.?.next.?.* = expr1;
                                    },
                                    .equal, .plus, .minus, .times, .divide => {
                                        if (expr0.token.precedence() >= expr1.token.precedence()) {
                                            expr1.next.?.* = expr0;
                                        }
                                        else {
                                            expr0.next.?.next = try allocator.create(Expression);
                                            expr0.next.?.next.?.* = expr1;
                                        }
                                    },
                                    else => {
                                        expr0.next.?.@"error" = ParseError.UnexpectedToken;
                                    }
                                }
                            }
                        },
                        else => {
                            expr0.@"error" = ParseError.UnexpectedToken;
                        }
                    }
                    
                    return expr0;
                }
                else {
                    return Expression{
                        .token = token,
                    };
                }
            },
            else => unreachable,
        }
    }
    else {
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

test "tokenise" {
    const test_input: []const u8 = "2 + 15 * -(100 - 1472) / 2";

    const expected_result = .{
        Token{ .position = 0, .len = 1, .type = .number },
        Token{ .position = 2, .len = 1, .type = .plus },
        Token{ .position = 4, .len = 2, .type = .number },
        Token{ .position = 7, .len = 1, .type = .times },
        Token{ .position = 9, .len = 1, .type = .minus },
        Token{ .position = 10, .len = 1, .type = .paren_open },
        Token{ .position = 11, .len = 3, .type = .number },
        Token{ .position = 15, .len = 1, .type = .minus },
        Token{ .position = 17, .len = 4, .type = .number },
        Token{ .position = 21, .len = 1, .type = .paren_close },
        Token{ .position = 23, .len = 1, .type = .divide },
        Token{ .position = 25, .len = 1, .type = .number },
    };

    var tokeniser = tokenise(test_input);

    var tokens = std.ArrayList(Token).init(testing.allocator);

    defer tokens.deinit();

    while (tokeniser.next()) |token| {
        try tokens.append(token);
    }

    try testing.expectEqualSlices(Token, &expected_result, tokens.items);
}

test "parse" {
    const test_input: []const u8 = "2 + 15 * 100";
    
    var expected_100 = Expression{
        .token = .{ .position = 9, .len = 3, .type = .number },  
    };
    
    var expected_15 = Expression{
        .token = .{ .position = 4, .len = 2, .type = .number },
        .next = &expected_100,
    };
    
    var expected_times = Expression{
        .token = .{ .position = 7, .len = 1, .type = .times },
        .next = &expected_15,
    };
    
    var expected_2 = Expression{
        .token = .{ .position = 0, .len = 1, .type = .number },
        .next = &expected_times,
    };
    
    var expected = Expression{
        .token = .{ .position = 2, .len = 1, .type = .plus },
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

