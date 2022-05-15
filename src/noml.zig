const std = @import("std");

const builtin_fns = .{
    "using",
    "let",
    "in",
};

const Expression = struct {
    token: Token = undefined,
    @"error": ParseError = ParseError.None,
    children: std.ArrayList(@This()),
    
    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .children = std.ArrayList(@This()).init(allocator),
        };
    }
    
    pub fn deinit(self: @This()) void {
        for (self.children.items) |child| {
            child.deinit();
        }
        self.children.deinit();
    }
};

const ParseError = error {
    None,
    UnexpectedToken,
    ExpectedToken,
};

const Token = struct {
    position: usize,
    len: usize,
    type: Type,

    const Type = enum {
        symbol,
        string,
        number,
        separator,
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
        .{ ",", .separator },
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
    
    pub fn isTerminal(self: @This()) bool {
        return switch (self.type) {
            .number, .paren_close, .list_close, .tuple_close => true,
            else => false, 
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
    
    pub fn putBack(self: *@This(), token: Token) !void {
        if (self.cursor - token.len != token.position) return error.TokenPutBackOutOfSequence;
        self.cursor -= token.len;
    }
};

fn tokenise(bytes: []const u8) Tokeniser {
    return .{
        .bytes = bytes,
        .cursor = 0,
    };
}

/// Caller should call `deinit` on the returned Expression to free allocated memory
pub fn parse(allocator: std.mem.Allocator, tokeniser: *Tokeniser) anyerror!Expression {
    var res = Expression.init(allocator);
    errdefer res.deinit();
    
    if (tokeniser.next()) |token| {
        switch (token.type) {
            .string => {
                res.token = token;
            },
            .number => {
                if (tokeniser.next()) |next_token| {
                    res.token = next_token;
                    
                    var lhs = Expression.init(allocator);
                    errdefer lhs.deinit();
                    
                    lhs.token = token;
                    
                    try res.children.append(lhs);
                    
                    switch (next_token.type) {
                        .plus, .minus, .times, .divide => {
                        },
                        else => {
                            res.@"error" = ParseError.UnexpectedToken;
                        }
                    }
                    
                    if (tokeniser.next()) |next_next_token| {
                        var rhs = Expression.init(allocator);
                        errdefer rhs.deinit();
                        
                        rhs.token = next_next_token;
                        
                        try res.children.append(rhs);
                    
                        switch (next_next_token.type) {
                            .number => {
                            },
                            else => {
                                res.@"error" = ParseError.UnexpectedToken;
                            }
                        }
                    }
                }
                else {
                    res.token = token;
                }
            },
            .list_open => {
                res.token = token;
                while (tokeniser.next()) |next_token| {
                    switch (next_token.type) {
                        .separator => {},
                        .list_close => break,
                        .paren_close, .tuple_close => {
                            res.@"error" = ParseError.UnexpectedToken;
                        },
                        else => {
                            try tokeniser.putBack(next_token);
                        }
                    }
                    
                    const elem = try parse(allocator, tokeniser);
                    errdefer elem.deinit();       
                    
                    try res.children.append(elem);
                }
            },
            else => {
                res.@"error" = ParseError.UnexpectedToken;
            }
        }
    }
    
    return res;
}

/// Parses source bytes
/// Caller should call `deinit` on the returned Expression to free allocated memory
pub fn parseBytes(allocator: std.mem.Allocator, bytes: []const u8) !Expression {
    var tokeniser = tokenise(bytes);
    return parse(allocator, &tokeniser);
}

pub fn evaluate(_: Expression, _: anytype) Expression {}

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
    const test_input: []const u8 = "2 + 15";

    var expected_result = Expression{
         .token = .{ .position = 2, .len = 1, .type = .plus },
         .@"error" = ParseError.None,
         .children = std.ArrayList(Expression).init(std.testing.allocator),
    };
    try expected_result.children.append(.{
        .token = .{ .position = 0, .len = 1, .type = .number },
        .@"error" = ParseError.None,
        .children = std.ArrayList(Expression).init(std.testing.allocator),
    });
    try expected_result.children.append(.{
        .token = .{ .position = 4, .len = 2, .type = .number },
        .@"error" = ParseError.None,
        .children = std.ArrayList(Expression).init(std.testing.allocator),
    });
    defer expected_result.deinit();
    
    const result = try parseBytes(std.testing.allocator, test_input);
    defer result.deinit();
    
    try testing.expectEqual(expected_result.token, result.token);
    try testing.expectEqualSlices(Expression, expected_result.children.items, result.children.items);
}

