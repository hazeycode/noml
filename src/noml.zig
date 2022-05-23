const std = @import("std");

const Expression = struct {
    data: union(enum) {
        identifier: []const u8,
        string: []const u8,
        number: Rational,
        boolean: bool,        
        list: void,
        unary_op_neg: void,
        unary_op_not: void,
        binary_op_add: void,
        binary_op_sub: void,
        binary_op_mul: void,
        binary_op_div: void,
        binary_op_eql: void,
        binary_op_and: void,
        binary_op_or: void,
        binary_op_not: void,
        @"error": ParseError,
    },
    token: ?Token = null,
    next: ?*Expression = null,
};

const Rational = struct {
    numerator: u64,
    denominator: u64,
};

const ParseError = error{
    UnexpectedToken,
    MalformedNumberLiteral,
    MalformedBooleanLiteral,
};

const Token = struct {
    position: usize,
    len: usize,
    tag: Tag,

    const Tag = enum {
        invalid_bytes,
        identifier,
        string,
        number,
        boolean,
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
        keyword_true,
        keyword_false,
        keyword_and,
        keyword_or,
        keyword_not,
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
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
        .{ "and", .keyword_and },
        .{ "or", .keyword_or },
        .{ "not", .keyword_not },
    });

    pub fn precedence(self: @This()) u32 {
        return switch (self.tag) {
            .plus, .minus => 10,
            .times, .divide => 11,
            .keyword_or => 20,
            .keyword_and => 21,
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
              
        if (std.ascii.isAlpha(self.bytes[self.cursor])) return self.keywordOrIdentifier()
        else if (self.bytes[self.cursor] == '\"') return self.string()
        else if (std.ascii.isDigit(self.bytes[self.cursor])) return self.number()
        else {
            defer self.cursor += 1;
            return Token{
                .position = self.cursor,
                .len = 1,
                .tag = .invalid_bytes,    
            };
        }
    }
    
    fn keywordOrIdentifier(self: *@This()) Token {
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
        
        const len = forward_cursor - self.cursor;
        const tag = Token.map.get(self.bytes[self.cursor..(self.cursor + len)]) orelse .identifier;
        return Token {
            .position = self.cursor,
            .len = len,
            .tag = tag,
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

fn gcd(a: anytype, b: anytype) @TypeOf(a) {
    var n = a;
    var m = b;
    while (n > 0) {
        const r = m % n;
        m = n;
        n = r;
    }
    return m;
}

fn number(source_bytes: []const u8) !Rational {
    if (source_bytes.len == 0) return ParseError.MalformedNumberLiteral;
    if (std.ascii.isDigit(source_bytes[0]) == false) {
        return ParseError.MalformedNumberLiteral;      
    }
    
    if (source_bytes.len > 2 and source_bytes[0] == '0') {
        if (source_bytes[1] == 'x') {
            // TODO(hazeycode): hex number parsing
            unreachable;
        }
    }
    
    { // parse base 10
        var maybe_decimal_point_pos: ?usize = null;
        for (source_bytes) |char, i| {
            if (char == '.') {
                if (i == source_bytes.len) {
                    return ParseError.MalformedNumberLiteral;
                }
                maybe_decimal_point_pos = i;
            }
            else if (std.ascii.isDigit(char) == false) {
                return ParseError.MalformedNumberLiteral;
            }
        }
        
        if (maybe_decimal_point_pos) |decimal_point_pos| {
            const s = try std.math.powi(u64, 10, source_bytes.len - (decimal_point_pos + 1));
            var n: u64 = 0;
            for (source_bytes) |char| {
                if (char == '.') continue;
                n = 10 * n + (char - '0');
            }
            const gcd_ = gcd(n, s);
            return Rational{
                .numerator = n / gcd_,
                .denominator = s / gcd_, 
            };
        }
        else {
            var n: u64 = 0;
            for (source_bytes) |char| {
                n = 10 * n + (char - '0');
            }
            return Rational{
                .numerator = n,
                .denominator = 1,
            };
        }
    }
}

fn boolean(source_bytes: []const u8) ParseError!bool {
    return if (std.mem.eql(u8, source_bytes, "true")) true
    else if (std.mem.eql(u8, source_bytes, "false")) false
    else ParseError.MalformedBooleanLiteral;
}

fn booleanExpression(allocator: std.mem.Allocator, tokeniser: *Tokeniser, expr: Expression) anyerror!?Expression {
    var expr0 = expr;
    
    if (tokeniser.next()) |next_token| {
        var expr1 = Expression{
            .token = next_token,
            .data = undefined,
        };
        
        switch (expr1.token.?.tag) {
            .keyword_and, .keyword_or => {
                expr1.data = switch (expr1.token.?.tag) {
                    .keyword_and => .binary_op_and,
                    .keyword_or => .binary_op_or,
                    else => unreachable,
                };
                
                if (try expression(allocator, tokeniser)) |expr2| {
                    switch (expr2.token.?.tag) {
                        .keyword_and, .keyword_or => {
                            if (expr2.token.?.precedence() < expr1.token.?.precedence()) {
                                const rhs = expr2.next.?.next;
                                const temp_lhs = expr2.next.?.*;
    
                                expr1.next = try allocator.create(Expression);
                                expr0.next = try allocator.create(Expression);
    
                                expr0.next.?.* = temp_lhs;
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
            .keyword_not => {
                expr1.data = .unary_op_not;
                if (try expression(allocator, tokeniser)) |expr2| {
                    expr1.next = try allocator.create(Expression);
                    expr1.next.?.* = expr2;
                }
                return expr1;
            },
            else => {
                expr1.data = .{ .@"error" = ParseError.UnexpectedToken };
    
                expr0.next = try allocator.create(Expression);
                expr0.next.?.* = expr1;
    
                return expr0;
            },
        }
    } else {
        return expr0;
    }
}

fn expression(allocator: std.mem.Allocator, tokeniser: *Tokeniser) anyerror!?Expression {
    if (tokeniser.next()) |token| {
        switch (token.tag) {
            .minus => {
                var res = Expression{
                    .token = token,
                    .data = .unary_op_neg,
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
                    .data = .{
                        .number = try number(tokeniser.bytes[token.position..(token.position + token.len)]),
                    },
                };

                if (tokeniser.next()) |next_token| {
                    var expr1 = Expression{
                        .token = next_token,
                        .data = undefined,
                    };

                    switch (expr1.token.?.tag) {
                        .equal, .plus, .minus, .times, .divide => {
                            expr1.data = switch (expr1.token.?.tag) {
                                .equal => .binary_op_eql,
                                .plus => .binary_op_add,
                                .minus => .binary_op_sub,
                                .times => .binary_op_mul,
                                .divide => .binary_op_div,
                                else => unreachable,
                            };
                        
                            if (try expression(allocator, tokeniser)) |expr2| {
                                switch (expr2.token.?.tag) {
                                    .equal, .plus, .minus, .times, .divide => {
                                        if (expr2.token.?.precedence() < expr1.token.?.precedence()) {
                                            const rhs = expr2.next.?.next;
                                            const temp_lhs = expr2.next.?.*;

                                            expr1.next = try allocator.create(Expression);
                                            expr0.next = try allocator.create(Expression);

                                            expr0.next.?.* = temp_lhs;
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
                            expr1.data = .{ .@"error" = ParseError.UnexpectedToken };

                            expr0.next = try allocator.create(Expression);
                            expr0.next.?.* = expr1;

                            return expr0;
                        },
                    }
                } else {
                    return expr0;
                }
            },
            .keyword_true => {
                return try booleanExpression(allocator, tokeniser, .{
                    .token = token,
                    .data = .{ .boolean = true },
                });
            },
            .keyword_false => {
                return try booleanExpression(allocator, tokeniser, .{
                    .token = token,
                    .data = .{ .boolean = false },
                });
            },
            .keyword_not => {
                return Expression{
                    .token = token,
                    .data = .unary_op_not,  
                };
            },
            else => {
                return Expression{
                    .token = token,
                    .data = .{ .@"error" = ParseError.UnexpectedToken },
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


// TESTS ----------------------------------------------------------------------------------------------------

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

fn testExpectExpression(expr: *Expression, expected: *Expression) !void {
    var maybe_expected_cur: ?*Expression = expected;
    var maybe_cur: ?*Expression = expr;
    while (maybe_expected_cur) |expected_cur| {
        { // test data before token for debugging purposes
            errdefer {
                // if data does not match expected then log the expected and actual token positions
                // for debugging purposes
                if (expected_cur.token) |expected_token| {
                    std.log.err(
                        "expected pos {}, got {}",
                        .{expected_token.position, maybe_cur.?.token.?.position},
                    );
                }
            }
            
            try testing.expectEqual(expected_cur.data, maybe_cur.?.data);
        }

        if (expected_cur.token) |expected_token| {
            // test token tag first for debugging purposes
            try testing.expectEqual(expected_token.tag, maybe_cur.?.token.?.tag);
            try testing.expectEqual(expected_token.position, maybe_cur.?.token.?.position);
            try testing.expectEqual(expected_token.len, maybe_cur.?.token.?.len);
        }
        else {
            try testing.expectEqual(@as(?Token, null), maybe_cur.?.token);
        }
        
        maybe_expected_cur = expected_cur.next;
        maybe_cur = maybe_cur.?.next;
    }
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

test "tokenise boolean logic keywords" {
    const test_input: []const u8 = "true or false and not true";
    
    const expected_result = .{
        Token{ .position = 0, .len = 4, .tag = .keyword_true },
        Token{ .position = 5, .len = 2, .tag = .keyword_or },
        Token{ .position = 8, .len = 5, .tag = .keyword_false },
        Token{ .position = 14, .len = 3, .tag = .keyword_and },
        Token{ .position = 18, .len = 3, .tag = .keyword_not },
        Token{ .position = 22, .len = 4, .tag = .keyword_true },
    };
    
    try testTokenise(test_input, &expected_result);
}

test "tokenise invalid bytes" {
    const test_input: []const u8 = "$";

    const expected_result = .{
        Token{ .position = 0, .len = 1, .tag = .invalid_bytes },
    };

    try testTokenise(test_input, &expected_result);
}

test "parse numbers" {
    { // integer literal
        const test_input: []const u8 = "3";
        try testing.expectEqual(Rational{ .numerator = 3, .denominator = 1 }, try number(test_input));
    }
    { // decimal literal
        const test_input: []const u8 = "3.14";
        try testing.expectEqual(Rational{ .numerator = 157, .denominator = 50 }, try number(test_input));
    }
}

test "parse basic arthimetic expressions" {
    const test_input: []const u8 = "2 + 15 * 100 - 17";

    var expected_17 = Expression{ 
        .data = .{ .number = Rational{ .numerator = 17, .denominator = 1 }},
        .token = .{ .position = 15, .len = 2, .tag = .number },
    };

    var expected_100 = Expression{
        .data = .{ .number = Rational{ .numerator = 100, .denominator = 1 }},
        .token = .{ .position = 9, .len = 3, .tag = .number },
        .next = &expected_17,
    };

    var expected_15 = Expression{
        .data = .{ .number = Rational{ .numerator = 15, .denominator = 1 }},
        .token = .{ .position = 4, .len = 2, .tag = .number },
        .next = &expected_100,
    };

    var expected_times = Expression{
        .data = .binary_op_mul,
        .token = .{ .position = 7, .len = 1, .tag = .times },
        .next = &expected_15,
    };

    var expected_minus = Expression{
        .data = .binary_op_sub,
        .token = .{ .position = 13, .len = 1, .tag = .minus },
        .next = &expected_times,
    };

    var expected_2 = Expression{
        .data = .{ .number = Rational{ .numerator = 2, .denominator = 1 }},
        .token = .{ .position = 0, .len = 1, .tag = .number },
        .next = &expected_minus,
    };

    var expected = Expression{
        .data = .binary_op_add,
        .token = .{ .position = 2, .len = 1, .tag = .plus },
        .next = &expected_2,
    };

    var expr = try parse(testing.allocator, test_input);
    try testing.expect(expr != null);

    defer free(testing.allocator, expr.?);
    
    try testExpectExpression(&expected, &expr.?);
}

test "parse boolean logic expressions" {
    const test_input: []const u8 = "true and false or true or not true"; // lol!
    
    var expected_true_3rd = Expression{
        .data = .{ .boolean = true },
        .token = .{ .position = 30, .len = 4, .tag = .keyword_true },
    };
    
    var expected_not = Expression{
        .data = .unary_op_not,
        .token = .{ .position = 26, .len = 3, .tag = .keyword_not },
        .next = &expected_true_3rd,
    };
    
    var expected_true_2nd = Expression{
        .data = .{ .boolean = true },
        .token = .{ .position = 18, .len = 4, .tag = .keyword_true },
        .next = &expected_not,
    };
    
    var expected_false = Expression{
        .data = .{ .boolean = false },
        .token = .{ .position = 9, .len = 5, .tag = .keyword_false },
        .next = &expected_true_2nd,
    };
    
    var expected_true_1st = Expression{
        .data = .{ .boolean = true },
        .token = .{ .position = 0, .len = 4, .tag = .keyword_true },
        .next = &expected_false,
    };
    
    var expected_and = Expression{
        .data = .binary_op_and,
        .token = .{ .position = 5, .len = 3, .tag = .keyword_and },
        .next = &expected_true_1st,
    };
    
    var expected_or_2nd = Expression{
        .data = .binary_op_or,
        .token = .{ .position = 23, .len = 2, .tag = .keyword_or },
        .next = &expected_and,
    };
    
    var expected = Expression{
        .data = .binary_op_or,
        .token = .{ .position = 15, .len = 2, .tag = .keyword_or },
        .next = &expected_or_2nd,
    };
    
    var expr = try parse(testing.allocator, test_input);
    try testing.expect(expr != null);
    
    defer free(testing.allocator, expr.?);
    
    try testExpectExpression(&expected, &expr.?);
}

