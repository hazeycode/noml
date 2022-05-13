const std = @import("std");

const builtin_fns = .{
    "using",
    "let",
    "in",
};

const Expression = struct {
    kind: enum { constant, func, list, map },
    next: ?*@This(),
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
};

pub const Tokeniser = struct {
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

pub fn tokenise(bytes: []const u8) Tokeniser {
    return .{
        .bytes = bytes,
        .cursor = 0,
    };
}

pub fn parse(_: anytype) ?Expression {}

pub fn evaluate(_: Expression, _: anytype) Expression {}

test "tokenise" {
    const testing = std.testing;

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
