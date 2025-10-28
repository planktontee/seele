const std = @import("std");

const State = enum {
    none,
    digit0,
    number,
    range1,
    range2,
    numberAfterRange,
};

pub fn Token(T: type) type {
    return union(enum) {
        number: T,
        range: struct {
            start: T,
            end: T,
        },
        done,
    };
}

pub fn DecimalRange(T: type) type {
    return struct {
        arr: []const []const u8,
        i: usize = 0,

        pub const Error = error{
            InvalidRangeToken,
            UnexpectedRangeTokenEnd,
            UnexpectedEndOfInput,
        } || std.fmt.ParseIntError;

        pub fn next(self: *@This()) Error!Token(T) {
            if (self.i >= self.arr.len) return .done;

            var state: State = .none;
            const item = self.arr[self.i];
            self.i += 1;
            var start: T = 0;
            var n2: usize = 0;
            var i: usize = 0;
            stateLoop: while (true) {
                errdefer self.i -= 1;
                switch (state) {
                    .none => {
                        if (i >= item.len) return Error.UnexpectedEndOfInput;
                        switch (item[i]) {
                            '0' => {
                                state = .digit0;
                                i += 1;
                                continue :stateLoop;
                            },
                            '1'...'9' => {
                                state = .number;
                                i += 1;
                                continue :stateLoop;
                            },
                            else => return Error.InvalidCharacter,
                        }
                    },
                    .digit0 => {
                        if (i >= item.len) return .{ .number = 0 };
                        switch (item[i]) {
                            '.' => {
                                start = 0;
                                i += 1;
                                state = .range1;
                                continue :stateLoop;
                            },
                            else => return Error.InvalidRangeToken,
                        }
                    },
                    .number => {
                        while (i < item.len) : (i += 1) {
                            switch (item[i]) {
                                '0'...'9' => continue,
                                '.' => {
                                    start = try std.fmt.parseInt(T, item[0..i], 10);
                                    i += 1;
                                    state = .range1;
                                    continue :stateLoop;
                                },
                                else => return Error.InvalidRangeToken,
                            }
                        }
                        return .{ .number = try std.fmt.parseInt(T, item[0..i], 10) };
                    },
                    .range1 => {
                        if (i >= item.len) return Error.UnexpectedEndOfInput;
                        switch (item[i]) {
                            '.' => {
                                i += 1;
                                state = .range2;
                                continue :stateLoop;
                            },
                            else => return Error.InvalidRangeToken,
                        }
                    },
                    .range2 => {
                        if (i >= item.len) return Error.UnexpectedEndOfInput;
                        switch (item[i]) {
                            '1'...'9' => {
                                n2 = i;
                                i += 1;
                                state = .numberAfterRange;
                                continue :stateLoop;
                            },
                            else => return Error.InvalidCharacter,
                        }
                    },
                    .numberAfterRange => {
                        while (i < item.len) : (i += 1) {
                            switch (item[i]) {
                                '0'...'9' => {
                                    continue;
                                },
                                else => return Error.InvalidCharacter,
                            }
                        }
                        return .{
                            .range = .{
                                .start = start,
                                .end = try std.fmt.parseInt(T, item[n2..i], 10),
                            },
                        };
                    },
                }
            }

            unreachable;
        }
    };
}

test "Parse range" {
    const t = std.testing;
    const TToken = Token(u8);
    var range: DecimalRange(u8) = .{
        .arr = &.{ "1..15", "10..12", "10..15", "7", "0" },
    };

    try t.expectEqualDeep(@as(TToken, .{
        .range = .{ .start = 1, .end = 15 },
    }), try range.next());
    try t.expectEqualDeep(@as(TToken, .{
        .range = .{ .start = 10, .end = 12 },
    }), try range.next());
    try t.expectEqualDeep(@as(TToken, .{
        .range = .{ .start = 10, .end = 15 },
    }), try range.next());
    try t.expectEqualDeep(@as(TToken, .{
        .number = 7,
    }), try range.next());
    try t.expectEqualDeep(@as(TToken, .{
        .number = 0,
    }), try range.next());
    try t.expectEqualDeep(@as(TToken, .done), try range.next());
}

test "parse errors" {
    const t = std.testing;
    const Range = DecimalRange(u8);
    var range: Range = .{
        .arr = &.{
            "1.15",
            "1,.15",
            "1...15",
            "01..10",
            "1..01",
            "a",
            "1+",
            "1..1,",
            "",
            "0.",
            "0..",
        },
    };

    try t.expectError(Range.Error.InvalidRangeToken, range.next());
    range.i += 1;
    try t.expectError(Range.Error.InvalidRangeToken, range.next());
    range.i += 1;
    try t.expectError(Range.Error.InvalidCharacter, range.next());
    range.i += 1;
    try t.expectError(Range.Error.InvalidRangeToken, range.next());
    range.i += 1;
    try t.expectError(Range.Error.InvalidCharacter, range.next());
    range.i += 1;
    try t.expectError(Range.Error.InvalidCharacter, range.next());
    range.i += 1;
    try t.expectError(Range.Error.InvalidRangeToken, range.next());
    range.i += 1;
    try t.expectError(Range.Error.InvalidCharacter, range.next());
    range.i += 1;
    try t.expectError(Range.Error.UnexpectedEndOfInput, range.next());
    range.i += 1;
    try t.expectError(Range.Error.UnexpectedEndOfInput, range.next());
    range.i += 1;
    try t.expectError(Range.Error.UnexpectedEndOfInput, range.next());
}
