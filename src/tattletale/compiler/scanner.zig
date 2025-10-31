const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Diagnostics = union(enum) {
    disabled,
    enabled: struct {
        allocator: Allocator,
        idx: *const usize,
        pattern: []const u8,
        message: ?[:0]u8 = null,

        pub fn messageRef(self: *@This(), size: usize) Allocator.Error![:0]u8 {
            if (self.message) |msg| {
                if (msg.len <= size) {
                    return msg[0..size :0];
                } else {
                    self.message = self.allocator.remap(self.message, size) orelse return Allocator.Error.OutOfMemory;
                    return self.message;
                }
            } else {
                self.message = try self.allocator.alloc(u8, size);
                return self.message;
            }
        }

        pub fn deinit(self: *@This()) void {
            self.allocator.free(self.message);
            self.message = undefined;
        }
    },
};

pub const Range = struct {
    min: usize,
    max: usize,
};

pub const Token = union(enum) {
    branchStart,
    branchEnd,

    // 1 to 4 bytes (utf8)
    // escapings
    // single chars
    atom: u8,

    quantifier: union(enum) {
        optional,

        anyGreedy,
        anyLazy,

        moreGreedy,
        moreLazy,

        minGreedy: usize,
        minLazy: usize,

        maxGreedy: usize,
        maxLazy: usize,

        rangeGreedy: Range,
        rangeLazy: Range,
    },

    done,
};

const State = union(enum) {
    init,

    seekPiece,

    initialRange,
    anyGreedy,
    moreGreedy,

    done,
};

state: State = .init,
pattern: []const u8,
i: usize = 0,
diagnostics: Diagnostics = .disabled,

pub const Error = error{
    TBA,
    Bad2ByteUTF8Start,
    InvalidUTF8Byte1,

    RangeEndBeforeStart,
};

pub fn next(self: *@This()) Error!Token {
    stateLoop: while (true) {
        switch (self.state) {
            .init => {
                if (self.i >= self.pattern.len) {
                    self.state = .done;
                    continue :stateLoop;
                }
                self.state = .seekPiece;
                return .branchStart;
            },
            .seekPiece => {
                if (self.i >= self.pattern.len) {
                    self.state = .done;
                    continue :stateLoop;
                }
                switch (self.pattern[self.i]) {
                    // 0x24
                    '$',
                    => {},
                    // 0x28
                    '(',
                    => {},
                    // 0x29
                    ')',
                    => {},

                    // Ranges
                    // 0x2A
                    '*',
                    => {
                        self.i += 1;
                        self.state = .anyGreedy;
                        continue :stateLoop;
                    },
                    // 0x2B
                    '+',
                    => {
                        self.i += 1;
                        self.state = .moreGreedy;
                        continue :stateLoop;
                    },
                    // 0x3F
                    '?',
                    => {
                        self.i += 1;
                        return .{ .quantifier = .optional };
                    },
                    // 0x7B
                    '{',
                    => {
                        self.state = .initialRange;
                        self.i += 1;
                        continue :stateLoop;
                    },
                    // 0x7D
                    '}' => return Error.RangeEndBeforeStart,

                    // 0x2E
                    '.',
                    => {},
                    // 0x5B
                    '[',
                    => {},
                    // 0x5C
                    '\\',
                    => {},
                    // 0x5D
                    ']',
                    => {},
                    // 0x5E
                    '^',
                    => {},
                    // 0x7C
                    '|',
                    => {},

                    // Non-printables
                    0x00...0x1F,
                    // [ !"#]
                    0x20...0x23,
                    // [%&']
                    0x25...0x27,
                    // [,-]
                    0x2C...0x2D,
                    // 0x2F
                    '/',
                    // 0x30 - 0x39
                    '0'...'9',
                    // [:;<=>]
                    0x3A...0x3E,
                    // 0x40
                    '@',
                    // 0x41 - 0x5A
                    'A'...'Z',
                    // 0x5F
                    '_',
                    // 0x60
                    '`',
                    // 0x61 - 7A
                    'a'...'z',
                    // 0x7E
                    '~',
                    0x7F,
                    => {
                        const token: Token = .{ .atom = self.pattern[self.i] };
                        self.i += 1;
                        return token;
                    },

                    // invalid continuation in utf8 in this state
                    0x80...0xBF => return Error.Bad2ByteUTF8Start,
                    // Those violate shortest encoding rules for 2 bytes
                    0xC0...0xC1 => {},
                    // 2 bytes utf8
                    0xC2...0xDF => {},
                    // 3 bytes utf8
                    0xE0...0xEF => {},
                    // 4 bytes utf8
                    0xF0...0xF4 => {},
                    0xF5...0xFF => return Error.InvalidUTF8Byte1,
                }
            },
            .anyGreedy,
            => {
                if (self.i >= self.pattern.len) {
                    self.state = .done;
                    return .{ .quantifier = .anyGreedy };
                }
                switch (self.pattern[self.i]) {
                    '?' => {
                        self.i += 1;
                        self.state = .seekPiece;
                        return .{ .quantifier = .anyLazy };
                    },
                    else => {
                        self.state = .seekPiece;
                        return .{ .quantifier = .anyGreedy };
                    },
                }
            },
            .moreGreedy,
            => {
                if (self.i >= self.pattern.len) {
                    self.state = .done;
                    return .{ .quantifier = .moreGreedy };
                }
                switch (self.pattern[self.i]) {
                    '?' => {
                        self.i += 1;
                        self.state = .seekPiece;
                        return .{ .quantifier = .moreLazy };
                    },
                    else => {
                        self.state = .seekPiece;
                        return .{ .quantifier = .moreGreedy };
                    },
                }
            },
            .initialRange,
            => {},
            .done => return .done,
        }
        return Error.TBA;
    }
    unreachable;
}

const Scanner = @This();

pub fn initWithDiag(allocator: Allocator, pattern: []const u8) Scanner {
    var scanner: Scanner = .{
        .pattern = pattern,
    };
    scanner.diagnostics = .{
        .enabled = .{ .allocator = allocator, .idx = &scanner.i, .pattern = pattern },
    };
    return scanner;
}

test "Parse tokens" {
    const t = std.testing;

    const pattern: []const u8 = "ab+?c?de*f*?";
    var scanner: Scanner = .initWithDiag(t.allocator, pattern);
    try t.expectEqualDeep(@as([]const Token, &.{
        .branchStart,
        .{ .atom = 'a' },
        .{ .atom = 'b' },
        .{ .quantifier = .moreLazy },
        .{ .atom = 'c' },
        .{ .quantifier = .optional },
        .{ .atom = 'd' },
        .{ .atom = 'e' },
        .{ .quantifier = .anyGreedy },
        .{ .atom = 'f' },
        .{ .quantifier = .anyLazy },
        .done,
    }), &.{
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
        try scanner.next(),
    });
}
