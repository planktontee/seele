const std = @import("std");
const coll = @import("../collections.zig");
const Allocator = std.mem.Allocator;

pub const TstArgCursor = struct {
    iterator: std.process.ArgIteratorGeneral(.{}),

    pub fn init(allocator: *const Allocator, data: [:0]const u8) !@This() {
        return .{
            .iterator = try std.process.ArgIteratorGeneral(.{}).init(
                allocator.*,
                data,
            ),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.iterator.deinit();
    }

    pub fn asCursor(self: *@This()) coll.Cursor([]const u8) {
        return .{
            .curr = null,
            .ptr = self,
            .vtable = &.{
                .next = next,
            },
        };
    }

    pub fn next(cursor: *anyopaque) ?[]const u8 {
        const self: *@This() = @ptrCast(@alignCast(cursor));
        return self.iterator.next();
    }
};

test "arg cursor peek" {
    const allocator = std.testing.allocator;
    var tstCursor = try TstArgCursor.init(&allocator,
        \\Hello
        \\World
    );
    defer tstCursor.deinit();
    var cursor = tstCursor.asCursor();

    try std.testing.expectEqualStrings("Hello", cursor.peek().?);
    try std.testing.expectEqualStrings("Hello", cursor.next().?);
    try std.testing.expectEqualStrings("World", cursor.peek().?);
    try std.testing.expectEqualStrings("World", cursor.peek().?);
    try std.testing.expectEqualStrings("World", cursor.next().?);
    try std.testing.expectEqual(null, cursor.peek());
}

test "arg cursor peek with stackItem" {
    const allocator = std.testing.allocator;
    var tstCursor = try TstArgCursor.init(&allocator, "");
    defer tstCursor.deinit();
    var cursor = tstCursor.asCursor();

    cursor.stackItem("Hello");
    try std.testing.expectEqualStrings("Hello", cursor.peek().?);
}

// TODO: move away from single quotes
pub const AtDepthArrayTokenizer = struct {
    input: []const u8,
    cursor: usize = 0,
    valueStart: usize = 0,
    state: State = .noop,
    stack: usize = 0,

    pub fn init(input: []const u8) @This() {
        return .{
            .input = input,
        };
    }

    pub const Error = error{
        CharBlackholed,
        UnsupportedQuotesOnArrayType,
        UnsupportedCharacterOnArrayType,
        EarlyBracketTermination,
        MissingArrayLayer,
        EmptyCommaSplit,
        EarlyArrayTermination,
        MissingCommaSeparator,
        ResultBlackholed,
        EarlyQuoteTermination,
        MissingTypeToken,

        UnsupportedBreakline,
        UnexpectedEndOfInput,
        SyntaxError,
    };

    const State = enum {
        noop,
        findArrayStart,
        phantom1dArray,
        value,
        arrayStart,
        postValue,
        post1dValue,
        string,
        subArrayString,
        anyValue,
        subArrayAnyValue,
    };

    pub fn skipWhiteSpace(self: *@This()) Error!void {
        while (self.cursor < self.input.len) : (self.cursor += 1) {
            switch (self.input[self.cursor]) {
                ' ', '\t' => continue,
                '\r', '\n' => return Error.UnsupportedBreakline,
                else => return,
            }
        }
    }

    pub fn expectPeek(self: *@This()) Error!u8 {
        if (self.cursor < self.input.len) return self.input[self.cursor];
        return Error.UnexpectedEndOfInput;
    }

    pub fn skipWhiteSpaceExpectByte(self: *@This()) Error!u8 {
        try self.skipWhiteSpace();
        return try self.expectPeek();
    }

    pub fn skipWhiteEspaceCheckEnd(self: *@This()) Error!bool {
        try self.skipWhiteSpace();
        if (self.cursor >= self.input.len) {
            if (self.stack == 0) {
                return true;
            }
            return Error.UnexpectedEndOfInput;
        }
        // Stacks are finished but there's a next token
        if (self.stack == 0 and self.state != .post1dValue) return Error.SyntaxError;
        return false;
    }

    pub fn takeValueSlice(self: *@This()) []const u8 {
        const slice = self.input[self.valueStart..self.cursor];
        self.valueStart = self.cursor;
        return slice;
    }

    // This relies on us skipping empty at start of match
    pub fn takeValueSliceWithoutEmpty(self: *@This()) []const u8 {
        const tmpCursor = self.cursor;
        self.cursor -= 1;
        while (self.cursor > 0) : (self.cursor -= 1) {
            switch (self.input[self.cursor]) {
                ' ', '\t' => continue,
                else => {
                    self.cursor += 1;
                    break;
                },
            }
        } else {
            self.cursor += 1;
        }
        const slice = self.takeValueSlice();
        self.cursor = tmpCursor;
        return slice;
    }

    pub fn next(self: *@This()) Error!?[]const u8 {
        stateLoop: while (true) {
            switch (self.state) {
                .noop => {
                    try self.skipWhiteSpace();
                    if (self.cursor >= self.input.len) {
                        self.state = .postValue;
                        continue :stateLoop;
                    } else {
                        self.state = .findArrayStart;
                        continue :stateLoop;
                    }
                },
                .findArrayStart => {
                    switch (try self.skipWhiteSpaceExpectByte()) {
                        '[' => {
                            self.cursor += 1;
                            self.stack += 1;
                            self.state = .arrayStart;
                            continue :stateLoop;
                        },
                        '\'', ']' => return Error.MissingArrayLayer,
                        ',' => return Error.SyntaxError,
                        else => {
                            self.valueStart = self.cursor;
                            self.cursor += 1;
                            self.state = .phantom1dArray;
                            continue :stateLoop;
                        },
                    }
                },
                .value => {
                    switch (try self.skipWhiteSpaceExpectByte()) {
                        '[' => {
                            self.valueStart = self.cursor;
                            self.cursor += 1;
                            self.stack += 1;
                            self.state = .subArrayAnyValue;
                            continue :stateLoop;
                        },

                        '\'' => {
                            self.cursor += 1;
                            self.valueStart = self.cursor;
                            self.state = .string;
                            continue :stateLoop;
                        },

                        ',', ']' => return Error.EmptyCommaSplit,

                        else => {
                            self.valueStart = self.cursor;
                            self.state = .anyValue;
                            continue :stateLoop;
                        },
                    }
                },
                .arrayStart => {
                    switch (try self.skipWhiteSpaceExpectByte()) {
                        ']' => {
                            self.stack -= 1;
                            self.cursor += 1;
                            self.state = .postValue;
                            continue :stateLoop;
                        },
                        else => {
                            self.state = .value;
                            continue :stateLoop;
                        },
                    }
                },
                .phantom1dArray => {
                    while (self.cursor < self.input.len) : (self.cursor += 1) {
                        switch (self.input[self.cursor]) {
                            ',' => {
                                // rewinding to remove empty to the right
                                const slice = self.takeValueSliceWithoutEmpty();
                                self.state = .post1dValue;
                                return slice;
                            },
                            ']',
                            '[',
                            '\'',
                            '\n',
                            '\r',
                            '"',
                            => return Error.SyntaxError,
                            else => continue,
                        }
                    }
                    const slice = self.takeValueSliceWithoutEmpty();
                    self.state = .post1dValue;
                    return slice;
                },
                .post1dValue => {
                    if (try self.skipWhiteEspaceCheckEnd()) return null;

                    switch (try self.expectPeek()) {
                        ',' => {
                            self.cursor += 1;
                            _ = try self.skipWhiteSpaceExpectByte();
                            self.valueStart = self.cursor;
                            self.state = .phantom1dArray;
                            continue :stateLoop;
                        },
                        else => return Error.SyntaxError,
                    }
                },
                .postValue => {
                    if (try self.skipWhiteEspaceCheckEnd()) return null;

                    switch (try self.expectPeek()) {
                        ']' => {
                            self.stack -= 1;
                            self.cursor += 1;
                            continue :stateLoop;
                        },
                        ',' => {
                            self.cursor += 1;
                            self.state = .value;
                            continue :stateLoop;
                        },
                        else => return Error.SyntaxError,
                    }
                },
                .string => {
                    while (self.cursor < self.input.len) : (self.cursor += 1) {
                        switch (self.input[self.cursor]) {
                            '\'' => {
                                const slice = self.takeValueSlice();
                                self.state = .postValue;
                                self.cursor += 1;
                                return slice;
                            },
                            else => continue,
                        }
                    }
                    return Error.EarlyQuoteTermination;
                },
                .subArrayString => {
                    while (self.cursor < self.input.len) : (self.cursor += 1) {
                        switch (self.input[self.cursor]) {
                            '\'' => {
                                self.state = .subArrayAnyValue;
                                self.cursor += 1;
                                continue :stateLoop;
                            },
                            else => continue,
                        }
                    }
                    return Error.EarlyQuoteTermination;
                },
                .anyValue => {
                    while (self.cursor < self.input.len) : (self.cursor += 1) {
                        switch (self.input[self.cursor]) {
                            ']' => {
                                const slice = self.takeValueSlice();
                                self.state = .postValue;
                                return slice;
                            },
                            '\t', ' ', ',' => {
                                const slice = self.takeValueSlice();
                                self.state = .postValue;
                                return slice;
                            },
                            '[' => {
                                self.stack += 1;
                                continue;
                            },
                            '\'', '\n', '\r', '"' => return Error.SyntaxError,
                            else => continue,
                        }
                    }
                    return Error.UnexpectedEndOfInput;
                },
                .subArrayAnyValue => {
                    while (self.cursor < self.input.len) : (self.cursor += 1) {
                        switch (self.input[self.cursor]) {
                            ']' => {
                                if (self.stack >= 2) self.stack -= 1;
                                if (self.stack == 1) {
                                    self.cursor += 1;
                                    const slice = self.takeValueSlice();
                                    self.state = .postValue;
                                    return slice;
                                }
                                continue;
                            },
                            '\'' => {
                                self.state = .subArrayString;
                                self.cursor += 1;
                                continue :stateLoop;
                            },
                            '[' => {
                                self.stack += 1;
                                continue;
                            },
                            '\n', '\r', '"' => return Error.SyntaxError,
                            else => continue,
                        }
                    }
                    return Error.UnexpectedEndOfInput;
                },
            }
        }
    }
};

test "simple array iterator" {
    const t = std.testing;
    var iterator = AtDepthArrayTokenizer.init("[1,2,3]");
    try t.expectEqualStrings("1", (try iterator.next()).?);
    try t.expectEqualStrings("2", (try iterator.next()).?);
    try t.expectEqualStrings("3", (try iterator.next()).?);
    try t.expectEqual(null, try iterator.next());
}

fn tstCollectTokens(allocator: *const Allocator, slice: []const u8) ![]const []const u8 {
    var result = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator.*, slice.len);
    var tokenizer = AtDepthArrayTokenizer.init(slice);

    while (try tokenizer.next()) |item| {
        try result.append(allocator.*, item);
    }

    return result.toOwnedSlice(allocator.*);
}

test "Depth 1 Array tokenizer (non-string)" {
    const t = std.testing;
    const E = AtDepthArrayTokenizer.Error;
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = &arena.allocator();

    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, "]"));
    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, " ]"));
    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, "] "));
    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, " ] "));
    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, "\t]"));
    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, "]\t"));
    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, "\t]\t"));
    try t.expectError(E.MissingArrayLayer, tstCollectTokens(allocator, " \t] \t "));

    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, ","));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, " ,"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, ", "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, " , "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "\t,\t"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, " \t,"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, ",\t "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, " \t, \t "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, ",1"));

    const expectEmpty: []const []const u8 = &.{};
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, ""));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, " "));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "  "));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "\t"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "\t "));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, " \t"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "\t\t"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, " \t "));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[ ]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[  ]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[\t]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[ \t]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[\t ]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[\t\t]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[ \t\t ]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, " []"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[] "));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, " [ ] "));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "\t[ ]\t"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, " \t[ \t ] \t"));

    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "["));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, " ["));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[ "));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "\t["));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[\t"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "\t[ \t"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, " \t[ \t "));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[1,"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, " [1,"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[1, "));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "\t[1,"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[1,\t"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "1,"));

    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1, ]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,  ]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,\t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,\t\t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1, \t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1\t,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1 \t,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1\t,\t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " [1, ] "));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "\t[1,\t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " \t[1, ]\t "));

    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ ,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[  ,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[\t,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[,\t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ \t,\t ]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ ,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[  ,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[\t,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[\t ,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ ,\t1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ \t,\t1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1, ,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,\t,]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1, ,\t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,\t,\t]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1, ,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,\t,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1, ,\t1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,\t,\t1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,\t, 1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " [1,,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,,1] "));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " [1,,1] "));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "\t[1,,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,,1]\t"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "\t[1,,1]\t"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " \t[1,,1]\t "));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ ,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " [ ,1] "));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "\t[ ,1]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ ,1]\t"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " \t[ ,1]\t "));

    const expectOne: []const []const u8 = &.{"1"};
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[1]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[ 1]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[1 ]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[ 1 ]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[\t1]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[1\t]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[\t1\t]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[ \t1\t ]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, " [1]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[1] "));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, " [1] "));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "\t[1]"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "[1]\t"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "\t[1]\t"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, " \t[1]\t "));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "1"));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, " 1 "));
    try t.expectEqualDeep(expectOne, try tstCollectTokens(allocator, "\t1\t"));

    const expectTwo: []const []const u8 = &.{ "1", "1" };
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1,1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1, 1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1 ,1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1 , 1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ 1,1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1,1 ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ 1,1 ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ 1 ,1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1 , 1 ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ 1 , 1 ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[\t1,1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1,\t1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1,1\t]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[\t1,\t1\t]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ \t1\t , \t1\t ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, " [1,1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1,1] "));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, " [1,1] "));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "\t[1,1]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[1,1]\t"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "\t[1,1]\t"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, " \t[1,1]\t "));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "1,1"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "1, 1"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, " 1 , 1 "));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "\t1,\t1\t"));
}

test "Depth 2+ Array tokenizer (non-string)" {
    const t = std.testing;
    const E = AtDepthArrayTokenizer.Error;
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = &arena.allocator();

    const expectOneEmpty: []const []const u8 = &.{"[]"};
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, "[[]]"));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, " [[]]"));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, "[[]] "));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, " [[]] "));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, "\t[[]]\t"));

    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, "[ [] ]"));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, " [ [] ]"));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, "[ [] ] "));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, " [ [] ] "));
    try t.expectEqualDeep(expectOneEmpty, try tstCollectTokens(allocator, "\t[ [] ]\t"));

    const expectOneSpace: []const []const u8 = &.{"[ ]"};
    try t.expectEqualDeep(expectOneSpace, try tstCollectTokens(allocator, "[ [ ] ]"));
    try t.expectEqualDeep(expectOneSpace, try tstCollectTokens(allocator, " [ [ ] ]"));
    try t.expectEqualDeep(expectOneSpace, try tstCollectTokens(allocator, "[ [ ] ] "));
    try t.expectEqualDeep(expectOneSpace, try tstCollectTokens(allocator, " [ [ ] ] "));
    try t.expectEqualDeep(expectOneSpace, try tstCollectTokens(allocator, "\t[ [ ] ]\t"));

    const expectOneRaw1: []const []const u8 = &.{"[1,]"};
    const expectOneRaw1Space: []const []const u8 = &.{"[1, ]"};
    const expectMultipleRaw: []const []const u8 = &.{ "[]", "[1]", "[2,3]" };

    try t.expectEqualDeep(expectOneRaw1, try tstCollectTokens(allocator, "[[1,]]"));
    try t.expectEqualDeep(expectOneRaw1, try tstCollectTokens(allocator, " [[1,]] "));
    try t.expectEqualDeep(expectOneRaw1, try tstCollectTokens(allocator, "\t[[1,]]\t"));
    try t.expectEqualDeep(expectOneRaw1Space, try tstCollectTokens(allocator, "[[1, ]]"));
    try t.expectEqualDeep(expectOneRaw1Space, try tstCollectTokens(allocator, " [[1, ]] "));
    try t.expectEqualDeep(expectOneRaw1Space, try tstCollectTokens(allocator, "\t[[1, ]]\t"));
    try t.expectEqualDeep(expectMultipleRaw, try tstCollectTokens(allocator, "[[],[1],[2,3]]"));
    try t.expectEqualDeep(expectMultipleRaw, try tstCollectTokens(allocator, " [[],[1],[2,3]] "));
    try t.expectEqualDeep(expectMultipleRaw, try tstCollectTokens(allocator, "\t[[],[1],[2,3]]\t"));
    try t.expectEqualDeep(expectMultipleRaw, try tstCollectTokens(allocator, "[ [], [1], [2,3] ]"));
    try t.expectEqualDeep(expectMultipleRaw, try tstCollectTokens(allocator, " [ [], [1], [2,3] ] "));
    try t.expectEqualDeep(expectMultipleRaw, try tstCollectTokens(allocator, "\t[ [], [1], [2,3] ]\t"));

    const expectOneRawNested: []const []const u8 = &.{"[[]]"};
    try t.expectEqualDeep(expectOneRawNested, try tstCollectTokens(allocator, "[[[]]]"));
    try t.expectEqualDeep(expectOneRawNested, try tstCollectTokens(allocator, " [[[]]] "));
    try t.expectEqualDeep(expectOneRawNested, try tstCollectTokens(allocator, "\t[[[]]]\t"));

    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[["));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[[]"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[[[]]"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[[1,]"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[[], [1], [2,3]"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, " [ [ ]"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[[1], [2,]"));

    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2], ]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2], [3],]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[], , [1]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2], , [3]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[], , [1]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[],  , [1]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[], \t, [1]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[], ,\t[1]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[], , [1]] "));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " [[], , [1]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2], , [3]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2],  , [3]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2], \t, [3]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2], ,\t[3]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[[1, 2], , [3]] "));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, " [[1, 2], , [3]]"));

    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[[]] ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[[]]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[[]] ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[[]]] "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, " [ []]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ [ ] ] ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ [ ] ] ] "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "\t[[]]]\t"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[1]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[1] ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[1]] "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, " [1]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ 1 ] ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ 1 ] ] "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "\t[1]]\t"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[1, 2]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[1, 2] ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[1, 2]] "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, " [1, 2]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ 1, 2 ] ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ 1, 2 ] ] "));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "\t[1, 2]]\t"));

    const expectMixedDepth: []const []const u8 = &.{ "1", "[3, 4]", "2", "[[4], 4]" };
    try t.expectEqualDeep(expectMixedDepth, try tstCollectTokens(allocator, "[1, [3, 4], 2, [[4], 4]]"));

    const expectMixedDepthSpaces: []const []const u8 = &.{ "1", "[ 3 , 4 ]", "2", "[[4], 4]" };
    try t.expectEqualDeep(expectMixedDepthSpaces, try tstCollectTokens(allocator, "[ 1 , [ 3 , 4 ] , 2 , [[4], 4] ]"));

    const expectMixedDepthTabs: []const []const u8 = &.{ "1", "[3,\t4]", "2", "[[4],\t4]" };
    try t.expectEqualDeep(expectMixedDepthTabs, try tstCollectTokens(allocator, "[1,\t[3,\t4],2,[[4],\t4]]"));

    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[1, [3, 4], 2, [[4], 4]"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[1, [3, 4], 2, [[4], 4"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "[1, [3, 4], 2, [[4], 4],"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1, [3, 4], , 2, [[4], 4]]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[1,, [3, 4], 2, [[4], 4]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[1, [3, 4], 2, [[4], 4]]]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "1, [3, 4], 2, [[4], 4]"));
}

test "Depth 1 Array tokenizer (strings)" {
    const t = std.testing;
    const E = AtDepthArrayTokenizer.Error;
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = &arena.allocator();

    const expectEmptyString: []const []const u8 = &.{""};
    try t.expectEqualDeep(expectEmptyString, try tstCollectTokens(allocator, "[ '' ]"));
    const expectSpaceString: []const []const u8 = &.{" "};
    try t.expectEqualDeep(expectSpaceString, try tstCollectTokens(allocator, "[ ' ' ]"));
    const expectWhitespaceString: []const []const u8 = &.{"  "};
    try t.expectEqualDeep(expectWhitespaceString, try tstCollectTokens(allocator, "[ '  ' ]"));
    const expectTabString: []const []const u8 = &.{"\t"};
    try t.expectEqualDeep(expectTabString, try tstCollectTokens(allocator, "[ '\t' ]"));
    const expectTabSpaceString: []const []const u8 = &.{"\t "};
    try t.expectEqualDeep(expectTabSpaceString, try tstCollectTokens(allocator, "[ '\t ' ]"));
    const expectMixedWhitespaceString: []const []const u8 = &.{" \t\t "};
    try t.expectEqualDeep(expectMixedWhitespaceString, try tstCollectTokens(allocator, "[ ' \t\t ' ]"));

    const expectTwo: []const []const u8 = &.{ "a", "b" };
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "['a','b']"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ 'a', 'b' ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ 'a' , 'b' ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[ 'a'\t,\t'b' ]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[\t'a'\t,\t'b'\t]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[\t'a' , 'b'\t]"));
    try t.expectEqualDeep(expectTwo, try tstCollectTokens(allocator, "[\t'a'\t, 'b' ]"));

    const expectEmpty: []const []const u8 = &.{ "", "" };
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "['','']"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[ '', '' ]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[ '' , '' ]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[ ''\t,\t'' ]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[\t''\t,\t''\t]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[\t'' , ''\t]"));
    try t.expectEqualDeep(expectEmpty, try tstCollectTokens(allocator, "[\t''\t, '' ]"));

    const expectArrayStrings: []const []const u8 = &.{ "[1,2]", "[3,4]" };
    try t.expectEqualDeep(expectArrayStrings, try tstCollectTokens(allocator, "['[1,2]', '[3,4]']"));
    try t.expectEqualDeep(expectArrayStrings, try tstCollectTokens(allocator, "[ '[1,2]' , '[3,4]' ]"));
    try t.expectEqualDeep(expectArrayStrings, try tstCollectTokens(allocator, "[\t'[1,2]',\t'[3,4]'\t]"));
    try t.expectEqualDeep(expectArrayStrings, try tstCollectTokens(allocator, "[ '[1,2]', '[3,4]' ]"));

    const expectArrayInsideString: []const []const u8 = &.{"[1,2]"};
    try t.expectEqualDeep(expectArrayInsideString, try tstCollectTokens(allocator, "['[1,2]']"));

    const expectNestedArrayInString: []const []const u8 = &.{"[[1,2]]"};
    try t.expectEqualDeep(expectNestedArrayInString, try tstCollectTokens(allocator, "['[[1,2]]']"));

    const expectArrayAndCommaInString: []const []const u8 = &.{"[1, 2], [3,4]"};
    try t.expectEqualDeep(expectArrayAndCommaInString, try tstCollectTokens(allocator, "['[1, 2], [3,4]']"));

    const expectMixed: []const []const u8 = &.{ "a", "[1,2]", "b" };
    try t.expectEqualDeep(expectMixed, try tstCollectTokens(allocator, "['a', '[1,2]', 'b']"));

    const expectMoreMixed: []const []const u8 = &.{ "[a]", "[b,c]", "x", "y" };
    try t.expectEqualDeep(expectMoreMixed, try tstCollectTokens(allocator, "['[a]', '[b,c]', 'x', 'y']"));

    const expectSpaced: []const []const u8 = &.{ "[ 1 , 2 ]", "[ 3 , 4 ]" };
    try t.expectEqualDeep(expectSpaced, try tstCollectTokens(allocator, "[ '[ 1 , 2 ]' , '[ 3 , 4 ]' ]"));
    try t.expectEqualDeep(expectSpaced, try tstCollectTokens(allocator, "[\t'[ 1 , 2 ]',\t'[ 3 , 4 ]'\t]"));

    const expectUnbalancedBracketsInString: []const []const u8 = &.{"[[[["};
    try t.expectEqualDeep(expectUnbalancedBracketsInString, try tstCollectTokens(allocator, "['[[[[']"));

    const expectMixedBracketInString2: []const []const u8 = &.{ "[w,]", "[", "]" };
    try t.expectEqualDeep(expectMixedBracketInString2, try tstCollectTokens(allocator, "['[w,]', '[', ']']"));

    try t.expectError(E.EarlyQuoteTermination, tstCollectTokens(allocator, "['a]"));
    try t.expectError(E.EarlyQuoteTermination, tstCollectTokens(allocator, "[ 'a ]"));
    try t.expectError(E.EarlyQuoteTermination, tstCollectTokens(allocator, "['a', 'b]"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "['a', 'b'"));

    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[''w]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ 'a'b ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "[ 'abc'1 ]"));
    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "['it'broken']"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[, 'a']"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "['a', , 'b']"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "['a', ]"));
    try t.expectError(E.EmptyCommaSplit, tstCollectTokens(allocator, "[ 'a',    ]"));

    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "['a',"));
    try t.expectError(E.UnexpectedEndOfInput, tstCollectTokens(allocator, "['a'"));
}

test "Depth 2+ Array tokenizer (strings)" {
    const t = std.testing;
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = &arena.allocator();

    const expectInnerQuoted: []const []const u8 = &.{ "[1, 'a']", "[2, 'b']" };
    try t.expectEqualDeep(expectInnerQuoted, try tstCollectTokens(allocator, "[[1, 'a'], [2, 'b']]"));

    const expectQuotedOnlyInner: []const []const u8 = &.{ "['a','b']", "['c','d']" };
    try t.expectEqualDeep(expectQuotedOnlyInner, try tstCollectTokens(allocator, "[['a','b'], ['c','d']]"));

    const expectNestedMix: []const []const u8 = &.{ "[1, 'a', [2, 'b']]", "x" };
    try t.expectEqualDeep(expectNestedMix, try tstCollectTokens(allocator, "[[1, 'a', [2, 'b']], 'x']"));

    const expectDeepStringInside: []const []const u8 = &.{ "[[ 'abc', [1,2] ]]", "tail" };
    try t.expectEqualDeep(expectDeepStringInside, try tstCollectTokens(allocator, "[[[ 'abc', [1,2] ]], 'tail']"));

    const expectSpacedQuoted: []const []const u8 = &.{ "[ 1 , 'a' ]", "[ 'b' , 2 ]" };
    try t.expectEqualDeep(expectSpacedQuoted, try tstCollectTokens(allocator, "[ [ 1 , 'a' ] , [ 'b' , 2 ] ]"));

    const expectMixedWhitespace: []const []const u8 = &.{ "[1,\t'a']", "[\t'b',2]" };
    try t.expectEqualDeep(expectMixedWhitespace, try tstCollectTokens(allocator, "[ [1,\t'a'] , [\t'b',2] ]"));
}

test "Any depth any match tricky cases" {
    const t = std.testing;
    const E = AtDepthArrayTokenizer.Error;
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = &arena.allocator();

    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "['a', 'b']['c', 'd']"));
    // Sanitization of inner-level doesn't happen at top-level
    try t.expectError(E.EarlyQuoteTermination, tstCollectTokens(allocator, "[[1, 2], [3, 4']"));

    const expectTricky1: []const []const u8 = &.{ "['a,b', 'c']", "['d', 'e']" };
    try t.expectEqualDeep(expectTricky1, try tstCollectTokens(allocator, "[['a,b', 'c'], ['d', 'e']]"));

    const expectTricky2: []const []const u8 = &.{"['[1,2]', '[3,4]']"};
    try t.expectEqualDeep(expectTricky2, try tstCollectTokens(allocator, "[['[1,2]', '[3,4]']]"));

    const expectTricky3: []const []const u8 = &.{"['1, 2], [2,3']"};
    try t.expectEqualDeep(expectTricky3, try tstCollectTokens(allocator, "[['1, 2], [2,3']]"));

    const expectTricky4: []const []const u8 = &.{"1, 2], [2,3"};
    try t.expectEqualDeep(expectTricky4, try tstCollectTokens(allocator, "['1, 2], [2,3']"));

    try t.expectError(E.EarlyQuoteTermination, tstCollectTokens(allocator, "['[1, 2]', [3, '4]"));

    const expectTricky5: []const []const u8 = &.{ "1", "2, 3], [4, 5" };
    try t.expectEqualDeep(expectTricky5, try tstCollectTokens(allocator, "[1, '2, 3], [4, 5']"));

    try t.expectError(E.SyntaxError, tstCollectTokens(allocator, "['[a, b'],"));

    const expectTricky6: []const []const u8 = &.{ "['[x, y], z']", "['a, b]']" };
    try t.expectEqualDeep(expectTricky6, try tstCollectTokens(allocator, "[['[x, y], z'], ['a, b]']]"));

    try t.expectError(E.EarlyQuoteTermination, tstCollectTokens(allocator, "[['1, 2'], ['3]]"));

    const expectTricky7: []const []const u8 = &.{ "1", "2", "3, [4, 5]" };
    try t.expectEqualDeep(expectTricky7, try tstCollectTokens(allocator, "[1, 2, '3, [4, 5]']"));
}
