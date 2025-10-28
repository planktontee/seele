const std = @import("std");
const zpec = @import("zpec");
const args = zpec.args;
const spec = args.spec;
const help = args.help;
const validate = args.validate;
const coll = zpec.collections;
const Cursor = coll.Cursor;
const ComptSb = coll.ComptSb;
const HelpData = help.HelpData;
const GroupMatchConfig = validate.GroupMatchConfig;
const SpecResponseWithConfig = spec.SpecResponseWithConfig;
const positionals = args.positionals;
const fs = @import("fs.zig");
const sink = @import("sink.zig");

const DefaultCodec = args.codec.ArgCodec(Args);
const CursorT = zpec.collections.Cursor([]const u8);

pub const ArgsCodec = struct {
    defaultCodec: DefaultCodec = .{},

    pub const Error = error{
        InvalidRangeToken,
        UnexpectedRangeTokenEnd,
        LinearItemsNotSorted,
        InvalidRange,
        RangesOverlap,
        RangesNotSorted,
        ItemOverlapsWithRange,
        EmptyTargetGroups,
        Group0NotAlone,
    } || DefaultCodec.Error;

    pub fn supports(
        comptime T: type,
        comptime tag: DefaultCodec.SpecFieldEnum,
    ) bool {
        return tag == .groups and T == TargetGroup;
    }

    pub fn parseByType(
        self: *@This(),
        comptime T: type,
        comptime tag: DefaultCodec.SpecFieldEnum,
        allocator: *const std.mem.Allocator,
        cursor: *CursorT,
    ) Error!T {
        if (comptime !supports(T, tag)) {
            return try self.defaultCodec.parseByType(T, tag, allocator, cursor);
        }

        if (comptime T != TargetGroup or tag != .groups) @compileError("Unsupported type in custom codec");

        return try self.parseGroups(allocator, cursor);
    }

    pub fn parseGroups(
        _: *@This(),
        allocator: *const std.mem.Allocator,
        cursor: *CursorT,
    ) Error!TargetGroup {
        const arr = try args.codec.PrimitiveCodec.parseArray(
            &args.codec.PrimitiveCodec{},
            []const []const u8,
            .null,
            allocator,
            cursor,
        );
        defer allocator.free(arr);

        var linearIterTargets = try std.ArrayListUnmanaged(u16).initCapacity(allocator.*, 3);
        errdefer linearIterTargets.deinit(allocator.*);
        var targets = try std.ArrayListUnmanaged(RangeGroup).initCapacity(allocator.*, 3);
        errdefer targets.deinit(allocator.*);

        const State = enum {
            none,
            digit0,
            number,
            range1,
            range2,
            numberAfterRange,
        };

        var state: State = .none;

        // TODO: move range tokenizer elsewhere
        for (arr) |item| {
            var start: u16 = 0;
            var end: u16 = 0;
            var n2: usize = 0;
            var i: usize = 0;
            stateLoop: while (true) {
                switch (state) {
                    .none => {
                        if (i >= item.len) break;
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
                            else => return std.fmt.ParseIntError.InvalidCharacter,
                        }
                    },
                    .digit0 => {
                        if (i >= item.len) {
                            start = 0;
                            state = .number;
                            break;
                        }
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
                                    start = try std.fmt.parseInt(u16, item[0..i], 10);
                                    i += 1;
                                    state = .range1;
                                    continue :stateLoop;
                                },
                                else => return std.fmt.ParseIntError.InvalidCharacter,
                            }
                        }
                        start = try std.fmt.parseInt(u16, item[0..i], 10);
                        break;
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
                            else => return std.fmt.ParseIntError.InvalidCharacter,
                        }
                    },
                    .numberAfterRange => {
                        while (i < item.len) : (i += 1) {
                            switch (item[i]) {
                                '0'...'9' => {
                                    continue;
                                },
                                else => return std.fmt.ParseIntError.InvalidCharacter,
                            }
                        }
                        end = try std.fmt.parseInt(u16, item[n2..i], 10);
                        break :stateLoop;
                    },
                }
            }

            switch (state) {
                .number => {
                    if (linearIterTargets.items.len == 0) {
                        try linearIterTargets.append(allocator.*, start);
                    } else {
                        const last = linearIterTargets.items[linearIterTargets.items.len - 1];
                        if (last == 0) return Error.Group0NotAlone;
                        if (last >= start) return Error.LinearItemsNotSorted;
                        try linearIterTargets.append(allocator.*, start);
                    }
                },
                .numberAfterRange => {
                    if (start >= end or start == 0) return Error.InvalidRange;

                    if (targets.items.len == 0) {
                        try targets.append(
                            allocator.*,
                            .{ .start = start, .end = end },
                        );
                    } else {
                        const last = targets.items[targets.items.len - 1];

                        if (last.includes(start) or
                            last.includes(end)) return Error.RangesOverlap;

                        if (last.end > start) return Error.RangesNotSorted;

                        try targets.append(
                            allocator.*,
                            .{ .start = start, .end = end },
                        );
                    }
                },
                else => return Error.UnexpectedEndOfInput,
            }

            state = .none;
        }

        if (targets.items.len == 0 and linearIterTargets.items.len == 0) return Error.EmptyTargetGroups;

        if (linearIterTargets.items.len == 1 and
            linearIterTargets.items[0] == 0)
        {
            targets.deinit(allocator.*);
            linearIterTargets.deinit(allocator.*);
            return .zero;
        }

        var chainedGroup: ChainedGroup = .{};
        if (linearIterTargets.items.len > 0) {
            const linearItems = try linearIterTargets.toOwnedSlice(allocator.*);
            for (targets.items) |range| {
                for (linearItems) |item| {
                    if (range.includes(item)) return Error.ItemOverlapsWithRange;
                }
            }

            chainedGroup.perItem = .{ .arr = linearItems };
        } else {
            linearIterTargets.deinit(allocator.*);
        }

        if (targets.items.len > 0) {
            chainedGroup.ranges = try targets.toOwnedSlice(allocator.*);
        } else {
            targets.deinit(allocator.*);
        }

        return .{ .chained = chainedGroup };
    }
};

pub const Args = struct {
    @"line-by-line": bool = true,
    @"invert-match": bool = false,
    @"match-only": bool = false,
    @"group-only": bool = false,

    groups: TargetGroup = .zero,
    @"group-delimiter": u8 = '\n',

    multiline: bool = false,
    recursive: bool = false,
    @"follow-links": bool = false,

    colored: ?bool = null,
    @"group-highlight": bool = false,

    verbose: bool = false,
    byteRanges: ?[]const []const usize = null,

    pub const Codec = ArgsCodec;

    pub const Positionals = positionals.PositionalOf(.{
        .TupleType = struct { []const u8 },
        .ReminderType = ?[]const []const u8,
    });

    pub const Short = .{
        .lB = .@"line-by-line",
        .v = .@"invert-match",
        .o = .@"match-only",
        .O = .@"group-only",

        .gN = .groups,
        .gD = .@"group-delimiter",

        .mL = .multiline,
        .R = .recursive,
        .fL = .@"follow-links",

        .gH = .@"group-highlight",

        .bR = .byteRanges,
    };

    pub const Verb = union(enum) {
        match: Match,
        diff: Diff,
        apply: Apply,
    };

    pub const GroupMatch: GroupMatchConfig(@This()) = .{
        // TODO: rethink groupmatching to allow defaults and checks to play together
        // .mutuallyExclusive = &.{
        //     &.{ .byteRanges, .@"line-by-line", .multiline },
        // },
        //
        // One-way inclusive:
        // --groups -> --group-highlight
        //
    };

    pub const Help: HelpData(@This()) = .{
        .usage = &.{"seeksub <options> <command> ... <pattern> <files>"},
        .description = "CLI tool to match, diff and apply regex in bulk using PCRE2. One of the main features of this CLI is the ability to seek byte ranges before matching or replacing",
        .optionsDescription = &.{
            .{ .field = .@"line-by-line", .description = "Line by line matching" },
            .{ .field = .@"invert-match", .description = "Invest matches, color wont be applied" },
            .{ .field = .@"match-only", .description = "Display only matched text" },
            .{ .field = .@"group-only", .description = "Display only groups matched, split by delimiter, see --group-delimiter." },

            .{ .field = .groups, .description = "Pick group numbers to be colored, or if -O is used, to be displayed" },
            .{ .field = .@"group-delimiter", .description = "Change group delimiter for --group-only" },

            .{ .field = .multiline, .description = "Multiline matching" },
            .{ .field = .recursive, .description = "Recursively matches all files in paths" },
            .{ .field = .@"follow-links", .description = "Follow symlinks, using a weakref visitor" },

            .{ .field = .colored, .description = "Colors matches" },
            .{ .field = .@"group-highlight", .description = "Uses a color table for each group match, this will also only color group matches other than 0 unless overriden with --groups" },

            .{ .field = .verbose, .description = "Verbose mode" },
            .{ .field = .byteRanges, .description = "Range of bytes for n files, top-level array length has to be of (len <= files.len) and will be applied sequentially over files" },
        },
        .positionalsDescription = .{
            .tuple = &.{
                "PCRE2 regex pattern to use on all files",
            },
            .reminder = "Files or paths to be operated on",
        },
    };

    pub const Match = struct {
        @"match-n": ?usize = null,

        pub const Positionals = positionals.EmptyPositionalsOf;

        pub const Short = .{
            .n = .@"match-n",
        };

        pub const Help: HelpData(@This()) = .{
            .usage = &.{"seeksub ... match <options> ..."},
            .description = "Matches based on options at the top-level. This performs no mutation or replacement, it's simply a dry-run",
            .shortDescription = "(Default) Match-only operation. This is a dry-run with no replacement",
            .optionsDescription = &.{
                .{ .field = .@"match-n", .description = "N-match stop for each file if set" },
            },
        };

        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .ensureCursorDone = false,
        };
    };

    pub const Diff = struct {
        replace: []const u8 = undefined,

        const Positionals = positionals.EmptyPositionalsOf;

        pub const Short = .{
            .r = .replace,
        };

        pub const Help: HelpData(@This()) = .{
            .usage = &.{"seeksub ... diff <options> ..."},
            .description = "Matches based on options at the top-level and then performs a replacement over matches, providing a diff return but not actually mutating the files",
            .shortDescription = "Dry-runs replacement. No mutation is performed",
            .optionsDescription = &.{
                .{ .field = .replace, .description = "Replace match on all files using this PCRE2 regex" },
            },
        };

        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .required = &.{.replace},
            .ensureCursorDone = false,
        };
    };

    pub const Apply = struct {
        replace: []const u8 = undefined,
        trace: bool = false,

        pub const Positionals = positionals.EmptyPositionalsOf;

        pub const Short = .{
            .r = .replace,
            .tt = .trace,
        };

        pub const Help: HelpData(@This()) = .{
            .usage = &.{"seeksub ... apply <options> ..."},
            .description = "Matches based on options at the top-level and then performs a replacement over matches. This is mutate the files",
            .shortDescription = "Replaces based on match and replace PCRE2 regexes over all files",
            .optionsDescription = &.{
                .{ .field = .replace, .description = "Replace match on all files using this PCRE2 regex" },
                .{ .field = .trace, .description = "Trace mutations" },
            },
        };

        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .required = &.{.replace},
            .ensureCursorDone = false,
        };
    };

    pub fn hasColor(self: *const @This(), fileType: fs.FileType) bool {
        const colored = self.colored;

        return !self.@"invert-match" and ((colored != null and colored.? == true) or
            (fileType == .tty and colored == null));
    }

    pub const TargetGroupError = error{
        InvalidGroupsForOptions,
    };

    pub fn targetGroup(self: *const @This(), fSink: *const sink.Sink, maxGroups: usize) TargetGroupError!TargetGroup {
        switch (fSink.eventHandler) {
            // Deciding whether or not to use colors is on the event handler
            .colorMatch,
            .coloredMatchOnly,
            => {
                if (maxGroups > 1 and self.@"group-highlight" and self.groups == .zero) {
                    return .{ .range = .{ .start = 1, .end = maxGroups } };
                }
                return self.groups;
            },
            .coloredGroupOnly,
            .groupOnly,
            => {
                if (maxGroups > 1 and self.groups == .zero) {
                    return .{ .range = .{ .start = 1, .end = maxGroups } };
                }
                return self.groups;
            },
            .matchOnly,
            .skipLineOnMatch,
            .pickNonMatchingLine,
            => return .zero,
        }
        unreachable;
    }
};

pub const RangeGroup = struct {
    start: usize = 1,
    end: usize = std.math.maxInt(usize),

    pub fn includes(self: *const @This(), group: usize) bool {
        return group >= self.start and group <= self.end;
    }

    pub fn anyGreaterThan(self: *const @This(), group: usize) bool {
        return self.end > group;
    }
};

pub const PerItemGroup = struct {
    arr: []const u16,

    pub fn includes(self: *const @This(), group: usize) bool {
        for (self.arr) |item| if (item == group) return true;
        return false;
    }

    pub fn anyGreaterThan(self: *const @This(), group: usize) bool {
        if (self.arr.len > 0 and self.arr[self.arr.len - 1] > group) return true;
        return false;
    }
};

pub const ChainedGroup = struct {
    ranges: []const RangeGroup = &.{},
    perItem: PerItemGroup = .{ .arr = &.{} },

    pub fn includes(self: *const @This(), group: usize) bool {
        for (self.ranges) |range| if (range.includes(group)) return true;
        return self.perItem.includes(group);
    }

    pub fn anyGreaterThan(self: *const @This(), group: usize) bool {
        if (self.ranges.len > 0 and
            self.ranges[self.ranges.len - 1].anyGreaterThan(group))
        {
            return true;
        }
        return self.perItem.anyGreaterThan(group);
    }
};

pub const ZeroGroup = struct {
    pub fn includes(_: *const @This(), group: usize) bool {
        return group == 0;
    }

    pub fn anyGreaterThan(_: *const @This(), _: usize) bool {
        return false;
    }
};

pub const TargetGroup = union(enum) {
    zero: ZeroGroup,
    range: RangeGroup,
    perItem: PerItemGroup,
    chained: ChainedGroup,

    pub fn includes(self: *const @This(), group: usize) bool {
        return switch (self.*) {
            inline else => |tg| tg.includes(group),
        };
    }

    pub fn anyGreaterThan(self: *const @This(), group: usize) bool {
        return switch (self.*) {
            inline else => |tg| tg.anyGreaterThan(group),
        };
    }
};

pub const HelpConf: help.HelpConf = .{
    .simpleTypes = true,
};

pub const ArgsRes = SpecResponseWithConfig(Args, HelpConf, true);
