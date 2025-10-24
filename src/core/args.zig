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

pub const Args = struct {
    @"line-by-line": bool = true,
    @"invert-match": bool = false,
    @"match-only": bool = false,

    // NOTE: default max group num
    // TODO: reintroduce bracket-less 1d arrays
    // TODO: expand tokenizer further to parse TargetGroup
    // TODO: Allow ranges
    groups: ?[]const u16 = null,

    multiline: bool = false,
    recursive: bool = false,
    @"follow-links": bool = false,

    colored: ?bool = null,
    @"group-highlight": bool = false,

    verbose: bool = false,
    byteRanges: ?[]const []const usize = null,

    pub const Positionals = positionals.PositionalOf(.{
        .TupleType = struct { []const u8 },
        .ReminderType = ?[]const []const u8,
    });

    pub const Short = .{
        .lB = .@"line-by-line",
        .v = .@"invert-match",
        .o = .@"match-only",

        .gN = .groups,

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
    };

    pub const Help: HelpData(@This()) = .{
        .usage = &.{"seeksub <options> <command> ... <pattern> <files>"},
        .description = "CLI tool to match, diff and apply regex in bulk using PCRE2. One of the main features of this CLI is the ability to seek byte ranges before matching or replacing",
        .optionsDescription = &.{
            .{ .field = .@"line-by-line", .description = "Line by line matching" },
            .{ .field = .@"invert-match", .description = "Invest matches, color wont be applied" },
            .{ .field = .@"match-only", .description = "Display only groups matched" },

            .{ .field = .groups, .description = "Pick group numbers to be colored, or if -O is used, to be displayed" },

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
        InvalidTargetGroup,
        TargetGroupsNotSorted,
    };

    pub fn targetGroup(self: *const @This(), max: usize) TargetGroupError!TargetGroup {
        // NOTE: we try to reduce to group0 as much as we can
        if (self.groups) |groups| {
            if (groups.len == 0) return TargetGroupError.InvalidTargetGroup;

            if (groups[0] + 1 > max) return TargetGroupError.InvalidTargetGroup;
            if (groups[0] == 0) return .{ .fixed = .{} };
            if (groups.len > 1) {
                // TODO: sort on parse
                for (groups[1..], 1..groups.len) |item, i| {
                    if (item == 0) return .{ .fixed = .{} };
                    if (item + 1 > max) return TargetGroupError.InvalidTargetGroup;
                    if (item < groups[i - 1]) return TargetGroupError.TargetGroupsNotSorted;
                }
            }

            return .{ .linearIter = .{ .arr = groups } };
        } else {
            if (self.@"group-highlight" and max > 1) {
                return .{ .range = .{} };
            }
            return .{ .fixed = .{} };
        }
    }
};

pub const FixedGroup = struct {
    target: usize = 0,

    pub fn includes(self: *const @This(), group: usize) bool {
        return self.target == group;
    }

    pub fn anyOtherThan(_: *const @This(), _: usize) bool {
        return false;
    }
};

pub const RangeGroup = struct {
    start: usize = 1,
    end: usize = std.math.maxInt(usize),

    pub fn includes(self: *const @This(), group: usize) bool {
        return group >= self.start and group <= self.end;
    }

    pub fn anyOtherThan(self: *const @This(), group: usize) bool {
        return self.end - group > rv: {
            break :rv if (self.includes(group)) @as(usize, 1) else @as(usize, 0);
        };
    }
};

pub const LinearGroup = struct {
    arr: []const u16,

    pub fn includes(self: *const @This(), group: usize) bool {
        for (self.arr) |item| if (item == group) return true;
        return false;
    }

    pub fn anyOtherThan(self: *const @This(), group: usize) bool {
        for (self.arr) |item| if (item > group) return true;
        return false;
    }
};

pub const TargetGroup = union(enum) {
    fixed: FixedGroup,
    range: RangeGroup,
    linearIter: LinearGroup,

    pub fn anyOtherThan(self: *const @This(), group: usize) bool {
        return switch (self.*) {
            inline else => |tg| tg.anyOtherThan(group),
        };
    }
};

pub const HelpConf: help.HelpConf = .{
    .simpleTypes = true,
};

pub const ArgsRes = SpecResponseWithConfig(Args, HelpConf, true);
