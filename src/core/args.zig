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
    group: ?[]const u8 = null,

    multiline: bool = false,
    recursive: bool = false,
    @"follow-links": bool = false,
    colored: ?bool = null,
    verbose: bool = false,
    byteRanges: ?[]const []const usize = null,

    pub const Positionals = positionals.PositionalOf(.{
        .TupleType = struct { []const u8 },
        .ReminderType = ?[]const []const u8,
    });

    pub const Short = .{
        .lB = .@"line-by-line",
        .v = .@"invert-match",
        .O = .group,

        .mL = .multiline,
        .R = .recursive,
        .fL = .@"follow-links",

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
            .{ .field = .group, .description = "Output group number only" },

            .{ .field = .multiline, .description = "Multiline matching" },
            .{ .field = .recursive, .description = "Recursively matches all files in paths" },
            .{ .field = .@"follow-links", .description = "Follow symlinks, using a weakref visitor" },

            .{ .field = .colored, .description = "Colors matches" },
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
            .shortDescription = "Match-only operation. This is a dry-run with no replacement",
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
};

pub const HelpConf: help.HelpConf = .{
    .simpleTypes = true,
};

pub const ArgsRes = SpecResponseWithConfig(Args, HelpConf, true);
