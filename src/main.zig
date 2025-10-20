const std = @import("std");
const zpec = @import("zpec");
const units = zpec.units;
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
const regex = @import("core/regex.zig");
const sink = @import("core/sink.zig");
const fs = @import("core/fs.zig");
const source = @import("core/source.zig");

const Args = struct {
    @"line-by-line": bool = true,
    @"group-only": bool = false,
    multiline: bool = false,
    recursive: bool = false,
    @"follow-links": bool = false,
    colored: bool = false,
    verbose: bool = false,
    byteRanges: ?[]const []const usize = null,

    pub const Positionals = positionals.PositionalOf(.{
        .TupleType = struct { []const u8 },
        .ReminderType = ?[]const []const u8,
    });

    pub const Short = .{
        .lB = .@"line-by-line",
        .O = .@"group-only",
        .mL = .multiline,
        .R = .recursive,
        .fL = .@"follow-links",
        .v = .verbose,
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
            .{ .field = .byteRanges, .description = "Range of bytes for n files, top-level array length has to be of (len <= files.len) and will be applied sequentially over files" },
            .{ .field = .@"line-by-line", .description = "Line by line matching" },
            .{ .field = .@"group-only", .description = "Only outputs group match 0 instead of whole line" },
            .{ .field = .multiline, .description = "Multiline matching" },
            .{ .field = .recursive, .description = "Recursively matches all files in paths" },
            .{ .field = .@"follow-links", .description = "Follow symlinks, using a weakref visitor" },
            .{ .field = .colored, .description = "Colors matches" },
            .{ .field = .verbose, .description = "Verbose mode" },
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
};

pub const HelpConf: help.HelpConf = .{
    .simpleTypes = true,
};

pub const ArgsRes = SpecResponseWithConfig(Args, HelpConf, true);

var reporter: *const sink.Reporter = undefined;

pub fn main() !u8 {
    reporter = rv: {
        var r: sink.Reporter = .{};
        var buffOut: [units.ByteUnit.mb * 1]u8 = undefined;
        var buffErr: [units.ByteUnit.mb * 1]u8 = undefined;

        r.stdoutW = rOut: {
            var writer = std.fs.File.stderr().writer(&buffOut);
            break :rOut &writer.interface;
        };
        r.stderrW = rErr: {
            var writer = std.fs.File.stderr().writer(&buffErr);
            break :rErr &writer.interface;
        };

        break :rv &r;
    };

    var sfba = std.heap.stackFallback(4098, std.heap.page_allocator);
    const allocator = sfba.get();

    var result: ArgsRes = .init(allocator);
    defer result.deinit();
    defer std.fs.File.stdout().writeAll(reporter.stdoutW.buffered()) catch unreachable;
    defer reporter.stderrW.flush() catch unreachable;

    if (result.parseArgs()) |err| {
        if (err.message) |message| {
            try reporter.stderrW.print("Last opt <{?s}>, Last token <{?s}>. ", .{
                err.lastOpt,
                err.lastToken,
            });
            try reporter.stderrW.writeAll(message);
            return 1;
        }
    }

    if (result.verb == null) result.verb = .{ .match = .initEmpty(allocator) };

    // TODO: make sink system
    // STDOUT pipe and file should have fast output, tty should be slower
    // STDIN pipe and file should have fast input, tty should be slower
    try run(&result);
    return 0;
}

pub const FileType = union(enum) {
    stdin,
    file: []const u8,

    pub fn name(self: *const @This()) []const u8 {
        return switch (self.*) {
            .stdin => "stdin",
            .file => |fileArg| fileArg,
        };
    }
};

pub const OpenError = std.fs.File.OpenError || std.posix.RealPathError;

pub fn open(fileArg: []const u8) OpenError!std.fs.File {
    var buff: [4098]u8 = undefined;
    const filePath = try std.fs.cwd().realpath(fileArg, &buff);
    return try std.fs.openFileAbsolute(filePath, .{ .mode = .read_only });
}

pub const FileCursor = struct {
    files: [1]FileType = undefined,
    current: ?std.fs.File = null,
    idx: usize = 0,

    pub fn init(argsRes: *const ArgsRes) @This() {
        var self: @This() = .{};

        if (argsRes.positionals.reminder) |reminder| {
            const target = reminder[0];
            if (target.len == 1 and target[0] == '-') {
                self.files[0] = .stdin;
            } else {
                self.files[0] = .{ .file = reminder[0] };
            }
        } else {
            self.files[0] = .stdin;
        }

        return self;
    }

    pub fn currentType(self: *const @This()) FileType {
        return self.files[self.idx];
    }

    pub fn next(self: *@This()) OpenError!?std.fs.File {
        std.debug.assert(self.current == null);
        if (self.idx >= self.files.len) return null;
        const fType = self.files[self.idx];

        self.current = switch (fType) {
            .file => |fileArg| try open(fileArg),
            .stdin => std.fs.File.stdin(),
        };

        return self.current;
    }

    pub fn close(self: *@This()) void {
        std.debug.assert(self.current != null);
        self.current.?.close();
        self.current = null;
        self.idx += 1;
    }
};

pub const RunError = error{} ||
    regex.CompileError ||
    regex.Regex.MatchError ||
    OpenError ||
    fs.DetectTypeError ||
    source.MmapBufferError ||
    source.ReadEvent.Error ||
    sink.Sink.ConsumeError ||
    std.Io.tty.Config.SetColorError ||
    std.Io.Reader.DelimiterError ||
    std.Io.Writer.Error;

pub fn run(argsRes: *const ArgsRes) RunError!void {
    const matchPattern = argsRes.positionals.tuple.@"0";
    var rgx = try regex.compile(matchPattern, reporter);
    defer rgx.deinit();

    var fileCursor = FileCursor.init(argsRes);
    // TODO: iterate over folders and more files
    const file = try fileCursor.next() orelse return;
    defer fileCursor.close();

    const inputFileType = try fs.detectType(file);
    const sourceBuffer = source.pickSourceBuffer(inputFileType);
    var fSource: source.Source = .{
        .sourceReader = rv: {
            switch (sourceBuffer) {
                .growingDoubleBuffer => |config| {
                    const buff = try std.heap.page_allocator.alloc(u8, config.readBuffer);
                    var fsReader = std.fs.File.stdin().reader(buff);
                    var writer = try std.Io.Writer.Allocating.initCapacity(
                        std.heap.page_allocator,
                        config.targetInitialSize,
                    );

                    var growing: source.GrowingDoubleBufferSource = .{
                        .allocator = std.heap.page_allocator,
                        .rBuff = buff,
                        .reader = &fsReader.interface,
                        .growingWriter = &writer,
                    };
                    break :rv .{ .growingDoubleBuffer = &growing };
                },
                .mmap => {
                    const buff: []align(std.heap.page_size_min) u8 = try source.mmapBuffer(file);
                    var reader = std.Io.Reader.fixed(buff);
                    var mmapSource: source.MmapSource = .{
                        .buffer = buff,
                        .reader = &reader,
                    };
                    break :rv .{ .mmap = &mmapSource };
                },
            }
        },
    };
    defer fSource.deinit();

    var stdout = std.fs.File.stdout();
    const fileType = try fs.detectType(stdout);
    const eventHandler = sink.pickEventHandler(fileType, stdout, true);
    const sinkBuffer = sink.pickSinkBuffer(fileType, eventHandler);

    var fSink: sink.Sink = .{
        .fileType = fileType,
        .eventHandler = eventHandler,
        .sinkWriter = rv: {
            switch (sinkBuffer) {
                .heapGrowing => |size| {
                    var allocating = try std.Io.Writer.Allocating.initCapacity(std.heap.page_allocator, size);
                    var fdWriter = stdout.writer(&.{});
                    var heapGrowing: sink.HeapGrowingWriter = .{
                        .allocating = &allocating,
                        .fdWriter = &fdWriter,
                    };
                    break :rv .{ .heapGrowing = &heapGrowing };
                },
                .buffered => |size| {
                    const buff = try std.heap.page_allocator.alloc(u8, size);
                    var writer = stdout.writer(buff);
                    var buffOwnedWriter: sink.BufferOwnedWriter = .{
                        .allocator = std.heap.page_allocator,
                        .buff = buff,
                        .writer = &writer.interface,
                    };
                    break :rv .{ .buffered = &buffOwnedWriter };
                },
                .directWrite => {
                    var writer = stdout.writer(&.{});
                    break :rv .{ .directWrite = &writer.interface };
                },
            }
        },
    };
    defer fSink.deinit();

    if (argsRes.options.@"line-by-line") {
        // TODO: checks for ovect[0] > ovect[1]
        // TODO: checks for empty
        // TODO: check for \K

        while (true) {
            const readEvent = try fSource.nextLine();
            switch (readEvent) {
                .endOfFile => {
                    _ = try fSink.consume(.endOfFile);
                    break;
                },
                // I could short circuit the last nextLine run here
                // but it's cleaner if I do one over and check the last event
                .endOfFileChunk,
                .line,
                => |line| {
                    var start: usize = 0;

                    while (start < line.len) {
                        const matchData = try rgx.offsetMatch(line, start) orelse {
                            const eol = line[start..line.len];
                            if (start != 0) {
                                _ = try fSink.consume(.{ .noMatchEndOfLineAfterMatch = eol });
                            } else {
                                _ = try fSink.consume(.{ .noMatchEndOfLine = eol });
                            }
                            break;
                        };

                        const group0 = matchData.group(0);
                        if (start != group0.start) {
                            _ = try fSink.consume(.{ .beforeMatch = line[start..group0.start] });
                        }

                        const res = try fSink.consume(.{
                            .match = .{
                                .data = group0.slice(line),
                                .line = line,
                            },
                        });
                        switch (res) {
                            .lineConsumed => break,
                            .eventSkipped, .eventConsumed => {
                                start = group0.end;
                            },
                        }
                    }

                    _ = try fSink.consume(.endOfLine);
                },
            }
        }
    }

    return;
}
