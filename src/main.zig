const std = @import("std");
const builtin = @import("builtin");
const zpec = @import("zpec");
const units = zpec.units;
const args = @import("core/args.zig");
const regex = @import("core/regex.zig");
const sink = @import("core/sink.zig");
const fs = @import("core/fs.zig");
const source = @import("core/source.zig");
const mem = @import("core/mem.zig");

var reporter: *const sink.Reporter = undefined;

pub fn main() !u8 {
    reporter = rv: {
        var r: sink.Reporter = .{};
        var buffOut: [units.ByteUnit.kb * 1]u8 = undefined;
        var buffErr: [units.ByteUnit.kb * 1]u8 = undefined;

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

    var result: args.ArgsRes = .init(allocator);
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

    pub fn init(argsRes: *const args.ArgsRes) @This() {
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
    args.Args.TargetGroupError ||
    regex.CompileError ||
    regex.Regex.MatchError ||
    regex.RegexMatch.GroupError ||
    OpenError ||
    fs.DetectTypeError ||
    source.SourceReader.InitError ||
    source.ReadEvent.Error ||
    sink.Sink.ConsumeError ||
    std.Io.tty.Config.SetColorError ||
    std.Io.Reader.DelimiterError ||
    std.Io.Writer.Error;

pub fn run(argsRes: *const args.ArgsRes) RunError!void {
    // NOTE: because Fba only resizes the last piece allocated, we need to split
    // the stack mem into 2 different allocators to ensure they can grow
    // Zig issues a prlimit64 for 16mb, I'm taking 12mb here for IO, probably not a good idea and wont work on OSs that return error on that call
    const stackPartitionSize = rv: {
        if (builtin.mode == .Debug) {
            break :rv units.CacheSize.L2 * 2;
        } else {
            break :rv units.CacheSize.L2 * 6;
        }
        unreachable;
    };
    var inputSfb = mem.stackFallback(stackPartitionSize, std.heap.page_allocator);
    var outputSfb = mem.stackFallback(stackPartitionSize, std.heap.page_allocator);
    const inputAlloc = inputSfb.get();
    const outputAlloc = outputSfb.get();

    const matchPattern = argsRes.positionals.tuple.@"0";
    var rgx = try regex.compile(matchPattern, reporter);
    defer rgx.deinit();

    var fileCursor = FileCursor.init(argsRes);
    // TODO: iterate over folders and more files
    const inputFile = try fileCursor.next() orelse return;
    defer fileCursor.close();

    const input = try fs.DetailedFile.from(inputFile);
    var fSource = try source.Source.init(&input, inputAlloc);
    defer fSource.deinit(inputAlloc);

    const stdout = try fs.DetailedFile.from(std.fs.File.stdout());
    var fSink = try sink.Sink.init(
        &stdout,
        outputAlloc,
        argsRes,
    );
    defer fSink.deinit(outputAlloc);

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
                .endOfFileChunk,
                .line,
                => |line| {
                    var start: usize = 0;

                    lineFeed: while (start < line.len) {
                        const matchData = try rgx.offsetMatch(line, start) orelse {
                            const eol = line[start..line.len];
                            if (start != 0) {
                                _ = try fSink.consume(.{ .noMatchEndOfLineAfterMatch = eol });
                            } else {
                                _ = try fSink.consume(.{ .noMatchEndOfLine = eol });
                            }
                            break;
                        };

                        // NOTE: handlign group0, group0 contains everything, hightlighting will be
                        // done on top of that for groups, in case it's needed
                        const targetGroup = try argsRes.options.targetGroup(matchData.count);
                        for (0..matchData.count) |i| {
                            if (i != 0) {
                                switch (targetGroup) {
                                    inline else => |tg| if (!tg.includes(i)) continue,
                                }
                            }

                            const group = try matchData.group(i);
                            if (start > group.start) continue;

                            if (start < group.start) {
                                _ = try fSink.consume(.{ .beforeMatch = line[start..group.start] });
                            }

                            const res = try fSink.consume(.{
                                .match = .{
                                    .line = line,
                                    .group = group,
                                    .count = matchData.count,
                                    .targetGroup = targetGroup,
                                },
                            });
                            switch (res) {
                                .lineConsumed => break :lineFeed,
                                // NOTE: start is not updated in this case because group
                                // was cached to do some activities between previous and new
                                // group
                                .eventPostponed,
                                => {},
                                .eventPostponedFinished,
                                => |lastIndex| {
                                    start = lastIndex;
                                },
                                .eventSkipped,
                                .eventConsumed,
                                => {
                                    // TODO: when considering group0 for a soft-match, we cant update start like this
                                    start = group.end;
                                },
                            }
                        }
                    }

                    _ = try fSink.consume(.endOfLine);

                    // NOTE: this avoids one extra syscall for read
                    if (readEvent == .endOfFileChunk) {
                        _ = try fSink.consume(.endOfFile);
                        break;
                    }
                },
            }
        }
    }

    return;
}
