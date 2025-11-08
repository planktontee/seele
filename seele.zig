const std = @import("std");
const builtin = @import("builtin");
const zcasp = @import("zcasp");
const regent = @import("regent");
const units = regent.units;
const args = @import("seele/args.zig");
const regex = @import("seele/regex.zig");
const sink = @import("seele/sink.zig");
const fs = @import("seele/fs.zig");
const source = @import("seele/source.zig");
const mem = @import("seele/mem.zig");
const limits = @import("seele/limits.zig");

pub const std_options: std.Options = .{
    .logFn = log,
};

pub fn log(
    comptime _: std.log.Level,
    comptime _: @TypeOf(.enum_literal),
    comptime format: []const u8,
    logArgs: anytype,
) void {
    var buffer: [64]u8 = undefined;
    const stderr = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    nosuspend stderr.print(format ++ "\n", logArgs) catch return;
}

pub fn main() !u8 {
    // var sfba = std.heap.stackFallback(4098, std.heap.page_allocator);
    // const allocator = sfba.get();
    // NOTE: this is completely arbitrary and currently set to no fallback for
    // testing purposes
    var buff: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buff);
    const allocator = fba.allocator();

    var result: args.ArgsRes = .init(allocator);
    defer result.deinit();

    if (result.parseArgs()) |err| {
        if (err.message) |message| {
            std.log.err("Last opt <{?s}>, Last token <{?s}>. ", .{
                err.lastOpt,
                err.lastToken,
            });
            std.log.err("{s}", .{message});
            return 1;
        }
    }

    if (result.verb == null) result.verb = .{ .match = .initEmpty(allocator) };

    const rlimit = try std.posix.getrlimit(.STACK);
    inline for (@typeInfo(@TypeOf(limits.StackSizes)).@"struct".fields) |field| {
        const fValue = field.defaultValue().?;
        if (rlimit.cur == fValue) {
            const fValueMb = fValue / units.ByteUnit.mb;
            const splitSize = ((fValueMb - (fValueMb / 8) * 2) / 2) * units.ByteUnit.mb;

            run(splitSize, &result) catch |e| switch (e) {
                regex.CompileError.BadRegex => return 1,
                else => return e,
            };
            break;
        }
    } else {
        run(units.ByteUnit.mb * 3, &result) catch |e| switch (e) {
            regex.CompileError.BadRegex => return 1,
            else => return e,
        };
    }

    return 0;
}

pub const FileArgType = union(enum) {
    stdin,
    file: []const u8,

    pub fn name(self: *const @This()) []const u8 {
        return switch (self.*) {
            .stdin => "stdin",
            .file => |fileArg| fileArg,
        };
    }
};

pub const FileCursor = struct {
    files: [1]FileArgType = undefined,
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

    pub fn currentType(self: *const @This()) FileArgType {
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

    pub const OpenError = std.fs.File.OpenError || std.posix.RealPathError;

    pub fn open(fileArg: []const u8) OpenError!std.fs.File {
        var buff: [4098]u8 = undefined;
        const filePath = try std.fs.cwd().realpath(fileArg, &buff);
        return try std.fs.openFileAbsolute(filePath, .{ .mode = .read_only });
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
    FileCursor.OpenError ||
    fs.DetectTypeError ||
    source.SourceReader.InitError ||
    source.ReadEvent.Error ||
    sink.Sink.ConsumeError ||
    std.Io.Reader.DelimiterError ||
    std.Io.Writer.Error;

pub fn run(comptime stackPartitionSize: usize, argsRes: *const args.ArgsRes) RunError!void {
    // NOTE: because Fba only resizes the last piece allocated, we need to split
    // the stack mem into 2 different allocators to ensure they can grow
    // Zig issues a prlimit64 for 16mb, I'm taking 12mb here for IO, probably not a good idea and wont work on OSs that return error on that call
    // For debug builds, bigger stacks were seemingly being consumed elsewhere, so I reduced
    // it to 4mb, completely arbitrarily, I will leave this here for now but I'm moving to a debug
    // allocator for Debug build instead

    const DebugAlloc = std.heap.DebugAllocator(.{
        .safety = true,
    });

    // TODO: move this to top-level
    const inputAlloc = rv: {
        if (builtin.mode == .Debug) {
            var dba = DebugAlloc.init;
            break :rv dba.allocator();
        } else {
            var inputSfb = mem.stackFallback(stackPartitionSize, std.heap.page_allocator);
            break :rv inputSfb.get();
        }
    };
    const outputAlloc = rv: {
        if (builtin.mode == .Debug) {
            var dba = DebugAlloc.init;
            break :rv dba.allocator();
        } else {
            var outputSfb = mem.stackFallback(stackPartitionSize, std.heap.page_allocator);
            break :rv outputSfb.get();
        }
    };
    defer {
        if (builtin.mode == .Debug) {
            const inDba: *DebugAlloc = @ptrCast(@alignCast(inputAlloc.ptr));
            const outDba: *DebugAlloc = @ptrCast(@alignCast(outputAlloc.ptr));
            if (inDba.deinit() == .leak) @panic("Input memory leaked!");
            if (outDba.deinit() == .leak) @panic("Output memory leaked!");
        }
    }

    const matchPattern = argsRes.positionals.tuple.@"0";
    const result = try regex.compile(inputAlloc, matchPattern);

    var rgx = switch (result) {
        .Ok => |rgx| rgx,
        .Err => |rgxErr| {
            std.log.err(
                "{s} [{d}]: {s}",
                .{ @errorName(rgxErr.err), rgxErr.code, rgxErr.message },
            );
            rgxErr.deinit(inputAlloc);

            try rgxErr.throw();
            unreachable;
        },
    };

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
        // This happens when the match is partial

        // TODO: confirm empty first match is not an issue

        // TODO: check for \K
        // \K handling needs partial and other types of handling for anchoring

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
                        const targetGroup = try argsRes.options.targetGroup(&fSink, matchData.count);
                        for (0..matchData.count) |i| {
                            const group = matchData.group(i) catch |e| switch (e) {
                                regex.RegexMatch.GroupError.GroupSkipped => continue,
                                else => return e,
                            };
                            switch (targetGroup) {
                                inline else => |tg| if (!tg.includes(i)) {
                                    _ = try fSink.consume(.{
                                        .excludedMatch = .{
                                            .group = group,
                                            .line = line,
                                        },
                                    });
                                    continue;
                                },
                            }

                            if (start > group.start) continue;

                            if (start < group.start) {
                                _ = try fSink.consume(.{ .beforeMatch = line[start..group.start] });
                            }

                            const res = if (group.start == group.end) try fSink.consume(.{
                                .emptyMatch = .{
                                    .line = line,
                                    .group = group,
                                    .count = matchData.count,
                                    .targetGroup = targetGroup,
                                },
                            }) else try fSink.consume(.{
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
                                .eventForward,
                                => |lastIndex| start = lastIndex,
                                .eventSkipped,
                                .eventConsumed,
                                => start = group.end,
                            }
                        }

                        _ = try fSink.consume(.endOfMatchGroups);
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

comptime {
    _ = mem;
    _ = @import("seele/tty.zig");
}
