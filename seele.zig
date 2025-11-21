const std = @import("std");
const assert = std.debug.assert;
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

    inline for (&.{ true, false }) |v| {
        if (v == result.options.trace and v)
            return try pickStackAndRun(&result, .trace)
        else
            return try pickStackAndRun(&result, .release);
    }

    unreachable;
}

pub inline fn pickStackAndRun(argsRes: *const args.ArgsRes, comptime mode: sink.Mode) !u8 {
    const rlimit = try std.posix.getrlimit(.STACK);
    inline for (@typeInfo(@TypeOf(limits.StackSizes)).@"struct".fields) |field| {
        const fValue = field.defaultValue().?;
        if (rlimit.cur == fValue) {
            const fValueMb = fValue / units.ByteUnit.mb;
            const splitSize = ((fValueMb - (fValueMb / 8) * 2) / 2) * units.ByteUnit.mb;

            run(splitSize, argsRes, mode) catch |e| switch (e) {
                regex.CompileError.BadRegex => return 1,
                else => return e,
            };
            break;
        }
    } else {
        run(units.ByteUnit.mb * 3, argsRes, mode) catch |e| switch (e) {
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

    // TODO: handle other fd-types by actually querying them
    pub fn open(fileArg: []const u8) OpenError!std.fs.File {
        const filePath = if (std.fs.path.isAbsolute(fileArg))
            fileArg
        else rv: {
            var buff: [4098]u8 = undefined;
            break :rv try std.fs.cwd().realpath(fileArg, &buff);
        };
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

pub fn run(comptime stackPartitionSize: usize, argsRes: *const args.ArgsRes, comptime mode: sink.Mode) RunError!void {
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

    // TODO: add multiline
    if (!argsRes.options.@"line-by-line") return;

    // TODO:
    // checks for ovect[0] > ovect[1]
    // This happens when the match is partial
    // confirm empty first match is not an issue
    // check for \K
    // \K handling needs partial and other types of handling for anchoring

    // TODO: move cursors to event
    fileLoop: while (true) {
        const readEvent = try fSource.nextLine();
        switch (readEvent) {
            .eof => {
                _ = try fSink.consume(mode, .eof);
                break :fileLoop;
            },
            .eofChunk,
            .line,
            => |line| {
                var lineCursor = LineMatchCursor.init(line);

                lineLoop: while (lineCursor.hasReminder()) {
                    var groupCursor = try lineCursor.matchNext(
                        rgx,
                        fSink.eventHandler,
                        argsRes,
                    ) orelse {
                        if (lineCursor.hadMatches)
                            _ = try fSink.consume(mode, .{
                                .afterMatch = lineCursor.reminder(),
                            })
                        else
                            _ = try fSink.consume(mode, .{
                                .noMatch = lineCursor.reminder(),
                            });
                        lineCursor.finish();
                        continue :lineLoop;
                    };

                    groupLoop: while (groupCursor.hasNextGroup()) {
                        const group = groupCursor.peekGroup() catch |e| switch (e) {
                            // NOTE: group was internally skipped by the regex engine
                            // it's declared but not in the path
                            regex.RegexMatch.GroupError.GroupSkipped => {
                                groupCursor.skipGroup();
                                continue :groupLoop;
                            },
                            else => return e,
                        };

                        if (group.start < groupCursor.lineToken) {
                            groupCursor.skipGroup();
                            continue :groupLoop;
                        }

                        if (groupCursor.beforeMatch()) |slice| {
                            const res = try fSink.consume(mode, .{
                                .beforeGroup = .{
                                    .slice = slice,
                                    .n = group.n,
                                },
                            });
                            switch (res) {
                                .consumedLine => {
                                    lineCursor.finish();
                                    continue :lineLoop;
                                },
                                else => {},
                            }
                        }

                        if (groupCursor.isGroupExcluded()) {
                            const res = try fSink.consume(mode, .{
                                .excludedGroup = .{
                                    .group = group,
                                    .line = line,
                                    .hasMore = groupCursor.hasMoreIncluded(),
                                },
                            });
                            switch (res) {
                                .cropLine,
                                => {
                                    groupCursor.skipGroup();
                                    groupCursor.moveTo(group.start);
                                    groupCursor.cropLine(group.end);
                                    continue :groupLoop;
                                },
                                .skipped,
                                => {
                                    groupCursor.skipGroup();
                                    continue :groupLoop;
                                },
                                .consumed,
                                => {
                                    groupCursor.consumeGroup();
                                    continue :groupLoop;
                                },
                                .consumedLine,
                                => {
                                    lineCursor.finish();
                                    continue :lineLoop;
                                },
                            }
                        }

                        const res = if (groupCursor.isEmptyGroup())
                            try fSink.consume(mode, .{
                                .emptyGroup = .{
                                    .line = line,
                                    .group = group,
                                    .hasMore = groupCursor.hasMoreIncluded(),
                                },
                            })
                        else
                            try fSink.consume(mode, .{
                                .groupMatch = .{
                                    .line = line,
                                    .group = group,
                                    .hasMore = groupCursor.hasMoreIncluded(),
                                },
                            });
                        switch (res) {
                            .consumedLine => {
                                lineCursor.finish();
                                continue :lineLoop;
                            },
                            .skipped,
                            .consumed,
                            => {
                                if (groupCursor.isEmptyGroup() and
                                    (groupCursor.currentIsMax() or groupCursor.currentIsZero()))
                                {
                                    groupCursor.consumeGroup();
                                    groupCursor.forwardOne();
                                } else groupCursor.consumeGroup();

                                if (group.n == 0)
                                    break :groupLoop;
                            },
                            .cropLine => unreachable,
                        }
                    }

                    const reminderOpt = groupCursor.reminder();
                    _ = try fSink.consume(mode, .{
                        .endOfGroups = .{
                            .sliceOpt = reminderOpt,
                            // we are either always +1
                            .hadBreakline = groupCursor.relativeToken(-1) == '\n',
                            .group0Empty = groupCursor.isGroup0Empty(),
                        },
                    });
                    if (reminderOpt != null)
                        groupCursor.finishGroup0();

                    lineCursor.forwardTo(groupCursor.lineToken);
                }

                var lastOpt: ?u8 = null;
                if (!lineCursor.hasBreakline()) lastOpt = '\n';

                _ = try fSink.consume(mode, .{
                    .eol = .{
                        .charOpt = lastOpt,
                        .hadMatches = lineCursor.hadMatches,
                    },
                });

                // NOTE: this avoids one extra syscall for read
                if (readEvent == .eofChunk) {
                    _ = try fSink.consume(mode, .eof);
                    break :fileLoop;
                }
            },
        }
    }
}

// TODO: use pointers
pub const GroupMatchCursor = struct {
    current: u16,
    count: u16,
    match: regex.RegexMatch,
    group0: ?regex.RegexMatchGroup,
    currGroup: ?regex.RegexMatchGroup,
    targetGroup: args.TargetGroup,
    line: []const u8,
    lineToken: usize,

    pub fn init(
        line: []const u8,
        lineToken: usize,
        match: regex.RegexMatch,
        eventHandlerT: sink.EventHanderT,
        argsRes: *const args.ArgsRes,
    ) args.Args.TargetGroupError!@This() {
        return .{
            .current = 0,
            .count = match.count,
            .match = match,
            .group0 = null,
            .currGroup = null,
            .targetGroup = try argsRes.options.targetGroup(
                eventHandlerT,
                match.count,
            ),
            .line = line,
            .lineToken = lineToken,
        };
    }

    pub fn peekGroup(self: *@This()) regex.RegexMatch.GroupError!regex.RegexMatchGroup {
        // Out of bound group is treated as an error
        if (self.currGroup) |group| return group;
        const group = try self.match.group(self.current);
        if (group.n == 0) self.group0 = group;
        self.currGroup = group;
        return group;
    }

    pub const CurrentError = error{
        GroupNotFound,
    };

    pub fn currentGroup(self: *const @This()) CurrentError!regex.RegexMatchGroup {
        if (self.currGroup) |group| return group;
        return CurrentError.GroupNotFound;
    }

    pub fn maxGroup(self: *const @This()) u16 {
        return self.count - 1;
    }

    pub fn currentIsMax(self: *const @This()) bool {
        assert(self.currGroup != null);
        return self.currGroup.?.n == self.maxGroup();
    }

    pub fn currentIsZero(self: *const @This()) bool {
        assert(self.currGroup != null);
        return self.currGroup.?.n == 0;
    }

    pub fn forwardOne(self: *@This()) void {
        assert(self.lineToken < self.line.len);
        self.lineToken += 1;
    }

    pub fn moveTo(self: *@This(), at: usize) void {
        assert(at < self.line.len);
        self.lineToken = at;
    }

    pub fn cropLine(self: *@This(), to: usize) void {
        assert(to < self.line.len);
        assert(self.lineToken <= to);
        self.line = self.line[0..to];
    }

    pub fn finishGroup0(self: *@This()) void {
        assert(self.group0 != null);
        self.lineToken = self.group0.?.end;
    }

    pub fn skipGroup(self: *@This()) void {
        assert(self.currGroup != null);
        self.currGroup = null;
        self.current += 1;
    }

    pub fn isEmptyGroup(self: *const @This()) bool {
        assert(self.currGroup != null);
        return self.currGroup.?.start == self.currGroup.?.end;
    }

    pub fn isGroup0Empty(self: *const @This()) bool {
        assert(self.group0 != null);
        return self.group0.?.start == self.group0.?.end;
    }

    pub fn relativeToken(self: *const @This(), comptime relativeTo: i128) u8 {
        assert(relativeTo < self.lineToken);
        const i: usize = if (self.lineToken > 0)
            @intCast(self.lineToken + relativeTo)
        else
            0;
        return self.line[i];
    }

    pub fn consumeGroup(self: *@This()) void {
        assert(self.currGroup != null);
        assert(self.currGroup.?.end >= self.lineToken);
        self.lineToken = self.currGroup.?.end;
        self.skipGroup();
    }

    pub fn hasMoreIncluded(self: *const @This()) bool {
        assert(self.currGroup != null);
        const group = self.currGroup.?;
        return self.targetGroup.anyGreaterThan(group.n, self.maxGroup());
    }

    pub fn isGroupExcluded(self: *const @This()) bool {
        assert(self.currGroup != null);
        return !self.targetGroup.includes(self.current);
    }

    pub fn beforeMatch(self: *const @This()) ?[]const u8 {
        assert(self.currGroup != null);
        const group = self.currGroup.?;

        if (self.lineToken >= group.start) return null;

        return self.line[self.lineToken..group.start];
    }

    pub fn reminder(self: *const @This()) ?[]const u8 {
        if (self.lineToken == self.line.len) return null;

        assert(self.group0 != null);
        const group0 = self.group0.?;
        if (group0.start == group0.end) return null;
        if (self.lineToken == group0.end) return null;

        return self.line[self.lineToken..group0.end];
    }

    pub fn matchEnd(self: *const @This()) usize {
        assert(self.group0 != null);
        return self.group0.?.end;
    }

    pub fn hasNextGroup(self: *const @This()) bool {
        return self.current < self.count;
    }
};

pub const LineMatchCursor = struct {
    line: []const u8,
    lineToken: usize = 0,
    hadMatches: bool = false,

    pub fn init(line: []const u8) @This() {
        return .{ .line = line };
    }

    pub const ErrorNext = regex.Regex.MatchError || args.Args.TargetGroupError;

    pub fn matchNext(
        self: *@This(),
        rgx: regex.Regex,
        eventHandlerT: sink.EventHanderT,
        argsRes: *const args.ArgsRes,
    ) ErrorNext!?GroupMatchCursor {
        assert(self.hasReminder());
        if (try rgx.offsetMatch(self.line, self.lineToken)) |match| {
            self.hadMatches = true;
            return try .init(
                self.line,
                self.lineToken,
                match,
                eventHandlerT,
                argsRes,
            );
        }
        return null;
    }

    pub fn hasBreakline(self: *const @This()) bool {
        assert(self.line.len > 0);
        return self.line[self.line.len - 1] == '\n';
    }

    pub fn finish(self: *@This()) void {
        self.forwardTo(self.line.len);
    }

    pub fn forwardTo(self: *@This(), i: usize) void {
        assert(i <= self.line.len);
        self.lineToken = i;
    }

    pub fn reminder(self: *const @This()) []const u8 {
        assert(self.hasReminder());
        return self.line[self.lineToken..];
    }

    pub fn hasReminder(self: *const @This()) bool {
        return self.lineToken < self.line.len;
    }
};

comptime {
    _ = mem;
    _ = @import("seele/tty.zig");
}
