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
const FileCursor = fs.FileCursor;
const source = @import("seele/source.zig");
const mem = @import("seele/mem.zig");
const Context = @import("seele/context.zig");
const DebugAlloc = std.heap.DebugAllocator(.{
    .safety = true,
});

pub fn main() !u8 {
    var ctx: Context = undefined;
    Context.instance = &ctx;

    const rlimit = try std.posix.getrlimit(.STACK);
    const currMb = rlimit.cur / units.ByteUnit.mb;
    const TargetMaxT = u6;

    if (currMb <= std.math.maxInt(TargetMaxT) and currMb > 5) {
        switch (@as(u6, @intCast(currMb))) {
            0...5 => unreachable,
            inline else => |targetMb| {
                const target: usize = @intCast(targetMb - 3);
                const size = target / 2 * units.ByteUnit.mb;

                return @intFromEnum(try stackBufferTrampoline(size));
            },
        }
    } else {
        defer Context.instance.deinit();

        // No stack-backed allocators in this case
        const heapAlloc = rv: {
            if (builtin.mode == .Debug) {
                var debugAlloc = DebugAlloc.init;
                break :rv debugAlloc.allocator();
            } else {
                break :rv std.heap.page_allocator;
            }
        };

        // After this we are left with 512kb stack mem in this scrap
        // it's still backed by page
        const stderrWBuff = try heapAlloc.alloc(
            u8,
            512 * units.ByteUnit.kb,
        );

        var stderrW = std.fs.File.stderr().writerStreaming(stderrWBuff);
        Context.instance.init(
            heapAlloc,
            heapAlloc,
            heapAlloc,
            &stderrW,
        );

        return @intFromEnum(try handleArgsAndRun());
    }
}

pub fn stackBufferTrampoline(comptime size: usize) !RunReturn {
    defer Context.instance.deinit();

    const scrapAlloc = rv: {
        if (builtin.mode == .Debug) {
            var debugAlloc = DebugAlloc.init;
            break :rv debugAlloc.allocator();
        } else {
            var sfba = mem.stackFallback(
                units.ByteUnit.mb,
                std.heap.page_allocator,
            );
            break :rv sfba.get();
        }
    };
    const inAlloc = rv: {
        if (builtin.mode == .Debug) {
            var debugAlloc = DebugAlloc.init;
            break :rv debugAlloc.allocator();
        } else {
            var sfba = mem.stackFallback(
                size,
                std.heap.page_allocator,
            );
            break :rv sfba.get();
        }
    };
    const outAlloc = rv: {
        if (builtin.mode == .Debug) {
            var debugAlloc = DebugAlloc.init;
            break :rv debugAlloc.allocator();
        } else {
            var sfba = mem.stackFallback(
                size,
                std.heap.page_allocator,
            );
            break :rv sfba.get();
        }
    };

    // After this we are left with 512kb stack mem in this scrap
    // it's still backed by page
    const stderrWBuff = try scrapAlloc.alloc(
        u8,
        512 * units.ByteUnit.kb,
    );

    var stderrW = std.fs.File.stderr().writerStreaming(stderrWBuff);
    Context.instance.init(
        scrapAlloc,
        inAlloc,
        outAlloc,
        &stderrW,
    );

    return try handleArgsAndRun();
}

pub fn handleArgsAndRun() !RunReturn {
    var argsRes: args.ArgsRes = .init(Context.instance.scrapAlloc);
    defer if (builtin.mode == .Debug) argsRes.deinit();

    if (argsRes.parseArgs()) |err| {
        if (err.message) |message| {
            try Context.instance.stderrW.print("Last opt <{?s}>, Last token <{?s}>. ", .{
                err.lastOpt,
                err.lastToken,
            });
            try Context.instance.stderrW.writeAll(message);
            return .badArgs;
        }
    }

    if (argsRes.verb == null) argsRes.verb = .{
        // NOTE: this is cleared in the argsRes.deinit
        .match = .initEmpty(Context.instance.scrapAlloc),
    };

    return switch (@intFromBool(argsRes.options.trace)) {
        0 => try run(.release, &argsRes),
        1 => try run(.trace, &argsRes),
    };
}

// TODO: review this
pub const RunError = error{
    BadInputFile,
    UnsupportedMatchMode,
} ||
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

const MatchStats = struct {
    matchCount: usize = 0,
    lineN: usize = 0,
};

const RunReturn = enum(u2) {
    ok = 0,
    matchNotFound = 1,
    badArgs = 2,
};

pub fn run(comptime mode: sink.Mode, argsRes: *const args.ArgsRes) RunError!RunReturn {
    const matchPattern = argsRes.positionals.tuple.@"0";
    const result = try regex.compile(
        Context.instance.scrapAlloc,
        matchPattern,
        argsRes.options.compileOptions(),
    );

    var rgx = switch (result) {
        .Ok => |rgx| rgx,
        .Err => |rgxErr| {
            try Context.instance.stderrW.print(
                "{s} [{d}]: {s}",
                .{ @errorName(rgxErr.err), rgxErr.code, rgxErr.message },
            );
            if (builtin.mode == .Debug) rgxErr.deinit(Context.instance.scrapAlloc);

            try rgxErr.throw();
            unreachable;
        },
    };

    defer rgx.deinit();

    var fileCursor = FileCursor.init(argsRes);
    // TODO: iterate over folders and more files
    const inputFile = try fileCursor.next() orelse return RunError.BadInputFile;
    defer fileCursor.close();

    const input = try fs.DetailedFile.from(inputFile);
    var fSource = try source.Source.init(
        &input,
        Context.instance.inAlloc,
    );
    defer fSource.deinit(Context.instance.inAlloc);

    const stdout = try fs.DetailedFile.from(std.fs.File.stdout());
    var fSink = try sink.Sink.init(
        &stdout,
        Context.instance.outAlloc,
        argsRes,
    );
    defer fSink.deinit(Context.instance.outAlloc);

    // TODO: add multiline
    if (!argsRes.options.@"line-by-line") return RunError.UnsupportedMatchMode;

    // TODO:
    // checks for ovect[0] > ovect[1]
    // This happens when the match is partial
    // confirm empty first match is not an issue
    // check for \K
    // \K handling needs partial and other types of handling for anchoring

    // TODO: move cursors to event
    var stats: MatchStats = .{};
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
                stats.lineN += 1;
                var lineCursor = LineMatchCursor.init(line);

                lineLoop: while (lineCursor.hasReminder()) {
                    // NOTE:
                    // This idea was taken from grep, needs testing but it does improve
                    // performance
                    lineCursor.skipBadUTF8();

                    // TODO: handle bad utf8 as cropping
                    var groupCursor = try lineCursor.matchNext(
                        rgx,
                        fSink.eventHandler,
                        argsRes,
                    ) orelse {
                        if (lineCursor.hadMatches)
                            _ = try fSink.consume(mode, .{
                                .afterMatch = .{
                                    .line = lineCursor.reminder(),
                                    .lineN = stats.lineN,
                                },
                            })
                        else
                            _ = try fSink.consume(mode, .{
                                .noMatch = .{
                                    .line = lineCursor.reminder(),
                                    .lineN = stats.lineN,
                                },
                            });
                        lineCursor.finish();
                        continue :lineLoop;
                    };
                    stats.matchCount += 1;

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
                                    .lineN = stats.lineN,
                                    .n = group.n,
                                },
                            });
                            switch (res) {
                                .consumed => {
                                    groupCursor.moveTo(group.start);
                                },
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
                                    .lineN = stats.lineN,
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
                                    .lineN = stats.lineN,
                                    .group = group,
                                    .hasMore = groupCursor.hasMoreIncluded(),
                                },
                            })
                        else
                            try fSink.consume(mode, .{
                                .groupMatch = .{
                                    .line = line,
                                    .lineN = stats.lineN,
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
                            // we are always +1
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

    if (stats.matchCount == 0) return .matchNotFound;
    return .ok;
}

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
        assert(to <= self.line.len);
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

    pub fn skipBadUTF8(self: *@This()) void {
        var offset: usize = self.lineToken;
        while (offset < self.line.len) {
            const codePointLen: u3 = switch (self.line[offset]) {
                0b0000_0000...0b0111_1111 => break,
                0b1100_0000...0b1101_1111 => 2,
                0b1110_0000...0b1110_1111 => 3,
                0b1111_0000...0b1111_0111 => 4,
                else => {
                    offset += 1;
                    continue;
                },
            };
            switch (codePointLen) {
                1 => unreachable,
                inline else => |end| {
                    if (std.unicode.utf8ValidateSlice(self.line[offset .. offset + end]))
                        break
                    else {
                        offset += end;
                        continue;
                    }
                },
            }
        }
        self.forwardTo(offset);
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
