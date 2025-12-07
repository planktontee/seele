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
            null,
            heapAlloc,
            null,
            heapAlloc,
            null,
            heapAlloc,
            &stderrW,
        );

        return @intFromEnum(try handleArgsAndRun());
    }
}

pub fn stackBufferTrampoline(comptime size: usize) !RunReturn {
    defer Context.instance.deinit();

    const scrapSize = units.ByteUnit.mb;
    const scrapAlloc = rv: {
        if (builtin.mode == .Debug) {
            var debugAlloc = DebugAlloc.init;
            break :rv debugAlloc.allocator();
        } else {
            var sfba = mem.stackFallback(
                scrapSize,
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
        scrapSize,
        scrapAlloc,
        size,
        inAlloc,
        size,
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
    fs.FileCursor.OpenError ||
    source.SourceReader.InitError ||
    source.ReadEvent.Error ||
    sink.Sink.ConsumeError ||
    std.Io.Reader.DelimiterError ||
    std.Io.Writer.Error;

const MatchData = struct {
    matchCount: usize,
    lineN: usize,
    max: usize,
    fDetailed: *const fs.DetailedFile,

    pub fn init(
        max: usize,
        fDetailed: *const fs.DetailedFile,
    ) @This() {
        return .{
            .matchCount = 0,
            .lineN = 0,
            .max = max,
            .fDetailed = fDetailed,
        };
    }

    pub fn restart(self: *@This(), fDetailed: *const fs.DetailedFile) void {
        self.fDetailed = fDetailed;
        self.lineN = 0;
        self.matchCount = 0;
    }

    pub fn isDone(self: @This()) bool {
        return self.max == self.matchCount;
    }
};

const RunReturn = enum(u2) {
    ok = 0,
    matchNotFound = 1,
    badArgs = 2,
};

pub fn run(comptime mode: sink.Mode, argsRes: *const args.ArgsRes) RunError!RunReturn {
    const scrapAlloactor = Context.instance.scrapAlloc;
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
            if (builtin.mode == .Debug) rgxErr.deinit(scrapAlloactor);

            try rgxErr.throw();
            unreachable;
        },
    };

    defer rgx.deinit();

    // TODO: add multiline
    if (!argsRes.options.@"line-by-line") return RunError.UnsupportedMatchMode;

    // TODO:
    // detection of binaries
    // filtering
    const isRecursive = argsRes.options.recursive;
    var fileCursor = FileCursor.init(argsRes, isRecursive);
    defer fileCursor.deinit();

    var firstMissing: bool = true;

    var fSource: source.Source = undefined;
    defer if (!firstMissing) fSource.deinit(Context.instance.inAlloc);

    const stdoutFd = std.fs.File.stdout();
    const stdout = try fs.DetailedFile.from(
        stdoutFd,
        "",
        "(stdout)",
        &(try stdoutFd.stat()),
    );
    var fSink = try sink.Sink.init(
        &stdout,
        Context.instance.outAlloc,
        argsRes,
    );
    defer fSink.deinit(Context.instance.outAlloc);

    var matchData: MatchData = undefined;
    var hadMatches = false;

    while (try fileCursor.next(scrapAlloactor)) |input| {
        defer fileCursor.closeCurrent();
        if (firstMissing) {
            const showFileName = fileCursor.hasRelativePaths();
            fSource = try source.Source.init(
                &input,
                Context.instance.inAlloc,
                isRecursive,
                argsRes.options.@"skip-binary",
            );
            firstMissing = false;
            matchData = .init(
                argsRes.options.@"match-max",
                &input,
            );
            if (showFileName and !argsRes.options.@"no-file-name")
                fSink.eventHandler.showFileName(&input);
        } else {
            try fSource.loadSource(&input);
            matchData.restart(&input);
            fSink.eventHandler.restart();
        }

        matchFile(
            mode,
            argsRes,
            rgx,
            &fSource,
            &fSink,
            &matchData,
        ) catch |e| switch (e) {
            sink.Sink.SinkWriteError.BadUTF8Encoding => {},
            else => return e,
        };

        hadMatches |= matchData.matchCount > 0;
    } else {
        if (firstMissing) return RunError.BadInputFile;
    }

    if (hadMatches) return .ok else return .matchNotFound;
}

pub fn matchFile(
    comptime mode: sink.Mode,
    argsRes: *const args.ArgsRes,
    rgx: regex.Regex,
    fSource: *source.Source,
    fSink: *sink.Sink,
    matchData: *MatchData,
) !void {
    // TODO:
    // checks for ovect[0] > ovect[1]
    // This happens when the match is partial
    // confirm empty first match is not an issue
    // check for \K
    // \K handling needs partial and other types of handling for anchoring

    var lineCursor: LineMatchCursor = undefined;
    var groupCursor: GroupMatchCursor = undefined;
    var base: sink.BaseEvent = .{
        .line = undefined,
        .lineN = matchData.lineN,
        .fDetailed = fSource.fDetailed,
    };

    fileLoop: while (true) {
        const readEvent = try fSource.nextLine();
        fileState: switch (readEvent) {
            .eof => {
                if (!fSink.eventHandler.skips(.eof))
                    _ = try fSink.consume(mode, .eof);
                break :fileLoop;
            },
            .eofChunk,
            .line,
            => |line| {
                // // TODO: make bin check optional
                if (fSource.isInvalid())
                    continue :fileState .eof;

                base.line = line;
                matchData.lineN += 1;
                base.lineN += 1;
                lineCursor.init(line);

                lineLoop: while (lineCursor.hasReminder()) {
                    if (matchData.isDone()) {
                        if (!fSink.eventHandler.skips(.eol)) {
                            var charOpt: ?u8 = null;
                            if (lineCursor.lineToken > 0) {
                                const lastChar = lineCursor.line[lineCursor.lineToken - 1];
                                if (lastChar != '\n') charOpt = '\n';
                            }

                            _ = try fSink.consume(mode, .{
                                .eol = .{
                                    .charOpt = charOpt,
                                    .hadMatches = lineCursor.hadMatches,
                                },
                            });
                        }
                        continue :fileState .eof;
                    }

                    if (argsRes.options.@"validate-utf8")
                        lineCursor.skipBadUTF8();

                    lineCursor.matchNext(
                        rgx,
                        fSink.eventHandler,
                        argsRes,
                        &groupCursor,
                    ) catch |e| switch (e) {
                        regex.Regex.MatchError.BadUTF8Encode => continue :fileState .eof,
                        regex.Regex.MatchError.NoMatch => {
                            if (lineCursor.hadMatches) {
                                if (!fSink.eventHandler.skips(.afterMatch))
                                    _ = try fSink.consume(mode, .{
                                        .afterMatch = &.{
                                            .slice = lineCursor.reminder(),
                                            .base = &base,
                                        },
                                    });
                            } else {
                                if (!fSink.eventHandler.skips(.noMatch))
                                    _ = try fSink.consume(mode, .{
                                        .noMatch = &.{
                                            .slice = lineCursor.reminder(),
                                            .base = &base,
                                        },
                                    });
                            }
                            lineCursor.finish();
                            continue :lineLoop;
                        },
                        else => return e,
                    };
                    matchData.matchCount += 1;

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

                        if (!fSink.eventHandler.skips(.beforeGroup))
                            if (groupCursor.beforeMatch()) |slice| {
                                const res = try fSink.consume(mode, .{
                                    .beforeGroup = &.{
                                        .base = &base,
                                        .slice = slice,
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
                            };

                        if (groupCursor.isGroupExcluded()) {
                            if (fSink.eventHandler.skips(.excludedGroup)) {
                                groupCursor.skipGroup();
                                continue :groupLoop;
                            }

                            const res = try fSink.consume(mode, .{
                                .excludedGroup = &.{
                                    .base = &base,
                                    .group = group,
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

                        const res: sink.Sink.ConsumeResponse = if (groupCursor.isEmptyGroup()) rv: {
                            if (fSink.eventHandler.skips(.emptyGroup)) break :rv .skipped;

                            break :rv try fSink.consume(mode, .{
                                .emptyGroup = &.{
                                    .base = &base,
                                    .group = group,
                                    .hasMore = groupCursor.hasMoreIncluded(),
                                },
                            });
                        } else rv: {
                            if (fSink.eventHandler.skips(.groupMatch)) break :rv .skipped;

                            break :rv try fSink.consume(mode, .{
                                .groupMatch = &.{
                                    .base = &base,
                                    .group = group,
                                    .hasMore = groupCursor.hasMoreIncluded(),
                                },
                            });
                        };
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
                    if (!fSink.eventHandler.skips(.endOfGroups)) {
                        _ = try fSink.consume(mode, .{
                            .endOfGroups = .{
                                .sliceOpt = reminderOpt,
                                // we are always +1
                                .hadBreakline = groupCursor.backtrack() == '\n',
                                .group0Empty = groupCursor.isGroup0Empty(),
                            },
                        });
                    }
                    if (reminderOpt != null)
                        groupCursor.finishGroup0();

                    lineCursor.forwardTo(groupCursor.lineToken);
                }

                var lastOpt: ?u8 = null;
                if (!lineCursor.hasBreakline()) lastOpt = '\n';

                if (!fSink.eventHandler.skips(.eol)) {
                    _ = try fSink.consume(mode, .{
                        .eol = .{
                            .charOpt = lastOpt,
                            .hadMatches = lineCursor.hadMatches,
                        },
                    });
                }

                // NOTE: this avoids one extra syscall for read
                if (readEvent == .eofChunk) {
                    if (!fSink.eventHandler.skips(.eol))
                        _ = try fSink.consume(mode, .eof);

                    break :fileLoop;
                }
            },
        }
    }
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
        self: *@This(),
        line: []const u8,
        lineToken: usize,
        match: regex.RegexMatch,
        eventHandlerT: sink.EventHanderT,
        argsRes: *const args.ArgsRes,
    ) args.Args.TargetGroupError!void {
        self.* = .{
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
        const group = self.match.group(self.current) catch |e| switch (e) {
            regex.RegexMatch.GroupError.GroupSkipped => {
                self.current += 1;
                return self.peekGroup();
            },
            else => return e,
        };

        if (group.n == 0) self.group0 = group;
        self.currGroup = group;
        return group;
    }

    pub const CurrentError = error{
        GroupNotFound,
    };

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

    pub fn backtrack(self: *const @This()) u8 {
        const i: usize = if (self.lineToken > 0)
            self.lineToken - 1
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
    lineToken: usize,
    hadMatches: bool,

    pub fn init(self: *@This(), line: []const u8) void {
        self.* = .{
            .line = line,
            .hadMatches = false,
            .lineToken = 0,
        };
    }

    pub const ErrorNext = regex.Regex.MatchError || args.Args.TargetGroupError;

    pub fn matchNext(
        self: *@This(),
        rgx: regex.Regex,
        eventHandlerT: sink.EventHanderT,
        argsRes: *const args.ArgsRes,
        groupCursor: *GroupMatchCursor,
    ) ErrorNext!void {
        assert(self.hasReminder());
        const match = try rgx.offsetMatch(
            self.line,
            self.lineToken,
            self.lineToken == 0,
            argsRes.options.@"validate-utf8",
        );
        self.hadMatches = true;

        try groupCursor.init(
            self.line,
            self.lineToken,
            match,
            eventHandlerT,
            argsRes,
        );
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
                    const chunkLen = offset + end;
                    if (chunkLen > self.line.len) {
                        // we skip the remaining because it's guaranteed to be busted if
                        // it doesnt match the required size
                        offset = self.line.len;
                        break;
                    }

                    const isValid = rv: {
                        _ = std.unicode.utf8Decode(self.line[offset..chunkLen]) catch break :rv false;
                        break :rv true;
                    };

                    if (isValid)
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
