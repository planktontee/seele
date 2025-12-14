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
        Context.usingStack = true;
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

    Context.argsRes = &argsRes;

    return switch (@intFromBool(argsRes.options.trace)) {
        0 => try run(.release),
        1 => try run(.trace),
    };
}

// TODO: review this
pub const RunError = error{
    BadInputFile,
    UnsupportedMatchMode,
} ||
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
    GroupMatchCursor.LoadGroupError ||
    std.Io.Writer.Error;

const RunReturn = enum(u2) {
    ok = 0,
    matchNotFound = 1,
    badArgs = 2,
};

pub fn run(comptime mode: sink.Mode) RunError!RunReturn {
    const argsRes = Context.argsRes;

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

    try Context.sinkp.init(
        &stdout,
        Context.instance.outAlloc,
        argsRes,
    );
    defer Context.sinkp.deinit(Context.instance.outAlloc);
    const fSink = &Context.sinkp;

    const scrapAlloactor = Context.instance.scrapAlloc;
    const matchPattern = argsRes.positionals.tuple.@"0";
    const result = try regex.compile(
        Context.instance.scrapAlloc,
        matchPattern,
        argsRes.options.compileOptions(fSink.eventHandler),
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
            if (showFileName and !argsRes.options.@"no-file-name")
                fSink.eventHandler.showFileName(&input);
        } else {
            try fSource.loadSource(&input);
            fSink.eventHandler.restart();
        }

        switch (@as(sink.EventHanderT, fSink.eventHandler)) {
            inline else => |eventT| {
                switch (@intFromBool(argsRes.options.@"match-max" == std.math.maxInt(@TypeOf(argsRes.options.@"match-max")))) {
                    inline else => |n| {
                        switch (fSource.sourceReader) {
                            inline else => |src| {
                                const matchT = @as(MatchFlavour, @enumFromInt(n));
                                const mCount = matchFile(
                                    @FieldType(sink.EventHandler, @tagName(eventT)),
                                    mode,
                                    matchT,
                                    rgx,
                                    src,
                                    &input,
                                ) catch |e| switch (e) {
                                    sink.Sink.SinkWriteError.BadUTF8Encoding => 0,
                                    else => return e,
                                };

                                hadMatches |= mCount > 0;
                            },
                        }
                    },
                }
            },
        }
    } else {
        if (firstMissing) return RunError.BadInputFile;
    }

    if (hadMatches) return .ok else return .matchNotFound;
}

const MatchFlavour = enum {
    limited,
    unlimited,
};

pub fn matchFile(
    comptime EventT: type,
    comptime mode: sink.Mode,
    comptime matchT: MatchFlavour,
    rgx: regex.Regex,
    fSource: anytype,
    fDetailed: *const fs.DetailedFile,
) !usize {
    // TODO:
    // checks for ovect[0] > ovect[1]
    // This happens when the match is partial
    // confirm empty first match is not an issue
    // check for \K
    // \K handling needs partial and other types of handling for anchoring

    const argsRes = Context.argsRes;
    var matches: usize = 0;

    const fSink = &Context.sinkp;
    var lineCursor: LineMatchCursor = undefined;
    var groupCursor: GroupMatchCursor = undefined;
    Context.baseEvent = .{
        .line = undefined,
        .lineN = 0,
        .fDetailed = fDetailed,
    };

    fileLoop: while (true) {
        const readEvent = try fSource.nextLine();
        fileState: switch (readEvent) {
            .eof => {
                if (comptime EventT.uses(.eof)) {
                    Context.event = .eof;
                    _ = try fSink.consume(mode, EventT);
                }
                break :fileLoop;
            },
            .line,
            => |line| {
                if (fSource.isInvalid()) {
                    @branchHint(.unlikely);
                    continue :fileState .eof;
                }

                Context.baseEvent.line = line;
                Context.baseEvent.lineN += 1;
                lineCursor.init();

                lineLoop: while (lineCursor.hasReminder()) {
                    if (comptime matchT == .limited) {
                        if (matches >= argsRes.options.@"match-max") {
                            // This check basically costs nothing
                            if (comptime EventT.uses(.eol)) {
                                var charOpt: ?u8 = null;
                                if (lineCursor.lineToken > 0) {
                                    const lastChar = lineCursor.line[lineCursor.lineToken - 1];
                                    if (lastChar != '\n') charOpt = '\n';
                                }

                                Context.event = .{
                                    .eol = .{
                                        .charOpt = charOpt,
                                        .hadMatches = lineCursor.hadMatches,
                                    },
                                };
                                _ = try fSink.consume(mode, EventT);
                            }
                            continue :fileState .eof;
                        }
                    }

                    if (argsRes.options.@"validate-utf8")
                        lineCursor.fastBadUTF8Skip();

                    lineCursor.matchNext(
                        rgx,
                        &groupCursor,
                    ) catch |e| switch (e) {
                        regex.Regex.MatchError.BadUTF8Encode => {
                            @branchHint(.unlikely);
                            continue :fileState .eof;
                        },
                        regex.Regex.MatchError.NoMatch => {
                            @branchHint(.unpredictable);
                            if (lineCursor.hadMatches) {
                                if (comptime EventT.uses(.afterMatch)) {
                                    Context.event = .{
                                        .afterMatch = .{
                                            .slice = lineCursor.reminder(),
                                        },
                                    };
                                    _ = try fSink.consume(mode, EventT);
                                }
                            } else {
                                if (comptime EventT.uses(.noMatch)) {
                                    Context.event = .{
                                        .noMatch = .{
                                            .slice = lineCursor.reminder(),
                                        },
                                    };
                                    _ = try fSink.consume(mode, EventT);
                                }
                            }
                            lineCursor.finish();
                            continue :lineLoop;
                        },
                        else => {
                            @branchHint(.cold);
                            return e;
                        },
                    };
                    matches += 1;

                    groupLoop: while (groupCursor.hasNextGroup()) {
                        groupCursor.loadGroup() catch |e| switch (e) {
                            // NOTE: group was internally skipped by the regex engine
                            // it's declared but not in the path
                            regex.RegexMatch.GroupError.GroupSkipped => {
                                groupCursor.skipGroup();
                                continue :groupLoop;
                            },
                            else => {
                                @branchHint(.cold);
                                return e;
                            },
                        };
                        const group = &groupCursor.currGroup;

                        if (groupCursor.startsAfterTracker()) {
                            groupCursor.skipGroup();
                            continue :groupLoop;
                        }

                        if (comptime EventT.uses(.beforeGroup))
                            if (groupCursor.hasBefore()) {
                                Context.event = .{
                                    .beforeGroup = .{
                                        .slice = groupCursor.beforeMatch(),
                                        .n = group.n,
                                    },
                                };
                                const res = try fSink.consume(mode, EventT);
                                switch (res) {
                                    .consumed => groupCursor.moveToGroupStart(),
                                    .consumedLine => {
                                        lineCursor.finish();
                                        continue :lineLoop;
                                    },
                                    else => {},
                                }
                            };

                        if (groupCursor.isExcluded()) {
                            if (comptime !EventT.uses(.excludedGroup)) {
                                groupCursor.skipGroup();
                                continue :groupLoop;
                            }

                            Context.event = .{
                                .excludedGroup = .{
                                    .group = group,
                                    .hasMore = groupCursor.hasMoreGroups(),
                                },
                            };
                            const res = try fSink.consume(mode, EventT);
                            switch (res) {
                                .cropLine,
                                => {
                                    groupCursor.skipGroup();
                                    groupCursor.moveToGroupStart();
                                    groupCursor.cropLineToEnd();
                                    continue :groupLoop;
                                },
                                .skipped,
                                => {
                                    groupCursor.skipGroup();
                                    continue :groupLoop;
                                },
                                .consumed,
                                => {
                                    groupCursor.consume();
                                    continue :groupLoop;
                                },
                                .consumedLine,
                                => {
                                    lineCursor.finish();
                                    continue :lineLoop;
                                },
                            }
                        }

                        const res: sink.Sink.ConsumeResponse = rv: {
                            if (groupCursor.isEmptyGroup()) {
                                if (comptime !EventT.uses(.emptyGroup))
                                    break :rv .skipped;

                                Context.event = .{
                                    .emptyGroup = .{
                                        .group = group,
                                        .hasMore = groupCursor.hasMoreGroups(),
                                    },
                                };
                                break :rv try fSink.consume(mode, EventT);
                            } else {
                                if (comptime !EventT.uses(.groupMatch))
                                    break :rv .skipped;

                                Context.event = .{
                                    .groupMatch = .{
                                        .group = group,
                                        .hasMore = groupCursor.hasMoreGroups(),
                                    },
                                };
                                break :rv try fSink.consume(mode, EventT);
                            }
                        };
                        switch (res) {
                            .consumedLine => {
                                lineCursor.finish();
                                continue :lineLoop;
                            },
                            .skipped,
                            .consumed,
                            => {
                                defer groupCursor.consume();

                                if (groupCursor.isEmptyGroup() and
                                    (groupCursor.isCurrentMax() or groupCursor.isCurrent0()))
                                {
                                    groupCursor.forwardOne();
                                }

                                if (groupCursor.isCurrent0())
                                    break :groupLoop;
                            },
                            .cropLine => unreachable,
                        }
                    }

                    const reminderOpt = groupCursor.reminder();
                    if (comptime EventT.uses(.endOfGroups)) {
                        Context.event = .{
                            .endOfGroups = .{
                                .sliceOpt = reminderOpt,
                                .hadBreakline = groupCursor.lastIs('\n'),
                                .group0Empty = groupCursor.isGroup0Empty(),
                            },
                        };
                        _ = try fSink.consume(mode, EventT);
                    }
                    if (reminderOpt != null)
                        groupCursor.finishGroup0();

                    lineCursor.forwardTo(groupCursor.lineToken);
                }

                if (comptime EventT.uses(.eol)) {
                    Context.event = .{
                        .eol = .{
                            .charOpt = if (lineCursor.hasBreakline()) null else '\n',
                            .hadMatches = lineCursor.hadMatches,
                        },
                    };
                    _ = try fSink.consume(mode, EventT);
                }
            },
        }
    }
    return matches;
}

pub const GroupMatchCursor = struct {
    current: u16,
    match: regex.RegexMatch,
    group0: regex.RegexMatchGroup,
    currGroup: regex.RegexMatchGroup,
    targetGroup: args.TargetGroup,
    line: []const u8,
    lineToken: usize,
    max: u16,

    pub fn init(
        self: *@This(),
        lineCursor: *const LineMatchCursor,
        match: regex.RegexMatch,
        targetGroup: args.TargetGroup,
    ) LoadGroupError!void {
        self.* = .{
            .current = 0,
            .match = match,
            .group0 = undefined,
            .currGroup = undefined,
            .targetGroup = targetGroup,
            .line = lineCursor.line,
            .lineToken = lineCursor.lineToken,
            .max = @min(
                match.count,
                if (targetGroup == .zero) @as(u16, 1) else std.math.maxInt(u16),
            ),
        };
        try self.loadGroup();
        self.group0 = self.currGroup;
    }

    pub const LoadGroupError = error{
        Group0Skipped,
    } ||
        regex.RegexMatch.GroupError;

    pub fn loadGroup(self: *@This()) LoadGroupError!void {
        // NOTE: Out of bound group is treated as an error
        while (true) {
            const group = self.match.group(self.current) catch |e| {
                @branchHint(.unlikely);

                switch (e) {
                    regex.RegexMatch.GroupError.GroupSkipped => {
                        @branchHint(.unlikely);

                        if (self.current == 0) {
                            @branchHint(.cold);
                            return LoadGroupError.Group0Skipped;
                        }
                        self.current += 1;
                        continue;
                    },
                    else => {
                        @branchHint(.cold);
                        return e;
                    },
                }
            };
            self.currGroup = group;
            break;
        }
    }

    pub const CurrentError = error{
        GroupNotFound,
    };

    pub fn startsAfterTracker(self: *const @This()) bool {
        return self.currGroup.start < self.lineToken;
    }

    pub fn maxGroup(self: *const @This()) u16 {
        return self.match.count - 1;
    }

    pub fn isCurrentMax(self: *const @This()) bool {
        return self.currGroup.n == self.maxGroup();
    }

    pub fn isCurrent0(self: *const @This()) bool {
        return self.currGroup.n == 0;
    }

    pub fn forwardOne(self: *@This()) void {
        assert(self.lineToken < self.line.len);
        self.lineToken += 1;
    }

    pub fn moveToGroupStart(self: *@This()) void {
        self.lineToken = self.currGroup.start;
    }

    pub fn cropLineToEnd(self: *@This()) void {
        const to = self.currGroup.end;
        assert(to <= self.line.len);
        assert(self.lineToken <= to);
        self.line = self.line[0..to];
    }

    pub fn finishGroup0(self: *@This()) void {
        self.lineToken = self.group0.end;
    }

    pub fn skipGroup(self: *@This()) void {
        self.current += 1;
    }

    pub fn isEmptyGroup(self: *const @This()) bool {
        return self.currGroup.start == self.currGroup.end;
    }

    pub fn isGroup0Empty(self: *const @This()) bool {
        return self.group0.start == self.group0.end;
    }

    pub fn lastIs(self: *const @This(), comptime c: u8) bool {
        const i: usize = if (self.lineToken > 0)
            self.lineToken - 1
        else
            0;
        return self.line[i] == c;
    }

    pub fn consume(self: *@This()) void {
        assert(self.currGroup.end >= self.lineToken);
        self.lineToken = self.currGroup.end;
        self.skipGroup();
    }

    pub fn hasMoreGroups(self: *const @This()) bool {
        return self.targetGroup.anyGreaterThan(self.currGroup.n, self.maxGroup());
    }

    pub fn isExcluded(self: *const @This()) bool {
        return !self.targetGroup.includes(self.current);
    }

    pub fn hasBefore(self: *const @This()) bool {
        return self.lineToken < self.currGroup.start;
    }

    pub fn beforeMatch(self: *const @This()) []const u8 {
        const group = self.currGroup;
        assert(self.lineToken < group.start);

        return self.line[self.lineToken..group.start];
    }

    pub fn reminder(self: *const @This()) ?[]const u8 {
        if (self.lineToken == self.line.len) return null;

        const group0 = self.group0;
        if (group0.start == group0.end) return null;
        if (self.lineToken == group0.end) return null;

        return self.line[self.lineToken..group0.end];
    }

    pub fn matchEnd(self: *const @This()) usize {
        return self.group0.end;
    }

    pub fn hasNextGroup(self: *const @This()) bool {
        return self.current < self.max;
    }
};

pub const LineMatchCursor = struct {
    line: []const u8,
    lineToken: usize,
    hadMatches: bool,

    pub fn init(self: *@This()) void {
        self.* = .{
            .line = Context.baseEvent.line,
            .hadMatches = false,
            .lineToken = 0,
        };
    }

    pub const ErrorNext = regex.Regex.MatchError || GroupMatchCursor.LoadGroupError;

    pub fn matchNext(
        self: *@This(),
        rgx: regex.Regex,
        groupCursor: *GroupMatchCursor,
    ) ErrorNext!void {
        // NOTE: this check is here because of fastBadUTF8Skip
        if (!self.hasReminder()) return ErrorNext.NoMatch;

        const match = try rgx.offsetMatch(
            self.line,
            self.lineToken,
        );
        self.hadMatches = true;

        try groupCursor.init(
            self,
            match,
            Context.argsRes.options.targetGroup(
                Context.sinkp.eventHandler,
                match.count,
            ),
        );
    }

    pub fn hasBreakline(self: *const @This()) bool {
        assert(self.line.len > 0);
        return self.line[self.line.len - 1] == '\n';
    }

    pub fn finish(self: *@This()) void {
        self.forwardTo(self.line.len);
    }

    // NOTE: we are skipping only the easy bytes here
    pub fn fastBadUTF8Skip(self: *@This()) void {
        var offset = self.lineToken;
        loop: switch (self.line[offset]) {
            0b0000_0000...0b0111_1111,
            0b1100_0000...0b1101_1111,
            0b1110_0000...0b1110_1111,
            0b1111_0000...0b1111_0111,
            => self.forwardTo(offset),
            else => {
                offset += 1;
                continue :loop if (offset < self.line.len) self.line[offset] else '\x00';
            },
        }
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
