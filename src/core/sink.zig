const std = @import("std");
const units = @import("zpec").units;
const fs = @import("fs.zig");
const args = @import("args.zig");
const regex = @import("regex.zig");
const tty = @import("tty.zig");
const File = std.fs.File;
const Writer = std.Io.Writer;

pub const ColorPicker = struct {
    colorPattern: tty.ColorPattern = .escape,
    trueColor: bool,
    state: State = .reset,

    pub const State = union(enum) {
        reset,
        color: u16,

        pub fn eql(self: @This(), other: @This()) bool {
            return switch (self) {
                .reset => switch (other) {
                    .reset => true,
                    .color => false,
                },
                .color => |offset| switch (other) {
                    .reset => false,
                    .color => offset == other.color,
                },
            };
        }
    };

    pub fn init(colorPattern: tty.ColorPattern) @This() {
        return .{
            .colorPattern = colorPattern,
            .trueColor = hasTrueColor(colorPattern),
        };
    }

    pub fn hasTrueColor(colorPattern: tty.ColorPattern) bool {
        if (colorPattern == .noColor) return false;

        if (std.posix.getenv("COLORTERM")) |colorterm| {
            return std.mem.eql(u8, colorterm, "truecolor");
        }
        return false;
    }

    pub fn reset(self: *@This()) []const u8 {
        if (self.state.eql(.reset)) return "";
        self.state = .reset;
        return self.colorPattern.reset();
    }

    pub fn pickColor(self: *@This(), offset: u16) []const u8 {
        const target: State = .{ .color = offset };
        if (self.state.eql(target)) return "";
        self.state = target;

        return rfd: switch (self.colorPattern) {
            .rainbow => rv: {
                if (!self.trueColor) {
                    self.colorPattern = .escape;
                    continue :rfd .escape;
                }
                break :rv self.colorPattern.pick(offset);
            },
            else => self.colorPattern.pick(offset),
        };
    }
};

pub fn ColoredChunks(comptime n: std.math.IntFittingRange(0, std.math.maxInt(usize) / 2)) type {
    const size = n * 2;
    return struct {
        colorOffset: u16 = undefined,
        chunks: [size][]const u8 = undefined,
        at: usize = 0,
        colorPicker: *ColorPicker = undefined,

        pub fn init(colorPicker: *ColorPicker, offset: u16) @This() {
            std.debug.assert(colorPicker.trueColor);
            var self: @This() = .{};

            self.colorPicker = colorPicker;
            self.colorOffset = offset;

            return self;
        }

        pub fn clearChunk(self: *@This(), chunk: []const u8) void {
            std.debug.assert(self.at + 2 <= self.chunks.len);
            if (chunk.len > 0) {
                self.chunks[self.at] = self.colorPicker.reset();
                self.chunks[self.at + 1] = chunk;
            } else {
                self.chunks[self.at] = "";
                self.chunks[self.at + 1] = "";
            }
            self.at += 2;
        }

        pub fn coloredChunk(self: *@This(), chunk: []const u8) void {
            std.debug.assert(self.at + 2 <= self.chunks.len);
            if (chunk.len > 0) {
                self.chunks[self.at] = self.colorPicker.pickColor(self.colorOffset);
                self.chunks[self.at + 1] = chunk;
            } else {
                self.chunks[self.at] = "";
                self.chunks[self.at + 1] = "";
            }
            self.at += 2;
        }

        pub fn skipChunk(self: *@This()) void {
            std.debug.assert(self.at + 2 <= self.chunks.len);
            self.chunks[self.at] = "";
            self.chunks[self.at + 1] = "";
            self.at += 2;
        }

        // crawls all chunks to see if there's a breakline already in there before all
        // skipped chunks
        pub fn breakline(self: *@This()) void {
            std.debug.assert(self.at + 2 == size);
            var chunk = size - 3;
            while (chunk > 0) : (chunk -|= 2) {
                const last = self.chunks[chunk];
                if (last.len == 0) continue;

                self.add(if (last.len > 0 and last[last.len - 1] != '\n') "\n" else "");
                return;
            }
        }

        pub fn add(self: *@This(), chunk: []const u8) void {
            std.debug.assert(self.at + 2 <= self.chunks.len);
            self.chunks[self.at] = "";
            self.chunks[self.at + 1] = chunk;
            self.at += 2;
        }
    };
}

pub const SinkBufferType = enum {
    growing,
    buffered,
    directWrite,
};

pub const SinkBuffer = union(SinkBufferType) {
    growing: usize,
    buffered: usize,
    directWrite,
};

pub fn pickSinkBuffer(fDetailed: *const fs.DetailedFile, eventHandler: EventHandler) SinkBuffer {
    switch (fDetailed.fileType) {
        .tty => {
            return switch (eventHandler) {
                .coloredMatchOnly,
                .colorMatch,
                .coloredGroupOnly,
                => .{ .growing = units.CacheSize.L2 },
                .skipLineOnMatch,
                .pickNonMatchingLine,
                => .directWrite,
                .matchOnly,
                .groupOnly,
                => .{ .buffered = units.CacheSize.L2 },
            };
        },
        .generic,
        .characterDevice,
        .pipe,
        .file,
        => return .{ .buffered = units.CacheSize.L2 },
    }
    unreachable;
}

pub const GroupTracker = struct {
    colorPicker: ColorPicker,
    cursor: usize = 0,
    group0Event: ?SimpleMatchEvent = null,
};

pub const GroupOnlyHelper = struct {
    colorPicker: ColorPicker,
    delimiter: u8,
};

pub const EventHandler = union(enum) {
    colorMatch: ColorPicker,
    coloredMatchOnly: GroupTracker,
    coloredGroupOnly: GroupOnlyHelper,
    matchOnly,
    groupOnly: u8,
    skipLineOnMatch,
    pickNonMatchingLine,
};

pub fn pickEventHandler(fDetailed: *const fs.DetailedFile, argsRes: *const args.ArgsRes) EventHandler {
    if (argsRes.options.@"invert-match") return .pickNonMatchingLine;

    switch (fDetailed.fileType) {
        .tty => {
            const colorPattern: tty.ColorPattern = if (argsRes.options.hasColor(fDetailed.fileType)) .escape else .noColor;

            if (argsRes.options.@"match-only") {
                return switch (colorPattern) {
                    .noColor => .matchOnly,
                    else => .{
                        .coloredMatchOnly = .{
                            .colorPicker = .init(colorPattern),
                        },
                    },
                };
            }

            if (argsRes.options.@"group-only") {
                return switch (colorPattern) {
                    .noColor => .{ .groupOnly = argsRes.options.@"group-delimiter" },
                    else => .{
                        .coloredGroupOnly = .{
                            .colorPicker = .init(colorPattern),
                            .delimiter = argsRes.options.@"group-delimiter",
                        },
                    },
                };
            } else {
                return switch (colorPattern) {
                    .noColor => .skipLineOnMatch,
                    else => .{ .colorMatch = .init(colorPattern) },
                };
            }
        },
        .generic,
        .characterDevice,
        .file,
        .pipe,
        => {
            if (argsRes.options.@"match-only") return .matchOnly;
            if (argsRes.options.@"group-only") return .{ .groupOnly = argsRes.options.@"group-delimiter" };
            return .skipLineOnMatch;
        },
    }
    unreachable;
}

pub const MatchEvent = struct {
    line: []const u8,
    group: regex.RegexMatchGroup,
    count: usize,
    targetGroup: args.TargetGroup,
};

pub const SimpleMatchEvent = struct {
    line: []const u8,
    group: regex.RegexMatchGroup,
};

pub const Events = union(enum) {
    emptyMatch: SimpleMatchEvent,
    excludedMatch: SimpleMatchEvent,
    beforeMatch: []const u8,
    match: MatchEvent,
    noMatchEndOfLineAfterMatch: []const u8,
    noMatchEndOfLine: []const u8,
    endOfMatchGroups,
    endOfLine,
    endOfFile,
};

// NOTE: this is not a writer because flush calls drain repeteadly
// Allocating doesnt accept chaining
pub const GrowingWriter = struct {
    allocating: std.Io.Writer.Allocating,
    fdWriter: std.fs.File.Writer,

    pub fn flush(self: *@This()) Writer.Error!void {
        const buff = self.allocating.writer.buffered();
        try self.fdWriter.interface.writeAll(buff);
        _ = self.allocating.writer.consumeAll();
    }

    pub fn writer(self: *@This()) *Writer {
        return &self.allocating.writer;
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.allocating.deinit();
        allocator.destroy(self);
    }
};

pub const BufferOwnedWriter = struct {
    allocator: std.mem.Allocator,
    writer: File.Writer,

    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.writer.interface.buffer);
    }
};

pub const SinkWriter = union(SinkBufferType) {
    growing: *GrowingWriter,
    buffered: File.Writer,
    directWrite: File.Writer,

    pub const InitError = std.mem.Allocator.Error;

    pub fn init(fDetailed: *const fs.DetailedFile, allocator: std.mem.Allocator, eventHandler: EventHandler) InitError!@This() {
        const sinkBuffer = pickSinkBuffer(fDetailed, eventHandler);

        return switch (sinkBuffer) {
            .growing => |size| rv: {
                const growing = try allocator.create(GrowingWriter);
                errdefer allocator.destroy(growing);

                const allocating = try std.Io.Writer.Allocating.initCapacity(allocator, size);
                const writer = switch (fDetailed.accessType) {
                    .streaming => fDetailed.file.writerStreaming(&.{}),
                    .positional => fDetailed.file.writer(&.{}),
                };

                growing.* = .{
                    .allocating = allocating,
                    .fdWriter = writer,
                };
                break :rv .{ .growing = growing };
            },
            .buffered => |size| rv: {
                const buff = try allocator.alloc(u8, size);
                const writer = switch (fDetailed.accessType) {
                    .streaming => fDetailed.file.writerStreaming(buff),
                    .positional => fDetailed.file.writer(buff),
                };
                break :rv .{ .buffered = writer };
            },
            .directWrite => rv: {
                const writer = switch (fDetailed.accessType) {
                    .streaming => fDetailed.file.writerStreaming(&.{}),
                    .positional => fDetailed.file.writer(&.{}),
                };
                break :rv .{ .directWrite = writer };
            },
        };
    }
};

pub const Sink = struct {
    fileType: fs.FileType,
    eventHandler: EventHandler,
    colorEnabled: bool = false,
    sinkWriter: SinkWriter,

    pub fn init(fDetailed: *const fs.DetailedFile, allocator: std.mem.Allocator, argsRes: *const args.ArgsRes) SinkWriter.InitError!@This() {
        const eventHandler = pickEventHandler(
            fDetailed,
            argsRes,
        );
        return .{
            .fileType = fDetailed.fileType,
            .eventHandler = eventHandler,
            .sinkWriter = try .init(fDetailed, allocator, eventHandler),
        };
    }

    pub fn writeAll(self: *@This(), data: []const u8) Writer.Error!void {
        switch (self.sinkWriter) {
            .growing,
            => |w| {
                try w.writer().writeAll(data);
            },
            .buffered,
            .directWrite,
            => |*w| {
                try w.interface.writeAll(data);
            },
        }
    }

    pub fn writeVecAll(self: *@This(), data: [][]const u8) Writer.Error!void {
        switch (self.sinkWriter) {
            .growing,
            => |w| {
                try w.writer().writeVecAll(data);
            },
            .buffered,
            .directWrite,
            => |*w| {
                try w.interface.writeVecAll(data);
            },
        }
    }

    pub fn sinkLine(self: *@This()) Writer.Error!void {
        // TODO: do I need a flush strategy too?
        switch (self.fileType) {
            .tty => {
                switch (self.sinkWriter) {
                    .growing => |alloc| try alloc.flush(),
                    .buffered,
                    => |*w| try w.interface.flush(),
                    .directWrite,
                    => {},
                }
            },
            else => {},
        }
    }

    pub fn sink(self: *@This()) Writer.Error!void {
        switch (self.sinkWriter) {
            .growing => |w| try w.flush(),
            .buffered => |*w| try w.interface.flush(),
            .directWrite => {},
        }
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        switch (self.sinkWriter) {
            .growing,
            => |w| w.deinit(allocator),
            .buffered,
            .directWrite,
            => |*w| {
                if (w.interface.buffer.len > 0) {
                    allocator.free(w.interface.buffer);
                }
            },
        }
    }

    pub const ConsumeError = error{} ||
        std.fs.File.WriteError ||
        Writer.Error;

    pub const ConsumeResponse = union(enum) {
        eventSkipped,
        eventConsumed,
        eventPostponed,
        eventForward: usize,
        lineConsumed,
    };

    pub fn consume(self: *@This(), event: Events) ConsumeError!ConsumeResponse {
        switch (self.eventHandler) {
            .colorMatch => |*colorPicker| {
                switch (event) {
                    // This type doesnt have an end of line handling
                    .emptyMatch,
                    => |emptyMatch| {
                        const group = emptyMatch.group;
                        const line = emptyMatch.line;

                        if (group.start == line.len - 1 and line[group.start] == '\n') {
                            var cChunks = ColoredChunks(1).init(colorPicker, 0);
                            cChunks.clearChunk("\n");
                            try self.writeVecAll(&cChunks.chunks);
                        }

                        return .{ .eventForward = group.end + 1 };
                    },
                    .excludedMatch,
                    => return .eventSkipped,
                    .beforeMatch,
                    => |data| {
                        var cChunks = ColoredChunks(1).init(colorPicker, 0);
                        cChunks.clearChunk(data);
                        try self.writeVecAll(&cChunks.chunks);
                        return .eventConsumed;
                    },
                    .match => |matchEvent| {
                        var cChunks = ColoredChunks(1).init(
                            colorPicker,
                            @intCast(matchEvent.group.n),
                        );
                        cChunks.coloredChunk(matchEvent.group.slice(matchEvent.line));
                        try self.writeVecAll(&cChunks.chunks);
                        return .eventConsumed;
                    },
                    .noMatchEndOfLineAfterMatch,
                    => |data| {
                        var cChunks = ColoredChunks(1).init(colorPicker, 0);
                        cChunks.clearChunk(data);
                        try self.writeVecAll(&cChunks.chunks);
                        return .eventConsumed;
                    },
                    .noMatchEndOfLine,
                    .endOfMatchGroups,
                    => return .eventSkipped,
                    .endOfLine => {
                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.writeAll(colorPicker.reset());
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            .coloredMatchOnly => |*groupTracker| {
                switch (event) {
                    .emptyMatch,
                    => |emptyEvent| return .{
                        .eventForward = emptyEvent.group.end + 1,
                    },
                    .excludedMatch,
                    => |exclude| {
                        if (exclude.group.n == 0) {
                            groupTracker.cursor = exclude.group.start;
                            groupTracker.group0Event = exclude;
                            return .eventConsumed;
                        }
                        return .eventSkipped;
                    },
                    .beforeMatch,
                    => return .eventSkipped,
                    // When --group-highlight is used, necesarily we need more than 0
                    // in case only 0 is available, targeGroups are reduced to .zero
                    // in all other cases they are .zero and ensured to be .zero unless
                    // --group-highlight is given
                    // When --group-highlight is used and targetGroups are non-zero, group0
                    // is guaranteed to be sent as .excludedMatch
                    .match => |matchEvent| {
                        switch (matchEvent.group.n) {
                            0 => {
                                std.debug.assert(!matchEvent.targetGroup.anyGreaterThan(0));

                                var cChunks = ColoredChunks(2).init(&groupTracker.colorPicker, 0);

                                cChunks.coloredChunk(matchEvent.group.slice(matchEvent.line));
                                cChunks.breakline();

                                try self.writeVecAll(&cChunks.chunks);

                                return .eventConsumed;
                            },
                            else => {
                                std.debug.assert(matchEvent.count > 1);
                                std.debug.assert(groupTracker.group0Event != null);

                                const excludedEvent = groupTracker.group0Event.?;
                                const group0 = excludedEvent.group;

                                const line = matchEvent.line;
                                const group = matchEvent.group;
                                const cursor = groupTracker.cursor;
                                const endChunk = !matchEvent.targetGroup.anyGreaterThan(group.n) or matchEvent.count - 1 == group.n;

                                var cChunks = ColoredChunks(4).init(
                                    &groupTracker.colorPicker,
                                    @intCast(group.n),
                                );

                                if (cursor < group.start) {
                                    cChunks.clearChunk(line[cursor..group.start]);
                                } else cChunks.skipChunk();

                                cChunks.coloredChunk(group.slice(line));

                                // There's a fallback for this in .endOfMatchGroups
                                // In case last group ends at or before a previous group
                                if (endChunk and group.end != group0.end) {
                                    cChunks.clearChunk(line[group.end..group0.end]);
                                    groupTracker.cursor = group0.end;
                                } else {
                                    groupTracker.cursor = group.end;
                                    cChunks.skipChunk();
                                }

                                if (groupTracker.cursor == group0.end) {
                                    cChunks.breakline();
                                } else cChunks.skipChunk();

                                try self.writeVecAll(&cChunks.chunks);

                                if (endChunk) {
                                    return .{ .eventForward = group0.end };
                                } else return .eventConsumed;
                            },
                        }
                    },
                    .noMatchEndOfLineAfterMatch,
                    .noMatchEndOfLine,
                    => return .eventSkipped,
                    .endOfMatchGroups,
                    => {
                        defer {
                            groupTracker.cursor = 0;
                            groupTracker.group0Event = null;
                        }

                        if (groupTracker.group0Event) |group0Event| {
                            const group0 = group0Event.group;
                            if (groupTracker.cursor != group0.end) {
                                const line = group0Event.line;

                                var cChunks = ColoredChunks(2).init(&groupTracker.colorPicker, 0);
                                cChunks.clearChunk(line[groupTracker.cursor..group0.end]);
                                groupTracker.cursor = group0.end;

                                cChunks.breakline();
                                try self.writeVecAll(&cChunks.chunks);

                                return .eventConsumed;
                            }
                        }
                        return .eventConsumed;
                    },
                    .endOfLine => {
                        std.debug.assert(groupTracker.group0Event == null);
                        std.debug.assert(groupTracker.cursor == 0);

                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        std.debug.assert(groupTracker.group0Event == null);
                        std.debug.assert(groupTracker.cursor == 0);

                        try self.writeAll(groupTracker.colorPicker.reset());
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            .coloredGroupOnly => |*groupOnlyHelper| {
                switch (event) {
                    .emptyMatch,
                    => |emptyEvent| {
                        if (emptyEvent.line[emptyEvent.group.end] != '\n') {
                            var buff: [2][]const u8 = .{
                                groupOnlyHelper.colorPicker.reset(),
                                &.{groupOnlyHelper.delimiter},
                            };
                            try self.writeVecAll(&buff);
                        }

                        return .{ .eventForward = emptyEvent.group.end + 1 };
                    },
                    .excludedMatch,
                    .beforeMatch,
                    => return .eventSkipped,
                    .match => |matchEvent| {
                        const group = matchEvent.group;
                        const endChunk = !matchEvent.targetGroup.anyGreaterThan(group.n) or matchEvent.count - 1 == group.n;

                        switch (matchEvent.group.n) {
                            0 => std.debug.assert(!matchEvent.targetGroup.anyGreaterThan(0)),
                            else => {},
                        }
                        var cChunks = ColoredChunks(2).init(
                            &groupOnlyHelper.colorPicker,
                            @intCast(matchEvent.group.n),
                        );

                        cChunks.coloredChunk(group.slice(matchEvent.line));
                        if (!endChunk) {
                            cChunks.clearChunk(&.{groupOnlyHelper.delimiter});
                        } else cChunks.breakline();

                        try self.writeVecAll(&cChunks.chunks);

                        return .eventConsumed;
                    },
                    .noMatchEndOfLineAfterMatch,
                    .noMatchEndOfLine,
                    .endOfMatchGroups,
                    => return .eventSkipped,
                    .endOfLine => {
                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.writeAll(groupOnlyHelper.colorPicker.reset());
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            // NOTE: this is currently wrong because targetGroups need to
            // be zero fixed for non-colored match-only
            .matchOnly => {
                switch (event) {
                    .emptyMatch,
                    => |emptyEvent| return .{
                        .eventForward = emptyEvent.group.end + 1,
                    },
                    .excludedMatch,
                    .beforeMatch,
                    => return .eventSkipped,
                    .match => |matchEvent| {
                        const slice = matchEvent.group.slice(matchEvent.line);

                        if (slice.len > 0 and
                            slice[slice.len - 1] != '\n')
                        {
                            var buff: [2][]const u8 = .{ slice, "\n" };
                            try self.writeVecAll(&buff);
                        } else try self.writeAll(slice);

                        return .eventConsumed;
                    },
                    .noMatchEndOfLineAfterMatch,
                    .noMatchEndOfLine,
                    .endOfMatchGroups,
                    => return .eventSkipped,
                    .endOfLine,
                    .endOfFile,
                    => {
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            .groupOnly => |delimiter| {
                switch (event) {
                    .emptyMatch,
                    => |emptyEvent| {
                        if (emptyEvent.line[emptyEvent.group.end] != '\n') {
                            try self.writeAll(&.{delimiter});
                        }

                        return .{ .eventForward = emptyEvent.group.end + 1 };
                    },
                    .excludedMatch,
                    .beforeMatch,
                    => return .eventSkipped,
                    .match => |matchEvent| {
                        const group = matchEvent.group;
                        const endChunk = !matchEvent.targetGroup.anyGreaterThan(group.n) or matchEvent.count - 1 == group.n;

                        switch (matchEvent.group.n) {
                            0 => std.debug.assert(!matchEvent.targetGroup.anyGreaterThan(0)),
                            else => {},
                        }

                        const slice = matchEvent.group.slice(matchEvent.line);

                        var buff: [2][]const u8 = if (endChunk) .{
                            slice,
                            if (slice[slice.len - 1] != '\n') "\n" else "",
                        } else .{ slice, &.{delimiter} };

                        try self.writeVecAll(&buff);

                        return .eventConsumed;
                    },
                    .noMatchEndOfLineAfterMatch,
                    .noMatchEndOfLine,
                    .endOfMatchGroups,
                    => return .eventSkipped,
                    .endOfLine => {
                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            .skipLineOnMatch => {
                switch (event) {
                    .emptyMatch,
                    => |emptyEvent| return .{
                        .eventForward = emptyEvent.group.end + 1,
                    },
                    .excludedMatch,
                    .beforeMatch,
                    => return .eventSkipped,
                    .match => |matchEvent| {
                        try self.writeAll(matchEvent.line);
                        return .lineConsumed;
                    },
                    .noMatchEndOfLineAfterMatch,
                    .noMatchEndOfLine,
                    .endOfMatchGroups,
                    => return .eventSkipped,
                    .endOfLine => {
                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            .pickNonMatchingLine => {
                switch (event) {
                    .emptyMatch,
                    => |emptyEvent| return .{
                        .eventForward = emptyEvent.group.end + 1,
                    },
                    .excludedMatch,
                    .beforeMatch,
                    .match,
                    .noMatchEndOfLineAfterMatch,
                    => return .eventSkipped,
                    .noMatchEndOfLine => |line| {
                        try self.writeAll(line);
                        return .lineConsumed;
                    },
                    .endOfMatchGroups,
                    => return .eventSkipped,
                    .endOfLine => {
                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
        }
    }
};
