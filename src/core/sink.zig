const std = @import("std");
const units = @import("zpec").units;
const fs = @import("fs.zig");
const args = @import("args.zig");
const regex = @import("regex.zig");
const File = std.fs.File;
const Writer = std.Io.Writer;

// TODO: delete this
pub const Reporter = struct {
    stdoutW: *Writer = undefined,
    stderrW: *Writer = undefined,
};

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
                .matchOnly,
                => .{ .growing = units.CacheSize.L2 },
                .skipLineOnMatch,
                .pickNonMatchingLine,
                => .directWrite,
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

pub const NoColor = struct {
    pub fn pick() []const u8 {
        return "";
    }

    pub fn resetCode() []const u8 {
        return "";
    }
};

pub const RGB = struct {
    escapeCode: []const u8,

    pub fn from(comptime hex: []const u8) *const @This() {
        comptime if (hex.len != 7 or hex[0] != '#') @compileError(
            std.fmt.comptimePrint("Hex [{s}] given isnt RGB\n", .{hex}),
        );

        comptime for (hex[1..]) |byte| {
            switch (byte) {
                '0'...'9',
                'A'...'F',
                'a'...'f',
                => continue,
                else => @compileError(
                    std.fmt.comptimePrint("Bad byte for RGB in [{s}] - {x}\n", .{ hex, byte }),
                ),
            }
        };

        return comptime &.{ .escapeCode = std.fmt.comptimePrint(
            "\x1b[38;2;{s};{s};{s}m",
            .{
                hexStrToDecStr(hex[1..3]),
                hexStrToDecStr(hex[3..5]),
                hexStrToDecStr(hex[5..7]),
            },
        ) };
    }

    fn hexStrToDecStr(slice: []const u8) []const u8 {
        return comptime std.fmt.comptimePrint(
            "{d}",
            .{std.fmt.parseInt(u8, slice, 16) catch @compileError(
                std.fmt.comptimePrint("Input not a hex num [{s}]\n", .{slice}),
            )},
        );
    }
};

pub const RainbowColor = struct {
    pub fn pick(offset: u16) []const u8 {
        const bucketIndex: u8 = @intCast(offset % 11);
        return @as(*const RGB, switch (bucketIndex) {
            0 => .from("#EF0000"),
            1 => .from("#FA8072"),
            2 => .from("#FFA500"),
            3 => .from("#FFFF00"),
            4 => .from("#B0FF00"),
            5 => .from("#00EF00"),
            6 => .from("#00EF80"),
            7 => .from("#99D9EA"),
            8 => .from("#0000EF"),
            9 => .from("#8F00FF"),
            10 => .from("#9400D3"),
            else => unreachable,
        }).escapeCode;
    }

    pub fn resetCode() []const u8 {
        return EscapeColor.reset.escapeCode();
    }
};

pub const EscapeColor = enum {
    boldRed,
    boldGreen,
    boldYellow,
    boldBlue,
    boldMagenta,
    boldCyan,
    boldBlack,
    boldWhite,
    reset,

    pub fn escapeCode(self: @This()) []const u8 {
        return switch (self) {
            .boldRed => "\x1b[1;31m",
            .boldGreen => "\x1b[1;32m",
            .boldYellow => "\x1b[1;33m",
            .boldBlue => "\x1b[1;34m",
            .boldMagenta => "\x1b[1;35m",
            .boldCyan => "\x1b[1;36m",
            .boldBlack => "\x1b[1;30m",
            .boldWhite => "\x1b[1;37m",
            .reset => "\x1b[0m",
        };
    }

    pub fn pick(offset: u16) []const u8 {
        // We are ignoring bold black, white and reset
        const bucketIndex: u8 = @intCast(offset % 6);
        const color: EscapeColor = @enumFromInt(bucketIndex);
        std.debug.assert(color != .reset);
        return color.escapeCode();
    }

    pub fn resetCode() []const u8 {
        return EscapeColor.reset.escapeCode();
    }
};

pub const ColorPicker = struct {
    colorPattern: ColorPattern = .escape,
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

    pub const ColorPattern = enum {
        rainbow,
        escape,
        noColor,

        pub fn pick(self: @This(), offset: u16) []const u8 {
            return switch (self) {
                .rainbow => RainbowColor.pick(offset),
                .escape => EscapeColor.pick(offset),
                .noColor => NoColor.pick(),
            };
        }

        pub fn reset(self: @This()) []const u8 {
            return switch (self) {
                .rainbow => RainbowColor.resetCode(),
                .escape => EscapeColor.resetCode(),
                .noColor => NoColor.resetCode(),
            };
        }
    };

    pub fn init(colorPattern: ColorPattern) @This() {
        return .{
            .colorPattern = colorPattern,
            .trueColor = hasTrueColor(colorPattern),
        };
    }

    pub fn hasTrueColor(colorPattern: ColorPattern) bool {
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

pub const GroupTracker = struct {
    colorPicker: ColorPicker,
    cursor: usize = 0,
    group0Event: ?ExcludedMatchEvent = null,
};

pub const EventHandler = union(enum) {
    colorMatch: ColorPicker,
    coloredMatchOnly: GroupTracker,
    matchOnly,
    skipLineOnMatch,
    pickNonMatchingLine,
};

pub fn pickEventHandler(fDetailed: *const fs.DetailedFile, argsRes: *const args.ArgsRes) EventHandler {
    if (argsRes.options.@"invert-match") return .pickNonMatchingLine;

    switch (fDetailed.fileType) {
        .tty => {
            const colorPattern: ColorPicker.ColorPattern = if (argsRes.options.hasColor(fDetailed.fileType)) .escape else .noColor;

            if (argsRes.options.@"match-only") {
                return switch (colorPattern) {
                    .noColor => .matchOnly,
                    else => .{
                        .coloredMatchOnly = .{
                            .colorPicker = .init(colorPattern),
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

pub const ExcludedMatchEvent = struct {
    line: []const u8,
    group: regex.RegexMatchGroup,
};

pub const Events = union(enum) {
    excludedMatch: ExcludedMatchEvent,
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
            // NOTE: this is currently wrong because targetGroups need to
            // be zero fixed for non-colored match-only
            .matchOnly => {
                switch (event) {
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
            .skipLineOnMatch => {
                switch (event) {
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
