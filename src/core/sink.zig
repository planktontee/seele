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
                .colorGroupMatchOnly,
                .colorMatch,
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

pub const ColorPicker = struct {
    trueColor: bool,
    config: std.Io.tty.Config,
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

    pub fn init(file: File) @This() {
        const config = std.Io.tty.Config.detect(file);
        return .{
            .config = config,
            .trueColor = hasTrueColor(config),
        };
    }

    pub fn hasTrueColor(config: std.Io.tty.Config) bool {
        if (config == .no_color or config == .windows_api) return false;

        if (std.posix.getenv("COLORTERM")) |colorterm| {
            return std.mem.eql(u8, colorterm, "truecolor");
        }
        return false;
    }

    pub fn reset(self: *@This(), w: *Writer) std.Io.tty.Config.SetColorError!void {
        if (self.config == .no_color) return;
        if (!self.state.eql(.reset)) {
            try w.writeAll(EscapeColor.reset.escapeCode());
            self.state = .reset;
        }
    }

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

    pub fn rainbowPick(offset: u16) []const u8 {
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
    };

    pub fn pickEscapeColor(offset: u16) []const u8 {
        // We are ignoring bold black, white and reset
        const bucketIndex: u8 = @intCast(offset % 6);
        const color: EscapeColor = @enumFromInt(bucketIndex);
        std.debug.assert(color != .reset);
        return color.escapeCode();
    }

    pub fn pickColor(offset: u16) []const u8 {
        return pickEscapeColor(offset);
    }

    fn useConfigColoring(self: *const @This()) bool {
        return (self.config == .escape_codes and !self.trueColor) or
            self.config == .windows_api;
    }

    pub fn setColor(self: *@This(), w: *Writer, offset: u16) std.Io.tty.Config.SetColorError!void {
        if (self.config == .no_color) return;
        if (self.useConfigColoring()) {
            const target: State = .{ .color = 0 };
            if (!self.state.eql(target)) {
                try w.writeAll(EscapeColor.boldRed.escapeCode());
                self.state = target;
            }
        } else {
            const target: State = .{ .color = offset };
            if (!self.state.eql(target)) {
                try w.writeAll(pickColor(offset));
                self.state = target;
            }
        }
    }
};

pub const GroupTracker = struct {
    colorPicker: ColorPicker,
    cursor: usize = 0,
    group0: ?regex.RegexMatchGroup = null,
    groupHighlight: bool = false,
};

pub const EventHandler = union(enum) {
    colorMatch: ColorPicker,
    colorGroupMatchOnly: GroupTracker,
    skipLineOnMatch,
    pickNonMatchingLine,
};

pub fn pickEventHandler(fDetailed: *const fs.DetailedFile, argsRes: *const args.ArgsRes) EventHandler {
    if (argsRes.options.@"invert-match") return .pickNonMatchingLine;

    switch (fDetailed.fileType) {
        .tty => {
            if (argsRes.options.hasColor(fDetailed.fileType)) {
                const colorPicker: ColorPicker = .init(fDetailed.file);
                if (argsRes.options.@"match-only") {
                    return .{
                        .colorGroupMatchOnly = .{
                            .colorPicker = colorPicker,
                            .groupHighlight = argsRes.options.@"group-highlight",
                        },
                    };
                } else {
                    return .{ .colorMatch = colorPicker };
                }
            } else {
                return .skipLineOnMatch;
            }
        },
        .generic,
        .characterDevice,
        .file,
        .pipe,
        => return .skipLineOnMatch,
    }
    unreachable;
}

pub const MatchEvent = struct {
    line: []const u8,
    group: regex.RegexMatchGroup,
    count: usize,
    targetGroup: args.TargetGroup,
};

pub const Events = union(enum) {
    beforeMatch: []const u8,
    match: MatchEvent,
    noMatchEndOfLineAfterMatch: []const u8,
    noMatchEndOfLine: []const u8,
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
        self.* = undefined;
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

    // TODO: move to buffer return
    pub fn sendColor(
        self: *@This(),
        colorPicker: *ColorPicker,
        offset: u16,
    ) std.Io.tty.Config.SetColorError!void {
        switch (self.sinkWriter) {
            .growing => |w| try colorPicker.setColor(w.writer(), offset),
            .buffered,
            .directWrite,
            => |*w| try colorPicker.setColor(&w.interface, offset),
        }
    }

    // TODO: move to buffer return
    pub fn sendResetColor(
        self: *@This(),
        colorPicker: *ColorPicker,
    ) std.Io.tty.Config.SetColorError!void {
        switch (self.sinkWriter) {
            .growing => |w| try colorPicker.reset(w.writer()),
            .buffered,
            .directWrite,
            => |*w| try colorPicker.reset(&w.interface),
        }
    }

    // TODO: move colored versions to writeVecAll
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
                allocator.free(w.interface.buffer);
                allocator.destroy(w);
            },
        }
    }

    pub const ConsumeError = error{} ||
        std.fs.File.WriteError ||
        std.Io.tty.Config.SetColorError ||
        Writer.Error;

    pub const ConsumeResponse = union(enum) {
        eventSkipped,
        eventConsumed,
        eventCached,
        eventUncached: usize,
        lineConsumed,
    };

    pub fn consume(self: *@This(), event: Events) ConsumeError!ConsumeResponse {
        switch (self.eventHandler) {
            .colorMatch => |*colorPicker| {
                switch (event) {
                    .match => |matchEvent| {
                        try self.sendColor(
                            colorPicker,
                            @intCast(matchEvent.group.n),
                        );
                        try self.writeAll(matchEvent.group.slice(matchEvent.line));
                        return .eventConsumed;
                    },
                    .beforeMatch,
                    .noMatchEndOfLineAfterMatch,
                    => |data| {
                        try self.sendResetColor(colorPicker);
                        try self.writeAll(data);
                        return .eventConsumed;
                    },
                    .noMatchEndOfLine => {
                        try self.sendResetColor(colorPicker);
                        return .eventSkipped;
                    },
                    .endOfLine => {
                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.sendResetColor(colorPicker);
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            .colorGroupMatchOnly => |*groupTracker| {
                switch (event) {
                    .beforeMatch,
                    .noMatchEndOfLineAfterMatch,
                    .noMatchEndOfLine,
                    => return .eventSkipped,
                    .match => |matchEvent| {
                        if (matchEvent.group.n == 0) {
                            groupTracker.cursor = matchEvent.group.start;
                            groupTracker.group0 = matchEvent.group;

                            if (groupTracker.groupHighlight and matchEvent.targetGroup.anyOtherThan(0))
                                return .eventCached;

                            var cChunks = try ColoredChunks(4).init(
                                &groupTracker.colorPicker,
                                @intCast(matchEvent.group.n),
                            );

                            const slice = matchEvent.group.slice(matchEvent.line);
                            cChunks.addAndColor(slice);
                            cChunks.add(rv: {
                                break :rv if (slice.len > 0 and slice[slice.len - 1] != '\n') "\n" else "";
                            });
                            try self.writeVecAll(&cChunks.chunks);
                            cChunks.updatePicker(&groupTracker.colorPicker);

                            return .eventConsumed;
                        } else {
                            std.debug.assert(groupTracker.group0 != null);

                            const group0 = groupTracker.group0.?;
                            const line = matchEvent.line;
                            const group = matchEvent.group;
                            const cursor = groupTracker.cursor;
                            const hasMoreGroups = matchEvent.targetGroup.anyOtherThan(group.n);
                            var cChunks = try ColoredChunks(8).init(
                                &groupTracker.colorPicker,
                                @intCast(group.n),
                            );

                            // INFO: Grammar for pieces:
                            // <reset color (if needed)> <before match if any>
                            // <match color (if needed)> <match>
                            // <reset color (if needed)> <post match if and and last group>
                            // <breakline> (if needed)
                            cChunks.addAndReset(rv: {
                                if (cursor < group.start) {
                                    break :rv line[cursor..group.start];
                                } else break :rv "";
                            });
                            const slice = group.slice(line);
                            cChunks.addAndColor(slice);
                            cChunks.addAndReset(rv: {
                                if (matchEvent.count - 1 == group.n or !hasMoreGroups) {
                                    break :rv line[group.end..group0.end];
                                } else break :rv "";
                            });
                            cChunks.add(rv: {
                                break :rv if (group.end == group0.end or
                                    (matchEvent.count - 1 == group.n or !hasMoreGroups) and
                                        slice.len > 0 and
                                        slice[slice.len - 1] != '\n') "\n" else "";
                            });

                            try self.writeVecAll(&cChunks.chunks);
                            cChunks.updatePicker(&groupTracker.colorPicker);

                            groupTracker.cursor = group.end;
                            if (matchEvent.count - 1 == group.n or !hasMoreGroups) {
                                return .{ .eventUncached = group0.end };
                            } else {
                                return .eventConsumed;
                            }
                        }
                    },
                    .endOfLine => {
                        try self.sinkLine();
                        groupTracker.cursor = 0;
                        groupTracker.group0 = null;
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.sendResetColor(&groupTracker.colorPicker);
                        try self.sink();
                        groupTracker.cursor = 0;
                        groupTracker.group0 = null;
                        return .eventConsumed;
                    },
                }
            },
            .skipLineOnMatch => {
                switch (event) {
                    .beforeMatch,
                    .noMatchEndOfLine,
                    .noMatchEndOfLineAfterMatch,
                    => return .eventSkipped,
                    .match => |matchEvent| {
                        try self.writeAll(matchEvent.line);
                        return .lineConsumed;
                    },
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
                    .noMatchEndOfLine => |line| {
                        try self.writeAll(line);
                        return .lineConsumed;
                    },
                    .beforeMatch,
                    .noMatchEndOfLineAfterMatch,
                    .match,
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

// TODO: can this be moved inside ColorPicker?
pub fn ColoredChunks(comptime size: usize) type {
    return struct {
        reset: [4]u8 = undefined,
        resetEnd: usize = 0,
        colored: [19]u8 = undefined,
        coloredEnd: usize = 0,
        colorOffset: u16 = undefined,
        chunks: [size][]const u8 = undefined,
        at: usize = 0,
        state: ColorPicker.State = undefined,

        pub fn init(colorPicker: *ColorPicker, offset: u16) std.Io.tty.Config.SetColorError!@This() {
            std.debug.assert(colorPicker.trueColor);
            var self: @This() = .{};

            const currState = colorPicker.state;

            // NOTE:
            // we are resetting colorPick state to ensure we write what's reset and whats
            // colored for this offset
            // slices are not stored because we later copy this struct to return it
            colorPicker.state = .{ .color = offset };
            var colorWriter: Writer = .fixed(&self.reset);
            try colorPicker.reset(&colorWriter);
            std.debug.assert(colorWriter.end != 0);
            self.resetEnd = colorWriter.end;

            colorPicker.state = .reset;
            colorWriter = .fixed(&self.colored);
            try colorPicker.setColor(&colorWriter, offset);
            std.debug.assert(colorWriter.end != 0);
            self.coloredEnd = colorWriter.end;

            colorPicker.state = currState;
            self.state = currState;
            self.colorOffset = offset;

            return self;
        }

        pub fn addAndReset(self: *@This(), chunk: []const u8) void {
            std.debug.assert(self.at + 2 <= self.chunks.len);
            if (chunk.len > 0) {
                self.chunks[self.at] = rv: {
                    if (self.state.eql(.reset)) {
                        break :rv "";
                    } else {
                        break :rv self.reset[0..self.resetEnd];
                    }
                };
                self.chunks[self.at + 1] = chunk;
                self.state = .reset;
            } else {
                self.chunks[self.at] = "";
                self.chunks[self.at + 1] = "";
            }
            self.at += 2;
        }

        pub fn addAndColor(self: *@This(), chunk: []const u8) void {
            std.debug.assert(self.at + 2 <= self.chunks.len);
            if (chunk.len > 0) {
                const target: ColorPicker.State = .{ .color = self.colorOffset };
                self.chunks[self.at] = rv: {
                    if (self.state.eql(target)) {
                        break :rv "";
                    } else {
                        self.state = target;
                        break :rv self.colored[0..self.coloredEnd];
                    }
                };
                self.chunks[self.at + 1] = chunk;
            } else {
                self.chunks[self.at] = "";
                self.chunks[self.at + 1] = "";
            }
            self.at += 2;
        }

        pub fn add(self: *@This(), chunk: []const u8) void {
            std.debug.assert(self.at + 2 <= self.chunks.len);
            self.chunks[self.at] = "";
            self.chunks[self.at + 1] = chunk;
            self.at += 2;
        }

        pub fn updatePicker(self: *@This(), colorPicker: *ColorPicker) void {
            colorPicker.state = self.state;
        }
    };
}
