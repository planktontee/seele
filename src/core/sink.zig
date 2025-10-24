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
    comptime colorPattern: ColorPattern = .escape,
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

    pub const ColorPattern = enum {
        rainbow,
        escape,

        pub fn pick(comptime self: @This(), offset: u16) []const u8 {
            return (switch (self) {
                .rainbow => RainbowColor,
                .escape => EscapeColor,
            }).pick(offset);
        }

        pub fn reset(comptime self: @This()) []const u8 {
            return (switch (self) {
                .rainbow => RainbowColor,
                .escape => EscapeColor,
            }).resetCode();
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

    pub fn reset(self: *@This()) []const u8 {
        if (self.config == .no_color) return "";
        if (!self.state.eql(.reset)) {
            self.state = .reset;
            return EscapeColor.reset.escapeCode();
        }
        return "";
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

    fn useConfigColoring(self: *const @This()) bool {
        return (self.config == .escape_codes and !self.trueColor) or
            self.config == .windows_api;
    }

    pub fn pickColor(self: *@This(), offset: u16) []const u8 {
        if (self.config == .no_color) return "";
        if (self.useConfigColoring()) {
            const target: State = .{ .color = 0 };
            if (!self.state.eql(target)) {
                self.state = target;
                return EscapeColor.boldRed.escapeCode();
            }
            return "";
        } else {
            const target: State = .{ .color = offset };
            if (!self.state.eql(target)) {
                self.state = target;
                return self.colorPattern.pick(offset);
            }
            return "";
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
        eventPostponed,
        eventPostponedFinished: usize,
        lineConsumed,
    };

    pub fn consume(self: *@This(), event: Events) ConsumeError!ConsumeResponse {
        switch (self.eventHandler) {
            .colorMatch => |*colorPicker| {
                switch (event) {
                    .match => |matchEvent| {
                        var cChunks = ColoredChunks(2).init(
                            colorPicker,
                            @intCast(matchEvent.group.n),
                        );
                        cChunks.coloredChunk(matchEvent.group.slice(matchEvent.line));
                        try self.writeVecAll(&cChunks.chunks);
                        return .eventConsumed;
                    },
                    .beforeMatch,
                    .noMatchEndOfLineAfterMatch,
                    => |data| {
                        var cChunks = ColoredChunks(2).init(
                            colorPicker,
                            @intCast(0),
                        );
                        cChunks.clearChunk(data);
                        try self.writeVecAll(&cChunks.chunks);
                        return .eventConsumed;
                    },
                    .noMatchEndOfLine => {
                        return .eventSkipped;
                    },
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

                            if (groupTracker.groupHighlight and matchEvent.targetGroup.anyOtherThan(0)) {
                                return .eventPostponed;
                            }

                            var cChunks = ColoredChunks(4).init(
                                &groupTracker.colorPicker,
                                @intCast(matchEvent.group.n),
                            );

                            cChunks.coloredChunk(matchEvent.group.slice(matchEvent.line));
                            cChunks.breakline();
                            try self.writeVecAll(&cChunks.chunks);

                            return .eventConsumed;
                        } else {
                            std.debug.assert(groupTracker.group0 != null);

                            const group0 = groupTracker.group0.?;
                            const line = matchEvent.line;
                            const group = matchEvent.group;
                            const cursor = groupTracker.cursor;
                            const endChunk = !matchEvent.targetGroup.anyOtherThan(group.n) or
                                matchEvent.count - 1 == group.n;

                            var cChunks = ColoredChunks(8).init(
                                &groupTracker.colorPicker,
                                @intCast(group.n),
                            );

                            // INFO: Grammar for pieces:
                            // <reset color (if needed)> <before match if any>
                            // <match color (if needed)> <match>
                            // <reset color (if needed)> <post match if and and last group>
                            // <breakline> (if needed)
                            if (cursor < group.start) {
                                cChunks.clearChunk(line[cursor..group.start]);
                            } else cChunks.skipChunk();

                            cChunks.coloredChunk(group.slice(line));

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
                                return .{ .eventPostponedFinished = group0.end };
                            } else return .eventConsumed;
                        }
                    },
                    .endOfLine => {
                        try self.sinkLine();
                        groupTracker.cursor = 0;
                        groupTracker.group0 = null;
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.writeAll(groupTracker.colorPicker.reset());
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
