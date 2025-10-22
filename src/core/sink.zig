const std = @import("std");
const units = @import("zpec").units;
const fs = @import("fs.zig");
const args = @import("args.zig");
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
                .colorGroupMathOnly,
                .colorMatch,
                => .{ .growing = units.CacheSize.L2 },
                else => .directWrite,
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
        colored: u16,
    };

    pub fn init(file: File, moreColors: bool) @This() {
        const config = std.Io.tty.Config.detect(file);
        return .{
            .config = config,
            .trueColor = moreColors and hasTrueColor(config),
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
        if (self.state == .reset) return;
        self.state = .reset;
        try self.config.setColor(w, .reset);
    }

    pub const RGB = struct {
        r: []const u8,
        g: []const u8,
        b: []const u8,

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

            return comptime &.{
                .r = hexStrToDecStr(hex[1..3]),
                .g = hexStrToDecStr(hex[3..5]),
                .b = hexStrToDecStr(hex[5..7]),
            };
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

    pub fn rainbowPick(index: u16) *const RGB {
        const bucketIndex: u8 = @intCast(index % 11);
        return switch (bucketIndex) {
            0 => .from("#FF0000"),
            1 => .from("#FFA500"),
            2 => .from("#FFFF00"),
            3 => .from("#FA8072"),
            4 => .from("#80FF00"),
            5 => .from("#00FF00"),
            6 => .from("#008080"),
            7 => .from("#99D9EA"),
            8 => .from("#0000FF"),
            9 => .from("#9400D3"),
            10 => .from("#8F00FF"),
            else => unreachable,
        };
    }

    pub fn pickColor(offset: u16, buff: []u8) Writer.Error![]const u8 {
        var w = Writer.fixed(buff);
        const rgb = rainbowPick(offset);
        try w.print("\x1b[38;2;{s};{s};{s}m", .{ rgb.r, rgb.g, rgb.b });
        return w.buffered();
    }

    pub fn setColor(self: *@This(), w: *Writer, offset: u16) std.Io.tty.Config.SetColorError!void {
        if ((self.config == .escape_codes and !self.trueColor) or (self.config == .no_color or self.config == .windows_api)) {
            switch (self.state) {
                .reset => {},
                .colored => |color| if (color == 0) return,
            }
            try self.config.setColor(w, .bright_red);
            self.state = .{ .colored = 0 };
        } else {
            switch (self.state) {
                .reset => {},
                .colored => |color| if (color == offset) return,
            }
            var buff: [19]u8 = undefined;
            try w.writeAll(try pickColor(offset, &buff));
            self.state = .{ .colored = offset };
        }
    }
};

pub const EventHandler = union(enum) {
    colorMatch: ColorPicker,
    colorGroupMathOnly: ColorPicker,
    skipLineOnMatch,
    pickNonMatchingLine,
};

pub fn pickEventHandler(fDetailed: *const fs.DetailedFile, argsRes: *const args.ArgsRes) EventHandler {
    if (argsRes.options.@"invert-match") return .pickNonMatchingLine;

    switch (fDetailed.fileType) {
        .tty => {
            if (argsRes.options.hasColor(fDetailed.fileType)) {
                const colorPicker: ColorPicker = .init(
                    fDetailed.file,
                    argsRes.options.@"more-colors",
                );
                if (argsRes.options.@"match-only") {
                    return .{ .colorGroupMathOnly = colorPicker };
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
    data: []const u8,
    groupN: u16,
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
                break :rv .{ .directWrite = fDetailed.file.writer(&.{}) };
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

    pub const ConsumeResponse = enum {
        eventSkipped,
        eventConsumed,
        lineConsumed,
    };

    pub fn consume(self: *@This(), event: Events) ConsumeError!ConsumeResponse {
        switch (self.eventHandler) {
            .colorMatch => |*colorPicker| {
                switch (event) {
                    .beforeMatch => |data| {
                        try self.sendResetColor(colorPicker);
                        try self.writeAll(data);
                        return .eventConsumed;
                    },
                    .match => |matchEvent| {
                        try self.sendColor(
                            colorPicker,
                            matchEvent.groupN,
                        );
                        try self.writeAll(matchEvent.data);
                        return .eventConsumed;
                    },
                    .noMatchEndOfLineAfterMatch => |data| {
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
            .colorGroupMathOnly => |*colorPicker| {
                switch (event) {
                    .beforeMatch,
                    .noMatchEndOfLineAfterMatch,
                    .noMatchEndOfLine,
                    => return .eventSkipped,
                    .match => |matchEvent| {
                        try self.sendColor(
                            colorPicker,
                            matchEvent.groupN,
                        );
                        var buff: [2][]const u8 = .{
                            matchEvent.data,
                            "\n",
                        };
                        try self.writeVecAll(&buff);
                        try self.sink();

                        return .eventConsumed;
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
