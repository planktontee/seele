const std = @import("std");
const units = @import("zpec").units;
const fs = @import("fs.zig");
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

pub fn pickSinkBuffer(fileType: fs.FileType, eventHandler: EventHandler) SinkBuffer {
    switch (fileType) {
        .tty => {
            return switch (eventHandler) {
                .colorMatch => .{ .growing = units.CacheSize.L2 },
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

pub const EventHandler = union(enum) {
    colorMatch: std.Io.tty.Config,
    skipLineOnMatch,
};

pub fn pickEventHandler(fileType: fs.FileType, file: File, colored: bool) EventHandler {
    switch (fileType) {
        .tty => {
            if (colored) {
                return .{ .colorMatch = .detect(file) };
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

    pub const InitError = std.mem.Allocator.Error ||
        fs.DetectTypeError;

    pub fn init(file: File, allocator: std.mem.Allocator, hasColor: bool) InitError!@This() {
        const outputFileType = try fs.detectType(file);
        const outputAccessType = fs.detectAccessType(outputFileType);
        const eventHandler = pickEventHandler(outputFileType, file, hasColor);
        const sinkBuffer = pickSinkBuffer(outputFileType, eventHandler);

        return switch (sinkBuffer) {
            .growing => |size| rv: {
                const growing = try allocator.create(GrowingWriter);
                errdefer allocator.destroy(growing);

                const allocating = try std.Io.Writer.Allocating.initCapacity(allocator, size);
                const writer = switch (outputAccessType) {
                    .streaming => file.writerStreaming(&.{}),
                    .positional => file.writer(&.{}),
                };

                growing.* = .{
                    .allocating = allocating,
                    .fdWriter = writer,
                };
                break :rv .{ .growing = growing };
            },
            .buffered => |size| rv: {
                const buff = try allocator.alloc(u8, size);
                const writer = switch (outputAccessType) {
                    .streaming => file.writerStreaming(buff),
                    .positional => file.writer(buff),
                };
                break :rv .{ .buffered = writer };
            },
            .directWrite => rv: {
                break :rv .{ .directWrite = file.writer(&.{}) };
            },
        };
    }
};

pub const Sink = struct {
    fileType: fs.FileType,
    eventHandler: EventHandler,
    // TODO: abstract coloring strategy
    colorEnabled: bool = false,
    sinkWriter: SinkWriter,

    pub fn init(file: File, allocator: std.mem.Allocator, hasColor: bool) SinkWriter.InitError!@This() {
        const fileType = try fs.detectType(file);
        return .{
            .fileType = fileType,
            .eventHandler = pickEventHandler(fileType, file, hasColor),
            .sinkWriter = try .init(file, allocator, hasColor),
        };
    }

    pub fn sendColor(
        self: *@This(),
        config: std.Io.tty.Config,
        color: std.Io.tty.Color,
    ) std.Io.tty.Config.SetColorError!void {
        switch (self.sinkWriter) {
            .growing => |w| try config.setColor(w.writer(), color),
            .buffered,
            .directWrite,
            => |*w| try config.setColor(&w.interface, color),
        }
    }

    pub fn resetColor(
        self: *@This(),
        config: std.Io.tty.Config,
    ) std.Io.tty.Config.SetColorError!void {
        if (self.colorEnabled) try self.sendColor(config, .reset);
    }

    pub fn reenableColor(
        self: *@This(),
        config: std.Io.tty.Config,
    ) std.Io.tty.Config.SetColorError!void {
        if (self.colorEnabled) try self.sendColor(config, .bright_red);
    }

    pub fn enableColor(self: *@This(), config: std.Io.tty.Config) std.Io.tty.Config.SetColorError!void {
        if (!self.colorEnabled) {
            self.colorEnabled = true;
            try self.reenableColor(config);
        }
    }

    pub fn disableColor(self: *@This(), config: std.Io.tty.Config) std.Io.tty.Config.SetColorError!void {
        if (self.colorEnabled) {
            try self.resetColor(config);
            self.colorEnabled = false;
        }
    }

    pub fn ttyReset(self: *@This()) std.Io.tty.Config.SetColorError!void {
        switch (self.eventHandler) {
            .colorMatch => |config| {
                try self.disableColor(config);
            },
            else => {},
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
            .colorMatch => |config| {
                switch (event) {
                    .beforeMatch => |data| {
                        try self.resetColor(config);
                        try self.writeAll(data);
                        try self.reenableColor(config);
                        return .eventConsumed;
                    },
                    .match => |matchEvent| {
                        try self.enableColor(config);
                        try self.writeAll(matchEvent.data);
                        return .eventConsumed;
                    },
                    .noMatchEndOfLineAfterMatch => |data| {
                        try self.disableColor(config);
                        try self.writeAll(data);
                        return .eventConsumed;
                    },
                    .noMatchEndOfLine => {
                        try self.disableColor(config);
                        return .eventSkipped;
                    },
                    .endOfLine => {
                        try self.sinkLine();
                        return .eventConsumed;
                    },
                    .endOfFile => {
                        try self.disableColor(config);
                        try self.sink();
                        return .eventConsumed;
                    },
                }
            },
            .skipLineOnMatch => {
                switch (event) {
                    // Generic events are silenced in this case
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
        }
    }
};
