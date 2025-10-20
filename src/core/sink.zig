const std = @import("std");
const units = @import("zpec").units;
const fs = @import("fs.zig");
const File = std.fs.File;
const Writer = std.Io.Writer;

pub const Reporter = struct {
    stdoutW: *Writer = undefined,
    stderrW: *Writer = undefined,
};

pub const SinkBufferType = enum {
    heapGrowing,
    buffered,
    directWrite,
};

pub const SinkBuffer = union(SinkBufferType) {
    heapGrowing: usize,
    buffered: usize,
    directWrite,
};

pub fn pickSinkBuffer(fileType: fs.FileType, eventHandler: EventHandler) SinkBuffer {
    switch (fileType) {
        .tty => {
            return switch (eventHandler) {
                .colorMatch => .{ .heapGrowing = units.CacheSize.L3 },
                else => .directWrite,
            };
        },
        .generic,
        => return .directWrite,
        // TODO: check which i/os would benefit from buffered
        // could be tied to write strategy (line by line) vs chunked
        .characterDevice,
        .pipe,
        .file,
        => return .{ .buffered = units.CacheSize.L3 },
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
pub const HeapGrowingWriter = struct {
    allocating: *std.Io.Writer.Allocating,
    fdWriter: *std.fs.File.Writer,

    pub fn flush(self: *@This()) !void {
        const buff = self.allocating.writer.buffered();
        try self.fdWriter.interface.writeAll(buff);
        _ = self.allocating.writer.consumeAll();
    }

    pub fn writer(self: *@This()) *Writer {
        return &self.allocating.writer;
    }

    pub fn deinit(self: *@This()) void {
        self.allocating.deinit();
        self.allocating = undefined;
        self.fdWriter = undefined;
    }
};

pub const BufferOwnedWriter = struct {
    allocator: std.mem.Allocator,
    buff: []u8,
    writer: *Writer,

    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.buff);
        self.buff = undefined;
        self.writer = undefined;
    }
};

pub const SinkWriter = union(SinkBufferType) {
    heapGrowing: *HeapGrowingWriter,
    buffered: *BufferOwnedWriter,
    directWrite: *std.Io.Writer,
};

pub const Sink = struct {
    fileType: fs.FileType,
    eventHandler: EventHandler,
    // TODO: abstract coloring strategy
    colorEnabled: bool = false,
    sinkWriter: SinkWriter,

    pub fn sendColor(
        self: *const @This(),
        config: std.Io.tty.Config,
        color: std.Io.tty.Color,
    ) std.Io.tty.Config.SetColorError!void {
        switch (self.sinkWriter) {
            .heapGrowing => |alloc| try config.setColor(alloc.writer(), color),
            .buffered,
            .directWrite,
            => {},
        }
    }

    pub fn resetColor(
        self: *const @This(),
        config: std.Io.tty.Config,
    ) std.Io.tty.Config.SetColorError!void {
        if (self.colorEnabled) try self.sendColor(config, .reset);
    }

    pub fn reenableColor(
        self: *const @This(),
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

    pub fn writeAll(self: *const @This(), data: []const u8) Writer.Error!void {
        switch (self.sinkWriter) {
            .heapGrowing,
            => |w| {
                try w.writer().writeAll(data);
            },
            .buffered,
            => |w| {
                try w.writer.writeAll(data);
            },
            .directWrite,
            => |w| {
                try w.writeAll(data);
            },
        }
    }

    pub fn sinkLine(self: *const @This()) Writer.Error!void {
        // TODO: do I need a flush strategy too?
        switch (self.fileType) {
            .tty => {
                switch (self.sinkWriter) {
                    .heapGrowing => |alloc| try alloc.flush(),
                    .buffered,
                    .directWrite,
                    => {},
                }
            },
            else => {},
        }
    }

    pub fn sink(self: *const @This()) Writer.Error!void {
        switch (self.sinkWriter) {
            .heapGrowing => |w| try w.flush(),
            .buffered => |w| try w.writer.flush(),
            .directWrite => {},
        }
    }

    pub fn deinit(self: *@This()) void {
        switch (self.sinkWriter) {
            inline .buffered,
            .heapGrowing,
            => |w| w.deinit(),
            .directWrite,
            => {},
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
