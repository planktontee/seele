const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const units = @import("regent").units;
const fs = @import("fs.zig");
const args = @import("args.zig");
const regex = @import("regex.zig");
const tty = @import("tty.zig");
const File = std.fs.File;
const Writer = std.Io.Writer;
const color = @import("color.zig");
const ColorPicker = color.ColorPicker;
const ColoredChunks = color.ColoredChunks;
const Context = @import("context.zig");

pub const SinkBufferT = @typeInfo(SinkBuffer).@"union".tag_type.?;

pub const SinkBuffer = union(enum) {
    growing: usize,
    buffered: usize,
    // This performs 2 syscall writes on the last line if there's no
    // \n in the line
    directWrite,
};

pub const EventHanderT = @typeInfo(EventHandler).@"union".tag_type.?;

// TODO: use pointers
pub const EventHandler = union(enum) {
    matchInLine: MatchInLine,
    matchOnly: MatchOnly,
    groupOnly: GroupOnly,
    lineOnMatch: LineOnMatch,
    nonMatchingLine: NonMatchingLine,

    pub fn handle(
        self: *@This(),
        sink: *Sink,
        event: Event,
    ) Sink.ConsumeError!Sink.ConsumeResponse {
        switch (self.*) {
            inline else => |*handler| return try handler.handle(sink, event),
        }
    }

    pub fn hasColor(self: *@This()) bool {
        return switch (self.*) {
            inline else => |*handler| handler.colorPicker.colorPattern != .noColor,
        };
    }

    pub fn showFileName(
        self: *@This(),
        input: *const fs.DetailedFile,
    ) void {
        switch (self.*) {
            inline else => |*handler| handler.fDetailed = input,
        }
    }

    pub fn restart(
        self: *@This(),
    ) void {
        switch (self.*) {
            inline else => |*handler| handler.restart(),
        }
    }

    pub fn stderrMessage(
        self: *@This(),
        vec: [][]const u8,
    ) Sink.ConsumeError!void {
        switch (self.*) {
            inline else => |*handler| {
                vec[0] = handler.colorPicker.reset();
                try Context.instance.stderrW.writeVecAll(vec);
            },
        }
    }

    pub fn skips(self: *const @This(), comptime eventT: EventT) bool {
        return switch (self.*) {
            inline else => |handler| @TypeOf(handler).skips(eventT),
        };
    }
};

pub fn pickEventHandler(fDetailed: *const fs.DetailedFile, argsRes: *const args.ArgsRes) EventHandler {
    const showLines = argsRes.options.@"line-number";

    if (argsRes.options.@"invert-match") return .{
        .nonMatchingLine = .init(showLines, .noColor),
    };

    switch (fDetailed.fileType) {
        .tty => {
            const colorPattern: tty.ColorPattern = if (argsRes.options.hasColor(fDetailed.fileType))
                .escape
            else
                .noColor;

            if (argsRes.options.@"match-only") {
                return .{ .matchOnly = .init(showLines, colorPattern) };
            }

            if (argsRes.options.@"group-only") {
                return .{
                    .groupOnly = .init(
                        argsRes.options.@"group-delimiter",
                        showLines,
                        colorPattern,
                    ),
                };
            } else {
                return switch (colorPattern) {
                    .noColor => .{ .lineOnMatch = .init(showLines, colorPattern) },
                    else => .{ .matchInLine = .init(showLines, colorPattern) },
                };
            }
        },
        .characterDevice,
        .file,
        .pipe,
        => {
            if (argsRes.options.@"match-only") return .{ .matchOnly = .init(showLines, .noColor) };
            if (argsRes.options.@"group-only") return .{
                .groupOnly = .init(
                    argsRes.options.@"group-delimiter",
                    showLines,
                    .noColor,
                ),
            };
            return .{ .lineOnMatch = .init(showLines, .noColor) };
        },
    }
    unreachable;
}

// TODO: handle pipe sizes adequatedly
// handle /dev/null as no-op write
pub fn pickSinkBuffer(
    fDetailed: *const fs.DetailedFile,
    eventHandler: EventHandler,
    showLineNumber: bool,
) SinkBuffer {
    switch (fDetailed.fileType) {
        .tty => {
            return switch (eventHandler) {
                .matchInLine,
                .matchOnly,
                .groupOnly,
                => .{ .growing = units.ByteUnit.mb },
                .lineOnMatch,
                .nonMatchingLine,
                => rv: {
                    // TODO: handle show file name
                    break :rv if (showLineNumber)
                        .{ .growing = units.ByteUnit.mb }
                    else
                        .directWrite;
                },
            };
        },
        .characterDevice,
        .pipe,
        .file,
        => return .{
            .buffered = units.ByteUnit.mb * 2,
        },
    }
    unreachable;
}

pub const GroupTracker = struct {
    colorPicker: ColorPicker,
    cursor: usize = 0,
    group0Event: ?SimpleMatchEvent = null,
};

pub const EolEvent = struct {
    charOpt: ?u8,
    hadMatches: bool,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(".{");
        if (self.charOpt) |char| {
            try writer.print(" .charOpt {s}", .{Event.translateChar(char)});
        }
        try writer.print(" .hadMatches {any} {c}", .{ self.hadMatches, '}' });
    }
};

pub const BaseEvent = struct {
    line: []const u8,
    lineN: usize,
    fDetailed: *const fs.DetailedFile,
};

pub const MatchEvent = struct {
    base: *const BaseEvent,
    group: regex.RegexMatchGroup,
    hasMore: bool,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(".{ .line ");
        const end = if (self.group.end > 0 and self.group.start != self.group.end)
            self.group.end - 1
        else
            self.group.end;
        try Event.formatSlice(
            writer,
            self.base.line,
            Event.SliceFormatMode.init(.{
                .start = self.group.start,
                .end = end,
            }),
        );

        try writer.print(" {f} .hasMore {any} .lineN {d} {c}", .{
            self.group,
            self.hasMore,
            self.base.lineN,
            '}',
        });
    }
};

pub const SimpleMatchEvent = struct {
    base: *const BaseEvent,
    group: regex.RegexMatchGroup,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(".{ .line ");
        const end = if (self.group.end > 0 and self.group.start != self.group.end)
            self.group.end - 1
        else
            self.group.end;
        try Event.formatSlice(
            writer,
            self.base.line,
            Event.SliceFormatMode.init(.{
                .start = self.group.start,
                .end = end,
            }),
        );

        try writer.print(" {f} .lineN {d} {c}", .{
            self.group,
            self.base.lineN,
            '}',
        });
    }
};

pub const BeforeGroup = struct {
    base: *const BaseEvent,
    slice: []const u8,
    n: u16,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(".{ .slice ");
        try Event.formatSlice(
            writer,
            self.slice,
            .noColor,
        );
        try writer.print(" .n {d} .lineN {d} {c}", .{ self.n, self.base.lineN, '}' });
    }
};

pub const EndOfGroups = struct {
    hadBreakline: bool,
    group0Empty: bool,
    sliceOpt: ?[]const u8,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(".{ .sliceOpt ");
        if (self.sliceOpt) |reminder| {
            try Event.formatSlice(writer, reminder, .noColor);
        } else try writer.writeAll("null");
        try writer.print(" .hadBreakline {any} .group0Empty {any} {c}", .{
            self.hadBreakline,
            self.group0Empty,
            '}',
        });
    }
};

pub const SliceEvent = struct {
    base: *const BaseEvent,
    slice: []const u8,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(".{ .slice ");
        try Event.formatSlice(
            writer,
            self.slice,
            .noColor,
        );

        try writer.print(" .lineN {d} {c}", .{
            self.base.lineN,
            '}',
        });
    }
};

pub const EventT = @typeInfo(Event).@"union".tag_type.?;

pub const Event = union(enum) {
    emptyGroup: *const MatchEvent,
    excludedGroup: *const MatchEvent,
    beforeGroup: *const BeforeGroup,
    groupMatch: *const MatchEvent,
    afterMatch: *const SliceEvent,
    noMatch: *const SliceEvent,
    endOfGroups: EndOfGroups,
    eol: EolEvent,
    eof,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.*) {
            .eof,
            => try writer.print(".{s}", .{@tagName(self.*)}),
            inline else => |t| {
                try writer.writeAll(".");
                try writer.writeAll(@tagName(self.*));
                try writer.print(" {f}", .{if (@typeInfo(@TypeOf(t)) == .pointer)
                    t.*
                else
                    t});
            },
        }
    }

    pub const SliceFormatMode = union(enum) {
        marked: CharMarked,
        colored: ColoredMark,
        noColor,

        const ColoredStub: @This() = .{
            .colored = .{
                .mark = undefined,
                .color = tty.RainbowColor.pick(11),
                .reset = tty.EscapeColor.reset.escapeCode(),
                .open = '<',
                .close = '>',
            },
        };

        const CharStub: @This() = .{
            .marked = .{
                .mark = undefined,
                .open = '<',
                .close = '>',
            },
        };

        pub fn init(mark: Mark) @This() {
            // NOTE: this will be incredibly wasteful
            // move to sink later as part of the trace mode
            const fileType: fs.FileType = rv: {
                const stderrFd = std.fs.File.stderr();
                const stat = stderrFd.stat() catch break :rv .file;
                const detailedFile = fs.DetailedFile.from(
                    stderrFd,
                    "",
                    "(stderr)",
                    &stat,
                ) catch
                    break :rv .file;
                break :rv detailedFile.fileType;
            };
            switch (fileType) {
                .tty => {
                    var mode = ColoredStub;
                    mode.colored.mark = mark;
                    return mode;
                },
                else => {
                    var mode = CharStub;
                    mode.marked.mark = mark;
                    return mode;
                },
            }
            unreachable;
        }

        pub const CharMarked = struct {
            mark: Mark,
            open: u8,
            close: u8,

            pub fn markOpen(
                self: *const @This(),
                writer: *Writer,
                i: usize,
            ) Writer.Error!void {
                if (i == self.mark.start) try writer.writeAll(&.{self.open});
            }

            pub fn markClose(
                self: *const @This(),
                writer: *Writer,
                i: usize,
            ) Writer.Error!void {
                if (i == self.mark.end) try writer.writeAll(&.{self.close});
            }
        };

        pub const ColoredMark = struct {
            mark: Mark,
            color: []const u8,
            reset: []const u8,
            open: u8,
            close: u8,

            pub fn markOpen(
                self: *const @This(),
                writer: *Writer,
                i: usize,
            ) Writer.Error!void {
                if (i == self.mark.start) {
                    var buff: [3][]const u8 = .{
                        self.color,
                        &.{self.open},
                        self.reset,
                    };
                    try writer.writeVecAll(&buff);
                }
            }

            pub fn markClose(
                self: *const @This(),
                writer: *Writer,
                i: usize,
            ) Writer.Error!void {
                if (i == self.mark.end) {
                    var buff: [3][]const u8 = .{
                        self.color,
                        &.{self.close},
                        self.reset,
                    };
                    try writer.writeVecAll(&buff);
                }
            }
        };

        pub const Mark = struct {
            start: usize,
            end: usize,
        };

        pub fn markOpen(
            self: *const @This(),
            writer: *Writer,
            i: usize,
        ) Writer.Error!void {
            switch (self.*) {
                .noColor => {},
                else => |mark| try mark.markOpen(writer, i),
            }
        }

        pub fn markClose(
            self: *const @This(),
            writer: *Writer,
            i: usize,
        ) Writer.Error!void {
            switch (self.*) {
                .noColor => {},
                else => |mark| try mark.markClose(writer, i),
            }
        }
    };

    pub fn formatSlice(
        writer: *Writer,
        slice: []const u8,
        mode: SliceFormatMode,
    ) Writer.Error!void {
        try writer.writeAll("⌈");
        for (slice, 0..) |c, i| {
            try mode.markOpen(writer, i);
            try writer.writeAll(translateChar(c));
            try mode.markClose(writer, i);
        }
        try writer.writeAll("⌋");
    }

    pub fn translateChar(c: u8) []const u8 {
        return switch (c) {
            '\n' => "␊",
            // NOTE: this is a precomputed table
            else => |preC| &.{preC},
        };
    }
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
        if (builtin.mode != .Debug) return;
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

pub const SinkWriter = union(SinkBufferT) {
    growing: *GrowingWriter,
    buffered: File.Writer,
    directWrite: File.Writer,

    pub const InitError = std.mem.Allocator.Error;

    pub fn init(
        fDetailed: *const fs.DetailedFile,
        allocator: std.mem.Allocator,
        eventHandler: EventHandler,
        showLineNumber: bool,
    ) InitError!@This() {
        const sinkBuffer = pickSinkBuffer(
            fDetailed,
            eventHandler,
            showLineNumber,
        );

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

pub const Mode = enum {
    trace,
    release,
};

pub const Sink = struct {
    fileType: fs.FileType,
    eventHandler: EventHandler,
    colorEnabled: bool = false,
    sinkWriter: SinkWriter,

    pub fn init(
        fDetailed: *const fs.DetailedFile,
        allocator: std.mem.Allocator,
        argsRes: *const args.ArgsRes,
    ) SinkWriter.InitError!@This() {
        const eventHandler = pickEventHandler(
            fDetailed,
            argsRes,
        );
        return .{
            .fileType = fDetailed.fileType,
            .eventHandler = eventHandler,
            .sinkWriter = try .init(
                fDetailed,
                allocator,
                eventHandler,
                argsRes.options.@"line-number",
            ),
        };
    }

    pub const SinkWriteError = error{
        BadUTF8Encoding,
    } ||
        Writer.Error;

    pub fn writeVecAll(self: *@This(), data: [][]const u8, comptime validate: bool) SinkWriteError!void {
        if (comptime validate) {
            const hasColor = self.eventHandler.hasColor();
            var i: usize = data.len;

            while (i > 0) {
                if (!std.unicode.utf8ValidateSlice(data[i - 1])) return SinkWriteError.BadUTF8Encoding;
                i -= if (hasColor) rv: {
                    break :rv if (i >= 2) 2 else 1;
                } else 1;
            }
        }

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
                if (builtin.mode != .Debug) return;
                if (w.interface.buffer.len > 0) {
                    allocator.free(w.interface.buffer);
                }
            },
        }
    }

    pub const ConsumeError = error{} ||
        SinkWriteError ||
        std.fs.File.WriteError ||
        Writer.Error;

    pub const ConsumeResponse = union(enum) {
        skipped,
        consumed,
        consumedLine,
        cropLine,

        pub fn format(
            self: *const @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            switch (self.*) {
                else => try writer.print(".{s}", .{@tagName(self.*)}),
            }
        }
    };

    pub fn consume(self: *@This(), comptime mode: Mode, event: Event) ConsumeError!ConsumeResponse {
        if (comptime mode == .release) {
            return self.innerConsume(event) catch |e| switch (e) {
                SinkWriteError.BadUTF8Encoding => {
                    _ = try self.innerConsume(.eof);
                    return e;
                },
                else => return e,
            };
        }

        var colorPicker = switch (self.eventHandler) {
            inline else => |*handler| &handler.colorPicker,
        };

        var stderrW = Context.instance.stderrW;

        try stderrW.writeAll(colorPicker.colorPattern.reset());
        try stderrW.writeAll("---\n");
        try stderrW.print("In ┊ {f}\n", .{event});

        // We need to ensure colors make sense always
        try stderrW.flush();

        const result = self.innerConsume(event) catch |e| {
            try stderrW.writeAll(colorPicker.colorPattern.reset());
            try stderrW.print("Err┊ {s}\n", .{@errorName(e)});

            try stderrW.flush();
            return e;
        };

        try stderrW.writeAll(colorPicker.colorPattern.reset());
        try stderrW.print("Out┊ {f}\n", .{result});

        switch (event) {
            .eol,
            .eof,
            .noMatch,
            => try stderrW.writeAll("ˉˉˉ\n"),
            else => {},
        }

        try stderrW.flush();

        return result;
    }

    pub fn innerConsume(self: *@This(), event: Event) ConsumeError!ConsumeResponse {
        return try self.eventHandler.handle(self, event);
    }
};

pub const GroupOnly = struct {
    colorPicker: ColorPicker,
    delimiter: []const u8,
    writeLine: bool = true,
    showLines: bool,
    fDetailed: ?*const fs.DetailedFile = null,
    lastLine: usize = 0,

    pub fn init(delimiter: u8, showLines: bool, pattern: tty.ColorPattern) @This() {
        return .{
            .delimiter = switch (delimiter) {
                inline else => |c| &.{c},
            },
            .showLines = showLines,
            .colorPicker = .init(pattern),
        };
    }

    pub fn restart(self: *@This()) void {
        self.lastLine = 0;
        self.writeLine = true;
    }

    pub fn prefixDelimiter(self: *const @This()) []const u8 {
        return self.delimiter;
    }

    pub fn skips(comptime eventT: EventT) bool {
        return switch (eventT) {
            .afterMatch,
            .noMatch,
            .excludedGroup,
            .beforeGroup,
            => true,
            else => false,
        };
    }

    pub fn handle(self: *@This(), sink: *Sink, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        var colorPicker = &self.colorPicker;
        switch (event) {
            .afterMatch,
            .noMatch,
            .excludedGroup,
            .beforeGroup,
            => return .skipped,
            .emptyGroup,
            => |emptyEvent| {
                if (!emptyEvent.hasMore) return .skipped;

                if (self.writeLine)
                    try MatchOnly.showPrefixes(
                        self,
                        sink,
                        colorPicker,
                        emptyEvent.base.lineN,
                    );

                var chunks = colorPicker.chunks(1, emptyEvent.group.n);
                chunks.clearChunk(self.delimiter);
                try sink.writeVecAll(chunks.slices(), false);

                return .consumed;
            },
            .groupMatch => |matchEvent| {
                if (self.writeLine)
                    try MatchOnly.showPrefixes(
                        self,
                        sink,
                        colorPicker,
                        matchEvent.base.lineN,
                    );

                var chunks = colorPicker.chunks(
                    2,
                    matchEvent.group.n,
                );

                chunks.coloredChunk(matchEvent.group.slice(matchEvent.base.line));
                if (matchEvent.hasMore)
                    chunks.clearChunk(self.delimiter)
                else
                    chunks.breakline();

                try sink.writeVecAll(chunks.slices(), true);

                return .consumed;
            },
            .endOfGroups,
            => {
                self.writeLine = true;
                return .skipped;
            },
            .eol,
            => return try MatchOnly.endLine(sink, event),
            .eof,
            => return try MatchInLine.endOfFile(sink, colorPicker, event),
        }
    }
};

pub const MatchOnly = struct {
    colorPicker: ColorPicker,
    showLines: bool,
    fDetailed: ?*const fs.DetailedFile = null,
    writeLine: bool = true,
    lastLine: usize = 0,

    pub fn init(showLines: bool, pattern: tty.ColorPattern) @This() {
        return .{
            .showLines = showLines,
            .colorPicker = .init(pattern),
        };
    }

    pub fn prefixDelimiter(_: *const @This()) []const u8 {
        return SHOW_LINE_DELIMITER;
    }

    pub fn restart(self: *@This()) void {
        self.lastLine = 0;
        self.writeLine = true;
    }

    pub fn skips(comptime eventT: EventT) bool {
        return switch (eventT) {
            .afterMatch,
            .noMatch,
            .emptyGroup,
            => true,
            else => false,
        };
    }

    pub fn handle(self: *@This(), sink: *Sink, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        var colorPicker = &self.colorPicker;
        _ = &colorPicker;
        return switch (event) {
            .afterMatch,
            .noMatch,
            .emptyGroup,
            => return .skipped,
            .excludedGroup,
            => |excluded| {
                if (excluded.group.n == 0 and excluded.hasMore) return .cropLine;

                if (self.writeLine)
                    try self.showPrefixes(sink, colorPicker, excluded.base.lineN);

                try MatchInLine.writeClear(
                    sink,
                    colorPicker,
                    excluded.group.slice(excluded.base.line),
                );
                return .consumed;
            },
            .beforeGroup,
            => |before| {
                if (before.n == 0) return .skipped;

                if (self.writeLine)
                    try self.showPrefixes(sink, colorPicker, before.base.lineN);

                try MatchInLine.writeClear(
                    sink,
                    colorPicker,
                    before.slice,
                );
                return .consumed;
            },
            .endOfGroups,
            => {
                self.writeLine = true;
                return try endOfGroupBreakline(
                    sink,
                    colorPicker,
                    event,
                );
            },
            .groupMatch,
            => |match| {
                if (self.writeLine or match.group.n == 0)
                    try self.showPrefixes(sink, colorPicker, match.base.lineN);

                try MatchInLine.writeColored(
                    sink,
                    colorPicker,
                    match.group.slice(match.base.line),
                    match.group.n,
                );
                return .consumed;
            },
            .eol,
            => return try endLine(sink, event),
            .eof,
            => return try MatchInLine.endOfFile(sink, colorPicker, event),
        };
    }

    // BUG: because this is split from line validation, we may write this and skip
    // writing the actual data, resulting in an empty line
    pub fn showPrefixes(self: anytype, sink: *Sink, colorPicker: *ColorPicker, lineN: usize) Sink.ConsumeError!void {
        self.lastLine = 0;
        try MatchInLine.showPrefixes(
            self,
            sink,
            colorPicker,
            lineN,
        );
        self.writeLine = false;
    }

    pub fn endOfGroupBreakline(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .endOfGroups);
        const endOfGroups = event.endOfGroups;

        if (endOfGroups.group0Empty) return .skipped;
        if (endOfGroups.hadBreakline) return .skipped;

        if (endOfGroups.sliceOpt) |slice| {
            var chunks = colorPicker.chunks(2, 0);
            chunks.clearChunk(slice);
            if (slice[slice.len - 1] == '\n')
                chunks.skipChunk()
            else
                chunks.breakline();
            try sink.writeVecAll(chunks.slices(), true);
            return .consumed;
        }

        try MatchInLine.breakline(
            sink,
            colorPicker,
        );
        return .consumed;
    }

    pub fn endLine(sink: *Sink, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .eol);
        const eolEvent = event.eol;

        if (eolEvent.hadMatches) {
            try sink.sinkLine();
            return .consumed;
        } else return .skipped;
    }
};

pub const NonMatchingLine = struct {
    colorPicker: ColorPicker,
    showLines: bool,
    fDetailed: ?*const fs.DetailedFile = null,
    lastLine: usize = 0,

    pub fn init(showLines: bool, pattern: tty.ColorPattern) @This() {
        return .{
            .showLines = showLines,
            .colorPicker = .init(pattern),
        };
    }

    pub fn prefixDelimiter(_: *const @This()) []const u8 {
        return SHOW_LINE_DELIMITER;
    }

    pub fn restart(self: *@This()) void {
        self.lastLine = 0;
    }

    pub fn skips(comptime eventT: EventT) bool {
        return switch (eventT) {
            else => false,
        };
    }

    pub fn handle(self: *@This(), sink: *Sink, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        var colorPicker = &self.colorPicker;
        _ = &colorPicker;
        switch (event) {
            .emptyGroup,
            .excludedGroup,
            .beforeGroup,
            .groupMatch,
            .afterMatch,
            .endOfGroups,
            => return .consumedLine,
            .noMatch => |lineEvent| return LineOnMatch.consumeLine(
                self,
                sink,
                colorPicker,
                lineEvent.slice,
                lineEvent.base.lineN,
            ),
            .eol => return try NonMatchingLine.endOfLine(
                sink,
                colorPicker,
                event,
            ),
            .eof => return try MatchInLine.endOfFile(
                sink,
                colorPicker,
                event,
            ),
        }
    }

    pub fn endOfLine(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .eol);
        const eolEvent = event.eol;

        if (!eolEvent.hadMatches) {
            // Line has no breakline
            if (eolEvent.charOpt) |char| {
                assert(char == '\n');
                try MatchInLine.breakline(sink, colorPicker);
            }
            try sink.sinkLine();
            return .consumed;
        } else return .skipped;
    }
};

pub const LineOnMatch = struct {
    colorPicker: ColorPicker,
    showLines: bool,
    fDetailed: ?*const fs.DetailedFile = null,
    lastLine: usize = 0,

    pub fn init(showLines: bool, pattern: tty.ColorPattern) @This() {
        return .{
            .showLines = showLines,
            .colorPicker = .init(pattern),
        };
    }

    pub fn prefixDelimiter(_: *const @This()) []const u8 {
        return SHOW_LINE_DELIMITER;
    }

    pub fn restart(self: *@This()) void {
        self.lastLine = 0;
    }

    pub fn skips(comptime eventT: EventT) bool {
        return switch (eventT) {
            .beforeGroup,
            .afterMatch,
            .noMatch,
            .endOfGroups,
            => true,
            else => false,
        };
    }

    pub fn handle(self: *@This(), sink: *Sink, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        var colorPicker = &self.colorPicker;
        _ = &colorPicker;
        switch (event) {
            .beforeGroup,
            .afterMatch,
            .noMatch,
            .endOfGroups,
            => return .skipped,
            .emptyGroup,
            .groupMatch,
            => |emptyMatch| return try self.consumeLine(
                sink,
                colorPicker,
                emptyMatch.base.line,
                emptyMatch.base.lineN,
            ),
            // All groups regardless of exclusion should cause line to be consumed
            .excludedGroup,
            => |matchEvent| return try self.consumeLine(
                sink,
                colorPicker,
                matchEvent.base.line,
                matchEvent.base.lineN,
            ),
            .eol => return try MatchInLine.endOfLine(
                sink,
                colorPicker,
                event,
            ),
            .eof => return try MatchInLine.endOfFile(
                sink,
                colorPicker,
                event,
            ),
        }
    }

    pub fn consumeLine(
        self: anytype,
        sink: *Sink,
        colorPicker: *ColorPicker,
        slice: []const u8,
        lineN: usize,
    ) Sink.ConsumeError!Sink.ConsumeResponse {
        try MatchInLine.showPrefixes(self, sink, colorPicker, lineN);
        try MatchInLine.writeClear(sink, colorPicker, slice);
        return .consumedLine;
    }
};

pub const MatchInLine = struct {
    colorPicker: ColorPicker,
    showLines: bool,
    fDetailed: ?*const fs.DetailedFile = null,
    lastLine: usize = 0,

    pub fn init(showLines: bool, pattern: tty.ColorPattern) @This() {
        return .{
            .showLines = showLines,
            .colorPicker = .init(pattern),
        };
    }

    pub fn prefixDelimiter(_: *const @This()) []const u8 {
        return SHOW_LINE_DELIMITER;
    }

    pub fn restart(self: *@This()) void {
        self.lastLine = 0;
    }

    pub fn skips(comptime eventT: EventT) bool {
        return switch (eventT) {
            .excludedGroup,
            .noMatch,
            => true,
            else => false,
        };
    }

    pub fn handle(self: *@This(), sink: *Sink, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        var colorPicker = &self.colorPicker;
        _ = &colorPicker;
        switch (event) {
            .excludedGroup,
            .noMatch,
            => return .skipped,
            .emptyGroup,
            => {
                return try MatchInLine.emptyMatch(
                    sink,
                    colorPicker,
                    event,
                );
            },
            .beforeGroup,
            => return try self.beforeGroup(
                sink,
                colorPicker,
                event,
            ),
            .groupMatch,
            => return try self.groupMatch(
                sink,
                colorPicker,
                event,
            ),
            .afterMatch,
            => return try MatchInLine.afterMatch(
                sink,
                colorPicker,
                event,
            ),
            .endOfGroups,
            => return try self.consumeEndOfGroups(sink, colorPicker, event),
            .eol,
            => return try MatchInLine.endOfLine(
                sink,
                colorPicker,
                event,
            ),
            .eof,
            => return try MatchInLine.endOfFile(
                sink,
                colorPicker,
                event,
            ),
        }
    }

    pub fn consumeEndOfGroups(_: anytype, sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .endOfGroups);
        const endOfGroups = event.endOfGroups;

        if (endOfGroups.group0Empty) return .skipped;
        if (endOfGroups.hadBreakline) return .skipped;

        if (endOfGroups.sliceOpt) |slice| {
            var chunks = colorPicker.chunks(1, 0);
            chunks.clearChunk(slice);
            try sink.writeVecAll(chunks.slices(), true);
            return .consumed;
        }

        return .skipped;
    }

    pub fn groupMatch(self: anytype, sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .groupMatch);
        const matchEvent = event.groupMatch;

        try self.showPrefixes(sink, colorPicker, matchEvent.base.lineN);
        try writeColored(
            sink,
            colorPicker,
            matchEvent.group.slice(matchEvent.base.line),
            matchEvent.group.n,
        );
        return .consumed;
    }

    pub fn beforeGroup(self: anytype, sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .beforeGroup);
        const beforeGroupEvent = event.beforeGroup;
        const slice = beforeGroupEvent.slice;

        try self.showPrefixes(sink, colorPicker, beforeGroupEvent.base.lineN);
        try writeClear(sink, colorPicker, slice);

        return .consumed;
    }

    pub fn showPrefixes(self: anytype, sink: *Sink, colorPicker: *ColorPicker, lineN: usize) Sink.ConsumeError!void {
        // NOTE: this is fine because we move away from direct io in case showLines is used
        if (self.lastLine != lineN) {
            if (self.fDetailed) |fDetailed| {
                var chunks = colorPicker.chunks(4, 0);
                chunks.forceColoredChunk(
                    tty.EscapeColor.magenta.escapeCode(),
                    fDetailed.path[0],
                );
                chunks.forceChunk(fDetailed.path[1]);
                chunks.forceChunk(fDetailed.path[2]);
                chunks.forceColoredChunk(tty.EscapeColor.reset.escapeCode(), self.prefixDelimiter());
                _ = colorPicker.reset();

                try sink.writeVecAll(chunks.slices(), false);
                self.lastLine = lineN;
            }

            if (self.showLines) {
                var chunks = colorPicker.chunks(2, 0);
                const buffLen = comptime std.fmt.count("{d}", .{std.math.maxInt(usize)});
                var buff: [buffLen]u8 = undefined;
                const lineNStr = try std.fmt.bufPrint(&buff, "{d}", .{lineN});
                chunks.forceColoredChunk(tty.EscapeColor.green.escapeCode(), lineNStr);
                chunks.forceColoredChunk(tty.EscapeColor.reset.escapeCode(), self.prefixDelimiter());
                _ = colorPicker.reset();

                try sink.writeVecAll(chunks.slices(), false);
                self.lastLine = lineN;
            }
        }
    }

    pub fn endOfFile(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .eof);

        var buff: [1][]const u8 = .{colorPicker.reset()};
        try sink.writeVecAll(&buff, false);
        try sink.sink();
        return .consumed;
    }

    pub fn endOfLine(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .eol);
        const eolEvent = event.eol;

        if (eolEvent.hadMatches) {
            // Line has no breakline
            if (eolEvent.charOpt) |char| {
                assert(char == '\n');
                try breakline(sink, colorPicker);
            }
            try sink.sinkLine();
            return .consumed;
        } else return .skipped;
    }

    pub fn afterMatch(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .afterMatch);
        const lineEvent = event.afterMatch;

        try writeClear(sink, colorPicker, lineEvent.slice);
        return .consumed;
    }

    // This consumed char at cursor, later
    // groupCursor is bumped to +1
    pub fn emptyMatch(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .emptyGroup);
        const empty = event.emptyGroup;

        const group = empty.group;
        assert(group.start == group.end);
        const line = empty.base.line;

        try writeClear(sink, colorPicker, &.{line[group.start]});
        return .consumed;
    }

    pub fn writeColored(sink: *Sink, colorPicker: *ColorPicker, slice: []const u8, offset: u16) Sink.ConsumeError!void {
        var chunks = colorPicker.chunks(1, offset);
        chunks.coloredChunk(slice);
        try sink.writeVecAll(chunks.slices(), true);
    }

    pub fn breakline(sink: *Sink, colorPicker: *ColorPicker) Sink.ConsumeError!void {
        var chunks = colorPicker.chunks(1, 0);
        chunks.breakline();
        try sink.writeVecAll(chunks.slices(), false);
    }

    pub fn writeClear(sink: *Sink, colorPicker: *ColorPicker, slice: []const u8) Sink.ConsumeError!void {
        var chunks = colorPicker.chunks(1, 0);
        chunks.clearChunk(slice);
        try sink.writeVecAll(chunks.slices(), true);
    }
};

const SHOW_LINE_DELIMITER = ":";
