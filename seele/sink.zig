const std = @import("std");
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
    matchInLine: ColorPicker,
    matchOnly: ColorPicker,
    groupOnly: GroupOnlyHelper,
    lineOnMatch: ColorPicker,
    nonMatchingLine: ColorPicker,
};

pub inline fn isEmptyGroup(group: *const regex.RegexMatchGroup) bool {
    return group.start == group.end;
}

pub inline fn isEmptyGroup0(group: *const regex.RegexMatchGroup) bool {
    return group.n == 0 and isEmptyGroup(group);
}

pub inline fn isEOL(group: *const regex.RegexMatchGroup, line: []const u8) bool {
    return group.start == line.len - 1;
}

pub inline fn isEOLBreakline(group: *const regex.RegexMatchGroup, line: []const u8) bool {
    return isEOL(group, line) and line[group.start] == '\n';
}

pub fn pickEventHandler(fDetailed: *const fs.DetailedFile, argsRes: *const args.ArgsRes) EventHandler {
    if (argsRes.options.@"invert-match") return .{ .nonMatchingLine = .init(.noColor) };

    switch (fDetailed.fileType) {
        .tty => {
            const colorPattern: tty.ColorPattern = if (argsRes.options.hasColor(fDetailed.fileType))
                .escape
            else
                .noColor;
            const colorPicker: ColorPicker = .init(colorPattern);

            if (argsRes.options.@"match-only") {
                return .{ .matchOnly = colorPicker };
            }

            if (argsRes.options.@"group-only") {
                return .{
                    .groupOnly = .{
                        .colorPicker = colorPicker,
                        .delimiter = argsRes.options.@"group-delimiter",
                    },
                };
            } else {
                return switch (colorPattern) {
                    .noColor => .{ .lineOnMatch = colorPicker },
                    else => .{ .matchInLine = colorPicker },
                };
            }
        },
        .generic,
        .characterDevice,
        .file,
        .pipe,
        => {
            if (argsRes.options.@"match-only") return .{ .matchOnly = .init(.noColor) };
            if (argsRes.options.@"group-only") return .{
                .groupOnly = .{
                    .colorPicker = .init(.noColor),
                    .delimiter = argsRes.options.@"group-delimiter",
                },
            };
            return .{ .lineOnMatch = .init(.noColor) };
        },
    }
    unreachable;
}

// TODO: handle pipe sizes adequatedly
// handle /dev/null as no-op write
pub fn pickSinkBuffer(fDetailed: *const fs.DetailedFile, eventHandler: EventHandler) SinkBuffer {
    switch (fDetailed.fileType) {
        .tty => {
            return switch (eventHandler) {
                .matchInLine,
                .matchOnly,
                .groupOnly,
                => .{ .growing = units.CacheSize.L2 },
                .lineOnMatch,
                .nonMatchingLine,
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

pub const EolEvent = struct {
    charOpt: ?u8,
    hadMatches: bool,
};

pub const GroupTracker = struct {
    colorPicker: ColorPicker,
    cursor: usize = 0,
    group0Event: ?SimpleMatchEvent = null,
};

pub const GroupOnlyHelper = struct {
    colorPicker: ColorPicker,
    delimiter: u8,
};

pub const MatchEvent = struct {
    line: []const u8,
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
            self.line,
            Event.SliceFormatMode.init(.{
                .start = self.group.start,
                .end = end,
            }),
        );

        try writer.print(" {f} .hasMore {any} {c}", .{
            self.group,
            self.hasMore,
            '}',
        });
    }
};

pub const SimpleMatchEvent = struct {
    line: []const u8,
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
            self.line,
            Event.SliceFormatMode.init(.{
                .start = self.group.start,
                .end = end,
            }),
        );

        try writer.print(" {f} {c}", .{ self.group, '}' });
    }
};

pub const BeforeGroup = struct {
    slice: []const u8,
    n: u16,
};

pub const EndOfGroups = struct {
    hadBreakline: bool,
    group0Empty: bool,
    sliceOpt: ?[]const u8,
};

// TODO: use pointers
pub const Event = union(enum) {
    emptyGroup: MatchEvent,
    excludedGroup: MatchEvent,
    beforeGroup: BeforeGroup,
    groupMatch: MatchEvent,
    afterMatch: []const u8,
    noMatch: []const u8,
    endOfGroups: EndOfGroups,
    eol: EolEvent,
    eof,

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.*) {
            .endOfGroups,
            => |endOfGroups| {
                try writer.print(".{s} {c} .sliceOpt ", .{ @tagName(self.*), '{' });
                if (endOfGroups.sliceOpt) |reminder| {
                    try formatSlice(writer, reminder, .noColor);
                } else try writer.writeAll("null");
                try writer.print(" .hadBreakline {any} .group0Empty {any} {c}", .{
                    endOfGroups.hadBreakline,
                    endOfGroups.group0Empty,
                    '}',
                });
            },
            .eol,
            => |eolEvent| {
                try writer.print(".{s}", .{@tagName(self.*)});
                if (eolEvent.charOpt) |char| {
                    try writer.print(" .charOpt {s}", .{Event.translateChar(char)});
                }
                try writer.print(" .hadMatches {any}", .{eolEvent.hadMatches});
            },
            .eof,
            => try writer.print(".{s}", .{@tagName(self.*)}),
            .afterMatch,
            .noMatch,
            => |s| {
                try writer.print(".{s} ", .{@tagName(self.*)});
                try formatSlice(writer, s, .noColor);
            },
            .beforeGroup,
            => |before| {
                try writer.print(".{s} {c} .slice ", .{ @tagName(self.*), '{' });
                try formatSlice(writer, before.slice, .noColor);
                try writer.print(" .n {d} {c}", .{ before.n, '}' });
            },
            inline else => |t| {
                try writer.writeAll(".");
                try writer.writeAll(@tagName(self.*));
                try writer.print(" {f} ", .{t});
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
                const detailedFile = fs.DetailedFile.from(std.fs.File.stderr()) catch
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
                inline else => |mark| try mark.markOpen(writer, i),
            }
        }

        pub fn markClose(
            self: *const @This(),
            writer: *Writer,
            i: usize,
        ) Writer.Error!void {
            switch (self.*) {
                .noColor => {},
                inline else => |mark| try mark.markClose(writer, i),
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
            else => &.{c},
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

pub const Mode = enum {
    trace,
    release,
};

// TODO: make interface call better
// should it be an actual vtable?
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
        skipped,
        consumed,
        consumedLine,
        cropLine,

        pub fn format(
            self: *const @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            switch (self.*) {
                inline else => try writer.print(".{s}", .{@tagName(self.*)}),
            }
        }
    };

    pub fn consume(self: *@This(), comptime mode: Mode, event: Event) ConsumeError!ConsumeResponse {
        if (comptime mode == .release) return try self.innerConsume(event);

        var colorPicker = switch (self.eventHandler) {
            .lineOnMatch,
            .matchInLine,
            .matchOnly,
            => |*colorPicker| colorPicker,
            .groupOnly,
            => |*groupOnlyHelper| &groupOnlyHelper.colorPicker,
            .nonMatchingLine,
            => rv: {
                var colorPicker: ColorPicker = .init(.noColor);
                break :rv &colorPicker;
            },
        };

        const state = colorPicker.state;
        try self.writeAll(colorPicker.reset());

        std.debug.print("---\n", .{});
        std.debug.print("In ┊ {f}\n", .{event});
        const result = self.innerConsume(event) catch |e| {
            std.debug.print("Err┊ {s}\n", .{@errorName(e)});
            return e;
        };
        std.debug.print("Out┊ {f}\n", .{result});

        switch (state) {
            .color => |c| try self.writeAll(colorPicker.pickColor(c)),
            else => {},
        }
        switch (event) {
            .eol,
            .eof,
            .noMatch,
            => std.debug.print("ˉˉˉ\n", .{}),
            else => {},
        }

        return result;
    }

    pub fn innerConsume(self: *@This(), event: Event) ConsumeError!ConsumeResponse {
        switch (self.eventHandler) {
            .matchInLine => |*colorPicker| return try MatchInLine.handle(
                self,
                colorPicker,
                event,
            ),
            .matchOnly => |*colorPicker| {
                return try MatchOnly.handle(
                    self,
                    colorPicker,
                    event,
                );
            },
            .groupOnly => |*groupOnlyHelper| {
                return try GroupOnly.handle(
                    self,
                    groupOnlyHelper,
                    event,
                );
            },
            .lineOnMatch => |*colorPicker| return try LineOnMatch.handle(
                self,
                colorPicker,
                event,
            ),
            .nonMatchingLine => |*colorPicker| return try NonMatchingLines.handle(
                self,
                colorPicker,
                event,
            ),
        }
    }
};

pub const GroupOnly = struct {
    pub inline fn handle(sink: *Sink, groupOnlyHelper: *GroupOnlyHelper, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        var colorPicker = &groupOnlyHelper.colorPicker;
        switch (event) {
            .afterMatch,
            .noMatch,
            .excludedGroup,
            .beforeGroup,
            => return .skipped,
            .emptyGroup,
            => |emptyEvent| {
                if (!emptyEvent.hasMore) return .skipped;

                var chunks = colorPicker.chunks(1, emptyEvent.group.n);
                chunks.clearChunk(&.{groupOnlyHelper.delimiter});

                try sink.writeVecAll(switch (chunks) {
                    inline else => |*c| &c.slices,
                });

                return .consumed;
            },
            .groupMatch => |matchEvent| {
                var chunks = colorPicker.chunks(
                    2,
                    matchEvent.group.n,
                );

                chunks.coloredChunk(matchEvent.group.slice(matchEvent.line));
                if (matchEvent.hasMore)
                    chunks.clearChunk(&.{groupOnlyHelper.delimiter})
                else
                    chunks.breakline();

                try sink.writeVecAll(switch (chunks) {
                    inline else => |*c| &c.slices,
                });

                return .consumed;
            },
            .endOfGroups,
            => return .skipped,
            .eol => {
                return try MatchOnly.endLine(sink, event);
            },
            .eof => {
                return try MatchOnly.endFileColored(sink, colorPicker);
            },
        }
    }
};

pub const MatchOnly = struct {
    pub inline fn handle(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        return switch (event) {
            .afterMatch,
            .noMatch,
            => return .skipped,
            .excludedGroup,
            => |excluded| {
                if (excluded.group.n == 0 and excluded.hasMore) return .cropLine;

                try MatchInLine.writeClear(
                    sink,
                    colorPicker,
                    excluded.group.slice(excluded.line),
                );
                return .consumed;
            },
            .beforeGroup,
            => |before| {
                if (before.n == 0) return .skipped;

                try MatchInLine.writeClear(
                    sink,
                    colorPicker,
                    before.slice,
                );
                return .consumed;
            },
            .endOfGroups,
            => {
                return try endOfGroupBreakline(sink, colorPicker, event);
            },
            .emptyGroup,
            => .skipped,
            .groupMatch,
            => |match| {
                try MatchInLine.writeColored(
                    sink,
                    colorPicker,
                    match.group.slice(match.line),
                    match.group.n,
                );
                return .consumed;
            },
            .eol,
            => {
                return try endLine(sink, event);
            },
            .eof,
            => {
                return try endFileColored(sink, colorPicker);
            },
        };
    }

    pub inline fn endOfGroupBreakline(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
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
            try sink.writeVecAll(switch (chunks) {
                inline else => |*c| &c.slices,
            });
            return .consumed;
        }

        try MatchInLine.breakline(
            sink,
            colorPicker,
        );
        return .consumed;
    }

    pub inline fn endLine(sink: *Sink, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .eol);
        const eolEvent = event.eol;

        if (eolEvent.hadMatches) {
            try sink.sinkLine();
            return .consumed;
        } else return .skipped;
    }

    pub inline fn endFileColored(sink: *Sink, colorPicker: *ColorPicker) Sink.ConsumeError!Sink.ConsumeResponse {
        try sink.writeAll(colorPicker.reset());
        try sink.sink();
        return .consumed;
    }
};

pub const NonMatchingLines = struct {
    pub inline fn handle(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        switch (event) {
            .emptyGroup,
            .excludedGroup,
            .beforeGroup,
            .groupMatch,
            .afterMatch,
            .endOfGroups,
            => return .consumedLine,
            .noMatch => |line| return LineOnMatch.consumeLine(
                sink,
                colorPicker,
                line,
            ),
            .eol => return try NonMatchingLines.endOfLine(
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

    pub inline fn endOfLine(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
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
    pub inline fn handle(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        switch (event) {
            .emptyGroup,
            .groupMatch,
            => |emptyMatch| return try LineOnMatch.consumeLine(
                sink,
                colorPicker,
                emptyMatch.line,
            ),
            // All groups regardless of exclusion should cause line to be consumed
            .excludedGroup,
            => |matchEvent| return try LineOnMatch.consumeLine(
                sink,
                colorPicker,
                matchEvent.line,
            ),
            .beforeGroup,
            .afterMatch,
            .noMatch,
            .endOfGroups,
            => return .skipped,
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

    pub inline fn consumeLine(sink: *Sink, colorPicker: *ColorPicker, slice: []const u8) Sink.ConsumeError!Sink.ConsumeResponse {
        try MatchInLine.writeClear(sink, colorPicker, slice);
        return .consumedLine;
    }
};

pub const MatchInLine = struct {
    pub inline fn handle(
        sink: *Sink,
        colorPicker: *ColorPicker,
        event: Event,
    ) Sink.ConsumeError!Sink.ConsumeResponse {
        switch (event) {
            .emptyGroup,
            => return try MatchInLine.emptyMatch(
                sink,
                colorPicker,
                event,
            ),
            .beforeGroup,
            => return try MatchInLine.beforeGroup(
                sink,
                colorPicker,
                event,
            ),
            .groupMatch => return try MatchInLine.groupMatch(
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
            .excludedGroup,
            .noMatch,
            .endOfGroups,
            => return .skipped,
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

    pub inline fn groupMatch(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .groupMatch);
        const matchEvent = event.groupMatch;

        try writeColored(
            sink,
            colorPicker,
            matchEvent.group.slice(matchEvent.line),
            matchEvent.group.n,
        );
        return .consumed;
    }

    pub inline fn beforeGroup(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .beforeGroup);
        const slice = event.beforeGroup.slice;

        try writeClear(sink, colorPicker, slice);
        return .consumed;
    }

    pub inline fn endOfFile(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .eof);

        try sink.writeAll(colorPicker.reset());
        try sink.sink();
        return .consumed;
    }

    pub inline fn endOfLine(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
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

    pub inline fn afterMatch(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .afterMatch);
        const slice = event.afterMatch;

        try writeClear(sink, colorPicker, slice);
        return .consumed;
    }

    // This consumed char at cursor, later
    // groupCursor is bumped to +1
    pub inline fn emptyMatch(sink: *Sink, colorPicker: *ColorPicker, event: Event) Sink.ConsumeError!Sink.ConsumeResponse {
        assert(event == .emptyGroup);
        const empty = event.emptyGroup;

        const group = empty.group;
        assert(group.start == group.end);
        const line = empty.line;

        try writeClear(sink, colorPicker, &.{line[group.start]});
        return .consumed;
    }

    pub inline fn writeColored(sink: *Sink, colorPicker: *ColorPicker, slice: []const u8, offset: u16) Sink.ConsumeError!void {
        var chunks = colorPicker.chunks(1, offset);
        chunks.coloredChunk(slice);
        try sink.writeVecAll(switch (chunks) {
            inline else => |*c| &c.slices,
        });
    }

    pub inline fn breakline(sink: *Sink, colorPicker: *ColorPicker) Sink.ConsumeError!void {
        var chunks = colorPicker.chunks(1, 0);
        chunks.breakline();
        try sink.writeVecAll(switch (chunks) {
            inline else => |*c| &c.slices,
        });
    }

    pub inline fn writeClear(sink: *Sink, colorPicker: *ColorPicker, slice: []const u8) Sink.ConsumeError!void {
        var chunks = colorPicker.chunks(1, 0);
        chunks.clearChunk(slice);
        try sink.writeVecAll(switch (chunks) {
            inline else => |*c| &c.slices,
        });
    }
};
