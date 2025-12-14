const std = @import("std");
const assert = std.debug.assert;
const units = @import("regent").units;
const tty = @import("tty.zig");
const Context = @import("context.zig");

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

    pub fn chunks(self: *@This(), comptime size: usize, offset: u16) Chunks {
        Context.slice(switch (self.colorPattern) {
            .noColor => size,
            else => size * 2,
        });

        return Chunks.init(
            self,
            offset,
        );
    }
};

pub const ColoredChunks = struct {
    colorOffset: u16,
    at: usize = 0,
    colorPicker: *ColorPicker,

    pub fn init(
        colorPicker: *ColorPicker,
        offset: u16,
    ) @This() {
        return .{
            .colorPicker = colorPicker,
            .colorOffset = offset,
        };
    }

    pub fn clearChunk(self: *@This(), chunk: []const u8) void {
        assert(self.at + 2 <= Context.slices.len);
        if (chunk.len > 0) {
            Context.slices[self.at] = self.colorPicker.reset();
            Context.slices[self.at + 1] = chunk;
        } else {
            Context.slices[self.at] = "";
            Context.slices[self.at + 1] = "";
        }
        self.at += 2;
    }

    pub fn coloredChunk(self: *@This(), chunk: []const u8) void {
        assert(self.at + 2 <= Context.slices.len);
        if (chunk.len > 0) {
            Context.slices[self.at] = self.colorPicker.pickColor(self.colorOffset);
            Context.slices[self.at + 1] = chunk;
        } else {
            Context.slices[self.at] = "";
            Context.slices[self.at + 1] = "";
        }
        self.at += 2;
    }

    pub fn forceColoredChunk(self: *@This(), color: []const u8, chunk: []const u8) void {
        assert(self.at + 2 <= Context.slices.len);
        Context.slices[self.at] = color;
        Context.slices[self.at + 1] = chunk;
        self.at += 2;
    }

    pub fn forceChunk(self: *@This(), chunk: []const u8) void {
        self.forceColoredChunk("", chunk);
    }

    pub fn skipChunk(self: *@This()) void {
        assert(self.at + 2 <= Context.slices.len);
        Context.slices[self.at] = "";
        Context.slices[self.at + 1] = "";
        self.at += 2;
    }

    pub fn breakline(self: *@This()) void {
        assert(self.at + 2 == Context.slices.len);
        self.add("\n");
    }

    pub fn add(self: *@This(), chunk: []const u8) void {
        assert(self.at + 2 <= Context.slices.len);
        Context.slices[self.at] = "";
        Context.slices[self.at + 1] = chunk;
        self.at += 2;
    }
};

pub const UncoloredChunks = struct {
    at: usize = 0,

    pub const init: @This() = .{};

    pub fn clearChunk(self: *@This(), chunk: []const u8) void {
        assert(self.at + 1 <= Context.slices.len);
        Context.slices[self.at] = chunk;
        self.at += 1;
    }

    pub fn coloredChunk(self: *@This(), chunk: []const u8) void {
        self.clearChunk(chunk);
    }

    pub fn skipChunk(self: *@This()) void {
        self.clearChunk("");
    }

    pub fn forceColoredChunk(self: *@This(), _: []const u8, chunk: []const u8) void {
        self.clearChunk(chunk);
    }

    pub fn forceChunk(self: *@This(), chunk: []const u8) void {
        self.clearChunk(chunk);
    }

    pub fn breakline(self: *@This()) void {
        assert(self.at + 1 == Context.slices.len);
        self.clearChunk("\n");
    }
};

pub const Chunks = union(enum) {
    colored: ColoredChunks,
    uncolored: UncoloredChunks,

    pub fn init(
        picker: *ColorPicker,
        offset: u16,
    ) @This() {
        return switch (picker.colorPattern) {
            .noColor => .{ .uncolored = .init },
            else => .{ .colored = .init(picker, offset) },
        };
    }

    pub fn clearChunk(self: *@This(), chunk: []const u8) void {
        switch (self.*) {
            inline else => |*chunks| chunks.clearChunk(chunk),
        }
    }

    // NOTE:
    // this does not alter the colorPicker state
    pub fn forceChunk(self: *@This(), chunk: []const u8) void {
        switch (self.*) {
            inline else => |*chunks| chunks.forceChunk(chunk),
        }
    }

    pub fn forceColoredChunk(
        self: *@This(),
        color: []const u8,
        chunk: []const u8,
    ) void {
        switch (self.*) {
            inline else => |*chunks| chunks.forceColoredChunk(color, chunk),
        }
    }

    pub fn coloredChunk(self: *@This(), chunk: []const u8) void {
        switch (self.*) {
            inline else => |*chunks| chunks.coloredChunk(chunk),
        }
    }

    pub fn skipChunk(self: *@This()) void {
        switch (self.*) {
            inline else => |*chunks| chunks.skipChunk(),
        }
    }

    pub fn breakline(self: *@This()) void {
        switch (self.*) {
            inline else => |*chunks| chunks.breakline(),
        }
    }
};
