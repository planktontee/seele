const std = @import("std");
const assert = std.debug.assert;
const units = @import("regent").units;
const tty = @import("tty.zig");

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

    pub fn chunks(self: *@This(), comptime size: usize, offset: u16) Chunks(size) {
        return Chunks(size).init(self, offset);
    }
};

pub fn ColoredChunks(comptime n: std.math.IntFittingRange(0, std.math.maxInt(usize) / 2)) type {
    const size = n * 2;
    return struct {
        colorOffset: u16 = undefined,
        slices: [size][]const u8 = undefined,
        at: usize = 0,
        colorPicker: *ColorPicker = undefined,

        pub fn init(colorPicker: *ColorPicker, offset: u16) @This() {
            return .{
                .colorPicker = colorPicker,
                .colorOffset = offset,
            };
        }

        pub fn clearChunk(self: *@This(), chunk: []const u8) void {
            assert(self.at + 2 <= self.slices.len);
            if (chunk.len > 0) {
                self.slices[self.at] = self.colorPicker.reset();
                self.slices[self.at + 1] = chunk;
            } else {
                self.slices[self.at] = "";
                self.slices[self.at + 1] = "";
            }
            self.at += 2;
        }

        pub fn coloredChunk(self: *@This(), chunk: []const u8) void {
            assert(self.at + 2 <= self.slices.len);
            if (chunk.len > 0) {
                self.slices[self.at] = self.colorPicker.pickColor(self.colorOffset);
                self.slices[self.at + 1] = chunk;
            } else {
                self.slices[self.at] = "";
                self.slices[self.at + 1] = "";
            }
            self.at += 2;
        }

        pub fn skipChunk(self: *@This()) void {
            assert(self.at + 2 <= self.slices.len);
            self.slices[self.at] = "";
            self.slices[self.at + 1] = "";
            self.at += 2;
        }

        // crawls all chunks to see if there's a breakline already in there before all
        // skipped chunks
        pub fn breakline(self: *@This()) void {
            assert(self.at + 2 == size);
            if (self.at == 0 or size <= 2) {
                self.add("\n");
                return;
            }
            assert(size >= 3);

            // [-3]<text> [-2]<color> [-1]<text>$
            var chunk = size - 3;
            // this if fine because [0] is color
            while (chunk > 0) : (chunk -|= 2) {
                const last = self.slices[chunk];
                if (last.len == 0) continue;

                self.add(if (last.len > 0 and last[last.len - 1] != '\n') "\n" else "");
                return;
            }
        }

        pub fn add(self: *@This(), chunk: []const u8) void {
            assert(self.at + 2 <= self.slices.len);
            self.slices[self.at] = "";
            self.slices[self.at + 1] = chunk;
            self.at += 2;
        }
    };
}

pub fn UncoloredChunks(comptime size: usize) type {
    return struct {
        slices: [size][]const u8 = undefined,
        at: usize = 0,

        pub const init: @This() = .{};

        pub fn clearChunk(self: *@This(), chunk: []const u8) void {
            assert(self.at + 1 <= self.slices.len);
            self.slices[self.at] = chunk;
            self.at += 1;
        }

        pub fn coloredChunk(self: *@This(), chunk: []const u8) void {
            self.clearChunk(chunk);
        }

        pub fn skipChunk(self: *@This()) void {
            self.clearChunk("");
        }

        pub fn breakline(self: *@This()) void {
            assert(self.at + 1 == size);
            if (self.at == 0 or size <= 1) {
                self.clearChunk("\n");
                return;
            }
            assert(size >= 1);

            for (0..self.at) |offset| {
                const last = self.slices[self.at - 1 - offset];
                if (last.len == 0) continue;

                self.clearChunk(if (last.len > 0 and last[last.len - 1] != '\n') "\n" else "");
                return;
            }
        }
    };
}

pub fn Chunks(comptime size: usize) type {
    return union(enum) {
        colored: ColoredChunks(size),
        uncolored: UncoloredChunks(size),

        pub fn init(picker: *ColorPicker, offset: u16) @This() {
            return switch (picker.colorPattern) {
                .noColor => .{ .uncolored = .init },
                else => .{ .colored = .init(picker, offset) },
            };
        }

        pub inline fn clearChunk(self: *@This(), chunk: []const u8) void {
            switch (self.*) {
                inline else => |*chunks| chunks.clearChunk(chunk),
            }
        }

        pub inline fn coloredChunk(self: *@This(), chunk: []const u8) void {
            switch (self.*) {
                inline else => |*chunks| chunks.coloredChunk(chunk),
            }
        }

        pub inline fn skipChunk(self: *@This()) void {
            switch (self.*) {
                inline else => |*chunks| chunks.skipChunk(),
            }
        }

        pub inline fn breakline(self: *@This()) void {
            switch (self.*) {
                inline else => |*chunks| chunks.breakline(),
            }
        }
    };
}
