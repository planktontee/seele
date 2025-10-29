const std = @import("std");

pub const NoColor = struct {
    pub fn pick(_: u16) []const u8 {
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

test "RGB to escape" {
    const t = std.testing;
    try t.expectEqualStrings("\x1b[38;2;192;192;192m", RGB.from("#C0C0C0").escapeCode);
    try t.expectEqualStrings("\x1b[38;2;15;192;2m", RGB.from("#0FC002").escapeCode);
}

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

test "Rainbow to escape" {
    const t = std.testing;
    try t.expectEqualStrings(RGB.from("#EF0000").escapeCode, RainbowColor.pick(0));
    try t.expectEqualStrings(RGB.from("#FA8072").escapeCode, RainbowColor.pick(1));
    try t.expectEqualStrings(RGB.from("#FFA500").escapeCode, RainbowColor.pick(2));
    try t.expectEqualStrings(RGB.from("#FFFF00").escapeCode, RainbowColor.pick(3));
    try t.expectEqualStrings(RGB.from("#B0FF00").escapeCode, RainbowColor.pick(4));
    try t.expectEqualStrings(RGB.from("#00EF00").escapeCode, RainbowColor.pick(5));
    try t.expectEqualStrings(RGB.from("#00EF80").escapeCode, RainbowColor.pick(6));
    try t.expectEqualStrings(RGB.from("#99D9EA").escapeCode, RainbowColor.pick(7));
    try t.expectEqualStrings(RGB.from("#0000EF").escapeCode, RainbowColor.pick(8));
    try t.expectEqualStrings(RGB.from("#8F00FF").escapeCode, RainbowColor.pick(9));
    try t.expectEqualStrings(RGB.from("#9400D3").escapeCode, RainbowColor.pick(10));
    try t.expectEqualStrings(RGB.from("#EF0000").escapeCode, RainbowColor.pick(11));
    try t.expectEqualStrings(RGB.from("#B0FF00").escapeCode, RainbowColor.pick(15));
    try t.expectEqualStrings("\x1b[0m", RainbowColor.resetCode());
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

test "EscapeColor to escape" {
    const t = std.testing;
    try t.expectEqualStrings("\x1b[1;31m", EscapeColor.pick(0));
    try t.expectEqualStrings("\x1b[1;32m", EscapeColor.pick(1));
    try t.expectEqualStrings("\x1b[1;33m", EscapeColor.pick(2));
    try t.expectEqualStrings("\x1b[1;34m", EscapeColor.pick(3));
    try t.expectEqualStrings("\x1b[1;35m", EscapeColor.pick(4));
    try t.expectEqualStrings("\x1b[1;36m", EscapeColor.pick(5));
    try t.expectEqualStrings("\x1b[0m", EscapeColor.resetCode());
}

pub const ColorPattern = enum {
    rainbow,
    escape,
    noColor,

    fn T(comptime self: @This()) type {
        return comptime switch (self) {
            .rainbow => RainbowColor,
            .escape => EscapeColor,
            .noColor => NoColor,
        };
    }

    pub fn pick(self: @This(), offset: u16) []const u8 {
        return switch (self) {
            inline else => |tag| T(tag).pick(offset),
        };
    }

    pub fn reset(self: @This()) []const u8 {
        return switch (self) {
            inline else => |tag| T(tag).resetCode(),
        };
    }
};

test "ColorPattern translation" {
    const t = std.testing;
    try t.expectEqualStrings(RGB.from("#EF0000").escapeCode, ColorPattern.rainbow.pick(0));
    try t.expectEqualStrings("\x1b[1;31m", ColorPattern.escape.pick(0));
    try t.expectEqualStrings("", ColorPattern.noColor.pick(0));

    try t.expectEqualStrings("\x1b[0m", ColorPattern.rainbow.reset());
    try t.expectEqualStrings("\x1b[0m", ColorPattern.escape.reset());
    try t.expectEqualStrings("", ColorPattern.noColor.reset());
}
