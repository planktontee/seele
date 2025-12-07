const std = @import("std");

pub const c = @cImport({
    @cInclude("fcntl.h");
    @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
    @cDefine("PCRE2_UNSET", std.fmt.comptimePrint("{d}", .{std.math.maxInt(usize)}));
    @cInclude("pcre2.h");
});
