const std = @import("std");

pub const pcre2 = @cImport({
    @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
    @cDefine("PCRE2_UNSET", std.fmt.comptimePrint("{d}", .{std.math.maxInt(usize)}));
    @cInclude("pcre2.h");
    @cInclude("fcntl.h");
});
pub const c = pcre2;
