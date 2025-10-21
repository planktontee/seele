const std = @import("std");
const pcre2 = @import("c.zig").pcre2;

pub const sink = @import("./sink.zig");

pub const CompileError = error{
    RegexInitFailed,
    BadRegex,
    MatchDataInitFailed,
} || std.Io.Writer.Error;

// TODO: extract flags from pattern
pub fn compile(pattern: []const u8, rpt: *const sink.Reporter) CompileError!Regex {
    const compContext = pcre2.pcre2_compile_context_create_8(null) orelse {
        return CompileError.RegexInitFailed;
    };
    errdefer pcre2.pcre2_compile_context_free_8(compContext);

    switch (pcre2.pcre2_set_newline_8(compContext, pcre2.PCRE2_NEWLINE_LF)) {
        0 => {},
        pcre2.PCRE2_ERROR_BADDATA => return CompileError.RegexInitFailed,
        // NOTE: no other error codes are defined in the source
        else => unreachable,
    }

    var err: c_int = undefined;
    var errOff: usize = undefined;
    const re = pcre2.pcre2_compile_8(
        pattern.ptr,
        pattern.len,
        // TODO: abstract flags support
        // NOTE: /g is actually an abstraction outside of the regex engine
        0,
        // pcre2.PCRE2_NEVER_UCP | pcre2.PCRE2_NEVER_UTF | pcre2.PCRE2_NEVER_BACKSLASH_C | pcre2.PCRE2_EXTRA_NEVER_CALLOUT,
        &err,
        &errOff,
        compContext,
    ) orelse {
        // TODO: make this process optional
        var buff: [4098]u8 = undefined;
        const end = pcre2.pcre2_get_error_message_8(err, &buff, buff.len);
        try rpt.stderrW.print("Compile failed {d}: {s}\n", .{ errOff, buff[0..@intCast(end)] });

        return CompileError.BadRegex;
    };
    _ = pcre2.pcre2_jit_compile_8(re, pcre2.PCRE2_JIT_COMPLETE);

    const matchData = pcre2.pcre2_match_data_create_from_pattern_8(re, null) orelse {
        return CompileError.MatchDataInitFailed;
    };

    return .{
        .re = re,
        .compContext = compContext,
        .matchData = matchData,
    };
}

pub const Regex = struct {
    re: *pcre2.pcre2_code_8,
    compContext: *pcre2.pcre2_compile_context_8,
    matchData: *pcre2.pcre2_match_data_8,

    pub fn deinit(self: *@This()) void {
        // std.debug.print("Add {*}\n", .{self.compContext});
        // pcre2.pcre2_compile_context_free_8(self.compContext);
        // self.compContext = undefined;
        pcre2.pcre2_match_data_free_8(self.matchData);
        self.matchData = undefined;
        // pcre2.pcre2_jit_free_unused_memory_8(null);
        pcre2.pcre2_code_free_8(self.re);
        self.re = undefined;
    }

    pub const MatchError = error{
        NoMatch,
        MatchDataNotAvailable,
        UnknownError,
    };

    pub fn match(self: *const @This(), data: []const u8) MatchError!?RegexMatch {
        return try self.offsetMatch(data, 0);
    }

    pub fn offsetMatch(self: *const @This(), data: []const u8, offset: usize) MatchError!?RegexMatch {
        const rc = pcre2.pcre2_match_8(
            self.re,
            data.ptr,
            data.len,
            offset,
            // TODO: see if flags need to be abstracted
            // NOTE: consider (?i) etc works here
            0,
            self.matchData,
            null,
        );
        if (rc <= 0) {
            switch (rc) {
                // NOTE: this implies ovector is not big enough for all substr
                // matches
                0 => return MatchError.MatchDataNotAvailable,
                pcre2.PCRE2_ERROR_NOMATCH => {
                    return null;
                },
                // NOTE: most error are data related or group related or utf related
                // check the ERROR definition in the lib
                else => return MatchError.UnknownError,
            }
        }

        const ovect = pcre2.pcre2_get_ovector_pointer_8(self.matchData);

        std.debug.assert(rc >= 0);
        return .init(ovect, @intCast(rc));
    }
};

pub const RegexMatchGroup = struct {
    n: usize,
    start: usize,
    end: usize,

    pub fn init(n: usize, start: usize, end: usize) @This() {
        return .{
            .n = n,
            .start = start,
            .end = end,
        };
    }
};

pub const RegexMatch = struct {
    ovector: []const usize,

    pub fn init(ovector: [*c]usize, length: usize) @This() {
        return .{
            .ovector = ovector[0 .. length * 2],
        };
    }

    pub fn groupCount(self: *const @This()) usize {
        return self.ovector.len / 2;
    }

    pub fn group(self: *const @This(), n: usize) RegexMatchGroup {
        const start = n * 2;
        const end = n * 2 + 1;
        return .init(n, self.ovector[start], self.ovector[end]);
    }
};
