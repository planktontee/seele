const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const c = @import("c.zig").c;
const Result = @import("regent").result.Result;

pub const sink = @import("./sink.zig");

pub const CompileError = error{
    RegexInitFailed,
    BadRegex,
    MatchDataInitFailed,
} || std.mem.Allocator.Error;

pub const RegexError = struct {
    code: c_int,
    buff: []u8,
    message: []const u8,
    err: CompileError,

    pub fn deinit(self: *const @This(), allocator: std.mem.Allocator) void {
        if (builtin.mode == .Debug) return;
        allocator.free(self.buff);
    }

    pub fn throw(self: *const @This()) CompileError!void {
        return @as(CompileError!void, self.err);
    }
};

// TODO: add own memory allocators
pub fn compile(
    allocator: std.mem.Allocator,
    pattern: []const u8,
    options: CompileOptions,
) CompileError!Result(Regex, RegexError) {
    const compContext = c.pcre2_compile_context_create_8(null) orelse {
        return CompileError.RegexInitFailed;
    };
    errdefer c.pcre2_compile_context_free_8(compContext);

    switch (c.pcre2_set_newline_8(compContext, c.PCRE2_NEWLINE_LF)) {
        0 => {},
        c.PCRE2_ERROR_BADDATA => return CompileError.RegexInitFailed,
        // NOTE: no other error codes are defined in the source
        else => unreachable,
    }

    // it's just an attribute set
    const extraFlags: u32 = @intCast(@as(u17, @bitCast(options.extraFlags)));
    _ = c.pcre2_set_compile_extra_options_8(
        compContext,
        extraFlags,
    );

    var err: c_int = undefined;
    var errOff: usize = undefined;
    const flags: u32 = @bitCast(options.flags);
    const re = c.pcre2_compile_8(
        pattern.ptr,
        pattern.len,
        flags,
        &err,
        &errOff,
        compContext,
    ) orelse {
        const buff = try allocator.alloc(u8, 4096);
        const end = c.pcre2_get_error_message_8(err, buff.ptr, buff.len);

        return .{
            .Err = .{
                .code = err,
                .buff = buff,
                .message = buff[0..@intCast(end)],
                .err = CompileError.BadRegex,
            },
        };
    };
    _ = c.pcre2_jit_compile_8(re, c.PCRE2_JIT_COMPLETE);

    const matchData = c.pcre2_match_data_create_from_pattern_8(re, null) orelse {
        return CompileError.MatchDataInitFailed;
    };

    return .{
        .Ok = .{
            .re = re,
            .compContext = compContext,
            .matchData = matchData,
        },
    };
}

pub const Regex = struct {
    re: *c.pcre2_code_8,
    compContext: *c.pcre2_compile_context_8,
    matchData: *c.pcre2_match_data_8,

    pub fn deinit(self: *@This()) void {
        if (builtin.mode == .Debug) return;
        c.pcre2_match_data_free_8(self.matchData);
        self.matchData = undefined;
        c.pcre2_code_free_8(self.re);
        self.re = undefined;
    }

    pub const MatchError = error{
        MatchDataTooBig,
        NoMatch,
        BadUTF8Encode,
        UnknownError,
    };

    pub fn match(self: *const @This(), data: []const u8) MatchError!?RegexMatch {
        return try self.offsetMatch(data, 0);
    }

    pub fn offsetMatch(
        self: *const @This(),
        data: []const u8,
        offset: usize,
        isLineStart: bool,
        validateUtf8: bool,
    ) MatchError!RegexMatch {
        const flags: u32 = @bitCast(ExecutionFlags{
            .notBol = !isLineStart,
            .noUTFCheck = !validateUtf8,
        });
        const rc = c.pcre2_match_8(
            self.re,
            data.ptr,
            data.len,
            offset,
            flags,
            self.matchData,
            null,
        );
        if (rc <= 0) {
            switch (rc) {
                // NOTE: this implies ovector is not big enough for all substr
                // matches
                0 => return MatchError.MatchDataTooBig,
                c.PCRE2_ERROR_NOMATCH => return MatchError.NoMatch,
                // -23 ... -3
                c.PCRE2_ERROR_UTF8_ERR21...c.PCRE2_ERROR_UTF8_ERR1 => return MatchError.BadUTF8Encode,
                // NOTE: most error are data related or group related or utf related
                // check the ERROR definition in the lib
                else => return MatchError.UnknownError,
            }
        }

        const ovect = c.pcre2_get_ovector_pointer_8(self.matchData);

        assert(rc >= 0);
        return .init(ovect, @intCast(rc));
    }
};

pub const RegexMatchGroup = struct {
    n: u16,
    start: usize,
    end: usize,

    pub fn init(n: u16, start: usize, end: usize) @This() {
        return .{
            .n = n,
            .start = start,
            .end = end,
        };
    }

    pub fn slice(self: *const @This(), data: []const u8) []const u8 {
        return data[self.start..self.end];
    }

    pub fn format(
        self: *const @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print(".{c} .n {d} [{d}..{d}] {c}", .{
            '{',
            self.n,
            self.start,
            self.end,
            '}',
        });
    }
};

pub const RegexMatch = struct {
    ovector: []const usize,
    count: u16,

    pub fn init(ovector: [*c]usize, length: usize) @This() {
        const slice = ovector[0 .. length * 2];
        assert(slice.len / 2 <= std.math.maxInt(u16));
        return .{
            .ovector = slice,
            .count = @intCast(slice.len / 2),
        };
    }

    pub const GroupError = error{
        InvalidGroup,
        GroupSkipped,
    };

    pub fn group(self: *const @This(), n: usize) GroupError!RegexMatchGroup {
        assert(n <= std.math.maxInt(u16));
        if (n + 1 > self.count) return GroupError.InvalidGroup;
        const start = n * 2;
        const end = n * 2 + 1;
        const idx0 = self.ovector[start];
        if (idx0 == c.PCRE2_UNSET) return GroupError.GroupSkipped;
        const idx1 = self.ovector[end];

        assert(idx0 != c.PCRE2_UNSET);
        assert(idx1 != c.PCRE2_UNSET);
        return .init(@intCast(n), idx0, idx1);
    }
};

pub const CompileOptions = struct {
    extraFlags: ExtraCompileFlags = .{},
    flags: CompileFlags = .{},
};

pub const ExecutionFlags = packed struct(u32) {
    notBol: bool = false,
    notEol: bool = false,
    notEmpty: bool = false,
    notEmptyAtStart: bool = false,
    partialSoft: bool = false,
    partialHard: bool = false,
    dfaRestart: bool = false,
    dfaShortest: bool = false,
    substituteGlobal: bool = false,
    substituteExtended: bool = false,
    substituteUnsetEmpty: bool = false,
    substituteUnknownUnset: bool = false,
    substituteOverflowLength: bool = false,
    noJit: bool = false,
    copyMatchedSubject: bool = false,
    substituteLiteral: bool = false,
    substituteMatched: bool = false,
    substituteReplacementOnly: bool = false,
    disableRecursiveLoopCheck: bool = false,
    // 20 - 29 inclusive
    _: u10 = 0,
    anchored: bool = false,
    noUTFCheck: bool = false,
    endAnchored: bool = false,
};

pub const ExtraCompileFlags = packed struct(u17) {
    allowSurrogateEscapes: bool = false,
    badEscapeIsLiteral: bool = false,
    matchWord: bool = false,
    matchLine: bool = false,
    escapedCrIsLf: bool = false,
    altBsux: bool = false,
    allowLookaroundBsk: bool = false,
    caselessRestrict: bool = false,
    asciiBsd: bool = true,
    asciiBss: bool = false,
    asciiBsw: bool = false,
    asciiPosix: bool = false,
    asciiDigit: bool = false,
    pythonOctal: bool = false,
    noBs0: bool = false,
    neverCallout: bool = true,
    turkishCasing: bool = false,
};

pub const CompileFlags = packed struct(u32) {
    allowEmptyClass: bool = false,
    altBsux: bool = false,
    autoCallout: bool = false,
    caseless: bool = false,
    dollarEndOnly: bool = true,
    dotall: bool = false,
    dupnames: bool = false,
    extended: bool = false,
    firstline: bool = false,
    matchUnsetBackref: bool = false,
    multiline: bool = false,
    neverUCP: bool = false,
    neverUTF: bool = false,
    noAutoCapture: bool = false,
    noAutoPossess: bool = false,
    noDotStartAnchor: bool = false,
    noStartOptimize: bool = false,
    ucp: bool = true,
    ungreedy: bool = false,
    utf: bool = true,
    neverBacklashC: bool = false,
    altCircumflex: bool = false,
    altVerbNames: bool = false,
    useOffsetLimit: bool = false,
    extendedMore: bool = false,
    literal: bool = false,
    matchInvalidUTF: bool = true,
    altExtendedClass: bool = false,
    _: bool = false,
    anchored: bool = false,
    noUTFCheck: bool = false,
    endAchored: bool = false,
};
