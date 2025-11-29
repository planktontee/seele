const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const pcre2 = @import("c.zig").pcre2;
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

// TODO: extract flags from pattern
pub fn compile(
    allocator: std.mem.Allocator,
    pattern: []const u8,
    options: CompileOptions,
) CompileError!Result(Regex, RegexError) {
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

    // it's just an attribute set
    _ = pcre2.pcre2_set_compile_extra_options_8(
        compContext,
        @intCast(@as(u17, @bitCast(options.extraFlags))),
    );

    var err: c_int = undefined;
    var errOff: usize = undefined;
    const re = pcre2.pcre2_compile_8(
        pattern.ptr,
        pattern.len,
        @bitCast(options.flags),
        &err,
        &errOff,
        compContext,
    ) orelse {
        const buff = try allocator.alloc(u8, 4096);
        const end = pcre2.pcre2_get_error_message_8(err, buff.ptr, buff.len);

        return .{
            .Err = .{
                .code = err,
                .buff = buff,
                .message = buff[0..@intCast(end)],
                .err = CompileError.BadRegex,
            },
        };
    };
    _ = pcre2.pcre2_jit_compile_8(re, pcre2.PCRE2_JIT_COMPLETE);

    const matchData = pcre2.pcre2_match_data_create_from_pattern_8(re, null) orelse {
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
    re: *pcre2.pcre2_code_8,
    compContext: *pcre2.pcre2_compile_context_8,
    matchData: *pcre2.pcre2_match_data_8,

    pub fn deinit(self: *@This()) void {
        if (builtin.mode == .Debug) return;
        pcre2.pcre2_match_data_free_8(self.matchData);
        self.matchData = undefined;
        pcre2.pcre2_code_free_8(self.re);
        self.re = undefined;
    }

    pub const MatchError = error{
        MatchDataNotAvailable,
        UnknownError,
    };

    pub fn match(self: *const @This(), data: []const u8) MatchError!?RegexMatch {
        return try self.offsetMatch(data, 0);
    }

    pub fn offsetMatch(self: *const @This(), data: []const u8, offset: usize) MatchError!?RegexMatch {
        const rc = pcre2.pcre2_jit_match_8(
            self.re,
            data.ptr,
            data.len,
            offset,
            @bitCast(ExecutionFlags{}),
            self.matchData,
            null,
        );
        if (rc <= 0) {
            // TODO: identify BADUTF8
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
        if (idx0 == pcre2.PCRE2_UNSET) return GroupError.GroupSkipped;
        const idx1 = self.ovector[end];

        assert(idx0 != pcre2.PCRE2_UNSET);
        assert(idx1 != pcre2.PCRE2_UNSET);
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
    noUTFCheck: bool = true,
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
    asciiBsd: bool = false,
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
    dollarEndonly: bool = false,
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
    ucp: bool = false,
    ungreedy: bool = false,
    utf: bool = false,
    neverBacklashC: bool = false,
    altCircumflex: bool = false,
    altVerbNames: bool = false,
    useOffsetLimit: bool = false,
    extendedMore: bool = false,
    literal: bool = false,
    matchInvalidUTF: bool = false,
    altExtendedClass: bool = false,
    _: bool = false,
    anchored: bool = false,
    noUTFCheck: bool = true,
    endAchored: bool = false,
};
