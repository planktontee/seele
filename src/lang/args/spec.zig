const std = @import("std");
const meta = @import("../meta.zig");
const coll = @import("../collections.zig");
const argIter = @import("iterator.zig");
const argCodec = @import("codec.zig");
const validate = @import("validate.zig");
const HelpFmt = @import("./help.zig").HelpFmt;
const HelpData = @import("./help.zig").HelpData;
const _HelpConf = @import("./help.zig").HelpConf;
const PositionalOf = @import("positionals.zig").PositionalOf;
const EmptyPositionalsOf = @import("positionals.zig").EmptyPositionalsOf;
const GroupTracker = validate.GroupTracker;
const GroupTrackerWithConfig = validate.GroupTrackerWithConfig;
const GroupMatchConfig = validate.GroupMatchConfig;
const Allocator = std.mem.Allocator;
const FieldEnum = std.meta.FieldEnum;
const TstArgCursor = argIter.TstArgCursor;
const PrimitiveCodec = argCodec.PrimitiveCodec;
const ArgCodec = argCodec.ArgCodec;
const Result = @import("../result.zig").Result;

// NOTE: This ties spec positionals default and helper's
pub fn defaultPositionals() type {
    return PositionalOf(.{});
}

pub fn SpecResponse(comptime Spec: type) type {
    return SpecResponseWithConfig(Spec, {}, false);
}

pub fn SpecResponseWithConfig(comptime Spec: type, comptime HelpConf: anytype, comptime withDiag: bool) type {
    // TODO: validate spec
    return struct {
        arena: std.heap.ArenaAllocator,
        codec: SpecCodec,
        options: Options,
        program: ?[]const u8,
        positionals: PosOf.Positionals,
        // TODO: support default verb
        verb: if (VerbT != void) ?VerbT else void,
        tracker: SpecTracker,

        fn SpecUnionVerbs() type {
            const VerbUnion = @typeInfo(Spec.Verb).@"union";
            comptime var newUnionFields: [VerbUnion.fields.len]std.builtin.Type.UnionField = undefined;
            for (VerbUnion.fields, 0..) |f, i| {
                const newSpecR = SpecResponseWithConfig(f.type, HelpConf, withDiag);
                newUnionFields[i] = .{
                    .name = f.name,
                    .type = newSpecR,
                    .alignment = @alignOf(newSpecR),
                };
            }
            const newUni: std.builtin.Type = .{
                .@"union" = .{
                    .layout = VerbUnion.layout,
                    .tag_type = VerbUnion.tag_type,
                    .fields = &newUnionFields,
                    .decls = VerbUnion.decls,
                },
            };
            return @Type(newUni);
        }

        fn SpecVerbsErrors() type {
            comptime var errors = error{};
            for (@typeInfo(SpecUnionVerbs()).@"union".fields) |field| {
                errors = errors || field.type.Error;
            }
            return errors;
        }

        pub const Options = Spec;
        const CursorT = coll.Cursor([]const u8);
        pub const VerbT = if (@hasDecl(Spec, "Verb")) SpecUnionVerbs() else void;
        const PosOf = if (@hasDecl(Spec, "Positionals")) Spec.Positionals else defaultPositionals();
        const SpecCodec = if (@hasDecl(Spec, "Codec")) Spec.Codec else ArgCodec(Spec);
        const SpecTracker = T: {
            if (@hasDecl(Spec, "GroupMatch")) break :T GroupTracker(Spec);
            break :T GroupTrackerWithConfig(Spec, .{
                .ensureCursorDone = true,
            });
        };
        const SpecEnumFields = std.meta.FieldEnum(Spec);

        pub const Error = E: {
            var errors = error{
                UnknownArgumentName,
                InvalidArgumentToken,
                MissingShorthandMetadata,
                MissingShorthandLink,
                UnknownShorthandName,
                ArgEqualSplitMissingValue,
                ArgEqualSplitNotConsumed,
                CodecParseMethodUnavailable,
            } ||
                SpecCodec.Error ||
                PosOf.Error;
            errors = errors || if (VerbT != void) SpecVerbsErrors() else error{};
            errors = errors || if (SpecTracker != void) SpecTracker.Error else error{};
            break :E errors;
        };

        pub const ParseError = struct {
            err: Error,
            message: ?[]const u8 = null,
            // TODO: bitrot this?
            // TODO: test diag
            lastToken: ?[]const u8 = null,
            lastOpt: ?[]const u8 = null,

            pub fn ErrOf(err: Error, cursor: *CursorT) @This() {
                var self: @This() = .{
                    .err = err,
                    .message = help(err),
                };

                if (comptime withDiag) {
                    const diag: *coll.DiagnosticsCursor = @ptrCast(@alignCast(cursor.ptr));
                    self.lastOpt = diag.lastOpt;
                    self.lastToken = diag.lastToken;
                }

                return self;
            }
        };

        pub fn init(baseAllc: Allocator) @This() {
            return .{
                .arena = std.heap.ArenaAllocator.init(baseAllc),
                .codec = .{},
                .options = .{},
                .program = null,
                .positionals = undefined,
                .verb = if (comptime VerbT != void) null else {},
                .tracker = if (comptime SpecTracker != void) .{} else {},
            };
        }

        pub fn initEmpty(baseAllc: Allocator) @This() {
            var positionalOf: PosOf = .{};
            return .{
                .arena = std.heap.ArenaAllocator.init(baseAllc),
                .codec = .{},
                .options = .{},
                .program = null,
                .positionals = positionalOf.collectEmpty(),
                .verb = if (comptime VerbT != void) null else {},
                .tracker = if (comptime SpecTracker != void) .{} else {},
            };
        }

        pub fn parseArgs(self: *@This()) ?ParseError {
            var iter = std.process.args();
            const cursor = rv: {
                var c: CursorT = .{
                    .curr = null,
                    .ptr = @ptrCast(&iter),
                    .vtable = &.{
                        .next = &coll.AsCursor(@TypeOf(iter)).next,
                    },
                };
                break :rv &c;
            };
            return self.parseWithDiag(cursor, true);
        }

        // TODO: test all error returns
        pub fn parse(self: *@This(), cursor: *CursorT) ?ParseError {
            return self.parseWithDiag(cursor, true);
        }

        pub fn parseWithDiag(self: *@This(), cursor: *CursorT, comptime parseProgram: bool) ?ParseError {
            if (comptime withDiag) {
                var diag: coll.DiagnosticsCursor = .{
                    .cursor = cursor,
                };
                var c = diag.asCursor();
                return self.parseInner(&c, parseProgram);
            }
            return self.parseInner(cursor, parseProgram);
        }

        fn parseInner(self: *@This(), cursor: *CursorT, comptime parseProgram: bool) ?ParseError {
            if (comptime parseProgram) {
                self.program = cursor.next();
                if (self.program == null) return null;
            }

            const allocator = &self.arena.allocator();
            var positionalOf: PosOf = .{};

            while (cursor.next()) |arg| {
                if (comptime withDiag) {
                    const diag: *coll.DiagnosticsCursor = @ptrCast(@alignCast(cursor.ptr));
                    diag.lastOpt = null;
                }
                if (arg.len == 1 and arg[0] == '-') {
                    cursor.stackItem(arg);
                    if (positionalOf.hasNext()) {
                        positionalOf.parseNextType(allocator, cursor) catch |e| return .ErrOf(e, cursor);
                    } else break;
                } else if (arg.len == 2 and std.mem.eql(u8, "--", arg)) {
                    break;
                } else if (arg.len >= 1 and arg[0] != '-') {
                    switch (self.parseVerb(arg, cursor)) {
                        .Ok => |hasVerb| {
                            if (!hasVerb) {
                                cursor.stackItem(arg);
                                if (positionalOf.hasNext()) {
                                    positionalOf.parseNextType(allocator, cursor) catch |e| return .ErrOf(e, cursor);
                                } else break;
                            }
                            // NOTE: already parsed otherwise
                        },
                        .Err => |err| return err,
                    }
                } else if (arg.len == 0) {
                    continue;
                } else {
                    var offset: usize = 1;
                    if (arg[1] == '-') offset += 1;
                    self.namedToken(offset, arg, cursor) catch |e| return .ErrOf(e, cursor);
                }
            }

            while (cursor.peek() != null and positionalOf.hasNext()) {
                positionalOf.parseNextType(allocator, cursor) catch |e| return .ErrOf(e, cursor);
            }

            self.positionals = positionalOf.collect(allocator) catch |e| return .ErrOf(e, cursor);

            if (comptime SpecTracker != void) {
                if (cursor.peek() == null) self.tracker.cursorDone();
                self.tracker.validate() catch |e| return .ErrOf(e, cursor);
            }

            return null;
        }

        fn parseVerb(
            self: *@This(),
            arg: []const u8,
            cursor: *CursorT,
        ) Result(bool, ParseError) {
            if (comptime VerbT == void) return .{ .Ok = false };

            inline for (@typeInfo(Spec.Verb).@"union".fields) |f| {
                if (std.mem.eql(u8, f.name, arg)) {
                    var verbR = SpecResponseWithConfig(
                        f.type,
                        HelpConf,
                        withDiag,
                    ).init(self.arena.child_allocator);
                    if (verbR.parseInner(cursor, false)) |e| {
                        return .{
                            .Err = .{
                                .err = e.err,
                                .message = e.message,
                                .lastOpt = e.lastOpt,
                                .lastToken = e.lastToken,
                            },
                        };
                    } else {
                        self.verb = @unionInit(VerbT, f.name, verbR);
                        if (comptime SpecTracker != void) self.tracker.parsedVerb();
                        return .{ .Ok = true };
                    }
                }
            }
            return .{ .Ok = false };
        }

        fn namedToken(self: *@This(), offset: usize, arg: []const u8, cursor: *CursorT) Error!void {
            var splitValue: ?[]const u8 = null;
            var slice: []const u8 = arg[offset..];
            const optValueIdx = std.mem.indexOfScalar(u8, slice, '=');

            if (optValueIdx) |i| {
                splitValue = slice[i + 1 ..];
                cursor.stackItem(splitValue.?);
                slice = slice[0..i];
                if (comptime withDiag) {
                    var c: *coll.DiagnosticsCursor = @ptrCast(@alignCast(cursor.ptr));
                    c.lastToken = splitValue.?;
                }
            }

            try switch (offset) {
                2 => rv: {
                    if (comptime withDiag) {
                        var c: *coll.DiagnosticsCursor = @ptrCast(@alignCast(cursor.ptr));
                        c.lastOpt = slice;
                    }
                    break :rv self.namedArg(slice, cursor);
                },
                1 => self.shortArg(slice, cursor),
                else => Error.InvalidArgumentToken,
            };

            // if split is not consumed, it's not sane to progress
            if (splitValue) |v| {
                const peekR: [*]const u8 = @ptrCast(cursor.peek() orelse return);
                if (peekR == @as([*]const u8, @ptrCast(v))) return Error.ArgEqualSplitNotConsumed;
            }
        }

        fn shortArg(self: *@This(), arg: []const u8, cursor: *CursorT) Error!void {
            if (comptime !@hasDecl(Spec, "Short")) return Error.MissingShorthandMetadata;
            // NOTE: Short will result in fields[] with the enum_literal undone
            // with enum_literal as a key, unfortunately there's no enforcement for it to be
            // fieldEnum(Spec)
            const ShortFields = comptime std.meta.fields(@TypeOf(Spec.Short));
            if (comptime ShortFields.len == 0) return Error.UnknownArgumentName;

            var start: usize = 0;
            var end: usize = @min(2, arg.len);
            var noneCursor = coll.UnitCursor([]const u8).asNoneCursor();
            while (end <= arg.len) {
                ret: inline for (ShortFields) |s| {
                    const short = arg[start..end];
                    if (std.mem.eql(u8, s.name, short)) {
                        const tag = @tagName(s.defaultValue() orelse return Error.MissingShorthandLink);
                        if (comptime withDiag) {
                            var c: *coll.DiagnosticsCursor = @ptrCast(@alignCast(cursor.ptr));
                            c.lastOpt = short;
                        }
                        try self.namedArg(
                            tag,
                            if (end == arg.len) cursor else &noneCursor,
                        );
                        start = end;
                        break :ret;
                    }
                } else if (end - start == 1) {
                    return Error.UnknownShorthandName;
                }

                if (start == end and end == arg.len) {
                    return;
                } else if (end - start == 0) {
                    end = @min(end + 2, arg.len);
                } else {
                    end -= 1;
                }
            }
        }

        fn namedArg(self: *@This(), arg: []const u8, cursor: *CursorT) Error!void {
            const fields = comptime @typeInfo(SpecEnumFields).@"enum".fields;
            if (comptime fields.len == 0) return Error.UnknownArgumentName;

            inline for (fields) |f| {
                if (std.mem.eql(u8, f.name, arg)) {
                    const tag: SpecEnumFields = @enumFromInt(f.value);

                    @field(self.options, f.name) = try self.codec.parseByType(
                        @FieldType(Spec, f.name),
                        tag,
                        &self.arena.allocator(),
                        cursor,
                    );

                    if (comptime SpecTracker != void) self.tracker.parsed(tag);
                    return;
                }
            } else {
                return Error.UnknownArgumentName;
            }
        }

        pub fn deinit(self: *const @This()) void {
            if (comptime VerbT != void) if (self.verb) |hasVerb| switch (hasVerb) {
                inline else => |v| v.deinit(),
            };
            self.arena.deinit();
        }

        pub fn help(e: Error) ?[]const u8 {
            if (comptime @TypeOf(HelpConf) == void) return null;

            return HelpFmt(Spec, HelpConf).helpForErr(Error, e, "Failed with reason: ");
        }
    };
}

pub fn tstParseSpec(allocator: Allocator, cursor: *coll.Cursor([]const u8), Spec: type) !SpecResponse(Spec) {
    var response = SpecResponse(Spec).init(allocator);
    if (response.parse(cursor)) |err| {
        return @as(@TypeOf(response).Error!@TypeOf(response), err.err);
    }
    return response;
}

fn tstParse(allocator: Allocator, data: [:0]const u8, Spec: type) !SpecResponse(Spec) {
    var tstCursor = try TstArgCursor.init(&allocator, data);
    var cursor = tstCursor.asCursor();
    return try tstParseSpec(allocator, &cursor, Spec);
}

test "empty args with default" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const r = try tstParse(allocator, "", struct {
        flag: bool = false,
    });
    try std.testing.expectEqual(null, r.program);
    try std.testing.expectEqual(false, r.options.flag);
}

test "program only" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const r = try tstParse(allocator, "program", struct {});
    try std.testing.expectEqualStrings("program", r.program.?);
}

test "parse named" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const r = try tstParse(allocator, "program --cool-flag", struct { @"cool-flag": bool = false });
    const r2 = try tstParse(allocator, "program --cool-flag true", struct { @"cool-flag": bool = false });
    const r3 = try tstParse(allocator, "program --cool-flag false", struct { @"cool-flag": bool = false });
    const r4 = try tstParse(allocator, "program --cool-flag something else", struct { @"cool-flag": bool = false });

    try std.testing.expectEqual(true, r.options.@"cool-flag");
    try std.testing.expectEqual(true, r2.options.@"cool-flag");
    try std.testing.expectEqual(false, r3.options.@"cool-flag");
    try std.testing.expectEqual(true, r4.options.@"cool-flag");
    const expected: []const []const u8 = &.{ "something", "else" };
    try std.testing.expectEqualDeep(expected, r4.positionals.reminder.?);
}

test "parsed chained flags" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        t1: bool = undefined,
        t2: bool = undefined,
        t3: bool = undefined,
        t4: bool = undefined,
        t5: bool = undefined,

        pub const Short = .{
            .a = .t1,
            .aA = .t2,
            .b = .t3,
            .Ss = .t4,
            .c = .t5,
        };
    };
    const r1 = try tstParse(allocator, "program -aaAbSsc=false", Spec);
    try std.testing.expect(r1.options.t1);
    try std.testing.expect(r1.options.t2);
    try std.testing.expect(r1.options.t3);
    try std.testing.expect(r1.options.t4);
    try std.testing.expect(!r1.options.t5);
}

test "parse short arg" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        something: bool = false,
        @"super-something": bool = false,

        const Short = .{
            .s = .something,
            .S = .@"super-something",
        };
    };
    const r1 = try tstParse(allocator, "program -s --super-something true", Spec);
    const r2 = try tstParse(allocator, "program --something false -S", Spec);
    const r3 = try tstParse(allocator, "program -s false -S false", Spec);
    const r4 = try tstParse(allocator, "program -s true -S true", Spec);

    try std.testing.expectEqual(true, r1.options.something);
    try std.testing.expectEqual(true, r1.options.@"super-something");
    try std.testing.expectEqual(false, r2.options.something);
    try std.testing.expectEqual(true, r2.options.@"super-something");
    try std.testing.expectEqual(false, r3.options.something);
    try std.testing.expectEqual(false, r3.options.@"super-something");
    try std.testing.expectEqual(true, r4.options.something);
    try std.testing.expectEqual(true, r4.options.@"super-something");
}

test "parse different types" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        o1: bool = undefined,
        u1: u1 = undefined,
        f1: f16 = undefined,
        b: bool = undefined,
        e: Animal = undefined,
        s1: []const u8 = undefined,
        s2: [:0]const u8 = undefined,
        pres1: [2]u8 = undefined,
        au1: []const u1 = undefined,
        au32: []const u32 = undefined,
        ai32: []const i32 = undefined,
        af32: []const f32 = undefined,
        as1: []const []const u8 = undefined,
        as2: []const [:0]const u8 = undefined,
        aau32: []const []const u32 = undefined,
        aaf32: []const []const f32 = undefined,
        aai32: []const []const i32 = undefined,
        aab: []const []const bool = undefined,
        ae: []const Animal = undefined,
        o2: bool = undefined,

        pub const Animal = enum { dog, cat };
    };

    const r1 = try tstParse(
        allocator,
        \\program
        \\--o1
        \\--u1=0
        \\--f1 1.1
        \\--b false
        \\--e dog
        \\--s1 Hello
        \\--s2 "Hello World"
        \\--pres1 Hi
        \\--au1 [1,0,1,0]
        \\--au32=[32,23,133,99,10]
        \\--ai32 "[-1, -44, 22222, -1]"
        \\--af32="[3.4, 58, 3.1]"
        \\--as1 "['Hello', ' World ', '!']"
        \\--as2="['Im', 'Losing it']"
        \\--aau32 "[[1,3], [3300, 222, 333, 33], [1]]"
        \\--aaf32 "[[1.1, 3.2, -1], [3.1, 2,5], [1.2], [7.1]]"
        \\--aai32 "[[-1], [2, -2]]"
        \\--aab="[[true, true, false], [false, true]]"
        \\--ae "[cat, cat]"
        \\--o2
    ,
        Spec,
    );

    try std.testing.expect(r1.options.o1);
    try std.testing.expectEqual(0, r1.options.u1);
    try std.testing.expectEqual(1.1, r1.options.f1);
    try std.testing.expect(!r1.options.b);
    try std.testing.expectEqual(Spec.Animal.dog, r1.options.e);
    try std.testing.expectEqualStrings("Hello", r1.options.s1);
    try std.testing.expectEqualStrings("Hello World", r1.options.s2);
    try std.testing.expectEqualStrings("Hi", &r1.options.pres1);
    try std.testing.expectEqualDeep(&[_]u1{ 1, 0, 1, 0 }, r1.options.au1);
    try std.testing.expectEqualDeep(&[_]u32{ 32, 23, 133, 99, 10 }, r1.options.au32);
    try std.testing.expectEqualDeep(&[_]i32{ -1, -44, 22222, -1 }, r1.options.ai32);
    const expectedAs1: []const []const u8 = &.{ "Hello", " World ", "!" };
    try std.testing.expectEqualDeep(expectedAs1, r1.options.as1);
    const expectedAs2: []const [:0]const u8 = &.{ "Im", "Losing it" };
    try std.testing.expectEqualDeep(expectedAs2, r1.options.as2);
    const expectedAau32: []const [:0]const u32 = &.{ &.{ 1, 3 }, &.{ 3300, 222, 333, 33 }, &.{1} };
    try std.testing.expectEqualDeep(expectedAau32, r1.options.aau32);
    const expectedAaf32: []const []const f32 = &.{ &.{ 1.1, 3.2, -1 }, &.{ 3.1, 2, 5 }, &.{1.2}, &.{7.1} };
    try std.testing.expectEqualDeep(expectedAaf32, r1.options.aaf32);
    const expectedAai32: []const []const i32 = &.{ &.{-1}, &.{ 2, -2 } };
    try std.testing.expectEqualDeep(expectedAai32, r1.options.aai32);
    const expectAab: []const []const bool = &.{ &.{ true, true, false }, &.{ false, true } };
    try std.testing.expectEqualDeep(expectAab, r1.options.aab);
    try std.testing.expectEqualDeep(&[_]Spec.Animal{ .cat, .cat }, r1.options.ae);
    try std.testing.expect(r1.options.o2);
}

test "parse kvargs" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        something: bool = false,
        @"super-something": bool = false,

        const Short = .{
            .s = .something,
            .S = .@"super-something",
        };
    };

    const r1 = try tstParse(allocator, "program -s=true --super-something=true", Spec);
    try std.testing.expectError(
        SpecResponse(Spec).Error.ArgEqualSplitNotConsumed,
        tstParse(allocator, "program -s= --super-something=true", Spec),
    );

    try std.testing.expectEqual(true, r1.options.something);
    try std.testing.expectEqual(true, r1.options.@"super-something");
}

test "parse positionals" {
    const t = std.testing;
    const base = &t.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const r1 = try tstParse(allocator, "program positional1 positional2 positional3", struct {});
    const expectedPositionals: []const []const u8 = &.{ "positional1", "positional2", "positional3" };
    try std.testing.expectEqual({}, r1.positionals.tuple);
    try std.testing.expectEqualDeep(expectedPositionals, r1.positionals.reminder.?);

    const r2 = try tstParse(allocator, "program --test positional1 positional2 positional3", struct { @"test": bool = false });
    try std.testing.expect(r2.options.@"test");
    try std.testing.expectEqual({}, r2.positionals.tuple);
    try std.testing.expectEqualDeep(expectedPositionals, r2.positionals.reminder.?);

    const r3 = try tstParse(allocator, "program -- --test positional1 positional2 positional3", struct { @"test": bool = false });
    try std.testing.expect(!r3.options.@"test");
    const expectSkip: []const []const u8 = &.{ "--test", "positional1", "positional2", "positional3" };
    try std.testing.expectEqual({}, r3.positionals.tuple);
    try std.testing.expectEqualDeep(expectSkip, r3.positionals.reminder.?);

    const r4 = try tstParse(
        allocator,
        "program --test - pos1 verb1 pos2 pos3",
        struct {
            @"test": bool = false,
            pub const Verb1 = struct {};
            pub const Verb = union(enum) { verb1: Verb1 };
        },
    );
    try std.testing.expect(r4.options.@"test");
    const expectL1Pos: []const []const u8 = &.{ "-", "pos1" };
    try std.testing.expectEqual({}, r4.positionals.tuple);
    try std.testing.expectEqualDeep(expectL1Pos, r4.positionals.reminder.?);
    const expectL2Pos: []const []const u8 = &.{ "pos2", "pos3" };
    try std.testing.expectEqual({}, r4.verb.?.verb1.positionals.tuple);
    try std.testing.expectEqualDeep(expectL2Pos, r4.verb.?.verb1.positionals.reminder.?);
}

test "parse custom positionals" {
    const t = std.testing;
    const base = &t.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        pub const Positionals = PositionalOf(.{
            .TupleType = struct { i32, [2]u8 },
            .ReminderType = void,
        });
    };

    const r1 = try tstParse(allocator, "program 2 ab", Spec);
    try t.expectEqual(2, r1.positionals.tuple.@"0");
    try t.expectEqualStrings("ab", &r1.positionals.tuple.@"1");
    try t.expectEqual({}, r1.positionals.reminder);

    try t.expectError(Spec.Positionals.CollectError.MissingPositionalField, tstParse(allocator, "program 2", Spec));
    try t.expectError(@TypeOf(r1).Error.CursorNotDone, tstParse(allocator, "program 2 ab 3", Spec));

    const Spec2 = struct {
        pub const Positionals = PositionalOf(.{
            .TupleType = struct { i32 },
            .ReminderType = void,
        });
        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .ensureCursorDone = false,
        };
    };
    const r2 = try tstParse(allocator, "program 2 ab 3", Spec2);
    try t.expectEqual(2, r2.positionals.tuple.@"0");
    try t.expectEqual({}, r2.positionals.reminder);
}

test "parse custom positionals with verb" {
    const t = std.testing;
    const base = &t.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        pub const Positionals = PositionalOf(.{
            .TupleType = struct { [1]u8 },
            .ReminderType = void,
        });
        pub const Copy = struct {
            pub const Positionals = EmptyPositionalsOf;
            pub const GroupMatch: GroupMatchConfig(@This()) = .{
                .ensureCursorDone = false,
            };
        };
        pub const Verb = union(enum) {
            copy: Copy,
        };
        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .ensureCursorDone = false,
        };
    };

    const r1 = try tstParse(allocator, "program copy 2", Spec);
    try t.expectEqualStrings("2", &r1.positionals.tuple.@"0");
    try t.expectEqualStrings(@tagName(Spec.Verb.copy), @tagName(r1.verb.?));

    const r2 = try tstParse(allocator, "program 2 5", Spec);
    try t.expectEqualStrings("2", &r2.positionals.tuple.@"0");
    try t.expectEqual(null, r2.verb);

    const r3 = try tstParse(allocator, "program - 3", Spec);
    try t.expectEqualStrings("-", &r3.positionals.tuple.@"0");
    try t.expectEqual(null, r3.verb);
}

test "parse verb" {
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        verbose: ?bool = null,

        pub const Copy = struct {
            src: []const u8 = undefined,
        };

        pub const Paste = struct {
            target: []const u8 = undefined,
        };

        pub const Verb = union(enum) {
            copy: Copy,
            paste: Paste,
        };

        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .mandatoryVerb = true,
        };
    };
    try std.testing.expectError(GroupTracker(Spec).Error.MissingVerb, tstParse(allocator, "program", Spec));
    const r1 = try tstParse(allocator, "program copy --src file1", Spec);
    const r2 = try tstParse(allocator, "program paste --target file2", Spec);
    const r3 = try tstParse(allocator, "program --verbose false copy --src file3", Spec);
    const r4 = try tstParse(allocator, "program --verbose true paste --target file4", Spec);
    const r5 = try tstParse(allocator, "program --verbose true paste --target file4 positional1", Spec);

    try std.testing.expectEqual(Spec.Copy, @TypeOf(r1.verb.?.copy.options));
    try std.testing.expectEqualStrings("file1", r1.verb.?.copy.options.src);
    try std.testing.expectEqual(null, r1.options.verbose);
    try std.testing.expectEqual(Spec.Paste, @TypeOf(r2.verb.?.paste.options));
    try std.testing.expectEqualStrings("file2", r2.verb.?.paste.options.target);
    try std.testing.expectEqual(null, r2.options.verbose);
    try std.testing.expectEqual(Spec.Copy, @TypeOf(r3.verb.?.copy.options));
    try std.testing.expectEqualStrings("file3", r3.verb.?.copy.options.src);
    try std.testing.expectEqual(false, r3.options.verbose.?);
    try std.testing.expectEqual(Spec.Paste, @TypeOf(r4.verb.?.paste.options));
    try std.testing.expectEqualStrings("file4", r4.verb.?.paste.options.target);
    try std.testing.expectEqual(true, r4.options.verbose.?);
    try std.testing.expectEqual(Spec.Paste, @TypeOf(r5.verb.?.paste.options));
    try std.testing.expectEqualStrings("file4", r5.verb.?.paste.options.target);
    try std.testing.expectEqual(true, r5.options.verbose);
    const expected: []const []const u8 = &.{"positional1"};
    try std.testing.expectEqualDeep(expected, r5.verb.?.paste.positionals.reminder.?);
}

// TODO: move to example
test "parse with custom codec" {
    const base = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base);
    defer arena.deinit();
    const allocator = arena.allocator();
    const Spec = struct {
        i: i32 = undefined,
        i2: i32 = undefined,
        arf: i32 = undefined,
        // NOTE: This is handled by CodecIn (not default)
        b: bool = undefined,

        pub const SpecFieldEnum = std.meta.FieldEnum(@This());
        pub const Spc = @This();

        pub const Codec = struct {
            innerCodec: CodecIn = .{},

            pub const CodecIn = argCodec.ArgCodec(Spc);
            pub const Error = CodecIn.Error;

            pub fn supports(
                comptime Tx: type,
                comptime tag: SpecFieldEnum,
            ) bool {
                _ = tag;
                return comptime switch (@typeInfo(Tx)) {
                    .int => true,
                    else => false,
                };
            }

            pub fn parseByType(
                self: *@This(),
                comptime Tx: type,
                comptime tag: SpecFieldEnum,
                allc: *const Allocator,
                crsor: *CodecIn.CursorT,
            ) Error!Tx {
                if (comptime !supports(Tx, tag)) {
                    return try CodecIn.parseByType(self, Tx, tag, allc, crsor);
                }

                return try switch (@typeInfo(Tx)) {
                    .int => switch (tag) {
                        .arf => self.parseSumAll(Tx, tag, allc, crsor),
                        .i => self.parseIntX2(Tx, crsor),
                        // forces inner-codec behaviour rather than self for ints other than
                        // .arf and .i
                        else => self.innerCodec.parseByType(Tx, tag, allc, crsor),
                    },
                    else => unreachable,
                };
            }

            pub fn parseIntX2(
                self: *@This(),
                comptime Tx: type,
                cursor: *CodecIn.CursorT,
            ) Error!Tx {
                _ = self;
                return (try PrimitiveCodec.parseInt(
                    Tx,
                    cursor,
                )) * 2;
            }

            pub fn parseSumAll(
                self: *@This(),
                comptime Tx: type,
                comptime tag: SpecFieldEnum,
                allc: *const Allocator,
                cursor: *CodecIn.CursorT,
            ) Error!Tx {
                var r: Tx = 0;
                const arr = (try PrimitiveCodec.parseArray(
                    &self.innerCodec,
                    []const Tx,
                    tag,
                    allc,
                    cursor,
                ));
                defer allc.free(arr);
                for (arr) |n| {
                    r += n;
                }
                return r;
            }
        };
    };
    const r = try tstParse(
        allocator,
        \\program
        \\--i 31
        \\--i2 31
        \\--arf "[1,2,3,4,5]"
        \\--b
    ,
        Spec,
    );
    try std.testing.expectEqual(62, r.options.i);
    try std.testing.expectEqual(31, r.options.i2);
    try std.testing.expectEqual(15, r.options.arf);
    try std.testing.expectEqual(true, r.options.b);
}

// TODO: move to example
test "parse verb with custom codec" {
    const base = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        verbose: ?bool = null,

        pub const Copy = struct {
            src: []const u8 = undefined,
        };

        pub const Paste = struct {
            target: []const u8 = undefined,

            pub const SpecFieldEnum = std.meta.FieldEnum(@This());
            pub const Spc = @This();

            pub const Codec = struct {
                innerCodec: CodecIn = .{},

                pub const CodecIn = argCodec.ArgCodec(Spc);
                pub const Error = error{
                    UnsupportedPathInFileName,
                } || CodecIn.Error;

                pub fn supports(
                    comptime Tx: type,
                    comptime tag: SpecFieldEnum,
                ) bool {
                    return tag == .target and Tx == []const u8;
                }

                pub fn parseByType(
                    self: *@This(),
                    comptime Tx: type,
                    comptime tag: SpecFieldEnum,
                    allc: *const Allocator,
                    crsor: *CodecIn.CursorT,
                ) Error!Tx {
                    if (comptime !supports(Tx, tag)) {
                        return try CodecIn.parseByType(self, Tx, tag, allc, crsor);
                    }

                    return try switch (@typeInfo(Tx)) {
                        .pointer => |ptr| if (ptr.child == u8 and tag == .target) self.parsePath(
                            Tx,
                            allc,
                            crsor,
                        ) else unreachable,
                        else => unreachable,
                    };
                }

                pub fn parsePath(
                    self: *@This(),
                    comptime Tx: type,
                    allc: *const Allocator,
                    crsor: *CodecIn.CursorT,
                ) Error!Tx {
                    _ = self;
                    const file = try PrimitiveCodec.parseString(Tx, crsor);
                    if (std.mem.indexOf(u8, file, "/") != null) return Error.UnsupportedPathInFileName;

                    var fullPath = try allc.alloc(u8, 19 + file.len);
                    _ = &fullPath;
                    var buff: [*]u8 = @as([*]u8, @ptrCast(fullPath));
                    @memcpy(buff, "~/.config/file/");
                    buff += 15;
                    @memcpy(buff, file);
                    buff += file.len;
                    @memcpy(buff, ".png");
                    buff -= 15 + file.len;
                    return fullPath;
                }
            };
        };

        pub const Verb = union(enum) {
            copy: Copy,
            paste: Paste,
        };
    };
    const r1 = try tstParse(
        allocator,
        "program paste --target file1",
        Spec,
    );
    try std.testing.expectEqualStrings(
        "~/.config/file/file1.png",
        r1.verb.?.paste.options.target,
    );

    try std.testing.expectError(
        Spec.Paste.Codec.Error.UnsupportedPathInFileName,
        tstParse(allocator, "program paste --target /file1", Spec),
    );

    const r2 = try tstParse(
        allocator,
        "program copy --src file1",
        Spec,
    );
    try std.testing.expectEqualStrings(
        "file1",
        r2.verb.?.copy.options.src,
    );
}

test "validate require" {
    const t = std.testing;
    const base = &std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(base.*);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Spec = struct {
        i: i32 = undefined,
        i2: i32 = undefined,
        i3: ?i32 = null,
        i4: ?i32 = null,
        i5: ?i32 = null,
        i6: ?i32 = null,

        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .mutuallyInclusive = &.{
                &.{ .i3, .i4 },
            },
            .mutuallyExclusive = &.{
                &.{ .i5, .i6 },
            },
            .required = &.{ .i, .i2 },
        };
    };
    const r = try tstParse(allocator,
        \\program
        \\--i 1
        \\--i2 2
        \\--i3 3
        \\--i4 4
        \\--i6 5
    , Spec);
    try t.expectEqualStrings("program", r.program.?);
}

test "parsed spec cleanup with verb" {
    const allocator = std.testing.allocator;
    var tstCursor = try TstArgCursor.init(&allocator,
        \\program
        \\--verbose
        \\copy
        \\--src "file1"
    );
    defer tstCursor.deinit();
    var cursor = tstCursor.asCursor();
    const Spec = struct {
        verbose: ?bool = null,

        pub const Copy = struct {
            // sentinels are cloned
            src: [:0]const u8 = undefined,
        };

        pub const Verb = union(enum) {
            copy: Copy,
        };
    };

    const r1 = try tstParseSpec(allocator, &cursor, Spec);
    defer r1.deinit();
}

test "help and inner help" {
    const Spec = struct {
        pub const Copy = struct {
            path: []const u8 = undefined,
            pub const Help: HelpData(@This()) = .{
                .description = "copy test",
                .shortDescription = "copy test",
            };
            pub const GroupMatch: GroupMatchConfig(@This()) = .{
                .required = &.{.path},
            };
        };
        pub const Verb = union(enum) {
            copy: Copy,
        };
        pub const GroupMatch: GroupMatchConfig(@This()) = .{
            .mandatoryVerb = true,
        };
        pub const Help: HelpData(@This()) = .{
            .description = "test",
        };
    };

    var dcur: coll.DebugCursor = .{
        .data = &.{
            "program",
        },
    };
    var c = dcur.asCursor();

    const Res = SpecResponseWithConfig(Spec, _HelpConf{}, true);
    var r = Res.init(std.testing.allocator);
    if (r.parse(&c)) |err| {
        try std.testing.expectEqualStrings(
            \\Failed with reason: MissingVerb
            \\
            \\  test
            \\
            \\Commands: [Required]
            \\
            \\  copy        copy test
            \\
        , err.message.?);
    } else {
        unreachable;
    }

    dcur = .{
        .data = &.{
            "program",
            "copy",
        },
    };
    c = dcur.asCursor();

    r = Res.init(std.testing.allocator);
    if (r.parse(&c)) |err| {
        try std.testing.expectEqualStrings(
            \\Failed with reason: MissingRequiredField
            \\
            \\  copy test
            \\
            \\Options:
            \\
            \\  --path
            \\
        , err.message.?);
    } else {
        unreachable;
    }
}
