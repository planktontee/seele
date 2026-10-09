const std = @import("std");
const regentBuild = @import("regent").lib.build;

pub fn build(b: *std.Build) !void {
    var scrapBuf: [1 << 20]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&scrapBuf);
    const scrapAlloc = fba.allocator();

    var errBuf: [1024]u8 = undefined;
    const io = v: {
        var tIo = std.Io.Threaded.init_single_threaded;
        break :v tIo.io();
    };
    var eW = std.Io.File.stderr().writerStreaming(io, &errBuf);
    const ctx: regentBuild.Ctx = .{
        .allocator = scrapAlloc,
        .io = io,
        .errW = &eW.interface,
    };

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.addModule("seele", .{
        .root_source_file = b.path("seele.zig"),
        .target = target,
        .optimize = optimize,
        .omit_frame_pointer = optimize == .ReleaseFast,
        .strip = (optimize == .ReleaseFast and !(b.option(bool, "keep-symbols", "Keep symbols") orelse false)) or optimize == .ReleaseFast,
    });
    const regent = b.dependency("regent", .{
        .target = target,
        .optimize = optimize,
    }).module("regent");
    const zcasp = b.dependency("zcasp", .{
        .target = target,
        .optimize = optimize,
    }).module("zcasp");

    module.addImport("regent", regent);
    module.addImport("zcasp", zcasp);
    zcasp.addImport("regent", regent);

    const pcre2_dep = b.dependency("pcre2", .{
        .target = target,
        .optimize = optimize,
        .support_jit = true,
        .linkage = .static,
    });

    // TODO: find a better way of doing this
    const sljitPathTarget = try std.mem.concatMaybeSentinel(
        ctx.allocator,
        u8,
        &.{
            pcre2_dep.path(".").getPath(b),
            "/deps/sljit",
        },
        null,
    );
    defer ctx.allocator.free(sljitPathTarget);

    const sljit = b.dependency("sljit", .{
        .target = target,
        .optimize = optimize,
    });

    regentBuild.runCmdQuiet(&ctx, pcre2_dep.builder, &.{
        "rmdir",
        sljitPathTarget,
    });

    const sljitPathStub, _ = std.mem.cutLast(u8, sljitPathTarget, "sljit").?;

    regentBuild.runCmdQuiet(&ctx, pcre2_dep.builder, &.{
        "mkdir",
        sljitPathStub,
    });
    const sljitPath = sljit.path(".").getPath(b);
    regentBuild.runCmdQuiet(
        &ctx,
        pcre2_dep.builder,
        &.{
            "ln",
            "-sfn",
            sljitPath,
            sljitPathTarget,
        },
    );
    try regentBuild.runCmdAssert(
        false,
        true,
        &ctx,
        pcre2_dep.builder,
        &.{
            "readlink",
            sljitPathTarget,
        },
        sljitPath,
    );

    module.linkLibrary(pcre2_dep.artifact("pcre2-8"));

    const test_filters = b.option(
        []const []const u8,
        "test-filter",
        "Filter tests by string match",
    ) orelse &.{};

    const unit_tests = b.addTest(.{
        .root_module = module,
        .filters = test_filters,
        .use_llvm = true,
    });
    b.installArtifact(unit_tests);
    const run_unit_tests = b.addRunArtifact(unit_tests);

    b.getInstallStep().dependOn(&run_unit_tests.step);

    const exe = b.addExecutable(.{
        .name = "seele",
        .root_module = module,
        .use_llvm = true,
    });
    b.installArtifact(exe);
}
