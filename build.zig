const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.addModule("seele", .{
        .root_source_file = b.path("seele.zig"),
        .target = target,
        .optimize = optimize,
        .omit_frame_pointer = optimize == .ReleaseFast,
    });
    module.addImport("regent", b.dependency("regent", .{
        .target = target,
        .optimize = optimize,
    }).module("regent"));
    module.addImport("zcasp", b.dependency("zcasp", .{
        .target = target,
        .optimize = optimize,
    }).module("zcasp"));

    const pcre2_dep = b.dependency("pcre2", .{
        .target = target,
        .optimize = optimize,
        .support_jit = true,
        .linkage = .static,
    });

    // TODO: find a better way of doing this
    var sljitTargetBuff: std.ArrayList(u8) = .empty;
    try sljitTargetBuff.print(b.allocator, "{s}/deps/sljit", .{
        pcre2_dep.path(".").getPath(b),
    });
    const sljitPathTarget = try sljitTargetBuff.toOwnedSlice(b.allocator);

    const sljit = b.dependency("sljit", .{
        .target = target,
        .optimize = optimize,
    });

    var rCode: u8 = undefined;
    _ = pcre2_dep.builder.runAllowFail(
        &.{
            "rmdir",
            sljitPathTarget,
        },
        &rCode,
        .Close,
    ) catch {};
    _ = pcre2_dep.builder.runAllowFail(
        &.{
            "ln",
            "-s",
            sljit.path(".").getPath(b),
            sljitPathTarget,
        },
        &rCode,
        .Close,
    ) catch {};
    module.linkLibrary(pcre2_dep.artifact("pcre2-8"));

    const unit_tests = b.addTest(.{
        .root_module = module,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const exe = b.addExecutable(.{
        .name = "seele",
        .root_module = module,
        .use_llvm = true,
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
