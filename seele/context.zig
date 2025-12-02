const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const mem = @import("mem.zig");
const FileWriter = std.fs.File.Writer;
const Writer = std.Io.Writer;
const Allocator = std.mem.Allocator;
const DebugAlloc = std.heap.DebugAllocator(.{
    .safety = true,
});

scrapSize: ?usize,
scrapAlloc: Allocator,
inSize: ?usize,
inAlloc: Allocator,
outSize: ?usize,
outAlloc: Allocator,
fdWriter: *FileWriter,
stderrW: *Writer,

pub fn init(
    self: *@This(),
    scrapSize: ?usize,
    scrapAlloc: Allocator,
    inSize: ?usize,
    inAlloc: Allocator,
    outSize: ?usize,
    outAlloc: Allocator,
    fdWriter: *FileWriter,
) void {
    self.* = .{
        // NOTE: Split matters because of how stack allocator works
        // if I put the input buffer before the output buffer, it cant
        // grow and vice-versa
        .scrapSize = scrapSize,
        .scrapAlloc = scrapAlloc,
        .inSize = inSize,
        .inAlloc = inAlloc,
        .outSize = outSize,
        .outAlloc = outAlloc,
        .fdWriter = fdWriter,
        .stderrW = &fdWriter.interface,
    };
}

pub fn deinit(self: *@This()) void {
    self.stderrW.flush() catch @panic("Failed last stderr flush");

    // NOTE: we are purposefully not cleaning up memory to improve process teardown
    // it will be left to the OS
    if (builtin.mode == .Debug) {
        self.scrapAlloc.free(self.stderrW.buffer);

        const scrapDba: *DebugAlloc = @ptrCast(@alignCast(self.scrapAlloc.ptr));
        const inDba: *DebugAlloc = @ptrCast(@alignCast(self.inAlloc.ptr));
        const outDba: *DebugAlloc = @ptrCast(@alignCast(self.outAlloc.ptr));
        if (scrapDba.deinit() == .leak) @panic("Scrap memory leaked!");
        if (inDba.deinit() == .leak) @panic("Input memory leaked!");
        if (outDba.deinit() == .leak) @panic("Output memory leaked!");
    }
}

pub var instance: *@This() = undefined;
