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
const sink = @import("sink.zig");
const ArgsRes = @import("args.zig").ArgsRes;

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
        if (usingStack == true) {
            if (inDba.deinit() == .leak) @panic("Input memory leaked!");
            if (outDba.deinit() == .leak) @panic("Output memory leaked!");
        }
    }
}

pub var usingStack: bool = false;

pub var instance: *@This() = undefined;

pub var event: sink.Event = undefined;
pub var baseEvent: sink.BaseEvent = undefined;

pub var chunksBuff: [32][]const u8 = undefined;
pub var slices: [][]const u8 = undefined;
pub var buffAt: usize = 0;
pub var lineNBuff: [std.fmt.count("{d}", .{std.math.maxInt(usize)})]u8 = undefined;

pub fn slice(size: usize) void {
    @This().slices = @This().chunksBuff[@This().buffAt .. @This().buffAt + size];
}

pub fn getSlices() [][]const u8 {
    return @This().chunksBuff[0 .. @This().buffAt + @This().slices.len];
}

pub var sinkp: sink.Sink = undefined;

pub var argsRes: *const ArgsRes = undefined;
