const std = @import("std");
const builtin = @import("builtin");
const units = @import("regent").units;
const fs = @import("fs.zig");
const c = @import("c.zig").c;
const File = std.fs.File;
const Reader = std.Io.Reader;
const Writer = std.Io.Writer;

pub const SourceBufferType = enum {
    growing,
    mmap,
};

pub const SourceBuffer = union(SourceBufferType) {
    growing: GrowingLineReader.Config,
    mmap,
};

pub fn pickSourceBuffer(fDetailed: *const fs.DetailedFile) SourceBuffer {
    // NOTE: capped sources wont read more than cap even with iovecs
    switch (fDetailed.fileType) {
        .generic,
        .tty,
        .pipe,
        .characterDevice,
        => return .{
            .growing = .{
                .initialSize = units.ByteUnit.mb,
                .growthFactor = 3,
            },
        },
        .file,
        => return .mmap,
    }
    unreachable;
}

pub const ReadEvent = union(enum) {
    eof,
    eofChunk: []const u8,
    line: []const u8,

    pub const Error = error{
        ReadFailed,
        OutOfMemory,
    };
};

pub const MmapLineReader = struct {
    buffer: []align(std.heap.page_size_min) const u8,
    reader: Reader,

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        const line = self.reader.takeDelimiterInclusive('\n') catch |e| switch (e) {
            std.Io.Reader.DelimiterError.EndOfStream => {
                if (self.reader.bufferedLen() == 0) return .eof;
                const slice = self.reader.buffered();
                self.reader.toss(slice.len);

                return .{ .eofChunk = slice };
            },
            std.Io.Reader.DelimiterError.ReadFailed,
            => return ReadEvent.Error.ReadFailed,
            std.Io.Reader.DelimiterError.StreamTooLong => unreachable,
        };
        return .{ .line = line };
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        if (builtin.mode != .Debug) return;
        std.posix.munmap(self.buffer);
        allocator.destroy(self);
    }

    pub const MmapBufferError = std.posix.MMapError || std.posix.MadviseError;

    pub fn mmapBuffer(fDetailed: *const fs.DetailedFile) MmapBufferError![]align(std.heap.page_size_min) u8 {
        const ptr = try std.posix.mmap(
            null,
            @intCast(fDetailed.stat.size),
            std.posix.PROT.READ,
            .{
                .TYPE = .PRIVATE,
                .NONBLOCK = true,
                .POPULATE = true,
            },
            fDetailed.file.handle,
            0,
        );
        _ = std.os.linux.fadvise(
            fDetailed.file.handle,
            0,
            @bitCast(fDetailed.stat.size),
            c.POSIX_FADV_SEQUENTIAL,
        );
        return ptr;
    }
};

pub const GrowingLineReader = struct {
    allocator: std.mem.Allocator,
    growthFactor: usize,
    fsReader: File.Reader,

    pub const Config = struct {
        growthFactor: usize,
        initialSize: usize,
    };

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        var r = &self.fsReader;
        var reader = &r.interface;
        const line = reader.takeDelimiterInclusive('\n') catch |e| switch (e) {
            std.Io.Reader.DelimiterError.EndOfStream => {
                if (reader.bufferedLen() == 0) return .eof;
                const slice = reader.buffered();
                reader.toss(slice.len);
                return .{ .eofChunk = slice };
            },
            std.Io.Reader.DelimiterError.StreamTooLong => {
                const newBuff: []u8 = self.allocator.remap(
                    reader.buffer,
                    reader.buffer.len * self.growthFactor,
                ) orelse return ReadEvent.Error.OutOfMemory;
                reader.buffer = newBuff;
                return try self.nextLine();
            },
            std.Io.Reader.DelimiterError.ReadFailed,
            => return ReadEvent.Error.ReadFailed,
        };
        return .{ .line = line };
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        if (builtin.mode != .Debug) return;
        self.allocator.free(self.fsReader.interface.buffer);
        allocator.destroy(self);
    }
};

pub const SourceReader = union(SourceBufferType) {
    growing: *GrowingLineReader,
    mmap: *MmapLineReader,

    pub const InitError = std.mem.Allocator.Error ||
        MmapLineReader.MmapBufferError ||
        fs.DetectTypeError;

    pub fn init(fDetailed: *const fs.DetailedFile, allocator: std.mem.Allocator) InitError!@This() {
        const sourceBuffer = pickSourceBuffer(fDetailed);

        return switch (sourceBuffer) {
            .growing => |config| rv: {
                const inPlace = try allocator.create(GrowingLineReader);
                errdefer allocator.destroy(inPlace);

                const buff = try allocator.alloc(u8, config.initialSize);
                const fsReader = switch (fDetailed.accessType) {
                    .streaming => fDetailed.file.readerStreaming(buff),
                    .positional => fDetailed.file.reader(buff),
                };

                inPlace.* = .{
                    .allocator = allocator,
                    .growthFactor = config.growthFactor,
                    .fsReader = fsReader,
                };
                break :rv .{ .growing = inPlace };
            },
            .mmap => rv: {
                const mmapSrc = try allocator.create(MmapLineReader);
                errdefer allocator.destroy(mmapSrc);

                const buff: []align(std.heap.page_size_min) u8 = try MmapLineReader.mmapBuffer(fDetailed);

                mmapSrc.* = .{
                    .buffer = buff,
                    .reader = .fixed(buff),
                };
                break :rv .{ .mmap = mmapSrc };
            },
        };
    }
};

pub const Source = struct {
    sourceReader: SourceReader,

    pub fn init(fDetailed: *const fs.DetailedFile, allocator: std.mem.Allocator) SourceReader.InitError!@This() {
        return .{
            .sourceReader = try .init(fDetailed, allocator),
        };
    }

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        switch (self.sourceReader) {
            inline else => |source| {
                return try source.nextLine();
            },
        }
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        switch (self.sourceReader) {
            inline else => |source| source.deinit(allocator),
        }
    }
};
