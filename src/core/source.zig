const std = @import("std");
const units = @import("zpec").units;
const fs = @import("fs.zig");
const File = std.fs.File;
const Reader = std.Io.Reader;
const Writer = std.Io.Writer;

pub const SourceBufferType = enum {
    growingDoubleBuffer,
    inPlaceGrowingBuffer,
    mmap,
};

pub const SourceBuffer = union(SourceBufferType) {
    growingDoubleBuffer: GrowingDoubleBufferSource.Config,
    inPlaceGrowingBuffer: InPlaceGrowingReader.Config,
    mmap,
};

pub fn pickSourceBuffer(fDetailed: *const fs.DetailedFile) SourceBuffer {
    // NOTE: capped sources wont read more than cap even with iovecs
    switch (fDetailed.fileType) {
        // => return .{
        //     .growingDoubleBuffer = .{
        //         .readBuffer = units.PipeSize,
        //         .targetInitialSize = units.CacheSize.L3,
        //     },
        // },
        .generic,
        .tty,
        .pipe,
        .characterDevice,
        => return .{
            .inPlaceGrowingBuffer = .{
                .initialSize = units.CacheSize.L2,
                .growthFactor = 3,
            },
        },
        .file,
        => return .mmap,
    }
    unreachable;
}

pub const ReadEvent = union(enum) {
    endOfFile,
    endOfFileChunk: []const u8,
    line: []const u8,

    pub const Error = error{
        ReadFailed,
        OutOfMemory,
    };
};

pub const MmapSource = struct {
    buffer: []align(std.heap.page_size_min) const u8,
    reader: Reader,

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        const line = self.reader.takeDelimiterInclusive('\n') catch |e| switch (e) {
            std.Io.Reader.DelimiterError.EndOfStream => {
                if (self.reader.bufferedLen() == 0) return .endOfFile;
                const slice = self.reader.buffered();
                self.reader.toss(slice.len);

                return .{ .endOfFileChunk = slice };
            },
            std.Io.Reader.DelimiterError.ReadFailed,
            => return ReadEvent.Error.ReadFailed,
            std.Io.Reader.DelimiterError.StreamTooLong => unreachable,
        };
        return .{ .line = line };
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        std.posix.munmap(self.buffer);
        allocator.destroy(self);
    }

    pub const MmapBufferError = std.posix.RealPathError || std.posix.MMapError;

    pub fn mmapBuffer(fDetailed: *const fs.DetailedFile) MmapBufferError![]align(std.heap.page_size_min) u8 {
        return try std.posix.mmap(
            null,
            fDetailed.stat.size,
            std.posix.PROT.READ,
            .{
                .TYPE = .SHARED,
                .NONBLOCK = true,
                .POPULATE = true,
            },
            fDetailed.file.handle,
            0,
        );
    }
};

pub const InPlaceGrowingReader = struct {
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
                if (reader.bufferedLen() == 0) return .endOfFile;
                const slice = reader.buffered();
                reader.toss(slice.len);
                return .{ .endOfFileChunk = slice };
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
        self.allocator.free(self.fsReader.interface.buffer);
        allocator.destroy(self);
    }
};

pub const GrowingDoubleBufferSource = struct {
    fsReader: File.Reader,
    growingWriter: Writer.Allocating,

    pub const Config = struct {
        readBuffer: usize,
        targetInitialSize: usize,
    };

    fn recoverBuffer(self: *@This()) []const u8 {
        const slice = self.growingWriter.writer.buffered();
        _ = self.growingWriter.writer.consumeAll();
        return slice;
    }

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        var writer = &self.growingWriter.writer;
        _ = &writer;
        var reader = &self.fsReader.interface;

        _ = reader.streamDelimiter(writer, '\n') catch |e| switch (e) {
            Reader.StreamError.EndOfStream => {
                std.debug.assert(reader.bufferedLen() == 0);
                const slice = self.recoverBuffer();

                if (slice.len == 0) return .endOfFile;
                return .{ .endOfFileChunk = slice };
            },
            Reader.StreamError.ReadFailed,
            Reader.StreamError.WriteFailed,
            => return ReadEvent.Error.ReadFailed,
        };

        if (reader.bufferedLen() >= 1) {
            reader.streamExact(writer, 1) catch |e| switch (e) {
                Reader.StreamError.EndOfStream => return .{
                    .endOfFileChunk = self.recoverBuffer(),
                },
                Reader.StreamError.ReadFailed,
                Reader.StreamError.WriteFailed,
                => return ReadEvent.Error.ReadFailed,
            };
        }

        return .{ .line = self.recoverBuffer() };
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.growingWriter.deinit();
        allocator.destroy(self);
        self.* = undefined;
    }
};

pub const SourceReader = union(SourceBufferType) {
    growingDoubleBuffer: *GrowingDoubleBufferSource,
    inPlaceGrowingBuffer: *InPlaceGrowingReader,
    mmap: *MmapSource,

    pub const InitError = std.mem.Allocator.Error ||
        MmapSource.MmapBufferError ||
        fs.DetectTypeError;

    pub fn init(fDetailed: *const fs.DetailedFile, allocator: std.mem.Allocator) InitError!@This() {
        const sourceBuffer = pickSourceBuffer(fDetailed);

        return switch (sourceBuffer) {
            .growingDoubleBuffer => |config| rv: {
                const growing = try allocator.create(GrowingDoubleBufferSource);
                errdefer allocator.destroy(growing);

                const buff = try allocator.alloc(u8, config.readBuffer);
                errdefer allocator.free(buff);

                const fsReader = switch (fDetailed.accessType) {
                    .streaming => fDetailed.file.readerStreaming(buff),
                    .positional => fDetailed.file.reader(buff),
                };
                const writer = try std.Io.Writer.Allocating.initCapacity(
                    allocator,
                    config.targetInitialSize,
                );

                growing.* = .{
                    .fsReader = fsReader,
                    .growingWriter = writer,
                };
                break :rv .{ .growingDoubleBuffer = growing };
            },
            .inPlaceGrowingBuffer => |config| rv: {
                const inPlace = try allocator.create(InPlaceGrowingReader);
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
                break :rv .{ .inPlaceGrowingBuffer = inPlace };
            },
            .mmap => rv: {
                const mmapSrc = try allocator.create(MmapSource);
                errdefer allocator.destroy(mmapSrc);

                const buff: []align(std.heap.page_size_min) u8 = try MmapSource.mmapBuffer(fDetailed);

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
