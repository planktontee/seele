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

pub fn pickSourceBuffer(fileType: fs.FileType) SourceBuffer {
    // NOTE: capped sources wont read more than cap even with iovecs
    switch (fileType) {
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

    pub fn deinit(self: *@This()) void {
        std.posix.munmap(self.buffer);
    }

    pub const MmapBufferError = std.posix.RealPathError || std.posix.MMapError;

    pub fn mmapBuffer(file: File) MmapBufferError![]align(std.heap.page_size_min) u8 {
        const stats = try file.stat();
        const fSize = stats.size;

        return try std.posix.mmap(
            null,
            fSize,
            std.posix.PROT.READ,
            .{
                .TYPE = .SHARED,
                .NONBLOCK = true,
                .POPULATE = true,
            },
            file.handle,
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

    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.fsReader.interface.buffer);
    }
};

pub const GrowingDoubleBufferSource = struct {
    allocator: std.mem.Allocator,
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

    pub fn deinit(self: *@This()) void {
        self.growingWriter.deinit();
    }
};

pub const SourceReader = union(SourceBufferType) {
    growingDoubleBuffer: *GrowingDoubleBufferSource,
    inPlaceGrowingBuffer: *InPlaceGrowingReader,
    mmap: *MmapSource,

    pub const InitError = std.mem.Allocator.Error ||
        MmapSource.MmapBufferError ||
        fs.DetectTypeError;

    pub fn init(file: File, allocator: std.mem.Allocator, container: anytype) InitError!@This() {
        const inputFileType = try fs.detectType(file);
        const inputAccessType = fs.detectAccessType(inputFileType);
        const sourceBuffer = pickSourceBuffer(inputFileType);

        return switch (sourceBuffer) {
            .growingDoubleBuffer => |config| rv: {
                const buff = try allocator.alloc(u8, config.readBuffer);
                const fsReader = switch (inputAccessType) {
                    .streaming => file.readerStreaming(buff),
                    .positional => file.reader(buff),
                };
                const writer = try std.Io.Writer.Allocating.initCapacity(
                    std.heap.page_allocator,
                    config.targetInitialSize,
                );

                const growing: *GrowingDoubleBufferSource = @ptrCast(@alignCast(container));
                growing.* = .{
                    .allocator = allocator,
                    .fsReader = fsReader,
                    .growingWriter = writer,
                };
                break :rv .{ .growingDoubleBuffer = growing };
            },
            .inPlaceGrowingBuffer => |config| rv: {
                const buff = try allocator.alloc(u8, config.initialSize);
                const fsReader = switch (inputAccessType) {
                    .streaming => file.readerStreaming(buff),
                    .positional => file.reader(buff),
                };

                const inPlace: *InPlaceGrowingReader = @ptrCast(@alignCast(container));
                inPlace.* = .{
                    .allocator = allocator,
                    .growthFactor = config.growthFactor,
                    .fsReader = fsReader,
                };
                break :rv .{ .inPlaceGrowingBuffer = inPlace };
            },
            .mmap => rv: {
                const buff: []align(std.heap.page_size_min) u8 = try MmapSource.mmapBuffer(file);
                const mmapSrc: *MmapSource = @ptrCast(@alignCast(container));
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

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        switch (self.sourceReader) {
            inline else => |source| {
                return try source.nextLine();
            },
        }
    }

    pub fn deinit(self: *@This()) void {
        switch (self.sourceReader) {
            inline else => |source| source.deinit(),
        }
    }
};
