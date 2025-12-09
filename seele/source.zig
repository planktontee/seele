const std = @import("std");
const builtin = @import("builtin");
const units = @import("regent").units;
const fs = @import("fs.zig");
const c = @import("c.zig").c;
const context = @import("context.zig");
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

    pub fn fromType(buffType: SourceBufferType) @This() {
        return switch (buffType) {
            .growing,
            => .{
                .growing = .{
                    .initialSize = units.ByteUnit.kb * 64,
                    .growthFactor = 3,
                },
            },
            .mmap => .mmap,
        };
    }
};

pub fn pickSourceBuffer(fDetailed: *const fs.DetailedFile, isRecursive: bool) SourceBufferType {
    // NOTE: capped sources wont read more than cap even with iovecs
    return switch (fDetailed.fileType) {
        .tty,
        // TODO: probe pibes / request more size
        .pipe,
        .characterDevice,
        => .growing,
        // TODO: choose mmap when file size is within stack size
        .file,
        => if (isRecursive) .growing else .mmap,
    };
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
    buffer: []align(std.heap.page_size_min) u8,
    reader: Reader,
    isInvalidFlag: bool = false,
    validateBin: bool = false,

    pub fn init(
        self: *@This(),
        buffer: []align(std.heap.page_size_min) u8,
        validateBin: bool,
    ) void {
        self.buffer = buffer;
        self.reader = .fixed(buffer);
        self.validateBin = validateBin;
        // TODO: this is mutch faster than checking every line
        // however for match counted and cases were we abruptly stop the match
        // this will be really slow and we need a different behaviour based on those flags
        self.isInvalidFlag = if (validateBin) FileValidatorReader.isInvalid(self.buffer) else false;
    }

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        var r = &self.reader;
        const line = r.takeDelimiterInclusive('\n') catch |e| switch (e) {
            std.Io.Reader.DelimiterError.EndOfStream => {
                if (r.bufferedLen() == 0) return .eof;
                const slice = r.buffered();
                r.toss(slice.len);

                return .{ .eofChunk = slice };
            },
            std.Io.Reader.DelimiterError.ReadFailed,
            => return ReadEvent.Error.ReadFailed,
            std.Io.Reader.DelimiterError.StreamTooLong => unreachable,
        };
        return .{ .line = line };
    }

    pub fn isInvalid(self: *const @This()) bool {
        return self.isInvalidFlag;
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        if (builtin.mode != .Debug) return;
        std.posix.munmap(self.buffer);
        allocator.destroy(self);
    }

    // NOTE:
    // This is actually not that bad for smaller files
    pub fn loadSource(self: *@This(), fDetailed: *const fs.DetailedFile) MmapBufferError!void {
        std.posix.munmap(self.buffer);
        self.init(try mmapBuffer(fDetailed), self.validateBin);
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

// Paper-thin layer over the backend Reader
// It will keep both of them in sync between vtable runs and
// perform binary checks for new data
pub const FileValidatorReader = struct {
    interface: Reader,
    backend: *Reader,
    isInvalidFlag: bool = false,

    pub fn init(backend: *Reader) @This() {
        return .{
            .interface = .{
                .buffer = backend.buffer,
                .seek = backend.seek,
                .end = backend.end,
                .vtable = &.{
                    .stream = &@This().stream,
                    .discard = &@This().discard,
                    .readVec = &@This().readVec,
                    .rebase = &@This().rebase,
                },
            },
            .backend = backend,
        };
    }

    pub fn stream(r: *Reader, w: *Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
        const self: *@This() = @fieldParentPtr("interface", r);
        self.syncBackend();
        const chunk = self.backend.vtable.stream(self.backend, w, limit) catch |e| switch (e) {
            else => {
                self.checkFile();
                self.syncFromBackend();
                return e;
            },
        };
        self.checkFile();
        self.syncFromBackend();
        return chunk;
    }

    pub fn discard(r: *Reader, limit: std.Io.Limit) std.Io.Reader.Error!usize {
        const self: *@This() = @fieldParentPtr("interface", r);
        self.syncBackend();
        const chunk = self.backend.vtable.discard(self.backend, limit) catch |e| switch (e) {
            else => {
                self.syncFromBackend();
                return e;
            },
        };
        self.syncFromBackend();
        return chunk;
    }

    pub fn readVec(r: *Reader, data: [][]u8) std.Io.Reader.Error!usize {
        const self: *@This() = @fieldParentPtr("interface", r);
        self.syncBackend();
        const chunk = self.backend.vtable.readVec(self.backend, data) catch |e| switch (e) {
            else => {
                self.checkFile();
                self.syncFromBackend();
                return e;
            },
        };
        self.checkFile();
        self.syncFromBackend();
        return chunk;
    }

    pub fn rebase(r: *Reader, capacity: usize) std.Io.Reader.RebaseError!void {
        const self: *@This() = @fieldParentPtr("interface", r);
        self.syncBackend();
        self.backend.vtable.rebase(self.backend, capacity) catch |e| switch (e) {
            else => {
                self.syncFromBackend();
                return e;
            },
        };
        self.syncFromBackend();
    }

    pub inline fn syncFromBackend(self: *@This()) void {
        self.interface.seek = self.backend.seek;
        self.interface.end = self.backend.end;
        self.interface.buffer = self.backend.buffer;
    }

    pub inline fn syncBackend(self: *@This()) void {
        self.backend.seek = self.interface.seek;
        self.backend.end = self.interface.end;
        self.backend.buffer = self.interface.buffer;
    }

    pub fn checkFile(self: *@This()) void {
        if (self.isInvalidFlag) return;
        if (self.interface.end >= self.backend.end) return;

        const slice = self.backend.buffer[self.interface.end..self.backend.end];
        self.isInvalidFlag = isInvalid(slice);
    }

    pub fn isInvalid(slice: []const u8) bool {
        return isBinary(slice);
    }

    pub fn isBinary(chunk: []const u8) bool {
        var remaining = chunk;

        if (comptime std.simd.suggestVectorLength(u8)) |VLen| {
            const Chunk = @Vector(VLen, u8);

            while (remaining.len >= VLen) {
                const chunkVec: Chunk = remaining[0..VLen].*;

                if (@reduce(.Min, chunkVec) == '\x00') {
                    return true;
                }
                remaining = remaining[VLen..];
            }
        }

        for (remaining) |chr| if (chr == '\x00') {
            return true;
        };
        return false;
    }
};

pub const GrowingLineReader = struct {
    allocator: std.mem.Allocator,
    growthFactor: usize,
    fsReader: File.Reader,
    fileValidatorR: FileValidatorReader,
    validateBin: bool,

    pub fn init(
        self: *@This(),
        allocator: std.mem.Allocator,
        growthFactor: usize,
        buff: []u8,
        fDetailed: *const fs.DetailedFile,
        validateBin: bool,
    ) void {
        self.allocator = allocator;
        self.growthFactor = growthFactor;
        self.validateBin = validateBin;
        self.newReader(buff, fDetailed);
    }

    pub inline fn newReader(self: *@This(), buff: []u8, fDetailed: *const fs.DetailedFile) void {
        self.fsReader = switch (fDetailed.accessType) {
            .streaming => fDetailed.file.readerStreaming(buff),
            .positional => fDetailed.file.reader(buff),
        };
        self.fileValidatorR = .init(&self.fsReader.interface);
    }

    pub const Config = struct {
        growthFactor: usize,
        initialSize: usize,
    };

    pub fn nextLine(self: *@This()) ReadEvent.Error!ReadEvent {
        var reader = if (self.validateBin)
            &self.fileValidatorR.interface
        else
            &self.fsReader.interface;

        const line = reader.takeDelimiterInclusive('\n') catch |e| switch (e) {
            std.Io.Reader.DelimiterError.EndOfStream => {
                if (reader.bufferedLen() == 0) return .eof;
                const slice = reader.buffered();
                reader.toss(slice.len);
                return .{ .eofChunk = slice };
            },
            std.Io.Reader.DelimiterError.StreamTooLong => {
                const targetSize = reader.buffer.len * self.growthFactor;
                const newBuff: []u8 = self.allocator.remap(
                    reader.buffer,
                    targetSize,
                ) orelse rv: {
                    // NOTE:
                    // remap will return null if it wants you to do the work, so here we are
                    const nb = try self.allocator.alloc(u8, targetSize);
                    @memcpy(nb[0..reader.buffer.len], reader.buffer);
                    self.allocator.free(reader.buffer);
                    break :rv nb;
                };
                reader.buffer = newBuff;
                return try self.nextLine();
            },
            std.Io.Reader.DelimiterError.ReadFailed,
            => return ReadEvent.Error.ReadFailed,
        };
        return .{ .line = line };
    }

    pub fn isInvalid(self: *const @This()) bool {
        return self.fileValidatorR.isInvalidFlag;
    }

    pub fn loadSource(self: *@This(), fDetailed: *const fs.DetailedFile) !void {
        self.newReader(self.fsReader.interface.buffer, fDetailed);
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        if (builtin.mode != .Debug) return;
        self.allocator.free(self.fileValidatorR.interface.buffer);
        allocator.destroy(self);
    }
};

pub const SourceReader = union(SourceBufferType) {
    growing: *GrowingLineReader,
    mmap: *MmapLineReader,

    pub const InitError = std.mem.Allocator.Error ||
        MmapLineReader.MmapBufferError ||
        fs.DetectTypeError;

    pub fn init(
        fDetailed: *const fs.DetailedFile,
        allocator: std.mem.Allocator,
        isRecursive: bool,
        validateBin: bool,
    ) InitError!@This() {
        return try .initWithBufferType(
            pickSourceBuffer(fDetailed, isRecursive),
            fDetailed,
            allocator,
            validateBin,
        );
    }

    pub fn initWithBufferType(
        bufferType: SourceBufferType,
        fDetailed: *const fs.DetailedFile,
        allocator: std.mem.Allocator,
        validateBin: bool,
    ) InitError!@This() {
        return switch (SourceBuffer.fromType(bufferType)) {
            .growing => |config| rv: {
                const inPlace = try allocator.create(GrowingLineReader);
                errdefer allocator.destroy(inPlace);

                const buff = try allocator.alloc(u8, config.initialSize);

                inPlace.init(
                    allocator,
                    config.growthFactor,
                    buff,
                    fDetailed,
                    validateBin,
                );

                break :rv .{ .growing = inPlace };
            },
            .mmap => rv: {
                const mmapSrc = try allocator.create(MmapLineReader);
                errdefer allocator.destroy(mmapSrc);

                const buff: []align(std.heap.page_size_min) u8 = try MmapLineReader.mmapBuffer(fDetailed);
                mmapSrc.init(buff, validateBin);

                break :rv .{ .mmap = mmapSrc };
            },
        };
    }
};

pub const Source = struct {
    sourceReader: SourceReader,
    fDetailed: *const fs.DetailedFile,

    pub fn init(
        fDetailed: *const fs.DetailedFile,
        allocator: std.mem.Allocator,
        isRecursive: bool,
        validateBin: bool,
    ) SourceReader.InitError!@This() {
        return .{
            .sourceReader = try .init(fDetailed, allocator, isRecursive, validateBin),
            .fDetailed = fDetailed,
        };
    }

    pub fn isInvalid(self: *const @This()) bool {
        return switch (self.sourceReader) {
            inline else => |source| source.isInvalid(),
        };
    }

    pub fn loadSource(self: *@This(), fDetailed: *const fs.DetailedFile) SourceReader.InitError!void {
        switch (self.sourceReader) {
            inline else => |source| {
                return try source.loadSource(fDetailed);
            },
        }
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
