const std = @import("std");
const c = @import("c.zig").c;
const builtin = @import("builtin");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const args = @import("args.zig");
const units = @import("regent").units;
const File = std.Io.File;
const Reader = std.Io.Reader;
const Context = @import("context.zig");
const regent = @import("regent");

pub const DetectTypeError = error{
    UnreadableFileType,
} || std.Io.Cancelable;

pub const FileType = enum {
    tty,
    characterDevice,
    file,
    pipe,
};

pub fn detectType(file: File, stat: *const File.Stat) DetectTypeError!FileType {
    switch (stat.kind) {
        .character_device => {
            return if (try file.isTty(Context.io)) .tty else .characterDevice;
        },
        .file => return .file,
        .named_pipe => return .pipe,
        else => return DetectTypeError.UnreadableFileType,
    }
    unreachable;
}

pub const AccesType = enum {
    streaming,
    positional,

    pub fn fromIoMode(mode: anytype) @This() {
        return switch (mode) {
            .streaming, .streaming_simple => .streaming,
            .positional, .positional_simple => .positional,
            .failure => unreachable,
        };
    }
};

pub fn detectAccessType(fileType: FileType) AccesType {
    return switch (fileType) {
        .tty,
        .pipe,
        => .streaming,
        .characterDevice,
        .file,
        => .positional,
    };
}

// DLS to avoid syscalls
pub const DetailedFile = struct {
    path: []const u8 = undefined,
    file: File,
    // NOTE: this is frozen in time at from
    // there's no need to make this a ptr because DetailedFile is always used as a ptr
    // We would need a copy either way for stack -> heap, so we gain nothing
    stat: File.Stat,
    accessType: AccesType,
    fileType: FileType,

    pub fn fromFStream(comptime mode: regent.fs.Mode, fstream: regent.fs.FileStream(mode), path: []const u8) !@This() {
        return .{
            .path = path,
            .file = fstream.stream.file,
            .stat = fstream.stat,
            .accessType = .fromIoMode(fstream.stream.mode),
            .fileType = try detectType(fstream.stream.file, &fstream.stat),
        };
    }

    pub fn next(
        comptime mode: regent.fs.Mode,
        fc: *regent.fs.FileCursor(mode),
        context: regent.ergo.Context,
    ) !?@This() {
        if (try fc.nextUnmanaged(context)) |fstream|
            return try .fromFStream(mode, fstream, fc.currentPath().?);
        return null;
    }
};
