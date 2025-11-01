const std = @import("std");
const units = @import("zcasp").units;
const File = std.fs.File;
const Reader = std.Io.Reader;

pub const DetectTypeError = error{
    UnableToQueryFd,
};

pub const FileType = enum {
    tty,
    characterDevice,
    file,
    pipe,
    generic,
};

pub fn detectType(file: File, stat: *const File.Stat) FileType {
    switch (stat.kind) {
        .character_device => {
            return if (file.isTty()) .tty else .characterDevice;
        },
        .file => return .file,
        .named_pipe => return .pipe,
        else => return .generic,
    }
    unreachable;
}

pub const AccesType = enum {
    streaming,
    positional,
};

pub fn detectAccessType(fileType: FileType) AccesType {
    return switch (fileType) {
        .tty,
        .generic,
        .pipe,
        => .streaming,
        .characterDevice,
        .file,
        => .positional,
    };
}

// DLS to avoid syscalls
pub const DetailedFile = struct {
    file: File,
    // NOTE: this is frozen in time at from
    stat: File.Stat,
    accessType: AccesType,
    fileType: FileType,

    pub fn from(file: File) DetectTypeError!@This() {
        const stat = file.stat() catch return DetectTypeError.UnableToQueryFd;
        const fileType = detectType(file, &stat);
        return .{
            .file = file,
            .stat = stat,
            .accessType = detectAccessType(fileType),
            .fileType = fileType,
        };
    }
};
