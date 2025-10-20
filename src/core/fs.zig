const std = @import("std");
const units = @import("zpec").units;
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

pub fn detectType(file: File) DetectTypeError!FileType {
    const stat = file.stat() catch return DetectTypeError.UnableToQueryFd;
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
