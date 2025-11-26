const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const args = @import("args.zig");
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
        const stat = file.stat() catch
            return DetectTypeError.UnableToQueryFd;

        const fileType = detectType(file, &stat);
        return .{
            .file = file,
            .stat = stat,
            .accessType = detectAccessType(fileType),
            .fileType = fileType,
        };
    }
};

pub const FileCursor = struct {
    fileArgsOpt: ?[]const []const u8,
    firstFile: bool = true,
    current: ?std.fs.File = null,
    idx: usize = 0,

    pub fn init(argsRes: *const args.ArgsRes) @This() {
        return .{
            .fileArgsOpt = argsRes.positionals.reminder,
        };
    }

    pub const OpenError = std.fs.File.OpenError || std.posix.RealPathError;
    pub fn next(self: *@This()) OpenError!?std.fs.File {
        assert(self.current == null);
        if (self.firstFile and self.fileArgsOpt == null) {
            self.current = std.fs.File.stdin();
            return self.current;
        }

        if (self.fileArgsOpt) |fileArgs| {
            const target = fileArgs[self.idx];
            if (target.len == 1 and target[0] == '-') {
                self.current = std.fs.File.stdin();
            } else {
                // TODO: handle folders
                const filePath = if (std.fs.path.isAbsolute(target))
                    target
                else rv: {
                    var buff: [4098]u8 = undefined;
                    break :rv try std.fs.cwd().realpath(target, &buff);
                };
                self.current = try std.fs.openFileAbsolute(
                    filePath,
                    .{ .mode = .read_only },
                );
            }
        }

        return self.current;
    }

    pub fn closeCurrent(self: *@This()) void {
        assert(self.current != null);
        self.current.?.close();
        self.current = null;
        if (self.firstFile)
            self.firstFile = false
        else
            self.idx += 1;
    }
};
