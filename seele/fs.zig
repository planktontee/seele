const std = @import("std");
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

pub const FileArgType = union(enum) {
    stdin,
    file: []const u8,

    pub fn name(self: *const @This()) []const u8 {
        return switch (self.*) {
            .stdin => "stdin",
            .file => |fileArg| fileArg,
        };
    }
};

pub const FileCursor = struct {
    files: [1]FileArgType = undefined,
    current: ?std.fs.File = null,
    idx: usize = 0,

    pub fn init(argsRes: *const args.ArgsRes) @This() {
        var self: @This() = .{};

        if (argsRes.positionals.reminder) |reminder| {
            const target = reminder[0];
            if (target.len == 1 and target[0] == '-') {
                self.files[0] = .stdin;
            } else {
                self.files[0] = .{ .file = reminder[0] };
            }
        } else {
            self.files[0] = .stdin;
        }

        return self;
    }

    pub fn currentType(self: *const @This()) FileArgType {
        return self.files[self.idx];
    }

    pub fn next(self: *@This()) OpenError!?std.fs.File {
        std.debug.assert(self.current == null);
        if (self.idx >= self.files.len) return null;
        const fType = self.files[self.idx];

        self.current = switch (fType) {
            .file => |fileArg| try open(fileArg),
            .stdin => std.fs.File.stdin(),
        };

        return self.current;
    }

    pub const OpenError = std.fs.File.OpenError || std.posix.RealPathError;

    // TODO: handle other fd-types by actually querying them
    pub fn open(fileArg: []const u8) OpenError!std.fs.File {
        const filePath = if (std.fs.path.isAbsolute(fileArg))
            fileArg
        else rv: {
            var buff: [4098]u8 = undefined;
            break :rv try std.fs.cwd().realpath(fileArg, &buff);
        };
        return try std.fs.openFileAbsolute(filePath, .{ .mode = .read_only });
    }

    pub fn close(self: *@This()) void {
        std.debug.assert(self.current != null);
        self.current.?.close();
        self.current = null;
        self.idx += 1;
    }
};
