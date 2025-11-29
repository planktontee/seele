const std = @import("std");
const c = @import("c.zig").c;
const builtin = @import("builtin");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const args = @import("args.zig");
const units = @import("regent").units;
const File = std.fs.File;
const Reader = std.Io.Reader;

pub const DetectTypeError = error{
    UnreadableFileType,
};

pub const FileType = enum {
    tty,
    characterDevice,
    file,
    pipe,
};

pub fn detectType(file: File, stat: *const File.Stat) DetectTypeError!FileType {
    switch (stat.kind) {
        .character_device => {
            return if (file.isTty()) .tty else .characterDevice;
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
    path: [3][]const u8 = undefined,
    file: File,
    // NOTE: this is frozen in time at from
    // there's no need to make this a ptr because DetailedFile is always used as a ptr
    // We would need a copy either way for stack -> heap, so we gain nothing
    stat: File.Stat,
    accessType: AccesType,
    fileType: FileType,

    pub fn from(
        file: File,
        baseDir: []const u8,
        filePath: []const u8,
        stat: *const std.fs.File.Stat,
    ) DetectTypeError!@This() {
        const fileType = try detectType(file, stat);
        return .{
            .path = .{
                baseDir,
                if (baseDir.len > 0) "/" else "",
                filePath,
            },
            .file = file,
            .stat = stat.*,
            .accessType = detectAccessType(fileType),
            .fileType = fileType,
        };
    }
};

pub const ActivateFile = struct {
    path: []const u8,
    file: std.fs.File,

    pub fn init(path: []const u8, file: std.fs.File) @This() {
        return .{
            .path = path,
            .file = file,
        };
    }
};

pub const FileCursor = struct {
    fileArgsOpt: ?[]const []const u8,
    stdinFilled: bool = true,
    current: ?DetailedFile = null,
    idx: usize = 0,
    walker: ?std.fs.Dir.Walker = null,
    recursive: bool = false,

    pub fn init(argsRes: *const args.ArgsRes, recursive: bool) @This() {
        return .{
            .fileArgsOpt = argsRes.positionals.reminder,
            .recursive = recursive,
        };
    }

    pub const OpenError = error{
        InvalidFileType,
    } ||
        std.fs.File.OpenError ||
        std.fs.File.StatError ||
        Allocator.Error ||
        DetectTypeError ||
        std.posix.RealPathError;

    fn nextFd(self: *@This()) OpenError!?ActivateFile {
        if (self.stdinFilled and self.fileArgsOpt == null)
            return .init(
                "(standard input)",
                std.fs.File.stdin(),
            );

        if (self.walker) |*walker| {
            while (try walker.next()) |entry| {
                switch (entry.kind) {
                    .character_device,
                    .named_pipe,
                    .file,
                    => {
                        return .init(
                            entry.path,
                            try entry.dir.openFile(
                                entry.basename,
                                .{ .mode = .read_only },
                            ),
                        );
                    },
                    else => continue,
                }
            }
            walker.deinit();
            self.idx += 1;
            self.walker = null;
        }

        if (self.fileArgsOpt) |fileArgs| {
            if (self.idx >= fileArgs.len) return null;
            self.stdinFilled = false;

            const target = fileArgs[self.idx];
            if (target.len == 1 and target[0] == '-') {
                return .init(
                    "(standard input)",
                    std.fs.File.stdin(),
                );
            } else {
                const path = if (std.fs.path.isAbsolute(target))
                    target
                else rv: {
                    var buff: [4096]u8 = undefined;
                    break :rv try std.fs.cwd().realpath(
                        target,
                        &buff,
                    );
                };

                return .init(
                    target,
                    try std.fs.openFileAbsolute(
                        path,
                        .{ .mode = .read_only },
                    ),
                );
            }
        }

        return null;
    }

    pub fn next(self: *@This(), allocator: Allocator) OpenError!?DetailedFile {
        assert(self.current == null);

        while (try self.nextFd()) |activeFile| {
            const path = activeFile.path;
            const f = activeFile.file;

            const stat = try f.stat();

            switch (stat.kind) {
                .character_device,
                .named_pipe,
                // TODO: check fstat size for pipes etc
                => {
                    self.current = try DetailedFile.from(
                        f,
                        if (self.walker != null) self.fileArgsOpt.?[self.idx] else "",
                        path,
                        &stat,
                    );
                    return self.current;
                },
                .file,
                => {
                    if (stat.size == 0) continue;
                    _ = std.os.linux.fadvise(
                        f.handle,
                        0,
                        @bitCast(stat.size),
                        c.POSIX_FADV_SEQUENTIAL,
                    );
                    _ = std.os.linux.fadvise(
                        f.handle,
                        0,
                        @bitCast(stat.size),
                        c.POSIX_FADV_NOREUSE,
                    );
                    self.current = try DetailedFile.from(
                        f,
                        if (self.walker != null) self.fileArgsOpt.?[self.idx] else "",
                        path,
                        &stat,
                    );
                    return self.current;
                },
                .directory,
                // TODO: test block devices
                .block_device,
                => {
                    if (!self.recursive) return OpenError.InvalidFileType;

                    var dir: std.fs.Dir = .{ .fd = f.handle };
                    self.walker = try dir.walk(allocator);
                    continue;
                },
                .sym_link,
                .unix_domain_socket,
                .whiteout,
                .door,
                .event_port,
                .unknown,
                => return OpenError.InvalidFileType,
            }
        }
        return null;
    }

    pub fn hasRelativePaths(self: *@This()) bool {
        assert(self.current != null);
        if (self.fileArgsOpt) |fileArgs| {
            if (fileArgs.len > 1) return true;
        }

        return self.walker != null;
    }

    pub fn closeCurrent(self: *@This()) void {
        assert(self.current != null);
        self.current.?.file.close();
        self.current = null;
        if (self.stdinFilled)
            self.stdinFilled = false
        else {
            if (self.walker == null)
                self.idx += 1;
        }
    }

    pub fn deinit(self: *@This()) void {
        if (builtin.mode != .Debug) return;

        if (self.current) |detailedFile| detailedFile.file.close();
        if (self.walker) |*walker| walker.deinit();
    }
};
