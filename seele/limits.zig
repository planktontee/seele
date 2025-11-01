const std = @import("std");
const units = @import("regent").units;

pub const StackSizes = .{
    .@"8mb" = units.ByteUnit.mb * 8,
    .@"10mb" = units.ByteUnit.mb * 10,
    .@"16mb" = units.ByteUnit.mb * 16,
};
