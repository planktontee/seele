const std = @import("std");
const units = @import("zpec").units;

pub const StackSizes = .{
    .@"8mb" = units.ByteUnit.mb * 8,
    .@"16mb" = units.ByteUnit.mb * 16,
};
