pub const args = @import("lang/args.zig");
pub const collections = @import("lang/collections.zig");
pub const meta = @import("lang/meta.zig");
pub const units = @import("lang/units.zig");
pub const result = @import("lang/result.zig");

test {
    comptime {
        _ = @import("lang/args.zig");
        _ = @import("lang/collections.zig");
        _ = @import("lang/meta.zig");
        _ = @import("lang/units.zig");
        _ = @import("lang/result.zig");
    }
}
