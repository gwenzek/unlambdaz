const std = @import("std");
const builtin = @import("builtin");
pub const Func = union(enum) {
    i,

    k,
    k1: *const Func,

    s,
    s1: *const Func,
    s2: [2]*const Func,

    v,

    d,
    d1: *const Func,

    print: u8,
};

pub fn apply(f: Func, g: *const Func) Func {
    eval(&f);
    if (f == .d) {
        return .{ .d1 = g };
    }
    eval(g);
    return call(f, g);
}

var _test_output_buffer: [4096]u8 = undefined;
var _test_output = std.ArrayListUnmanaged(u8).fromOwnedSlice(&_test_output_buffer);

pub fn eval(f: *const Func) void {
    switch (f.*) {
        .print => |c| {
            if (builtin.is_test) {
                _test_output.appendAssumeCapacity(c);
            } else {
                std.debug.print("{s}", .{c});
            }
        },
        else => {},
    }
}

pub fn call(f: Func, g: *const Func) Func {
    return switch (f) {
        .i, .print => g.*,
        .k => .{ .k1 = g },
        .k1 => |cst| cst.*,
        .s => .{ .s1 = g },
        .s1 => |x| .{ .s2 = .{ x, g } },
        .s2 => g.*, // TODO
        .v => .v,
        .d => .{ .d1 = g },
        .d1 => |f0| {
            eval(f0);
            return call(f0.*, g);
        },
    };
}

const v: Func = .v;
test "print" {
    _test_output.shrinkRetainingCapacity(0);
    _ = apply(.{ .print = '*' }, &v);
    try std.testing.expectEqualStrings("*", _test_output.items);
}
