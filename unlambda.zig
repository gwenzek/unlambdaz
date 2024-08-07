const std = @import("std");
const builtin = @import("builtin");

pub const Func = union(enum) {
    const Id = u32;

    apply2: [2]Id,

    i,

    k,
    k1: Id,

    s,
    s1: Id,
    s2: [2]Id,

    v,

    d,
    d1: Id,

    print: u8,
};

pub const i: Func = .i;
pub const k: Func = .k;
pub const s: Func = .s;
pub const v: Func = .v;
pub const d: Func = .d;

pub fn p(c: u8) Func {
    return .{ .print = c };
}

pub const r: Func = .{ .print = '\n' };

pub const Runtime = struct {
    output: std.BoundedArray(u8, 4096) = .{},
    memory: std.ArrayList(Func),

    pub fn init(allocator: std.mem.Allocator) !Runtime {
        const res: Runtime = .{
            .memory = std.ArrayList(Func).init(allocator),
        };

        // const well_known_funcs = [_]Func{ i, k, s, v, d };
        // try res.memory.appendSlice(&well_known_funcs);
        return res;
    }

    pub fn deinit(self: *const Runtime) void {
        self.memory.deinit();
    }

    pub fn interpret(self: *Runtime, code: []const Func) !void {
        const n = self.memory.items.len;
        try self.memory.ensureUnusedCapacity(code.len);
        for (code) |f| {
            self.memory.appendAssumeCapacity(f);
        }
        if (code[n] != .apply2) {
            @panic("malformed unlambda code. Should start with apply symbol '`'");
        }
        _ = self._interpret(@intCast(n));
        return;
    }

    pub fn get(self: *const Runtime, n: Func.Id) Func {
        return self.memory.items[n];
    }

    pub fn parse(self: *Runtime, code: []const u8) ![]const u8 {
        switch (code[0]) {
            '`' => {
                const remaining = try self.parse(code[1..]);
                const n: Func.Id = @intCast(self.memory.items.len - 1);
                const remaining2 = try self.parse(remaining);
                const m: Func.Id = @intCast(self.memory.items.len - 1);
                try self.memory.append(.{ .apply2 = .{ n, m } });
                // if (remaining2.len != 0) std.debug.panic("Invalide unlambda code. Trailing code: {s}", .{remaining2});
                return remaining2;
            },
            inline 'd', 'i', 's', 'k', 'v' => |name| {
                const f = @unionInit(Func, &.{name}, {});
                try self.memory.append(f);
                return code[1..];
            },
            '.' => {
                try self.memory.append(p(code[1]));
                return code[2..];
            },
            else => |c| std.debug.panic("Invalide unlambda code. unexpected char {d}", .{c}),
        }
    }

    fn _interpret(self: *Runtime, n: Func.Id) Func {
        std.log.warn("interpreting({any}) at {d}", .{ self.memory.items, n });
        const x = self.get(n);

        switch (x) {
            .apply2 => |fg| {
                const f = self.get(fg[0]);
                return self.call(f, fg[1]);
            },
            else => std.debug.panic("malformed unlambda code: unexpected {} at {}", .{ x, n }),
        }
    }

    pub fn call(self: *Runtime, f: Func, g_id: Func.Id) Func {
        const g = self.get(g_id);
        std.log.warn("call({}, {})", .{ f, g });
        return switch (f) {
            .i => g,
            .print => |c| {
                if (builtin.is_test) {
                    self.output.appendAssumeCapacity(c);
                } else {
                    std.debug.print("{s}", .{c});
                }
                return g;
            },
            .k => .{ .k1 = g_id },
            .k1 => |cst| self.get(cst),
            .s => .{ .s1 = g_id },
            .s1 => |x| .{ .s2 = .{ x, g_id } },
            .s2 => |xy| {
                const f0 = self.call(self.get(xy[0]), g_id);
                std.debug.assert(f0 != .d); // TODO
                const g0 = self.call(self.get(xy[1]), g_id);
                self.memory.append(g0) catch unreachable; // TODO: propagate oom
                return self.call(f0, @intCast(self.memory.items.len - 1));
            },
            .v => .v,
            .d => .{ .d1 = g_id },
            .d1 => |f0_id| {
                // Force the evaluation of the delayed.
                const f0 = self.get(f0_id);
                return self.call(f0, g_id);
            },
            .apply2 => |fg| {
                // We need to resolve this apply to be able to call it.
                const f0 = self.call(self.get(fg[0]), fg[1]);
                return self.call(f0, g_id);
            },
        };
    }
};

fn testOutputEql(expected: []const u8, code: []const Func) !void {
    var runtime = try Runtime.init(std.testing.allocator);
    defer runtime.deinit();

    _ = try runtime.interpret(code);
    try std.testing.expectEqualStrings(expected, runtime.output.constSlice());
}

fn testCodeOutput(code: []const u8, expected: []const u8) !void {
    var runtime = try Runtime.init(std.testing.allocator);
    defer runtime.deinit();
    _ = try runtime.parse(code);
    // parse put the apply at end
    _ = runtime._interpret(@intCast(runtime.memory.items.len - 1));
    try std.testing.expectEqualStrings(expected, runtime.output.constSlice());
}

test "parse" {
    var runtime = try Runtime.init(std.testing.allocator);
    defer runtime.deinit();

    {
        runtime.memory.clearRetainingCapacity();
        _ = try runtime.parse("`.*v");
        try std.testing.expectEqualSlices(Func, &.{ p('*'), v, .{ .apply2 = .{ 0, 1 } } }, runtime.memory.items);
    }

    {
        runtime.memory.clearRetainingCapacity();
        _ = try runtime.parse("`d`.*i");
        try std.testing.expectEqualSlices(Func, &.{ d, p('*'), i, .{ .apply2 = .{ 1, 2 } }, .{ .apply2 = .{ 0, 3 } } }, runtime.memory.items);
    }
    {
        runtime.memory.clearRetainingCapacity();
        _ = try runtime.parse("``d`.*ii ");
        try std.testing.expectEqualSlices(Func, &.{ d, p('*'), i, .{ .apply2 = .{ 1, 2 } }, .{ .apply2 = .{ 0, 3 } }, i, .{ .apply2 = .{ 4, 5 } } }, runtime.memory.items);
    }
}

test "print" {
    // `.*v -> print *
    try testOutputEql("*", &.{ .{ .apply2 = .{ 1, 2 } }, p('*'), v });
    try testCodeOutput("`.*v", "*");
}

test "delayed" {
    // `d`.*i -> create a delayed
    try testCodeOutput("`d`.*i", "");
    try testOutputEql("", &.{ .{ .apply2 = .{ 1, 2 } }, d, .{ .apply2 = .{ 3, 4 } }, p('*'), i });

    // ``d`.*ii -> create a delayed, then force the evaluation -> print *
    try testCodeOutput("``d`.*ii", "*");
    try testOutputEql("*", &.{ .{ .apply2 = .{ 1, 6 } }, .{ .apply2 = .{ 2, 3 } }, d, .{ .apply2 = .{ 4, 5 } }, p('*'), i, i });
}

// TODO test s
