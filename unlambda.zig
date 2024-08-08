const std = @import("std");
const builtin = @import("builtin");

pub const Func = union(enum) {
    const Id = u32;

    /// Function application operator: `fg <=> f(g)
    apply: [2]Id,

    /// identity function
    i,

    /// "constant factory" ``kxy -> x
    k,
    k1: Id,

    /// substitution function: ```sxyz -> ``xz`yz
    s,
    s1: Id,
    s2: [2]Id,

    /// "void" `vx -> v
    v,

    /// delayed function. The evaluation of the argument of this function will be delayed.
    d,
    d1: Id,

    /// print a char to stdout.
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

    pub fn interpret(self: *Runtime, code: []const Func) !Func {
        const n = self.memory.items.len;
        try self.memory.ensureUnusedCapacity(code.len);
        for (code) |f| {
            self.memory.appendAssumeCapacity(f);
        }
        if (code[n] != .apply) {
            @panic("malformed unlambda code. Should start with apply symbol '`'");
        }
        return self._interpret(@intCast(n));
    }

    pub fn get(self: *const Runtime, n: Func.Id) Func {
        return self.memory.items[n];
    }

    pub fn parse(self: *Runtime, code: []const u8) ![]const u8 {
        if (code.len == 0) @panic("unexpected end of code");
        switch (code[0]) {
            '`' => {
                const remaining = try self.parse(code[1..]);
                const n: Func.Id = @intCast(self.memory.items.len - 1);
                const remaining2 = try self.parse(remaining);
                const m: Func.Id = @intCast(self.memory.items.len - 1);
                try self.memory.append(.{ .apply = .{ n, m } });
                // if (remaining2.len != 0) std.debug.panic("Invalide unlambda code. Trailing code: {s}", .{remaining2});
                return remaining2;
            },
            inline 'd', 'i', 's', 'k', 'v' => |name| {
                const f = @unionInit(Func, &.{name}, {});
                try self.memory.append(f);
                return code[1..];
            },
            'r' => {
                try self.memory.append(r);
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
            .apply => |fg| {
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
                if (f0 == .d) {
                    const delayed: Func = .{ .apply = .{ xy[1], g_id } };
                    const n_delayed = self.push(delayed) catch unreachable; // TODO: propagate oom
                    return .{ .d1 = n_delayed };
                }
                const g0 = self.call(self.get(xy[1]), g_id);
                const g0_id = self.push(g0) catch unreachable; // TODO: propagate oom
                return self.call(f0, g0_id);
            },
            .v => .v,
            .d => .{ .d1 = g_id },
            .d1 => |f0_id| {
                // Force the evaluation of the delayed.
                const f0 = self.get(f0_id);
                return self.call(f0, g_id);
            },
            .apply => |fg| {
                // We need to resolve this apply to be able to call it.
                const f0 = self.call(self.get(fg[0]), fg[1]);
                return self.call(f0, g_id);
            },
        };
    }

    pub fn push(self: *Runtime, f: Func) !Func.Id {
        try self.memory.append(f);
        return @intCast(self.memory.items.len - 1);
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
        try std.testing.expectEqualSlices(Func, &.{ p('*'), v, .{ .apply = .{ 0, 1 } } }, runtime.memory.items);
    }

    {
        runtime.memory.clearRetainingCapacity();
        _ = try runtime.parse("`d`.*i");
        try std.testing.expectEqualSlices(Func, &.{ d, p('*'), i, .{ .apply = .{ 1, 2 } }, .{ .apply = .{ 0, 3 } } }, runtime.memory.items);
    }
    {
        runtime.memory.clearRetainingCapacity();
        _ = try runtime.parse("``d`.*ii ");
        try std.testing.expectEqualSlices(Func, &.{ d, p('*'), i, .{ .apply = .{ 1, 2 } }, .{ .apply = .{ 0, 3 } }, i, .{ .apply = .{ 4, 5 } } }, runtime.memory.items);
    }
}

test "print" {
    // `.*v -> print *
    try testOutputEql("*", &.{ .{ .apply = .{ 1, 2 } }, p('*'), v });
    try testCodeOutput("`.*v", "*");
}

test "delayed" {
    // `d`ri -> create a delayed
    try testCodeOutput("`d`ri", "");
    try testOutputEql("", &.{ .{ .apply = .{ 1, 2 } }, d, .{ .apply = .{ 3, 4 } }, r, i });

    // ``d`.*ii -> create a delayed, then force the evaluation -> print new line
    try testCodeOutput("``d`rii", "\n");
    try testOutputEql("\n", &.{ .{ .apply = .{ 1, 6 } }, .{ .apply = .{ 2, 3 } }, d, .{ .apply = .{ 4, 5 } }, r, i, i });

    try testCodeOutput("```s`kdri", "");
}

fn testFn(code: []const u8, in_outs: []const [2]Func) !void {
    var runtime = try Runtime.init(std.testing.allocator);
    defer runtime.deinit();
    _ = try runtime.parse(code);

    const n: u32 = @intCast(runtime.memory.items.len);
    for (in_outs) |in_out| {
        const in, const out = in_out;
        defer runtime.memory.items.len = n;
        try runtime.memory.append(in);
        try runtime.memory.append(.{ .apply = .{ n - 1, n } });
        const res = runtime._interpret(n + 1);
        try std.testing.expectEqual(out, res);
    }
}

test s {
    // ``skk is the identity
    try testFn("``skk", &.{ .{ v, v }, .{ p('*'), p('*') } });
}
