const std = @import("std");
const builtin = @import("builtin");
const log = std.log.scoped(.unlambda);

pub const Func = union(enum) {
    const Id = u32;

    /// Function application operator: `fg <=> f(g)
    apply: [2]Id,

    /// identity function
    i,

    /// "constant factory" ``kxy -> x
    k,
    _k1: Id,

    /// substitution function: ```sxyz -> ``xz`yz
    s,
    _s1: Id,
    _s2: [2]Id,

    /// "void" `vx -> v
    v,

    /// delayed function. The evaluation of the argument of this function will be delayed.
    d,
    _d1: Id,

    /// call with current continuation
    c,
    _cont: *Func,

    /// print a char to stdout.
    print: u8,
    // TODO @, ?*, | to read stdin

    // TODO e
};

pub const i: Func = .i;
pub const k: Func = .k;
pub const s: Func = .s;
pub const v: Func = .v;
pub const r: Func = .{ .print = '\n' };
pub const d: Func = .d;
pub const c: Func = .c;

pub fn p(char: u8) Func {
    return .{ .print = char };
}

pub fn parse(out: *std.ArrayList(Func), code: []const u8) error{ OutOfMemory, Invalid }!void {
    var n = try _parse(out, code, 0);
    // detect trailing code that didn't get parsed. Whitespace is ok though.
    while (n < code.len) {
        switch (code[n]) {
            ' ', '\n', '\t' => n += 1,
            // TODO accept trailing comments
            else => {
                log.err("Invalide unlambda code. Trailing code at pos {d}: {s}|{s} ({d})", .{ n, code[0..n], code[n..], code.len });
                return error.Invalid;
            },
        }
    }
}

pub fn _parse(out: *std.ArrayList(Func), code: []const u8, pos: usize) error{ OutOfMemory, Invalid }!usize {
    if (pos >= code.len) {
        log.err("unexpected end of code", .{});
        return error.Invalid;
    }

    return switch (code[pos]) {
        '`' => {
            // reserve a slot for apply, but don't keep the pointer, which can be invalidated.
            _ = try out.addOne();
            const n: Func.Id = @intCast(out.items.len);
            const remaining = try _parse(out, code, pos + 1);
            const m: Func.Id = @intCast(out.items.len);
            const remaining2 = try _parse(out, code, remaining);
            out.items[n - 1] = .{ .apply = .{ n, m } };
            return remaining2;
        },
        inline 'd', 'i', 's', 'k', 'v', 'c' => |name| {
            const f = @unionInit(Func, &.{name}, {});
            try out.append(f);
            return pos + 1;
        },
        'r' => {
            try out.append(r);
            return pos + 1;
        },
        '.' => {
            try out.append(p(code[pos + 1]));
            return pos + 2;
        },
        // skip whitespaces
        ' ', '\n', '\t' => _parse(out, code, pos + 1),
        // TODO comments #...
        else => |char| std.debug.panic("Invalide unlambda code. unexpected char {d}", .{char}),
    };
}

pub const Runtime = struct {
    memory: std.ArrayList(Func),
    output: std.BoundedArray(u8, 4096) = .{},
    stdout: std.fs.File,

    pub fn init(allocator: std.mem.Allocator) !Runtime {
        const res: Runtime = .{
            .memory = std.ArrayList(Func).init(allocator),
            .stdout = std.io.getStdOut(),
        };

        // const well_known_funcs = [_]Func{ i, k, s, v, d };
        // try res.memory.appendSlice(&well_known_funcs);
        return res;
    }

    pub fn deinit(self: *const Runtime) void {
        self.memory.deinit();
    }

    pub fn interpret(self: *Runtime, code: []const Func) !Func {
        try self.memory.appendSlice(code);
        if (self.get(0) != .apply) {
            @panic("malformed unlambda code. Should start with apply symbol '`'");
        }
        return self._interpret(0);
    }

    pub fn get(self: *const Runtime, n: Func.Id) Func {
        return self.memory.items[n];
    }

    fn _interpret(self: *Runtime, n: Func.Id) error{OutOfMemory}!Func {
        std.log.warn("interpreting({any}) at {d}", .{ self.memory.items, n });
        const x = self.get(n);

        switch (x) {
            .apply => |fg| {
                const f = self.get(fg[0]);
                return self.call(f, fg[1]) catch |err| switch (err) {
                    error.Interrupted => {
                        @panic("congrats you managed to break my interpreter, with a cont that escape its c");
                    },
                    inline else => |e| e,
                };
            },
            else => std.debug.panic("malformed unlambda code: unexpected {} at {}", .{ x, n }),
        }
    }

    pub const CallError = error{ OutOfMemory, Interrupted };

    pub fn call(self: *Runtime, f: Func, g_id: Func.Id) CallError!Func {
        const g = self.get(g_id);
        std.log.warn("call({}, {})", .{ f, g });
        return switch (f) {
            .i => g,
            .print => |char| {
                if (builtin.is_test) {
                    self.output.appendAssumeCapacity(char);
                } else {
                    std.debug.print("{s}", .{&[1]u8{char}});
                }
                return g;
            },
            .k => .{ ._k1 = g_id },
            ._k1 => |cst| self.get(cst),
            .s => .{ ._s1 = g_id },
            ._s1 => |x| .{ ._s2 = .{ x, g_id } },
            ._s2 => |xy| {
                // The substitution operator requires allocation.
                // This is expected because 's' is what makes Unlambda Turing complete.
                // But currently we don't have a strategy to free memory.
                // The Unlambda one pager suggest using reference counting,
                // since it's not possible to create cycles.
                // TODO: implement RC and a free list.
                var f0 = try self.call(self.get(xy[0]), g_id);
                if (f0 == .d) {
                    const delayed: Func = .{ .apply = .{ xy[1], g_id } };
                    const n_delayed = self.push(delayed) catch unreachable; // TODO: propagate oom
                    return .{ ._d1 = n_delayed };
                }

                const g0 = try self.call(self.get(xy[1]), g_id);
                const g0_id = self.push(g0) catch unreachable; // TODO: propagate oom

                if (f0 == .c) {
                    f0 = .{ ._cont = &f0 };
                    const old_f0 = f0;
                    return self.call(f0, g_id) catch |err| switch (err) {
                        // Check if _cont was called.
                        error.Interrupted => if (f0 == ._cont and f0._cont == old_f0._cont) err else try self.call(f0, g_id),
                        else => return err,
                    };
                }

                return try self.call(f0, g0_id);
            },
            .v => .v,
            .d => .{ ._d1 = g_id },
            ._d1 => |f0_id| {
                // Force the evaluation of the delayed.
                const f0 = self.get(f0_id);
                return self.call(f0, g_id);
            },
            .c => .c,
            ._cont => |res| {
                res.* = g;
                return error.Interrupted;
            },
            .apply => |fg| {
                // We need to resolve this apply to be able to call it.
                var f0 = try self.call(self.get(fg[0]), fg[1]);
                if (f0 == .c) {
                    f0 = .{ ._cont = &f0 };
                    const old_f0 = f0;
                    return self.call(f0, g_id) catch |err| switch (err) {
                        // Check if _cont was called.
                        error.Interrupted => if (f0 == ._cont and f0._cont == old_f0._cont) err else try self.call(f0, g_id),
                        else => return err,
                    };
                }
                return try self.call(f0, g_id);
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
    var bytecode = std.ArrayList(Func).init(std.testing.allocator);
    defer bytecode.deinit();
    try parse(&bytecode, code);

    var runtime: Runtime = .{ .memory = bytecode, .stdout = undefined };

    _ = try runtime._interpret(0);
    try std.testing.expectEqualStrings(expected, runtime.output.constSlice());
}

test parse {
    var bytecode = std.ArrayList(Func).init(std.testing.allocator);
    defer bytecode.deinit();

    {
        bytecode.clearRetainingCapacity();
        try parse(&bytecode, "`.*v");
        try std.testing.expectEqualSlices(Func, &.{ .{ .apply = .{ 1, 2 } }, p('*'), v }, bytecode.items);
    }

    {
        bytecode.clearRetainingCapacity();
        try parse(&bytecode, "`d`.*i");
        try std.testing.expectEqualSlices(Func, &.{ .{ .apply = .{ 1, 2 } }, d, .{ .apply = .{ 3, 4 } }, p('*'), i }, bytecode.items);
    }
    {
        bytecode.clearRetainingCapacity();
        try parse(&bytecode, "``d`.*ii ");
        try std.testing.expectEqualSlices(Func, &.{ .{ .apply = .{ 1, 6 } }, .{ .apply = .{ 2, 3 } }, d, .{ .apply = .{ 4, 5 } }, p('*'), i, i }, bytecode.items);
    }
    {
        bytecode.clearRetainingCapacity();
        try parse(&bytecode, "``cir");
        try std.testing.expectEqualSlices(Func, &.{ .{ .apply = .{ 1, 4 } }, .{ .apply = .{ 2, 3 } }, c, i, r }, bytecode.items);
    }
}

test "print" {
    // `.*v -> print *
    try testOutputEql("*", &.{ .{ .apply = .{ 1, 2 } }, p('*'), v });
    try testCodeOutput("`.*v", "*");
}

test d {
    // `d`ri -> create a delayed
    try testCodeOutput("`d`ri", "");
    try testOutputEql("", &.{ .{ .apply = .{ 1, 2 } }, d, .{ .apply = .{ 3, 4 } }, r, i });

    // ``d`.*ii -> create a delayed, then force the evaluation -> print new line
    try testCodeOutput("``d`rii", "\n");
    try testOutputEql("\n", &.{ .{ .apply = .{ 1, 6 } }, .{ .apply = .{ 2, 3 } }, d, .{ .apply = .{ 4, 5 } }, r, i, i });

    // ```s`kdri -> ` ``kdi `ri -> `d`ri -> the printing is delayed
    try testCodeOutput("```s`kdri", "");
}

fn testFn(code: []const u8, in_outs: []const [2]Func) !void {
    var bytecode = std.ArrayList(Func).init(std.testing.allocator);
    try parse(&bytecode, code);
    var runtime: Runtime = .{ .memory = bytecode, .stdout = undefined };
    bytecode = undefined;
    // bytecode shouldn't be used anymore, it's owned by the runtime now.
    defer runtime.deinit();

    const n: u32 = @intCast(runtime.memory.items.len);
    for (in_outs) |in_out| {
        const in, const out = in_out;
        defer runtime.memory.items.len = n;
        const in_idx = try runtime.push(in);
        const apply_idx = try runtime.push(.{ .apply = .{ 0, in_idx } });
        const res = runtime._interpret(apply_idx);
        try std.testing.expectEqual(out, res);
    }
}

fn testFnIsId(code: []const u8) !void {
    const in_outs = [_][2]Func{ .{ v, v }, .{ p('*'), p('*') }, .{ i, i }, .{ d, d }, .{ c, c } };
    return testFn(code, &in_outs);
}

test s {
    // ``skk is the identity
    try testFnIsId("``skk");
}

pub fn main() !void {
    const fib =
        \\
        \\ ```s``s``sii`ki
        \\   `k.*``s``s`ks
        \\   ``s`k`s`ks``s``s`ks``s`k`s`kr``s`k`sikk
        \\   `k``s`ksk
    ;

    var bytecode = std.ArrayList(Func).init(std.heap.page_allocator);
    defer bytecode.deinit();
    try parse(&bytecode, fib);

    var runtime: Runtime = .{
        .memory = bytecode,
        .stdout = std.io.getStdOut(),
    };
    _ = try runtime.interpret(&.{});
}

test c {
    try testCodeOutput("``cir", "\n");
    try testCodeOutput("`c``s`kr``si`ki", "");
    try testFn("``s`kc``s`k`sv``ss`k`ki", &.{.{ i, i }});
}
