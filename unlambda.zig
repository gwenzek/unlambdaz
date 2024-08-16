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
    c: ?Id,
    _cont: struct { origin: Id, res_ptr: Id },

    /// print a char to stdout.
    print: u8,
    // TODO @, ?*, | to read stdin

    // TODO e

    pub fn format(
        self: Func,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .i, .k, .s, .v, .d => _ = try writer.write(@tagName(self)),
            .apply => |fg| try writer.print("`({d},{d})", .{ fg[0], fg[1] }),
            ._k1 => |f| try writer.print("k1({d})", .{f}),
            ._d1 => |f| try writer.print("d1({d})", .{f}),
            ._s1 => |f| try writer.print("s1({d})", .{f}),
            ._s2 => |fg| try writer.print("s2({d},{d})", .{ fg[0], fg[1] }),
            .c => |f| try writer.print("c({?})", .{f}),
            ._cont => |ptr| try writer.print("cont(origin={}, res={})", .{ ptr.origin, ptr.res_ptr }),
            .print => |char| {
                if (char == '\n') {
                    _ = try writer.write("r");
                } else {
                    try writer.print(".{s}", .{[1]u8{char}});
                }
            },
        }
    }
};

pub const i: Func = .i;
pub const k: Func = .k;
pub const s: Func = .s;
pub const v: Func = .v;
pub const r: Func = .{ .print = '\n' };
pub const d: Func = .d;
pub const c: Func = .{ .c = null };

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
                log.warn("Invalide unlambda code. Trailing code at pos {d}: {s}|{s} ({d})", .{ n, code[0..n], code[n..], code.len });
                return error.Invalid;
            },
        }
    }
}

pub fn _parse(out: *std.ArrayList(Func), code: []const u8, pos: usize) error{ OutOfMemory, Invalid }!usize {
    if (pos >= code.len) {
        log.warn("unexpected end of code", .{});
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
        inline 'd', 'i', 's', 'k', 'v' => |name| {
            const f = @unionInit(Func, &.{name}, {});
            try out.append(f);
            return pos + 1;
        },
        'c' => {
            try out.append(c);
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
        else => |char| {
            log.warn("Invalide unlambda code. unexpected char {d} at pos {d}", .{ char, pos });
            return error.Invalid;
        },
    };
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

test "fuzz parser" {
    // Fuzzing only works on linux atm, so I didn't test it's working correctly.
    const input_bytes = std.testing.fuzzInput(.{});

    var code = std.ArrayList(u8).init(std.testing.allocator);
    try fuzzingCodeGenerator(&code, input_bytes);
    defer code.deinit();

    var runtime = try Runtime.init(std.testing.allocator, &.{});
    defer runtime.deinit();
    parse(&runtime.memory, code.items) catch {
        return;
    };

    std.log.warn("interpreting {s}", .{code.items});
    _ = try runtime.interpret();
}

fn fuzzingCodeGenerator(code: *std.ArrayList(u8), input_bytes: []const u8) !void {
    const valid_chars = "iksvrdc";
    return switch (input_bytes.len) {
        0 => {},
        1 => try code.append(valid_chars[input_bytes.len % valid_chars.len]),
        2 => {
            try code.append(valid_chars[input_bytes[0] % valid_chars.len]);
            try code.append(valid_chars[input_bytes[1] % valid_chars.len]);
            return;
        },
        else => {
            const b = input_bytes[0];
            try code.append('`');
            var ratio: f32 = @floatFromInt(@divTrunc(b, 8));
            ratio /= (254.0 / 8.0);
            ratio *= @floatFromInt(input_bytes.len);
            const left: usize = @intFromFloat(@max(1.0, ratio));
            try fuzzingCodeGenerator(code, input_bytes[0..left]);
            try fuzzingCodeGenerator(code, input_bytes[left..]);
        },
    };
}

test fuzzingCodeGenerator {
    var code = std.ArrayList(u8).init(std.testing.allocator);
    try fuzzingCodeGenerator(&code, "hello world");
    defer code.deinit();

    var runtime: Runtime = .{ .memory = std.ArrayList(Func).init(std.testing.allocator), .stdout = undefined };
    defer runtime.deinit();
    parse(&runtime.memory, code.items) catch {
        return;
    };

    _ = try runtime.interpret();
}

pub const Runtime = struct {
    memory: std.ArrayList(Func),
    output: std.BoundedArray(u8, 4096) = .{},
    stdout: std.fs.File,

    _call_graph: std.AutoHashMapUnmanaged(Func.Id, Progress) = .{},
    _should_resume: Func.Id = std.math.maxInt(Func.Id),
    const Progress = struct { res: ?Func = null, caller: ?Func.Id };

    pub fn initFromCode(allocator: std.mem.Allocator, code: []const u8) !Runtime {
        var res = Runtime.init(allocator, &.{}) catch unreachable;
        try parse(&res.memory, code);
        return res;
    }

    pub fn init(allocator: std.mem.Allocator, code: []const Func) !Runtime {
        var res = .{
            .memory = std.ArrayList(Func).init(allocator),
            .stdout = if (builtin.is_test) undefined else std.io.getStdOut(),
        };
        try res.memory.appendSlice(code);
        return res;
    }

    pub fn deinit(self: *Runtime) void {
        self.memory.deinit();
        self._call_graph.deinit(self.memory.allocator);
    }

    pub fn interpret(
        self: *Runtime,
    ) !Func {
        if (self.get(0) != .apply) {
            @panic("malformed unlambda code. Should start with apply symbol '`'");
        }
        return self._interpret(0);
    }

    pub fn _interpret(self: *Runtime, start_id: Func.Id) !Func {
        std.log.warn("interpreting({any})", .{self.memory.items});
        var continuation: ?Func.Id = null;
        var id: Func.Id = start_id;
        try self.setCaller(.{ .caller = null, .callee = start_id });
        while (true) {
            continuation = try self.apply(id);
            if (continuation) |cont| {
                id = cont;
            } else break;
        }
        return self._call_graph.get(start_id).?.res.?;
    }

    /// Read memory at the given index.
    pub fn get(self: *const Runtime, n: Func.Id) Func {
        return self.memory.items[n];
    }

    /// Fetch the value at the given index, but returns null when it finds an apply.
    pub fn isReady(self: *Runtime, id: Func.Id) ?Func {
        return switch (self.get(id)) {
            .apply => if (self._call_graph.get(id)) |progress| progress.res else null,
            else => |x| x,
        };
    }

    pub fn apply(self: *Runtime, apply_id: Func.Id) error{OutOfMemory}!?Func.Id {
        const f_id, const g_id = self.get(apply_id).apply;
        std.log.warn("apply({}, {}, {})", .{ apply_id, f_id, g_id });
        std.log.warn("{any}", .{self.memory.items});
        const caller = if (self._call_graph.get(apply_id)) |progress| progress.caller else null;
        // if (self.isReady(apply_id)) |_| {
        // TODO: we have a problem here, those can be invalidated by a continuation.
        // We should avoid this. Currently this is only needed for s.
        // return caller;
        // }
        if (self.isReady(f_id) == null) {
            try self.setCaller(.{ .caller = apply_id, .callee = f_id });
            return f_id;
        }
        const f = self.isReady(f_id).?;
        if (f == .d) {
            const res: Func = if (self.isReady(g_id)) |g| g else .{ ._d1 = g_id };
            self.saveRes(apply_id, res);
            return caller;
        }
        if (self.isReady(g_id) == null) {
            try self.setCaller(.{ .caller = apply_id, .callee = g_id });
            return g_id;
        }
        const g = self.isReady(g_id).?;
        if (f == .c) {
            // Instead of applying c to g, we apply g to the current continuation.
            const res_ptr = try self.push(v);
            const cont = try self.push(.{ ._cont = .{ .origin = apply_id, .res_ptr = res_ptr } });
            self.memory.items[apply_id].apply = .{ g_id, cont };
            return apply_id;
        }

        std.log.warn("call({}: {}, {}: {})", .{ f_id, f, g_id, g });
        const res: Func = switch (f) {
            .c, .d => unreachable,
            .apply => unreachable,
            .i => g,
            .print => |char| print: {
                if (builtin.is_test) {
                    if (self.output.len < self.output.capacity()) {
                        // If test output is too long, we drop trailing bytes.
                        // This prevent the fuzzer to be limited by IO.
                        self.output.resize(0) catch unreachable;
                    }
                    self.output.appendAssumeCapacity(char);
                } else {
                    std.debug.print("{s}", .{&[1]u8{char}});
                }
                break :print g;
            },
            .k => .{ ._k1 = g_id },
            ._k1 => |cst| blk: {
                break :blk self.get(cst);
            },
            .s => .{ ._s1 = g_id },
            ._s1 => |x| .{ ._s2 = .{ x, g_id } },
            ._s2 => |xy| {
                // The substitution operator requires allocation.
                // This is expected because 's' is what makes Unlambda Turing complete.
                // But currently we don't have a strategy to free memory.
                // The Unlambda one pager suggest using reference counting,
                // since it's not possible to create cycles.
                // TODO: implement RC and a free list.

                // Note: originally `s` was making `call` directly,
                // and using Zig stack to store intermediary result.
                // but this can be perturbated by a continuation triggering.
                // So we do the expansion explicitly, then use the general interpret logic.
                const x = try self.push(.{ .apply = .{ xy[0], g_id } });
                const y = try self.push(.{ .apply = .{ xy[1], g_id } });
                // const z = try self.push(.{ .apply = .{ x, y } });
                try self.setCaller(.{ .caller = apply_id, .callee = x });
                try self.setCaller(.{ .caller = apply_id, .callee = y });
                const old = self.memory.items[apply_id];
                self.memory.items[apply_id].apply = .{ x, y };
                std.log.warn("```s rewrote {}: {} to ` `({},{}) `({},{})", .{ apply_id, old, self.get(xy[0]), g, self.get(xy[1]), g });
                return apply_id;
            },
            .v => v,
            // Force the evaluation of the delayed.
            ._d1 => |delayed| blk: {
                if (self.isReady(delayed)) |res| {
                    break :blk res;
                }
                try self.setCaller(.{ .caller = apply_id, .callee = delayed });
                return delayed;
            },
            ._cont => |cont| {
                // Store `g` as the return value of the original c.
                self.memory.items[cont.res_ptr] = g;
                // Invalidate the original apply result, so we apply it a seconde time with the new g.
                self._call_graph.getPtr(cont.origin).?.res = null;
                const og_g_id = self.memory.items[cont.origin].apply[0];
                self.memory.items[cont.origin].apply = .{ cont.res_ptr, og_g_id };
                // var stack_frame = cont.origin;
                // while (self._call_graph.getPtr(stack_frame)) |progress| {
                //     if (progress.res) |old_res| {
                //         std.log.warn("Invalidated result for {}: {}", .{stack_frame, old_res});
                //     }
                //     progress.res = null;
                //     stack_frame = progress.caller orelse break;
                // }
                const y = try self.push(.{ .apply = .{ xy[1], g_id } });
                try self.setCaller(.{ .caller = apply_id, .callee = z });
                std.log.warn("calling cont {}({})", .{ cont, g });
                return cont.origin;
            },
        };
        self.saveRes(apply_id, res);
        std.log.warn(" call -> {}", .{res});
        return caller;
    }

    pub const CallError = error{ OutOfMemory, Interrupted };

    pub fn pushCall(self: *Runtime, caller_id: Func.Id, f_id: Func.Id, g_id: Func.Id) CallError!Func {
        const a = try self.push(.{ .apply = .{ f_id, g_id } });
        try self.setCaller(.{ .caller = caller_id, .callee = a });
        return try self.call(a);
    }

    pub fn push(self: *Runtime, f: Func) !Func.Id {
        try self.memory.append(f);
        return @intCast(self.memory.items.len - 1);
    }

    pub fn setCaller(self: *Runtime, args: struct { caller: ?Func.Id, callee: Func.Id }) !void {
        try self._call_graph.put(self.memory.allocator, args.callee, .{ .caller = args.caller });
    }

    pub fn saveRes(self: *Runtime, callee: Func.Id, res: Func) void {
        std.debug.assert(res != .apply);
        const progress = self._call_graph.getPtr(callee).?;
        progress.res = res;
    }
};

fn testOutputEql(expected: []const u8, code: []const Func) !void {
    var runtime = try Runtime.init(std.testing.allocator, code);
    defer runtime.deinit();

    _ = try runtime.interpret();
    try std.testing.expectEqualStrings(expected, runtime.output.constSlice());
}

fn testCodeOutput(code: []const u8, expected: []const u8) !void {
    var runtime = try Runtime.initFromCode(std.testing.allocator, code);
    defer runtime.deinit();

    _ = try runtime.interpret();
    try std.testing.expectEqualStrings(expected, runtime.output.constSlice());
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

test c {
    try testCodeOutput("``cir", "\n");
    try testCodeOutput("```s `ck ir", "\n\n");
    // try testCodeOutput("`c``s`kr``si`ki", "");
    // ```s `ck ir -> ```s `k<cont> ir -> ` ``k<cont>r `ir -> ` <cont>r -> ```s r ir -> ` `rr `ir -> `r
    // try testFn("``s`kc``s`k`sv``ss`k`ki", &.{.{ i, i }});
}

test "hard test cases found by fuzzer" {
    // try testCodeOutput("``c`v`vv``c`ri`c`s`vs", "\n");
    // try testCodeOutput("``c`ri `ci", "\n");

    // ` `cv ` `c `ri`c`s`vs
    // ` v ` `c `ri `c  `sv
    // ` v ` `c `ri ``sv<cont>

}

pub fn main() !void {
    const fib =
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
