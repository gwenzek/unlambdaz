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
            .i, .k, .s, .v, .d, .c => _ = try writer.write(@tagName(self)),
            .apply => |fg| try writer.print("`({d},{d})", .{ fg[0], fg[1] }),
            ._k1 => |f| try writer.print("k1({d})", .{f}),
            ._d1 => |f| try writer.print("d1({d})", .{f}),
            ._s1 => |f| try writer.print("s1({d})", .{f}),
            ._s2 => |fg| try writer.print("s2({d},{d})", .{ fg[0], fg[1] }),
            ._cont => |ptr| try writer.print("cont({})", .{ptr}),
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

    var runtime: Runtime = .{ .memory = std.ArrayList(Func).init(std.testing.allocator), .stdout = undefined };
    defer runtime.deinit();
    parse(&runtime.memory, code.items) catch {
        return;
    };

    std.log.warn("interpreting {s}", .{code.items});
    _ = try runtime._interpret(0);
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

    _ = try runtime._interpret(0);
}

pub const Runtime = struct {
    memory: std.ArrayList(Func),
    output: std.BoundedArray(u8, 4096) = .{},
    stdout: std.fs.File,

    _should_resume: Func.Id = std.math.maxInt(Func.Id),

    pub fn init(allocator: std.mem.Allocator) !Runtime {
        return .{
            .memory = std.ArrayList(Func).init(allocator),
            .stdout = if (builtin.is_test) undefined else std.io.getStdOut(),
        };
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
                return self.call(fg[0], fg[1]) catch |err| switch (err) {
                    error.Interrupted => {
                        return self._interpret(self._should_resume);
                    },
                    inline else => |e| e,
                };
            },
            else => std.debug.panic("malformed unlambda code: unexpected {} at {}", .{ x, n }),
        }
    }

    pub const CallError = error{ OutOfMemory, Interrupted };

    pub fn call(self: *Runtime, f_id: Func.Id, g_id: Func.Id) CallError!Func {
        const f = self.get(f_id);
        const g = self.get(g_id);
        std.log.warn("call({}, {})", .{ f, g });
        return switch (f) {
            .i => g,
            .print => |char| {
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
                const f0 = try self.call(xy[0], g_id);
                if (f0 == .d) {
                    const delayed: Func = .{ .apply = .{ xy[1], g_id } };
                    const n_delayed = try self.push(delayed);
                    return .{ ._d1 = n_delayed };
                }

                const g0 = try self.call(xy[1], g_id);
                const g0_id = try self.push(g0);

                if (f0 == .c) {
                    @panic("c");
                }
                const f0_id = try self.push(f0);
                return try self.call(f0_id, g0_id);
            },
            .v => .v,
            .d => .{ ._d1 = g_id },
            ._d1 => |f0_id| {
                // Force the evaluation of the delayed.
                return self.call(f0_id, g_id);
            },
            .c => {
                const res_id = try self.push(undefined);
                const origin = f_id -| 1;
                // Edit the calling apply. If the contination trigger,
                // it will be evaluated a second time using the value passed to the continuation.
                self.memory.items[origin].apply[0] = res_id;
                const cont_id = try self.push(.{ ._cont = .{ .origin = origin, .res_ptr = res_id } });
                return try self.call(g_id, cont_id);
            },
            ._cont => |cont| {
                self.memory.items[cont.res_ptr] = switch (g) {
                    .apply => |fg| try self.call(fg[0], fg[1]),
                    else => g,
                };
                self._should_resume = cont.origin;
                return error.Interrupted;
            },
            .apply => |fg| {
                // We need to resolve this apply to be able to call it.
                const f0 = try self.call(fg[0], fg[1]);
                if (f0 == .c) {
                    @panic("c");
                }
                const f0_id = try self.push(f0);
                return try self.call(f0_id, g_id);
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
    try parse(&bytecode, code);

    var runtime: Runtime = .{ .memory = bytecode, .stdout = undefined };
    defer runtime.deinit();

    _ = try runtime._interpret(0);
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
    try testCodeOutput("`c``s`kr``si`ki", "");
    // try testFn("``s`kc``s`k`sv``ss`k`ki", &.{.{ i, i }});
}

test "hard test cases found by fuzzer" {
    // try testCodeOutput("``c`v`vv``c`ri`c`s`vs", "\n");
    try testCodeOutput("``c`ri `ci", "\n");

    // ` `c`ri `ci
    // ` ``ri<cont> `ci
    // ` <cont> `ci

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
