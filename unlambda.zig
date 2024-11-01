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
    _cont: struct { apply: Id },

    /// print a char to stdout.
    print: u8,

    read,
    // TODO @, ?*, | to read stdin

    /// The e function takes an argument X.
    /// It exits immediately, pretending (if the interpreter cares) that the result of the evaluation of the program is X.
    /// e (“exit”) only in Unlambda version 2 and greater
    e,

    pub fn format(
        self: Func,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .i, .k, .s, .v, .d, .c, .e => _ = try writer.write(@tagName(self)),
            .apply => |fg| try writer.print("`({d},{d})", .{ fg[0], fg[1] }),
            ._k1 => |f| try writer.print("k1({d})", .{f}),
            ._d1 => |f| try writer.print("d1({d})", .{f}),
            ._s1 => |f| try writer.print("s1({d})", .{f}),
            ._s2 => |fg| try writer.print("s2({d},{d})", .{ fg[0], fg[1] }),
            ._cont => |ptr| try writer.print("cont({})", .{ptr.apply}),
            .print => |char| {
                if (char == '\n') {
                    _ = try writer.write("r");
                } else {
                    try writer.print(".{s}", .{[1]u8{char}});
                }
            },
            .read => _ = try writer.write("@"),
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
pub const e: Func = .e;

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
        inline 'd', 'i', 's', 'k', 'v', 'e' => |name| {
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

    // if fuzzer generated invalid code, then it's a failure of the fuzz generator.
    var runtime = try Runtime.initFromCode(std.testing.allocator, code.items);
    defer runtime.deinit();

    _ = try runtime.interpret();
}

fn fuzzingCodeGenerator(code: *std.ArrayList(u8), input_bytes: []const u8) !void {
    const valid_chars = "iksvrdc";
    return switch (input_bytes.len) {
        0 => {},
        1 => try code.append(valid_chars[input_bytes.len % valid_chars.len]),
        2 => {
            try code.append('`');
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
    // Check the generated code is valid, for given input bytes
    var code = std.ArrayList(u8).init(std.testing.allocator);
    try fuzzingCodeGenerator(&code, "hello world");
    defer code.deinit();

    log.warn("code: {s}", .{code.items});
    var runtime = try Runtime.initFromCode(std.testing.allocator, code.items);
    defer runtime.deinit();

    // _ = try runtime.interpret();
}

pub const Runtime = struct {
    memory: std.ArrayList(Func),
    // TODO: make output/stdout
    output: std.BoundedArray(u8, 4096) = .{},
    stdout: std.fs.File,

    max_tick: u64 = 1024,
    return_value: ?Func = null,
    current_char: ?u8 = null,

    input: std.io.AnyReader,

    _call_graph: std.ArrayListUnmanaged(Progress) = .{},
    _should_resume: Func.Id = std.math.maxInt(Func.Id),
    // TODO: move res to main memory.
    const Progress = struct { caller: Func.Id, pos: enum { left, right, root_node }, res: ?Func = null };
    const Cont = struct { apply_id: Func.Id, left_id: Func.Id, right: Func.Id };

    pub fn initFromCode(allocator: std.mem.Allocator, code: []const u8) !Runtime {
        var res = Runtime.init(allocator, &.{}) catch unreachable;
        try parse(&res.memory, code);
        try res._resetCallGraph();
        return res;
    }

    pub fn init(allocator: std.mem.Allocator, code: []const Func) !Runtime {
        var empty_reader = std.io.fixedBufferStream("");
        var res = Runtime{
            .memory = std.ArrayList(Func).init(allocator),
            .stdout = if (builtin.is_test) undefined else std.io.getStdOut(),
            .input = if (builtin.is_test)
                empty_reader.reader().any()
            else
                std.io.getStdIn().reader().any(),
        };
        try res.memory.appendSlice(code);
        try res._resetCallGraph();
        return res;
    }

    pub fn initWithInput(allocator: std.mem.Allocator, code: []const Func, input: []const u8) !Runtime {
        var res = Runtime{
            .memory = std.ArrayList(Func).init(allocator),
            .stdout = if (builtin.is_test) undefined else std.io.getStdOut(),
            .stdin = std.io.fixedBufferStream(input).reader(),
        };
        try res.memory.appendSlice(code);
        try res._resetCallGraph();
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

    fn _resetCallGraph(self: *Runtime) !void {
        try self._call_graph.resize(self.memory.allocator, self.memory.items.len);
        for (self.memory.items, 0..) |func, apply_id| {
            if (func != .apply) continue;
            const f_id, const g_id = func.apply;
            self._call_graph.items[f_id] = .{ .caller = @intCast(apply_id), .pos = .left };
            self._call_graph.items[g_id] = .{ .caller = @intCast(apply_id), .pos = .right };
        }
    }

    pub fn _interpret(self: *Runtime, start_id: Func.Id) !Func {
        // log.debug("interpreting({any})", .{self.memory.items});

        var continuation: ?Func.Id = null;
        var id: Func.Id = start_id;
        self._call_graph.items[start_id] = .{ .caller = undefined, .pos = .root_node };
        while (true) {
            continuation = self.apply(id) catch |err| return switch (err) {
                error.Exit => self.return_value.?,
                error.OutOfMemory => error.OutOfMemory,
                error.IoError => error.IoError,
            };
            if (continuation) |cont| {
                id = cont;
            } else break;

            self.max_tick -|= 1;
            if (self.max_tick == 0) {
                return error.TimedOut;
            }
        }
        return self.isReady(start_id).?;
    }

    /// Read memory at the given index.
    pub fn get(self: *const Runtime, n: Func.Id) Func {
        return self.memory.items[n];
    }

    /// Fetch the value at the given index, but returns null when it finds an apply.
    pub fn isReady(self: Runtime, id: Func.Id) ?Func {
        return switch (self.get(id)) {
            .apply => self._call_graph.items[id].res,
            else => |x| x,
        };
    }

    pub fn apply(self: *Runtime, apply_id: Func.Id) error{ Exit, IoError, OutOfMemory }!?Func.Id {
        const f_id, const g_id = self.get(apply_id).apply;
        // log.debug("apply({}, {}, {})", .{ apply_id, f_id, g_id });
        // log.debug("{any}", .{self.memory.items});
        // log.debug("{}", .{self});
        const progress = self._call_graph.items[apply_id];
        const caller = switch (progress.pos) {
            .left, .right => progress.caller,
            .root_node => null,
        };
        // Remind the caller about us. This is only needed when "switching tree", ie when calling continuation.
        switch (progress.pos) {
            .left => self.memory.items[caller.?].apply[0] = apply_id,
            .right => self.memory.items[caller.?].apply[1] = apply_id,
            .root_node => {},
        }

        if (self.isReady(f_id) == null) {
            return f_id;
        }
        const f = self.isReady(f_id).?;
        if (f == .d) {
            const res: Func = if (self.isReady(g_id)) |g| g else .{ ._d1 = g_id };
            self.saveRes(apply_id, res);
            return caller;
        }
        if (self.isReady(g_id) == null) {
            return g_id;
        }
        const g = self.isReady(g_id).?;
        if (f == .c) {
            // Instead of applying c to g, we apply g to the current continuation.
            try self.updateApply(apply_id, g_id, Func{ ._cont = .{ .apply = apply_id } });
            return apply_id;
        }

        // log.debug("call({}: {}, {}: {})", .{ f_id, f, g_id, g });
        const res: Func = switch (f) {
            .c, .d => unreachable, // explicitly handled above.
            .apply => unreachable, // apply is detected with `isReady` above.
            .i => g,
            .e => {
                self.return_value = g;
                return error.Exit;
            },
            .print => |char| print: {
                if (builtin.is_test) {
                    if (self.output.len >= self.output.capacity()) {
                        // If test output is too long, we drop trailing bytes.
                        // This prevent the fuzzer to be limited by IO.
                        self.output.resize(0) catch unreachable;
                        log.warn("Wrote too many char for the internal buffer ! Resetting to empty. Previously written:\n{s}", .{self.output.constSlice()});
                    }
                    self.output.appendAssumeCapacity(char);
                    // log.debug("outputing char: {}. stdout: {s}", .{ char, self.output.constSlice() });
                } else {
                    // TODO also use self.output as buffer instead of calling this every byte
                    self.stdout.writer().writeByte(char) catch return error.IoError;
                }
                break :print g;
            },
            .k => .{ ._k1 = g_id },
            ._k1 => |cst| self.isReady(cst).?,
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
                // const old = self.memory.items[apply_id];
                try self.updateApply(apply_id, Func{ .apply = .{ xy[0], g_id } }, Func{ .apply = .{ xy[1], g_id } });
                // log.debug("```s rewrote {}: {} to ` `({},{}) `({},{})", .{ apply_id, old, self.get(xy[0]), g, self.get(xy[1]), g });
                return apply_id;
            },
            .v => v,
            // Force the evaluation of the delayed.
            ._d1 => |delayed| if (self.isReady(delayed)) |res|
                res
            else {
                // try self.setCaller(.{ .caller = apply_id, .callee = delayed });
                return delayed;
            },
            ._cont => |cont| {
                // When calling the continuation, the `cx applies immediatly return g.
                // So the new continuation is the original caller of `cx.
                // We partially rewrite it's argument to replace the `cx by g.
                // log.debug("calling cont {}({})", .{ cont, g });
                const og_progress = self._call_graph.items[cont.apply];
                self.saveRes(cont.apply, g);
                switch (og_progress.pos) {
                    .left => {
                        self.memory.items[og_progress.caller].apply[0] = g_id;
                        return og_progress.caller;
                    },
                    .right => {
                        self.memory.items[og_progress.caller].apply[1] = g_id;
                        return og_progress.caller;
                    },
                    .root_node => return null,
                }
            },
            .read => unreachable,
        };
        // log.debug(" call -> {}", .{res});
        self.saveRes(apply_id, res);
        return caller;
    }

    pub const CallError = error{ OutOfMemory, Interrupted };

    pub fn push(self: *Runtime, caller: Func.Id, pos: std.meta.FieldType(Progress, .pos), f: Func) !Func.Id {
        try self.memory.append(f);
        try self._call_graph.append(self.memory.allocator, .{ .caller = caller, .pos = pos });
        return @intCast(self.memory.items.len - 1);
    }

    /// Rewrite an existing apply, to new arguments.
    /// This is used to evaluate `cx and ```sxyz.
    /// Note that this is a lossless operation because
    /// original apply information is already saved in the call graph.
    pub fn updateApply(self: *Runtime, apply_id: Func.Id, f: anytype, g: anytype) !void {
        const f_id: Func.Id = if (@TypeOf(f) == Func)
            try self.push(apply_id, .left, f)
        else
            f;

        const g_id: Func.Id = if (@TypeOf(g) == Func)
            try self.push(apply_id, .right, g)
        else
            g;

        self.memory.items[apply_id].apply = .{ f_id, g_id };
    }

    pub fn saveRes(self: *Runtime, callee: Func.Id, res: Func) void {
        // TODO: save result in the caller
        std.debug.assert(res != .apply);
        self._call_graph.items[callee].res = res;
    }

    pub fn format(
        self: Runtime,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try self._print(writer, 0);
    }

    fn _print(self: Runtime, writer: anytype, start_id: Func.Id) !void {
        const bytecode = self.memory.items;
        switch (bytecode[start_id]) {
            .apply => |a| {
                const f_id, const g_id = a;
                try writer.writeByte('`');
                if (self.isReady(f_id)) |f| {
                    try writer.print("{}", .{f});
                } else {
                    try self._print(writer, f_id);
                }

                if (self.isReady(g_id)) |g| {
                    try writer.print("{}", .{g});
                } else {
                    try self._print(writer, g_id);
                }
            },
            inline .s, .k, .i, .d, .c, .v, .e => |_, tag| _ = try writer.write(@tagName(tag)),
            .print => |char| {
                if (char == '\n') {
                    try writer.writeByte('r');
                } else {
                    try writer.writeByte('.');
                    try writer.writeByte(char);
                }
            },
            .read => try writer.writeByte('@'),
            // those can only appear as a result of an apply,
            // so they will be printed with Func.format, in the `isReady` branch.
            ._s1, ._s2, ._k1, ._d1, ._cont => unreachable,
        }
    }

    test format {
        const code = "```skii";
        var bytecode = std.ArrayList(Func).init(std.testing.allocator);
        defer bytecode.deinit();

        var runtime = try Runtime.initFromCode(std.testing.allocator, code);
        defer runtime.deinit();

        var out = std.ArrayList(u8).init(std.testing.allocator);
        try out.writer().print("{}", .{runtime});
        defer out.deinit();

        try std.testing.expectEqualStrings(code, out.items);
    }
};

fn testOutputEql(expected: []const u8, code: []const Func) !void {
    var runtime = try Runtime.init(std.testing.allocator, code);
    defer runtime.deinit();

    _ = try runtime.interpret();
    try std.testing.expectEqualStrings(expected, runtime.output.constSlice());
}

fn expectCodeOutput(code: []const u8, expected: []const u8) !void {
    var runtime = try Runtime.initFromCode(std.testing.allocator, code);
    defer runtime.deinit();

    _ = try runtime.interpret();
    try std.testing.expectEqualStrings(expected, runtime.output.constSlice());
}

fn expectCodeTimesOut(code: []const u8) !void {
    var runtime = try Runtime.initFromCode(std.testing.allocator, code);
    defer runtime.deinit();

    try std.testing.expectEqual(error.TimedOut, runtime.interpret());
}

test "print" {
    // `.*v -> print *
    try testOutputEql("*", &.{ .{ .apply = .{ 1, 2 } }, p('*'), v });
    try expectCodeOutput("`.*v", "*");
}

test k {
    try expectCodeOutput("``k`vv.*", "");
}

test d {
    // `d`ri -> create a delayed
    try expectCodeOutput("`d`ri", "");
    try testOutputEql("", &.{ .{ .apply = .{ 1, 2 } }, d, .{ .apply = .{ 3, 4 } }, r, i });

    // ``d`.*ii -> create a delayed, then force the evaluation -> print new line
    try expectCodeOutput("``d`rii", "\n");
    try testOutputEql("\n", &.{ .{ .apply = .{ 1, 6 } }, .{ .apply = .{ 2, 3 } }, d, .{ .apply = .{ 4, 5 } }, r, i, i });

    // ```s`kdri -> ` ``kdi `ri -> `d`ri -> the printing is delayed
    try expectCodeOutput("```s`kdri", "");
}

fn testFn(code: []const u8, in_outs: []const [2]Func) !void {
    var runtime = try Runtime.initFromCode(std.testing.allocator, code);
    defer runtime.deinit();

    const n: u32 = @intCast(runtime.memory.items.len);
    const apply_idx = try runtime.push(undefined, .root_node, .{ .apply = undefined });
    const input_idx = try runtime.push(apply_idx, .right, undefined);

    try runtime.updateApply(apply_idx, 0, input_idx);
    std.debug.assert(input_idx == n + 1);
    runtime._call_graph.items[0] = .{ .caller = apply_idx, .pos = .left };
    runtime._call_graph.items[input_idx] = .{ .caller = apply_idx, .pos = .right };

    for (in_outs) |in_out| {
        const in, const out = in_out;
        runtime.memory.items[input_idx] = in;
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
    try expectCodeOutput("``cir", "\n");
    try expectCodeOutput("```s `ck ir", "\n\n");
    try expectCodeOutput("`c``s`kr``si`ki", "");
    // ```s `ck ir -> ```s `k<cont> ir -> ` ``k<cont>r `ir -> ` <cont>r -> ```s r ir -> ` `rr `ir -> `rr -> r
    try expectCodeOutput("```s `ck ir", "\n\n");
    try testFn("``s`kc``s`k`sv``ss`k`ki", &.{.{ i, i }});
}

test e {
    var runtime = try Runtime.initFromCode(std.testing.allocator, "``cie");
    defer runtime.deinit();

    const res = try runtime.interpret();
    try std.testing.expectEqual(.e, res);
}

test "infinite loop" {
    // This is actually a infinite loop.
    // The first continuation, c1, is apply to the second one, c2.
    // Then c2 is applied to itself, which creates a loop.
    // try expectCodeTimesOut("` `ci `ci");

    // ` `ci `r`ci
    // -> ` <cont1> `r`ci
    // -> ` <cont1> `r<cont2>
    // -> ` <cont1> <cont2>     | "\n"
    // -> ` <cont2> `r`ci
    // -> ` <cont2> `r<cont2_bis>
    // -> ` <cont2> <cont2_bis> | "\n"
    // -> ` <cont1> `r<cont2_bis>
    // ...

}

pub fn main() !void {
    const fib =
        \\ ```s``s``sii`ki
        \\   `k.*``s``s`ks
        \\   ``s`k`s`ks``s``s`ks``s`k`s`kr``s`k`sikk
        \\   `k``s`ksk
    ;

    var runtime = try Runtime.initFromCode(std.heap.page_allocator, fib);
    runtime.max_tick = 1024 * 1024 * 1024;
    _ = try runtime.interpret();
}
