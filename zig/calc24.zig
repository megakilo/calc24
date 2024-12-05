const std = @import("std");

const OpType = enum {
    None,
    Add,
    Subtract,
    Multiply,
    Divide,
};

fn List(comptime T: anytype, comptime N: usize) type {
    return struct {
        entries: [N]T = undefined,
        count: usize = 0,

        fn add(self: *@This(), item: T) usize {
            const current = self.count;
            self.entries[current] = item;
            self.count += 1;
            return current;
        }

        fn get(self: *const @This(), i: usize) *const T {
            return &self.entries[i];
        }
    };
}

const Node = struct {
    index: usize = 0,
    op: OpType = .None,
    left: usize = 0,
    right: usize = 0,
};

const Index = usize;
const Pool = List(Node, 5000);

fn reduce(op1: Index, op2: Index, pool: *Pool) [6]Index {
    const add = pool.add(.{ .op = .Add, .left = op1, .right = op2 });
    const mult = pool.add(.{ .op = .Multiply, .left = op1, .right = op2 });
    const sub1 = pool.add(.{ .op = .Subtract, .left = op1, .right = op2 });
    const sub2 = pool.add(.{ .op = .Subtract, .left = op2, .right = op1 });
    const div1 = pool.add(.{ .op = .Divide, .left = op1, .right = op2 });
    const div2 = pool.add(.{ .op = .Divide, .left = op2, .right = op1 });
    return [_]Index{ add, mult, sub1, sub2, div1, div2 };
}

fn resultCount(comptime N: u8) u32 {
    if (N == 1) {
        return 1;
    } else {
        return (N - 1) * N / 2 * 6 * resultCount(N - 1);
    }
}

fn generate(comptime N: u8, indexes: []Index, pool: *Pool) [resultCount(N)]Index {
    var result: [resultCount(N)]Index = undefined;
    var r: usize = 0;
    if (N == 1) {
        result[0] = indexes[0];
        return result;
    }
    var reduced: [N - 1]Index = undefined;
    for (0..N) |i| {
        for ((i + 1)..N) |j| {
            var m: u8 = 0;
            for (0..N) |k| {
                if (i != k and j != k) {
                    reduced[m] = indexes[k];
                    m += 1;
                }
            }
            for (reduce(indexes[i], indexes[j], pool)) |c| {
                reduced[N - 2] = c;
                const roots = generate(N - 1, &reduced, pool);
                for (roots) |root| {
                    result[r] = root;
                    r += 1;
                }
            }
        }
    }
    return result;
}

fn Calc24(comptime N: u8) type {
    return struct {
        formula: [resultCount(N)]Index = undefined,
        pool: Pool = undefined,

        pub fn init() Calc24(N) {
            var c = Calc24(N){};
            var indexes: [N]Index = undefined;
            for (0..N) |i| {
                indexes[i] = c.pool.add(.{ .index = i, .op = .None });
            }
            c.formula = generate(N, &indexes, &c.pool);
            return c;
        }

        pub fn calc(self: *const @This(), nums: []u8, aa: std.mem.Allocator) !std.ArrayList(u8) {
            const target: f64 = 24;
            var result = std.ArrayList(u8).init(aa);
            for (self.formula) |f| {
                if (self.eval(nums, f) == target) {
                    return self.toString(nums, f, aa);
                }
            }
            try result.appendSlice("No Solution");
            return result;
        }

        fn eval(self: *const @This(), nums: []u8, index: Index) f64 {
            const node = self.pool.get(index);
            return switch (node.op) {
                .None => @floatFromInt(nums[node.index]),
                .Add => self.eval(nums, node.left) + self.eval(nums, node.right),
                .Subtract => self.eval(nums, node.left) - self.eval(nums, node.right),
                .Multiply => self.eval(nums, node.left) * self.eval(nums, node.right),
                .Divide => self.eval(nums, node.left) / self.eval(nums, node.right),
            };
        }

        fn toString(self: *const @This(), nums: []u8, index: Index, alloc: std.mem.Allocator) !std.ArrayList(u8) {
            var result = std.ArrayList(u8).init(alloc);
            const node = self.pool.get(index);
            switch (node.op) {
                .None => try result.writer().print("{d}", .{nums[node.index]}),
                .Add => {
                    const l = try self.toString(nums, node.left, alloc);
                    const r = try self.toString(nums, node.right, alloc);
                    try result.writer().print("{s} + {s}", .{ l.items, r.items });
                },
                .Subtract => {
                    const l = try self.toString(nums, node.left, alloc);
                    const r = try self.toStringWrapped(nums, node.right, alloc, false);
                    try result.writer().print("{s} - {s}", .{ l.items, r.items });
                },
                .Multiply => {
                    const l = try self.toStringWrapped(nums, node.left, alloc, false);
                    const r = try self.toStringWrapped(nums, node.right, alloc, false);
                    try result.writer().print("{s} * {s}", .{ l.items, r.items });
                },
                .Divide => {
                    const l = try self.toStringWrapped(nums, node.left, alloc, false);
                    const r = try self.toStringWrapped(nums, node.right, alloc, true);
                    try result.writer().print("{s} / {s}", .{ l.items, r.items });
                },
            }
            return result;
        }

        fn toStringWrapped(self: *const @This(), nums: []u8, index: Index, alloc: std.mem.Allocator, is_denominator: bool) error{ WriteError, OutOfMemory }!std.ArrayList(u8) {
            var result = std.ArrayList(u8).init(alloc);
            const r = try self.toString(nums, index, alloc);
            const node = self.pool.get(index);
            if (node.op == .Add or node.op == .Subtract or (node.op != .None and is_denominator)) {
                try result.writer().print("({s})", .{r.items});
            } else {
                try result.appendSlice(r.items);
            }
            return result;
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const aa = arena.allocator();

    const stdout = std.io.getStdOut().writer();

    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    var prng = std.rand.DefaultPrng.init(seed);
    const rand = prng.random();

    const N: u8 = 4;
    var nums: [N]u8 = undefined;
    var challenge = std.ArrayList(u8).init(aa);
    const calc = Calc24(N).init();
    for (0..100000) |_| {
        for (0..N) |i| {
            nums[i] = rand.intRangeAtMost(u8, 1, 13);
            if (i > 0) {
                try challenge.appendSlice(", ");
            }
            try challenge.writer().print("{d}", .{nums[i]});
        }
        const result = try calc.calc(&nums, aa);
        try stdout.print("{s} -> {s}\n", .{ challenge.items, result.items });
        challenge.clearRetainingCapacity();
    }
}