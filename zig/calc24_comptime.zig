const std = @import("std");

const OpType = enum {
    None,
    Add,
    Subtract,
    Multiply,
    Divide,
};

const Node = struct {
    index: usize = 0,
    op: OpType = .None,
    left: usize = 0,
    right: usize = 0,
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

fn resultCount(comptime N: u8) u32 {
    if (N == 1) {
        return 1;
    } else {
        return (N - 1) * N / 2 * 6 * resultCount(N - 1);
    }
}

fn nodeCount(comptime N: u8) u32 {
    if (N == 1) {
        return 0;
    } else {
        return (N - 1) * N / 2 * 6 * (1 + nodeCount(N - 1));
    }
}

pub fn Calc24(comptime N: u8) type {
    const Index = usize;
    const Pool = List(Node, nodeCount(N) + N);
    const resultSize = resultCount(N);

    return struct {
        formula: [resultSize]Index = undefined,
        pool: Pool = undefined,

        fn reduce(op1: Index, op2: Index, pool: *Pool) [6]Index {
            const add = pool.add(.{ .op = .Add, .left = op1, .right = op2 });
            const mult = pool.add(.{ .op = .Multiply, .left = op1, .right = op2 });
            const sub1 = pool.add(.{ .op = .Subtract, .left = op1, .right = op2 });
            const sub2 = pool.add(.{ .op = .Subtract, .left = op2, .right = op1 });
            const div1 = pool.add(.{ .op = .Divide, .left = op1, .right = op2 });
            const div2 = pool.add(.{ .op = .Divide, .left = op2, .right = op1 });
            return [_]Index{ add, mult, sub1, sub2, div1, div2 };
        }

        fn generate(comptime L: u8, indexes: []Index, pool: *Pool) [resultCount(L)]Index {
            var result: [resultCount(L)]Index = undefined;
            var r: usize = 0;
            if (L == 1) {
                result[0] = indexes[0];
                return result;
            }
            var reduced: [L - 1]Index = undefined;
            for (0..L) |i| {
                for ((i + 1)..L) |j| {
                    var m: u8 = 0;
                    for (0..L) |k| {
                        if (i != k and j != k) {
                            reduced[m] = indexes[k];
                            m += 1;
                        }
                    }
                    for (reduce(indexes[i], indexes[j], pool)) |c| {
                        reduced[L - 2] = c;
                        const roots = generate(L - 1, &reduced, pool);
                        for (roots) |root| {
                            result[r] = root;
                            r += 1;
                        }
                    }
                }
            }
            return result;
        }

        pub fn init() Calc24(N) {
            var c = Calc24(N){ .pool = .{} };
            var indexes: [N]Index = undefined;
            for (0..N) |i| {
                indexes[i] = c.pool.add(.{ .index = i, .op = .None });
            }
            c.formula = generate(N, &indexes, &c.pool);
            return c;
        }

        pub fn calc(self: *const @This(), nums: []u8, arena: std.mem.Allocator) ![]const u8 {
            const target: f64 = 24;
            for (self.formula) |f| {
                if (self.eval(nums, f) == target) {
                    return self.toString(nums, f, arena);
                }
            }
            return "No Solution";
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

        fn toString(self: *const @This(), nums: []u8, index: Index, arena: std.mem.Allocator) ![]const u8 {
            const node = self.pool.get(index);
            switch (node.op) {
                .None => return std.fmt.allocPrint(arena, "{d}", .{nums[node.index]}),
                .Add => {
                    const l = try self.toString(nums, node.left, arena);
                    const r = try self.toString(nums, node.right, arena);
                    return std.fmt.allocPrint(arena, "{s} + {s}", .{ l, r });
                },
                .Subtract => {
                    const l = try self.toString(nums, node.left, arena);
                    const r = try self.toStringWrapped(nums, node.right, false, arena);
                    return std.fmt.allocPrint(arena, "{s} - {s}", .{ l, r });
                },
                .Multiply => {
                    const l = try self.toStringWrapped(nums, node.left, false, arena);
                    const r = try self.toStringWrapped(nums, node.right, false, arena);
                    return std.fmt.allocPrint(arena, "{s} * {s}", .{ l, r });
                },
                .Divide => {
                    const l = try self.toStringWrapped(nums, node.left, false, arena);
                    const r = try self.toStringWrapped(nums, node.right, true, arena);
                    return std.fmt.allocPrint(arena, "{s} / {s}", .{ l, r });
                },
            }
        }

        fn toStringWrapped(self: *const @This(), nums: []u8, index: Index, is_denominator: bool, arena: std.mem.Allocator) error{OutOfMemory}![]const u8 {
            const r = try self.toString(nums, index, arena);
            const node = self.pool.get(index);
            if (node.op == .Add or node.op == .Subtract or (node.op != .None and is_denominator)) {
                return std.fmt.allocPrint(arena, "({s})", .{r});
            } else {
                return r;
            }
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
    @setEvalBranchQuota(40000);
    const calc = comptime Calc24(N).init();
    for (0..100000) |_| {
        for (0..N) |i| {
            nums[i] = rand.intRangeAtMost(u8, 1, 13);
            if (i > 0) {
                try challenge.appendSlice(", ");
            }
            try challenge.writer().print("{d}", .{nums[i]});
        }
        const result = try calc.calc(&nums, aa);
        try stdout.print("{s} -> {s}\n", .{ challenge.items, result });
        challenge.clearRetainingCapacity();
    }
}
