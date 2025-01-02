const std = @import("std");

const OpType = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
};

fn Node(comptime N: usize) type {
    if (N == 1) {
        return struct {
            index: usize = 0,
        };
    }
    return union(enum) {
        wrapped: Node(N - 1),
        calculated: struct {
            op: OpType,
            left: Node(N - 1),
            right: Node(N - 1),
        },
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
    const resultSize = resultCount(N);

    return struct {
        formula: [resultSize]Node(N) = undefined,

        fn reduce(comptime K: usize, op1: Node(K), op2: Node(K)) [6]Node(K + 1) {
            return .{
                .{ .calculated = .{ .op = .Add, .left = op1, .right = op2 } },
                .{ .calculated = .{ .op = .Multiply, .left = op1, .right = op2 } },
                .{ .calculated = .{ .op = .Subtract, .left = op1, .right = op2 } },
                .{ .calculated = .{ .op = .Subtract, .left = op2, .right = op1 } },
                .{ .calculated = .{ .op = .Divide, .left = op1, .right = op2 } },
                .{ .calculated = .{ .op = .Divide, .left = op2, .right = op1 } },
            };
        }

        fn wrap(comptime K: usize, op: Node(K)) Node(K + 1) {
            return .{ .wrapped = op };
        }

        fn generate(comptime L: u8, nodes: []Node(N - L + 1)) [resultCount(L)]Node(N) {
            var result: [resultCount(L)]Node(N) = undefined;
            var r: usize = 0;
            if (L == 1) {
                result[0] = nodes[0];
                return result;
            }
            var reduced: [L - 1]Node(N - L + 2) = undefined;
            for (0..L) |i| {
                for ((i + 1)..L) |j| {
                    var m: u8 = 0;
                    for (0..L) |k| {
                        if (i != k and j != k) {
                            reduced[m] = wrap(N - L + 1, nodes[k]);
                            m += 1;
                        }
                    }
                    for (reduce(N - L + 1, nodes[i], nodes[j])) |c| {
                        reduced[L - 2] = c;
                        const roots = generate(L - 1, &reduced);
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
            var c = Calc24(N){};
            var nodes: [N]Node(1) = undefined;
            for (0..N) |i| {
                nodes[i] = .{ .index = i };
            }
            c.formula = generate(N, &nodes);
            return c;
        }

        pub fn calc(self: *const @This(), nums: []u8, arena: std.mem.Allocator) ![]const u8 {
            const target: f64 = 24;
            for (self.formula) |f| {
                if (self.eval(nums, N, f) == target) {
                    return self.toString(nums, N, f, arena);
                }
            }
            return "No Solution";
        }

        fn eval(self: *const @This(), nums: []u8, comptime L: u8, node: Node(L)) f64 {
            if (L == 1) {
                return @floatFromInt(nums[node.index]);
            } else {
                return switch (node) {
                    .wrapped => |value| self.eval(nums, L - 1, value),
                    .calculated => |value| switch (value.op) {
                        .Add => self.eval(nums, L - 1, value.left) + self.eval(nums, L - 1, value.right),
                        .Subtract => self.eval(nums, L - 1, value.left) - self.eval(nums, L - 1, value.right),
                        .Multiply => self.eval(nums, L - 1, value.left) * self.eval(nums, L - 1, value.right),
                        .Divide => self.eval(nums, L - 1, value.left) / self.eval(nums, L - 1, value.right),
                    },
                };
            }
        }

        fn toString(self: *const @This(), nums: []u8, comptime L: u8, node: Node(L), arena: std.mem.Allocator) ![]const u8 {
            if (L == 1) {
                return std.fmt.allocPrint(arena, "{d}", .{nums[node.index]});
            } else {
                switch (node) {
                    .wrapped => |value| return self.toString(nums, L - 1, value, arena),
                    .calculated => |value| switch (value.op) {
                        .Add => {
                            const l = try self.toString(nums, L - 1, value.left, arena);
                            const r = try self.toString(nums, L - 1, value.right, arena);
                            return std.fmt.allocPrint(arena, "{s} + {s}", .{ l, r });
                        },
                        .Subtract => {
                            const l = try self.toString(nums, L - 1, value.left, arena);
                            const r = try self.toStringWrapped(nums, L - 1, value.right, false, arena);
                            return std.fmt.allocPrint(arena, "{s} - {s}", .{ l, r });
                        },
                        .Multiply => {
                            const l = try self.toStringWrapped(nums, L - 1, value.left, false, arena);
                            const r = try self.toStringWrapped(nums, L - 1, value.right, false, arena);
                            return std.fmt.allocPrint(arena, "{s} * {s}", .{ l, r });
                        },
                        .Divide => {
                            const l = try self.toStringWrapped(nums, L - 1, value.left, false, arena);
                            const r = try self.toStringWrapped(nums, L - 1, value.right, true, arena);
                            return std.fmt.allocPrint(arena, "{s} / {s}", .{ l, r });
                        },
                    },
                }
            }
        }

        fn toStringWrapped(self: *const @This(), nums: []u8, comptime L: u8, node: Node(L), is_denominator: bool, arena: std.mem.Allocator) error{OutOfMemory}![]const u8 {
            if (L == 1) {
                return try self.toString(nums, L, node, arena);
            } else {
                switch (node) {
                    .wrapped => |value| return try self.toStringWrapped(nums, L - 1, value, is_denominator, arena),
                    .calculated => |value| {
                        const r = try self.toString(nums, L, node, arena);
                        if (value.op == .Add or value.op == .Subtract or is_denominator) {
                            return std.fmt.allocPrint(arena, "({s})", .{r});
                        } else {
                            return r;
                        }
                    },
                }
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
    @setEvalBranchQuota(60000);
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
