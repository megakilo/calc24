const std = @import("std");

const OpType = enum {
    None,
    Add,
    Subtract,
    Multiply,
    Divide,
};

const Operand = struct {
    index: usize = 0,
    op: OpType = .None,
    left: ?*Operand = null,
    right: ?*Operand = null,

    pub fn eval(self: @This(), nums: []u8) f64 {
        return switch (self.op) {
            .None => @floatFromInt(nums[self.index]),
            .Add => self.left.?.eval(nums) + self.right.?.eval(nums),
            .Subtract => self.left.?.eval(nums) - self.right.?.eval(nums),
            .Multiply => self.left.?.eval(nums) * self.right.?.eval(nums),
            .Divide => self.left.?.eval(nums) / self.right.?.eval(nums),
        };
    }

    pub fn toString(
        self: @This(),
        nums: []u8,
        alloc: std.mem.Allocator,
    ) !std.ArrayList(u8) {
        var result = std.ArrayList(u8).init(alloc);
        switch (self.op) {
            .None => try result.writer().print("{d}", .{nums[self.index]}),
            .Add => {
                const l = try self.left.?.toString(nums, alloc);
                const r = try self.right.?.toString(nums, alloc);
                try result.writer().print("{s} + {s}", .{ l.items, r.items });
            },
            .Subtract => {
                const l = try self.left.?.toString(nums, alloc);
                const r = try self.right.?.toStringWrapped(nums, false, alloc);
                try result.writer().print("{s} - {s}", .{ l.items, r.items });
            },
            .Multiply => {
                const l = try self.left.?.toStringWrapped(nums, false, alloc);
                const r = try self.right.?.toStringWrapped(nums, false, alloc);
                try result.writer().print("{s} * {s}", .{ l.items, r.items });
            },
            .Divide => {
                const l = try self.left.?.toStringWrapped(nums, false, alloc);
                const r = try self.right.?.toStringWrapped(nums, true, alloc);
                try result.writer().print("{s} / {s}", .{ l.items, r.items });
            },
        }
        return result;
    }

    pub fn toStringWrapped(
        self: @This(),
        nums: []u8,
        is_denominator: bool,
        alloc: std.mem.Allocator,
    ) error{ WriteError, OutOfMemory }!std.ArrayList(u8) {
        var result = std.ArrayList(u8).init(alloc);
        const r = try self.toString(nums, alloc);
        if (self.op == .Add or self.op == .Subtract or (self.op != .None and is_denominator)) {
            try result.writer().print("({s})", .{r.items});
        } else {
            try result.appendSlice(r.items);
        }
        return result;
    }
};

fn combine(op1: *Operand, op2: *Operand, alloc: std.mem.Allocator) ![6]*Operand {
    const add = try alloc.create(Operand);
    add.* = .{ .op = .Add, .left = op1, .right = op2 };
    const mult = try alloc.create(Operand);
    mult.* = .{ .op = .Multiply, .left = op1, .right = op2 };
    const sub1 = try alloc.create(Operand);
    sub1.* = .{ .op = .Subtract, .left = op1, .right = op2 };
    const sub2 = try alloc.create(Operand);
    sub2.* = .{ .op = .Subtract, .left = op2, .right = op1 };
    const div1 = try alloc.create(Operand);
    div1.* = .{ .op = .Divide, .left = op1, .right = op2 };
    const div2 = try alloc.create(Operand);
    div2.* = .{ .op = .Divide, .left = op2, .right = op1 };

    return [_]*Operand{ add, mult, sub1, sub2, div1, div2 };
}

fn resultCount(comptime N: u8) u32 {
    if (N == 1) {
        return 1;
    } else {
        return (N - 1) * N / 2 * 6 * resultCount(N - 1);
    }
}

fn generateFormula(comptime N: u8, indexes: []*Operand, alloc: std.mem.Allocator) ![resultCount(N)]*Operand {
    var result: [resultCount(N)]*Operand = undefined;
    var r: usize = 0;
    if (N == 1) {
        result[0] = indexes[0];
        return result;
    }
    var reduced: [N - 1]*Operand = undefined;
    for (0..N) |i| {
        for ((i + 1)..N) |j| {
            var m: u8 = 0;
            for (0..N) |k| {
                if (i != k and j != k) {
                    reduced[m] = indexes[k];
                    m += 1;
                }
            }
            for (try combine(indexes[i], indexes[j], alloc)) |c| {
                reduced[N - 2] = c;
                const roots = try generateFormula(N - 1, &reduced, alloc);
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
        formula: [resultCount(N)]*Operand,
        allocator: std.mem.Allocator,

        pub fn init(alloc: std.mem.Allocator) !Calc24(N) {
            var indexes: [N]*Operand = undefined;
            for (0..N) |i| {
                const x = try alloc.create(Operand);
                x.* = .{ .index = i, .op = .None };
                indexes[i] = x;
            }
            return .{ .allocator = alloc, .formula = try generateFormula(N, &indexes, alloc) };
        }

        pub fn calc(self: @This(), nums: []u8) !std.ArrayList(u8) {
            const target: f64 = 24;
            var result = std.ArrayList(u8).init(self.allocator);
            for (self.formula) |f| {
                if (f.eval(nums) == target) {
                    return f.toString(nums, self.allocator);
                }
            }
            try result.appendSlice("No Solution");
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
    const calc = try Calc24(N).init(aa);
    for (0..100000) |_| {
        for (0..N) |i| {
            nums[i] = rand.intRangeAtMost(u8, 1, 13);
            if (i > 0) {
                try challenge.appendSlice(", ");
            }
            try challenge.writer().print("{d}", .{nums[i]});
        }
        const result = try calc.calc(&nums);
        try stdout.print("{s} -> {s}\n", .{ challenge.items, result.items });
        challenge.clearRetainingCapacity();
    }
}
