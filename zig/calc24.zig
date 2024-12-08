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
        arena: std.mem.Allocator,
    ) ![]const u8 {
        switch (self.op) {
            .None => return std.fmt.allocPrint(arena, "{d}", .{nums[self.index]}),
            .Add => {
                const l = try self.left.?.toString(nums, arena);
                const r = try self.right.?.toString(nums, arena);
                return std.fmt.allocPrint(arena, "{s} + {s}", .{ l, r });
            },
            .Subtract => {
                const l = try self.left.?.toString(nums, arena);
                const r = try self.right.?.toStringWrapped(nums, false, arena);
                return std.fmt.allocPrint(arena, "{s} - {s}", .{ l, r });
            },
            .Multiply => {
                const l = try self.left.?.toStringWrapped(nums, false, arena);
                const r = try self.right.?.toStringWrapped(nums, false, arena);
                return std.fmt.allocPrint(arena, "{s} * {s}", .{ l, r });
            },
            .Divide => {
                const l = try self.left.?.toStringWrapped(nums, false, arena);
                const r = try self.right.?.toStringWrapped(nums, true, arena);
                return std.fmt.allocPrint(arena, "{s} / {s}", .{ l, r });
            },
        }
    }

    pub fn toStringWrapped(
        self: @This(),
        nums: []u8,
        is_denominator: bool,
        arena: std.mem.Allocator,
    ) error{ OutOfMemory, AllocPrintError }![]const u8 {
        const r = try self.toString(nums, arena);
        if (self.op == .Add or self.op == .Subtract or (self.op != .None and is_denominator)) {
            return std.fmt.allocPrint(arena, "({s})", .{r});
        } else {
            return r;
        }
    }
};

fn combine(op1: *Operand, op2: *Operand, arena: std.mem.Allocator) ![6]*Operand {
    const add = try arena.create(Operand);
    add.* = .{ .op = .Add, .left = op1, .right = op2 };
    const mult = try arena.create(Operand);
    mult.* = .{ .op = .Multiply, .left = op1, .right = op2 };
    const sub1 = try arena.create(Operand);
    sub1.* = .{ .op = .Subtract, .left = op1, .right = op2 };
    const sub2 = try arena.create(Operand);
    sub2.* = .{ .op = .Subtract, .left = op2, .right = op1 };
    const div1 = try arena.create(Operand);
    div1.* = .{ .op = .Divide, .left = op1, .right = op2 };
    const div2 = try arena.create(Operand);
    div2.* = .{ .op = .Divide, .left = op2, .right = op1 };

    return [_]*Operand{ add, mult, sub1, sub2, div1, div2 };
}

fn generateFormula(indexes: std.ArrayList(*Operand), arena: std.mem.Allocator) !std.ArrayList(*Operand) {
    const n = indexes.items.len;
    var result = std.ArrayList(*Operand).init(arena);
    if (n == 1) {
        try result.appendSlice(indexes.items);
        return result;
    }
    var reduced = std.ArrayList(*Operand).init(arena);
    for (0..n) |i| {
        for ((i + 1)..n) |j| {
            reduced.clearRetainingCapacity();
            for (0..n) |k| {
                if (i != k and j != k) {
                    try reduced.append(indexes.items[k]);
                }
            }
            for (try combine(indexes.items[i], indexes.items[j], arena)) |r| {
                try reduced.append(r);
                const expanded = try generateFormula(reduced, arena);
                try result.appendSlice(expanded.items);
                _ = reduced.pop();
            }
        }
    }
    return result;
}

pub const Calc24 = struct {
    formula: std.ArrayList(*Operand),
    arena: std.mem.Allocator,

    pub fn init(comptime N: u8, arena: std.mem.Allocator) !Calc24 {
        var indexes = std.ArrayList(*Operand).init(arena);
        for (0..N) |i| {
            const x = try arena.create(Operand);
            x.* = .{ .index = i, .op = .None };
            try indexes.append(x);
        }
        return .{ .arena = arena, .formula = try generateFormula(indexes, arena) };
    }

    pub fn calc(self: @This(), nums: []u8) ![]const u8 {
        const target: f64 = 24;
        for (self.formula.items) |f| {
            if (f.eval(nums) == target) {
                return f.toString(nums, self.arena);
            }
        }
        return "No Solution";
    }
};

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

    const N: i32 = 4;
    var nums: [N]u8 = undefined;
    var challenge = std.ArrayList(u8).init(aa);
    const calc = try Calc24.init(N, aa);
    for (0..100000) |_| {
        for (0..N) |i| {
            nums[i] = rand.intRangeAtMost(u8, 1, 13);
            if (i > 0) {
                try challenge.appendSlice(", ");
            }
            try challenge.writer().print("{d}", .{nums[i]});
        }
        const result = try calc.calc(&nums);
        try stdout.print("{s} -> {s}\n", .{ challenge.items, result });
        challenge.clearRetainingCapacity();
    }
}
