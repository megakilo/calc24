const std = @import("std");
const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

const calc24 = @import("calc24.zig");

test "Basic3Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = try calc24.Calc24.init(3, aa);
    var input = [_]u8{ 2, 3, 4 };
    try expect(!std.mem.eql(u8, "No Solution", try calc.calc(&input)));
}

test "NoResult3Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = try calc24.Calc24.init(3, aa);
    var input = [_]u8{ 1, 3, 4 };
    try expectEqualStrings("No Solution", try calc.calc(&input));
}

test "Basic4Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = try calc24.Calc24.init(4, aa);
    var input = [_]u8{ 1, 2, 3, 4 };
    try expect(!std.mem.eql(u8, "No Solution", try calc.calc(&input)));
}

test "NoResult4Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = try calc24.Calc24.init(4, aa);
    var input = [_]u8{ 1, 1, 1, 4 };
    try expectEqualStrings("No Solution", try calc.calc(&input));
}

test "FractionalCalc" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = try calc24.Calc24.init(4, aa);
    var input = [_]u8{ 3, 3, 7, 7 };
    try expect(!std.mem.eql(u8, "No Solution", try calc.calc(&input)));
}
