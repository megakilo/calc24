const std = @import("std");
const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

const calc24 = @import("calc24_comptime.zig");

test "Basic3Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = calc24.Calc24(3).init();
    var input = [_]u8{ 2, 3, 4 };
    try expect(!std.mem.eql(u8, "No Solution", try calc.calc(&input, aa)));
}

test "NoResult3Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = calc24.Calc24(3).init();
    var input = [_]u8{ 1, 3, 4 };
    try expectEqualStrings("No Solution", try calc.calc(&input, aa));
}

test "Basic4Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = calc24.Calc24(4).init();
    var input = [_]u8{ 1, 2, 3, 4 };
    try expect(!std.mem.eql(u8, "No Solution", try calc.calc(&input, aa)));
}

test "NoResult4Numbers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = calc24.Calc24(4).init();
    var input = [_]u8{ 1, 1, 1, 4 };
    try expectEqualStrings("No Solution", try calc.calc(&input, aa));
}

test "FractionalCalc" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const aa = arena.allocator();
    const calc = calc24.Calc24(4).init();
    var input = [_]u8{ 3, 3, 7, 7 };
    try expect(!std.mem.eql(u8, "No Solution", try calc.calc(&input, aa)));
}
