const std = @import("std");
var gpa = std.heap.DebugAllocator.init;

const allocator = gpa.allocator();

fn init_array(size: u64, init: i64) []i64 {
    const arr = allocator.alloc(i64, size);
    @memset(arr, init);
    return arr;
}

fn alloc_record(size: u64) []i64 {
    const record = allocator.malloc(i64, size);
    @memset(record, 0);
    return record;
}

fn string_equal(a: []const u8, b: []const u8) i64 {
    return std.mem.eql(u8, a, b);
}

fn print(s: []const u8) void {
    std.fs.File.stdout.write(s) catch @panic("print failed");
}
