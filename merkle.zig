const std = @import("std");

const Hash = [32]u8;

fn leafHash(data: []u8) Hash {
    var h: Hash = undefined;
    std.crypto.Sha256.hash(data[0..], h[0..]);
    return h;
}

fn nodeHash(l: Hash, r: Hash) Hash {
    var buf: [64]u8 = undefined;
    std.mem.copy(u8, buf[0..32], l[0..]);
    std.mem.copy(u8, buf[32..], r[0..]);
    return leafHash(buf[0..]);
}

const Forest = struct {
    const Self = @This();

    trees: [64]Hash,
    numLeaves: u64,

    fn init() Self {
        var ctx: Forest = undefined;
        ctx.reset();
        return ctx;
    }

    fn reset(ctx: *Self) void {
        ctx.numLeaves = 0;
    }

    fn hasTreeAtHeight(ctx: *Self, height: u6) bool {
        return ctx.numLeaves & (u64(1) << height) != 0;
    }

    fn addLeaf(ctx: *Self, leaf: []u8) void {
        var tree: Hash = leafHash(leaf);
        var i: u6 = 0;
        while (ctx.hasTreeAtHeight(i)) : (i += 1) {
            tree = nodeHash(ctx.trees[i], tree);
        }
        ctx.trees[i] = tree;
        ctx.numLeaves += 1;
    }

    fn root(ctx: *Self) Hash {
        var i: u7 = @ctz(u64, ctx.numLeaves);
        var r: Hash = ctx.trees[i];
        i += 1;
        while (i < 64) : (i += 1) {
            if (ctx.hasTreeAtHeight(@intCast(u6, i))) {
                r = nodeHash(ctx.trees[i], r);
            }
        }
        return r;
    }
};

const segSize = 64;

pub fn main() !void {
    var file = try std.fs.File.openRead("test.dat");
    defer file.close();

    var f: Forest = Forest.init();
    var data: []u8 = try std.heap.direct_allocator.alloc(u8, segSize);
    while (true) {
        var n: u64 = try file.read(data[0..]);
        f.addLeaf(data[0..n]);
        if (n < segSize) {
            break;
        }
    }

    var h: Hash = f.root();
    std.debug.warn("{x}\n", h);
}
