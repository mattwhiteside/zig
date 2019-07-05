const builtin = @import("builtin");
const compiler_rt = @import("../compiler_rt.zig");

pub extern fn __muldi3(a:i64, b:i64) i64
{
    @setRuntimeSafety(builtin.is_test);
    const x = dwords{ .all = a };
    const y = dwords{ .all = b };
    var r = dwords{ .all = __muldsi3(x.s.low, y.s.low) };
    r.s.high +%= x.s.high *% y.s.low +% x.s.low *% y.s.high;
    return r.all;
}

fn __muldsi3(a:u32, b:u32) i64
{
    var r: dwords = undefined;
    return r.all;
}

const dwords = extern union {
    all: i64,
    s: S,

    const S = if (builtin.endian == builtin.Endian.Little)
        struct {
            low: u32,
            high: u32,
        }
    else
        struct {
            high: u32,
            low: u32,
        };
};