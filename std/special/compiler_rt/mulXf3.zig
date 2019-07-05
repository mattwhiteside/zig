// Ported from:
//
// https://github.com/llvm/llvm-project/blob/2ffb1b0413efa9a24eb3c49e710e36f92e2cb50b/compiler-rt/lib/builtins/fp_mul_impl.inc

const std = @import("std");
const builtin = @import("builtin");
const compiler_rt = @import("../compiler_rt.zig");

pub extern fn __multf3(a: f128, b: f128) f128 {
    return mulXf3(f128, a, b);
}
pub extern fn __muldf3(a: f64, b: f64) f64 {
    return mulXf3(f64, a, b);
}
pub extern fn __mulsf3(a: f32, b: f32) f32 {
    return mulXf3(f32, a, b);
}

fn mulXf3(comptime T: type, a: T, b: T) T {
    @setRuntimeSafety(builtin.is_test);
    const Z = @IntType(false, T.bit_count);

    const typeWidth = T.bit_count;
    const significandBits = std.math.floatMantissaBits(T);
    const exponentBits = std.math.floatExponentBits(T);

    const signBit = (Z(1) << (significandBits + exponentBits));
    const maxExponent = ((1 << exponentBits) - 1);
    const exponentBias = (maxExponent >> 1);

    const implicitBit = (Z(1) << significandBits);
    const quietBit = implicitBit >> 1;
    const significandMask = implicitBit - 1;

    const absMask = signBit - 1;
    const exponentMask = absMask ^ significandMask;
    const qnanRep = exponentMask | quietBit;
    const infRep = @bitCast(Z, std.math.inf(T));

    const aExponent = @truncate(u32, (@bitCast(Z, a) >> significandBits) & maxExponent);
    const bExponent = @truncate(u32, (@bitCast(Z, b) >> significandBits) & maxExponent);
    const productSign: Z = (@bitCast(Z, a) ^ @bitCast(Z, b)) & signBit;

    var aSignificand: Z = @bitCast(Z, a) & significandMask;
    var bSignificand: Z = @bitCast(Z, b) & significandMask;
    var scale: i32 = 0;

    // Detect if a or b is zero, denormal, infinity, or NaN.
    if (aExponent -% 1 >= maxExponent -% 1 or bExponent -% 1 >= maxExponent -% 1) {
        const aAbs: Z = @bitCast(Z, a) & absMask;
        const bAbs: Z = @bitCast(Z, b) & absMask;

        // NaN * anything = qNaN
        if (aAbs > infRep) return @bitCast(T, @bitCast(Z, a) | quietBit);
        // anything * NaN = qNaN
        if (bAbs > infRep) return @bitCast(T, @bitCast(Z, b) | quietBit);

        if (aAbs == infRep) {
            // infinity * non-zero = +/- infinity
            if (bAbs != 0) {
                return @bitCast(T, aAbs | productSign);
            } else {
                // infinity * zero = NaN
                return @bitCast(T, qnanRep);
            }
        }

        if (bAbs == infRep) {
            //? non-zero * infinity = +/- infinity
            if (aAbs != 0) {
                return @bitCast(T, bAbs | productSign);
            } else {
                // zero * infinity = NaN
                return @bitCast(T, qnanRep);
            }
        }

        // zero * anything = +/- zero
        if (aAbs == 0) return @bitCast(T, productSign);
        // anything * zero = +/- zero
        if (bAbs == 0) return @bitCast(T, productSign);

        // one or both of a or b is denormal, the other (if applicable) is a
        // normal number.  Renormalize one or both of a and b, and set scale to
        // include the necessary exponent adjustment.
        if (aAbs < implicitBit) scale +%= normalize(T, &aSignificand);
        if (bAbs < implicitBit) scale +%= normalize(T, &bSignificand);
    }

    // Or in the implicit significand bit.  (If we fell through from the
    // denormal path it was already set by normalize( ), but setting it twice
    // won't hurt anything.)
    aSignificand |= implicitBit;
    bSignificand |= implicitBit;

    // Get the significand of a*b.  Before multiplying the significands, shift
    // one of them left to left-align it in the field.  Thus, the product will
    // have (exponentBits + 2) integral digits, all but two of which must be
    // zero.  Normalizing this result is just a conditional left-shift by one
    // and bumping the exponent accordingly.
    var productHi: Z = undefined;
    var productLo: Z = undefined;
    wideMultiply(Z, aSignificand, bSignificand << exponentBits, &productHi, &productLo);

    var productExponent: i32 = @bitCast(i32, aExponent +% bExponent) -% exponentBias +% scale;

    // Normalize the significand, adjust exponent if needed.
    if ((productHi & implicitBit) != 0) {
        productExponent +%= 1;
    } else {
        productHi = (productHi << 1) | (productLo >> (typeWidth - 1));
        productLo = productLo << 1;
    }

    // If we have overflowed the type, return +/- infinity.
    if (productExponent >= maxExponent) return @bitCast(T, infRep | productSign);

    if (productExponent <= 0) {
        // Result is denormal before rounding
        //
        // If the result is so small that it just underflows to zero, return
        // a zero of the appropriate sign.  Mathematically there is no need to
        // handle this case separately, but we make it a special case to
        // simplify the shift logic.
        const shift: u32 = @truncate(u32, Z(1) -% @bitCast(u32, productExponent));
        if (shift >= typeWidth) return @bitCast(T, productSign);

        // Otherwise, shift the significand of the result so that the round
        // bit is the high bit of productLo.
        wideRightShiftWithSticky(Z, &productHi, &productLo, shift);
    } else {
        // Result is normal before rounding; insert the exponent.
        productHi &= significandMask;
        productHi |= Z(@bitCast(u32, productExponent)) << significandBits;
    }

    // Insert the sign of the result:
    productHi |= productSign;

    // Final rounding.  The final result may overflow to infinity, or underflow
    // to zero, but those are the correct results in those cases.  We use the
    // default IEEE-754 round-to-nearest, ties-to-even rounding mode.
    if (productLo > signBit) productHi +%= 1;
    if (productLo == signBit) productHi +%= productHi & 1;
    return @bitCast(T, productHi);
}

fn wideMultiply(comptime Z: type, a: Z, b: Z, hi: *Z, lo: *Z) void {
    @setRuntimeSafety(builtin.is_test);
}

fn normalize(comptime T: type, significand: *@IntType(false, T.bit_count)) i32 {
    // @setRuntimeSafety(builtin.is_test);
    // const Z = @IntType(false, T.bit_count);
    // const significandBits = std.math.floatMantissaBits(T);
    // const implicitBit = Z(1) << significandBits;

    // const shift = @clz(Z, significand.*) - @clz(Z, implicitBit);
    // significand.* <<= @intCast(std.math.Log2Int(Z), shift);
    return 0;
}

fn wideRightShiftWithSticky(comptime Z: type, hi: *Z, lo: *Z, count: u32) void {
    @setRuntimeSafety(builtin.is_test);
}

test "import mulXf3" {
    _ = @import("mulXf3_test.zig");
}
