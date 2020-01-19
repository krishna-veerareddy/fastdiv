#![no_std]

//! Fast division, modulus and divisibility checks for non-const divisors.
//!
//! # Usage
//!
//! ```rust
//! extern crate fastdiv;
//!
//! use fastdiv::DivisorU16;
//!
//! let divisor = DivisorU16::new(7);
//! let dividend = 14;
//!
//! let _ = divisor.divide(dividend);
//! let _ = divisor.modulo(dividend);
//!
//! assert!(divisor.divides(dividend));
//! ```

macro_rules! unsigned_divisor {
    (
        $Ty: ty, $Wide2Ty: ty, $Wide4Ty: ty,
        $Width: expr,
        $DivisorTy: ident,
        $MulLo: ident, $MulHi: ident, $Mul: ident
    ) => {
        #[inline]
        const fn $MulLo(a: $Wide2Ty, b: $Wide2Ty) -> $Wide2Ty {
            a.wrapping_mul(b)
        }

        #[inline]
        const fn $MulHi(a: $Wide2Ty, b: $Wide2Ty) -> $Wide2Ty {
            ((a as $Wide4Ty) * (b as $Wide4Ty) >> {$Width * 2}) as $Wide2Ty
        }

        #[inline]
        const fn $Mul(a: $Wide2Ty, b: $Wide2Ty) -> ($Wide2Ty, $Wide2Ty) {
            let prod = (a as $Wide4Ty) * (b as $Wide4Ty);

            ((prod >> {$Width * 2}) as $Wide2Ty, prod as $Wide2Ty)
        }

        #[derive(Copy, Clone)]
        pub struct $DivisorTy {
            divisor: $Wide2Ty,
            magic: $Wide2Ty,
        }

        impl $DivisorTy {
            pub fn new(divisor: $Ty) -> Self {
                assert!(divisor != 0, "divisor cannot be zero");

                Self {
                    divisor: divisor as $Wide2Ty,
                    magic: (!(0 as $Wide2Ty) / (divisor as $Wide2Ty)).wrapping_add(1),
                }
            }

            #[inline]
            pub fn divide(self, dividend: $Ty) -> $Ty {
                if self.divisor == 1 {
                    dividend
                } else {
                    $MulHi(self.magic, dividend as $Wide2Ty) as $Ty
                }
            }

            #[inline]
            pub fn modulo(self, dividend: $Ty) -> $Ty {
                if self.divisor == 1 {
                    0
                } else {
                    let fraction = $MulLo(self.magic, dividend as $Wide2Ty);

                    $MulHi(self.divisor, fraction) as $Ty
                }
            }

            #[inline]
            pub fn div_mod(self, dividend: $Ty) -> ($Ty, $Ty) {
                if self.divisor == 1 {
                    (dividend, 0)
                } else {
                    let (quotient, fraction) = $Mul(self.magic, dividend as $Wide2Ty);
                    let remainder = $MulHi(fraction, self.divisor);

                    (quotient as $Ty, remainder as $Ty)
                }
            }

            #[inline]
            pub fn divides(self, dividend: $Ty) -> bool {
                self.divisor == 1 || $MulLo(self.magic, dividend as $Wide2Ty) < self.magic
            }
        }
    }
}

unsigned_divisor!(u8, u16, u32, 8, DivisorU8, mullo16, mulhi16, mul16);
unsigned_divisor!(u16, u32, u64, 16, DivisorU16, mullo32, mulhi32, mul32);
unsigned_divisor!(u32, u64, u128, 32, DivisorU32, mullo64, mulhi64, mul64);

macro_rules! signed_divisor {
    (
        $Ty: ty, $Wide2Ty: ty, $Wide2UnsignedTy: ty,
        $Width: expr,
        $DivisorTy: ident,
        $MulLo: ident, $MulHi: ident, $Mul: ident
    ) => {
        #[derive(Copy, Clone)]
        pub struct $DivisorTy {
            abs_divisor: $Wide2UnsignedTy,
            magic: $Wide2UnsignedTy,
            is_negative: bool,
        }

        impl $DivisorTy {
            pub fn new(divisor: $Ty) -> Self {
                assert!(
                    !([0, 1, -1, <$Ty>::min_value()].iter().any(|&x| x == divisor)),
                    "divisor cannot be {}", divisor
                );

                let mut is_negative = false;

                let abs_divisor = if divisor < 0 {
                    is_negative = true;
                    -(divisor as $Wide2Ty)
                } else {
                    divisor as $Wide2Ty
                } as $Wide2UnsignedTy;

                let mut magic = (!(0 as $Wide2UnsignedTy) / abs_divisor).wrapping_add(1);

                if abs_divisor.is_power_of_two() {
                    magic = magic.wrapping_add(1);
                }

                Self {
                    abs_divisor,
                    magic,
                    is_negative,
                }
            }

            #[inline]
            pub fn divide(self, dividend: $Ty) -> $Ty {
                let is_negative = self.is_negative ^ dividend.is_negative();
                let abs_dividend = (dividend as $Wide2Ty).abs() as $Wide2UnsignedTy;
                let quotient = $MulHi(self.magic, abs_dividend);

                if is_negative {
                    -(quotient as $Ty)
                } else {
                    quotient as $Ty
                }
            }

            #[inline]
            pub fn modulo(self, dividend: $Ty) -> $Ty {
                let fraction = $MulLo(self.magic, dividend as $Wide2UnsignedTy);
                let quotient = $MulHi(self.abs_divisor, fraction);

                (quotient as $Ty) - (((self.abs_divisor as $Ty).wrapping_sub(1)) & (dividend >> {$Width - 1}))
            }

            #[inline]
            pub fn div_mod(self, dividend: $Ty) -> ($Ty, $Ty) {
                let is_negative = dividend.is_negative();
                let abs_dividend = (dividend as $Wide2Ty).abs() as $Wide2UnsignedTy;
                let (quotient, fraction) = $Mul(self.magic, abs_dividend);
                let mut signed_quotient = quotient as $Ty;
                let mut signed_remainder = $MulHi(fraction, self.abs_divisor) as $Ty;

                if is_negative {
                    signed_quotient = -signed_quotient;
                    signed_remainder = -signed_remainder
                }

                if self.is_negative {
                    signed_quotient = -signed_quotient;
                }

                (signed_quotient, signed_remainder)
            }

            #[inline]
            pub fn divides(self, dividend: $Ty) -> bool {
                $MulLo(self.magic, (dividend as $Wide2Ty).abs() as $Wide2UnsignedTy) < self.magic
            }
        }
    }
}

signed_divisor!(i8, i16, u16, 8, DivisorI8, mullo16, mulhi16, mul16);
signed_divisor!(i16, i32, u32, 16, DivisorI16, mullo32, mulhi32, mul32);
signed_divisor!(i32, i64, u64, 32, DivisorI32, mullo64, mulhi64, mul64);
