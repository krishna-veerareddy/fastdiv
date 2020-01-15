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
