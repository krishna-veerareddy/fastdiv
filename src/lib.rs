#![no_std]

//! Fast division, modulus and divisibility checks for repeatedly used
//! non-const divisors.
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
//! let quotient = dividend / divisor;
//! let remainder = dividend % divisor;
//!
//! assert!(divisor.divides(dividend));
//! ```

use core::fmt;
use core::ops::{Div, DivAssign, Rem, RemAssign};
use core::mem::size_of;

macro_rules! doc_comment {
    ($x:expr, $($tt:tt)*) => {
        #[doc = $x]
        $($tt)*
    };
}

macro_rules! unsigned_multipliers {
    (
        $Ty: ty, $Wide2Ty: ty,
        $MulLo: ident, $MulHi: ident, $Mul: ident
    ) => {
        #[inline]
        const fn $MulLo(a: $Ty, b: $Ty) -> $Ty {
            a.wrapping_mul(b)
        }

        #[inline]
        const fn $MulHi(a: $Ty, b: $Ty) -> $Ty {
            ((a as $Wide2Ty) * (b as $Wide2Ty) >> {size_of::<$Ty>() * 8}) as $Ty
        }

        #[inline]
        const fn $Mul(a: $Ty, b: $Ty) -> ($Ty, $Ty) {
            let prod = (a as $Wide2Ty) * (b as $Wide2Ty);

            ((prod >> {size_of::<$Ty>() * 8}) as $Ty, prod as $Ty)
        }
    }
}

unsigned_multipliers!(u16, u32, mullo16, mulhi16, mul16);
unsigned_multipliers!(u32, u64, mullo32, mulhi32, mul32);
unsigned_multipliers!(u64, u128, mullo64, mulhi64, mul64);

macro_rules! unsigned_divisor {
    (
        $Ty: ty, $Wide2Ty: ty,
        $DivisorTy: ident,
        $MulLo: ident, $MulHi: ident, $Mul: ident
    ) => {
        #[derive(Copy, Clone, Debug)]
        pub struct $DivisorTy {
            divisor: $Wide2Ty,
            magic: $Wide2Ty,
        }

        impl $DivisorTy {
            doc_comment! {
                concat! (
"Creates a new `", stringify!($Ty), "` divisor.

# Examples
```
extern crate fastdiv;

let d = fastdiv::", stringify!($DivisorTy), "::new(7);
```

# Panics
Panics if divisor is 0."
                ),
                pub fn new(divisor: $Ty) -> Self {
                    assert!(divisor != 0, "divisor cannot be zero");

                    Self {
                        divisor: divisor as $Wide2Ty,
                        magic: (!(0 as $Wide2Ty) / (divisor as $Wide2Ty)).wrapping_add(1),
                    }
                }
            }

            doc_comment! {
                concat!(
"Simultaneously computes the quotient and remainder when
dividend is divided by self. This method is more efficient
than computing quotient and remainder independently.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(7);
let dividend = 13;
let (quotient, remainder) = divisor.div_mod(dividend);

assert!(quotient == 1 && remainder == 6);
```"
                ),
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
            }

            doc_comment! {
                concat!(
"Checks whether dividend is divisible by self. This method is
more efficient than checking divisibility by computing the
remainder and comparing the result to zero.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(7);
let dividend = 13;

assert!(!divisor.divides(dividend));
```"
                ),
                #[inline]
                pub fn divides(self, dividend: $Ty) -> bool {
                    self.divisor == 1 || $MulLo(self.magic, dividend as $Wide2Ty) < self.magic
                }
            }
        }

        impl Div<$DivisorTy> for $Ty {
            type Output = $Ty;

            #[inline]
            fn div(self, rhs: $DivisorTy) -> $Ty {
                if rhs.divisor == 1 {
                    self
                } else {
                    $MulHi(rhs.magic, self as $Wide2Ty) as $Ty
                }
            }
        }

        impl DivAssign<$DivisorTy> for $Ty {
            #[inline]
            fn div_assign(&mut self, rhs: $DivisorTy) {
                if rhs.divisor != 1 {
                    *self = $MulHi(rhs.magic, *self as $Wide2Ty) as $Ty;
                }
            }
        }

        impl Rem<$DivisorTy> for $Ty {
            type Output = $Ty;

            #[inline]
            fn rem(self, rhs: $DivisorTy) -> $Ty {
                if rhs.divisor == 1 {
                    0
                } else {
                    let fraction = $MulLo(rhs.magic, self as $Wide2Ty);

                    $MulHi(rhs.divisor, fraction) as $Ty
                }
            }
        }

        impl RemAssign<$DivisorTy> for $Ty {
            #[inline]
            fn rem_assign(&mut self, rhs: $DivisorTy) {
                if rhs.divisor == 1 {
                    *self = 0
                } else {
                    let fraction = $MulLo(rhs.magic, *self as $Wide2Ty);

                    *self = $MulHi(rhs.divisor, fraction) as $Ty;
                }
            }
        }

        impl fmt::Display for $DivisorTy {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(formatter, "{}({})", stringify!($DivisorTy), self.divisor)
            }
        }
    }
}

unsigned_divisor!(u8, u16, DivisorU8, mullo16, mulhi16, mul16);
unsigned_divisor!(u16, u32, DivisorU16, mullo32, mulhi32, mul32);
unsigned_divisor!(u32, u64, DivisorU32, mullo64, mulhi64, mul64);

macro_rules! signed_divisor {
    (
        $Ty: ty, $Wide2Ty: ty, $Wide2UnsignedTy: ty,
        $DivisorTy: ident,
        $MulLo: ident, $MulHi: ident, $Mul: ident
    ) => {
        #[derive(Copy, Clone, Debug)]
        pub struct $DivisorTy {
            abs_divisor: $Wide2UnsignedTy,
            magic: $Wide2UnsignedTy,
            is_negative: bool,
        }

        impl $DivisorTy {
            doc_comment! {
                concat! (
"Creates a new `", stringify!($Ty), "` divisor.

# Examples
```
extern crate fastdiv;

let d = fastdiv::", stringify!($DivisorTy), "::new(-7);
```

# Panics
Panics if divisor is one of -1, 0, 1 or std::", stringify!($Ty), "::MIN."
                ),
                pub fn new(divisor: $Ty) -> Self {
                    assert!(
                        !([0, 1, -1, <$Ty>::min_value()].iter().any(|&x| x == divisor)),
                        "divisor cannot be {}", divisor
                    );

                    let is_negative = divisor.is_negative();
                    let abs_divisor = Self::flip_sign(divisor, is_negative) as $Wide2UnsignedTy;

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
            }

            // Flips sign of an integer if condition is true
            #[inline]
            fn flip_sign(number: $Ty, condition: bool) -> $Ty {
                let sign = -(condition as $Ty);

                (number ^ sign) - sign
            }

            doc_comment! {
                concat!(
"Simultaneously computes the quotient and remainder when
dividend is divided by self. This method is more efficient
than computing quotient and remainder independently.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(-7);
let dividend = 13;
let (quotient, remainder) = divisor.div_mod(dividend);

assert!(quotient == -1 && remainder == 6);
```"
                ),
                #[inline]
                pub fn div_mod(self, dividend: $Ty) -> ($Ty, $Ty) {
                    let dividend_is_negative = dividend.is_negative();
                    let abs_dividend = (dividend as $Wide2Ty).abs() as $Wide2UnsignedTy;
                    let (quotient, fraction) = $Mul(self.magic, abs_dividend);
                    let signed_quotient = quotient as $Ty;
                    let signed_remainder = $MulHi(fraction, self.abs_divisor) as $Ty;

                    (
                        Self::flip_sign(signed_quotient, self.is_negative ^ dividend_is_negative),
                        Self::flip_sign(signed_remainder, dividend_is_negative)
                    )
                }
            }

            doc_comment! {
                concat!(
"Checks whether dividend is divisible by self. This method is
more efficient than checking divisibility by computing the
remainder and comparing the result to zero.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(-7);
let dividend = 13;

assert!(!divisor.divides(dividend));
```"
                ),
                #[inline]
                pub fn divides(self, dividend: $Ty) -> bool {
                    $MulLo(self.magic, (dividend as $Wide2Ty).abs() as $Wide2UnsignedTy) < self.magic
                }
            }
        }

        impl Div<$DivisorTy> for $Ty {
            type Output = $Ty;

            #[inline]
            #[allow(clippy::suspicious_arithmetic_impl)]
            fn div(self, rhs: $DivisorTy) -> $Ty {
                let is_negative = rhs.is_negative ^ self.is_negative();
                let abs_dividend = (self as $Wide2Ty).abs() as $Wide2UnsignedTy;
                let quotient = $MulHi(rhs.magic, abs_dividend);

                <$DivisorTy>::flip_sign(quotient as $Ty, is_negative)
            }
        }

        impl DivAssign<$DivisorTy> for $Ty {
            #[inline]
            #[allow(clippy::suspicious_op_assign_impl)]
            fn div_assign(&mut self, rhs: $DivisorTy) {
                let is_negative = rhs.is_negative ^ self.is_negative();
                let abs_dividend = (*self as $Wide2Ty).abs() as $Wide2UnsignedTy;
                let quotient = $MulHi(rhs.magic, abs_dividend);

                *self = <$DivisorTy>::flip_sign(quotient as $Ty, is_negative);
            }
        }

        impl Rem<$DivisorTy> for $Ty {
            type Output = $Ty;

            #[inline]
            fn rem(self, rhs: $DivisorTy) -> $Ty {
                let fraction = $MulLo(rhs.magic, self as $Wide2UnsignedTy);
                let quotient = $MulHi(rhs.abs_divisor, fraction);

                (quotient as $Ty) - (((rhs.abs_divisor as $Ty).wrapping_sub(1)) & (self >> {(size_of::<$Ty>() * 8) - 1}))
            }
        }

        impl RemAssign<$DivisorTy> for $Ty {
            #[inline]
            fn rem_assign(&mut self, rhs: $DivisorTy) {
                let fraction = $MulLo(rhs.magic, *self as $Wide2UnsignedTy);
                let quotient = $MulHi(rhs.abs_divisor, fraction);

                *self = (quotient as $Ty) - (((rhs.abs_divisor as $Ty).wrapping_sub(1)) & (*self >> {(size_of::<$Ty>() * 8) - 1}));
            }
        }

        impl fmt::Display for $DivisorTy {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(
                    formatter,
                    "{}({}{})",
                    stringify!($DivisorTy),
                    if self.is_negative { "-" } else { "" },
                    self.abs_divisor
                )
            }
        }
    }
}

signed_divisor!(i8, i16, u16, DivisorI8, mullo16, mulhi16, mul16);
signed_divisor!(i16, i32, u32, DivisorI16, mullo32, mulhi32, mul32);
signed_divisor!(i32, i64, u64, DivisorI32, mullo64, mulhi64, mul64);
