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
//! let quotient = divisor.divide(dividend);
//! let remainder = divisor.modulo(dividend);
//!
//! assert!(divisor.divides(dividend));
//! ```

macro_rules! doc_comment {
    ($x:expr, $($tt:tt)*) => {
        #[doc = $x]
        $($tt)*
    };
}

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
                concat! (
"Computes `dividend / self` and returns the quotient.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(7);
let dividend = 13;
let quotient = divisor.divide(dividend);

assert_eq!(quotient, 1);
```"
                ),
                #[inline]
                pub fn divide(self, dividend: $Ty) -> $Ty {
                    if self.divisor == 1 {
                        dividend
                    } else {
                        $MulHi(self.magic, dividend as $Wide2Ty) as $Ty
                    }
                }
            }

            doc_comment! {
                concat!(
"Computes `dividend % self` and returns the remainder.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(7);
let dividend = 13;
let remainder = divisor.modulo(dividend);

assert_eq!(remainder, 6);
```"
                ),
                #[inline]
                pub fn modulo(self, dividend: $Ty) -> $Ty {
                    if self.divisor == 1 {
                        0
                    } else {
                        let fraction = $MulLo(self.magic, dividend as $Wide2Ty);

                        $MulHi(self.divisor, fraction) as $Ty
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
                concat! (
"Computes `dividend / self` and returns the quotient.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(-7);
let dividend = 13;
let quotient = divisor.divide(dividend);

assert_eq!(quotient, -1);
```"
                ),
                #[inline]
                pub fn divide(self, dividend: $Ty) -> $Ty {
                    let is_negative = self.is_negative ^ dividend.is_negative();
                    let abs_dividend = (dividend as $Wide2Ty).abs() as $Wide2UnsignedTy;
                    let quotient = $MulHi(self.magic, abs_dividend);

                    Self::flip_sign(quotient as $Ty, is_negative)
                }
            }

            doc_comment! {
                concat!(
"Computes `dividend % self` and returns the remainder.

# Examples
```
extern crate fastdiv;

let divisor = fastdiv::", stringify!($DivisorTy), "::new(-7);
let dividend = 13;
let remainder = divisor.modulo(dividend);

assert_eq!(remainder, 6);
```"
                ),
                #[inline]
                pub fn modulo(self, dividend: $Ty) -> $Ty {
                    let fraction = $MulLo(self.magic, dividend as $Wide2UnsignedTy);
                    let quotient = $MulHi(self.abs_divisor, fraction);

                    (quotient as $Ty) - (((self.abs_divisor as $Ty).wrapping_sub(1)) & (dividend >> {$Width - 1}))
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
    }
}

signed_divisor!(i8, i16, u16, 8, DivisorI8, mullo16, mulhi16, mul16);
signed_divisor!(i16, i32, u32, 16, DivisorI16, mullo32, mulhi32, mul32);
signed_divisor!(i32, i64, u64, 32, DivisorI32, mullo64, mulhi64, mul64);
