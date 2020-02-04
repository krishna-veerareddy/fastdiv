extern crate rand;
extern crate fastdiv;

use rand::prelude::*;
use rand::rngs::SmallRng;
use std::panic;

macro_rules! generate_test {
    ($Ty: ty, $DivisorTy: ident, $Iterations: expr, $TestFn: ident, $InvalidDivisors: expr) => {
        #[test]
        fn $TestFn() {
            let mut rng = SmallRng::from_rng(rand::thread_rng()).unwrap();

            for _ in 0..$Iterations {
                let numerator: $Ty = rng.gen();
                let denominator: $Ty = rng.gen();

                // Ensure invalid divisors cause panic
                if $InvalidDivisors.iter().any(|&divisor| divisor == denominator) {
                    assert!(
                        std::panic::catch_unwind(|| {
                            let _ = fastdiv::$DivisorTy::new(denominator);
                        }).is_err(),
                        "{} should panic when constructed with {}", stringify!($DivisorTy), denominator
                    );

                    continue;
                }

                let divisor = fastdiv::$DivisorTy::new(denominator);
                let (ctrl_quot, ctrl_rem) = (numerator / denominator, numerator % denominator);
                let ctrl_divisibility = numerator % denominator == 0;
                let (quot, rem) = (numerator / divisor, numerator % divisor);
                let (dm_quot, dm_rem) = divisor.div_mod(numerator);
                let mut numerator_div_assign = numerator;

                numerator_div_assign /= divisor;

                assert_eq!(
                    quot, ctrl_quot,
                    "expected {} / {} to be {} but found {}",
                    numerator, denominator, ctrl_quot, quot
                );

                assert_eq!(
                    rem, ctrl_rem,
                    "expected {} % {} to be {} but found {}",
                    numerator, denominator, ctrl_rem, rem
                );

                assert_eq!(
                    numerator_div_assign, ctrl_quot,
                    "expected dividend after div_assign({}, {}) to be {} but found {}",
                    numerator, denominator, ctrl_quot, numerator_div_assign
                );

                assert!(
                    (ctrl_quot == dm_quot) && (ctrl_rem == dm_rem),
                    "expected div_mod({}, {}) to be ({}, {}) but found ({}, {})",
                    numerator, denominator, ctrl_quot, ctrl_rem, dm_quot, dm_rem
                );

                assert_eq!(
                    divisor.divides(numerator),
                    ctrl_divisibility,
                    "{} {} divisible by {}",
                    numerator, if ctrl_divisibility { "is" } else { "is not" }, denominator
                );
            }
        }
    }
}

generate_test!(u8, DivisorU8, 100_000, fastdiv_u8, [0]);
generate_test!(u16, DivisorU16, 10_000_000, fastdiv_u16, [0]);
generate_test!(u32, DivisorU32, 100_000_000, fastdiv_u32, [0]);

generate_test!(i8, DivisorI8, 100_000, fastdiv_i8, [i8::min_value(), -1, 0, 1]);
generate_test!(i16, DivisorI16, 10_000_000, fastdiv_i16, [i16::min_value(), -1, 0, 1]);
generate_test!(i32, DivisorI32, 100_000_000, fastdiv_i32, [i32::min_value(), -1, 0, 1]);
