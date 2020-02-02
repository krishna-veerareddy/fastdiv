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
                let x: $Ty = rng.gen();

                // Ensure invalid divisors cause panic
                if $InvalidDivisors.iter().any(|&divisor| divisor == x) {
                    assert!(
                        std::panic::catch_unwind(|| {
                            let _ = fastdiv::$DivisorTy::new(x);
                        }).is_err(),
                        "{} should panic when constructed with {}", stringify!($DivisorTy), x
                    );

                    continue;
                }

                let divisor = fastdiv::$DivisorTy::new(x);
                let y: $Ty = rng.gen();
                let (ctrl_quot, ctrl_rem) = (y / x, y % x);
                let (quot, rem) = (divisor.divide(y), divisor.modulo(y));
                let (dr_quot, dr_rem) = divisor.div_mod(y);

                assert_eq!(
                    quot, ctrl_quot,
                    "expected ({} / {}) to be {} but found {}",
                    y, x, ctrl_quot, quot
                );

                assert_eq!(
                    rem, ctrl_rem,
                    "expected ({} % {}) to be {} but found {}",
                    y, x, ctrl_rem, rem
                );

                assert!(
                    (ctrl_quot == dr_quot) && (ctrl_rem == dr_rem),
                    "expected div_rem({}, {}) to be ({}, {}) but found ({}, {})",
                    y, x, ctrl_quot, ctrl_rem, dr_quot, dr_rem
                );

                assert_eq!(
                    divisor.divides(y),
                    (y % x) == 0,
                    "{} {} divisible by {}",
                    y, if y % x == 0 { "is" } else { "is not" }, x
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
