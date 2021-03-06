# Fastdiv
Efficiently computes integer division, modulus and divisibility checks for repeatedly used non-const divisors using techniques discussed in ["Faster Remainder by Direct Computation: Applications to Compilers and Software Libraries"](https://arxiv.org/abs/1902.01961) by Daniel Lemire, Owen Kaser, Nathan Kurz.

Inspired by the Go library [fastdiv](https://github.com/bmkessler/fastdiv) written by [bmkessler](https://github.com/bmkessler).

[![](https://github.com/krishna-veerareddy/fastdiv/workflows/ci/badge.svg)](https://github.com/krishna-veerareddy/fastdiv/actions)

## Usage
```rust
extern crate fastdiv;

use fastdiv::DivisorU16;

fn main() {
    let divisor = DivisorU16::new(7);
    let dividend = 14;

    let quotient = dividend / divisor;
    let remainder = dividend % divisor;
    assert!(divisor.divides(dividend));
}
```
