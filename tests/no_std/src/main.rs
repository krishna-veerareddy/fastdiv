#![feature(lang_items, start)]
#![no_std]

extern crate fastdiv;

use fastdiv::DivisorU16;

#[lang = "eh_personality"]
#[no_mangle]
pub extern "C" fn rust_eh_personality() {}

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe {
        libc::abort();
    }
}

#[start]
fn start(_argc: isize, _argv: *const *const u8) -> isize {
    let divisor = DivisorU16::new(7);
    let dividend = 10;

    let _ = dividend / divisor;
    let _ = dividend % divisor;
    let _ = divisor.divides(dividend);

    0
}
