#![no_std]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[path = "../../src/long_mode.rs"]
mod long_mode;

pub use long_mode::*;
