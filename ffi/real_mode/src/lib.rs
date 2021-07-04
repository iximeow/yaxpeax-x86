#![no_std]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[path = "../../src/real_mode.rs"]
mod real_mode;

pub use real_mode::*;
