#![no_std]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[path = "../../src/protected_mode.rs"]
mod protected_mode;

pub use protected_mode::*;
