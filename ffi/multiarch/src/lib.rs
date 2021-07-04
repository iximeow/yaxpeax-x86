#![no_std]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[path = "../../src/long_mode.rs"]
mod long_mode;

pub use long_mode::*;

#[path = "../../src/protected_mode.rs"]
mod protected_mode;

pub use protected_mode::*;

#[path = "../../src/real_mode.rs"]
mod real_mode;

pub use real_mode::*;
