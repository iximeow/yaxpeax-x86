#![no_std]

#[cfg(feature="use-serde")]
#[macro_use] extern crate serde_derive;
#[cfg(feature="use-serde")]
extern crate serde;

#[cfg(feature="std")]
extern crate alloc;

extern crate yaxpeax_arch;

pub mod long_mode;
pub use long_mode::Arch as x86_64;

mod protected_mode;
pub use protected_mode::Arch as x86_32;

mod real_mode;
pub use real_mode::Arch as x86_16;
