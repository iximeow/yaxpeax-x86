#![no_std]

#[cfg(feature="use-serde")]
#[macro_use] extern crate serde_derive;
#[cfg(feature="use-serde")]
extern crate serde;

#[cfg(feature="std")]
extern crate alloc;

extern crate yaxpeax_arch;

pub mod long_mode;
pub use long_mode as amd64;
pub use long_mode::Arch as x86_64;

pub mod protected_mode;
pub use protected_mode::Arch as x86_32;

mod real_mode;
pub use real_mode::Arch as x86_16;

const MEM_SIZE_STRINGS: [&'static str; 64] = [
    "byte", "word", "BUG", "dword", "far", "ptr", "BUG", "qword",
    "far", "mword", "BUG", "BUG", "BUG", "BUG", "BUG", "xmmword",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "ymmword",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "ptr", "zmmword",
];

pub struct MemoryAccessSize {
    size: u8,
}
impl MemoryAccessSize {
    pub fn bytes_size(&self) -> Option<u8> {
        if self.size == 63 {
            None
        } else {
            Some(self.size)
        }
    }

    pub fn size_name(&self) -> &'static str {
        MEM_SIZE_STRINGS[self.size as usize - 1]
    }
}
