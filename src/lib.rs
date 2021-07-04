//! # `yaxpeax-x86`, a decoder for x86-family instruction sets
//!
//! `yaxpeax-x86` provides x86 decoders, for 64-, 32-, and 16-bit modes. `yaxpeax-x86` also
//! implements traits defined by `yaxpeax_arch`, making it suitable for interchangeable use with
//! other `yaxpeax`-family instruction decoders.
//!
//! ## usage
//!
//! the fastest way to decode an x86 instruction is through [`amd64::InstDecoder::decode_slice()`]:
//! ```
//! let decoder = yaxpeax_x86::amd64::InstDecoder::default();
//!
//! let inst = decoder.decode_slice(&[0x33, 0xc0]).unwrap();
//!
//! assert_eq!("xor eax, eax", inst.to_string());
//! ```
//!
//! instructions, operands, registers, and generally all decoding structures, are in their mode's
//! repsective submodule:
//! * `x86_64`/`amd64` decoding is under [`yaxpeax_x86::long_mode`]
//! * `x86_32`/`x86` decoding is under [`yaxpeax_x86::protected_mode`]
//! * `x86_16`/`8086` decoding is under [`yaxpeax_x86::real_mode`]
//!
//! all modes have equivalent data available in a decoded instruction. for example, all modes have
//! library-friendly `Operand` and `RegSpec` types:
//!
//! ```
//! use yaxpeax_x86::amd64::{InstDecoder, Operand, RegSpec};
//!
//! let decoder = yaxpeax_x86::amd64::InstDecoder::default();
//!
//! let inst = decoder.decode_slice(&[0x33, 0x01]).unwrap();
//!
//! assert_eq!("xor eax, dword [rcx]", inst.to_string());
//!
//! assert_eq!(Operand::Register(RegSpec::eax()), inst.operand(0));
//! assert_eq!("eax", inst.operand(0).to_string());
//! assert_eq!(Operand::RegDeref(RegSpec::rcx()), inst.operand(1));
//!
//! // an operand in isolation does not know the size of memory it references, if any
//! assert_eq!("[rcx]", inst.operand(1).to_string());
//!
//! // and for memory operands, the size must be read from the instruction itself:
//! let mem_size: yaxpeax_x86::amd64::MemoryAccessSize = inst.mem_size().unwrap();
//! assert_eq!("dword", mem_size.size_name());
//!
//! // `MemoryAccessSize::size_name()` is how its `Display` impl works, as well:
//! assert_eq!("dword", mem_size.to_string());
//! ```
//!
//! ## `#![no_std]`
//!
//! `yaxpeax-x86` supports `no_std` usage. to be built `no_std`, `yaxpeax-x86` only needs
//! `default-features = false` in the corresponding `Cargo.toml` dependency. if formatting is
//! needed with `std` disabled, it can be re-enabled by explicitly requesting the `fmt` features
//! like:
//! ```text
//! yaxpeax-x86 = { version = "*", default-features = false, features = ["fmt"] }
//! ```
//!
//! this is how the `.so` and `.a` packaging in
//! [`ffi/`](https://github.com/iximeow/yaxpeax-x86/tree/no-gods-no-/ffi) is performed.

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

#[cfg(feature = "fmt")]
impl core::fmt::Display for MemoryAccessSize {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str(self.size_name())
    }
}
