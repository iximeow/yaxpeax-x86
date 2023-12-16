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
//! #[cfg(features="fmt")]
//! assert_eq!("xor eax, eax", inst.to_string());
//! ```
//!
//! instructions, operands, registers, and generally all decoding structures, are in their mode's
//! respective submodule:
//! * `x86_64`/`amd64` decoding is under [`long_mode`]
//! * `x86_32`/`x86` decoding is under [`protected_mode`]
//! * `x86_16`/`8086` decoding is under [`real_mode`]
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
//! #[cfg(features="fmt")]
//! assert_eq!("xor eax, dword [rcx]", inst.to_string());
//!
//! assert_eq!(Operand::Register(RegSpec::eax()), inst.operand(0));
//! #[cfg(features="fmt")]
//! assert_eq!("eax", inst.operand(0).to_string());
//! assert_eq!(Operand::RegDeref(RegSpec::rcx()), inst.operand(1));
//!
//! // an operand in isolation does not know the size of memory it references, if any
//! #[cfg(features="fmt")]
//! assert_eq!("[rcx]", inst.operand(1).to_string());
//!
//! // and for memory operands, the size must be read from the instruction itself:
//! let mem_size: yaxpeax_x86::amd64::MemoryAccessSize = inst.mem_size().unwrap();
//! assert_eq!("dword", mem_size.size_name());
//!
//! // `MemoryAccessSize::size_name()` is how its `Display` impl works, as well:
//! #[cfg(features="fmt")]
//! assert_eq!("dword", mem_size.to_string());
//! ```
//!
//! `yaxpeax-x86` can also be used to decode instructions generically through the `yaxpeax-arch`
//! traits:
//! ```
//! mod decoder {
//!     use yaxpeax_arch::{Arch, AddressDisplay, Decoder, Reader, ReaderBuilder};
//!
//!     // have to play some games so this example works right even without `fmt` enabled!
//!     #[cfg(feature="fmt")]
//!     trait InstBound: std::fmt::Display {}
//!     #[cfg(not(feature="fmt"))]
//!     trait InstBound {}
//!
//!     #[cfg(feature="fmt")]
//!     impl <T: std::fmt::Display> InstBound for T {}
//!     #[cfg(not(feature="fmt"))]
//!     impl <T> InstBound for T {}
//!
//!     pub fn decode_stream<
//!         'data,
//!         A: yaxpeax_arch::Arch,
//!         U: ReaderBuilder<A::Address, A::Word>,
//!     >(data: U) where
//!         A::Instruction: InstBound,
//!     {
//!         let mut reader = ReaderBuilder::read_from(data);
//!         let mut address: A::Address = reader.total_offset();
//!
//!         let decoder = A::Decoder::default();
//!         let mut decode_res = decoder.decode(&mut reader);
//!         loop {
//!             match decode_res {
//!                 Ok(ref inst) => {
//!                     #[cfg(feature="fmt")]
//!                     println!("{}: {}", address.show(), inst);
//!                     decode_res = decoder.decode(&mut reader);
//!                     address = reader.total_offset();
//!                 }
//!                 Err(e) => {
//!                     println!("{}: decode error: {}", address.show(), e);
//!                     break;
//!                 }
//!             }
//!         }
//!     }
//! }
//!
//! use yaxpeax_x86::amd64::{Arch as x86_64};
//! use yaxpeax_arch::{ReaderBuilder, U8Reader};
//! let data: &[u8] = &[0x55, 0x33, 0xc0, 0x48, 0x8b, 0x02, 0x5d, 0xc3];
//! decoder::decode_stream::<x86_64, _>(data);
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

pub mod long_mode;
pub use long_mode as amd64;
pub use long_mode::Arch as x86_64;

pub mod protected_mode;
pub use protected_mode::Arch as x86_32;

pub mod real_mode;
pub use real_mode::Arch as x86_16;

mod safer_unchecked;

const MEM_SIZE_STRINGS: [&'static str; 64] = [
    "byte", "word", "BUG", "dword", "ptr", "far", "BUG", "qword",
    "BUG", "mword", "BUG", "BUG", "BUG", "BUG", "BUG", "xmmword",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "ymmword",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "m384b",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "ptr", "zmmword",
];

pub struct MemoryAccessSize {
    size: u8,
}
impl MemoryAccessSize {
    /// return the number of bytes referenced by this memory access.
    ///
    /// if the number of bytes cannot be confidently known by the instruction in isolation (as is
    /// the case for `xsave`/`xrstor`-style "operate on all processor state" instructions), this
    /// function will return `None`.
    pub fn bytes_size(&self) -> Option<u8> {
        if self.size == 63 {
            None
        } else {
            Some(self.size)
        }
    }

    /// a human-friendly label for the number of bytes this memory access references.
    ///
    /// there are some differences from size names that may be expected elsewhere; `yaxpeax-x86`
    /// prefers to use consistent names for a width even if the way those bytes are used varies.
    ///
    /// the sizes `yaxpeax-x86` knows are as follows:
    /// | size (bytes) | name       |
    /// |--------------|------------|
    /// | 1            | `byte`     |
    /// | 2            | `word`     |
    /// | 4            | `dword`    |
    /// | 6            | `far`      |
    /// | 8            | `qword`    |
    /// | 10           | `mword`    |
    /// | 16           | `xmmword`  |
    /// | 32           | `ymmword`  |
    /// | 64           | `zmmword`  |
    /// | variable     | `ptr`      |
    ///
    /// "mword" refers to an mmx-sized access - 80 bits, or 10 bytes. `mword` is also used for
    /// 64-bit far calls, because they reference a contiguous ten bytes; two bytes of segment
    /// selector and eight bytes of address.
    ///
    /// "variable" accesses access a number of bytes dependent on the physical processor and its
    /// operating mode. this is particularly relevant for `xsave`/`xrstor`-style instructions.
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

#[cfg(feature = "fmt")]
impl core::fmt::Debug for MemoryAccessSize {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}
