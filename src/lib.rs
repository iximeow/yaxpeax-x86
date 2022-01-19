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
//! `yaxpeax-x86` can also be used to decode instructions generically through the `yaxpeax-arch`
//! traits:
//! ```
//! mod decoder {
//!     use yaxpeax_arch::{Arch, AddressDisplay, Decoder, Reader, ReaderBuilder};
//!
//!     pub fn decode_stream<
//!         'data,
//!         A: yaxpeax_arch::Arch,
//!         U: ReaderBuilder<A::Address, A::Word>,
//!     >(data: U) where
//!         A::Instruction: std::fmt::Display,
//!     {
//!         let mut reader = ReaderBuilder::read_from(data);
//!         let mut address: A::Address = reader.total_offset();
//!
//!         let decoder = A::Decoder::default();
//!         let mut decode_res = decoder.decode(&mut reader);
//!         loop {
//!             match decode_res {
//!                 Ok(ref inst) => {
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

#[cfg(feature = "use-serde")]
#[macro_use]
extern crate serde_derive;
#[cfg(feature = "use-serde")]
extern crate serde;

#[cfg(feature = "std")]
extern crate alloc;

pub mod long_mode;
pub use long_mode as amd64;
pub use long_mode::Arch as x86_64;

pub mod protected_mode;
pub use protected_mode::Arch as x86_32;

pub mod real_mode;
pub use real_mode::Arch as x86_16;
use yaxpeax_arch::Instruction;

use crate::safer_unchecked::unreachable_kinda_unchecked as unreachable_unchecked;

mod display;
mod safer_unchecked;

const MEM_SIZE_STRINGS: [&'static str; 64] = [
    "byte", "word", "BUG", "dword", "ptr", "far", "BUG", "qword", "BUG", "mword", "BUG", "BUG",
    "BUG", "BUG", "BUG", "xmmword", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "ymmword", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "ptr", "zmmword",
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

pub trait X86Instruction: Instruction {
    /// Get the opcode of this instruction
    fn opcode(&self) -> Opcode;

    /// get the `Operand` at the provided index.
    ///
    /// panics if the index is `>= 4`.
    fn operand(&self, i: u8) -> Operand;

    /// get the number of operands in this instruction. useful in iterating an instruction's
    /// operands generically.
    fn operand_count(&self) -> u8;

    /// get the `Segment` that will *actually* be used for accessing the operand at index `i`.
    ///
    /// `stos`, `lods`, `movs`, and `cmps` specifically name some segments for use regardless of
    /// prefixes.
    fn segment_override_for_op(&self, op: u8) -> Option<Segment>;
}

impl X86Instruction for crate::long_mode::Instruction {
    fn opcode(&self) -> Opcode {
        self.opcode()
    }

    fn operand(&self, i: u8) -> Operand {
        self.operand(i)
    }

    fn operand_count(&self) -> u8 {
        self.operand_count()
    }

    fn segment_override_for_op(&self, op: u8) -> Option<Segment> {
        self.segment_override_for_op(op)
    }
}

impl X86Instruction for crate::protected_mode::Instruction {
    fn opcode(&self) -> Opcode {
        self.opcode()
    }

    fn operand(&self, i: u8) -> Operand {
        self.operand(i)
    }

    fn operand_count(&self) -> u8 {
        self.operand_count()
    }

    fn segment_override_for_op(&self, op: u8) -> Option<Segment> {
        self.segment_override_for_op(op)
    }
}

impl X86Instruction for crate::real_mode::Instruction {
    fn opcode(&self) -> Opcode {
        self.opcode()
    }

    fn operand(&self, i: u8) -> Operand {
        self.operand(i)
    }

    fn operand_count(&self) -> u8 {
        self.operand_count()
    }

    fn segment_override_for_op(&self, op: u8) -> Option<Segment> {
        self.segment_override_for_op(op)
    }
}

const REGISTER_CLASS_NAMES: &[&'static str] = &[
    "qword",
    "BUG. PLEASE REPORT.",
    "dword",
    "BUG. PLEASE REPORT.",
    "word",
    "BUG. PLEASE REPORT.",
    "byte",
    "BUG. PLEASE REPORT.",
    "rex-byte",
    "BUG. PLEASE REPORT.",
    "cr",
    "BUG. PLEASE REPORT.",
    "dr",
    "BUG. PLEASE REPORT.",
    "segment",
    "xmm",
    "BUG. PLEASE REPORT.",
    "BUG. PLEASE REPORT.",
    "BUG. PLEASE REPORT.",
    "ymm",
    "BUG. PLEASE REPORT.",
    "BUG. PLEASE REPORT.",
    "BUG. PLEASE REPORT.",
    "zmm",
    "BUG. PLEASE REPORT.",
    "BUG. PLEASE REPORT.",
    "BUG. PLEASE REPORT.",
    "x87-stack",
    "mmx",
    "k",
    "eip",
    "rip",
    "eflags",
    "rflags",
];

/// an `x86_64` register class - `qword`, `dword`, `xmmword`, `segment`, and so on.
///
/// this is mostly useful for comparing a `RegSpec`'s [`RegSpec::class()`] with a constant out of
/// [`register_class`].
#[cfg_attr(feature = "use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct RegisterClass {
    kind: RegisterBank,
}

impl RegisterClass {
    /// return a human-friendly name for this register class
    pub fn name(&self) -> &'static str {
        REGISTER_CLASS_NAMES[self.kind as usize]
    }

    /// return the size of this register class, in bytes
    pub fn width(&self) -> u8 {
        match self.kind {
            RegisterBank::Q => 8,
            RegisterBank::D => 4,
            RegisterBank::W => 2,
            RegisterBank::B | RegisterBank::rB => 1,
            RegisterBank::CR | RegisterBank::DR => 8,
            RegisterBank::S => 2,
            RegisterBank::EIP => 4,
            RegisterBank::RIP => 8,
            RegisterBank::EFlags => 4,
            RegisterBank::RFlags => 8,
            RegisterBank::X => 16,
            RegisterBank::Y => 32,
            RegisterBank::Z => 64,
            RegisterBank::ST => 10,
            RegisterBank::MM => 8,
            RegisterBank::K => 8,
        }
    }
}

#[allow(non_camel_case_types)]
#[cfg_attr(feature = "use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(u8)]
enum RegisterBank {
    Q = 0,
    D = 2,
    W = 4,
    B = 6,
    rB = 8, // Quadword, Dword, Word, Byte
    CR = 10,
    DR = 12,
    S = 14,
    EIP = 30,
    RIP = 31,
    EFlags = 32,
    RFlags = 33, // Control reg, Debug reg, Selector, ...
    X = 15,
    Y = 19,
    Z = 23, // XMM, YMM, ZMM
    ST = 27,
    MM = 28, // ST, MM regs (x87, mmx)
    K = 29,  // AVX512 mask registers
}

/// the segment register used by the corresponding instruction.
///
/// typically this will be `ds` but can be overridden. some instructions have specific segment
/// registers used regardless of segment prefixes, and in these cases `yaxpeax-x86` will report the
/// actual segment register a physical processor would use.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Segment {
    DS = 0,
    CS,
    ES,
    FS,
    GS,
    SS,
}

/// an `x86` register, including its number and type. if `fmt` is enabled, name too.
///
/// ```
/// use yaxpeax_x86::{RegSpec, long_mode::register_class};
///
/// assert_eq!(RegSpec::ecx().num(), 1);
/// assert_eq!(RegSpec::ecx().class(), register_class::D);
/// ```
///
/// some registers have classes of their own, and only one member: `eip` and `eflags`.
#[cfg_attr(feature = "use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialOrd, Ord, Eq, PartialEq)]
pub struct RegSpec {
    num: u8,
    bank: RegisterBank,
}

use core::hash::Hash;
use core::hash::Hasher;
impl Hash for RegSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let code = ((self.bank as u16) << 8) | (self.num as u16);
        code.hash(state);
    }
}

macro_rules! register {
    ($bank:ident, $name:ident => $num:expr, $($tail:tt)+) => {
        #[inline]
        pub const fn $name() -> RegSpec {
            RegSpec { bank: RegisterBank::$bank, num: $num }
        }

        register!($bank, $($tail)*);
    };
    ($bank:ident, $name:ident => $num:expr) => {
        #[inline]
        pub const fn $name() -> RegSpec {
            RegSpec { bank: RegisterBank::$bank, num: $num }
        }
    };
}

#[inline]
fn width_to_gp_reg_bank(width: u8, rex: bool) -> RegisterBank {
    match width {
        1 => {
            return if rex {
                RegisterBank::rB
            } else {
                RegisterBank::B
            }
        }
        2 => return RegisterBank::W,
        4 => return RegisterBank::D,
        8 => return RegisterBank::Q,
        _ => unsafe {
            unreachable_unchecked();
        },
    }
}

#[allow(non_snake_case)]
impl RegSpec {
    /// the register `rip`. this register is in the class `rip`, which contains only it.
    pub const RIP: RegSpec = RegSpec::rip();
    /// the register `eip`. this register is in the class `rip`, which contains only it.
    pub const EIP: RegSpec = RegSpec::eip();

    /// the number of this register in its `RegisterClass`.
    ///
    /// for many registers this is a number in the name, but for registers harkening back to
    /// `x86_32`, the first eight registers are `rax`, `rcx`, `rdx`, `rbx`, `rsp`, `rbp`, `rsi`,
    /// and `rdi` (or `eXX` for the 32-bit forms, `XX` for 16-bit forms).
    pub fn num(&self) -> u8 {
        self.num
    }

    /// the class of register this register is in.
    ///
    /// this corresponds to the register's size, but is by the register's usage in the instruction
    /// set; `rax` and `mm0` are the same size, but different classes (`Q`(word) and `MM` (mmx)
    /// respectively).
    pub fn class(&self) -> RegisterClass {
        RegisterClass { kind: self.bank }
    }

    #[cfg(feature = "fmt")]
    /// return a human-friendly name for this register. the returned name is the same as would be
    /// used to render this register in an instruction.
    pub fn name(&self) -> &'static str {
        display::regspec_label(self)
    }

    /// construct a `RegSpec` for x87 register `st(num)`
    #[inline]
    pub fn st(num: u8) -> RegSpec {
        if num >= 8 {
            panic!("invalid x87 reg st({})", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::ST,
        }
    }

    /// construct a `RegSpec` for xmm reg `num`
    #[inline]
    pub fn xmm(num: u8) -> RegSpec {
        if num >= 32 {
            panic!("invalid x86 xmm reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::X,
        }
    }

    /// construct a `RegSpec` for ymm reg `num`
    #[inline]
    pub fn ymm(num: u8) -> RegSpec {
        if num >= 32 {
            panic!("invalid x86 ymm reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::Y,
        }
    }

    /// construct a `RegSpec` for zmm reg `num`
    #[inline]
    pub fn zmm(num: u8) -> RegSpec {
        if num >= 32 {
            panic!("invalid x86 zmm reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::Z,
        }
    }

    /// construct a `RegSpec` for qword reg `num`
    #[inline]
    pub fn q(num: u8) -> RegSpec {
        if num >= 16 {
            panic!("invalid x86 qword reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::Q,
        }
    }

    /// construct a `RegSpec` for mask reg `num`
    #[inline]
    pub fn mask(num: u8) -> RegSpec {
        if num >= 8 {
            panic!("invalid x86 mask reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::K,
        }
    }

    /// construct a `RegSpec` for dword reg `num`
    #[inline]
    pub fn d(num: u8) -> RegSpec {
        if num >= 16 {
            panic!("invalid x86 dword reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::D,
        }
    }

    /// construct a `RegSpec` for word reg `num`
    #[inline]
    pub fn w(num: u8) -> RegSpec {
        if num >= 16 {
            panic!("invalid x86 word reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::W,
        }
    }

    /// construct a `RegSpec` for non-rex byte reg `num`
    #[inline]
    pub fn rb(num: u8) -> RegSpec {
        if num >= 16 {
            panic!("invalid x86 rex-byte reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::rB,
        }
    }

    /// construct a `RegSpec` for non-rex byte reg `num`
    #[inline]
    pub fn b(num: u8) -> RegSpec {
        if num >= 8 {
            panic!("invalid x86 non-rex byte reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::B,
        }
    }

    #[inline]
    fn from_parts(num: u8, extended: bool, bank: RegisterBank) -> RegSpec {
        RegSpec {
            num: num + if extended { 0b1000 } else { 0 },
            bank: bank,
        }
    }

    #[inline]
    fn gp_from_parts(num: u8, extended: bool, width: u8, rex: bool) -> RegSpec {
        RegSpec {
            num: num + if extended { 0b1000 } else { 0 },
            bank: width_to_gp_reg_bank(width, rex),
        }
    }

    register!(RIP, rip => 0);
    register!(EIP, eip => 0);

    register!(RFlags, rflags => 0);
    register!(EFlags, eflags => 0);

    register!(S, es => 0, cs => 1, ss => 2, ds => 3, fs => 4, gs => 5);

    register!(Q,
        rax => 0, rcx => 1, rdx => 2, rbx => 3,
        rsp => 4, rbp => 5, rsi => 6, rdi => 7,
        r8 => 8, r9 => 9, r10 => 10, r11 => 11,
        r12 => 8, r13 => 9, r14 => 14, r15 => 15
    );

    register!(D,
        eax => 0, ecx => 1, edx => 2, ebx => 3,
        esp => 4, ebp => 5, esi => 6, edi => 7,
        r8d => 8, r9d => 9, r10d => 10, r11d => 11,
        r12d => 8, r13d => 9, r14d => 14, r15d => 15
    );

    register!(W,
        ax => 0, cx => 1, dx => 2, bx => 3,
        sp => 4, bp => 5, si => 6, di => 7,
        r8w => 8, r9w => 9, r10w => 10, r11w => 11,
        r12w => 8, r13w => 9, r14w => 14, r15w => 15
    );

    register!(B,
        al => 0, cl => 1, dl => 2, bl => 3,
        ah => 4, ch => 5, dh => 6, bh => 7
    );

    register!(rB,
        spl => 4, bpl => 5, sil => 6, dil => 7,
        r8b => 8, r9b => 9, r10b => 10, r11b => 11,
        r12b => 8, r13b => 9, r14b => 14, r15b => 15
    );

    #[inline]
    pub const fn zmm0() -> RegSpec {
        RegSpec {
            bank: RegisterBank::Z,
            num: 0,
        }
    }

    #[inline]
    pub const fn ymm0() -> RegSpec {
        RegSpec {
            bank: RegisterBank::Y,
            num: 0,
        }
    }

    #[inline]
    pub const fn xmm0() -> RegSpec {
        RegSpec {
            bank: RegisterBank::X,
            num: 0,
        }
    }

    #[inline]
    pub const fn st0() -> RegSpec {
        RegSpec {
            bank: RegisterBank::ST,
            num: 0,
        }
    }

    #[inline]
    pub const fn mm0() -> RegSpec {
        RegSpec {
            bank: RegisterBank::MM,
            num: 0,
        }
    }

    /// return the size of this register, in bytes.
    #[inline]
    pub fn width(&self) -> u8 {
        self.class().width()
    }
}

/// an `avx512` merging mode.
///
/// the behavior for non-`avx512` instructions is equivalent to `merge`.  `zero` is only useful in
/// conjunction with a mask register, where bits specified in the mask register correspond to
/// unmodified items in the instruction's desination.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MergeMode {
    Merge,
    Zero,
}
impl From<bool> for MergeMode {
    fn from(b: bool) -> Self {
        if b {
            MergeMode::Zero
        } else {
            MergeMode::Merge
        }
    }
}
/// an `avx512` custom rounding mode.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SaeMode {
    RoundNearest,
    RoundDown,
    RoundUp,
    RoundZero,
}
const SAE_MODES: [SaeMode; 4] = [
    SaeMode::RoundNearest,
    SaeMode::RoundDown,
    SaeMode::RoundUp,
    SaeMode::RoundZero,
];
impl SaeMode {
    /// a human-friendly label for this `SaeMode`:
    ///
    /// ```
    /// use yaxpeax_x86::long_mode::SaeMode;
    ///
    /// assert_eq!(SaeMode::RoundNearest.label(), "{rne-sae}");
    /// assert_eq!(SaeMode::RoundDown.label(), "{rd-sae}");
    /// assert_eq!(SaeMode::RoundUp.label(), "{ru-sae}");
    /// assert_eq!(SaeMode::RoundZero.label(), "{rz-sae}");
    /// ```
    pub fn label(&self) -> &'static str {
        match self {
            SaeMode::RoundNearest => "{rne-sae}",
            SaeMode::RoundDown => "{rd-sae}",
            SaeMode::RoundUp => "{ru-sae}",
            SaeMode::RoundZero => "{rz-sae}",
        }
    }

    fn from(l: bool, lp: bool) -> Self {
        let mut idx = 0;
        if l {
            idx |= 1;
        }
        if lp {
            idx |= 2;
        }
        SAE_MODES[idx]
    }
}

/// an operand for an `x86` instruction.
///
/// `Operand::Nothing` should be unreachable in practice; any such instructions should have an
/// operand count of 0 (or at least one fewer than the `Nothing` operand's position).
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum Operand {
    /// a sign-extended byte
    ImmediateI8(i8),
    /// a zero-extended byte
    ImmediateU8(u8),
    /// a sign-extended word
    ImmediateI16(i16),
    /// a zero-extended word
    ImmediateU16(u16),
    /// a sign-extended dword
    ImmediateI32(i32),
    /// a zero-extended dword
    ImmediateU32(u32),
    /// a sign-extended qword
    ImmediateI64(i64),
    /// a zero-extended qword
    ImmediateU64(u64),
    /// a bare register operand, such as `rcx`.
    Register(RegSpec),
    /// an `avx512` register operand with optional mask register and merge mode, such as
    /// `zmm3{k4}{z}`.
    ///
    /// if the mask register is `k0`, there is no masking applied, and the default x86 operation is
    /// `MergeMode::Merge`.
    RegisterMaskMerge(RegSpec, RegSpec, MergeMode),
    /// an `avx512` register operand with optional mask register, merge mode, and suppressed
    /// exceptions, such as `zmm3{k4}{z}{rd-sae}`.
    ///
    /// if the mask register is `k0`, there is no masking applied, and the default x86 operation is
    /// `MergeMode::Merge`.
    RegisterMaskMergeSae(RegSpec, RegSpec, MergeMode, SaeMode),
    /// an `avx512` register operand with optional mask register, merge mode, and suppressed
    /// exceptions, with no overridden rounding mode, such as `zmm3{k4}{z}{sae}`.
    ///
    /// if the mask register is `k0`, there is no masking applied, and the default x86 operation is
    /// `MergeMode::Merge`.
    RegisterMaskMergeSaeNoround(RegSpec, RegSpec, MergeMode),
    /// a memory access to a literal word address. it's extremely rare that a well-formed x86
    /// instruction uses this mode. as an example, `[0x1133]`
    DisplacementU16(u16),
    /// a memory access to a literal qword address. it's relatively rare that a well-formed x86
    /// instruction uses this mode, but plausibe. for example, `fs:[0x14]`. segment overrides,
    /// however, are maintained on the instruction itself.
    DisplacementU32(u32),
    /// a memory access to a literal qword address. it's relatively rare that a well-formed x86
    /// instruction uses this mode, but plausibe. for example, `gs:[0x14]`. segment overrides,
    /// however, are maintained on the instruction itself.
    DisplacementU64(u64),
    /// a simple dereference of the address held in some register. for example: `[esi]`.
    RegDeref(RegSpec),
    /// a dereference of the address held in some register with offset. for example: `[esi + 0x14]`.
    RegDisp(RegSpec, i32),
    /// a dereference of the address held in some register scaled by 1, 2, 4, or 8. this is almost always used with the `lea` instruction. for example: `[edx * 4]`.
    RegScale(RegSpec, u8),
    /// a dereference of the address from summing two registers. for example: `[ebp + rax]`
    RegIndexBase(RegSpec, RegSpec),
    /// a dereference of the address from summing two registers with offset. for example: `[edi + ecx + 0x40]`
    RegIndexBaseDisp(RegSpec, RegSpec, i32),
    /// a dereference of the address held in some register scaled by 1, 2, 4, or 8 with offset. this is almost always used with the `lea` instruction. for example: `[eax * 4 + 0x30]`.
    RegScaleDisp(RegSpec, u8, i32),
    /// a dereference of the address from summing a register and index register scaled by 1, 2, 4,
    /// or 8. for
    /// example: `[esi + ecx * 4]`
    RegIndexBaseScale(RegSpec, RegSpec, u8),
    /// a dereference of the address from summing a register and index register scaled by 1, 2, 4,
    /// or 8, with offset. for
    /// example: `[esi + ecx * 4 + 0x1234]`
    RegIndexBaseScaleDisp(RegSpec, RegSpec, u8, i32),
    /// an `avx512` dereference of register with optional masking. for example: `[edx]{k3}`
    RegDerefMasked(RegSpec, RegSpec),
    /// an `avx512` dereference of register plus offset, with optional masking. for example: `[esp + 0x40]{k3}`
    RegDispMasked(RegSpec, i32, RegSpec),
    /// an `avx512` dereference of a register scaled by 1, 2, 4, or 8, with optional masking. this
    /// seems extraordinarily unlikely to occur in practice. for example: `[esi * 4]{k2}`
    RegScaleMasked(RegSpec, u8, RegSpec),
    /// an `avx512` dereference of a register plus index scaled by 1, 2, 4, or 8, with optional masking.
    /// for example: `[esi + eax * 4]{k6}`
    RegIndexBaseMasked(RegSpec, RegSpec, RegSpec),
    /// an `avx512` dereference of a register plus offset, with optional masking.  for example:
    /// `[esi + eax + 0x1313]{k6}`
    RegIndexBaseDispMasked(RegSpec, RegSpec, i32, RegSpec),
    /// an `avx512` dereference of a register scaled by 1, 2, 4, or 8 plus offset, with optional
    /// masking. this seems extraordinarily unlikely to occur in practice. for example: `[esi *
    /// 4 + 0x1357]{k2}`
    RegScaleDispMasked(RegSpec, u8, i32, RegSpec),
    /// an `avx512` dereference of a register plus index scaled by 1, 2, 4, or 8, with optional
    /// masking.  for example: `[esi + eax * 4]{k6}`
    RegIndexBaseScaleMasked(RegSpec, RegSpec, u8, RegSpec),
    /// an `avx512` dereference of a register plus index scaled by 1, 2, 4, or 8 and offset, with
    /// optional masking.  for example: `[esi + eax * 4 + 0x1313]{k6}`
    RegIndexBaseScaleDispMasked(RegSpec, RegSpec, u8, i32, RegSpec),
    /// no operand. it is a bug for `yaxpeax-x86` to construct an `Operand` of this kind for public
    /// use; the instruction's `operand_count` should be reduced so as to make this invisible to
    /// library clients.
    Nothing,
}

impl Operand {
    /// returns `true` if this operand implies a memory access, `false` otherwise.
    ///
    /// notably, the `lea` instruction uses a memory operand without actually ever accessing
    /// memory.
    pub fn is_memory(&self) -> bool {
        match self {
            Operand::DisplacementU16(_)
            | Operand::DisplacementU32(_)
            | Operand::DisplacementU64(_)
            | Operand::RegDeref(_)
            | Operand::RegDisp(_, _)
            | Operand::RegScale(_, _)
            | Operand::RegIndexBase(_, _)
            | Operand::RegIndexBaseDisp(_, _, _)
            | Operand::RegScaleDisp(_, _, _)
            | Operand::RegIndexBaseScale(_, _, _)
            | Operand::RegIndexBaseScaleDisp(_, _, _, _)
            | Operand::RegDerefMasked(_, _)
            | Operand::RegDispMasked(_, _, _)
            | Operand::RegScaleMasked(_, _, _)
            | Operand::RegIndexBaseMasked(_, _, _)
            | Operand::RegIndexBaseDispMasked(_, _, _, _)
            | Operand::RegScaleDispMasked(_, _, _, _)
            | Operand::RegIndexBaseScaleMasked(_, _, _, _)
            | Operand::RegIndexBaseScaleDispMasked(_, _, _, _, _) => true,
            Operand::ImmediateI8(_)
            | Operand::ImmediateU8(_)
            | Operand::ImmediateI16(_)
            | Operand::ImmediateU16(_)
            | Operand::ImmediateU32(_)
            | Operand::ImmediateI32(_)
            | Operand::ImmediateU64(_)
            | Operand::ImmediateI64(_)
            | Operand::Register(_)
            | Operand::RegisterMaskMerge(_, _, _)
            | Operand::RegisterMaskMergeSae(_, _, _, _)
            | Operand::RegisterMaskMergeSaeNoround(_, _, _)
            | Operand::Nothing => false,
        }
    }

    /// return the width of this operand, in bytes. register widths are determined by the
    /// register's class. the widths of memory operands are recorded on the instruction this
    /// `Operand` came from; `None` here means the authoritative width is `instr.mem_size()`.
    pub fn width(&self) -> Option<u8> {
        match self {
            Operand::Register(reg) => Some(reg.width()),
            Operand::RegisterMaskMerge(reg, _, _) => Some(reg.width()),
            Operand::ImmediateI8(_) | Operand::ImmediateU8(_) => Some(1),
            Operand::ImmediateI16(_) | Operand::ImmediateU16(_) => Some(2),
            Operand::ImmediateI32(_) | Operand::ImmediateU32(_) => Some(4),
            Operand::ImmediateI64(_) | Operand::ImmediateU64(_) => Some(8),
            // memory operands or `Nothing`
            _ => None,
        }
    }
}

/// the condition for a conditional instruction.
///
/// these are only obtained through [`Opcode::condition()`]:
/// ```
/// use yaxpeax_x86::{Opcode, ConditionCode};
///
/// assert_eq!(Opcode::JB.condition(), Some(ConditionCode::B));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConditionCode {
    O,
    NO,
    B,
    AE,
    Z,
    NZ,
    A,
    BE,
    S,
    NS,
    P,
    NP,
    L,
    GE,
    G,
    LE,
}

/// an x86 opcode. there sure are a lot of these.
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Opcode {
    Invalid,
    ADD,
    OR,
    ADC,
    SBB,
    AND,
    XOR,
    SUB,
    CMP,
    XADD,
    BT,
    BTS,
    BTC,
    BTR,
    BSF,
    BSR,
    TZCNT,
    MOVSS,
    ADDSS,
    SUBSS,
    MULSS,
    DIVSS,
    MINSS,
    MAXSS,
    SQRTSS,
    MOVSD,
    SQRTSD,
    ADDSD,
    SUBSD,
    MULSD,
    DIVSD,
    MINSD,
    MAXSD,
    MOVSLDUP,
    MOVSHDUP,
    MOVDDUP,
    HADDPS,
    HSUBPS,
    ADDSUBPD,
    ADDSUBPS,
    CVTSI2SS,
    CVTSI2SD,
    CVTTSD2SI,
    CVTTPS2DQ,
    CVTPD2DQ,
    CVTPD2PS,
    CVTPS2DQ,
    CVTSD2SI,
    CVTSD2SS,
    CVTTSS2SI,
    CVTSS2SI,
    CVTSS2SD,
    CVTDQ2PD,
    LDDQU,
    MOVZX,
    MOVSX,
    MOVSXD,
    SAR,
    SAL,
    SHR,
    SHRD,
    SHL,
    RCR,
    RCL,
    ROR,
    ROL,
    INC,
    DEC,
    HLT,
    CALL,
    CALLF,
    JMP,
    JMPF,
    PUSH,
    POP,
    LEA,
    NOP,
    PREFETCHNTA,
    PREFETCH0,
    PREFETCH1,
    PREFETCH2,
    XCHG,
    POPF,
    INT,
    INTO,
    IRET,
    IRETD,
    IRETQ,
    RETF,
    ENTER,
    LEAVE,
    MOV,
    RETURN,
    PUSHF,
    WAIT,
    CBW,
    CWDE,
    CDQE,
    CWD,
    CDQ,
    CQO,
    LODS,
    STOS,
    LAHF,
    SAHF,
    CMPS,
    SCAS,
    MOVS,
    TEST,
    INS,
    IN,
    OUTS,
    OUT,
    IMUL,
    JO,
    JNO,
    JB,
    JNB,
    JZ,
    JNZ,
    JA,
    JNA,
    JS,
    JNS,
    JP,
    JNP,
    JL,
    JGE,
    JLE,
    JG,
    CMOVA,
    CMOVB,
    CMOVG,
    CMOVGE,
    CMOVL,
    CMOVLE,
    CMOVNA,
    CMOVNB,
    CMOVNO,
    CMOVNP,
    CMOVNS,
    CMOVNZ,
    CMOVO,
    CMOVP,
    CMOVS,
    CMOVZ,
    DIV,
    IDIV,
    MUL,
    NEG,
    NOT,
    CMPXCHG,
    SETO,
    SETNO,
    SETB,
    SETAE,
    SETZ,
    SETNZ,
    SETBE,
    SETA,
    SETS,
    SETNS,
    SETP,
    SETNP,
    SETL,
    SETGE,
    SETLE,
    SETG,
    CPUID,
    UD0,
    UD1,
    UD2,
    WBINVD,
    INVD,
    SYSRET,
    CLTS,
    SYSCALL,
    LSL,
    LAR,
    LES,
    LDS,
    SGDT,
    SIDT,
    LGDT,
    LIDT,
    SMSW,
    LMSW,
    SWAPGS,
    RDTSCP,
    INVLPG,
    FXSAVE,
    FXRSTOR,
    LDMXCSR,
    STMXCSR,
    XSAVE,
    XRSTOR,
    XSAVEOPT,
    LFENCE,
    MFENCE,
    SFENCE,
    CLFLUSH,
    CLFLUSHOPT,
    CLWB,
    WRMSR,
    RDTSC,
    RDMSR,
    RDPMC,
    SLDT,
    STR,
    LLDT,
    LTR,
    VERR,
    VERW,
    CMC,
    CLC,
    STC,
    CLI,
    STI,
    CLD,
    STD,
    JMPE,
    POPCNT,
    MOVDQU,
    MOVDQA,
    MOVQ,
    CMPSS,
    CMPSD,
    UNPCKLPS,
    UNPCKLPD,
    UNPCKHPS,
    UNPCKHPD,
    PSHUFHW,
    PSHUFLW,
    MOVUPS,
    MOVQ2DQ,
    MOVDQ2Q,
    RSQRTSS,
    RCPSS,

    ANDN,
    BEXTR,
    BLSI,
    BLSMSK,
    BLSR,
    VMCLEAR,
    VMXON,
    VMCALL,
    VMLAUNCH,
    VMRESUME,
    VMXOFF,
    PCONFIG,
    MONITOR,
    MWAIT,
    MONITORX,
    MWAITX,
    CLAC,
    STAC,
    ENCLS,
    ENCLV,
    XGETBV,
    XSETBV,
    VMFUNC,
    XABORT,
    XBEGIN,
    XEND,
    XTEST,
    ENCLU,
    RDPKRU,
    WRPKRU,

    RDPRU,
    CLZERO,

    RDSEED,
    RDRAND,

    ADDPS,
    ADDPD,
    ANDNPS,
    ANDNPD,
    ANDPS,
    ANDPD,
    BSWAP,
    CMPPD,
    CMPPS,
    COMISD,
    COMISS,
    CVTDQ2PS,
    CVTPI2PS,
    CVTPI2PD,
    CVTPS2PD,
    CVTPS2PI,
    CVTPD2PI,
    CVTTPS2PI,
    CVTTPD2PI,
    CVTTPD2DQ,
    DIVPS,
    DIVPD,
    EMMS,
    GETSEC,
    LFS,
    LGS,
    LSS,
    MASKMOVQ,
    MASKMOVDQU,
    MAXPS,
    MAXPD,
    MINPS,
    MINPD,
    MOVAPS,
    MOVAPD,
    MOVD,
    MOVLPS,
    MOVLPD,
    MOVHPS,
    MOVHPD,
    MOVLHPS,
    MOVHLPS,
    MOVUPD,
    MOVMSKPS,
    MOVMSKPD,
    MOVNTI,
    MOVNTPS,
    MOVNTPD,
    EXTRQ,
    INSERTQ,
    MOVNTSS,
    MOVNTSD,
    MOVNTQ,
    MOVNTDQ,
    MULPS,
    MULPD,
    ORPS,
    ORPD,
    PACKSSDW,
    PACKSSWB,
    PACKUSWB,
    PADDB,
    PADDD,
    PADDQ,
    PADDSB,
    PADDSW,
    PADDUSB,
    PADDUSW,
    PADDW,
    PAND,
    PANDN,
    PAVGB,
    PAVGW,
    PCMPEQB,
    PCMPEQD,
    PCMPEQW,
    PCMPGTB,
    PCMPGTD,
    PCMPGTW,
    PINSRW,
    PMADDWD,
    PMAXSW,
    PMAXUB,
    PMINSW,
    PMINUB,
    PMOVMSKB,
    PMULHUW,
    PMULHW,
    PMULLW,
    PMULUDQ,
    POR,
    PSADBW,
    PSHUFW,
    PSHUFD,
    PSLLD,
    PSLLDQ,
    PSLLQ,
    PSLLW,
    PSRAD,
    PSRAW,
    PSRLD,
    PSRLDQ,
    PSRLQ,
    PSRLW,
    PSUBB,
    PSUBD,
    PSUBQ,
    PSUBSB,
    PSUBSW,
    PSUBUSB,
    PSUBUSW,
    PSUBW,
    PUNPCKHBW,
    PUNPCKHDQ,
    PUNPCKHWD,
    PUNPCKLBW,
    PUNPCKLDQ,
    PUNPCKLWD,
    PUNPCKLQDQ,
    PUNPCKHQDQ,
    PXOR,
    RCPPS,
    RSM,
    RSQRTPS,
    SHLD,
    SHUFPD,
    SHUFPS,
    SLHD,
    SQRTPS,
    SQRTPD,
    SUBPS,
    SUBPD,
    SYSENTER,
    SYSEXIT,
    UCOMISD,
    UCOMISS,
    VMREAD,
    VMWRITE,
    XORPS,
    XORPD,

    VMOVDDUP,
    VPSHUFLW,
    VPSHUFHW,
    VHADDPS,
    VHSUBPS,
    VADDSUBPS,
    VCVTPD2DQ,
    VLDDQU,

    VCOMISD,
    VCOMISS,
    VUCOMISD,
    VUCOMISS,
    VADDPD,
    VADDPS,
    VADDSD,
    VADDSS,
    VADDSUBPD,
    VAESDEC,
    VAESDECLAST,
    VAESENC,
    VAESENCLAST,
    VAESIMC,
    VAESKEYGENASSIST,
    VBLENDPD,
    VBLENDPS,
    VBLENDVPD,
    VBLENDVPS,
    VBROADCASTF128,
    VBROADCASTI128,
    VBROADCASTSD,
    VBROADCASTSS,
    VCMPSD,
    VCMPSS,
    VCMPPD,
    VCMPPS,
    VCVTDQ2PD,
    VCVTDQ2PS,
    VCVTPD2PS,
    VCVTPH2PS,
    VCVTPS2DQ,
    VCVTPS2PD,
    VCVTSS2SD,
    VCVTSI2SS,
    VCVTSI2SD,
    VCVTSD2SI,
    VCVTSD2SS,
    VCVTPS2PH,
    VCVTSS2SI,
    VCVTTPD2DQ,
    VCVTTPS2DQ,
    VCVTTSS2SI,
    VCVTTSD2SI,
    VDIVPD,
    VDIVPS,
    VDIVSD,
    VDIVSS,
    VDPPD,
    VDPPS,
    VEXTRACTF128,
    VEXTRACTI128,
    VEXTRACTPS,
    VFMADD132PD,
    VFMADD132PS,
    VFMADD132SD,
    VFMADD132SS,
    VFMADD213PD,
    VFMADD213PS,
    VFMADD213SD,
    VFMADD213SS,
    VFMADD231PD,
    VFMADD231PS,
    VFMADD231SD,
    VFMADD231SS,
    VFMADDSUB132PD,
    VFMADDSUB132PS,
    VFMADDSUB213PD,
    VFMADDSUB213PS,
    VFMADDSUB231PD,
    VFMADDSUB231PS,
    VFMSUB132PD,
    VFMSUB132PS,
    VFMSUB132SD,
    VFMSUB132SS,
    VFMSUB213PD,
    VFMSUB213PS,
    VFMSUB213SD,
    VFMSUB213SS,
    VFMSUB231PD,
    VFMSUB231PS,
    VFMSUB231SD,
    VFMSUB231SS,
    VFMSUBADD132PD,
    VFMSUBADD132PS,
    VFMSUBADD213PD,
    VFMSUBADD213PS,
    VFMSUBADD231PD,
    VFMSUBADD231PS,
    VFNMADD132PD,
    VFNMADD132PS,
    VFNMADD132SD,
    VFNMADD132SS,
    VFNMADD213PD,
    VFNMADD213PS,
    VFNMADD213SD,
    VFNMADD213SS,
    VFNMADD231PD,
    VFNMADD231PS,
    VFNMADD231SD,
    VFNMADD231SS,
    VFNMSUB132PD,
    VFNMSUB132PS,
    VFNMSUB132SD,
    VFNMSUB132SS,
    VFNMSUB213PD,
    VFNMSUB213PS,
    VFNMSUB213SD,
    VFNMSUB213SS,
    VFNMSUB231PD,
    VFNMSUB231PS,
    VFNMSUB231SD,
    VFNMSUB231SS,
    VGATHERDPD,
    VGATHERDPS,
    VGATHERQPD,
    VGATHERQPS,
    VHADDPD,
    VHSUBPD,
    VINSERTF128,
    VINSERTI128,
    VINSERTPS,
    VMASKMOVDQU,
    VMASKMOVPD,
    VMASKMOVPS,
    VMAXPD,
    VMAXPS,
    VMAXSD,
    VMAXSS,
    VMINPD,
    VMINPS,
    VMINSD,
    VMINSS,
    VMOVAPD,
    VMOVAPS,
    VMOVD,
    VMOVDQA,
    VMOVDQU,
    VMOVHLPS,
    VMOVHPD,
    VMOVHPS,
    VMOVLHPS,
    VMOVLPD,
    VMOVLPS,
    VMOVMSKPD,
    VMOVMSKPS,
    VMOVNTDQ,
    VMOVNTDQA,
    VMOVNTPD,
    VMOVNTPS,
    VMOVQ,
    VMOVSS,
    VMOVSD,
    VMOVSHDUP,
    VMOVSLDUP,
    VMOVUPD,
    VMOVUPS,
    VMPSADBW,
    VMULPD,
    VMULPS,
    VMULSD,
    VMULSS,
    VPABSB,
    VPABSD,
    VPABSW,
    VPACKSSDW,
    VPACKUSDW,
    VPACKSSWB,
    VPACKUSWB,
    VPADDB,
    VPADDD,
    VPADDQ,
    VPADDSB,
    VPADDSW,
    VPADDUSB,
    VPADDUSW,
    VPADDW,
    VPALIGNR,
    VANDPD,
    VANDPS,
    VORPD,
    VORPS,
    VANDNPD,
    VANDNPS,
    VPAND,
    VPANDN,
    VPAVGB,
    VPAVGW,
    VPBLENDD,
    VPBLENDVB,
    VPBLENDW,
    VPBROADCASTB,
    VPBROADCASTD,
    VPBROADCASTQ,
    VPBROADCASTW,
    VPCLMULQDQ,
    VPCMPEQB,
    VPCMPEQD,
    VPCMPEQQ,
    VPCMPEQW,
    VPCMPGTB,
    VPCMPGTD,
    VPCMPGTQ,
    VPCMPGTW,
    VPCMPESTRI,
    VPCMPESTRM,
    VPCMPISTRI,
    VPCMPISTRM,
    VPERM2F128,
    VPERM2I128,
    VPERMD,
    VPERMILPD,
    VPERMILPS,
    VPERMPD,
    VPERMPS,
    VPERMQ,
    VPEXTRB,
    VPEXTRD,
    VPEXTRQ,
    VPEXTRW,
    VPGATHERDD,
    VPGATHERDQ,
    VPGATHERQD,
    VPGATHERQQ,
    VPHADDD,
    VPHADDSW,
    VPHADDW,
    VPMADDUBSW,
    VPHMINPOSUW,
    VPHSUBD,
    VPHSUBSW,
    VPHSUBW,
    VPINSRB,
    VPINSRD,
    VPINSRQ,
    VPINSRW,
    VPMADDWD,
    VPMASKMOVD,
    VPMASKMOVQ,
    VPMAXSB,
    VPMAXSD,
    VPMAXSW,
    VPMAXUB,
    VPMAXUW,
    VPMAXUD,
    VPMINSB,
    VPMINSW,
    VPMINSD,
    VPMINUB,
    VPMINUW,
    VPMINUD,
    VPMOVMSKB,
    VPMOVSXBD,
    VPMOVSXBQ,
    VPMOVSXBW,
    VPMOVSXDQ,
    VPMOVSXWD,
    VPMOVSXWQ,
    VPMOVZXBD,
    VPMOVZXBQ,
    VPMOVZXBW,
    VPMOVZXDQ,
    VPMOVZXWD,
    VPMOVZXWQ,
    VPMULDQ,
    VPMULHRSW,
    VPMULHUW,
    VPMULHW,
    VPMULLQ,
    VPMULLD,
    VPMULLW,
    VPMULUDQ,
    VPOR,
    VPSADBW,
    VPSHUFB,
    VPSHUFD,
    VPSIGNB,
    VPSIGND,
    VPSIGNW,
    VPSLLD,
    VPSLLDQ,
    VPSLLQ,
    VPSLLVD,
    VPSLLVQ,
    VPSLLW,
    VPSRAD,
    VPSRAVD,
    VPSRAW,
    VPSRLD,
    VPSRLDQ,
    VPSRLQ,
    VPSRLVD,
    VPSRLVQ,
    VPSRLW,
    VPSUBB,
    VPSUBD,
    VPSUBQ,
    VPSUBSB,
    VPSUBSW,
    VPSUBUSB,
    VPSUBUSW,
    VPSUBW,
    VPTEST,
    VPUNPCKHBW,
    VPUNPCKHDQ,
    VPUNPCKHQDQ,
    VPUNPCKHWD,
    VPUNPCKLBW,
    VPUNPCKLDQ,
    VPUNPCKLQDQ,
    VPUNPCKLWD,
    VPXOR,
    VRCPPS,
    VROUNDPD,
    VROUNDPS,
    VROUNDSD,
    VROUNDSS,
    VRSQRTPS,
    VRSQRTSS,
    VRCPSS,
    VSHUFPD,
    VSHUFPS,
    VSQRTPD,
    VSQRTPS,
    VSQRTSS,
    VSQRTSD,
    VSUBPD,
    VSUBPS,
    VSUBSD,
    VSUBSS,
    VTESTPD,
    VTESTPS,
    VUNPCKHPD,
    VUNPCKHPS,
    VUNPCKLPD,
    VUNPCKLPS,
    VXORPD,
    VXORPS,
    VZEROUPPER,
    VZEROALL,
    VLDMXCSR,
    VSTMXCSR,

    PCLMULQDQ,
    AESKEYGENASSIST,
    AESIMC,
    AESENC,
    AESENCLAST,
    AESDEC,
    AESDECLAST,
    PCMPGTQ,
    PCMPISTRM,
    PCMPISTRI,
    PCMPESTRI,
    PACKUSDW,
    PCMPESTRM,
    PCMPEQQ,
    PTEST,
    PHMINPOSUW,
    DPPS,
    DPPD,
    MPSADBW,
    PMOVZXDQ,
    PMOVSXDQ,
    PMOVZXBD,
    PMOVSXBD,
    PMOVZXWQ,
    PMOVSXWQ,
    PMOVZXBQ,
    PMOVSXBQ,
    PMOVSXWD,
    PMOVZXWD,
    PEXTRQ,
    PEXTRD,
    PEXTRW,
    PEXTRB,
    PMOVSXBW,
    PMOVZXBW,
    PINSRQ,
    PINSRD,
    PINSRB,
    EXTRACTPS,
    INSERTPS,
    ROUNDSS,
    ROUNDSD,
    ROUNDPS,
    ROUNDPD,
    PMAXSB,
    PMAXSD,
    PMAXUW,
    PMAXUD,
    PMINSD,
    PMINSB,
    PMINUD,
    PMINUW,
    BLENDW,
    PBLENDVB,
    PBLENDW,
    BLENDVPS,
    BLENDVPD,
    BLENDPS,
    BLENDPD,
    PMULDQ,
    MOVNTDQA,
    PMULLD,
    PALIGNR,
    PSIGNW,
    PSIGND,
    PSIGNB,
    PSHUFB,
    PMULHRSW,
    PMADDUBSW,
    PABSD,
    PABSW,
    PABSB,
    PHSUBSW,
    PHSUBW,
    PHSUBD,
    PHADDD,
    PHADDSW,
    PHADDW,
    HSUBPD,
    HADDPD,

    SHA1RNDS4,
    SHA1NEXTE,
    SHA1MSG1,
    SHA1MSG2,
    SHA256RNDS2,
    SHA256MSG1,
    SHA256MSG2,

    LZCNT,
    CLGI,
    STGI,
    SKINIT,
    VMLOAD,
    VMMCALL,
    VMSAVE,
    VMRUN,
    INVLPGA,
    INVLPGB,
    TLBSYNC,

    MOVBE,

    ADCX,
    ADOX,

    PREFETCHW,

    RDPID,
    CMPXCHG8B,
    CMPXCHG16B,
    VMPTRLD,
    VMPTRST,

    BZHI,
    MULX,
    SHLX,
    SHRX,
    SARX,
    PDEP,
    PEXT,
    RORX,
    XRSTORS,
    XRSTORS64,
    XSAVEC,
    XSAVEC64,
    XSAVES,
    XSAVES64,

    RDFSBASE,
    RDGSBASE,
    WRFSBASE,
    WRGSBASE,

    CRC32,
    SALC,
    XLAT,

    F2XM1,
    FABS,
    FADD,
    FADDP,
    FBLD,
    FBSTP,
    FCHS,
    FCMOVB,
    FCMOVBE,
    FCMOVE,
    FCMOVNB,
    FCMOVNBE,
    FCMOVNE,
    FCMOVNU,
    FCMOVU,
    FCOM,
    FCOMI,
    FCOMIP,
    FCOMP,
    FCOMPP,
    FCOS,
    FDECSTP,
    FDISI8087_NOP,
    FDIV,
    FDIVP,
    FDIVR,
    FDIVRP,
    FENI8087_NOP,
    FFREE,
    FFREEP,
    FIADD,
    FICOM,
    FICOMP,
    FIDIV,
    FIDIVR,
    FILD,
    FIMUL,
    FINCSTP,
    FIST,
    FISTP,
    FISTTP,
    FISUB,
    FISUBR,
    FLD,
    FLD1,
    FLDCW,
    FLDENV,
    FLDL2E,
    FLDL2T,
    FLDLG2,
    FLDLN2,
    FLDPI,
    FLDZ,
    FMUL,
    FMULP,
    FNCLEX,
    FNINIT,
    FNOP,
    FNSAVE,
    FNSTCW,
    FNSTENV,
    FNSTOR,
    FNSTSW,
    FPATAN,
    FPREM,
    FPREM1,
    FPTAN,
    FRNDINT,
    FRSTOR,
    FSCALE,
    FSETPM287_NOP,
    FSIN,
    FSINCOS,
    FSQRT,
    FST,
    FSTP,
    FSTPNCE,
    FSUB,
    FSUBP,
    FSUBR,
    FSUBRP,
    FTST,
    FUCOM,
    FUCOMI,
    FUCOMIP,
    FUCOMP,
    FUCOMPP,
    FXAM,
    FXCH,
    FXTRACT,
    FYL2X,
    FYL2XP1,

    LOOPNZ,
    LOOPZ,
    LOOP,
    JRCXZ,
    JECXZ,
    JCXZ,

    PUSHA,
    POPA,
    BOUND,
    ARPL,
    AAS,
    AAA,
    DAS,
    DAA,
    AAM,
    AAD,

    // started shipping in Tremont, 2020 sept 23
    MOVDIR64B,
    MOVDIRI,

    // started shipping in Tiger Lake, 2020 sept 2
    AESDEC128KL,
    AESDEC256KL,
    AESDECWIDE128KL,
    AESDECWIDE256KL,
    AESENC128KL,
    AESENC256KL,
    AESENCWIDE128KL,
    AESENCWIDE256KL,
    ENCODEKEY128,
    ENCODEKEY256,
    LOADIWKEY,

    // unsure
    HRESET,

    // 3dnow
    FEMMS,
    PI2FW,
    PI2FD,
    PF2IW,
    PF2ID,
    PMULHRW,
    PFCMPGE,
    PFMIN,
    PFRCP,
    PFRSQRT,
    PFSUB,
    PFADD,
    PFCMPGT,
    PFMAX,
    PFRCPIT1,
    PFRSQIT1,
    PFSUBR,
    PFACC,
    PFCMPEQ,
    PFMUL,
    PFMULHRW,
    PFRCPIT2,
    PFNACC,
    PFPNACC,
    PSWAPD,
    PAVGUSB,

    // ENQCMD
    ENQCMD,
    ENQCMDS,

    // INVPCID
    INVEPT,
    INVVPID,
    INVPCID,

    // PTWRITE
    PTWRITE,

    // GFNI
    GF2P8AFFINEQB,
    GF2P8AFFINEINVQB,
    GF2P8MULB,

    // CET
    WRUSS,
    WRSS,
    INCSSP,
    SAVEPREVSSP,
    SETSSBSY,
    CLRSSBSY,
    RSTORSSP,
    ENDBR64,
    ENDBR32,

    // TDX
    TDCALL,
    SEAMRET,
    SEAMOPS,
    SEAMCALL,

    // WAITPKG
    TPAUSE,
    UMONITOR,
    UMWAIT,

    // UINTR
    UIRET,
    TESTUI,
    CLUI,
    STUI,
    SENDUIPI,

    // TSXLDTRK
    XSUSLDTRK,
    XRESLDTRK,

    // AVX512F
    VALIGND,
    VALIGNQ,
    VBLENDMPD,
    VBLENDMPS,
    VCOMPRESSPD,
    VCOMPRESSPS,
    VCVTPD2UDQ,
    VCVTTPD2UDQ,
    VCVTPS2UDQ,
    VCVTTPS2UDQ,
    VCVTQQ2PD,
    VCVTQQ2PS,
    VCVTSD2USI,
    VCVTTSD2USI,
    VCVTSS2USI,
    VCVTTSS2USI,
    VCVTUDQ2PD,
    VCVTUDQ2PS,
    VCVTUSI2USD,
    VCVTUSI2USS,
    VEXPANDPD,
    VEXPANDPS,
    VEXTRACTF32X4,
    VEXTRACTF64X4,
    VEXTRACTI32X4,
    VEXTRACTI64X4,
    VFIXUPIMMPD,
    VFIXUPIMMPS,
    VFIXUPIMMSD,
    VFIXUPIMMSS,
    VGETEXPPD,
    VGETEXPPS,
    VGETEXPSD,
    VGETEXPSS,
    VGETMANTPD,
    VGETMANTPS,
    VGETMANTSD,
    VGETMANTSS,
    VINSERTF32X4,
    VINSERTF64X4,
    VINSERTI64X4,
    VMOVDQA32,
    VMOVDQA64,
    VMOVDQU32,
    VMOVDQU64,
    VPBLENDMD,
    VPBLENDMQ,
    VPCMPD,
    VPCMPUD,
    VPCMPQ,
    VPCMPUQ,
    VPCOMPRESSQ,
    VPCOMPRESSD,
    VPERMI2D,
    VPERMI2Q,
    VPERMI2PD,
    VPERMI2PS,
    VPERMT2D,
    VPERMT2Q,
    VPERMT2PD,
    VPERMT2PS,
    VPMAXSQ,
    VPMAXUQ,
    VPMINSQ,
    VPMINUQ,
    VPMOVSQB,
    VPMOVUSQB,
    VPMOVSQW,
    VPMOVUSQW,
    VPMOVSQD,
    VPMOVUSQD,
    VPMOVSDB,
    VPMOVUSDB,
    VPMOVSDW,
    VPMOVUSDW,
    VPROLD,
    VPROLQ,
    VPROLVD,
    VPROLVQ,
    VPRORD,
    VPRORQ,
    VPRORRD,
    VPRORRQ,
    VPSCATTERDD,
    VPSCATTERDQ,
    VPSCATTERQD,
    VPSCATTERQQ,
    VPSRAQ,
    VPSRAVQ,
    VPTESTNMD,
    VPTESTNMQ,
    VPTERNLOGD,
    VPTERNLOGQ,
    VPTESTMD,
    VPTESTMQ,
    VRCP14PD,
    VRCP14PS,
    VRCP14SD,
    VRCP14SS,
    VRNDSCALEPD,
    VRNDSCALEPS,
    VRNDSCALESD,
    VRNDSCALESS,
    VRSQRT14PD,
    VRSQRT14PS,
    VRSQRT14SD,
    VRSQRT14SS,
    VSCALEDPD,
    VSCALEDPS,
    VSCALEDSD,
    VSCALEDSS,
    VSCATTERDD,
    VSCATTERDQ,
    VSCATTERQD,
    VSCATTERQQ,
    VSHUFF32X4,
    VSHUFF64X2,
    VSHUFI32X4,
    VSHUFI64X2,

    // AVX512DQ
    VCVTTPD2QQ,
    VCVTPD2QQ,
    VCVTTPD2UQQ,
    VCVTPD2UQQ,
    VCVTTPS2QQ,
    VCVTPS2QQ,
    VCVTTPS2UQQ,
    VCVTPS2UQQ,
    VCVTUQQ2PD,
    VCVTUQQ2PS,
    VEXTRACTF64X2,
    VEXTRACTI64X2,
    VFPCLASSPD,
    VFPCLASSPS,
    VFPCLASSSD,
    VFPCLASSSS,
    VINSERTF64X2,
    VINSERTI64X2,
    VPMOVM2D,
    VPMOVM2Q,
    VPMOVB2D,
    VPMOVQ2M,
    VRANGEPD,
    VRANGEPS,
    VRANGESD,
    VRANGESS,
    VREDUCEPD,
    VREDUCEPS,
    VREDUCESD,
    VREDUCESS,

    // AVX512BW
    VDBPSADBW,
    VMOVDQU8,
    VMOVDQU16,
    VPBLENDMB,
    VPBLENDMW,
    VPCMPB,
    VPCMPUB,
    VPCMPW,
    VPCMPUW,
    VPERMW,
    VPERMI2B,
    VPERMI2W,
    VPMOVM2B,
    VPMOVM2W,
    VPMOVB2M,
    VPMOVW2M,
    VPMOVSWB,
    VPMOVUSWB,
    VPSLLVW,
    VPSRAVW,
    VPSRLVW,
    VPTESTNMB,
    VPTESTNMW,
    VPTESTMB,
    VPTESTMW,

    // AVX512CD
    VPBROADCASTM,
    VPCONFLICTD,
    VPCONFLICTQ,
    VPLZCNTD,
    VPLZCNTQ,

    KUNPCKBW,
    KUNPCKWD,
    KUNPCKDQ,

    KADDB,
    KANDB,
    KANDNB,
    KMOVB,
    KNOTB,
    KORB,
    KORTESTB,
    KSHIFTLB,
    KSHIFTRB,
    KTESTB,
    KXNORB,
    KXORB,
    KADDW,
    KANDW,
    KANDNW,
    KMOVW,
    KNOTW,
    KORW,
    KORTESTW,
    KSHIFTLW,
    KSHIFTRW,
    KTESTW,
    KXNORW,
    KXORW,
    KADDD,
    KANDD,
    KANDND,
    KMOVD,
    KNOTD,
    KORD,
    KORTESTD,
    KSHIFTLD,
    KSHIFTRD,
    KTESTD,
    KXNORD,
    KXORD,
    KADDQ,
    KANDQ,
    KANDNQ,
    KMOVQ,
    KNOTQ,
    KORQ,
    KORTESTQ,
    KSHIFTLQ,
    KSHIFTRQ,
    KTESTQ,
    KXNORQ,
    KXORQ,

    // AVX512ER
    VEXP2PD,
    VEXP2PS,
    VEXP2SD,
    VEXP2SS,
    VRCP28PD,
    VRCP28PS,
    VRCP28SD,
    VRCP28SS,
    VRSQRT28PD,
    VRSQRT28PS,
    VRSQRT28SD,
    VRSQRT28SS,

    // AVX512PF
    VGATHERPF0DPD,
    VGATHERPF0DPS,
    VGATHERPF0QPD,
    VGATHERPF0QPS,
    VGATHERPF1DPD,
    VGATHERPF1DPS,
    VGATHERPF1QPD,
    VGATHERPF1QPS,
    VSCATTERPF0DPD,
    VSCATTERPF0DPS,
    VSCATTERPF0QPD,
    VSCATTERPF0QPS,
    VSCATTERPF1DPD,
    VSCATTERPF1DPS,
    VSCATTERPF1QPD,
    VSCATTERPF1QPS,

    // MPX
    BNDMK,
    BNDCL,
    BNDCU,
    BNDCN,
    BNDMOV,
    BNDLDX,
    BNDSTX,

    VGF2P8AFFINEQB,
    VGF2P8AFFINEINVQB,
    VPSHRDQ,
    VPSHRDD,
    VPSHRDW,
    VPSHLDQ,
    VPSHLDD,
    VPSHLDW,
    VBROADCASTF32X8,
    VBROADCASTF64X4,
    VBROADCASTF32X4,
    VBROADCASTF64X2,
    VBROADCASTF32X2,
    VBROADCASTI32X8,
    VBROADCASTI64X4,
    VBROADCASTI32X4,
    VBROADCASTI64X2,
    VBROADCASTI32X2,
    VEXTRACTI32X8,
    VEXTRACTF32X8,
    VINSERTI32X8,
    VINSERTF32X8,
    VINSERTI32X4,
    V4FNMADDSS,
    V4FNMADDPS,
    VCVTNEPS2BF16,
    V4FMADDSS,
    V4FMADDPS,
    VCVTNE2PS2BF16,
    VP2INTERSECTD,
    VP2INTERSECTQ,
    VP4DPWSSDS,
    VP4DPWSSD,
    VPDPWSSDS,
    VPDPWSSD,
    VPDPBUSDS,
    VDPBF16PS,
    VPBROADCASTMW2D,
    VPBROADCASTMB2Q,
    VPMOVD2M,
    VPMOVQD,
    VPMOVWB,
    VPMOVDB,
    VPMOVDW,
    VPMOVQB,
    VPMOVQW,
    VGF2P8MULB,
    VPMADD52HUQ,
    VPMADD52LUQ,
    VPSHUFBITQMB,
    VPERMB,
    VPEXPANDD,
    VPEXPANDQ,
    VPABSQ,
    VPRORVD,
    VPRORVQ,
    VPMULTISHIFTQB,
    VPERMT2B,
    VPERMT2W,
    VPSHRDVQ,
    VPSHRDVD,
    VPSHRDVW,
    VPSHLDVQ,
    VPSHLDVD,
    VPSHLDVW,
    VPCOMPRESSB,
    VPCOMPRESSW,
    VPEXPANDB,
    VPEXPANDW,
    VPOPCNTD,
    VPOPCNTQ,
    VPOPCNTB,
    VPOPCNTW,
    VSCALEFSS,
    VSCALEFSD,
    VSCALEFPS,
    VSCALEFPD,
    VPDPBUSD,
    VCVTUSI2SD,
    VCVTUSI2SS,
    VPXORD,
    VPXORQ,
    VPORD,
    VPORQ,
    VPANDND,
    VPANDNQ,
    VPANDD,
    VPANDQ,

    PSMASH,
    PVALIDATE,
    RMPADJUST,
    RMPUPDATE,
}

impl Opcode {
    /// get the [`ConditionCode`] for this instruction, if it is in fact conditional. x86's
    /// conditional instructions are `Jcc`, `CMOVcc`, andd `SETcc`.
    pub fn condition(&self) -> Option<ConditionCode> {
        match self {
            Opcode::JO | Opcode::CMOVO | Opcode::SETO => Some(ConditionCode::O),
            Opcode::JNO | Opcode::CMOVNO | Opcode::SETNO => Some(ConditionCode::NO),
            Opcode::JB | Opcode::CMOVB | Opcode::SETB => Some(ConditionCode::B),
            Opcode::JNB | Opcode::CMOVNB | Opcode::SETAE => Some(ConditionCode::AE),
            Opcode::JZ | Opcode::CMOVZ | Opcode::SETZ => Some(ConditionCode::Z),
            Opcode::JNZ | Opcode::CMOVNZ | Opcode::SETNZ => Some(ConditionCode::NZ),
            Opcode::JA | Opcode::CMOVA | Opcode::SETA => Some(ConditionCode::A),
            Opcode::JNA | Opcode::CMOVNA | Opcode::SETBE => Some(ConditionCode::BE),
            Opcode::JS | Opcode::CMOVS | Opcode::SETS => Some(ConditionCode::S),
            Opcode::JNS | Opcode::CMOVNS | Opcode::SETNS => Some(ConditionCode::NS),
            Opcode::JP | Opcode::CMOVP | Opcode::SETP => Some(ConditionCode::P),
            Opcode::JNP | Opcode::CMOVNP | Opcode::SETNP => Some(ConditionCode::NP),
            Opcode::JL | Opcode::CMOVL | Opcode::SETL => Some(ConditionCode::L),
            Opcode::JGE | Opcode::CMOVGE | Opcode::SETGE => Some(ConditionCode::GE),
            Opcode::JG | Opcode::CMOVG | Opcode::SETG => Some(ConditionCode::G),
            Opcode::JLE | Opcode::CMOVLE | Opcode::SETLE => Some(ConditionCode::LE),
            _ => None,
        }
    }
}
