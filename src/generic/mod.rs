mod display;

//#[cfg(feature = "fmt")]
//mod display;

use crate::MemoryAccessSize;

use core::cmp::PartialEq;
use crate::safer_unchecked::unreachable_kinda_unchecked as unreachable_unchecked;
pub use crate::generated::opcode::Opcode as Opcode;

use yaxpeax_arch::{AddressDiff, Decoder, Reader, LengthedInstruction};
use yaxpeax_arch::annotation::{AnnotatingDecoder, DescriptionSink, NullSink};
use yaxpeax_arch::{DecodeError as ArchDecodeError};

use core::fmt;
impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.description())
    }
}

use core::hash::Hash;
use core::hash::Hasher;
impl Hash for RegSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let code = ((self.bank as u16) << 8) | (self.num as u16);
        code.hash(state);
    }
}

/// the condition for a conditional instruction.
///
/// these are only obtained through [`Opcode::condition()`]:
/// ```
/// use yaxpeax_x86::long_mode::{Opcode, ConditionCode};
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

#[allow(non_snake_case)]
impl RegSpec {
    /// the register `rip`. this register is in the class `rip`, which contains only it.
    pub const RIP: RegSpec = RegSpec::rip();

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
            bank: RegisterBank::ST
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
            bank: RegisterBank::X
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
            bank: RegisterBank::Y
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
            bank: RegisterBank::Z
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
            bank: RegisterBank::Q
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
            bank: RegisterBank::K
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
            bank: RegisterBank::D
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
            bank: RegisterBank::W
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
            bank: RegisterBank::rB
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
            bank: RegisterBank::B
        }
    }

    #[inline]
    fn from_parts(num: u8, extended: bool, bank: RegisterBank) -> RegSpec {
        RegSpec {
            num: num + if extended { 0b1000 } else { 0 },
            bank: bank
        }
    }

    #[inline]
    fn gp_from_parts(num: u8, extended: bool, width: u8, rex: bool) -> RegSpec {
        RegSpec {
            num: num + if extended { 0b1000 } else { 0 },
            bank: width_to_gp_reg_bank(width, rex)
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
        RegSpec { bank: RegisterBank::Z, num: 0 }
    }

    #[inline]
    pub const fn ymm0() -> RegSpec {
        RegSpec { bank: RegisterBank::Y, num: 0 }
    }

    #[inline]
    pub const fn xmm0() -> RegSpec {
        RegSpec { bank: RegisterBank::X, num: 0 }
    }

    #[inline]
    pub const fn st0() -> RegSpec {
        RegSpec { bank: RegisterBank::ST, num: 0 }
    }

    #[inline]
    pub const fn mm0() -> RegSpec {
        RegSpec { bank: RegisterBank::MM, num: 0 }
    }

    /// return the size of this register, in bytes.
    #[inline]
    pub fn width(&self) -> u8 {
        self.class().width()
    }
}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
enum SizeCode {
    b,
    vd,
    vq,
    vqp
}

/// an operand for an `x86_64` instruction.
///
/// `Operand::Nothing` should be unreachable in practice; any such instructions should have an
/// operand count of 0 (or at least one fewer than the `Nothing` operand's position).
#[derive(Clone, Debug, PartialEq, Eq)]
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
    /// a memory access to a literal dword address. it's extremely rare that a well-formed x86
    /// instruction uses this mode. as an example, `[0x11335577]`
    DisplacementU32(u32),
    /// a memory access to a literal qword address. it's relatively rare that a well-formed x86
    /// instruction uses this mode, but plausibe. for example, `gs:[0x14]`. segment overrides,
    /// however, are maintained on the instruction itself.
    DisplacementU64(u64),
    /// a simple dereference of the address held in some register. for example: `[rsi]`.
    RegDeref(RegSpec),
    /// a dereference of the address held in some register with offset. for example: `[rsi + 0x14]`.
    RegDisp(RegSpec, i32),
    /// a dereference of the address held in some register scaled by 1, 2, 4, or 8. this is almost always used with the `lea` instruction. for example: `[rdx * 4]`.
    RegScale(RegSpec, u8),
    /// a dereference of the address from summing two registers. for example: `[rbp + rax]`
    RegIndexBase(RegSpec, RegSpec),
    /// a dereference of the address from summing two registers with offset. for example: `[rdi + rcx + 0x40]`
    RegIndexBaseDisp(RegSpec, RegSpec, i32),
    /// a dereference of the address held in some register scaled by 1, 2, 4, or 8 with offset. this is almost always used with the `lea` instruction. for example: `[rax * 4 + 0x30]`.
    RegScaleDisp(RegSpec, u8, i32),
    /// a dereference of the address from summing a register and index register scaled by 1, 2, 4,
    /// or 8. for
    /// example: `[rsi + rcx * 4]`
    RegIndexBaseScale(RegSpec, RegSpec, u8),
    /// a dereference of the address from summing a register and index register scaled by 1, 2, 4,
    /// or 8, with offset. for
    /// example: `[rsi + rcx * 4 + 0x1234]`
    RegIndexBaseScaleDisp(RegSpec, RegSpec, u8, i32),
    /// an `avx512` dereference of register with optional masking. for example: `[rdx]{k3}`
    RegDerefMasked(RegSpec, RegSpec),
    /// an `avx512` dereference of register plus offset, with optional masking. for example: `[rsp + 0x40]{k3}`
    RegDispMasked(RegSpec, i32, RegSpec),
    /// an `avx512` dereference of a register scaled by 1, 2, 4, or 8, with optional masking. this
    /// seems extraordinarily unlikely to occur in practice. for example: `[rsi * 4]{k2}`
    RegScaleMasked(RegSpec, u8, RegSpec),
    /// an `avx512` dereference of a register plus index scaled by 1, 2, 4, or 8, with optional masking.
    /// for example: `[rsi + rax * 4]{k6}`
    RegIndexBaseMasked(RegSpec, RegSpec, RegSpec),
    /// an `avx512` dereference of a register plus offset, with optional masking.  for example:
    /// `[rsi + rax + 0x1313]{k6}`
    RegIndexBaseDispMasked(RegSpec, RegSpec, i32, RegSpec),
    /// an `avx512` dereference of a register scaled by 1, 2, 4, or 8 plus offset, with optional
    /// masking. this seems extraordinarily unlikely to occur in practice. for example: `[rsi *
    /// 4 + 0x1357]{k2}`
    RegScaleDispMasked(RegSpec, u8, i32, RegSpec),
    /// an `avx512` dereference of a register plus index scaled by 1, 2, 4, or 8, with optional
    /// masking.  for example: `[rsi + rax * 4]{k6}`
    RegIndexBaseScaleMasked(RegSpec, RegSpec, u8, RegSpec),
    /// an `avx512` dereference of a register plus index scaled by 1, 2, 4, or 8 and offset, with
    /// optional masking.  for example: `[rsi + rax * 4 + 0x1313]{k6}`
    RegIndexBaseScaleDispMasked(RegSpec, RegSpec, u8, i32, RegSpec),
    /// no operand. it is a bug for `yaxpeax-x86` to construct an `Operand` of this kind for public
    /// use; the instruction's `operand_count` should be reduced so as to make this invisible to
    /// library clients.
    Nothing,
}

impl OperandSpec {
    fn masked(self) -> Self {
        match self {
            OperandSpec::RegRRR => OperandSpec::RegRRR_maskmerge,
            OperandSpec::RegMMM => OperandSpec::RegMMM_maskmerge,
            OperandSpec::RegVex => OperandSpec::RegVex_maskmerge,
            OperandSpec::Deref => OperandSpec::Deref_mask,
            OperandSpec::RegDisp => OperandSpec::RegDisp_mask,
            OperandSpec::RegScale => OperandSpec::RegScale_mask,
            OperandSpec::RegScaleDisp => OperandSpec::RegScaleDisp_mask,
            OperandSpec::RegIndexBaseScale => OperandSpec::RegIndexBaseScale_mask,
            OperandSpec::RegIndexBaseScaleDisp => OperandSpec::RegIndexBaseScaleDisp_mask,
            o => o,
        }
    }
    fn is_memory(&self) -> bool {
        match self {
            OperandSpec::DispU32 |
            OperandSpec::DispU64 |
            OperandSpec::Deref |
            OperandSpec::Deref_esi |
            OperandSpec::Deref_edi |
            OperandSpec::Deref_rsi |
            OperandSpec::Deref_rdi |
            OperandSpec::RegDisp |
            OperandSpec::RegScale |
            OperandSpec::RegScaleDisp |
            OperandSpec::RegIndexBaseScale |
            OperandSpec::RegIndexBaseScaleDisp |
            OperandSpec::Deref_mask |
            OperandSpec::RegDisp_mask |
            OperandSpec::RegScale_mask |
            OperandSpec::RegScaleDisp_mask |
            OperandSpec::RegIndexBaseScale_mask |
            OperandSpec::RegIndexBaseScaleDisp_mask => {
                true
            },
            OperandSpec::ImmI8 |
            OperandSpec::ImmI16 |
            OperandSpec::ImmI32 |
            OperandSpec::ImmI64 |
            OperandSpec::ImmU8 |
            OperandSpec::ImmU16 |
            OperandSpec::RegRRR |
            OperandSpec::RegRRR_maskmerge |
            OperandSpec::RegRRR_maskmerge_sae |
            OperandSpec::RegRRR_maskmerge_sae_noround |
            OperandSpec::RegMMM |
            OperandSpec::RegMMM_maskmerge |
            OperandSpec::RegMMM_maskmerge_sae_noround |
            OperandSpec::RegVex |
            OperandSpec::RegVex_maskmerge |
            OperandSpec::Reg4 |
            OperandSpec::ImmInDispField |
            OperandSpec::Nothing => {
                false
            }
        }
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
impl Operand {
    fn from_spec(inst: &Instruction, spec: OperandSpec) -> Operand {
        match spec {
            OperandSpec::Nothing => {
                Operand::Nothing
            }
            // the register in modrm_rrr
            OperandSpec::RegRRR => {
                Operand::Register(inst.regs[0])
            }
            OperandSpec::RegRRR_maskmerge => {
                Operand::RegisterMaskMerge(
                    inst.regs[0],
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::RegRRR_maskmerge_sae => {
                Operand::RegisterMaskMergeSae(
                    inst.regs[0],
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                    SaeMode::from(inst.prefixes.evex_unchecked().vex().l(), inst.prefixes.evex_unchecked().lp()),
                )
            }
            OperandSpec::RegRRR_maskmerge_sae_noround => {
                Operand::RegisterMaskMergeSaeNoround(
                    inst.regs[0],
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            // the register in modrm_mmm (eg modrm mod bits were 11)
            OperandSpec::RegMMM => {
                Operand::Register(inst.regs[1])
            }
            OperandSpec::RegMMM_maskmerge => {
                Operand::RegisterMaskMerge(
                    inst.regs[1],
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::RegMMM_maskmerge_sae_noround => {
                Operand::RegisterMaskMergeSaeNoround(
                    inst.regs[1],
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::RegVex => {
                Operand::Register(inst.regs[3])
            }
            OperandSpec::RegVex_maskmerge => {
                Operand::RegisterMaskMerge(
                    inst.regs[3],
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::Reg4 => {
                Operand::Register(RegSpec { num: inst.imm as u8, bank: inst.regs[3].bank })
            }
            OperandSpec::ImmI8 => Operand::ImmediateI8(inst.imm as i8),
            OperandSpec::ImmU8 => Operand::ImmediateU8(inst.imm as u8),
            OperandSpec::ImmI16 => Operand::ImmediateI16(inst.imm as i16),
            OperandSpec::ImmU16 => Operand::ImmediateU16(inst.imm as u16),
            OperandSpec::ImmI32 => Operand::ImmediateI32(inst.imm as i32),
            OperandSpec::ImmI64 => Operand::ImmediateI64(inst.imm as i64),
            OperandSpec::ImmInDispField => Operand::ImmediateU16(inst.disp as u16),
            OperandSpec::DispU32 => Operand::DisplacementU32(inst.disp as u32),
            OperandSpec::DispU64 => Operand::DisplacementU64(inst.disp as u64),
            OperandSpec::Deref => {
                Operand::RegDeref(inst.regs[1])
            }
            OperandSpec::Deref_esi => {
                Operand::RegDeref(RegSpec::esi())
            }
            OperandSpec::Deref_edi => {
                Operand::RegDeref(RegSpec::edi())
            }
            OperandSpec::Deref_rsi => {
                Operand::RegDeref(RegSpec::rsi())
            }
            OperandSpec::Deref_rdi => {
                Operand::RegDeref(RegSpec::rdi())
            }
            OperandSpec::RegDisp => {
                Operand::RegDisp(inst.regs[1], inst.disp as i32)
            }
            OperandSpec::RegScale => {
                Operand::RegScale(inst.regs[2], inst.scale)
            }
            OperandSpec::RegScaleDisp => {
                Operand::RegScaleDisp(inst.regs[2], inst.scale, inst.disp as i32)
            }
            OperandSpec::RegIndexBaseScale => {
                Operand::RegIndexBaseScale(inst.regs[1], inst.regs[2], inst.scale)
            }
            OperandSpec::RegIndexBaseScaleDisp => {
                Operand::RegIndexBaseScaleDisp(inst.regs[1], inst.regs[2], inst.scale, inst.disp as i32)
            }
            OperandSpec::Deref_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegDerefMasked(inst.regs[1], RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegDeref(inst.regs[1])
                }
            }
            OperandSpec::RegDisp_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegDispMasked(inst.regs[1], inst.disp as i32, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegDisp(inst.regs[1], inst.disp as i32)
                }
            }
            OperandSpec::RegScale_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegScaleMasked(inst.regs[2], inst.scale, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegScale(inst.regs[2], inst.scale)
                }
            }
            OperandSpec::RegScaleDisp_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegScaleDispMasked(inst.regs[2], inst.scale, inst.disp as i32, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegScaleDisp(inst.regs[2], inst.scale, inst.disp as i32)
                }
            }
            OperandSpec::RegIndexBaseScale_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegIndexBaseScaleMasked(inst.regs[1], inst.regs[2], inst.scale, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegIndexBaseScale(inst.regs[1], inst.regs[2], inst.scale)
                }
            }
            OperandSpec::RegIndexBaseScaleDisp_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegIndexBaseScaleDispMasked(inst.regs[1], inst.regs[2], inst.scale, inst.disp as i32, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegIndexBaseScaleDisp(inst.regs[1], inst.regs[2], inst.scale, inst.disp as i32)
                }
            }
        }
    }

    /// returns `true` if this operand implies a memory access, `false` otherwise.
    ///
    /// notably, the `lea` instruction uses a memory operand without actually ever accessing
    /// memory.
    pub fn is_memory(&self) -> bool {
        match self {
            Operand::DisplacementU32(_) |
            Operand::DisplacementU64(_) |
            Operand::RegDeref(_) |
            Operand::RegDisp(_, _) |
            Operand::RegScale(_, _) |
            Operand::RegIndexBase(_, _) |
            Operand::RegIndexBaseDisp(_, _, _) |
            Operand::RegScaleDisp(_, _, _) |
            Operand::RegIndexBaseScale(_, _, _) |
            Operand::RegIndexBaseScaleDisp(_, _, _, _) |
            Operand::RegDerefMasked(_, _) |
            Operand::RegDispMasked(_, _, _) |
            Operand::RegScaleMasked(_, _, _) |
            Operand::RegIndexBaseMasked(_, _, _) |
            Operand::RegIndexBaseDispMasked(_, _, _, _) |
            Operand::RegScaleDispMasked(_, _, _, _) |
            Operand::RegIndexBaseScaleMasked(_, _, _, _) |
            Operand::RegIndexBaseScaleDispMasked(_, _, _, _, _) => {
                true
            },
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) |
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) |
            Operand::ImmediateU32(_) |
            Operand::ImmediateI32(_) |
            Operand::ImmediateU64(_) |
            Operand::ImmediateI64(_) |
            Operand::Register(_) |
            Operand::RegisterMaskMerge(_, _, _) |
            Operand::RegisterMaskMergeSae(_, _, _, _) |
            Operand::RegisterMaskMergeSaeNoround(_, _, _) |
            Operand::Nothing => {
                false
            }
        }
    }

    /// return the width of this operand, in bytes. register widths are determined by the
    /// register's class. the widths of memory operands are recorded on the instruction this
    /// `Operand` came from; `None` here means the authoritative width is `instr.mem_size()`.
    pub fn width(&self) -> Option<u8> {
        match self {
            Operand::Register(reg) => {
                Some(reg.width())
            }
            Operand::RegisterMaskMerge(reg, _, _) => {
                Some(reg.width())
            }
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) => {
                Some(1)
            }
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) => {
                Some(2)
            }
            Operand::ImmediateI32(_) |
            Operand::ImmediateU32(_) => {
                Some(4)
            }
            Operand::ImmediateI64(_) |
            Operand::ImmediateU64(_) => {
                Some(8)
            }
            // memory operands or `Nothing`
            _ => {
                None
            }
        }
    }
}

#[test]
fn operand_size() {
    assert_eq!(core::mem::size_of::<OperandSpec>(), 1);
    assert_eq!(core::mem::size_of::<RegSpec>(), 2);
    // assert_eq!(core::mem::size_of::<Prefixes>(), 4);
    // assert_eq!(core::mem::size_of::<Instruction>(), 40);
}

/// a trivial struct for `yaxpeax_arch::Arch` to be implemented on. it's only interesting for the
/// associated type parameters.
#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
#[allow(non_camel_case_types)]
pub struct Arch;

impl yaxpeax_arch::Arch for Arch {
    type Address = u64;
    type Word = u8;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = InstDecoder;
    type Operand = Operand;
}

#[derive(PartialEq, Copy, Clone, Eq, Hash, PartialOrd, Ord)]
struct InstDecoder {
    flags: u64,
}

impl InstDecoder {
    pub fn minimal() -> Self {
        InstDecoder {
            flags: 0,
        }
    }

    pub fn decode_slice(&self, data: &[u8]) -> Result<Instruction, DecodeError> {
        let mut reader = yaxpeax_arch::U8Reader::new(data);
        self.decode(&mut reader)
    }

    // TODO: map isa extensions over. maybe codegen the whole thing?
    fn as_64b_best_effort(&self) -> crate::long_mode::InstDecoder {
        crate::long_mode::InstDecoder::default()
    }

    // TODO: map isa extensions over. maybe codegen the whole thing?
    fn as_32b_best_effort(&self) -> crate::protected_mode::InstDecoder {
        crate::protected_mode::InstDecoder::default()
    }

    // TODO: map isa extensions over. maybe codegen the whole thing?
    fn as_16b_best_effort(&self) -> crate::real_mode::InstDecoder {
        crate::real_mode::InstDecoder::default()
    }
}

// this is layout-compatible with 64-bit RegSpec (`RegisterBank` is the same), but not 32-bit or
// 16-bit (fewer register banks). hopefully this remains generally true.
/// an `x86` register, including its number and type. if `fmt` is enabled, name too.
///
/// ```
/// use yaxpeax_x86::generic::{RegSpec, register_class};
///
/// assert_eq!(RegSpec::ecx().num(), 1);
/// assert_eq!(RegSpec::ecx().class(), register_class::D);
/// ```
///
/// some registers have classes of their own, and only one member: `rip`, `eip`, `rflags`, and
/// `eflags`.
#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialOrd, Ord, Eq, PartialEq)]
pub struct RegSpec {
    num: u8,
    bank: RegisterBank,
}

/// an `x86_64` register class - `qword`, `dword`, `xmmword`, `segment`, and so on.
///
/// this is mostly useful for comparing a `RegSpec`'s [`RegSpec::class()`] with a constant out of
/// [`register_class`].
#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct RegisterClass {
    kind: RegisterBank,
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

/// high-level register classes in an x86 machine, such as "8-byte general purpose", "xmm", "x87",
/// and so on. constants in this module are useful for inspecting the register class of a decoded
/// instruction. as an example:
/// ```
/// use yaxpeax_x86::long_mode::{self as amd64};
/// use yaxpeax_x86::long_mode::{Opcode, Operand, RegisterClass};
/// use yaxpeax_arch::{Decoder, U8Reader};
///
/// let movsx_eax_cl = &[0x0f, 0xbe, 0xc1];
/// let decoder = amd64::InstDecoder::default();
/// let instruction = decoder
///     .decode(&mut U8Reader::new(movsx_eax_cl))
///     .expect("can decode");
///
/// assert_eq!(instruction.opcode(), Opcode::MOVSX);
///
/// fn show_register_class_info(regclass: RegisterClass) {
///     match regclass {
///         amd64::register_class::D => {
///             println!("  and is a dword register");
///         }
///         amd64::register_class::B => {
///             println!("  and is a byte register");
///         }
///         other => {
///             panic!("unexpected and invalid register class {:?}", other);
///         }
///     }
/// }
///
/// if let Operand::Register(regspec) = instruction.operand(0) {
///     #[cfg(feature="fmt")]
///     println!("first operand is {}", regspec);
///     show_register_class_info(regspec.class());
/// }
///
/// if let Operand::Register(regspec) = instruction.operand(1) {
///     #[cfg(feature="fmt")]
///     println!("first operand is {}", regspec);
///     show_register_class_info(regspec.class());
/// }
/// ```
///
/// this is preferable to alternatives like checking register names against a known list: a
/// register class is one byte and "is qword general-purpose" can then be a simple one-byte
/// compare, instead of 16 string compares.
///
/// `yaxpeax-x86` does not attempt to further distinguish between, for example, register
/// suitability as operands. as an example, `cl` is only a byte register, with no additional
/// register class to describe its use as an implicit shift operand.
pub mod register_class {
    use super::{RegisterBank, RegisterClass};
    /// quadword registers: rax through r15
    pub const Q: RegisterClass = RegisterClass { kind: RegisterBank::Q };
    /// doubleword registers: eax through r15d
    pub const D: RegisterClass = RegisterClass { kind: RegisterBank::D };
    /// word registers: ax through r15w
    pub const W: RegisterClass = RegisterClass { kind: RegisterBank::W };
    /// byte registers: al, cl, dl, bl, ah, ch, dh, bh. `B` registers do *not* have a rex prefix.
    pub const B: RegisterClass = RegisterClass { kind: RegisterBank::B };
    /// byte registers with rex prefix present: al through r15b. `RB` registers have a rex prefix.
    pub const RB: RegisterClass = RegisterClass { kind: RegisterBank::rB };
    /// control registers cr0 through cr15.
    pub const CR: RegisterClass = RegisterClass { kind: RegisterBank::CR};
    /// debug registers dr0 through dr15.
    pub const DR: RegisterClass = RegisterClass { kind: RegisterBank::DR };
    /// segment registers es, cs, ss, ds, fs, gs.
    pub const S: RegisterClass = RegisterClass { kind: RegisterBank::S };
    /// xmm registers xmm0 through xmm31.
    pub const X: RegisterClass = RegisterClass { kind: RegisterBank::X };
    /// ymm registers ymm0 through ymm31.
    pub const Y: RegisterClass = RegisterClass { kind: RegisterBank::Y };
    /// zmm registers zmm0 through zmm31.
    pub const Z: RegisterClass = RegisterClass { kind: RegisterBank::Z };
    /// x87 floating point stack entries st(0) through st(7).
    pub const ST: RegisterClass = RegisterClass { kind: RegisterBank::ST };
    /// mmx registers mm0 through mm7.
    pub const MM: RegisterClass = RegisterClass { kind: RegisterBank::MM };
    /// `avx512` mask registers k0 through k7.
    pub const K: RegisterClass = RegisterClass { kind: RegisterBank::K };
    /// the full instruction pointer register.
    pub const RIP: RegisterClass = RegisterClass { kind: RegisterBank::RIP };
    /// the low 32 bits of `rip`.
    pub const EIP: RegisterClass = RegisterClass { kind: RegisterBank::EIP };
    /// the full cpu flags register.
    pub const RFLAGS: RegisterClass = RegisterClass { kind: RegisterBank::RFlags };
    /// the low 32 bits of rflags.
    pub const EFLAGS: RegisterClass = RegisterClass { kind: RegisterBank::EFlags };
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
            RegisterBank::B |
            RegisterBank::rB => {
                1
            },
            RegisterBank::CR |
            RegisterBank::DR => {
                8
            },
            RegisterBank::S => {
                2
            },
            RegisterBank::EIP => {
                4
            }
            RegisterBank::RIP => {
                8
            }
            RegisterBank::EFlags => {
                4
            }
            RegisterBank::RFlags => {
                8
            }
            RegisterBank::X => {
                16
            }
            RegisterBank::Y => {
                32
            }
            RegisterBank::Z => {
                64
            }
            RegisterBank::ST => {
                10
            }
            RegisterBank::MM => {
                8
            }
            RegisterBank::K => {
                8
            }
        }
    }
}

#[allow(non_camel_case_types)]
#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum RegisterBank {
    Q = 0, D = 2, W = 4, B = 6, rB = 8, // Quadword, Dword, Word, Byte
    CR = 10, DR = 12, S = 14, EIP = 30, RIP = 31, EFlags = 32, RFlags = 33,  // Control reg, Debug reg, Selector, ...
    X = 15, Y = 19, Z = 23,    // XMM, YMM, ZMM
    ST = 27, MM = 28,     // ST, MM regs (x87, mmx)
    K = 29, // AVX512 mask registers
}

// this enum is identical across long-mode, protected-mode, and real-mode forms. translating
// between these can be a "simple" transmute.
/// the segment register used by the corresponding instruction.
///
/// typically this will be `ds` but can be overridden. some instructions have specific segment
/// registers used regardless of segment prefixes, and in these cases `yaxpeax-x86` will report the
/// actual segment register a physical processor would use.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Segment {
    DS = 0, CS, ES, FS, GS, SS
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct EvexData {
    // data: present, z, b, Lp, Rp. aaa
    bits: u8,
}

// `Prefixes` should be layout-compatbile with 64-bit prefixes, and convertible from 32-bit and
// 16-bit forms. this is a distinct type from 64-bit prefixes in case they stop being
// layout-compatible one day.
/// the prefixes on an instruction.
///
/// `rep`, `repnz`, `lock`, and segment override prefixes are directly accessible here. `rex`,
/// `vex`, and `evex` prefixes are available through their associated helpers.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Prefixes {
    bits: u8,
    rex: PrefixRex,
    segment: Segment,
    evex_data: EvexData,
}

/// the `avx512`-related data from an [`evex`](https://en.wikipedia.org/wiki/EVEX_prefix) prefix.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PrefixEvex {
    vex: PrefixVex,
    evex_data: EvexData,
}

impl PrefixEvex {
    fn present(&self) -> bool {
        self.evex_data.present()
    }
    /// the `evex` prefix's parts that overlap with `vex` definitions - `L`, `W`, `R`, `X`, and `B`
    /// bits.
    pub fn vex(&self) -> PrefixVex {
        self.vex
    }
    /// the `avx512` mask register in use. `0` indicates "no mask register".
    pub fn mask_reg(&self) -> u8 {
        self.evex_data.aaa()
    }
    pub fn broadcast(&self) -> bool {
        self.evex_data.b()
    }
    pub fn merge(&self) -> bool {
        self.evex_data.z()
    }
    /// the `evex` `L'` bit.
    pub fn lp(&self) -> bool {
        self.evex_data.lp()
    }
    /// the `evex` `R'` bit.
    pub fn rp(&self) -> bool {
        self.evex_data.rp()
    }
}

/// bits specified in an avx/avx2 [`vex`](https://en.wikipedia.org/wiki/VEX_prefix) prefix, `L`, `W`, `R`, `X`, and `B`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PrefixVex {
    bits: u8,
}

#[allow(dead_code)]
impl PrefixVex {
    #[inline]
    fn present(&self) -> bool { (self.bits & 0x80) == 0x80 }
    #[inline]
    pub fn b(&self) -> bool { (self.bits & 0x01) == 0x01 }
    #[inline]
    pub fn x(&self) -> bool { (self.bits & 0x02) == 0x02 }
    #[inline]
    pub fn r(&self) -> bool { (self.bits & 0x04) == 0x04 }
    #[inline]
    pub fn w(&self) -> bool { (self.bits & 0x08) == 0x08 }
    #[inline]
    pub fn l(&self) -> bool { (self.bits & 0x10) == 0x10 }
    #[inline]
    fn compressed_disp(&self) -> bool { (self.bits & 0x20) == 0x20 }
}

/// bits specified in an x86_64
/// [`rex`](https://wiki.osdev.org/X86-64_Instruction_Encoding#REX_prefix) prefix.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PrefixRex {
    bits: u8
}

impl Prefixes {
    fn new(bits: u8) -> Prefixes {
        Prefixes {
            bits: bits,
            rex: PrefixRex { bits: 0 },
            segment: Segment::DS,
            evex_data: EvexData { bits: 0 },
        }
    }

    #[inline]
    pub fn rep(&self) -> bool { self.bits & 0x30 == 0x10 }
    #[inline]
    pub fn repnz(&self) -> bool { self.bits & 0x30 == 0x30 }
    #[inline]
    pub fn rep_any(&self) -> bool { self.bits & 0x30 != 0x00 }
    #[inline]
    fn operand_size(&self) -> bool { self.bits & 0x1 == 1 }
    #[inline]
    fn address_size(&self) -> bool { self.bits & 0x2 == 2 }
    #[inline]
    pub fn lock(&self) -> bool { self.bits & 0x4 == 4 }
    #[inline]
    pub fn cs(&mut self) { self.segment = Segment::CS }
    #[inline]
    pub fn ds(&self) -> bool { self.segment == Segment::DS }
    #[inline]
    pub fn es(&self) -> bool { self.segment == Segment::ES }
    #[inline]
    pub fn fs(&self) -> bool { self.segment == Segment::FS }
    #[inline]
    pub fn gs(&self) -> bool { self.segment == Segment::GS }
    #[inline]
    pub fn ss(&self) -> bool { self.segment == Segment::SS }
    #[inline]
    fn rex_unchecked(&self) -> PrefixRex { self.rex }
    #[inline]
    pub fn rex(&self) -> Option<PrefixRex> {
        let rex = self.rex_unchecked();
        if rex.present() {
            Some(rex)
        } else {
            None
        }
    }
    #[inline]
    fn vex_unchecked(&self) -> PrefixVex { PrefixVex { bits: self.rex.bits } }
    #[inline]
    pub fn vex(&self) -> Option<PrefixVex> {
        let vex = self.vex_unchecked();
        if vex.present() {
            Some(vex)
        } else {
            None
        }
    }
    #[inline]
    fn evex_unchecked(&self) -> PrefixEvex { PrefixEvex { vex: PrefixVex { bits: self.rex.bits }, evex_data: self.evex_data } }
    #[inline]
    pub fn evex(&self) -> Option<PrefixEvex> {
        let evex = self.evex_unchecked();
        if evex.present() {
            Some(evex)
        } else {
            None
        }
    }
}

impl EvexData {
    pub(crate) fn present(&self) -> bool {
        self.bits & 0b1000_0000 != 0
    }

    pub(crate) fn aaa(&self) -> u8 {
        self.bits & 0b111
    }

    pub(crate) fn b(&self) -> bool {
        (self.bits & 0b0000_1000) != 0
    }

    pub(crate) fn z(&self) -> bool {
        (self.bits & 0b0001_0000) != 0
    }

    pub(crate) fn lp(&self) -> bool {
        (self.bits & 0b0010_0000) != 0
    }

    pub(crate) fn rp(&self) -> bool {
        (self.bits & 0b0100_0000) != 0
    }
}

impl PrefixRex {
    #[inline]
    fn present(&self) -> bool { (self.bits & 0xc0) == 0x40 }
    #[inline]
    pub fn b(&self) -> bool { (self.bits & 0x01) == 0x01 }
    #[inline]
    pub fn x(&self) -> bool { (self.bits & 0x02) == 0x02 }
    #[inline]
    pub fn r(&self) -> bool { (self.bits & 0x04) == 0x04 }
    #[inline]
    pub fn w(&self) -> bool { (self.bits & 0x08) == 0x08 }
}

#[cfg(feature = "std")]
extern crate std;
#[cfg(feature = "std")]
impl std::error::Error for DecodeError {
    fn description(&self) -> &str {
        <Self as yaxpeax_arch::DecodeError>::description(self)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum OperandSpec {
    Nothing,
    // the register in regs[0]
    RegRRR,
    // the register in regs[0] and is EVEX-encoded (may have a mask register, is merged or
    // zeroed)
    RegRRR_maskmerge,
    // the register in regs[0] and is EVEX-encoded (may have a mask register, is merged or
    // zeroed). additionally, this instruction has exceptions suppressed with a potentially
    // custom rounding mode.
    RegRRR_maskmerge_sae,
    // the register in regs[0] and is EVEX-encoded (may have a mask register, is merged or
    // zeroed). additionally, this instruction has exceptions suppressed.
    RegRRR_maskmerge_sae_noround,
    // the register in modrm_mmm (eg modrm mod bits were 11)
    RegMMM,
    // same as `RegRRR`: the register is modrm's `mmm` bits, and may be masekd.
    RegMMM_maskmerge,
    RegMMM_maskmerge_sae_noround,
    // the register selected by vex-vvvv bits
    RegVex,
    RegVex_maskmerge,
    // the register selected by a handful of avx2 vex-coded instructions,
    // stuffed in imm4.
    Reg4,
    ImmI8,
    ImmI16,
    ImmI32,
    ImmI64,
    ImmU8,
    ImmU16,
    // ENTER is a two-immediate instruction, where the first immediate is stored in the disp field.
    // for this case, a second immediate-style operand is needed.
    // turns out `insertq` and `extrq` are also two-immediate instructions, so this is generalized
    // to cover them too.
    ImmInDispField,
    DispU32,
    DispU64,
    Deref,
    Deref_esi,
    Deref_edi,
    Deref_rsi,
    Deref_rdi,
    RegDisp,
    RegScale,
    RegScaleDisp,
    RegIndexBaseScale,
    RegIndexBaseScaleDisp,
    Deref_mask,
    RegDisp_mask,
    RegScale_mask,
    RegScaleDisp_mask,
    RegIndexBaseScale_mask,
    RegIndexBaseScaleDisp_mask,

    // protected mode
    DispU16,
    Deref_si,
    Deref_di,
    RegIndexBase_mask,
    RegIndexBaseDisp_mask,
    // u16:u{16,32} immediate address for a far call
    AbsoluteFarAddress,

    // real mode
    RegIndexBase,
    RegIndexBaseDisp,
}

/// an `x86_64` instruction.
///
/// typically an opcode will be inspected by [`Instruction::opcode()`], and an instruction has
/// [`Instruction::operand_count()`] many operands. operands are provided by
/// [`Instruction::operand()`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    pub prefixes: Prefixes,
    /*
    modrm_rrr: RegSpec,
    modrm_mmm: RegSpec, // doubles as sib_base
    sib_index: RegSpec,
    vex_reg: RegSpec,
    */
    regs: [RegSpec; 4],
    scale: u8,
    length: u8,
    operand_count: u8,
    operands: [OperandSpec; 4],
    imm: u64,
    disp: u64,
    pub(crate) opcode: Opcode,
    mem_size: u8,
}

impl yaxpeax_arch::Instruction for Instruction {
    fn well_defined(&self) -> bool {
        // TODO: this is incorrect!
        true
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[non_exhaustive]
pub enum DecodeError {
    ExhaustedInput,
    InvalidOpcode,
    InvalidOperand,
    InvalidPrefixes,
    TooLong,
    IncompleteDecoder,
}

impl yaxpeax_arch::DecodeError for DecodeError {
    fn data_exhausted(&self) -> bool { self == &DecodeError::ExhaustedInput }
    fn bad_opcode(&self) -> bool { self == &DecodeError::InvalidOpcode }
    fn bad_operand(&self) -> bool { self == &DecodeError::InvalidOperand }
    fn description(&self) -> &'static str {
        match self {
            DecodeError::ExhaustedInput => { "exhausted input" },
            DecodeError::InvalidOpcode => { "invalid opcode" },
            DecodeError::InvalidOperand => { "invalid operand" },
            DecodeError::InvalidPrefixes => { "invalid prefixes" },
            DecodeError::TooLong => { "too long" },
            DecodeError::IncompleteDecoder => { "the decoder is incomplete" },
        }
    }
}

impl LengthedInstruction for Instruction {
    type Unit = AddressDiff<u64>;
    #[inline]
    fn len(&self) -> Self::Unit {
        AddressDiff::from_const(self.length.into())
    }
    #[inline]
    fn min_size() -> Self::Unit {
        AddressDiff::from_const(1)
    }
}

impl Default for InstDecoder {
    /// Instantiates an x86 decoder that probably decodes what you want.
    ///
    /// Attempts to match real processors in interpretation of undefined sequences, and decodes any
    /// instruction defined in any extension.
    fn default() -> Self {
        Self {
            flags: 0xffffffff_ffffffff,
        }
    }
}

impl Decoder<Arch> for InstDecoder {
    fn decode<T: Reader<<Arch as yaxpeax_arch::Arch>::Address, <Arch as yaxpeax_arch::Arch>::Word>>(&self, words: &mut T) -> Result<Instruction, <Arch as yaxpeax_arch::Arch>::DecodeError> {
        let mut inst = crate::generic::Instruction::default();

        self.decode_into(&mut inst, words)?;

        Ok(inst)
    }

    fn decode_into<T: Reader<<Arch as yaxpeax_arch::Arch>::Address, <Arch as yaxpeax_arch::Arch>::Word>>(&self, instr: &mut Instruction, words: &mut T) -> Result<(), <Arch as yaxpeax_arch::Arch>::DecodeError> {
        let mut inst = crate::generic::Instruction::default();
        self.decode_with_annotation(instr, words, &mut NullSink)
    }
}

impl AnnotatingDecoder<Arch> for InstDecoder {
    type FieldDescription = FieldDescription;

    fn decode_with_annotation<
        T: Reader<<Arch as yaxpeax_arch::Arch>::Address, <Arch as yaxpeax_arch::Arch>::Word>,
        S: DescriptionSink<Self::FieldDescription>
    >(&self, instr: &mut Instruction, words: &mut T, sink: &mut S) -> Result<(), <Arch as yaxpeax_arch::Arch>::DecodeError> {
        // we only get one shot to read words from `&mut T`, so buffer up the max length of an
        // x86 instruct's worth of bytes...
        let mut bytes = [0u8; 15];
        let mut available = 0;
        for i in 0..bytes.len() {
            if let Ok(word) = words.next() {
                bytes[i] = word;
                available += 1;
            } else {
                break;
            }
        }

        // first try as a 64-bit instruction..
        let decoder_64b = self.as_64b_best_effort();
        let mut instr_64b = crate::long_mode::Instruction::default();
        let mut sink_64b = NullSink;
        let res = decoder_64b.decode_with_annotation(&mut instr_64b, &mut yaxpeax_arch::U8Reader::new(&bytes), &mut sink_64b);
        let err_64b = match res {
            Ok(()) => {
                // TODO: flush sink_64b to sink
                *instr = instr_64b.to_generic();
                return Ok(());
            }
            Err(e) => {
                e.to_generic()
            }
        };

        // first try as a 32-bit instruction..
        let decoder_32b = self.as_32b_best_effort();
        let mut instr_32b = crate::protected_mode::Instruction::default();
        let mut sink_32b = NullSink;
        let res = decoder_32b.decode_with_annotation(&mut instr_32b, &mut yaxpeax_arch::U8Reader::new(&bytes), &mut sink_32b);
        let err_32b = match res {
            Ok(()) => {
                // TODO: flush sink_32b to sink
                *instr = instr_32b.to_generic();
                return Ok(());
            }
            Err(e) => {
                e.to_generic()
            }
        };

        // lastly as a 16-bit instruction..
        let decoder_16b = self.as_16b_best_effort();
        let mut instr_16b = crate::real_mode::Instruction::default();
        let mut sink_16b = NullSink;
        let res = decoder_16b.decode_with_annotation(&mut instr_16b, &mut yaxpeax_arch::U8Reader::new(&bytes), &mut sink_16b);
        let err_16b = match res {
            Ok(()) => {
                // TODO: flush sink_16b to sink
                *instr = instr_16b.to_generic();
                return Ok(());
            }
            Err(e) => {
                e.to_generic()
            }
        };

        // if all errors are the same, just return it. if the errors do not all agree, either:
        // * they disagree by having different strings associated (64b/32b/16b-specific text)
        // or
        // * they disagree semantically due to differences in 64b/32b/16b decoding.
        //
        // for generic decoding, we try 64-bit decoding first, falling back to 32-bit and 16-bit as
        // a last-ditch, but 16-bit decoding is *probably* not what a user of yaxpeax-x86 intends
        // to decode. for many errors, 64b and 32b versions should compare the same and get the
        // first arm here, but for some they might not quite be the same. since 32b is the
        // likely-intended fallback mode and we've fallen back (and through it) in trying to
        // decode, we'll return that error in the case of disagreements. there's no direct way to
        // access a 16-bit decode error through the generic decode interface.
        if err_64b == err_32b && err_32b == err_16b {
            Err(err_64b)
        } else {
            Err(err_32b)
        }
    }
}
impl Default for Instruction {
    fn default() -> Self {
        Instruction::invalid()
    }
}

impl Instruction {
    /// get the `Opcode` of this instruction.
    pub fn opcode(&self) -> Opcode {
        self.opcode
    }

    /// get the `Operand` at the provided index.
    ///
    /// panics if the index is `>= 4`.
    pub fn operand(&self, i: u8) -> Operand {
        assert!(i < 4);
        Operand::from_spec(self, self.operands[i as usize])
    }

    /// get the number of operands in this instruction. useful in iterating an instruction's
    /// operands generically.
    pub fn operand_count(&self) -> u8 {
        self.operand_count
    }

    /// check if operand `i` is an actual operand or not. will be `false` for `i >=
    /// inst.operand_count()`.
    pub fn operand_present(&self, i: u8) -> bool {
        assert!(i < 4);
        if i >= self.operand_count {
            return false;
        }

        if let OperandSpec::Nothing = self.operands[i as usize] {
            false
        } else {
            true
        }
    }

    /// get the memory access information for this instruction, if it accesses memory.
    ///
    /// the corresponding `MemoryAccessSize` may report that the size of accessed memory is
    /// indeterminate; this is the case for `xsave/xrestor`-style instructions whose operation size
    /// varies based on physical processor.
    pub fn mem_size(&self) -> Option<MemoryAccessSize> {
        if self.mem_size != 0 {
            Some(MemoryAccessSize { size: self.mem_size })
        } else {
            None
        }
    }

    /// build a new instruction representing nothing in particular. this is primarily useful as a
    /// default to pass to `decode_into`.
    pub fn invalid() -> Instruction {
        Instruction {
            prefixes: Prefixes::new(0),
            opcode: Opcode::NOP,
            mem_size: 0,
            regs: [RegSpec::rax(); 4],
            scale: 0,
            length: 0,
            disp: 0,
            imm: 0,
            operand_count: 0,
            operands: [OperandSpec::Nothing; 4],
        }
    }

    /// get the `Segment` that will *actually* be used for accessing the operand at index `i`.
    ///
    /// `stos`, `lods`, `movs`, and `cmps` specifically name some segments for use regardless of
    /// prefixes.
    pub fn segment_override_for_op(&self, op: u8) -> Option<Segment> {
        match self.opcode {
            Opcode::STOS |
            Opcode::SCAS => {
                if op == 0 {
                    Some(Segment::ES)
                } else {
                    None
                }
            }
            Opcode::LODS => {
                if op == 1 {
                    Some(self.prefixes.segment)
                } else {
                    None
                }
            }
            Opcode::MOVS => {
                if op == 0 {
                    Some(Segment::ES)
                } else if op == 1 {
                    Some(self.prefixes.segment)
                } else {
                    None
                }
            }
            Opcode::CMPS => {
                if op == 0 {
                    Some(self.prefixes.segment)
                } else if op == 1 {
                    Some(Segment::ES)
                } else {
                    None
                }
            },
            _ => {
                // most operands are pretty simple:
                if self.operands[op as usize].is_memory() &&
                    self.prefixes.segment != Segment::DS {
                    Some(self.prefixes.segment)
                } else {
                    None
                }
            }
        }
    }

    #[cfg(feature = "fmt")]
    /// wrap a reference to this instruction with a `DisplayStyle` to format the instruction with
    /// later. see the documentation on [`display::DisplayStyle`] for more.
    ///
    /// ```
    /// use yaxpeax_x86::long_mode::{InstDecoder, DisplayStyle};
    ///
    /// let decoder = InstDecoder::default();
    /// let inst = decoder.decode_slice(&[0x33, 0xc1]).unwrap();
    ///
    /// assert_eq!("eax ^= ecx", inst.display_with(DisplayStyle::C).to_string());
    /// assert_eq!("xor eax, ecx", inst.display_with(DisplayStyle::Intel).to_string());
    /// ```
    pub fn display_with<'a>(&'a self, style: display::DisplayStyle) -> display::InstructionDisplayer<'a> {
        display::InstructionDisplayer {
            style,
            instr: self,
        }
    }

    /// does this instruction include the `xacquire` hint for hardware lock elision?
    pub fn xacquire(&self) -> bool {
        if self.prefixes.repnz() {
            // xacquire is permitted on typical `lock` instructions, OR `xchg` with memory operand,
            // regardless of `lock` prefix.
            if self.prefixes.lock() {
                true
            } else if self.opcode == Opcode::XCHG {
                self.operands[0] != OperandSpec::RegMMM && self.operands[1] != OperandSpec::RegMMM
            } else {
                false
            }
        } else {
            false
        }
    }

    /// does this instruction include the `xrelease` hint for hardware lock elision?
    pub fn xrelease(&self) -> bool {
        if self.prefixes.rep() {
            // xrelease is permitted on typical `lock` instructions, OR `xchg` with memory operand,
            // regardless of `lock` prefix. additionally, xrelease is permitted on some forms of mov.
            if self.prefixes.lock() {
                true
            } else if self.opcode == Opcode::XCHG {
                self.operands[0] != OperandSpec::RegMMM && self.operands[1] != OperandSpec::RegMMM
            } else if self.opcode == Opcode::MOV {
                self.operands[0] != OperandSpec::RegMMM && (
                    self.operands[1] == OperandSpec::RegRRR ||
                    self.operands[1] == OperandSpec::ImmI8 ||
                    self.operands[1] == OperandSpec::ImmI16 ||
                    self.operands[1] == OperandSpec::ImmI32 ||
                    self.operands[1] == OperandSpec::ImmI64
                )
            } else {
                false
            }
        } else {
            false
        }
    }
}

#[inline]
fn width_to_gp_reg_bank(width: u8, rex: bool) -> RegisterBank {
    // transform (width, rex) into an index into an index into a LUT, instead of branching as
    // `match` would.
    let index = (width.trailing_zeros() << 1) | (rex as u32);

    const BANK_LUT: [RegisterBank; 8] = [
        RegisterBank::B, RegisterBank::rB,
        RegisterBank::W, RegisterBank::W,
        RegisterBank::D, RegisterBank::D,
        RegisterBank::Q, RegisterBank::Q,
    ];

    *BANK_LUT.get(index as usize).unwrap_or_else(|| unsafe { unreachable_unchecked() })
}

/// a wrapper to hide internal library implementation details. this is only useful for the inner
/// content's `Display` impl, which itself is unstable and suitable only for human consumption.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct OperandCodeWrapper { code: () }
// TODO: something with OperandCodeWrapper...

/// the actual description for a selection of bits involved in decoding a [`generic::Instruction`].
///
/// TODO: adjust wording w.r.t generic instructions, this can't be entirely precise..!
/// some prefixes are only identified as an `InnerDescription::Misc` string, while some are full
/// `InnerDescription::SegmentPrefix(Segment)`. generally, strings should be considered unstable
/// and only useful for displaying for human consumption.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InnerDescription {
    /// the literal byte read for a `rex` prefix, `0x4_`.
    RexPrefix(u8),
    /// the segment selected by a segment override prefix. this is not necessarily the actual
    /// segement used in the instruction's memory accesses, if any are made.
    SegmentPrefix(Segment),
    /// the opcode read for this instruction. this may be reported multiple times in an instruction
    /// if multiple spans of bits are necessary to determine the opcode. it is a bug if two
    /// different `Opcode` are indicated by different `InnerDescription::Opcode` reported from
    /// decoding the same instruction. this invariant is not well-tested, and may occur in
    /// practice.
    Opcode(Opcode),
    /// the operand code indicating how to read operands for this instruction. this is an internal
    /// detail of `yaxpeax-x86` but is typically named in a manner that can aid understanding the
    /// decoding process. `OperandCode` names are unstable, and this variant is only useful for
    /// displaying for human consumption.
    OperandCode(OperandCodeWrapper),
    /// a decoded register: a name for the bits used to decode it, the register number those bits
    /// specify, and the fully-constructed [`long_mode::RegSpec`] that was decoded.
    RegisterNumber(&'static str, u8, RegSpec),
    /// a miscellaneous string describing some bits of the instruction. this may describe a prefix,
    /// internal details of a prefix, error or constraints on an opcode, operand encoding details,
    /// or other items involved in an instruction.
    Misc(&'static str),
    /// a number involved in the instruction: typically either a disaplacement or immediate. the
    /// string describes which. the `i64` member is typically a sign-extended value from the
    /// appropriate original size, meaning there may be incorrect cases of a `65535u16` sign
    /// extending to `-1`. bug reports are highly encouraged for unexpected values.
    Number(&'static str, i64),
    /// a boundary between two logically distinct sections of an instruction. these typically
    /// separate the leading prefix string (if any), opcode, and operands (if any). the included
    /// string describes which boundary this is. boundary names should not be considered stable,
    /// and are useful at most for displaying for human consumption.
    Boundary(&'static str),
}

impl InnerDescription {
    fn with_id(self, id: u32) -> FieldDescription {
        FieldDescription {
            desc: self,
            id,
        }
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature="fmt")] {
        impl fmt::Display for InnerDescription {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    InnerDescription::RexPrefix(bits) => {
                        write!(f, "rex prefix: {}{}{}{}",
                            if bits & 0x8 != 0 { "w" } else { "-" },
                            if bits & 0x4 != 0 { "r" } else { "-" },
                            if bits & 0x2 != 0 { "x" } else { "-" },
                            if bits & 0x1 != 0 { "b" } else { "-" },
                        )
                    }
                    InnerDescription::SegmentPrefix(segment) => {
                        write!(f, "segment override: {}", segment)
                    }
                    InnerDescription::Misc(text) => {
                        f.write_str(text)
                    }
                    InnerDescription::Number(text, num) => {
                        write!(f, "{}: {:#x}", text, num)
                    }
                    InnerDescription::Opcode(opc) => {
                        write!(f, "opcode `{}`", opc)
                    }
                    InnerDescription::OperandCode(OperandCodeWrapper { code }) => {
                        write!(f, "operand code `{:?}`", code)
                    }
                    InnerDescription::RegisterNumber(name, num, reg) => {
                        write!(f, "`{}` (`{}` selects register number {})", reg, name, num)
                    }
                    InnerDescription::Boundary(desc) => {
                        write!(f, "{}", desc)
                    }
                }
            }
        }
    } else {
        impl fmt::Display for InnerDescription {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("non-fmt build")
            }
        }
    }
}

#[cfg_attr(feature="fmt", derive(Debug))]
#[derive(Clone, PartialEq, Eq)]
pub struct FieldDescription {
    desc: InnerDescription,
    id: u32,
}

impl FieldDescription {
    /// the actual description associated with this bitfield.
    pub fn desc(&self) -> &InnerDescription {
        &self.desc
    }
}

impl yaxpeax_arch::annotation::FieldDescription for FieldDescription {
    fn id(&self) -> u32 {
        self.id
    }
    fn is_separator(&self) -> bool {
        if let InnerDescription::Boundary(_) = &self.desc {
            true
        } else {
            false
        }
    }
}

impl fmt::Display for FieldDescription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.desc, f)
    }
}
