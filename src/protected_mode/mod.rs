mod vex;
mod evex;
#[cfg(feature = "fmt")]
mod display;
pub mod uarch;

#[cfg(feature = "fmt")]
pub use self::display::DisplayStyle;

use core::hint::unreachable_unchecked;

use yaxpeax_arch::{AddressDiff, Decoder, LengthedInstruction};

use core::fmt;
impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DecodeError::ExhaustedInput => { write!(f, "exhausted input") },
            DecodeError::InvalidOpcode => { write!(f, "invalid opcode") },
            DecodeError::InvalidOperand => { write!(f, "invalid operand") },
            DecodeError::InvalidPrefixes => { write!(f, "invalid prefixes") },
            DecodeError::TooLong => { write!(f, "too long") },
            DecodeError::IncompleteDecoder => { write!(f, "the decoder is incomplete") },
        }
    }
}

#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialOrd, Ord, Eq, PartialEq)]
pub struct RegSpec {
    num: u8,
    bank: RegisterBank
}

use core::hash::Hash;
use core::hash::Hasher;
impl Hash for RegSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let code = ((self.bank as u16) << 8) | (self.num as u16);
        code.hash(state);
    }
}

#[derive(Debug)]
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

#[allow(non_snake_case)]
impl RegSpec {
    /// the register `eip`. this register is in the class `eip`, which contains only it.
    pub const EIP: RegSpec = RegSpec::eip();

    pub fn num(&self) -> u8 {
        self.num
    }

    pub fn class(&self) -> RegisterClass {
        RegisterClass { kind: self.bank }
    }

    #[cfg(feature = "fmt")]
    /// return a human-friendly name for this register. the returned name is the same as would be
    /// used to render this register in an instruction.
    pub fn name(&self) -> &'static str {
        display::regspec_label(self)
    }

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

    /// construct a `RegSpec` for mask reg `num`
    #[inline]
    pub fn mask(num: u8) -> RegSpec {
        if num >= 32 {
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
        if num >= 8 {
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
        if num >= 8 {
            panic!("invalid x86 word reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::W
        }
    }

    /// construct a `RegSpec` for non-rex byte reg `num`
    #[inline]
    pub fn b(num: u8) -> RegSpec {
        if num >= 8 {
            panic!("invalid x86 byte reg {}", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::B
        }
    }

    #[inline]
    fn from_parts(num: u8, bank: RegisterBank) -> RegSpec {
        RegSpec {
            num: num,
            bank: bank
        }
    }

    #[inline]
    pub const fn eip() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::EIP
        }
    }

    #[inline]
    pub const fn eflags() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::EFlags
        }
    }

    #[inline]
    pub const fn esp() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 4 }
    }

    #[inline]
    pub const fn ebp() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 5 }
    }

    #[inline]
    pub const fn cs() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 1 }
    }

    #[inline]
    pub const fn ds() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 3 }
    }

    #[inline]
    pub const fn es() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 0 }
    }

    #[inline]
    pub const fn ss() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 2 }
    }

    #[inline]
    pub const fn fs() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 4 }
    }

    #[inline]
    pub const fn gs() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 5 }
    }

    #[inline]
    pub const fn eax() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 0 }
    }

    #[inline]
    pub const fn ecx() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 1 }
    }

    #[inline]
    pub const fn edx() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 2 }
    }

    #[inline]
    pub const fn ebx() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 3 }
    }

    #[inline]
    pub const fn esi() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 6 }
    }

    #[inline]
    pub const fn edi() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 7 }
    }

    #[inline]
    pub const fn ax() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 0 }
    }

    #[inline]
    pub const fn cx() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 1 }
    }

    #[inline]
    pub const fn dx() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 2 }
    }

    #[inline]
    pub const fn bx() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 3 }
    }

    #[inline]
    pub const fn sp() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 4 }
    }

    #[inline]
    pub const fn bp() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 5 }
    }

    #[inline]
    pub const fn si() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 6 }
    }

    #[inline]
    pub const fn di() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 7 }
    }

    #[inline]
    pub const fn al() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 0 }
    }

    #[inline]
    pub const fn cl() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 1 }
    }

    #[inline]
    pub const fn dl() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 2 }
    }

    #[inline]
    pub const fn ah() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 4 }
    }

    #[inline]
    pub const fn ch() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 5 }
    }

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

    /// return the size of this register, in bytes
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
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum Operand {
    ImmediateI8(i8),
    ImmediateU8(u8),
    ImmediateI16(i16),
    ImmediateU16(u16),
    ImmediateU32(u32),
    ImmediateI32(i32),
    Register(RegSpec),
    RegisterMaskMerge(RegSpec, RegSpec, MergeMode),
    RegisterMaskMergeSae(RegSpec, RegSpec, MergeMode, SaeMode),
    RegisterMaskMergeSaeNoround(RegSpec, RegSpec, MergeMode),
    DisplacementU16(u16),
    DisplacementU32(u32),
    RegDeref(RegSpec),
    RegDisp(RegSpec, i32),
    RegScale(RegSpec, u8),
    RegIndexBase(RegSpec, RegSpec),
    RegIndexBaseDisp(RegSpec, RegSpec, i32),
    RegScaleDisp(RegSpec, u8, i32),
    RegIndexBaseScale(RegSpec, RegSpec, u8),
    RegIndexBaseScaleDisp(RegSpec, RegSpec, u8, i32),
    RegDerefMasked(RegSpec, RegSpec),
    RegDispMasked(RegSpec, i32, RegSpec),
    RegScaleMasked(RegSpec, u8, RegSpec),
    RegIndexBaseMasked(RegSpec, RegSpec, RegSpec),
    RegIndexBaseDispMasked(RegSpec, RegSpec, i32, RegSpec),
    RegScaleDispMasked(RegSpec, u8, i32, RegSpec),
    RegIndexBaseScaleMasked(RegSpec, RegSpec, u8, RegSpec),
    RegIndexBaseScaleDispMasked(RegSpec, RegSpec, u8, i32, RegSpec),
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
    pub fn is_memory(&self) -> bool {
        match self {
            OperandSpec::DispU16 |
            OperandSpec::DispU32 |
            OperandSpec::Deref |
            OperandSpec::Deref_esi |
            OperandSpec::Deref_edi |
            OperandSpec::RegDisp |
            OperandSpec::RegScale |
            OperandSpec::RegIndexBase |
            OperandSpec::RegIndexBaseDisp |
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
                Operand::Register(inst.modrm_rrr)
            }
            OperandSpec::RegRRR_maskmerge => {
                Operand::RegisterMaskMerge(
                    inst.modrm_rrr,
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::RegRRR_maskmerge_sae => {
                Operand::RegisterMaskMergeSae(
                    inst.modrm_rrr,
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                    SaeMode::from(inst.prefixes.evex_unchecked().vex().l(), inst.prefixes.evex_unchecked().lp()),
                )
            }
            OperandSpec::RegRRR_maskmerge_sae_noround => {
                Operand::RegisterMaskMergeSaeNoround(
                    inst.modrm_rrr,
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            // the register in modrm_mmm (eg modrm mod bits were 11)
            OperandSpec::RegMMM => {
                Operand::Register(inst.modrm_mmm)
            }
            OperandSpec::RegMMM_maskmerge => {
                Operand::RegisterMaskMerge(
                    inst.modrm_mmm,
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::RegMMM_maskmerge_sae_noround => {
                Operand::RegisterMaskMergeSaeNoround(
                    inst.modrm_mmm,
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::RegVex => {
                Operand::Register(inst.vex_reg)
            }
            OperandSpec::RegVex_maskmerge => {
                Operand::RegisterMaskMerge(
                    inst.vex_reg,
                    RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()),
                    MergeMode::from(inst.prefixes.evex_unchecked().merge()),
                )
            }
            OperandSpec::Reg4 => {
                Operand::Register(RegSpec { num: inst.imm as u8, bank: inst.vex_reg.bank })
            }
            OperandSpec::ImmI8 => Operand::ImmediateI8(inst.imm as i8),
            OperandSpec::ImmU8 => Operand::ImmediateU8(inst.imm as u8),
            OperandSpec::ImmI16 => Operand::ImmediateI16(inst.imm as i16),
            OperandSpec::ImmU16 => Operand::ImmediateU16(inst.imm as u16),
            OperandSpec::ImmI32 => Operand::ImmediateI32(inst.imm as i32),
            OperandSpec::ImmInDispField => Operand::ImmediateU16(inst.disp as u16),
            OperandSpec::DispU16 => Operand::DisplacementU16(inst.disp as u16),
            OperandSpec::DispU32 => Operand::DisplacementU32(inst.disp),
            OperandSpec::Deref => {
                Operand::RegDeref(inst.modrm_mmm)
            }
            OperandSpec::Deref_esi => {
                Operand::RegDeref(RegSpec::esi())
            }
            OperandSpec::Deref_edi => {
                Operand::RegDeref(RegSpec::edi())
            }
            OperandSpec::RegDisp => {
                Operand::RegDisp(inst.modrm_mmm, inst.disp as i32)
            }
            OperandSpec::RegScale => {
                Operand::RegScale(inst.sib_index, inst.scale)
            }
            OperandSpec::RegIndexBase => {
                Operand::RegIndexBase(inst.modrm_mmm, inst.sib_index)
            }
            OperandSpec::RegIndexBaseDisp => {
                Operand::RegIndexBaseDisp(inst.modrm_mmm, inst.sib_index, inst.disp as i32)
            }
            OperandSpec::RegScaleDisp => {
                Operand::RegScaleDisp(inst.sib_index, inst.scale, inst.disp as i32)
            }
            OperandSpec::RegIndexBaseScale => {
                Operand::RegIndexBaseScale(inst.modrm_mmm, inst.sib_index, inst.scale)
            }
            OperandSpec::RegIndexBaseScaleDisp => {
                Operand::RegIndexBaseScaleDisp(inst.modrm_mmm, inst.sib_index, inst.scale, inst.disp as i32)
            }
            OperandSpec::Deref_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegDerefMasked(inst.modrm_mmm, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegDeref(inst.modrm_mmm)
                }
            }
            OperandSpec::RegDisp_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegDispMasked(inst.modrm_mmm, inst.disp as i32, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegDisp(inst.modrm_mmm, inst.disp as i32)
                }
            }
            OperandSpec::RegScale_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegScaleMasked(inst.sib_index, inst.scale, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegScale(inst.sib_index, inst.scale)
                }
            }
            OperandSpec::RegScaleDisp_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegScaleDispMasked(inst.sib_index, inst.scale, inst.disp as i32, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegScaleDisp(inst.sib_index, inst.scale, inst.disp as i32)
                }
            }
            OperandSpec::RegIndexBaseScale_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegIndexBaseScaleMasked(inst.modrm_mmm, inst.sib_index, inst.scale, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegIndexBaseScale(inst.modrm_mmm, inst.sib_index, inst.scale)
                }
            }
            OperandSpec::RegIndexBaseScaleDisp_mask => {
                if inst.prefixes.evex_unchecked().mask_reg() != 0 {
                    Operand::RegIndexBaseScaleDispMasked(inst.modrm_mmm, inst.sib_index, inst.scale, inst.disp as i32, RegSpec::mask(inst.prefixes.evex_unchecked().mask_reg()))
                } else {
                    Operand::RegIndexBaseScaleDisp(inst.modrm_mmm, inst.sib_index, inst.scale, inst.disp as i32)
                }
            }
        }
    }
    pub fn is_memory(&self) -> bool {
        match self {
            Operand::DisplacementU16(_) |
            Operand::DisplacementU32(_) |
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
    /// register's class.
    ///
    /// TODO: /!\ MEMORY WIDTHS ARE ALWAYS REPORTED AS 8 /!\
    pub fn width(&self) -> u8 {
        match self {
            Operand::Nothing => {
                panic!("non-operand does not have a size");
            }
            Operand::Register(reg) => {
                reg.width()
            }
            Operand::RegisterMaskMerge(reg, _, _) => {
                reg.width()
            }
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) => {
                1
            }
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) => {
                2
            }
            Operand::ImmediateI32(_) |
            Operand::ImmediateU32(_) => {
                4
            }
            // memory operands
            _ => {
                4
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

#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct RegisterClass {
    pub(self) kind: RegisterBank,
}

const REGISTER_CLASS_NAMES: &[&'static str] = &[
    "dword",
    "word",
    "byte",
    "cr",
    "dr",
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
    "eflags",
];

/// high-level register classes in an x86 machine, such as "8-byte general purpose", "xmm", "x87",
/// and so on. constants in this module are useful for inspecting the register class of a decoded
/// instruction. as an example:
/// ```
/// extern crate yaxpeax_arch;
/// use yaxpeax_x86::protected_mode::{self as amd64};
/// use yaxpeax_x86::protected_mode::{Opcode, Operand, RegisterClass};
/// use yaxpeax_arch::Decoder;
///
/// let movsx_eax_cl = &[0x0f, 0xbe, 0xc1];
/// let decoder = amd64::InstDecoder::default();
/// let instruction = decoder
///     .decode(movsx_eax_cl.into_iter().cloned())
///     .expect("can decode");
///
/// assert_eq!(instruction.opcode(), Opcode::MOVSX_b);
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
///     println!("first operand is {}", regspec);
///     show_register_class_info(regspec.class());
/// }
///
/// if let Operand::Register(regspec) = instruction.operand(1) {
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
    /// doubleword registers: eax through edi.
    pub const D: RegisterClass = RegisterClass { kind: RegisterBank::D };
    /// word registers: ax through di.
    pub const W: RegisterClass = RegisterClass { kind: RegisterBank::W };
    /// byte registers: al, cl, dl, bl, ah, ch, dh, bh.
    pub const B: RegisterClass = RegisterClass { kind: RegisterBank::B };
    /// control registers cr0 through cr7.
    pub const CR: RegisterClass = RegisterClass { kind: RegisterBank::CR};
    /// debug registers dr0 through dr7.
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
    /// `AVX512` mask registers k0 through k7.
    pub const K: RegisterClass = RegisterClass { kind: RegisterBank::K };
    /// the full instruction pointer register.
    pub const EIP: RegisterClass = RegisterClass { kind: RegisterBank::EIP };
    /// the full cpu flags register.
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
            RegisterBank::D => 4,
            RegisterBank::W => 2,
            RegisterBank::B => 1,
            RegisterBank::CR |
            RegisterBank::DR => {
                4
            },
            RegisterBank::S => {
                2
            },
            RegisterBank::EIP => {
                4
            }
            RegisterBank::EFlags => {
                4
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
    D = 0, W = 1, B = 2, // Dword, Word, Byte
    CR = 3, DR = 4, S = 5, EIP = 21, EFlags = 22,  // Control reg, Debug reg, Selector, ...
    X = 6, Y = 10, Z = 14,    // XMM, YMM, ZMM
    ST = 18, MM = 19,     // ST, MM regs (x87, mmx)
    K = 20, // AVX512 mask registers
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Segment {
    DS = 0, CS, ES, FS, GS, SS
}

const BMI1: [Opcode; 6] = [
    Opcode::ANDN,
    Opcode::BEXTR,
    Opcode::BLSI,
    Opcode::BLSMSK,
    Opcode::BLSR,
    Opcode::TZCNT,
];

const BMI2: [Opcode; 8] = [
    Opcode::BZHI,
    Opcode::MULX,
    Opcode::PDEP,
    Opcode::PEXT,
    Opcode::RORX,
    Opcode::SARX,
    Opcode::SHRX,
    Opcode::SHLX,
];

#[allow(dead_code)]
const XSAVE: [Opcode; 10] = [
    Opcode::XGETBV,
    Opcode::XRSTOR,
    Opcode::XRSTORS,
    Opcode::XSAVE,
    Opcode::XSAVEC,
    Opcode::XSAVEC64,
    Opcode::XSAVEOPT,
    Opcode::XSAVES,
    Opcode::XSAVES64,
    Opcode::XSETBV,
];

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
    MOVSX_b,
    MOVSX_w,
    MOVZX_b,
    MOVZX_w,
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

    PUSHA,
    POPA,
    BOUND,
    ARPL,
    AAS,
    AAA,
    DAS,
    DAA,
    AMX,
    ADX,

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
}

#[derive(Debug)]
pub struct Instruction {
    pub prefixes: Prefixes,
    modrm_rrr: RegSpec,
    modrm_mmm: RegSpec, // doubles as sib_base
    sib_index: RegSpec,
    vex_reg: RegSpec,
    scale: u8,
    length: u8,
    operand_count: u8,
    operands: [OperandSpec; 4],
    imm: u32,
    disp: u32,
    opcode: Opcode,
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
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
enum OperandSpec {
    Nothing,
    // the register in modrm_rrr
    RegRRR,
    // the register in modrm_rrr and is EVEX-encoded (may have a mask register, is merged or
    // zeroed)
    RegRRR_maskmerge,
    // the register in modrm_rrr and is EVEX-encoded (may have a mask register, is merged or
    // zeroed). additionally, this instruction has exceptions suppressed with a potentially
    // custom rounding mode.
    RegRRR_maskmerge_sae,
    // the register in modrm_rrr and is EVEX-encoded (may have a mask register, is merged or
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
    ImmU8,
    ImmU16,
    // ENTER is a two-immediate instruction, where the first immediate is stored in the disp field.
    // for this case, a second immediate-style operand is needed.
    // turns out `insertq` and `extrq` are also two-immediate instructions, so this is generalized
    // to cover them too.
    ImmInDispField,
    DispU16,
    DispU32,
    Deref,
    Deref_esi,
    Deref_edi,
    RegDisp,
    RegScale,
    RegIndexBase,
    RegIndexBaseDisp,
    RegScaleDisp,
    RegIndexBaseScale,
    RegIndexBaseScaleDisp,
    Deref_mask,
    RegDisp_mask,
    RegScale_mask,
    RegScaleDisp_mask,
    RegIndexBaseScale_mask,
    RegIndexBaseScaleDisp_mask,
}

// the Hash, Eq, and PartialEq impls here are possibly misleading.
// They exist because downstream some structs are spelled like
// Foo<T> for T == x86. This is only to access associated types
// which themselves are bounded, but their #[derive] require T to
// implement these traits.
#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Hash, Eq, PartialEq, Debug)]
#[allow(non_camel_case_types)]
pub struct Arch;

impl yaxpeax_arch::Arch for Arch {
    type Address = u32;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = InstDecoder;
    type Operand = Operand;
}

impl LengthedInstruction for Instruction {
    type Unit = AddressDiff<u32>;
    #[inline]
    fn len(&self) -> Self::Unit {
        AddressDiff::from_const(self.length.into())
    }
    #[inline]
    fn min_size() -> Self::Unit {
        AddressDiff::from_const(1)
    }
}

#[derive(PartialEq)]
pub struct InstDecoder {
    // extensions tracked here:
    //  0. SSE3
    //  1. SSSE3
    //  2. monitor (intel-only?)
    //  3. vmx (some atom chips still lack it)
    //  4. fma3 (intel haswell/broadwell+, amd piledriver+)
    //  5. cmpxchg16b (some amd are missingt this one)
    //  6. sse4.1
    //  7. sse4.2
    //  8. movbe
    //  9. popcnt (independent of BMI)
    // 10. aesni
    // 11. xsave (xsave, xrestor, xsetbv, xgetbv)
    // 12. rdrand (intel ivybridge+, amd ..??)
    // 13. sgx (eadd, eblock, ecreate, edbgrd, edbgwr, einit, eldb, eldu, epa, eremove, etrace,
    //     ewb, eenter, eexit, egetkey, ereport, eresume)
    // 14. bmi1 (intel haswell+, amd jaguar+)
    // 15. avx2 (intel haswell+, amd excavator+)
    // 16. bmi2 (intel ?, amd ?)
    // 17. invpcid
    // 18. mpx
    // 19. avx512_f
    // 20. avx512_dq
    // 21. rdseed
    // 22. adx
    // 23. avx512_fma
    // 24. pcommit
    // 25. clflushopt
    // 26. clwb
    // 27. avx512_pf
    // 28. avx512_er
    // 29. avx512_cd
    // 30. sha
    // 31. avx512_bw
    // 32. avx512_vl
    // 33. prefetchwt1
    // 34. avx512_vbmi
    // 35. avx512_vbmi2
    // 36. gfni (galois field instructions)
    // 37. vaes
    // 38. pclmulqdq
    // 39. avx_vnni
    // 40. avx512_bitalg
    // 41. avx512_vpopcntdq
    // 42. avx512_4vnniw
    // 43. avx512_4fmaps
    // 44. cx8 // cmpxchg8 - is this actually optional in x86?
    // 45. syscall // syscall/sysret - actually optional in x86?
    // 46. rdtscp // actually optional in x86?
    // 47. abm (lzcnt, popcnt)
    // 48. sse4a
    // 49. 3dnowprefetch // actually optional?
    // 50. xop
    // 51. skinit
    // 52. tbm
    // 53. intel quirks
    // 54. amd quirks
    // 55. avx (intel ?, amd ?)
    // 56. amd-v/svm
    // 57. lahfsahf
    // 58. cmov
    // 59. f16c
    // 60. fma4
    // 61. prefetchw
    // 62. tsx
    // 63. lzcnt
    flags: u64,
}

impl InstDecoder {
    /// Instantiates an x86 decoder that decodes the bare minimum of protected-mode x86.
    ///
    /// Pedantic and only decodes what the spec says is well-defined, rejecting undefined sequences
    /// and any instructions defined by extensions.
    pub fn minimal() -> Self {
        InstDecoder {
            flags: 0,
        }
    }

    pub fn sse3(&self) -> bool {
        self.flags & (1 << 0) != 0
    }

    pub fn with_sse3(mut self) -> Self {
        self.flags |= 1 << 0;
        self
    }

    pub fn ssse3(&self) -> bool {
        self.flags & (1 << 1) != 0
    }

    pub fn with_ssse3(mut self) -> Self {
        self.flags |= 1 << 1;
        self
    }

    pub fn monitor(&self) -> bool {
        self.flags & (1 << 2) != 0
    }

    pub fn with_monitor(mut self) -> Self {
        self.flags |= 1 << 2;
        self
    }

    pub fn vmx(&self) -> bool {
        self.flags & (1 << 3) != 0
    }

    pub fn with_vmx(mut self) -> Self {
        self.flags |= 1 << 3;
        self
    }

    pub fn fma3(&self) -> bool {
        self.flags & (1 << 4) != 0
    }

    pub fn with_fma3(mut self) -> Self {
        self.flags |= 1 << 4;
        self
    }

    pub fn cmpxchg16b(&self) -> bool {
        self.flags & (1 << 5) != 0
    }

    pub fn with_cmpxchg16b(mut self) -> Self {
        self.flags |= 1 << 5;
        self
    }

    pub fn sse4_1(&self) -> bool {
        self.flags & (1 << 6) != 0
    }

    pub fn with_sse4_1(mut self) -> Self {
        self.flags |= 1 << 6;
        self
    }

    pub fn sse4_2(&self) -> bool {
        self.flags & (1 << 7) != 0
    }

    pub fn with_sse4_2(mut self) -> Self {
        self.flags |= 1 << 7;
        self
    }

    pub fn with_sse4(self) -> Self {
        self
            .with_sse4_1()
            .with_sse4_2()
    }

    pub fn movbe(&self) -> bool {
        self.flags & (1 << 8) != 0
    }

    pub fn with_movbe(mut self) -> Self {
        self.flags |= 1 << 8;
        self
    }

    pub fn popcnt(&self) -> bool {
        self.flags & (1 << 9) != 0
    }

    pub fn with_popcnt(mut self) -> Self {
        self.flags |= 1 << 9;
        self
    }

    pub fn aesni(&self) -> bool {
        self.flags & (1 << 10) != 0
    }

    pub fn with_aesni(mut self) -> Self {
        self.flags |= 1 << 10;
        self
    }

    pub fn xsave(&self) -> bool {
        self.flags & (1 << 11) != 0
    }

    pub fn with_xsave(mut self) -> Self {
        self.flags |= 1 << 11;
        self
    }

    pub fn rdrand(&self) -> bool {
        self.flags & (1 << 12) != 0
    }

    pub fn with_rdrand(mut self) -> Self {
        self.flags |= 1 << 12;
        self
    }

    pub fn sgx(&self) -> bool {
        self.flags & (1 << 13) != 0
    }

    pub fn with_sgx(mut self) -> Self {
        self.flags |= 1 << 13;
        self
    }

    pub fn bmi1(&self) -> bool {
        self.flags & (1 << 14) != 0
    }

    pub fn with_bmi1(mut self) -> Self {
        self.flags |= 1 << 14;
        self
    }

    pub fn avx2(&self) -> bool {
        self.flags & (1 << 15) != 0
    }

    pub fn with_avx2(mut self) -> Self {
        self.flags |= 1 << 15;
        self
    }

    /// `bmi2` indicates support for the `BZHI`, `MULX`, `PDEP`, `PEXT`, `RORX`, `SARX`, `SHRX`,
    /// and `SHLX` instructions. `bmi2` is implemented in all x86 chips that implement `bmi`,
    /// except the amd `piledriver` and `steamroller` microarchitectures.
    pub fn bmi2(&self) -> bool {
        self.flags & (1 << 16) != 0
    }

    pub fn with_bmi2(mut self) -> Self {
        self.flags |= 1 << 16;
        self
    }

    pub fn invpcid(&self) -> bool {
        self.flags & (1 << 17) != 0
    }

    pub fn with_invpcid(mut self) -> Self {
        self.flags |= 1 << 17;
        self
    }

    pub fn mpx(&self) -> bool {
        self.flags & (1 << 18) != 0
    }

    pub fn with_mpx(mut self) -> Self {
        self.flags |= 1 << 18;
        self
    }

    pub fn avx512_f(&self) -> bool {
        self.flags & (1 << 19) != 0
    }

    pub fn with_avx512_f(mut self) -> Self {
        self.flags |= 1 << 19;
        self
    }

    pub fn avx512_dq(&self) -> bool {
        self.flags & (1 << 20) != 0
    }

    pub fn with_avx512_dq(mut self) -> Self {
        self.flags |= 1 << 20;
        self
    }

    pub fn rdseed(&self) -> bool {
        self.flags & (1 << 21) != 0
    }

    pub fn with_rdseed(mut self) -> Self {
        self.flags |= 1 << 21;
        self
    }

    pub fn adx(&self) -> bool {
        self.flags & (1 << 22) != 0
    }

    pub fn with_adx(mut self) -> Self {
        self.flags |= 1 << 22;
        self
    }

    pub fn avx512_fma(&self) -> bool {
        self.flags & (1 << 23) != 0
    }

    pub fn with_avx512_fma(mut self) -> Self {
        self.flags |= 1 << 23;
        self
    }

    pub fn pcommit(&self) -> bool {
        self.flags & (1 << 24) != 0
    }

    pub fn with_pcommit(mut self) -> Self {
        self.flags |= 1 << 24;
        self
    }

    pub fn clflushopt(&self) -> bool {
        self.flags & (1 << 25) != 0
    }

    pub fn with_clflushopt(mut self) -> Self {
        self.flags |= 1 << 25;
        self
    }

    pub fn clwb(&self) -> bool {
        self.flags & (1 << 26) != 0
    }

    pub fn with_clwb(mut self) -> Self {
        self.flags |= 1 << 26;
        self
    }

    pub fn avx512_pf(&self) -> bool {
        self.flags & (1 << 27) != 0
    }

    pub fn with_avx512_pf(mut self) -> Self {
        self.flags |= 1 << 27;
        self
    }

    pub fn avx512_er(&self) -> bool {
        self.flags & (1 << 28) != 0
    }

    pub fn with_avx512_er(mut self) -> Self {
        self.flags |= 1 << 28;
        self
    }

    pub fn avx512_cd(&self) -> bool {
        self.flags & (1 << 29) != 0
    }

    pub fn with_avx512_cd(mut self) -> Self {
        self.flags |= 1 << 29;
        self
    }

    pub fn sha(&self) -> bool {
        self.flags & (1 << 30) != 0
    }

    pub fn with_sha(mut self) -> Self {
        self.flags |= 1 << 30;
        self
    }

    pub fn avx512_bw(&self) -> bool {
        self.flags & (1 << 31) != 0
    }

    pub fn with_avx512_bw(mut self) -> Self {
        self.flags |= 1 << 31;
        self
    }

    pub fn avx512_vl(&self) -> bool {
        self.flags & (1 << 32) != 0
    }

    pub fn with_avx512_vl(mut self) -> Self {
        self.flags |= 1 << 32;
        self
    }

    pub fn prefetchwt1(&self) -> bool {
        self.flags & (1 << 33) != 0
    }

    pub fn with_prefetchwt1(mut self) -> Self {
        self.flags |= 1 << 33;
        self
    }

    pub fn avx512_vbmi(&self) -> bool {
        self.flags & (1 << 34) != 0
    }

    pub fn with_avx512_vbmi(mut self) -> Self {
        self.flags |= 1 << 34;
        self
    }

    pub fn avx512_vbmi2(&self) -> bool {
        self.flags & (1 << 35) != 0
    }

    pub fn with_avx512_vbmi2(mut self) -> Self {
        self.flags |= 1 << 35;
        self
    }

    pub fn gfni(&self) -> bool {
        self.flags & (1 << 36) != 0
    }

    pub fn with_gfni(mut self) -> Self {
        self.flags |= 1 << 36;
        self
    }

    pub fn vaes(&self) -> bool {
        self.flags & (1 << 37) != 0
    }

    pub fn with_vaes(mut self) -> Self {
        self.flags |= 1 << 37;
        self
    }

    pub fn pclmulqdq(&self) -> bool {
        self.flags & (1 << 38) != 0
    }

    pub fn with_pclmulqdq(mut self) -> Self {
        self.flags |= 1 << 38;
        self
    }

    pub fn avx_vnni(&self) -> bool {
        self.flags & (1 << 39) != 0
    }

    pub fn with_avx_vnni(mut self) -> Self {
        self.flags |= 1 << 39;
        self
    }

    pub fn avx512_bitalg(&self) -> bool {
        self.flags & (1 << 40) != 0
    }

    pub fn with_avx512_bitalg(mut self) -> Self {
        self.flags |= 1 << 40;
        self
    }

    pub fn avx512_vpopcntdq(&self) -> bool {
        self.flags & (1 << 41) != 0
    }

    pub fn with_avx512_vpopcntdq(mut self) -> Self {
        self.flags |= 1 << 41;
        self
    }

    pub fn avx512_4vnniw(&self) -> bool {
        self.flags & (1 << 42) != 0
    }

    pub fn with_avx512_4vnniw(mut self) -> Self {
        self.flags |= 1 << 42;
        self
    }

    pub fn avx512_4fmaps(&self) -> bool {
        self.flags & (1 << 43) != 0
    }

    pub fn with_avx512_4fmaps(mut self) -> Self {
        self.flags |= 1 << 43;
        self
    }

    pub fn avx512(&self) -> bool {
        let avx512_mask =
            (1 << 19) |
            (1 << 20) |
            (1 << 23) |
            (1 << 27) |
            (1 << 28) |
            (1 << 29) |
            (1 << 31) |
            (1 << 32) |
            (1 << 34) |
            (1 << 35) |
            (1 << 40) |
            (1 << 41) |
            (1 << 42) |
            (1 << 43);

        (self.flags & avx512_mask) == avx512_mask
    }

    pub fn with_avx512(mut self) -> Self {
        let avx512_mask =
            (1 << 19) |
            (1 << 20) |
            (1 << 23) |
            (1 << 27) |
            (1 << 28) |
            (1 << 29) |
            (1 << 31) |
            (1 << 32) |
            (1 << 34) |
            (1 << 35) |
            (1 << 40) |
            (1 << 41) |
            (1 << 42) |
            (1 << 43);

        self.flags |= avx512_mask;
        self
    }

    pub fn cx8(&self) -> bool {
        self.flags & (1 << 44) != 0
    }

    pub fn with_cx8(mut self) -> Self {
        self.flags |= 1 << 44;
        self
    }

    pub fn syscall(&self) -> bool {
        self.flags & (1 << 45) != 0
    }

    pub fn with_syscall(mut self) -> Self {
        self.flags |= 1 << 45;
        self
    }

    pub fn rdtscp(&self) -> bool {
        self.flags & (1 << 46) != 0
    }

    pub fn with_rdtscp(mut self) -> Self {
        self.flags |= 1 << 46;
        self
    }

    pub fn abm(&self) -> bool {
        self.flags & (1 << 47) != 0
    }

    pub fn with_abm(mut self) -> Self {
        self.flags |= 1 << 47;
        self
    }

    pub fn sse4a(&self) -> bool {
        self.flags & (1 << 48) != 0
    }

    pub fn with_sse4a(mut self) -> Self {
        self.flags |= 1 << 48;
        self
    }

    pub fn _3dnowprefetch(&self) -> bool {
        self.flags & (1 << 49) != 0
    }

    pub fn with_3dnowprefetch(mut self) -> Self {
        self.flags |= 1 << 49;
        self
    }

    pub fn xop(&self) -> bool {
        self.flags & (1 << 50) != 0
    }

    pub fn with_xop(mut self) -> Self {
        self.flags |= 1 << 50;
        self
    }

    pub fn skinit(&self) -> bool {
        self.flags & (1 << 51) != 0
    }

    pub fn with_skinit(mut self) -> Self {
        self.flags |= 1 << 51;
        self
    }

    pub fn tbm(&self) -> bool {
        self.flags & (1 << 52) != 0
    }

    pub fn with_tbm(mut self) -> Self {
        self.flags |= 1 << 52;
        self
    }

    pub fn intel_quirks(&self) -> bool {
        self.flags & (1 << 53) != 0
    }

    pub fn with_intel_quirks(mut self) -> Self {
        self.flags |= 1 << 53;
        self
    }

    pub fn amd_quirks(&self) -> bool {
        self.flags & (1 << 54) != 0
    }

    pub fn with_amd_quirks(mut self) -> Self {
        self.flags |= 1 << 54;
        self
    }

    pub fn avx(&self) -> bool {
        self.flags & (1 << 55) != 0
    }

    pub fn with_avx(mut self) -> Self {
        self.flags |= 1 << 55;
        self
    }

    pub fn svm(&self) -> bool {
        self.flags & (1 << 56) != 0
    }

    pub fn with_svm(mut self) -> Self {
        self.flags |= 1 << 56;
        self
    }

    /// `lahfsahf` is only unset for early revisions of 64-bit amd and intel chips. unfortunately
    /// the clearest documentation on when these instructions were reintroduced into 64-bit
    /// architectures seems to be
    /// [wikipedia](https://en.wikipedia.org/wiki/X86-64#Older_implementations):
    /// ```text
    /// Early AMD64 and Intel 64 CPUs lacked LAHF and SAHF instructions in 64-bit mode. AMD
    /// introduced these instructions (also in 64-bit mode) with their Athlon 64, Opteron and
    /// Turion 64 revision D processors in March 2005[48][49][50] while Intel introduced the
    /// instructions with the Pentium 4 G1 stepping in December 2005. The 64-bit version of Windows
    /// 8.1 requires this feature.[47]
    /// ```
    ///
    /// this puts reintroduction of these instructions somewhere in the middle of prescott and k8
    /// lifecycles, for intel and amd respectively. because there is no specific uarch where these
    /// features become enabled, prescott and k8 default to not supporting these instructions,
    /// where later uarches support these instructions.
    pub fn lahfsahf(&self) -> bool {
        self.flags & (1 << 57) != 0
    }

    pub fn with_lahfsahf(mut self) -> Self {
        self.flags |= 1 << 57;
        self
    }

    pub fn cmov(&self) -> bool {
        self.flags & (1 << 58) != 0
    }

    pub fn with_cmov(mut self) -> Self {
        self.flags |= 1 << 58;
        self
    }

    pub fn f16c(&self) -> bool {
        self.flags & (1 << 59) != 0
    }

    pub fn with_f16c(mut self) -> Self {
        self.flags |= 1 << 59;
        self
    }

    pub fn fma4(&self) -> bool {
        self.flags & (1 << 60) != 0
    }

    pub fn with_fma4(mut self) -> Self {
        self.flags |= 1 << 60;
        self
    }

    pub fn prefetchw(&self) -> bool {
        self.flags & (1 << 61) != 0
    }

    pub fn with_prefetchw(mut self) -> Self {
        self.flags |= 1 << 61;
        self
    }

    pub fn tsx(&self) -> bool {
        self.flags & (1 << 62) != 0
    }

    pub fn with_tsx(mut self) -> Self {
        self.flags |= 1 << 62;
        self
    }

    pub fn lzcnt(&self) -> bool {
        self.flags & (1 << 63) != 0
    }

    pub fn with_lzcnt(mut self) -> Self {
        self.flags |= 1 << 63;
        self
    }

    /// Optionally reject or reinterpret instruction according to the decoder's
    /// declared extensions.
    fn revise_instruction(&self, inst: &mut Instruction) -> Result<(), DecodeError> {
        if inst.prefixes.evex().is_some() {
            if !self.avx512() {
                return Err(DecodeError::InvalidOpcode);
            } else {
                return Ok(());
            }
        }
        match inst.opcode {
            Opcode::TZCNT => {
                if !self.bmi1() {
                    // tzcnt is only supported if bmi1 is enabled. without bmi1, this decodes as
                    // bsf.
                    inst.opcode = Opcode::BSF;
                }
            }
            Opcode::LDDQU |
            Opcode::ADDSUBPS |
            Opcode::ADDSUBPD |
            Opcode::HADDPS |
            Opcode::HSUBPS |
            Opcode::HADDPD |
            Opcode::HSUBPD |
            Opcode::MOVSHDUP |
            Opcode::MOVSLDUP |
            Opcode::MOVDDUP |
            Opcode::MONITOR |
            Opcode::MWAIT => {
                // via Intel section 5.7, SSE3 Instructions
                if !self.sse3() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::PHADDW |
            Opcode::PHADDSW |
            Opcode::PHADDD |
            Opcode::PHSUBW |
            Opcode::PHSUBSW |
            Opcode::PHSUBD |
            Opcode::PABSB |
            Opcode::PABSW |
            Opcode::PABSD |
            Opcode::PMADDUBSW |
            Opcode::PMULHRSW |
            Opcode::PSHUFB |
            Opcode::PSIGNB |
            Opcode::PSIGNW |
            Opcode::PSIGND |
            Opcode::PALIGNR => {
                // via Intel section 5.8, SSSE3 Instructions
                if !self.ssse3() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::PMULLD |
            Opcode::PMULDQ |
            Opcode::MOVNTDQA |
            Opcode::BLENDPD |
            Opcode::BLENDPS |
            Opcode::BLENDVPD |
            Opcode::BLENDVPS |
            Opcode::PBLENDVB |
            Opcode::BLENDW |
            Opcode::PMINUW |
            Opcode::PMINUD |
            Opcode::PMINSB |
            Opcode::PMINSD |
            Opcode::PMAXUW |
            Opcode::PMAXUD |
            Opcode::PMAXSB |
            Opcode::PMAXSD |
            Opcode::ROUNDPS |
            Opcode::ROUNDPD |
            Opcode::ROUNDSS |
            Opcode::ROUNDSD |
            Opcode::PBLENDW |
            Opcode::EXTRACTPS |
            Opcode::INSERTPS |
            Opcode::PINSRB |
            Opcode::PINSRD |
            Opcode::PINSRQ |
            Opcode::PMOVSXBW |
            Opcode::PMOVZXBW |
            Opcode::PMOVSXBD |
            Opcode::PMOVZXBD |
            Opcode::PMOVSXWD |
            Opcode::PMOVZXWD |
            Opcode::PMOVSXBQ |
            Opcode::PMOVZXBQ |
            Opcode::PMOVSXWQ |
            Opcode::PMOVZXWQ |
            Opcode::PMOVSXDQ |
            Opcode::PMOVZXDQ |
            Opcode::DPPS |
            Opcode::DPPD |
            Opcode::MPSADBW |
            Opcode::PHMINPOSUW |
            Opcode::PTEST |
            Opcode::PCMPEQQ |
            Opcode::PEXTRB |
            Opcode::PEXTRW |
            Opcode::PEXTRD |
            Opcode::PEXTRQ |
            Opcode::PACKUSDW => {
                // via Intel section 5.10, SSE4.1 Instructions
                if !self.sse4_1() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::EXTRQ |
            Opcode::INSERTQ |
            Opcode::MOVNTSS |
            Opcode::MOVNTSD => {
                if !self.sse4a() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::CRC32 |
            Opcode::PCMPESTRI |
            Opcode::PCMPESTRM |
            Opcode::PCMPISTRI |
            Opcode::PCMPISTRM |
            Opcode::PCMPGTQ => {
                // via Intel section 5.11, SSE4.2 Instructions
                if !self.sse4_2() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::AESDEC |
            Opcode::AESDECLAST |
            Opcode::AESENC |
            Opcode::AESENCLAST |
            Opcode::AESIMC |
            Opcode::AESKEYGENASSIST => {
                // via Intel section 5.12. AESNI AND PCLMULQDQ
                if !self.aesni() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::PCLMULQDQ => {
                // via Intel section 5.12. AESNI AND PCLMULQDQ
                if !self.pclmulqdq() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::XABORT |
            Opcode::XBEGIN |
            Opcode::XEND |
            Opcode::XTEST => {
                if !self.tsx() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::SHA1MSG1 |
            Opcode::SHA1MSG2 |
            Opcode::SHA1NEXTE |
            Opcode::SHA1RNDS4 |
            Opcode::SHA256MSG1 |
            Opcode::SHA256MSG2 |
            Opcode::SHA256RNDS2 => {
                if !self.sha() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::ENCLV |
            Opcode::ENCLS |
            Opcode::ENCLU => {
                if !self.sgx() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            // AVX...
            Opcode::VMOVDDUP |
            Opcode::VPSHUFLW |
            Opcode::VPSHUFHW |
            Opcode::VHADDPS |
            Opcode::VHSUBPS |
            Opcode::VADDSUBPS |
            Opcode::VCVTPD2DQ |
            Opcode::VLDDQU |
            Opcode::VCOMISD |
            Opcode::VCOMISS |
            Opcode::VUCOMISD |
            Opcode::VUCOMISS |
            Opcode::VADDPD |
            Opcode::VADDPS |
            Opcode::VADDSD |
            Opcode::VADDSS |
            Opcode::VADDSUBPD |
            Opcode::VBLENDPD |
            Opcode::VBLENDPS |
            Opcode::VBLENDVPD |
            Opcode::VBLENDVPS |
            Opcode::VBROADCASTF128 |
            Opcode::VBROADCASTI128 |
            Opcode::VBROADCASTSD |
            Opcode::VBROADCASTSS |
            Opcode::VCMPSD |
            Opcode::VCMPSS |
            Opcode::VCMPPD |
            Opcode::VCMPPS |
            Opcode::VCVTDQ2PD |
            Opcode::VCVTDQ2PS |
            Opcode::VCVTPD2PS |
            Opcode::VCVTPS2DQ |
            Opcode::VCVTPS2PD |
            Opcode::VCVTSS2SD |
            Opcode::VCVTSI2SS |
            Opcode::VCVTSI2SD |
            Opcode::VCVTSD2SI |
            Opcode::VCVTSD2SS |
            Opcode::VCVTSS2SI |
            Opcode::VCVTTPD2DQ |
            Opcode::VCVTTPS2DQ |
            Opcode::VCVTTSS2SI |
            Opcode::VCVTTSD2SI |
            Opcode::VDIVPD |
            Opcode::VDIVPS |
            Opcode::VDIVSD |
            Opcode::VDIVSS |
            Opcode::VDPPD |
            Opcode::VDPPS |
            Opcode::VEXTRACTF128 |
            Opcode::VEXTRACTI128 |
            Opcode::VEXTRACTPS |
            Opcode::VFMADD132PD |
            Opcode::VFMADD132PS |
            Opcode::VFMADD132SD |
            Opcode::VFMADD132SS |
            Opcode::VFMADD213PD |
            Opcode::VFMADD213PS |
            Opcode::VFMADD213SD |
            Opcode::VFMADD213SS |
            Opcode::VFMADD231PD |
            Opcode::VFMADD231PS |
            Opcode::VFMADD231SD |
            Opcode::VFMADD231SS |
            Opcode::VFMADDSUB132PD |
            Opcode::VFMADDSUB132PS |
            Opcode::VFMADDSUB213PD |
            Opcode::VFMADDSUB213PS |
            Opcode::VFMADDSUB231PD |
            Opcode::VFMADDSUB231PS |
            Opcode::VFMSUB132PD |
            Opcode::VFMSUB132PS |
            Opcode::VFMSUB132SD |
            Opcode::VFMSUB132SS |
            Opcode::VFMSUB213PD |
            Opcode::VFMSUB213PS |
            Opcode::VFMSUB213SD |
            Opcode::VFMSUB213SS |
            Opcode::VFMSUB231PD |
            Opcode::VFMSUB231PS |
            Opcode::VFMSUB231SD |
            Opcode::VFMSUB231SS |
            Opcode::VFMSUBADD132PD |
            Opcode::VFMSUBADD132PS |
            Opcode::VFMSUBADD213PD |
            Opcode::VFMSUBADD213PS |
            Opcode::VFMSUBADD231PD |
            Opcode::VFMSUBADD231PS |
            Opcode::VFNMADD132PD |
            Opcode::VFNMADD132PS |
            Opcode::VFNMADD132SD |
            Opcode::VFNMADD132SS |
            Opcode::VFNMADD213PD |
            Opcode::VFNMADD213PS |
            Opcode::VFNMADD213SD |
            Opcode::VFNMADD213SS |
            Opcode::VFNMADD231PD |
            Opcode::VFNMADD231PS |
            Opcode::VFNMADD231SD |
            Opcode::VFNMADD231SS |
            Opcode::VFNMSUB132PD |
            Opcode::VFNMSUB132PS |
            Opcode::VFNMSUB132SD |
            Opcode::VFNMSUB132SS |
            Opcode::VFNMSUB213PD |
            Opcode::VFNMSUB213PS |
            Opcode::VFNMSUB213SD |
            Opcode::VFNMSUB213SS |
            Opcode::VFNMSUB231PD |
            Opcode::VFNMSUB231PS |
            Opcode::VFNMSUB231SD |
            Opcode::VFNMSUB231SS |
            Opcode::VGATHERDPD |
            Opcode::VGATHERDPS |
            Opcode::VGATHERQPD |
            Opcode::VGATHERQPS |
            Opcode::VHADDPD |
            Opcode::VHSUBPD |
            Opcode::VINSERTF128 |
            Opcode::VINSERTI128 |
            Opcode::VINSERTPS |
            Opcode::VMASKMOVDQU |
            Opcode::VMASKMOVPD |
            Opcode::VMASKMOVPS |
            Opcode::VMAXPD |
            Opcode::VMAXPS |
            Opcode::VMAXSD |
            Opcode::VMAXSS |
            Opcode::VMINPD |
            Opcode::VMINPS |
            Opcode::VMINSD |
            Opcode::VMINSS |
            Opcode::VMOVAPD |
            Opcode::VMOVAPS |
            Opcode::VMOVD |
            Opcode::VMOVDQA |
            Opcode::VMOVDQU |
            Opcode::VMOVHLPS |
            Opcode::VMOVHPD |
            Opcode::VMOVHPS |
            Opcode::VMOVLHPS |
            Opcode::VMOVLPD |
            Opcode::VMOVLPS |
            Opcode::VMOVMSKPD |
            Opcode::VMOVMSKPS |
            Opcode::VMOVNTDQ |
            Opcode::VMOVNTDQA |
            Opcode::VMOVNTPD |
            Opcode::VMOVNTPS |
            Opcode::VMOVQ |
            Opcode::VMOVSS |
            Opcode::VMOVSD |
            Opcode::VMOVSHDUP |
            Opcode::VMOVSLDUP |
            Opcode::VMOVUPD |
            Opcode::VMOVUPS |
            Opcode::VMPSADBW |
            Opcode::VMULPD |
            Opcode::VMULPS |
            Opcode::VMULSD |
            Opcode::VMULSS |
            Opcode::VPABSB |
            Opcode::VPABSD |
            Opcode::VPABSW |
            Opcode::VPACKSSDW |
            Opcode::VPACKUSDW |
            Opcode::VPACKSSWB |
            Opcode::VPACKUSWB |
            Opcode::VPADDB |
            Opcode::VPADDD |
            Opcode::VPADDQ |
            Opcode::VPADDSB |
            Opcode::VPADDSW |
            Opcode::VPADDUSB |
            Opcode::VPADDUSW |
            Opcode::VPADDW |
            Opcode::VPALIGNR |
            Opcode::VPAND |
            Opcode::VANDPD |
            Opcode::VANDPS |
            Opcode::VANDNPD |
            Opcode::VANDNPS |
            Opcode::VORPD |
            Opcode::VORPS |
            Opcode::VPANDN |
            Opcode::VPAVGB |
            Opcode::VPAVGW |
            Opcode::VPBLENDD |
            Opcode::VPBLENDVB |
            Opcode::VPBLENDW |
            Opcode::VPBROADCASTB |
            Opcode::VPBROADCASTD |
            Opcode::VPBROADCASTQ |
            Opcode::VPBROADCASTW |
            Opcode::VPCLMULQDQ |
            Opcode::VPCMPEQB |
            Opcode::VPCMPEQD |
            Opcode::VPCMPEQQ |
            Opcode::VPCMPEQW |
            Opcode::VPCMPGTB |
            Opcode::VPCMPGTD |
            Opcode::VPCMPGTQ |
            Opcode::VPCMPGTW |
            Opcode::VPCMPISTRI |
            Opcode::VPCMPISTRM |
            Opcode::VPERM2F128 |
            Opcode::VPERM2I128 |
            Opcode::VPERMD |
            Opcode::VPERMILPD |
            Opcode::VPERMILPS |
            Opcode::VPERMPD |
            Opcode::VPERMPS |
            Opcode::VPERMQ |
            Opcode::VPEXTRB |
            Opcode::VPEXTRD |
            Opcode::VPEXTRQ |
            Opcode::VPEXTRW |
            Opcode::VPGATHERDD |
            Opcode::VPGATHERDQ |
            Opcode::VPGATHERQD |
            Opcode::VPGATHERQQ |
            Opcode::VPHADDD |
            Opcode::VPHADDSW |
            Opcode::VPHADDW |
            Opcode::VPMADDUBSW |
            Opcode::VPHMINPOSUW |
            Opcode::VPHSUBD |
            Opcode::VPHSUBSW |
            Opcode::VPHSUBW |
            Opcode::VPINSRB |
            Opcode::VPINSRD |
            Opcode::VPINSRQ |
            Opcode::VPINSRW |
            Opcode::VPMADDWD |
            Opcode::VPMASKMOVD |
            Opcode::VPMASKMOVQ |
            Opcode::VPMAXSB |
            Opcode::VPMAXSD |
            Opcode::VPMAXSW |
            Opcode::VPMAXUB |
            Opcode::VPMAXUW |
            Opcode::VPMAXUD |
            Opcode::VPMINSB |
            Opcode::VPMINSW |
            Opcode::VPMINSD |
            Opcode::VPMINUB |
            Opcode::VPMINUW |
            Opcode::VPMINUD |
            Opcode::VPMOVMSKB |
            Opcode::VPMOVSXBD |
            Opcode::VPMOVSXBQ |
            Opcode::VPMOVSXBW |
            Opcode::VPMOVSXDQ |
            Opcode::VPMOVSXWD |
            Opcode::VPMOVSXWQ |
            Opcode::VPMOVZXBD |
            Opcode::VPMOVZXBQ |
            Opcode::VPMOVZXBW |
            Opcode::VPMOVZXDQ |
            Opcode::VPMOVZXWD |
            Opcode::VPMOVZXWQ |
            Opcode::VPMULDQ |
            Opcode::VPMULHRSW |
            Opcode::VPMULHUW |
            Opcode::VPMULHW |
            Opcode::VPMULLQ |
            Opcode::VPMULLD |
            Opcode::VPMULLW |
            Opcode::VPMULUDQ |
            Opcode::VPOR |
            Opcode::VPSADBW |
            Opcode::VPSHUFB |
            Opcode::VPSHUFD |
            Opcode::VPSIGNB |
            Opcode::VPSIGND |
            Opcode::VPSIGNW |
            Opcode::VPSLLD |
            Opcode::VPSLLDQ |
            Opcode::VPSLLQ |
            Opcode::VPSLLVD |
            Opcode::VPSLLVQ |
            Opcode::VPSLLW |
            Opcode::VPSRAD |
            Opcode::VPSRAVD |
            Opcode::VPSRAW |
            Opcode::VPSRLD |
            Opcode::VPSRLDQ |
            Opcode::VPSRLQ |
            Opcode::VPSRLVD |
            Opcode::VPSRLVQ |
            Opcode::VPSRLW |
            Opcode::VPSUBB |
            Opcode::VPSUBD |
            Opcode::VPSUBQ |
            Opcode::VPSUBSB |
            Opcode::VPSUBSW |
            Opcode::VPSUBUSB |
            Opcode::VPSUBUSW |
            Opcode::VPSUBW |
            Opcode::VPTEST |
            Opcode::VPUNPCKHBW |
            Opcode::VPUNPCKHDQ |
            Opcode::VPUNPCKHQDQ |
            Opcode::VPUNPCKHWD |
            Opcode::VPUNPCKLBW |
            Opcode::VPUNPCKLDQ |
            Opcode::VPUNPCKLQDQ |
            Opcode::VPUNPCKLWD |
            Opcode::VPXOR |
            Opcode::VRCPPS |
            Opcode::VROUNDPD |
            Opcode::VROUNDPS |
            Opcode::VROUNDSD |
            Opcode::VROUNDSS |
            Opcode::VRSQRTPS |
            Opcode::VRSQRTSS |
            Opcode::VRCPSS |
            Opcode::VSHUFPD |
            Opcode::VSHUFPS |
            Opcode::VSQRTPD |
            Opcode::VSQRTPS |
            Opcode::VSQRTSS |
            Opcode::VSQRTSD |
            Opcode::VSUBPD |
            Opcode::VSUBPS |
            Opcode::VSUBSD |
            Opcode::VSUBSS |
            Opcode::VTESTPD |
            Opcode::VTESTPS |
            Opcode::VUNPCKHPD |
            Opcode::VUNPCKHPS |
            Opcode::VUNPCKLPD |
            Opcode::VUNPCKLPS |
            Opcode::VXORPD |
            Opcode::VXORPS |
            Opcode::VZEROUPPER |
            Opcode::VZEROALL |
            Opcode::VLDMXCSR |
            Opcode::VSTMXCSR => {
                // TODO: check a table for these
                if !self.avx() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::VAESDEC |
            Opcode::VAESDECLAST |
            Opcode::VAESENC |
            Opcode::VAESENCLAST |
            Opcode::VAESIMC |
            Opcode::VAESKEYGENASSIST => {
                // TODO: check a table for these
                if !self.avx() || !self.aesni() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::MOVBE => {
                if !self.movbe() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::POPCNT => {
                /*
                 * from the intel SDM:
                 * ```
                 * Before an application attempts to use the POPCNT instruction, it must check that
                 * the processor supports SSE4.2 (if CPUID.01H:ECX.SSE4_2[bit 20] = 1) and POPCNT
                 * (if CPUID.01H:ECX.POPCNT[bit 23] = 1).
                 * ```
                 */
                if self.intel_quirks() && (self.sse4_2() || self.popcnt()) {
                    return Ok(());
                } else if !self.popcnt() {
                    /*
                     * elsewhere from the amd APM:
                     * `Instruction Subsets and CPUID Feature Flags` on page 507 indicates that
                     * popcnt is present when the popcnt bit is reported by cpuid. this seems to be
                     * the less quirky default, so `intel_quirks` is considered the outlier, and
                     * before this default.
                     * */
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::LZCNT => {
                /*
                 * amd APM, `LZCNT` page 212:
                 * LZCNT is an Advanced Bit Manipulation (ABM) instruction. Support for the LZCNT
                 * instruction is indicated by CPUID Fn8000_0001_ECX[ABM] = 1.
                 *
                 * meanwhile the intel SDM simply states:
                 * ```
                 * CPUID.EAX=80000001H:ECX.LZCNT[bit 5]: if 1 indicates the processor supports the
                 * LZCNT instruction.
                 * ```
                 *
                 * so that's considered the less-quirky (default) case here.
                 * */
                if self.amd_quirks() && !self.abm() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                } else if !self.lzcnt() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::ADCX |
            Opcode::ADOX => {
                if !self.adx() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::VMRUN |
            Opcode::VMLOAD |
            Opcode::VMSAVE |
            Opcode::CLGI |
            Opcode::VMMCALL |
            Opcode::INVLPGA => {
                if !self.svm() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::STGI |
            Opcode::SKINIT => {
                if !self.svm() || !self.skinit() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::LAHF |
            Opcode::SAHF => {
                if !self.lahfsahf() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::VCVTPS2PH |
            Opcode::VCVTPH2PS => {
                /*
                 * from intel SDM:
                 * ```
                 * 14.4.1 Detection of F16C Instructions Application using float 16 instruction
                 *    must follow a detection sequence similar to AVX to ensure: • The OS has
                 *    enabled YMM state management support, • The processor support AVX as
                 *    indicated by the CPUID feature flag, i.e. CPUID.01H:ECX.AVX[bit 28] = 1.  •
                 *    The processor support 16-bit floating-point conversion instructions via a
                 *    CPUID feature flag (CPUID.01H:ECX.F16C[bit 29] = 1).
                 * ```
                 *
                 * TODO: only the VEX-coded variant of this instruction should be gated on `f16c`.
                 * the EVEX-coded variant should be gated on `avx512f` or `avx512vl` if not
                 * EVEX.512-coded.
                 */
                if !self.avx() || !self.f16c() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::RDRAND => {
                if !self.rdrand() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::RDSEED => {
                if !self.rdseed() {
                    inst.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            Opcode::MONITORX | Opcode::MWAITX | // these are gated on the `monitorx` and `mwaitx` cpuid bits, but are AMD-only.
            Opcode::CLZERO | Opcode::RDPRU => { // again, gated on specific cpuid bits, but AMD-only.
                if !self.amd_quirks() {
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            other => {
                if !self.bmi1() {
                    if BMI1.contains(&other) {
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
                if !self.bmi2() {
                    if BMI2.contains(&other) {
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
            }
        }
        Ok(())
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

impl Decoder<Instruction> for InstDecoder {
    type Error = DecodeError;

    fn decode<T: IntoIterator<Item=u8>>(&self, bytes: T) -> Result<Instruction, Self::Error> {
        let mut instr = Instruction::invalid();
        decode_one(self, bytes, &mut instr)
            .map(|_: ()| instr)
    }
    fn decode_into<T: IntoIterator<Item=u8>>(&self, instr: &mut Instruction, bytes: T) -> Result<(), Self::Error> {
        decode_one(self, bytes, instr)
    }
}

impl Opcode {
    pub fn condition(&self) -> Option<ConditionCode> {
        match self {
            Opcode::JO |
            Opcode::CMOVO |
            Opcode::SETO => { Some(ConditionCode::O) },
            Opcode::JNO |
            Opcode::CMOVNO |
            Opcode::SETNO => { Some(ConditionCode::NO) },
            Opcode::JB |
            Opcode::CMOVB |
            Opcode::SETB => { Some(ConditionCode::B) },
            Opcode::JNB |
            Opcode::CMOVNB |
            Opcode::SETAE => { Some(ConditionCode::AE) },
            Opcode::JZ |
            Opcode::CMOVZ |
            Opcode::SETZ => { Some(ConditionCode::Z) },
            Opcode::JNZ |
            Opcode::CMOVNZ |
            Opcode::SETNZ => { Some(ConditionCode::NZ) },
            Opcode::JA |
            Opcode::CMOVA |
            Opcode::SETA => { Some(ConditionCode::A) },
            Opcode::JNA |
            Opcode::CMOVNA |
            Opcode::SETBE => { Some(ConditionCode::BE) },
            Opcode::JS |
            Opcode::CMOVS |
            Opcode::SETS => { Some(ConditionCode::S) },
            Opcode::JNS |
            Opcode::CMOVNS |
            Opcode::SETNS => { Some(ConditionCode::NS) },
            Opcode::JP |
            Opcode::CMOVP |
            Opcode::SETP => { Some(ConditionCode::P) },
            Opcode::JNP |
            Opcode::CMOVNP |
            Opcode::SETNP => { Some(ConditionCode::NP) },
            Opcode::JL |
            Opcode::CMOVL |
            Opcode::SETL => { Some(ConditionCode::L) },
            Opcode::JGE |
            Opcode::CMOVGE |
            Opcode::SETGE => { Some(ConditionCode::GE) },
            Opcode::JG |
            Opcode::CMOVG |
            Opcode::SETG => { Some(ConditionCode::G) },
            Opcode::JLE |
            Opcode::CMOVLE |
            Opcode::SETLE => { Some(ConditionCode::LE) },
            _ => None,
        }
    }
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction::invalid()
    }
}

impl Instruction {
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

    /// build a new instruction representing nothing in particular. this is primarily useful as a
    /// default to pass to `decode_into`.
    pub fn invalid() -> Instruction {
        Instruction {
            prefixes: Prefixes::new(0),
            opcode: Opcode::Invalid,
            mem_size: 0,
            modrm_rrr: RegSpec::eax(),
            modrm_mmm: RegSpec::eax(), // doubles as sib_base
            sib_index: RegSpec::eax(),
            vex_reg: RegSpec::eax(),
            scale: 0,
            length: 0,
            disp: 0,
            imm: 0,
            operand_count: 0,
            operands: [OperandSpec::Nothing; 4],
        }
    }

    pub fn is_invalid(&self) -> bool {
        match self.opcode {
            Opcode::Invalid => true,
            _ => false
        }
    }

    pub fn segment_override_for_op(&self, op: u8) -> Option<Segment> {
        match self.opcode {
            Opcode::STOS => {
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
    pub fn display_with<'a>(&'a self, style: display::DisplayStyle) -> display::InstructionDisplayer<'a> {
        display::InstructionDisplayer {
            style,
            instr: self,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct EvexData {
    // data: present, z, b, Lp, Rp. aaa
    bits: u8,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Prefixes {
    bits: u8,
    vex: PrefixVex,
    segment: Segment,
    evex_data: EvexData,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PrefixEvex {
    vex: PrefixVex,
    evex_data: EvexData,
}

impl PrefixEvex {
    fn present(&self) -> bool {
        self.evex_data.present()
    }
    fn vex(&self) -> &PrefixVex {
        &self.vex
    }
    fn mask_reg(&self) -> u8 {
        self.evex_data.aaa()
    }
    fn broadcast(&self) -> bool {
        self.evex_data.b()
    }
    fn merge(&self) -> bool {
        self.evex_data.z()
    }
    fn lp(&self) -> bool {
        self.evex_data.lp()
    }
    fn rp(&self) -> bool {
        self.evex_data.rp()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PrefixVex {
    bits: u8,
}

#[allow(dead_code)]
impl PrefixVex {
    #[inline]
    fn b(&self) -> bool { (self.bits & 0x01) == 0x01 }
    #[inline]
    fn x(&self) -> bool { (self.bits & 0x02) == 0x02 }
    #[inline]
    fn r(&self) -> bool { (self.bits & 0x04) == 0x04 }
    #[inline]
    fn w(&self) -> bool { (self.bits & 0x08) == 0x08 }
    #[inline]
    fn l(&self) -> bool { (self.bits & 0x10) == 0x10 }
    #[inline]
    fn present(&self) -> bool { (self.bits & 0x80) == 0x80 }
    #[inline]
    fn compressed_disp(&self) -> bool { (self.bits & 0x20) == 0x20 }
}

#[allow(dead_code)]
impl Prefixes {
    fn new(bits: u8) -> Prefixes {
        Prefixes {
            bits: bits,
            vex: PrefixVex { bits: 0 },
            segment: Segment::DS,
            evex_data: EvexData { bits: 0 },
        }
    }
    fn vex_from(&mut self, bits: u8) {
        self.vex = PrefixVex {
            bits
        };
    }
    #[inline]
    pub fn rep(&self) -> bool { self.bits & 0x30 == 0x10 }
    #[inline]
    fn set_rep(&mut self) { self.bits = (self.bits & 0xcf) | 0x10 }
    #[inline]
    pub fn repz(&self) -> bool { self.bits & 0x30 == 0x20 }
    #[inline]
    fn set_repz(&mut self) { self.bits = (self.bits & 0xcf) | 0x20 }
    #[inline]
    pub fn repnz(&self) -> bool { self.bits & 0x30 == 0x30 }
    #[inline]
    fn set_repnz(&mut self) { self.bits = (self.bits & 0xcf) | 0x30 }
    #[inline]
    pub fn rep_any(&self) -> bool { self.bits & 0x30 != 0x00 }
    #[inline]
    fn operand_size(&self) -> bool { self.bits & 0x1 == 1 }
    #[inline]
    fn set_operand_size(&mut self) { self.bits = self.bits | 0x1 }
    #[inline]
    fn unset_operand_size(&mut self) { self.bits = self.bits & !0x1 }
    #[inline]
    fn address_size(&self) -> bool { self.bits & 0x2 == 2 }
    #[inline]
    fn set_address_size(&mut self) { self.bits = self.bits | 0x2 }
    #[inline]
    fn set_lock(&mut self) { self.bits |= 0x4 }
    #[inline]
    pub fn lock(&self) -> bool { self.bits & 0x4 == 4 }
    #[inline]
    pub fn cs(&mut self) { self.segment = Segment::CS }
    #[inline]
    fn set_cs(&mut self) { self.segment = Segment::CS }
    #[inline]
    pub fn ds(&self) -> bool { self.segment == Segment::DS }
    #[inline]
    fn set_ds(&mut self) { self.segment = Segment::DS }
    #[inline]
    pub fn es(&self) -> bool { self.segment == Segment::ES }
    #[inline]
    fn set_es(&mut self) { self.segment = Segment::ES }
    #[inline]
    pub fn fs(&self) -> bool { self.segment == Segment::FS }
    #[inline]
    fn set_fs(&mut self) { self.segment = Segment::FS }
    #[inline]
    pub fn gs(&self) -> bool { self.segment == Segment::GS }
    #[inline]
    fn set_gs(&mut self) { self.segment = Segment::GS }
    #[inline]
    pub fn ss(&self) -> bool { self.segment == Segment::SS }
    #[inline]
    fn set_ss(&mut self) { self.segment = Segment::SS }
    #[inline]
    fn vex(&self) -> PrefixVex { PrefixVex { bits: self.vex.bits } }
    #[inline]
    fn evex_unchecked(&self) -> PrefixEvex { PrefixEvex { vex: PrefixVex { bits: self.vex.bits }, evex_data: self.evex_data } }
    #[inline]
    fn evex(&self) -> Option<PrefixEvex> {
        let evex = self.evex_unchecked();
        if evex.present() {
            Some(evex)
        } else {
            None
        }
    }

    #[inline]
    fn apply_compressed_disp(&mut self, state: bool) {
        if state {
            self.vex.bits |= 0x20;
        } else {
            self.vex.bits &= 0xdf;
        }
    }

    #[inline]
    fn vex_from_c5(&mut self, bits: u8) {
        // collect rex bits
        let r = bits & 0x80;
        let wrxb = (r >> 5) ^ 0x04;
        let l = (bits & 0x04) << 2;
        let synthetic_rex = wrxb | l | 0x80;
        self.vex = PrefixVex { bits: synthetic_rex };
    }

    #[inline]
    fn vex_from_c4(&mut self, high: u8, low: u8) {
        let w = low & 0x80;
        let rxb = (high >> 5) ^ 0x07;
        let wrxb = rxb | w >> 4;
        let l = (low & 0x04) << 2;
        let synthetic_vex = wrxb | l | 0x80;
        self.vex = PrefixVex { bits: synthetic_vex };
    }

    #[inline]
    fn evex_from(&mut self, b1: u8, b2: u8, b3: u8) {
        let w = b2 & 0x80;
        let rxb = ((b1 >> 5) & 0b111) ^ 0b111; // `rxb` is provided in inverted form
        let wrxb = rxb | (w >> 4);
        let l = (b3 & 0x20) >> 1;
        let synthetic_vex = wrxb | l | 0x80;
        self.vex_from(synthetic_vex);

        // R' is provided in inverted form
        let rp = ((b1 & 0x10) >> 4) ^ 1;
        let lp = (b3 & 0x40) >> 6;
        let aaa = b3 & 0b111;
        let z = (b3 & 0x80) >> 7;
        let b = (b3 & 0x10) >> 4;
        self.evex_data.from(rp, lp, z, b, aaa);
    }
}

impl EvexData {
    fn from(&mut self, rp: u8, lp: u8, z: u8, b: u8, aaa: u8) {
        let mut bits = 0;
        bits |= aaa;
        bits |= b << 3;
        bits |= z << 4;
        bits |= lp << 5;
        bits |= rp << 6;
        bits |= 0x80;
        self.bits = bits;
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

#[derive(Debug)]
struct OperandCodeBuilder {
    bits: u16
}

#[allow(non_camel_case_types)]
enum ZOperandCategory {
    Zv_R = 0,
    Zv_AX = 1,
    Zb_Ib_R = 2,
    Zv_Iv_R = 3,
}

struct ZOperandInstructions {
    bits: u16
}

impl ZOperandInstructions {
    fn category(&self) -> u8 {
        (self.bits >> 4) as u8 & 0b11
    }
    fn reg(&self) -> u8 {
        (self.bits & 0b111) as u8
    }
}

struct EmbeddedOperandInstructions {
    bits: u16
}

impl EmbeddedOperandInstructions {
    #[allow(unused)]
    fn bits(&self) -> u16 {
        self.bits
    }
}

#[allow(non_snake_case)]
impl OperandCodeBuilder {
    const fn new() -> Self {
        OperandCodeBuilder { bits: 0 }
    }

    const fn bits(&self) -> u16 {
        self.bits
    }

    const fn from_bits(bits: u16) -> Self {
        Self { bits }
    }

    const fn read_modrm(mut self) -> Self {
        self.bits |= 0x8000;
        self
    }

    const fn op0_is_rrr_and_embedded_instructions(mut self) -> Self {
        self = self.set_embedded_instructions();
        self.bits |= 0x2000;
        self
    }

    const fn set_embedded_instructions(mut self) -> Self {
        self.bits |= 0x4000;
        self
    }

    #[allow(unused)]
    const fn op0_is_rrr(&self) -> bool {
        self.bits & 0x2000 != 0
    }

    fn has_embedded_instructions(&self) -> bool {
        self.bits & 0x4000 != 0
    }

    fn get_embedded_instructions(&self) -> Result<ZOperandInstructions, EmbeddedOperandInstructions> {
        // 0x4000 indicates embedded instructions
        // 0x3fff > 0x0080 indicates the embedded instructions are a Z-style operand
        if self.bits & 0x7f80 <= 0x407f {
            Ok(ZOperandInstructions { bits: self.bits })
        } else {
            Err(EmbeddedOperandInstructions { bits: self.bits })
        }
    }

    #[allow(unused)]
    fn special_case_handler_index(&self) -> u16 {
        self.bits & 0xff
    }

    const fn special_case(mut self, case: u16) -> Self {
        // leave 0x4000 unset
        self.bits |= case & 0xff;
        self
    }

    const fn operand_case(mut self, case: u16) -> Self {
        // leave 0x4000 unset
        self.bits |= case & 0xff;
        self
    }

    const fn op0_is_rrr_and_Z_operand(mut self, category: ZOperandCategory, reg_num: u8) -> Self {
        self = self.set_embedded_instructions();
        // if op0 is rrr, 0x2000 unset indicates the operand category written in bits 11:10
        // further, reg number is bits 0:2
        //
        // when 0x2000 is unset:
        //   0x1cf8 are all unused bits, so far
        //
        // if you're counting, that's 8 bits remaining.
        // it also means one of those (0x0400?) can be used to pick some other interpretation
        // scheme.
        self.bits |= (category as u8 as u16) << 4;
        self.bits |= reg_num as u16 & 0b111;
        self
    }

    const fn read_E(mut self) -> Self {
        self.bits |= 0x1000;
        self
    }

    const fn has_read_E(&self) -> bool {
        self.bits & 0x1000 != 0
    }

    const fn byte_operands(mut self) -> Self {
        self.bits |= 0x0800;
        self
    }

    const fn mem_reg(mut self) -> Self {
        self.bits |= 0x0400;
        self
    }

    const fn reg_mem(self) -> Self {
        // 0x0400 unset
        self
    }

    const fn has_byte_operands(&self) -> bool {
        (self.bits & 0x0800) != 0
    }

    #[allow(unused)]
    const fn has_mem_reg(&self) -> bool {
        (self.bits & 0x0400) != 0
    }

    const fn has_reg_mem(&self) -> bool {
        (self.bits & 0x0400) == 0
    }

    const fn only_modrm_operands(mut self) -> Self {
        self.bits |= 0x0200;
        self
    }

    const fn is_only_modrm_operands(&self) -> bool {
        self.bits & 0x0200 != 0
    }

    // WHEN AN IMMEDIATE IS PRESENT, THERE ARE ONLY 0x3F ALLOWED SPECIAL CASES.
    // WHEN NO IMMEDIATE IS PRESENT, THERE ARE 0xFF ALLOWED SPECIAL CASES.
    // SIZE IS DECIDED BY THE FOLLOWING TABLE:
    // 0: 1 BYTE
    // 1: 4 BYTES
    const fn with_imm(mut self, only_imm: bool, size: u8) -> Self {
        self.bits |= 0x100;
        self.bits |= (size as u16) << 6;
        self.bits |= (only_imm as u16) << 7;
        self
    }

    fn has_imm(&self) -> Option<(bool, u8)> {
        if self.bits & 0x100 != 0 {
            Some(((self.bits & 0x80) != 0, (self.bits as u8 >> 6) & 1))
        } else {
            None
        }
    }
}

#[allow(non_camel_case_types)]
// might be able to pack these into a u8, but with `Operand` being u16 as well now there's little
// point. table entries will have a padding byte per record already.
//
// many of the one-off per-opcode variants could be written as 'decide based on opcode' but trying
// to pack this more tightly only makes sense if opcode were smaller, to get some space savings.
//
// bit 15 is "read modrm?"
// 0bMxxx_xxxx_xxxx_xxxx
//   |
//   |
//   |
//   |
//   |
//   |
//   ---------------------------> read modr/m?
#[repr(u16)]
#[derive(Copy, Clone, Debug, PartialEq)]
enum OperandCode {
    Ivs = OperandCodeBuilder::new().special_case(25).bits(),
    I_3 = OperandCodeBuilder::new().special_case(27).bits(),
    Nothing = OperandCodeBuilder::new().special_case(28).bits(),
    Ib = OperandCodeBuilder::new().with_imm(false, 0).special_case(32).bits(),
    Ibs = OperandCodeBuilder::new().with_imm(true, 0).special_case(33).bits(),
    Jvds = OperandCodeBuilder::new().with_imm(true, 1).special_case(32).bits(),
    Yv_Xv = OperandCodeBuilder::new().special_case(50).bits(),

    x87_d8 = OperandCodeBuilder::new().special_case(32).bits(),
    x87_d9 = OperandCodeBuilder::new().special_case(33).bits(),
    x87_da = OperandCodeBuilder::new().special_case(34).bits(),
    x87_db = OperandCodeBuilder::new().special_case(35).bits(),
    x87_dc = OperandCodeBuilder::new().special_case(36).bits(),
    x87_dd = OperandCodeBuilder::new().special_case(37).bits(),
    x87_de = OperandCodeBuilder::new().special_case(38).bits(),
    x87_df = OperandCodeBuilder::new().special_case(39).bits(),

    Eb_R0 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(0)
        .bits(),
    AL_Ib = OperandCodeBuilder::new().special_case(33).with_imm(false, 0).bits(),
    AX_Ib = OperandCodeBuilder::new().special_case(34).with_imm(false, 0).bits(),
    Ib_AL = OperandCodeBuilder::new().special_case(35).with_imm(false, 0).bits(),
    Ib_AX = OperandCodeBuilder::new().special_case(36).with_imm(false, 0).bits(),
    AX_DX = OperandCodeBuilder::new().special_case(44).bits(),
    AL_DX = OperandCodeBuilder::new().special_case(45).bits(),
    DX_AX = OperandCodeBuilder::new().special_case(46).bits(),
    DX_AL = OperandCodeBuilder::new().special_case(47).bits(),
    MOVQ_f30f = OperandCodeBuilder::new().special_case(48).bits(),

//    Unsupported = OperandCodeBuilder::new().special_case(49).bits(),

    ModRM_0x0f00 = OperandCodeBuilder::new().read_modrm().special_case(40).bits(),
    ModRM_0x0f01 = OperandCodeBuilder::new().read_modrm().special_case(41).bits(),
    ModRM_0x0f0d = OperandCodeBuilder::new().read_modrm().special_case(42).bits(),
    ModRM_0x0f0f = OperandCodeBuilder::new().read_modrm().special_case(65).bits(), // 3dnow
    ModRM_0x0fae = OperandCodeBuilder::new().read_modrm().special_case(43).bits(),
    ModRM_0x0fba = OperandCodeBuilder::new().read_modrm().special_case(44).bits(),
//    ModRM_0xf30fae = OperandCodeBuilder::new().read_modrm().special_case(46).bits(),
//    ModRM_0x660fae = OperandCodeBuilder::new().read_modrm().special_case(47).bits(),
//    ModRM_0xf30fc7 = OperandCodeBuilder::new().read_modrm().special_case(48).bits(),
//    ModRM_0x660f38 = OperandCodeBuilder::new().read_modrm().special_case(49).bits(),
//    ModRM_0xf20f38 = OperandCodeBuilder::new().read_modrm().special_case(50).bits(),
//    ModRM_0xf30f38 = OperandCodeBuilder::new().read_modrm().special_case(51).bits(),
    ModRM_0xf30f38d8 = OperandCodeBuilder::new().read_modrm().special_case(45).bits(),
    ModRM_0xf30f38dc = OperandCodeBuilder::new().read_modrm().special_case(46).bits(),
    ModRM_0xf30f38dd = OperandCodeBuilder::new().read_modrm().special_case(47).bits(),
    ModRM_0xf30f38de = OperandCodeBuilder::new().read_modrm().special_case(48).bits(),
    ModRM_0xf30f38df = OperandCodeBuilder::new().read_modrm().special_case(49).bits(),
    ModRM_0xf30f38fa = OperandCodeBuilder::new().read_modrm().special_case(50).bits(),
    ModRM_0xf30f38fb = OperandCodeBuilder::new().read_modrm().special_case(51).bits(),
//    ModRM_0x660f3a = OperandCodeBuilder::new().read_modrm().special_case(52).bits(),
//    ModRM_0x0f38 = OperandCodeBuilder::new().read_modrm().special_case(53).bits(),
//    ModRM_0x0f3a = OperandCodeBuilder::new().read_modrm().special_case(54).bits(),
    ModRM_0x0f71 = OperandCodeBuilder::new().read_modrm().special_case(55).bits(),
    ModRM_0x0f72 = OperandCodeBuilder::new().read_modrm().special_case(56).bits(),
    ModRM_0x0f73 = OperandCodeBuilder::new().read_modrm().special_case(57).bits(),
    ModRM_0xf20f78 = OperandCodeBuilder::new().read_modrm().special_case(58).bits(),
    ModRM_0x660f78 = OperandCodeBuilder::new().read_modrm().special_case(59).bits(),
//    ModRM_0x660f12 = OperandCodeBuilder::new().read_modrm().special_case(58).bits(),
//    ModRM_0x660f16 = OperandCodeBuilder::new().read_modrm().special_case(59).bits(),
//    ModRM_0x660f71 = OperandCodeBuilder::new().read_modrm().special_case(60).bits(),
//    ModRM_0x660f72 = OperandCodeBuilder::new().read_modrm().special_case(61).bits(),
//    ModRM_0x660f73 = OperandCodeBuilder::new().read_modrm().special_case(62).bits(),
//    ModRM_0x660fc7 = OperandCodeBuilder::new().read_modrm().special_case(63).bits(),
    ModRM_0x0fc7 = OperandCodeBuilder::new().read_modrm().special_case(64).bits(),
    ModRM_0xc4 = OperandCodeBuilder::new().read_modrm().special_case(66).bits(),
    ModRM_0xc5 = OperandCodeBuilder::new().read_modrm().special_case(67).bits(),
    // xmmword?
    ModRM_0x0f12 = OperandCodeBuilder::new()
        .read_modrm()
        .op0_is_rrr_and_embedded_instructions()
        .read_E()
        .reg_mem()
        .operand_case(65)
        .bits(),
    // xmmword?
    ModRM_0x0f16 = OperandCodeBuilder::new()
        .read_modrm()
        .op0_is_rrr_and_embedded_instructions()
        .read_E()
        .reg_mem()
        .operand_case(66)
        .bits(),
    // encode immediates?
    ModRM_0xc0_Eb_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(5)
        .bits(),
    ModRM_0xc1_Ev_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(6)
        .bits(),
    ModRM_0xd0_Eb_1 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(7)
        .bits(),
    ModRM_0xd1_Ev_1 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(8)
        .bits(),
    ModRM_0xd2_Eb_CL = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(9)
        .bits(),
    ModRM_0xd3_Ev_CL = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(10)
        .bits(),
    ModRM_0x80_Eb_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .with_imm(false, 0)
        .byte_operands()
        .operand_case(1)
        .bits(),
    ModRM_0x83_Ev_Ibs = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .with_imm(false, 0)
        .operand_case(26)
        .bits(),
    // this would be Eb_Ivs, 0x8e
    ModRM_0x81_Ev_Ivs = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(2)
        .bits(),
    ModRM_0xc6_Eb_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(3)
        .bits(),
    ModRM_0xc7_Ev_Iv = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(4)
        .bits(),
    ModRM_0xfe_Eb = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(13)
        .bits(),
    ModRM_0x8f_Ev = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(30)
        .bits(),
    // gap, 0x94
    ModRM_0xff_Ev = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(14)
        .bits(),
    ModRM_0x0f18 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(58)
        .bits(),
    ModRM_0xf6 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(11)
        .bits(),
    ModRM_0xf7 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(12)
        .bits(),
    Ev = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(18)
        .bits(),
    Zv_R0 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 0).bits(),
    Zv_R1 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 1).bits(),
    Zv_R2 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 2).bits(),
    Zv_R3 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 3).bits(),
    Zv_R4 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 4).bits(),
    Zv_R5 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 5).bits(),
    Zv_R6 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 6).bits(),
    Zv_R7 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_R, 7).bits(),
    // Zv_AX_R0 = 0x48,
    Zv_AX_R1 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_AX, 1).bits(),
    Zv_AX_R2 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_AX, 2).bits(),
    Zv_AX_R3 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_AX, 3).bits(),
    Zv_AX_R4 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_AX, 4).bits(),
    Zv_AX_R5 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_AX, 5).bits(),
    Zv_AX_R6 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_AX, 6).bits(),
    Zv_AX_R7 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_AX, 7).bits(),
    Zb_Ib_R0 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 0).bits(),
    Zb_Ib_R1 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 1).bits(),
    Zb_Ib_R2 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 2).bits(),
    Zb_Ib_R3 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 3).bits(),
    Zb_Ib_R4 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 4).bits(),
    Zb_Ib_R5 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 5).bits(),
    Zb_Ib_R6 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 6).bits(),
    Zb_Ib_R7 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zb_Ib_R, 7).bits(),
    Zv_Iv_R0 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 0).bits(),
    Zv_Iv_R1 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 1).bits(),
    Zv_Iv_R2 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 2).bits(),
    Zv_Iv_R3 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 3).bits(),
    Zv_Iv_R4 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 4).bits(),
    Zv_Iv_R5 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 5).bits(),
    Zv_Iv_R6 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 6).bits(),
    Zv_Iv_R7 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Iv_R, 7).bits(),
    Gv_Eb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(15).bits(),
    Gv_Ew = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(16).bits(),
    Gv_Ew_LSL = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(37).bits(),
//    Gdq_Ed = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(17).bits(),
    Gd_Ed = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().operand_case(51).bits(),
    Md_Gd = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().operand_case(52).bits(),
//    Gdq_Ed = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(17).bits(),
    Gd_Ev = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(40).bits(),
//    Md_Gd = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(51).bits(),
    G_E_xmm_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(22).bits(),
    G_E_xmm_Ub = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(60).bits(),
    G_U_xmm_Ub = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(61).bits(),
    AL_Ob = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(50).bits(),
    AL_Xb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(52).bits(),
    AX_Ov = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(53).bits(),
    AL_Ibs = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().with_imm(false, 0).byte_operands().operand_case(23).bits(),
    AX_Ivd = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(24).bits(),

    Eb_Gb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().byte_operands().only_modrm_operands().mem_reg().bits(),
    Ev_Gv = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().only_modrm_operands().mem_reg().bits(),
    Gb_Eb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().byte_operands().only_modrm_operands().reg_mem().bits(),
    Gv_Ev = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().only_modrm_operands().reg_mem().bits(),
    Gv_M = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().only_modrm_operands().reg_mem().operand_case(25).bits(),
    MOVDIR64B = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(108).bits(),
    M_Gv = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(109).bits(),
    Gv_Ev_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().with_imm(false, 0).reg_mem().operand_case(40).bits(),

    Gv_Ev_Iv = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(41).bits(),
    Rv_Gmm_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_modrm().read_E().reg_mem().operand_case(55).bits(),
    // gap, 0x9a
    G_xmm_E_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(53).bits(),
    G_xmm_U_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(54).bits(),
    U_mm_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(55).bits(),
    G_xmm_Ed = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(57).bits(),
    G_mm_E_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(58).bits(),
    Gd_U_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(59).bits(),
    Gv_E_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(60).bits(),
    //= 0x816f, // mirror G_xmm_Ed, but also read an immediate
    G_xmm_Ew_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(61).bits(),
    G_U_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(62).bits(),
    G_M_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(20).bits(),
    G_E_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(21).bits(),
    E_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(19).bits(),
    G_Ed_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(31).bits(),
    Ed_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(29).bits(),
    M_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(63).bits(),
    G_E_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(64).bits(),
    G_U_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(65).bits(),
    E_G_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(66).bits(),
    Ed_G_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(67).bits(),
    // Ed_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(68).bits(),
    G_mm_Ed = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(69).bits(),
    G_mm_E = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(70).bits(),
    Ev_Gv_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(71).bits(),
    Ev_Gv_CL = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(72).bits(),
    G_mm_U_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(73).bits(),
    G_Mq_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(74).bits(),
    G_mm_Ew_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(75).bits(),
    G_E_q = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(76).bits(),
    E_G_q = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(77).bits(),
    CVT_AA = OperandCodeBuilder::new().special_case(77).bits(),
    CVT_DA = OperandCodeBuilder::new().special_case(78).bits(),
    Rq_Cq_0 = OperandCodeBuilder::new().special_case(79).bits(),
    Rq_Dq_0 = OperandCodeBuilder::new().special_case(80).bits(),
    Cq_Rq_0 = OperandCodeBuilder::new().special_case(81).bits(),
    Dq_Rq_0 = OperandCodeBuilder::new().special_case(82).bits(),
    FS = OperandCodeBuilder::new().special_case(83).bits(),
    GS = OperandCodeBuilder::new().special_case(84).bits(),
    Yb_DX = OperandCodeBuilder::new().special_case(85).bits(),
    Yv_DX = OperandCodeBuilder::new().special_case(86).bits(),
    DX_Xb = OperandCodeBuilder::new().special_case(87).bits(),
    DX_Xv = OperandCodeBuilder::new().special_case(88).bits(),
    AH = OperandCodeBuilder::new().special_case(89).bits(),
    AX_Xv = OperandCodeBuilder::new().special_case(90).bits(),
    Ew_Sw = OperandCodeBuilder::new().special_case(91).bits(),
    Fw = OperandCodeBuilder::new().special_case(92).bits(),
    I_1 = OperandCodeBuilder::new().special_case(93).bits(),
    Iw = OperandCodeBuilder::new().special_case(94).bits(),
    Iw_Ib = OperandCodeBuilder::new().special_case(95).bits(),
    Ob_AL = OperandCodeBuilder::new().special_case(96).bits(),
    Ov_AX = OperandCodeBuilder::new().special_case(97).bits(),
    Sw_Ew = OperandCodeBuilder::new().special_case(98).bits(),
    Yb_AL = OperandCodeBuilder::new().special_case(99).bits(),
    Yb_Xb = OperandCodeBuilder::new().special_case(100).bits(),
    Yv_AX = OperandCodeBuilder::new().special_case(101).bits(),
    Ew_Gw = OperandCodeBuilder::new().special_case(102).bits(),
    ES = OperandCodeBuilder::new().special_case(103).bits(),
    CS = OperandCodeBuilder::new().special_case(104).bits(),
    SS = OperandCodeBuilder::new().special_case(105).bits(),
    DS = OperandCodeBuilder::new().special_case(106).bits(),
    ModRM_0x62 = OperandCodeBuilder::new().special_case(107).bits(),
    INV_Gv_M = OperandCodeBuilder::new().special_case(108).bits(),
    PMOVX_G_E_xmm = OperandCodeBuilder::new().operand_case(109).bits(),
    PMOVX_E_G_xmm = OperandCodeBuilder::new().operand_case(110).bits(),
    G_Ev_xmm_Ib = OperandCodeBuilder::new().operand_case(111).bits(),
    G_E_mm_Ib = OperandCodeBuilder::new().operand_case(112).bits(),
}

const LOCKABLE_INSTRUCTIONS: &[Opcode] = &[
    Opcode::ADD,
    Opcode::ADC,
    Opcode::AND,
    Opcode::BTC,
    Opcode::BTR,
    Opcode::BTS,
    Opcode::CMPXCHG,
    Opcode::CMPXCHG8B,
    Opcode::DEC,
    Opcode::INC,
    Opcode::NEG,
    Opcode::NOT,
    Opcode::OR,
    Opcode::SBB,
    Opcode::SUB,
    Opcode::XOR,
    Opcode::XADD,
    Opcode::XCHG,
];

fn base_opcode_map(v: u8) -> Opcode {
    match v {
        0 => Opcode::ADD,
        1 => Opcode::OR,
        2 => Opcode::ADC,
        3 => Opcode::SBB,
        4 => Opcode::AND,
        5 => Opcode::SUB,
        6 => Opcode::XOR,
        7 => Opcode::CMP,
        _ => { unsafe { unreachable_unchecked() } }
    }
}

const BITWISE_OPCODE_MAP: [Opcode; 8] = [
    Opcode::ROL,
    Opcode::ROR,
    Opcode::RCL,
    Opcode::RCR,
    Opcode::SHL,
    Opcode::SHR,
    Opcode::SAL,
    Opcode::SAR
];

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Interpretation {
    Instruction(Opcode),
    Prefix,
}

#[derive(Copy, Clone, Debug, PartialEq)]
// this should be a 32-byte struct..
struct OpcodeRecord(Interpretation, OperandCode);

#[test]
fn opcode_record_size() {
    // there are more than 256 opcodes...
    assert_eq!(core::mem::size_of::<OpcodeRecord>(), 4);
}

const OPCODES: [OpcodeRecord; 256] = [
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::ES),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::ES),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::CS),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::SS),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::SS),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::DS),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::DS),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::DAA), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::DAS), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::AAA), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::AAS), OperandCode::Nothing),
// 0x40:
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::INC), OperandCode::Zv_R7),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::DEC), OperandCode::Zv_R7),
// 0x50:
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Zv_R7),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::Zv_R7),
// 0x60
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSHA), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::POPA), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::BOUND), OperandCode::ModRM_0x62),
    OpcodeRecord(Interpretation::Instruction(Opcode::ARPL), OperandCode::Ew_Gw),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Ivs),
    OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev_Iv),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::INS), OperandCode::Yb_DX),
    OpcodeRecord(Interpretation::Instruction(Opcode::INS), OperandCode::Yv_DX),
    OpcodeRecord(Interpretation::Instruction(Opcode::OUTS), OperandCode::DX_Xb),
    OpcodeRecord(Interpretation::Instruction(Opcode::OUTS), OperandCode::DX_Xv),
// 0x70
    OpcodeRecord(Interpretation::Instruction(Opcode::JO), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNO), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JB), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNB), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JZ), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNZ), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNA), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JA), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JS), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNS), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JP), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNP), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JL), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JGE), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JLE), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JG), OperandCode::Ibs),
// 0x80
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x80_Eb_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x81_Ev_Ivs),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x83_Ev_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::TEST), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::TEST), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Ew_Sw),
    OpcodeRecord(Interpretation::Instruction(Opcode::LEA), OperandCode::Gv_M),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Sw_Ew),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x8f_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Zv_AX_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Zv_AX_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Zv_AX_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Zv_AX_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Zv_AX_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Zv_AX_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Zv_AX_R7),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::CVT_AA),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::CVT_DA),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::WAIT), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSHF), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::POPF), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SAHF), OperandCode::AH),
    OpcodeRecord(Interpretation::Instruction(Opcode::LAHF), OperandCode::AH),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::AL_Ob),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::AX_Ov),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Ob_AL),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Ov_AX),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVS), OperandCode::Yb_Xb),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVS), OperandCode::Yv_Xv),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPS), OperandCode::Yb_Xb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPS), OperandCode::Yv_Xv),
    OpcodeRecord(Interpretation::Instruction(Opcode::TEST), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::TEST), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::STOS), OperandCode::Yb_AL),
    OpcodeRecord(Interpretation::Instruction(Opcode::STOS), OperandCode::Yv_AX),
    OpcodeRecord(Interpretation::Instruction(Opcode::LODS), OperandCode::AL_Xb),
    OpcodeRecord(Interpretation::Instruction(Opcode::LODS), OperandCode::AX_Xv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SCAS), OperandCode::Yb_AL),
    OpcodeRecord(Interpretation::Instruction(Opcode::SCAS), OperandCode::Yv_AX),
// 0xb0
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zb_Ib_R7),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Iv_R7),
// 0xc0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xc0_Eb_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xc1_Ev_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETURN), OperandCode::Iw),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETURN), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::LES), OperandCode::ModRM_0xc4),
    OpcodeRecord(Interpretation::Instruction(Opcode::LDS), OperandCode::ModRM_0xc5),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::ModRM_0xc6_Eb_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::ModRM_0xc7_Ev_Iv),
    OpcodeRecord(Interpretation::Instruction(Opcode::ENTER), OperandCode::Iw_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::LEAVE), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETF), OperandCode::Iw),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETF), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::INT), OperandCode::I_3),
    OpcodeRecord(Interpretation::Instruction(Opcode::INT), OperandCode::Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::INTO), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::IRET), OperandCode::Fw),
// 0xd0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd0_Eb_1),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd1_Ev_1),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd2_Eb_CL),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd3_Ev_CL),
    OpcodeRecord(Interpretation::Instruction(Opcode::AMX), OperandCode::Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADX), OperandCode::Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::SALC), OperandCode::Nothing),
    // XLAT
    OpcodeRecord(Interpretation::Instruction(Opcode::XLAT), OperandCode::Nothing),
    // x86 d8
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_d8),
    // x86 d9
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_d9),
    // x86 da
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_da),
    // x86 db
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_db),
    // x86 dc
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_dc),
    // x86 dd
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_dd),
    // x86 de
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_de),
    // x86 df
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::x87_df),
// 0xe0
    OpcodeRecord(Interpretation::Instruction(Opcode::LOOPNZ), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::LOOPZ), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::LOOP), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JRCXZ), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::IN), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::IN), OperandCode::AX_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::OUT), OperandCode::Ib_AL),
    OpcodeRecord(Interpretation::Instruction(Opcode::OUT), OperandCode::Ib_AX),
// 0xe8
    OpcodeRecord(Interpretation::Instruction(Opcode::CALL), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JMP), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::JMP), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::IN), OperandCode::AL_DX),
    OpcodeRecord(Interpretation::Instruction(Opcode::IN), OperandCode::AX_DX),
    OpcodeRecord(Interpretation::Instruction(Opcode::OUT), OperandCode::DX_AL),
    OpcodeRecord(Interpretation::Instruction(Opcode::OUT), OperandCode::DX_AX),
// 0xf0
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    // ICEBP?
    OpcodeRecord(Interpretation::Instruction(Opcode::INT), OperandCode::I_1),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
// 0xf4
    OpcodeRecord(Interpretation::Instruction(Opcode::HLT), OperandCode::Nothing),
    // CMC
    OpcodeRecord(Interpretation::Instruction(Opcode::CMC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf6),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf7),
    OpcodeRecord(Interpretation::Instruction(Opcode::CLC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::STC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CLI), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::STI), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CLD), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::STD), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xfe_Eb),
    // TODO: test 0xff /3
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xff_Ev),
];

#[allow(non_snake_case)]
pub(self) fn read_E<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, width: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    let bank = width_to_gp_reg_bank(width);
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, bank)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}
#[allow(non_snake_case)]
pub(self) fn read_E_mm<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, RegisterBank::MM)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}
#[allow(non_snake_case)]
pub(self) fn read_E_st<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, RegisterBank::ST)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}
#[allow(non_snake_case)]
pub(self) fn read_E_xmm<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, RegisterBank::X)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}
#[allow(non_snake_case)]
pub(self) fn read_E_ymm<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, RegisterBank::Y)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}
#[allow(non_snake_case)]
pub(self) fn read_E_vex<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8, bank: RegisterBank) -> Result<OperandSpec, DecodeError> {
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, bank)
    } else {
        let res = read_M(bytes_iter, instr, modrm, length)?;
        if (modrm & 0b01_000_000) == 0b01_000_000 {
            instr.prefixes.apply_compressed_disp(true);
        }
        Ok(res)
    }
}

#[allow(non_snake_case)]
fn read_modrm_reg(instr: &mut Instruction, modrm: u8, reg_bank: RegisterBank) -> Result<OperandSpec, DecodeError> {
    instr.modrm_mmm = RegSpec::from_parts(modrm & 7, reg_bank);
    Ok(OperandSpec::RegMMM)
}

#[allow(non_snake_case)]
fn read_sib<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    let modbits = modrm >> 6;
    let sibbyte = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
    *length += 1;

    let disp = if modbits == 0b00 {
        if (sibbyte & 7) == 0b101 {
            *length += 4;
            read_num(bytes_iter, 4)? as i32 as u32
        } else {
            0
        }
    } else if modbits == 0b01 {
        *length += 1;
        read_num(bytes_iter, 1)? as i8 as i32 as u32
    } else {
        *length += 4;
        read_num(bytes_iter, 4)? as i32 as u32
    };
    instr.disp = disp;

    let op_spec = if (sibbyte & 7) == 0b101 {
        if ((sibbyte >> 3) & 7) == 0b100 {
            if modbits == 0b00 {
                OperandSpec::DispU32
            } else {
                instr.modrm_mmm.num |= 0b101;

                if disp == 0 {
                    OperandSpec::Deref
                } else {
                    OperandSpec::RegDisp
                }
            }
        } else {
            instr.modrm_mmm.num |= 0b101;
            instr.sib_index.num |= (sibbyte >> 3) & 7;

            let scale = 1u8 << (sibbyte >> 6);
            instr.scale = scale;

            if disp == 0 {
                if modbits == 0 {
                    OperandSpec::RegScale
                } else {
                    OperandSpec::RegIndexBaseScale
                }
            } else {
                if modbits == 0 {
                    OperandSpec::RegScaleDisp
                } else {
                    OperandSpec::RegIndexBaseScaleDisp
                }
            }
        }
    } else {
        instr.modrm_mmm.num |= sibbyte & 7;

        if ((sibbyte >> 3) & 7) == 0b100 {
            if disp == 0 {
                OperandSpec::Deref
            } else {
                OperandSpec::RegDisp
            }
        } else {
            instr.sib_index.num |= (sibbyte >> 3) & 7;

            let scale = 1u8 << (sibbyte >> 6);
            instr.scale = scale;
            if disp == 0 {
                OperandSpec::RegIndexBaseScale
            } else {
                OperandSpec::RegIndexBaseScaleDisp
            }
        }
    };
    Ok(op_spec)
}

#[allow(non_snake_case)]
fn read_M_16bit<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    let modbits = modrm >> 6;
    let mmm = modrm & 7;
    if modbits == 0b00 && mmm == 0b110 {
        return Ok(OperandSpec::DispU16);
    }
    match mmm {
        0b000 => {
            instr.modrm_mmm = RegSpec::bx();
            instr.sib_index = RegSpec::si();
        },
        0b001 => {
            instr.modrm_mmm = RegSpec::bx();
            instr.sib_index = RegSpec::di();
        },
        0b010 => {
            instr.modrm_mmm = RegSpec::bp();
            instr.sib_index = RegSpec::si();
        },
        0b011 => {
            instr.modrm_mmm = RegSpec::bp();
            instr.sib_index = RegSpec::di();
        },
        0b100 => {
            instr.modrm_mmm = RegSpec::si();
        },
        0b101 => {
            instr.modrm_mmm = RegSpec::di();
        },
        0b110 => {
            instr.modrm_mmm = RegSpec::bp();
        },
        0b111 => {
            instr.modrm_mmm = RegSpec::bx();
        },
        _ => { unreachable!("impossible bit pattern"); }
    }
    match modbits {
        0b00 => {
            if mmm > 3 {
                Ok(OperandSpec::Deref)
            } else {
                Ok(OperandSpec::RegIndexBase)
            }
        },
        0b01 => {
            instr.disp = read_num(bytes_iter, 1)?;
            *length += 1;
            if mmm > 3 {
                Ok(OperandSpec::RegDisp)
            } else {
                Ok(OperandSpec::RegIndexBaseDisp)
            }
        },
        0b10 => {
            instr.disp = read_num(bytes_iter, 2)?;
            *length += 2;
            if mmm > 3 {
                Ok(OperandSpec::RegDisp)
            } else {
                Ok(OperandSpec::RegIndexBaseDisp)
            }
        },
        _ => {
            unreachable!("read_M_16but but mod bits were not a memory operand");
        }
    }
}

#[allow(non_snake_case)]
fn read_M<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    if instr.prefixes.address_size() {
        return read_M_16bit(bytes_iter, instr, modrm, length);
    }
    instr.modrm_mmm.bank = RegisterBank::D;
    let modbits = modrm >> 6;
    let mmm = modrm & 7;
    let op_spec = if mmm == 4 {
        return read_sib(bytes_iter, instr, modrm, length);
    } else if mmm == 5 && modbits == 0b00 {
        *length += 4;
        instr.disp = read_num(bytes_iter, 4)?;
        OperandSpec::DispU32
    } else {
        instr.modrm_mmm.num |= mmm;

        if modbits == 0b00 {
            OperandSpec::Deref
        } else {
            let disp = if modbits == 0b01 {
                *length += 1;
                read_num(bytes_iter, 1)? as i8 as i32
            } else {
                *length += 4;
                read_num(bytes_iter, 4)? as i32
            };
            if disp == 0 {
                OperandSpec::Deref
            } else {
                instr.disp = disp as i32 as u32;
                OperandSpec::RegDisp
            }
        }
    };
    Ok(op_spec)
}

#[inline]
fn width_to_gp_reg_bank(width: u8) -> RegisterBank {
    match width {
        1 => return RegisterBank::B,
        2 => return RegisterBank::W,
        4 => return RegisterBank::D,
        _ => unsafe { unreachable_unchecked(); }
    }
}

fn read_0f_opcode(opcode: u8, prefixes: &mut Prefixes) -> OpcodeRecord {
    // seems like f2 takes priority, then f3, then 66, then "no prefix".  for SOME instructions an
    // invalid prefix is in fact an invalid instruction. so just duplicate for the four kinds of
    // opcode lists.
    if prefixes.repnz() {
        match opcode {
            0x00 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f00),
            0x01 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f01),
            0x02 => OpcodeRecord(Interpretation::Instruction(Opcode::LAR), OperandCode::Gv_Ew),
            0x03 => OpcodeRecord(Interpretation::Instruction(Opcode::LSL), OperandCode::Gv_Ew_LSL),
            0x04 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x05 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSCALL), OperandCode::Nothing),
            0x06 => OpcodeRecord(Interpretation::Instruction(Opcode::CLTS), OperandCode::Nothing),
            0x07 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSRET), OperandCode::Nothing),
            0x08 => OpcodeRecord(Interpretation::Instruction(Opcode::INVD), OperandCode::Nothing),
            0x09 => OpcodeRecord(Interpretation::Instruction(Opcode::WBINVD), OperandCode::Nothing),
            0x0a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0b => OpcodeRecord(Interpretation::Instruction(Opcode::UD2), OperandCode::Nothing),
            0x0c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0d),
            0x0e => OpcodeRecord(Interpretation::Instruction(Opcode::FEMMS), OperandCode::Nothing),
            0x0f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0f),

            0x10 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSD), OperandCode::PMOVX_G_E_xmm),
            0x11 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSD), OperandCode::PMOVX_E_G_xmm),
            0x12 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDDUP), OperandCode::PMOVX_G_E_xmm),
            0x13 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x14 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x15 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x16 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x17 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x18 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f18),
            0x19 => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1a => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1b => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1c => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1d => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1e => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1f => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
// 0x20
            0x20 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Cq_0),
            0x21 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Dq_0),
            0x22 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Cq_Rq_0),
            0x23 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Dq_Rq_0),
            0x24 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x25 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x26 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x27 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x28 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x29 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x2a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTSI2SD), OperandCode::G_xmm_Ed),
            0x2b => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTSD), OperandCode::M_G_xmm),
            0x2c => OpcodeRecord(Interpretation::Instruction(Opcode::CVTTSD2SI), OperandCode::PMOVX_G_E_xmm),
            0x2d => OpcodeRecord(Interpretation::Instruction(Opcode::CVTSD2SI), OperandCode::PMOVX_G_E_xmm),
            0x2e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x2f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

            0x30 => OpcodeRecord(Interpretation::Instruction(Opcode::WRMSR), OperandCode::Nothing),
            0x31 => OpcodeRecord(Interpretation::Instruction(Opcode::RDTSC), OperandCode::Nothing),
            0x32 => OpcodeRecord(Interpretation::Instruction(Opcode::RDMSR), OperandCode::Nothing),
            0x33 => OpcodeRecord(Interpretation::Instruction(Opcode::RDPMC), OperandCode::Nothing),
            0x34 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSENTER), OperandCode::Nothing),
            0x35 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSEXIT), OperandCode::Nothing),
            0x36 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x37 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x38 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x39 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x3b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

            0x40 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVO), OperandCode::Gv_Ev),
            0x41 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNO), OperandCode::Gv_Ev),
            0x42 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVB), OperandCode::Gv_Ev),
            0x43 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNB), OperandCode::Gv_Ev),
            0x44 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVZ), OperandCode::Gv_Ev),
            0x45 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNZ), OperandCode::Gv_Ev),
            0x46 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNA), OperandCode::Gv_Ev),
            0x47 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVA), OperandCode::Gv_Ev),
            0x48 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVS), OperandCode::Gv_Ev),
            0x49 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNS), OperandCode::Gv_Ev),
            0x4a => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVP), OperandCode::Gv_Ev),
            0x4b => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNP), OperandCode::Gv_Ev),
            0x4c => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVL), OperandCode::Gv_Ev),
            0x4d => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVGE), OperandCode::Gv_Ev),
            0x4e => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVLE), OperandCode::Gv_Ev),
            0x4f => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVG), OperandCode::Gv_Ev),

            0x50 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x51 => OpcodeRecord(Interpretation::Instruction(Opcode::SQRTSD), OperandCode::PMOVX_G_E_xmm),
            0x52 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x53 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x54 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x55 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x56 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x57 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x58 => OpcodeRecord(Interpretation::Instruction(Opcode::ADDSD), OperandCode::PMOVX_G_E_xmm),
            0x59 => OpcodeRecord(Interpretation::Instruction(Opcode::MULSD), OperandCode::PMOVX_G_E_xmm),
            0x5a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTSD2SS), OperandCode::PMOVX_G_E_xmm),
            0x5b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x5c => OpcodeRecord(Interpretation::Instruction(Opcode::SUBSD), OperandCode::PMOVX_G_E_xmm),
            0x5d => OpcodeRecord(Interpretation::Instruction(Opcode::MINSD), OperandCode::PMOVX_G_E_xmm),
            0x5e => OpcodeRecord(Interpretation::Instruction(Opcode::DIVSD), OperandCode::PMOVX_G_E_xmm),
            0x5f => OpcodeRecord(Interpretation::Instruction(Opcode::MAXSD), OperandCode::PMOVX_G_E_xmm),

            0x60 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x61 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x62 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x63 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x64 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x65 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x66 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x67 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x68 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x69 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

            0x70 => OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFLW), OperandCode::G_E_xmm_Ib),
            0x71 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // no f2-0f71 instructions, so we can stop early
            0x72 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // no f2-0f72 instructions, so we can stop early
            0x73 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // no f2-0f73 instructions, so we can stop early
            0x74 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x75 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x76 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x77 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x78 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf20f78),
            0x79 => OpcodeRecord(Interpretation::Instruction(Opcode::INSERTQ), OperandCode::G_U_xmm),
            0x7a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7c => OpcodeRecord(Interpretation::Instruction(Opcode::HADDPS), OperandCode::G_E_xmm),
            0x7d => OpcodeRecord(Interpretation::Instruction(Opcode::HSUBPS), OperandCode::G_E_xmm),
            0x7e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x80
            0x80 => OpcodeRecord(Interpretation::Instruction(Opcode::JO), OperandCode::Jvds),
            0x81 => OpcodeRecord(Interpretation::Instruction(Opcode::JNO), OperandCode::Jvds),
            0x82 => OpcodeRecord(Interpretation::Instruction(Opcode::JB), OperandCode::Jvds),
            0x83 => OpcodeRecord(Interpretation::Instruction(Opcode::JNB), OperandCode::Jvds),
            0x84 => OpcodeRecord(Interpretation::Instruction(Opcode::JZ), OperandCode::Jvds),
            0x85 => OpcodeRecord(Interpretation::Instruction(Opcode::JNZ), OperandCode::Jvds),
            0x86 => OpcodeRecord(Interpretation::Instruction(Opcode::JNA), OperandCode::Jvds),
            0x87 => OpcodeRecord(Interpretation::Instruction(Opcode::JA), OperandCode::Jvds),
            0x88 => OpcodeRecord(Interpretation::Instruction(Opcode::JS), OperandCode::Jvds),
            0x89 => OpcodeRecord(Interpretation::Instruction(Opcode::JNS), OperandCode::Jvds),
            0x8a => OpcodeRecord(Interpretation::Instruction(Opcode::JP), OperandCode::Jvds),
            0x8b => OpcodeRecord(Interpretation::Instruction(Opcode::JNP), OperandCode::Jvds),
            0x8c => OpcodeRecord(Interpretation::Instruction(Opcode::JL), OperandCode::Jvds),
            0x8d => OpcodeRecord(Interpretation::Instruction(Opcode::JGE), OperandCode::Jvds),
            0x8e => OpcodeRecord(Interpretation::Instruction(Opcode::JLE), OperandCode::Jvds),
            0x8f => OpcodeRecord(Interpretation::Instruction(Opcode::JG), OperandCode::Jvds),

// 0x90
            0x90 => OpcodeRecord(Interpretation::Instruction(Opcode::SETO), OperandCode::Eb_R0),
            0x91 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNO), OperandCode::Eb_R0),
            0x92 => OpcodeRecord(Interpretation::Instruction(Opcode::SETB), OperandCode::Eb_R0),
            0x93 => OpcodeRecord(Interpretation::Instruction(Opcode::SETAE), OperandCode::Eb_R0),
            0x94 => OpcodeRecord(Interpretation::Instruction(Opcode::SETZ), OperandCode::Eb_R0),
            0x95 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNZ), OperandCode::Eb_R0),
            0x96 => OpcodeRecord(Interpretation::Instruction(Opcode::SETBE), OperandCode::Eb_R0),
            0x97 => OpcodeRecord(Interpretation::Instruction(Opcode::SETA), OperandCode::Eb_R0),
            0x98 => OpcodeRecord(Interpretation::Instruction(Opcode::SETS), OperandCode::Eb_R0),
            0x99 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNS), OperandCode::Eb_R0),
            0x9a => OpcodeRecord(Interpretation::Instruction(Opcode::SETP), OperandCode::Eb_R0),
            0x9b => OpcodeRecord(Interpretation::Instruction(Opcode::SETNP), OperandCode::Eb_R0),
            0x9c => OpcodeRecord(Interpretation::Instruction(Opcode::SETL), OperandCode::Eb_R0),
            0x9d => OpcodeRecord(Interpretation::Instruction(Opcode::SETGE), OperandCode::Eb_R0),
            0x9e => OpcodeRecord(Interpretation::Instruction(Opcode::SETLE), OperandCode::Eb_R0),
            0x9f => OpcodeRecord(Interpretation::Instruction(Opcode::SETG), OperandCode::Eb_R0),

// 0xa0
            0xa0 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::FS),
            0xa1 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::FS),
            0xa2 => OpcodeRecord(Interpretation::Instruction(Opcode::CPUID), OperandCode::Nothing),
            0xa3 => OpcodeRecord(Interpretation::Instruction(Opcode::BT), OperandCode::Ev_Gv),
            0xa4 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_Ib),
            0xa5 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_CL),
            0xa6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa8 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::GS),
            0xa9 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::GS),
            0xaa => OpcodeRecord(Interpretation::Instruction(Opcode::RSM), OperandCode::Nothing),
            0xab => OpcodeRecord(Interpretation::Instruction(Opcode::BTS), OperandCode::Ev_Gv),
            0xac => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_Ib),
            0xad => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_CL),
            0xae => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fae),
            0xaf => OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev),

// 0xb0
            0xb0 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Eb_Gb),
            0xb1 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Ev_Gv),
            0xb2 => OpcodeRecord(Interpretation::Instruction(Opcode::LSS), OperandCode::INV_Gv_M),
            0xb3 => OpcodeRecord(Interpretation::Instruction(Opcode::BTR), OperandCode::Ev_Gv),
            0xb4 => OpcodeRecord(Interpretation::Instruction(Opcode::LFS), OperandCode::INV_Gv_M),
            0xb5 => OpcodeRecord(Interpretation::Instruction(Opcode::LGS), OperandCode::INV_Gv_M),
            0xb6 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_b), OperandCode::Gv_Eb),
            0xb7 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_w), OperandCode::Gv_Ew),
            0xb8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xb9 => OpcodeRecord(Interpretation::Instruction(Opcode::UD1), OperandCode::Gv_Ev),
            0xba => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fba),
            0xbb => OpcodeRecord(Interpretation::Instruction(Opcode::BTC), OperandCode::Ev_Gv),
            0xbc => OpcodeRecord(Interpretation::Instruction(Opcode::BSF), OperandCode::Gv_Ev),
            0xbd => OpcodeRecord(Interpretation::Instruction(Opcode::BSR), OperandCode::Gv_Ev),
            0xbe => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_b), OperandCode::Gv_Eb),
            0xbf => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_w), OperandCode::Gv_Ew),
// 0xc0
            0xc0 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Eb_Gb),
            0xc1 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Ev_Gv),
            0xc2 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPSD), OperandCode::G_E_xmm_Ib),
            0xc3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fc7), // cmpxchg permits an f2 prefix, which is the only reason this entry is not `Nothing`
            0xc8 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R0),
            0xc9 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R1),
            0xca => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R2),
            0xcb => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R3),
            0xcc => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R4),
            0xcd => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R5),
            0xce => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R6),
            0xcf => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R7),

            0xd0 => OpcodeRecord(Interpretation::Instruction(Opcode::ADDSUBPS), OperandCode::G_E_xmm),
            0xd1 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd2 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd6 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQ2Q), OperandCode::U_mm_G_xmm),
            0xd7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd9 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xda => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdb => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdc => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdd => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xde => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdf => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xe0
            0xe0 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe1 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe2 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe6 => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPD2DQ), OperandCode::G_E_xmm),
            0xe7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe9 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xea => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xeb => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xec => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xed => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xee => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xef => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

            0xf0 => OpcodeRecord(Interpretation::Instruction(Opcode::LDDQU), OperandCode::G_M_xmm),
            0xf1 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf2 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf9 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfa => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfb => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfc => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfd => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfe => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xff => OpcodeRecord(Interpretation::Instruction(Opcode::UD0), OperandCode::Gd_Ed),
        }
    } else if prefixes.rep() {
        match opcode {
            0x00 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f00),
            0x01 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f01),
            0x02 => OpcodeRecord(Interpretation::Instruction(Opcode::LAR), OperandCode::Gv_Ew),
            0x03 => OpcodeRecord(Interpretation::Instruction(Opcode::LSL), OperandCode::Gv_Ew_LSL),
            0x04 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x05 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSCALL), OperandCode::Nothing),
            0x06 => OpcodeRecord(Interpretation::Instruction(Opcode::CLTS), OperandCode::Nothing),
            0x07 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSRET), OperandCode::Nothing),
            0x08 => OpcodeRecord(Interpretation::Instruction(Opcode::INVD), OperandCode::Nothing),
            0x09 => OpcodeRecord(Interpretation::Instruction(Opcode::WBINVD), OperandCode::Nothing),
            0x0a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0b => OpcodeRecord(Interpretation::Instruction(Opcode::UD2), OperandCode::Nothing),
            0x0c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0d),
            0x0e => OpcodeRecord(Interpretation::Instruction(Opcode::FEMMS), OperandCode::Nothing),
            0x0f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0f),

            0x10 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSS), OperandCode::G_Ed_xmm),
            0x11 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSS), OperandCode::Ed_G_xmm),
            0x12 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSLDUP), OperandCode::G_E_xmm),
            0x13 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x14 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x15 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x16 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSHDUP), OperandCode::G_E_xmm),
            0x17 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x18 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f18),
            0x19 => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1a => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1b => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1c => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1d => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1e => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1f => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),

            0x20 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Cq_0),
            0x21 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Dq_0),
            0x22 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Cq_Rq_0),
            0x23 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Dq_Rq_0),
            0x24 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x25 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x26 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x27 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x28 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x29 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x2a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTSI2SS), OperandCode::G_xmm_Ed),
            0x2b => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTSS), OperandCode::M_G_xmm),
            0x2c => OpcodeRecord(Interpretation::Instruction(Opcode::CVTTSS2SI), OperandCode::Gv_E_xmm),
            0x2d => OpcodeRecord(Interpretation::Instruction(Opcode::CVTSS2SI), OperandCode::Gv_E_xmm),
            0x2e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x2f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

            0x30 => OpcodeRecord(Interpretation::Instruction(Opcode::WRMSR), OperandCode::Nothing),
            0x31 => OpcodeRecord(Interpretation::Instruction(Opcode::RDTSC), OperandCode::Nothing),
            0x32 => OpcodeRecord(Interpretation::Instruction(Opcode::RDMSR), OperandCode::Nothing),
            0x33 => OpcodeRecord(Interpretation::Instruction(Opcode::RDPMC), OperandCode::Nothing),
            0x34 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSENTER), OperandCode::Nothing),
            0x35 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSEXIT), OperandCode::Nothing),
            0x36 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x37 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x38 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x39 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x3b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

            0x40 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVO), OperandCode::Gv_Ev),
            0x41 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNO), OperandCode::Gv_Ev),
            0x42 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVB), OperandCode::Gv_Ev),
            0x43 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNB), OperandCode::Gv_Ev),
            0x44 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVZ), OperandCode::Gv_Ev),
            0x45 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNZ), OperandCode::Gv_Ev),
            0x46 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNA), OperandCode::Gv_Ev),
            0x47 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVA), OperandCode::Gv_Ev),
            0x48 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVS), OperandCode::Gv_Ev),
            0x49 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNS), OperandCode::Gv_Ev),
            0x4a => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVP), OperandCode::Gv_Ev),
            0x4b => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNP), OperandCode::Gv_Ev),
            0x4c => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVL), OperandCode::Gv_Ev),
            0x4d => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVGE), OperandCode::Gv_Ev),
            0x4e => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVLE), OperandCode::Gv_Ev),
            0x4f => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVG), OperandCode::Gv_Ev),
// 0x50
            0x50 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x51 => OpcodeRecord(Interpretation::Instruction(Opcode::SQRTSS), OperandCode::G_Ed_xmm),
            0x52 => OpcodeRecord(Interpretation::Instruction(Opcode::RSQRTSS), OperandCode::G_Ed_xmm),
            0x53 => OpcodeRecord(Interpretation::Instruction(Opcode::RCPSS), OperandCode::G_Ed_xmm),
            0x54 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x55 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x56 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x57 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x58 => OpcodeRecord(Interpretation::Instruction(Opcode::ADDSS), OperandCode::G_Ed_xmm),
            0x59 => OpcodeRecord(Interpretation::Instruction(Opcode::MULSS), OperandCode::G_Ed_xmm),
            0x5a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTSS2SD), OperandCode::PMOVX_G_E_xmm),
            0x5b => OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPS2DQ), OperandCode::G_E_xmm),
            0x5c => OpcodeRecord(Interpretation::Instruction(Opcode::SUBSS), OperandCode::G_Ed_xmm),
            0x5d => OpcodeRecord(Interpretation::Instruction(Opcode::MINSS), OperandCode::G_Ed_xmm),
            0x5e => OpcodeRecord(Interpretation::Instruction(Opcode::DIVSS), OperandCode::G_Ed_xmm),
            0x5f => OpcodeRecord(Interpretation::Instruction(Opcode::MAXSS), OperandCode::G_Ed_xmm),
// 0x60
            0x60 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x61 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x62 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x63 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x64 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x65 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x66 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x67 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x68 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x69 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6f => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQU), OperandCode::G_E_xmm),
// 0x70
            0x70 => OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFHW), OperandCode::G_E_xmm_Ib),
            0x71 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // no f3-0f71 instructions, so we can stop early
            0x72 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // no f3-0f72 instructions, so we can stop early
            0x73 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // no f3-0f73 instructions, so we can stop early
            0x74 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x75 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x76 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x77 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x78 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x79 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7e => OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::MOVQ_f30f),
            0x7f => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQU), OperandCode::E_G_xmm),
// 0x80
            0x80 => OpcodeRecord(Interpretation::Instruction(Opcode::JO), OperandCode::Jvds),
            0x81 => OpcodeRecord(Interpretation::Instruction(Opcode::JNO), OperandCode::Jvds),
            0x82 => OpcodeRecord(Interpretation::Instruction(Opcode::JB), OperandCode::Jvds),
            0x83 => OpcodeRecord(Interpretation::Instruction(Opcode::JNB), OperandCode::Jvds),
            0x84 => OpcodeRecord(Interpretation::Instruction(Opcode::JZ), OperandCode::Jvds),
            0x85 => OpcodeRecord(Interpretation::Instruction(Opcode::JNZ), OperandCode::Jvds),
            0x86 => OpcodeRecord(Interpretation::Instruction(Opcode::JNA), OperandCode::Jvds),
            0x87 => OpcodeRecord(Interpretation::Instruction(Opcode::JA), OperandCode::Jvds),
            0x88 => OpcodeRecord(Interpretation::Instruction(Opcode::JS), OperandCode::Jvds),
            0x89 => OpcodeRecord(Interpretation::Instruction(Opcode::JNS), OperandCode::Jvds),
            0x8a => OpcodeRecord(Interpretation::Instruction(Opcode::JP), OperandCode::Jvds),
            0x8b => OpcodeRecord(Interpretation::Instruction(Opcode::JNP), OperandCode::Jvds),
            0x8c => OpcodeRecord(Interpretation::Instruction(Opcode::JL), OperandCode::Jvds),
            0x8d => OpcodeRecord(Interpretation::Instruction(Opcode::JGE), OperandCode::Jvds),
            0x8e => OpcodeRecord(Interpretation::Instruction(Opcode::JLE), OperandCode::Jvds),
            0x8f => OpcodeRecord(Interpretation::Instruction(Opcode::JG), OperandCode::Jvds),

// 0x90
            0x90 => OpcodeRecord(Interpretation::Instruction(Opcode::SETO), OperandCode::Eb_R0),
            0x91 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNO), OperandCode::Eb_R0),
            0x92 => OpcodeRecord(Interpretation::Instruction(Opcode::SETB), OperandCode::Eb_R0),
            0x93 => OpcodeRecord(Interpretation::Instruction(Opcode::SETAE), OperandCode::Eb_R0),
            0x94 => OpcodeRecord(Interpretation::Instruction(Opcode::SETZ), OperandCode::Eb_R0),
            0x95 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNZ), OperandCode::Eb_R0),
            0x96 => OpcodeRecord(Interpretation::Instruction(Opcode::SETBE), OperandCode::Eb_R0),
            0x97 => OpcodeRecord(Interpretation::Instruction(Opcode::SETA), OperandCode::Eb_R0),
            0x98 => OpcodeRecord(Interpretation::Instruction(Opcode::SETS), OperandCode::Eb_R0),
            0x99 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNS), OperandCode::Eb_R0),
            0x9a => OpcodeRecord(Interpretation::Instruction(Opcode::SETP), OperandCode::Eb_R0),
            0x9b => OpcodeRecord(Interpretation::Instruction(Opcode::SETNP), OperandCode::Eb_R0),
            0x9c => OpcodeRecord(Interpretation::Instruction(Opcode::SETL), OperandCode::Eb_R0),
            0x9d => OpcodeRecord(Interpretation::Instruction(Opcode::SETGE), OperandCode::Eb_R0),
            0x9e => OpcodeRecord(Interpretation::Instruction(Opcode::SETLE), OperandCode::Eb_R0),
            0x9f => OpcodeRecord(Interpretation::Instruction(Opcode::SETG), OperandCode::Eb_R0),

// 0xa0
            0xa0 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::FS),
            0xa1 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::FS),
            0xa2 => OpcodeRecord(Interpretation::Instruction(Opcode::CPUID), OperandCode::Nothing),
            0xa3 => OpcodeRecord(Interpretation::Instruction(Opcode::BT), OperandCode::Ev_Gv),
            0xa4 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_Ib),
            0xa5 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_CL),
            0xa6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa8 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::GS),
            0xa9 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::GS),
            0xaa => OpcodeRecord(Interpretation::Instruction(Opcode::RSM), OperandCode::Nothing),
            0xab => OpcodeRecord(Interpretation::Instruction(Opcode::BTS), OperandCode::Ev_Gv),
            0xac => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_Ib),
            0xad => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_CL),
            0xae => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fae),
            0xaf => OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev),

// 0xb0
            0xb0 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Eb_Gb),
            0xb1 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Ev_Gv),
            0xb2 => OpcodeRecord(Interpretation::Instruction(Opcode::LSS), OperandCode::INV_Gv_M),
            0xb3 => OpcodeRecord(Interpretation::Instruction(Opcode::BTR), OperandCode::Ev_Gv),
            0xb4 => OpcodeRecord(Interpretation::Instruction(Opcode::LFS), OperandCode::INV_Gv_M),
            0xb5 => OpcodeRecord(Interpretation::Instruction(Opcode::LGS), OperandCode::INV_Gv_M),
            0xb6 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_b), OperandCode::Gv_Eb),
            0xb7 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_w), OperandCode::Gv_Ew),
            0xb8 => OpcodeRecord(Interpretation::Instruction(Opcode::POPCNT), OperandCode::Gv_Ev),
            0xb9 => OpcodeRecord(Interpretation::Instruction(Opcode::UD1), OperandCode::Gv_Ev),
            0xba => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fba),
            0xbb => OpcodeRecord(Interpretation::Instruction(Opcode::BTC), OperandCode::Ev_Gv),
            0xbc => OpcodeRecord(Interpretation::Instruction(Opcode::TZCNT), OperandCode::Gv_Ev),
            0xbd => OpcodeRecord(Interpretation::Instruction(Opcode::LZCNT), OperandCode::Gv_Ev),
            0xbe => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_b), OperandCode::Gv_Eb),
            0xbf => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_w), OperandCode::Gv_Ew),
// 0xc0
            0xc0 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Eb_Gb),
            0xc1 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Ev_Gv),
            0xc2 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPSS), OperandCode::G_E_xmm_Ib),
            0xc3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fc7),
            0xc8 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R0),
            0xc9 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R1),
            0xca => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R2),
            0xcb => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R3),
            0xcc => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R4),
            0xcd => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R5),
            0xce => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R6),
            0xcf => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R7),

            0xd0 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd1 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd2 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd6 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ2DQ), OperandCode::G_xmm_U_mm),
            0xd7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd9 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xda => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdb => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdc => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdd => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xde => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xdf => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xe0
            0xe0 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe1 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe2 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe6 => OpcodeRecord(Interpretation::Instruction(Opcode::CVTDQ2PD), OperandCode::G_E_xmm),
            0xe7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe9 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xea => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xeb => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xec => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xed => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xee => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xef => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xf0
            0xf0 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf1 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf2 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf4 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf5 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf9 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfa => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfb => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfc => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfd => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xfe => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xff => OpcodeRecord(Interpretation::Instruction(Opcode::UD0), OperandCode::Gd_Ed),
        }
    } else if prefixes.operand_size() {
        match opcode {
            0x00 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f00),
            0x01 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f01),
            0x02 => OpcodeRecord(Interpretation::Instruction(Opcode::LAR), OperandCode::Gv_Ew),
            0x03 => OpcodeRecord(Interpretation::Instruction(Opcode::LSL), OperandCode::Gv_Ew_LSL),
            0x04 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x05 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSCALL), OperandCode::Nothing),
            0x06 => OpcodeRecord(Interpretation::Instruction(Opcode::CLTS), OperandCode::Nothing),
            0x07 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSRET), OperandCode::Nothing),
            0x08 => OpcodeRecord(Interpretation::Instruction(Opcode::INVD), OperandCode::Nothing),
            0x09 => OpcodeRecord(Interpretation::Instruction(Opcode::WBINVD), OperandCode::Nothing),
            0x0a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0b => OpcodeRecord(Interpretation::Instruction(Opcode::UD2), OperandCode::Nothing),
            0x0c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0d),
            0x0e => OpcodeRecord(Interpretation::Instruction(Opcode::FEMMS), OperandCode::Nothing),
            0x0f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0f),

            0x10 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPD), OperandCode::G_E_xmm),
            0x11 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPD), OperandCode::E_G_xmm),
            0x12 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVLPD), OperandCode::PMOVX_G_E_xmm),
            0x13 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVLPD), OperandCode::PMOVX_E_G_xmm),
            0x14 => OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKLPD), OperandCode::G_E_xmm),
            0x15 => OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKHPD), OperandCode::G_E_xmm),
            0x16 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVHPD), OperandCode::PMOVX_G_E_xmm),
            0x17 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVHPD), OperandCode::PMOVX_E_G_xmm),
            0x18 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f18),
            0x19 => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1a => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1b => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1c => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1d => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1e => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1f => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),

            0x20 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Cq_0),
            0x21 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Dq_0),
            0x22 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Cq_Rq_0),
            0x23 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Dq_Rq_0),
            0x24 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x25 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x26 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x27 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x28 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPD), OperandCode::G_E_xmm),
            0x29 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPD), OperandCode::E_G_xmm),
            0x2a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPI2PD), OperandCode::G_xmm_E_mm),
            0x2b => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTPD), OperandCode::M_G_xmm),
            0x2c => OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPD2PI), OperandCode::G_mm_E_xmm),
            0x2d => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPD2PI), OperandCode::G_mm_E_xmm),
            0x2e => OpcodeRecord(Interpretation::Instruction(Opcode::UCOMISD), OperandCode::PMOVX_G_E_xmm),
            0x2f => OpcodeRecord(Interpretation::Instruction(Opcode::COMISD), OperandCode::PMOVX_G_E_xmm),

            0x30 => OpcodeRecord(Interpretation::Instruction(Opcode::WRMSR), OperandCode::Nothing),
            0x31 => OpcodeRecord(Interpretation::Instruction(Opcode::RDTSC), OperandCode::Nothing),
            0x32 => OpcodeRecord(Interpretation::Instruction(Opcode::RDMSR), OperandCode::Nothing),
            0x33 => OpcodeRecord(Interpretation::Instruction(Opcode::RDPMC), OperandCode::Nothing),
            0x34 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSENTER), OperandCode::Nothing),
            0x35 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSEXIT), OperandCode::Nothing),
            0x36 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x37 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x38 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x39 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x3b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

            0x40 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVO), OperandCode::Gv_Ev),
            0x41 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNO), OperandCode::Gv_Ev),
            0x42 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVB), OperandCode::Gv_Ev),
            0x43 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNB), OperandCode::Gv_Ev),
            0x44 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVZ), OperandCode::Gv_Ev),
            0x45 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNZ), OperandCode::Gv_Ev),
            0x46 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNA), OperandCode::Gv_Ev),
            0x47 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVA), OperandCode::Gv_Ev),
            0x48 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVS), OperandCode::Gv_Ev),
            0x49 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNS), OperandCode::Gv_Ev),
            0x4a => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVP), OperandCode::Gv_Ev),
            0x4b => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNP), OperandCode::Gv_Ev),
            0x4c => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVL), OperandCode::Gv_Ev),
            0x4d => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVGE), OperandCode::Gv_Ev),
            0x4e => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVLE), OperandCode::Gv_Ev),
            0x4f => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVG), OperandCode::Gv_Ev),
            0x50 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVMSKPD), OperandCode::Gd_U_xmm),
            0x51 => OpcodeRecord(Interpretation::Instruction(Opcode::SQRTPD), OperandCode::G_E_xmm),
            0x52 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x53 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x54 => OpcodeRecord(Interpretation::Instruction(Opcode::ANDPD), OperandCode::G_E_xmm),
            0x55 => OpcodeRecord(Interpretation::Instruction(Opcode::ANDNPD), OperandCode::G_E_xmm),
            0x56 => OpcodeRecord(Interpretation::Instruction(Opcode::ORPD), OperandCode::G_E_xmm),
            0x57 => OpcodeRecord(Interpretation::Instruction(Opcode::XORPD), OperandCode::G_E_xmm),
            0x58 => OpcodeRecord(Interpretation::Instruction(Opcode::ADDPD), OperandCode::G_E_xmm),
            0x59 => OpcodeRecord(Interpretation::Instruction(Opcode::MULPD), OperandCode::G_E_xmm),
            0x5a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPD2PS), OperandCode::G_E_xmm),
            0x5b => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPS2DQ), OperandCode::G_E_xmm),
            0x5c => OpcodeRecord(Interpretation::Instruction(Opcode::SUBPD), OperandCode::G_E_xmm),
            0x5d => OpcodeRecord(Interpretation::Instruction(Opcode::MINPD), OperandCode::G_E_xmm),
            0x5e => OpcodeRecord(Interpretation::Instruction(Opcode::DIVPD), OperandCode::G_E_xmm),
            0x5f => OpcodeRecord(Interpretation::Instruction(Opcode::MAXPD), OperandCode::G_E_xmm),
            0x60 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLBW), OperandCode::G_E_xmm),
            0x61 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLWD), OperandCode::G_E_xmm),
            0x62 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLDQ), OperandCode::G_E_xmm),
            0x63 => OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSWB), OperandCode::G_E_xmm),
            0x64 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTB), OperandCode::G_E_xmm),
            0x65 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTW), OperandCode::G_E_xmm),
            0x66 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTD), OperandCode::G_E_xmm),
            0x67 => OpcodeRecord(Interpretation::Instruction(Opcode::PACKUSWB), OperandCode::G_E_xmm),
            0x68 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHBW), OperandCode::G_E_xmm),
            0x69 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHWD), OperandCode::G_E_xmm),
            0x6a => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHDQ), OperandCode::G_E_xmm),
            0x6b => OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSDW), OperandCode::G_E_xmm),
            0x6c => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLQDQ), OperandCode::G_E_xmm),
            0x6d => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHQDQ), OperandCode::G_E_xmm),
            0x6e => OpcodeRecord(Interpretation::Instruction(Opcode::MOVD), OperandCode::G_xmm_Ed),
            0x6f => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQA), OperandCode::G_E_xmm),
            0x70 => OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFD), OperandCode::G_E_xmm_Ib),
            0x71 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f71),
            0x72 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f72),
            0x73 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f73),
            0x74 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQB), OperandCode::G_E_xmm),
            0x75 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQW), OperandCode::G_E_xmm),
            0x76 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQD), OperandCode::G_E_xmm),
            0x77 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x78 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660f78),
            0x79 => OpcodeRecord(Interpretation::Instruction(Opcode::EXTRQ), OperandCode::G_U_xmm),
            0x7a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7c => OpcodeRecord(Interpretation::Instruction(Opcode::HADDPD), OperandCode::G_E_xmm),
            0x7d => OpcodeRecord(Interpretation::Instruction(Opcode::HSUBPD), OperandCode::G_E_xmm),
            0x7e => OpcodeRecord(Interpretation::Instruction(Opcode::MOVD), OperandCode::Ed_G_xmm),
            0x7f => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQA), OperandCode::E_G_xmm),
// 0x80
            0x80 => OpcodeRecord(Interpretation::Instruction(Opcode::JO), OperandCode::Jvds),
            0x81 => OpcodeRecord(Interpretation::Instruction(Opcode::JNO), OperandCode::Jvds),
            0x82 => OpcodeRecord(Interpretation::Instruction(Opcode::JB), OperandCode::Jvds),
            0x83 => OpcodeRecord(Interpretation::Instruction(Opcode::JNB), OperandCode::Jvds),
            0x84 => OpcodeRecord(Interpretation::Instruction(Opcode::JZ), OperandCode::Jvds),
            0x85 => OpcodeRecord(Interpretation::Instruction(Opcode::JNZ), OperandCode::Jvds),
            0x86 => OpcodeRecord(Interpretation::Instruction(Opcode::JNA), OperandCode::Jvds),
            0x87 => OpcodeRecord(Interpretation::Instruction(Opcode::JA), OperandCode::Jvds),
            0x88 => OpcodeRecord(Interpretation::Instruction(Opcode::JS), OperandCode::Jvds),
            0x89 => OpcodeRecord(Interpretation::Instruction(Opcode::JNS), OperandCode::Jvds),
            0x8a => OpcodeRecord(Interpretation::Instruction(Opcode::JP), OperandCode::Jvds),
            0x8b => OpcodeRecord(Interpretation::Instruction(Opcode::JNP), OperandCode::Jvds),
            0x8c => OpcodeRecord(Interpretation::Instruction(Opcode::JL), OperandCode::Jvds),
            0x8d => OpcodeRecord(Interpretation::Instruction(Opcode::JGE), OperandCode::Jvds),
            0x8e => OpcodeRecord(Interpretation::Instruction(Opcode::JLE), OperandCode::Jvds),
            0x8f => OpcodeRecord(Interpretation::Instruction(Opcode::JG), OperandCode::Jvds),

// 0x90
            0x90 => OpcodeRecord(Interpretation::Instruction(Opcode::SETO), OperandCode::Eb_R0),
            0x91 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNO), OperandCode::Eb_R0),
            0x92 => OpcodeRecord(Interpretation::Instruction(Opcode::SETB), OperandCode::Eb_R0),
            0x93 => OpcodeRecord(Interpretation::Instruction(Opcode::SETAE), OperandCode::Eb_R0),
            0x94 => OpcodeRecord(Interpretation::Instruction(Opcode::SETZ), OperandCode::Eb_R0),
            0x95 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNZ), OperandCode::Eb_R0),
            0x96 => OpcodeRecord(Interpretation::Instruction(Opcode::SETBE), OperandCode::Eb_R0),
            0x97 => OpcodeRecord(Interpretation::Instruction(Opcode::SETA), OperandCode::Eb_R0),
            0x98 => OpcodeRecord(Interpretation::Instruction(Opcode::SETS), OperandCode::Eb_R0),
            0x99 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNS), OperandCode::Eb_R0),
            0x9a => OpcodeRecord(Interpretation::Instruction(Opcode::SETP), OperandCode::Eb_R0),
            0x9b => OpcodeRecord(Interpretation::Instruction(Opcode::SETNP), OperandCode::Eb_R0),
            0x9c => OpcodeRecord(Interpretation::Instruction(Opcode::SETL), OperandCode::Eb_R0),
            0x9d => OpcodeRecord(Interpretation::Instruction(Opcode::SETGE), OperandCode::Eb_R0),
            0x9e => OpcodeRecord(Interpretation::Instruction(Opcode::SETLE), OperandCode::Eb_R0),
            0x9f => OpcodeRecord(Interpretation::Instruction(Opcode::SETG), OperandCode::Eb_R0),

// 0xa0
            0xa0 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::FS),
            0xa1 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::FS),
            0xa2 => OpcodeRecord(Interpretation::Instruction(Opcode::CPUID), OperandCode::Nothing),
            0xa3 => OpcodeRecord(Interpretation::Instruction(Opcode::BT), OperandCode::Ev_Gv),
            0xa4 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_Ib),
            0xa5 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_CL),
            0xa6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa8 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::GS),
            0xa9 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::GS),
            0xaa => OpcodeRecord(Interpretation::Instruction(Opcode::RSM), OperandCode::Nothing),
            0xab => OpcodeRecord(Interpretation::Instruction(Opcode::BTS), OperandCode::Ev_Gv),
            0xac => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_Ib),
            0xad => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_CL),
            0xae => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fae),
            0xaf => OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev),

// 0xb0
            0xb0 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Eb_Gb),
            0xb1 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Ev_Gv),
            0xb2 => OpcodeRecord(Interpretation::Instruction(Opcode::LSS), OperandCode::Gv_M),
            0xb3 => OpcodeRecord(Interpretation::Instruction(Opcode::BTR), OperandCode::Ev_Gv),
            0xb4 => OpcodeRecord(Interpretation::Instruction(Opcode::LFS), OperandCode::Gv_M),
            0xb5 => OpcodeRecord(Interpretation::Instruction(Opcode::LGS), OperandCode::Gv_M),
            0xb6 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_b), OperandCode::Gv_Eb),
            0xb7 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_w), OperandCode::Gv_Ew),
            0xb8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xb9 => OpcodeRecord(Interpretation::Instruction(Opcode::UD1), OperandCode::Gv_Ev),
            0xba => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fba),
            0xbb => OpcodeRecord(Interpretation::Instruction(Opcode::BTC), OperandCode::Ev_Gv),
            0xbc => OpcodeRecord(Interpretation::Instruction(Opcode::BSF), OperandCode::Gv_Ev),
            0xbd => OpcodeRecord(Interpretation::Instruction(Opcode::BSR), OperandCode::Gv_Ev),
            0xbe => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_b), OperandCode::Gv_Eb),
            0xbf => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_w), OperandCode::Gv_Ew),
// 0xc0
            0xc0 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Eb_Gb),
            0xc1 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Ev_Gv),
            0xc2 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPPD), OperandCode::G_E_xmm_Ib),
            0xc3 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xc4 => OpcodeRecord(Interpretation::Instruction(Opcode::PINSRW), OperandCode::G_xmm_Ew_Ib),
            0xc5 => OpcodeRecord(Interpretation::Instruction(Opcode::PEXTRW), OperandCode::G_U_xmm_Ub),
            0xc6 => OpcodeRecord(Interpretation::Instruction(Opcode::SHUFPD), OperandCode::G_E_xmm_Ib),
            0xc7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fc7),
            0xc8 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R0),
            0xc9 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R1),
            0xca => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R2),
            0xcb => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R3),
            0xcc => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R4),
            0xcd => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R5),
            0xce => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R6),
            0xcf => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R7),
// 0xd0
            0xd0 => OpcodeRecord(Interpretation::Instruction(Opcode::ADDSUBPD), OperandCode::G_E_xmm),
            0xd1 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRLW), OperandCode::G_E_xmm),
            0xd2 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRLD), OperandCode::G_E_xmm),
            0xd3 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRLQ), OperandCode::G_E_xmm),
            0xd4 => OpcodeRecord(Interpretation::Instruction(Opcode::PADDQ), OperandCode::G_E_xmm),
            0xd5 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULLW), OperandCode::G_E_xmm),
            0xd6 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::PMOVX_E_G_xmm),
            0xd7 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVMSKB), OperandCode::Gd_U_xmm),
            0xd8 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSB), OperandCode::G_E_xmm),
            0xd9 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSW), OperandCode::G_E_xmm),
            0xda => OpcodeRecord(Interpretation::Instruction(Opcode::PMINUB), OperandCode::G_E_xmm),
            0xdb => OpcodeRecord(Interpretation::Instruction(Opcode::PAND), OperandCode::G_E_xmm),
            0xdc => OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSB), OperandCode::G_E_xmm),
            0xdd => OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSW), OperandCode::G_E_xmm),
            0xde => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXUB), OperandCode::G_E_xmm),
            0xdf => OpcodeRecord(Interpretation::Instruction(Opcode::PANDN), OperandCode::G_E_xmm),
// 0xe0
            0xe0 => OpcodeRecord(Interpretation::Instruction(Opcode::PAVGB), OperandCode::G_E_xmm),
            0xe1 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRAW), OperandCode::G_E_xmm),
            0xe2 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRAD), OperandCode::G_E_xmm),
            0xe3 => OpcodeRecord(Interpretation::Instruction(Opcode::PAVGW), OperandCode::G_E_xmm),
            0xe4 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULHUW), OperandCode::G_E_xmm),
            0xe5 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULHW), OperandCode::G_E_xmm),
            0xe6 => OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPD2DQ), OperandCode::G_E_xmm),
            0xe7 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTDQ), OperandCode::M_G_xmm),
            0xe8 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSB), OperandCode::G_E_xmm),
            0xe9 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSW), OperandCode::G_E_xmm),
            0xea => OpcodeRecord(Interpretation::Instruction(Opcode::PMINSW), OperandCode::G_E_xmm),
            0xeb => OpcodeRecord(Interpretation::Instruction(Opcode::POR), OperandCode::G_E_xmm),
            0xec => OpcodeRecord(Interpretation::Instruction(Opcode::PADDSB), OperandCode::G_E_xmm),
            0xed => OpcodeRecord(Interpretation::Instruction(Opcode::PADDSW), OperandCode::G_E_xmm),
            0xee => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXSW), OperandCode::G_E_xmm),
            0xef => OpcodeRecord(Interpretation::Instruction(Opcode::PXOR), OperandCode::G_E_xmm),
// 0xf0
            0xf0 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf1 => OpcodeRecord(Interpretation::Instruction(Opcode::PSLLW), OperandCode::G_E_xmm),
            0xf2 => OpcodeRecord(Interpretation::Instruction(Opcode::PSLLD), OperandCode::G_E_xmm),
            0xf3 => OpcodeRecord(Interpretation::Instruction(Opcode::PSLLQ), OperandCode::G_E_xmm),
            0xf4 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULUDQ), OperandCode::G_E_xmm),
            0xf5 => OpcodeRecord(Interpretation::Instruction(Opcode::PMADDWD), OperandCode::G_E_xmm),
            0xf6 => OpcodeRecord(Interpretation::Instruction(Opcode::PSADBW), OperandCode::G_E_xmm),
            0xf7 => OpcodeRecord(Interpretation::Instruction(Opcode::MASKMOVDQU), OperandCode::G_U_xmm),
            0xf8 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBB), OperandCode::G_E_xmm),
            0xf9 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBW), OperandCode::G_E_xmm),
            0xfa => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBD), OperandCode::G_E_xmm),
            0xfb => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBQ), OperandCode::G_E_xmm),
            0xfc => OpcodeRecord(Interpretation::Instruction(Opcode::PADDB), OperandCode::G_E_xmm),
            0xfd => OpcodeRecord(Interpretation::Instruction(Opcode::PADDW), OperandCode::G_E_xmm),
            0xfe => OpcodeRecord(Interpretation::Instruction(Opcode::PADDD), OperandCode::G_E_xmm),
            0xff => OpcodeRecord(Interpretation::Instruction(Opcode::UD0), OperandCode::Gd_Ed),
        }
    } else {
        match opcode {
            0x00 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f00),
            0x01 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f01),
            0x02 => OpcodeRecord(Interpretation::Instruction(Opcode::LAR), OperandCode::Gv_Ew),
            0x03 => OpcodeRecord(Interpretation::Instruction(Opcode::LSL), OperandCode::Gv_Ew_LSL),
            0x04 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x05 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSCALL), OperandCode::Nothing),
            0x06 => OpcodeRecord(Interpretation::Instruction(Opcode::CLTS), OperandCode::Nothing),
            0x07 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSRET), OperandCode::Nothing),
            0x08 => OpcodeRecord(Interpretation::Instruction(Opcode::INVD), OperandCode::Nothing),
            0x09 => OpcodeRecord(Interpretation::Instruction(Opcode::WBINVD), OperandCode::Nothing),
            0x0a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0b => OpcodeRecord(Interpretation::Instruction(Opcode::UD2), OperandCode::Nothing),
            0x0c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x0d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0d),
            0x0e => OpcodeRecord(Interpretation::Instruction(Opcode::FEMMS), OperandCode::Nothing),
            0x0f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0f),

            0x10 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPS), OperandCode::G_E_xmm),
            0x11 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPS), OperandCode::E_G_xmm),
            0x12 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f12),
            0x13 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVLPS), OperandCode::M_G_xmm),
            0x14 => OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKLPS), OperandCode::G_E_xmm),
            0x15 => OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKHPS), OperandCode::G_E_xmm),
            0x16 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f16),
            0x17 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVHPS), OperandCode::PMOVX_E_G_xmm),
            0x18 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f18),
            0x19 => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1a => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1b => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1c => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1d => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1e => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
            0x1f => OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),

            0x20 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Cq_0),
            0x21 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Dq_0),
            0x22 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Cq_Rq_0),
            0x23 => OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Dq_Rq_0),
            0x24 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x25 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x26 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x27 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x28 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPS), OperandCode::G_E_xmm),
            0x29 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPS), OperandCode::E_G_xmm),
            0x2a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPI2PS), OperandCode::G_xmm_E_mm),
            0x2b => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTPS), OperandCode::M_G_xmm),
            0x2c => OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPS2PI), OperandCode::G_mm_E_xmm),
            0x2d => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPS2PI), OperandCode::G_mm_E_xmm),
            0x2e => OpcodeRecord(Interpretation::Instruction(Opcode::UCOMISS), OperandCode::PMOVX_G_E_xmm),
            0x2f => OpcodeRecord(Interpretation::Instruction(Opcode::COMISS), OperandCode::PMOVX_G_E_xmm),
// 0x30
            0x30 => OpcodeRecord(Interpretation::Instruction(Opcode::WRMSR), OperandCode::Nothing),
            0x31 => OpcodeRecord(Interpretation::Instruction(Opcode::RDTSC), OperandCode::Nothing),
            0x32 => OpcodeRecord(Interpretation::Instruction(Opcode::RDMSR), OperandCode::Nothing),
            0x33 => OpcodeRecord(Interpretation::Instruction(Opcode::RDPMC), OperandCode::Nothing),
            0x34 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSENTER), OperandCode::Nothing),
            0x35 => OpcodeRecord(Interpretation::Instruction(Opcode::SYSEXIT), OperandCode::Nothing),
            0x36 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x37 => OpcodeRecord(Interpretation::Instruction(Opcode::GETSEC), OperandCode::Nothing),
            0x38 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x39 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // handled before getting to `read_0f_opcode`
            0x3b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3e => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x3f => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

// 0x40
            0x40 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVO), OperandCode::Gv_Ev),
            0x41 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNO), OperandCode::Gv_Ev),
            0x42 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVB), OperandCode::Gv_Ev),
            0x43 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNB), OperandCode::Gv_Ev),
            0x44 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVZ), OperandCode::Gv_Ev),
            0x45 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNZ), OperandCode::Gv_Ev),
            0x46 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNA), OperandCode::Gv_Ev),
            0x47 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVA), OperandCode::Gv_Ev),
            0x48 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVS), OperandCode::Gv_Ev),
            0x49 => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNS), OperandCode::Gv_Ev),
            0x4a => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVP), OperandCode::Gv_Ev),
            0x4b => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNP), OperandCode::Gv_Ev),
            0x4c => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVL), OperandCode::Gv_Ev),
            0x4d => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVGE), OperandCode::Gv_Ev),
            0x4e => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVLE), OperandCode::Gv_Ev),
            0x4f => OpcodeRecord(Interpretation::Instruction(Opcode::CMOVG), OperandCode::Gv_Ev),

// 0x50
            0x50 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVMSKPS), OperandCode::Gd_U_xmm),
            0x51 => OpcodeRecord(Interpretation::Instruction(Opcode::SQRTPS), OperandCode::G_E_xmm),
            0x52 => OpcodeRecord(Interpretation::Instruction(Opcode::RSQRTPS), OperandCode::G_E_xmm),
            0x53 => OpcodeRecord(Interpretation::Instruction(Opcode::RCPPS), OperandCode::G_E_xmm),
            0x54 => OpcodeRecord(Interpretation::Instruction(Opcode::ANDPS), OperandCode::G_E_xmm),
            0x55 => OpcodeRecord(Interpretation::Instruction(Opcode::ANDNPS), OperandCode::G_E_xmm),
            0x56 => OpcodeRecord(Interpretation::Instruction(Opcode::ORPS), OperandCode::G_E_xmm),
            0x57 => OpcodeRecord(Interpretation::Instruction(Opcode::XORPS), OperandCode::G_E_xmm),
            0x58 => OpcodeRecord(Interpretation::Instruction(Opcode::ADDPS), OperandCode::G_E_xmm),
            0x59 => OpcodeRecord(Interpretation::Instruction(Opcode::MULPS), OperandCode::G_E_xmm),
            0x5a => OpcodeRecord(Interpretation::Instruction(Opcode::CVTPS2PD), OperandCode::PMOVX_G_E_xmm),
            0x5b => OpcodeRecord(Interpretation::Instruction(Opcode::CVTDQ2PS), OperandCode::G_E_xmm),
            0x5c => OpcodeRecord(Interpretation::Instruction(Opcode::SUBPS), OperandCode::G_E_xmm),
            0x5d => OpcodeRecord(Interpretation::Instruction(Opcode::MINPS), OperandCode::G_E_xmm),
            0x5e => OpcodeRecord(Interpretation::Instruction(Opcode::DIVPS), OperandCode::G_E_xmm),
            0x5f => OpcodeRecord(Interpretation::Instruction(Opcode::MAXPS), OperandCode::G_E_xmm),

// 0x60
            0x60 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLBW), OperandCode::G_E_mm),
            0x61 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLWD), OperandCode::G_E_mm),
            0x62 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLDQ), OperandCode::G_E_mm),
            0x63 => OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSWB), OperandCode::G_E_mm),
            0x64 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTB), OperandCode::G_E_mm),
            0x65 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTW), OperandCode::G_E_mm),
            0x66 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTD), OperandCode::G_E_mm),
            0x67 => OpcodeRecord(Interpretation::Instruction(Opcode::PACKUSWB), OperandCode::G_E_mm),
            0x68 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHBW), OperandCode::G_E_mm),
            0x69 => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHWD), OperandCode::G_E_mm),
            0x6a => OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHDQ), OperandCode::G_E_mm),
            0x6b => OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSDW), OperandCode::G_E_mm),
            0x6c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x6e => OpcodeRecord(Interpretation::Instruction(Opcode::MOVD), OperandCode::G_mm_Ed),
            0x6f => OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::G_mm_E),

// 0x70
            0x70 => OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFW), OperandCode::G_E_mm_Ib),
            0x71 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f71),
            0x72 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f72),
            0x73 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f73),
            0x74 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQB), OperandCode::G_E_mm),
            0x75 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQW), OperandCode::G_E_mm),
            0x76 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQD), OperandCode::G_E_mm),
            0x77 => OpcodeRecord(Interpretation::Instruction(Opcode::EMMS), OperandCode::Nothing),
            0x78 => OpcodeRecord(Interpretation::Instruction(Opcode::VMREAD), OperandCode::E_G_q),
            0x79 => OpcodeRecord(Interpretation::Instruction(Opcode::VMWRITE), OperandCode::G_E_q),
            0x7a => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7b => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7c => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7d => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0x7e => OpcodeRecord(Interpretation::Instruction(Opcode::MOVD), OperandCode::Ed_G_mm),
            0x7f => OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::E_G_mm),

// 0x80
            0x80 => OpcodeRecord(Interpretation::Instruction(Opcode::JO), OperandCode::Jvds),
            0x81 => OpcodeRecord(Interpretation::Instruction(Opcode::JNO), OperandCode::Jvds),
            0x82 => OpcodeRecord(Interpretation::Instruction(Opcode::JB), OperandCode::Jvds),
            0x83 => OpcodeRecord(Interpretation::Instruction(Opcode::JNB), OperandCode::Jvds),
            0x84 => OpcodeRecord(Interpretation::Instruction(Opcode::JZ), OperandCode::Jvds),
            0x85 => OpcodeRecord(Interpretation::Instruction(Opcode::JNZ), OperandCode::Jvds),
            0x86 => OpcodeRecord(Interpretation::Instruction(Opcode::JNA), OperandCode::Jvds),
            0x87 => OpcodeRecord(Interpretation::Instruction(Opcode::JA), OperandCode::Jvds),
            0x88 => OpcodeRecord(Interpretation::Instruction(Opcode::JS), OperandCode::Jvds),
            0x89 => OpcodeRecord(Interpretation::Instruction(Opcode::JNS), OperandCode::Jvds),
            0x8a => OpcodeRecord(Interpretation::Instruction(Opcode::JP), OperandCode::Jvds),
            0x8b => OpcodeRecord(Interpretation::Instruction(Opcode::JNP), OperandCode::Jvds),
            0x8c => OpcodeRecord(Interpretation::Instruction(Opcode::JL), OperandCode::Jvds),
            0x8d => OpcodeRecord(Interpretation::Instruction(Opcode::JGE), OperandCode::Jvds),
            0x8e => OpcodeRecord(Interpretation::Instruction(Opcode::JLE), OperandCode::Jvds),
            0x8f => OpcodeRecord(Interpretation::Instruction(Opcode::JG), OperandCode::Jvds),

// 0x90
            0x90 => OpcodeRecord(Interpretation::Instruction(Opcode::SETO), OperandCode::Eb_R0),
            0x91 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNO), OperandCode::Eb_R0),
            0x92 => OpcodeRecord(Interpretation::Instruction(Opcode::SETB), OperandCode::Eb_R0),
            0x93 => OpcodeRecord(Interpretation::Instruction(Opcode::SETAE), OperandCode::Eb_R0),
            0x94 => OpcodeRecord(Interpretation::Instruction(Opcode::SETZ), OperandCode::Eb_R0),
            0x95 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNZ), OperandCode::Eb_R0),
            0x96 => OpcodeRecord(Interpretation::Instruction(Opcode::SETBE), OperandCode::Eb_R0),
            0x97 => OpcodeRecord(Interpretation::Instruction(Opcode::SETA), OperandCode::Eb_R0),
            0x98 => OpcodeRecord(Interpretation::Instruction(Opcode::SETS), OperandCode::Eb_R0),
            0x99 => OpcodeRecord(Interpretation::Instruction(Opcode::SETNS), OperandCode::Eb_R0),
            0x9a => OpcodeRecord(Interpretation::Instruction(Opcode::SETP), OperandCode::Eb_R0),
            0x9b => OpcodeRecord(Interpretation::Instruction(Opcode::SETNP), OperandCode::Eb_R0),
            0x9c => OpcodeRecord(Interpretation::Instruction(Opcode::SETL), OperandCode::Eb_R0),
            0x9d => OpcodeRecord(Interpretation::Instruction(Opcode::SETGE), OperandCode::Eb_R0),
            0x9e => OpcodeRecord(Interpretation::Instruction(Opcode::SETLE), OperandCode::Eb_R0),
            0x9f => OpcodeRecord(Interpretation::Instruction(Opcode::SETG), OperandCode::Eb_R0),

// 0xa0
            0xa0 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::FS),
            0xa1 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::FS),
            0xa2 => OpcodeRecord(Interpretation::Instruction(Opcode::CPUID), OperandCode::Nothing),
            0xa3 => OpcodeRecord(Interpretation::Instruction(Opcode::BT), OperandCode::Ev_Gv),
            0xa4 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_Ib),
            0xa5 => OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_CL),
            0xa6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xa8 => OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::GS),
            0xa9 => OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::GS),
            0xaa => OpcodeRecord(Interpretation::Instruction(Opcode::RSM), OperandCode::Nothing),
            0xab => OpcodeRecord(Interpretation::Instruction(Opcode::BTS), OperandCode::Ev_Gv),
            0xac => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_Ib),
            0xad => OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_CL),
            0xae => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fae),
            0xaf => OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev),

// 0xb0
            0xb0 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Eb_Gb),
            0xb1 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Ev_Gv),
            0xb2 => OpcodeRecord(Interpretation::Instruction(Opcode::LSS), OperandCode::INV_Gv_M),
            0xb3 => OpcodeRecord(Interpretation::Instruction(Opcode::BTR), OperandCode::Ev_Gv),
            0xb4 => OpcodeRecord(Interpretation::Instruction(Opcode::LFS), OperandCode::INV_Gv_M),
            0xb5 => OpcodeRecord(Interpretation::Instruction(Opcode::LGS), OperandCode::INV_Gv_M),
            0xb6 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_b), OperandCode::Gv_Eb),
            0xb7 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_w), OperandCode::Gv_Ew),
            0xb8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // JMPE, ITANIUM
            0xb9 => OpcodeRecord(Interpretation::Instruction(Opcode::UD1), OperandCode::Gv_Ev),
            0xba => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fba),
            0xbb => OpcodeRecord(Interpretation::Instruction(Opcode::BTC), OperandCode::Ev_Gv),
            0xbc => OpcodeRecord(Interpretation::Instruction(Opcode::BSF), OperandCode::Gv_Ev),
            0xbd => OpcodeRecord(Interpretation::Instruction(Opcode::BSR), OperandCode::Gv_Ev),
            0xbe => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_b), OperandCode::Gv_Eb),
            0xbf => OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_w), OperandCode::Gv_Ew),

// 0xc0
            0xc0 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Eb_Gb),
            0xc1 => OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Ev_Gv),
            0xc2 => OpcodeRecord(Interpretation::Instruction(Opcode::CMPPS), OperandCode::G_E_xmm_Ib),
            0xc3 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTI), OperandCode::Md_Gd),
            0xc4 => OpcodeRecord(Interpretation::Instruction(Opcode::PINSRW), OperandCode::G_mm_Ew_Ib),
            0xc5 => OpcodeRecord(Interpretation::Instruction(Opcode::PEXTRW), OperandCode::Rv_Gmm_Ib),
            0xc6 => OpcodeRecord(Interpretation::Instruction(Opcode::SHUFPS), OperandCode::G_E_xmm_Ib),
            0xc7 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fc7),
            0xc8 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R0),
            0xc9 => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R1),
            0xca => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R2),
            0xcb => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R3),
            0xcc => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R4),
            0xcd => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R5),
            0xce => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R6),
            0xcf => OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R7),

// 0xd0
            0xd0 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd1 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRLW), OperandCode::G_E_mm),
            0xd2 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRLD), OperandCode::G_E_mm),
            0xd3 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRLQ), OperandCode::G_E_mm),
            0xd4 => OpcodeRecord(Interpretation::Instruction(Opcode::PADDQ), OperandCode::G_E_mm),
            0xd5 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULLW), OperandCode::G_E_mm),
            0xd6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xd7 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVMSKB), OperandCode::G_U_mm),
            0xd8 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSB), OperandCode::G_E_mm),
            0xd9 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSW), OperandCode::G_E_mm),
            0xda => OpcodeRecord(Interpretation::Instruction(Opcode::PMINUB), OperandCode::G_E_mm),
            0xdb => OpcodeRecord(Interpretation::Instruction(Opcode::PAND), OperandCode::G_E_mm),
            0xdc => OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSB), OperandCode::G_E_mm),
            0xdd => OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSW), OperandCode::G_E_mm),
            0xde => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXUB), OperandCode::G_E_mm),
            0xdf => OpcodeRecord(Interpretation::Instruction(Opcode::PANDN), OperandCode::G_E_mm),

// 0xe0
            0xe0 => OpcodeRecord(Interpretation::Instruction(Opcode::PAVGB), OperandCode::G_E_mm),
            0xe1 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRAW), OperandCode::G_E_mm),
            0xe2 => OpcodeRecord(Interpretation::Instruction(Opcode::PSRAD), OperandCode::G_E_mm),
            0xe3 => OpcodeRecord(Interpretation::Instruction(Opcode::PAVGW), OperandCode::G_E_mm),
            0xe4 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULHUW), OperandCode::G_E_mm),
            0xe5 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULHW), OperandCode::G_E_mm),
            0xe6 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xe7 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTQ), OperandCode::G_Mq_mm),
            0xe8 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSB), OperandCode::G_E_mm),
            0xe9 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSW), OperandCode::G_E_mm),
            0xea => OpcodeRecord(Interpretation::Instruction(Opcode::PMINSW), OperandCode::G_E_mm),
            0xeb => OpcodeRecord(Interpretation::Instruction(Opcode::POR), OperandCode::G_E_mm),
            0xec => OpcodeRecord(Interpretation::Instruction(Opcode::PADDSB), OperandCode::G_E_mm),
            0xed => OpcodeRecord(Interpretation::Instruction(Opcode::PADDSW), OperandCode::G_E_mm),
            0xee => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXSW), OperandCode::G_E_mm),
            0xef => OpcodeRecord(Interpretation::Instruction(Opcode::PXOR), OperandCode::G_E_mm),
// 0xf0
            0xf0 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
            0xf1 => OpcodeRecord(Interpretation::Instruction(Opcode::PSLLW), OperandCode::G_E_mm),
            0xf2 => OpcodeRecord(Interpretation::Instruction(Opcode::PSLLD), OperandCode::G_E_mm),
            0xf3 => OpcodeRecord(Interpretation::Instruction(Opcode::PSLLQ), OperandCode::G_E_mm),
            0xf4 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULUDQ), OperandCode::G_E_mm),
            0xf5 => OpcodeRecord(Interpretation::Instruction(Opcode::PMADDWD), OperandCode::G_E_mm),
            0xf6 => OpcodeRecord(Interpretation::Instruction(Opcode::PSADBW), OperandCode::G_E_mm),
            0xf7 => OpcodeRecord(Interpretation::Instruction(Opcode::MASKMOVQ), OperandCode::G_mm_U_mm),
            0xf8 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBB), OperandCode::G_E_mm),
            0xf9 => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBW), OperandCode::G_E_mm),
            0xfa => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBD), OperandCode::G_E_mm),
            0xfb => OpcodeRecord(Interpretation::Instruction(Opcode::PSUBQ), OperandCode::G_E_mm),
            0xfc => OpcodeRecord(Interpretation::Instruction(Opcode::PADDB), OperandCode::G_E_mm),
            0xfd => OpcodeRecord(Interpretation::Instruction(Opcode::PADDW), OperandCode::G_E_mm),
            0xfe => OpcodeRecord(Interpretation::Instruction(Opcode::PADDD), OperandCode::G_E_mm),
            0xff => OpcodeRecord(Interpretation::Instruction(Opcode::UD0), OperandCode::Gd_Ed),
        }
    }
}

fn read_0f38_opcode(opcode: u8, prefixes: &mut Prefixes) -> OpcodeRecord {
    if prefixes.rep() {
        return match opcode {
            0xd8 => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38d8),
            0xdc => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38dc),
            0xdd => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38dd),
            0xde => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38de),
            0xdf => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38df),
            0xf6 => OpcodeRecord(Interpretation::Instruction(Opcode::ADOX), OperandCode::Gv_Ev),
            0xf8 => {
                prefixes.unset_operand_size();
                OpcodeRecord(Interpretation::Instruction(Opcode::ENQCMDS), OperandCode::INV_Gv_M)
            },
            0xfa => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38fa),
            0xfb => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38fb),
            _ => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
        };
    }

    if prefixes.repnz() {
        return match opcode {
            0xf0 => OpcodeRecord(Interpretation::Instruction(Opcode::CRC32), OperandCode::Gv_Eb),
            0xf1 => OpcodeRecord(Interpretation::Instruction(Opcode::CRC32), OperandCode::Gd_Ev),
            0xf8 => {
                prefixes.unset_operand_size();
                OpcodeRecord(Interpretation::Instruction(Opcode::ENQCMD), OperandCode::INV_Gv_M)
            },
            _ => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
        };
    }

    if prefixes.operand_size() {
        // leave operand size present for `movbe`
        if opcode != 0xf0 && opcode != 0xf1 {
            prefixes.unset_operand_size();
        }

        return match opcode {
            0x00 => OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFB), OperandCode::G_E_xmm),
            0x01 => OpcodeRecord(Interpretation::Instruction(Opcode::PHADDW), OperandCode::G_E_xmm),
            0x02 => OpcodeRecord(Interpretation::Instruction(Opcode::PHADDD), OperandCode::G_E_xmm),
            0x03 => OpcodeRecord(Interpretation::Instruction(Opcode::PHADDSW), OperandCode::G_E_xmm),
            0x04 => OpcodeRecord(Interpretation::Instruction(Opcode::PMADDUBSW), OperandCode::G_E_xmm),
            0x05 => OpcodeRecord(Interpretation::Instruction(Opcode::PHSUBW), OperandCode::G_E_xmm),
            0x06 => OpcodeRecord(Interpretation::Instruction(Opcode::PHSUBD), OperandCode::G_E_xmm),
            0x07 => OpcodeRecord(Interpretation::Instruction(Opcode::PHSUBSW), OperandCode::G_E_xmm),
            0x08 => OpcodeRecord(Interpretation::Instruction(Opcode::PSIGNB), OperandCode::G_E_xmm),
            0x09 => OpcodeRecord(Interpretation::Instruction(Opcode::PSIGNW), OperandCode::G_E_xmm),
            0x0a => OpcodeRecord(Interpretation::Instruction(Opcode::PSIGND), OperandCode::G_E_xmm),
            0x0b => OpcodeRecord(Interpretation::Instruction(Opcode::PMULHRSW), OperandCode::G_E_xmm),
            0x10 => OpcodeRecord(Interpretation::Instruction(Opcode::PBLENDVB), OperandCode::G_E_xmm),
            0x14 => OpcodeRecord(Interpretation::Instruction(Opcode::BLENDVPS), OperandCode::G_E_xmm),
            0x15 => OpcodeRecord(Interpretation::Instruction(Opcode::BLENDVPD), OperandCode::G_E_xmm),
            0x17 => OpcodeRecord(Interpretation::Instruction(Opcode::PTEST), OperandCode::G_E_xmm),
            0x1c => OpcodeRecord(Interpretation::Instruction(Opcode::PABSB), OperandCode::G_E_xmm),
            0x1d => OpcodeRecord(Interpretation::Instruction(Opcode::PABSW), OperandCode::G_E_xmm),
            0x1e => OpcodeRecord(Interpretation::Instruction(Opcode::PABSD), OperandCode::G_E_xmm),
            0x20 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVSXBW), OperandCode::PMOVX_G_E_xmm),
            0x21 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVSXBD), OperandCode::PMOVX_G_E_xmm),
            0x22 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVSXBQ), OperandCode::PMOVX_G_E_xmm),
            0x23 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVSXWD), OperandCode::PMOVX_G_E_xmm),
            0x24 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVSXWQ), OperandCode::PMOVX_G_E_xmm),
            0x25 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVSXDQ), OperandCode::PMOVX_G_E_xmm),
            0x28 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULDQ), OperandCode::G_E_xmm),
            0x29 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQQ), OperandCode::G_E_xmm),
            0x2a => OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTDQA), OperandCode::G_M_xmm),
            0x2b => OpcodeRecord(Interpretation::Instruction(Opcode::PACKUSDW), OperandCode::G_E_xmm),
            0x30 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVZXBW), OperandCode::PMOVX_G_E_xmm),
            0x31 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVZXBD), OperandCode::PMOVX_G_E_xmm),
            0x32 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVZXBQ), OperandCode::PMOVX_G_E_xmm),
            0x33 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVZXWD), OperandCode::PMOVX_G_E_xmm),
            0x34 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVZXWQ), OperandCode::PMOVX_G_E_xmm),
            0x35 => OpcodeRecord(Interpretation::Instruction(Opcode::PMOVZXDQ), OperandCode::PMOVX_G_E_xmm),
            0x37 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTQ), OperandCode::G_E_xmm),
            0x38 => OpcodeRecord(Interpretation::Instruction(Opcode::PMINSB), OperandCode::G_E_xmm),
            0x39 => OpcodeRecord(Interpretation::Instruction(Opcode::PMINSD), OperandCode::G_E_xmm),
            0x3a => OpcodeRecord(Interpretation::Instruction(Opcode::PMINUW), OperandCode::G_E_xmm),
            0x3b => OpcodeRecord(Interpretation::Instruction(Opcode::PMINUD), OperandCode::G_E_xmm),
            0x3c => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXSB), OperandCode::G_E_xmm),
            0x3d => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXSD), OperandCode::G_E_xmm),
            0x3e => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXUW), OperandCode::G_E_xmm),
            0x3f => OpcodeRecord(Interpretation::Instruction(Opcode::PMAXUD), OperandCode::G_E_xmm),
            0x40 => OpcodeRecord(Interpretation::Instruction(Opcode::PMULLD), OperandCode::G_E_xmm),
            0x41 => OpcodeRecord(Interpretation::Instruction(Opcode::PHMINPOSUW), OperandCode::G_E_xmm),
            0x80 => OpcodeRecord(Interpretation::Instruction(Opcode::INVEPT), OperandCode::INV_Gv_M),
            0x81 => OpcodeRecord(Interpretation::Instruction(Opcode::INVVPID), OperandCode::INV_Gv_M),
            0x82 => OpcodeRecord(Interpretation::Instruction(Opcode::INVPCID), OperandCode::INV_Gv_M),
            0xcf => OpcodeRecord(Interpretation::Instruction(Opcode::GF2P8MULB), OperandCode::G_E_xmm),
            0xdb => OpcodeRecord(Interpretation::Instruction(Opcode::AESIMC), OperandCode::G_E_xmm),
            0xdc => OpcodeRecord(Interpretation::Instruction(Opcode::AESENC), OperandCode::G_E_xmm),
            0xdd => OpcodeRecord(Interpretation::Instruction(Opcode::AESENCLAST), OperandCode::G_E_xmm),
            0xde => OpcodeRecord(Interpretation::Instruction(Opcode::AESDEC), OperandCode::G_E_xmm),
            0xdf => OpcodeRecord(Interpretation::Instruction(Opcode::AESDECLAST), OperandCode::G_E_xmm),
            0xf0 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVBE), OperandCode::Gv_M),
            0xf1 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVBE), OperandCode::M_Gv),
            0xf5 => OpcodeRecord(Interpretation::Instruction(Opcode::WRUSS), OperandCode::Md_Gd),
            0xf6 => OpcodeRecord(Interpretation::Instruction(Opcode::ADCX), OperandCode::Gv_Ev),
            0xf8 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDIR64B), OperandCode::MOVDIR64B),
            _ => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
        };
    } else {
        return match opcode {
            0x00 => OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFB), OperandCode::G_E_mm),
            0x01 => OpcodeRecord(Interpretation::Instruction(Opcode::PHADDW), OperandCode::G_E_mm),
            0x02 => OpcodeRecord(Interpretation::Instruction(Opcode::PHADDD), OperandCode::G_E_mm),
            0x03 => OpcodeRecord(Interpretation::Instruction(Opcode::PHADDSW), OperandCode::G_E_mm),
            0x04 => OpcodeRecord(Interpretation::Instruction(Opcode::PMADDUBSW), OperandCode::G_E_mm),
            0x05 => OpcodeRecord(Interpretation::Instruction(Opcode::PHSUBW), OperandCode::G_E_mm),
            0x06 => OpcodeRecord(Interpretation::Instruction(Opcode::PHSUBD), OperandCode::G_E_mm),
            0x07 => OpcodeRecord(Interpretation::Instruction(Opcode::PHSUBSW), OperandCode::G_E_mm),
            0x08 => OpcodeRecord(Interpretation::Instruction(Opcode::PSIGNB), OperandCode::G_E_mm),
            0x09 => OpcodeRecord(Interpretation::Instruction(Opcode::PSIGNW), OperandCode::G_E_mm),
            0x0a => OpcodeRecord(Interpretation::Instruction(Opcode::PSIGND), OperandCode::G_E_mm),
            0x0b => OpcodeRecord(Interpretation::Instruction(Opcode::PMULHRSW), OperandCode::G_E_mm),
            0x1c => OpcodeRecord(Interpretation::Instruction(Opcode::PABSB), OperandCode::G_E_mm),
            0x1d => OpcodeRecord(Interpretation::Instruction(Opcode::PABSW), OperandCode::G_E_mm),
            0x1e => OpcodeRecord(Interpretation::Instruction(Opcode::PABSD), OperandCode::G_E_mm),
            0xc8 => OpcodeRecord(Interpretation::Instruction(Opcode::SHA1NEXTE), OperandCode::G_E_xmm),
            0xc9 => OpcodeRecord(Interpretation::Instruction(Opcode::SHA1MSG1), OperandCode::G_E_xmm),
            0xca => OpcodeRecord(Interpretation::Instruction(Opcode::SHA1MSG2), OperandCode::G_E_xmm),
            0xcb => OpcodeRecord(Interpretation::Instruction(Opcode::SHA256RNDS2), OperandCode::G_E_xmm),
            0xcc => OpcodeRecord(Interpretation::Instruction(Opcode::SHA256MSG1), OperandCode::G_E_xmm),
            0xcd => OpcodeRecord(Interpretation::Instruction(Opcode::SHA256MSG2), OperandCode::G_E_xmm),
            0xf0 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVBE), OperandCode::Gv_M),
            0xf1 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVBE), OperandCode::M_Gv),
            0xf6 => OpcodeRecord(Interpretation::Instruction(Opcode::WRSS), OperandCode::Md_Gd),
            0xf9 => OpcodeRecord(Interpretation::Instruction(Opcode::MOVDIRI), OperandCode::Md_Gd),
            _ => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
        };
    }
}

fn read_0f3a_opcode(opcode: u8, prefixes: &mut Prefixes) -> OpcodeRecord {
    if prefixes.rep() || prefixes.repnz() {
        return OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing);
    }

    if prefixes.operand_size() {
        return match opcode {
            0x08 => OpcodeRecord(Interpretation::Instruction(Opcode::ROUNDPS), OperandCode::G_E_xmm_Ib),
            0x09 => OpcodeRecord(Interpretation::Instruction(Opcode::ROUNDPD), OperandCode::G_E_xmm_Ib),
            0x0a => OpcodeRecord(Interpretation::Instruction(Opcode::ROUNDSS), OperandCode::G_E_xmm_Ib),
            0x0b => OpcodeRecord(Interpretation::Instruction(Opcode::ROUNDSD), OperandCode::G_E_xmm_Ib),
            0x0c => OpcodeRecord(Interpretation::Instruction(Opcode::BLENDPS), OperandCode::G_E_xmm_Ib),
            0x0d => OpcodeRecord(Interpretation::Instruction(Opcode::BLENDPD), OperandCode::G_E_xmm_Ib),
            0x0e => OpcodeRecord(Interpretation::Instruction(Opcode::PBLENDW), OperandCode::G_E_xmm_Ib),
            0x0f => OpcodeRecord(Interpretation::Instruction(Opcode::PALIGNR), OperandCode::G_E_xmm_Ib),
            0x14 => OpcodeRecord(Interpretation::Instruction(Opcode::PEXTRB), OperandCode::G_Ev_xmm_Ib),
            0x15 => OpcodeRecord(Interpretation::Instruction(Opcode::PEXTRW), OperandCode::G_Ev_xmm_Ib),
            0x16 => OpcodeRecord(Interpretation::Instruction(Opcode::PEXTRD), OperandCode::G_Ev_xmm_Ib),
            0x17 => OpcodeRecord(Interpretation::Instruction(Opcode::EXTRACTPS), OperandCode::G_Ev_xmm_Ib),
            0x20 => OpcodeRecord(Interpretation::Instruction(Opcode::PINSRB), OperandCode::G_Ev_xmm_Ib),
            0x21 => OpcodeRecord(Interpretation::Instruction(Opcode::INSERTPS), OperandCode::G_Ev_xmm_Ib),
            0x22 => OpcodeRecord(Interpretation::Instruction(Opcode::PINSRD), OperandCode::G_Ev_xmm_Ib),
            0x40 => OpcodeRecord(Interpretation::Instruction(Opcode::DPPS), OperandCode::G_E_xmm_Ib),
            0x41 => OpcodeRecord(Interpretation::Instruction(Opcode::DPPD), OperandCode::G_E_xmm_Ib),
            0x42 => OpcodeRecord(Interpretation::Instruction(Opcode::MPSADBW), OperandCode::G_E_xmm_Ib),
            0x44 => OpcodeRecord(Interpretation::Instruction(Opcode::PCLMULQDQ), OperandCode::G_E_xmm_Ib),
            0x60 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPESTRM), OperandCode::G_E_xmm_Ib),
            0x61 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPESTRI), OperandCode::G_E_xmm_Ib),
            0x62 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPISTRM), OperandCode::G_E_xmm_Ib),
            0x63 => OpcodeRecord(Interpretation::Instruction(Opcode::PCMPISTRI), OperandCode::G_E_xmm_Ib),
            0xcc => OpcodeRecord(Interpretation::Instruction(Opcode::SHA1RNDS4), OperandCode::G_E_xmm_Ib),
            0xce => OpcodeRecord(Interpretation::Instruction(Opcode::GF2P8AFFINEQB), OperandCode::G_E_xmm_Ub),
            0xcf => OpcodeRecord(Interpretation::Instruction(Opcode::GF2P8AFFINEINVQB), OperandCode::G_E_xmm_Ub),
            0xdf => OpcodeRecord(Interpretation::Instruction(Opcode::AESKEYGENASSIST), OperandCode::G_E_xmm_Ub),
            _ => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
        };
    }

    return match opcode {
        0xcc => OpcodeRecord(Interpretation::Instruction(Opcode::SHA1RNDS4), OperandCode::G_E_xmm_Ub),
        0x0f => OpcodeRecord(Interpretation::Instruction(Opcode::PALIGNR), OperandCode::G_E_mm_Ib),
        _ => OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    };
}

fn read_instr<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction) -> Result<(), DecodeError> {
    let mut length = 0u8;
//    use core::intrinsics::unlikely;
    let mut prefixes = Prefixes::new(0);

    let record: OpcodeRecord = loop {
        let b = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
        length += 1;
        if length >= 15 {
            return Err(DecodeError::TooLong);
        }
        let record = OPCODES[b as usize];
        if b == 0x0f {
            let b = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
            length += 1;
            if b == 0x38 {
                let b = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
                length += 1;
                break read_0f38_opcode(b, &mut prefixes);
            } else if b == 0x3a {
                let b = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
                length += 1;
                break read_0f3a_opcode(b, &mut prefixes);
            } else {
                break read_0f_opcode(b, &mut prefixes);
            }
        } else if let Interpretation::Instruction(_) = record.0 {
            break record;
        } else {
            match b {
                0x26 => {
                    prefixes.set_es();
                },
                0x2e => {
                    prefixes.set_cs();
                },
                0x36 => {
                    prefixes.set_ss();
                },
                0x3e => {
                    prefixes.set_ds();
                },
                0x64 => {
                    prefixes.set_fs();
                },
                0x65 => {
                    prefixes.set_gs();
                },
                0x66 => {
                    prefixes.set_operand_size();
                },
                0x67 => {
                    prefixes.set_address_size();
                },
                0xf0 => {
                    prefixes.set_lock();
                },
                0xf2 => {
                    prefixes.set_repnz();
                },
                0xf3 => {
                    prefixes.set_rep();
                },
                _ => { unsafe { unreachable_unchecked(); } }
            }
        }
    };
    if record == OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing) {
        return Err(DecodeError::InvalidOpcode);
    }
    if let Interpretation::Instruction(opcode) = record.0 {
        instruction.opcode = opcode;
    } else {
        unsafe { unreachable_unchecked(); }
    }
    instruction.prefixes = prefixes;
    read_operands(decoder, bytes_iter, instruction, record.1, &mut length)?;
    if length > 15 {
        return Err(DecodeError::TooLong);
    }
    instruction.length = length;

    if instruction.prefixes.lock() {
        if !LOCKABLE_INSTRUCTIONS.contains(&instruction.opcode) || !instruction.operands[0].is_memory() {
            return Err(DecodeError::InvalidPrefixes);
        }
    }

    if decoder != &InstDecoder::default() {
        // we might have to fix up or reject this instruction under whatever cpu features we need to
        // pretend to have.
        decoder.revise_instruction(instruction)?;
    }
    Ok(())
}
/* likely cases
        OperandCode::Eb_R0 => 0
        _op @ OperandCode::ModRM_0x80_Eb_Ib => 1
        _op @ OperandCode::ModRM_0x81_Ev_Ivs => 2
        op @ OperandCode::ModRM_0xc6_Eb_Ib => 3
        op @ OperandCode::ModRM_0xc7_Ev_Iv => 4
        op @ OperandCode::ModRM_0xc0_Eb_Ib => 5
        op @ OperandCode::ModRM_0xc1_Ev_Ib => 6
        op @ OperandCode::ModRM_0xd0_Eb_1 => 7
        op @ OperandCode::ModRM_0xd1_Ev_1 => 8
        op @ OperandCode::ModRM_0xd2_Eb_CL => 9
        op @ OperandCode::ModRM_0xd3_Ev_CL => 10
        _op @ OperandCode::ModRM_0xf6 => 11
        _op @ OperandCode::ModRM_0xf7 => 12
        OperandCode::ModRM_0xfe_Eb => 13
        OperandCode::ModRM_0xff_Ev => 14
        OperandCode::Gv_Eb => 15
        OperandCode::Gv_Ew => 16
        OperandCode::Ev => 18
        OperandCode::E_G_xmm => 19
        op @ OperandCode::G_M_xmm => 20
        op @ OperandCode::G_E_xmm => 21
        OperandCode::G_E_xmm_Ib => 22
        OperandCode::AL_Ibs => 23
        OperandCode::AX_Ivd => 24
        OperandCode::Ivs => 25
        OperandCode::ModRM_0x83_Ev_Ibs => 26
        OperandCode::I_3 => 27
        OperandCode::Nothing => 28
        OperandCode::G_E_mm_Ib => 29
        OperandCode::ModRM_0x8f_Ev => 30

 */
fn read_operands<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction, operand_code: OperandCode, length: &mut u8) -> Result<(), DecodeError> {
    instruction.operands[0] = OperandSpec::RegRRR;
    instruction.operand_count = 2;
    let operand_code = OperandCodeBuilder::from_bits(operand_code as u16);
    if operand_code.has_embedded_instructions() {
        match operand_code.get_embedded_instructions() {
            Ok(z_operand_code) => {
                let reg = z_operand_code.reg();
                match z_operand_code.category() {
                    0 => {
                        // these are Zv_R
                        let bank = if !instruction.prefixes.operand_size() {
                            RegisterBank::D
                        } else {
                            RegisterBank::W
                        };
                        instruction.modrm_rrr =
                            RegSpec::from_parts(reg, bank);
                        instruction.operand_count = 1;
                    }
                    1 => {
                        // Zv_AX
                        let bank = if !instruction.prefixes.operand_size() {
                            RegisterBank::D
                        } else {
                            RegisterBank::W
                        };
                        instruction.modrm_rrr =
                            RegSpec::from_parts(0, bank);
                        instruction.operands[1] = OperandSpec::RegMMM;
                        instruction.modrm_mmm =
                            RegSpec::from_parts(reg, bank);
                        instruction.operand_count = 2;
                    }
                    2 => {
                        // these are Zb_Ib_R
                        instruction.modrm_rrr =
                            RegSpec::from_parts(reg, RegisterBank::B);
                        instruction.imm =
                            read_imm_unsigned(&mut bytes_iter, 1, length)?;
                        instruction.operands[1] = OperandSpec::ImmU8;
                    }
                    3 => {
                        // category == 3, Zv_Iv_R
                        if !instruction.prefixes.operand_size() {
                            instruction.modrm_rrr =
                                RegSpec::from_parts(reg, RegisterBank::D);
                            instruction.imm =
                                read_imm_unsigned(&mut bytes_iter, 4, length)?;
                            instruction.operands[1] = OperandSpec::ImmI32;
                        } else {
                            instruction.modrm_rrr =
                                RegSpec::from_parts(reg, RegisterBank::W);
                            instruction.imm =
                                read_imm_unsigned(&mut bytes_iter, 2, length)?;
                            instruction.operands[1] = OperandSpec::ImmI16;
                        }
                    }
                    _ => {
                        unreachable!("bad category");
                    }
                }
                return Ok(());
            },
            // EmbeddedOperandInstructions but those are entirely handled in the fall-through
            // below. one day this may grow to be an `Err(the_operand_instructions)` though, so for
            // a simpler diff the above is pre-`match`/`Ok`'d.
            _ => {}
        }
    }

    let mut modrm = 0;
    let bank: RegisterBank;
    let mut mem_oper = OperandSpec::Nothing;
    if operand_code.has_read_E() {
        // cool! we can precompute opwidth and know we need to read_E.
        if !operand_code.has_byte_operands() {
            // further, this is an vd E
            if !instruction.prefixes.operand_size() {
                instruction.mem_size = 4;
                bank = RegisterBank::D;
            } else {
                instruction.mem_size = 2;
                bank = RegisterBank::W;
            }
        } else {
            instruction.mem_size = 1;
            bank = RegisterBank::B;
        };
        modrm = read_modrm(&mut bytes_iter, length)?;
        instruction.modrm_rrr.bank = bank;
        instruction.modrm_rrr.num = (modrm >> 3) & 7;

        mem_oper = if modrm >= 0b11000000 {
            if operand_code.bits() == (OperandCode::Gv_M as u16) {
                return Err(DecodeError::InvalidOperand);
            }
            read_modrm_reg(instruction, modrm, bank)?
        } else {
            read_M(&mut bytes_iter, instruction, modrm, length)?
        };
        instruction.operands[1] = mem_oper;
    }

    if let Some((only_imm, immsz)) = operand_code.has_imm() {
        instruction.imm =
            read_imm_signed(&mut bytes_iter, 1 << (immsz * 2), length)? as u32;
        if only_imm {
            if immsz == 0 {
                instruction.operands[0] = OperandSpec::ImmI8;
            } else {
                instruction.operands[0] = OperandSpec::ImmI32;
            }
            instruction.operand_count = 1;
            return Ok(());
        }
    }

    if operand_code.is_only_modrm_operands() {
        if !operand_code.has_reg_mem() {
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
        };
    } else {
//    match operand_code {
    match operand_code.special_case_handler_index() {
        0 => {
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
        },
        1 => {
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        }
        2 => {
            instruction.operands[0] = mem_oper;
            let numwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, length)? as u32;
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[1] = match numwidth {
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 2;
        },
        3 => { // ModRM_0xc6_Eb_Ib
            if modrm == 0xf8 {
                instruction.opcode = Opcode::XABORT;
                instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u32;
                instruction.operands[0] = OperandSpec::ImmI8;
                instruction.operand_count = 1;
                return Ok(());
            }
            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOperand); // Err("Invalid modr/m for opcode 0xc7".to_string());
            }

            instruction.operands[0] = mem_oper;
            instruction.opcode = Opcode::MOV;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u32;
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        }
        4 => { // ModRM_0xc7_Ev_Iv
            if modrm == 0xf8 {
                instruction.opcode = Opcode::XBEGIN;
                instruction.imm = if instruction.prefixes.operand_size() {
                    read_imm_signed(&mut bytes_iter, 2, length)? as i16 as i32 as u32
                } else {
                    read_imm_signed(&mut bytes_iter, 4, length)? as i32 as u32
                };
                instruction.operands[0] = OperandSpec::ImmI32;
                instruction.operand_count = 1;
                return Ok(());
            }
            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOperand); // Err("Invalid modr/m for opcode 0xc7".to_string());
            }

            instruction.operands[0] = mem_oper;
            instruction.opcode = Opcode::MOV;
            if instruction.prefixes.operand_size() {
                instruction.imm = read_imm_signed(&mut bytes_iter, 2, length)? as u32;
                instruction.operands[1] = OperandSpec::ImmI16;
            } else {
                instruction.imm = read_imm_signed(&mut bytes_iter, 4, length)? as u32;
                instruction.operands[1] = OperandSpec::ImmI32;
            }
        },
        op @ 5 |
        op @ 6 |
        op @ 7 |
        op @ 8 |
        op @ 9 |
        op @ 10 => {
            instruction.operands[0] = mem_oper;
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            if op == 10 {
                instruction.modrm_rrr = RegSpec::cl();
                instruction.operands[1] = OperandSpec::RegRRR;
            } else if op == 9 {
                instruction.modrm_rrr = RegSpec::cl();
                instruction.operands[1] = OperandSpec::RegRRR;
            } else {
                let num = match op {
                    5 |
                    6 => {
                        *length += 1;
                        read_num(&mut bytes_iter, 1)?
                    }
                    _ => {
                        // these are the _1 variants, everything else is unreachable
                        1
                    }
                };
                instruction.imm = num;
                instruction.operands[1] = OperandSpec::ImmI8;
            }
            instruction.operand_count = 2;
        },
        op @ 11 |
        op @ 12 => {
            let opwidth = if op == 11 {
                1
            } else {
                if instruction.prefixes.operand_size() {
                    2
                } else {
                    4
                }
            };
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
            match (modrm >> 3) & 7 {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    instruction.imm = read_imm_signed(&mut bytes_iter, opwidth, length)? as u32;
                    instruction.operands[1] = match opwidth {
                        1 => OperandSpec::ImmI8,
                        2 => OperandSpec::ImmI16,
                        4 => OperandSpec::ImmI32,
                        _ => unsafe { unreachable_unchecked() }
                    };
                    instruction.operand_count = 2;
                },
                2 => {
                    instruction.opcode = Opcode::NOT;
                },
                3 => {
                    instruction.opcode = Opcode::NEG;
                },
                4 => {
                    instruction.opcode = Opcode::MUL;
                },
                5 => {
                    instruction.opcode = Opcode::IMUL;
                },
                6 => {
                    instruction.opcode = Opcode::DIV;
                },
                7 => {
                    instruction.opcode = Opcode::IDIV;
                },
                _ => {
                    unsafe { unreachable_unchecked(); }
                }
            }
        },
        13 => {
            instruction.operands[0] = mem_oper;
            let r = (modrm >> 3) & 7;
            if r >= 2 {
                return Err(DecodeError::InvalidOpcode);
            }
            instruction.opcode = [
                Opcode::INC,
                Opcode::DEC,
            ][r as usize];
            instruction.operand_count = 1;
        }
        14 => {
            instruction.operands[0] = mem_oper;
            let r = (modrm >> 3) & 7;
            if r == 7 {
                return Err(DecodeError::InvalidOpcode);
            }
            let opcode = [
                Opcode::INC,
                Opcode::DEC,
                Opcode::CALL,
                Opcode::CALLF,
                Opcode::JMP,
                Opcode::JMPF,
                Opcode::PUSH,
            ][r as usize];
            if instruction.operands[0] == OperandSpec::RegMMM {
                if opcode == Opcode::CALL || opcode == Opcode::JMP {
                    instruction.modrm_mmm.bank = RegisterBank::D;
                } else if opcode == Opcode::CALLF || opcode == Opcode::JMPF {
                    return Err(DecodeError::InvalidOperand);
                }
            } else {
                if opcode == Opcode::CALL || opcode == Opcode::JMP || opcode == Opcode::PUSH || opcode == Opcode::POP {
                    instruction.mem_size = 4;
                } else if opcode == Opcode::CALLF || opcode == Opcode::JMPF {
                    instruction.mem_size = 5;
                }
            }
            instruction.opcode = opcode;
            instruction.operand_count = 1;
        }
        15 => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 1, length)?;
            instruction.modrm_rrr = if instruction.prefixes.operand_size() {
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::W)
            } else {
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::D)
            };
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = 1;
            }
            instruction.operand_count = 2;
        },
        16 => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            instruction.modrm_rrr = if instruction.prefixes.operand_size() {
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::W)
            } else {
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::D)
            };
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = 2;
            }
            instruction.operand_count = 2;
        },
        18 => {
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
        },
        19 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
            if instruction.operands[0] == OperandSpec::RegMMM {
                // fix the register to XMM
                instruction.modrm_mmm.bank = RegisterBank::X;
            } else {
                instruction.mem_size = 16;
            }
        },
        op @ 20 |
        op @ 21 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operand_count = 2;
            if instruction.operands[1] == OperandSpec::RegMMM {
                if op == 20 {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOperand);
                } else {
                    // fix the register to XMM
                    instruction.modrm_mmm.bank = RegisterBank::X;
                }
            } else {
                if instruction.opcode == Opcode::MOVDDUP {
                    instruction.mem_size = 8;
                } else {
                    instruction.mem_size = 16;
                }
            }
        },
        22 => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::X);
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 1;
            if instruction.operands[1] != OperandSpec::RegMMM {
                if instruction.opcode == Opcode::CMPSS {
                    instruction.mem_size = 4;
                } else if instruction.opcode == Opcode::CMPSD {
                    instruction.mem_size = 8;
                } else {
                    instruction.mem_size = 16;
                }
            }
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        },
        23 => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        }
        24 => {
            let opwidth = if instruction.prefixes.operand_size() {
                instruction.modrm_rrr =
                    RegSpec::from_parts(0, RegisterBank::W);
                2
            } else {
                instruction.modrm_rrr =
                    RegSpec::from_parts(0, RegisterBank::D);
                4
            };
            instruction.imm =
                read_imm_signed(&mut bytes_iter, opwidth, length)? as u32;
            instruction.operands[1] = match opwidth {
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 2;
        }
        25 => {
            let opwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, opwidth, length)?;
            instruction.operands[0] = match opwidth {
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 1;
        },
        26 => {
            instruction.operands[0] = mem_oper;
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        },
        27 => {
            instruction.imm = 3;
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operand_count = 1;
        }
        28 => {
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
            return Ok(());
        },
        29 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
            if instruction.operands[0] == OperandSpec::RegMMM {
                // fix the register to XMM
                if instruction.opcode == Opcode::MOVD {
                    instruction.modrm_mmm.bank = RegisterBank::D;
                } else {
                    instruction.modrm_mmm.bank = RegisterBank::X;
                }
            } else {
                instruction.mem_size = 4;
            }
        },
        30 => {
            instruction.operands[0] = mem_oper;
            let r = (modrm >> 3) & 7;
            if r >= 1 {
                return Err(DecodeError::InvalidOpcode);
            }
            instruction.opcode = [
                Opcode::POP,
            ][r as usize];
            instruction.operand_count = 1;
        }
        31 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operand_count = 2;
            if instruction.operands[1] == OperandSpec::RegMMM {
                // fix the register to XMM
                instruction.modrm_mmm.bank = RegisterBank::X;
            } else {
                instruction.mem_size = 4;
            }
        },
        _ => {
        let operand_code: OperandCode = unsafe { core::mem::transmute(operand_code.bits()) };
            unlikely_operands(decoder, bytes_iter, instruction, operand_code, mem_oper, length)?;
        }
    };
    }

    Ok(())
}
fn unlikely_operands<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction, operand_code: OperandCode, mem_oper: OperandSpec, length: &mut u8) -> Result<(), DecodeError> {
    match operand_code {
        OperandCode::G_E_mm_Ib => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E_mm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr = RegSpec { bank: RegisterBank::MM, num: (modrm >> 3) & 7 };
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
            } else {
                instruction.mem_size = 8;
            }
            instruction.imm = read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 1;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::G_Ev_xmm_Ib => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr = RegSpec { bank: RegisterBank::X, num: (modrm >> 3) & 7 };
            instruction.imm = read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 1;
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = match instruction.opcode {
                    Opcode::PEXTRB => 1,
                    Opcode::PEXTRW => 2,
                    Opcode::PEXTRD => 4,
                    Opcode::EXTRACTPS => 4,
                    Opcode::INSERTPS => 4,
                    Opcode::PINSRB => 1,
                    Opcode::PINSRW => 2,
                    Opcode::PINSRD => 4,
                    _ => 8,
                };
            }
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::PMOVX_E_G_xmm => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.modrm_rrr = RegSpec { bank: RegisterBank::X, num: (modrm >> 3) & 7 };
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operands[0] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            if instruction.operands[0] != OperandSpec::RegMMM {
                if [].contains(&instruction.opcode) {
                    instruction.mem_size = 2;
                } else {
                    instruction.mem_size = 8;
                }
            } else {
                if instruction.opcode == Opcode::MOVLPD || instruction.opcode == Opcode::MOVHPD || instruction.opcode == Opcode::MOVHPS {
                    return Err(DecodeError::InvalidOperand);
                }
            }
        }
        OperandCode::PMOVX_G_E_xmm => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.modrm_rrr = RegSpec { bank: RegisterBank::X, num: (modrm >> 3) & 7 };
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            if instruction.operands[1] != OperandSpec::RegMMM {
                if [Opcode::PMOVSXBQ, Opcode::PMOVZXBQ].contains(&instruction.opcode) {
                    instruction.mem_size = 2;
                } else if [Opcode::PMOVZXBD, Opcode::UCOMISS, Opcode::COMISS, Opcode::CVTSS2SD].contains(&instruction.opcode) {
                    instruction.mem_size = 4;
                } else {
                    instruction.mem_size = 8;
                }
            } else {
                if instruction.opcode == Opcode::MOVLPD || instruction.opcode == Opcode::MOVHPD {
                    return Err(DecodeError::InvalidOperand);
                }
            }
        }
        OperandCode::INV_Gv_M => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.modrm_rrr = RegSpec { bank: RegisterBank::D, num: (modrm >> 3) & 7 };
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = read_M(&mut bytes_iter, instruction, modrm, length)?;
            if instruction.operands[1] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            if [Opcode::LFS, Opcode::LGS, Opcode::LSS].contains(&instruction.opcode) {
                if instruction.prefixes.vex().w() {
                    instruction.mem_size = 6;
                } else {
                    instruction.mem_size = 4;
                }
            } else if [Opcode::ENQCMD, Opcode::ENQCMDS].contains(&instruction.opcode) {
                instruction.mem_size = 64;
            } else {
                instruction.mem_size = 16;
            }
        }
        OperandCode::ModRM_0xc4 => {
            let modrm = bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })?;
            if modrm & 0b11000000 == 0b11000000 {
                // interpret the c4 as a vex prefix
                if instruction.prefixes.lock() || instruction.prefixes.operand_size() || instruction.prefixes.rep() || instruction.prefixes.repnz() {
                    // prefixes and then vex is invalid! reject it.
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidPrefixes);
                } else {
                    vex::three_byte_vex(&mut bytes_iter, modrm, instruction, *length)?;
                    *length = instruction.length;

                    if decoder != &InstDecoder::default() {
                        decoder.revise_instruction(instruction)?;
                    }
                    return Ok(());
                }
            } else {
                // LES
                instruction.modrm_rrr = RegSpec::from_parts((modrm >> 3) & 7, if instruction.prefixes.operand_size() { RegisterBank::W } else { RegisterBank::D });
                instruction.operands[0] = OperandSpec::RegRRR;
                instruction.operands[1] = read_M(&mut bytes_iter, instruction, modrm, length)?;
            }
        },
        OperandCode::ModRM_0xc5 => {
            let modrm = bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })?;
            if (modrm & 0b1100_0000) == 0b1100_0000 {
                // interpret the c5 as a vex prefix
                if instruction.prefixes.lock() || instruction.prefixes.operand_size() || instruction.prefixes.rep() || instruction.prefixes.repnz() {
                    // prefixes and then vex is invalid! reject it.
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidPrefixes);
                } else {
                    vex::two_byte_vex(&mut bytes_iter, modrm, instruction, *length)?;
                    *length = instruction.length;

                    if decoder != &InstDecoder::default() {
                        decoder.revise_instruction(instruction)?;
                    }
                    return Ok(());
                }
            } else {
                // LDS
                instruction.modrm_rrr = RegSpec::from_parts((modrm >> 3) & 7, if instruction.prefixes.operand_size() { RegisterBank::W } else { RegisterBank::D });
                instruction.operands[0] = OperandSpec::RegRRR;
                instruction.operands[1] = read_M(&mut bytes_iter, instruction, modrm, length)?;
            }
        },
        OperandCode::G_U_xmm_Ub => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            if instruction.operands[1] != OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::D);
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 1;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
        }
        OperandCode::ModRM_0xf20f78 => {
            instruction.opcode = Opcode::INSERTQ;

            let modrm = read_modrm(&mut bytes_iter, length)?;

            if modrm < 0b11_000_000 {
                return Err(DecodeError::InvalidOperand);
            }

            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::X);
            instruction.operands[1] = OperandSpec::RegMMM;
            instruction.modrm_mmm =
                RegSpec::from_parts(modrm & 7, RegisterBank::X);
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            instruction.disp =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 2;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operands[3] = OperandSpec::ImmInDispField;
            instruction.operand_count = 4;
        }
        OperandCode::ModRM_0x660f78 => {
            instruction.opcode = Opcode::EXTRQ;

            let modrm = read_modrm(&mut bytes_iter, length)?;

            if modrm < 0b11_000_000 {
                return Err(DecodeError::InvalidOperand);
            }

            if modrm >= 0b11_001_000 {
                return Err(DecodeError::InvalidOperand);
            }

            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.modrm_mmm =
                RegSpec::from_parts(modrm & 7, RegisterBank::X);
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            instruction.disp =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 2;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operands[2] = OperandSpec::ImmInDispField;
            instruction.operand_count = 3;

        }
        OperandCode::G_E_xmm_Ub => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::X);
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 1;
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = 16;
            }
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
        }
        OperandCode::Gd_Ed => {
            instruction.modrm_rrr.bank = RegisterBank::D;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        OperandCode::Md_Gd => {
            instruction.modrm_rrr.bank = RegisterBank::D;
            if mem_oper == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 2;
        }
        OperandCode::G_U_xmm => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper != OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_mmm.bank = RegisterBank::X;
            instruction.operand_count = 2;
        },
        OperandCode::Gv_Ev_Ib => {
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::Gv_Ev_Iv => {
            let opwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            instruction.imm =
                read_imm_signed(&mut bytes_iter, opwidth, length)? as u32;
            instruction.operands[2] = match opwidth {
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 3;
        }
        OperandCode::Ev_Gv_Ib => {
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, length)? as u32;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::Ev_Gv_CL => {
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operands[2] = OperandSpec::RegVex;
            instruction.vex_reg = RegSpec::cl();
            instruction.operand_count = 3;
        }
        OperandCode::G_mm_Ew_Ib => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::MM);
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            } else {
                instruction.mem_size = 2;
            }
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u32;
            *length += 1;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::G_E_mm => {
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            } else {
                if [Opcode::PACKSSWB, Opcode::PCMPGTB, Opcode::PCMPGTW, Opcode::PCMPGTD, Opcode::PACKUSWB, Opcode::PUNPCKHBW, Opcode::PUNPCKHWD, Opcode::PUNPCKHDQ, Opcode::PACKSSDW, Opcode::PSRLW, Opcode::PMULHW, Opcode::PSHUFB, Opcode::PHADDW, Opcode::PHADDD, Opcode::PHADDSW, Opcode::PMADDUBSW, Opcode::PHSUBW, Opcode::PHSUBD, Opcode::PHSUBSW, Opcode::PSIGNB, Opcode::PSIGNW, Opcode::PSIGND, Opcode::PMULHRSW, Opcode::PABSB, Opcode::PABSW, Opcode::PABSD].contains(&instruction.opcode) {
                    instruction.mem_size = 8;
                } else {
                    instruction.mem_size = 4;
                }
            }
            instruction.operand_count = 2;
        },
        OperandCode::G_U_mm => {
            instruction.modrm_rrr.bank = RegisterBank::D;
            if mem_oper != OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_mmm.bank = RegisterBank::MM;
            instruction.modrm_mmm.num &= 0b111;
            instruction.operand_count = 2;
        },
        OperandCode::Gv_Ew_LSL => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            if instruction.prefixes.operand_size() {
                instruction.modrm_rrr =
                    RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::W);
            } else {
                instruction.modrm_rrr =
                    RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::D);
            };

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            // lsl is weird. the full register width is written, but only the low 16 bits are used.
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
            instruction.operand_count = 2;
        },
        OperandCode::Gd_Ev => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            let opwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::D);
            instruction.operand_count = 2;
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = opwidth;
            }
        },
        op @ OperandCode::AL_Ob |
        op @ OperandCode::AX_Ov => {
            instruction.modrm_rrr = match op {
                OperandCode::AL_Ob => {
                    instruction.mem_size = 1;
                    RegSpec::al()
                },
                OperandCode::AX_Ov => {
                    if instruction.prefixes.operand_size() {
                        instruction.mem_size = 2;
                        RegSpec::ax()
                    } else {
                        instruction.mem_size = 4;
                        RegSpec::eax()
                    }
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let imm = read_num(&mut bytes_iter, addr_width)?;
            *length += addr_width;
            instruction.disp = imm;
            if instruction.prefixes.address_size() {
                instruction.operands[1] = OperandSpec::DispU16;
            } else {
                instruction.operands[1] = OperandSpec::DispU32;
            };
            instruction.operand_count = 2;
        }
        op @ OperandCode::Ob_AL |
        op @ OperandCode::Ov_AX => {
            instruction.modrm_rrr = match op {
                OperandCode::Ob_AL => {
                    instruction.mem_size = 1;
                    RegSpec::al()
                },
                OperandCode::Ov_AX => {
                    if instruction.prefixes.operand_size() {
                        instruction.mem_size = 2;
                        RegSpec::ax()
                    } else {
                        instruction.mem_size = 4;
                        RegSpec::eax()
                    }
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let imm = read_num(&mut bytes_iter, addr_width)?;
            *length += addr_width;
            instruction.disp = imm;
            instruction.operands[0] = if instruction.prefixes.address_size() {
                OperandSpec::DispU16
            } else {
                OperandSpec::DispU32
            };
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::I_1 => {
            instruction.imm = 1;
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operand_count = 1;
        }
        /*
        OperandCode::Unsupported => {
            return Err(DecodeError::IncompleteDecoder);
        }
        */
        OperandCode::Iw_Ib => {
            instruction.disp = read_num(&mut bytes_iter, 2)?;
            instruction.imm = read_num(&mut bytes_iter, 1)?;
            instruction.operands[0] = OperandSpec::ImmInDispField;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operand_count = 2;
            *length += 3;
        }
        OperandCode::Fw => {
            if instruction.prefixes.operand_size() {
                instruction.opcode = Opcode::IRET;
            } else {
                instruction.opcode = Opcode::IRETD;
            }
            instruction.operand_count = 0;
        }
        OperandCode::G_mm_U_mm => {
            instruction.modrm_rrr.bank = RegisterBank::MM;
            if mem_oper != OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_mmm.bank = RegisterBank::MM;
            instruction.modrm_mmm.num &= 0b111;
            instruction.modrm_rrr.num &= 0b111;
            instruction.operand_count = 2;
        },
        OperandCode::E_G_q => {
            if instruction.prefixes.operand_size() {
                return Err(DecodeError::InvalidOpcode);
            }

            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::D);
            instruction.operand_count = 2;
            if instruction.operands[0] != OperandSpec::RegMMM {
                instruction.mem_size = 8;
            }
        }
        OperandCode::G_E_q => {
            if instruction.prefixes.operand_size() {
                return Err(DecodeError::InvalidOpcode);
            }

            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::D);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[0] != OperandSpec::RegMMM {
                instruction.mem_size = 8;
            }
            instruction.operand_count = 2;
        }
        OperandCode::G_Mq_mm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            if mem_oper == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            } else {
                instruction.mem_size = 8;
            }
            instruction.modrm_rrr.num &= 0b111;
            instruction.operand_count = 2;
        },
        OperandCode::MOVQ_f30f => {
            instruction.operand_count = 2;
            let modrm = read_modrm(&mut bytes_iter, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, RegisterBank::X);
            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
        }
        OperandCode::ModRM_0x0f0d => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 0b111;

            let opwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };

            match r {
                1 => {
                    instruction.opcode = Opcode::PREFETCHW;
                }
                _ => {
                    instruction.opcode = Opcode::NOP;
                }
            }
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
            if instruction.operands[0] != OperandSpec::RegMMM {
                instruction.mem_size = 64;
            }
            instruction.operand_count = 1;
        }
        OperandCode::ModRM_0x0f0f => {
            // 3dnow instructions are WILD, the opcode is encoded as an imm8 trailing the
            // instruction.

            let modrm = read_modrm(&mut bytes_iter, length)?;
            instruction.operands[1] = read_E_mm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec { bank: RegisterBank::MM, num: (modrm >> 3) & 7 };
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = 8;
            }

            let opcode = read_modrm(&mut bytes_iter, length)?;
            match opcode {
                0x0c => {
                    instruction.opcode = Opcode::PI2FW;
                }
                0x0d => {
                    instruction.opcode = Opcode::PI2FD;
                }
                0x1c => {
                    instruction.opcode = Opcode::PF2IW;
                }
                0x1d => {
                    instruction.opcode = Opcode::PF2ID;
                }
                0x8a => {
                    instruction.opcode = Opcode::PFNACC;
                }
                0x8e => {
                    instruction.opcode = Opcode::PFPNACC;
                }
                0x90 => {
                    instruction.opcode = Opcode::PFCMPGE;
                }
                0x94 => {
                    instruction.opcode = Opcode::PFMIN;
                }
                0x96 => {
                    instruction.opcode = Opcode::PFRCP;
                }
                0x97 => {
                    instruction.opcode = Opcode::PFRSQRT;
                }
                0x9a => {
                    instruction.opcode = Opcode::PFSUB;
                }
                0x9e => {
                    instruction.opcode = Opcode::PFADD;
                }
                0xa0 => {
                    instruction.opcode = Opcode::PFCMPGT;
                }
                0xa4 => {
                    instruction.opcode = Opcode::PFMAX;
                }
                0xa6 => {
                    instruction.opcode = Opcode::PFRCPIT1;
                }
                0xa7 => {
                    instruction.opcode = Opcode::PFRSQIT1;
                }
                0xaa => {
                    instruction.opcode = Opcode::PFSUBR;
                }
                0xae => {
                    instruction.opcode = Opcode::PFACC;
                }
                0xb0 => {
                    instruction.opcode = Opcode::PFCMPEQ;
                }
                0xb4 => {
                    instruction.opcode = Opcode::PFMUL;
                }
                0xb6 => {
                    instruction.opcode = Opcode::PFRCPIT2;
                }
                0xb7 => {
                    instruction.opcode = Opcode::PMULHRW;
                }
                0xbb => {
                    instruction.opcode = Opcode::PSWAPD;
                }
                0xbf => {
                    instruction.opcode = Opcode::PAVGUSB;
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }
        }
        OperandCode::ModRM_0x0fc7 => {
            if instruction.prefixes.repnz() {
                let modrm = read_modrm(&mut bytes_iter, length)?;
                let is_reg = (modrm & 0xc0) == 0xc0;

                let r = (modrm >> 3) & 7;
                match r {
                    1 => {
                        if is_reg {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOperand);
                        } else {
                            instruction.opcode = Opcode::CMPXCHG8B;
                            instruction.mem_size = 8;
                            instruction.operand_count = 1;
                            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
                        }
                        return Ok(());
                    }
                    _ => {
                        return Err(DecodeError::InvalidOperand);
                    }
                }
            }
            if instruction.prefixes.operand_size() {
                let opwidth = if instruction.prefixes.operand_size() {
                    2
                } else {
                    4
                };
                let modrm = read_modrm(&mut bytes_iter, length)?;
                let is_reg = (modrm & 0xc0) == 0xc0;

                let r = (modrm >> 3) & 7;
                match r {
                    1 => {
                        if is_reg {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOperand);
                        } else {
                            instruction.opcode = Opcode::CMPXCHG8B;
                            instruction.mem_size = 8;
                            instruction.operand_count = 1;
                            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
                        }
                        return Ok(());
                    }
                    6 => {
                        instruction.opcode = Opcode::VMCLEAR;
                        instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                        if instruction.operands[0] == OperandSpec::RegMMM {
                            // this would be invalid as `vmclear`, so fall back to the parse as
                            // 66-prefixed rdrand. this is a register operand, so just demote it to the
                            // word-form operand:
                            instruction.modrm_mmm = RegSpec { bank: RegisterBank::W, num: instruction.modrm_mmm.num };
                            instruction.opcode = Opcode::RDRAND;
                        } else {
                            instruction.mem_size = 8;
                        }
                        instruction.operand_count = 1;
                        return Ok(());
                    }
                    7 => {
                        instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                        if instruction.operands[0] == OperandSpec::RegMMM {
                            // this would be invalid as `vmclear`, so fall back to the parse as
                            // 66-prefixed rdrand. this is a register operand, so just demote it to the
                            // word-form operand:
                            instruction.modrm_mmm = RegSpec { bank: RegisterBank::W, num: instruction.modrm_mmm.num };
                            instruction.opcode = Opcode::RDSEED;
                        } else {
                            return Err(DecodeError::InvalidOpcode);
                        }
                        instruction.operand_count = 1;
                        return Ok(());
                    }
                    _ => {
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
            }

            if instruction.prefixes.rep() {
                let opwidth = if instruction.prefixes.operand_size() {
                    2
                } else {
                    4
                };
                let modrm = read_modrm(&mut bytes_iter, length)?;
                let is_reg = (modrm & 0xc0) == 0xc0;

                let r = (modrm >> 3) & 7;
                match r {
                    1 => {
                        if is_reg {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOperand);
                        } else {
                            instruction.opcode = Opcode::CMPXCHG8B;
                            instruction.mem_size = 8;
                            instruction.operand_count = 1;
                            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
                        }
                    }
                    6 => {
                        instruction.opcode = Opcode::VMXON;
                        instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                        if instruction.operands[0] == OperandSpec::RegMMM {
                            // invalid as `vmxon`, reg-form is `senduipi`
                            instruction.opcode = Opcode::SENDUIPI;
                            // and the operand is always a dword register
                            instruction.modrm_mmm.bank = RegisterBank::D;
                        } else {
                            instruction.mem_size = 8;
                        }
                        instruction.operand_count = 1;
                    }
                    7 => {
                        instruction.opcode = Opcode::RDPID;
                        instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                        if instruction.operands[0] != OperandSpec::RegMMM {
                            return Err(DecodeError::InvalidOperand);
                        }
                        instruction.operand_count = 1;
                    }
                    _ => {
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
                return Ok(());
            }

            let modrm = read_modrm(&mut bytes_iter, length)?;
            let is_reg = (modrm & 0xc0) == 0xc0;

            let r = (modrm >> 3) & 0b111;

            let opcode = match r {
                0b001 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        instruction.mem_size = 8;
                        Opcode::CMPXCHG8B
                    }
                }
                0b011 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        instruction.mem_size = 63;
                        Opcode::XRSTORS
                    }
                }
                0b100 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        instruction.mem_size = 63;
                        Opcode::XSAVEC
                    }
                }
                0b101 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        instruction.mem_size = 63;
                        Opcode::XSAVES
                    }
                }
                0b110 => {
                    if is_reg {
                        Opcode::RDRAND
                    } else {
                        instruction.mem_size = 8;
                        Opcode::VMPTRLD
                    }
                }
                0b111 => {
                    if is_reg {
                        Opcode::RDSEED
                    } else {
                        instruction.mem_size = 8;
                        Opcode::VMPTRST
                    }
                }
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOperand);
                }
            };

            instruction.opcode = opcode;
            instruction.operand_count = 1;
            let opwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
        },
        OperandCode::ModRM_0x0f71 => {
            if instruction.prefixes.rep() || instruction.prefixes.repnz() {
                return Err(DecodeError::InvalidOperand);
            }

            instruction.operand_count = 2;

            let modrm = read_modrm(&mut bytes_iter, length)?;
            if modrm & 0xc0 != 0xc0 {
                return Err(DecodeError::InvalidOperand);
            }

            let r = (modrm >> 3) & 7;
            match r {
                2 => {
                    instruction.opcode = Opcode::PSRLW;
                }
                4 => {
                    instruction.opcode = Opcode::PSRAW;
                }
                6 => {
                    instruction.opcode = Opcode::PSLLW;
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }

            if instruction.prefixes.operand_size() {
                instruction.modrm_mmm = RegSpec { bank: RegisterBank::X, num: modrm & 7 };
            } else {
                instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            }
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u32;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0x0f72 => {
            instruction.operand_count = 2;

            let modrm = read_modrm(&mut bytes_iter, length)?;
            if modrm & 0xc0 != 0xc0 {
                return Err(DecodeError::InvalidOperand);
            }

            let r = (modrm >> 3) & 7;
            match r {
                2 => {
                    instruction.opcode = Opcode::PSRLD;
                }
                4 => {
                    instruction.opcode = Opcode::PSRAD;
                }
                6 => {
                    instruction.opcode = Opcode::PSLLD;
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }

            if instruction.prefixes.operand_size() {
                instruction.modrm_mmm = RegSpec { bank: RegisterBank::X, num: modrm & 7 };
            } else {
                instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            }
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u32;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0x0f73 => {
            if instruction.prefixes.rep() || instruction.prefixes.repnz() {
                return Err(DecodeError::InvalidOperand);
            }

            instruction.operand_count = 2;

            let modrm = read_modrm(&mut bytes_iter, length)?;
            if modrm & 0xc0 != 0xc0 {
                return Err(DecodeError::InvalidOperand);
            }

            let r = (modrm >> 3) & 7;
            match r {
                2 => {
                    instruction.opcode = Opcode::PSRLQ;
                }
                3 => {
                    if !instruction.prefixes.operand_size() {
                        return Err(DecodeError::InvalidOperand);
                    }
                    instruction.opcode = Opcode::PSRLDQ;
                }
                6 => {
                    instruction.opcode = Opcode::PSLLQ;
                }
                7 => {
                    if !instruction.prefixes.operand_size() {
                        return Err(DecodeError::InvalidOperand);
                    }
                    instruction.opcode = Opcode::PSLLDQ;
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }

            if instruction.prefixes.operand_size() {
                instruction.modrm_mmm = RegSpec { bank: RegisterBank::X, num: modrm & 7 };
            } else {
                instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            }
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u32;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0xf30f38d8 => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 7;
            match r {
                0b000 => {
                    if modrm >= 0b11_000_000 {
                        return Err(DecodeError::InvalidOperand);
                    }
                    instruction.opcode = Opcode::AESENCWIDE128KL;
                    instruction.operands[0] = read_M(&mut bytes_iter, instruction, modrm, length)?;
                    return Ok(());
                }
                0b001 => {
                    if modrm >= 0b11_000_000 {
                        return Err(DecodeError::InvalidOperand);
                    }
                    instruction.opcode = Opcode::AESDECWIDE128KL;
                    instruction.operands[0] = read_M(&mut bytes_iter, instruction, modrm, length)?;
                    return Ok(());
                }
                0b010 => {
                    if modrm >= 0b11_000_000 {
                        return Err(DecodeError::InvalidOperand);
                    }
                    instruction.opcode = Opcode::AESENCWIDE256KL;
                    instruction.operands[0] = read_M(&mut bytes_iter, instruction, modrm, length)?;
                    return Ok(());
                }
                0b011 => {
                    if modrm >= 0b11_000_000 {
                        return Err(DecodeError::InvalidOperand);
                    }
                    instruction.opcode = Opcode::AESDECWIDE256KL;
                    instruction.operands[0] = read_M(&mut bytes_iter, instruction, modrm, length)?;
                    return Ok(());
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }
        }
        OperandCode::ModRM_0xf30f38dc => {
            read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm, length)?;
            if let OperandSpec::RegMMM = instruction.operands[1] {
                instruction.opcode = Opcode::LOADIWKEY;
            } else {
                instruction.opcode = Opcode::AESENC128KL;
            }
        }
        OperandCode::ModRM_0xf30f38dd => {
            read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm, length)?;
            if let OperandSpec::RegMMM = instruction.operands[1] {
                return Err(DecodeError::InvalidOperand);
            } else {
                instruction.opcode = Opcode::AESDEC128KL;
            }
        }
        OperandCode::ModRM_0xf30f38de => {
            read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm, length)?;
            if let OperandSpec::RegMMM = instruction.operands[1] {
                return Err(DecodeError::InvalidOperand);
            } else {
                instruction.opcode = Opcode::AESENC256KL;
            }
        }
        OperandCode::ModRM_0xf30f38df => {
            read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm, length)?;
            if let OperandSpec::RegMMM = instruction.operands[1] {
                return Err(DecodeError::InvalidOperand);
            } else {
                instruction.opcode = Opcode::AESDEC256KL;
            }
        }
        OperandCode::ModRM_0xf30f38fa => {
            instruction.opcode = Opcode::ENCODEKEY128;
            read_operands(decoder, bytes_iter, instruction, OperandCode::G_U_xmm, length)?;
            instruction.modrm_rrr.bank = RegisterBank::D;
            instruction.modrm_mmm.bank = RegisterBank::D;
        }
        OperandCode::ModRM_0xf30f38fb => {
            instruction.opcode = Opcode::ENCODEKEY256;
            read_operands(decoder, bytes_iter, instruction, OperandCode::G_U_xmm, length)?;
            instruction.modrm_rrr.bank = RegisterBank::D;
            instruction.modrm_mmm.bank = RegisterBank::D;
        }
        OperandCode::G_mm_Ed => {
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            } else {
                instruction.mem_size = 4;
            }
        }
        OperandCode::G_mm_E => {
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            } else {
                instruction.mem_size = 8;
            }
        }
        OperandCode::Ed_G_mm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            } else {
                instruction.mem_size = 4;
            }
        }
        OperandCode::Ed_G_xmm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            } else {
                instruction.mem_size = 4;
            }
        }
        OperandCode::E_G_mm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            } else {
                instruction.mem_size = 8;
            }
        }
        OperandCode::G_xmm_Ew_Ib => {
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
            instruction.imm =
                read_num(&mut bytes_iter, 1)?;
            *length += 1;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            } else {
                instruction.mem_size = 2;
            }
        },
        OperandCode::G_xmm_Ed => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            } else {
                instruction.mem_size = 4;
            }
        },
        OperandCode::G_mm_E_xmm => {
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::X;
            } else {
                instruction.mem_size = 16;
            }
        },
        op @ OperandCode::G_xmm_U_mm |
        op @ OperandCode::G_xmm_E_mm => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            } else {
                if op == OperandCode::G_xmm_U_mm {
                    return Err(DecodeError::InvalidOperand);
                } else {
                    instruction.mem_size = 8;
                }
            }
        },
        OperandCode::Rv_Gmm_Ib => {
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
            instruction.imm =
                read_num(&mut bytes_iter, 1)?;
            *length += 1;
            instruction.modrm_rrr.bank = RegisterBank::D;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            } else {
                return Err(DecodeError::InvalidOperand);
            }
        }
        OperandCode::U_mm_G_xmm => {
            instruction.modrm_mmm.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_rrr.bank = RegisterBank::MM;
                instruction.modrm_rrr.num &= 0b111;
            } else {
                return Err(DecodeError::InvalidOperand);
            }
        }
        // sure hope these aren't backwards huh
        OperandCode::AL_Xb => {
            instruction.modrm_rrr = RegSpec::al();
            instruction.modrm_mmm = RegSpec::esi();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::Deref;
            instruction.mem_size = 1;
            instruction.operand_count = 2;
        }
        OperandCode::Yb_Xb => {
            instruction.operands[0] = OperandSpec::Deref_edi;
            instruction.operands[1] = OperandSpec::Deref_esi;
            instruction.mem_size = 1;
            instruction.operand_count = 2;
        }
        OperandCode::Yb_AL => {
            instruction.modrm_rrr = RegSpec::al();
            instruction.modrm_mmm = RegSpec::esi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.mem_size = 1;
            instruction.operand_count = 2;
        }
        OperandCode::AX_Xv => {
            instruction.modrm_rrr = if instruction.prefixes.operand_size() {
                instruction.mem_size = 2;
                RegSpec::ax()
            } else {
                instruction.mem_size = 4;
                RegSpec::eax()
            };
            instruction.modrm_mmm = RegSpec::esi();
            instruction.operands[1] = OperandSpec::Deref;
        }
        OperandCode::Yv_AX => {
            instruction.modrm_rrr = if instruction.prefixes.operand_size() {
                instruction.mem_size = 2;
                RegSpec::ax()
            } else {
                instruction.mem_size = 4;
                RegSpec::eax()
            };
            instruction.modrm_mmm = RegSpec::edi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
        }
        OperandCode::Yv_Xv => {
            instruction.mem_size = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            instruction.operands[0] = OperandSpec::Deref_edi;
            instruction.operands[1] = OperandSpec::Deref_esi;
        }
        OperandCode::ModRM_0x0f12 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[1] = mem_oper;
            if instruction.operands[1] == OperandSpec::RegMMM {
                if instruction.prefixes.operand_size() {
                    return Err(DecodeError::InvalidOpcode);
                }
                instruction.modrm_mmm.bank = RegisterBank::X;
                instruction.opcode = Opcode::MOVHLPS;
            } else {
                instruction.mem_size = 8;
                if instruction.prefixes.operand_size() {
                    instruction.opcode = Opcode::MOVLPD;
                } else {
                    instruction.opcode = Opcode::MOVLPS;
                }
            }
        }
        OperandCode::ModRM_0x0f16 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[1] = mem_oper;
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::X;
                if instruction.prefixes.operand_size() {
                    return Err(DecodeError::InvalidOpcode);
                }
                instruction.opcode = Opcode::MOVLHPS;
            } else {
                instruction.mem_size = 8;
                if instruction.prefixes.operand_size() {
                    instruction.opcode = Opcode::MOVHPD;
                } else {
                    instruction.opcode = Opcode::MOVHPS;
                }
            }
        }
        OperandCode::ModRM_0x0f18 => {
            let rrr = instruction.modrm_rrr.num & 0b111;
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
            instruction.opcode = if mem_oper == OperandSpec::RegMMM && rrr < 4 {
                Opcode::NOP
            } else {
                match rrr {
                    0 => Opcode::PREFETCHNTA,
                    1 => Opcode::PREFETCH0,
                    2 => Opcode::PREFETCH1,
                    3 => Opcode::PREFETCH2,
                    _ => Opcode::NOP,
                }
            };
            if mem_oper != OperandSpec::RegMMM {
                instruction.mem_size = 64;
            }
        }
        OperandCode::Gd_U_xmm => {
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_rrr.bank = RegisterBank::D;
            instruction.modrm_mmm.bank = RegisterBank::X;
        }
        OperandCode::Gv_E_xmm => {
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::X;
            } else {
                instruction.mem_size = 4;
            }
        }
        OperandCode::M_G_xmm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            if instruction.operands[0] == OperandSpec::RegMMM {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOperand);
            } else {
                if instruction.opcode == Opcode::MOVNTSS {
                    instruction.mem_size = 4;
                } else if instruction.opcode == Opcode::MOVNTPD || instruction.opcode == Opcode::MOVNTDQ || instruction.opcode == Opcode::MOVNTPS {
                    instruction.mem_size = 16;
                } else {
                    instruction.mem_size = 8;
                }
            }
            instruction.modrm_rrr.bank = RegisterBank::X;
        }
        OperandCode::Ew_Gw => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.modrm_rrr =
                RegSpec { bank: RegisterBank::W, num: (modrm >> 3) & 7 };
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.mem_size = 2;
            instruction.operand_count = 2;
        },
        OperandCode::Ew_Sw => {
            let opwidth = 2;
            let modrm = read_modrm(&mut bytes_iter, length)?;

            // check r
            if ((modrm >> 3) & 7) > 5 {
                // return Err(()); //Err("Invalid r".to_owned());
                return Err(DecodeError::InvalidOperand);
            }

            instruction.modrm_rrr =
                RegSpec { bank: RegisterBank::S, num: (modrm >> 3) & 7 };
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            let mod_bits = modrm >> 6;
            if mod_bits == 0b11 {
                instruction.modrm_mmm =
                    RegSpec { bank: RegisterBank::W, num: modrm & 7};
                instruction.operands[0] = OperandSpec::RegMMM;
            } else {
                instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                instruction.mem_size = 2;
            }
        },
        OperandCode::Sw_Ew => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            // check r
            if ((modrm >> 3) & 7) > 5 {
                // return Err(()); // Err("Invalid r".to_owned());
                return Err(DecodeError::InvalidOperand);
            }

            instruction.modrm_rrr =
                RegSpec { bank: RegisterBank::S, num: (modrm >> 3) & 7 };

            // quoth the manual:
            // ```
            // The MOV instruction cannot be used to load the CS register. Attempting to do so
            // results in an invalid opcode excep-tion (#UD). To load the CS register, use the far
            // JMP, CALL, or RET instruction.
            // ```
            if instruction.modrm_rrr.num == 1 {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            let mod_bits = modrm >> 6;
            if mod_bits == 0b11 {
                instruction.modrm_mmm =
                    RegSpec { bank: RegisterBank::W, num: modrm & 7};
                instruction.operands[1] = OperandSpec::RegMMM;
            } else {
                instruction.operands[1] = read_M(&mut bytes_iter, instruction, modrm, length)?;
                instruction.mem_size = 2;
            }
        },
        OperandCode::CVT_AA => {
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
            instruction.opcode = if !instruction.prefixes.operand_size() {
                Opcode::CWDE
            } else {
                Opcode::CBW
            };
        }
        OperandCode::CVT_DA => {
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
            instruction.opcode = if !instruction.prefixes.operand_size() {
                Opcode::CDQ
            } else {
                Opcode::CWD
            };
        }
        OperandCode::Ib => {
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operand_count = 1;
        }
        OperandCode::Iw => {
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, 2, length)?;
            instruction.operands[0] = OperandSpec::ImmU16;
            instruction.operand_count = 1;
        }
        OperandCode::ModRM_0x0f00 => {
            instruction.operand_count = 1;
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 7;
            if r == 0 {
                instruction.opcode = Opcode::SLDT;
            } else if r == 1 {
                instruction.opcode = Opcode::STR;
            } else if r == 2 {
                instruction.opcode = Opcode::LLDT;
            } else if r == 3 {
                instruction.opcode = Opcode::LTR;
            } else if r == 4 {
                instruction.opcode = Opcode::VERR;
            } else if r == 5 {
                instruction.opcode = Opcode::VERW;
            } else if r == 6 {
                // TODO: this would be jmpe for x86-on-itanium systems.
                instruction.opcode = Opcode::Invalid;
                instruction.operands[0] = OperandSpec::Nothing;
                instruction.operand_count = 0;
                return Err(DecodeError::InvalidOperand);
            } else if r == 7 {
                instruction.opcode = Opcode::Invalid;
                instruction.operands[0] = OperandSpec::Nothing;
                instruction.operand_count = 0;
                return Err(DecodeError::InvalidOperand);
            } else {
                unreachable!("r <= 8");
            }
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
        }
        OperandCode::ModRM_0x0f01 => {
            let opwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 7;
            if r == 0 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    instruction.operands[0] = OperandSpec::Nothing;
                    instruction.operand_count = 0;
                    match m {
                        0b000 => {
                            instruction.opcode = Opcode::ENCLV;
                        },
                        0b001 => {
                            instruction.opcode = Opcode::VMCALL;
                        },
                        0b010 => {
                            instruction.opcode = Opcode::VMLAUNCH;
                        },
                        0b011 => {
                            instruction.opcode = Opcode::VMRESUME;
                        },
                        0b100 => {
                            instruction.opcode = Opcode::VMXOFF;
                        },
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                } else {
                    instruction.opcode = Opcode::SGDT;
                    instruction.operand_count = 1;
                    instruction.mem_size = 63;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else if r == 1 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    instruction.operands[0] = OperandSpec::Nothing;
                    instruction.operand_count = 0;
                    if instruction.prefixes.operand_size() {
                        match m {
                            0b100 => {
                                instruction.opcode = Opcode::TDCALL;
                            }
                            0b101 => {
                                instruction.opcode = Opcode::SEAMRET;
                            }
                            0b110 => {
                                instruction.opcode = Opcode::SEAMOPS;
                            }
                            0b111 => {
                                instruction.opcode = Opcode::SEAMCALL;
                            }
                            _ => {
                                instruction.opcode = Opcode::Invalid;
                                return Err(DecodeError::InvalidOpcode);
                            }
                        }
                    } else {
                        if instruction.prefixes.rep() || instruction.prefixes.repnz() {
                            return Err(DecodeError::InvalidOpcode);
                        }
                        match m {
                            0b000 => {
                                instruction.opcode = Opcode::MONITOR;
                            }
                            0b001 => {
                                instruction.opcode = Opcode::MWAIT;
                            },
                            0b010 => {
                                instruction.opcode = Opcode::CLAC;
                            }
                            0b011 => {
                                instruction.opcode = Opcode::STAC;
                            }
                            0b111 => {
                                instruction.opcode = Opcode::ENCLS;
                            }
                            _ => {
                                instruction.opcode = Opcode::Invalid;
                                return Err(DecodeError::InvalidOpcode);
                            }
                        }
                    }
                } else {
                    instruction.opcode = Opcode::SIDT;
                    instruction.operand_count = 1;
                    instruction.mem_size = 63;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else if r == 2 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    instruction.operands[0] = OperandSpec::Nothing;
                    instruction.operand_count = 0;
                    match m {
                        0b000 => {
                            instruction.opcode = Opcode::XGETBV;
                        }
                        0b001 => {
                            instruction.opcode = Opcode::XSETBV;
                        }
                        0b100 => {
                            instruction.opcode = Opcode::VMFUNC;
                        }
                        0b101 => {
                            instruction.opcode = Opcode::XEND;
                        }
                        0b110 => {
                            instruction.opcode = Opcode::XTEST;
                        }
                        0b111 => {
                            instruction.opcode = Opcode::ENCLU;
                        }
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                } else {
                    instruction.opcode = Opcode::LGDT;
                    instruction.operand_count = 1;
                    instruction.mem_size = 63;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else if r == 3 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    match m {
                        0b000 => {
                            instruction.opcode = Opcode::VMRUN;
                            instruction.operand_count = 1;
                            instruction.modrm_rrr = RegSpec::eax();
                            instruction.operands[0] = OperandSpec::RegRRR;
                        },
                        0b001 => {
                            instruction.opcode = Opcode::VMMCALL;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                        },
                        0b010 => {
                            instruction.opcode = Opcode::VMLOAD;
                            instruction.operand_count = 1;
                            instruction.modrm_rrr = RegSpec::eax();
                            instruction.operands[0] = OperandSpec::RegRRR;
                        },
                        0b011 => {
                            instruction.opcode = Opcode::VMSAVE;
                            instruction.operand_count = 1;
                            instruction.modrm_rrr = RegSpec::eax();
                            instruction.operands[0] = OperandSpec::RegRRR;
                        },
                        0b100 => {
                            instruction.opcode = Opcode::STGI;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                        },
                        0b101 => {
                            instruction.opcode = Opcode::CLGI;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                        },
                        0b110 => {
                            instruction.opcode = Opcode::SKINIT;
                            instruction.operand_count = 1;
                            instruction.operands[0] = OperandSpec::RegRRR;
                            instruction.modrm_rrr = RegSpec::eax();
                        },
                        0b111 => {
                            instruction.opcode = Opcode::INVLPGA;
                            instruction.operand_count = 2;
                            instruction.operands[0] = OperandSpec::RegRRR;
                            instruction.operands[1] = OperandSpec::RegMMM;
                            instruction.modrm_rrr = RegSpec::eax();
                            instruction.modrm_mmm = RegSpec::ecx();
                        },
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Err(DecodeError::InvalidOperand);
                        }
                    }
                } else {
                    instruction.opcode = Opcode::LIDT;
                    instruction.operand_count = 1;
                    instruction.mem_size = 63;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else if r == 4 {
                // TODO: this permits storing only to word-size registers
                // spec suggets this might do something different for f.ex rdi?
                instruction.opcode = Opcode::SMSW;
                instruction.operand_count = 1;
                instruction.mem_size = 2;
                instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            } else if r == 5 {
                let mod_bits = modrm >> 6;
                if mod_bits != 0b11 {
                    if !instruction.prefixes.rep() {
                        return Err(DecodeError::InvalidOpcode);
                    }
                    instruction.opcode = Opcode::RSTORSSP;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
                    instruction.mem_size = 8;
                    instruction.operand_count = 1;
                    return Ok(());
                }

                let m = modrm & 7;
                match m {
                    0b000 => {
                        if instruction.prefixes.repnz() {
                            instruction.opcode = Opcode::XSUSLDTRK;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Ok(());
                        }
                        if !instruction.prefixes.rep() || instruction.prefixes.repnz() {
                            return Err(DecodeError::InvalidOpcode);
                        }
                        instruction.opcode = Opcode::SETSSBSY;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    }
                    0b001 => {
                        if instruction.prefixes.repnz() {
                            instruction.opcode = Opcode::XRESLDTRK;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Ok(());
                        } else {
                            instruction.opcode = Opcode::Invalid;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                    0b010 => {
                        if !instruction.prefixes.rep() || instruction.prefixes.repnz() {
                            return Err(DecodeError::InvalidOpcode);
                        }
                        instruction.opcode = Opcode::SAVEPREVSSP;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    }
                    0b100 => {
                        if instruction.prefixes.rep() {
                            instruction.opcode = Opcode::UIRET;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                        } else {
                            instruction.opcode = Opcode::Invalid;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                    0b101 => {
                        if instruction.prefixes.rep() {
                            instruction.opcode = Opcode::TESTUI;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                        } else {
                            instruction.opcode = Opcode::Invalid;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                    0b110 => {
                        if instruction.prefixes.rep() {
                            instruction.opcode = Opcode::CLUI;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Ok(());
                        } else if instruction.prefixes.operand_size() || instruction.prefixes.repnz() {
                            return Err(DecodeError::InvalidOpcode);
                        }
                        instruction.opcode = Opcode::RDPKRU;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    }
                    0b111 => {
                        if instruction.prefixes.rep() {
                            instruction.opcode = Opcode::STUI;
                            instruction.operands[0] = OperandSpec::Nothing;
                            instruction.operand_count = 0;
                            return Ok(());
                        } else if instruction.prefixes.operand_size() || instruction.prefixes.repnz() {
                            return Err(DecodeError::InvalidOpcode);
                        }
                        instruction.opcode = Opcode::WRPKRU;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    }
                    _ => {
                        instruction.opcode = Opcode::Invalid;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
            } else if r == 6 {
                instruction.opcode = Opcode::LMSW;
                instruction.operand_count = 1;
                instruction.mem_size = 2;
                instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            } else if r == 7 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    if m == 0 {
                        // swapgs is not valid in modes other than 64-bit
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOpcode);
                    } else if m == 1 {
                        instruction.opcode = Opcode::RDTSCP;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    } else if m == 2 {
                        instruction.opcode = Opcode::MONITORX;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    } else if m == 3 {
                        instruction.opcode = Opcode::MWAITX;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    } else if m == 4 {
                        instruction.opcode = Opcode::CLZERO;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    } else if m == 5 {
                        instruction.opcode = Opcode::RDPRU;
                        instruction.operands[0] = OperandSpec::RegRRR;
                        instruction.modrm_rrr = RegSpec::ecx();
                        instruction.operand_count = 1;
                    } else if m == 6 {
                        instruction.opcode = Opcode::INVLPGB;
                        instruction.operand_count = 3;
                        instruction.operands[0] = OperandSpec::RegRRR;
                        instruction.operands[1] = OperandSpec::RegMMM;
                        instruction.operands[2] = OperandSpec::RegVex;
                        instruction.modrm_rrr = RegSpec::eax();
                        instruction.modrm_mmm = RegSpec::edx();
                        instruction.vex_reg = RegSpec::ecx();
                    } else if m == 7 {
                        instruction.opcode = Opcode::TLBSYNC;
                        instruction.operand_count = 0;
                    } else {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOpcode);
                    }
                } else {
                    instruction.opcode = Opcode::INVLPG;
                    instruction.operand_count = 1;
                    instruction.mem_size = 1;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else {
                unreachable!("r <= 8");
            }
        }
        OperandCode::ModRM_0x0fae => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 7;
            let m = modrm & 7;

            if instruction.prefixes.operand_size() && !(instruction.prefixes.rep() || instruction.prefixes.repnz()) {
                instruction.prefixes.unset_operand_size();
                if modrm < 0xc0 {
                    instruction.opcode = match (modrm >> 3) & 7 {
                        6 => {
                            Opcode::CLWB
                        }
                        7 => {
                            Opcode::CLFLUSHOPT
                        }
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    };
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 1 /* opwidth */, length)?;
                    instruction.mem_size = 64;
                    instruction.operand_count = 1;
                } else {
                    instruction.opcode = match (modrm >> 3) & 7 {
                        6 => {
                            Opcode::TPAUSE
                        }
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    };
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
                    instruction.operand_count = 1;
                }

                return Ok(());
            }

            if instruction.prefixes.repnz() {
                if (modrm & 0xc0) == 0xc0 {
                    match r {
                        6 => {
                            instruction.opcode = Opcode::UMWAIT;
                            instruction.modrm_rrr = RegSpec {
                                bank: RegisterBank::D,
                                num: m,
                            };
                            instruction.operands[0] = OperandSpec::RegRRR;
                            instruction.operand_count = 1;
                        }
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                    return Ok(());
                }
            }

            if instruction.prefixes.rep() {
                if r == 4 {
                    if instruction.prefixes.operand_size() {
                        // xed specifically rejects this. seeems out of line since rep takes
                        // precedence elsewhere, but ok i guess
                        return Err(DecodeError::InvalidOpcode);
                    }
                    instruction.opcode = Opcode::PTWRITE;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
                    if instruction.operands[0] != OperandSpec::RegMMM {
                        instruction.mem_size = 4;
                    }
                    instruction.operand_count = 1;
                    return Ok(());
                }
                if (modrm & 0xc0) == 0xc0 {
                    match r {
                        0 => {
                            instruction.opcode = Opcode::RDFSBASE;
                            instruction.modrm_mmm = RegSpec::from_parts(m, RegisterBank::D);
                            instruction.operands[0] = OperandSpec::RegMMM;
                            instruction.operand_count = 1;
                        }
                        1 => {
                            instruction.opcode = Opcode::RDGSBASE;
                            instruction.modrm_mmm = RegSpec::from_parts(m, RegisterBank::D);
                            instruction.operands[0] = OperandSpec::RegMMM;
                            instruction.operand_count = 1;

                        }
                        2 => {
                            instruction.opcode = Opcode::WRFSBASE;
                            instruction.modrm_mmm = RegSpec::from_parts(m, RegisterBank::D);
                            instruction.operands[0] = OperandSpec::RegMMM;
                            instruction.operand_count = 1;
                        }
                        3 => {
                            instruction.opcode = Opcode::WRGSBASE;
                            instruction.modrm_mmm = RegSpec::from_parts(m, RegisterBank::D);
                            instruction.operands[0] = OperandSpec::RegMMM;
                            instruction.operand_count = 1;
                        }
                        5 => {
                            instruction.opcode = Opcode::INCSSP;
                            instruction.modrm_mmm = RegSpec::from_parts(m, RegisterBank::D);
                            instruction.operands[0] = OperandSpec::RegMMM;
                            instruction.operand_count = 1;
                        }
                        6 => {
                            instruction.opcode = Opcode::UMONITOR;
                            instruction.modrm_mmm = RegSpec::from_parts(m, RegisterBank::D);
                            instruction.operands[0] = OperandSpec::RegMMM;
                            instruction.operand_count = 1;
                        }
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                    return Ok(());
                } else {
                    match r {
                        6 => {
                            instruction.opcode = Opcode::CLRSSBSY;
                            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
                            instruction.operand_count = 1;
                            instruction.mem_size = 8;
                            return Ok(());
                        }
                        _ => {
                            return Err(DecodeError::InvalidOperand);
                        }
                    }
                }
            }

            let mod_bits = modrm >> 6;

            // all the 0b11 instructions are err or no-operands
            if mod_bits == 0b11 {
                instruction.operands[0] = OperandSpec::Nothing;
                instruction.operand_count = 0;
                match r {
                    // invalid rrr for 0x0fae, mod: 11
                    0 | 1 | 2 | 3 | 4 => {
                        return Err(DecodeError::InvalidOpcode);
                    },
                    5 => {
                        instruction.opcode = Opcode::LFENCE;
                        // Intel's manual accepts m != 0, AMD supports m != 0 though the manual
                        // doesn't say (tested on threadripper)
                        if !decoder.amd_quirks() && !decoder.intel_quirks() {
                            if m != 0 {
                                instruction.opcode = Opcode::Invalid;
                                return Err(DecodeError::InvalidOperand);
                            }
                        }
                    },
                    6 => {
                        instruction.opcode = Opcode::MFENCE;
                        // Intel's manual accepts m != 0, AMD supports m != 0 though the manual
                        // doesn't say (tested on threadripper)
                        if !decoder.amd_quirks() && !decoder.intel_quirks() {
                            if m != 0 {
                                instruction.opcode = Opcode::Invalid;
                                return Err(DecodeError::InvalidOperand);
                            }
                        }
                    },
                    7 => {
                        instruction.opcode = Opcode::SFENCE;
                        // Intel's manual accepts m != 0, AMD supports m != 0 though the manual
                        // doesn't say (tested on threadripper)
                        if !decoder.amd_quirks() && !decoder.intel_quirks() {
                            if m != 0 {
                                instruction.opcode = Opcode::Invalid;
                                return Err(DecodeError::InvalidOperand);
                            }
                        }
                    },
                    _ => { unsafe { unreachable_unchecked() } /* r <=7 */ }
                }
            } else {
                // these can't be prefixed, so says `xed` i guess.
                if instruction.prefixes.operand_size() || instruction.prefixes.rep() || instruction.prefixes.repnz() {
                    return Err(DecodeError::InvalidOperand);
                }
                instruction.operand_count = 1;
                let (opcode, mem_size) = [
                    (Opcode::FXSAVE, 63),
                    (Opcode::FXRSTOR, 63),
                    (Opcode::LDMXCSR, 4),
                    (Opcode::STMXCSR, 4),
                    (Opcode::XSAVE, 63),
                    (Opcode::XRSTOR, 63),
                    (Opcode::XSAVEOPT, 63),
                    (Opcode::CLFLUSH, 64),
                ][r as usize];
                instruction.opcode = opcode;
                instruction.mem_size = mem_size;
                instruction.operands[0] = read_M(&mut bytes_iter, instruction, modrm, length)?;
            }
        },
        OperandCode::ModRM_0x0fba => {
            let opwidth = if instruction.prefixes.operand_size() {
                2
            } else {
                4
            };
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 7;
            match r {
                0 | 1 | 2 | 3 => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                },
                4 => {
                    instruction.opcode = Opcode::BT;
                }
                5 => {
                    instruction.opcode = Opcode::BTS;
                }
                6 => {
                    instruction.opcode = Opcode::BTR;
                }
                7 => {
                    instruction.opcode = Opcode::BTC;
                }
                _ => {
                    unreachable!("r < 8");
                }
            }

            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;

            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u32;
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        }
        op @ OperandCode::Rq_Cq_0 |
        op @ OperandCode::Rq_Dq_0 |
        op @ OperandCode::Cq_Rq_0 |
        op @ OperandCode::Dq_Rq_0 => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let m = modrm & 7;
            let r = (modrm >> 3) & 7;

            let bank = match op {
                OperandCode::Rq_Cq_0 |
                OperandCode::Cq_Rq_0 => {
                    if r != 0 && r != 2 && r != 3 && r != 4 {
                        return Err(DecodeError::InvalidOperand);
                    }
                    RegisterBank::CR
                },
                OperandCode::Rq_Dq_0 |
                OperandCode::Dq_Rq_0 => {
                    if r > 7 { // unreachable but mirrors x86_64 code
                        return Err(DecodeError::InvalidOperand);
                    }
                    RegisterBank::DR
                },
                _ => unsafe { unreachable_unchecked() }
            };
            let (rrr, mmm) = match op {
                OperandCode::Rq_Cq_0 |
                OperandCode::Rq_Dq_0 => (1, 0),
                OperandCode::Cq_Rq_0 |
                OperandCode::Dq_Rq_0 => (0, 1),
                _ => unsafe { unreachable_unchecked() }
            };

            instruction.modrm_rrr =
                RegSpec { bank: bank, num: r };
            instruction.modrm_mmm =
                RegSpec { bank: RegisterBank::D, num: m };
            instruction.operands[mmm] = OperandSpec::RegMMM;
            instruction.operands[rrr] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::FS => {
            instruction.modrm_rrr = RegSpec::fs();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 1;
        }
        OperandCode::GS => {
            instruction.modrm_rrr = RegSpec::gs();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 1;
        }
        OperandCode::CS => {
            instruction.modrm_rrr = RegSpec::cs();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 1;
        }
        OperandCode::DS => {
            instruction.modrm_rrr = RegSpec::ds();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 1;
        }
        OperandCode::ES => {
            instruction.modrm_rrr = RegSpec::es();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 1;
        }
        OperandCode::SS => {
            instruction.modrm_rrr = RegSpec::ss();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 1;
        }
        OperandCode::AL_Ib => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operand_count = 2;
        }
        OperandCode::AX_Ib => {
            instruction.modrm_rrr = if !instruction.prefixes.operand_size() {
               RegSpec::eax()
            } else {
               RegSpec::ax()
            };
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operand_count = 2;
        }
        OperandCode::Ib_AL => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::Ib_AX => {
            instruction.modrm_rrr = if !instruction.prefixes.operand_size() {
                RegSpec::eax()
            } else {
                RegSpec::ax()
            };
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::AX_DX => {
            instruction.modrm_rrr = if !instruction.prefixes.operand_size() {
                RegSpec::eax()
            } else {
                RegSpec::ax()
            };
            instruction.modrm_mmm = RegSpec::dx();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegMMM;
            instruction.operand_count = 2;
        }
        OperandCode::AL_DX => {
            instruction.modrm_rrr = RegSpec::al();
            instruction.modrm_mmm = RegSpec::dx();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegMMM;
            instruction.operand_count = 2;
        }
        OperandCode::DX_AX => {
            instruction.modrm_rrr = if !instruction.prefixes.operand_size() {
                RegSpec::eax()
            } else {
                RegSpec::ax()
            };
            instruction.modrm_mmm = RegSpec::dx();
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::DX_AL => {
            instruction.modrm_rrr = RegSpec::al();
            instruction.modrm_mmm = RegSpec::dx();
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::Yb_DX => {
            instruction.modrm_rrr = RegSpec::dl();
            instruction.modrm_mmm = RegSpec::edi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::Yv_DX => {
            instruction.modrm_rrr = RegSpec::dx();
            instruction.modrm_mmm = RegSpec::edi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::DX_Xb => {
            instruction.modrm_rrr = RegSpec::dl();
            instruction.modrm_mmm = RegSpec::esi();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::Deref;
            instruction.operand_count = 2;
        }
        OperandCode::AH => {
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
        }
        OperandCode::DX_Xv => {
            instruction.modrm_rrr = RegSpec::dx();
            instruction.modrm_mmm = RegSpec::esi();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::Deref;
            instruction.operand_count = 2;
        }
        OperandCode::x87_d8 |
        OperandCode::x87_d9 |
        OperandCode::x87_da |
        OperandCode::x87_db |
        OperandCode::x87_dc |
        OperandCode::x87_dd |
        OperandCode::x87_de |
        OperandCode::x87_df => {
            return decode_x87(decoder, bytes_iter, instruction, operand_code, length);
        }
        OperandCode::M_Gv => {
            // `lea` operands (`Gv_M`) opportunistically reject a register form of `mmm` early, but
            // leaves `M_Gv` to test memory-ness of the `mmm` operand directly. also, swap
            // operands.
            if let OperandSpec::RegMMM = instruction.operands[1] {
                return Err(DecodeError::InvalidOperand);
            }
            let temp = instruction.operands[1];
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = temp;
        }
        OperandCode::ModRM_0x62 => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            if modrm < 0xc0 {
                instruction.modrm_rrr =
                    RegSpec { bank: RegisterBank::D, num: (modrm >> 3) & 7 };
                if instruction.prefixes.operand_size() {
                    instruction.modrm_rrr.bank = RegisterBank::W;
                    instruction.mem_size = 4;
                } else {
                    instruction.mem_size = 8;
                }

                instruction.operands[0] = OperandSpec::RegRRR;
                instruction.operands[1] = read_M(&mut bytes_iter, instruction, modrm, length)?;
                instruction.operand_count = 2;
            } else {
                let prefixes = &instruction.prefixes;
                if prefixes.lock() || prefixes.operand_size() || prefixes.rep_any() {
                    return Err(DecodeError::InvalidPrefixes);
                } else {
                    evex::read_evex(&mut bytes_iter, instruction, *length, Some(modrm))?;
                    // there's an unavoidable `instruction.length = *length;` after
                    // `unlikely_operands`. the current length is correct, so store it back to
                    // length to make the reassignment store a correct length.
                    *length = instruction.length;
                }
            }
        }
        _ => {
            // TODO: this should be unreachable - safe to panic now?
            // can't simply delete this arm because the non-unlikely operands are handled outside
            // here, and some operands are entirely decoded before reaching match in the first
            // place.
            // perhaps fully-decoded operands could be a return here? they would be jump table
            // entries anyway, so no extra space for the dead arms.
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
            instruction.opcode = Opcode::Invalid;
            return Err(DecodeError::InvalidOperand);
        }
    };
    Ok(())
}

fn decode_x87<T: Iterator<Item=u8>>(_decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction, operand_code: OperandCode, length: &mut u8) -> Result<(), DecodeError> {
    #[allow(non_camel_case_types)]
    enum OperandCodeX87 {
        Est,
        St_Est,
        St_Edst,
        St_Eqst,
        St_Ew,
        St_Mw,
        St_Md,
        St_Mq,
        St_Mm,
        Ew,
        Est_St,
        Edst_St,
        Eqst_St,
        Ed_St,
        Mw_St,
        Md_St,
        Mq_St,
        Mm_St,
        Ex87S,
        Nothing,
    }

    // every x87 instruction is conditional on rrr bits
    let modrm = read_modrm(&mut bytes_iter, length)?;
    let r = (modrm >> 3) & 0b111;

    let (opcode, x87_operands) = match operand_code {
        OperandCode::x87_d8 => {
            match r {
                0 => (Opcode::FADD, OperandCodeX87::St_Edst),
                1 => (Opcode::FMUL, OperandCodeX87::St_Edst),
                2 => (Opcode::FCOM, OperandCodeX87::St_Edst),
                3 => (Opcode::FCOMP, OperandCodeX87::St_Edst),
                4 => (Opcode::FSUB, OperandCodeX87::St_Edst),
                5 => (Opcode::FSUBR, OperandCodeX87::St_Edst),
                6 => (Opcode::FDIV, OperandCodeX87::St_Edst),
                7 => (Opcode::FDIVR, OperandCodeX87::St_Edst),
                _ => { unreachable!("impossible r"); }
            }
        }
        OperandCode::x87_d9 => {
            match r {
                0 => (Opcode::FLD, OperandCodeX87::St_Edst),
                1 => {
                    if modrm >= 0xc0 {
                        (Opcode::FXCH, OperandCodeX87::St_Est)
                    } else {
                        return Err(DecodeError::InvalidOpcode);
                    }
                },
                2 => {
                    if modrm >= 0xc0 {
                        if modrm == 0xd0 {
                            (Opcode::FNOP, OperandCodeX87::Nothing)
                        } else {
                            return Err(DecodeError::InvalidOpcode);
                        }
                    } else {
                        (Opcode::FST, OperandCodeX87::Ed_St)
                    }
                }
                3 => {
                    if modrm >= 0xc0 {
                        (Opcode::FSTPNCE, OperandCodeX87::Est_St)
                    } else {
                        (Opcode::FSTP, OperandCodeX87::Edst_St)
                    }
                },
                4 => {
                    if modrm >= 0xc0 {
                        match modrm {
                            0xe0 => (Opcode::FCHS, OperandCodeX87::Nothing),
                            0xe1 => (Opcode::FABS, OperandCodeX87::Nothing),
                            0xe2 => { return Err(DecodeError::InvalidOpcode); },
                            0xe3 => { return Err(DecodeError::InvalidOpcode); },
                            0xe4 => (Opcode::FTST, OperandCodeX87::Nothing),
                            0xe5 => (Opcode::FXAM, OperandCodeX87::Nothing),
                            0xe6 => { return Err(DecodeError::InvalidOpcode); },
                            0xe7 => { return Err(DecodeError::InvalidOpcode); },
                            _ => { unreachable!("invalid modrm"); }
                        }
                    } else {
                        (Opcode::FLDENV, OperandCodeX87::Ex87S) // x87 state
                    }
                },
                5 => {
                    if modrm >= 0xc0 {
                        match modrm {
                            0xe8 => (Opcode::FLD1, OperandCodeX87::Nothing),
                            0xe9 => (Opcode::FLDL2T, OperandCodeX87::Nothing),
                            0xea => (Opcode::FLDL2E, OperandCodeX87::Nothing),
                            0xeb => (Opcode::FLDPI, OperandCodeX87::Nothing),
                            0xec => (Opcode::FLDLG2, OperandCodeX87::Nothing),
                            0xed => (Opcode::FLDLN2, OperandCodeX87::Nothing),
                            0xee => (Opcode::FLDZ, OperandCodeX87::Nothing),
                            0xef => (Opcode::Invalid, OperandCodeX87::Nothing),
                            _ => { unreachable!("invalid modrm"); }
                        }
                    } else {
                        (Opcode::FLDCW, OperandCodeX87::Ew)
                    }
                }
                6 => {
                    if modrm >= 0xc0 {
                        match modrm {
                            0xf0 => (Opcode::F2XM1, OperandCodeX87::Nothing),
                            0xf1 => (Opcode::FYL2X, OperandCodeX87::Nothing),
                            0xf2 => (Opcode::FPTAN, OperandCodeX87::Nothing),
                            0xf3 => (Opcode::FPATAN, OperandCodeX87::Nothing),
                            0xf4 => (Opcode::FXTRACT, OperandCodeX87::Nothing),
                            0xf5 => (Opcode::FPREM1, OperandCodeX87::Nothing),
                            0xf6 => (Opcode::FDECSTP, OperandCodeX87::Nothing),
                            0xf7 => (Opcode::FINCSTP, OperandCodeX87::Nothing),
                            _ => { unreachable!("invalid modrm"); }
                        }
                    } else {
                        (Opcode::FNSTENV, OperandCodeX87::Ex87S) // x87 state
                    }
                }
                7 => {
                    if modrm >= 0xc0 {
                        match modrm {
                            0xf8 => (Opcode::FPREM, OperandCodeX87::Nothing),
                            0xf9 => (Opcode::FYL2XP1, OperandCodeX87::Nothing),
                            0xfa => (Opcode::FSQRT, OperandCodeX87::Nothing),
                            0xfb => (Opcode::FSINCOS, OperandCodeX87::Nothing),
                            0xfc => (Opcode::FRNDINT, OperandCodeX87::Nothing),
                            0xfd => (Opcode::FSCALE, OperandCodeX87::Nothing),
                            0xfe => (Opcode::FSIN, OperandCodeX87::Nothing),
                            0xff => (Opcode::FCOS, OperandCodeX87::Nothing),
                            _ => { unreachable!("invalid modrm"); }
                        }
                    } else {
                        (Opcode::FNSTCW, OperandCodeX87::Ew)
                    }
                }
                _ => { unreachable!("impossible r"); }
            }
        }
        OperandCode::x87_da => {
            if modrm >= 0xc0 {
                match r {
                    0 => (Opcode::FCMOVB, OperandCodeX87::St_Est),
                    1 => (Opcode::FCMOVE, OperandCodeX87::St_Est),
                    2 => (Opcode::FCMOVBE, OperandCodeX87::St_Est),
                    3 => (Opcode::FCMOVU, OperandCodeX87::St_Est),
                    _ => {
                        if modrm == 0xe9 {
                            (Opcode::FUCOMPP, OperandCodeX87::Nothing)
                        } else {
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                }
            } else {
                match r {
                    0 => (Opcode::FIADD, OperandCodeX87::St_Md), // 0xd9d0 -> fnop
                    1 => (Opcode::FIMUL, OperandCodeX87::St_Md),
                    2 => (Opcode::FICOM, OperandCodeX87::St_Md), // FCMOVE
                    3 => (Opcode::FICOMP, OperandCodeX87::St_Md), // FCMOVBE
                    4 => (Opcode::FISUB, OperandCodeX87::St_Md),
                    5 => (Opcode::FISUBR, OperandCodeX87::St_Md), // FUCOMPP
                    6 => (Opcode::FIDIV, OperandCodeX87::St_Md),
                    7 => (Opcode::FIDIVR, OperandCodeX87::St_Md),
                    _ => { unreachable!("impossible r"); }
                }
            }
        }
        OperandCode::x87_db => {
            if modrm >= 0xc0 {
                match r {
                    0 => (Opcode::FCMOVNB, OperandCodeX87::St_Est),
                    1 => (Opcode::FCMOVNE, OperandCodeX87::St_Est),
                    2 => (Opcode::FCMOVNBE, OperandCodeX87::St_Est),
                    3 => (Opcode::FCMOVNU, OperandCodeX87::St_Est),
                    4 => {
                        match modrm {
                            0xe0 => (Opcode::FENI8087_NOP, OperandCodeX87::Nothing),
                            0xe1 => (Opcode::FDISI8087_NOP, OperandCodeX87::Nothing),
                            0xe2 => (Opcode::FNCLEX, OperandCodeX87::Nothing),
                            0xe3 => (Opcode::FNINIT, OperandCodeX87::Nothing),
                            0xe4 => (Opcode::FSETPM287_NOP, OperandCodeX87::Nothing),
                            _ => {
                                return Err(DecodeError::InvalidOpcode);
                            }
                        }
                    }
                    5 => (Opcode::FUCOMI, OperandCodeX87::St_Est),
                    6 => (Opcode::FCOMI, OperandCodeX87::St_Est),
                    _ => {
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
            } else {
                match r {
                    0 => (Opcode::FILD, OperandCodeX87::St_Md),
                    1 => (Opcode::FISTTP, OperandCodeX87::Md_St),
                    2 => (Opcode::FIST, OperandCodeX87::Md_St),
                    3 => (Opcode::FISTP, OperandCodeX87::Md_St),
                    5 => (Opcode::FLD, OperandCodeX87::St_Mm), // 80bit
                    7 => (Opcode::FSTP, OperandCodeX87::Mm_St), // 80bit
                    _ => {
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
            }

        }
        OperandCode::x87_dc => {
            // mod=11 swaps operand order for some instructions
            if modrm >= 0xc0 {
                match r {
                    0 => (Opcode::FADD, OperandCodeX87::Eqst_St),
                    1 => (Opcode::FMUL, OperandCodeX87::Eqst_St),
                    2 => (Opcode::FCOM, OperandCodeX87::St_Eqst),
                    3 => (Opcode::FCOMP, OperandCodeX87::St_Eqst),
                    4 => (Opcode::FSUBR, OperandCodeX87::Eqst_St),
                    5 => (Opcode::FSUB, OperandCodeX87::Eqst_St),
                    6 => (Opcode::FDIVR, OperandCodeX87::Eqst_St),
                    7 => (Opcode::FDIV, OperandCodeX87::Eqst_St),
                    _ => { unreachable!("impossible r"); }
                }
            } else {
                match r {
                    0 => (Opcode::FADD, OperandCodeX87::St_Eqst),
                    1 => (Opcode::FMUL, OperandCodeX87::St_Eqst),
                    2 => (Opcode::FCOM, OperandCodeX87::St_Eqst),
                    3 => (Opcode::FCOMP, OperandCodeX87::St_Eqst),
                    4 => (Opcode::FSUB, OperandCodeX87::St_Eqst),
                    5 => (Opcode::FSUBR, OperandCodeX87::St_Eqst),
                    6 => (Opcode::FDIV, OperandCodeX87::St_Eqst),
                    7 => (Opcode::FDIVR, OperandCodeX87::St_Eqst),
                    _ => { unreachable!("impossible r"); }
                }
            }
        }
        OperandCode::x87_dd => {
            if modrm >= 0xc0 {
                match r {
                    0 => (Opcode::FFREE, OperandCodeX87::Est),
                    1 => (Opcode::FXCH, OperandCodeX87::St_Est),
                    2 => (Opcode::FST, OperandCodeX87::Est_St),
                    3 => (Opcode::FSTP, OperandCodeX87::Est_St),
                    4 => (Opcode::FUCOM, OperandCodeX87::St_Est),
                    5 => (Opcode::FUCOMP, OperandCodeX87::St_Est),
                    6 => (Opcode::Invalid, OperandCodeX87::Nothing),
                    7 => (Opcode::Invalid, OperandCodeX87::Nothing),
                    _ => { unreachable!("impossible r"); }
                }
            } else {
                match r {
                    0 => (Opcode::FLD, OperandCodeX87::St_Eqst),
                    1 => (Opcode::FISTTP, OperandCodeX87::Eqst_St),
                    2 => (Opcode::FST, OperandCodeX87::Eqst_St),
                    3 => (Opcode::FSTP, OperandCodeX87::Eqst_St),
                    4 => (Opcode::FRSTOR, OperandCodeX87::Ex87S),
                    5 => (Opcode::Invalid, OperandCodeX87::Nothing),
                    6 => (Opcode::FNSAVE, OperandCodeX87::Ex87S),
                    7 => (Opcode::FNSTSW, OperandCodeX87::Ew),
                    _ => { unreachable!("impossible r"); }
                }
            }
        }
        OperandCode::x87_de => {
            if modrm >= 0xc0 {
                match r {
                    0 => (Opcode::FADDP, OperandCodeX87::Est_St),
                    1 => (Opcode::FMULP, OperandCodeX87::Est_St),
                    // undocumented in intel manual, argument order inferred from
                    // by xed and capstone. TODO: check amd manual.
                    2 => (Opcode::FCOMP, OperandCodeX87::St_Est),
                    3 => {
                        if modrm == 0xd9 {
                            (Opcode::FCOMPP, OperandCodeX87::Nothing)
                        } else {
                            return Err(DecodeError::InvalidOperand);
                        }
                    },
                    4 => (Opcode::FSUBRP, OperandCodeX87::Est_St),
                    5 => (Opcode::FSUBP, OperandCodeX87::Est_St),
                    6 => (Opcode::FDIVRP, OperandCodeX87::Est_St),
                    7 => (Opcode::FDIVP, OperandCodeX87::Est_St),
                    _ => { unreachable!("impossible r"); }
                }
            } else {
                match r {
                    0 => (Opcode::FIADD, OperandCodeX87::St_Ew),
                    1 => (Opcode::FIMUL, OperandCodeX87::St_Ew),
                    2 => (Opcode::FICOM, OperandCodeX87::St_Ew),
                    3 => (Opcode::FICOMP, OperandCodeX87::St_Ew),
                    4 => (Opcode::FISUB, OperandCodeX87::St_Ew),
                    5 => (Opcode::FISUBR, OperandCodeX87::St_Ew),
                    6 => (Opcode::FIDIV, OperandCodeX87::St_Ew),
                    7 => (Opcode::FIDIVR, OperandCodeX87::St_Ew),
                    _ => { unreachable!("impossible r"); }
                }
            }
        }
        OperandCode::x87_df => {
            if modrm >= 0xc0 {
                match r {
                    0 => (Opcode::FFREEP, OperandCodeX87::Est),
                    1 => (Opcode::FXCH, OperandCodeX87::St_Est),
                    2 => (Opcode::FSTP, OperandCodeX87::Est_St),
                    3 => (Opcode::FSTP, OperandCodeX87::Est_St),
                    4 => {
                        if modrm == 0xe0 {
                            (Opcode::FNSTSW, OperandCodeX87::Ew)
                        } else {
                            return Err(DecodeError::InvalidOpcode);
                        }
                    },
                    5 => (Opcode::FUCOMIP, OperandCodeX87::St_Est),
                    6 => (Opcode::FCOMIP, OperandCodeX87::St_Est),
                    7 => {
                        return Err(DecodeError::InvalidOpcode);
                    },
                    _ => { unreachable!("impossible r"); }
                }
            } else {
                match r {
                    0 => (Opcode::FILD, OperandCodeX87::St_Mw),
                    1 => (Opcode::FISTTP, OperandCodeX87::Mw_St),
                    2 => (Opcode::FIST, OperandCodeX87::Mw_St),
                    3 => (Opcode::FISTP, OperandCodeX87::Mw_St),
                    4 => (Opcode::FBLD, OperandCodeX87::St_Mm),
                    5 => (Opcode::FILD, OperandCodeX87::St_Mq),
                    6 => (Opcode::FBSTP, OperandCodeX87::Mm_St),
                    7 => (Opcode::FISTP, OperandCodeX87::Mq_St),
                    _ => { unreachable!("impossible r"); }
                }
            }
        }
        other => {
            panic!("invalid x87 operand dispatch, operand code is {:?}", other);
        }
    };
    instruction.opcode = opcode;
    if instruction.opcode == Opcode::Invalid {
        return Err(DecodeError::InvalidOpcode);
    }

    match x87_operands {
        OperandCodeX87::Est => {
            instruction.operands[0] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operand_count = 1;
        }
        OperandCodeX87::St_Est => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Edst => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = 4;
            }
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Eqst => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = 8;
            }
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Ew => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.mem_size = 2;
            }
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Mm => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[1] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 10;
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Mq => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[1] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 8;
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Md => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[1] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 4;
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Mw => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[1] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 2;
            instruction.operand_count = 2;
        }
        OperandCodeX87::Ew => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            instruction.operand_count = 1;
            if instruction.operands[0] != OperandSpec::RegMMM {
                instruction.mem_size = 2;
            }
        }
        OperandCodeX87::Est_St => {
            instruction.operands[0] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Edst_St => {
            instruction.operands[0] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
            if instruction.operands[0] != OperandSpec::RegMMM {
                instruction.mem_size = 4;
            }
        }
        OperandCodeX87::Eqst_St => {
            instruction.operands[0] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
            if instruction.operands[0] != OperandSpec::RegMMM {
                instruction.mem_size = 8;
            }
        }
        OperandCodeX87::Ed_St => {
            instruction.operands[0] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            if instruction.operands[0] != OperandSpec::RegMMM {
                instruction.mem_size = 4;
            }
            instruction.operand_count = 2;
        }
        OperandCodeX87::Mm_St => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 10;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Mq_St => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 8;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Md_St => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 4;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Mw_St => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 2;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Ex87S => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            instruction.operand_count = 1;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.mem_size = 63;
        }
        OperandCodeX87::Nothing => {
            instruction.operand_count = 0;
        },
    }

    Ok(())
}

fn decode_one<'b, T: IntoIterator<Item=u8>>(decoder: &InstDecoder, bytes: T, instr: &'b mut Instruction) -> Result<(), DecodeError> {
    let bytes_iter = bytes.into_iter();
    read_instr(decoder, bytes_iter, instr)
}

#[inline]
fn read_num<T: Iterator<Item=u8>>(bytes: &mut T, width: u8) -> Result<u32, DecodeError> {
    match width {
        1 => { bytes.next().map(|x| x as u32).ok_or(DecodeError::ExhaustedInput) }
        2 => {
            bytes.next().and_then(|b0| {
                bytes.next().map(|b1| u16::from_le_bytes([b0, b1]) as u32)
            }).ok_or(DecodeError::ExhaustedInput)
        }
        4 => {
            bytes.next()
                .and_then(|b0| bytes.next().map(|b1| (b0, b1)))
                .and_then(|(b0, b1)| bytes.next().map(|b2| (b0, b1, b2)))
                .and_then(|(b0, b1, b2)| bytes.next().map(|b3| u32::from_le_bytes([b0, b1, b2, b3])))
                .ok_or(DecodeError::ExhaustedInput)
        }
        _ => {
            panic!("unsupported read size");
        }
    }
}

#[inline]
fn read_imm_signed<T: Iterator<Item=u8>>(bytes: &mut T, num_width: u8, length: &mut u8) -> Result<i32, DecodeError> {
    if num_width == 1 {
        *length += 1;
        Ok(read_num(bytes, 1)? as i8 as i32)
    } else if num_width == 2 {
        *length += 2;
        Ok(read_num(bytes, 2)? as i16 as i32)
    } else {
        *length += 4;
        Ok(read_num(bytes, 4)? as i32)
    }
}

#[inline]
fn read_imm_unsigned<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<u32, DecodeError> {
    read_num(bytes, width).map(|res| {
        *length += width;
        res
    })
}

#[inline]
fn read_modrm<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<u8, DecodeError> {
    bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })
}
