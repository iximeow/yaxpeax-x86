mod vex;
mod display;
pub mod uarch;

use core::hint::unreachable_unchecked;

use yaxpeax_arch::{AddressDiff, Decoder, LengthedInstruction};

#[cfg(feature="use-serde")]
#[derive(Copy, Clone, Debug, PartialOrd, Ord, Eq, PartialEq, Serialize, Deserialize)]
pub struct RegSpec {
    pub num: u8,
    pub bank: RegisterBank
}
#[cfg(not(feature="use-serde"))]
#[derive(Copy, Clone, Debug, PartialOrd, Ord, Eq, PartialEq)]
pub struct RegSpec {
    pub num: u8,
    pub bank: RegisterBank
}

use core::hash::Hash;
use core::hash::Hasher;
impl Hash for RegSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let code = ((self.bank as u16) << 8) | (self.num as u16);
        code.hash(state);
    }
}

// This is only to select alternate opcode maps for the 0f escape byte.
// This often could be treated as a size prefix but in some cases selects
// an entirely different operation.
#[derive(Debug)]
enum OpcodeMap {
    Map66,
    MapF2,
    MapF3,
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
    pub fn name(&self) -> &'static str {
        display::regspec_label(self)
    }

    #[inline]
    fn st(num: u8) -> RegSpec {
        if num >= 8 {
            panic!("invalid x87 reg st({})", num);
        }

        RegSpec {
            num,
            bank: RegisterBank::ST
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

    #[inline]
    pub fn rip() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::RIP
        }
    }

    #[inline]
    pub fn eip() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::EIP
        }
    }

    #[inline]
    pub fn eflags() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::EFlags
        }
    }

    #[inline]
    pub fn rflags() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::RFlags
        }
    }

    #[inline]
    pub fn rbp() -> RegSpec {
        RegSpec {
            num: 5,
            bank: RegisterBank::Q
        }
    }

    #[inline]
    pub fn rsp() -> RegSpec {
        RegSpec {
            num: 4,
            bank: RegisterBank::Q
        }
    }

    #[inline]
    pub fn esp() -> RegSpec {
        RegSpec {
            num: 4,
            bank: RegisterBank::D
        }
    }

    #[inline]
    pub fn sp() -> RegSpec {
        RegSpec {
            num: 4,
            bank: RegisterBank::W
        }
    }

    #[inline]
    pub fn fs() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 3 }
    }

    #[inline]
    pub fn gs() -> RegSpec {
        RegSpec { bank: RegisterBank::S, num: 4 }
    }

    #[inline]
    pub fn rax() -> RegSpec {
        RegSpec { bank: RegisterBank::Q, num: 0 }
    }

    #[inline]
    pub fn rcx() -> RegSpec {
        RegSpec { bank: RegisterBank::Q, num: 1 }
    }

    #[inline]
    pub fn rdx() -> RegSpec {
        RegSpec { bank: RegisterBank::Q, num: 2 }
    }

    #[inline]
    pub fn rsi() -> RegSpec {
        RegSpec { bank: RegisterBank::Q, num: 6 }
    }

    #[inline]
    pub fn rdi() -> RegSpec {
        RegSpec { bank: RegisterBank::Q, num: 7 }
    }

    #[inline]
    pub fn r8() -> RegSpec {
        RegSpec { bank: RegisterBank::Q, num: 8 }
    }

    #[inline]
    pub fn r9() -> RegSpec {
        RegSpec { bank: RegisterBank::Q, num: 9 }
    }

    #[inline]
    pub fn eax() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 0 }
    }

    #[inline]
    pub fn ecx() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 1 }
    }

    #[inline]
    pub fn edx() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 2 }
    }

    #[inline]
    pub fn ebx() -> RegSpec {
        RegSpec { bank: RegisterBank::D, num: 3 }
    }

    #[inline]
    pub fn ax() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 0 }
    }

    #[inline]
    pub fn dx() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 2 }
    }

    #[inline]
    pub fn al() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 0 }
    }

    #[inline]
    pub fn cl() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 1 }
    }

    #[inline]
    pub fn dl() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 2 }
    }

    #[inline]
    pub fn ah() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 4 }
    }

    #[inline]
    pub fn ch() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 5 }
    }

    #[inline]
    pub fn width(&self) -> u8 {
        match self.bank {
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
#[allow(dead_code)]
enum SizeCode {
    b,
    vd,
    vq,
    vqp
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    ImmediateI8(i8),
    ImmediateU8(u8),
    ImmediateI16(i16),
    ImmediateU16(u16),
    ImmediateU32(u32),
    ImmediateI32(i32),
    ImmediateU64(u64),
    ImmediateI64(i64),
    Register(RegSpec),
    DisplacementU32(u32),
    DisplacementU64(u64),
    RegDeref(RegSpec),
    RegDisp(RegSpec, i32),
    RegScale(RegSpec, u8),
    RegIndexBase(RegSpec, RegSpec),
    RegIndexBaseDisp(RegSpec, RegSpec, i32),
    RegScaleDisp(RegSpec, u8, i32),
    RegIndexBaseScale(RegSpec, RegSpec, u8),
    RegIndexBaseScaleDisp(RegSpec, RegSpec, u8, i32),
    Nothing,
}

impl OperandSpec {
    pub fn is_memory(&self) -> bool {
        match self {
            OperandSpec::DispU32 |
            OperandSpec::DispU64 |
            OperandSpec::Deref |
            OperandSpec::Deref_rsi |
            OperandSpec::Deref_rdi |
            OperandSpec::RegDisp |
            OperandSpec::RegScale |
            OperandSpec::RegIndexBase |
            OperandSpec::RegIndexBaseDisp |
            OperandSpec::RegScaleDisp |
            OperandSpec::RegIndexBaseScale |
            OperandSpec::RegIndexBaseScaleDisp => {
                true
            },
            OperandSpec::ImmI8 |
            OperandSpec::ImmI16 |
            OperandSpec::ImmI32 |
            OperandSpec::ImmI64 |
            OperandSpec::ImmU8 |
            OperandSpec::ImmU16 |
            OperandSpec::ImmU32 |
            OperandSpec::ImmU64 |
            OperandSpec::RegRRR |
            OperandSpec::RegMMM |
            OperandSpec::RegVex |
            OperandSpec::EnterFrameSize |
            OperandSpec::Nothing => {
                false
            }
        }
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
            // the register in modrm_mmm (eg modrm mod bits were 11)
            OperandSpec::RegMMM => {
                Operand::Register(inst.modrm_mmm)
            }
            OperandSpec::RegVex => {
                Operand::Register(inst.vex_reg)
            }
            OperandSpec::ImmI8 => Operand::ImmediateI8(inst.imm as i8),
            OperandSpec::ImmU8 => Operand::ImmediateU8(inst.imm as u8),
            OperandSpec::ImmI16 => Operand::ImmediateI16(inst.imm as i16),
            OperandSpec::ImmU16 => Operand::ImmediateU16(inst.imm as u16),
            OperandSpec::ImmI32 => Operand::ImmediateI32(inst.imm as i32),
            OperandSpec::ImmU32 => Operand::ImmediateU32(inst.imm as u32),
            OperandSpec::ImmI64 => Operand::ImmediateI64(inst.imm as i64),
            OperandSpec::ImmU64 => Operand::ImmediateU64(inst.imm as u64),
            OperandSpec::EnterFrameSize => Operand::ImmediateU16(inst.disp as u16),
            OperandSpec::DispU32 => Operand::DisplacementU32(inst.disp as u32),
            OperandSpec::DispU64 => Operand::DisplacementU64(inst.disp as u64),
            OperandSpec::Deref => {
                Operand::RegDeref(inst.modrm_mmm)
            }
            OperandSpec::Deref_rsi => {
                Operand::RegDeref(RegSpec::rsi())
            }
            OperandSpec::Deref_rdi => {
                Operand::RegDeref(RegSpec::rdi())
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
        }
    }
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
            Operand::RegIndexBaseScaleDisp(_, _, _, _) => {
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
            Operand::Nothing => {
                false
            }
        }
    }

    pub fn width(&self) -> u8 {
        match self {
            Operand::Nothing => {
                panic!("non-operand does not have a size");
            }
            Operand::Register(reg) => {
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
            Operand::ImmediateI64(_) |
            Operand::ImmediateU64(_) => {
                8
            }
            // memory operands
            _ => {
                8
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

#[allow(non_camel_case_types)]
#[cfg(feature="use-serde")]
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP, EFlags, RFlags,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
    K, // AVX512 mask registers
}
#[allow(non_camel_case_types)]
#[cfg(not(feature="use-serde"))]
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP, EFlags, RFlags,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
    K, // AVX512 mask registers
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

// TODO:
// PTWRITE
// TPAUSE
// UMONITOR
// UMWAIT
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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
    UD2,
    WBINVD,
    INVD,
    SYSRET,
    CLTS,
    SYSCALL,
    LSL,
    LAR,
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
    UD2E,
    VMREAD,
    VMWRITE,
    XORPS,
    XORPD,

    VMOVDDUP,
    VPSHUFLW,
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
    VPHADDUBSW,
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
    VPMAXUD,
    VPMINSW,
    VPMINSD,
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
    imm: u64,
    disp: u64,
    pub opcode: Opcode,
}

impl yaxpeax_arch::Instruction for Instruction {
    fn well_defined(&self) -> bool {
        // TODO: this is incorrect!
        true
    }
}

#[derive(Debug, PartialEq)]
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
    // the register in modrm_mmm (eg modrm mod bits were 11)
    RegMMM,
    // the register selected by vex-vvvv bits
    RegVex,
    ImmI8,
    ImmI16,
    ImmI32,
    ImmI64,
    ImmU8,
    ImmU16,
    ImmU32,
    ImmU64,
    // ENTER is a two-immediate instruction, where the first immediate is stored in the disp field.
    // for this case, a second immediate-style operand is needed.
    EnterFrameSize,
    DispU32,
    DispU64,
    Deref,
    Deref_rsi,
    Deref_rdi,
    RegDisp,
    RegScale,
    RegIndexBase,
    RegIndexBaseDisp,
    RegScaleDisp,
    RegIndexBaseScale,
    RegIndexBaseScaleDisp
}

// the Hash, Eq, and PartialEq impls here are possibly misleading.
// They exist because downstream some structs are spelled like
// Foo<T> for T == x86_64. This is only to access associated types
// which themselves are bounded, but their #[derive] require T to
// implement these traits.
#[cfg(feature="use-serde")]
#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub struct Arch;

#[cfg(not(feature="use-serde"))]
#[derive(Hash, Eq, PartialEq, Debug)]
#[allow(non_camel_case_types)]
pub struct Arch;

impl yaxpeax_arch::Arch for Arch {
    type Address = u64;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = InstDecoder;
    type Operand = Operand;
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
    // 44. cx8 // cmpxchg8 - is this actually optional in x86_64?
    // 45. syscall // syscall/sysret - actually optional in x86_64?
    // 46. rdtscp // actually optional in x86_64?
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
    /// Instantiates an x86_64 decoder that decodes the bare minimum of x86_64.
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
    /// and `SHLX` instructions. `bmi2` is implemented in all x86_64 chips that implement `bmi`,
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
            Opcode::VPHADDUBSW |
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
            Opcode::VPMAXUD |
            Opcode::VPMINSW |
            Opcode::VPMINSD |
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
            Opcode::VZEROUPPER => {
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
    /// Instantiates an x86_64 decoder that probably decodes what you want.
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
    pub fn operand(&self, i: u8) -> Operand {
        assert!(i < 4);
        Operand::from_spec(self, self.operands[i as usize])
    }

    pub fn operand_count(&self) -> u8 {
        let mut i = 0;
        for op in self.operands.iter() {
            if let OperandSpec::Nothing = op {
                return i;
            } else {
                i += 1;
            }
        }
        return i;
    }

    pub fn operand_present(&self, i: u8) -> bool {
        assert!(i < 4);
        if let OperandSpec::Nothing = self.operands[i as usize] {
            false
        } else {
            true
        }
    }

    pub fn invalid() -> Instruction {
        Instruction {
            prefixes: Prefixes::new(0),
            opcode: Opcode::Invalid,
            modrm_rrr: RegSpec::rax(),
            modrm_mmm: RegSpec::rax(), // doubles as sib_base
            sib_index: RegSpec::rax(),
            vex_reg: RegSpec::rax(),
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
}

#[derive(Debug, Copy, Clone)]
pub struct Prefixes {
    bits: u8,
    rex: PrefixRex,
    segment: Segment,
    _pad: u8,
}

#[derive(Debug, Copy, Clone)]
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
}

#[derive(Debug, Copy, Clone)]
pub struct PrefixRex {
    bits: u8
}

#[allow(dead_code)]
impl Prefixes {
    fn new(bits: u8) -> Prefixes {
        Prefixes {
            bits: bits,
            rex: PrefixRex { bits: 0 },
            segment: Segment::DS,
            _pad: 0,
        }
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
    fn operand_size(&self) -> bool { self.bits & 0x1 == 1 }
    #[inline]
    fn set_operand_size(&mut self) { self.bits = self.bits | 0x1 }
    #[inline]
    fn address_size(&self) -> bool { self.bits & 0x2 == 2 }
    #[inline]
    fn set_address_size(&mut self) { self.bits = self.bits | 0x2 }
    #[inline]
    fn set_lock(&mut self) { self.bits |= 0x4 }
    #[inline]
    pub fn lock(&self) -> bool { self.bits & 0x4 == 4 }
    #[inline]
    fn cs(&mut self) { self.segment = Segment::CS }
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
    fn rex(&self) -> &PrefixRex { &self.rex }
    #[inline]
    fn vex(&self) -> PrefixVex { PrefixVex { bits: self.rex.bits } }

    #[inline]
    fn rex_from(&mut self, bits: u8) {
        self.rex.bits = bits;
    }

    #[inline]
    fn vex_from_c5(&mut self, bits: u8) {
        // collect rex bits
        let r = bits & 0x80;
        let wrxb = (r >> 5) ^ 0x04;
        let l = (bits & 0x04) << 2;
        let synthetic_rex = wrxb | l | 0x80;
        self.rex.from(synthetic_rex);
    }

    #[inline]
    fn vex_from_c4(&mut self, high: u8, low: u8) {
        let w = low & 0x80;
        let rxb = (high >> 5) ^ 0x07;
        let wrxb = rxb | w >> 4;
        let l = (low & 0x04) << 2;
        let synthetic_rex = wrxb | l | 0x80;
        self.rex.from(synthetic_rex);
    }
}

impl PrefixRex {
    #[inline]
    fn present(&self) -> bool { (self.bits & 0xc0) == 0x40 }
    #[inline]
    fn b(&self) -> bool { (self.bits & 0x01) == 0x01 }
    #[inline]
    fn x(&self) -> bool { (self.bits & 0x02) == 0x02 }
    #[inline]
    fn r(&self) -> bool { (self.bits & 0x04) == 0x04 }
    #[inline]
    fn w(&self) -> bool { (self.bits & 0x08) == 0x08 }
    #[inline]
    fn from(&mut self, prefix: u8) {
        self.bits = prefix;
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
    Zv_Ivq_R = 3,
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
        self.bits & 0x1ff
    }

    const fn special_case(mut self, case: u16) -> Self {
        // leave 0x4000 unset
        self.bits |= case & 0x1ff;
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
pub enum OperandCode {
    Nothing = 0,
    CVT_AA,
    CVT_DA,
    Rq_Cq_0,
    Rq_Dq_0,
    Cq_Rq_0,
    Dq_Rq_0,
    FS,
    GS,
    Yb_DX,
    Yv_DX,
    DX_Xb,
    DX_Xv,
    AH,
    AX_Xv,
    // DX_AX,
    // Ev_Ivs,
    Ew_Sw,
    Fw,
    I_3,
    Ib,
    Ibs,
    Ivs,
    Iw,
    Iw_Ib,
    Jvds,
    Ob_AL,
    Ov_AX,
    Sw_Ew,
    Yb_AL,
    Yb_Xb,
    Yv_AX,
    Yv_Xv,

    x87_d8 = OperandCodeBuilder::new().special_case(31).bits(),
    x87_d9 = OperandCodeBuilder::new().special_case(32).bits(),
    x87_da = OperandCodeBuilder::new().special_case(33).bits(),
    x87_db = OperandCodeBuilder::new().special_case(34).bits(),
    x87_dc = OperandCodeBuilder::new().special_case(35).bits(),
    x87_dd = OperandCodeBuilder::new().special_case(36).bits(),
    x87_de = OperandCodeBuilder::new().special_case(37).bits(),
    x87_df = OperandCodeBuilder::new().special_case(38).bits(),

    Eb_R0 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(20)
        .bits(),
    AL_Ib = OperandCodeBuilder::new().special_case(40).bits(),
    AX_Ib = OperandCodeBuilder::new().special_case(41).bits(),
    Ib_AL = OperandCodeBuilder::new().special_case(42).bits(),
    Ib_AX = OperandCodeBuilder::new().special_case(43).bits(),
    AX_DX = OperandCodeBuilder::new().special_case(44).bits(),
    AL_DX = OperandCodeBuilder::new().special_case(45).bits(),
    DX_AX = OperandCodeBuilder::new().special_case(46).bits(),
    DX_AL = OperandCodeBuilder::new().special_case(47).bits(),
    MOVQ_f30f = OperandCodeBuilder::new().special_case(48).bits(),

    Unsupported = OperandCodeBuilder::new().special_case(49).bits(),

    ModRM_0x0f00 = OperandCodeBuilder::new().read_modrm().special_case(0).bits(),
    ModRM_0x0f01 = OperandCodeBuilder::new().read_modrm().special_case(1).bits(),
    ModRM_0x0f0d = OperandCodeBuilder::new().read_modrm().special_case(2).bits(),
    ModRM_0x0fae = OperandCodeBuilder::new().read_modrm().special_case(3).bits(),
    ModRM_0x0fba = OperandCodeBuilder::new().read_modrm().special_case(4).bits(),
    ModRM_0xf30fae = OperandCodeBuilder::new().read_modrm().special_case(6).bits(),
    ModRM_0x660fae = OperandCodeBuilder::new().read_modrm().special_case(7).bits(),
    ModRM_0xf30fc7 = OperandCodeBuilder::new().read_modrm().special_case(8).bits(),
    ModRM_0x660f38 = OperandCodeBuilder::new().read_modrm().special_case(9).bits(),
    ModRM_0xf20f38 = OperandCodeBuilder::new().read_modrm().special_case(24).bits(),
    ModRM_0xf30f38 = OperandCodeBuilder::new().read_modrm().special_case(10).bits(),
    ModRM_0x660f3a = OperandCodeBuilder::new().read_modrm().special_case(11).bits(),
    ModRM_0x0f38 = OperandCodeBuilder::new().read_modrm().special_case(12).bits(),
    ModRM_0x0f3a = OperandCodeBuilder::new().read_modrm().special_case(13).bits(),
    ModRM_0x0f71 = OperandCodeBuilder::new().read_modrm().special_case(14).bits(),
    ModRM_0x0f72 = OperandCodeBuilder::new().read_modrm().special_case(15).bits(),
    ModRM_0x0f73 = OperandCodeBuilder::new().read_modrm().special_case(16).bits(),
    ModRM_0x660f12 = OperandCodeBuilder::new().read_modrm().special_case(17).bits(),
    ModRM_0x660f16 = OperandCodeBuilder::new().read_modrm().special_case(18).bits(),
    ModRM_0x660f71 = OperandCodeBuilder::new().read_modrm().special_case(19).bits(),
    ModRM_0x660f72 = OperandCodeBuilder::new().read_modrm().special_case(20).bits(),
    ModRM_0x660f73 = OperandCodeBuilder::new().read_modrm().special_case(21).bits(),
    ModRM_0x660fc7 = OperandCodeBuilder::new().read_modrm().special_case(22).bits(),
    ModRM_0x0fc7 = OperandCodeBuilder::new().read_modrm().special_case(23).bits(),
    // xmmword?
    ModRM_0x0f12 = OperandCodeBuilder::new()
        .read_modrm()
        .op0_is_rrr_and_embedded_instructions()
        .read_E()
        .reg_mem()
        .operand_case(0)
        .bits(),
    // xmmword?
    ModRM_0x0f16 = OperandCodeBuilder::new()
        .read_modrm()
        .op0_is_rrr_and_embedded_instructions()
        .read_E()
        .reg_mem()
        .operand_case(1)
        .bits(),
    // encode immediates?
    ModRM_0xc0_Eb_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(0)
        .bits(),
    ModRM_0xc1_Ev_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(0)
        .bits(),
    ModRM_0xd0_Eb_1 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(1)
        .bits(),
    ModRM_0xd1_Ev_1 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(1)
        .bits(),
    ModRM_0xd2_Eb_CL = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(2)
        .bits(),
    ModRM_0xd3_Ev_CL = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(2)
        .bits(),
    ModRM_0x80_Eb_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(3)
        .bits(),
    ModRM_0x83_Ev_Ibs = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(3)
        .bits(),
    // this would be Eb_Ivs, 0x8e
    ModRM_0x81_Ev_Ivs = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(4)
        .bits(),
    ModRM_0xc6_Eb_Ib = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(5)
        .bits(),
    ModRM_0xc7_Ev_Iv = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(5)
        .bits(),
    ModRM_0xfe_Eb = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(6)
        .bits(),
    ModRM_0x8f_Ev = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(6)
        .bits(),
    // gap, 0x94
    ModRM_0xff_Ev = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(7)
        .bits(),
    ModRM_0x0f18 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(8)
        .bits(),
    ModRM_0xf6 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .byte_operands()
        .operand_case(9)
        .bits(),
    ModRM_0xf7 = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(10)
        .bits(),
    Ev = OperandCodeBuilder::new()
        .read_modrm()
        .set_embedded_instructions()
        .read_E()
        .operand_case(11)
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
    Zv_Ivq_R0 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 0).bits(),
    Zv_Ivq_R1 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 1).bits(),
    Zv_Ivq_R2 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 2).bits(),
    Zv_Ivq_R3 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 3).bits(),
    Zv_Ivq_R4 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 4).bits(),
    Zv_Ivq_R5 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 5).bits(),
    Zv_Ivq_R6 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 6).bits(),
    Zv_Ivq_R7 = OperandCodeBuilder::new().op0_is_rrr_and_Z_operand(ZOperandCategory::Zv_Ivq_R, 7).bits(),
    Gv_Eb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(4).bits(),
    Gv_Ew = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(5).bits(),
    Gv_Ew_LSL = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(7).bits(),
    Gdq_Ed = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(6).bits(),
    Gdq_Ev = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(10).bits(),
    Mdq_Gdq = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(11).bits(),
    G_E_mm_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(2).bits(),
    G_E_xmm_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(8).bits(),
    AL_Ob = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(0).bits(),
    AL_Xb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(1).bits(),
    AX_Ov = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(3).bits(),
    AL_Ibs = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().byte_operands().operand_case(0).bits(),
    AX_Ivd = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(9).bits(),

    Eb_Gb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().byte_operands().only_modrm_operands().mem_reg().bits(),
    Ev_Gv = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().only_modrm_operands().mem_reg().bits(),
    Gb_Eb = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().byte_operands().only_modrm_operands().reg_mem().bits(),
    Gv_Ev = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().only_modrm_operands().reg_mem().bits(),
    Gv_M = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().only_modrm_operands().reg_mem().operand_case(25).bits(),
    Gb_Eb_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().byte_operands().reg_mem().operand_case(1).bits(),
    Gv_Ev_Iv = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(1).bits(),
    Rv_Gmm_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_modrm().read_E().reg_mem().operand_case(25).bits(),
    // gap, 0x9a
    G_xmm_E_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(23).bits(),
    G_xmm_U_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(24).bits(),
    U_mm_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(25).bits(),
    G_xmm_Edq = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(26).bits(),
    G_xmm_Eq = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(27).bits(),
    G_mm_E_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(28).bits(),
    Gd_U_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(29).bits(),
    Gv_E_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(30).bits(),
    //= 0x816f, // mirror G_xmm_Edq, but also read an immediate
    G_xmm_Ed_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(31).bits(),
    G_U_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(13).bits(),
    G_M_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(10).bits(),
    G_E_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(9).bits(),
    E_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(11).bits(),
    M_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(12).bits(),
    G_E_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(14).bits(),
    G_U_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(15).bits(),
    E_G_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(16).bits(),
    Edq_G_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(17).bits(),
    Edq_G_xmm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(18).bits(),
    G_mm_Edq = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(19).bits(),
    G_mm_E = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().mem_reg().operand_case(20).bits(),
    Ev_Gv_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(21).bits(),
    Ev_Gv_CL = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(22).bits(),
    G_mm_U_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(32).bits(),
    G_Md_mm = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().read_E().reg_mem().operand_case(33).bits(),
    G_mm_Ew_Ib = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(20).bits(),
    G_E_q = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(21).bits(),
    E_G_q = OperandCodeBuilder::new().op0_is_rrr_and_embedded_instructions().operand_case(22).bits(),
}

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

const OPCODE_660F_MAP: [OpcodeRecord; 256] = [
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x10
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPD), OperandCode::E_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660f12),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVLPD), OperandCode::M_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKLPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKHPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVHPD), OperandCode::ModRM_0x660f16),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVHPD), OperandCode::M_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x20
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPD), OperandCode::E_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPI2PD), OperandCode::G_xmm_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTPD), OperandCode::M_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPD2PI), OperandCode::G_mm_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPD2PI), OperandCode::G_mm_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::UCOMISD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::COMISD), OperandCode::G_E_xmm),
// 0x30
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660f38),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660f3a),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x40
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x50
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVMSKPD), OperandCode::Gd_U_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::SQRTPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::ANDPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::ANDNPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::ORPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::XORPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADDPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MULPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPD2PS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPS2DQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUBPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MINPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::DIVPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MAXPD), OperandCode::G_E_xmm),
// 0x60
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLBW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLWD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLDQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSWB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PACKUSWB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHBW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHWD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHDQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSDW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLQDQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHQDQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::G_xmm_Eq),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQA), OperandCode::G_E_xmm),
// 0x70
    OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFD), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660f71),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660f72),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660f73),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::HADDPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::HSUBPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVD), OperandCode::Edq_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQA), OperandCode::E_G_xmm),
// 0x80
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x90
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xa0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660fae),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xb0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xc0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPPD), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PINSRW), OperandCode::G_xmm_Ed_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::PEXTRW), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHUFPD), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x660fc7),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xd0
    OpcodeRecord(Interpretation::Instruction(Opcode::ADDSUBPD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRLW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRLD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRLQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULLW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::E_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMOVMSKB), OperandCode::Gd_U_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMINUB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PAND), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMAXUB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PANDN), OperandCode::G_E_xmm),
// 0xe0
    OpcodeRecord(Interpretation::Instruction(Opcode::PAVGB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRAW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRAD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PAVGW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULHUW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULHW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPD2DQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTDQ), OperandCode::M_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMINSW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::POR), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDSB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDSW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMAXSW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PXOR), OperandCode::G_E_xmm),
// 0xf0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULUDQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMADDWD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSADBW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MASKMOVDQU), OperandCode::G_U_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDB), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDW), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDQ), OperandCode::G_E_xmm),
];

const OPCODE_F20F_MAP: [OpcodeRecord; 256] = [
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x10
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSD), OperandCode::E_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVDDUP), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x20
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSI2SD), OperandCode::G_xmm_Edq),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTSD2SI), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSD2SI), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x30
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf20f38),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x40
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x50
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SQRTSD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADDSD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MULSD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSD2SS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUBSD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MINSD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::DIVSD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MAXSD), OperandCode::G_E_xmm),
// 0x60
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x70
    OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFLW), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::HADDPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::HSUBPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x80
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x90
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xa0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xb0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xc0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPSD), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xd0
    OpcodeRecord(Interpretation::Instruction(Opcode::ADDSUBPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQ2Q), OperandCode::U_mm_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xe0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPD2DQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xf0
    OpcodeRecord(Interpretation::Instruction(Opcode::LDDQU), OperandCode::G_M_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
];

const OPCODE_F30F_MAP: [OpcodeRecord; 256] = [
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x10
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSS), OperandCode::E_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSLDUP), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSHDUP), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x20
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSI2SS), OperandCode::G_xmm_Edq),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTSS2SI), OperandCode::Gv_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSS2SI), OperandCode::Gv_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x30
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30f38),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x40
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x50
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SQRTSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::RSQRTSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::RCPSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADDSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MULSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSS2SD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPS2DQ), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUBSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MINSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::DIVSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MAXSS), OperandCode::G_E_xmm),
// 0x60
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQU), OperandCode::G_E_xmm),
// 0x70
    OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFHW), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::MOVQ_f30f),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQU), OperandCode::E_G_xmm),
// 0x80
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x90
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xa0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30fae),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xb0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::POPCNT), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::LZCNT), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xc0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPSS), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf30fc7),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xd0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ2DQ), OperandCode::G_xmm_U_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xe0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTDQ2PD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xf0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
];

const OPCODE_0F_MAP: [OpcodeRecord; 256] = [
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f00),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f01),
    OpcodeRecord(Interpretation::Instruction(Opcode::LAR), OperandCode::Gv_Ew),
    OpcodeRecord(Interpretation::Instruction(Opcode::LSL), OperandCode::Gv_Ew_LSL),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SYSCALL), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CLTS), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SYSRET), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::INVD), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::WBINVD), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::UD2), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f0d),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x10
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVUPS), OperandCode::E_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f12),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVLPS), OperandCode::M_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKLPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKHPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f16),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVHPS), OperandCode::M_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f18),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
// 0x20
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Cq_0),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Rq_Dq_0),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Cq_Rq_0),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Dq_Rq_0),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVAPS), OperandCode::E_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPI2PS), OperandCode::G_xmm_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTPS), OperandCode::M_G_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPS2PI), OperandCode::G_mm_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPS2PI), OperandCode::G_mm_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::UCOMISS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::COMISS), OperandCode::G_E_xmm),

// 0x30
    OpcodeRecord(Interpretation::Instruction(Opcode::WRMSR), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RDTSC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RDMSR), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RDPMC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SYSENTER), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SYSEXIT), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::GETSEC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f38),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f3a),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

// 0x40
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVO), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNO), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVZ), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNZ), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNA), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVA), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVS), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNS), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVP), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVNP), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVL), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVGE), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVLE), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMOVG), OperandCode::Gv_Ev),

// 0x50
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVMSKPS), OperandCode::Gd_U_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::SQRTPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::RSQRTPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::RCPPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::ANDPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::ANDNPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::ORPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::XORPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADDPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MULPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTPS2PD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTDQ2PS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUBPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MINPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::DIVPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MAXPS), OperandCode::G_E_xmm),

// 0x60
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLBW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLWD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKLDQ), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSWB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPGTD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PACKUSWB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHBW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHWD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUNPCKHDQ), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PACKSSDW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVD), OperandCode::G_mm_Edq),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::G_mm_E),

// 0x70
    OpcodeRecord(Interpretation::Instruction(Opcode::PSHUFW), OperandCode::G_E_mm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f71),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f72),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f73),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::EMMS), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::VMREAD), OperandCode::E_G_q),
    OpcodeRecord(Interpretation::Instruction(Opcode::VMWRITE), OperandCode::G_E_q),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVD), OperandCode::Edq_G_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::E_G_mm),

// 0x80
    OpcodeRecord(Interpretation::Instruction(Opcode::JO), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNO), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JB), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNB), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JZ), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNZ), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNA), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JA), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JS), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNS), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JP), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNP), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JL), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JGE), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JLE), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JG), OperandCode::Jvds),

// 0x90
    OpcodeRecord(Interpretation::Instruction(Opcode::SETO), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETNO), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETB), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETAE), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETZ), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETNZ), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETBE), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETA), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETS), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETNS), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETP), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETNP), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETL), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETGE), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETLE), OperandCode::Eb_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::SETG), OperandCode::Eb_R0),

// 0xa0
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::FS),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::FS),
    OpcodeRecord(Interpretation::Instruction(Opcode::CPUID), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::BT), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHLD), OperandCode::Ev_Gv_CL),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::GS),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::GS),
    OpcodeRecord(Interpretation::Instruction(Opcode::RSM), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::BTS), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Ev_Gv_CL),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fae),
    OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev),

// 0xb0
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::LSS), OperandCode::Gv_M),
    OpcodeRecord(Interpretation::Instruction(Opcode::BTR), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::LFS), OperandCode::Gv_M),
    OpcodeRecord(Interpretation::Instruction(Opcode::LGS), OperandCode::Gv_M),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_b), OperandCode::Gv_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_w), OperandCode::Gv_Ew),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing), // JMPE, ITANIUM
    OpcodeRecord(Interpretation::Instruction(Opcode::UD2E), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fba),
    OpcodeRecord(Interpretation::Instruction(Opcode::BTC), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::TZCNT), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_b), OperandCode::Gv_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_w), OperandCode::Gv_Ew),

// 0xc0
    OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPPS), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTI), OperandCode::Mdq_Gdq),
    OpcodeRecord(Interpretation::Instruction(Opcode::PINSRW), OperandCode::G_mm_Ew_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::PEXTRW), OperandCode::Rv_Gmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHUFPS), OperandCode::G_E_xmm_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fc7),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSWAP), OperandCode::Zv_R7),

// 0xd0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRLW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRLD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRLQ), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDQ), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULLW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMOVMSKB), OperandCode::G_U_mm),

    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMINUB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PAND), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMAXUB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PANDN), OperandCode::G_E_mm),

// 0xe0
    OpcodeRecord(Interpretation::Instruction(Opcode::PAVGB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRAW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRAD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PAVGW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULHUW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULHW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTQ), OperandCode::G_Md_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMINSW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::POR), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDSB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDSW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMAXSW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PXOR), OperandCode::G_E_mm),
// 0xf0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLQ), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULUDQ), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMADDWD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSADBW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MASKMOVQ), OperandCode::G_mm_U_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBQ), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDB), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDW), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDD), OperandCode::G_E_mm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::AL_Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0x40:
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSXD), OperandCode::Gdq_Ed),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Ivs),
    OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev_Iv),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::Ibs),
    OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gb_Eb_Ib),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XCHG), OperandCode::Gv_Ev),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R0),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R1),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R2),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R3),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R4),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R5),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R6),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::Zv_Ivq_R7),
// 0xc0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xc0_Eb_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xc1_Ev_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETURN), OperandCode::Iw),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETURN), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::ModRM_0xc6_Eb_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOV), OperandCode::ModRM_0xc7_Ev_Iv),
    OpcodeRecord(Interpretation::Instruction(Opcode::ENTER), OperandCode::Iw_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::LEAVE), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETF), OperandCode::Iw),
    OpcodeRecord(Interpretation::Instruction(Opcode::RETF), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::INT), OperandCode::I_3),
    OpcodeRecord(Interpretation::Instruction(Opcode::INT), OperandCode::Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::IRET), OperandCode::Fw),
// 0xd0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd0_Eb_1),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd1_Ev_1),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd2_Eb_CL),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xd3_Ev_CL),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
// 0xf4
    OpcodeRecord(Interpretation::Instruction(Opcode::HLT), OperandCode::Nothing),
    // CMC
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    let bank = width_to_gp_reg_bank(width, instr.prefixes.rex().present());
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, bank)
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
fn read_modrm_reg(instr: &mut Instruction, modrm: u8, reg_bank: RegisterBank) -> Result<OperandSpec, DecodeError> {
    instr.modrm_mmm = RegSpec::from_parts(modrm & 7, instr.prefixes.rex().b(), reg_bank);
    Ok(OperandSpec::RegMMM)
}

#[allow(non_snake_case)]
fn read_sib<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    let modbits = modrm >> 6;
    let addr_width = if instr.prefixes.address_size() { RegisterBank::D } else { RegisterBank::Q };
    let sibbyte = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
    *length += 1;

    let disp = if modbits == 0b00 {
        if (sibbyte & 7) == 0b101 {
            *length += 4;
            read_num(bytes_iter, 4)? as i32
        } else {
            0
        }
    } else if modbits == 0b01 {
        *length += 1;
        read_num(bytes_iter, 1)? as i8 as i32
    } else {
        *length += 4;
        read_num(bytes_iter, 4)? as i32
    };
    instr.disp = disp as u32 as u64;

    let op_spec = if (sibbyte & 7) == 0b101 {
        if ((sibbyte >> 3) & 7) == 0b100 {
            if modbits == 0b00 && !instr.prefixes.rex().x() {
                instr.disp = disp as u32 as u64;

                OperandSpec::DispU32
            } else {
                if instr.prefixes.rex().x() {
                    let reg = RegSpec::from_parts(0b100, true, addr_width);
                    instr.sib_index = reg;
                    let scale = 1u8 << (sibbyte >> 6);
                    instr.scale = scale;

                    if disp == 0 {
                        OperandSpec::RegIndexBaseScale
                    } else {
                        instr.disp = disp as i64 as u64;
                        OperandSpec::RegIndexBaseScaleDisp
                    }
                } else {
                    let reg = RegSpec::from_parts(0b101, instr.prefixes.rex().b(), addr_width);
                    instr.modrm_mmm = reg;

                    if disp == 0 {
                        OperandSpec::Deref
                    } else {
                        instr.disp = disp as i64 as u64;
                        OperandSpec::RegDisp
                    }
                }
            }
        } else {
            instr.modrm_mmm = RegSpec::from_parts(5, instr.prefixes.rex().b(), addr_width);

            instr.sib_index = RegSpec::from_parts((sibbyte >> 3) & 7, instr.prefixes.rex().x(), addr_width);
            let scale = 1u8 << (sibbyte >> 6);
            instr.scale = scale;

            if disp == 0 {
                if modbits == 0 {
                    OperandSpec::RegScale
                } else {
                    OperandSpec::RegIndexBaseScale
                }
            } else {
                instr.disp = disp as i64 as u64;
                if modbits == 0 {
                    OperandSpec::RegScaleDisp
                } else {
                    OperandSpec::RegIndexBaseScaleDisp
                }
            }
        }
    } else {
        instr.modrm_mmm = RegSpec::from_parts(sibbyte & 7, instr.prefixes.rex().b(), addr_width);

        if ((sibbyte >> 3) & 7) == 0b100 {
            if instr.prefixes.rex().x() {
                let reg = RegSpec::from_parts(0b100, true, addr_width);
                instr.sib_index = reg;
                let scale = 1u8 << (sibbyte >> 6);
                instr.scale = scale;

                if disp == 0 {
                    OperandSpec::RegIndexBaseScale
                } else {
                    instr.disp = disp as i64 as u64;
                    OperandSpec::RegIndexBaseScaleDisp
                }
            } else {
                if disp == 0 {
                    OperandSpec::Deref
                } else {
                    instr.disp = disp as i64 as u64;
                    OperandSpec::RegDisp
                }
            }
        } else {
            instr.sib_index = RegSpec::from_parts((sibbyte >> 3) & 7, instr.prefixes.rex().x(), addr_width);
            let scale = 1u8 << (sibbyte >> 6);
            instr.scale = scale;
            if disp == 0 {
                OperandSpec::RegIndexBaseScale
            } else {
                instr.disp = disp as i64 as u64;
                OperandSpec::RegIndexBaseScaleDisp
            }
        }
    };
    Ok(op_spec)
}

#[allow(non_snake_case)]
fn read_M<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, DecodeError> {
    let modbits = modrm >> 6;
    let addr_width = if instr.prefixes.address_size() { RegisterBank::D } else { RegisterBank::Q };
    let mmm = modrm & 7;
    let op_spec = if mmm == 4 {
        return read_sib(bytes_iter, instr, modrm, length);
    } else if mmm == 5 && modbits == 0b00 {
        *length += 4;
        let disp = read_num(bytes_iter, 4)? as i32;
        instr.modrm_mmm =
            if addr_width == RegisterBank::Q { RegSpec::rip() } else { RegSpec::eip() };
        if disp == 0 {
            OperandSpec::Deref
        } else {
            instr.disp = disp as i64 as u64;
            OperandSpec::RegDisp
        }
    } else {
        instr.modrm_mmm = RegSpec::from_parts(mmm, instr.prefixes.rex().b(), addr_width);

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
                instr.disp = disp as i64 as u64;
                OperandSpec::RegDisp
            }
        }
    };
    Ok(op_spec)
}

#[inline]
fn width_to_gp_reg_bank(width: u8, rex: bool) -> RegisterBank {
    match width {
        1 => return if rex { RegisterBank::rB } else { RegisterBank::B },
        2 => return RegisterBank::W,
        4 => return RegisterBank::D,
        8 => return RegisterBank::Q,
        _ => unsafe { unreachable_unchecked(); }
    }
}

fn read_instr<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction) -> Result<(), DecodeError> {
    let mut length = 0u8;
    let mut alternate_opcode_map: Option<OpcodeMap> = None;
//    use core::intrinsics::unlikely;
    let mut prefixes = Prefixes::new(0);

    fn escapes_are_prefixes_actually(prefixes: &mut Prefixes, opc_map: &mut Option<OpcodeMap>) {
        match opc_map {
            Some(OpcodeMap::Map66) => {
                prefixes.set_operand_size();
            },
            Some(OpcodeMap::MapF2) => {
                prefixes.set_repnz();
            },
            Some(OpcodeMap::MapF3) => {
                prefixes.set_rep();
            },
            None => {}
        }
        *opc_map = None;
    }

    let record: OpcodeRecord = loop {
//    let operand_code = loop {
        match bytes_iter.next() {
            Some(b) => {
                length += 1;
                if length >= 15 {
                    return Err(DecodeError::TooLong);
                }
                let record = OPCODES[b as usize];
                if (b & 0xf0) == 0x40 {
                    prefixes.rex_from(b);
                } else if b == 0x0f {
                    let b = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
                    length += 1;
                    let record = match alternate_opcode_map {
                        Some(opcode_map) => {
                            let rec = match opcode_map {
                                OpcodeMap::Map66 => {
                                    OPCODE_660F_MAP[b as usize]
                                },
                                OpcodeMap::MapF2 => {
                                    OPCODE_F20F_MAP[b as usize]
                                },
                                OpcodeMap::MapF3 => {
                                    OPCODE_F30F_MAP[b as usize]
                                },
                            };
                            if rec == OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing) {
                                escapes_are_prefixes_actually(&mut prefixes, &mut Some(opcode_map));
                                OPCODE_0F_MAP[b as usize]
                            } else {
                                rec
                            }
                        },
                        None => {
                            OPCODE_0F_MAP[b as usize]
                        }
                    };

                    break record;
                } else if let Interpretation::Instruction(_) = record.0 {
                    escapes_are_prefixes_actually(&mut prefixes, &mut alternate_opcode_map);
                    break record;
                } else {
                    // some prefix seen after we saw rex, but before the 0f escape or an actual
                    // opcode. so we must forget the rex prefix!
                    // this is to handle sequences like 41660f21cf
                    // where if 660f21 were a valid opcode, 41 would apply a rex.b
                    // prefix, but since 660f21 is not valid, the opcode is interpreted
                    // as 0f21, where 66 is a prefix, which makes 41 not the last
                    // prefix before the opcode, and it's discarded.

                    // 2.3.2
                    // Any VEX-encoded instruction with a LOCK prefix preceding VEX will #UD.
                    // 2.3.3
                    // Any VEX-encoded instruction with a 66H, F2H, or F3H prefix preceding VEX
                    // will #UD.
                    // 2.3.4
                    // Any VEX-encoded instruction with a REX prefix proceeding VEX will #UD. 
                    if b == 0xc5 {
                        if prefixes.rex().present() || prefixes.lock() || prefixes.operand_size() || prefixes.rep() || prefixes.repnz() {
                            // rex and then vex is invalid! reject it.
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidPrefixes);
                        } else {
                            instruction.prefixes = prefixes;
                            vex::two_byte_vex(&mut bytes_iter, instruction, length)?;

                            if decoder != &InstDecoder::default() {
                                decoder.revise_instruction(instruction)?;
                            }
                            return Ok(());
                        }
                    } else if b == 0xc4 {
                        if prefixes.rex().present() || prefixes.lock() || prefixes.operand_size() || prefixes.rep() || prefixes.repnz() {
                            // rex and then vex is invalid! reject it.
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidPrefixes);
                        } else {
                            instruction.prefixes = prefixes;
                            vex::three_byte_vex(&mut bytes_iter, instruction, length)?;
                            if decoder != &InstDecoder::default() {
                                decoder.revise_instruction(instruction)?;
                            }
                            return Ok(());
                        }
                    }

                    prefixes.rex_from(0);
                    escapes_are_prefixes_actually(&mut prefixes, &mut alternate_opcode_map);
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
                            alternate_opcode_map = Some(OpcodeMap::Map66);
                        },
                        0x67 => {
                            prefixes.set_address_size();
                        },
                        0xf0 => {
                            prefixes.set_lock();
                        },
                        0xf2 => {
                            alternate_opcode_map = Some(OpcodeMap::MapF2);
                        },
                        0xf3 => {
                            alternate_opcode_map = Some(OpcodeMap::MapF3);
                        },
                        _ => { unsafe { unreachable_unchecked(); } }
                    }
                }
            },
            None => {
                return Err(DecodeError::ExhaustedInput);
            }
        }
    };
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

    if decoder != &InstDecoder::default() {
        // we might have to fix up or reject this instruction under whatever cpu features we need to
        // pretend to have.
        decoder.revise_instruction(instruction)?;
    }
    Ok(())
}
fn read_operands<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction, operand_code: OperandCode, length: &mut u8) -> Result<(), DecodeError> {
    instruction.operand_count = 2;
    instruction.operands[0] = OperandSpec::RegRRR;
    let operand_code = OperandCodeBuilder::from_bits(operand_code as u16);
    if operand_code.has_embedded_instructions() {
        match operand_code.get_embedded_instructions() {
            Ok(z_operand_code) => {
                let reg = z_operand_code.reg();
                match z_operand_code.category() {
                    0 => {
                        // these are Zv_R
                        let opwidth = imm_width_from_prefixes_64(SizeCode::vq, instruction.prefixes);
                        let bank = if opwidth == 4 {
                            RegisterBank::D
                        } else if opwidth == 2 {
                            RegisterBank::W
                        } else {
                            RegisterBank::Q
                        };
                        instruction.modrm_rrr =
                            RegSpec::from_parts(reg, instruction.prefixes.rex().b(), bank);
                        instruction.operand_count = 1;
                    }
                    1 => {
                        // Zv_AX
                    }
                    2 => {
                        // these are Zb_Ib_R
                        instruction.modrm_rrr =
                            RegSpec::gp_from_parts(reg, instruction.prefixes.rex().b(), 1, instruction.prefixes.rex().present());
                        instruction.imm =
                            read_imm_unsigned(&mut bytes_iter, 1, length)?;
                        instruction.operands[1] = OperandSpec::ImmU8;
                        instruction.operand_count = 2;
                    }
                    3 => {
                        // category == 3, Zv_Ivq_R
                        let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
                        let bank = if opwidth == 4 {
                            RegisterBank::D
                        } else if opwidth == 2 {
                            RegisterBank::W
                        } else {
                            RegisterBank::Q
                        };
                        instruction.modrm_rrr =
                            RegSpec::from_parts(reg, instruction.prefixes.rex().b(), bank);
                        instruction.imm =
                            read_imm_ivq(&mut bytes_iter, opwidth, length)?;
                        instruction.operands[1] = match opwidth {
                            1 => OperandSpec::ImmI8,
                            2 => OperandSpec::ImmI16,
                            4 => OperandSpec::ImmI32,
                            8 => OperandSpec::ImmI64,
                            _ => unsafe { unreachable_unchecked() }
                        };
                        instruction.operand_count = 2;
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
    let mut opwidth = 0;
    let mut mem_oper = OperandSpec::Nothing;
    let mut bank = RegisterBank::Q;
    if operand_code.has_read_E() {
        // cool! we can precompute opwidth and know we need to read_E.
        if !operand_code.has_byte_operands() {
            // further, this is an vdq E
            opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            if opwidth == 4 {
                bank = RegisterBank::D;
            } else if opwidth == 2 {
                bank = RegisterBank::W;
            }
        } else {
            opwidth = 1;
            if instruction.prefixes.rex().present() {
                bank = RegisterBank::rB;
            } else {
                bank = RegisterBank::B;
            }
        };
        modrm = read_modrm(&mut bytes_iter, length)?;
        instruction.modrm_rrr.bank = bank;
        instruction.modrm_rrr.num = ((modrm >> 3) & 7) + if instruction.prefixes.rex().r() { 0b1000 } else { 0 };

        mem_oper = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
        if operand_code.bits() == (OperandCode::Gv_M as u16) {
            if mem_oper == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
        }
    }

    if operand_code.is_only_modrm_operands() {
        let (mmm, rrr) = if operand_code.has_reg_mem() {
            (1, 0)
        } else {
            (0, 1)
        };
        instruction.operands[mmm] = mem_oper;
        instruction.operands[rrr] = OperandSpec::RegRRR;
    } else if operand_code.bits() == OperandCode::Ibs as u16 {
        instruction.imm =
            read_imm_signed(&mut bytes_iter, 1, length)? as u64;
        instruction.operands[0] = OperandSpec::ImmI8;
        instruction.operand_count = 1;
    } else {
        let operand_code: OperandCode = unsafe { core::mem::transmute(operand_code.bits()) };
    match operand_code {
        OperandCode::Eb_R0 => {
            // turns out xed cand capstone both permit nonzero rrr bits here.
            // if (modrm & 0b00111000) != 0 {
            //    instruction.opcode = Opcode::Invalid;
            //    return Err(DecodeError::InvalidOperand);
            //}

            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
        },
        op @ OperandCode::AL_Ob |
        op @ OperandCode::AX_Ov => {
            let opwidth = match op {
                OperandCode::AL_Ob => 1,
                OperandCode::AX_Ov => {
                    imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            let imm = read_num(&mut bytes_iter, addr_width)?;
            *length += addr_width;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present());
            instruction.disp = imm;
            if instruction.prefixes.address_size() {
                instruction.operands[1] = OperandSpec::DispU32;
            } else {
                instruction.operands[1] = OperandSpec::DispU64;
            };
            instruction.operand_count = 2;
        }
        op @ OperandCode::Ob_AL |
        op @ OperandCode::Ov_AX => {
            let opwidth = match op {
                OperandCode::Ob_AL => 1,
                OperandCode::Ov_AX => {
                    imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            let imm = read_num(&mut bytes_iter, addr_width)?;
            *length += addr_width;
            instruction.disp = imm;
            instruction.operands[0] = if instruction.prefixes.address_size() {
                OperandSpec::DispU32
            } else {
                OperandSpec::DispU64
            };
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present());
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        _op @ OperandCode::ModRM_0x80_Eb_Ib |
        _op @ OperandCode::ModRM_0x81_Ev_Ivs => {
            instruction.operands[0] = mem_oper;
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[1] = match opwidth {
                1 => OperandSpec::ImmI8,
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                8 => OperandSpec::ImmI64,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 2;
        },
        op @ OperandCode::ModRM_0xc6_Eb_Ib |
        op @ OperandCode::ModRM_0xc7_Ev_Iv => {
            if modrm == 0xf8 {
                if op == OperandCode::ModRM_0xc6_Eb_Ib {
                    instruction.opcode = Opcode::XABORT;
                    instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
                    instruction.operands[0] = OperandSpec::ImmI8;
                    instruction.operand_count = 1;
                    return Ok(());
                } else {
                    instruction.opcode = Opcode::XBEGIN;
                    instruction.imm = if opwidth == 2 {
                        read_imm_signed(&mut bytes_iter, 2, length)? as i16 as i64 as u64
                    } else {
                        read_imm_signed(&mut bytes_iter, 4, length)? as i32 as i64 as u64
                    };
                    instruction.operands[0] = OperandSpec::ImmI32;
                    instruction.operand_count = 1;
                    return Ok(());
                }
            }
            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOperand); // Err("Invalid modr/m for opcode 0xc7".to_string());
            }

            instruction.operands[0] = mem_oper;
            instruction.opcode = Opcode::MOV;
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
            instruction.operands[1] = match opwidth {
                1 => OperandSpec::ImmI8,
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                8 => OperandSpec::ImmI64,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 2;
        },
        op @ OperandCode::ModRM_0xc0_Eb_Ib |
        op @ OperandCode::ModRM_0xc1_Ev_Ib |
        op @ OperandCode::ModRM_0xd0_Eb_1 |
        op @ OperandCode::ModRM_0xd1_Ev_1 |
        op @ OperandCode::ModRM_0xd2_Eb_CL |
        op @ OperandCode::ModRM_0xd3_Ev_CL => {
            instruction.operands[0] = mem_oper;
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            if let OperandCode::ModRM_0xd3_Ev_CL = op {
                instruction.modrm_rrr = RegSpec::cl();
                instruction.operands[1] = OperandSpec::RegRRR;
            } else if let OperandCode::ModRM_0xd2_Eb_CL = op {
                instruction.modrm_rrr = RegSpec::cl();
                instruction.operands[1] = OperandSpec::RegRRR;
            } else {
                let num = match op {
                    OperandCode::ModRM_0xc0_Eb_Ib |
                    OperandCode::ModRM_0xc1_Ev_Ib => {
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
        _op @ OperandCode::ModRM_0xf6 |
        _op @ OperandCode::ModRM_0xf7 => {
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
            match (modrm >> 3) & 7 {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    let numwidth = if opwidth == 8 { 4 } else { opwidth };
                    instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
                    instruction.operands[1] = match opwidth {
                        1 => OperandSpec::ImmI8,
                        2 => OperandSpec::ImmI16,
                        4 => OperandSpec::ImmI32,
                        8 => OperandSpec::ImmI64,
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
        OperandCode::ModRM_0xfe_Eb => {
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
        OperandCode::ModRM_0x8f_Ev => {
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
        OperandCode::ModRM_0xff_Ev => {
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
                    instruction.modrm_mmm.bank = RegisterBank::Q;
                } else if opcode == Opcode::CALLF || opcode == Opcode::JMPF {
                    return Err(DecodeError::InvalidOperand);
                }
            }
            instruction.opcode = opcode;
            instruction.operand_count = 1;
        }
        OperandCode::Gv_Eb => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 1, length)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Gv_Ew => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Gv_Ew_LSL => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            // lsl is weird. the full register width is written, but only the low 16 bits are used.
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Gdq_Ed => {
            let opwidth = 8;
            let modrm = read_modrm(&mut bytes_iter, length)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4 /* opwidth */, length)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Gdq_Ev => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, length)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
            // `opwidth` can be 2, 4, or 8 here. if opwidth is 2, the first operand is a dword.
            // if opwidth is 4, both registers are dwords. and if opwidth is 8, both registers are
            // qword.
            let regwidth = if opwidth == 2 {
                4
            } else {
                opwidth
            };
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), regwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Ev => {
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
        },
        OperandCode::E_G_xmm => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
            if instruction.operands[0] == OperandSpec::RegMMM {
                // fix the register to XMM
                instruction.modrm_mmm.bank = RegisterBank::X;
            }
        },
        OperandCode::G_E_mm => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            }
            instruction.operand_count = 2;
        },
        OperandCode::G_U_mm => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::D;
            if mem_oper != OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_mmm.bank = RegisterBank::MM;
            instruction.modrm_mmm.num &= 0b111;
            instruction.operand_count = 2;
        },
        OperandCode::G_U_xmm => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper != OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_mmm.bank = RegisterBank::X;
            instruction.operand_count = 2;
        },
        op @ OperandCode::G_M_xmm |
        op @ OperandCode::G_E_xmm => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
            if instruction.operands[1] == OperandSpec::RegMMM {
                if op == OperandCode::G_M_xmm {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOperand);
                } else {
                    // fix the register to XMM
                    instruction.modrm_mmm.bank = RegisterBank::X;
                }
            }
        },
        OperandCode::G_E_xmm_Ib => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u64;
            *length += 1;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        },
        OperandCode::G_E_mm_Ib => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr = RegSpec { bank: RegisterBank::MM, num: (modrm >> 3) & 7 };
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u64;
            *length += 1;
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
            }
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        },
        OperandCode::G_mm_Ew_Ib => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, false, RegisterBank::MM);
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u64;
            *length += 1;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::AL_Ibs => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        }
        OperandCode::AX_Ivd => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(0, false, opwidth, false);
            instruction.imm =
                read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
            instruction.operands[1] = match opwidth {
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                8 => OperandSpec::ImmI64,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 2;
        }
        OperandCode::Ivs => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, instruction.prefixes);
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, opwidth, length)?;
            instruction.operands[0] = match opwidth {
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                8 => OperandSpec::ImmI64,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 1;
        },
        OperandCode::ModRM_0x83_Ev_Ibs => {
            instruction.operands[0] = mem_oper;
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        },
        OperandCode::Jvds => {
            let offset = read_num(&mut bytes_iter, 4)?;
            *length += 4;
            instruction.imm = offset;
            instruction.operand_count = 1;
            instruction.operands[0] = OperandSpec::ImmI32;
        }
        OperandCode::Gb_Eb_Ib => {
            instruction.operands[1] = mem_oper;
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::Gv_Ev_Iv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.operands[1] = mem_oper;
            instruction.imm =
                read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
            instruction.operands[2] = match opwidth {
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                8 => OperandSpec::ImmI64,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 3;
        }
        OperandCode::Ev_Gv_Ib => {
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, length)? as u64;
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
        OperandCode::I_3 => {
            instruction.imm = 3;
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operand_count = 1;
        }
        _ => {
            unlikely_operands(decoder, bytes_iter, instruction, operand_code, mem_oper, length)?;
        }
    };
    }

    Ok(())
}
fn unlikely_operands<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction, operand_code: OperandCode, mem_oper: OperandSpec, length: &mut u8) -> Result<(), DecodeError> {
    match operand_code {
        OperandCode::Nothing => {
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
            return Ok(());
        },
        OperandCode::Unsupported => {
            return Err(DecodeError::IncompleteDecoder);
        }
        OperandCode::Iw_Ib => {
            instruction.disp = read_num(&mut bytes_iter, 2)? as u64;
            instruction.imm = read_num(&mut bytes_iter, 1)? as u64;
            instruction.operands[0] = OperandSpec::EnterFrameSize;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operand_count = 2;
            *length += 3;
        }
        OperandCode::Fw => {
            if instruction.prefixes.rex().w() {
                instruction.opcode = Opcode::IRETQ;
            } else if instruction.prefixes.operand_size() {
                instruction.opcode = Opcode::IRET;
            } else {
                instruction.opcode = Opcode::IRETD;
            }
            instruction.operand_count = 0;
        }
        OperandCode::Mdq_Gdq => {
            let opwidth = if instruction.prefixes.rex().w() { 8 } else { 4 };
            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;

        }
        OperandCode::G_mm_U_mm => {
            instruction.operands[1] = mem_oper;
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

            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 8, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::Q);
            instruction.operand_count = 2;
        }
        OperandCode::G_E_q => {
            if instruction.prefixes.operand_size() {
                return Err(DecodeError::InvalidOpcode);
            }

            let modrm = read_modrm(&mut bytes_iter, length)?;

            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::Q);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 8, length)?;
            instruction.operand_count = 2;
        }
        OperandCode::G_Md_mm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            if mem_oper == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_rrr.num &= 0b111;
            instruction.operand_count = 2;
        },
        OperandCode::MOVQ_f30f => {
            // if rex.w is set, the f3 prefix no longer applies and this becomes an 0f7e movq,
            // rather than f30f7e movq.
            //
            // the difference here is that 0f7e movq has reversed operand order from f30f7e movq,
            // in addition to the selected register banks being different.
            //
            // anyway, there are two operands, and the primary concern here is "what are they?".
            instruction.operand_count = 2;
            let modrm = read_modrm(&mut bytes_iter, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);
            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            if instruction.prefixes.rex().w() {
                let op = instruction.operands[0];
                instruction.operands[0] = instruction.operands[1];
                instruction.operands[1] = op;
                instruction.modrm_mmm.bank = RegisterBank::Q;
                instruction.modrm_rrr.bank = RegisterBank::MM;
                instruction.modrm_rrr.num &= 0b111;
                instruction.opcode = Opcode::MOVD;
            }
        }
        OperandCode::ModRM_0x0f0d => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 0b111;

            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, instruction.prefixes);

            match r {
                1 => {
                    instruction.opcode = Opcode::PREFETCHW;
                }
                _ => {
                    instruction.opcode = Opcode::NOP;
                }
            }
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
            instruction.operand_count = 1;
        }
        OperandCode::ModRM_0x0f38 => {
            let opcode = read_modrm(&mut bytes_iter, length)?;

            let high = opcode >> 4;
            let low = opcode & 0xf;

            let operands = match high {
                0 => {
                    // PqQq
                    if low != 0x0f {
                        OperandCode::G_E_mm
                    } else {
                        // PALIGNR
                        OperandCode::G_E_mm_Ib
                    }
                },
                1 => {
                    // PqQq
                    OperandCode::G_E_mm
                },
                0xc => {
                    // Vdq,Wdq
                    OperandCode::G_E_xmm
                }
                0xf => {
                    match low {
                        0 => OperandCode::Gv_Ev,
                        1 => OperandCode::Ev_Gv,
                        _ => {
                            instruction.opcode = Opcode::Invalid;
                            return Err(DecodeError::InvalidOpcode);
                        }
                    }
                }
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            };
            instruction.opcode = match opcode {
                0x00 => Opcode::PSHUFB,
                0x01 => Opcode::PHADDW,
                0x02 => Opcode::PHADDD,
                0x03 => Opcode::PHADDSW,
                0x04 => Opcode::PMADDUBSW,
                0x05 => Opcode::PHSUBW,
                0x06 => Opcode::PHSUBD,
                0x07 => Opcode::PHSUBSW,
                0x08 => Opcode::PSIGNB,
                0x09 => Opcode::PSIGNW,
                0x0a => Opcode::PSIGND,
                0x0b => Opcode::PMULHRSW,

                0x0f => Opcode::PALIGNR,

                0x1c => Opcode::PABSB,
                0x1d => Opcode::PABSW,
                0x1e => Opcode::PABSD,

                0xc8 => Opcode::SHA1NEXTE,
                0xc9 => Opcode::SHA1MSG1,
                0xca => Opcode::SHA1MSG2,
                0xcb => Opcode::SHA256RNDS2,
                0xcc => Opcode::SHA256MSG1,
                0xcd => Opcode::SHA256MSG2,
                0xf0 | 0xf1 => Opcode::MOVBE,
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            };

            return read_operands(decoder, bytes_iter, instruction, operands, length);
        },
        OperandCode::ModRM_0x0f3a => {
            let opcode = read_modrm(&mut bytes_iter, length)?;
            if opcode == 0xcc {
                instruction.opcode = Opcode::SHA1RNDS4;
                return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
            } else if opcode == 0x0f {
                instruction.opcode = Opcode::PALIGNR;
                return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_mm_Ib, length);
            }
        },
        OperandCode::ModRM_0x0fc7 => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let is_reg = (modrm & 0xc0) == 0xc0;

            let r = (modrm >> 3) & 0b111;

            let opcode = match r {
                0b001 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        if instruction.prefixes.rex().w() {
                            Opcode::CMPXCHG16B
                        } else {
                            Opcode::CMPXCHG8B
                        }
                    }
                }
                0b011 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        if instruction.prefixes.rex().w() {
                            Opcode::XRSTORS64
                        } else {
                            Opcode::XRSTORS
                        }
                    }
                }
                0b100 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        if instruction.prefixes.rex().w() {
                            Opcode::XSAVEC64
                        } else {
                            Opcode::XSAVEC
                        }
                    }
                }
                0b101 => {
                    if is_reg {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOperand);
                    } else {
                        if instruction.prefixes.rex().w() {
                            Opcode::XSAVES64
                        } else {
                            Opcode::XSAVES
                        }
                    }
                }
                0b110 => {
                    if is_reg {
                        Opcode::RDRAND
                    } else {
                        Opcode::VMPTRLD
                    }
                }
                0b111 => {
                    if is_reg {
                        Opcode::RDSEED
                    } else {
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
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
        },
        OperandCode::ModRM_0x0f71 => {
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

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
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

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0x0f73 => {
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
                6 => {
                    instruction.opcode = Opcode::PSLLQ;
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0x660f12 => {
            // If this is reg-reg, interpret the instruction as 66-prefixed (no-op here)
            // `movhlps`. If this is reg-mem, it's a `movlpd`.
            let modrm = read_modrm(&mut bytes_iter, length)?;
            if modrm & 0xc0 == 0xc0 {
                instruction.opcode = Opcode::MOVHLPS;
            } else {
                instruction.opcode = Opcode::MOVLPD;
            }
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);
            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operand_count = 2;
        }
        OperandCode::ModRM_0x660f16 => {
            // If this is reg-reg, interpret the instruction as 66-prefixed (no-op here)
            // `movlhps`. If this is reg-mem, it's a `movhpd`.
            let modrm = read_modrm(&mut bytes_iter, length)?;
            if modrm & 0xc0 == 0xc0 {
                instruction.opcode = Opcode::MOVLHPS;
            } else {
                instruction.opcode = Opcode::MOVHPD;
            }
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);
            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operand_count = 2;
        }
        OperandCode::ModRM_0xf20f38 => {
            let op = bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })?;
            match op {
                0xf0 => {
                    instruction.opcode = Opcode::CRC32;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::Gv_Eb, length);
                }
                0xf1 => {
                    instruction.opcode = Opcode::CRC32;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::Gdq_Ev, length);
                }
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            };
        }
        OperandCode::ModRM_0xf30f38 => {
            let op = bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })?;
            match op {
                0xf6 => {
                    instruction.opcode = Opcode::ADOX;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::Gv_Ev, length);
                }
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            };
        }
        OperandCode::ModRM_0x660f38 => {
            let op = bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })?;
            match op {
                0x00 => { instruction.opcode = Opcode::PSHUFB; }
                0x01 => { instruction.opcode = Opcode::PHADDW; }
                0x02 => { instruction.opcode = Opcode::PHADDD; }
                0x03 => { instruction.opcode = Opcode::PHADDSW; }
                0x04 => { instruction.opcode = Opcode::PMADDUBSW; }
                0x05 => { instruction.opcode = Opcode::PHSUBW; }
                0x06 => { instruction.opcode = Opcode::PHSUBD; }
                0x07 => { instruction.opcode = Opcode::PHSUBSW; }
                0x08 => { instruction.opcode = Opcode::PSIGNB; }
                0x09 => { instruction.opcode = Opcode::PSIGNW; }
                0x0a => { instruction.opcode = Opcode::PSIGND; }
                0x0b => { instruction.opcode = Opcode::PMULHRSW; }
                0x0c => { instruction.opcode = Opcode::BLENDPS; }
                0x0d => { instruction.opcode = Opcode::BLENDPD; }

                0x10 => { instruction.opcode = Opcode::PBLENDVB; }

                0x14 => { instruction.opcode = Opcode::BLENDVPS; }
                0x15 => { instruction.opcode = Opcode::BLENDVPD; }

                0x17 => { instruction.opcode = Opcode::PTEST; }

                0x1c => { instruction.opcode = Opcode::PABSB; }
                0x1d => { instruction.opcode = Opcode::PABSW; }
                0x1e => { instruction.opcode = Opcode::PABSD; }

                0x20 => { instruction.opcode = Opcode::PMOVSXBW; }
                0x21 => { instruction.opcode = Opcode::PMOVSXBD; }
                0x22 => { instruction.opcode = Opcode::PMOVSXBQ; }
                0x23 => { instruction.opcode = Opcode::PMOVSXWD; }
                0x24 => { instruction.opcode = Opcode::PMOVSXWQ; }
                0x25 => { instruction.opcode = Opcode::PMOVSXDQ; }

                0x28 => { instruction.opcode = Opcode::PMULDQ; }
                0x29 => { instruction.opcode = Opcode::PCMPEQQ; }
                0x2a => { instruction.opcode = Opcode::MOVNTDQA; }
                0x2b => { instruction.opcode = Opcode::PACKUSDW; }

                0x30 => { instruction.opcode = Opcode::PMOVZXBW; }
                0x31 => { instruction.opcode = Opcode::PMOVZXBD; }
                0x32 => { instruction.opcode = Opcode::PMOVZXBQ; }
                0x33 => { instruction.opcode = Opcode::PMOVZXWD; }
                0x34 => { instruction.opcode = Opcode::PMOVZXWQ; }
                0x35 => { instruction.opcode = Opcode::PMOVZXDQ; }

                0x37 => { instruction.opcode = Opcode::PCMPGTQ; }
                0x38 => { instruction.opcode = Opcode::PMINSB; }
                0x39 => { instruction.opcode = Opcode::PMINSD; }
                0x3a => { instruction.opcode = Opcode::PMINUW; }
                0x3b => { instruction.opcode = Opcode::PMINUD; }
                0x3c => { instruction.opcode = Opcode::PMAXSB; }
                0x3d => { instruction.opcode = Opcode::PMAXSD; }
                0x3e => { instruction.opcode = Opcode::PMAXUW; }
                0x3f => { instruction.opcode = Opcode::PMAXUD; }

                0x40 => { instruction.opcode = Opcode::PMULLD; }
                0x41 => { instruction.opcode = Opcode::PHMINPOSUW; }

                0xdb => { instruction.opcode = Opcode::AESIMC; }
                0xdc => { instruction.opcode = Opcode::AESENC; }
                0xdd => { instruction.opcode = Opcode::AESENCLAST; }
                0xde => { instruction.opcode = Opcode::AESDEC; }
                0xdf => { instruction.opcode = Opcode::AESDECLAST; }
                0xf6 => {
                    instruction.opcode = Opcode::ADCX;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::Gv_Ev, length);
                }
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            };
            // all these SO FAR are G_E_xmm
            let modrm = read_modrm(&mut bytes_iter, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);


            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operand_count = 2;
        }
        OperandCode::ModRM_0x660f3a => {
            let op = bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })?;
            match op {
                0x08 => {
                    instruction.opcode = Opcode::ROUNDPS;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x09 => {
                    instruction.opcode = Opcode::ROUNDPD;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x0a => {
                    instruction.opcode = Opcode::ROUNDSS;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x0b => {
                    instruction.opcode = Opcode::ROUNDSD;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x0e => {
                    instruction.opcode = Opcode::PBLENDW;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x0f => {
                    instruction.opcode = Opcode::PALIGNR;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x14 => {
                    instruction.opcode = Opcode::PEXTRB;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x15 => {
                    instruction.opcode = Opcode::PEXTRW;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x16 => {
                    instruction.opcode = Opcode::PEXTRD;
                    if instruction.prefixes.rex().w() {
                        instruction.opcode = Opcode::PEXTRQ;
                    }
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x17 => {
                    instruction.opcode = Opcode::EXTRACTPS;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x20 => {
                    instruction.opcode = Opcode::PINSRB;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x21 => {
                    instruction.opcode = Opcode::INSERTPS;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x22 => {
                    instruction.opcode = Opcode::PINSRD;
                    if instruction.prefixes.rex().w() {
                        instruction.opcode = Opcode::PINSRQ;
                    }
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x40 => {
                    instruction.opcode = Opcode::DPPS;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x41 => {
                    instruction.opcode = Opcode::DPPD;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x42 => {
                    instruction.opcode = Opcode::MPSADBW;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }

                0x60 => {
                    instruction.opcode = Opcode::PCMPESTRM;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x61 => {
                    instruction.opcode = Opcode::PCMPESTRI;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x62 => {
                    instruction.opcode = Opcode::PCMPISTRM;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0x63 => {
                    instruction.opcode = Opcode::PCMPISTRI;
                    return read_operands(decoder, bytes_iter, instruction, OperandCode::G_E_xmm_Ib, length);
                }
                0xcc => {
                    instruction.opcode = Opcode::SHA1RNDS4;

                    let modrm = read_modrm(&mut bytes_iter, length)?;
                    instruction.modrm_rrr =
                        RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);


                    instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
                    instruction.imm =
                        read_imm_unsigned(&mut bytes_iter, 1, length)?;
                    instruction.operands[2] = OperandSpec::ImmU8;
                    instruction.operand_count = 3;
                }
                0xdf => {
                    instruction.opcode = Opcode::AESKEYGENASSIST;
                    // read operands right here right now

                    let modrm = read_modrm(&mut bytes_iter, length)?;
                    instruction.modrm_rrr =
                        RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);


                    instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
                    instruction.imm =
                        read_imm_unsigned(&mut bytes_iter, 1, length)?;
                    instruction.operands[2] = OperandSpec::ImmU8;
                    instruction.operand_count = 3;
                }
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(DecodeError::InvalidOpcode);
                }
            };
        }
        OperandCode::ModRM_0x660f71 => {
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

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::X, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0x660f72 => {
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

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::X, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0x660f73 => {
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
                    instruction.opcode = Opcode::PSRLDQ;
                }
                6 => {
                    instruction.opcode = Opcode::PSLLQ;
                }
                7 => {
                    instruction.opcode = Opcode::PSLLDQ;
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::X, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmU8;
        },
        OperandCode::ModRM_0x660fc7 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, length)?;

            let r = (modrm >> 3) & 7;
            match r {
                6 => {
                    instruction.opcode = Opcode::VMCLEAR;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                    if instruction.operands[0] == OperandSpec::RegMMM {
                        // this would be invalid as `vmclear`, so fall back to the parse as
                        // 66-prefixed rdrand. this is a register operand, so just demote it to the
                        // word-form operand:
                        instruction.modrm_mmm = RegSpec { bank: RegisterBank::W, num: instruction.modrm_mmm.num };
                        instruction.opcode = Opcode::RDRAND;
                    }
                    instruction.operand_count = 1;
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
                }
                _ => {
                    return Err(DecodeError::InvalidOpcode);
                }
            }
        },
        OperandCode::ModRM_0x660fae => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
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
                instruction.operand_count = 1;
            } else {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOpcode);
            }
        },
        OperandCode::ModRM_0xf30fae => {
            let modrm = read_modrm(&mut bytes_iter, length)?;

            if (modrm & 0xc0) == 0xc0 {
                let r = (modrm >> 3) & 7;
                let m = modrm & 7;
                match r {
                    0 => {
                        instruction.opcode = Opcode::RDFSBASE;
                        let opwidth = if instruction.prefixes.rex().w() {
                            RegisterBank::Q
                        } else {
                            RegisterBank::D
                        };
                        instruction.modrm_mmm = RegSpec::from_parts(m, instruction.prefixes.rex().x(), opwidth);
                        instruction.operands[0] = OperandSpec::RegMMM;
                        instruction.operand_count = 1;
                    }
                    1 => {
                        instruction.opcode = Opcode::RDGSBASE;
                        let opwidth = if instruction.prefixes.rex().w() {
                            RegisterBank::Q
                        } else {
                            RegisterBank::D
                        };
                        instruction.modrm_mmm = RegSpec::from_parts(m, instruction.prefixes.rex().x(), opwidth);
                        instruction.operands[0] = OperandSpec::RegMMM;
                        instruction.operand_count = 1;

                    }
                    2 => {
                        instruction.opcode = Opcode::WRFSBASE;
                        let opwidth = if instruction.prefixes.rex().w() {
                            RegisterBank::Q
                        } else {
                            RegisterBank::D
                        };
                        instruction.modrm_mmm = RegSpec::from_parts(m, instruction.prefixes.rex().x(), opwidth);
                        instruction.operands[0] = OperandSpec::RegMMM;
                        instruction.operand_count = 1;
                    }
                    3 => {
                        instruction.opcode = Opcode::WRGSBASE;
                        let opwidth = if instruction.prefixes.rex().w() {
                            RegisterBank::Q
                        } else {
                            RegisterBank::D
                        };
                        instruction.modrm_mmm = RegSpec::from_parts(m, instruction.prefixes.rex().x(), opwidth);
                        instruction.operands[0] = OperandSpec::RegMMM;
                        instruction.operand_count = 1;

                    }
                    _ => {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOpcode);
                    }
                }
            }
        }
        OperandCode::ModRM_0xf30fc7 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, length)?;

            let r = (modrm >> 3) & 7;
            match r {
                6 => {
                    instruction.opcode = Opcode::VMXON;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                    if instruction.operands[0] == OperandSpec::RegMMM {
                        // this would be invalid as `vmxon`, so fall back to the parse as
                        // f3-prefixed rdrand
                        instruction.opcode = Opcode::RDRAND;
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
        },
        OperandCode::G_mm_Edq => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.rex().w() {
                    instruction.modrm_mmm.bank = RegisterBank::Q;
                } else {
                    instruction.modrm_mmm.bank = RegisterBank::D;
                }
            }
        }
        OperandCode::G_mm_E => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            }
        }
        OperandCode::Edq_G_mm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.rex().w() {
                    instruction.modrm_mmm.bank = RegisterBank::Q;
                } else {
                    instruction.modrm_mmm.bank = RegisterBank::D;
                }
            }
        }
        OperandCode::Edq_G_xmm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.rex().w() {
                    instruction.modrm_mmm.bank = RegisterBank::Q;
                    // movd/q is so weird
                    instruction.opcode = Opcode::MOVQ;
                } else {
                    instruction.modrm_mmm.bank = RegisterBank::D;
                }
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
            }
        }
        /*
        OperandCode::G_xmm_Ed => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
        },
        */
        OperandCode::G_xmm_Edq => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.rex().w() {
                    instruction.modrm_mmm.bank = RegisterBank::Q;
                } else {
                    instruction.modrm_mmm.bank = RegisterBank::D;
                }
            }
        },
        OperandCode::G_xmm_Ed_Ib => {
            instruction.operands[1] = mem_oper;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u64;
            *length += 1;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
        },
        OperandCode::G_xmm_Eq => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::Q;
            }
        },
        OperandCode::G_mm_E_xmm => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::MM;
            instruction.modrm_rrr.num &= 0b111;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::X;
            }
        },
        op @ OperandCode::G_xmm_U_mm |
        op @ OperandCode::G_xmm_E_mm => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            } else {
                if op == OperandCode::G_xmm_U_mm {
                    return Err(DecodeError::InvalidOperand);
                }
            }
        },
        OperandCode::Rv_Gmm_Ib => {
            instruction.operands[1] = mem_oper;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u64;
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
            instruction.operands[1] = mem_oper;
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
            instruction.modrm_mmm = RegSpec::rsi();
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::Deref;
            instruction.operand_count = 2;
        }
        // TODO: two memory operands! this is wrong!!!
        OperandCode::Yb_Xb => {
            instruction.operands[0] = OperandSpec::Deref_rdi;
            instruction.operands[1] = OperandSpec::Deref_rsi;
            instruction.operand_count = 2;
        }
        OperandCode::Yb_AL => {
            instruction.modrm_rrr = RegSpec::al();
            instruction.modrm_mmm = RegSpec::rsi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::AX_Xv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.modrm_rrr = match opwidth {
                2 => RegSpec::ax(),
                4 => RegSpec::eax(),
                8 => RegSpec::rax(),
                _ => { unreachable!(); }
            };
            instruction.modrm_mmm = RegSpec::rsi();
            instruction.operands[1] = OperandSpec::Deref;
        }
        OperandCode::Yv_AX => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.modrm_rrr = match opwidth {
                2 => RegSpec::ax(),
                4 => RegSpec::eax(),
                8 => RegSpec::rax(),
                _ => { unreachable!(); }
            };
            instruction.modrm_mmm = RegSpec::rdi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
        }
        OperandCode::Yv_Xv => {
            // TODO: repsect prefixes
            instruction.operands[0] = OperandSpec::Deref_rdi;
            instruction.operands[1] = OperandSpec::Deref_rsi;
        }
        OperandCode::ModRM_0x0f12 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[1] = mem_oper;
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::X;
                instruction.opcode = Opcode::MOVHLPS;
            } else {
                instruction.opcode = Opcode::MOVLPS;
            }
        }
        OperandCode::ModRM_0x0f16 => {
            instruction.modrm_rrr.bank = RegisterBank::X;
            instruction.operands[1] = mem_oper;
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::X;
                instruction.opcode = Opcode::MOVLHPS;
            } else {
                instruction.opcode = Opcode::MOVHPS;
            }
        }
        OperandCode::ModRM_0x0f18 => {
            if mem_oper == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            let rrr = instruction.modrm_rrr.num & 0b111;
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
            instruction.opcode = match rrr {
                0 => Opcode::PREFETCHNTA,
                1 => Opcode::PREFETCH0,
                2 => Opcode::PREFETCH1,
                3 => Opcode::PREFETCH2,
                _ => Opcode::NOP,
            };
        }
        OperandCode::Gd_U_xmm => {
            instruction.operands[1] = mem_oper;
            if instruction.operands[1] != OperandSpec::RegMMM {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_rrr.bank = RegisterBank::D;
            instruction.modrm_mmm.bank = RegisterBank::X;
        }
        OperandCode::Gv_E_xmm => {
            instruction.operands[1] = mem_oper;
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::X;
            }
        }
        OperandCode::M_G_xmm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            if instruction.operands[0] == OperandSpec::RegMMM {
                instruction.opcode = Opcode::Invalid;
                return Err(DecodeError::InvalidOperand);
            }
            instruction.modrm_rrr.bank = RegisterBank::X;
        }
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
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            let mod_bits = modrm >> 6;
            if mod_bits == 0b11 {
                instruction.modrm_mmm =
                    RegSpec { bank: RegisterBank::W, num: modrm & 7};
                instruction.operands[1] = OperandSpec::RegMMM;
            } else {
                instruction.operands[1] = read_M(&mut bytes_iter, instruction, modrm, length)?;
            }
        },
        OperandCode::CVT_AA => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
            instruction.opcode = match opwidth {
                2 => { Opcode::CBW },
                4 => { Opcode::CWDE },
                8 => { Opcode::CDQE },
                _ => { unreachable!("invalid operation width"); },
            }
        }
        OperandCode::CVT_DA => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.operands[0] = OperandSpec::Nothing;
            instruction.operand_count = 0;
            instruction.opcode = match opwidth {
                2 => { Opcode::CWD },
                4 => { Opcode::CDQ },
                8 => { Opcode::CQO },
                _ => { unreachable!("invalid operation width"); },
            }
        }
        OperandCode::Ib => {
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, 1, length)?;
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
                instruction.opcode = Opcode::JMPE;
                instruction.operands[0] = OperandSpec::Nothing;
                instruction.operand_count = 0;
                return Ok(());
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
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, instruction.prefixes);
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
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else if r == 1 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    instruction.operands[0] = OperandSpec::Nothing;
                    instruction.operand_count = 0;
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
                } else {
                    instruction.opcode = Opcode::SIDT;
                    instruction.operand_count = 1;
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
                            instruction.modrm_rrr = RegSpec::rax();
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
                            instruction.modrm_rrr = RegSpec::rax();
                            instruction.operands[0] = OperandSpec::RegRRR;
                        },
                        0b011 => {
                            instruction.opcode = Opcode::VMSAVE;
                            instruction.operand_count = 1;
                            instruction.modrm_rrr = RegSpec::rax();
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
                            instruction.modrm_rrr = RegSpec::rax();
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
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else if r == 4 {
                // TODO: this permits storing only to word-size registers
                // spec suggets this might do something different for f.ex rdi?
                instruction.opcode = Opcode::SMSW;
                instruction.operand_count = 1;
                instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            } else if r == 5 {
                let m = modrm & 7;
                match m {
                    0b110 => {
                        instruction.opcode = Opcode::RDPKRU;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    }
                    0b111 => {
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
                instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            } else if r == 7 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    if m == 0 {
                        instruction.opcode = Opcode::SWAPGS;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    } else if m == 1 {
                        instruction.opcode = Opcode::RDTSCP;
                        instruction.operands[0] = OperandSpec::Nothing;
                        instruction.operand_count = 0;
                    } else {
                        instruction.opcode = Opcode::Invalid;
                        return Err(DecodeError::InvalidOpcode);
                    }
                } else {
                    instruction.opcode = Opcode::INVLPG;
                    instruction.operand_count = 1;
                    instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
                }
            } else {
                unreachable!("r <= 8");
            }
        }
        OperandCode::ModRM_0x0fae => {
            let modrm = read_modrm(&mut bytes_iter, length)?;
            let r = (modrm >> 3) & 7;
            let mod_bits = modrm >> 6;

            // all the 0b11 instructions are err or no-operands
            if mod_bits == 0b11 {
                instruction.operands[0] = OperandSpec::Nothing;
                instruction.operand_count = 0;
                let m = modrm & 7;
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
                instruction.operand_count = 1;
                instruction.opcode = [
                    Opcode::FXSAVE,
                    Opcode::FXRSTOR,
                    Opcode::LDMXCSR,
                    Opcode::STMXCSR,
                    Opcode::XSAVE,
                    Opcode::XRSTOR,
                    Opcode::XSAVEOPT,
                    Opcode::CLFLUSH,
                ][r as usize];
                instruction.operands[0] = read_M(&mut bytes_iter, instruction, modrm, length)?;
            }
        }
        OperandCode::ModRM_0x0fba => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, instruction.prefixes);
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

            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmI8;
            instruction.operand_count = 2;
        }
        op @ OperandCode::Rq_Cq_0 |
        op @ OperandCode::Rq_Dq_0 |
        op @ OperandCode::Cq_Rq_0 |
        op @ OperandCode::Dq_Rq_0 => {
            let bank = match op {
                OperandCode::Rq_Cq_0 |
                OperandCode::Cq_Rq_0 => RegisterBank::CR,
                OperandCode::Rq_Dq_0 |
                OperandCode::Dq_Rq_0 => RegisterBank::DR,
                _ => unsafe { unreachable_unchecked() }
            };
            let (rrr, mmm) = match op {
                OperandCode::Rq_Cq_0 |
                OperandCode::Rq_Dq_0 => (1, 0),
                OperandCode::Cq_Rq_0 |
                OperandCode::Dq_Rq_0 => (0, 1),
                _ => unsafe { unreachable_unchecked() }
            };

            let modrm = read_modrm(&mut bytes_iter, length)?;
            let mut m = modrm & 7;
            let mut r = (modrm >> 3) & 7;
            if instruction.prefixes.rex().r() {
                r += 0b1000;
            }
            if instruction.prefixes.rex().b() {
                m += 0b1000;
            }
            instruction.modrm_rrr =
                RegSpec { bank: bank, num: r };
            instruction.modrm_mmm =
                RegSpec { bank: RegisterBank::Q, num: m };
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
        OperandCode::AL_Ib => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u64;
            *length += 1;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operand_count = 2;
        }
        OperandCode::AX_Ib => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, instruction.prefixes);
            instruction.modrm_rrr = if opwidth == 4 {
               RegSpec::eax()
            } else {
               RegSpec::ax()
            };
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u64;
            *length += 1;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operand_count = 2;
        }
        OperandCode::Ib_AL => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u64;
            *length += 1;
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::Ib_AX => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, instruction.prefixes);
            instruction.modrm_rrr = if opwidth == 4 {
                RegSpec::eax()
            } else {
                RegSpec::ax()
            };
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u64;
            *length += 1;
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::AX_DX => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, instruction.prefixes);
            instruction.modrm_rrr = if opwidth == 4 {
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
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, instruction.prefixes);
            instruction.modrm_rrr = if opwidth == 4 {
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
            instruction.modrm_mmm = RegSpec::rdi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::Yv_DX => {
            instruction.modrm_rrr = RegSpec::dx();
            instruction.modrm_mmm = RegSpec::rdi();
            instruction.operands[0] = OperandSpec::Deref;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        OperandCode::DX_Xb => {
            instruction.modrm_rrr = RegSpec::dl();
            instruction.modrm_mmm = RegSpec::rsi();
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
            instruction.modrm_mmm = RegSpec::rsi();
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
        St_Ew,
        St_Md,
        Md,
        Ew,
        Est_St,
        Ed_St,
        Md_St,
        Ex87S,
        Nothing,
    }

    // every x87 instruction is conditional on rrr bits
    let modrm = read_modrm(&mut bytes_iter, length)?;
    let r = (modrm >> 3) & 0b111;

    let (opcode, x87_operands) = match operand_code {
        OperandCode::x87_d8 => {
            match r {
                0 => (Opcode::FADD, OperandCodeX87::St_Est),
                1 => (Opcode::FMUL, OperandCodeX87::St_Est),
                2 => (Opcode::FCOM, OperandCodeX87::St_Est),
                3 => (Opcode::FCOMP, OperandCodeX87::St_Est),
                4 => (Opcode::FSUB, OperandCodeX87::St_Est),
                5 => (Opcode::FSUBR, OperandCodeX87::St_Est),
                6 => (Opcode::FDIV, OperandCodeX87::St_Est),
                7 => (Opcode::FDIVR, OperandCodeX87::St_Est),
                _ => { unreachable!("impossible r"); }
            }
        }
        OperandCode::x87_d9 => {
            match r {
                0 => (Opcode::FLD, OperandCodeX87::St_Est),
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
                        (Opcode::FSTP, OperandCodeX87::Est_St)
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
                    5 => (Opcode::FLD, OperandCodeX87::St_Md), // 80bit
                    7 => (Opcode::FSTP, OperandCodeX87::Md_St), // 80bit
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
                    0 => (Opcode::FADD, OperandCodeX87::Est_St),
                    1 => (Opcode::FMUL, OperandCodeX87::Est_St),
                    2 => (Opcode::FCOM, OperandCodeX87::St_Est),
                    3 => (Opcode::FCOMP, OperandCodeX87::St_Est),
                    4 => (Opcode::FSUBR, OperandCodeX87::Est_St),
                    5 => (Opcode::FSUB, OperandCodeX87::Est_St),
                    6 => (Opcode::FDIVR, OperandCodeX87::Est_St),
                    7 => (Opcode::FDIV, OperandCodeX87::Est_St),
                    _ => { unreachable!("impossible r"); }
                }
            } else {
                match r {
                    0 => (Opcode::FADD, OperandCodeX87::St_Est),
                    1 => (Opcode::FMUL, OperandCodeX87::St_Est),
                    2 => (Opcode::FCOM, OperandCodeX87::St_Est),
                    3 => (Opcode::FCOMP, OperandCodeX87::St_Est),
                    4 => (Opcode::FSUB, OperandCodeX87::St_Est),
                    5 => (Opcode::FSUBR, OperandCodeX87::St_Est),
                    6 => (Opcode::FDIV, OperandCodeX87::St_Est),
                    7 => (Opcode::FDIVR, OperandCodeX87::St_Est),
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
                    0 => (Opcode::FLD, OperandCodeX87::St_Est),
                    1 => (Opcode::FISTTP, OperandCodeX87::Est_St),
                    2 => (Opcode::FST, OperandCodeX87::Est_St),
                    3 => (Opcode::FSTP, OperandCodeX87::Est_St),
                    4 => (Opcode::FRSTOR, OperandCodeX87::Md), // TODO: m94/108byte
                    5 => (Opcode::Invalid, OperandCodeX87::Nothing),
                    6 => (Opcode::FNSAVE, OperandCodeX87::Est),
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
                    0 => (Opcode::FILD, OperandCodeX87::St_Est),
                    1 => (Opcode::FISTTP, OperandCodeX87::Est_St),
                    2 => (Opcode::FIST, OperandCodeX87::Est_St),
                    3 => (Opcode::FISTP, OperandCodeX87::Est_St),
                    4 => (Opcode::FBLD, OperandCodeX87::St_Est),
                    5 => (Opcode::FILD, OperandCodeX87::St_Est),
                    6 => (Opcode::FBSTP, OperandCodeX87::Est_St),
                    7 => (Opcode::FISTP, OperandCodeX87::Est_St),
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
    // TODO: support 80-bit operands
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
        OperandCodeX87::St_Ew => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            instruction.operand_count = 2;
        }
        OperandCodeX87::St_Md => {
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[1] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.operand_count = 2;
        }
        OperandCodeX87::Md => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.operand_count = 1;
        }
        OperandCodeX87::Ew => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            instruction.operand_count = 1;
        }
        OperandCodeX87::Est_St => {
            instruction.operands[0] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Ed_St => {
            instruction.operands[0] = read_E_st(&mut bytes_iter, instruction, modrm, length)?;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Md_St => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            if instruction.operands[0] == OperandSpec::RegMMM {
                return Err(DecodeError::InvalidOperand);
            }
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.modrm_rrr = RegSpec::st(0);
            instruction.operand_count = 2;
        }
        OperandCodeX87::Ex87S => {
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            instruction.operand_count = 1;
        }
        OperandCodeX87::Nothing => {
            instruction.operand_count = 0;
        },
    }

    Ok(())
}

fn decode_one<'b, T: IntoIterator<Item=u8>>(decoder: &InstDecoder, bytes: T, instr: &'b mut Instruction) -> Result<(), DecodeError> {
    instr.operands = [
        OperandSpec::Nothing,
        OperandSpec::Nothing,
        OperandSpec::Nothing,
        OperandSpec::Nothing,
    ];
    let bytes_iter = bytes.into_iter();
    read_instr(decoder, bytes_iter, instr)
}

#[inline]
fn read_num<T: Iterator<Item=u8>>(bytes: &mut T, width: u8) -> Result<u64, DecodeError> {
    match width {
        1 => { bytes.next().map(|x| x as u64).ok_or(DecodeError::ExhaustedInput) }
        2 => {
            bytes.next().and_then(|b0| {
                bytes.next().map(|b1| u16::from_le_bytes([b0, b1]) as u64)
            }).ok_or(DecodeError::ExhaustedInput)
        }
        4 => {
            bytes.next()
                .and_then(|b0| bytes.next().map(|b1| (b0, b1)))
                .and_then(|(b0, b1)| bytes.next().map(|b2| (b0, b1, b2)))
                .and_then(|(b0, b1, b2)| bytes.next().map(|b3| u32::from_le_bytes([b0, b1, b2, b3]) as u64))
                .ok_or(DecodeError::ExhaustedInput)
        }
        8 => {
            bytes.next()
                .and_then(|b0| bytes.next().map(|b1| (b0, b1)))
                .and_then(|(b0, b1)| bytes.next().map(|b2| (b0, b1, b2)))
                .and_then(|(b0, b1, b2)| bytes.next().map(|b3| (b0, b1, b2, b3)))
                .and_then(|(b0, b1, b2, b3)| bytes.next().map(|b4| (b0, b1, b2, b3, b4)))
                .and_then(|(b0, b1, b2, b3, b4)| bytes.next().map(|b5| (b0, b1, b2, b3, b4, b5)))
                .and_then(|(b0, b1, b2, b3, b4, b5)| bytes.next().map(|b6| (b0, b1, b2, b3, b4, b5, b6)))
                .and_then(|(b0, b1, b2, b3, b4, b5, b6)| {
                    bytes.next().map(|b7| u64::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7]) as u64)
                })
                .ok_or(DecodeError::ExhaustedInput)
        }
        _ => {
            panic!("unsupported read size");
        }
    }
}

#[inline]
fn read_imm_ivq<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<u64, DecodeError> {
    match width {
        2 => {
            *length += 2;
            Ok(read_num(bytes, 2)? as u16 as u64)
        },
        4 => {
            *length += 4;
            Ok(read_num(bytes, 4)? as u32 as u64)
        },
        8 => {
            *length += 8;
            Ok(read_num(bytes, 8)? as u64)
        },
        _ => {
            unsafe { unreachable_unchecked(); }
        }
    }
}

#[inline]
fn read_imm_signed<T: Iterator<Item=u8>>(bytes: &mut T, num_width: u8, length: &mut u8) -> Result<i64, DecodeError> {
    if num_width == 1 {
        *length += 1;
        Ok(read_num(bytes, 1)? as i8 as i64)
    } else if num_width == 2 {
        *length += 2;
        Ok(read_num(bytes, 2)? as i16 as i64)
    } else {
        *length += 4;
        // this is for 4 and 8, the only values for num_width may be 1, 2, 4, and 8.
        Ok(read_num(bytes, 4)? as i32 as i64)
    }
}

#[inline]
fn read_imm_unsigned<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<u64, DecodeError> {
    read_num(bytes, width).map(|res| {
        *length += width;
        res
    })
}

#[inline]
fn imm_width_from_prefixes_64(interpretation: SizeCode, prefixes: Prefixes) -> u8 {
    match interpretation {
        SizeCode::b => 1,
        SizeCode::vd => {
            if prefixes.rex().w() || !prefixes.operand_size() { 4 } else { 2 }
        },
        SizeCode::vq => {
            // TODO: this should be address_size
            // but i'm not sure if that breaks other instructions rn
            if prefixes.operand_size() {
                2
            } else {
                8 // TODO: this 8 should be arch width.
            }
        },
        SizeCode::vqp => {
            if prefixes.rex().w() {
                8
            } else if prefixes.operand_size() {
                2
            } else {
                4
            }
        },
    }
}

#[inline]
fn read_modrm<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<u8, DecodeError> {
    bytes_iter.next().ok_or(DecodeError::ExhaustedInput).map(|b| { *length += 1; b })
}
