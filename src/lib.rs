#[cfg(feature="use-serde")]
#[macro_use] extern crate serde_derive;
#[cfg(feature="use-serde")]
extern crate serde;

extern crate yaxpeax_arch;
extern crate termion;

mod vex;
mod display;

use std::hint::unreachable_unchecked;

use yaxpeax_arch::{Arch, Decoder, LengthedInstruction};

#[cfg(feature="use-serde")]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct RegSpec {
    pub num: u8,
    pub bank: RegisterBank
}
#[cfg(not(feature="use-serde"))]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct RegSpec {
    pub num: u8,
    pub bank: RegisterBank
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
    #[inline]
    fn from_parts(num: u8, extended: bool, bank: RegisterBank) -> RegSpec {
        RegSpec {
            num: num + if extended { 0b1000 } else { 0 },
            bank: bank
        }
    }

    #[inline]
    fn gp_from_parts(num: u8, extended: bool, width: u8, rex: bool) -> RegSpec {
//        println!("from_parts width: {}, num: {}, extended: {}", width, num, extended);
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
    pub fn ax() -> RegSpec {
        RegSpec { bank: RegisterBank::W, num: 0 }
    }

    #[inline]
    pub fn al() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 0 }
    }

    #[inline]
    pub fn cl() -> RegSpec {
        RegSpec { bank: RegisterBank::B, num: 1 }
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
    Many(Vec<Operand>),
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
            OperandSpec::AL |
            OperandSpec::CL |
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
            OperandSpec::AL => {
                Operand::Register(RegSpec::al())
            }
            OperandSpec::CL => {
                Operand::Register(RegSpec::cl())
            }
            OperandSpec::ImmI8 => Operand::ImmediateI8(inst.imm as i8),
            OperandSpec::ImmU8 => Operand::ImmediateU8(inst.imm as u8),
            OperandSpec::ImmI16 => Operand::ImmediateI16(inst.imm as i16),
            OperandSpec::ImmU16 => Operand::ImmediateU16(inst.imm as u16),
            OperandSpec::ImmI32 => Operand::ImmediateI32(inst.imm as i32),
            OperandSpec::ImmU32 => Operand::ImmediateU32(inst.imm as u32),
            OperandSpec::ImmI64 => Operand::ImmediateI64(inst.imm as i64),
            OperandSpec::ImmU64 => Operand::ImmediateU64(inst.imm as u64),
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
            Operand::Many(els) => {
                for el in els.iter() {
                    if el.is_memory() {
                        return true;
                    }
                }

                false
            }
        }
    }
}

#[test]
fn operand_size() {
    assert_eq!(std::mem::size_of::<OperandSpec>(), 1);
    assert_eq!(std::mem::size_of::<RegSpec>(), 2);
    // assert_eq!(std::mem::size_of::<Prefixes>(), 4);
    // assert_eq!(std::mem::size_of::<Instruction>(), 40);
}

#[allow(non_camel_case_types)]
#[cfg(feature="use-serde")]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP, EFlags, RFlags,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
    K, // AVX512 mask registers
}
#[allow(non_camel_case_types)]
#[cfg(not(feature="use-serde"))]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
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

// TODO:
// PTWRITE
// RDFSBASE
// RDGSBASE
// WRFSBASE
// WRGSBASE
// TPAUSE
// UMONITOR
// UMWAIT
// CLFLUSHOPT
// CLWB
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Opcode {
    ADD = 1,
    OR = 2,
    ADC = 3,
    SBB = 4,
    AND = 5,
    XOR = 6,
    SUB = 7,
    CMP = 8,
    Invalid,
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
    ADDSUBPS,
    CVTSI2SS,
    CVTSI2SD,
    CVTTSD2SI,
    CVTTPS2DQ,
    CVTPD2DQ,
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
    CBD,
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
    OUTS,
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
    MOVQ,
    CMPSS,
    CMPSD,
    UNPCKLPS,
    UNPCKHPS,
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
    XEND,
    XTEST,
    ENCLU,
    RDPKRU,
    WRPKRU,

    ADDPS,
    ANDNPS,
    ANDPS,
    BSWAP,
    CMPPS,
    COMISS,
    CVTDQ2PS,
    CVTPI2PS,
    CVTPI2PD,
    CVTPS2PD,
    CVTPS2PI,
    CVTTPS2PI,
    DIVPS,
    EMMS,
    GETSEC,
    LFS,
    LGS,
    LSS,
    MASKMOVQ,
    MAXPS,
    MINPS,
    MOVAPS,
    MOVAPD,
    MOVD,
    MOVLPS,
    MOVHPS,
    MOVLHPS,
    MOVHLPS,
    MOVUPD,
    MOVMSKPS,
    MOVNTI,
    MOVNTPS,
    MOVNTQ,
    MULPS,
    ORPS,
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
    PEXTRW,
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
    PSLLD,
    PSLLQ,
    PSLLW,
    PSRAD,
    PSRAW,
    PSRLD,
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
    PXOR,
    RCPPS,
    RSM,
    RSQRTPS,
    SHLD,
    SHUFPS,
    SLHD,
    SQRTPS,
    SUBPS,
    SYSENTER,
    SYSEXIT,
    UCOMISS,
    UD2E,
    VMREAD,
    VMWRITE,
    XORPS,

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
    VPMAXUD,
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
    PMAXUW,
    PMAXUD,
    PMINSD,
    PMINSB,
    PMINUD,
    PMINUW,
    BLENDW,
    BLENDDVB,
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
    PMULHRSU,
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
    ADDSUBPD,
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

#[derive(Debug, Copy, Clone, PartialEq)]
enum OperandSpec {
    Nothing,
    // the register in modrm_rrr
    RegRRR,
    // the register in modrm_mmm (eg modrm mod bits were 11)
    RegMMM,
    // the register selected by vex-vvvv bits
    RegVex,
    // the register `al`. Used for MOVS.
    AL,
    // the register `cl`. Used for SHLD and SHRD.
    CL,
    ImmI8,
    ImmI16,
    ImmI32,
    ImmI64,
    ImmU8,
    ImmU16,
    ImmU32,
    ImmU64,
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
pub struct x86_64;

#[cfg(not(feature="use-serde"))]
#[derive(Hash, Eq, PartialEq, Debug)]
#[allow(non_camel_case_types)]
pub struct x86_64;

impl Arch for x86_64 {
    type Address = u64;
    type Instruction = Instruction;
    type Decoder = InstDecoder;
    type Operand = Operand;
}

impl LengthedInstruction for Instruction {
    type Unit = u64;
    fn len(&self) -> u64 {
        self.length.into()
    }
    fn min_size() -> u64 {
        1
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
        self.flags & (1 << 54) != 0
    }

    pub fn with_avx(mut self) -> Self {
        self.flags |= 1 << 54;
        self
    }

    /// Optionally reject or reinterpret instruction according to the decoder's
    /// declared extensions.
    fn revise_instruction(&self, inst: &mut Instruction) -> Result<(), ()> {
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
                    return Err(());
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
            Opcode::PMULHRSU |
            Opcode::PSHUFB |
            Opcode::PSIGNB |
            Opcode::PSIGNW |
            Opcode::PSIGND |
            Opcode::PALIGNR => {
                // via Intel section 5.8, SSSE3 Instructions
                if !self.ssse3() {
                    inst.opcode = Opcode::Invalid;
                    return Err(());
                }
            }
            Opcode::PMULLD |
            Opcode::PMULDQ |
            Opcode::MOVNTDQA |
            Opcode::BLENDPD |
            Opcode::BLENDPS |
            Opcode::BLENDVPD |
            Opcode::BLENDVPS |
            Opcode::BLENDDVB |
            Opcode::BLENDW |
            Opcode::PMINUW |
            Opcode::PMINUD |
            Opcode::PMINSB |
            Opcode::PMINSD |
            Opcode::PMAXUW |
            Opcode::PMAXUD |
            Opcode::PMAXSB |
            Opcode::ROUNDPS |
            Opcode::ROUNDPD |
            Opcode::ROUNDSS |
            Opcode::ROUNDSD |
            Opcode::EXTRACTPS |
            Opcode::INSERTPS |
            Opcode::PINSRB |
            Opcode::PINSRD |
            Opcode::PINSRQ |
            Opcode::PEXTRB |
            Opcode::PEXTRW |
            Opcode::PEXTRQ |
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
            Opcode::MPSADBW |
            Opcode::PHMINPOSUW |
            Opcode::PTEST |
            Opcode::PCMPEQQ |
            Opcode::PACKUSDW => {
                // via Intel section 5.10, SSE4.1 Instructions
                if !self.sse4_1() {
                    inst.opcode = Opcode::Invalid;
                    return Err(());
                }
            }
            Opcode::PCMPESTRI |
            Opcode::PCMPESTRM |
            Opcode::PCMPISTRI |
            Opcode::PCMPISTRM |
            Opcode::PCMPGTQ => {
                // via Intel section 5.11, SSE4.2 Instructions
                if !self.sse4_2() {
                    inst.opcode = Opcode::Invalid;
                    return Err(());
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
                    return Err(());
                }
            }
            Opcode::PCLMULQDQ => {
                // via Intel section 5.12. AESNI AND PCLMULQDQ
                if !self.pclmulqdq() {
                    inst.opcode = Opcode::Invalid;
                    return Err(());
                }
            }
            // AVX...
            /* // TODO
            Opcode::XABORT |
            Opcode::XACQUIRE |
            Opcode::XRELEASE |
            Opcode::XBEGIN |
            Opcode::XEND |
            Opcode::XTEST => {
                if !self.tsx() {
                    inst.opcode = Opcode::Invalid;
                    return Err(());
                }
            }
            */
            /* // TODO
            Opcode::SHA1MSG1 |
            Opcode::SHA1MSG2 |
            Opcode::SHA1NEXTE |
            Opcode::SHA1RNDS4 |
            Opcode::SHA256MSG1 |
            Opcode::SHA256MSG2 |
            Opcode::SHA256RNDS2 => {
                if !self.sha() {
                    inst.opcode = Opcode::Invalid;
                    return Err(());
                }
            }*/
            Opcode::ENCLV |
            Opcode::ENCLS |
            Opcode::ENCLU => {
                if !self.sgx() {
                    inst.opcode = Opcode::Invalid;
                    return Err(());
                }
            }
            _ => {}
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
    fn decode<T: IntoIterator<Item=u8>>(&self, bytes: T) -> Option<Instruction> {
        let mut instr = Instruction::invalid();
        match decode_one(self, bytes, &mut instr) {
            Some(_) => Some(instr),
            None => None
        }
    }
    fn decode_into<T: IntoIterator<Item=u8>>(&self, instr: &mut Instruction, bytes: T) -> Option<()> {
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
}

#[derive(Debug, Copy, Clone)]
pub struct PrefixVex {
    bits: u8,
}

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
        }
    }
    #[inline]
    fn rep(&self) -> bool { self.bits & 0x30 == 0x10 }
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
    pub fn set_lock(&mut self) { self.bits |= 0x4 }
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

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OperandCode {
    ModRM_0x0f00,
    ModRM_0x0f01,
    ModRM_0x0f13,
    ModRM_0x0fae,
    ModRM_0x0fba,
    ModRM_0xf238,
    ModRM_0xf30fc7,
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
    OR,
    AH,
    AX_Xv,
    DX_AX,
    E_G_xmm,
    E_G_ymm,
    E_G_zmm,
    G_E_ymm,
    G_E_zmm,
    Ev_Ivs,
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
    G_E_q,
    G_M_q,
    E_G_q,
    Rv_Gmm_Ib,
    G_mm_Ew_Ib,
    Mq_Dq,
    eAX,
    ModRM_0x0f38,
    ModRM_0x0f3a,
    ModRM_0x0f71,
    ModRM_0x0f72,
    ModRM_0x0f73,
    ModRM_0x0fc7,
    Nothing,
    Implied,
    Unsupported,
    Zv_R0 = 0x40,
    Zv_R1 = 0x41,
    Zv_R2 = 0x42,
    Zv_R3 = 0x43,
    Zv_R4 = 0x44,
    Zv_R5 = 0x45,
    Zv_R6 = 0x46,
    Zv_R7 = 0x47,
    Zv_AX_R0 = 0x48,
    Zv_AX_R1 = 0x49,
    Zv_AX_R2 = 0x4a,
    Zv_AX_R3 = 0x4b,
    Zv_AX_R4 = 0x4c,
    Zv_AX_R5 = 0x4d,
    Zv_AX_R6 = 0x4e,
    Zv_AX_R7 = 0x4f,
    Zb_Ib_R0 = 0x50,
    Zb_Ib_R1 = 0x51,
    Zb_Ib_R2 = 0x52,
    Zb_Ib_R3 = 0x53,
    Zb_Ib_R4 = 0x54,
    Zb_Ib_R5 = 0x55,
    Zb_Ib_R6 = 0x56,
    Zb_Ib_R7 = 0x57,
    Zv_Ivq_R0 = 0x58,
    Zv_Ivq_R1 = 0x59,
    Zv_Ivq_R2 = 0x5a,
    Zv_Ivq_R3 = 0x5b,
    Zv_Ivq_R4 = 0x5c,
    Zv_Ivq_R5 = 0x5d,
    Zv_Ivq_R6 = 0x5e,
    Zv_Ivq_R7 = 0x5f,
    Gv_Eb = 0x60,
    Gv_Ew = 0x61,
    Gdq_Ed = 0x62,
    G_E_xmm = 0x63,
    G_E_mm_Ib = 0x64,
    G_E_xmm_Ib = 0x65,
    AL_Ib = 0x66,
    AX_Ivd = 0x67,
    AL_Ob = 0x68,
    AL_Xb = 0x69,
    AX_AL = 0x6a,
    AX_Ov = 0x6b,

    Eb_Gb = 0x80,
    Ev_Gv = 0x81,
    Gb_Eb = 0xc2,
    Gv_Ev = 0xc3,
    Gb_Eb_Ib = 0xc4,
    Gv_Ev_Iv = 0xc5,
    // gap: 0xc6
    Gd_U_xmm = 0xc7,
    M_G_xmm = 0xc9,
    ModRM_0x0f12 = 0xcb,
    ModRM_0x0f16 = 0xce,
    ModRM_0xc0_Eb_Ib = 0x86,
    ModRM_0xc1_Ev_Ib = 0x87,
    ModRM_0xd0_Eb_1 = 0x88,
    ModRM_0xd1_Ev_1 = 0x89,
    ModRM_0xd2_Eb_CL = 0x8a,
    ModRM_0xd3_Ev_CL = 0x8b,
    ModRM_0x80_Eb_Ib = 0x8c,
    ModRM_0x83_Ev_Ibs = 0x8d,
    // this would be Eb_Ivs, 0x8e
    ModRM_0x81_Ev_Ivs = 0x8f,
    ModRM_0xc6_Eb_Ib = 0x90,
    ModRM_0xc7_Ev_Iv = 0x91,
    ModRM_0xfe_Eb = 0x92,
    ModRM_0x8f_Ev = 0x93,
    // gap, 0x94
    ModRM_0xff_Ev = 0x95,
    ModRM_0xf6 = 0x96,
    ModRM_0xf7 = 0x97,
    Eb_R0 = 0x98,
    Ev = 0x99,
    ModRM_0x0f18 = 0x9b,
    // gap, 0x9a
    Gv_M = 0xdb,
    G_mm_Edq = 0xdd,
    G_mm_E = 0xdf,
    G_xmm_Ed = 0xe1,
    G_xmm_Eq = 0xe3,
    G_mm_E_xmm = 0xe5,
    G_E_mm = 0xe7,
    Edq_G_mm = 0xe9,
    E_G_mm = 0xeb,
    G_xmm_E_mm = 0xed,
    G_xmm_Edq = 0xef,
    G_U_mm = 0xf1,
    Ev_Gv_Ib = 0xf3,
    Ev_Gv_CL = 0xf5,
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
// 0xd0
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
// 0xe0
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
    OpcodeRecord(Interpretation::Instruction(Opcode::PXOR), OperandCode::G_E_xmm),
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

fn read_opcode_660f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<(OpcodeRecord, u8), ()> {
    bytes_iter.next().ok_or(()).map(|b| {
        *length += 1;
        (OPCODE_660F_MAP[b as usize], b)
    })
}

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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0xf238),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVDQ2Q), OperandCode::Unsupported),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::LDDQU), OperandCode::G_E_xmm),
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

fn read_opcode_f20f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<(OpcodeRecord, u8), ()> {
    bytes_iter.next().ok_or(()).map(|b| {
        *length += 1;
        (OPCODE_F20F_MAP[b as usize], b)
    })
}

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
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTSS2SI), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSS2SI), OperandCode::G_E_xmm),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::RCPSS), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADDSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::MULSS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSS2SD), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTTPS2DQ), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ), OperandCode::G_E_xmm),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::POPCNT), OperandCode::Gv_Ev),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVQ2DQ), OperandCode::Unsupported),
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

fn read_opcode_f30f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<(OpcodeRecord, u8), ()> {
    bytes_iter.next().ok_or(()).map(|b| {
        *length += 1;
        (OPCODE_F30F_MAP[b as usize], b)
    })
    /*
    match bytes_iter.next() {
        Some(b) => {
            *length += 1;
            match b {
                0x10 => { instruction.opcode = Opcode::MOVSS; Some(OperandCode::G_E_xmm) },
                0x11 => { instruction.opcode = Opcode::MOVSS; Some(OperandCode::E_G_xmm) },
                0x12 => { instruction.opcode = Opcode::MOVSLDUP; Some(OperandCode::G_E_xmm) },
                0x2a => { instruction.opcode = Opcode::CVTSI2SS; Some(OperandCode::G_E_xmm) },
                0x2c => { instruction.opcode = Opcode::CVTTSS2SI; Some(OperandCode::G_E_xmm) },
                0x2d => { instruction.opcode = Opcode::CVTSS2SI; Some(OperandCode::G_E_xmm) },
                0x51 => { instruction.opcode = Opcode::SQRTSS; Some(OperandCode::G_E_xmm) },
                0x58 => { instruction.opcode = Opcode::ADDSS; Some(OperandCode::G_E_xmm) },
                0x59 => { instruction.opcode = Opcode::MULSS; Some(OperandCode::G_E_xmm) },
                0x5a => { instruction.opcode = Opcode::CVTSS2SD; Some(OperandCode::G_E_xmm) },
                0x5c => { instruction.opcode = Opcode::SUBSS; Some(OperandCode::G_E_xmm) },
                0x5d => { instruction.opcode = Opcode::MINSS; Some(OperandCode::G_E_xmm) },
                0x5e => { instruction.opcode = Opcode::DIVSS; Some(OperandCode::G_E_xmm) },
                0x5f => { instruction.opcode = Opcode::MAXSS; Some(OperandCode::G_E_xmm) },
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    Some(OperandCode::Nothing)
                }
            }
        }
        None => {
            None
        }
    }
    */
}
const OPCODE_0F_MAP: [OpcodeRecord; 256] = [
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f00),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f01),
    OpcodeRecord(Interpretation::Instruction(Opcode::LAR), OperandCode::Gv_Ew),
    OpcodeRecord(Interpretation::Instruction(Opcode::LSL), OperandCode::Gv_M),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SYSCALL), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CLTS), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SYSRET), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::INVD), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::WBINVD), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::UD2), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::NOP), OperandCode::Ev),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PCMPEQD), OperandCode::Unsupported),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::BTR), OperandCode::E_G_q),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTI), OperandCode::Mq_Dq),
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

    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBUSW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMINUB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PAND), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDUSW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMAXUB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PANDN), OperandCode::Unsupported),

// 0xe0
    OpcodeRecord(Interpretation::Instruction(Opcode::PAVGB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRAW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSRAD), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PAVGW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULHUW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULHW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVNTQ), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBSW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMINSW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::POR), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDSB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDSW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMAXSW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PXOR), OperandCode::G_E_mm),
// 0xf0
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLD), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSLLQ), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMULUDQ), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PMADDWD), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSADBW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::MASKMOVQ), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBD), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PSUBQ), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDB), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDW), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::PADDD), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
];
fn read_opcode_0f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<OpcodeRecord, ()> {
    bytes_iter.next().ok_or(()).map(|b| {
        *length += 1;
        OPCODE_0F_MAP[b as usize]
    })
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Interpretation {
    Instruction(Opcode),
    Prefix,
}

#[derive(Copy, Clone, Debug)]
// this should be a 32-byte struct..
struct OpcodeRecord(Interpretation, OperandCode);

#[test]
fn opcode_record_size() {
    // there are more than 256 opcodes...
    assert_eq!(std::mem::size_of::<OpcodeRecord>(), 4);
}

const OPCODES: [OpcodeRecord; 256] = [
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADD), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::OR), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::ADC), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::SBB), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::AND), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::SUB), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::AL_Ib),
    OpcodeRecord(Interpretation::Instruction(Opcode::XOR), OperandCode::AX_Ivd),
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Gb_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMP), OperandCode::AL_Ib),
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
    OpcodeRecord(Interpretation::Prefix, OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::TEST), OperandCode::AL_Ib),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::INTO), OperandCode::Fw),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 d8
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 d9
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 da
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 db
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 dc
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 dd
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 de
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // x86 df
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xe0
    // LOOPNZ
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // LOOPZ
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // LOOP
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // JECXZ
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // IN
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // IN
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // OUT
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // OUT
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
// 0xe8
    OpcodeRecord(Interpretation::Instruction(Opcode::CALL), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::JMP), OperandCode::Jvds),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::JMP), OperandCode::Ibs),
    // IN
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // IN
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // OUT
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    // OUT
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
pub(crate) fn read_E<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, width: u8, length: &mut u8) -> Result<OperandSpec, ()> {
    let bank = width_to_gp_reg_bank(width, instr.prefixes.rex().present());
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, bank)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}
#[allow(non_snake_case)]
pub(crate) fn read_E_xmm<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, ()> {
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, RegisterBank::X)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}
#[allow(non_snake_case)]
pub(crate) fn read_E_ymm<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, ()> {
    if modrm >= 0b11000000 {
        read_modrm_reg(instr, modrm, RegisterBank::Y)
    } else {
        read_M(bytes_iter, instr, modrm, length)
    }
}

#[allow(non_snake_case)]
fn read_modrm_reg(instr: &mut Instruction, modrm: u8, reg_bank: RegisterBank) -> Result<OperandSpec, ()> {
    instr.modrm_mmm = RegSpec::from_parts(modrm & 7, instr.prefixes.rex().b(), reg_bank);
    Ok(OperandSpec::RegMMM)
}

#[allow(non_snake_case)]
fn read_sib<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, ()> {
    let modbits = (modrm >> 6);
    let addr_width = if instr.prefixes.address_size() { RegisterBank::D } else { RegisterBank::Q };
    let sibbyte = match bytes_iter.next() {
        Some(b) => b,
//        None => { unsafe { unreachable_unchecked(); } }
        None => { return Err(()); } //Err("Out of bytes".to_string())
    };
    *length += 1;

    let op_spec = if (sibbyte & 7) == 0b101 {
        let disp = if modbits == 0b00 {
            *length += 4;
            read_num(bytes_iter, 4)? as i32
        } else if modbits == 0b01 {
            *length += 1;
            read_num(bytes_iter, 1)? as i8 as i32
        } else {
            *length += 4;
            read_num(bytes_iter, 4)? as i32
        };

        if ((sibbyte >> 3) & 7) == 0b100 {
            if modbits == 0b00 && !instr.prefixes.rex().x() {
                instr.disp = disp as u32 as u64;

                OperandSpec::DispU32
            } else {
                let reg = RegSpec::from_parts(0b100, instr.prefixes.rex().x(), addr_width);
                instr.modrm_mmm = reg;

                if disp == 0 {
                    OperandSpec::Deref
                } else {
                    instr.disp = disp as i64 as u64;
                    OperandSpec::RegDisp
                }
            }
        } else {
            instr.modrm_mmm = RegSpec::from_parts(5, instr.prefixes.rex().b(), addr_width);

            instr.sib_index = RegSpec::from_parts((sibbyte >> 3) & 7, instr.prefixes.rex().x(), addr_width);
            let scale = 1u8 << (sibbyte >> 6);
            instr.scale = scale;

            if disp == 0 {
                if modbits == 0 {
                    OperandSpec::RegScaleDisp
                } else {
                    OperandSpec::RegIndexBaseScaleDisp
                }
            } else {
                instr.disp = disp as i64 as u64;
                if modbits == 0 {
                    OperandSpec::RegScale
                } else {
                    OperandSpec::RegIndexBaseScale
                }
            }
        }
    } else {
        instr.modrm_mmm = RegSpec::from_parts((sibbyte & 7), instr.prefixes.rex().b(), addr_width);

        let disp = if modbits == 0b00 {
            0
        } else if modbits == 0b01 {
            *length += 1;
            read_num(bytes_iter, 1)? as i8 as i32
        } else {
            *length += 4;
            read_num(bytes_iter, 4)? as i32
        };

        if ((sibbyte >> 3) & 7) == 0b100 {
            if disp == 0 {
                OperandSpec::Deref
            } else {
                instr.disp = disp as i64 as u64;
                OperandSpec::RegDisp
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
fn read_M<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, length: &mut u8) -> Result<OperandSpec, ()> {
    let modbits = (modrm >> 6);
    let addr_width = if instr.prefixes.address_size() { RegisterBank::D } else { RegisterBank::Q };
    let mmm = modrm & 7;
    let op_spec = if mmm == 4 {
        return read_sib(bytes_iter, instr, modrm, length);
//         let (ss, index, base) = octets_of(sibbyte);

//            println!("scale: {:b}, index: {:b}, base: {:b}", ss, index, base);
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

pub fn read_instr<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction) -> Result<(), ()> {
    let mut length = 0u8;
    let mut alternate_opcode_map: Option<OpcodeMap> = None;
//    use std::intrinsics::unlikely;
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
                let record = OPCODES[b as usize];
                if (b & 0xf0) == 0x40 {
                    prefixes.rex_from(b);
                } else if b == 0x0f {
                    let record = match alternate_opcode_map {
                        Some(opcode_map) => {
                            let (rec, opcode_byte) = match opcode_map {
                                OpcodeMap::Map66 => {
                                    read_opcode_660f_map(&mut bytes_iter, &mut length)?
                                },
                                OpcodeMap::MapF2 => {
                                    read_opcode_f20f_map(&mut bytes_iter, &mut length)?
                                },
                                OpcodeMap::MapF3 => {
                                    read_opcode_f30f_map(&mut bytes_iter, &mut length)?
                                },
                            };
                            if rec.0 == Interpretation::Instruction(Opcode::Invalid) {
                                escapes_are_prefixes_actually(&mut prefixes, &mut Some(opcode_map));
                                OPCODE_0F_MAP[opcode_byte as usize]
                            } else {
                                rec
                            }
                        },
                        None => {
                            read_opcode_0f_map(&mut bytes_iter, &mut length)?
                        }
                    };

                    break record;
                } else if let Interpretation::Instruction(_) = record.0 {
                    escapes_are_prefixes_actually(&mut prefixes, &mut alternate_opcode_map);
                    break record;
                } else {
                    println!("prefix: {:02x}, extant prefixes: {:?}", b, prefixes);
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
                            return Err(());
                        } else {
                            instruction.prefixes = prefixes;
                            return vex::two_byte_vex(&mut bytes_iter, instruction, length);
                        }
                    } else if b == 0xc4 {
                        if prefixes.rex().present() || prefixes.lock() || prefixes.operand_size() || prefixes.rep() || prefixes.repnz() {
                            // rex and then vex is invalid! reject it.
                            instruction.opcode = Opcode::Invalid;
                            return Err(());
                        } else {
                            instruction.prefixes = prefixes;
                            return vex::three_byte_vex(&mut bytes_iter, instruction, length);
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
//                unsafe { unreachable_unchecked(); }
                return Err(());
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
    instruction.length = length;

    if decoder != &InstDecoder::default() {
        // we might have to fix up or reject this instruction under whatever cpu features we need to
        // pretend to have.
        decoder.revise_instruction(instruction)?;
    }
    Ok(())
}
pub fn read_operands<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction, operand_code: OperandCode, length: &mut u8) -> Result<(), ()> {
    let mut bytes_read = 0;
    if (operand_code as u8) & 0x40 == 0x40 {
        instruction.operands[0] = OperandSpec::RegRRR;
    }
    if (operand_code as u8) >= 0x40 && (operand_code as u8) < 0x60 {
        let reg = (operand_code as u8) & 0x07;
        let category = ((operand_code as u8) & 0x18) >> 3;
        if category == 0 {
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
        // Zv_AX are missing!
        } else if category == 2 {
            // these are Zb_Ib_R
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(reg, instruction.prefixes.rex().b(), 1, instruction.prefixes.rex().present());
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, 1)?;
            bytes_read = 1;
            instruction.operands[1] = OperandSpec::ImmU8;
            instruction.operand_count = 2;
        } else {
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
            bytes_read = opwidth;
            instruction.operands[1] = match opwidth {
                1 => OperandSpec::ImmI8,
                2 => OperandSpec::ImmI16,
                4 => OperandSpec::ImmI32,
                8 => OperandSpec::ImmI64,
                _ => unsafe { unreachable_unchecked() }
            };
            instruction.operand_count = 2;
        }
        return Ok(());
    }

    let mut modrm = 0;
    let mut opwidth = 0;
    let mut mem_oper = OperandSpec::Nothing;
    let mut bank = RegisterBank::Q;
    let code_int = operand_code as u8;
    if ((code_int) & 0x80) == 0x80 {
        // cool! we can precompute opwidth and know we need to read_E.
        if (code_int & 1) == 1 {
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
        modrm = read_modrm(&mut bytes_iter, instruction, length)?;
        instruction.modrm_rrr =
            RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), bank);

        mem_oper = read_E(&mut bytes_iter, instruction, modrm, opwidth, length)?;
    }

    let numeric_code = (operand_code as u8) & 0xbf;
    if numeric_code >= 0x80 && numeric_code < 0x84 {
        let (mmm, rrr) = if numeric_code & 0x02 == 2 {
            (1, 0)
        } else {
            (0, 1)
        };
        instruction.operands[mmm] = mem_oper;
        instruction.operands[rrr] = OperandSpec::RegRRR;
        instruction.operand_count = 2;
    } else if operand_code == OperandCode::Ibs {
        instruction.imm =
            read_imm_signed(&mut bytes_iter, 1, length)? as u64;
        instruction.operands[0] = OperandSpec::ImmI8;
        instruction.operand_count = 1;
    } else {
    match operand_code {
        /*
        Gv_Ev_Iv,
        Gb_Eb_Ib,
        Yb_DX,
        Yv_DX,
        DX_Xb,
        DX_Xv,
        OR,
        AH,
        AL_Ib,
        AL_Ob,
        AL_Xb,
        AX_AL,
        AX_Ivd,
        AX_Ov,
        AX_Xv,
        DX_AX,
        Eb_1,
        Eb_Ib,
        Eb_CL,
        Ev,
        Ev_1,
        Ev_CL,
        Ev_Ibs,
        Ev_Iv,
        Ev_Ivs,
        Ew_Sw,
        Fw,
        Gv_M,
        I_3,
        Ib,
        Ibs,
        Ivs,
        Iw,
        Iw_Ib,
        Ob_AL,
        Ov_AX,
        Sw_Ew,
        Yb_AL,
        Yb_Xb,
        Yv_AX,
        Yv_Xv,
        Zb_Ib,
        Zv,
        Zv_AX,
        */
        OperandCode::Eb_R0 => {
            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err(()); // Err("Invalid modr/m for opcode 0xc6".to_owned());
            }

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
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let imm = read_num(&mut bytes_iter, addr_width)?;
            *length += addr_width;
            bytes_read = addr_width;
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
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let imm = read_num(&mut bytes_iter, addr_width)?;
            *length += addr_width;
            bytes_read = addr_width;
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
        op @ OperandCode::ModRM_0x80_Eb_Ib |
        op @ OperandCode::ModRM_0x81_Ev_Ivs => {
            instruction.operands[0] = mem_oper;
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
            bytes_read = numwidth;
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
            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err(()); // Err("Invalid modr/m for opcode 0xc7".to_string());
            }

            instruction.operands[0] = mem_oper;
            instruction.opcode = Opcode::MOV;
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
            bytes_read = numwidth;
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
        op @ OperandCode::ModRM_0xd3_Ev_CL => {
            instruction.operands[0] = mem_oper;
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            if let OperandCode::ModRM_0xd3_Ev_CL = op {
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
        op @ OperandCode::ModRM_0xf6 |
        op @ OperandCode::ModRM_0xf7 => {
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
            match ((modrm >> 3) & 7) {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    let numwidth = if opwidth == 8 { 4 } else { opwidth };
                    instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, length)? as u64;
                    bytes_read = numwidth;
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
            instruction.opcode = [
                Opcode::INC,
                Opcode::DEC,
                Opcode::Invalid,
                Opcode::Invalid,
                Opcode::Invalid,
                Opcode::Invalid,
                Opcode::Invalid,
                Opcode::Invalid
            ][((modrm >> 3) & 7) as usize];
            instruction.operand_count = 1;
        }
        OperandCode::ModRM_0xff_Ev => {
            instruction.operands[0] = mem_oper;
            let opcode = [
                Opcode::INC,
                Opcode::DEC,
                Opcode::CALL,
                Opcode::CALLF,
                Opcode::JMP,
                Opcode::JMPF,
                Opcode::PUSH,
                Opcode::Invalid
            ][((modrm >> 3) & 7) as usize];
            instruction.opcode = opcode;
            instruction.operand_count = 1;
        }
        OperandCode::Gv_Eb => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 1, length)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Gv_Ew => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Gdq_Ed => {
            let opwidth = 8;
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4 /* opwidth */, length)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::Ev => {
            instruction.operands[0] = mem_oper;
            instruction.operand_count = 1;
        },
        OperandCode::Gv_M => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present());
            instruction.operand_count = 2;
        },
        OperandCode::E_G_xmm => {
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[0] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
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
                return Err(());
            }
            instruction.modrm_mmm.bank = RegisterBank::MM;
            instruction.modrm_mmm.num &= 0b111;
            instruction.operand_count = 2;
        },
        OperandCode::G_E_xmm => {
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X);
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        },
        OperandCode::G_E_mm_Ib => {
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[1] = read_E_xmm(&mut bytes_iter, instruction, modrm, length)?;
            instruction.modrm_rrr = RegSpec { bank: RegisterBank::MM, num: (modrm >> 3) & 7 };
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u64;
            *length += 1;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        },
        OperandCode::G_mm_Ew_Ib => {
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            instruction.operands[1] = read_E(&mut bytes_iter, instruction, modrm, 4, length)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, false, RegisterBank::MM);
            instruction.operands[0] = OperandSpec::RegRRR;
            if instruction.operands[1] == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
            instruction.imm =
                read_num(&mut bytes_iter, 1)? as u8 as u64;
            *length += 1;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::AL_Ib => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            bytes_read = 1;
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
            bytes_read = numwidth;
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
                read_imm_unsigned(&mut bytes_iter, opwidth)?;
            bytes_read = opwidth;
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
            bytes_read = 2;
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
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::Gv_Ev_Iv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.operands[0] = OperandSpec::RegRRR;
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
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[2] = OperandSpec::ImmI8;
            instruction.operand_count = 3;
        }
        OperandCode::Ev_Gv_CL => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operands[2] = OperandSpec::CL;
            instruction.operand_count = 3;
        }
        OperandCode::Nothing => {
            instruction.operand_count = 0;
        }
        _ => {
            unlikely_operands(decoder, bytes_iter, instruction, operand_code, mem_oper, length)?;
        }
    };
    }

    Ok(())
}
fn unlikely_operands<T: Iterator<Item=u8>>(decoder: &InstDecoder, mut bytes_iter: T, instruction: &mut Instruction, operand_code: OperandCode, mem_oper: OperandSpec, length: &mut u8) -> Result<(), ()> {
    let mut bytes_read = 0;
    match operand_code {
        OperandCode::ModRM_0x0f71 => {
            instruction.operand_count = 2;

            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            if modrm & 0xc0 != 0xc0 {
                return Err(());
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
                    return Err(());
                }
            }

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmI8;
        },
        OperandCode::ModRM_0x0f72 => {
            instruction.operand_count = 2;

            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            if modrm & 0xc0 != 0xc0 {
                return Err(());
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
                    return Err(());
                }
            }

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmI8;
        },
        OperandCode::ModRM_0x0f73 => {
            instruction.operand_count = 2;

            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            if modrm & 0xc0 != 0xc0 {
                return Err(());
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
                    return Err(());
                }
            }

            instruction.modrm_mmm = RegSpec { bank: RegisterBank::MM, num: modrm & 7 };
            instruction.operands[0] = OperandSpec::RegMMM;
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, length)? as u64;
            instruction.operands[1] = OperandSpec::ImmI8;
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
        OperandCode::G_xmm_Ed => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::D;
            }
        },
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
        OperandCode::G_xmm_E_mm => {
            instruction.operands[1] = mem_oper;
            instruction.modrm_rrr.bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.modrm_mmm.bank = RegisterBank::MM;
                instruction.modrm_mmm.num &= 0b111;
            }
        },
        // sure hope these aren't backwards huh
        OperandCode::AL_Xb => {
            instruction.operands[0] = OperandSpec::AL;
            instruction.operands[1] = OperandSpec::Deref_rsi;
        }
        // TODO: two memory operands! this is wrong!!!
        OperandCode::Yb_Xb => {
            instruction.operands[0] = OperandSpec::Deref_rdi;
            instruction.operands[1] = OperandSpec::Deref_rsi;
        }
        OperandCode::Yb_AL => {
            instruction.operands[0] = OperandSpec::Deref_rdi;
            instruction.operands[1] = OperandSpec::AL;
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
            instruction.operands[0] = OperandSpec::RegRRR;
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
                return Err(());
            }
            let rrr = instruction.modrm_rrr.num & 0b111;
            instruction.operands[0] = mem_oper;
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
                return Err(());
            }
            instruction.modrm_rrr.bank = RegisterBank::D;
            instruction.modrm_mmm.bank = RegisterBank::X;
        }
        OperandCode::M_G_xmm => {
            instruction.operands[1] = instruction.operands[0];
            instruction.operands[0] = mem_oper;
            if instruction.operands[0] == OperandSpec::RegMMM {
                instruction.opcode = Opcode::Invalid;
                return Err(());
            }
            instruction.modrm_rrr.bank = RegisterBank::X;
        }
        OperandCode::Ew_Sw => {
            let opwidth = 2;
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            bytes_read = 1;

            // check r
            if ((modrm >> 3) & 7) > 5 {
                return Err(()); //Err("Invalid r".to_owned());
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
            let opwidth = 2;
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;

            // check r
            if ((modrm >> 3) & 7) > 5 {
                return Err(()); // Err("Invalid r".to_owned());
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
            instruction.opcode = match opwidth {
                2 => { Opcode::CBW },
                4 => { Opcode::CWDE },
                8 => { Opcode::CDQE },
                _ => { unreachable!("invalid operation width"); },
            }
        }
        OperandCode::CVT_DA => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, instruction.prefixes);
            instruction.opcode = match opwidth {
                2 => { Opcode::CBD },
                4 => { Opcode::CDQ },
                8 => { Opcode::CQO },
                _ => { unreachable!("invalid operation width"); },
            }
        }
        OperandCode::Iw => {
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, 2)?;
            bytes_read = 2;
            instruction.operands[0] = OperandSpec::ImmU16;
            instruction.operand_count = 1;
        }
        OperandCode::ModRM_0x0f00 => {
            instruction.operand_count = 1;
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
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
                instruction.operand_count = 0;
                return Ok(());
            } else if r == 7 {
                instruction.opcode = Opcode::Invalid;
                instruction.operand_count = 0;
                return Err(());
            } else {
                unreachable!("r <= 8");
            }
            instruction.operands[0] = read_E(&mut bytes_iter, instruction, modrm, 2, length)?;
        }
        OperandCode::ModRM_0x0f01 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, instruction.prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            let r = (modrm >> 3) & 7;
            if r == 0 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
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
                            return Err(());
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
                            return Err(());
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
                            return Err(());
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
                    instruction.opcode = Opcode::Invalid;
                    instruction.operand_count = 0;
                    return Err(());
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
                        instruction.operand_count = 1;
                    }
                    0b111 => {
                        instruction.opcode = Opcode::WRPKRU;
                        instruction.operand_count = 1;
                    }
                    _ => {
                        instruction.opcode = Opcode::Invalid;
                        instruction.operand_count = 0;
                        return Err(());
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
                        instruction.operand_count = 0;
                    } else if m == 1 {
                        instruction.opcode = Opcode::RDTSCP;
                        instruction.operand_count = 0;
                    } else {
                        instruction.opcode = Opcode::Invalid;
                        return Err(());
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
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            let r = (modrm >> 3) & 7;
            let mod_bits = modrm >> 6;

            // all the 0b11 instructions are err or no-operands
            if mod_bits == 0b11 {
                instruction.operand_count = 0;
                let m = modrm & 7;
                match r {
                    // invalid rrr for 0x0fae, mod: 11
                    0 | 1 | 2 | 3 | 4 => {
                        return Err(());
                    },
                    5 => {
                        instruction.opcode = Opcode::LFENCE;
                        // Intel's manual accepts m != 0, AMD supports m != 0 though the manual
                        // doesn't say (tested on threadripper)
                        if !decoder.amd_quirks() && !decoder.intel_quirks() {
                            if m != 0 {
                                instruction.opcode = Opcode::Invalid;
                                return Err(());
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
                                return Err(());
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
                                return Err(());
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
            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
            let r = (modrm >> 3) & 7;
            match r {
                0 | 1 | 2 | 3 => {
                    instruction.opcode = Opcode::Invalid;
                    return Err(()); //Err("invalid instruction".to_string());
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
            bytes_read += 2;
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

            let modrm = read_modrm(&mut bytes_iter, instruction, length)?;
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
        OperandCode::I_3 => {
            instruction.imm = 3;
            instruction.operands[0] = OperandSpec::ImmU8;
            instruction.operand_count = 1;
        }
        _ => {
            instruction.operand_count = 0;
            instruction.opcode = Opcode::Invalid;
            return Err(()); // Err(format!("unsupported operand code: {:?}", operand_code));
        //    unsafe { unreachable_unchecked(); }
        }
    };
    Ok(())
}

pub fn decode_one<'b, T: IntoIterator<Item=u8>>(decoder: &InstDecoder, bytes: T, instr: &'b mut Instruction) -> Option<()> {
    instr.operands = [
        OperandSpec::Nothing,
        OperandSpec::Nothing,
        OperandSpec::Nothing,
        OperandSpec::Nothing,
    ];
    let mut bytes_iter = bytes.into_iter();
    read_instr(decoder, bytes_iter, instr).ok()
}
/*
    match read_opcode(&mut bytes_iter, instr) {
        Some(operand_code) => {
            match read_operands(&mut bytes_iter, instr, operand_code) {
                Ok(()) => {
                    Some(())
                },
                Err(_reason) => {
//                    panic!("Decode error on operand: {:?}", reason);
       //             println!("Invalid instruction: {}", reason);
//                        return Instruction::invalid()
                    None
                }
            }
        }
//        Err(_reason) => {
        None => {
//            panic!("Decode error on opcode: {:?}", reason);
        //    println!("Invalid instruction: {}", reason);
//                return Instruction::invalid()
            None
        }
    }
}
*/

#[inline]
fn read_num<T: Iterator<Item=u8>>(bytes: &mut T, width: u8) -> Result<u64, ()> {
    let mut result = 0u64;
    let mut idx = 0;
    loop {
        if idx == width {
            return Ok(result);
        }
        if let Some(byte) = bytes.next() {
            result |= (byte as u64) << (idx * 8);
            idx += 1;
        } else {
            return Err(());
        }
    }
}

#[inline]
fn read_imm_ivq<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<u64, ()> {
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
fn read_imm_signed<T: Iterator<Item=u8>>(bytes: &mut T, num_width: u8, length: &mut u8) -> Result<i64, ()> {
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
fn read_imm_unsigned<T: Iterator<Item=u8>>(bytes: &mut T, width: u8) -> Result<u64, ()> {
    read_num(bytes, width)
}

#[inline]
fn octets_of(byte: u8) -> (u8, u8, u8) {
    (byte >> 6 & 0b11, (byte >> 3) & 0b111, byte & 0b111)
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
fn read_modrm<T: Iterator<Item=u8>>(bytes_iter: &mut T, inst: &mut Instruction, length: &mut u8) -> Result<u8, ()> {
    bytes_iter.next().map(|b| { *length += 1; b }).ok_or(())
}
