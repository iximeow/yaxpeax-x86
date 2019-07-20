#[cfg(feature="use-serde")]
#[macro_use] extern crate serde_derive;
#[cfg(feature="use-serde")]
extern crate serde;
#[cfg(feature="use-serde")]
use serde::{Serialize, Deserialize};

extern crate yaxpeax_arch;
extern crate termion;

mod display;

use std::hint::unreachable_unchecked;

use yaxpeax_arch::{Arch, Decodable, LengthedInstruction};

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
    pub fn RIP() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::RIP
        }
    }

    #[inline]
    pub fn EIP() -> RegSpec {
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
            bank: RegisterBank::RFlags
        }
    }

    #[inline]
    pub fn rsp() -> RegSpec {
        RegSpec {
            num: 4,
            bank: RegisterBank::RFlags
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
}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
enum SizeCode {
    b,
    vd,
    vq,
    vqp
}

#[derive(Clone, Debug)]
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
    // Many(Vec<Operand>),
    Nothing
}

impl Operand {
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
            //Operand::Many(_) |
            Operand::Nothing => {
                false
            }
        }
    }
}

#[cfg(feature="use-serde")]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP, EFlags, RFlags,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
}
#[allow(non_camel_case_types)]
#[cfg(not(feature="use-serde"))]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP, EFlags, RFlags,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum Segment {
    CS, DS, ES, FS, GS, SS
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    XADD,
    BT,
    BTS,
    BTC,
    BTR,
    BSF,
    BSR,
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
    MOVDDUP,
    HADDPS,
    HSUBPS,
    ADDSUBPS,
    CVTSI2SS,
    CVTSI2SD,
    CVTTSD2SI,
    CVTSD2SI,
    CVTSD2SS,
    CVTTSS2SI,
    CVTSS2SI,
    CVTSS2SD,
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
    SBB,
    AND,
    XOR,
    OR,
    PUSH,
    POP,
    LEA,
    NOP,
    XCHG,
    POPF,
    ADD,
    ADC,
    SUB,
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
    CDW,
    LODS,
    STOS,
    LAHF,
    SAHF,
    CMPS,
    SCAS,
    MOVS,
    TEST,
    CMP,
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
    XSTOR,
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
    Invalid
}
#[derive(Debug)]
pub struct Instruction {
    pub prefixes: Prefixes,
    pub opcode: Opcode,
    pub operands: [Operand; 2],
    pub length: u8
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

impl Decodable for Instruction {
    fn decode<T: IntoIterator<Item=u8>>(bytes: T) -> Option<Self> {
        let mut instr = Instruction::invalid();
        match decode_one(bytes, &mut instr) {
            Some(_) => Some(instr),
            None => None
        }
    }
    fn decode_into<T: IntoIterator<Item=u8>>(&mut self, bytes: T) -> Option<()> {
        decode_one(bytes, self)
    }
}

impl Instruction {
    pub fn invalid() -> Instruction {
        Instruction {
            prefixes: Prefixes::new(0),
            opcode: Opcode::Invalid,
            operands: [Operand::Nothing, Operand::Nothing],
            length: 0
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

#[derive(Debug)]
pub struct Prefixes {
    bits: u8,
    rep_prefix: Option<RepPrefix>,
    rex: PrefixRex,
    segment: Segment,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RepPrefix {
    E,
    NE
}

#[derive(Debug)]
pub struct PrefixRex {
    bits: u8
}

#[allow(dead_code)]
impl Prefixes {
    fn new(bits: u8) -> Prefixes {
        Prefixes {
            bits: bits,
            rep_prefix: None,
            rex: PrefixRex { bits: 0 },
            segment: Segment::DS,
        }
    }
    #[inline]
    fn rep(&self) -> bool { self.bits & 0x30 == 0x10 }
    #[inline]
    fn set_rep(&mut self) { self.bits = (self.bits & 0xcf) | 0x10 }
    #[inline]
    fn repz(&self) -> bool { self.bits & 0x30 == 0x20 }
    #[inline]
    fn set_repz(&mut self) { self.bits = (self.bits & 0xcf) | 0x20 }
    #[inline]
    fn repnz(&self) -> bool { self.bits & 0x30 == 0x30 }
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
    pub fn repne(&self) -> bool { self.rep_prefix == Some(RepPrefix::NE) }
    #[inline]
    fn set_repne(&mut self) { self.rep_prefix = Some(RepPrefix::NE); }
    #[inline]
    pub fn repe(&self) -> bool { self.rep_prefix == Some(RepPrefix::E) }
    #[inline]
    fn set_repe(&mut self) { self.rep_prefix = Some(RepPrefix::E); }
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
    fn rex_mut(&mut self) -> &mut PrefixRex { &mut self.rex }
}

impl PrefixRex {
    #[inline]
    fn present(&self) -> bool { (self.bits & 0x10) == 0x10 }
    #[inline]
    fn set_present(&mut self) { self.bits |= 0x10; }
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
        self.bits = prefix & 0x0f;
        self.set_present();
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum OperandCode {
    ModRM_0x0f00,
    ModRM_0x0f01,
    ModRM_0x0fae,
    ModRM_0x0fba,
    Rq_Cq_0,
    Rq_Dq_0,
    Cq_Rq_0,
    Dq_Rq_0,
    FS,
    GS,
    Eb_R0,
    ModRM_0xf6,
    ModRM_0xf7,
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
    ModRM_0xc0_Eb_Ib,
    ModRM_0xd0_Eb_1,
    ModRM_0xd2_Eb_CL,
    ModRM_0xfe_Eb,
    ModRM_0x8f_Ev,
    ModRM_0xc1_Ev_Ib,
    ModRM_0xd1_Ev_1,
    ModRM_0xd3_Ev_CL,
    ModRM_0xff_Ev,
    ModRM_0x80_Eb_Ib,
    ModRM_0x81_Ev_Ivs,
    ModRM_0x83_Ev_Ibs,
    ModRM_0xc6_Eb_Ib,
    ModRM_0xc7_Ev_Iv,
    Eb_Gb,
    Ev_Gv,
    E_G_xmm,
    Ev_Ivs,
    Ev,
    Ew_Sw,
    Fw,
    Gb_Eb,
    Gv_Eb,
    Gv_Ew,
    Gv_Ed,
    Gv_Ev,
    G_E_xmm,
    Gv_M,
    I_3,
    Ib,
    Ibs,
    Ivs,
    Iw,
    Iw_Ib,
    Jvds,
    Jbs,
    Ob_AL,
    Ov_AX,
    Sw_Ew,
    Yb_AL,
    Yb_Xb,
    Yv_AX,
    Yv_Xv,
    Zb_Ib(u8),
    Zv(u8),
    Zv_AX(u8),
    Zv_Ivq(u8),
    Nothing,
    Implied
}

const BASE_OPCODE_MAP: [Opcode; 8] = [
    Opcode::ADD,
    Opcode::OR,
    Opcode::ADC,
    Opcode::SBB,
    Opcode::AND,
    Opcode::SUB,
    Opcode::XOR,
    Opcode::CMP
];

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
fn read_opcode_660f_map<T: Iterator<Item=u8>>(_bytes_iter: &mut T, _instruction: &mut Instruction, _length: &mut u8) -> Result<OperandCode, String> {
    Err("660f opcode map unsupported".to_string())
}
fn read_opcode_f20f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction, length: &mut u8) -> Result<OperandCode, String> {
    match bytes_iter.next() {
        Some(b) => {
            *length += 1;
            match b {
                0x10 => {
                    instruction.opcode = Opcode::MOVSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x11 => {
                    instruction.opcode = Opcode::MOVSD;
                    Ok(OperandCode::E_G_xmm)
                },
                0x12 => {
                    instruction.opcode = Opcode::MOVDDUP;
                    Ok(OperandCode::G_E_xmm)
                },
                0x2a => {
                    instruction.opcode = Opcode::CVTSI2SD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x2c => {
                    instruction.opcode = Opcode::CVTTSD2SI;
                    Ok(OperandCode::G_E_xmm)
                },
                0x2d => {
                    instruction.opcode = Opcode::CVTSD2SI;
                    Ok(OperandCode::G_E_xmm)
                },
                0x38 => {
                    /*
                     * There are a handful of instruction variants here, but
                     * in the f20f opcode map, only the CRC32 instruction is valid
                     */
                    Err("x86_64 CRC32 not currently supported".to_string())
                }
                0x51 => {
                    instruction.opcode = Opcode::SQRTSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x58 => {
                    instruction.opcode = Opcode::ADDSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x59 => {
                    instruction.opcode = Opcode::MULSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5a => {
                    instruction.opcode = Opcode::CVTSD2SS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5c => {
                    instruction.opcode = Opcode::SUBSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5d => {
                    instruction.opcode = Opcode::MINSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5e => {
                    instruction.opcode = Opcode::DIVSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5f => {
                    instruction.opcode = Opcode::MAXSD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x7c => {
                    instruction.opcode = Opcode::HADDPS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x7d => {
                    instruction.opcode = Opcode::HSUBPS;
                    Ok(OperandCode::G_E_xmm)
                },
                0xD0 => {
                    instruction.opcode = Opcode::ADDSUBPS;
                    Ok(OperandCode::G_E_xmm)
                },
                0xf0 => {
                    instruction.opcode = Opcode::LDDQU;
                    Ok(OperandCode::G_E_xmm)
                },
                /*
                0x70 PSHUFLW
                0xc2 CMPSD
                0xd6 MOVDQ2Q
                0xe6 CVTPD2DQ
                */
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    Err("Invalid opcode".to_string())
                }
            }
        }
        None => {
            Err("No more bytes".to_owned())
        }
    }
}
fn read_opcode_f30f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction, length: &mut u8) -> Result<OperandCode, String> {
    match bytes_iter.next() {
        Some(b) => {
            *length += 1;
            match b {
                0x10 => {
                    instruction.opcode = Opcode::MOVSS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x11 => {
                    instruction.opcode = Opcode::MOVSS;
                    Ok(OperandCode::E_G_xmm)
                },
                0x12 => {
                    instruction.opcode = Opcode::MOVSLDUP;
                    Ok(OperandCode::G_E_xmm)
                },
                0x2a => {
                    instruction.opcode = Opcode::CVTSI2SS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x2c => {
                    instruction.opcode = Opcode::CVTTSS2SI;
                    Ok(OperandCode::G_E_xmm)
                },
                0x2d => {
                    instruction.opcode = Opcode::CVTSS2SI;
                    Ok(OperandCode::G_E_xmm)
                },
                0x51 => {
                    instruction.opcode = Opcode::SQRTSS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x58 => {
                    instruction.opcode = Opcode::ADDSS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x59 => {
                    instruction.opcode = Opcode::MULSS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5a => {
                    instruction.opcode = Opcode::CVTSS2SD;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5c => {
                    instruction.opcode = Opcode::SUBSS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5d => {
                    instruction.opcode = Opcode::MINSS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5e => {
                    instruction.opcode = Opcode::DIVSS;
                    Ok(OperandCode::G_E_xmm)
                },
                0x5f => {
                    instruction.opcode = Opcode::MAXSS;
                    Ok(OperandCode::G_E_xmm)
                },
                _ => {
                    instruction.opcode = Opcode::Invalid;
                    Err("Invalid opcode".to_string())
                }
            }
        }
        None => {
            Err("No more bytes".to_owned())
        }
    }
}
fn read_opcode_0f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction, length: &mut u8) -> Result<OperandCode, String> {
    match bytes_iter.next() {
        Some(b) => {
            *length += 1;
            match b {
                0x00 => {
                    Ok(OperandCode::ModRM_0x0f00)
                }
                0x01 => {
                    Ok(OperandCode::ModRM_0x0f01)
                }
                0x02 => {
                    instruction.opcode = Opcode::LAR;
                    Ok(OperandCode::Gv_M)
                }
                0x03 => {
                    instruction.opcode = Opcode::LSL;
                    Ok(OperandCode::Gv_M)
                }
                0x05 => {
                    instruction.opcode = Opcode::SYSCALL;
                    Ok(OperandCode::Nothing)
                }
                0x06 => {
                    instruction.opcode = Opcode::CLTS;
                    Ok(OperandCode::Nothing)
                }
                0x07 => {
                    instruction.opcode = Opcode::SYSRET;
                    Ok(OperandCode::Nothing)
                }
                0x08 => {
                    instruction.opcode = Opcode::INVD;
                    Ok(OperandCode::Nothing)
                }
                0x09 => {
                    instruction.opcode = Opcode::WBINVD;
                    Ok(OperandCode::Nothing)
                }
                0x0b => {
                    instruction.opcode = Opcode::UD2;
                    Ok(OperandCode::Nothing)
                }
                0x0d => {
                    instruction.opcode = Opcode::NOP;
                    Ok(OperandCode::Ev)
                }
                0x1f => {
                    instruction.opcode = Opcode::NOP;
                    Ok(OperandCode::Ev)
                },
                0x20 => {
                    instruction.opcode = Opcode::MOV;
                    Ok(OperandCode::Rq_Cq_0)
                },
                0x21 => {
                    instruction.opcode = Opcode::MOV;
                    Ok(OperandCode::Rq_Dq_0)
                },
                0x22 => {
                    instruction.opcode = Opcode::MOV;
                    Ok(OperandCode::Cq_Rq_0)
                },
                0x23 => {
                    instruction.opcode = Opcode::MOV;
                    Ok(OperandCode::Dq_Rq_0)
                },
                0x30 => {
                    instruction.opcode = Opcode::WRMSR;
                    Ok(OperandCode::Nothing)
                },
                0x31 => {
                    instruction.opcode = Opcode::RDTSC;
                    Ok(OperandCode::Nothing)
                },
                0x32 => {
                    instruction.opcode = Opcode::RDMSR;
                    Ok(OperandCode::Nothing)
                },
                0x33 => {
                    instruction.opcode = Opcode::RDPMC;
                    Ok(OperandCode::Nothing)
                },
                0x40 => {
                    instruction.opcode = Opcode::CMOVO;
                    Ok(OperandCode::Gv_Ev)
                },
                0x41 => {
                    instruction.opcode = Opcode::CMOVNO;
                    Ok(OperandCode::Gv_Ev)
                },
                0x42 => {
                    instruction.opcode = Opcode::CMOVB;
                    Ok(OperandCode::Gv_Ev)
                },
                0x43 => {
                    instruction.opcode = Opcode::CMOVNB;
                    Ok(OperandCode::Gv_Ev)
                },
                0x44 => {
                    instruction.opcode = Opcode::CMOVZ;
                    Ok(OperandCode::Gv_Ev)
                },
                0x45 => {
                    instruction.opcode = Opcode::CMOVNZ;
                    Ok(OperandCode::Gv_Ev)
                },
                0x46 => {
                    instruction.opcode = Opcode::CMOVNA;
                    Ok(OperandCode::Gv_Ev)
                },
                0x47 => {
                    instruction.opcode = Opcode::CMOVA;
                    Ok(OperandCode::Gv_Ev)
                },
                0x48 => {
                    instruction.opcode = Opcode::CMOVS;
                    Ok(OperandCode::Gv_Ev)
                },
                0x49 => {
                    instruction.opcode = Opcode::CMOVNS;
                   Ok(OperandCode::Gv_Ev)
                },
                0x4a => {
                    instruction.opcode = Opcode::CMOVP;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4b => {
                    instruction.opcode = Opcode::CMOVNP;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4c => {
                    instruction.opcode = Opcode::CMOVL;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4d => {
                    instruction.opcode = Opcode::CMOVGE;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4e => {
                    instruction.opcode = Opcode::CMOVLE;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4f => {
                    instruction.opcode = Opcode::CMOVG;
                    Ok(OperandCode::Gv_Ev)
                },
                0x80 => {
                    instruction.opcode = Opcode::JO;
                    Ok(OperandCode::Jvds)
                },
                0x81 => {
                    instruction.opcode = Opcode::JNO;
                    Ok(OperandCode::Jvds)
                },
                0x82 => {
                   instruction.opcode = Opcode::JB;
                    Ok(OperandCode::Jvds)
                },
                0x83 => {
                    instruction.opcode = Opcode::JNB;
                    Ok(OperandCode::Jvds)
                },
                0x84 => {
                    instruction.opcode = Opcode::JZ;
                    Ok(OperandCode::Jvds)
                },
                0x85 => {
                    instruction.opcode = Opcode::JNZ;
                    Ok(OperandCode::Jvds)
                },
                0x86 => {
                    instruction.opcode = Opcode::JNA;
                    Ok(OperandCode::Jvds)
                },
                0x87 => {
                    instruction.opcode = Opcode::JA;
                    Ok(OperandCode::Jvds)
                },
                0x88 => {
                    instruction.opcode = Opcode::JS;
                    Ok(OperandCode::Jvds)
                },
                0x89 => {
                    instruction.opcode = Opcode::JNS;
                    Ok(OperandCode::Jvds)
                },
                0x8a => {
                    instruction.opcode = Opcode::JP;
                    Ok(OperandCode::Jvds)
                },
                0x8b => {
                    instruction.opcode = Opcode::JNP;
                    Ok(OperandCode::Jvds)
                },
                0x8c => {
                    instruction.opcode = Opcode::JL;
                    Ok(OperandCode::Jvds)
                },
                0x8d => {
                    instruction.opcode = Opcode::JGE;
                    Ok(OperandCode::Jvds)
                },
                0x8e => {
                    instruction.opcode = Opcode::JLE;
                    Ok(OperandCode::Jvds)
                },
                0x8f => {
                    instruction.opcode = Opcode::JG;
                    Ok(OperandCode::Jvds)
                },
                x if x < 0xa0 => {
                    instruction.opcode = [
                        Opcode::SETO,
                        Opcode::SETNO,
                        Opcode::SETB,
                        Opcode::SETAE,
                        Opcode::SETZ,
                        Opcode::SETNZ,
                        Opcode::SETBE,
                        Opcode::SETA,
                        Opcode::SETS,
                        Opcode::SETNS,
                        Opcode::SETP,
                        Opcode::SETNP,
                        Opcode::SETL,
                        Opcode::SETGE,
                        Opcode::SETLE,
                        Opcode::SETG
                    ][(x & 0xf) as usize];
                    Ok(OperandCode::Eb_R0)
                }
                0xa0 => {
                    instruction.opcode = Opcode::PUSH;
                    Ok(OperandCode::FS)
                }
                0xa1 => {
                    instruction.opcode = Opcode::POP;
                    Ok(OperandCode::GS)
                }
                0xa2 => {
                    instruction.opcode = Opcode::CPUID;
                    Ok(OperandCode::Nothing)
                }
                0xa3 => {
                    instruction.opcode = Opcode::BT;
                    Ok(OperandCode::Gv_Ev)
                }
                0xa8 => {
                    instruction.opcode = Opcode::PUSH;
                    Ok(OperandCode::Nothing)
                }
                0xa9 => {
                    instruction.opcode = Opcode::PUSH;
                    Ok(OperandCode::GS)
                }
                0xae => {
                    Ok(OperandCode::ModRM_0x0fae)
                }
                0xaf => {
                    instruction.opcode = Opcode::IMUL;
                    Ok(OperandCode::Gv_Ev)
                }
                0xb0 => {
                    instruction.opcode = Opcode::CMPXCHG;
                    Ok(OperandCode::Eb_Gb)
                },
                0xb1 => {
                    instruction.opcode = Opcode::CMPXCHG;
                    Ok(OperandCode::Ev_Gv)
                }
                0xb6 => {
                    instruction.opcode = Opcode::MOVZX_b;
                    Ok(OperandCode::Gv_Eb)
                },
                0xb7 => {
                    instruction.opcode = Opcode::MOVZX_w;
                    Ok(OperandCode::Gv_Ew)
                }
                0xba => {
                    Ok(OperandCode::ModRM_0x0fba)
                }
                0xbb => {
                    instruction.opcode = Opcode::BTC;
                    Ok(OperandCode::Gv_Ev)
                }
                0xbc => {
                    instruction.opcode = Opcode::BSF;
                    Ok(OperandCode::Gv_Ev)
                }
                0xbd => {
                    instruction.opcode = Opcode::BSR;
                    Ok(OperandCode::Gv_Ev)
                }
                0xbe => {
                    instruction.opcode = Opcode::MOVSX_b;
                    Ok(OperandCode::Gv_Eb)
                }
                0xbf => {
                    instruction.opcode = Opcode::MOVSX_w;
                    Ok(OperandCode::Gv_Ew)
                }
                0xc0 => {
                    instruction.opcode = Opcode::XADD;
                    Ok(OperandCode::Eb_Gb)
                }
                0xc1 => {
                    instruction.opcode = Opcode::XADD;
                    Ok(OperandCode::Ev_Gv)
                }
                _ => {
                    Err(format!("Unknown opcode: 0f{:x}", b))
                }
            }
        },
        None => {
            Err("No more bytes".to_owned())
        }
    }
}

fn read_opcode<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction, length: &mut u8) -> Result<OperandCode, String> {
    let mut alternate_opcode_map: Option<OpcodeMap> = None;
    use std::hint::unreachable_unchecked;
//    use std::intrinsics::unlikely;
    instruction.prefixes = Prefixes::new(0);
    let (opcode, operand_code) = loop {
        match bytes_iter.next() {
            Some(b) => {
                *length += 1;
                match b {
                    x if x < 0x40 => {
                        if x % 8 > 5 {
                            // for x86_32 this is push/pop prefixes and 0x0f escape
                            // for x86_64 this is mostly invalid
                            match x {
                                0x0f => {
                                    return match alternate_opcode_map {
                                        Some(OpcodeMap::Map66) => {
                                            read_opcode_660f_map(bytes_iter, instruction, length)
                                        },
                                        Some(OpcodeMap::MapF2) => {
                                            read_opcode_f20f_map(bytes_iter, instruction, length)
                                        },
                                        Some(OpcodeMap::MapF3) => {
                                            read_opcode_f30f_map(bytes_iter, instruction, length)
                                        },
                                        None => {
                                            read_opcode_0f_map(bytes_iter, instruction, length)
                                        }
                                    };
                                },
                                0x26 => {
                                    instruction.prefixes.set_es();
                                    alternate_opcode_map = None;
                                },
                                0x2e => {
                                    instruction.prefixes.set_cs();
                                    alternate_opcode_map = None;
                                },
                                0x36 => {
                                    instruction.prefixes.set_ss();
                                    alternate_opcode_map = None;
                                },
                                0x3e => {
                                    instruction.prefixes.set_ds();
                                    alternate_opcode_map = None;
                                },
                                0x06
                                | 0x07
                                | 0x0e
                                | 0x16
                                | 0x17
                                | 0x1e
                                | 0x1f
                                | 0x27
                                | 0x2f
                                | 0x37
                                | 0x3f => { break (Opcode::Invalid, OperandCode::Nothing); },
                                _ => { unsafe { unreachable_unchecked(); } }
                            }
                            continue;
                        }
                        let op = BASE_OPCODE_MAP[(x / 8) as usize].clone();
                        let operand_code = [
                            OperandCode::Eb_Gb,
                            OperandCode::Ev_Gv,
                            OperandCode::Gb_Eb,
                            OperandCode::Gv_Ev,
                            OperandCode::AL_Ib,
                            OperandCode::AX_Ivd
                        ][(x % 8) as usize];

                        break (op, operand_code);
                    },
                    x if x < 0x50 => {
                        // x86_32 inc/dec
                        // x86_64 rex
                        instruction.prefixes.rex_mut().from(x);
                    },
                    x if x < 0x60 => {
                        let op = if x < 0x58 { Opcode::PUSH } else { Opcode::POP };
                        break (op, OperandCode::Zv(x));
                    },
                    0x63 => { break (Opcode::MOVSXD, OperandCode::Gv_Ed); }
                    0x64 => {
                        instruction.prefixes.set_fs();
                        alternate_opcode_map = None;
                    },
                    0x65 => {
                        instruction.prefixes.set_gs();
                        alternate_opcode_map = None;
                    },
                    0x66 => {
                        instruction.prefixes.set_operand_size();
                        alternate_opcode_map = Some(OpcodeMap::Map66);
                    },
                    0x67 => {
                        instruction.prefixes.set_address_size();
                        alternate_opcode_map = None;
                    },
                    0x68 => { break (Opcode::PUSH, OperandCode::Ivs); },
                    0x69 => { break (Opcode::IMUL, OperandCode::Gv_Ev_Iv); },
                    0x6a => { break (Opcode::PUSH, OperandCode::Ibs); },
                    0x6b => { break (Opcode::IMUL, OperandCode::Gb_Eb_Ib); },
                    0x6c => { break (Opcode::INS, OperandCode::Yb_DX); },
                    0x6d => { break (Opcode::INS, OperandCode::Yv_DX); },
                    0x6e => { break (Opcode::OUTS, OperandCode::DX_Xb); },
                    0x6f => { break (Opcode::OUTS, OperandCode::DX_Xv); },
                    x if x < 0x80 => {
                        let op = [
                            Opcode::JO,
                            Opcode::JNO,
                            Opcode::JB,
                            Opcode::JNB,
                            Opcode::JZ,
                            Opcode::JNZ,
                            Opcode::JNA,
                            Opcode::JA,
                            Opcode::JS,
                            Opcode::JNS,
                            Opcode::JP,
                            Opcode::JNP,
                            Opcode::JL,
                            Opcode::JGE,
                            Opcode::JLE,
                            Opcode::JG
                        ][(x & 0xf) as usize].clone();
                        instruction.opcode = op;
                        return Ok(OperandCode::Jbs);
                    }
                    x if x < 0x84 => {
                        // oh right op depends on modrm....
                        if x == 0x82 {
                            return Err("invalid opcode".to_owned());
                        } else {
                            let op = Opcode::Invalid;
                            let operand = if x == 0x80 {
                                OperandCode::ModRM_0x80_Eb_Ib
                            } else if x == 0x81 {
                                OperandCode::ModRM_0x81_Ev_Ivs
                            } else {
                                OperandCode::ModRM_0x83_Ev_Ibs
                            };
                            instruction.opcode = op;
                            return Ok(operand);
                        }
                    },
                    0x84 => { break (Opcode::TEST, OperandCode::Eb_Gb); },
                    0x85 => { break (Opcode::TEST, OperandCode::Ev_Gv); },
                    0x86 => { break (Opcode::XCHG, OperandCode::Gb_Eb); },
                    0x87 => { break (Opcode::XCHG, OperandCode::Gv_Ev); },
                    0x88 => { break (Opcode::MOV, OperandCode::Eb_Gb); },
                    0x89 => { break (Opcode::MOV, OperandCode::Ev_Gv); },
                    0x8a => { break (Opcode::MOV, OperandCode::Gb_Eb); },
                    0x8b => { break (Opcode::MOV, OperandCode::Gv_Ev); },
                    0x8c => { break (Opcode::MOV, OperandCode::Ew_Sw); },
                    0x8d => { break (Opcode::LEA, OperandCode::Gv_M); },
                    0x8e => { break (Opcode::MOV, OperandCode::Sw_Ew); },
                    0x8f => { break (Opcode::Invalid, OperandCode::ModRM_0x8f_Ev); },
                    0x90 => { break (Opcode::NOP, OperandCode::Nothing); },
                    x if x < 0x98 => { break (Opcode::XCHG, OperandCode::Zv_AX(x)); },
                    0x98 => { break (Opcode::CBW, OperandCode::AX_AL); },
                    0x99 => { break (Opcode::CBW, OperandCode::DX_AX); },
                    0x9a => { return Err("invalid opcode".to_owned()); },
                    0x9b => { break (Opcode::WAIT, OperandCode::Nothing); },
                    0x9c => { break (Opcode::PUSHF, OperandCode::Nothing); },
                    0x9d => { break (Opcode::POPF, OperandCode::Nothing); },
                    0x9e => { break (Opcode::SAHF, OperandCode::AH); },
                    0x9f => { break (Opcode::LAHF, OperandCode::AH); },
                    0xa0 => { break (Opcode::MOV, OperandCode::AL_Ob); },
                    0xa1 => { break (Opcode::MOV, OperandCode::AX_Ov); },
                    0xa2 => { break (Opcode::MOV, OperandCode::Ob_AL); },
                    0xa3 => { break (Opcode::MOV, OperandCode::Ov_AX); },
                    0xa4 => { break (Opcode::MOVS, OperandCode::Yb_Xb); },
                    0xa5 => { break (Opcode::MOVS, OperandCode::Yv_Xv); },
                    0xa6 => { break (Opcode::CMPS, OperandCode::Yb_Xb); },
                    0xa7 => { break (Opcode::CMPS, OperandCode::Yv_Xv); },
                    0xa8 => { break (Opcode::TEST, OperandCode::AL_Ib); },
                    0xa9 => { break (Opcode::TEST, OperandCode::AX_Ivd); },
                    0xaa => { break (Opcode::STOS, OperandCode::Yb_AL); },
                    0xab => { break (Opcode::STOS, OperandCode::Yv_AX); },
                    0xac => { break (Opcode::LODS, OperandCode::AL_Xb); },
                    0xad => { break (Opcode::LODS, OperandCode::AX_Xv); },
                    0xae => { break (Opcode::SCAS, OperandCode::Yb_AL); },
                    0xaf => { break (Opcode::SCAS, OperandCode::Yv_AX); },
                    x if x < 0xc0 => {
                        let operand = if x < 0xb8 {
                            OperandCode::Zb_Ib(x)
                        } else {
                            OperandCode::Zv_Ivq(x)
                        };
                        instruction.opcode = Opcode::MOV;
                        return Ok(operand);
                    },
                    0xc0 => { break (Opcode::Invalid, OperandCode::ModRM_0xc0_Eb_Ib); },
                    0xc1 => { break (Opcode::Invalid, OperandCode::ModRM_0xc1_Ev_Ib); },
                    0xc2 => { break (Opcode::RETURN, OperandCode::Iw); },
                    0xc3 => { break (Opcode::RETURN, OperandCode::Nothing); },
                    0xc4  | 0xc5 => {
                        return Err("invalid opcode".to_owned());
                    },
                    0xc6 => { break (Opcode::MOV, OperandCode::ModRM_0xc6_Eb_Ib); },
                    0xc7 => { break (Opcode::MOV, OperandCode::ModRM_0xc7_Ev_Iv); },
                    0xc8 => { break (Opcode::ENTER, OperandCode::Iw_Ib); },
                    0xc9 => { break (Opcode::LEAVE, OperandCode::Nothing); },
                    0xca => { break (Opcode::RETF, OperandCode::Iw); },
                    0xcb => { break (Opcode::RETF, OperandCode::Nothing); },
                    0xcc => { break (Opcode::INT, OperandCode::I_3); },
                    0xcd => { break (Opcode::INT, OperandCode::Ib); },
                    0xce => { break (Opcode::INTO, OperandCode::Fw); },
                    0xcf => { break (Opcode::IRET, OperandCode::Fw); },
                    x if x < 0xd4 => {
                        let operand = [
                            OperandCode::ModRM_0xd0_Eb_1,
                            OperandCode::ModRM_0xd1_Ev_1,
                            OperandCode::ModRM_0xd2_Eb_CL,
                            OperandCode::ModRM_0xd3_Ev_CL
                        ][(x & 0x3) as usize].clone();
                        instruction.opcode = Opcode::Invalid;
                        return Ok(operand);
                    },
                    0xdc => {
                        // TODO: WRONG
                        // x87 instructions
                        instruction.opcode = Opcode::NOP;
                        let _ = read_imm_unsigned(bytes_iter, 2, length)?;
                        return Ok(OperandCode::Nothing);
                    }
                    0xdd => {
                        // x87 instructions
                        // TODO: WRONG
                        instruction.opcode = Opcode::NOP;
                        let _ = read_imm_unsigned(bytes_iter, 2, length)?;
                        return Ok(OperandCode::Nothing);
                    }
                    // TODO: GAP
                    0xe8 => { break (Opcode::CALL, OperandCode::Jvds); },
                    0xe9 => { break (Opcode::JMP, OperandCode::Jvds); },
                    0xeb => { break (Opcode::JMP, OperandCode::Jbs); },
                    0xf0 => {
                        instruction.prefixes.set_lock();
                    },
                    0xf2 => {
                        instruction.prefixes.set_repnz();
                        alternate_opcode_map = Some(OpcodeMap::MapF2);
                    },
                    0xf3 => {
                        instruction.prefixes.set_rep();
                        alternate_opcode_map = Some(OpcodeMap::MapF3);
                    },
                    0xf4 => { break (Opcode::HLT, OperandCode::Nothing); },
                    0xf6 => { break (Opcode::Invalid, OperandCode::ModRM_0xf6); },
                    0xf7 => { break (Opcode::Invalid, OperandCode::ModRM_0xf7); },
                    0xf8 => { break (Opcode::CLC, OperandCode::Nothing); },
                    0xf9 => { break (Opcode::STC, OperandCode::Nothing); },
                    0xfa => { break (Opcode::CLI, OperandCode::Nothing); },
                    0xfb => { break (Opcode::STI, OperandCode::Nothing); },
                    0xfc => { break (Opcode::CLD, OperandCode::Nothing); },
                    0xfd => { break (Opcode::STD, OperandCode::Nothing); },
                    0xfe => { break (Opcode::Invalid, OperandCode::ModRM_0xfe_Eb); },
                    // TODO: test 0xff /3
                    0xff => { break (Opcode::Invalid, OperandCode::ModRM_0xff_Ev); },
                    _ => {
                        return Err("unsupported opcode".to_owned());
                    }
                }
            },
            None => {
                return Err("no more bytes".to_owned());
            }
        }
    };
    instruction.opcode = opcode;
    Ok(operand_code)
}

#[allow(non_snake_case)]
fn read_E<T: Iterator<Item=u8>>(bytes_iter: &mut T, prefixes: &Prefixes, m: u8, modbits: u8, width: u8, result: &mut Operand, length: &mut u8) -> Result<(), String> {
    read_E_anybank(bytes_iter, prefixes, m, modbits, width, result, length, width_to_gp_reg_bank(width, prefixes.rex().present()))
}
#[allow(non_snake_case)]
fn read_E_xmm<T: Iterator<Item=u8>>(bytes_iter: &mut T, prefixes: &Prefixes, m: u8, modbits: u8, width: u8, result: &mut Operand, length: &mut u8) -> Result<(), String> {
    read_E_anybank(bytes_iter, prefixes, m, modbits, width, result, length, RegisterBank::X)
}

#[allow(non_snake_case)]
fn read_E_anybank<T: Iterator<Item=u8>>(bytes_iter: &mut T, prefixes: &Prefixes, m: u8, modbits: u8, _width: u8, result: &mut Operand, length: &mut u8, reg_bank: RegisterBank) -> Result<(), String> {
    let addr_width = if prefixes.address_size() { 4 } else { 8 };
    if modbits == 0b11 {
        *result = Operand::Register(RegSpec::from_parts(m, prefixes.rex().b(), reg_bank))
    } else if m == 5 && modbits == 0b00 {
        let disp = read_num(bytes_iter, 4, length);
        *result = Operand::RegDisp(
            if addr_width == 8 { RegSpec::RIP() } else { RegSpec::EIP() },
            disp as i32
        );
    } else if m == 4 {
        let sibbyte = match bytes_iter.next() {
            Some(b) => b,
            None => return Err("Out of bytes".to_string())
        };
        *length += 1;
        let (ss, index, base) = octets_of(sibbyte);

//            println!("scale: {:b}, index: {:b}, base: {:b}", ss, index, base);

        if base == 0b101 {
            let disp = if modbits == 0b00 {
                read_num(bytes_iter, 4, length) as i32
            } else if modbits == 0b01 {
                read_num(bytes_iter, 1, length) as i8 as i32
            } else {
                read_num(bytes_iter, 4, length) as i32
            };

            if index == 0b100 {
                if modbits == 0b00 && !prefixes.rex().x() {
                    *result = Operand::DisplacementU32(disp as u32);
                } else {
                    let reg = RegSpec::gp_from_parts(0b100, prefixes.rex().x(), addr_width, prefixes.rex().present());

                    if disp == 0 {
                        *result = Operand::RegDeref(reg);
                    } else {
                        *result = Operand::RegDisp(reg, disp as i32);
                    }
                }
            } else {
                let base_reg = RegSpec::gp_from_parts(5, prefixes.rex().b(), addr_width, prefixes.rex().present());

                let index_reg = RegSpec::gp_from_parts(index, prefixes.rex().x(), addr_width, prefixes.rex().present());

                *result = match (ss, modbits, disp) {
                    (0, 0b00, 0) => {
                        Operand::RegDeref(index_reg)
                    },
                    (0, 0b00, _) => {
                        Operand::RegDisp(index_reg, disp as i32)
                    },
                    (0, _, 0) => {
                        Operand::RegIndexBase(base_reg, index_reg)
                    },
                    (0, _, _) => {
                        Operand::RegIndexBaseDisp(base_reg, index_reg, disp as i32)
                    },
                    (_, 0b00, 0) => {
                        Operand::RegScale(index_reg, 1u8 << ss)
                    },
                    (_, 0b00, _) => {
                        Operand::RegScaleDisp(index_reg, 1u8 << ss, disp as i32)
                    },
                    (_, _, 0) => {
                        Operand::RegIndexBaseScale(base_reg, index_reg, 1u8 << ss)
                    },
                    (_, _, _) => {
                        Operand::RegIndexBaseScaleDisp(base_reg, index_reg, 1u8 << ss, disp as i32)
                    }
                };
            }
        } else {
            let base_reg = RegSpec::gp_from_parts(base, prefixes.rex().b(), addr_width, prefixes.rex().present());

            let disp = if modbits == 0b00 {
                0
            } else if modbits == 0b01 {
                read_num(bytes_iter, 1, length) as i8 as i32
            } else {
                read_num(bytes_iter, 4, length) as i32
            };

            if index == 0b100 {
                if disp == 0 {
                    *result = Operand::RegDeref(base_reg);
                } else {
                    *result = Operand::RegDisp(base_reg, disp as i32);
                }
            } else {
                let index_reg = RegSpec::gp_from_parts(index, prefixes.rex().x(), addr_width, prefixes.rex().present());
                if disp == 0 {
                    if ss == 0 {
                        *result = Operand::RegIndexBase(base_reg, index_reg)
                    } else {
                        *result = Operand::RegIndexBaseScale(base_reg, index_reg, 1u8 << ss);
                    }
                } else {
                    if ss == 0 {

                        *result = Operand::RegIndexBaseDisp(base_reg, index_reg, disp as i32);
                    } else {
                        *result = Operand::RegIndexBaseScaleDisp(base_reg, index_reg, 1u8 << ss, disp as i32);
                    }
                }
            }
        }
    } else {
        let reg = RegSpec::gp_from_parts(m, prefixes.rex().b(), addr_width, prefixes.rex().present());

        if modbits == 0b00 {
            *result = Operand::RegDeref(reg);
        } else {
            let disp = if modbits == 0b01 {
                read_num(bytes_iter, 1, length) as i8 as i32
            } else {
                read_num(bytes_iter, 4, length) as i32
            };
            *result = Operand::RegDisp(reg, disp);
        }
    }
    Ok(())
}

#[inline]
fn read_operands<T: Iterator<Item=u8>>(
    bytes_iter: &mut T,
    instruction: &mut Instruction,
    operand_code: OperandCode,
    length: &mut u8
) -> Result<(), String> {
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
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            if r != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err("Invalid modr/m for opcode 0xc6".to_owned());
            }

            read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)
        },
        OperandCode::AL_Ob => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = 1;
            let imm = read_num(bytes_iter, addr_width, length);
            instruction.operands = [
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present())),
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                }
            ];
            Ok(())
        }
        OperandCode::AX_Ov => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let imm = read_num(bytes_iter, addr_width, length);
            instruction.operands = [
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present())),
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                }
            ];
            Ok(())
        }
        OperandCode::Ob_AL => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = 1;
            let imm = read_num(bytes_iter, addr_width, length);
            instruction.operands = [
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                },
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present()))
            ];
            Ok(())
        }
        OperandCode::Ov_AX => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let imm = read_num(bytes_iter, addr_width, length);
            instruction.operands = [
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                },
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present()))
            ];
            Ok(())
        }
        OperandCode::ModRM_0x80_Eb_Ib => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1, length) as i8;
                    let opcode = BASE_OPCODE_MAP[r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::ImmediateI8(num);
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0x81_Ev_Ivs => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    match read_imm_signed(bytes_iter, if opwidth == 8 { 4 } else { opwidth }, opwidth, length) {
                        Ok(imm) => {
                            let opcode = BASE_OPCODE_MAP[r as usize].clone();
                            instruction.opcode = opcode;
                            instruction.operands[1] = imm;
                            Ok(())
                        },
                        Err(reason) => {
                            instruction.opcode = Opcode::Invalid;
                            Err(reason)
                        }

                    }
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xc0_Eb_Ib => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1, length) as i8;
                    let opcode = BITWISE_OPCODE_MAP[r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::ImmediateI8(num);
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xc1_Ev_Ib => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1, length) as i8;
                    let opcode = BITWISE_OPCODE_MAP[r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::ImmediateI8(num);
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xc6_Eb_Ib => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1, length) as i8;
                    if r != 0 {
                        instruction.opcode = Opcode::Invalid;
                        return Err("Invalid modr/m for opcode 0xc6".to_owned());
                    }
                    instruction.opcode = Opcode::MOV;
                    instruction.operands[1] = Operand::ImmediateI8(num);
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xc7_Ev_Iv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            if r != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err("Invalid modr/m for opcode 0xc7".to_string());
            }

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    match read_imm_unsigned(bytes_iter, opwidth, length) {
                        Ok(imm) => {
                            instruction.opcode = Opcode::MOV;
                            instruction.operands[1] = imm;
                            Ok(())
                        },
                        Err(reason) => Err(reason)
                    }
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xd0_Eb_1 => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let opcode = BITWISE_OPCODE_MAP[r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::ImmediateI8(1);
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xd1_Ev_1 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let opcode = BITWISE_OPCODE_MAP[r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::ImmediateI8(1);
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xf6 => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;
            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => { },
                Err(reason) => { return Err(reason); }
            };
            match r {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    match read_imm_signed(bytes_iter, 1, opwidth, length) {
                        Ok(imm) => {
                            instruction.operands[1] = imm;
                        },
                        Err(reason) => { return Err(reason); }
                    }
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
            Ok(())
        },
        OperandCode::ModRM_0xf7 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;
            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => { },
                Err(reason) => { return Err(reason); }
            };
            match r {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    let numwidth = if opwidth == 8 { 4 } else { opwidth };
                    match read_imm_signed(bytes_iter, numwidth, opwidth, length) {
                        Ok(imm) => {
                            instruction.operands[1] = imm;
                        },
                        Err(reason) => { return Err(reason); }
                    }
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
            Ok(())
        },
        OperandCode::ModRM_0xfe_Eb => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let opcode = [
                        Opcode::INC,
                        Opcode::DEC,
                        Opcode::Invalid,
                        Opcode::Invalid,
                        Opcode::Invalid,
                        Opcode::Invalid,
                        Opcode::Invalid,
                        Opcode::Invalid
                    ][r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::Nothing;
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }
        OperandCode::ModRM_0xff_Ev => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let opcode = [
                        Opcode::INC,
                        Opcode::DEC,
                        Opcode::CALL,
                        Opcode::CALLF,
                        Opcode::JMP,
                        Opcode::JMPF,
                        Opcode::PUSH,
                        Opcode::Invalid
                    ][r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::Nothing;
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }
        OperandCode::Ev => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, _, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    instruction.operands[1] = Operand::Nothing;
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Eb_Gb => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    instruction.operands[1] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Ev_Gv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    instruction.operands[1] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Gb_Eb => {
            let opwidth = 1;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1], length) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Gv_Eb => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1], length) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Gv_Ew => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[1], length) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Ew_Sw => {
            let opwidth = 2;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            if r > 5 {
                return Err("Invalid r".to_owned());
            }

            instruction.operands[1] =
                Operand::Register(RegSpec { bank: RegisterBank::S, num: r });

            if mod_bits == 0b11 {
                instruction.operands[0] =
                    Operand::Register(RegSpec { bank: RegisterBank::W, num: m });
                Ok(())
            } else {
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)
            }
        },
        OperandCode::Sw_Ew => {
            let opwidth = 2;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

            if r > 5 {
                return Err("Invalid r".to_owned());
            }

            instruction.operands[0] =
                Operand::Register(RegSpec { bank: RegisterBank::S, num: r });

            if mod_bits == 0b11 {
                instruction.operands[1] =
                    Operand::Register(RegSpec { bank: RegisterBank::W, num: m });
                Ok(())
            } else {
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1], length)
            }
        },
        // TODO: verify M
        OperandCode::Gv_Ed => {
            let opwidth = 4;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1], length) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Gv_Ev | OperandCode::Gv_M => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1], length) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::E_G_xmm => {
            let opwidth = 8;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            match read_E_xmm(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    instruction.operands[1] =
                        Operand::Register(RegSpec::from_parts(r, instruction.prefixes.rex().r(), RegisterBank::X));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::G_E_xmm => {
            let opwidth = 8;
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            match read_E_xmm(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1], length) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::from_parts(r, instruction.prefixes.rex().r(), RegisterBank::X));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Zv_Ivq(opcode_byte) => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let reg_idx = opcode_byte & 0x7;
            match read_imm_ivq(bytes_iter, opwidth, length) {
                Ok(imm) => {
                    instruction.operands = [
                        Operand::Register(RegSpec::gp_from_parts(reg_idx, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present())),
                        imm
                    ];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::AL_Ib => {
            let opwidth = 1;
            let numwidth = 1;
            match read_imm_signed(bytes_iter, numwidth, opwidth, length) {
                Ok(imm) => {
                    instruction.operands = [
                        Operand::Register(RegSpec::al()),
                        imm
                    ];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }
        OperandCode::AX_Ivd => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            match read_imm_signed(bytes_iter, numwidth, opwidth, length) {
                Ok(imm) => {
                    instruction.operands = [
                        Operand::Register(RegSpec::gp_from_parts(0, false, opwidth, false)),
                        imm
                    ];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }
        OperandCode::Zb_Ib(opcode_byte) => {
            let reg_idx = opcode_byte & 0x7;
            match read_imm_unsigned(bytes_iter, 1, length) {
                Ok(imm) => {
                    instruction.operands = [
                        Operand::Register(RegSpec::gp_from_parts(reg_idx, instruction.prefixes.rex().b(), 1, instruction.prefixes.rex().present())),
                        imm];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Iw => {
            match read_imm_unsigned(bytes_iter, 2, length) {
                Ok(imm) => {
                    instruction.operands = [imm, Operand::Nothing];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }
        OperandCode::Jbs => {
            // TODO: arch width (8 in 64, 4 in 32, 2 in 16)
            match read_imm_signed(bytes_iter, 1, 8, length) {
                Ok(imm) => {
                    instruction.operands = [imm, Operand::Nothing];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Ibs => {
            match read_imm_signed(bytes_iter, 1, 8, length) {
                Ok(imm) => {
                    instruction.operands = [imm, Operand::Nothing];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Ivs => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, &instruction.prefixes);
            match read_imm_unsigned(bytes_iter, opwidth, length) {
                Ok(imm) => {
                    instruction.operands = [imm, Operand::Nothing];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0x83_Ev_Ibs => {
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length) {
                Ok(()) => {
                    let opcode = BASE_OPCODE_MAP[r as usize].clone();

                    instruction.opcode = opcode;
                    match read_imm_signed(bytes_iter, 1, opwidth, length) {
                        Ok(op) => {
                            instruction.operands[1] = op;
                            Ok(())
                        },
                        Err(reason) => Err(reason)
                    }
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Zv(opcode_byte) => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, &instruction.prefixes);
            instruction.operands = [Operand::Register(
                RegSpec::gp_from_parts(
                    opcode_byte & 0b111, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present()
                )
            ), Operand::Nothing];
            Ok(())
        },
        OperandCode::Jvds => {
            let offset = read_num(bytes_iter, 4, length);
            instruction.operands = [Operand::ImmediateI32(offset as i32), Operand::Nothing];
            Ok(())
        }
        OperandCode::ModRM_0x0f00 => {
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;
            if r == 0 {
                instruction.opcode = Opcode::SLDT;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 1 {
                instruction.opcode = Opcode::STR;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 2 {
                instruction.opcode = Opcode::LLDT;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 3 {
                instruction.opcode = Opcode::LTR;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 4 {
                instruction.opcode = Opcode::VERR;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 5 {
                instruction.opcode = Opcode::VERW;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 6 {
                instruction.opcode = Opcode::JMPE;
                instruction.operands = [Operand::Nothing, Operand::Nothing];
                Ok(())
            } else if r == 7 {
                Err("Invalid modr/m bits".to_owned())
            } else {
                unreachable!("r <= 8");
            }
        }
        OperandCode::ModRM_0x0f01 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;
            if r == 0 {
                if mod_bits == 0b11 {
                    panic!("Unsupported instruction: 0x0f01 with modrm: 11 000 ___");
                } else {
                    instruction.opcode = Opcode::SGDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)
                }
            } else if r == 1 {
                if mod_bits == 0b11 {
                    // TOOD: MONITOR
                    instruction.opcode = Opcode::NOP;
                    instruction.operands[0] = Operand::Nothing;
                    Ok(())
                } else {
                    instruction.opcode = Opcode::SIDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)
                }
            } else if r == 2 {
                if mod_bits == 0b11 {
                    // TOOD: XGETBV
                    instruction.opcode = Opcode::NOP;
                    instruction.operands[0] = Operand::Nothing;
                    Ok(())
                } else {
                    instruction.opcode = Opcode::LGDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)
                }
            } else if r == 3 {
                if mod_bits == 0b11 {
                    // TOOD: VMRUN
                    instruction.opcode = Opcode::NOP;
                    instruction.operands[0] = Operand::Nothing;
                    Ok(())
                } else {
                    instruction.opcode = Opcode::LIDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)
                }
            } else if r == 4 {
                // TODO: this permits storing only to word-size registers
                // spec suggets this might do something different for f.ex rdi?
                instruction.opcode = Opcode::SMSW;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 5 {
                panic!("Unsupported instruction: 0x0f01 with modrm: __ 101 ___");
            } else if r == 6 {
                instruction.opcode = Opcode::LMSW;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[0], length)
            } else if r == 7 {
                if mod_bits == 0b11 {
                    if m == 1 {
                        instruction.opcode = Opcode::SWAPGS;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                        Ok(())
                    } else if m == 2 {
                        instruction.opcode = Opcode::RDTSCP;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                        Ok(())
                    } else {
                    //    panic!("Unsupported instruction: 0x0f01 with modrm: 11 110 r >= 2");
                        Err("unsupported 0x0f01 variant".to_string())
                    }
                } else {
                    instruction.opcode = Opcode::INVLPG;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)
                }
            } else {
                unreachable!("r <= 8");
            }
        }
        OperandCode::ModRM_0x0fae => {
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;
            match r {
                0 => {
                    if mod_bits == 0b11 {
                        Err("Invalid mod bits".to_owned())
                    } else {
                        instruction.opcode = Opcode::FXSAVE;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                1 => {
                    if mod_bits == 0b11 {
                        Err("Invalid mod bits".to_owned())
                    } else {
                        instruction.opcode = Opcode::FXRSTOR;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                2 => {
                    if mod_bits == 0b11 {
                        Err("Invalid mod bits".to_owned())
                    } else {
                        instruction.opcode = Opcode::LDMXCSR;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                3 => {
                    if mod_bits == 0b11 {
                        Err("Invalid mod bits".to_owned())
                    } else {
                        instruction.opcode = Opcode::STMXCSR;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                4 => {
                    if mod_bits == 0b11 {
                        Err("Invalid mod bits".to_owned())
                    } else {
                        instruction.opcode = Opcode::XSAVE;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                5 => {
                    if mod_bits == 0b11 {
                        instruction.opcode = Opcode::LFENCE;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                        Ok(())
                    } else {
                        instruction.opcode = Opcode::XSTOR;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                6 => {
                    if mod_bits == 0b11 {
                        instruction.opcode = Opcode::MFENCE;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                        Ok(())
                    } else {
                        // TODO: radare reports this, but i'm not sure?
                        instruction.opcode = Opcode::XSAVEOPT;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                7 => {
                    if mod_bits == 0b11 {
                        instruction.opcode = Opcode::SFENCE;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                        Ok(())
                    } else {
                        // TODO: radare reports this, but i'm not sure?
                        instruction.opcode = Opcode::CLFLUSH;
                        instruction.operands[1] = Operand::Nothing;
                        read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 8, &mut instruction.operands[0], length)
                    }
                }
                _ => { unreachable!("r < 6"); }
            }
        }
        OperandCode::ModRM_0x0fba => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, &instruction.prefixes);
            let (mod_bits, r, m) = read_modrm(bytes_iter, length)?;
            match r {
                0 | 1 | 2 | 3 => {
                    instruction.opcode = Opcode::Invalid;
                    return Err("invalid instruction".to_string());
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

            read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0], length)?;

            match read_imm_signed(bytes_iter, 1, 1, length) {
                Ok(op) => {
                    instruction.operands[1] = op;
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }

        OperandCode::Rq_Cq_0 => {
            let (_, mut r, mut m) = read_modrm(bytes_iter, length)?;
            if instruction.prefixes.rex().r() {
                r += 0b1000;
            }
            if instruction.prefixes.rex().b() {
                m += 0b1000;
            }
            instruction.operands = [
                Operand::Register(RegSpec { bank: RegisterBank::Q, num: m }),
                Operand::Register(RegSpec { bank: RegisterBank::CR, num: r })
            ];
            Ok(())
        }
        OperandCode::Rq_Dq_0 => {
            let (_, mut r, mut m) = read_modrm(bytes_iter, length)?;
            if instruction.prefixes.rex().r() {
                r += 0b1000;
            }
            if instruction.prefixes.rex().b() {
                m += 0b1000;
            }
            instruction.operands = [
                Operand::Register(RegSpec { bank: RegisterBank::Q, num: m }),
                Operand::Register(RegSpec { bank: RegisterBank::DR, num: r })
            ];
            Ok(())
        }
        OperandCode::Cq_Rq_0 => {
            let (_, mut r, mut m) = read_modrm(bytes_iter, length)?;
            if instruction.prefixes.rex().r() {
                r += 0b1000;
            }
            if instruction.prefixes.rex().b() {
                m += 0b1000;
            }
            instruction.operands = [
                Operand::Register(RegSpec { bank: RegisterBank::CR, num: r }),
                Operand::Register(RegSpec { bank: RegisterBank::Q, num: m })
            ];
            Ok(())
        }
        OperandCode::Dq_Rq_0 => {
            let (_, mut r, mut m) = read_modrm(bytes_iter, length)?;
            if instruction.prefixes.rex().r() {
                r += 0b1000;
            }
            if instruction.prefixes.rex().b() {
                m += 0b1000;
            }
            instruction.operands = [
                Operand::Register(RegSpec { bank: RegisterBank::DR, num: r }),
                Operand::Register(RegSpec { bank: RegisterBank::Q, num: m })
            ];
            Ok(())
        }
        OperandCode::FS => {
            instruction.operands = [Operand::Register(RegSpec::fs()), Operand::Nothing];
            Ok(())
        }
        OperandCode::GS => {
            instruction.operands = [Operand::Register(RegSpec::fs()), Operand::Nothing];
            Ok(())
        }
        OperandCode::I_3 => {
            instruction.operands = [Operand::ImmediateU8(3), Operand::Nothing];
            Ok(())
        }
        OperandCode::Nothing => {
            instruction.operands = [Operand::Nothing, Operand::Nothing];
            Ok(())
        }
        _ => {
            instruction.operands = [Operand::Nothing, Operand::Nothing];
            instruction.opcode = Opcode::Invalid;
        //    use std::hint::unreachable_unchecked;
            Err(format!("unsupported operand code: {:?}", operand_code))
        //    unsafe { unreachable_unchecked(); }
        }
    }
}

#[inline]
fn width_to_gp_reg_bank(width: u8, rex: bool) -> RegisterBank {
    use std::hint::unreachable_unchecked;
    match width {
        1 => return if rex { RegisterBank::rB } else { RegisterBank::B },
        2 => return RegisterBank::W,
        4 => return RegisterBank::D,
        8 => return RegisterBank::Q,
        _ => unsafe { unreachable_unchecked(); }
    }
}

pub fn decode_one<'b, T: IntoIterator<Item=u8>>(bytes: T, instr: &'b mut Instruction) -> Option<()> {
    let mut bytes_iter = bytes.into_iter();
    let mut len: u8 = 0;
    match read_opcode(&mut bytes_iter, instr, &mut len) {
        Ok(operand_code) => {
            match read_operands(&mut bytes_iter, instr, operand_code, &mut len) {
                Ok(()) => {
                    instr.length = len;
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
        Err(_reason) => {
//            panic!("Decode error on opcode: {:?}", reason);
        //    println!("Invalid instruction: {}", reason);
//                return Instruction::invalid()
            None
        }
    }
}

#[inline]
fn read_num<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> u64 {
    let mut result = 0u64;
    let mut idx = 0;
    loop {
        if idx == width {
            return result;
        }
        let byte = bytes.next().unwrap();
        *length += 1;
        result |= (byte as u64) << (idx * 8);
        idx += 1;
    }
}

#[inline]
fn read_imm_ivq<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<Operand, String> {
    match width {
        2 => {
            Ok(Operand::ImmediateU16(read_num(bytes, 2, length) as u16))
        },
        4 => {
            Ok(Operand::ImmediateU32(read_num(bytes, 4, length) as u32))
        },
        8 => {
            Ok(Operand::ImmediateU64(read_num(bytes, 8, length) as u64))
        },
        _ => {
            unsafe { unreachable_unchecked(); }
        }
    }
}

#[inline]
fn read_imm_signed<T: Iterator<Item=u8>>(bytes: &mut T, num_width: u8, extend_to: u8, length: &mut u8) -> Result<Operand, String> {
    let num = match num_width {
        1 => read_num(bytes, 1, length) as i8 as i64,
        2 => read_num(bytes, 2, length) as i16 as i64,
        4 => read_num(bytes, 4, length) as i32 as i64,
        8 => read_num(bytes, 4, length) as i32 as i64,
        _ => { unsafe { unreachable_unchecked() } }
    };

    match extend_to {
        1 => Ok(Operand::ImmediateI8(num as i8)),
        2 => Ok(Operand::ImmediateI16(num as i16)),
        4 => Ok(Operand::ImmediateI32(num as i32)),
        8 => Ok(Operand::ImmediateI64(num as i64)),
        _ => { unsafe { unreachable_unchecked() } }
    }
}

#[inline]
fn read_imm_unsigned<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<Operand, String> {
    match width {
        1 => {
            Ok(Operand::ImmediateU8(read_num(bytes, 1, length) as u8))
        },
        2 => {
            Ok(Operand::ImmediateU16(read_num(bytes, 2, length) as u16))
        },
        4 => {
            Ok(Operand::ImmediateU32(read_num(bytes, 4, length) as u32))
        },
        8 => {
            Ok(Operand::ImmediateU64(read_num(bytes, 4, length) as u64))
        }
        _ => {
            unsafe { unreachable_unchecked(); }
        }
    }
}

#[inline]
fn octets_of(byte: u8) -> (u8, u8, u8) {
    (byte >> 6 & 0b11, (byte >> 3) & 0b111, byte & 0b111)
}

#[inline]
fn imm_width_from_prefixes_64(interpretation: SizeCode, prefixes: &Prefixes) -> u8 {
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
fn read_modrm<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<(u8, u8, u8), String> {
    let modrm = match bytes_iter.next() {
        Some(b) => b,
        // TODO: ...
        None => return Err("Out of bytes".to_string()),
    };
    *length += 1;
    Ok(octets_of(modrm))
}
