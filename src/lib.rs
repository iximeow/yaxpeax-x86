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
    DS = 0, CS, ES, FS, GS, SS
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    ADD = 0,
    OR = 1,
    ADC = 2,
    SBB = 3,
    AND = 4,
    XOR = 6,
    SUB = 5,
    CMP = 7,
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
    PUSH,
    POP,
    LEA,
    NOP,
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
    CDW,
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
    Eb_Gb,
    Ev_Gv,
    Gb_Eb,
    Gv_Ev,
    AL_Ib,
    AX_Ivd,
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
    AL_Ob,
    AL_Xb,
    AX_AL,
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
    E_G_xmm,
    Ev_Ivs,
    Ev,
    Ew_Sw,
    Fw,
    Gv_Eb,
    Gv_Ew,
    Gv_Ed,
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
fn read_opcode_660f_map<T: Iterator<Item=u8>>(_bytes_iter: &mut T, _instruction: &mut Instruction) -> Option<OperandCode> {
    panic!("660f opcode map unsupported".to_string());
}

#[derive(Copy, Clone)]
struct OpcodeInstructionRecord(Opcode, OperandCode);

const OPCODE_F20F_MAP: [OpcodeInstructionRecord; 256] = [
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x10
    OpcodeInstructionRecord(Opcode::MOVSD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::MOVSD, OperandCode::E_G_xmm),
    OpcodeInstructionRecord(Opcode::MOVDDUP, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x20
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::CVTSI2SD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::CVTTSD2SI, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::CVTSD2SI, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x30
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x40
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x50
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::SQRTSD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::ADDSD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::MULSD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::CVTSD2SS, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::SUBSD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::MINSD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::DIVSD, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::MAXSD, OperandCode::G_E_xmm),
// 0x60
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x70
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::HADDPS, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::HSUBPS, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x80
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x90
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0xa0
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0xb0
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0xc0
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0xd0
    OpcodeInstructionRecord(Opcode::ADDSUBPS, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0xe0
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0xf0
    OpcodeInstructionRecord(Opcode::LDDQU, OperandCode::G_E_xmm),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
];

fn read_opcode_f20f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<OperandCode> {
    match bytes_iter.next() {
        Some(b) => {
            instruction.length += 1;
            let record = OPCODE_F20F_MAP[b as usize];
            instruction.opcode = record.0;
            Some(record.1)
        }
        None => {
            None
        }
    }
}
fn read_opcode_f30f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<OperandCode> {
    match bytes_iter.next() {
        Some(b) => {
            instruction.length += 1;
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
}
const OPCODE_0F_MAP: [OpcodeInstructionRecord; 256] = [
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::ModRM_0x0f00),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::ModRM_0x0f01),
    OpcodeInstructionRecord(Opcode::LAR, OperandCode::Gv_M),
    OpcodeInstructionRecord(Opcode::LSL, OperandCode::Gv_M),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::SYSCALL, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::CLTS, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::SYSRET, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::INVD, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::WBINVD, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::UD2, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::NOP, OperandCode::Ev),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0x10
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::NOP, OperandCode::Ev),
// 0x20
    OpcodeInstructionRecord(Opcode::MOV, OperandCode::Rq_Cq_0),
    OpcodeInstructionRecord(Opcode::MOV, OperandCode::Rq_Dq_0),
    OpcodeInstructionRecord(Opcode::MOV, OperandCode::Cq_Rq_0),
    OpcodeInstructionRecord(Opcode::MOV, OperandCode::Dq_Rq_0),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),

// 0x30
    OpcodeInstructionRecord(Opcode::WRMSR, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::RDTSC, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::RDMSR, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::RDPMC, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),

// 0x40
    OpcodeInstructionRecord(Opcode::CMOVO, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVNO, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVB, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVNB, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVZ, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVNZ, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVNA, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVA, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVS, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVNS, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVP, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVNP, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVL, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVGE, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVLE, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::CMOVG, OperandCode::Gv_Ev),

// 0x50
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),

// 0x60
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),

// 0x70
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),

// 0x80
    OpcodeInstructionRecord(Opcode::JO, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JNO, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JB, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JNB, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JZ, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JNZ, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JNA, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JA, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JS, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JNS, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JP, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JNP, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JL, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JGE, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JLE, OperandCode::Jvds),
    OpcodeInstructionRecord(Opcode::JG, OperandCode::Jvds),

// 0x90
    OpcodeInstructionRecord(Opcode::SETO, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETNO, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETB, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETAE, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETZ, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETNZ, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETBE, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETA, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETS, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETNS, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETP, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETNP, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETL, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETGE, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETLE, OperandCode::Eb_R0),
    OpcodeInstructionRecord(Opcode::SETG, OperandCode::Eb_R0),

// 0xa0
    OpcodeInstructionRecord(Opcode::PUSH, OperandCode::FS),
    OpcodeInstructionRecord(Opcode::POP, OperandCode::GS),
    OpcodeInstructionRecord(Opcode::CPUID, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::BT, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::PUSH, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::PUSH, OperandCode::GS),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::ModRM_0x0fae),
    OpcodeInstructionRecord(Opcode::IMUL, OperandCode::Gv_Ev),

// 0xb0
    OpcodeInstructionRecord(Opcode::CMPXCHG, OperandCode::Eb_Gb),
    OpcodeInstructionRecord(Opcode::CMPXCHG, OperandCode::Ev_Gv),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::MOVZX_b, OperandCode::Gv_Eb),
    OpcodeInstructionRecord(Opcode::MOVZX_w, OperandCode::Gv_Ew),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::ModRM_0x0fba),
    OpcodeInstructionRecord(Opcode::BTC, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::BSF, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::BSR, OperandCode::Gv_Ev),
    OpcodeInstructionRecord(Opcode::MOVSX_b, OperandCode::Gv_Eb),
    OpcodeInstructionRecord(Opcode::MOVSX_w, OperandCode::Gv_Ew),

// 0xc0
    OpcodeInstructionRecord(Opcode::XADD, OperandCode::Eb_Gb),
    OpcodeInstructionRecord(Opcode::XADD, OperandCode::Ev_Gv),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),

// 0xd0
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),

// 0xe0
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
// 0xf0
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
    OpcodeInstructionRecord(Opcode::Invalid, OperandCode::Nothing),
];
fn read_opcode_0f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<OperandCode> {
    match bytes_iter.next() {
        Some(b) => {
            instruction.length += 1;
            let record = OPCODE_0F_MAP[b as usize];
            instruction.opcode = record.0;
            Some(record.1)
        }
        None => {
            None
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Interpretation {
    Instruction,
    Prefix,
}

#[derive(Copy, Clone)]
// this should be a 32-byte struct..
struct OpcodeRecord(Opcode, Interpretation, OperandCode);

const OPCODES: [OpcodeRecord; 256] = [
    OpcodeRecord(Opcode::ADD, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::ADD, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::ADD, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::ADD, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::ADD, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::ADD, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::OR, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::OR, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::OR, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::OR, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::OR, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::OR, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::ADC, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::ADC, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::ADC, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::ADC, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::ADC, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::ADC, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::SBB, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::SBB, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::SBB, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::SBB, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::SBB, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::SBB, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::AND, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::AND, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::AND, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::AND, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::AND, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::AND, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::SUB, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::SUB, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::SUB, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::SUB, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::SUB, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::SUB, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::XOR, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::XOR, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::XOR, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::XOR, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::XOR, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::XOR, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::CMP, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::CMP, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::CMP, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::CMP, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::CMP, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::CMP, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
// 0x40:
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
// 0x50:
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(0)),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(1)),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(2)),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(3)),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(4)),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(5)),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(6)),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Zv(7)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(0)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(1)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(2)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(3)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(4)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(5)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(6)),
    OpcodeRecord(Opcode::POP, Interpretation::Instruction, OperandCode::Zv(7)),
// 0x60
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::MOVSXD, Interpretation::Instruction, OperandCode::Gv_Ed),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Ivs),
    OpcodeRecord(Opcode::IMUL, Interpretation::Instruction, OperandCode::Gv_Ev_Iv),
    OpcodeRecord(Opcode::PUSH, Interpretation::Instruction, OperandCode::Ibs),
    OpcodeRecord(Opcode::IMUL, Interpretation::Instruction, OperandCode::Gb_Eb_Ib),
    OpcodeRecord(Opcode::INS, Interpretation::Instruction, OperandCode::Yb_DX),
    OpcodeRecord(Opcode::INS, Interpretation::Instruction, OperandCode::Yv_DX),
    OpcodeRecord(Opcode::OUTS, Interpretation::Instruction, OperandCode::DX_Xb),
    OpcodeRecord(Opcode::OUTS, Interpretation::Instruction, OperandCode::DX_Xv),
// 0x70
    OpcodeRecord(Opcode::JO, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JNO, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JB, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JNB, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JZ, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JNZ, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JNA, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JA, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JS, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JNS, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JP, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JNP, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JL, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JGE, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JLE, Interpretation::Instruction, OperandCode::Jbs),
    OpcodeRecord(Opcode::JG, Interpretation::Instruction, OperandCode::Jbs),
// 0x80
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0x80_Eb_Ib),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0x81_Ev_Ivs),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0x83_Ev_Ibs),
    OpcodeRecord(Opcode::TEST, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::TEST, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Eb_Gb),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Ev_Gv),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Gb_Eb),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Gv_Ev),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Ew_Sw),
    OpcodeRecord(Opcode::LEA, Interpretation::Instruction, OperandCode::Gv_M),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Sw_Ew),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0x8f_Ev),
    OpcodeRecord(Opcode::NOP, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Zv_AX(1)),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Zv_AX(2)),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Zv_AX(3)),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Zv_AX(4)),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Zv_AX(5)),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Zv_AX(6)),
    OpcodeRecord(Opcode::XCHG, Interpretation::Instruction, OperandCode::Zv_AX(7)),
    OpcodeRecord(Opcode::CBW, Interpretation::Instruction, OperandCode::AX_AL),
    OpcodeRecord(Opcode::CBW, Interpretation::Instruction, OperandCode::DX_AX),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::WAIT, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::PUSHF, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::POPF, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::SAHF, Interpretation::Instruction, OperandCode::AH),
    OpcodeRecord(Opcode::LAHF, Interpretation::Instruction, OperandCode::AH),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::AL_Ob),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::AX_Ov),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Ob_AL),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Ov_AX),
    OpcodeRecord(Opcode::MOVS, Interpretation::Instruction, OperandCode::Yb_Xb),
    OpcodeRecord(Opcode::MOVS, Interpretation::Instruction, OperandCode::Yv_Xv),
    OpcodeRecord(Opcode::CMPS, Interpretation::Instruction, OperandCode::Yb_Xb),
    OpcodeRecord(Opcode::CMPS, Interpretation::Instruction, OperandCode::Yv_Xv),
    OpcodeRecord(Opcode::TEST, Interpretation::Instruction, OperandCode::AL_Ib),
    OpcodeRecord(Opcode::TEST, Interpretation::Instruction, OperandCode::AX_Ivd),
    OpcodeRecord(Opcode::STOS, Interpretation::Instruction, OperandCode::Yb_AL),
    OpcodeRecord(Opcode::STOS, Interpretation::Instruction, OperandCode::Yv_AX),
    OpcodeRecord(Opcode::LODS, Interpretation::Instruction, OperandCode::AL_Xb),
    OpcodeRecord(Opcode::LODS, Interpretation::Instruction, OperandCode::AX_Xv),
    OpcodeRecord(Opcode::SCAS, Interpretation::Instruction, OperandCode::Yb_AL),
    OpcodeRecord(Opcode::SCAS, Interpretation::Instruction, OperandCode::Yv_AX),
// 0xb0
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(0)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(1)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(2)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(3)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(4)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(5)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(6)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zb_Ib(7)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(0)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(1)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(2)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(3)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(4)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(5)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(6)),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::Zv_Ivq(7)),
// 0xc0
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xc0_Eb_Ib),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xc1_Ev_Ib),
    OpcodeRecord(Opcode::RETURN, Interpretation::Instruction, OperandCode::Iw),
    OpcodeRecord(Opcode::RETURN, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::ModRM_0xc6_Eb_Ib),
    OpcodeRecord(Opcode::MOV, Interpretation::Instruction, OperandCode::ModRM_0xc7_Ev_Iv),
    OpcodeRecord(Opcode::ENTER, Interpretation::Instruction, OperandCode::Iw_Ib),
    OpcodeRecord(Opcode::LEAVE, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::RETF, Interpretation::Instruction, OperandCode::Iw),
    OpcodeRecord(Opcode::RETF, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::INT, Interpretation::Instruction, OperandCode::I_3),
    OpcodeRecord(Opcode::INT, Interpretation::Instruction, OperandCode::Ib),
    OpcodeRecord(Opcode::INTO, Interpretation::Instruction, OperandCode::Fw),
    OpcodeRecord(Opcode::IRET, Interpretation::Instruction, OperandCode::Fw),
// 0xd0
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xd0_Eb_1),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xd1_Ev_1),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xd2_Eb_CL),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xd3_Ev_CL),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // XLAT
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 d8
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 d9
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 da
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 db
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 dc
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 dd
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 de
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // x86 df
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
// 0xe0
    // LOOPNZ
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // LOOPZ
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // LOOP
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // JECXZ
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // IN
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // IN
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // OUT
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // OUT
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
// 0xe8
    OpcodeRecord(Opcode::CALL, Interpretation::Instruction, OperandCode::Jvds),
    OpcodeRecord(Opcode::JMP, Interpretation::Instruction, OperandCode::Jvds),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::JMP, Interpretation::Instruction, OperandCode::Jbs),
    // IN
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // IN
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // OUT
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    // OUT
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
// 0xf0
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    // ICEBP?
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Prefix, OperandCode::Nothing),
// 0xf4
    OpcodeRecord(Opcode::HLT, Interpretation::Instruction, OperandCode::Nothing),
    // CMC
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xf6),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xf7),
    OpcodeRecord(Opcode::CLC, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::STC, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::CLI, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::STI, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::CLD, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::STD, Interpretation::Instruction, OperandCode::Nothing),
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xfe_Eb),
    // TODO: test 0xff /3
    OpcodeRecord(Opcode::Invalid, Interpretation::Instruction, OperandCode::ModRM_0xff_Ev),
];

fn read_opcode<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<OperandCode> {
    let mut alternate_opcode_map: Option<OpcodeMap> = None;
    use std::hint::unreachable_unchecked;
//    use std::intrinsics::unlikely;
    instruction.prefixes = Prefixes::new(0);
    loop {
        match bytes_iter.next() {
            Some(b) => {
                instruction.length += 1;
                let record = OPCODES[b as usize];
                if record.1 == Interpretation::Instruction {
                    instruction.opcode = record.0;
                    return Some(record.2);
                } else {
                    match b {
                        0x0f => {
                            return match alternate_opcode_map {
                                Some(OpcodeMap::Map66) => {
                                    read_opcode_660f_map(bytes_iter, instruction)
                                },
                                Some(OpcodeMap::MapF2) => {
                                    read_opcode_f20f_map(bytes_iter, instruction)
                                },
                                Some(OpcodeMap::MapF3) => {
                                    read_opcode_f30f_map(bytes_iter, instruction)
                                },
                                None => {
                                    read_opcode_0f_map(bytes_iter, instruction)
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
                        x if x < 0x50 => {
                            // x86_32 inc/dec
                            // x86_64 rex
                            instruction.prefixes.rex_mut().from(x);
                        },
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
                        _ => { unsafe { unreachable_unchecked(); } }
                    }
                }
            },
            None => {
                return None;
            }
        }
    }
}

#[allow(non_snake_case)]
fn read_E<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, width: u8, result: usize) -> Option<()> {
    let bank = width_to_gp_reg_bank(width, instr.prefixes.rex().present());
    read_E_anybank(bytes_iter, instr, modrm, width, result, bank)
}
#[allow(non_snake_case)]
fn read_E_xmm<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, width: u8, result: usize) -> Option<()> {
    read_E_anybank(bytes_iter, instr, modrm, width, result, RegisterBank::X)
}

#[allow(non_snake_case)]
fn read_E_anybank<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, _width: u8, result: usize, reg_bank: RegisterBank) -> Option<()> {
    let modbits = (modrm >> 6);
    let m = modrm & 7;
    let addr_width = if instr.prefixes.address_size() { 4 } else { 8 };
    if modbits == 0b11 {
        instr.operands[result] = Operand::Register(RegSpec::from_parts(m, instr.prefixes.rex().b(), reg_bank))
    } else if m == 5 && modbits == 0b00 {
        let disp = read_num(bytes_iter, 4, &mut instr.length);
        instr.operands[result] = Operand::RegDisp(
            if addr_width == 8 { RegSpec::RIP() } else { RegSpec::EIP() },
            disp as i32
        );
    } else if m == 4 {
        let sibbyte = match bytes_iter.next() {
            Some(b) => b,
            None => { return None; } //Err("Out of bytes".to_string())
        };
        instr.length += 1;
//         let (ss, index, base) = octets_of(sibbyte);

//            println!("scale: {:b}, index: {:b}, base: {:b}", ss, index, base);

        if (sibbyte & 7) == 0b101 {
            let disp = if modbits == 0b00 {
                read_num(bytes_iter, 4, &mut instr.length) as i32
            } else if modbits == 0b01 {
                read_num(bytes_iter, 1, &mut instr.length) as i8 as i32
            } else {
                read_num(bytes_iter, 4, &mut instr.length) as i32
            };

            if ((sibbyte >> 3) & 7) == 0b100 {
                if modbits == 0b00 && !instr.prefixes.rex().x() {
                    instr.operands[result] = Operand::DisplacementU32(disp as u32);
                } else {
                    let reg = RegSpec::gp_from_parts(0b100, instr.prefixes.rex().x(), addr_width, instr.prefixes.rex().present());

                    if disp == 0 {
                        instr.operands[result] = Operand::RegDeref(reg);
                    } else {
                        instr.operands[result] = Operand::RegDisp(reg, disp as i32);
                    }
                }
            } else {
                let base_reg = RegSpec::gp_from_parts(5, instr.prefixes.rex().b(), addr_width, instr.prefixes.rex().present());

                let index_reg = RegSpec::gp_from_parts((sibbyte >> 3) & 7, instr.prefixes.rex().x(), addr_width, instr.prefixes.rex().present());
                let scale = 1u8 << (sibbyte >> 6);

                instr.operands[result] = match (scale, modbits, disp) {
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
                        Operand::RegScale(index_reg, scale)
                    },
                    (_, 0b00, _) => {
                        Operand::RegScaleDisp(index_reg, scale, disp as i32)
                    },
                    (_, _, 0) => {
                        Operand::RegIndexBaseScale(base_reg, index_reg, scale)
                    },
                    (_, _, _) => {
                        Operand::RegIndexBaseScaleDisp(base_reg, index_reg, scale, disp as i32)
                    }
                };
            }
        } else {
            let base_reg = RegSpec::gp_from_parts((sibbyte & 7), instr.prefixes.rex().b(), addr_width, instr.prefixes.rex().present());

            let disp = if modbits == 0b00 {
                0
            } else if modbits == 0b01 {
                read_num(bytes_iter, 1, &mut instr.length) as i8 as i32
            } else {
                read_num(bytes_iter, 4, &mut instr.length) as i32
            };

            if ((sibbyte >> 3) & 7) == 0b100 {
                if disp == 0 {
                    instr.operands[result] = Operand::RegDeref(base_reg);
                } else {
                    instr.operands[result] = Operand::RegDisp(base_reg, disp as i32);
                }
            } else {
                let index_reg = RegSpec::gp_from_parts((sibbyte >> 3) & 7, instr.prefixes.rex().x(), addr_width, instr.prefixes.rex().present());
                let scale = 1u8 << (sibbyte >> 6);
                if disp == 0 {
                    if scale == 0 {
                        instr.operands[result] = Operand::RegIndexBase(base_reg, index_reg)
                    } else {
                        instr.operands[result] = Operand::RegIndexBaseScale(base_reg, index_reg, scale);
                    }
                } else {
                    if scale == 0 {

                        instr.operands[result] = Operand::RegIndexBaseDisp(base_reg, index_reg, disp as i32);
                    } else {
                        instr.operands[result] = Operand::RegIndexBaseScaleDisp(base_reg, index_reg, scale, disp as i32);
                    }
                }
            }
        }
    } else {
        let reg = RegSpec::gp_from_parts(m, instr.prefixes.rex().b(), addr_width, instr.prefixes.rex().present());

        if modbits == 0b00 {
            instr.operands[result] = Operand::RegDeref(reg);
        } else {
            let disp = if modbits == 0b01 {
                read_num(bytes_iter, 1, &mut instr.length) as i8 as i32
            } else {
                read_num(bytes_iter, 4, &mut instr.length) as i32
            };
            instr.operands[result] = Operand::RegDisp(reg, disp);
        }
    }
    Some(())
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

pub fn read_instr<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<()> {
    let mut alternate_opcode_map: Option<OpcodeMap> = None;
    use std::hint::unreachable_unchecked;
//    use std::intrinsics::unlikely;
    instruction.prefixes = Prefixes::new(0);
    let operand_code = loop {
        match bytes_iter.next() {
            Some(b) => {
                instruction.length += 1;
                let record = OPCODES[b as usize];
                if record.1 == Interpretation::Instruction {
                    instruction.opcode = record.0;
                    break record.2;
                } else {
                    match b {
                        0x0f => {
                            let opc = match alternate_opcode_map {
                                Some(OpcodeMap::Map66) => {
                                    read_opcode_660f_map(bytes_iter, instruction)
                                },
                                Some(OpcodeMap::MapF2) => {
                                    read_opcode_f20f_map(bytes_iter, instruction)
                                },
                                Some(OpcodeMap::MapF3) => {
                                    read_opcode_f30f_map(bytes_iter, instruction)
                                },
                                None => {
                                    read_opcode_0f_map(bytes_iter, instruction)
                                }
                            };
                            match opc {
                                Some(o) => {
                                    break o;
                                },
                                None => { return None; }
                            }
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
                        x if x < 0x50 => {
                            // x86_32 inc/dec
                            // x86_64 rex
                            instruction.prefixes.rex_mut().from(x);
                        },
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
                        _ => { unsafe { unreachable_unchecked(); } }
                    }
                }
            },
            None => {
                return None;
            }
        }
    };
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
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return None; // Err("Invalid modr/m for opcode 0xc6".to_owned());
            }

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
        },
        OperandCode::AL_Ob => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = 1;
            let imm = read_num(bytes_iter, addr_width, &mut instruction.length);
            instruction.operands = [
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present())),
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                }
            ];
        }
        OperandCode::AX_Ov => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let imm = read_num(bytes_iter, addr_width, &mut instruction.length);
            instruction.operands = [
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present())),
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                }
            ];
        }
        OperandCode::Ob_AL => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = 1;
            let imm = read_num(bytes_iter, addr_width, &mut instruction.length);
            instruction.operands = [
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                },
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present()))
            ];
        }
        OperandCode::Ov_AX => {
            let _addr_width = if instruction.prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if instruction.prefixes.address_size() { 2 } else { 4 };
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let imm = read_num(bytes_iter, addr_width, &mut instruction.length);
            instruction.operands = [
                if instruction.prefixes.address_size() {
                    Operand::DisplacementU32(imm as u32)
                } else {
                    Operand::DisplacementU64(imm)
                },
                Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present()))
            ];
        }
        OperandCode::ModRM_0x80_Eb_Ib => {
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            let num = read_num(bytes_iter, 1, &mut instruction.length) as i8;
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[1] = Operand::ImmediateI8(num);
        },
        OperandCode::ModRM_0x81_Ev_Ivs => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            let imm = read_imm_signed(bytes_iter, if opwidth == 8 { 4 } else { opwidth }, opwidth, &mut instruction.length).unwrap();
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[1] = imm;
        },
        OperandCode::ModRM_0xc0_Eb_Ib => {
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            let num = read_num(bytes_iter, 1, &mut instruction.length) as i8;
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            instruction.operands[1] = Operand::ImmediateI8(num);
        },
        OperandCode::ModRM_0xc1_Ev_Ib => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            let num = read_num(bytes_iter, 1, &mut instruction.length) as i8;
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            instruction.operands[1] = Operand::ImmediateI8(num);
        },
        OperandCode::ModRM_0xc6_Eb_Ib => {
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            let num = read_num(bytes_iter, 1, &mut instruction.length) as i8;
            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return None; // Err("Invalid modr/m for opcode 0xc6".to_owned());
            }
            instruction.opcode = Opcode::MOV;
            instruction.operands[1] = Operand::ImmediateI8(num);
        },
        OperandCode::ModRM_0xc7_Ev_Iv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return None; // Err("Invalid modr/m for opcode 0xc7".to_string());
            }

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.opcode = Opcode::MOV;
            instruction.operands[1] = read_imm_unsigned(bytes_iter, opwidth, &mut instruction.length).unwrap();
        },
        OperandCode::ModRM_0xd0_Eb_1 => {
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            instruction.operands[1] = Operand::ImmediateI8(1);
        },
        OperandCode::ModRM_0xd1_Ev_1 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            instruction.operands[1] = Operand::ImmediateI8(1);
        },
        OperandCode::ModRM_0xf6 => {
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            match (modrm >> 3) & 7 {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    instruction.operands[1] = read_imm_signed(bytes_iter, 1, opwidth, &mut instruction.length).unwrap();
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
        OperandCode::ModRM_0xf7 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            match ((modrm >> 3) & 7) {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    let numwidth = if opwidth == 8 { 4 } else { opwidth };
                    instruction.operands[1] = read_imm_signed(bytes_iter, numwidth, opwidth, &mut instruction.length).unwrap();
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
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
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
            instruction.operands[1] = Operand::Nothing;
        }
        OperandCode::ModRM_0xff_Ev => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
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
            instruction.operands[1] = Operand::Nothing;
        }
        OperandCode::Ev => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.operands[1] = Operand::Nothing;
        },
        OperandCode::Eb_Gb => {
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.operands[1] =
                Operand::Register(RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
        },
        OperandCode::Ev_Gv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.operands[1] =
                Operand::Register(RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
        },
        OperandCode::Gb_Eb => {
            let opwidth = 1;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 1).unwrap();
            instruction.operands[0] =
                Operand::Register(RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
        },
        OperandCode::Gv_Eb => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, opwidth, 1).unwrap();
            instruction.operands[0] =
                Operand::Register(RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
        },
        OperandCode::Gv_Ew => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            read_E(bytes_iter, instruction, modrm, 2, 1).unwrap();
            instruction.operands[0] =
                Operand::Register(RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
        },
        OperandCode::Ew_Sw => {
            let opwidth = 2;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            // check r
            if ((modrm >> 3) & 7) > 5 {
                return None; //Err("Invalid r".to_owned());
            }

            instruction.operands[1] =
                Operand::Register(RegSpec { bank: RegisterBank::S, num: (modrm >> 3) & 7 });

            let mod_bits = modrm >> 6;
            if mod_bits == 0b11 {
                instruction.operands[0] =
                    Operand::Register(RegSpec { bank: RegisterBank::W, num: modrm & 7});
            } else {
                read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            }
        },
        OperandCode::Sw_Ew => {
            let opwidth = 2;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

            // check r
            if ((modrm >> 3) & 7) > 5 {
                return None; // Err("Invalid r".to_owned());
            }

            instruction.operands[0] =
                Operand::Register(RegSpec { bank: RegisterBank::S, num: (modrm >> 3) & 7 });

            let mod_bits = modrm >> 6;
            if mod_bits == 0b11 {
                instruction.operands[1] =
                    Operand::Register(RegSpec { bank: RegisterBank::W, num: modrm & 7});
            } else {
                read_E(bytes_iter, instruction, modrm, opwidth, 1).unwrap();
            }
        },
        // TODO: verify M
        OperandCode::Gv_Ed => {
            let opwidth = 4;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E(bytes_iter, instruction, modrm, opwidth, 1).unwrap();
            instruction.operands[0] =
                Operand::Register(RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
        },
        OperandCode::Gv_Ev | OperandCode::Gv_M => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E(bytes_iter, instruction, modrm, opwidth, 1).unwrap();
            instruction.operands[0] =
                Operand::Register(RegSpec::gp_from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
        },
        OperandCode::E_G_xmm => {
            let opwidth = 8;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E_xmm(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.operands[1] =
                Operand::Register(RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X));
        },
        OperandCode::G_E_xmm => {
            let opwidth = 8;
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E_xmm(bytes_iter, instruction, modrm, opwidth, 1).unwrap();
            instruction.operands[0] =
                Operand::Register(RegSpec::from_parts((modrm >> 3) & 7, instruction.prefixes.rex().r(), RegisterBank::X));
        },
        OperandCode::Zv_Ivq(opcode_byte) => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let reg_idx = opcode_byte & 0x7;
            instruction.operands = [
                Operand::Register(RegSpec::gp_from_parts(reg_idx, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present())),
                read_imm_ivq(bytes_iter, opwidth, &mut instruction.length).unwrap()
            ];
        },
        OperandCode::AL_Ib => {
            let opwidth = 1;
            let numwidth = 1;
            instruction.operands = [
                Operand::Register(RegSpec::al()),
                read_imm_signed(bytes_iter, numwidth, opwidth, &mut instruction.length).unwrap()
            ];
        }
        OperandCode::AX_Ivd => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.operands = [
                Operand::Register(RegSpec::gp_from_parts(0, false, opwidth, false)),
                read_imm_signed(bytes_iter, numwidth, opwidth, &mut instruction.length).unwrap()
            ];
        }
        OperandCode::Zb_Ib(opcode_byte) => {
            let reg_idx = opcode_byte & 0x7;
            instruction.operands = [
                Operand::Register(RegSpec::gp_from_parts(reg_idx, instruction.prefixes.rex().b(), 1, instruction.prefixes.rex().present())),
                read_imm_unsigned(bytes_iter, 1, &mut instruction.length).unwrap()
            ];
        },
        OperandCode::Iw => {
            instruction.operands = [
                read_imm_unsigned(bytes_iter, 2, &mut instruction.length).unwrap(),
                Operand::Nothing
            ];
        }
        OperandCode::Jbs => {
            // TODO: arch width (8 in 64, 4 in 32, 2 in 16)
            instruction.operands = [
                read_imm_signed(bytes_iter, 1, 8, &mut instruction.length).unwrap(),
                Operand::Nothing
            ];
        },
        OperandCode::Ibs => {
            instruction.operands = [
                read_imm_signed(bytes_iter, 1, 8, &mut instruction.length).unwrap(),
                Operand::Nothing
            ];
        },
        OperandCode::Ivs => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, &instruction.prefixes);
            instruction.operands = [
                read_imm_unsigned(bytes_iter, opwidth, &mut instruction.length).unwrap(),
                Operand::Nothing
            ];
        },
        OperandCode::ModRM_0x83_Ev_Ibs => {
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[1] = read_imm_signed(bytes_iter, 1, opwidth, &mut instruction.length).unwrap();
        },
        OperandCode::Zv(opcode_byte) => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, &instruction.prefixes);
            instruction.operands = [Operand::Register(
                RegSpec::gp_from_parts(
                    opcode_byte & 0b111, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present()
                )
            ), Operand::Nothing];
        },
        OperandCode::Jvds => {
            let offset = read_num(bytes_iter, 4, &mut instruction.length);
            instruction.operands = [Operand::ImmediateI32(offset as i32), Operand::Nothing];
        }
        OperandCode::ModRM_0x0f00 => {
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let r = (modrm >> 3) & 7;
            if r == 0 {
                instruction.opcode = Opcode::SLDT;
                instruction.operands[1] = Operand::Nothing;
            } else if r == 1 {
                instruction.opcode = Opcode::STR;
                instruction.operands[1] = Operand::Nothing;
            } else if r == 2 {
                instruction.opcode = Opcode::LLDT;
                instruction.operands[1] = Operand::Nothing;
            } else if r == 3 {
                instruction.opcode = Opcode::LTR;
                instruction.operands[1] = Operand::Nothing;
            } else if r == 4 {
                instruction.opcode = Opcode::VERR;
                instruction.operands[1] = Operand::Nothing;
            } else if r == 5 {
                instruction.opcode = Opcode::VERW;
                instruction.operands[1] = Operand::Nothing;
            } else if r == 6 {
                instruction.opcode = Opcode::JMPE;
                instruction.operands = [Operand::Nothing, Operand::Nothing];
                return Some(());
            } else if r == 7 {
                instruction.opcode = Opcode::Invalid;
                instruction.operands = [Operand::Nothing, Operand::Nothing];
                return Some(());
            } else {
                unreachable!("r <= 8");
            }
            read_E(bytes_iter, instruction, modrm, 2, 0).unwrap();
        }
        OperandCode::ModRM_0x0f01 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let r = (modrm >> 3) & 7;
            if r == 0 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    panic!("Unsupported instruction: 0x0f01 with modrm: 11 000 ___");
                } else {
                    instruction.opcode = Opcode::SGDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
                }
            } else if r == 1 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    // TOOD: MONITOR
                    instruction.opcode = Opcode::NOP;
                    instruction.operands[0] = Operand::Nothing;
                } else {
                    instruction.opcode = Opcode::SIDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
                }
            } else if r == 2 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    // TOOD: XGETBV
                    instruction.opcode = Opcode::NOP;
                    instruction.operands[0] = Operand::Nothing;
                } else {
                    instruction.opcode = Opcode::LGDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
                }
            } else if r == 3 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    // TOOD: VMRUN
                    instruction.opcode = Opcode::NOP;
                    instruction.operands[0] = Operand::Nothing;
                } else {
                    instruction.opcode = Opcode::LIDT;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
                }
            } else if r == 4 {
                // TODO: this permits storing only to word-size registers
                // spec suggets this might do something different for f.ex rdi.unwrap()
                instruction.opcode = Opcode::SMSW;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, instruction, modrm, 2, 0).unwrap();
            } else if r == 5 {
                panic!("Unsupported instruction: 0x0f01 with modrm: __ 101 ___");
            } else if r == 6 {
                instruction.opcode = Opcode::LMSW;
                instruction.operands[1] = Operand::Nothing;
                read_E(bytes_iter, instruction, modrm, 2, 0).unwrap();
            } else if r == 7 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    if m == 1 {
                        instruction.opcode = Opcode::SWAPGS;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                    } else if m == 2 {
                        instruction.opcode = Opcode::RDTSCP;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                    } else {
                    //    panic!("Unsupported instruction: 0x0f01 with modrm: 11 110 r >= 2");
                        return None; // Err("unsupported 0x0f01 variant".to_string())
                    }
                } else {
                    instruction.opcode = Opcode::INVLPG;
                    instruction.operands[1] = Operand::Nothing;
                    read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();
                }
            } else {
                unreachable!("r <= 8");
            }
        }
        OperandCode::ModRM_0x0fae => {
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let r = (modrm >> 3) & 7;
            let mod_bits = modrm >> 6;
            match r {
                0 => {
                    if mod_bits == 0b11 {
                        return None; // Err("Invalid mod bits".to_owned())
                    } else {
                        instruction.opcode = Opcode::FXSAVE;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                1 => {
                    if mod_bits == 0b11 {
                        return None; // Err("Invalid mod bits".to_owned());
                    } else {
                        instruction.opcode = Opcode::FXRSTOR;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                2 => {
                    if mod_bits == 0b11 {
                        return None; // Err("Invalid mod bits".to_owned());
                    } else {
                        instruction.opcode = Opcode::LDMXCSR;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                3 => {
                    if mod_bits == 0b11 {
                        return None; //Err("Invalid mod bits".to_owned());
                    } else {
                        instruction.opcode = Opcode::STMXCSR;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                4 => {
                    if mod_bits == 0b11 {
                        return None; //Err("Invalid mod bits".to_owned());
                    } else {
                        instruction.opcode = Opcode::XSAVE;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                5 => {
                    if mod_bits == 0b11 {
                        instruction.opcode = Opcode::LFENCE;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                    } else {
                        instruction.opcode = Opcode::XSTOR;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                6 => {
                    if mod_bits == 0b11 {
                        instruction.opcode = Opcode::MFENCE;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                        return Some(());
                    } else {
                        // TODO: radare reports this, but i'm not sure.unwrap()
                        instruction.opcode = Opcode::XSAVEOPT;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                7 => {
                    if mod_bits == 0b11 {
                        instruction.opcode = Opcode::SFENCE;
                        instruction.operands = [Operand::Nothing, Operand::Nothing];
                        return Some(());
                    } else {
                        // TODO: radare reports this, but i'm not sure.unwrap()
                        instruction.opcode = Opcode::CLFLUSH;
                        instruction.operands[1] = Operand::Nothing;
                    }
                }
                _ => { unreachable!("r < 6"); }
            }
            read_E(bytes_iter, instruction, modrm, 8, 0).unwrap();
        }
        OperandCode::ModRM_0x0fba => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, &instruction.prefixes);
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let r = (modrm >> 3) & 7;
            match r {
                0 | 1 | 2 | 3 => {
                    instruction.opcode = Opcode::Invalid;
                    return None; //Err("invalid instruction".to_string());
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

            read_E(bytes_iter, instruction, modrm, opwidth, 0).unwrap();

            instruction.operands[1] = read_imm_signed(bytes_iter, 1, 1, &mut instruction.length).unwrap();
        }

        OperandCode::Rq_Cq_0 => {
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let mut m = modrm & 7;
            let mut r = (modrm >> 3) & 7;
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
        }
        OperandCode::Rq_Dq_0 => {
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let mut m = modrm & 7;
            let mut r = (modrm >> 3) & 7;
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
        }
        OperandCode::Cq_Rq_0 => {
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let mut m = modrm & 7;
            let mut r = (modrm >> 3) & 7;
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
        }
        OperandCode::Dq_Rq_0 => {
            let modrm = read_modrm(bytes_iter, &mut instruction.length).unwrap();
            let mut m = modrm & 7;
            let mut r = (modrm >> 3) & 7;
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
        }
        OperandCode::FS => {
            instruction.operands = [Operand::Register(RegSpec::fs()), Operand::Nothing];
        }
        OperandCode::GS => {
            instruction.operands = [Operand::Register(RegSpec::fs()), Operand::Nothing];
        }
        OperandCode::I_3 => {
            instruction.operands = [Operand::ImmediateU8(3), Operand::Nothing];
        }
        OperandCode::Nothing => {
            instruction.operands = [Operand::Nothing, Operand::Nothing];
        }
        _ => {
            instruction.operands = [Operand::Nothing, Operand::Nothing];
            instruction.opcode = Opcode::Invalid;
        //    use std::hint::unreachable_unchecked;
            return None; // Err(format!("unsupported operand code: {:.unwrap()}", operand_code));
        //    unsafe { unreachable_unchecked(); }
        }
    };

    Some(())
}

pub fn decode_one<'b, T: IntoIterator<Item=u8>>(bytes: T, instr: &'b mut Instruction) -> Option<()> {
    let mut bytes_iter = bytes.into_iter();
    instr.length = 0;
    read_instr(&mut bytes_iter, instr)
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
fn read_modrm<T: Iterator<Item=u8>>(bytes_iter: &mut T, length: &mut u8) -> Result<u8, String> {
    let modrm = match bytes_iter.next() {
        Some(b) => b,
        // TODO: ...
        None => return Err("Out of bytes".to_string()),
    };
    *length += 1;
    Ok(modrm)
}
