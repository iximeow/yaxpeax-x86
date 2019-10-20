#[cfg(feature="use-serde")]
#[macro_use] extern crate serde_derive;
#[cfg(feature="use-serde")]
extern crate serde;

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
            OperandSpec::Disp |
            OperandSpec::Deref |
            OperandSpec::RegDisp |
            OperandSpec::RegScale |
            OperandSpec::RegIndexBase |
            OperandSpec::RegIndexBaseDisp |
            OperandSpec::RegScaleDisp |
            OperandSpec::RegIndexBaseScale |
            OperandSpec::RegIndexBaseScaleDisp => {
                true
            },
            OperandSpec::Imm |
            OperandSpec::RegRRR |
            OperandSpec::RegMMM |
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
            OperandSpec::Imm => {
                match inst.imm {
                    ImmediateKind::ImmediateI8(i) => Operand::ImmediateI8(i),
                    ImmediateKind::ImmediateU8(i) => Operand::ImmediateU8(i),
                    ImmediateKind::ImmediateI16(i) => Operand::ImmediateI16(i),
                    ImmediateKind::ImmediateU16(i) => Operand::ImmediateU16(i),
                    ImmediateKind::ImmediateI32(i) => Operand::ImmediateI32(i),
                    ImmediateKind::ImmediateU32(i) => Operand::ImmediateU32(i),
                    ImmediateKind::ImmediateI64(i) => Operand::ImmediateI64(i),
                    ImmediateKind::ImmediateU64(i) => Operand::ImmediateU64(i),
                }
            }
            OperandSpec::Disp => {
                match inst.disp {
                    DisplacementKind::DisplacementU32(d) => Operand::DisplacementU32(d),
                    DisplacementKind::DisplacementI32(_) => {
                        panic!("invalid state, signed displacement isn't available except for complex memory modes");
                    }
                    DisplacementKind::DisplacementU64(d) => Operand::DisplacementU64(d),
                }
            }
            OperandSpec::Deref => {
                Operand::RegDeref(inst.modrm_mmm)
            }
            OperandSpec::RegDisp => {
                let disp = match inst.disp {
                    DisplacementKind::DisplacementI32(d) => d,
                    _ => {
                        unreachable!("unsigned displacement isn't available in complex memory modes");
                    }
                };
                Operand::RegDisp(inst.modrm_mmm, disp)
            }
            OperandSpec::RegScale => {
                Operand::RegScale(inst.sib_index, inst.scale)
            }
            OperandSpec::RegIndexBase => {
                Operand::RegIndexBase(inst.sib_index, inst.modrm_mmm)
            }
            OperandSpec::RegIndexBaseDisp => {
                let disp = match inst.disp {
                    DisplacementKind::DisplacementI32(d) => d,
                    _ => {
                        unreachable!("unsigned displacement isn't available in complex memory modes");
                    }
                };
                Operand::RegIndexBaseDisp(inst.sib_index, inst.modrm_mmm, disp)
            }
            OperandSpec::RegScaleDisp => {
                let disp = match inst.disp {
                    DisplacementKind::DisplacementI32(d) => d,
                    _ => {
                        unreachable!("unsigned displacement isn't available in complex memory modes");
                    }
                };
                Operand::RegScaleDisp(inst.sib_index, inst.scale, disp)
            }
            OperandSpec::RegIndexBaseScale => {
                Operand::RegIndexBaseScale(inst.sib_index, inst.modrm_mmm, inst.scale)
            }
            OperandSpec::RegIndexBaseScaleDisp => {
                let disp = match inst.disp {
                    DisplacementKind::DisplacementI32(d) => d,
                    _ => {
                        unreachable!("unsigned displacement isn't available in complex memory modes");
                    }
                };
                Operand::RegIndexBaseScaleDisp(inst.sib_index, inst.modrm_mmm, inst.scale, disp)
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
    assert_eq!(std::mem::size_of::<Prefixes>(), 4);
    // assert_eq!(std::mem::size_of::<Instruction>(), 3);
}

#[allow(non_camel_case_types)]
#[cfg(feature="use-serde")]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP, EFlags, RFlags,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
}
#[allow(non_camel_case_types)]
#[cfg(not(feature="use-serde"))]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP, EFlags, RFlags,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Segment {
    DS = 0, CS, ES, FS, GS, SS
}

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
}
#[derive(Debug)]
pub struct Instruction {
    pub prefixes: Prefixes,
    modrm_rrr: RegSpec,
    modrm_mmm: RegSpec, // doubles as sib_base
    sib_index: RegSpec,
    scale: u8,
    disp: DisplacementKind,
    imm: ImmediateKind,
    operand_count: u8,
    operands: [OperandSpec; 4],
    pub opcode: Opcode,
    pub length: u8
}

#[derive(Debug, Copy, Clone)]
enum DisplacementKind {
    DisplacementU32(u32),
    DisplacementI32(i32),
    DisplacementU64(u64),
}

#[derive(Debug, Copy, Clone)]
enum ImmediateKind {
    ImmediateI8(i8),
    ImmediateU8(u8),
    ImmediateI16(i16),
    ImmediateU16(u16),
    ImmediateI32(i32),
    ImmediateU32(u32),
    ImmediateI64(i64),
    ImmediateU64(u64),
}

#[derive(Debug, Copy, Clone)]
enum OperandSpec {
    Nothing,
    // the register in modrm_rrr
    RegRRR,
    // the register in modrm_mmm (eg modrm mod bits were 11)
    RegMMM,
    Imm,
    Disp,
    Deref,
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
            modrm_rrr: RegSpec::rax(),
            modrm_mmm: RegSpec::rax(), // doubles as sib_base
            sib_index: RegSpec::rax(),
            scale: 0,
            disp: DisplacementKind::DisplacementU32(0),
            imm: ImmediateKind::ImmediateI8(0),
            operand_count: 0,
            operands: [OperandSpec::Nothing; 4],
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

#[derive(Debug, Copy, Clone)]
pub struct Prefixes {
    bits: u8,
    rep_prefix: RepPrefix,
    rex: PrefixRex,
    segment: Segment,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RepPrefix {
    None = 0,
    E = 1,
    NE = 2,
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
            rep_prefix: RepPrefix::None,
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
    pub fn repne(&self) -> bool { self.rep_prefix == RepPrefix::NE }
    #[inline]
    fn set_repne(&mut self) { self.rep_prefix = RepPrefix::NE; }
    #[inline]
    pub fn repe(&self) -> bool { self.rep_prefix == RepPrefix::E }
    #[inline]
    fn set_repe(&mut self) { self.rep_prefix = RepPrefix::E; }
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
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OperandCode {
    Eb_Gb,
    Ev_Gv,
    Gb_Eb,
    Gb_Eb_Ib,
    Gv_Ev,
    Gv_Ev_Iv,
    AL_Ib,
    AX_Ivd,
    ModRM_0x0f00,
    ModRM_0x0f01,
    ModRM_0x0f12,
    ModRM_0x0f13,
    ModRM_0x0fae,
    ModRM_0x0fba,
    ModRM_0xf238,
    ModRM_0xf30fc7,
    Rq_Cq_0,
    Rq_Dq_0,
    Cq_Rq_0,
    Dq_Rq_0,
    FS,
    GS,
    Eb_R0,
    ModRM_0xf6,
    ModRM_0xf7,
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
    Gdq_Ed,
    G_E_xmm,
    G_E_xmm_Ib,
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
    Zb_Ib_R0 = 200,
    Zb_Ib_R1 = 201,
    Zb_Ib_R2 = 202,
    Zb_Ib_R3 = 203,
    Zb_Ib_R4 = 204,
    Zb_Ib_R5 = 205,
    Zb_Ib_R6 = 206,
    Zb_Ib_R7 = 207,
    Zv_R0 = 208,
    Zv_R1 = 209,
    Zv_R2 = 210,
    Zv_R3 = 211,
    Zv_R4 = 212,
    Zv_R5 = 213,
    Zv_R6 = 214,
    Zv_R7 = 215,
    Zv_AX_R0 = 216,
    Zv_AX_R1 = 217,
    Zv_AX_R2 = 218,
    Zv_AX_R3 = 219,
    Zv_AX_R4 = 220,
    Zv_AX_R5 = 221,
    Zv_AX_R6 = 222,
    Zv_AX_R7 = 223,
    Zv_Ivq_R0 = 224,
    Zv_Ivq_R1 = 225,
    Zv_Ivq_R2 = 226,
    Zv_Ivq_R3 = 227,
    Zv_Ivq_R4 = 228,
    Zv_Ivq_R5 = 229,
    Zv_Ivq_R6 = 230,
    Zv_Ivq_R7 = 231,
    Nothing,
    Implied,
    Unsupported,
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
fn read_opcode_660f_map<T: Iterator<Item=u8>>(_bytes_iter: &mut T, _instruction: &mut Instruction) -> Option<OpcodeRecord> {
    panic!("660f opcode map unsupported".to_string());
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
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSI2SD), OperandCode::G_E_xmm),
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

fn read_opcode_f20f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<OpcodeRecord> {
    match bytes_iter.next() {
        Some(b) => {
            instruction.length += 1;
            let record = OPCODE_F20F_MAP[b as usize];
            if let Interpretation::Instruction(opc) = record.0 {
                instruction.opcode = opc;
            } else {
                unsafe { unreachable_unchecked(); }
            }
            Some(record)
        }
        None => {
            None
        }
    }
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
    OpcodeRecord(Interpretation::Instruction(Opcode::CVTSI2SS), OperandCode::G_E_xmm),
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

fn read_opcode_f30f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<OpcodeRecord> {
    match bytes_iter.next() {
        Some(b) => {
            instruction.length += 1;
            let record = OPCODE_F30F_MAP[b as usize];
            if let Interpretation::Instruction(opc) = record.0 {
                instruction.opcode = opc;
            } else {
                unsafe { unreachable_unchecked(); }
            }
            Some(record)
        }
        None => {
            None
        }
    }
    /*
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0f13),
    OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKLPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::UNPCKHPS), OperandCode::G_E_xmm),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),

// 0x30
    OpcodeRecord(Interpretation::Instruction(Opcode::WRMSR), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RDTSC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RDMSR), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::RDPMC), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::GS),
    OpcodeRecord(Interpretation::Instruction(Opcode::CPUID), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::BT), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::PUSH), OperandCode::GS),
    OpcodeRecord(Interpretation::Instruction(Opcode::POP), OperandCode::GS),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::BTS), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::SHRD), OperandCode::Unsupported),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fae),
    OpcodeRecord(Interpretation::Instruction(Opcode::IMUL), OperandCode::Gv_Ev),

// 0xb0
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::CMPXCHG), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_b), OperandCode::Gv_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVZX_w), OperandCode::Gv_Ew),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::ModRM_0x0fba),
    OpcodeRecord(Interpretation::Instruction(Opcode::BTC), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSF), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::BSR), OperandCode::Gv_Ev),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_b), OperandCode::Gv_Eb),
    OpcodeRecord(Interpretation::Instruction(Opcode::MOVSX_w), OperandCode::Gv_Ew),

// 0xc0
    OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Eb_Gb),
    OpcodeRecord(Interpretation::Instruction(Opcode::XADD), OperandCode::Ev_Gv),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
fn read_opcode_0f_map<T: Iterator<Item=u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Option<OpcodeRecord> {
    match bytes_iter.next() {
        Some(b) => {
            instruction.length += 1;
            let record = OPCODE_0F_MAP[b as usize];
            if let Interpretation::Instruction(opc) = record.0 {
                instruction.opcode = opc;
            } else {
                unsafe { unreachable_unchecked(); }
            }
            Some(record)
        }
        None => {
            None
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Interpretation {
    Instruction(Opcode),
    Prefix,
}

#[derive(Copy, Clone)]
// this should be a 32-byte struct..
struct OpcodeRecord(Interpretation, OperandCode);

#[test]
fn opcode_record_size() {
    assert_eq!(std::mem::size_of::<OpcodeRecord>(), 2);
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
    OpcodeRecord(Interpretation::Instruction(Opcode::JO), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNO), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JB), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNB), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JZ), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNZ), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNA), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JA), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JS), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNS), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JP), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JNP), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JL), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JGE), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JLE), OperandCode::Jbs),
    OpcodeRecord(Interpretation::Instruction(Opcode::JG), OperandCode::Jbs),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::CBW), OperandCode::AX_AL),
    OpcodeRecord(Interpretation::Instruction(Opcode::CBW), OperandCode::DX_AX),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
    OpcodeRecord(Interpretation::Instruction(Opcode::Invalid), OperandCode::Nothing),
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
    OpcodeRecord(Interpretation::Instruction(Opcode::JMP), OperandCode::Jbs),
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
fn read_E<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, result: usize, width: u8) -> Result<(), ()> {
    let bank = width_to_gp_reg_bank(width, instr.prefixes.rex().present());
    if modrm >= 0b11000000 {
        read_modrm_reg(bytes_iter, instr, modrm, result, bank)
    } else {
        read_M(bytes_iter, instr, modrm, result)
    }
}
#[allow(non_snake_case)]
fn read_E_xmm<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, result: usize) -> Result<(), ()> {
    if modrm >= 0b11000000 {
        read_modrm_reg(bytes_iter, instr, modrm, result, RegisterBank::X)
    } else {
        read_M(bytes_iter, instr, modrm, result)
    }
}

#[allow(non_snake_case)]
fn read_modrm_reg<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, result: usize, reg_bank: RegisterBank) -> Result<(), ()> {
    instr.modrm_mmm = RegSpec::from_parts(modrm & 7, instr.prefixes.rex().b(), reg_bank);
    instr.operands[result] = OperandSpec::RegMMM;
    Ok(())
}

#[allow(non_snake_case)]
fn read_sib<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, result: usize) -> Result<(), ()> {
    let modbits = (modrm >> 6);
    let addr_width = if instr.prefixes.address_size() { 4 } else { 8 };
    let sibbyte = match bytes_iter.next() {
        Some(b) => b,
        None => { return Err(()); } //Err("Out of bytes".to_string())
    };
    instr.length += 1;

    instr.operands[result] = if (sibbyte & 7) == 0b101 {
        let disp = if modbits == 0b00 {
            read_num(bytes_iter, 4, &mut instr.length)? as i32
        } else if modbits == 0b01 {
            read_num(bytes_iter, 1, &mut instr.length)? as i8 as i32
        } else {
            read_num(bytes_iter, 4, &mut instr.length)? as i32
        };

        if ((sibbyte >> 3) & 7) == 0b100 {
            if modbits == 0b00 && !instr.prefixes.rex().x() {
                instr.disp = DisplacementKind::DisplacementU32(disp as u32);

                OperandSpec::RegDisp
            } else {
                let reg = RegSpec::gp_from_parts(0b100, instr.prefixes.rex().x(), addr_width, instr.prefixes.rex().present());
                instr.modrm_mmm = reg;

                if disp == 0 {
                    OperandSpec::Deref
                } else {
                    instr.disp = DisplacementKind::DisplacementI32(disp);
                    OperandSpec::RegDisp
                }
            }
        } else {
            instr.modrm_mmm = RegSpec::gp_from_parts(5, instr.prefixes.rex().b(), addr_width, instr.prefixes.rex().present());

            instr.sib_index = RegSpec::gp_from_parts((sibbyte >> 3) & 7, instr.prefixes.rex().x(), addr_width, instr.prefixes.rex().present());
            instr.disp = DisplacementKind::DisplacementI32(disp);
            let scale = 1u8 << (sibbyte >> 6);
            instr.scale = scale;

            match (scale, modbits, disp) {
                (0, 0b00, 0) => {
                    OperandSpec::Deref
                },
                (0, 0b00, _) => {
                    OperandSpec::RegDisp
                },
                (0, _, 0) => {
                    OperandSpec::RegIndexBase
                },
                (0, _, _) => {
                    OperandSpec::RegIndexBaseDisp
                },
                (_, 0b00, 0) => {
                    OperandSpec::RegScale
                },
                (_, 0b00, _) => {
                    OperandSpec::RegScaleDisp
                },
                (_, _, 0) => {
                    OperandSpec::RegIndexBaseScale
                },
                (_, _, _) => {
                    OperandSpec::RegIndexBaseScaleDisp
                }
            }
        }
    } else {
        instr.modrm_mmm = RegSpec::gp_from_parts((sibbyte & 7), instr.prefixes.rex().b(), addr_width, instr.prefixes.rex().present());

        let disp = if modbits == 0b00 {
            0
        } else if modbits == 0b01 {
            read_num(bytes_iter, 1, &mut instr.length)? as i8 as i32
        } else {
            read_num(bytes_iter, 4, &mut instr.length)? as i32
        };
        instr.disp = DisplacementKind::DisplacementI32(disp);

        if ((sibbyte >> 3) & 7) == 0b100 {
            if disp == 0 {
                OperandSpec::Deref
            } else {
                OperandSpec::RegDisp
            }
        } else {
            instr.sib_index = RegSpec::gp_from_parts((sibbyte >> 3) & 7, instr.prefixes.rex().x(), addr_width, instr.prefixes.rex().present());
            let scale = 1u8 << (sibbyte >> 6);
            instr.scale = scale;
            if disp == 0 {
                if scale == 0 {
                    OperandSpec::RegIndexBase
                } else {
                    OperandSpec::RegIndexBaseScale
                }
            } else {
                if scale == 0 {
                    OperandSpec::RegIndexBaseDisp
                } else {
                    OperandSpec::RegIndexBaseScaleDisp
                }
            }
        }
    };
    Ok(())
}

#[allow(non_snake_case)]
fn read_M<T: Iterator<Item=u8>>(bytes_iter: &mut T, instr: &mut Instruction, modrm: u8, result: usize) -> Result<(), ()> {
    let modbits = (modrm >> 6);
    let addr_width = if instr.prefixes.address_size() { 4 } else { 8 };
    let mmm = modrm & 7;
    instr.operands[result] = if modbits == 0b11 {
        unsafe { unreachable_unchecked() }
    } else if mmm == 4 {
        return read_sib(bytes_iter, instr, modrm, result);
//         let (ss, index, base) = octets_of(sibbyte);

//            println!("scale: {:b}, index: {:b}, base: {:b}", ss, index, base);
    } else if mmm == 5 && modbits == 0b00 {
        let disp = read_num(bytes_iter, 4, &mut instr.length)? as i32;
        instr.modrm_mmm =
            if addr_width == 8 { RegSpec::rip() } else { RegSpec::eip() };
        instr.disp = DisplacementKind::DisplacementI32(disp);
        OperandSpec::RegDisp
    } else {
        instr.modrm_mmm = RegSpec::gp_from_parts(mmm, instr.prefixes.rex().b(), addr_width, instr.prefixes.rex().present());

        if modbits == 0b00 {
            OperandSpec::Deref
        } else {
            let disp = if modbits == 0b01 {
                read_num(bytes_iter, 1, &mut instr.length)? as i8 as i32
            } else {
                read_num(bytes_iter, 4, &mut instr.length)? as i32
            };
            instr.disp = DisplacementKind::DisplacementI32(disp);
            OperandSpec::RegDisp
        }
    };
    Ok(())
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

pub fn read_instr<T: Iterator<Item=u8>>(mut bytes_iter: T, instruction: &mut Instruction) -> Result<(), ()> {
    let mut alternate_opcode_map: Option<OpcodeMap> = None;
//    use std::intrinsics::unlikely;
    instruction.prefixes = Prefixes::new(0);
    let record: OpcodeRecord = loop {
//    let operand_code = loop {
        match bytes_iter.next() {
            Some(b) => {
                instruction.length += 1;
                let record: u16 = (unsafe { std::mem::transmute::<&'static [OpcodeRecord], &'static [u16]>(&OPCODES[..]) })[b as usize];
                if let Interpretation::Instruction(opcode) = (unsafe { std::mem::transmute::<u16, OpcodeRecord>(record) }).0 {
                    instruction.opcode = opcode;
                    break unsafe { std::mem::transmute::<u16, OpcodeRecord>(record) };
                } else {
                    match b {
                        x if (x & 0xf0 == 0x40) => {
                            // x86_32 inc/dec
                            // x86_64 rex
                            instruction.prefixes.rex_mut().from(x);
                        },
                        0x0f => {
                            if let Some(record) = match alternate_opcode_map {
                                Some(OpcodeMap::Map66) => {
                                    read_opcode_660f_map(&mut bytes_iter, instruction)
                                },
                                Some(OpcodeMap::MapF2) => {
                                    read_opcode_f20f_map(&mut bytes_iter, instruction)
                                },
                                Some(OpcodeMap::MapF3) => {
                                    read_opcode_f30f_map(&mut bytes_iter, instruction)
                                },
                                None => {
                                    read_opcode_0f_map(&mut bytes_iter, instruction)
                                }
                            } {
                                break record;
                            } else {
                                return Err(());
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
//                unsafe { unreachable_unchecked(); }
                return Err(());
            }
        }
    };
    read_operands(bytes_iter, instruction, instruction.prefixes, record.1)
}
pub fn read_operands<T: Iterator<Item=u8>>(mut bytes_iter: T, instruction: &mut Instruction, prefixes: Prefixes, operand_code: OperandCode) -> Result<(), ()> {
    if operand_code == OperandCode::Gv_Ev {
        let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
        let modrm = read_modrm(&mut bytes_iter, instruction)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
        read_E(&mut bytes_iter, instruction, modrm, 1, opwidth)?;
        instruction.modrm_rrr =
            RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
        instruction.operands[0] = OperandSpec::RegRRR;
        instruction.operand_count = 2;
    } else if operand_code == OperandCode::Ev_Gv {
        let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
        let modrm = read_modrm(&mut bytes_iter, instruction)?;

        read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
        instruction.modrm_rrr =
            RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
        instruction.operands[1] = OperandSpec::RegRRR;
        instruction.operand_count = 2;
    } else if operand_code == OperandCode::Jbs {
        // TODO: arch width (8 in 64, 4 in 32, 2 in 16)
        instruction.imm =
            read_imm_signed(&mut bytes_iter, 1, 8, &mut instruction.length)?;
        instruction.operands[0] = OperandSpec::Imm;
        instruction.operand_count = 1;
    } else if operand_code == OperandCode::Gb_Eb {
        let opwidth = 1;
        let modrm = read_modrm(&mut bytes_iter, instruction)?;

        read_E(&mut bytes_iter, instruction, modrm, 1, opwidth)?;
        instruction.modrm_rrr =
            RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
        instruction.operands[0] = OperandSpec::RegRRR;
        instruction.operand_count = 2;
    } else if operand_code == OperandCode::Eb_Gb {
        let opwidth = 1;
        let modrm = read_modrm(&mut bytes_iter, instruction)?;

        read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
        instruction.modrm_rrr =
            RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
        instruction.operands[1] = OperandSpec::RegRRR;
        instruction.operand_count = 2;
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
            let opwidth = 1;
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err(()); // Err("Invalid modr/m for opcode 0xc6".to_owned());
            }

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            instruction.operand_count = 1;
        },
        op @ OperandCode::AL_Ob |
        op @ OperandCode::AX_Ov => {
            let opwidth = match op {
                OperandCode::AL_Ob => 1,
                OperandCode::AX_Ov => {
                    imm_width_from_prefixes_64(SizeCode::vqp, prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let _addr_width = if prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if prefixes.address_size() { 2 } else { 4 };
            let imm = read_num(&mut bytes_iter, addr_width, &mut instruction.length)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(0, prefixes.rex().b(), opwidth, prefixes.rex().present());
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.disp = if prefixes.address_size() {
                DisplacementKind::DisplacementU32(imm as u32)
            } else {
                DisplacementKind::DisplacementU64(imm)
            };
            instruction.operands[1] = OperandSpec::Disp;
            instruction.operand_count = 2;
        }
        op @ OperandCode::Ob_AL |
        op @ OperandCode::Ov_AX => {
            let opwidth = match op {
                OperandCode::Ob_AL => 1,
                OperandCode::Ov_AX => {
                    imm_width_from_prefixes_64(SizeCode::vqp, prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let _addr_width = if prefixes.address_size() { 4 } else { 8 };
            // stupid RCT thing:
            let addr_width = if prefixes.address_size() { 2 } else { 4 };
            let imm = read_num(&mut bytes_iter, addr_width, &mut instruction.length)?;
            instruction.disp = if prefixes.address_size() {
                DisplacementKind::DisplacementU32(imm as u32)
            } else {
                DisplacementKind::DisplacementU64(imm)
            };
            instruction.operands[0] = OperandSpec::Disp;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(0, prefixes.rex().b(), opwidth, prefixes.rex().present());
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        op @ OperandCode::ModRM_0x80_Eb_Ib |
        op @ OperandCode::ModRM_0x81_Ev_Ivs => {
            let opwidth = match op {
                OperandCode::ModRM_0x80_Eb_Ib => 1,
                OperandCode::ModRM_0x81_Ev_Ivs => {
                    imm_width_from_prefixes_64(SizeCode::vqp, prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            instruction.imm = read_imm_signed(&mut bytes_iter, if opwidth == 8 { 4 } else { opwidth }, opwidth, &mut instruction.length)?;
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        },
        op @ OperandCode::ModRM_0xc0_Eb_Ib |
        op @ OperandCode::ModRM_0xc1_Ev_Ib => {
            let opwidth = match op {
                OperandCode::ModRM_0xc0_Eb_Ib => 1,
                OperandCode::ModRM_0xc1_Ev_Ib => {
                    imm_width_from_prefixes_64(SizeCode::vqp, prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            let num = read_num(&mut bytes_iter, 1, &mut instruction.length)? as i8;
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            instruction.imm = ImmediateKind::ImmediateI8(num);
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        },
        op @ OperandCode::ModRM_0xc6_Eb_Ib |
        op @ OperandCode::ModRM_0xc7_Ev_Iv => {
            let opwidth = match op {
                OperandCode::ModRM_0xc6_Eb_Ib => 1,
                OperandCode::ModRM_0xc7_Ev_Iv => {
                    imm_width_from_prefixes_64(SizeCode::vqp, prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            if (modrm & 0b00111000) != 0 {
                instruction.opcode = Opcode::Invalid;
                return Err(()); // Err("Invalid modr/m for opcode 0xc7".to_string());
            }

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            instruction.opcode = Opcode::MOV;
            instruction.imm = read_imm_signed(&mut bytes_iter, if opwidth == 8 { 4 } else { opwidth }, opwidth, &mut instruction.length)?;
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        },
        op @ OperandCode::ModRM_0xd0_Eb_1 |
        op @ OperandCode::ModRM_0xd1_Ev_1 => {
            let opwidth = match op {
                OperandCode::ModRM_0xd0_Eb_1 => 1,
                OperandCode::ModRM_0xd1_Ev_1 => {
                    imm_width_from_prefixes_64(SizeCode::vqp, prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            instruction.opcode = BITWISE_OPCODE_MAP[((modrm >> 3) & 7) as usize].clone();
            instruction.imm = ImmediateKind::ImmediateI8(1);
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        },
        OperandCode::ModRM_0xd3_Ev_CL => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let (mod_bits, r, m) = octets_of(modrm);

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            let opcode = BITWISE_OPCODE_MAP[r as usize].clone();
            instruction.opcode = opcode;
            instruction.modrm_rrr = RegSpec::cl();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        op @ OperandCode::ModRM_0xf6 |
        op @ OperandCode::ModRM_0xf7 => {
            let opwidth = match op {
                OperandCode::ModRM_0xf6 => 1,
                OperandCode::ModRM_0xf7 => {
                    imm_width_from_prefixes_64(SizeCode::vqp, prefixes)
                }
                _ => {
                    unsafe { unreachable_unchecked() }
                }
            };
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            instruction.operand_count = 1;
            match ((modrm >> 3) & 7) {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    let numwidth = if opwidth == 8 { 4 } else { opwidth };
                    instruction.imm = read_imm_signed(&mut bytes_iter, numwidth, opwidth, &mut instruction.length)?;
                    instruction.operands[1] = OperandSpec::Imm;
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
            let opwidth = 1;
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
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
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
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
        OperandCode::Ev => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            instruction.operand_count = 1;
        },
        OperandCode::Gb_Eb_Ib => {
            let opwidth = 1;
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let (mod_bits, r, m) = octets_of(modrm);

            let mut ext = vec![Operand::Nothing; 2];

            // TODO
            panic!("oh no, a mul!");
//            read_E(&mut bytes_iter, instruction, modrm, opwidth, &mut ext[0])?;
            /*
            instruction.operands[0] =
                RegSpec::gp_from_parts(r, prefixes.rex().r(), opwidth, prefixes.rex().present());
            read_imm_signed(&mut bytes_iter, 1, 1, &mut instruction.length).map(|imm| {
                ext[1] = imm;
                instruction.operands[1] = Operand::Many(ext);
            })?

            instruction.operand_count = 3;
            */
        }
        OperandCode::Gv_Eb => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 1, opwidth)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        },
        OperandCode::Gv_Ew => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

            read_E(&mut bytes_iter, instruction, modrm, 1, 2)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        },
        OperandCode::Ew_Sw => {
            let opwidth = 2;
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

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
                read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            }
        },
        OperandCode::Sw_Ew => {
            let opwidth = 2;
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

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
                read_M(&mut bytes_iter, instruction, modrm, 1)?;
            }
        },
        OperandCode::Gdq_Ed => {
            let opwidth = 8;
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E(&mut bytes_iter, instruction, modrm, 1, 4 /* opwidth */)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        },
        OperandCode::Gv_M => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E(&mut bytes_iter, instruction, modrm, 1, opwidth)?;
            instruction.modrm_rrr =
                RegSpec::gp_from_parts((modrm >> 3) & 7, prefixes.rex().r(), opwidth, prefixes.rex().present());
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        },
        OperandCode::Gv_Ev_Iv => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let (mod_bits, r, m) = octets_of(modrm);

            let mut ext = vec![Operand::Nothing; 2];

            // TODO
            panic!("oh no, a mul!");
//            read_E(&mut bytes_iter, instruction, modrm, opwidth, &mut ext[0])?;
            /*
            instruction.operands[0] =
                RegSpec::gp_from_parts(r, prefixes.rex().r(), opwidth, prefixes.rex().present());
            read_imm_signed(&mut bytes_iter, if opwidth == 8 { 4 } else { opwidth }, opwidth, &mut instruction.length).map(|imm| {
                ext[1] = imm;
                instruction.operands[1] = Operand::Many(ext);
            })?
            */
        }
        OperandCode::E_G_xmm => {
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E_xmm(&mut bytes_iter, instruction, modrm, 0)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, prefixes.rex().r(), RegisterBank::X);
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        },
        OperandCode::G_E_xmm => {
            let modrm = read_modrm(&mut bytes_iter, instruction)?;

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            read_E_xmm(&mut bytes_iter, instruction, modrm, 1)?;
            instruction.modrm_rrr =
                RegSpec::from_parts((modrm >> 3) & 7, prefixes.rex().r(), RegisterBank::X);
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        },
        op @ OperandCode::Zv_Ivq_R0 |
        op @ OperandCode::Zv_Ivq_R1 |
        op @ OperandCode::Zv_Ivq_R2 |
        op @ OperandCode::Zv_Ivq_R3 |
        op @ OperandCode::Zv_Ivq_R4 |
        op @ OperandCode::Zv_Ivq_R5 |
        op @ OperandCode::Zv_Ivq_R6 |
        op @ OperandCode::Zv_Ivq_R7 => {
            let reg = (op as u8) - (OperandCode::Zv_Ivq_R0 as u8);
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(reg, prefixes.rex().b(), opwidth, prefixes.rex().present());
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.imm =
                read_imm_ivq(&mut bytes_iter, opwidth, &mut instruction.length)?;
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        },
        OperandCode::AL_Ib => {
            instruction.modrm_rrr =
                RegSpec::al();
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, 1, &mut instruction.length)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        }
        OperandCode::AX_Ivd => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(0, false, opwidth, false);
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.imm =
                read_imm_signed(&mut bytes_iter, numwidth, opwidth, &mut instruction.length)?;
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        }
        op @ OperandCode::Zb_Ib_R0 |
        op @ OperandCode::Zb_Ib_R1 |
        op @ OperandCode::Zb_Ib_R2 |
        op @ OperandCode::Zb_Ib_R3 |
        op @ OperandCode::Zb_Ib_R4 |
        op @ OperandCode::Zb_Ib_R5 |
        op @ OperandCode::Zb_Ib_R6 |
        op @ OperandCode::Zb_Ib_R7 => {
            let reg = (op as u8) - (OperandCode::Zb_Ib_R0 as u8);
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(reg, prefixes.rex().b(), 1, prefixes.rex().present());
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, 1, &mut instruction.length)?;
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        },
        OperandCode::Iw => {
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, 2, &mut instruction.length)?;
            instruction.operands[0] = OperandSpec::Imm;
            instruction.operand_count = 1;
        }
        OperandCode::Ibs => {
            instruction.imm =
                read_imm_signed(&mut bytes_iter, 1, 8, &mut instruction.length)?;
            instruction.operands[0] = OperandSpec::Imm;
            instruction.operand_count = 1;
        },
        OperandCode::Ivs => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vd, prefixes);
            instruction.imm =
                read_imm_unsigned(&mut bytes_iter, opwidth, &mut instruction.length)?;
            instruction.operands[0] = OperandSpec::Imm;
            instruction.operand_count = 1;
        },
        OperandCode::ModRM_0x83_Ev_Ibs => {
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, prefixes);

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
            instruction.opcode = base_opcode_map((modrm >> 3) & 7);
            instruction.imm = read_imm_signed(&mut bytes_iter, 1, opwidth, &mut instruction.length)?;
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        },
        op @ OperandCode::Zv_R0 |
        op @ OperandCode::Zv_R1 |
        op @ OperandCode::Zv_R2 |
        op @ OperandCode::Zv_R3 |
        op @ OperandCode::Zv_R4 |
        op @ OperandCode::Zv_R5 |
        op @ OperandCode::Zv_R6 |
        op @ OperandCode::Zv_R7 => {
            let reg = (op as u8) - (OperandCode::Zv_R0 as u8);
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, prefixes);
            instruction.modrm_rrr =
                RegSpec::gp_from_parts(
                    reg, prefixes.rex().b(), opwidth, prefixes.rex().present()
                );
            instruction.operand_count = 1;
            instruction.operands[0] = OperandSpec::RegRRR;
        },
        OperandCode::Jvds => {
            let offset = read_num(&mut bytes_iter, 4, &mut instruction.length)?;
            instruction.imm = ImmediateKind::ImmediateI32(offset as i32);
            instruction.operand_count = 1;
            instruction.operands[0] = OperandSpec::Imm;
        }
        OperandCode::ModRM_0x0f00 => {
            instruction.operand_count = 1;
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
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
                return Ok(());
            } else {
                unreachable!("r <= 8");
            }
            read_E(&mut bytes_iter, instruction, modrm, 0, 2)?;
        }
        OperandCode::ModRM_0x0f01 => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let r = (modrm >> 3) & 7;
            if r == 0 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    panic!("Unsupported instruction: 0x0f01 with modrm: 11 000 ___");
                } else {
                    instruction.opcode = Opcode::SGDT;
                    instruction.operand_count = 1;
                    read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
                }
            } else if r == 1 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    // TOOD: MONITOR
                    instruction.opcode = Opcode::NOP;
                    instruction.operand_count = 0;
                } else {
                    instruction.opcode = Opcode::SIDT;
                    instruction.operand_count = 1;
                    read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
                }
            } else if r == 2 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    // TOOD: XGETBV
                    instruction.opcode = Opcode::NOP;
                    instruction.operand_count = 0;
                } else {
                    instruction.opcode = Opcode::LGDT;
                    instruction.operand_count = 1;
                    read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
                }
            } else if r == 3 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    // TOOD: VMRUN
                    instruction.opcode = Opcode::NOP;
                    instruction.operand_count = 0;
                } else {
                    instruction.opcode = Opcode::LIDT;
                    instruction.operand_count = 1;
                    read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
                }
            } else if r == 4 {
                // TODO: this permits storing only to word-size registers
                // spec suggets this might do something different for f.ex rdi?
                instruction.opcode = Opcode::SMSW;
                instruction.operand_count = 1;
                read_E(&mut bytes_iter, instruction, modrm, 0, 2)?;
            } else if r == 5 {
                panic!("Unsupported instruction: 0x0f01 with modrm: __ 101 ___");
            } else if r == 6 {
                instruction.opcode = Opcode::LMSW;
                instruction.operand_count = 1;
                read_E(&mut bytes_iter, instruction, modrm, 0, 2)?;
            } else if r == 7 {
                let mod_bits = modrm >> 6;
                let m = modrm & 7;
                if mod_bits == 0b11 {
                    if m == 1 {
                        instruction.opcode = Opcode::SWAPGS;
                        instruction.operand_count = 0;
                    } else if m == 2 {
                        instruction.opcode = Opcode::RDTSCP;
                        instruction.operand_count = 0;
                    } else {
                    //    panic!("Unsupported instruction: 0x0f01 with modrm: 11 110 r >= 2");
                        return Err(()); // Err("unsupported 0x0f01 variant".to_string())
                    }
                } else {
                    instruction.opcode = Opcode::INVLPG;
                    instruction.operand_count = 1;
                    read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;
                }
            } else {
                unreachable!("r <= 8");
            }
        }
        OperandCode::ModRM_0x0fae => {
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let r = (modrm >> 3) & 7;
            let mod_bits = modrm >> 6;

            // all the 0b11 instructions are err or no-operands
            if mod_bits == 0b11 {
                instruction.operand_count = 0;
                match r {
                    // invalid rrr for 0x0fae, mod: 11
                    0 | 1 | 2 | 3 | 4 => {
                        return Err(());
                    },
                    5 => {
                        instruction.opcode = Opcode::LFENCE
                    },
                    6 => {
                        instruction.opcode = Opcode::MFENCE
                    },
                    7 => {
                        instruction.opcode = Opcode::SFENCE
                    },
                    _ => { unsafe { unreachable_unchecked() } /* r <=7 */ }
                }
                Ok(())
            } else {
                instruction.operand_count = 1;
                instruction.opcode = [
                    Opcode::FXSAVE,
                    Opcode::FXRSTOR,
                    Opcode::LDMXCSR,
                    Opcode::STMXCSR,
                    Opcode::XSAVE,
                    Opcode::XSTOR,
                    // TODO: radare reports this, but i'm not sure?
                    Opcode::XSAVEOPT,
                    // TODO: radare reports this, but i'm not sure?
                    Opcode::CLFLUSH,
                    Opcode::Invalid,
                ][r as usize];
                read_M(&mut bytes_iter, instruction, modrm, 0)
            }?;
        }
        OperandCode::ModRM_0x0fba => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vq, prefixes);
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
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

            read_E(&mut bytes_iter, instruction, modrm, 0, opwidth)?;

            instruction.imm = read_imm_signed(&mut bytes_iter, 1, 1, &mut instruction.length)?;
            instruction.operands[1] = OperandSpec::Imm;
            instruction.operand_count = 2;
        }

        op @ OperandCode::Rq_Cq_0 |
        op @ OperandCode::Rq_Dq_0 => {
            let bank = match op {
                OperandCode::Rq_Cq_0 => RegisterBank::CR,
                OperandCode::Rq_Dq_0 => RegisterBank::DR,
                _ => unsafe { unreachable_unchecked() }
            };
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let mut m = modrm & 7;
            let mut r = (modrm >> 3) & 7;
            if prefixes.rex().r() {
                r += 0b1000;
            }
            if prefixes.rex().b() {
                m += 0b1000;
            }
            instruction.modrm_rrr =
                RegSpec { bank: RegisterBank::Q, num: m };
            instruction.modrm_mmm =
                RegSpec { bank: bank, num: r };
            instruction.operands[1] = OperandSpec::RegMMM;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        op @ OperandCode::Cq_Rq_0 |
        op @ OperandCode::Dq_Rq_0 => {
            let bank = match op {
                OperandCode::Cq_Rq_0 => RegisterBank::CR,
                OperandCode::Dq_Rq_0 => RegisterBank::DR,
                _ => unsafe { unreachable_unchecked() }
            };
            let modrm = read_modrm(&mut bytes_iter, instruction)?;
            let mut m = modrm & 7;
            let mut r = (modrm >> 3) & 7;
            if prefixes.rex().r() {
                r += 0b1000;
            }
            if prefixes.rex().b() {
                m += 0b1000;
            }
            instruction.modrm_rrr =
                RegSpec { bank: bank, num: r };
            instruction.modrm_mmm =
                RegSpec { bank: RegisterBank::Q, num: m };
            instruction.operands[1] = OperandSpec::RegMMM;
            instruction.operands[0] = OperandSpec::RegRRR;
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
            instruction.imm = ImmediateKind::ImmediateU8(3);
            instruction.operands[0] = OperandSpec::Imm;
            instruction.operand_count = 1;
        }
        OperandCode::Nothing => {
            instruction.operand_count = 0;
        }
        _ => {
            instruction.operand_count = 0;
            instruction.opcode = Opcode::Invalid;
            return Err(()); // Err(format!("unsupported operand code: {:?}", operand_code));
        //    unsafe { unreachable_unchecked(); }
        }
    };
    }

    Ok(())
}

pub fn decode_one<'b, T: IntoIterator<Item=u8>>(bytes: T, instr: &'b mut Instruction) -> Option<()> {
    let mut bytes_iter = bytes.into_iter();
    instr.length = 0;
    read_instr(bytes_iter, instr).ok()
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
fn read_num<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<u64, ()> {
    let mut result = 0u64;
    let mut idx = 0;
    loop {
        if idx == width {
            return Ok(result);
        }
        if let Some(byte) = bytes.next() {
            *length += 1;
            result |= (byte as u64) << (idx * 8);
            idx += 1;
        } else {
            return Err(());
        }
    }
}

#[inline]
fn read_imm_ivq<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<ImmediateKind, ()> {
    match width {
        2 => {
            Ok(ImmediateKind::ImmediateU16(read_num(bytes, 2, length)? as u16))
        },
        4 => {
            Ok(ImmediateKind::ImmediateU32(read_num(bytes, 4, length)? as u32))
        },
        8 => {
            Ok(ImmediateKind::ImmediateU64(read_num(bytes, 8, length)? as u64))
        },
        _ => {
            unsafe { unreachable_unchecked(); }
        }
    }
}

#[inline]
fn read_imm_signed<T: Iterator<Item=u8>>(bytes: &mut T, num_width: u8, extend_to: u8, length: &mut u8) -> Result<ImmediateKind, ()> {
    let num = match num_width {
        1 => read_num(bytes, 1, length)? as i8 as i64,
        2 => read_num(bytes, 2, length)? as i16 as i64,
        4 => read_num(bytes, 4, length)? as i32 as i64,
        8 => read_num(bytes, 4, length)? as i32 as i64,
        _ => { unsafe { unreachable_unchecked() } }
    };

    match extend_to {
        1 => Ok(ImmediateKind::ImmediateI8(num as i8)),
        2 => Ok(ImmediateKind::ImmediateI16(num as i16)),
        4 => Ok(ImmediateKind::ImmediateI32(num as i32)),
        8 => Ok(ImmediateKind::ImmediateI64(num as i64)),
        _ => { unsafe { unreachable_unchecked() } }
    }
}

#[inline]
fn read_imm_unsigned<T: Iterator<Item=u8>>(bytes: &mut T, width: u8, length: &mut u8) -> Result<ImmediateKind, ()> {
    match width {
        1 => {
            Ok(ImmediateKind::ImmediateU8(read_num(bytes, 1, length)? as u8))
        },
        2 => {
            Ok(ImmediateKind::ImmediateU16(read_num(bytes, 2, length)? as u16))
        },
        4 => {
            Ok(ImmediateKind::ImmediateU32(read_num(bytes, 4, length)? as u32))
        },
        8 => {
            Ok(ImmediateKind::ImmediateU64(read_num(bytes, 4, length)? as u64))
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
fn read_modrm<T: Iterator<Item=u8>>(bytes_iter: &mut T, inst: &mut Instruction) -> Result<u8, ()> {
    let modrm = match bytes_iter.next() {
        Some(b) => b,
        // TODO: ...
        None => {
            return Err(());
        }
    };
    inst.length += 1;
    Ok(modrm)
}
