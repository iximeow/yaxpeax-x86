use std::fs::File;
use std::io::Read;

//use std;
use std::fmt;

use std::hint::unreachable_unchecked;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct RegSpec {
    num: u8,
    bank: RegisterBank
}

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
    fn RIP() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::RIP
        }
    }

    #[inline]
    fn EIP() -> RegSpec {
        RegSpec {
            num: 0,
            bank: RegisterBank::EIP
        }
    }
}

impl fmt::Display for RegSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self.bank {
            RegisterBank::Q => {
                ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"][self.num as usize]
            },
            RegisterBank::D => {
                ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"][self.num as usize]
            },
            RegisterBank::W => {
                ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"][self.num as usize]
            },
            RegisterBank::B => {
                ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"][self.num as usize]
            },
            RegisterBank::rB => {
                ["al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"][self.num as usize]
            },
            RegisterBank::EIP => { "eip" },
            RegisterBank::RIP => { "rip" },
            _ => panic!("unnamable register")
        };
        write!(f, "{}", name)
    }
}

enum SizeCode {
    b,
    vd,
    vq,
    vqp
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Operand::ImmediateI16(imm) => write!(f, "0x{:x}", imm),
            &Operand::ImmediateU16(imm) => write!(f, "0x{:x}", imm),
            &Operand::ImmediateU32(imm) => write!(f, "0x{:x}", imm),
            &Operand::ImmediateI32(imm) => write!(f, "0x{:x}", imm),
            &Operand::ImmediateU64(imm) => write!(f, "0x{:x}", imm),
            &Operand::ImmediateI64(imm) => write!(f, "0x{:x}", imm),
            &Operand::Register(ref spec) => write!(f, "{}", spec),
            &Operand::ImmediateI8(imm) => write!(f, "0x{:x}", imm),
            &Operand::ImmediateU8(imm) => write!(f, "0x{:x}", imm),
            &Operand::DisplacementU32(imm) => write!(f, "[0x{:x}]", imm),
            &Operand::DisplacementI32(imm) => write!(f, "[0x{:x}]", imm),
            &Operand::RegDisp(ref spec, ref disp) => write!(f, "[{} + 0x{:x}]",
                spec, disp),
            &Operand::RegDeref(ref spec) => write!(f, "[{}]", spec),
            &Operand::RegScale(ref spec, scale) => write!(f, "[{} * {}]", spec, scale),
            &Operand::RegScaleDisp(ref spec, scale, disp) => {
                write!(f, "[{} * {} + 0x{:x}]", spec, scale, disp)
            },
            &Operand::RegIndexBase(ref base, ref index) => {
                write!(f, "[{} + {}]", base, index)
            }
            &Operand::RegIndexBaseDisp(ref base, ref index, disp) => {
                write!(f, "[{} + {} + 0x{:x}]", base, index, disp)
            },
            &Operand::RegIndexBaseScale(ref base, ref index, scale) => {
                if scale == 1 {
                    write!(f, "[{} + {}]", base, index)
                } else {
                    write!(f, "[{} + {} * {}]", base, index, scale)
                }
            }
            &Operand::RegIndexBaseScaleDisp(ref base, ref index, scale, disp) => {
                if scale == 1 {
                    write!(f, "[{} + {} + {:#x}]", base, index, disp)
                } else {
                    write!(f, "[{} + {} * {} + {:#x}]", base, index, scale, disp)
                }
            },
            &Operand::Nothing => { Ok(()) },
            &Operand::Many(_) => { panic!("many not covered"); }
        }
    }
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
    DisplacementI32(u32),
    DisplacementU32(i32),
    RegDeref(RegSpec),
    RegDisp(RegSpec, i32),
    RegScale(RegSpec, u8),
    RegIndexBase(RegSpec, RegSpec),
    RegIndexBaseDisp(RegSpec, RegSpec, i32),
    RegScaleDisp(RegSpec, u8, i32),
    RegIndexBaseScale(RegSpec, RegSpec, u8),
    RegIndexBaseScaleDisp(RegSpec, RegSpec, u8, i32),
    Many(Vec<Operand>),
    Nothing
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RegisterBank {
    Q, D, W, B, rB, // Quadword, Dword, Word, Byte
    CR, DR, S, EIP, RIP,  // Control reg, Debug reg, Selector, ...
    X, Y, Z,    // XMM, YMM, ZMM
    ST, MM,     // ST, MM regs (x87, mmx)
}

#[derive(Debug)]
pub enum Segment {
    CS, DS, ES, FS, GS, SS
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Opcode::INC => write!(f, "{}", "inc"),
            &Opcode::DEC => write!(f, "{}", "dec"),
            &Opcode::HLT => write!(f, "{}", "hlt"),
            &Opcode::SBB => write!(f, "{}", "sbb"),
            &Opcode::AND => write!(f, "{}", "and"),
            &Opcode::XOR => write!(f, "{}", "xor"),
            &Opcode::OR => write!(f, "{}", "or"),
            &Opcode::PUSH => write!(f, "{}", "push"),
            &Opcode::POP => write!(f, "{}", "pop"),
            &Opcode::LEA => write!(f, "{}", "lea"),
            &Opcode::NOP => write!(f, "{}", "nop"),
            &Opcode::XCHG => write!(f, "{}", "xchg"),
            &Opcode::POPF => write!(f, "{}", "popf"),
            &Opcode::ADD => write!(f, "{}", "add"),
            &Opcode::ADC => write!(f, "{}", "adc"),
            &Opcode::SUB => write!(f, "{}", "sub"),
            &Opcode::INT => write!(f, "{}", "int"),
            &Opcode::INTO => write!(f, "{}", "into"),
            &Opcode::IRET => write!(f, "{}", "iret"),
            &Opcode::RETF => write!(f, "{}", "retf"),
            &Opcode::ENTER => write!(f, "{}", "enter"),
            &Opcode::LEAVE => write!(f, "{}", "leave"),
            &Opcode::MOV => write!(f, "{}", "mov"),
            &Opcode::RETURN => write!(f, "{}", "ret"),
            &Opcode::PUSHF => write!(f, "{}", "pushf"),
            &Opcode::WAIT => write!(f, "{}", "wait"),
            &Opcode::CBW => write!(f, "{}", "cbw"),
            &Opcode::CDW => write!(f, "{}", "cdw"),
            &Opcode::LODS => write!(f, "{}", "lods"),
            &Opcode::STOS => write!(f, "{}", "stos"),
            &Opcode::LAHF => write!(f, "{}", "lahf"),
            &Opcode::SAHF => write!(f, "{}", "sahf"),
            &Opcode::CMPS => write!(f, "{}", "cmps"),
            &Opcode::SCAS => write!(f, "{}", "scas"),
            &Opcode::MOVS => write!(f, "{}", "movs"),
            &Opcode::TEST => write!(f, "{}", "test"),
            &Opcode::CMP => write!(f, "{}", "cmp"),
            &Opcode::INS => write!(f, "{}", "ins"),
            &Opcode::OUTS => write!(f, "{}", "outs"),
            &Opcode::IMUL => write!(f, "{}", "imul"),
            &Opcode::JO => write!(f, "{}", "jo"),
            &Opcode::JNO => write!(f, "{}", "jno"),
            &Opcode::JB => write!(f, "{}", "jb"),
            &Opcode::JNB => write!(f, "{}", "jnb"),
            &Opcode::JZ => write!(f, "{}", "jz"),
            &Opcode::JNZ => write!(f, "{}", "jnz"),
            &Opcode::JA => write!(f, "{}", "ja"),
            &Opcode::JNA => write!(f, "{}", "jna"),
            &Opcode::JS => write!(f, "{}", "js"),
            &Opcode::JNS => write!(f, "{}", "jns"),
            &Opcode::JP => write!(f, "{}", "jp"),
            &Opcode::JNP => write!(f, "{}", "jnp"),
            &Opcode::JL => write!(f, "{}", "jl"),
            &Opcode::JGE => write!(f, "{}", "jge"),
            &Opcode::JLE => write!(f, "{}", "jle"),
            &Opcode::JG => write!(f, "{}", "jg"),
            &Opcode::CALL => write!(f, "{}", "call"),
            &Opcode::JMP => write!(f, "{}", "jmp"),
            &Opcode::CALLF => write!(f, "{}", "callf"),
            &Opcode::JMPF => write!(f, "{}", "jmpf"),
            &Opcode::SAR => write!(f, "{}", "sar"),
            &Opcode::SAL => write!(f, "{}", "sal"),
            &Opcode::SHR => write!(f, "{}", "shr"),
            &Opcode::SHL => write!(f, "{}", "shl"),
            &Opcode::RCR => write!(f, "{}", "rcr"),
            &Opcode::RCL => write!(f, "{}", "rcl"),
            &Opcode::ROR => write!(f, "{}", "ror"),
            &Opcode::ROL => write!(f, "{}", "rol"),
            &Opcode::CMOVA => write!(f, "{}", "cmova"),
            &Opcode::CMOVB => write!(f, "{}", "cmovb"),
            &Opcode::CMOVG => write!(f, "{}", "cmovg"),
            &Opcode::CMOVGE => write!(f, "{}", "cmovge"),
            &Opcode::CMOVL => write!(f, "{}", "cmovl"),
            &Opcode::CMOVLE => write!(f, "{}", "cmovle"),
            &Opcode::CMOVNA => write!(f, "{}", "cmovna"),
            &Opcode::CMOVNB => write!(f, "{}", "cmovnb"),
            &Opcode::CMOVNO => write!(f, "{}", "cmovno"),
            &Opcode::CMOVNP => write!(f, "{}", "cmovnp"),
            &Opcode::CMOVNS => write!(f, "{}", "cmovns"),
            &Opcode::CMOVNZ => write!(f, "{}", "cmovnz"),
            &Opcode::CMOVO => write!(f, "{}", "cmovo"),
            &Opcode::CMOVP => write!(f, "{}", "cmovp"),
            &Opcode::CMOVS => write!(f, "{}", "cmovs"),
            &Opcode::CMOVZ => write!(f, "{}", "cmovz"),
            &Opcode::NEG => write!(f, "{}", "neg"),
            &Opcode::NOT => write!(f, "{}", "not"),
            &Opcode::MUL => write!(f, "{}", "mul"),
            &Opcode::IMUL => write!(f, "{}", "imul"),
            &Opcode::DIV => write!(f, "{}", "div"),
            &Opcode::IDIV => write!(f, "{}", "idiv"),
            &Opcode::CMPXCHG => write!(f, "{}", "cmpxchg"),
            &Opcode::MOVZX_b => write!(f, "{}", "movzx"),
            &Opcode::MOVZX_w => write!(f, "{}", "movzx"),
            &Opcode::MOVSX => write!(f, "{}", "movsx"),
            &Opcode::Invalid => write!(f, "{}", "invalid")
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Opcode {
    MOVZX_b,
    MOVZX_w,
    MOVSX,
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
    Invalid
}
#[derive(Debug)]
pub struct Instruction {
    pub prefixes: Prefixes,
    pub opcode: Opcode,
    pub operands: [Operand; 2]
}

impl Instruction {
    pub fn invalid() -> Instruction {
        Instruction {
            prefixes: Prefixes::new(0),
            opcode: Opcode::Invalid,
            operands: [Operand::Nothing, Operand::Nothing]
        }
    }
    pub fn is_invalid(&self) -> bool {
        match self.opcode {
            Opcode::Invalid => true,
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Prefixes {
    bits: u8,
    rex: PrefixRex,
    segment: Segment,
    lock: bool
}

#[derive(Debug)]
pub struct PrefixRex {
    bits: u8
}

impl Prefixes {
    fn new(bits: u8) -> Prefixes {
        Prefixes {
            bits: bits,
            rex: PrefixRex { bits: 0 },
            segment: Segment::DS,
            lock: false
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
    fn lock(&self) -> bool { self.lock }
    #[inline]
    fn set_lock(&mut self) { self.lock = true; }
    #[inline]
    fn operand_size(&self) -> bool { self.bits & 0x1 == 1 }
    #[inline]
    fn set_operand_size(&mut self) { self.bits = self.bits | 0x1 }
    #[inline]
    fn address_size(&self) -> bool { self.bits & 0x2 == 2 }
    #[inline]
    fn set_address_size(&mut self) { self.bits = self.bits | 0x2 }
    #[inline]
    fn cs(&self) -> bool { self.bits & 0x70 == 0x10 }
    #[inline]
    fn set_cs(&mut self) { self.bits = (self.bits & 0x8f) | 0x10 }
    #[inline]
    fn ds(&self) -> bool { self.bits & 0x70 == 0x20 }
    #[inline]
    fn set_ds(&mut self) { self.bits = (self.bits & 0x8f) | 0x20 }
    #[inline]
    fn es(&self) -> bool { self.bits & 0x70 == 0x30 }
    #[inline]
    fn set_es(&mut self) { self.bits = (self.bits & 0x8f) | 0x30 }
    #[inline]
    fn fs(&self) -> bool { self.bits & 0x70 == 0x40 }
    #[inline]
    fn set_fs(&mut self) { self.bits = (self.bits & 0x8f) | 0x40 }
    #[inline]
    fn gs(&self) -> bool { self.bits & 0x70 == 0x50 }
    #[inline]
    fn set_gs(&mut self) { self.bits = (self.bits & 0x8f) | 0x50 }
    #[inline]
    fn ss(&self) -> bool { self.bits & 0x70 == 0x60 }
    #[inline]
    fn set_ss(&mut self) { self.bits = (self.bits & 0x8f) | 0x60 }
    #[inline]
    fn rex(&self) -> &PrefixRex { &self.rex }
    #[inline]
    fn rex_mut(&mut self) -> &mut PrefixRex { &mut self.rex }
}

impl PrefixRex {
    #[inline]
    fn set_present(&mut self) { self.bits |= 0x10 }
    #[inline]
    fn present(&self) -> bool { (self.bits & 0x10) == 0x10 }
    #[inline]
    fn set_b(&mut self) { self.bits |= 0x01 }
    #[inline]
    fn b(&self) -> bool { (self.bits & 0x01) == 0x01 }
    #[inline]
    fn set_x(&mut self) { self.bits |= 0x02 }
    #[inline]
    fn x(&self) -> bool { (self.bits & 0x02) == 0x02 }
    #[inline]
    fn set_r(&mut self) { self.bits |= 0x04 }
    #[inline]
    fn r(&self) -> bool { (self.bits & 0x04) == 0x04 }
    #[inline]
    fn set_w(&mut self) { self.bits |= 0x08 }
    #[inline]
    fn w(&self) -> bool { (self.bits & 0x08) == 0x08 }
    #[inline]
    fn from(&mut self, prefix: u8) {
        self.bits = prefix & 0x0f;
        self.set_present();
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.prefixes.lock {
            write!(f, "lock ");
        }
        write!(f, "{}", self.opcode);
        match &self.operands[0] {
            &Operand::Nothing => {
                return Ok(());
            },
            x @ &_ => {
                write!(f, " {}", x)
            }
        };
        match self.opcode {
            Opcode::MOVZX_b => {
                match &self.operands[1] {
                    &Operand::Nothing => {
                        return Ok(());
                    },
                    x @ &Operand::Register(_) => {
                        write!(f, ", {}", x)
                    }
                    x @ _ => {
                        write!(f, ", byte {}", x)
                    }
                }
            },
            Opcode::MOVZX_w => {
                match &self.operands[1] {
                    &Operand::Nothing => {
                        return Ok(());
                    },
                    x @ &Operand::Register(_) => {
                        write!(f, ", {}", x)
                    }
                    x @ _ => {
                        write!(f, ", word {}", x)
                    }
                }
            },
            _ => {
                match &self.operands[1] {
                    &Operand::Nothing => {
                        return Ok(());
                    },
                    x @ &_ => {
                        write!(f, ", {}", x)
                    }
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum OperandCode {
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
    Ev_Ivs,
    Ev,
    Ew_Sw,
    Fw,
    Gb_Eb,
    Gv_Eb,
    Gv_Ew,
    Gv_Ev,
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

fn read_opcode_0f_map<'a, T: Iterator<Item=&'a u8>>(bytes_iter: &mut T, instruction: &mut Instruction, prefixes: Prefixes) -> Result<OperandCode, String> {
    match bytes_iter.next() {
        Some(b) => {
            match *b {
                0x1f => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::NOP;
                    Ok(OperandCode::Ev)
                },
                0x40 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVO;
                    Ok(OperandCode::Gv_Ev)
                },
                0x41 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVNO;
                    Ok(OperandCode::Gv_Ev)
                },
                0x42 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVB;
                    Ok(OperandCode::Gv_Ev)
                },
                0x43 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVNB;
                    Ok(OperandCode::Gv_Ev)
                },
                0x44 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVZ;
                    Ok(OperandCode::Gv_Ev)
                },
                0x45 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVNZ;
                    Ok(OperandCode::Gv_Ev)
                },
                0x46 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVNA;
                    Ok(OperandCode::Gv_Ev)
                },
                0x47 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVA;
                    Ok(OperandCode::Gv_Ev)
                },
                0x48 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVS;
                    Ok(OperandCode::Gv_Ev)
                },
                0x49 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVNS;
                   Ok(OperandCode::Gv_Ev)
                },
                0x4a => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVP;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4b => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVNP;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4c => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVL;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4d => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVGE;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4e => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVLE;
                    Ok(OperandCode::Gv_Ev)
                },
                0x4f => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMOVG;
                    Ok(OperandCode::Gv_Ev)
                },
                0x80 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JO;
                    Ok(OperandCode::Jvds)
                },
                0x81 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JNO;
                    Ok(OperandCode::Jvds)
                },
                0x82 => {
                    instruction.prefixes = prefixes;
                   instruction.opcode = Opcode::JB;
                    Ok(OperandCode::Jvds)
                },
                0x83 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JNB;
                    Ok(OperandCode::Jvds)
                },
                0x84 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JZ;
                    Ok(OperandCode::Jvds)
                },
                0x85 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JNZ;
                    Ok(OperandCode::Jvds)
                },
                0x86 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JNA;
                    Ok(OperandCode::Jvds)
                },
                0x87 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JA;
                    Ok(OperandCode::Jvds)
                },
                0x88 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JS;
                    Ok(OperandCode::Jvds)
                },
                0x89 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JNS;
                    Ok(OperandCode::Jvds)
                },
                0x8a => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JP;
                    Ok(OperandCode::Jvds)
                },
                0x8b => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JNP;
                    Ok(OperandCode::Jvds)
                },
                0x8c => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JL;
                    Ok(OperandCode::Jvds)
                },
                0x8d => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JGE;
                    Ok(OperandCode::Jvds)
                },
                0x8e => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JLE;
                    Ok(OperandCode::Jvds)
                },
                0x8f => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::JG;
                    Ok(OperandCode::Jvds)
                },
                0xb0 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMPXCHG;
                    Ok(OperandCode::Eb_Gb)
                },
                0xb1 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::CMPXCHG;
                    Ok(OperandCode::Ev_Gv)
                }
                0xb6 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::MOVZX_b;
                    Ok(OperandCode::Gv_Eb)
                },
                0xb7 => {
                    instruction.prefixes = prefixes;
                    instruction.opcode = Opcode::MOVZX_w;
                    Ok(OperandCode::Gv_Ew)
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

fn read_opcode<'a, T: Iterator<Item=&'a u8>>(bytes_iter: &mut T, instruction: &mut Instruction) -> Result<OperandCode, String> {
    use std::hint::unreachable_unchecked;
//    use std::intrinsics::unlikely;
    let mut reading = true;
    let mut prefixes = Prefixes::new(0);
    let mut two_byte_map = false;
    loop {
        match bytes_iter.next() {
            Some(b) => {
                match *b {
                    x if x < 0x40 => {
                        if x % 8 > 5 { // unsafe { unlikely(x % 8 > 5) } {
                            // for x86_32 this is push/pop prefixes and 0x0f escape
                            // for x86_64 this is mostly invalid
                            match x {
                                0x0f => {
                                    return read_opcode_0f_map(bytes_iter, instruction, prefixes);
                                },
                                0x26 => {
                                    prefixes.set_es()
                                },
                                0x2e => {
                                    prefixes.set_cs()
                                },
                                0x36 => {
                                    prefixes.set_ss()
                                },
                                0x3e => {
                                    prefixes.set_ds()
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
                                | 0x3f => {
                                    instruction.prefixes = prefixes;
                                    instruction.opcode = Opcode::Invalid;
                                    return Ok(OperandCode::Nothing);
                                },
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
                        ][(x % 8) as usize].clone();

                        instruction.prefixes = prefixes;
                        instruction.opcode = op;
                        return Ok(operand_code);
                    },
                    x if x < 0x50 => {
                        // x86_32 inc/dec
                        // x86_64 rex
                        prefixes.rex_mut().from(x);
                    },
                    x if x < 0x60 => {
                        let op = if x < 0x58 { Opcode::PUSH } else { Opcode::POP };
                        instruction.prefixes = prefixes;
                        instruction.opcode = op;
                        return Ok(OperandCode::Zv(x));
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
                    0x68 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::PUSH;
                        return Ok(OperandCode::Ivs);
                    },
                    0x69 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::IMUL;
                        return Ok(OperandCode::Gv_Ev_Iv);
                    },
                    0x6a => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::PUSH;
                        return Ok(OperandCode::Ibs);
                    },
                    0x6b => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::IMUL;
                        return Ok(OperandCode::Gb_Eb_Ib);
                    },
                    0x6c => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::INS;
                        return Ok(OperandCode::Yb_DX);
                    },
                    0x6d => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::INS;
                        return Ok(OperandCode::Yv_DX);
                    },
                    0x6e => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::OUTS;
                        return Ok(OperandCode::DX_Xb);
                    },
                    0x6f => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::OUTS;
                        return Ok(OperandCode::DX_Xv);
                    },
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
                        instruction.prefixes = prefixes;
                        instruction.opcode = op;
                        return Ok(OperandCode::Jbs);
                    }
                    x if x < 0x84 => {
                        // oh right op depends on modrm....
                        if x == 0x82 {
                            return (Err("invalid opcode".to_owned()));
                        } else {
                            let op = Opcode::Invalid;
                            let operand = if x == 0x80 {
                                OperandCode::ModRM_0x80_Eb_Ib
                            } else if x == 0x81 {
                                OperandCode::ModRM_0x81_Ev_Ivs
                            } else {
                                OperandCode::ModRM_0x83_Ev_Ibs
                            };
                            instruction.prefixes = prefixes;
                            instruction.opcode = op;
                            return Ok(operand);
                        }
                    },
                    0x84 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::TEST;
                        return Ok(OperandCode::Eb_Gb);
                    },
                    0x85 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::TEST;
                        return Ok(OperandCode::Ev_Gv);
                    },
                    0x86 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::XCHG;
                        return Ok(OperandCode::Gb_Eb);
                    },
                    0x87 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::XCHG;
                        return Ok(OperandCode::Gv_Ev);
                    },
                    0x88 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Eb_Gb);
                    }
                    0x89 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Ev_Gv);
                    }
                    0x8a => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Gb_Eb);
                    }
                    0x8b => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Gv_Ev);
                    }
                    0x8c => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Ew_Sw);
                    }
                    0x8d => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::LEA;
                        return Ok(OperandCode::Gv_M);
                    }
                    0x8e => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Sw_Ew);
                    },
                    0x8f => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(OperandCode::ModRM_0x8f_Ev);
                    },
                    0x90 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::NOP;
                        return Ok(OperandCode::Nothing);
                    },
                    x if x < 0x98 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::XCHG;
                        return Ok(OperandCode::Zv_AX(x));
                    },
                    0x98 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::CBW;
                        return Ok(OperandCode::AX_AL);
                    },
                    0x99 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::CBW;
                        return Ok(OperandCode::DX_AX);
                    },
                    0x9a => { return Err("invalid opcode".to_owned()); },
                    0x9b => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::WAIT;
                        return Ok(OperandCode::Nothing);
                    }
                    0x9c => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::PUSHF;
                        return Ok(OperandCode::Fw);
                    },
                    0x9d => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::POPF;
                        return Ok(OperandCode::Fw);
                    },
                    0x9e => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::SAHF;
                        return Ok(OperandCode::AH);
                    },
                    0x9f => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::LAHF;
                        return Ok(OperandCode::AH);
                    },
                    0xa0 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::AL_Ob);
                    },
                    0xa1 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::AX_Ov);
                    },
                    0xa2 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Ob_AL);
                    },
                    0xa3 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::Ov_AX);
                    },
                    0xa4 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOVS;
                        return Ok(OperandCode::Yb_Xb);
                    },
                    0xa5 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOVS;
                        return Ok(OperandCode::Yv_Xv);
                    },
                    0xa6 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::CMPS;
                        return Ok(OperandCode::Yb_Xb);
                    },
                    0xa7 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::CMPS;
                        return Ok(OperandCode::Yv_Xv);
                    },
                    0xa8 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::TEST;
                        return Ok(OperandCode::AL_Ib);
                    },
                    0xa9 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::TEST;
                        return Ok(OperandCode::AX_Ivd);
                    },
                    0xaa => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::STOS;
                        return Ok(OperandCode::Yb_AL);
                    },
                    0xab => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::STOS;
                        return Ok(OperandCode::Yv_AX);
                    },
                    0xac => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::LODS;
                        return Ok(OperandCode::AL_Xb);
                    },
                    0xad => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::LODS;
                        return Ok(OperandCode::AX_Xv);
                    },
                    0xae => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::SCAS;
                        return Ok(OperandCode::Yb_AL);
                    },
                    0xaf => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::SCAS;
                        return Ok(OperandCode::Yv_AX);
                    },
                    x if x < 0xc0 => {
                        let operand = if x < 0xb8 {
                            OperandCode::Zb_Ib(x)
                        } else {
                            OperandCode::Zv_Ivq(x)
                        };
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(operand);
                    },
                    0xc0 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(OperandCode::ModRM_0xc0_Eb_Ib);
                    },
                    0xc1 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(OperandCode::ModRM_0xc1_Ev_Ib);
                    },
                    0xc2 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::RETURN;
                        return Ok(OperandCode::Iw);
                    },
                    0xc3 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::RETURN;
                        return Ok(OperandCode::Nothing);
                    },
                    0xc4  | 0xc5 => {
                        return Err("invalid opcode".to_owned());
                    },
                    0xc6 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::ModRM_0xc6_Eb_Ib);
                    },
                    0xc7 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::MOV;
                        return Ok(OperandCode::ModRM_0xc7_Ev_Iv);
                    },
                    0xc8 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::ENTER;
                        return Ok(OperandCode::Iw_Ib);
                    },
                    0xc9 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::LEAVE;
                        return Ok(OperandCode::Nothing);
                    },
                    0xca => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::RETF;
                        return Ok(OperandCode::Iw);
                    }
                    0xcb => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::RETF;
                        return Ok(OperandCode::Nothing);
                    }
                    0xcc => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::INT;
                        return Ok(OperandCode::I_3);
                    },
                    0xcd => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::INT;
                        return Ok(OperandCode::Ib);
                    },
                    0xce => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::INTO;
                        return Ok(OperandCode::Fw);
                    },
                    0xcf => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::IRET;
                        return Ok(OperandCode::Fw);
                    },
                    x if x < 0xd4 => {
                        let operand = [
                            OperandCode::ModRM_0xd0_Eb_1,
                            OperandCode::ModRM_0xd1_Ev_1,
                            OperandCode::ModRM_0xd2_Eb_CL,
                            OperandCode::ModRM_0xd3_Ev_CL
                        ][(x & 0x3) as usize].clone();
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(operand);
                    },
                    // TODO: GAP
                    0xe8 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::CALL;
                        return Ok(OperandCode::Jvds);
                    },
                    0xe9 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::JMP;
                        return Ok(OperandCode::Jvds);
                    },
                    0xeb => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::JMP;
                        return Ok(OperandCode::Jbs);
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
                    0xf4 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::HLT;
                        return Ok(OperandCode::Nothing);
                    },
                    0xf6 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(OperandCode::ModRM_0xf6);
                    },
                    0xf7 => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(OperandCode::ModRM_0xf7);
                    },
                    0xfe => {
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(OperandCode::ModRM_0xfe_Eb);
                    },
                    0xff => {
                        // TODO: test 0xff /3
                        instruction.prefixes = prefixes;
                        instruction.opcode = Opcode::Invalid;
                        return Ok(OperandCode::ModRM_0xff_Ev);
                    },
                    _ => {
                        return Err("unsupported opcode".to_owned());
                    }
                }
            },
            None => {
                return Err("no more bytes".to_owned());
            }
        }
    }
}

fn read_E<'a, T: Iterator<Item=&'a u8>>(bytes_iter: &mut T, prefixes: &Prefixes, m: u8, modbits: u8, width: u8, result: &mut Operand) -> Result<(), String> {
    let addr_width = if prefixes.address_size() { 4 } else { 8 };
    if modbits == 0b11 {
        *result = Operand::Register(RegSpec::gp_from_parts(m, prefixes.rex().b(), width, prefixes.rex().present()))
    } else if m == 5 && modbits == 0b00 {
        let disp = read_num(bytes_iter, 4);
        *result = Operand::RegDisp(
            if addr_width == 8 { RegSpec::RIP() } else { RegSpec::EIP() },
            disp as i32
        );
    } else if m == 4 {
        let sibbyte = *bytes_iter.next().unwrap();
        let (ss, index, base) = octets_of(sibbyte);

//            println!("scale: {:b}, index: {:b}, base: {:b}", ss, index, base);

        if base == 0b101 {
            let disp = if modbits == 0b00 {
                read_num(bytes_iter, 4)
            } else if modbits == 0b01 {
                read_num(bytes_iter, 1)
            } else {
                read_num(bytes_iter, 4)
            };

            if index == 0b100 {
                if modbits == 0b00 && !prefixes.rex().x() {
                    *result = Operand::DisplacementU32(disp as i32);
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
                read_num(bytes_iter, 1)
            } else {
                read_num(bytes_iter, 4)
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
                    *result = Operand::RegIndexBaseScale(base_reg, index_reg, 1u8 << ss);
                } else {
                    *result = Operand::RegIndexBaseScaleDisp(base_reg, index_reg, 1u8 << ss, disp as i32);
                }
            }
        }
    } else {
        let reg = RegSpec::gp_from_parts(m, prefixes.rex().b(), addr_width, prefixes.rex().present());

        if modbits == 0b00 {
            *result = Operand::RegDeref(reg);
        } else {
            let disp_width = if modbits == 0b01 { 1 } else { 4 };
            let disp = read_num(bytes_iter, disp_width) as i32;
            *result = Operand::RegDisp(reg, disp);
        }
    }
    Ok(())
}

#[inline]
fn read_operands<'a, T: Iterator<Item=&'a u8>>(
    bytes_iter: &mut T,
    instruction: &mut Instruction,
    operand_code: OperandCode
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
        OperandCode::ModRM_0x80_Eb_Ib => {
            let opwidth = 1;
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1) as i8;
                    let opcode = BASE_OPCODE_MAP[r as usize].clone();
                    instruction.opcode = opcode;
                    instruction.operands[1] = Operand::ImmediateI8(num);
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0xc0_Eb_Ib => {
            let opwidth = 1;
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1) as i8;
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1) as i8;
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => {
                    let num = read_num(bytes_iter, 1) as i8;
                    if r != 0 {
                        return (Err("modrm invalid".to_owned()));
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(e_op) => {
                    match read_imm_unsigned(bytes_iter, opwidth) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
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
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);
            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => { },
                Err(reason) => { return Err(reason); }
            };
            match r {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    match read_imm_signed(bytes_iter, 1, opwidth) {
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
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);
            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => { },
                Err(reason) => { return Err(reason); }
            };
            match r {
                0 | 1 => {
                    instruction.opcode = Opcode::TEST;
                    let numwidth = if opwidth == 8 { 4 } else { opwidth };
                    match read_imm_signed(bytes_iter, numwidth, opwidth) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => {
                    instruction.operands[1] = Operand::Nothing;
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Eb_Gb => {
            let opwidth = 1;
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1]) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1]) {
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
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, 2, &mut instruction.operands[1]) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        // TODO: verify M
        OperandCode::Gv_Ev | OperandCode::Gv_M => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            // TODO: ...
            let modrm = bytes_iter.next().unwrap();
            let (mod_bits, r, m) = octets_of(*modrm);

//                println!("mod_bits: {:2b}, r: {:3b}, m: {:3b}", mod_bits, r, m);
            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[1]) {
                Ok(()) => {
                    instruction.operands[0] =
                        Operand::Register(RegSpec::gp_from_parts(r, instruction.prefixes.rex().r(), opwidth, instruction.prefixes.rex().present()));
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::Zv_Ivq(opcode_byte) => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let reg_idx = opcode_byte & 0x7;
            match read_imm_ivq(bytes_iter, opwidth) {
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
        OperandCode::AX_Ivd => {
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);
            let numwidth = if opwidth == 8 { 4 } else { opwidth };
            match read_imm_signed(bytes_iter, numwidth, opwidth) {
                Ok(imm) => {
                    instruction.operands = [
                        Operand::Register(RegSpec::gp_from_parts(0, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present())),
                        imm
                    ];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }
        OperandCode::Zb_Ib(opcode_byte) => {
            let reg_idx = opcode_byte & 0x7;
            match read_imm_unsigned(bytes_iter, 1) {
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
            match read_imm_unsigned(bytes_iter, 2) {
                Ok(imm) => {
                    instruction.operands = [imm, Operand::Nothing];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        }
        OperandCode::Jbs => {
            // TODO: arch width (8 in 64, 4 in 32, 2 in 16)
            match read_imm_signed(bytes_iter, 1, 8) {
                Ok(imm) => {
                    instruction.operands = [imm, Operand::Nothing];
                    Ok(())
                },
                Err(reason) => Err(reason)
            }
        },
        OperandCode::ModRM_0x83_Ev_Ibs => {
            let modrm = bytes_iter.next().unwrap();
            let opwidth = imm_width_from_prefixes_64(SizeCode::vqp, &instruction.prefixes);

            let (mod_bits, r, m) = octets_of(*modrm);

            match read_E(bytes_iter, &instruction.prefixes, m, mod_bits, opwidth, &mut instruction.operands[0]) {
                Ok(()) => {
                    let opcode = BASE_OPCODE_MAP[r as usize].clone();

                    instruction.opcode = opcode;
                    match read_imm_signed(bytes_iter, 1, opwidth) {
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
            let index = (opcode_byte & 0b111) + if instruction.prefixes.rex().b() { 0b1000 } else { 0 };
            instruction.operands = [Operand::Register(
                RegSpec::gp_from_parts(
                    opcode_byte & 0b111, instruction.prefixes.rex().b(), opwidth, instruction.prefixes.rex().present()
                )
            ), Operand::Nothing];
            Ok(())
        },
        OperandCode::Jbs => {
            let offset = read_num(bytes_iter, 1);
            instruction.operands = [Operand::ImmediateI8(offset as i8), Operand::Nothing];
            Ok(())
        },
        OperandCode::Jvds => {
            let offset = read_num(bytes_iter, 4);
            instruction.operands = [Operand::ImmediateI32(offset as i32), Operand::Nothing];
            Ok(())
        }
        OperandCode::Nothing => {
            instruction.operands = [Operand::Nothing, Operand::Nothing];
            Ok(())
        }
        _ => {
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

pub fn decode_one<'a, 'b, T: IntoIterator<Item=&'a u8>>(bytes: T, instr: &'b mut Instruction) -> Option<()> {
    let mut bytes_iter = bytes.into_iter();
    match read_opcode(&mut bytes_iter, instr) {
        Ok(operand_code) => {
            match read_operands(&mut bytes_iter, instr, operand_code) {
                Ok(()) => {
                    Some(())
                },
                Err(reason) => {
//                    panic!("Decode error on operand: {:?}", reason);
       //             println!("Invalid instruction: {}", reason);
//                        return Instruction::invalid()
                    None
                }
            }
        }
        Err(reason) => {
//            panic!("Decode error on opcode: {:?}", reason);
        //    println!("Invalid instruction: {}", reason);
//                return Instruction::invalid()
            None
        }
    }
}

#[inline]
fn read_num<'a, T: Iterator<Item=&'a u8>>(bytes: &mut T, width: u8) -> u64 {
    let mut result = 0u64;
    let mut idx = 0;
    loop {
        if idx == width {
            return result;
        }
        let byte = *bytes.next().unwrap();
        result |= (byte as u64) << (idx * 8);
        idx += 1;
    }
}

#[inline]
fn read_imm_ivq<'a, T: Iterator<Item=&'a u8>>(bytes: &mut T, width: u8) -> Result<Operand, String> {
    match width {
        2 => {
            Ok(Operand::ImmediateU16(read_num(bytes, 2) as u16))
        },
        4 => {
            Ok(Operand::ImmediateU32(read_num(bytes, 4) as u32))
        },
        8 => {
            Ok(Operand::ImmediateU64(read_num(bytes, 8) as u64))
        },
        _ => {
            unsafe { unreachable_unchecked(); }
        }
    }
}

#[inline]
fn read_imm_signed<'a, T: Iterator<Item=&'a u8>>(bytes: &mut T, num_width: u8, extend_to: u8) -> Result<Operand, String> {
    let num = match num_width {
        1 => read_num(bytes, 1) as i8 as i64,
        2 => read_num(bytes, 2) as i16 as i64,
        4 => read_num(bytes, 4) as i32 as i64,
        8 => read_num(bytes, 4) as i32 as i64,
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
fn read_imm_unsigned<'a, T: Iterator<Item=&'a u8>>(bytes: &mut T, width: u8) -> Result<Operand, String> {
    match width {
        1 => {
            Ok(Operand::ImmediateU8(read_num(bytes, 1) as u8))
        },
        2 => {
            Ok(Operand::ImmediateU16(read_num(bytes, 2) as u16))
        },
        4 => {
            Ok(Operand::ImmediateU32(read_num(bytes, 4) as u32))
        },
        8 => {
            Ok(Operand::ImmediateU64(read_num(bytes, 4) as u64))
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
