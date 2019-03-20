extern crate yaxpeax_arch;
extern crate termion;

use termion::color;

use std::fmt;

use std::hint::unreachable_unchecked;

use yaxpeax_arch::{Arch, Colorize, ColorSettings, Decodable, LengthedInstruction, ShowContextual};

use ::{RegSpec, RegisterBank, Opcode, Operand, Instruction};

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

impl fmt::Display for Operand {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.colorize(None, fmt)
    }
}

impl <T: std::fmt::Write> Colorize<T> for Operand {
    fn colorize(&self, colors: Option<&ColorSettings>, f: &mut T) -> std::fmt::Result {
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
            &Operand::DisplacementU64(imm) => write!(f, "[0x{:x}]", imm),
            &Operand::RegDisp(ref spec, ref disp) => {
                if *disp < 0 {
                    write!(f, "[{} - 0x{:x}]", spec, -disp)
                } else {
                    write!(f, "[{} + 0x{:x}]", spec, disp)
                }
            },
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
            &Opcode::DIV => write!(f, "{}", "div"),
            &Opcode::IDIV => write!(f, "{}", "idiv"),
            &Opcode::CMPXCHG => write!(f, "{}", "cmpxchg"),
            &Opcode::MOVZX_b => write!(f, "{}", "movzx"),
            &Opcode::MOVZX_w => write!(f, "{}", "movzx"),
            &Opcode::MOVSX => write!(f, "{}", "movsx"),
            &Opcode::SETO => write!(f, "{}", "seto"),
            &Opcode::SETNO => write!(f, "{}", "setno"),
            &Opcode::SETB => write!(f, "{}", "setb"),
            &Opcode::SETAE => write!(f, "{}", "setae"),
            &Opcode::SETZ => write!(f, "{}", "setz"),
            &Opcode::SETNZ => write!(f, "{}", "setnz"),
            &Opcode::SETBE => write!(f, "{}", "setbe"),
            &Opcode::SETA => write!(f, "{}", "seta"),
            &Opcode::SETS => write!(f, "{}", "sets"),
            &Opcode::SETNS => write!(f, "{}", "setns"),
            &Opcode::SETP => write!(f, "{}", "setp"),
            &Opcode::SETNP => write!(f, "{}", "setnp"),
            &Opcode::SETL => write!(f, "{}", "setl"),
            &Opcode::SETGE => write!(f, "{}", "setge"),
            &Opcode::SETLE => write!(f, "{}", "setle"),
            &Opcode::SETG => write!(f, "{}", "setg"),
            &Opcode::Invalid => write!(f, "{}", "invalid")
        }
    }
}

impl <T: std::fmt::Write> Colorize<T> for Opcode {
    fn colorize(&self, colors: Option<&ColorSettings>, out: &mut T) -> std::fmt::Result {
        match colors {
            Some(colors) => {
                write!(out, "{}{}{}", color_for(*self, colors), self, color::Fg(&color::Reset as &color::Color))
            },
            None => {
                write!(out, "{}", self)
            }
        }
    }
}

fn color_for(opcode: Opcode, colors: &ColorSettings) -> &color::Fg<&'static color::Color> {
    match opcode {
        Opcode::DIV |
        Opcode::IDIV |
        Opcode::MUL |
        Opcode::NEG |
        Opcode::NOT |
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL |
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL |
        Opcode::INC |
        Opcode::DEC |
        Opcode::SBB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR |
        Opcode::LEA |
        Opcode::ADD |
        Opcode::ADC |
        Opcode::SUB |
        Opcode::IMUL => { colors.arithmetic_op() }
        Opcode::POPF |
        Opcode::PUSHF |
        Opcode::ENTER |
        Opcode::LEAVE |
        Opcode::PUSH |
        Opcode::POP => { colors.stack_op() }
        Opcode::WAIT |
        Opcode::NOP => { colors.nop_op() }

        /* Control flow */
        Opcode::HLT |
        Opcode::INT |
        Opcode::INTO |
        Opcode::IRET |
        Opcode::RETF |
        Opcode::RETURN => { colors.stop_op() }
        Opcode::CALL |
        Opcode::CALLF |
        Opcode::JMP |
        Opcode::JMPF |
        Opcode::JO |
        Opcode::JNO |
        Opcode::JB |
        Opcode::JNB |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JL |
        Opcode::JGE |
        Opcode::JLE |
        Opcode::JG => { colors.control_flow_op() }

        /* Data transfer */
        Opcode::MOV |
        Opcode::CBW |
        Opcode::CDW |
        Opcode::LODS |
        Opcode::STOS |
        Opcode::LAHF |
        Opcode::SAHF |
        Opcode::MOVS |
        Opcode::INS |
        Opcode::OUTS |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::XCHG |
        Opcode::CMOVA |
        Opcode::CMOVB |
        Opcode::CMOVG |
        Opcode::CMOVGE |
        Opcode::CMOVL |
        Opcode::CMOVLE |
        Opcode::CMOVNA |
        Opcode::CMOVNB |
        Opcode::CMOVNO |
        Opcode::CMOVNP |
        Opcode::CMOVNS |
        Opcode::CMOVNZ |
        Opcode::CMOVO |
        Opcode::CMOVP |
        Opcode::CMOVS |
        Opcode::CMOVZ |
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETB |
        Opcode::SETAE |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETBE |
        Opcode::SETA |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETL |
        Opcode::SETGE |
        Opcode::SETLE |
        Opcode::SETG => { colors.data_op() }

        Opcode::CMPS |
        Opcode::SCAS |
        Opcode::TEST |
        Opcode::CMP |
        Opcode::CMPXCHG => { colors.comparison_op() }

        Opcode::Invalid => { colors.invalid_op() }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.colorize(None, fmt)
    }
}

/*
 * Can't implement this as accepting a formatter because rust
 * doesn't let me build one outside println! or write! or whatever.
 *
 * can't write this as an intermediate struct because i refuse to copy
 * all data into the struct, and having a function producing a struct with
 * some lifetimes gets really hairy if it's from a trait - same GAT kind
 * of nonsense as i saw with ContextRead, because someone could hold onto
 * the dang intermediate struct forever.
 *
 * so write to some Write thing i guess. bite me. i really just want to
 * stop thinking about how to support printing instructions...
 */
impl <T: std::fmt::Write> Colorize<T> for Instruction {
    fn colorize(&self, colors: Option<&ColorSettings>, out: &mut T) -> std::fmt::Result {
        // TODO: I DONT LIKE THIS, there is no address i can give contextualize here,
        // the address operand maybe should be optional..
        self.contextualize(colors, 0, None, out)
    }
}

impl <T: std::fmt::Write> ShowContextual<u64, [Option<String>], T> for Instruction {
    fn contextualize(&self, colors: Option<&ColorSettings>, address: u64, context: Option<&[Option<String>]>, out: &mut T) -> std::fmt::Result {
        if self.prefixes.lock {
            write!(out, "lock ")?;
        }
        colors.map(|c| { write!(out, "{}{}{}", color_for(self.opcode, c), self.opcode, color::Fg(&color::Reset as &color::Color)) })
            .unwrap_or_else(|| { write!(out, "{}", self.opcode) })?;
        /* For when contextualization is a thing we can do?
        match self.0.opers.as_ref().and_then(|xs| xs[0].as_ref()) {
            Some(s) => { write!(out, " {}", s)?; },
            None => {
        */
                match self.operands[0] {
                    Operand::Nothing => {
                        return Ok(());
                    },
                    ref x @ _ => {
                        write!(out, " ")?;
                        x.colorize(colors, out)?;
                    }
                }
                /*
            }
        }*/;
        match self.opcode {
            Opcode::MOVZX_b => {
                /*
                match self.0.opers.as_ref().and_then(|xs| xs[1].as_ref()) {
                    Some(s) => { write!(out, ", {}", s) }
                    None => {
                    */
                        match &self.operands[1] {
                            &Operand::Nothing => {
                                return Ok(());
                            },
                            x @ &Operand::Register(_) => {
                                write!(out, ", ")?;
                                x.colorize(colors, out)
                            }
                            x @ _ => {
                                write!(out, ", byte ")?;
                                x.colorize(colors, out)
                            }
                        }
                        /*
                    }
                }
                */
            },
            Opcode::MOVZX_w => {
                /*
                match self.0.opers.as_ref().and_then(|xs| xs[1].as_ref()) {
                    Some(s) => { write!(out, ", {}", s) }
                    None => {
                    */
                        match &self.operands[1] {
                            &Operand::Nothing => {
                                return Ok(());
                            },
                            x @ &Operand::Register(_) => {
                                write!(out, ", ")?;
                                x.colorize(colors, out)
                            }
                            x @ _ => {
                                write!(out, ", word ")?;
                                x.colorize(colors, out)
                            }
                        }
                        /*
                    }
                }
                */
            },
            _ => {
                /*
                match self.0.opers.as_ref().and_then(|xs| xs[1].as_ref()) {
                    Some(s) => { write!(out, ", {}", s) }
                    None => {
                    */
                        match &self.operands[1] {
                            &Operand::Nothing => {
                                return Ok(());
                            },
                            x @ _ => {
                                write!(out, ", ")?;
                                x.colorize(colors, out)
                            }
                        }
                        /*
                    }
                }
                */
            }
        }
    }
}
