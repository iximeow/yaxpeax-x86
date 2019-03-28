extern crate yaxpeax_arch;
extern crate termion;

use termion::color;

use std::fmt;

use std::hint::unreachable_unchecked;

use yaxpeax_arch::{Arch, Colorize, ColorSettings, Decodable, LengthedInstruction, ShowContextual, YaxColors};

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
            RegisterBank::EFlags => { "eflags" },
            RegisterBank::RFlags => { "rflags" },
            RegisterBank::CR => {
                ["cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7", "cr8", "cr9", "cr10", "cr11", "cr12", "cr13", "cr14", "cr15"][self.num as usize]
            }
            RegisterBank::DR => {
                ["dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7", "dr8", "dr9", "dr10", "dr11", "dr12", "dr13", "dr14", "dr15"][self.num as usize]
            }
            RegisterBank::X => {
                ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"][self.num as usize]
            },
            RegisterBank::Y => {
                ["ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"][self.num as usize]
            },
            RegisterBank::Z => {
                ["zmm0", "zmm1", "zmm2", "zmm3", "zmm4", "zmm5", "zmm6", "zmm7", "zmm8", "zmm9", "zmm10", "zmm11", "zmm12", "zmm13", "zmm14", "zmm15", "zmm16", "zmm17", "zmm18", "zmm19", "zmm20", "zmm21", "zmm22", "zmm23", "zmm24", "zmm25", "zmm26", "zmm27", "zmm28", "zmm29", "zmm30", "zmm31"][self.num as usize]
            },
            RegisterBank::ST => {
                ["st(0)", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)"][self.num as usize]
            },
            RegisterBank::MM => {
                ["mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7"][self.num as usize]
            }
            RegisterBank::S => {
                ["cs", "ds", "es", "fs", "gs", "ss"][self.num as usize]
            }
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
            &Operand::ImmediateU8(imm) => {
                write!(f, "{}", colors.number(format!("{:#x}", imm)))
            }
            &Operand::ImmediateI8(imm) => {
                let (sign, imm) = if imm < 0 {
                    (true, -imm)
                } else {
                    (false, imm)
                };
                write!(f, "{}",
                    colors.number(format!("{}{:#x}",
                        if sign { "-" } else { "" },
                        imm))
                )
            },
            &Operand::ImmediateU16(imm) => {
                write!(f, "{}", colors.number(format!("{:#x}", imm)))
            }
            &Operand::ImmediateI16(imm) => {
                let (sign, imm) = if imm < 0 {
                    (true, -imm)
                } else {
                    (false, imm)
                };
                write!(f, "{}",
                    colors.number(format!("{}{:#x}",
                        if sign { "-" } else { "" },
                        imm))
                )
            },
            &Operand::ImmediateU32(imm) => {
                write!(f, "{}", colors.number(format!("{:#x}", imm)))
            }
            &Operand::ImmediateI32(imm) => {
                let (sign, imm) = if imm == std::i32::MIN {
                    (false, imm)
                } else if imm < 0 {
                    (true, -imm)
                } else {
                    (false, imm)
                };
                write!(f, "{}",
                    colors.number(format!("{}{:#x}",
                        if sign { "-" } else { "" },
                        imm))
                )
            },
            &Operand::ImmediateU64(imm) => {
                write!(f, "{}", colors.number(format!("{:#x}", imm)))
            }
            &Operand::ImmediateI64(imm) => {
                let (sign, imm) = if imm == std::i64::MIN {
                    (false, imm)
                } else if imm < 0 {
                    (true, -imm)
                } else {
                    (false, imm)
                };
                write!(f, "{}",
                    colors.number(format!("{}{:#x}",
                        if sign { "-" } else { "" },
                        imm))
                )
            },
            &Operand::Register(ref spec) => {
                write!(f, "{}", colors.register(spec))
            }
            &Operand::DisplacementU32(imm) => {
                write!(f, "[{}]", colors.address(format!("{:#x}", imm)))
            }
            &Operand::DisplacementU64(imm) => {
                write!(f, "[{}]", colors.address(format!("{:#x}", imm)))
            }
            &Operand::RegDisp(ref spec, ref disp) => {
                let (sign, disp) = if *disp < 0 {
                    (true, -*disp)
                } else {
                    (false, *disp)
                };
                write!(f, "[{} {} {}]",
                    colors.register(spec),
                    if sign { "-" } else { "+" },
                    colors.number(format!("{:#x}", disp))
                )
            },
            &Operand::RegDeref(ref spec) => {
                write!(f, "[{}]", colors.register(spec))
            },
            &Operand::RegScale(ref spec, scale) => {
                write!(f, "[{} * {}]",
                    colors.register(spec),
                    colors.number(scale)
                )
            },
            &Operand::RegScaleDisp(ref spec, scale, disp) => {
                write!(f, "[{} * {} + {}]",
                    colors.register(spec),
                    colors.number(scale),
                    colors.number(format!("{:#x}", disp))
                )
            },
            &Operand::RegIndexBase(ref base, ref index) => {
                write!(f, "[{} + {}]",
                    colors.register(base),
                    colors.register(index)
                )
            }
            &Operand::RegIndexBaseDisp(ref base, ref index, disp) => {
                write!(f, "[{} + {} + {}]",
                    colors.register(base),
                    colors.register(index),
                    colors.register(format!("{:#x}", disp))
                )
            },
            &Operand::RegIndexBaseScale(ref base, ref index, scale) => {
                write!(f, "[{} + {} * {}]",
                    colors.register(base),
                    colors.register(index),
                    colors.number(scale)
                )
            }
            &Operand::RegIndexBaseScaleDisp(ref base, ref index, scale, disp) => {
                write!(f, "[{} + {} * {} + {}]",
                    colors.register(base),
                    colors.register(index),
                    colors.number(scale),
                    colors.number(format!("{:#x}", scale))
                )
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
        match self {
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
            Opcode::IMUL => { write!(out, "{}", colors.arithmetic_op(self)) }
            Opcode::POPF |
            Opcode::PUSHF |
            Opcode::ENTER |
            Opcode::LEAVE |
            Opcode::PUSH |
            Opcode::POP => { write!(out, "{}", colors.stack_op(self)) }
            Opcode::WAIT |
            Opcode::NOP => { write!(out, "{}", colors.nop_op(self)) }

            /* Control flow */
            Opcode::HLT |
            Opcode::INT |
            Opcode::INTO |
            Opcode::IRET |
            Opcode::RETF |
            Opcode::RETURN => { write!(out, "{}", colors.stop_op(self)) }
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
            Opcode::JG => { write!(out, "{}", colors.control_flow_op(self)) }

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
            Opcode::SETG => { write!(out, "{}", colors.data_op(self)) }

            Opcode::CMPS |
            Opcode::SCAS |
            Opcode::TEST |
            Opcode::CMP |
            Opcode::CMPXCHG => { write!(out, "{}", colors.comparison_op(self)) }

            Opcode::Invalid => { write!(out, "{}", colors.invalid_op(self)) }
        }
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
        self.opcode.colorize(colors, out)?;
        match context.and_then(|xs| xs[0].as_ref()) {
            Some(s) => { write!(out, " {}", s)?; },
            None => {
                match self.operands[0] {
                    Operand::Nothing => {
                        return Ok(());
                    },
                    ref x @ _ => {
                        write!(out, " ")?;
                        x.colorize(colors, out)?;
                    }
                }
            }
        };
        match self.opcode {
            Opcode::MOVZX_b => {
                match context.and_then(|xs| xs[1].as_ref()) {
                    Some(s) => { write!(out, ", {}", s) }
                    None => {
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
                    }
                }
            },
            Opcode::MOVZX_w => {
                match context.and_then(|xs| xs[1].as_ref()) {
                    Some(s) => { write!(out, ", {}", s) }
                    None => {
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
                    }
                }
            },
            _ => {
                match context.and_then(|xs| xs[1].as_ref()) {
                    Some(s) => { write!(out, ", {}", s) }
                    None => {
                        match &self.operands[1] {
                            &Operand::Nothing => {
                                return Ok(());
                            },
                            x @ _ => {
                                write!(out, ", ")?;
                                x.colorize(colors, out)
                            }
                        }
                    }
                }
            }
        }
    }
}