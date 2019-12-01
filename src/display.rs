extern crate yaxpeax_arch;
extern crate termion;

use std::fmt;

use yaxpeax_arch::{Colorize, ColorSettings, ShowContextual, YaxColors};
use yaxpeax_arch::display::*;

use ::{RegSpec, RegisterBank, Opcode, Operand, Instruction, Segment, PrefixRex, OperandSpec};

impl fmt::Display for PrefixRex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.present() {
            write!(f, "rex:{}{}{}{}",
                if self.w() { "w" } else { "-" },
                if self.r() { "r" } else { "-" },
                if self.x() { "x" } else { "-" },
                if self.b() { "b" } else { "-" },
            )
        } else {
            write!(f, "rex:none")
        }
    }
}

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Segment::CS => write!(f, "cs"),
            Segment::DS => write!(f, "ds"),
            Segment::ES => write!(f, "es"),
            Segment::FS => write!(f, "fs"),
            Segment::GS => write!(f, "gs"),
            Segment::SS => write!(f, "ss"),
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
                write!(f, "{}",
                    colors.number(signed_i8_hex(imm)))
            },
            &Operand::ImmediateU16(imm) => {
                write!(f, "{}", colors.number(format!("{:#x}", imm)))
            }
            &Operand::ImmediateI16(imm) => {
                write!(f, "{}",
                    colors.number(signed_i16_hex(imm)))
            },
            &Operand::ImmediateU32(imm) => {
                write!(f, "{}", colors.number(format!("{:#x}", imm)))
            }
            &Operand::ImmediateI32(imm) => {
                write!(f, "{}",
                    colors.number(signed_i32_hex(imm)))
            },
            &Operand::ImmediateU64(imm) => {
                write!(f, "{}", colors.number(format!("{:#x}", imm)))
            }
            &Operand::ImmediateI64(imm) => {
                write!(f, "{}",
                    colors.number(signed_i64_hex(imm)))
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
                    (true, (-std::num::Wrapping(*disp)).0)
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
                    colors.number(format!("{:#x}", disp))
                )
            },
            &Operand::Nothing => { Ok(()) },
            // &Operand::Many(_) => { panic!("many not covered"); }
            &Operand::Many(ref ops) => {
                for op in ops.iter() {
                    write!(f, ", {}", op)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Opcode::POPCNT => write!(f, "{}", "popcnt"),
            &Opcode::MOVDQU => write!(f, "{}", "movdqu"),
            &Opcode::MOVQ => write!(f, "{}", "movq"),
            &Opcode::CMPSS => write!(f, "{}", "cmpss"),
            &Opcode::CMPSD => write!(f, "{}", "cmpsd"),
            &Opcode::UNPCKLPS => write!(f, "{}", "unpcklps"),
            &Opcode::UNPCKHPS => write!(f, "{}", "unpckhps"),
            &Opcode::MOVUPS => write!(f, "{}", "movups"),
            &Opcode::MOVQ2DQ => write!(f, "{}", "movq2dq"),
            &Opcode::MOVDQ2Q => write!(f, "{}", "movdq2q"),
            &Opcode::RSQRTSS => write!(f, "{}", "rsqrtss"),
            &Opcode::MOVSHDUP => write!(f, "{}", "movshdup"),
            &Opcode::CVTTPS2DQ => write!(f, "{}", "cvttps2dq"),
            &Opcode::CVTPD2DQ => write!(f, "{}", "cvtpd2dq"),
            &Opcode::RCPSS => write!(f, "{}", "rcpss"),
            &Opcode::CVTDQ2PD => write!(f, "{}", "cvtdq2pd"),
            &Opcode::PSHUFHW => write!(f, "{}", "pshufhw"),
            &Opcode::PSHUFLW => write!(f, "{}", "pshuflw"),
            &Opcode::XADD => write!(f, "{}", "xadd"),
            &Opcode::BT => write!(f, "{}", "bt"),
            &Opcode::BTS => write!(f, "{}", "bts"),
            &Opcode::BTR => write!(f, "{}", "btr"),
            &Opcode::BTC => write!(f, "{}", "btc"),
            &Opcode::BSF => write!(f, "{}", "bsf"),
            &Opcode::BSR => write!(f, "{}", "bsr"),
            &Opcode::MOVSS => write!(f, "{}", "movss"),
            &Opcode::SQRTSS => write!(f, "{}", "sqrtss"),
            &Opcode::ADDSS => write!(f, "{}", "addss"),
            &Opcode::SUBSS => write!(f, "{}", "subss"),
            &Opcode::MULSS => write!(f, "{}", "mulss"),
            &Opcode::DIVSS => write!(f, "{}", "divss"),
            &Opcode::MINSS => write!(f, "{}", "minss"),
            &Opcode::MAXSS => write!(f, "{}", "maxss"),
            &Opcode::MOVSD => write!(f, "{}", "movsd"),
            &Opcode::SQRTSD => write!(f, "{}", "sqrtsd"),
            &Opcode::ADDSD => write!(f, "{}", "addsd"),
            &Opcode::SUBSD => write!(f, "{}", "subsd"),
            &Opcode::MULSD => write!(f, "{}", "mulsd"),
            &Opcode::DIVSD => write!(f, "{}", "divsd"),
            &Opcode::MINSD => write!(f, "{}", "minsd"),
            &Opcode::MAXSD => write!(f, "{}", "maxsd"),
            &Opcode::MOVDDUP => write!(f, "{}", "movddup"),
            &Opcode::MOVSLDUP => write!(f, "{}", "movsldup"),
            &Opcode::HADDPS => write!(f, "{}", "haddps"),
            &Opcode::HSUBPS => write!(f, "{}", "hsubps"),
            &Opcode::ADDSUBPS => write!(f, "{}", "addsubps"),
            &Opcode::CVTSI2SS => write!(f, "{}", "cvtsi2ss"),
            &Opcode::CVTSI2SD => write!(f, "{}", "cvtsi2sd"),
            &Opcode::CVTTSD2SI => write!(f, "{}", "cvttsd2si"),
            &Opcode::CVTSD2SI => write!(f, "{}", "cvtsd2si"),
            &Opcode::CVTSD2SS => write!(f, "{}", "cvtsd2ss"),
            &Opcode::CVTTSS2SI => write!(f, "{}", "cvttss2si"),
            &Opcode::CVTSS2SI => write!(f, "{}", "cvtss2si"),
            &Opcode::CVTSS2SD => write!(f, "{}", "cvtss2sd"),
            &Opcode::LDDQU => write!(f, "{}", "lddqu"),
            &Opcode::STI => write!(f, "{}", "sti"),
            &Opcode::STD => write!(f, "{}", "std"),
            &Opcode::STC => write!(f, "{}", "stc"),
            &Opcode::CLI => write!(f, "{}", "cli"),
            &Opcode::CLD => write!(f, "{}", "cld"),
            &Opcode::CLC => write!(f, "{}", "clc"),
            &Opcode::SLDT => write!(f, "{}", "sldt"),
            &Opcode::STR => write!(f, "{}", "str"),
            &Opcode::LLDT => write!(f, "{}", "lldt"),
            &Opcode::LTR => write!(f, "{}", "ltr"),
            &Opcode::VERR => write!(f, "{}", "verr"),
            &Opcode::VERW => write!(f, "{}", "verw"),
            &Opcode::JMPE => write!(f, "{}", "jmpe"),
            &Opcode::WRMSR => write!(f, "{}", "wrmsr"),
            &Opcode::RDMSR => write!(f, "{}", "rdmsr"),
            &Opcode::RDTSC => write!(f, "{}", "rdtsc"),
            &Opcode::RDPMC => write!(f, "{}", "rdpmc"),
            &Opcode::FXSAVE => write!(f, "{}", "fxsave"),
            &Opcode::FXRSTOR => write!(f, "{}", "fxstor"),
            &Opcode::LDMXCSR => write!(f, "{}", "ldmxcsr"),
            &Opcode::STMXCSR => write!(f, "{}", "stmxcsr"),
            &Opcode::XSAVE => write!(f, "{}", "xsave"),
            &Opcode::XSTOR => write!(f, "{}", "xstor"),
            &Opcode::XSAVEOPT => write!(f, "{}", "xsaveopt"),
            &Opcode::LFENCE => write!(f, "{}", "lfence"),
            &Opcode::MFENCE => write!(f, "{}", "mfence"),
            &Opcode::SFENCE => write!(f, "{}", "sfence"),
            &Opcode::CLFLUSH => write!(f, "{}", "clflush"),
            &Opcode::SGDT => write!(f, "{}", "sgdt"),
            &Opcode::SIDT => write!(f, "{}", "sidt"),
            &Opcode::LGDT => write!(f, "{}", "lgdt"),
            &Opcode::LIDT => write!(f, "{}", "lidt"),
            &Opcode::SMSW => write!(f, "{}", "smsw"),
            &Opcode::LMSW => write!(f, "{}", "lmsw"),
            &Opcode::SWAPGS => write!(f, "{}", "swapgs"),
            &Opcode::RDTSCP => write!(f, "{}", "rdtscp"),
            &Opcode::INVLPG => write!(f, "{}", "invlpg"),
            &Opcode::CPUID => write!(f, "{}", "cpuid"),
            &Opcode::UD2 => write!(f, "{}", "ud2"),
            &Opcode::WBINVD => write!(f, "{}", "wbinvd"),
            &Opcode::INVD => write!(f, "{}", "invd"),
            &Opcode::SYSRET => write!(f, "{}", "sysret"),
            &Opcode::CLTS => write!(f, "{}", "clts"),
            &Opcode::SYSCALL => write!(f, "{}", "syscall"),
            &Opcode::LSL => write!(f, "{}", "lsl"),
            &Opcode::LAR => write!(f, "{}", "lar"),
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
            &Opcode::PREFETCHNTA => write!(f, "{}", "prefetchnta"),
            &Opcode::PREFETCH0 => write!(f, "{}", "prefetch0"),
            &Opcode::PREFETCH1 => write!(f, "{}", "prefetch1"),
            &Opcode::PREFETCH2 => write!(f, "{}", "prefetch2"),
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
            &Opcode::SHRD => write!(f, "{}", "shrd"),
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
            &Opcode::MOVSX_b => write!(f, "{}", "movsx"),
            &Opcode::MOVSX_w => write!(f, "{}", "movsx"),
            &Opcode::MOVZX_b => write!(f, "{}", "movzx"),
            &Opcode::MOVZX_w => write!(f, "{}", "movzx"),
            &Opcode::MOVSXD => write!(f, "{}", "movsxd"),
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
            &Opcode::ADDPS => write!(f, "{}", "addps"),
            &Opcode::ANDNPS => write!(f, "{}", "andnps"),
            &Opcode::ANDPS => write!(f, "{}", "andps"),
            &Opcode::BSWAP => write!(f, "{}", "bswap"),
            &Opcode::CMPPS => write!(f, "{}", "cmpps"),
            &Opcode::COMISS => write!(f, "{}", "comiss"),
            &Opcode::CVTDQ2PS => write!(f, "{}", "cvtdq2ps"),
            &Opcode::CVTPI2PS => write!(f, "{}", "cvtpi2ps"),
            &Opcode::CVTPS2PD => write!(f, "{}", "cvtps2pd"),
            &Opcode::CVTPS2PI => write!(f, "{}", "cvtps2pi"),
            &Opcode::CVTTPS2PI => write!(f, "{}", "cvttps2pi"),
            &Opcode::DIVPS => write!(f, "{}", "divps"),
            &Opcode::EMMS => write!(f, "{}", "emms"),
            &Opcode::GETSEC => write!(f, "{}", "getsec"),
            &Opcode::LFS => write!(f, "{}", "lfs"),
            &Opcode::LGS => write!(f, "{}", "lgs"),
            &Opcode::LSS => write!(f, "{}", "lss"),
            &Opcode::MASKMOVQ => write!(f, "{}", "maskmovq"),
            &Opcode::MAXPS => write!(f, "{}", "maxps"),
            &Opcode::MINPS => write!(f, "{}", "minps"),
            &Opcode::MOVAPS => write!(f, "{}", "movaps"),
            &Opcode::MOVAPD => write!(f, "{}", "movapd"),
            &Opcode::MOVD => write!(f, "{}", "movd"),
            &Opcode::MOVLPS => write!(f, "{}", "movlps"),
            &Opcode::MOVLHPS => write!(f, "{}", "movlhps"),
            &Opcode::MOVHPS => write!(f, "{}", "movhps"),
            &Opcode::MOVHLPS => write!(f, "{}", "movhlps"),
            &Opcode::MOVUPD => write!(f, "{}", "movupd"),
            &Opcode::MOVMSKPS => write!(f, "{}", "movmskps"),
            &Opcode::MOVNTI => write!(f, "{}", "movnti"),
            &Opcode::MOVNTPS => write!(f, "{}", "movntps"),
            &Opcode::MOVNTQ => write!(f, "{}", "movntq"),
            &Opcode::MULPS => write!(f, "{}", "mulps"),
            &Opcode::ORPS => write!(f, "{}", "orps"),
            &Opcode::PACKSSDW => write!(f, "{}", "packssdw"),
            &Opcode::PACKSSWB => write!(f, "{}", "packsswb"),
            &Opcode::PACKUSWB => write!(f, "{}", "packuswb"),
            &Opcode::PADDB => write!(f, "{}", "paddb"),
            &Opcode::PADDD => write!(f, "{}", "paddd"),
            &Opcode::PADDQ => write!(f, "{}", "paddq"),
            &Opcode::PADDSB => write!(f, "{}", "paddsb"),
            &Opcode::PADDSW => write!(f, "{}", "paddsw"),
            &Opcode::PADDUSB => write!(f, "{}", "paddusb"),
            &Opcode::PADDUSW => write!(f, "{}", "paddusw"),
            &Opcode::PADDW => write!(f, "{}", "paddw"),
            &Opcode::PAND => write!(f, "{}", "pand"),
            &Opcode::PANDN => write!(f, "{}", "pandn"),
            &Opcode::PAVGB => write!(f, "{}", "pavgb"),
            &Opcode::PAVGW => write!(f, "{}", "pavgw"),
            &Opcode::PCMPEQB => write!(f, "{}", "pcmpeqb"),
            &Opcode::PCMPEQD => write!(f, "{}", "pcmpeqd"),
            &Opcode::PCMPEQW => write!(f, "{}", "pcmpeqw"),
            &Opcode::PCMPGTB => write!(f, "{}", "pcmpgtb"),
            &Opcode::PCMPGTD => write!(f, "{}", "pcmpgtd"),
            &Opcode::PCMPGTW => write!(f, "{}", "pcmpgtw"),
            &Opcode::PEXTRW => write!(f, "{}", "pextrw"),
            &Opcode::PINSRW => write!(f, "{}", "pinsrw"),
            &Opcode::PMADDWD => write!(f, "{}", "pmaddwd"),
            &Opcode::PMAXSW => write!(f, "{}", "pmaxsw"),
            &Opcode::PMAXUB => write!(f, "{}", "pmaxub"),
            &Opcode::PMINSW => write!(f, "{}", "pminsw"),
            &Opcode::PMINUB => write!(f, "{}", "pminub"),
            &Opcode::PMOVMSKB => write!(f, "{}", "pmovmskb"),
            &Opcode::PMULHUW => write!(f, "{}", "pmulhuw"),
            &Opcode::PMULHW => write!(f, "{}", "pmulhw"),
            &Opcode::PMULLW => write!(f, "{}", "pmullw"),
            &Opcode::PMULUDQ => write!(f, "{}", "pmuludq"),
            &Opcode::POR => write!(f, "{}", "por"),
            &Opcode::PSADBW => write!(f, "{}", "psadbw"),
            &Opcode::PSHUFW => write!(f, "{}", "pshufw"),
            &Opcode::PSLLD => write!(f, "{}", "pslld"),
            &Opcode::PSLLQ => write!(f, "{}", "psllq"),
            &Opcode::PSLLW => write!(f, "{}", "psllw"),
            &Opcode::PSRAD => write!(f, "{}", "psrad"),
            &Opcode::PSRAW => write!(f, "{}", "psraw"),
            &Opcode::PSRLD => write!(f, "{}", "psrld"),
            &Opcode::PSRLQ => write!(f, "{}", "psrlq"),
            &Opcode::PSRLW => write!(f, "{}", "psrlw"),
            &Opcode::PSUBB => write!(f, "{}", "psubb"),
            &Opcode::PSUBD => write!(f, "{}", "psubd"),
            &Opcode::PSUBQ => write!(f, "{}", "psubq"),
            &Opcode::PSUBSB => write!(f, "{}", "psubsb"),
            &Opcode::PSUBSW => write!(f, "{}", "psubsw"),
            &Opcode::PSUBUSB => write!(f, "{}", "psubusb"),
            &Opcode::PSUBUSW => write!(f, "{}", "psubusw"),
            &Opcode::PSUBW => write!(f, "{}", "psubw"),
            &Opcode::PUNPCKHBW => write!(f, "{}", "punpckhbw"),
            &Opcode::PUNPCKHDQ => write!(f, "{}", "punpckhdq"),
            &Opcode::PUNPCKHWD => write!(f, "{}", "punpckhwd"),
            &Opcode::PUNPCKLBW => write!(f, "{}", "punpcklbw"),
            &Opcode::PUNPCKLDQ => write!(f, "{}", "punpckldq"),
            &Opcode::PUNPCKLWD => write!(f, "{}", "punpcklwd"),
            &Opcode::PXOR => write!(f, "{}", "pxor"),
            &Opcode::RCPPS => write!(f, "{}", "rcpps"),
            &Opcode::RSM => write!(f, "{}", "rsm"),
            &Opcode::RSQRTPS => write!(f, "{}", "rsqrtps"),
            &Opcode::SHLD => write!(f, "{}", "shld"),
            &Opcode::SHUFPS => write!(f, "{}", "shufps"),
            &Opcode::SLHD => write!(f, "{}", "slhd"),
            &Opcode::SQRTPS => write!(f, "{}", "sqrtps"),
            &Opcode::SUBPS => write!(f, "{}", "subps"),
            &Opcode::SYSENTER => write!(f, "{}", "sysenter"),
            &Opcode::SYSEXIT => write!(f, "{}", "sysexit"),
            &Opcode::UCOMISS => write!(f, "{}", "ucomiss"),
            &Opcode::UD2E => write!(f, "{}", "ud2e"),
            &Opcode::VMREAD => write!(f, "{}", "vmread"),
            &Opcode::VMWRITE => write!(f, "{}", "vmwrite"),
            &Opcode::XORPS => write!(f, "{}", "xorps"),
            &Opcode::CBW => write!(f, "{}", "cbw"),
            &Opcode::CWDE => write!(f, "{}", "cwde"),
            &Opcode::CDQE => write!(f, "{}", "cdqe"),
            &Opcode::CBD => write!(f, "{}", "cbd"),
            &Opcode::CDQ => write!(f, "{}", "cdq"),
            &Opcode::CQO => write!(f, "{}", "cqo"),
            &Opcode::Invalid => write!(f, "{}", "invalid"),
        }
    }
}

impl <T: std::fmt::Write> Colorize<T> for Opcode {
    fn colorize(&self, colors: Option<&ColorSettings>, out: &mut T) -> std::fmt::Result {
        match self {
            Opcode::RCPSS |
            Opcode::RSQRTSS |
            Opcode::SQRTSD |
            Opcode::ADDSD |
            Opcode::SUBSD |
            Opcode::MULSD |
            Opcode::DIVSD |
            Opcode::SQRTSS |
            Opcode::ADDSS |
            Opcode::SUBSS |
            Opcode::MULSS |
            Opcode::DIVSS |
            Opcode::HADDPS |
            Opcode::HSUBPS |
            Opcode::ADDSUBPS |
            Opcode::XADD|
            Opcode::DIV |
            Opcode::IDIV |
            Opcode::MUL |
            Opcode::NEG |
            Opcode::NOT |
            Opcode::SAR |
            Opcode::SAL |
            Opcode::SHR |
            Opcode::SHRD |
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
            Opcode::POPCNT |
            Opcode::BT |
            Opcode::BTS |
            Opcode::BTR |
            Opcode::BTC |
            Opcode::BSF |
            Opcode::BSR |
            Opcode::ADDPS |
            Opcode::ANDNPS |
            Opcode::ANDPS |
            Opcode::COMISS |
            Opcode::DIVPS |
            Opcode::MULPS |
            Opcode::ORPS |
            Opcode::PADDB |
            Opcode::PADDD |
            Opcode::PADDQ |
            Opcode::PADDSB |
            Opcode::PADDSW |
            Opcode::PADDUSB |
            Opcode::PADDUSW |
            Opcode::PADDW |
            Opcode::PAND |
            Opcode::PANDN |
            Opcode::PAVGB |
            Opcode::PAVGW |
            Opcode::PMADDWD |
            Opcode::PMULHUW |
            Opcode::PMULHW |
            Opcode::PMULLW |
            Opcode::PMULUDQ |
            Opcode::POR |
            Opcode::PSADBW |
            Opcode::PSHUFW |
            Opcode::PSLLD |
            Opcode::PSLLQ |
            Opcode::PSLLW |
            Opcode::PSRAD |
            Opcode::PSRAW |
            Opcode::PSRLD |
            Opcode::PSRLQ |
            Opcode::PSRLW |
            Opcode::PSUBB |
            Opcode::PSUBD |
            Opcode::PSUBQ |
            Opcode::PSUBSB |
            Opcode::PSUBSW |
            Opcode::PSUBUSB |
            Opcode::PSUBUSW |
            Opcode::PSUBW |
            Opcode::PXOR |
            Opcode::RSQRTPS |
            Opcode::SQRTPS |
            Opcode::SUBPS |
            Opcode::XORPS |
            Opcode::RCPPS |
            Opcode::SHLD |
            Opcode::SLHD |
            Opcode::UCOMISS |
            Opcode::IMUL => { write!(out, "{}", colors.arithmetic_op(self)) }
            Opcode::POPF |
            Opcode::PUSHF |
            Opcode::ENTER |
            Opcode::LEAVE |
            Opcode::PUSH |
            Opcode::POP => { write!(out, "{}", colors.stack_op(self)) }
            Opcode::WAIT |
            Opcode::PREFETCHNTA |
            Opcode::PREFETCH0 |
            Opcode::PREFETCH1 |
            Opcode::PREFETCH2 |
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
            Opcode::BSWAP |
            Opcode::CVTDQ2PD |
            Opcode::CVTDQ2PS |
            Opcode::CVTPD2DQ |
            Opcode::CVTPI2PS |
            Opcode::CVTPS2PD |
            Opcode::CVTPS2PI |
            Opcode::CVTSD2SI |
            Opcode::CVTSD2SS |
            Opcode::CVTSI2SD |
            Opcode::CVTSI2SS |
            Opcode::CVTSS2SD |
            Opcode::CVTSS2SI |
            Opcode::CVTTPS2DQ |
            Opcode::CVTTPS2PI |
            Opcode::CVTTSD2SI |
            Opcode::CVTTSS2SI |
            Opcode::MASKMOVQ |
            Opcode::MOVAPS |
            Opcode::MOVAPD |
            Opcode::MOVD |
            Opcode::MOVHPS |
            Opcode::MOVHLPS |
            Opcode::MOVLPS |
            Opcode::MOVLHPS |
            Opcode::MOVMSKPS |
            Opcode::MOVNTI |
            Opcode::MOVNTPS |
            Opcode::MOVNTQ |
            Opcode::MOVSD |
            Opcode::MOVSS |
            Opcode::MOVUPD |
            Opcode::PSHUFHW |
            Opcode::PSHUFLW |
            Opcode::PUNPCKHBW |
            Opcode::PUNPCKHDQ |
            Opcode::PUNPCKHWD |
            Opcode::PUNPCKLBW |
            Opcode::PUNPCKLDQ |
            Opcode::PUNPCKLWD |
            Opcode::PACKSSDW |
            Opcode::PACKSSWB |
            Opcode::PACKUSWB |
            Opcode::UNPCKHPS |
            Opcode::UNPCKLPS |
            Opcode::SHUFPS |
            Opcode::PMOVMSKB |
            Opcode::LDDQU |
            Opcode::CLC |
            Opcode::CLI |
            Opcode::CLD |
            Opcode::STC |
            Opcode::STI |
            Opcode::STD |
            Opcode::CBW |
            Opcode::CWDE |
            Opcode::CDQE |
            Opcode::CBD |
            Opcode::CDQ |
            Opcode::CQO |
            Opcode::MOVDDUP |
            Opcode::MOVSLDUP |
            Opcode::MOVDQ2Q |
            Opcode::MOVDQU |
            Opcode::MOVQ |
            Opcode::MOVQ2DQ |
            Opcode::MOVSHDUP |
            Opcode::MOVUPS |
            Opcode::PEXTRW |
            Opcode::PINSRW |
            Opcode::MOV |
            Opcode::LODS |
            Opcode::STOS |
            Opcode::LAHF |
            Opcode::SAHF |
            Opcode::MOVS |
            Opcode::INS |
            Opcode::OUTS |
            Opcode::MOVSX_b |
            Opcode::MOVSX_w |
            Opcode::MOVZX_b |
            Opcode::MOVZX_w |
            Opcode::MOVSX |
            Opcode::MOVSXD |
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

            Opcode::PCMPEQB |
            Opcode::PCMPEQD |
            Opcode::PCMPEQW |
            Opcode::PCMPGTB |
            Opcode::PCMPGTD |
            Opcode::PCMPGTW |
            Opcode::MAXPS |
            Opcode::MAXSD |
            Opcode::MAXSS |
            Opcode::MINPS |
            Opcode::MINSD |
            Opcode::MINSS |
            Opcode::PMAXSW |
            Opcode::PMAXUB |
            Opcode::PMINSW |
            Opcode::PMINUB |
            Opcode::CMPS |
            Opcode::SCAS |
            Opcode::TEST |
            Opcode::CMPSD |
            Opcode::CMPSS |
            Opcode::CMP |
            Opcode::CMPPS |
            Opcode::CMPXCHG => { write!(out, "{}", colors.comparison_op(self)) }

            Opcode::WRMSR |
            Opcode::RDMSR |
            Opcode::RDTSC |
            Opcode::RDPMC |
            Opcode::FXSAVE |
            Opcode::FXRSTOR |
            Opcode::LDMXCSR |
            Opcode::STMXCSR |
            Opcode::XSAVE |
            Opcode::XSTOR |
            Opcode::XSAVEOPT |
            Opcode::LFENCE |
            Opcode::MFENCE |
            Opcode::SFENCE |
            Opcode::CLFLUSH |
            Opcode::SGDT |
            Opcode::SIDT |
            Opcode::LGDT |
            Opcode::LIDT |
            Opcode::SMSW |
            Opcode::LMSW |
            Opcode::SWAPGS |
            Opcode::RDTSCP |
            Opcode::INVLPG |
            Opcode::CPUID |
            Opcode::WBINVD |
            Opcode::INVD |
            Opcode::SYSRET |
            Opcode::CLTS |
            Opcode::SYSCALL |
            Opcode::LSL |
            Opcode::SLDT |
            Opcode::STR |
            Opcode::LLDT |
            Opcode::LTR |
            Opcode::VERR |
            Opcode::VERW |
            Opcode::JMPE |
            Opcode::EMMS |
            Opcode::GETSEC |
            Opcode::LFS |
            Opcode::LGS |
            Opcode::LSS |
            Opcode::RSM |
            Opcode::SYSENTER |
            Opcode::SYSEXIT |
            Opcode::UD2E |
            Opcode::VMREAD |
            Opcode::VMWRITE |
            Opcode::LAR => { write!(out, "{}", colors.platform_op(self)) }

            Opcode::UD2 |
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
    fn contextualize(&self, colors: Option<&ColorSettings>, _address: u64, context: Option<&[Option<String>]>, out: &mut T) -> std::fmt::Result {
        if self.prefixes.lock() {
            write!(out, "lock ")?;
        }

        if [Opcode::MOVS, Opcode::CMPS, Opcode::LODS, Opcode::STOS, Opcode::INS, Opcode::OUTS].contains(&self.opcode) {
            // only a few of you actually use the prefix...
            if self.prefixes.rep() {
                write!(out, "rep ")?;
            } else if self.prefixes.repz() {
                write!(out, "repz ")?;
            } else if self.prefixes.repnz() {
                write!(out, "repnz ")?;
            }
        }

        self.opcode.colorize(colors, out)?;

        match context.and_then(|xs| xs[0].as_ref()) {
            Some(s) => { write!(out, " {}", s)?; },
            None => {
                match self.operands[0] {
                    OperandSpec::Nothing => {
                        return Ok(());
                    },
                    _ => {
                        write!(out, " ")?;
                        if let Some(prefix) = self.segment_override_for_op(0) {
                            write!(out, "{}:", prefix)?;
                        }
                    }
                }
                let x = Operand::from_spec(self, self.operands[0]);
                x.colorize(colors, out)?;
            }
        };
        for i in 1..4 {
            match self.opcode {
                Opcode::MOVSX_b |
                Opcode::MOVZX_b => {
                    match context.and_then(|xs| xs[i].as_ref()) {
                        Some(s) => { write!(out, ", {}", s)? }
                        None => {
                            match &self.operands[i] {
                                &OperandSpec::Nothing => {
                                    return Ok(());
                                },
                                &OperandSpec::RegMMM => {
                                    write!(out, ", ")?;
                                }
                                x @ _ => {
                                    write!(out, ", byte ")?;
                                    if let Some(prefix) = self.segment_override_for_op(1) {
                                        write!(out, "{}:", prefix)?;
                                    }
                                }
                            }
                            let x = Operand::from_spec(self, self.operands[i]);
                            x.colorize(colors, out)?
                        }
                    }
                },
                Opcode::MOVSX_w |
                Opcode::MOVZX_w => {
                    match context.and_then(|xs| xs[i].as_ref()) {
                        Some(s) => { write!(out, ", {}", s)? }
                        None => {
                            match &self.operands[i] {
                                &OperandSpec::Nothing => {
                                    return Ok(());
                                },
                                &OperandSpec::RegMMM => {
                                    write!(out, ", ")?;
                                }
                                _ => {
                                    write!(out, ", word ")?;
                                    if let Some(prefix) = self.segment_override_for_op(1) {
                                        write!(out, "{}:", prefix)?;
                                    }
                                }
                            }
                            let x = Operand::from_spec(self, self.operands[i]);
                            x.colorize(colors, out)?
                        }
                    }
                },
                _ => {
                    match context.and_then(|xs| xs[i].as_ref()) {
                        Some(s) => { write!(out, ", {}", s)? }
                        None => {
                            match &self.operands[i] {
                                &OperandSpec::Nothing => {
                                    return Ok(());
                                },
                                _ => {
                                    write!(out, ", ")?;
                                    if let Some(prefix) = self.segment_override_for_op(1) {
                                        write!(out, "{}:", prefix)?;
                                    }
                                    let x = Operand::from_spec(self, self.operands[i]);
                                    x.colorize(colors, out)?
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
