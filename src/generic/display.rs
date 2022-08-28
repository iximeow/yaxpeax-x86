use core::fmt;
use crate::safer_unchecked::GetSaferUnchecked as _;

use yaxpeax_arch::{Colorize, ShowContextual, NoColors, YaxColors};
use yaxpeax_arch::display::*;

use crate::MEM_SIZE_STRINGS;
use crate::generic::{RegSpec, Opcode, Operand, MergeMode, InstDecoder, Instruction, Segment, PrefixVex, OperandSpec};

impl fmt::Display for InstDecoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &InstDecoder::default() {
            return write!(f, "<all features>");
        } else if self == &InstDecoder::minimal() {
            return write!(f, "<no features>");
        }
        if self.sse3() { write!(f, "sse3 ")? }
        if self.ssse3() { write!(f, "ssse3 ")? }
        if self.monitor() { write!(f, "monitor ")? }
        if self.vmx() { write!(f, "vmx ")? }
        if self.fma3() { write!(f, "fma3 ")? }
        if self.cmpxchg16b() { write!(f, "cmpxchg16b ")? }
        if self.sse4_1() { write!(f, "sse4_1 ")? }
        if self.sse4_2() { write!(f, "sse4_2 ")? }
        if self.movbe() { write!(f, "movbe ")? }
        if self.popcnt() { write!(f, "popcnt ")? }
        if self.aesni() { write!(f, "aesni ")? }
        if self.xsave() { write!(f, "xsave ")? }
        if self.rdrand() { write!(f, "rdrand ")? }
        if self.sgx() { write!(f, "sgx ")? }
        if self.bmi1() { write!(f, "bmi1 ")? }
        if self.avx2() { write!(f, "avx2 ")? }
        if self.bmi2() { write!(f, "bmi2 ")? }
        if self.invpcid() { write!(f, "invpcid ")? }
        if self.mpx() { write!(f, "mpx ")? }
        if self.avx512_f() { write!(f, "avx512_f ")? }
        if self.avx512_dq() { write!(f, "avx512_dq ")? }
        if self.rdseed() { write!(f, "rdseed ")? }
        if self.adx() { write!(f, "adx ")? }
        if self.avx512_fma() { write!(f, "avx512_fma ")? }
        if self.pcommit() { write!(f, "pcommit ")? }
        if self.clflushopt() { write!(f, "clflushopt ")? }
        if self.clwb() { write!(f, "clwb ")? }
        if self.avx512_pf() { write!(f, "avx512_pf ")? }
        if self.avx512_er() { write!(f, "avx512_er ")? }
        if self.avx512_cd() { write!(f, "avx512_cd ")? }
        if self.sha() { write!(f, "sha ")? }
        if self.avx512_bw() { write!(f, "avx512_bw ")? }
        if self.avx512_vl() { write!(f, "avx512_vl ")? }
        if self.prefetchwt1() { write!(f, "prefetchwt1 ")? }
        if self.avx512_vbmi() { write!(f, "avx512_vbmi ")? }
        if self.avx512_vbmi2() { write!(f, "avx512_vbmi2 ")? }
        if self.gfni() { write!(f, "gfni ")? }
        if self.vaes() { write!(f, "vaes ")? }
        if self.pclmulqdq() { write!(f, "pclmulqdq ")? }
        if self.avx_vnni() { write!(f, "avx_vnni ")? }
        if self.avx512_bitalg() { write!(f, "avx512_bitalg ")? }
        if self.avx512_vpopcntdq() { write!(f, "avx512_vpopcntdq ")? }
        if self.avx512_4vnniw() { write!(f, "avx512_4vnniw ")? }
        if self.avx512_4fmaps() { write!(f, "avx512_4fmaps ")? }
        if self.cx8() { write!(f, "cx8 ")? }
        if self.syscall() { write!(f, "syscall ")? }
        if self.rdtscp() { write!(f, "rdtscp ")? }
        if self.abm() { write!(f, "abm ")? }
        if self.sse4a() { write!(f, "sse4a ")? }
        if self._3dnowprefetch() { write!(f, "_3dnowprefetch ")? }
        if self.xop() { write!(f, "xop ")? }
        if self.skinit() { write!(f, "skinit ")? }
        if self.tbm() { write!(f, "tbm ")? }
        if self.intel_quirks() { write!(f, "intel_quirks ")? }
        if self.amd_quirks() { write!(f, "amd_quirks ")? }
        if self.avx() { write!(f, "avx ")? }
        Ok(())
    }
}

impl fmt::Display for PrefixVex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.present() {
            write!(f, "vex:{}{}{}{}",
                if self.w() { "w" } else { "-" },
                if self.r() { "r" } else { "-" },
                if self.x() { "x" } else { "-" },
                if self.b() { "b" } else { "-" },
            )
        } else {
            write!(f, "vex:none")
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

// register names are grouped by indices scaled by 16.
// xmm, ymm, zmm all get two indices.
const REG_NAMES: &[&'static str] = &[
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
    "ax", "cx", "dx", "bx", "sp", "bp", "si", "di",
    "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh",
    "cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7",
    "dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7",
    "es", "cs", "ss", "ds", "fs", "gs", "", "",
    "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
    "xmm16", "xmm17", "xmm18", "xmm19", "xmm20", "xmm21", "xmm22", "xmm23", "xmm24", "xmm25", "xmm26", "xmm27", "xmm28", "xmm29", "xmm30", "xmm31",
    "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15",
    "ymm16", "ymm17", "ymm18", "ymm19", "ymm20", "ymm21", "ymm22", "ymm23", "ymm24", "ymm25", "ymm26", "ymm27", "ymm28", "ymm29", "ymm30", "ymm31",
    "zmm0", "zmm1", "zmm2", "zmm3", "zmm4", "zmm5", "zmm6", "zmm7", "zmm8", "zmm9", "zmm10", "zmm11", "zmm12", "zmm13", "zmm14", "zmm15", "zmm16", "zmm17", "zmm18", "zmm19", "zmm20", "zmm21", "zmm22", "zmm23", "zmm24", "zmm25", "zmm26", "zmm27", "zmm28", "zmm29", "zmm30", "zmm31",
    "st(0)", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)",
    "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7",
    "k0", "k1", "k2", "k3", "k4", "k5", "k6", "k7",
    "eip", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "eflags", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
];

pub(crate) fn regspec_label(spec: &RegSpec) -> &'static str {
    unsafe { REG_NAMES.get_kinda_unchecked((spec.num as u16 + ((spec.bank as u16) << 3)) as usize) }
}

impl fmt::Display for RegSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(regspec_label(self))
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.colorize(&NoColors, fmt)
    }
}

impl <T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Operand {
    fn colorize(&self, colors: &Y, f: &mut T) -> fmt::Result {
        match self {
            &Operand::ImmediateU8(imm) => {
                write!(f, "{}", colors.number(u8_hex(imm)))
            }
            &Operand::ImmediateI8(imm) => {
                write!(f, "{}",
                    colors.number(signed_i8_hex(imm)))
            },
            &Operand::ImmediateU16(imm) => {
                write!(f, "{}", colors.number(u16_hex(imm)))
            }
            &Operand::ImmediateI16(imm) => {
                write!(f, "{}",
                    colors.number(signed_i16_hex(imm)))
            },
            &Operand::ImmediateU32(imm) => {
                write!(f, "{}", colors.number(u32_hex(imm)))
            }
            &Operand::ImmediateI32(imm) => {
                write!(f, "{}",
                    colors.number(signed_i32_hex(imm)))
            },
            &Operand::AbsoluteFarAddress { segment, address } => {
                write!(f, "{}:{}",
                    colors.number(u16_hex(segment as u16)),
                    colors.number(u32_hex(address as u32)),
                )
            },
            &Operand::Register(ref spec) => {
                f.write_str(regspec_label(spec))
            }
            &Operand::RegisterMaskMerge(ref spec, ref mask, merge_mode) => {
                f.write_str(regspec_label(spec))?;
                if mask.num != 0 {
                    f.write_str("{")?;
                    f.write_str(regspec_label(mask))?;
                    f.write_str("}")?;
                }
                if let MergeMode::Zero = merge_mode {
                    f.write_str("{z}")?;
                }
                Ok(())
            }
            &Operand::RegisterMaskMergeSae(ref spec, ref mask, merge_mode, sae_mode) => {
                f.write_str(regspec_label(spec))?;
                if mask.num != 0 {
                    f.write_str("{")?;
                    f.write_str(regspec_label(mask))?;
                    f.write_str("}")?;
                }
                if let MergeMode::Zero = merge_mode {
                    f.write_str("{z}")?;
                }
                f.write_str(sae_mode.label())?;
                Ok(())
            }
            &Operand::RegisterMaskMergeSaeNoround(ref spec, ref mask, merge_mode) => {
                f.write_str(regspec_label(spec))?;
                if mask.num != 0 {
                    f.write_str("{")?;
                    f.write_str(regspec_label(mask))?;
                    f.write_str("}")?;
                }
                if let MergeMode::Zero = merge_mode {
                    f.write_str("{z}")?;
                }
                f.write_str("{sae}")?;
                Ok(())
            }
            &Operand::DisplacementU16(imm) => {
                write!(f, "[{}]", colors.address(u16_hex(imm)))
            }
            &Operand::DisplacementU32(imm) => {
                write!(f, "[{}]", colors.address(u32_hex(imm)))
            }
            &Operand::RegDisp(ref spec, disp) => {
                write!(f, "[{} ", regspec_label(spec))?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")
            },
            &Operand::RegDeref(ref spec) => {
                f.write_str("[")?;
                f.write_str(regspec_label(spec))?;
                f.write_str("]")
            },
            &Operand::RegScale(ref spec, scale) => {
                write!(f, "[{} * {}]",
                    regspec_label(spec),
                    colors.number(scale)
                )
            },
            &Operand::RegScaleDisp(ref spec, scale, disp) => {
                write!(f, "[{} * {} ",
                    regspec_label(spec),
                    colors.number(scale),
                )?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")
            },
            &Operand::RegIndexBase(ref base, ref index) => {
                f.write_str("[")?;
                f.write_str(regspec_label(base))?;
                f.write_str(" + ")?;
                f.write_str(regspec_label(index))?;
                f.write_str("]")
            }
            &Operand::RegIndexBaseDisp(ref base, ref index, disp) => {
                write!(f, "[{} + {} ",
                    regspec_label(base),
                    regspec_label(index),
                )?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")
            },
            &Operand::RegIndexBaseScale(ref base, ref index, scale) => {
                write!(f, "[{} + {} * {}]",
                    regspec_label(base),
                    regspec_label(index),
                    colors.number(scale)
                )
            }
            &Operand::RegIndexBaseScaleDisp(ref base, ref index, scale, disp) => {
                write!(f, "[{} + {} * {} ",
                    regspec_label(base),
                    regspec_label(index),
                    colors.number(scale),
                )?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")
            },
            &Operand::RegDispMasked(ref spec, disp, ref mask_reg) => {
                write!(f, "[{} ", regspec_label(spec))?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            },
            &Operand::RegDerefMasked(ref spec, ref mask_reg) => {
                f.write_str("[")?;
                f.write_str(regspec_label(spec))?;
                f.write_str("]")?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            },
            &Operand::RegScaleMasked(ref spec, scale, ref mask_reg) => {
                write!(f, "[{} * {}]",
                    regspec_label(spec),
                    colors.number(scale)
                )?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            },
            &Operand::RegScaleDispMasked(ref spec, scale, disp, ref mask_reg) => {
                write!(f, "[{} * {} ",
                    regspec_label(spec),
                    colors.number(scale),
                )?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            },
            &Operand::RegIndexBaseMasked(ref base, ref index, ref mask_reg) => {
                f.write_str("[")?;
                f.write_str(regspec_label(base))?;
                f.write_str(" + ")?;
                f.write_str(regspec_label(index))?;
                f.write_str("]")?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            }
            &Operand::RegIndexBaseDispMasked(ref base, ref index, disp, ref mask_reg) => {
                write!(f, "[{} + {} ",
                    regspec_label(base),
                    regspec_label(index),
                )?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            },
            &Operand::RegIndexBaseScaleMasked(ref base, ref index, scale, ref mask_reg) => {
                write!(f, "[{} + {} * {}]",
                    regspec_label(base),
                    regspec_label(index),
                    colors.number(scale)
                )?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            }
            &Operand::RegIndexBaseScaleDispMasked(ref base, ref index, scale, disp, ref mask_reg) => {
                write!(f, "[{} + {} * {} ",
                    regspec_label(base),
                    regspec_label(index),
                    colors.number(scale),
                )?;
                format_number_i32(colors, f, disp, NumberStyleHint::HexSignedWithSignSplit)?;
                write!(f, "]")?;
                write!(f, "{{{}}}", regspec_label(mask_reg))
            },
            &Operand::Nothing => { Ok(()) },
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.display_with(DisplayStyle::Intel).colorize(&NoColors, fmt)
    }
}

impl<'instr> fmt::Display for InstructionDisplayer<'instr> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.colorize(&NoColors, fmt)
    }
}

/// enum controlling how `Instruction::display_with` renders instructions. `Intel` is more or less
/// intel syntax, though memory operand sizes are elided if they can be inferred from other
/// operands.
#[derive(Copy, Clone)]
pub enum DisplayStyle {
    /// intel-style syntax for instructions, like
    /// `add eax, [edx + ecx * 2 + 0x1234]`
    Intel,
    /// C-style syntax for instructions, like
    /// `eax += [edx + ecx * 2 + 0x1234]`
    C,
    // one might imagine an ATT style here, which is mostly interesting for reversing operand
    // order.
    // well.
    // it also complicates memory operands in an offset-only operand, and is just kind of awful, so
    // it's just not implemented yet.
    // ATT,
}

/// implementation of [`Display`](fmt::Display) that renders instructions using a specified display
/// style.
pub struct InstructionDisplayer<'instr> {
    pub(crate) instr: &'instr Instruction,
    pub(crate) style: DisplayStyle,
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
impl <'instr, T: fmt::Write, Y: YaxColors> Colorize<T, Y> for InstructionDisplayer<'instr> {
    fn colorize(&self, colors: &Y, out: &mut T) -> fmt::Result {
        // TODO: I DONT LIKE THIS, there is no address i can give contextualize here,
        // the address operand maybe should be optional..
        self.contextualize(colors, 0, Some(&NoContext), out)
    }
}

/// No per-operand context when contextualizing an instruction!
struct NoContext;

impl Instruction {
    pub fn write_to<T: fmt::Write>(&self, out: &mut T) -> fmt::Result {
        self.display_with(DisplayStyle::Intel).contextualize(&NoColors, 0, Some(&NoContext), out)
    }
}

fn contextualize_intel<T: fmt::Write, Y: YaxColors>(instr: &Instruction, colors: &Y, _address: u32, _context: Option<&NoContext>, out: &mut T) -> fmt::Result {
    if instr.xacquire() {
        out.write_str("xacquire ")?;
    }
    if instr.xrelease() {
        out.write_str("xrelease ")?;
    }
    if instr.prefixes.lock() {
        out.write_str("lock ")?;
    }

    if instr.prefixes.rep_any() {
        if [Opcode::MOVS, Opcode::CMPS, Opcode::LODS, Opcode::STOS, Opcode::INS, Opcode::OUTS].contains(&instr.opcode) {
            if instr.prefixes.rep() {
                write!(out, "rep ")?;
            } else if instr.prefixes.repnz() {
                write!(out, "repnz ")?;
            }
        }
    }

    out.write_str(instr.opcode.name())?;

    if instr.opcode == Opcode::XBEGIN {
        if (instr.imm as i32) >= 0 {
            return write!(out, " $+{}", colors.number(signed_i32_hex(instr.imm as i32)));
        } else {
            return write!(out, " ${}", colors.number(signed_i32_hex(instr.imm as i32)));
        }
    }

    if instr.operand_count > 0 {
        out.write_str(" ")?;

        let x = Operand::from_spec(instr, instr.operands[0]);

        const RELATIVE_BRANCHES: [Opcode; 21] = [
            Opcode::JMP, Opcode::JECXZ,
            Opcode::LOOP, Opcode::LOOPZ, Opcode::LOOPNZ,
            Opcode::JO, Opcode::JNO,
            Opcode::JB, Opcode::JNB,
            Opcode::JZ, Opcode::JNZ,
            Opcode::JNA, Opcode::JA,
            Opcode::JS, Opcode::JNS,
            Opcode::JP, Opcode::JNP,
            Opcode::JL, Opcode::JGE,
            Opcode::JLE, Opcode::JG,
        ];

        if instr.operands[0] == OperandSpec::ImmI8 || instr.operands[0] == OperandSpec::ImmI32 {
            if RELATIVE_BRANCHES.contains(&instr.opcode) {
                return match x {
                    Operand::ImmediateI8(rel) => {
                        if rel >= 0 {
                            write!(out, "$+{}", colors.number(signed_i32_hex(rel as i32)))
                        } else {
                            write!(out, "${}", colors.number(signed_i32_hex(rel as i32)))
                        }
                    }
                    Operand::ImmediateI32(rel) => {
                        if rel >= 0 {
                            write!(out, "$+{}", colors.number(signed_i32_hex(rel)))
                        } else {
                            write!(out, "${}", colors.number(signed_i32_hex(rel)))
                        }
                    }
                    _ => { unreachable!() }
                };
            }
        }

        if x.is_memory() {
            out.write_str(MEM_SIZE_STRINGS[instr.mem_size as usize - 1])?;
            out.write_str(" ")?;
        }

        if let Some(prefix) = instr.segment_override_for_op(0) {
            write!(out, "{}:", prefix)?;
        }
        x.colorize(colors, out)?;

        for i in 1..instr.operand_count {
            match instr.opcode {
                _ => {
                    match &instr.operands[i as usize] {
                        &OperandSpec::Nothing => {
                            return Ok(());
                        },
                        _ => {
                            out.write_str(", ")?;
                            let x = Operand::from_spec(instr, instr.operands[i as usize]);
                            if x.is_memory() {
                                out.write_str(MEM_SIZE_STRINGS[instr.mem_size as usize - 1])?;
                                out.write_str(" ")?;
                            }
                            if let Some(prefix) = instr.segment_override_for_op(i) {
                                write!(out, "{}:", prefix)?;
                            }
                            x.colorize(colors, out)?;
                            if let Some(evex) = instr.prefixes.evex() {
                                if evex.broadcast() && x.is_memory() {
                                    let scale = if instr.opcode == Opcode::VCVTPD2PS || instr.opcode == Opcode::VCVTTPD2UDQ || instr.opcode == Opcode::VCVTPD2UDQ || instr.opcode == Opcode::VCVTUDQ2PD || instr.opcode == Opcode::VCVTPS2PD || instr.opcode == Opcode::VCVTQQ2PS || instr.opcode == Opcode::VCVTDQ2PD || instr.opcode == Opcode::VCVTTPD2DQ || instr.opcode == Opcode::VFPCLASSPS || instr.opcode == Opcode::VFPCLASSPD || instr.opcode == Opcode::VCVTNEPS2BF16 || instr.opcode == Opcode::VCVTUQQ2PS || instr.opcode == Opcode::VCVTPD2DQ || instr.opcode == Opcode::VCVTTPS2UQQ || instr.opcode == Opcode::VCVTPS2UQQ || instr.opcode == Opcode::VCVTTPS2QQ || instr.opcode == Opcode::VCVTPS2QQ {
                                        if instr.opcode == Opcode::VFPCLASSPS || instr.opcode ==  Opcode::VCVTNEPS2BF16 {
                                            if evex.vex().l() {
                                                8
                                            } else if evex.lp() {
                                                16
                                            } else {
                                                4
                                            }
                                        } else if instr.opcode == Opcode::VFPCLASSPD {
                                            if evex.vex().l() {
                                                4
                                            } else if evex.lp() {
                                                8
                                            } else {
                                                2
                                            }
                                        } else {
                                            // vcvtpd2ps is "cool": in broadcast mode, it can read a
                                            // double-precision float (qword), resize to single-precision,
                                            // then broadcast that to the whole destination register. this
                                            // means we need to show `xmm, qword [addr]{1to4}` if vector
                                            // size is 256. likewise, scale of 8 for the same truncation
                                            // reason if vector size is 512.
                                            // vcvtudq2pd is the same story.
                                            // vfpclassp{s,d} is a mystery to me.
                                            if evex.vex().l() {
                                                4
                                            } else if evex.lp() {
                                                8
                                            } else {
                                                2
                                            }
                                        }
                                    } else {
                                        // this should never be `None` - that would imply two
                                        // memory operands for a broadcasted operation.
                                        if let Some(width) = Operand::from_spec(instr, instr.operands[i as usize - 1]).width() {
                                            width / instr.mem_size
                                        } else {
                                            0
                                        }
                                    };
                                    write!(out, "{{1to{}}}", scale)?;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

fn contextualize_c<T: fmt::Write, Y: YaxColors>(instr: &Instruction, colors: &Y, _address: u32, _context: Option<&NoContext>, out: &mut T) -> fmt::Result {
    let mut brace_count = 0;

    let mut prefixed = false;

    if instr.xacquire() {
        out.write_str("xacquire ")?;
        prefixed = true;
    }
    if instr.xrelease() {
        out.write_str("xrelease ")?;
        prefixed = true;
    }
    if instr.prefixes.lock() {
        out.write_str("lock ")?;
        prefixed = true;
    }

    if prefixed {
        out.write_str("{ ")?;
        brace_count += 1;
    }

    if instr.prefixes.rep_any() {
        if [Opcode::MOVS, Opcode::CMPS, Opcode::LODS, Opcode::STOS, Opcode::INS, Opcode::OUTS].contains(&instr.opcode) {
            let word_str = match instr.mem_size {
                1 => "byte",
                2 => "word",
                4 => "dword",
                8 => "qword",
                _ => { unreachable!("invalid word size") }
            };

            // only a few of you actually use the prefix...
            if instr.prefixes.rep() {
                out.write_str("rep ")?;
            } else if instr.prefixes.repnz() {
                out.write_str("repnz ")?;
            } // TODO: other rep kinds?

            out.write_str(word_str)?;
            out.write_str(" { ")?;
            brace_count += 1;
        }
    }

    fn write_jmp_operand<T: fmt::Write, Y: YaxColors>(op: Operand, colors: &Y, out: &mut T) -> fmt::Result {
        match op {
            Operand::ImmediateI8(rel) => {
                if rel >= 0 {
                    write!(out, "$+{}", colors.number(signed_i32_hex(rel as i32)))
                } else {
                    write!(out, "${}", colors.number(signed_i32_hex(rel as i32)))
                }
            }
            Operand::ImmediateI32(rel) => {
                if rel >= 0 {
                    write!(out, "$+{}", colors.number(signed_i32_hex(rel)))
                } else {
                    write!(out, "${}", colors.number(signed_i32_hex(rel)))
                }
            }
            other => {
                write!(out, "{}", other)
            }
        }
    }

    match instr.opcode {
        Opcode::Invalid => { out.write_str("invalid")?; },
        Opcode::MOVS => {
            out.write_str("es:[edi++] = ds:[esi++]")?;
        },
        Opcode::CMPS => {
            out.write_str("eflags = flags(ds:[esi++] - es:[edi++])")?;
        },
        Opcode::LODS => {
            // TODO: size
            out.write_str("rax = ds:[esi++]")?;
        },
        Opcode::STOS => {
            // TODO: size
            out.write_str("es:[edi++] = rax")?;
        },
        Opcode::INS => {
            // TODO: size
            out.write_str("es:[edi++] = port(dx)")?;
        },
        Opcode::OUTS => {
            // TODO: size
            out.write_str("port(dx) = ds:[esi++]")?;
        }
        Opcode::ADD => {
            write!(out, "{} += {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::OR => {
            write!(out, "{} |= {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::ADC => {
            write!(out, "{} += {} + eflags.cf", instr.operand(0), instr.operand(1))?;
        }
        Opcode::ADCX => {
            write!(out, "{} += {} + eflags.cf", instr.operand(0), instr.operand(1))?;
        }
        Opcode::ADOX => {
            write!(out, "{} += {} + eflags.of", instr.operand(0), instr.operand(1))?;
        }
        Opcode::SBB => {
            write!(out, "{} -= {} + eflags.cf", instr.operand(0), instr.operand(1))?;
        }
        Opcode::AND => {
            write!(out, "{} &= {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::XOR => {
            write!(out, "{} ^= {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::SUB => {
            write!(out, "{} -= {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::CMP => {
            write!(out, "eflags = flags({} - {})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::TEST => {
            write!(out, "eflags = flags({} & {})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::XADD => {
            write!(out, "({}, {}) = ({} + {}, {})", instr.operand(0), instr.operand(1), instr.operand(0), instr.operand(1), instr.operand(0))?;
        }
        Opcode::BT => {
            write!(out, "bt")?;
        }
        Opcode::BTS => {
            write!(out, "bts")?;
        }
        Opcode::BTC => {
            write!(out, "btc")?;
        }
        Opcode::BSR => {
            write!(out, "{} = msb({})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::BSF => {
            write!(out, "{} = lsb({}) (x86 bsf)", instr.operand(0), instr.operand(1))?;
        }
        Opcode::TZCNT => {
            write!(out, "{} = lsb({})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::MOV => {
            write!(out, "{} = {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::SAR => {
            write!(out, "{} = {} >>> {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::SAL => {
            write!(out, "{} = {} <<< {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::SHR => {
            write!(out, "{} = {} >> {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::SHRX => {
            write!(out, "{} = {} >> {} (x86 shrx)", instr.operand(0), instr.operand(1), instr.operand(2))?;
        }
        Opcode::SHL => {
            write!(out, "{} = {} << {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::SHLX => {
            write!(out, "{} = {} << {} (x86 shlx)", instr.operand(0), instr.operand(1), instr.operand(2))?;
        }
        Opcode::ROR => {
            write!(out, "{} = {} ror {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::RORX => {
            write!(out, "{} = {} ror {} (x86 rorx)", instr.operand(0), instr.operand(1), instr.operand(2))?;
        }
        Opcode::ROL => {
            write!(out, "{} = {} rol {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::RCR => {
            write!(out, "{} = {} rcr {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::RCL => {
            write!(out, "{} = {} rcl {}", instr.operand(0), instr.operand(0), instr.operand(1))?;
        }
        Opcode::PUSH => {
            write!(out, "push({})", instr.operand(0))?;
        }
        Opcode::POP => {
            write!(out, "{} = pop()", instr.operand(0))?;
        }
        Opcode::MOVD => {
            write!(out, "{} = movd({})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::MOVQ => {
            write!(out, "{} = movq({})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::MOVNTQ => {
            write!(out, "{} = movntq({})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::INC => {
            if instr.operand(0).is_memory() {
                match instr.mem_size {
                    1 => { write!(out, "byte {}++", instr.operand(0))?; },
                    2 => { write!(out, "word {}++", instr.operand(0))?; },
                    4 => { write!(out, "dword {}++", instr.operand(0))?; },
                    _ => { write!(out, "qword {}++", instr.operand(0))?; }, // sizes that are not 1, 2, or 4, *better* be 8.
                }
            } else {
                write!(out, "{}++", instr.operand(0))?;
            }
        }
        Opcode::DEC => {
            if instr.operand(0).is_memory() {
                match instr.mem_size {
                    1 => { write!(out, "byte {}--", instr.operand(0))?; },
                    2 => { write!(out, "word {}--", instr.operand(0))?; },
                    4 => { write!(out, "dword {}--", instr.operand(0))?; },
                    _ => { write!(out, "qword {}--", instr.operand(0))?; }, // sizes that are not 1, 2, or 4, *better* be 8.
                }
            } else {
                write!(out, "{}--", instr.operand(0))?;
            }
        }
        Opcode::JMP => {
            out.write_str("jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JECXZ => {
            out.write_str("if ecx == 0 then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::LOOP => {
            out.write_str("ecx--; if ecx != 0 then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::LOOPZ => {
            out.write_str("ecx--; if ecx != 0 and zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::LOOPNZ => {
            out.write_str("ecx--; if ecx != 0 and !zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JO => {
            out.write_str("if _(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JNO => {
            out.write_str("if _(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JB => {
            out.write_str("if /* unsigned */ below(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JNB => {
            out.write_str("if /* unsigned */ above_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JZ => {
            out.write_str("if zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JNZ => {
            out.write_str("if !zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JNA => {
            out.write_str("if /* unsigned */ below_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JA => {
            out.write_str("if /* unsigned */ above(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JS => {
            out.write_str("if signed(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JNS => {
            out.write_str("if !signed(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JP => {
            out.write_str("if parity(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JNP => {
            out.write_str("if !parity(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JL => {
            out.write_str("if /* signed */ less(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JGE => {
            out.write_str("if /* signed */ greater_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JLE => {
            out.write_str("if /* signed */ less_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::JG => {
            out.write_str("if /* signed */ greater(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), colors, out)?;
        },
        Opcode::NOP => {
            write!(out, "nop")?;
        }
        _ => {
            if instr.operand_count() == 0 {
                write!(out, "{}()", instr.opcode())?;
            } else {
                write!(out, "{} = {}({}", instr.operand(0), instr.opcode(), instr.operand(0))?;
                let mut comma = true;
                for i in 1..instr.operand_count() {
                    if comma {
                        write!(out, ", ")?;
                    }
                    write!(out, "{}", instr.operand(i))?;
                    comma = true;
                }
                write!(out, ")")?;
            }
        }
    }

    while brace_count > 0 {
        out.write_str(" }")?;
        brace_count -= 1;
    }

    Ok(())
}

impl <'instr, T: fmt::Write, Y: YaxColors> ShowContextual<u32, NoContext, T, Y> for InstructionDisplayer<'instr> {
    fn contextualize(&self, colors: &Y, address: u32, context: Option<&NoContext>, out: &mut T) -> fmt::Result {
        let InstructionDisplayer {
            instr,
            style,
        } = self;

        match style {
            DisplayStyle::Intel => {
                contextualize_intel(instr, colors, address, context, out)
            }
            DisplayStyle::C => {
                contextualize_c(instr, colors, address, context, out)
            }
        }
    }
}

#[cfg(feature="std")]
impl <T: fmt::Write, Y: YaxColors> ShowContextual<u64, [Option<alloc::string::String>], T, Y> for Instruction {
    fn contextualize(&self, colors: &Y, _address: u64, context: Option<&[Option<alloc::string::String>]>, out: &mut T) -> fmt::Result {
        if self.prefixes.lock() {
            write!(out, "lock ")?;
        }

        if [Opcode::MOVS, Opcode::CMPS, Opcode::LODS, Opcode::STOS, Opcode::INS, Opcode::OUTS].contains(&self.opcode) {
            // only a few of you actually use the prefix...
            if self.prefixes.rep() {
                write!(out, "rep ")?;
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
        for i in 1..self.operand_count {
            let i = i as usize;
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
        Ok(())
    }
}
