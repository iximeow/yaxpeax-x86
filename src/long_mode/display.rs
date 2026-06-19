mod masm;

use core::fmt;

// allowing these deprecated items for the time being, not yet breaking yaxpeax-x86 apis
#[allow(deprecated)]
use yaxpeax_arch::{Colorize, ShowContextual, NoColors, YaxColors};

use crate::MEM_SIZE_STRINGS;
use crate::long_mode::{
    RegSpec, Opcode, Operand, OperandSpec,
    MergeMode, InstDecoder, Instruction, Segment, PrefixRex
};

use yaxpeax_arch::display::DisplaySink;
use yaxpeax_arch::safer_unchecked::GetSaferUnchecked as _;
use yaxpeax_arch::safer_unchecked::unreachable_kinda_unchecked as unreachable_unchecked;

trait DisplaySinkExt {
    // `write_opcode` depends on all mnemonics being less than 32 bytes long. check that here, at
    // compile time. referenced later to force evaluation of this const.
    const MNEMONIC_LT_32: () = {
        let mut i = 0;
        while i < MNEMONICS.len() {
            let name = &MNEMONICS[i];
            if name.len() >= 32 {
                panic!("mnemonic too long");
            }
            i += 1;
        }
    };

    // `write_reg` depends on all register names being less than 8 bytes long. check that here, at
    // compile time. referenced later to force evaluation of this const.
    const REG_LABEL_LT_8: () = {
        let mut i = 0;
        while i < REG_NAMES.len() {
            let name = &REG_NAMES[i];
            if name.len() >= 8 {
                panic!("register name too long");
            }
            i += 1;
        }
    };

    // `write_mem_size_label` depends on all memory size labels being less than 8 bytes long. check
    // that here, at compile time. referenced later to force evaluation of this const.
    const MEM_SIZE_LABEL_LT_8: () = {
        let mut i = 0;
        while i < crate::MEM_SIZE_STRINGS.len() {
            let name = &MEM_SIZE_STRINGS[i];
            if name.len() >= 8 {
                panic!("memory label name too long");
            }
            i += 1;
        }
    };

    // `write_sae_mode` depends on all sae mode labels being less than 16 bytes long. check that
    // here, at compile time. referenced later to force evaluation of this const.
    const SAE_LABEL_LT_16: () = {
        let mut i = 0;
        while i < super::SAE_MODES.len() {
            let mode = &super::SAE_MODES[i];
            if mode.label().len() >= 16 {
                panic!("sae mode label too long");
            }
            i += 1;
        }
    };

    fn write_opcode(&mut self, opcode: super::Opcode) -> Result<(), core::fmt::Error>;
    fn write_reg(&mut self, reg: RegSpec) -> Result<(), core::fmt::Error>;
    fn write_displacement(&mut self, disp: i32) -> Result<(), core::fmt::Error>;
    fn write_scale(&mut self, scale: u8) -> Result<(), core::fmt::Error>;
    fn write_mem_size_label(&mut self, mem_size: u8) -> Result<(), core::fmt::Error>;
    fn write_sae_mode(&mut self, sae: super::SaeMode) -> Result<(), core::fmt::Error>;
}

impl<T: DisplaySink> DisplaySinkExt for T {
    #[inline(always)]
    fn write_opcode(&mut self, opcode: super::Opcode) -> Result<(), core::fmt::Error> {
        self.span_start_opcode();
        let name = opcode.name();

        let _ = Self::MNEMONIC_LT_32;
        // Safety: all opcode mnemonics are 31 bytes or fewer.
        unsafe { self.write_lt_32(name)?; }
        self.span_end_opcode();
        Ok(())
    }

    #[inline(always)]
    fn write_reg(&mut self, reg: RegSpec) -> Result<(), core::fmt::Error> {
        self.span_start_register();
        let label = regspec_label(&reg);

        let _ = Self::REG_LABEL_LT_8;
        // Safety: all register labels are 7 bytes or fewer.
        unsafe { self.write_lt_8(label)?; }
        self.span_end_register();
        Ok(())
    }

    #[inline(always)]
    fn write_displacement(&mut self, disp: i32) -> Result<(), core::fmt::Error> {
        let mut v = disp as u32;
        if disp < 0 {
            self.write_fixed_size("- ")?;
            v = disp.unsigned_abs();
        } else {
            self.write_fixed_size("+ ")?;
        }
        self.span_start_number();
        self.write_prefixed_u32(v)?;
        self.span_end_number();
        Ok(())
    }

    #[inline(always)]
    fn write_scale(&mut self, scale: u8) -> Result<(), core::fmt::Error> {
        self.span_start_number();
        self.write_char((0x30 + scale) as char)?; // translate scale=1 to '1', scale=2 to '2', etc
        self.span_end_number();
        Ok(())
    }

    #[inline(always)]
    fn write_mem_size_label(&mut self, mem_size: u8) -> Result<(), core::fmt::Error> {
        let label = mem_size_label(mem_size);
        let _ = Self::MEM_SIZE_LABEL_LT_8;
        // Safety: all memory size labels are 7 bytes or fewer
        unsafe { self.write_lt_8(label) }
    }

    #[inline(always)]
    fn write_sae_mode(&mut self, sae_mode: super::SaeMode) -> Result<(), core::fmt::Error> {
        let label = sae_mode.label();

        let _ = Self::SAE_LABEL_LT_16;
        // Safety: all sae labels are 15 bytes or fewer.
        unsafe { self.write_lt_16(label) }
    }
}

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

impl Segment {
    fn name(&self) -> &'static [u8; 2] {
        match self {
            Segment::CS => b"cs",
            Segment::DS => b"ds",
            Segment::ES => b"es",
            Segment::FS => b"fs",
            Segment::GS => b"gs",
            Segment::SS => b"ss",
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
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh",
    "ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
    "BUG", "BUG", "BUG", "BUG", "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
    "cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7", "cr8", "cr9", "cr10", "cr11", "cr12", "cr13", "cr14", "cr15",
    "dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7", "dr8", "dr9", "dr10", "dr11", "dr12", "dr13", "dr14", "dr15",
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
    "rip", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "eflags", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "rflags", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
];

pub(crate) fn regspec_label(spec: &RegSpec) -> &'static str {
    unsafe { REG_NAMES.get_kinda_unchecked((spec.num as u16 + ((spec.bank as u16) << 3)) as usize) }
}

pub(crate) fn mem_size_label(size: u8) -> &'static str {
    unsafe { MEM_SIZE_STRINGS.get_kinda_unchecked(size as usize) }
}

impl fmt::Display for RegSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(regspec_label(self))
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        // to reuse one implementation, call the deprecated function for now.
        #[allow(deprecated)]
        self.colorize(&NoColors, fmt)
    }
}

// allowing these deprecated items for the time being, not yet breaking yaxpeax-x86 apis
#[allow(deprecated)]
impl <T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Operand {
    fn colorize(&self, _colors: &Y, f: &mut T) -> fmt::Result {
        let mut f = yaxpeax_arch::display::FmtSink::new(f);
        let rules = DefaultRules::for_style(DisplayStyle::Intel);
        let mut visitor = DisplayingOperandVisitor {
            f: &mut f,
            rules: &rules,
        };
        self.visit(&mut visitor)
    }
}

struct DisplayingOperandVisitor<'a, 'rules, T, R> {
    f: &'a mut T,
    rules: &'rules R,
}

impl <T: DisplaySink, R: DisplayRules<T>> DisplayingOperandVisitor<'_, '_, T, R> {
    fn write_register(&mut self, reg: RegSpec) -> Result<(), core::fmt::Error> {
        if self.rules.emit_register(reg, &mut self.f)? {
            return Ok(());
        }

        self.f.write_reg(reg)
    }
}

impl <T: DisplaySink, R: DisplayRules<T>> super::OperandVisitor for DisplayingOperandVisitor<'_, '_, T, R> {
    type Ok = ();
    type Error = core::fmt::Error;

    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_u8(&mut self, imm: u8) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_unsigned_immediate(imm as u64, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        self.f.write_fixed_size("0x")?;
        self.f.write_u8(imm)?;
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i8(&mut self, imm: i8) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_signed_immediate(imm as i64, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        let mut v = imm as u8;
        if imm < 0 {
            self.f.write_char('-')?;
            v = imm.unsigned_abs();
        }
        self.f.write_fixed_size("0x")?;
        self.f.write_u8(v)?;
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_u16(&mut self, imm: u16) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_unsigned_immediate(imm as u64, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        self.f.write_fixed_size("0x")?;
        self.f.write_u16(imm)?;
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i16(&mut self, imm: i16) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_signed_immediate(imm as i64, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        let mut v = imm as u16;
        if imm < 0 {
            self.f.write_char('-')?;
            v = imm.unsigned_abs();
        }
        self.f.write_fixed_size("0x")?;
        self.f.write_u16(v)?;
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_u32(&mut self, imm: u32) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_unsigned_immediate(imm as u64, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        self.f.write_fixed_size("0x")?;
        self.f.write_u32(imm)?;
        self.f.span_end_immediate();
        Ok(())
    }
    fn visit_i32(&mut self, imm: i32) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_signed_immediate(imm as i64, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        let mut v = imm as u32;
        if imm < 0 {
            self.f.write_char('-')?;
            v = imm.unsigned_abs();
        }
        self.f.write_fixed_size("0x")?;
        self.f.write_u32(v)?;
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_u64(&mut self, imm: u64) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_unsigned_immediate(imm, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        self.f.write_fixed_size("0x")?;
        self.f.write_u64(imm)?;
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i64(&mut self, imm: i64) -> Result<Self::Ok, Self::Error> {
        if self.rules.emit_signed_immediate(imm, self.f)? {
            return Ok(());
        }

        self.f.span_start_immediate();
        let mut v = imm as u64;
        if imm < 0 {
            self.f.write_char('-')?;
            v = imm.unsigned_abs();
        }
        self.f.write_fixed_size("0x")?;
        self.f.write_u64(v)?;
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_reg(&mut self, reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.write_register(reg)?;
        Ok(())
    }
    fn visit_reg_mask_merge(&mut self, spec: RegSpec, mask: RegSpec, merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        self.write_register(spec)?;
        if mask.num != 0 {
            self.f.write_fixed_size("{")?;
            self.write_register(mask)?;
            self.f.write_fixed_size("}")?;
        }
        if let MergeMode::Zero = merge_mode {
            self.f.write_fixed_size("{z}")?;
        }
        Ok(())
    }
    fn visit_reg_mask_merge_sae(&mut self, spec: RegSpec, mask: RegSpec, merge_mode: MergeMode, sae_mode: crate::long_mode::SaeMode) -> Result<Self::Ok, Self::Error> {
        self.write_register(spec)?;
        if mask.num != 0 {
            self.f.write_fixed_size("{")?;
            self.write_register(mask)?;
            self.f.write_fixed_size("}")?;
        }
        if let MergeMode::Zero = merge_mode {
            self.f.write_fixed_size("{z}")?;
        }
        self.f.write_sae_mode(sae_mode)?;
        Ok(())
    }
    fn visit_reg_mask_merge_sae_noround(&mut self, spec: RegSpec, mask: RegSpec, merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        self.write_register(spec)?;
        if mask.num != 0 {
            self.f.write_fixed_size("{")?;
            self.write_register(mask)?;
            self.f.write_fixed_size("}")?;
        }
        if let MergeMode::Zero = merge_mode {
            self.f.write_fixed_size("{z}")?;
        }
        self.f.write_fixed_size("{sae}")?;
        Ok(())
    }
    fn visit_abs_u32(&mut self, imm: u32) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        if !self.rules.emit_absolute_address(imm as u64, self.f)? {
            self.f.span_start_address();
            self.f.write_prefixed_u32(imm)?;
            self.f.span_end_address();
        }
        self.f.write_fixed_size("]")?;
        Ok(())
    }
    fn visit_abs_u64(&mut self, imm: u64) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        if !self.rules.emit_absolute_address(imm as u64, self.f)? {
            self.f.span_start_address();
            self.f.write_prefixed_u64(imm)?;
            self.f.span_end_address();
        }
        self.f.write_fixed_size("]")?;
        Ok(())
    }
    #[cfg_attr(not(feature="profiling"), inline(always))]
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_disp(&mut self, base: RegSpec, disp: i32) -> Result<Self::Ok, Self::Error> {
        self.f.write_char('[')?;
        let mut printed = false;
        if base == RegSpec::rip() {
            printed = self.rules.emit_relative_address(disp, self.f)?;
        }
        if !printed {
            self.write_register(base)?;
            self.f.write_fixed_size(" ")?;
            self.f.write_displacement(disp)?;
        }
        self.f.write_fixed_size("]")
    }
    fn visit_deref(&mut self, base: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size("]")
    }
    fn visit_index_scale(&mut self, index: RegSpec, scale: u8) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_fixed_size("]")?;

        Ok(())
    }
    fn visit_index_scale_disp(&mut self, index: RegSpec, scale: u8, disp: i32) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_fixed_size(" ")?;
        self.f.write_displacement(disp)?;
        self.f.write_char(']')
    }
    fn visit_base_index_scale(&mut self, base: RegSpec, index: RegSpec, scale: u8) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size(" + ")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_fixed_size("]")
    }
    fn visit_base_index_scale_disp(&mut self, base: RegSpec, index: RegSpec, scale: u8, disp: i32) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size(" + ")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_fixed_size(" ")?;
        self.f.write_displacement(disp)?;
        self.f.write_fixed_size("]")
    }
    fn visit_disp_masked(&mut self, base: RegSpec, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_char('[')?;
        self.write_register(base)?;
        self.f.write_char(' ')?;
        self.f.write_displacement(disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_deref_masked(&mut self, base: RegSpec, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_index_scale_masked(&mut self, index: RegSpec, scale: u8, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_index_scale_disp_masked(&mut self, index: RegSpec, scale: u8, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_fixed_size(" ")?;
        self.f.write_displacement(disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_masked(&mut self, base: RegSpec, index: RegSpec, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size(" + ")?;
        self.write_register(index)?;
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_disp_masked(&mut self, base: RegSpec, index: RegSpec, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size(" + ")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" ")?;
        self.f.write_displacement(disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_scale_masked(&mut self, base: RegSpec, index: RegSpec, scale: u8, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size(" + ")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_scale_disp_masked(&mut self, base: RegSpec, index: RegSpec, scale: u8, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.write_register(base)?;
        self.f.write_fixed_size(" + ")?;
        self.write_register(index)?;
        self.f.write_fixed_size(" * ")?;
        self.f.write_scale(scale)?;
        self.f.write_char(' ')?;
        self.f.write_displacement(disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.write_register(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }

    fn visit_other(&mut self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

const MNEMONICS: &[&'static str] = &[
    "add",
    "or",
    "adc",
    "sbb",
    "and",
    "sub",
    "xor",
    "cmp",
    "rol",
    "ror",
    "rcl",
    "rcr",
    "shl",
    "shr",
    "sal",
    "sar",
    "btc",
    "btr",
    "bts",
    "cmpxchg",
    "cmpxchg8b",
    "cmpxchg16b",
    "dec",
    "inc",
    "neg",
    "not",
    "xadd",
    "xchg",
    "cmps",
    "scas",
    "movs",
    "lods",
    "stos",
    "ins",
    "outs",
    "invalid",
    "bt",
    "bsf",
    "bsr",
    "tzcnt",
    "movss",
    "addss",
    "subss",
    "mulss",
    "divss",
    "minss",
    "maxss",
    "sqrtss",
    "movsd",
    "sqrtsd",
    "addsd",
    "subsd",
    "mulsd",
    "divsd",
    "minsd",
    "maxsd",
    "movsldup",
    "movshdup",
    "movddup",
    "haddps",
    "hsubps",
    "addsubpd",
    "addsubps",
    "cvtsi2ss",
    "cvtsi2sd",
    "cvttsd2si",
    "cvttps2dq",
    "cvtpd2dq",
    "cvtpd2ps",
    "cvtps2dq",
    "cvtsd2si",
    "cvtsd2ss",
    "cvttss2si",
    "cvtss2si",
    "cvtss2sd",
    "cvtdq2pd",
    "lddqu",
    "movzx",
    "movsx",
    "movsxd",
    "shrd",
//    "inc",
//    "dec",
    "hlt",
    "call",
    "callf",
    "jmp",
    "jmpf",
    "push",
    "pop",
    "lea",
    "nop",
    "prefetchnta",
    "prefetcht0",
    "prefetcht1",
    "prefetcht2",
//    "xchg",
    "popf",
    "int",
    "into",
    "iret",
    "iretd",
    "iretq",
    "retf",
    "enter",
    "leave",
    "mov",
    "ret",
    "pushf",
    "wait",
    "cbw",
    "cwde",
    "cdqe",
    "cwd",
    "cdq",
    "cqo",
    "lahf",
    "sahf",
    "test",
    "in",
    "out",
    "imul",
    "jo",
    "jno",
    "jb",
    "jnb",
    "jz",
    "jnz",
    "ja",
    "jna",
    "js",
    "jns",
    "jp",
    "jnp",
    "jl",
    "jge",
    "jle",
    "jg",
    "cmova",
    "cmovb",
    "cmovg",
    "cmovge",
    "cmovl",
    "cmovle",
    "cmovna",
    "cmovnb",
    "cmovno",
    "cmovnp",
    "cmovns",
    "cmovnz",
    "cmovo",
    "cmovp",
    "cmovs",
    "cmovz",
    "div",
    "idiv",
    "mul",
//    "neg",
//    "not",
//    "cmpxchg",
    "seto",
    "setno",
    "setb",
    "setae",
    "setz",
    "setnz",
    "setbe",
    "seta",
    "sets",
    "setns",
    "setp",
    "setnp",
    "setl",
    "setge",
    "setle",
    "setg",
    "cpuid",
    "ud0",
    "ud1",
    "ud2",
    "wbinvd",
    "invd",
    "sysret",
    "clts",
    "syscall",
    "lsl",
    "lar",
    "sgdt",
    "sidt",
    "lgdt",
    "lidt",
    "smsw",
    "lmsw",
    "swapgs",
    "rdtscp",
    "invlpg",
    "fxsave",
    "fxrstor",
    "ldmxcsr",
    "stmxcsr",
    "xsave",
    "xrstor",
    "xsaveopt",
    "lfence",
    "mfence",
    "sfence",
    "clflush",
    "clflushopt",
    "clwb",
    "wrmsr",
    "rdtsc",
    "rdmsr",
    "rdpmc",
    "sldt",
    "str",
    "lldt",
    "ltr",
    "verr",
    "verw",
    "cmc",
    "clc",
    "stc",
    "cli",
    "sti",
    "cld",
    "std",
    "jmpe",
    "popcnt",
    "movdqu",
    "movdqa",
    "movq",
    "cmpss",
    "cmpsd",
    "unpcklps",
    "unpcklpd",
    "unpckhps",
    "unpckhpd",
    "pshufhw",
    "pshuflw",
    "movups",
    "movq2dq",
    "movdq2q",
    "rsqrtss",
    "rcpss",

    "andn",
    "bextr",
    "blsi",
    "blsmsk",
    "blsr",
    "vmclear",
    "vmxon",
    "vmcall",
    "vmlaunch",
    "vmresume",
    "vmxoff",
    "pconfig",
    "monitor",
    "mwait",
    "monitorx",
    "mwaitx",
    "clac",
    "stac",
    "encls",
    "enclv",
    "xgetbv",
    "xsetbv",
    "vmfunc",
    "xabort",
    "xbegin",
    "xend",
    "xtest",
    "enclu",
    "rdpkru",
    "wrpkru",

    "rdpru",
    "clzero",

    "rdseed",
    "rdrand",

    "addps",
    "addpd",
    "andnps",
    "andnpd",
    "andps",
    "andpd",
    "bswap",
    "cmppd",
    "cmpps",
    "comisd",
    "comiss",
    "cvtdq2ps",
    "cvtpi2ps",
    "cvtpi2pd",
    "cvtps2pd",
    "cvtps2pi",
    "cvtpd2pi",
    "cvttps2pi",
    "cvttpd2pi",
    "cvttpd2dq",
    "divps",
    "divpd",
    "emms",
    "getsec",
    "lfs",
    "lgs",
    "lss",
    "maskmovq",
    "maskmovdqu",
    "maxps",
    "maxpd",
    "minps",
    "minpd",
    "movaps",
    "movapd",
    "movd",
    "movlps",
    "movlpd",
    "movhps",
    "movhpd",
    "movlhps",
    "movhlps",
    "movupd",
    "movmskps",
    "movmskpd",
    "movnti",
    "movntps",
    "movntpd",
    "extrq",
    "insertq",
    "movntss",
    "movntsd",
    "movntq",
    "movntdq",
    "mulps",
    "mulpd",
    "orps",
    "orpd",
    "packssdw",
    "packsswb",
    "packuswb",
    "paddb",
    "paddd",
    "paddq",
    "paddsb",
    "paddsw",
    "paddusb",
    "paddusw",
    "paddw",
    "pand",
    "pandn",
    "pavgb",
    "pavgw",
    "pcmpeqb",
    "pcmpeqd",
    "pcmpeqw",
    "pcmpgtb",
    "pcmpgtd",
    "pcmpgtw",
    "pinsrw",
    "pmaddwd",
    "pmaxsw",
    "pmaxub",
    "pminsw",
    "pminub",
    "pmovmskb",
    "pmulhuw",
    "pmulhw",
    "pmullw",
    "pmuludq",
    "por",
    "psadbw",
    "pshufw",
    "pshufd",
    "pslld",
    "pslldq",
    "psllq",
    "psllw",
    "psrad",
    "psraw",
    "psrld",
    "psrldq",
    "psrlq",
    "psrlw",
    "psubb",
    "psubd",
    "psubq",
    "psubsb",
    "psubsw",
    "psubusb",
    "psubusw",
    "psubw",
    "punpckhbw",
    "punpckhdq",
    "punpckhwd",
    "punpcklbw",
    "punpckldq",
    "punpcklwd",
    "punpcklqdq",
    "punpckhqdq",
    "pxor",
    "rcpps",
    "rsm",
    "rsqrtps",
    "shld",
    "shufpd",
    "shufps",
    "slhd",
    "sqrtps",
    "sqrtpd",
    "subps",
    "subpd",
    "sysenter",
    "sysexit",
    "ucomisd",
    "ucomiss",
    "vmread",
    "vmwrite",
    "xorps",
    "xorpd",

    "vmovddup",
    "vpshuflw",
    "vpshufhw",
    "vhaddps",
    "vhsubps",
    "vaddsubps",
    "vcvtpd2dq",
    "vlddqu",

    "vcomisd",
    "vcomiss",
    "vucomisd",
    "vucomiss",
    "vaddpd",
    "vaddps",
    "vaddsd",
    "vaddss",
    "vaddsubpd",
    "vaesdec",
    "vaesdeclast",
    "vaesenc",
    "vaesenclast",
    "vaesimc",
    "vaeskeygenassist",
    "vblendpd",
    "vblendps",
    "vblendvpd",
    "vblendvps",
    "vbroadcastf128",
    "vbroadcasti128",
    "vbroadcastsd",
    "vbroadcastss",
    "vcmpsd",
    "vcmpss",
    "vcmppd",
    "vcmpps",
    "vcvtdq2pd",
    "vcvtdq2ps",
    "vcvtpd2ps",
    "vcvtph2ps",
    "vcvtps2dq",
    "vcvtps2pd",
    "vcvtss2sd",
    "vcvtsi2ss",
    "vcvtsi2sd",
    "vcvtsd2si",
    "vcvtsd2ss",
    "vcvtps2ph",
    "vcvtss2si",
    "vcvttpd2dq",
    "vcvttps2dq",
    "vcvttss2si",
    "vcvttsd2si",
    "vdivpd",
    "vdivps",
    "vdivsd",
    "vdivss",
    "vdppd",
    "vdpps",
    "vextractf128",
    "vextracti128",
    "vextractps",
    "vfmadd132pd",
    "vfmadd132ps",
    "vfmadd132sd",
    "vfmadd132ss",
    "vfmadd213pd",
    "vfmadd213ps",
    "vfmadd213sd",
    "vfmadd213ss",
    "vfmadd231pd",
    "vfmadd231ps",
    "vfmadd231sd",
    "vfmadd231ss",
    "vfmaddsub132pd",
    "vfmaddsub132ps",
    "vfmaddsub213pd",
    "vfmaddsub213ps",
    "vfmaddsub231pd",
    "vfmaddsub231ps",
    "vfmsub132pd",
    "vfmsub132ps",
    "vfmsub132sd",
    "vfmsub132ss",
    "vfmsub213pd",
    "vfmsub213ps",
    "vfmsub213sd",
    "vfmsub213ss",
    "vfmsub231pd",
    "vfmsub231ps",
    "vfmsub231sd",
    "vfmsub231ss",
    "vfmsubadd132pd",
    "vfmsubadd132ps",
    "vfmsubadd213pd",
    "vfmsubadd213ps",
    "vfmsubadd231pd",
    "vfmsubadd231ps",
    "vfnmadd132pd",
    "vfnmadd132ps",
    "vfnmadd132sd",
    "vfnmadd132ss",
    "vfnmadd213pd",
    "vfnmadd213ps",
    "vfnmadd213sd",
    "vfnmadd213ss",
    "vfnmadd231pd",
    "vfnmadd231ps",
    "vfnmadd231sd",
    "vfnmadd231ss",
    "vfnmsub132pd",
    "vfnmsub132ps",
    "vfnmsub132sd",
    "vfnmsub132ss",
    "vfnmsub213pd",
    "vfnmsub213ps",
    "vfnmsub213sd",
    "vfnmsub213ss",
    "vfnmsub231pd",
    "vfnmsub231ps",
    "vfnmsub231sd",
    "vfnmsub231ss",
    "vgatherdpd",
    "vgatherdps",
    "vgatherqpd",
    "vgatherqps",
    "vhaddpd",
    "vhsubpd",
    "vinsertf128",
    "vinserti128",
    "vinsertps",
    "vmaskmovdqu",
    "vmaskmovpd",
    "vmaskmovps",
    "vmaxpd",
    "vmaxps",
    "vmaxsd",
    "vmaxss",
    "vminpd",
    "vminps",
    "vminsd",
    "vminss",
    "vmovapd",
    "vmovaps",
    "vmovd",
    "vmovdqa",
    "vmovdqu",
    "vmovhlps",
    "vmovhpd",
    "vmovhps",
    "vmovlhps",
    "vmovlpd",
    "vmovlps",
    "vmovmskpd",
    "vmovmskps",
    "vmovntdq",
    "vmovntdqa",
    "vmovntpd",
    "vmovntps",
    "vmovq",
    "vmovss",
    "vmovsd",
    "vmovshdup",
    "vmovsldup",
    "vmovupd",
    "vmovups",
    "vmpsadbw",
    "vmulpd",
    "vmulps",
    "vmulsd",
    "vmulss",
    "vpabsb",
    "vpabsd",
    "vpabsw",
    "vpackssdw",
    "vpackusdw",
    "vpacksswb",
    "vpackuswb",
    "vpaddb",
    "vpaddd",
    "vpaddq",
    "vpaddsb",
    "vpaddsw",
    "vpaddusb",
    "vpaddusw",
    "vpaddw",
    "vpalignr",
    "vandpd",
    "vandps",
    "vorpd",
    "vorps",
    "vandnpd",
    "vandnps",
    "vpand",
    "vpandn",
    "vpavgb",
    "vpavgw",
    "vpblendd",
    "vpblendvb",
    "vpblendw",
    "vpbroadcastb",
    "vpbroadcastd",
    "vpbroadcastq",
    "vpbroadcastw",
    "vpclmulqdq",
    "vpcmpeqb",
    "vpcmpeqd",
    "vpcmpeqq",
    "vpcmpeqw",
    "vpcmpgtb",
    "vpcmpgtd",
    "vpcmpgtq",
    "vpcmpgtw",
    "vpcmpestri",
    "vpcmpestrm",
    "vpcmpistri",
    "vpcmpistrm",
    "vperm2f128",
    "vperm2i128",
    "vpermd",
    "vpermilpd",
    "vpermilps",
    "vpermpd",
    "vpermps",
    "vpermq",
    "vpextrb",
    "vpextrd",
    "vpextrq",
    "vpextrw",
    "vpgatherdd",
    "vpgatherdq",
    "vpgatherqd",
    "vpgatherqq",
    "vphaddd",
    "vphaddsw",
    "vphaddw",
    "vpmaddubsw",
    "vphminposuw",
    "vphsubd",
    "vphsubsw",
    "vphsubw",
    "vpinsrb",
    "vpinsrd",
    "vpinsrq",
    "vpinsrw",
    "vpmaddwd",
    "vpmaskmovd",
    "vpmaskmovq",
    "vpmaxsb",
    "vpmaxsd",
    "vpmaxsw",
    "vpmaxub",
    "vpmaxuw",
    "vpmaxud",
    "vpminsb",
    "vpminsw",
    "vpminsd",
    "vpminub",
    "vpminuw",
    "vpminud",
    "vpmovmskb",
    "vpmovsxbd",
    "vpmovsxbq",
    "vpmovsxbw",
    "vpmovsxdq",
    "vpmovsxwd",
    "vpmovsxwq",
    "vpmovzxbd",
    "vpmovzxbq",
    "vpmovzxbw",
    "vpmovzxdq",
    "vpmovzxwd",
    "vpmovzxwq",
    "vpmuldq",
    "vpmulhrsw",
    "vpmulhuw",
    "vpmulhw",
    "vpmullq",
    "vpmulld",
    "vpmullw",
    "vpmuludq",
    "vpor",
    "vpsadbw",
    "vpshufb",
    "vpshufd",
    "vpsignb",
    "vpsignd",
    "vpsignw",
    "vpslld",
    "vpslldq",
    "vpsllq",
    "vpsllvd",
    "vpsllvq",
    "vpsllw",
    "vpsrad",
    "vpsravd",
    "vpsraw",
    "vpsrld",
    "vpsrldq",
    "vpsrlq",
    "vpsrlvd",
    "vpsrlvq",
    "vpsrlw",
    "vpsubb",
    "vpsubd",
    "vpsubq",
    "vpsubsb",
    "vpsubsw",
    "vpsubusb",
    "vpsubusw",
    "vpsubw",
    "vptest",
    "vpunpckhbw",
    "vpunpckhdq",
    "vpunpckhqdq",
    "vpunpckhwd",
    "vpunpcklbw",
    "vpunpckldq",
    "vpunpcklqdq",
    "vpunpcklwd",
    "vpxor",
    "vrcpps",
    "vroundpd",
    "vroundps",
    "vroundsd",
    "vroundss",
    "vrsqrtps",
    "vrsqrtss",
    "vrcpss",
    "vshufpd",
    "vshufps",
    "vsqrtpd",
    "vsqrtps",
    "vsqrtss",
    "vsqrtsd",
    "vsubpd",
    "vsubps",
    "vsubsd",
    "vsubss",
    "vtestpd",
    "vtestps",
    "vunpckhpd",
    "vunpckhps",
    "vunpcklpd",
    "vunpcklps",
    "vxorpd",
    "vxorps",
    "vzeroupper",
    "vzeroall",
    "vldmxcsr",
    "vstmxcsr",

    "pclmulqdq",
    "aeskeygenassist",
    "aesimc",
    "aesenc",
    "aesenclast",
    "aesdec",
    "aesdeclast",
    "pcmpgtq",
    "pcmpistrm",
    "pcmpistri",
    "pcmpestri",
    "packusdw",
    "pcmpestrm",
    "pcmpeqq",
    "ptest",
    "phminposuw",
    "dpps",
    "dppd",
    "mpsadbw",
    "pmovzxdq",
    "pmovsxdq",
    "pmovzxbd",
    "pmovsxbd",
    "pmovzxwq",
    "pmovsxwq",
    "pmovzxbq",
    "pmovsxbq",
    "pmovsxwd",
    "pmovzxwd",
    "pextrq",
    "pextrd",
    "pextrw",
    "pextrb",
    "pmovsxbw",
    "pmovzxbw",
    "pinsrq",
    "pinsrd",
    "pinsrb",
    "extractps",
    "insertps",
    "roundss",
    "roundsd",
    "roundps",
    "roundpd",
    "pmaxsb",
    "pmaxsd",
    "pmaxuw",
    "pmaxud",
    "pminsd",
    "pminsb",
    "pminud",
    "pminuw",
    "blendw",
    "pblendvb",
    "pblendw",
    "blendvps",
    "blendvpd",
    "blendps",
    "blendpd",
    "pmuldq",
    "movntdqa",
    "pmulld",
    "palignr",
    "psignw",
    "psignd",
    "psignb",
    "pshufb",
    "pmulhrsw",
    "pmaddubsw",
    "pabsd",
    "pabsw",
    "pabsb",
    "phsubsw",
    "phsubw",
    "phsubd",
    "phaddd",
    "phaddsw",
    "phaddw",
    "hsubpd",
    "haddpd",

    "sha1rnds4",
    "sha1nexte",
    "sha1msg1",
    "sha1msg2",
    "sha256rnds2",
    "sha256msg1",
    "sha256msg2",

    "lzcnt",
    "clgi",
    "stgi",
    "skinit",
    "vmload",
    "vmmcall",
    "vmsave",
    "vmrun",
    "invlpga",
    "invlpgb",
    "tlbsync",

    "movbe",

    "adcx",
    "adox",

    "prefetchw",

    "rdpid",
//    "cmpxchg8b",
//    "cmpxchg16b",
    "vmptrld",
    "vmptrst",

    "bzhi",
    "mulx",
    "shlx",
    "shrx",
    "sarx",
    "pdep",
    "pext",
    "rorx",
    "xrstors",
    "xrstors64",
    "xsavec",
    "xsavec64",
    "xsaves",
    "xsaves64",

    "rdfsbase",
    "rdgsbase",
    "wrfsbase",
    "wrgsbase",

    "crc32",
    "salc",
    "xlat",

    "f2xm1",
    "fabs",
    "fadd",
    "faddp",
    "fbld",
    "fbstp",
    "fchs",
    "fcmovb",
    "fcmovbe",
    "fcmove",
    "fcmovnb",
    "fcmovnbe",
    "fcmovne",
    "fcmovnu",
    "fcmovu",
    "fcom",
    "fcomi",
    "fcomip",
    "fcomp",
    "fcompp",
    "fcos",
    "fdecstp",
    "fdisi8087_nop",
    "fdiv",
    "fdivp",
    "fdivr",
    "fdivrp",
    "feni8087_nop",
    "ffree",
    "ffreep",
    "fiadd",
    "ficom",
    "ficomp",
    "fidiv",
    "fidivr",
    "fild",
    "fimul",
    "fincstp",
    "fist",
    "fistp",
    "fisttp",
    "fisub",
    "fisubr",
    "fld",
    "fld1",
    "fldcw",
    "fldenv",
    "fldl2e",
    "fldl2t",
    "fldlg2",
    "fldln2",
    "fldpi",
    "fldz",
    "fmul",
    "fmulp",
    "fnclex",
    "fninit",
    "fnop",
    "fnsave",
    "fnstcw",
    "fnstenv",
    "fnstor",
    "fnstsw",
    "fpatan",
    "fprem",
    "fprem1",
    "fptan",
    "frndint",
    "frstor",
    "fscale",
    "fsetpm287_nop",
    "fsin",
    "fsincos",
    "fsqrt",
    "fst",
    "fstp",
    "fstpnce",
    "fsub",
    "fsubp",
    "fsubr",
    "fsubrp",
    "ftst",
    "fucom",
    "fucomi",
    "fucomip",
    "fucomp",
    "fucompp",
    "fxam",
    "fxch",
    "fxtract",
    "fyl2x",
    "fyl2xp1",

    "loopnz",
    "loopz",
    "loop",
    "jrcxz",

    // started shipping in Tremont, 2020 sept 23
    "movdir64b",
    "movdiri",

    // started shipping in Tiger Lake, 2020 sept 2
    "aesdec128kl",
    "aesdec256kl",
    "aesdecwide128kl",
    "aesdecwide256kl",
    "aesenc128kl",
    "aesenc256kl",
    "aesencwide128kl",
    "aesencwide256kl",
    "encodekey128",
    "encodekey256",
    "loadiwkey",

    // unsure
    "hreset",

    // 3dnow
    "femms",
    "pi2fw",
    "pi2fd",
    "pf2iw",
    "pf2id",
    "pmulhrw",
    "pfcmpge",
    "pfmin",
    "pfrcp",
    "pfrsqrt",
    "pfsub",
    "pfadd",
    "pfcmpgt",
    "pfmax",
    "pfrcpit1",
    "pfrsqit1",
    "pfsubr",
    "pfacc",
    "pfcmpeq",
    "pfmul",
    "pfmulhrw",
    "pfrcpit2",
    "pfnacc",
    "pfpnacc",
    "pswapd",
    "pavgusb",

    // ENQCMD
    "enqcmd",
    "enqcmds",

    // INVPCID
    "invept",
    "invvpid",
    "invpcid",

    // PTWRITE
    "ptwrite",

    // GFNI
    "gf2p8affineqb",
    "gf2p8affineinvqb",
    "gf2p8mulb",

    // CET
    "wruss",
    "wrss",
    "incssp",
    "saveprevssp",
    "setssbsy",
    "clrssbsy",
    "rstorssp",
    "endbr64",
    "endbr32",

    // TDX
    "tdcall",
    "seamret",
    "seamops",
    "seamcall",

    // WAITPKG
    "tpause",
    "umonitor",
    "umwait",

    // UINTR
    "uiret",
    "testui",
    "clui",
    "stui",
    "senduipi",

    // TSXLDTRK
    "xsusldtrk",
    "xresldtrk",

    // AVX512F
    "valignd",
    "valignq",
    "vblendmpd",
    "vblendmps",
    "vcompresspd",
    "vcompressps",
    "vcvtpd2udq",
    "vcvttpd2udq",
    "vcvtps2udq",
    "vcvttps2udq",
    "vcvtqq2pd",
    "vcvtqq2ps",
    "vcvtsd2usi",
    "vcvttsd2usi",
    "vcvtss2usi",
    "vcvttss2usi",
    "vcvtudq2pd",
    "vcvtudq2ps",
    "vcvtusi2usd",
    "vcvtusi2uss",
    "vexpandpd",
    "vexpandps",
    "vextractf32x4",
    "vextractf64x4",
    "vextracti32x4",
    "vextracti64x4",
    "vfixupimmpd",
    "vfixupimmps",
    "vfixupimmsd",
    "vfixupimmss",
    "vgetexppd",
    "vgetexpps",
    "vgetexpsd",
    "vgetexpss",
    "vgetmantpd",
    "vgetmantps",
    "vgetmantsd",
    "vgetmantss",
    "vinsertf32x4",
    "vinsertf64x4",
    "vinserti64x4",
    "vmovdqa32",
    "vmovdqa64",
    "vmovdqu32",
    "vmovdqu64",
    "vpblendmd",
    "vpblendmq",
    "vpcmpd",
    "vpcmpud",
    "vpcmpq",
    "vpcmpuq",
    "vpcompressq",
    "vpcompressd",
    "vpermi2d",
    "vpermi2q",
    "vpermi2pd",
    "vpermi2ps",
    "vpermt2d",
    "vpermt2q",
    "vpermt2pd",
    "vpermt2ps",
    "vpmaxsq",
    "vpmaxuq",
    "vpminsq",
    "vpminuq",
    "vpmovsqb",
    "vpmovusqb",
    "vpmovsqw",
    "vpmovusqw",
    "vpmovsqd",
    "vpmovusqd",
    "vpmovsdb",
    "vpmovusdb",
    "vpmovsdw",
    "vpmovusdw",
    "vprold",
    "vprolq",
    "vprolvd",
    "vprolvq",
    "vprord",
    "vprorq",
    "vprorrd",
    "vprorrq",
    "vpscatterdd",
    "vpscatterdq",
    "vpscatterqd",
    "vpscatterqq",
    "vpsraq",
    "vpsravq",
    "vptestnmd",
    "vptestnmq",
    "vpternlogd",
    "vpternlogq",
    "vptestmd",
    "vptestmq",
    "vrcp14pd",
    "vrcp14ps",
    "vrcp14sd",
    "vrcp14ss",
    "vrndscalepd",
    "vrndscaleps",
    "vrndscalesd",
    "vrndscaless",
    "vrsqrt14pd",
    "vrsqrt14ps",
    "vrsqrt14sd",
    "vrsqrt14ss",
    "vscaledpd",
    "vscaledps",
    "vscaledsd",
    "vscaledss",
    "vscatterdd",
    "vscatterdq",
    "vscatterqd",
    "vscatterqq",
    "vshuff32x4",
    "vshuff64x2",
    "vshufi32x4",
    "vshufi64x2",

    // AVX512DQ
    "vcvttpd2qq",
    "vcvtpd2qq",
    "vcvttpd2uqq",
    "vcvtpd2uqq",
    "vcvttps2qq",
    "vcvtps2qq",
    "vcvttps2uqq",
    "vcvtps2uqq",
    "vcvtuqq2pd",
    "vcvtuqq2ps",
    "vextractf64x2",
    "vextracti64x2",
    "vfpclasspd",
    "vfpclassps",
    "vfpclasssd",
    "vfpclassss",
    "vinsertf64x2",
    "vinserti64x2",
    "vpmovm2d",
    "vpmovm2q",
    "vpmovb2d",
    "vpmovq2m",
    "vrangepd",
    "vrangeps",
    "vrangesd",
    "vrangess",
    "vreducepd",
    "vreduceps",
    "vreducesd",
    "vreducess",

    // AVX512BW
    "vdbpsadbw",
    "vmovdqu8",
    "vmovdqu16",
    "vpblendmb",
    "vpblendmw",
    "vpcmpb",
    "vpcmpub",
    "vpcmpw",
    "vpcmpuw",
    "vpermw",
    "vpermi2b",
    "vpermi2w",
    "vpmovm2b",
    "vpmovm2w",
    "vpmovb2m",
    "vpmovw2m",
    "vpmovswb",
    "vpmovuswb",
    "vpsllvw",
    "vpsravw",
    "vpsrlvw",
    "vptestnmb",
    "vptestnmw",
    "vptestmb",
    "vptestmw",

    // AVX512CD
    "vpbroadcastm",
    "vpconflictd",
    "vpconflictq",
    "vplzcntd",
    "vplzcntq",

    "kunpckbw",
    "kunpckwd",
    "kunpckdq",

    "kaddb",
    "kandb",
    "kandnb",
    "kmovb",
    "knotb",
    "korb",
    "kortestb",
    "kshiftlb",
    "kshiftrb",
    "ktestb",
    "kxnorb",
    "kxorb",
    "kaddw",
    "kandw",
    "kandnw",
    "kmovw",
    "knotw",
    "korw",
    "kortestw",
    "kshiftlw",
    "kshiftrw",
    "ktestw",
    "kxnorw",
    "kxorw",
    "kaddd",
    "kandd",
    "kandnd",
    "kmovd",
    "knotd",
    "kord",
    "kortestd",
    "kshiftld",
    "kshiftrd",
    "ktestd",
    "kxnord",
    "kxord",
    "kaddq",
    "kandq",
    "kandnq",
    "kmovq",
    "knotq",
    "korq",
    "kortestq",
    "kshiftlq",
    "kshiftrq",
    "ktestq",
    "kxnorq",
    "kxorq",

    // AVX512ER
    "vexp2pd",
    "vexp2ps",
    "vexp2sd",
    "vexp2ss",
    "vrcp28pd",
    "vrcp28ps",
    "vrcp28sd",
    "vrcp28ss",
    "vrsqrt28pd",
    "vrsqrt28ps",
    "vrsqrt28sd",
    "vrsqrt28ss",

    // AVX512PF
    "vgatherpf0dpd",
    "vgatherpf0dps",
    "vgatherpf0qpd",
    "vgatherpf0qps",
    "vgatherpf1dpd",
    "vgatherpf1dps",
    "vgatherpf1qpd",
    "vgatherpf1qps",
    "vscatterpf0dpd",
    "vscatterpf0dps",
    "vscatterpf0qpd",
    "vscatterpf0qps",
    "vscatterpf1dpd",
    "vscatterpf1dps",
    "vscatterpf1qpd",
    "vscatterpf1qps",

    // MPX
    "bndmk",
    "bndcl",
    "bndcu",
    "bndcn",
    "bndmov",
    "bndldx",
    "bndstx",

    "vgf2p8affineqb",
    "vgf2p8affineinvqb",
    "vpshrdq",
    "vpshrdd",
    "vpshrdw",
    "vpshldq",
    "vpshldd",
    "vpshldw",
    "vbroadcastf32x8",
    "vbroadcastf64x4",
    "vbroadcastf32x4",
    "vbroadcastf64x2",
    "vbroadcastf32x2",
    "vbroadcasti32x8",
    "vbroadcasti64x4",
    "vbroadcasti32x4",
    "vbroadcasti64x2",
    "vbroadcasti32x2",
    "vextracti32x8",
    "vextractf32x8",
    "vinserti32x8",
    "vinsertf32x8",
    "vinserti32x4",
    "v4fnmaddss",
    "v4fnmaddps",
    "vcvtneps2bf16",
    "v4fmaddss",
    "v4fmaddps",
    "vcvtne2ps2bf16",
    "vp2intersectd",
    "vp2intersectq",
    "vp4dpwssds",
    "vp4dpwssd",
    "vpdpwssds",
    "vpdpwssd",
    "vpdpbusds",
    "vdpbf16ps",
    "vpbroadcastmw2d",
    "vpbroadcastmb2q",
    "vpmovd2m",
    "vpmovqd",
    "vpmovwb",
    "vpmovdb",
    "vpmovdw",
    "vpmovqb",
    "vpmovqw",
    "vgf2p8mulb",
    "vpmadd52huq",
    "vpmadd52luq",
    "vpshufbitqmb",
    "vpermb",
    "vpexpandd",
    "vpexpandq",
    "vpabsq",
    "vprorvd",
    "vprorvq",
    "vpmultishiftqb",
    "vpermt2b",
    "vpermt2w",
    "vpshrdvq",
    "vpshrdvd",
    "vpshrdvw",
    "vpshldvq",
    "vpshldvd",
    "vpshldvw",
    "vpcompressb",
    "vpcompressw",
    "vpexpandb",
    "vpexpandw",
    "vpopcntd",
    "vpopcntq",
    "vpopcntb",
    "vpopcntw",
    "vscalefss",
    "vscalefsd",
    "vscalefps",
    "vscalefpd",
    "vpdpbusd",
    "vcvtusi2sd",
    "vcvtusi2ss",
    "vpxord",
    "vpxorq",
    "vpord",
    "vporq",
    "vpandnd",
    "vpandnq",
    "vpandd",
    "vpandq",

    "psmash",
    "pvalidate",
    "rmpadjust",
    "rmpupdate",

    "jecxz",
];

impl Opcode {
    fn name(&self) -> &'static str {
        unsafe {
            MNEMONICS.get_kinda_unchecked((*self as usize) & 0xfff)
        }
    }
}

// allowing these deprecated items for the time being, not yet breaking yaxpeax-x86 apis
#[allow(deprecated)]
impl <T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Opcode {
    fn colorize(&self, _colors: &Y, out: &mut T) -> fmt::Result {
        out.write_str(self.name())
        // a way to preserve opcode colorization might be to see entering an opcode span,
        // collecting text into a buffer, waiting for the span to exit, looking that up in a map
        // for opcode types, and then picking a color. this really should be something like "opcode
        // information" that can be looked up (including things like operand read/write behavior)..
        //
        // leaving this commented out as a reminder of what opcode to behavior mapping was like
        /*
        match self {
            Opcode::VGF2P8AFFINEQB |
            Opcode::VGF2P8AFFINEINVQB |
            Opcode::VPSHRDQ |
            Opcode::VPSHRDD |
            Opcode::VPSHRDW |
            Opcode::VPSHLDQ |
            Opcode::VPSHLDD |
            Opcode::VPSHLDW |
            Opcode::VBROADCASTF32X8 |
            Opcode::VBROADCASTF64X4 |
            Opcode::VBROADCASTF32X4 |
            Opcode::VBROADCASTF64X2 |
            Opcode::VBROADCASTF32X2 |
            Opcode::VBROADCASTI32X8 |
            Opcode::VBROADCASTI64X4 |
            Opcode::VBROADCASTI32X4 |
            Opcode::VBROADCASTI64X2 |
            Opcode::VBROADCASTI32X2 |
            Opcode::VEXTRACTI32X8 |
            Opcode::VEXTRACTF32X8 |
            Opcode::VINSERTI32X8 |
            Opcode::VINSERTF32X8 |
            Opcode::VINSERTI32X4 |
            Opcode::V4FNMADDSS |
            Opcode::V4FNMADDPS |
            Opcode::VCVTNEPS2BF16 |
            Opcode::V4FMADDSS |
            Opcode::V4FMADDPS |
            Opcode::VCVTNE2PS2BF16 |
            Opcode::VP2INTERSECTD |
            Opcode::VP2INTERSECTQ |
            Opcode::VP4DPWSSDS |
            Opcode::VP4DPWSSD |
            Opcode::VPDPWSSDS |
            Opcode::VPDPWSSD |
            Opcode::VPDPBUSDS |
            Opcode::VDPBF16PS |
            Opcode::VPBROADCASTMW2D |
            Opcode::VPBROADCASTMB2Q |
            Opcode::VPMOVD2M |
            Opcode::VPMOVQD |
            Opcode::VPMOVWB |
            Opcode::VPMOVDB |
            Opcode::VPMOVDW |
            Opcode::VPMOVQB |
            Opcode::VPMOVQW |
            Opcode::VGF2P8MULB |
            Opcode::VPMADD52HUQ |
            Opcode::VPMADD52LUQ |
            Opcode::VPSHUFBITQMB |
            Opcode::VPERMB |
            Opcode::VPEXPANDD |
            Opcode::VPEXPANDQ |
            Opcode::VPABSQ |
            Opcode::VPRORVD |
            Opcode::VPRORVQ |
            Opcode::VPMULTISHIFTQB |
            Opcode::VPERMT2B |
            Opcode::VPERMT2W |
            Opcode::VPSHRDVQ |
            Opcode::VPSHRDVD |
            Opcode::VPSHRDVW |
            Opcode::VPSHLDVQ |
            Opcode::VPSHLDVD |
            Opcode::VPSHLDVW |
            Opcode::VPCOMPRESSB |
            Opcode::VPCOMPRESSW |
            Opcode::VPEXPANDB |
            Opcode::VPEXPANDW |
            Opcode::VPOPCNTD |
            Opcode::VPOPCNTQ |
            Opcode::VPOPCNTB |
            Opcode::VPOPCNTW |
            Opcode::VSCALEFSS |
            Opcode::VSCALEFSD |
            Opcode::VSCALEFPS |
            Opcode::VSCALEFPD |
            Opcode::VPDPBUSD |
            Opcode::VCVTUSI2SD |
            Opcode::VCVTUSI2SS |
            Opcode::VPXORD |
            Opcode::VPXORQ |
            Opcode::VPORD |
            Opcode::VPORQ |
            Opcode::VPANDND |
            Opcode::VPANDNQ |
            Opcode::VPANDD |
            Opcode::VPANDQ |

            Opcode::VHADDPS |
            Opcode::VHSUBPS |
            Opcode::VADDSUBPS |
            Opcode::VADDPD |
            Opcode::VADDPS |
            Opcode::VADDSD |
            Opcode::VADDSS |
            Opcode::VADDSUBPD |
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
            Opcode::VDIVPD |
            Opcode::VDIVPS |
            Opcode::VDIVSD |
            Opcode::VDIVSS |
            Opcode::VHADDPD |
            Opcode::VHSUBPD |
            Opcode::HADDPD |
            Opcode::HSUBPD |
            Opcode::VMULPD |
            Opcode::VMULPS |
            Opcode::VMULSD |
            Opcode::VMULSS |
            Opcode::VPABSB |
            Opcode::VPABSD |
            Opcode::VPABSW |
            Opcode::PABSB |
            Opcode::PABSD |
            Opcode::PABSW |
            Opcode::VPSIGNB |
            Opcode::VPSIGND |
            Opcode::VPSIGNW |
            Opcode::PSIGNB |
            Opcode::PSIGND |
            Opcode::PSIGNW |
            Opcode::VPADDB |
            Opcode::VPADDD |
            Opcode::VPADDQ |
            Opcode::VPADDSB |
            Opcode::VPADDSW |
            Opcode::VPADDUSB |
            Opcode::VPADDUSW |
            Opcode::VPADDW |
            Opcode::VPAVGB |
            Opcode::VPAVGW |
            Opcode::VPMULDQ |
            Opcode::VPMULHRSW |
            Opcode::VPMULHUW |
            Opcode::VPMULHW |
            Opcode::VPMULLQ |
            Opcode::VPMULLD |
            Opcode::VPMULLW |
            Opcode::VPMULUDQ |
            Opcode::PCLMULQDQ |
            Opcode::PMULDQ |
            Opcode::PMULHRSW |
            Opcode::PMULLD |
            Opcode::VPSUBB |
            Opcode::VPSUBD |
            Opcode::VPSUBQ |
            Opcode::VPSUBSB |
            Opcode::VPSUBSW |
            Opcode::VPSUBUSB |
            Opcode::VPSUBUSW |
            Opcode::VPSUBW |
            Opcode::VROUNDPD |
            Opcode::VROUNDPS |
            Opcode::VEXP2PD |
            Opcode::VEXP2PS |
            Opcode::VEXP2SD |
            Opcode::VEXP2SS |
            Opcode::VRCP28PD |
            Opcode::VRCP28PS |
            Opcode::VRCP28SD |
            Opcode::VRCP28SS |
            Opcode::VRCP14PD |
            Opcode::VRCP14PS |
            Opcode::VRCP14SD |
            Opcode::VRCP14SS |
            Opcode::VRNDSCALEPD |
            Opcode::VRNDSCALEPS |
            Opcode::VRNDSCALESD |
            Opcode::VRNDSCALESS |
            Opcode::VRSQRT14PD |
            Opcode::VRSQRT14PS |
            Opcode::VRSQRT14SD |
            Opcode::VRSQRT14SS |
            Opcode::VSCALEDPD |
            Opcode::VSCALEDPS |
            Opcode::VSCALEDSD |
            Opcode::VSCALEDSS |
            Opcode::VRSQRT28PD |
            Opcode::VRSQRT28PS |
            Opcode::VRSQRT28SD |
            Opcode::VRSQRT28SS |
            Opcode::VRSQRTPS |
            Opcode::VSQRTPD |
            Opcode::VSQRTPS |
            Opcode::VSUBPD |
            Opcode::VSUBPS |
            Opcode::VSUBSD |
            Opcode::VSUBSS |
            Opcode::VRCPSS |
            Opcode::VROUNDSD |
            Opcode::VROUNDSS |
            Opcode::ROUNDPD |
            Opcode::ROUNDPS |
            Opcode::ROUNDSD |
            Opcode::ROUNDSS |
            Opcode::VRSQRTSS |
            Opcode::VSQRTSD |
            Opcode::VSQRTSS |
            Opcode::VPSADBW |
            Opcode::VMPSADBW |
            Opcode::VDBPSADBW |
            Opcode::VPHADDD |
            Opcode::VPHADDSW |
            Opcode::VPHADDW |
            Opcode::VPHSUBD |
            Opcode::VPHSUBSW |
            Opcode::VPHSUBW |
            Opcode::VPMADDUBSW |
            Opcode::VPMADDWD |
            Opcode::VDPPD |
            Opcode::VDPPS |
            Opcode::VRCPPS |
            Opcode::VORPD |
            Opcode::VORPS |
            Opcode::VANDPD |
            Opcode::VANDPS |
            Opcode::VANDNPD |
            Opcode::VANDNPS |
            Opcode::VPAND |
            Opcode::VPANDN |
            Opcode::VPOR |
            Opcode::VPXOR |
            Opcode::VXORPD |
            Opcode::VXORPS |
            Opcode::VPSLLD |
            Opcode::VPSLLDQ |
            Opcode::VPSLLQ |
            Opcode::VPSLLVD |
            Opcode::VPSLLVQ |
            Opcode::VPSLLW |
            Opcode::VPROLD |
            Opcode::VPROLQ |
            Opcode::VPROLVD |
            Opcode::VPROLVQ |
            Opcode::VPRORD |
            Opcode::VPRORQ |
            Opcode::VPRORRD |
            Opcode::VPRORRQ |
            Opcode::VPSLLVW |
            Opcode::VPSRAQ |
            Opcode::VPSRAVQ |
            Opcode::VPSRAVW |
            Opcode::VPSRLVW |
            Opcode::VPSRAD |
            Opcode::VPSRAVD |
            Opcode::VPSRAW |
            Opcode::VPSRLD |
            Opcode::VPSRLDQ |
            Opcode::VPSRLQ |
            Opcode::VPSRLVD |
            Opcode::VPSRLVQ |
            Opcode::VPSRLW |
            Opcode::PHADDD |
            Opcode::PHADDSW |
            Opcode::PHADDW |
            Opcode::PHSUBD |
            Opcode::PHSUBSW |
            Opcode::PHSUBW |
            Opcode::PMADDUBSW |
            Opcode::ADDSUBPD |
            Opcode::DPPS |
            Opcode::DPPD |
            Opcode::MPSADBW |
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
            Opcode::PMULHRW |
            Opcode::PFRCP |
            Opcode::PFRSQRT |
            Opcode::PFSUB |
            Opcode::PFADD |
            Opcode::PFRCPIT1 |
            Opcode::PFRSQIT1 |
            Opcode::PFSUBR |
            Opcode::PFACC |
            Opcode::PFMUL |
            Opcode::PFMULHRW |
            Opcode::PFRCPIT2 |
            Opcode::PFNACC |
            Opcode::PFPNACC |
            Opcode::PSWAPD |
            Opcode::PAVGUSB |
            Opcode::XADD|
            Opcode::DIV |
            Opcode::IDIV |
            Opcode::MUL |
            Opcode::MULX |
            Opcode::NEG |
            Opcode::NOT |
            Opcode::SAR |
            Opcode::SAL |
            Opcode::SHR |
            Opcode::SARX |
            Opcode::SHLX |
            Opcode::SHRX |
            Opcode::SHRD |
            Opcode::SHL |
            Opcode::RCR |
            Opcode::RCL |
            Opcode::ROR |
            Opcode::RORX |
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
            Opcode::ADCX |
            Opcode::ADOX |
            Opcode::SUB |
            Opcode::POPCNT |
            Opcode::LZCNT |
            Opcode::VPLZCNTD |
            Opcode::VPLZCNTQ |
            Opcode::BT |
            Opcode::BTS |
            Opcode::BTR |
            Opcode::BTC |
            Opcode::BSF |
            Opcode::BSR |
            Opcode::BZHI |
            Opcode::PDEP |
            Opcode::PEXT |
            Opcode::TZCNT |
            Opcode::ANDN |
            Opcode::BEXTR |
            Opcode::BLSI |
            Opcode::BLSMSK |
            Opcode::BLSR |
            Opcode::ADDPS |
            Opcode::ADDPD |
            Opcode::ANDNPS |
            Opcode::ANDNPD |
            Opcode::ANDPS |
            Opcode::ANDPD |
            Opcode::COMISD |
            Opcode::COMISS |
            Opcode::DIVPS |
            Opcode::DIVPD |
            Opcode::MULPS |
            Opcode::MULPD |
            Opcode::ORPS |
            Opcode::ORPD |
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
            Opcode::PSHUFD |
            Opcode::PSHUFW |
            Opcode::PSHUFB |
            Opcode::PSLLD |
            Opcode::PSLLDQ |
            Opcode::PSLLQ |
            Opcode::PSLLW |
            Opcode::PSRAD |
            Opcode::PSRAW |
            Opcode::PSRLD |
            Opcode::PSRLDQ |
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
            Opcode::SQRTPD |
            Opcode::SUBPS |
            Opcode::SUBPD |
            Opcode::XORPS |
            Opcode::XORPD |
            Opcode::RCPPS |
            Opcode::SHLD |
            Opcode::SLHD |
            Opcode::UCOMISD |
            Opcode::UCOMISS |
            Opcode::F2XM1 |
            Opcode::FABS |
            Opcode::FADD |
            Opcode::FADDP |
            Opcode::FCHS |
            Opcode::FCOS |
            Opcode::FDIV |
            Opcode::FDIVP |
            Opcode::FDIVR |
            Opcode::FDIVRP |
            Opcode::FIADD |
            Opcode::FIDIV |
            Opcode::FIDIVR |
            Opcode::FIMUL |
            Opcode::FISUB |
            Opcode::FISUBR |
            Opcode::FMUL |
            Opcode::FMULP |
            Opcode::FNCLEX |
            Opcode::FNINIT |
            Opcode::FPATAN |
            Opcode::FPREM |
            Opcode::FPREM1 |
            Opcode::FPTAN |
            Opcode::FRNDINT |
            Opcode::FSCALE |
            Opcode::FSIN |
            Opcode::FSINCOS |
            Opcode::FSQRT |
            Opcode::FSUB |
            Opcode::FSUBP |
            Opcode::FSUBR |
            Opcode::FSUBRP |
            Opcode::FXTRACT |
            Opcode::FYL2X |
            Opcode::FYL2XP1 |
            Opcode::KADDB |
            Opcode::KANDB |
            Opcode::KANDNB |
            Opcode::KNOTB |
            Opcode::KORB |
            Opcode::KSHIFTLB |
            Opcode::KSHIFTRB |
            Opcode::KXNORB |
            Opcode::KXORB |
            Opcode::KADDW |
            Opcode::KANDW |
            Opcode::KANDNW |
            Opcode::KNOTW |
            Opcode::KORW |
            Opcode::KSHIFTLW |
            Opcode::KSHIFTRW |
            Opcode::KXNORW |
            Opcode::KXORW |
            Opcode::KADDD |
            Opcode::KANDD |
            Opcode::KANDND |
            Opcode::KNOTD |
            Opcode::KORD |
            Opcode::KSHIFTLD |
            Opcode::KSHIFTRD |
            Opcode::KXNORD |
            Opcode::KXORD |
            Opcode::KADDQ |
            Opcode::KANDQ |
            Opcode::KANDNQ |
            Opcode::KNOTQ |
            Opcode::KORQ |
            Opcode::KSHIFTLQ |
            Opcode::KSHIFTRQ |
            Opcode::KXNORQ |
            Opcode::KXORQ |
            Opcode::IMUL => { write!(out, "{}", colors.arithmetic_op(self)) }
            Opcode::POPF |
            Opcode::PUSHF |
            Opcode::ENTER |
            Opcode::LEAVE |
            Opcode::PUSH |
            Opcode::POP => { write!(out, "{}", colors.stack_op(self)) }
            Opcode::WAIT |
            Opcode::FNOP |
            Opcode::FDISI8087_NOP |
            Opcode::FENI8087_NOP |
            Opcode::FSETPM287_NOP |
            Opcode::PREFETCHNTA |
            Opcode::PREFETCH0 |
            Opcode::PREFETCH1 |
            Opcode::PREFETCH2 |
            Opcode::PREFETCHW |
            Opcode::NOP => { write!(out, "{}", colors.nop_op(self)) }

            /* Control flow */
            Opcode::HLT |
            Opcode::INT |
            Opcode::INTO |
            Opcode::IRET |
            Opcode::IRETD |
            Opcode::IRETQ |
            Opcode::RETF |
            Opcode::RETURN => { write!(out, "{}", colors.stop_op(self)) }
            Opcode::LOOPNZ |
            Opcode::LOOPZ |
            Opcode::LOOP |
            Opcode::JRCXZ |
            Opcode::JECXZ |
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
            Opcode::PI2FW |
            Opcode::PI2FD |
            Opcode::PF2ID |
            Opcode::PF2IW |
            Opcode::VCVTDQ2PD |
            Opcode::VCVTDQ2PS |
            Opcode::VCVTPD2DQ |
            Opcode::VCVTPD2PS |
            Opcode::VCVTPH2PS |
            Opcode::VCVTPS2DQ |
            Opcode::VCVTPS2PD |
            Opcode::VCVTPS2PH |
            Opcode::VCVTTPD2DQ |
            Opcode::VCVTTPS2DQ |
            Opcode::VCVTSD2SI |
            Opcode::VCVTSD2SS |
            Opcode::VCVTSI2SD |
            Opcode::VCVTSI2SS |
            Opcode::VCVTSS2SD |
            Opcode::VCVTSS2SI |
            Opcode::VCVTTSD2SI |
            Opcode::VCVTTSS2SI |
            Opcode::VCVTPD2UDQ |
            Opcode::VCVTTPD2UDQ |
            Opcode::VCVTPS2UDQ |
            Opcode::VCVTTPS2UDQ |
            Opcode::VCVTQQ2PD |
            Opcode::VCVTQQ2PS |
            Opcode::VCVTSD2USI |
            Opcode::VCVTTSD2USI |
            Opcode::VCVTSS2USI |
            Opcode::VCVTTSS2USI |
            Opcode::VCVTUDQ2PD |
            Opcode::VCVTUDQ2PS |
            Opcode::VCVTUSI2USD |
            Opcode::VCVTUSI2USS |
            Opcode::VCVTTPD2QQ |
            Opcode::VCVTPD2QQ |
            Opcode::VCVTTPD2UQQ |
            Opcode::VCVTPD2UQQ |
            Opcode::VCVTTPS2QQ |
            Opcode::VCVTPS2QQ |
            Opcode::VCVTTPS2UQQ |
            Opcode::VCVTPS2UQQ |
            Opcode::VCVTUQQ2PD |
            Opcode::VCVTUQQ2PS |
            Opcode::VMOVDDUP |
            Opcode::VPSHUFLW |
            Opcode::VPSHUFHW |
            Opcode::VBLENDMPD |
            Opcode::VBLENDMPS |
            Opcode::VPBLENDMD |
            Opcode::VPBLENDMQ |
            Opcode::VBLENDPD |
            Opcode::VBLENDPS |
            Opcode::VBLENDVPD |
            Opcode::VBLENDVPS |
            Opcode::VPBLENDMB |
            Opcode::VPBLENDMW |
            Opcode::PBLENDVB |
            Opcode::PBLENDW |
            Opcode::BLENDPD |
            Opcode::BLENDPS |
            Opcode::BLENDVPD |
            Opcode::BLENDVPS |
            Opcode::BLENDW |
            Opcode::VBROADCASTF128 |
            Opcode::VBROADCASTI128 |
            Opcode::VBROADCASTSD |
            Opcode::VBROADCASTSS |
            Opcode::VPBROADCASTM |
            Opcode::VEXTRACTF128 |
            Opcode::VEXTRACTI128 |
            Opcode::VEXTRACTPS |
            Opcode::EXTRACTPS |
            Opcode::VGATHERDPD |
            Opcode::VGATHERDPS |
            Opcode::VGATHERQPD |
            Opcode::VGATHERQPS |
            Opcode::VGATHERPF0DPD |
            Opcode::VGATHERPF0DPS |
            Opcode::VGATHERPF0QPD |
            Opcode::VGATHERPF0QPS |
            Opcode::VGATHERPF1DPD |
            Opcode::VGATHERPF1DPS |
            Opcode::VGATHERPF1QPD |
            Opcode::VGATHERPF1QPS |
            Opcode::VSCATTERDD |
            Opcode::VSCATTERDQ |
            Opcode::VSCATTERQD |
            Opcode::VSCATTERQQ |
            Opcode::VPSCATTERDD |
            Opcode::VPSCATTERDQ |
            Opcode::VPSCATTERQD |
            Opcode::VPSCATTERQQ |
            Opcode::VSCATTERPF0DPD |
            Opcode::VSCATTERPF0DPS |
            Opcode::VSCATTERPF0QPD |
            Opcode::VSCATTERPF0QPS |
            Opcode::VSCATTERPF1DPD |
            Opcode::VSCATTERPF1DPS |
            Opcode::VSCATTERPF1QPD |
            Opcode::VSCATTERPF1QPS |
            Opcode::VINSERTF128 |
            Opcode::VINSERTI128 |
            Opcode::VINSERTPS |
            Opcode::INSERTPS |
            Opcode::VEXTRACTF32X4 |
            Opcode::VEXTRACTF64X2 |
            Opcode::VEXTRACTF64X4 |
            Opcode::VEXTRACTI32X4 |
            Opcode::VEXTRACTI64X2 |
            Opcode::VEXTRACTI64X4 |
            Opcode::VINSERTF32X4 |
            Opcode::VINSERTF64X2 |
            Opcode::VINSERTF64X4 |
            Opcode::VINSERTI64X2 |
            Opcode::VINSERTI64X4 |
            Opcode::VSHUFF32X4 |
            Opcode::VSHUFF64X2 |
            Opcode::VSHUFI32X4 |
            Opcode::VSHUFI64X2 |
            Opcode::VMASKMOVDQU |
            Opcode::VMASKMOVPD |
            Opcode::VMASKMOVPS |
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
            Opcode::MOVDIR64B |
            Opcode::MOVDIRI |
            Opcode::MOVNTDQA |
            Opcode::VMOVQ |
            Opcode::VMOVSHDUP |
            Opcode::VMOVSLDUP |
            Opcode::VMOVUPD |
            Opcode::VMOVUPS |
            Opcode::VMOVSD |
            Opcode::VMOVSS |
            Opcode::VMOVDQA32 |
            Opcode::VMOVDQA64 |
            Opcode::VMOVDQU32 |
            Opcode::VMOVDQU64 |
            Opcode::VPMOVM2B |
            Opcode::VPMOVM2W |
            Opcode::VPMOVB2M |
            Opcode::VPMOVW2M |
            Opcode::VPMOVSWB |
            Opcode::VPMOVUSWB |
            Opcode::VPMOVSQB |
            Opcode::VPMOVUSQB |
            Opcode::VPMOVSQW |
            Opcode::VPMOVUSQW |
            Opcode::VPMOVSQD |
            Opcode::VPMOVUSQD |
            Opcode::VPMOVSDB |
            Opcode::VPMOVUSDB |
            Opcode::VPMOVSDW |
            Opcode::VPMOVUSDW |
            Opcode::VPMOVM2D |
            Opcode::VPMOVM2Q |
            Opcode::VPMOVB2D |
            Opcode::VPMOVQ2M |
            Opcode::VMOVDQU8 |
            Opcode::VMOVDQU16 |

            Opcode::VPBLENDD |
            Opcode::VPBLENDVB |
            Opcode::VPBLENDW |
            Opcode::VPBROADCASTB |
            Opcode::VPBROADCASTD |
            Opcode::VPBROADCASTQ |
            Opcode::VPBROADCASTW |
            Opcode::VPGATHERDD |
            Opcode::VPGATHERDQ |
            Opcode::VPGATHERQD |
            Opcode::VPGATHERQQ |
            Opcode::VPCLMULQDQ |
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
            Opcode::PMOVSXBD |
            Opcode::PMOVSXBQ |
            Opcode::PMOVSXBW |
            Opcode::PMOVSXDQ |
            Opcode::PMOVSXWD |
            Opcode::PMOVSXWQ |
            Opcode::PMOVZXBD |
            Opcode::PMOVZXBQ |
            Opcode::PMOVZXBW |
            Opcode::PMOVZXDQ |
            Opcode::PMOVZXWD |
            Opcode::PMOVZXWQ |
            Opcode::KUNPCKBW |
            Opcode::KUNPCKWD |
            Opcode::KUNPCKDQ |
            Opcode::VUNPCKHPD |
            Opcode::VUNPCKHPS |
            Opcode::VUNPCKLPD |
            Opcode::VUNPCKLPS |
            Opcode::VPUNPCKHBW |
            Opcode::VPUNPCKHDQ |
            Opcode::VPUNPCKHQDQ |
            Opcode::VPUNPCKHWD |
            Opcode::VPUNPCKLBW |
            Opcode::VPUNPCKLDQ |
            Opcode::VPUNPCKLQDQ |
            Opcode::VPUNPCKLWD |
            Opcode::VSHUFPD |
            Opcode::VSHUFPS |
            Opcode::VPACKSSDW |
            Opcode::VPACKUSDW |
            Opcode::PACKUSDW |
            Opcode::VPACKSSWB |
            Opcode::VPACKUSWB |
            Opcode::VALIGND |
            Opcode::VALIGNQ |
            Opcode::VPALIGNR |
            Opcode::PALIGNR |
            Opcode::VPERM2F128 |
            Opcode::VPERM2I128 |
            Opcode::VPERMD |
            Opcode::VPERMILPD |
            Opcode::VPERMILPS |
            Opcode::VPERMPD |
            Opcode::VPERMPS |
            Opcode::VPERMQ |
            Opcode::VPERMI2D |
            Opcode::VPERMI2Q |
            Opcode::VPERMI2PD |
            Opcode::VPERMI2PS |
            Opcode::VPERMT2D |
            Opcode::VPERMT2Q |
            Opcode::VPERMT2PD |
            Opcode::VPERMT2PS |
            Opcode::VPERMI2B |
            Opcode::VPERMI2W |
            Opcode::VPERMW |
            Opcode::VPEXTRB |
            Opcode::VPEXTRD |
            Opcode::VPEXTRQ |
            Opcode::VPEXTRW |
            Opcode::PEXTRB |
            Opcode::PEXTRD |
            Opcode::PEXTRQ |
            Opcode::EXTRQ |
            Opcode::PINSRB |
            Opcode::PINSRD |
            Opcode::PINSRQ |
            Opcode::INSERTQ |
            Opcode::VPINSRB |
            Opcode::VPINSRD |
            Opcode::VPINSRQ |
            Opcode::VPINSRW |
            Opcode::VPMASKMOVD |
            Opcode::VPMASKMOVQ |
            Opcode::VCOMPRESSPD |
            Opcode::VCOMPRESSPS |
            Opcode::VPCOMPRESSQ |
            Opcode::VPCOMPRESSD |
            Opcode::VEXPANDPD |
            Opcode::VEXPANDPS |
            Opcode::VPSHUFB |
            Opcode::VPSHUFD |
            Opcode::VPHMINPOSUW |
            Opcode::PHMINPOSUW |
            Opcode::VZEROUPPER |
            Opcode::VZEROALL |
            Opcode::VFIXUPIMMPD |
            Opcode::VFIXUPIMMPS |
            Opcode::VFIXUPIMMSD |
            Opcode::VFIXUPIMMSS |
            Opcode::VREDUCEPD |
            Opcode::VREDUCEPS |
            Opcode::VREDUCESD |
            Opcode::VREDUCESS |
            Opcode::VGETEXPPD |
            Opcode::VGETEXPPS |
            Opcode::VGETEXPSD |
            Opcode::VGETEXPSS |
            Opcode::VGETMANTPD |
            Opcode::VGETMANTPS |
            Opcode::VGETMANTSD |
            Opcode::VGETMANTSS |
            Opcode::VLDDQU |
            Opcode::BSWAP |
            Opcode::CVTDQ2PD |
            Opcode::CVTDQ2PS |
            Opcode::CVTPS2DQ |
            Opcode::CVTPD2DQ |
            Opcode::CVTPI2PS |
            Opcode::CVTPI2PD |
            Opcode::CVTPS2PD |
            Opcode::CVTPD2PS |
            Opcode::CVTPS2PI |
            Opcode::CVTPD2PI |
            Opcode::CVTSD2SI |
            Opcode::CVTSD2SS |
            Opcode::CVTSI2SD |
            Opcode::CVTSI2SS |
            Opcode::CVTSS2SD |
            Opcode::CVTSS2SI |
            Opcode::CVTTPD2DQ |
            Opcode::CVTTPS2DQ |
            Opcode::CVTTPS2PI |
            Opcode::CVTTPD2PI |
            Opcode::CVTTSD2SI |
            Opcode::CVTTSS2SI |
            Opcode::MASKMOVQ |
            Opcode::MASKMOVDQU |
            Opcode::MOVAPS |
            Opcode::MOVAPD |
            Opcode::MOVD |
            Opcode::MOVHPS |
            Opcode::MOVHPD |
            Opcode::MOVHLPS |
            Opcode::MOVLPS |
            Opcode::MOVLPD |
            Opcode::MOVLHPS |
            Opcode::MOVMSKPS |
            Opcode::MOVMSKPD |
            Opcode::MOVNTI |
            Opcode::MOVNTPS |
            Opcode::MOVNTPD |
            Opcode::MOVNTSS |
            Opcode::MOVNTSD |
            Opcode::MOVNTQ |
            Opcode::MOVNTDQ |
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
            Opcode::PUNPCKLQDQ |
            Opcode::PUNPCKHQDQ |
            Opcode::PACKSSDW |
            Opcode::PACKSSWB |
            Opcode::PACKUSWB |
            Opcode::UNPCKHPS |
            Opcode::UNPCKHPD |
            Opcode::UNPCKLPS |
            Opcode::UNPCKLPD |
            Opcode::SHUFPD |
            Opcode::SHUFPS |
            Opcode::PMOVMSKB |
            Opcode::KMOVB |
            Opcode::KMOVW |
            Opcode::KMOVD |
            Opcode::KMOVQ |
            Opcode::BNDMOV |
            Opcode::LDDQU |
            Opcode::CMC |
            Opcode::CLC |
            Opcode::CLI |
            Opcode::CLD |
            Opcode::STC |
            Opcode::STI |
            Opcode::STD |
            Opcode::CBW |
            Opcode::CWDE |
            Opcode::CDQE |
            Opcode::CWD |
            Opcode::CDQ |
            Opcode::CQO |
            Opcode::MOVDDUP |
            Opcode::MOVSLDUP |
            Opcode::MOVDQ2Q |
            Opcode::MOVDQU |
            Opcode::MOVDQA |
            Opcode::MOVQ |
            Opcode::MOVQ2DQ |
            Opcode::MOVSHDUP |
            Opcode::MOVUPS |
            Opcode::PEXTRW |
            Opcode::PINSRW |
            Opcode::MOV |
            Opcode::MOVBE |
            Opcode::LODS |
            Opcode::STOS |
            Opcode::LAHF |
            Opcode::SAHF |
            Opcode::MOVS |
            Opcode::INS |
            Opcode::IN |
            Opcode::OUTS |
            Opcode::OUT |
            Opcode::MOVZX |
            Opcode::MOVSX |
            Opcode::MOVSXD |
            Opcode::FILD |
            Opcode::FBLD |
            Opcode::FBSTP |
            Opcode::FIST |
            Opcode::FISTP |
            Opcode::FISTTP |
            Opcode::FLD |
            Opcode::FLD1 |
            Opcode::FLDCW |
            Opcode::FLDENV |
            Opcode::FLDL2E |
            Opcode::FLDL2T |
            Opcode::FLDLG2 |
            Opcode::FLDLN2 |
            Opcode::FLDPI |
            Opcode::FLDZ |
            Opcode::FST |
            Opcode::FSTP |
            Opcode::FSTPNCE |
            Opcode::FNSAVE |
            Opcode::FNSTCW |
            Opcode::FNSTENV |
            Opcode::FNSTOR |
            Opcode::FNSTSW |
            Opcode::FRSTOR |
            Opcode::FXCH |
            Opcode::XCHG |
            Opcode::XLAT |
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
            Opcode::FCMOVB |
            Opcode::FCMOVBE |
            Opcode::FCMOVE |
            Opcode::FCMOVNB |
            Opcode::FCMOVNBE |
            Opcode::FCMOVNE |
            Opcode::FCMOVNU |
            Opcode::FCMOVU |
            Opcode::SALC |
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

            Opcode::VCOMISD |
            Opcode::VCOMISS |
            Opcode::VUCOMISD |
            Opcode::VUCOMISS |
            Opcode::KORTESTB |
            Opcode::KTESTB |
            Opcode::KORTESTW |
            Opcode::KTESTW |
            Opcode::KORTESTD |
            Opcode::KTESTD |
            Opcode::KORTESTQ |
            Opcode::KTESTQ |
            Opcode::VPTESTNMD |
            Opcode::VPTESTNMQ |
            Opcode::VPTERNLOGD |
            Opcode::VPTERNLOGQ |
            Opcode::VPTESTMD |
            Opcode::VPTESTMQ |
            Opcode::VPTESTNMB |
            Opcode::VPTESTNMW |
            Opcode::VPTESTMB |
            Opcode::VPTESTMW |
            Opcode::VPCMPD |
            Opcode::VPCMPUD |
            Opcode::VPCMPQ |
            Opcode::VPCMPUQ |
            Opcode::VPCMPB |
            Opcode::VPCMPUB |
            Opcode::VPCMPW |
            Opcode::VPCMPUW |
            Opcode::VCMPPD |
            Opcode::VCMPPS |
            Opcode::VCMPSD |
            Opcode::VCMPSS |
            Opcode::VMAXPD |
            Opcode::VMAXPS |
            Opcode::VMAXSD |
            Opcode::VMAXSS |
            Opcode::VPMAXSQ |
            Opcode::VPMAXUQ |
            Opcode::VPMINSQ |
            Opcode::VPMINUQ |
            Opcode::VMINPD |
            Opcode::VMINPS |
            Opcode::VMINSD |
            Opcode::VMINSS |
            Opcode::VPCMPEQB |
            Opcode::VPCMPEQD |
            Opcode::VPCMPEQQ |
            Opcode::VPCMPEQW |
            Opcode::VPCMPGTB |
            Opcode::VPCMPGTD |
            Opcode::VPCMPGTQ |
            Opcode::VPCMPGTW |
            Opcode::VPCMPESTRI |
            Opcode::VPCMPESTRM |
            Opcode::VPCMPISTRI |
            Opcode::VPCMPISTRM |
            Opcode::VPMAXSB |
            Opcode::VPMAXSD |
            Opcode::VPMAXSW |
            Opcode::VPMAXUB |
            Opcode::VPMAXUW |
            Opcode::VPMAXUD |
            Opcode::VPMINSB |
            Opcode::VPMINSW |
            Opcode::VPMINSD |
            Opcode::VPMINUB |
            Opcode::VPMINUW |
            Opcode::VPMINUD |
            Opcode::VFPCLASSPD |
            Opcode::VFPCLASSPS |
            Opcode::VFPCLASSSD |
            Opcode::VFPCLASSSS |
            Opcode::VRANGEPD |
            Opcode::VRANGEPS |
            Opcode::VRANGESD |
            Opcode::VRANGESS |
            Opcode::VPCONFLICTD |
            Opcode::VPCONFLICTQ |
            Opcode::VPTEST |
            Opcode::VTESTPD |
            Opcode::VTESTPS |
            Opcode::PCMPEQB |
            Opcode::PCMPEQD |
            Opcode::PCMPEQQ |
            Opcode::PCMPEQW |
            Opcode::PCMPESTRI |
            Opcode::PCMPESTRM |
            Opcode::PCMPGTB |
            Opcode::PCMPGTD |
            Opcode::PCMPGTQ |
            Opcode::PCMPGTW |
            Opcode::PCMPISTRI |
            Opcode::PCMPISTRM |
            Opcode::PTEST |
            Opcode::MAXPD |
            Opcode::MAXPS |
            Opcode::MAXSD |
            Opcode::MAXSS |
            Opcode::MINPD |
            Opcode::MINPS |
            Opcode::MINSD |
            Opcode::MINSS |
            Opcode::PMAXSB |
            Opcode::PMAXSD |
            Opcode::PMAXSW |
            Opcode::PMAXUB |
            Opcode::PMAXUD |
            Opcode::PMAXUW |
            Opcode::PMINSB |
            Opcode::PMINSD |
            Opcode::PMINSW |
            Opcode::PMINUB |
            Opcode::PMINUD |
            Opcode::PMINUW |
            Opcode::PFCMPGE |
            Opcode::PFMIN |
            Opcode::PFCMPGT |
            Opcode::PFMAX |
            Opcode::PFCMPEQ |
            Opcode::CMPS |
            Opcode::SCAS |
            Opcode::TEST |
            Opcode::FTST |
            Opcode::FXAM |
            Opcode::FUCOM |
            Opcode::FUCOMI |
            Opcode::FUCOMIP |
            Opcode::FUCOMP |
            Opcode::FUCOMPP |
            Opcode::FCOM |
            Opcode::FCOMI |
            Opcode::FCOMIP |
            Opcode::FCOMP |
            Opcode::FCOMPP |
            Opcode::FICOM |
            Opcode::FICOMP |
            Opcode::CMPSD |
            Opcode::CMPSS |
            Opcode::CMP |
            Opcode::CMPPS |
            Opcode::CMPPD |
            Opcode::CMPXCHG8B |
            Opcode::CMPXCHG16B |
            Opcode::CMPXCHG => { write!(out, "{}", colors.comparison_op(self)) }

            Opcode::WRMSR |
            Opcode::RDMSR |
            Opcode::RDTSC |
            Opcode::RDPMC |
            Opcode::RDPID |
            Opcode::RDFSBASE |
            Opcode::RDGSBASE |
            Opcode::WRFSBASE |
            Opcode::WRGSBASE |
            Opcode::FXSAVE |
            Opcode::FXRSTOR |
            Opcode::LDMXCSR |
            Opcode::STMXCSR |
            Opcode::VLDMXCSR |
            Opcode::VSTMXCSR |
            Opcode::XSAVE |
            Opcode::XSAVEC |
            Opcode::XSAVES |
            Opcode::XSAVEC64 |
            Opcode::XSAVES64 |
            Opcode::XRSTOR |
            Opcode::XRSTORS |
            Opcode::XRSTORS64 |
            Opcode::XSAVEOPT |
            Opcode::LFENCE |
            Opcode::MFENCE |
            Opcode::SFENCE |
            Opcode::CLFLUSH |
            Opcode::CLFLUSHOPT |
            Opcode::CLWB |
            Opcode::SGDT |
            Opcode::SIDT |
            Opcode::LGDT |
            Opcode::LIDT |
            Opcode::SMSW |
            Opcode::LMSW |
            Opcode::SWAPGS |
            Opcode::RDTSCP |
            Opcode::INVEPT |
            Opcode::INVVPID |
            Opcode::INVPCID |
            Opcode::INVLPG |
            Opcode::INVLPGA |
            Opcode::INVLPGB |
            Opcode::TLBSYNC |
            Opcode::PSMASH |
            Opcode::PVALIDATE |
            Opcode::RMPADJUST |
            Opcode::RMPUPDATE |
            Opcode::CPUID |
            Opcode::WBINVD |
            Opcode::INVD |
            Opcode::SYSRET |
            Opcode::CLTS |
            Opcode::SYSCALL |
            Opcode::TDCALL |
            Opcode::SEAMRET |
            Opcode::SEAMOPS |
            Opcode::SEAMCALL |
            Opcode::TPAUSE |
            Opcode::UMONITOR |
            Opcode::UMWAIT |
            Opcode::LSL |
            Opcode::SLDT |
            Opcode::STR |
            Opcode::LLDT |
            Opcode::LTR |
            Opcode::VERR |
            Opcode::VERW |
            Opcode::JMPE |
            Opcode::EMMS |
            Opcode::FEMMS |
            Opcode::GETSEC |
            Opcode::LFS |
            Opcode::LGS |
            Opcode::LSS |
            Opcode::RSM |
            Opcode::SYSENTER |
            Opcode::SYSEXIT |
            Opcode::VMREAD |
            Opcode::VMWRITE |
            Opcode::VMCLEAR |
            Opcode::VMPTRLD |
            Opcode::VMPTRST |
            Opcode::VMXON |
            Opcode::VMCALL |
            Opcode::VMLAUNCH |
            Opcode::VMRESUME |
            Opcode::VMLOAD |
            Opcode::VMMCALL |
            Opcode::VMSAVE |
            Opcode::VMRUN |
            Opcode::VMXOFF |
            Opcode::PCONFIG |
            Opcode::MONITOR |
            Opcode::MWAIT |
            Opcode::MONITORX |
            Opcode::MWAITX |
            Opcode::SKINIT |
            Opcode::CLGI |
            Opcode::STGI |
            Opcode::CLAC |
            Opcode::STAC |
            Opcode::ENCLS |
            Opcode::ENCLV |
            Opcode::XGETBV |
            Opcode::XSETBV |
            Opcode::VMFUNC |
            Opcode::XEND |
            Opcode::XTEST |
            Opcode::XABORT |
            Opcode::XBEGIN |
            Opcode::ENCLU |
            Opcode::RDPKRU |
            Opcode::WRPKRU |
            Opcode::RDPRU |
            Opcode::CLZERO |
            Opcode::ENQCMD |
            Opcode::ENQCMDS |
            Opcode::PTWRITE |
            Opcode::UIRET |
            Opcode::TESTUI |
            Opcode::CLUI |
            Opcode::STUI |
            Opcode::SENDUIPI |
            Opcode::XSUSLDTRK |
            Opcode::XRESLDTRK |
            Opcode::BNDMK |
            Opcode::BNDCL |
            Opcode::BNDCU |
            Opcode::BNDCN |
            Opcode::BNDLDX |
            Opcode::BNDSTX |
            Opcode::LAR => { write!(out, "{}", colors.platform_op(self)) }

            Opcode::CRC32 |
            Opcode::RDSEED |
            Opcode::RDRAND |
            Opcode::SHA1RNDS4 |
            Opcode::SHA1NEXTE |
            Opcode::SHA1MSG1 |
            Opcode::SHA1MSG2 |
            Opcode::SHA256RNDS2 |
            Opcode::SHA256MSG1 |
            Opcode::SHA256MSG2 |
            Opcode::FFREE |
            Opcode::FFREEP |
            Opcode::FDECSTP |
            Opcode::FINCSTP |
            Opcode::GF2P8MULB |
            Opcode::GF2P8AFFINEQB |
            Opcode::GF2P8AFFINEINVQB |
            Opcode::AESDEC128KL |
            Opcode::AESDEC256KL |
            Opcode::AESDECWIDE128KL |
            Opcode::AESDECWIDE256KL |
            Opcode::AESENC128KL |
            Opcode::AESENC256KL |
            Opcode::AESENCWIDE128KL |
            Opcode::AESENCWIDE256KL |
            Opcode::ENCODEKEY128 |
            Opcode::ENCODEKEY256 |
            Opcode::LOADIWKEY |
            Opcode::HRESET |
            Opcode::WRUSS |
            Opcode::WRSS |
            Opcode::INCSSP |
            Opcode::SAVEPREVSSP |
            Opcode::SETSSBSY |
            Opcode::CLRSSBSY |
            Opcode::RSTORSSP |
            Opcode::ENDBR64 |
            Opcode::ENDBR32 |
            Opcode::AESDEC |
            Opcode::AESDECLAST |
            Opcode::AESENC |
            Opcode::AESENCLAST |
            Opcode::AESIMC |
            Opcode::AESKEYGENASSIST |
            Opcode::VAESDEC |
            Opcode::VAESDECLAST |
            Opcode::VAESENC |
            Opcode::VAESENCLAST |
            Opcode::VAESIMC |
            Opcode::VAESKEYGENASSIST => { write!(out, "{}", colors.misc_op(self)) }

            Opcode::UD0 |
            Opcode::UD1 |
            Opcode::UD2 |
            Opcode::Invalid => { write!(out, "{}", colors.invalid_op(self)) }
        }
        */
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let rules = DefaultRules::for_style(DisplayStyle::Intel);
        let bundle = rules.display(self);
        fmt::Display::fmt(&bundle, fmt)
    }
}

impl<'instr> fmt::Display for InstructionDisplayer<'instr> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        // to reuse one implementation, call the deprecated function for now.
        #[allow(deprecated)]
        self.colorize(&NoColors, fmt)
    }
}

/// enum controlling how `Instruction::display_with` renders instructions. `Intel` is more or less
/// intel syntax, though memory operand sizes are elided if they can be inferred from other
/// operands.
///
/// ## lossiness
///
/// for some display styles, `yaxpeax-x86` tries to ensure instructions are formatted in a way that
/// is accepted by some corresponding assemblers. even so, such round-tripping is inherently lossy;
/// some instructions may have many equally-valid encodings, so re-assembling an instruction does
/// not guarantee an assembler would produce the same bytes as were initially decoded.
///
/// a non-exhaustive list of ways that going through `bytes -> decode -> assemble -> bytes` can be lossy:
/// * opcode choice (for example, `0x31` vs `0x33` encodings of register-register `xor`) may not be controllable,
/// * immediates and displacements may have multiple valid encodings,
/// * non-effectual prefixes are generally not printed by `yaxpeax-x86`,
/// * repeated prefixes (imagine "`rep rep rep movsb`") are not printed and generally not accepted by assemblers,
#[non_exhaustive]
#[derive(Copy, Clone, Debug)]
pub enum DisplayStyle {
    /// intel-style syntax for instructions, like
    /// `add rax, [rdx + rcx * 2 + 0x1234]`
    Intel,
    /// C-style syntax for instructions, like
    /// `rax += [rdx + rcx * 2 + 0x1234]`
    C,
    /// format instructions in the syntax used by the Microsoft Macro Assembler (MASM), like
    /// `add rax, dword ptr [rdx + rcx * 2 + 1234h]`
    ///
    /// some instructions are decoded by `dumpbin.exe` and `yaxpeax-x86` but cannot be assembled by
    /// `masm.exe` or `ml64.exe`. as one example, `ud0`.
    Masm,
    // one might imagine an ATT style here, which is mostly interesting for reversing operand
    // order.
    // well.
    // it also complicates memory operands in an offset-only operand, and is just kind of awful, so
    // it's just not implemented yet.
    // ATT,
}

/// implementation of [`Display`](fmt::Display) that renders instructions using a specified display
/// style.
///
/// this is less flexible than the full gamut of `DisplayRules`, in favor of using `yaxpeax-x86`'s
/// default address-insensitive instruction formatting.
pub struct InstructionDisplayer<'instr> {
    pub(crate) instr: &'instr Instruction,
    pub(crate) style: DisplayStyle,
}

pub struct InstructionRuleBundle<'instr, 'rules, Rules> {
    pub(crate) instr: &'instr Instruction,
    pub(crate) rules: &'rules Rules,
}

impl<'instr, 'rules, Rules> InstructionRuleBundle<'instr, 'rules, Rules> {
    fn new(instr: &'instr Instruction, rules: &'rules Rules) -> Self {
        Self { instr, rules }
    }
}

impl<'instr, 'fmt, Rules> fmt::Display for
    InstructionRuleBundle<'instr, 'fmt, Rules> where
      Rules: for<'f, 'g> DisplayRules<yaxpeax_arch::display::FmtSink<'f, fmt::Formatter<'g>>>
{
    fn fmt<'a, 'b, 'c>(&'a self, fmt: &'b mut fmt::Formatter<'c>) -> fmt::Result {
        let mut sink = yaxpeax_arch::display::FmtSink::new(fmt);
        let style = self.rules.display_style();
        match style {
            DisplayStyle::Intel => format_intel(&self.instr, self.rules, &mut sink),
            DisplayStyle::C => {
                format_c(&self.instr, self.rules, &mut sink)
            }
            DisplayStyle::Masm => {
                masm::contextualize(&self.instr, self.rules, &mut sink)
            }
        }
    }
}

struct DefaultRules {
    style: DisplayStyle
}

impl DefaultRules {
    fn for_style(style: DisplayStyle) -> Self {
        Self { style }
    }

    fn display<'me, 'instr>(&'me self, instr: &'instr Instruction) -> InstructionRuleBundle<'instr, 'me, Self> {
        InstructionRuleBundle {
            instr,
            rules: self,
        }
    }
}

impl<S: DisplaySink> DisplayRules<S> for DefaultRules {
    fn display_style(&self) -> DisplayStyle {
        self.style
    }
}

/// ```rust
/// // `AbsoluteAddressFormatter` prints instructions as a contiguous sequence starting from the
/// // provided address.
/// let mut addr_formatter = AbsoluteAddressFormatter::new(0x10);
///
/// let decoder = long_mdoe::InstDecoder::default();
///
/// let instr = decoder.decode_slice(&[0x33, 0x05, 0x08, 0x00, 0x00, 0x00])
///     .expect("can decode 'xor eax, dword [rip + 0x10]'");
///
/// // instructions are printed with `rip` taken to be the address in the formatter, which
/// // overrides rip-relative display.
/// let formatted = format!("{}", addr_formatter.display(instr));
/// assert_eq!(formatted, "xor eax, dword [0x18]");
/// // the program address in `addr_formatter` must be advanced to the next instruction.
/// addr_formatter.advance(instr);
/// let formatted = format!("{}", addr_formatter.display(instr));
/// assert_eq!(formatted, "xor eax, dword [0x1e]");
///
/// let branch = decoder.decode_slice(&[0xeb, 0x70])
///     .expect("can decode 'jmp rip+0x70');
///
/// // jump destinations are also made absolute.
/// let formatted = format!("{}", addr_formatter.display(instr));
/// assert_eq!(formatted, "jmp 0x7e");
/// ```
pub struct AbsoluteAddressFormatter {
    rip: u64,
    style: DisplayStyle,
}

impl AbsoluteAddressFormatter {
    pub fn new(rip: u64) -> Self {
        AbsoluteAddressFormatter {
            rip,
            style: DisplayStyle::Intel,
        }
    }

    pub fn with_style(&mut self, style: DisplayStyle) -> &mut Self {
        self.style = style;
        self
    }

    pub fn display<'me, 'instr>(&'me self, instr: &'instr Instruction) -> InstructionRuleBundle<'instr, 'me, Self> {
        InstructionRuleBundle {
            instr,
            rules: self,
        }
    }

    pub fn advance(&mut self, instr: &Instruction) {
        use yaxpeax_arch::{AddressBase, LengthedInstruction};
        self.rip = self.rip.wrapping_offset(instr.len());
    }
}

impl<S: DisplaySink> DisplayRules<S> for AbsoluteAddressFormatter {
    fn display_style(&self) -> DisplayStyle {
        self.style
    }

    unsafe fn max_size() -> Option<usize> {
        // Safety: this is somewhat subtle.
        //
        // `MAX_INSTRUCTION_LEN` is sized for the longest possible instruction lengths. the longest
        // strings that `emit_address` may write are u64-sized: "0xffffffffffffffff". the previous
        // maximum length of replaced fields ranges from `$ + 0x12341234` to `rip + 0x12341234`.
        // addresses are mildly longer now.
        //
        // instructions that include a rip-sensitive value have only one: a branch offset or a
        // rip-relative memory reference. this means the actual addition to an instruction's
        // maximum printed size is `8 (u32->u64 digits) - 4 ("$ + ")` four more bytes.
        // `MAX_INSTRUCTION_LEN` has around 180 bytes of excess reservation, so this is within that
        // margin of error.
        Some(crate::MAX_INSTRUCTION_LEN)
    }

    fn instr_addr(&self) -> Option<u64> {
        Some(self.rip)
    }

    fn emit_address(&self, addr: u64, s: &mut S) -> Result<bool, fmt::Error> {
        s.span_start_immediate();
        s.write_fixed_size("0x")?;
        s.write_u64(addr)?;
        s.span_end_immediate();
        Ok(true)
    }
}

/// a set of functions controlling how instructions are formatted in an [`InstructionDisplayer`].
///
/// generally, functions on this trait return either `Option`, where `None` indicates "data not
/// available, caller should use some kind of default fallback behavior",
/// or `Result<bool, fmt::Error>` where the bool indicates if the implementation has done the
/// formatting work that would otherwise be left up to the caller.
///
/// functions on this trait default to returning `None` and `Ok(false)`, meaning a minimal
/// `impl DisplayRules for ... { }` will not override any display logic.
///
/// ## interaction with `DisplayStyle`
///
/// `DisplayStyle` controls the overall style used for instruction printing, through to how
/// registers and numbers are printed. as `DisplayRules` functions override default `yaxpeax-x86`
/// printing behavior, implementations must consider if they want to support variants of
/// `DisplayStyle as well, or only format with an assumed `DisplayStyle`.
///
/// the `DisplayStyle` used when formatting an instruction and invoking `DisplayRules` functions is
/// controlled by `DisplayRules::display_style`. it is an implementation's decision to customize
/// this (as an example: [`AbsoluteAddressFormatter::with_style()`]) or to simply return one style
/// that is always used for instruction formatting.
///
/// `DisplayStyle` variants in some cases also control the display (or non-display) of implicit
/// operands. for example, the `Intel` style writes x87 instructions with implicit operands, but
/// `Masm` omits some (such as `fld` or `fst`, where `masm.exe` will treat `st(0)` as a syntax
/// error). correspondingly, `DisplayRules::emit_operand` will be invoked only for the
/// normally-printed non-`st(0)` operand of `fld` or `fst`. in a handful of cases the differences
/// across display styles involve invoking more specific `DisplayRules` rules, particularly when
/// there may not be an explicit operand to format and a register or immediate is shown in its
/// stead; an example of this is formatting of the implicitly-read registers of `monitor` or
/// `mwait` under `DisplayStyle::Masm`.
///
/// some other `DisplayStyle` variance includes size suffixes on opcodes, though there is no
/// comprehensive list of per-style formatting variance.
pub trait DisplayRules<S: DisplaySink> {
    /// provide a `DisplayStyle` used when formatting instructions through this impl of
    /// `DisplayRules`.
    ///
    /// if this impl of `DisplayRules` overrides some aspect of instruction formatting, it is that
    /// override's responsibility to follow the `DisplayStyle` reported here. or ignore it, if you
    /// so choose!
    fn display_style(&self) -> DisplayStyle;

    /// report the maximum size of an instruction formatted by this `DisplayRules` implementation,
    /// across all x86_64 instructions.
    ///
    /// `max_size()` is used to control the use of bounds checks when formatting into an
    /// [`InstructionTextBuffer`]. this is fundamentally a hint, and may be ignored by future
    /// versions of `yaxpeax-x86`.
    ///
    /// # implementation guidance
    ///
    /// user code is extraordinarily unlikely to be able to safely return `Some` from this
    /// function.
    ///
    /// implementations that return `Some` must never produce more than that many *bytes* of
    /// output, including any output produced by `yaxpeax-x86`, in formatting an instruction. take
    /// care to consider multi-byte UTF-8 codepoints, as `InstructionTextBuffer` writes into Rust
    /// `alloc::string::String`, which uses that representation character data.
    ///
    /// implementations that use arbitrary strings for symbols, for example, are extremly unlikely
    /// to be able to uphold a fixed-max-size hint.
    ///
    /// note that `yaxpeax-x86` may write additional data in future `DisplayStyle` variants. new
    /// instructions may have longer mnemonics. new extensions may define longer register names, or
    /// memory label sizes. new encodings may have additional (longer!) decorators or otherwise
    /// raise the maximum size of a formatted instruction. any of these may occur across a patch or
    /// minor version.
    unsafe fn max_size() -> Option<usize> { None }

    /// the address of the instruction to be printed.
    ///
    /// if this returns `None`, the default behavior is to show the relevant details of an
    /// instruction in an address-insensitive way. see [`DisplayRules::emit_branch_addr`] and
    /// [`DisplayRules::emit_relative_address`] for more.
    fn instr_addr(&self) -> Option<u64> {
        None
    }

    /// override all aspects of formatting the instruction.
    fn emit_instruction(&self, instr: &Instruction, s: &mut S) -> Result<bool, fmt::Error> {
        let _ = instr;
        let _ = s;
        Ok(false)
    }

    /// override the display of the `op_idx`'th explicit operand in `instr`.
    ///
    /// if a memory access, the overridden region includes the operand's memory size label and
    /// segment override. if the operand has descriptors (like AVX512 rounding or broadcast modes),
    /// they are overridden here as well.
    fn emit_operand(&self, instr: &Instruction, op_idx: u8, s: &mut S) -> Result<bool, fmt::Error> {
        let _ = instr;
        let _ = op_idx;
        let _ = s;
        Ok(false)
    }

    fn emit_register(&self, reg: RegSpec, s: &mut S) -> Result<bool, fmt::Error> {
        let _ = reg;
        let _ = s;
        Ok(false)
    }

    /// override the display of an immediate in an instruction.
    ///
    /// this is the `0x50` in `push 0x50`, as well as `0x60` in `ret 0x60`, and `0x500098` in
    /// `mov rax, 0x500098`. this function is a unified formatter for all size of immediate; the
    /// size of the encoded value is not reported.
    fn emit_signed_immediate(&self, imm: i64, s: &mut S) -> Result<bool, fmt::Error> {
        let _ = imm;
        let _ = s;
        Ok(false)
    }

    /// override the display of an unsigned immediate in an instruction.
    ///
    /// this is typically an integer involved in SIMD instruction configuration, or immediates in
    /// instructions that operate on single-byte registers. that is, this is the `0xa4` in
    /// `cmp al, 0xa4`. this also reports u16 immediates as in `return 0x1234` or `retf 0x1234`.
    fn emit_unsigned_immediate(&self, imm: u64, s: &mut S) -> Result<bool, fmt::Error> {
        let _ = imm;
        let _ = s;
        Ok(false)
    }

    /// override the display of a conditional or unconditional relative branch target.
    ///
    /// this is the `$+0x60` in `jmp $+0x60`. the default behavior is to check
    /// [`DisplayRules::instr_addr()`] for the address of this instruction, convert the relative
    /// branch to an absolute destination, and call [`DisplayRules::emit_address()`] with that
    /// address.
    ///
    /// note that custom implementations of `emit_branch_addr` may want to retain the above
    /// behavior.
    fn emit_branch_addr(&self, instr: &Instruction, rel: i32, s: &mut S) -> Result<bool, fmt::Error> {
        let Some(ip) = self.instr_addr() else {
            return Ok(false);
        };

        use yaxpeax_arch::{AddressBase, LengthedInstruction};

        let next = ip.wrapping_offset(instr.len());
        let dest = next.wrapping_add(rel as i64 as u64);
        self.emit_address(dest, s)
    }

    /// override the display of a rip-relative address.
    ///
    /// this is the `rip + 0x1234` in `xor rax, [rip + 0x1234]`.
    fn emit_relative_address(&self, rel: i32, s: &mut S) -> Result<bool, fmt::Error> {
        let Some(ip) = self.instr_addr() else {
            return Ok(false);
        };

        let dest = ip.wrapping_add(rel as i64 as u64);
        self.emit_address(dest, s)
    }

    /// override the display of a literal absolute address.
    ///
    /// this is the `0x10` in `mov rax, gs:[0x10]`, as well as
    /// the `0x12345678` in `add rax, [0x12345678]`. the default behavior is to simply call
    /// [`emit_address`] with the absolute value being dereferenced.
    fn emit_absolute_address(&self, abs: u64, s: &mut S) -> Result<bool, fmt::Error> {
        self.emit_address(abs, s)
    }

    /// write some address out to the provided `DisplaySink`.
    ///
    /// notably, this does *not* include values like relocated immeidates in expressions like
    /// `mov rax, fn_ptr`; immediates are always printed through [`DisplayRules::exit_immediate`].
    fn emit_address(&self, addr: u64, s: &mut S) -> Result<bool, fmt::Error> {
        let _ = addr;
        let _ = s;
        Ok(false)
    }

    // 32-bit, 16-bit-only..
    /*
    fn emit_far_address(&mut self, seg: u16, offs: u32, s: &mut S) -> Result<bool, fmt::Error> {
        Ok(false)
    }
    */
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
 *
 * UPDATE: really wish i thought of DisplaySink back then, really wish this was bounded as T:
 * DisplaySink.
 */
// allowing these deprecated items for the time being, not yet breaking yaxpeax-x86 apis
#[allow(deprecated)]
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
    /// format this instruction into `out` as a plain text string, in the default display
    /// configuration for an `x86_64` instruction (that is, roughly Intel syntax).
    ///
    /// for more customizable formatting options, see [`Instruction::display_with`].
    #[cfg_attr(feature="profiling", inline(never))]
    pub fn write_to<T: fmt::Write>(&self, out: &mut T) -> fmt::Result {
        let mut out = yaxpeax_arch::display::FmtSink::new(out);
        let rules = DefaultRules::for_style(DisplayStyle::Intel);
        format_intel(self, &rules, &mut out)
    }

    /// format this instruction into `out`, which may perform additional styling based on its
    /// `DisplaySink` implementation.
    #[cfg_attr(feature="profiling", inline(never))]
    pub fn display_into<T: DisplaySink>(&self, out: &mut T) -> fmt::Result {
        let rules = DefaultRules::for_style(DisplayStyle::Intel);
        format_intel(self, &rules, out)
    }

    /*
    /// format this instruction into `out`, using the provided `rules` to potentially override the
    /// default formatting for a given `DisplayStyle`.
    #[cfg_attr(feature="profiling", inline(never))]
    pub fn format_into<T: DisplaySink, Rules: DisplayRules<T>>(&self, 
    */
}

#[cfg_attr(feature="profiling", inline(never))]
pub(crate) fn format_intel<T: DisplaySink, R: DisplayRules<T>>(instr: &Instruction, rules: &R, out: &mut T) -> fmt::Result {
    if rules.emit_instruction(instr, out)? {
        return Ok(());
    }

    if instr.xacquire() {
        out.write_fixed_size("xacquire ")?;
    }
    if instr.xrelease() {
        out.write_fixed_size("xrelease ")?;
    }
    if instr.prefixes.lock() {
        out.write_fixed_size("lock ")?;
    }

    if instr.prefixes.rep_any() {
        if instr.opcode.can_rep() {
            if instr.prefixes.rep() {
                out.write_fixed_size("rep ")?;
            } else if instr.prefixes.repnz() {
                out.write_fixed_size("repnz ")?;
            }
        }
    }

    out.write_opcode(instr.opcode)?;

    if instr.operand_count > 0 {
        out.write_fixed_size(" ")?;

        if instr.operands[0] == OperandSpec::ImmI8 || instr.operands[0] == OperandSpec::ImmI32 {
            if RELATIVE_BRANCHES.contains(&instr.opcode) {
                // relative branch instructions have only one operand, so print this one and we're
                // done. relative branch instructions *also* have a ... relative branch ... as
                // their only operand, so don't `emit_operand()` which would confuse these for a
                // "normal" immediate.
                if rules.emit_operand(instr, 0, out)? {
                    return Ok(());
                }
            }
            if instr.visit_operand(0, &mut RelativeBranchPrinter {
                inst: instr,
                rules,
                out,
            })? {
                return Ok(());
            }
        }

        if !rules.emit_operand(instr, 0, out)? {
            if instr.operands[0 as usize].is_memory() {
                out.write_mem_size_label(instr.mem_size)?;
                if let Some(prefix) = instr.segment_override_for_op(0) {
                    let name = prefix.name();
                    out.write_char(' ')?;
                    out.write_char(name[0] as char)?;
                    out.write_char(name[1] as char)?;
                    out.write_fixed_size(":")?;
                } else {
                    out.write_fixed_size(" ")?;
                }
            }

            let mut displayer = DisplayingOperandVisitor {
                f: out,
                rules,
            };
            instr.visit_operand(0 as u8, &mut displayer)?;
        }

        for i in 1..instr.operand_count {
            // don't worry about checking for `instr.operands[i] != Nothing`, it would be a bug to
            // reach that while iterating only to `operand_count`..
            out.write_fixed_size(", ")?;
            // hint that accessing `inster.operands[i]` can't panic: this is useful for
            // `instr.operands` and the segment selector check after.
            if i >= 4 {
                // Safety: Instruction::operands is a four-element array; operand_count is always
                // low enough that 0..operand_count is a valid index.
                unsafe { unreachable_unchecked(); }
            }

            if rules.emit_operand(instr, i, out)? {
                // if the rule printed an operand out, continue on to the next one!
                continue;
            }

            if instr.operands[i as usize].is_memory() {
                out.write_mem_size_label(instr.mem_size)?;
                if let Some(prefix) = instr.segment_override_for_op(i) {
                    let name = prefix.name();
                    out.write_char(' ')?;
                    out.write_char(name[0] as char)?;
                    out.write_char(name[1] as char)?;
                    out.write_fixed_size(":")?;
                } else {
                    out.write_fixed_size(" ")?;
                }
            }

            let mut displayer = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            instr.visit_operand(i as u8, &mut displayer)?;
            if let Some(evex) = instr.prefixes.evex() {
                if evex.broadcast() && instr.operands[i as usize].is_memory() {
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
                    out.write_fixed_size("{1to")?;
                    static STRING_LUT: &'static [&'static str] = &[
                        "0", "1", "2", "3", "4", "5", "6", "7", "8",
                        "9", "10", "11", "12", "13", "14", "15", "16",
                    ];
                    unsafe {
                        out.write_lt_16(STRING_LUT.get_kinda_unchecked(scale as usize))?;
                    }
                    out.write_char('}')?;
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn format_c<T: DisplaySink, R: DisplayRules<T>>(instr: &Instruction, _rules: &R, out: &mut T) -> fmt::Result {
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
        if instr.opcode.can_rep() {
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

    fn write_jmp_operand<T: fmt::Write>(op: Operand, out: &mut T) -> fmt::Result {
        let mut out = yaxpeax_arch::display::FmtSink::new(out);
        use core::fmt::Write;
        match op {
            Operand::ImmediateI8 { imm: rel } => {
                let rel = if rel >= 0 {
                    out.write_str("$+")?;
                    rel as u8
                } else {
                    out.write_str("$-")?;
                    rel.unsigned_abs()
                };
                out.write_prefixed_u8(rel)
            }
            Operand::ImmediateI32 { imm: rel } => {
                let rel = if rel >= 0 {
                    out.write_str("$+")?;
                    rel as u32
                } else {
                    out.write_str("$-")?;
                    rel.unsigned_abs()
                };
                out.write_prefixed_u32(rel)
            }
            other => {
                write!(out, "{}", other)
            }
        }
    }

    match instr.opcode {
        Opcode::Invalid => { out.write_str("invalid")?; },
        Opcode::MOVS => {
            out.write_str("es:[rdi++] = ds:[rsi++]")?;
        },
        Opcode::CMPS => {
            out.write_str("rflags = flags(ds:[rsi++] - es:[rdi++])")?;
        },
        Opcode::LODS => {
            // TODO: size
            out.write_str("rax = ds:[rsi++]")?;
        },
        Opcode::STOS => {
            // TODO: size
            out.write_str("es:[rdi++] = rax")?;
        },
        Opcode::INS => {
            // TODO: size
            out.write_str("es:[rdi++] = port(dx)")?;
        },
        Opcode::OUTS => {
            // TODO: size
            out.write_str("port(dx) = ds:[rsi++]")?;
        }
        Opcode::ADD => {
            write!(out, "{} += {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::OR => {
            write!(out, "{} |= {}", instr.operand(0), instr.operand(1))?;
        }
        Opcode::ADC => {
            write!(out, "{} += {} + rflags.cf", instr.operand(0), instr.operand(1))?;
        }
        Opcode::ADCX => {
            write!(out, "{} += {} + rflags.cf", instr.operand(0), instr.operand(1))?;
        }
        Opcode::ADOX => {
            write!(out, "{} += {} + rflags.of", instr.operand(0), instr.operand(1))?;
        }
        Opcode::SBB => {
            write!(out, "{} -= {} + rflags.cf", instr.operand(0), instr.operand(1))?;
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
            write!(out, "rflags = flags({} - {})", instr.operand(0), instr.operand(1))?;
        }
        Opcode::TEST => {
            write!(out, "rflags = flags({} & {})", instr.operand(0), instr.operand(1))?;
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
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JRCXZ => {
            out.write_str("if rcx == 0 then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JECXZ => {
            out.write_str("if ecx == 0 then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::LOOP => {
            out.write_str("rcx--; if rcx != 0 then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::LOOPZ => {
            out.write_str("rcx--; if rcx != 0 and zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::LOOPNZ => {
            out.write_str("rcx--; if rcx != 0 and !zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JO => {
            out.write_str("if _(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JNO => {
            out.write_str("if _(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JB => {
            out.write_str("if /* unsigned */ below(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JNB => {
            out.write_str("if /* unsigned */ above_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JZ => {
            out.write_str("if zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JNZ => {
            out.write_str("if !zero(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JNA => {
            out.write_str("if /* unsigned */ below_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JA => {
            out.write_str("if /* unsigned */ above(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JS => {
            out.write_str("if signed(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JNS => {
            out.write_str("if !signed(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JP => {
            out.write_str("if parity(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JNP => {
            out.write_str("if !parity(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JL => {
            out.write_str("if /* signed */ less(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JGE => {
            out.write_str("if /* signed */ greater_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JLE => {
            out.write_str("if /* signed */ less_or_equal(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
        },
        Opcode::JG => {
            out.write_str("if /* signed */ greater(rflags) then jmp ")?;
            write_jmp_operand(instr.operand(0), out)?;
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

// allowing these deprecated items for the time being, not yet breaking yaxpeax-x86 apis
#[allow(deprecated)]
impl <'instr, T: fmt::Write, Y: YaxColors> ShowContextual<u64, NoContext, T, Y> for InstructionDisplayer<'instr> {
    fn contextualize(&self, _colors: &Y, _address: u64, _context: Option<&NoContext>, out: &mut T) -> fmt::Result {
        let InstructionDisplayer {
            instr,
            style,
        } = self;

        let mut out = yaxpeax_arch::display::FmtSink::new(out);

        let rules = DefaultRules::for_style(*style);
        match style {
            DisplayStyle::Intel => {
                format_intel(instr, &rules, &mut out)
            }
            DisplayStyle::C => {
                format_c(instr, &rules, &mut out)
            }
            DisplayStyle::Masm => {
                masm::contextualize(&instr, &rules, &mut out)
            }
        }
    }
}

// allowing these deprecated items for the time being, not yet breaking yaxpeax-x86 apis
#[allow(deprecated)]
#[cfg(feature="std")]
impl <T: fmt::Write, Y: YaxColors> ShowContextual<u64, [Option<alloc::string::String>], T, Y> for Instruction {
    fn contextualize(&self, colors: &Y, _address: u64, context: Option<&[Option<alloc::string::String>]>, out: &mut T) -> fmt::Result {
        let mut out = yaxpeax_arch::display::FmtSink::new(out);
        let out = &mut out;
        let rules = DefaultRules::for_style(DisplayStyle::Intel);
        let rules = &rules;
        use core::fmt::Write;

        if self.prefixes.lock() {
            write!(out, "lock ")?;
        }

        if self.prefixes.rep_any() {
            if self.opcode.can_rep() {
                // only a few of you actually use the prefix...
                if self.prefixes.rep() {
                    write!(out, "rep ")?;
                } else if self.prefixes.repnz() {
                    write!(out, "repnz ")?;
                }
            }
        }

        self.opcode.colorize(colors, out)?;

        match context.and_then(|xs| xs[0].as_ref()) {
            Some(s) => { write!(out, " {}", s)?; },
            None => {
                match self.operands[0] {
                    super::OperandSpec::Nothing => {
                        return Ok(());
                    },
                    _ => {
                        write!(out, " ")?;
                        if let Some(prefix) = self.segment_override_for_op(0) {
                            write!(out, "{}:", prefix)?;
                        }
                    }
                }

                let mut displayer = DisplayingOperandVisitor {
                    f: out,
                    rules,
                };
                self.visit_operand(0, &mut displayer)?;
            }
        };
        for i in 1..self.operand_count {
            let i = i as usize;
            match context.and_then(|xs| xs[i].as_ref()) {
                Some(s) => { write!(out, ", {}", s)? }
                None => {
                    match &self.operands[i] {
                        &super::OperandSpec::Nothing => {
                            return Ok(());
                        },
                        _ => {
                            write!(out, ", ")?;
                            let mut displayer = DisplayingOperandVisitor {
                                f: out,
                                rules,
                            };
                            self.visit_operand(i as u8, &mut displayer)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

static RELATIVE_BRANCHES: [Opcode; 23] = [
    Opcode::JMP, Opcode::CALL,
    Opcode::JRCXZ, Opcode::JECXZ,
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

struct RelativeBranchPrinter<'a, F: DisplaySink, Rules: DisplayRules<F>> {
    inst: &'a Instruction,
    out: &'a mut F,
    rules: &'a Rules,
}

impl<'a, F: DisplaySink, Rules: DisplayRules<F>> super::OperandVisitor for RelativeBranchPrinter<'a, F, Rules> {
    // return true if we printed a relative branch offset, false otherwise
    type Ok = bool;
    // but errors are errors
    type Error = fmt::Error;

    fn visit_reg(&mut self, _reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_deref(&mut self, _base: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_disp(&mut self, _base: RegSpec, _disp: i32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i8(&mut self, rel: i8) -> Result<Self::Ok, Self::Error> {
        if RELATIVE_BRANCHES.contains(&self.inst.opcode) {
            if self.rules.emit_branch_addr(&self.inst, rel as i32, &mut self.out)? {
                // the display rule declared it has fully printed the relative address, so we have
                // nothing to do.
                return Ok(true);
            }

            self.out.write_char('$')?;
            let mut v = rel as u8;
            if rel < 0 {
                self.out.write_char('-')?;
                v = rel.unsigned_abs();
            } else {
                self.out.write_char('+')?;
            }
            self.out.write_fixed_size("0x")?;
            self.out.write_u8(v)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i32(&mut self, rel: i32) -> Result<Self::Ok, Self::Error> {
        if RELATIVE_BRANCHES.contains(&self.inst.opcode) || self.inst.opcode == Opcode::XBEGIN {
            if self.rules.emit_branch_addr(&self.inst, rel, &mut self.out)? {
                // the display rule declared it has fully printed the relative address, so we have
                // nothing to do.
                return Ok(true);
            }

            self.out.write_char('$')?;
            let mut v = rel as u32;
            if rel < 0 {
                self.out.write_char('-')?;
                v = rel.unsigned_abs();
            } else {
                self.out.write_char('+')?;
            }
            self.out.write_fixed_size("0x")?;
            self.out.write_u32(v)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
    fn visit_u8(&mut self, _imm: u8) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_i16(&mut self, _imm: i16) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_u16(&mut self, _imm: u16) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_u32(&mut self, _imm: u32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_i64(&mut self, _imm: i64) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_u64(&mut self, _imm: u64) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_abs_u32(&mut self, _imm: u32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_abs_u64(&mut self, _imm: u64) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale(&mut self, _index: RegSpec, _scale: u8) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale_disp(&mut self, _index: RegSpec, _scale: u8, _disp: i32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale_disp(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8, _disp: i32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_other(&mut self) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_reg_mask_merge(&mut self, _spec: RegSpec, _mask: RegSpec, _merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_reg_mask_merge_sae(&mut self, _spec: RegSpec, _mask: RegSpec, _merge_mode: MergeMode, _sae_mode: super::SaeMode) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_reg_mask_merge_sae_noround(&mut self, _spec: RegSpec, _mask: RegSpec, _merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_disp_masked(&mut self, _base: RegSpec, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_deref_masked(&mut self, _base: RegSpec, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale_masked(&mut self, _index: RegSpec, _scale: u8, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale_disp_masked(&mut self, _index: RegSpec, _scale: u8, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_masked(&mut self, _base: RegSpec, _index: RegSpec, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_disp_masked(&mut self, _base: RegSpec, _index: RegSpec, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale_masked(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale_disp_masked(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
}

#[cfg(feature="alloc")]
mod buffer_sink {
    use core::fmt;
    use super::super::{DisplayStyle, InstructionDisplayer};
    use super::{format_c, format_intel};
    use super::DefaultRules;

    /// helper to format `amd64` instructions with highest throughput and least configuration. this is
    /// functionally a buffer for one x86 instruction's text.
    ///
    /// ### when to use this over `fmt::Display`?
    ///
    /// `fmt::Display` is a fair choice in most cases. in some cases, `InstructionTextBuffer` may
    /// support formatting options that may be difficult to configure for a `Display` impl.
    /// additionally, `InstructionTextBuffer` may be able to specialize more effectively where
    /// `fmt::Display`, writing to a generic `fmt::Write`, may not.
    ///
    /// if your use case for `yaxpeax-x86` involves being bounded on the speed of disassembling and
    /// formatting instructions, [`InstructionTextBuffer::format_inst`] has been measured as up to 11%
    /// faster than an equivalent `write!(buf, "{}", inst)`.
    ///
    /// `InstructionTextBuffer` involves internal allocations; if your use case for `yaxpeax-x86`
    /// requires allocations never occurring, it is not an appropriate tool.
    ///
    /// ### example
    ///
    /// ```
    /// use yaxpeax_x86::long_mode::InstDecoder;
    /// use yaxpeax_x86::long_mode::InstructionTextBuffer;
    /// use yaxpeax_x86::long_mode::DisplayStyle;
    ///
    /// let bytes = &[0x33, 0xc0];
    /// let inst = InstDecoder::default().decode_slice(bytes).expect("can decode");
    /// let mut text_buf = InstructionTextBuffer::new();
    /// assert_eq!(
    ///     text_buf.format_inst(&inst.display_with(DisplayStyle::Intel)).expect("can format"),
    ///     "xor eax, eax"
    /// );
    ///
    /// // or, getting the formatted instruction with `text_str`:
    /// assert_eq!(
    ///     text_buf.text_str(),
    ///     "xor eax, eax"
    /// );
    /// ```
    pub struct InstructionTextBuffer {
        content: alloc::string::String,
    }

    impl InstructionTextBuffer {
        /// create an `InstructionTextBuffer` with default settings. `InstructionTextBuffer`'s default
        /// settings format instructions identically to their corresponding `fmt::Display`.
        pub fn new() -> Self {
            let mut buf = alloc::string::String::new();
            buf.reserve(crate::MAX_INSTRUCTION_LEN);
            Self {
                content: buf,
            }
        }

        /// format `inst` into this buffer. returns a borrow of that same internal buffer for convenience.
        ///
        /// this clears and reuses an internal buffer; if an instruction had been previously formatted
        /// through this buffer, it will be overwritten.
        pub fn format_inst<'buf, 'instr>(&'buf mut self, display: &InstructionDisplayer<'instr>) -> Result<&'buf str, fmt::Error> {
            // Safety: this sink is used to format exactly one instruction and then dropped. it can
            // never escape `format_inst`.
            let mut handle = unsafe { self.write_handle() };

            let rules = DefaultRules::for_style(display.style);
            match display.style {
                DisplayStyle::Intel => {
                    format_intel(&display.instr, &rules, &mut handle)?;
                }
                DisplayStyle::C => {
                    format_c(&display.instr, &rules, &mut handle)?;
                }
                DisplayStyle::Masm => {
                    super::masm::contextualize(&display.instr, &rules, &mut handle)?;
                }
            }

            Ok(self.text_str())
        }

        /// return a borrow of the internal buffer. if an instruction has been formatted, the
        /// returned `&str` contains that instruction's buffered text.
        pub fn text_str(&self) -> &str {
            self.content.as_str()
        }

        /// do the necessary bookkeeping and provide an `InstructionTextSink` to write an instruction
        /// into.
        ///
        /// SAFETY: callers must print at most one instruction into this handle.
        unsafe fn write_handle<'buf>(&'buf mut self) -> yaxpeax_arch::display::InstructionTextSink<'buf> {
            self.content.clear();
            // Safety: `content` was just cleared, so writing begins at the start of the buffer.
            // `content`is large enough to hold a fully-formatted instruction (see
            // `InstructionTextBuffer::new`).
            yaxpeax_arch::display::InstructionTextSink::new(&mut self.content)
        }
    }
}
#[cfg(feature="alloc")]
pub use buffer_sink::InstructionTextBuffer;
