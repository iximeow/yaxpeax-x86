use crate::{safer_unchecked::GetSaferUnchecked, Opcode, RegSpec, Segment};
use core::fmt;

use yaxpeax_arch::{Colorize, YaxColors};

// register names are grouped by indices scaled by 16.
// xmm, ymm, zmm all get two indices.
#[rustfmt::skip]
const REG_NAMES: &[&'static str] = &[
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
    "ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",
    "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
    "al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",
    "cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7", "cr8", "cr9", "cr10", "cr11", "cr12", "cr13", "cr14", "cr15",
    "dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7", "dr8", "dr9", "dr10", "dr11", "dr12", "dr13", "dr14", "dr15",
    "es", "cs", "ss", "ds", "fs", "gs", "BUG", "BUG",
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

impl fmt::Display for RegSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(regspec_label(self))
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name())
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

const MNEMONICS: &[&'static str] = &[
    "invalid",
    "add",
    "or",
    "adc",
    "sbb",
    "and",
    "xor",
    "sub",
    "cmp",
    "xadd",
    "bt",
    "bts",
    "btc",
    "btr",
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
    "sar",
    "sal",
    "shr",
    "shrd",
    "shl",
    "rcr",
    "rcl",
    "ror",
    "rol",
    "inc",
    "dec",
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
    "prefetch0",
    "prefetch1",
    "prefetch2",
    "xchg",
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
    "lods",
    "stos",
    "lahf",
    "sahf",
    "cmps",
    "scas",
    "movs",
    "test",
    "ins",
    "in",
    "outs",
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
    "neg",
    "not",
    "cmpxchg",
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
    "les",
    "lds",
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
    "cmpxchg8b",
    "cmpxchg16b",
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
    "jecxz",
    "jcxz",
    "pusha",
    "popa",
    "bound",
    "arpl",
    "aas",
    "aaa",
    "das",
    "daa",
    "aam",
    "aad",
    "movdir64b",
    "movdiri",
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
    "pi2iw",
    "pi2id",
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
    // INVPCID,
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
];

impl Opcode {
    pub(crate) fn name(&self) -> &'static str {
        unsafe { MNEMONICS.get_kinda_unchecked(*self as usize) }
    }
}

impl<T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Opcode {
    fn colorize(&self, colors: &Y, out: &mut T) -> fmt::Result {
        match self {
            Opcode::VGF2P8AFFINEQB
            | Opcode::VGF2P8AFFINEINVQB
            | Opcode::VPSHRDQ
            | Opcode::VPSHRDD
            | Opcode::VPSHRDW
            | Opcode::VPSHLDQ
            | Opcode::VPSHLDD
            | Opcode::VPSHLDW
            | Opcode::VBROADCASTF32X8
            | Opcode::VBROADCASTF64X4
            | Opcode::VBROADCASTF32X4
            | Opcode::VBROADCASTF64X2
            | Opcode::VBROADCASTF32X2
            | Opcode::VBROADCASTI32X8
            | Opcode::VBROADCASTI64X4
            | Opcode::VBROADCASTI32X4
            | Opcode::VBROADCASTI64X2
            | Opcode::VBROADCASTI32X2
            | Opcode::VEXTRACTI32X8
            | Opcode::VEXTRACTF32X8
            | Opcode::VINSERTI32X8
            | Opcode::VINSERTF32X8
            | Opcode::VINSERTI32X4
            | Opcode::V4FNMADDSS
            | Opcode::V4FNMADDPS
            | Opcode::VCVTNEPS2BF16
            | Opcode::V4FMADDSS
            | Opcode::V4FMADDPS
            | Opcode::VCVTNE2PS2BF16
            | Opcode::VP2INTERSECTD
            | Opcode::VP2INTERSECTQ
            | Opcode::VP4DPWSSDS
            | Opcode::VP4DPWSSD
            | Opcode::VPDPWSSDS
            | Opcode::VPDPWSSD
            | Opcode::VPDPBUSDS
            | Opcode::VDPBF16PS
            | Opcode::VPBROADCASTMW2D
            | Opcode::VPBROADCASTMB2Q
            | Opcode::VPMOVD2M
            | Opcode::VPMOVQD
            | Opcode::VPMOVWB
            | Opcode::VPMOVDB
            | Opcode::VPMOVDW
            | Opcode::VPMOVQB
            | Opcode::VPMOVQW
            | Opcode::VGF2P8MULB
            | Opcode::VPMADD52HUQ
            | Opcode::VPMADD52LUQ
            | Opcode::VPSHUFBITQMB
            | Opcode::VPERMB
            | Opcode::VPEXPANDD
            | Opcode::VPEXPANDQ
            | Opcode::VPABSQ
            | Opcode::VPRORVD
            | Opcode::VPRORVQ
            | Opcode::VPMULTISHIFTQB
            | Opcode::VPERMT2B
            | Opcode::VPERMT2W
            | Opcode::VPSHRDVQ
            | Opcode::VPSHRDVD
            | Opcode::VPSHRDVW
            | Opcode::VPSHLDVQ
            | Opcode::VPSHLDVD
            | Opcode::VPSHLDVW
            | Opcode::VPCOMPRESSB
            | Opcode::VPCOMPRESSW
            | Opcode::VPEXPANDB
            | Opcode::VPEXPANDW
            | Opcode::VPOPCNTD
            | Opcode::VPOPCNTQ
            | Opcode::VPOPCNTB
            | Opcode::VPOPCNTW
            | Opcode::VSCALEFSS
            | Opcode::VSCALEFSD
            | Opcode::VSCALEFPS
            | Opcode::VSCALEFPD
            | Opcode::VPDPBUSD
            | Opcode::VCVTUSI2SD
            | Opcode::VCVTUSI2SS
            | Opcode::VPXORD
            | Opcode::VPXORQ
            | Opcode::VPORD
            | Opcode::VPORQ
            | Opcode::VPANDND
            | Opcode::VPANDNQ
            | Opcode::VPANDD
            | Opcode::VPANDQ
            | Opcode::VHADDPS
            | Opcode::VHSUBPS
            | Opcode::VADDSUBPS
            | Opcode::VADDPD
            | Opcode::VADDPS
            | Opcode::VADDSD
            | Opcode::VADDSS
            | Opcode::VADDSUBPD
            | Opcode::VFMADD132PD
            | Opcode::VFMADD132PS
            | Opcode::VFMADD132SD
            | Opcode::VFMADD132SS
            | Opcode::VFMADD213PD
            | Opcode::VFMADD213PS
            | Opcode::VFMADD213SD
            | Opcode::VFMADD213SS
            | Opcode::VFMADD231PD
            | Opcode::VFMADD231PS
            | Opcode::VFMADD231SD
            | Opcode::VFMADD231SS
            | Opcode::VFMADDSUB132PD
            | Opcode::VFMADDSUB132PS
            | Opcode::VFMADDSUB213PD
            | Opcode::VFMADDSUB213PS
            | Opcode::VFMADDSUB231PD
            | Opcode::VFMADDSUB231PS
            | Opcode::VFMSUB132PD
            | Opcode::VFMSUB132PS
            | Opcode::VFMSUB132SD
            | Opcode::VFMSUB132SS
            | Opcode::VFMSUB213PD
            | Opcode::VFMSUB213PS
            | Opcode::VFMSUB213SD
            | Opcode::VFMSUB213SS
            | Opcode::VFMSUB231PD
            | Opcode::VFMSUB231PS
            | Opcode::VFMSUB231SD
            | Opcode::VFMSUB231SS
            | Opcode::VFMSUBADD132PD
            | Opcode::VFMSUBADD132PS
            | Opcode::VFMSUBADD213PD
            | Opcode::VFMSUBADD213PS
            | Opcode::VFMSUBADD231PD
            | Opcode::VFMSUBADD231PS
            | Opcode::VFNMADD132PD
            | Opcode::VFNMADD132PS
            | Opcode::VFNMADD132SD
            | Opcode::VFNMADD132SS
            | Opcode::VFNMADD213PD
            | Opcode::VFNMADD213PS
            | Opcode::VFNMADD213SD
            | Opcode::VFNMADD213SS
            | Opcode::VFNMADD231PD
            | Opcode::VFNMADD231PS
            | Opcode::VFNMADD231SD
            | Opcode::VFNMADD231SS
            | Opcode::VFNMSUB132PD
            | Opcode::VFNMSUB132PS
            | Opcode::VFNMSUB132SD
            | Opcode::VFNMSUB132SS
            | Opcode::VFNMSUB213PD
            | Opcode::VFNMSUB213PS
            | Opcode::VFNMSUB213SD
            | Opcode::VFNMSUB213SS
            | Opcode::VFNMSUB231PD
            | Opcode::VFNMSUB231PS
            | Opcode::VFNMSUB231SD
            | Opcode::VFNMSUB231SS
            | Opcode::VDIVPD
            | Opcode::VDIVPS
            | Opcode::VDIVSD
            | Opcode::VDIVSS
            | Opcode::VHADDPD
            | Opcode::VHSUBPD
            | Opcode::HADDPD
            | Opcode::HSUBPD
            | Opcode::VMULPD
            | Opcode::VMULPS
            | Opcode::VMULSD
            | Opcode::VMULSS
            | Opcode::VPABSB
            | Opcode::VPABSD
            | Opcode::VPABSW
            | Opcode::PABSB
            | Opcode::PABSD
            | Opcode::PABSW
            | Opcode::VPSIGNB
            | Opcode::VPSIGND
            | Opcode::VPSIGNW
            | Opcode::PSIGNB
            | Opcode::PSIGND
            | Opcode::PSIGNW
            | Opcode::VPADDB
            | Opcode::VPADDD
            | Opcode::VPADDQ
            | Opcode::VPADDSB
            | Opcode::VPADDSW
            | Opcode::VPADDUSB
            | Opcode::VPADDUSW
            | Opcode::VPADDW
            | Opcode::VPAVGB
            | Opcode::VPAVGW
            | Opcode::VPMULDQ
            | Opcode::VPMULHRSW
            | Opcode::VPMULHUW
            | Opcode::VPMULHW
            | Opcode::VPMULLQ
            | Opcode::VPMULLD
            | Opcode::VPMULLW
            | Opcode::VPMULUDQ
            | Opcode::PCLMULQDQ
            | Opcode::PMULDQ
            | Opcode::PMULHRSW
            | Opcode::PMULLD
            | Opcode::VPSUBB
            | Opcode::VPSUBD
            | Opcode::VPSUBQ
            | Opcode::VPSUBSB
            | Opcode::VPSUBSW
            | Opcode::VPSUBUSB
            | Opcode::VPSUBUSW
            | Opcode::VPSUBW
            | Opcode::VROUNDPD
            | Opcode::VROUNDPS
            | Opcode::VEXP2PD
            | Opcode::VEXP2PS
            | Opcode::VEXP2SD
            | Opcode::VEXP2SS
            | Opcode::VRCP28PD
            | Opcode::VRCP28PS
            | Opcode::VRCP28SD
            | Opcode::VRCP28SS
            | Opcode::VRCP14PD
            | Opcode::VRCP14PS
            | Opcode::VRCP14SD
            | Opcode::VRCP14SS
            | Opcode::VRNDSCALEPD
            | Opcode::VRNDSCALEPS
            | Opcode::VRNDSCALESD
            | Opcode::VRNDSCALESS
            | Opcode::VRSQRT14PD
            | Opcode::VRSQRT14PS
            | Opcode::VRSQRT14SD
            | Opcode::VRSQRT14SS
            | Opcode::VSCALEDPD
            | Opcode::VSCALEDPS
            | Opcode::VSCALEDSD
            | Opcode::VSCALEDSS
            | Opcode::VRSQRT28PD
            | Opcode::VRSQRT28PS
            | Opcode::VRSQRT28SD
            | Opcode::VRSQRT28SS
            | Opcode::VRSQRTPS
            | Opcode::VSQRTPD
            | Opcode::VSQRTPS
            | Opcode::VSUBPD
            | Opcode::VSUBPS
            | Opcode::VSUBSD
            | Opcode::VSUBSS
            | Opcode::VRCPSS
            | Opcode::VROUNDSD
            | Opcode::VROUNDSS
            | Opcode::ROUNDPD
            | Opcode::ROUNDPS
            | Opcode::ROUNDSD
            | Opcode::ROUNDSS
            | Opcode::VRSQRTSS
            | Opcode::VSQRTSD
            | Opcode::VSQRTSS
            | Opcode::VPSADBW
            | Opcode::VMPSADBW
            | Opcode::VDBPSADBW
            | Opcode::VPHADDD
            | Opcode::VPHADDSW
            | Opcode::VPHADDW
            | Opcode::VPHSUBD
            | Opcode::VPHSUBSW
            | Opcode::VPHSUBW
            | Opcode::VPMADDUBSW
            | Opcode::VPMADDWD
            | Opcode::VDPPD
            | Opcode::VDPPS
            | Opcode::VRCPPS
            | Opcode::VORPD
            | Opcode::VORPS
            | Opcode::VANDPD
            | Opcode::VANDPS
            | Opcode::VANDNPD
            | Opcode::VANDNPS
            | Opcode::VPAND
            | Opcode::VPANDN
            | Opcode::VPOR
            | Opcode::VPXOR
            | Opcode::VXORPD
            | Opcode::VXORPS
            | Opcode::VPSLLD
            | Opcode::VPSLLDQ
            | Opcode::VPSLLQ
            | Opcode::VPSLLVD
            | Opcode::VPSLLVQ
            | Opcode::VPSLLW
            | Opcode::VPROLD
            | Opcode::VPROLQ
            | Opcode::VPROLVD
            | Opcode::VPROLVQ
            | Opcode::VPRORD
            | Opcode::VPRORQ
            | Opcode::VPRORRD
            | Opcode::VPRORRQ
            | Opcode::VPSLLVW
            | Opcode::VPSRAQ
            | Opcode::VPSRAVQ
            | Opcode::VPSRAVW
            | Opcode::VPSRLVW
            | Opcode::VPSRAD
            | Opcode::VPSRAVD
            | Opcode::VPSRAW
            | Opcode::VPSRLD
            | Opcode::VPSRLDQ
            | Opcode::VPSRLQ
            | Opcode::VPSRLVD
            | Opcode::VPSRLVQ
            | Opcode::VPSRLW
            | Opcode::PHADDD
            | Opcode::PHADDSW
            | Opcode::PHADDW
            | Opcode::PHSUBD
            | Opcode::PHSUBSW
            | Opcode::PHSUBW
            | Opcode::PMADDUBSW
            | Opcode::ADDSUBPD
            | Opcode::DPPS
            | Opcode::DPPD
            | Opcode::MPSADBW
            | Opcode::RCPSS
            | Opcode::RSQRTSS
            | Opcode::SQRTSD
            | Opcode::ADDSD
            | Opcode::SUBSD
            | Opcode::MULSD
            | Opcode::DIVSD
            | Opcode::SQRTSS
            | Opcode::ADDSS
            | Opcode::SUBSS
            | Opcode::MULSS
            | Opcode::DIVSS
            | Opcode::HADDPS
            | Opcode::HSUBPS
            | Opcode::ADDSUBPS
            | Opcode::PMULHRW
            | Opcode::PFRCP
            | Opcode::PFRSQRT
            | Opcode::PFSUB
            | Opcode::PFADD
            | Opcode::PFRCPIT1
            | Opcode::PFRSQIT1
            | Opcode::PFSUBR
            | Opcode::PFACC
            | Opcode::PFMUL
            | Opcode::PFMULHRW
            | Opcode::PFRCPIT2
            | Opcode::PFNACC
            | Opcode::PFPNACC
            | Opcode::PSWAPD
            | Opcode::PAVGUSB
            | Opcode::XADD
            | Opcode::DIV
            | Opcode::IDIV
            | Opcode::MUL
            | Opcode::MULX
            | Opcode::NEG
            | Opcode::NOT
            | Opcode::SAR
            | Opcode::SAL
            | Opcode::SHR
            | Opcode::SARX
            | Opcode::SHLX
            | Opcode::SHRX
            | Opcode::SHRD
            | Opcode::SHL
            | Opcode::RCR
            | Opcode::RCL
            | Opcode::ROR
            | Opcode::RORX
            | Opcode::ROL
            | Opcode::INC
            | Opcode::DEC
            | Opcode::SBB
            | Opcode::AND
            | Opcode::XOR
            | Opcode::OR
            | Opcode::LEA
            | Opcode::ADD
            | Opcode::ADC
            | Opcode::ADCX
            | Opcode::ADOX
            | Opcode::SUB
            | Opcode::POPCNT
            | Opcode::LZCNT
            | Opcode::VPLZCNTD
            | Opcode::VPLZCNTQ
            | Opcode::BT
            | Opcode::BTS
            | Opcode::BTR
            | Opcode::BTC
            | Opcode::BSF
            | Opcode::BSR
            | Opcode::BZHI
            | Opcode::PDEP
            | Opcode::PEXT
            | Opcode::TZCNT
            | Opcode::ANDN
            | Opcode::BEXTR
            | Opcode::BLSI
            | Opcode::BLSMSK
            | Opcode::BLSR
            | Opcode::ADDPS
            | Opcode::ADDPD
            | Opcode::ANDNPS
            | Opcode::ANDNPD
            | Opcode::ANDPS
            | Opcode::ANDPD
            | Opcode::COMISD
            | Opcode::COMISS
            | Opcode::DIVPS
            | Opcode::DIVPD
            | Opcode::MULPS
            | Opcode::MULPD
            | Opcode::ORPS
            | Opcode::ORPD
            | Opcode::PADDB
            | Opcode::PADDD
            | Opcode::PADDQ
            | Opcode::PADDSB
            | Opcode::PADDSW
            | Opcode::PADDUSB
            | Opcode::PADDUSW
            | Opcode::PADDW
            | Opcode::PAND
            | Opcode::PANDN
            | Opcode::PAVGB
            | Opcode::PAVGW
            | Opcode::PMADDWD
            | Opcode::PMULHUW
            | Opcode::PMULHW
            | Opcode::PMULLW
            | Opcode::PMULUDQ
            | Opcode::POR
            | Opcode::PSADBW
            | Opcode::PSHUFD
            | Opcode::PSHUFW
            | Opcode::PSHUFB
            | Opcode::PSLLD
            | Opcode::PSLLDQ
            | Opcode::PSLLQ
            | Opcode::PSLLW
            | Opcode::PSRAD
            | Opcode::PSRAW
            | Opcode::PSRLD
            | Opcode::PSRLDQ
            | Opcode::PSRLQ
            | Opcode::PSRLW
            | Opcode::PSUBB
            | Opcode::PSUBD
            | Opcode::PSUBQ
            | Opcode::PSUBSB
            | Opcode::PSUBSW
            | Opcode::PSUBUSB
            | Opcode::PSUBUSW
            | Opcode::PSUBW
            | Opcode::PXOR
            | Opcode::RSQRTPS
            | Opcode::SQRTPS
            | Opcode::SQRTPD
            | Opcode::SUBPS
            | Opcode::SUBPD
            | Opcode::XORPS
            | Opcode::XORPD
            | Opcode::RCPPS
            | Opcode::SHLD
            | Opcode::SLHD
            | Opcode::UCOMISD
            | Opcode::UCOMISS
            | Opcode::F2XM1
            | Opcode::FABS
            | Opcode::FADD
            | Opcode::FADDP
            | Opcode::FCHS
            | Opcode::FCOS
            | Opcode::FDIV
            | Opcode::FDIVP
            | Opcode::FDIVR
            | Opcode::FDIVRP
            | Opcode::FIADD
            | Opcode::FIDIV
            | Opcode::FIDIVR
            | Opcode::FIMUL
            | Opcode::FISUB
            | Opcode::FISUBR
            | Opcode::FMUL
            | Opcode::FMULP
            | Opcode::FNCLEX
            | Opcode::FNINIT
            | Opcode::FPATAN
            | Opcode::FPREM
            | Opcode::FPREM1
            | Opcode::FPTAN
            | Opcode::FRNDINT
            | Opcode::FSCALE
            | Opcode::FSIN
            | Opcode::FSINCOS
            | Opcode::FSQRT
            | Opcode::FSUB
            | Opcode::FSUBP
            | Opcode::FSUBR
            | Opcode::FSUBRP
            | Opcode::FXTRACT
            | Opcode::FYL2X
            | Opcode::FYL2XP1
            | Opcode::AAA
            | Opcode::AAS
            | Opcode::DAS
            | Opcode::DAA
            | Opcode::AAD
            | Opcode::AAM
            | Opcode::KADDB
            | Opcode::KANDB
            | Opcode::KANDNB
            | Opcode::KNOTB
            | Opcode::KORB
            | Opcode::KSHIFTLB
            | Opcode::KSHIFTRB
            | Opcode::KXNORB
            | Opcode::KXORB
            | Opcode::KADDW
            | Opcode::KANDW
            | Opcode::KANDNW
            | Opcode::KNOTW
            | Opcode::KORW
            | Opcode::KSHIFTLW
            | Opcode::KSHIFTRW
            | Opcode::KXNORW
            | Opcode::KXORW
            | Opcode::KADDD
            | Opcode::KANDD
            | Opcode::KANDND
            | Opcode::KNOTD
            | Opcode::KORD
            | Opcode::KSHIFTLD
            | Opcode::KSHIFTRD
            | Opcode::KXNORD
            | Opcode::KXORD
            | Opcode::KADDQ
            | Opcode::KANDQ
            | Opcode::KANDNQ
            | Opcode::KNOTQ
            | Opcode::KORQ
            | Opcode::KSHIFTLQ
            | Opcode::KSHIFTRQ
            | Opcode::KXNORQ
            | Opcode::KXORQ
            | Opcode::IMUL => {
                write!(out, "{}", colors.arithmetic_op(self))
            }
            Opcode::POPF
            | Opcode::PUSHF
            | Opcode::ENTER
            | Opcode::LEAVE
            | Opcode::PUSHA
            | Opcode::POPA
            | Opcode::PUSH
            | Opcode::POP => {
                write!(out, "{}", colors.stack_op(self))
            }
            Opcode::WAIT
            | Opcode::FNOP
            | Opcode::FDISI8087_NOP
            | Opcode::FENI8087_NOP
            | Opcode::FSETPM287_NOP
            | Opcode::PREFETCHNTA
            | Opcode::PREFETCH0
            | Opcode::PREFETCH1
            | Opcode::PREFETCH2
            | Opcode::PREFETCHW
            | Opcode::NOP => {
                write!(out, "{}", colors.nop_op(self))
            }

            /* Control flow */
            Opcode::HLT
            | Opcode::INT
            | Opcode::INTO
            | Opcode::IRET
            | Opcode::IRETD
            | Opcode::IRETQ
            | Opcode::RETF
            | Opcode::RETURN => {
                write!(out, "{}", colors.stop_op(self))
            }
            Opcode::LOOPNZ
            | Opcode::LOOPZ
            | Opcode::LOOP
            | Opcode::JCXZ
            | Opcode::JECXZ
            | Opcode::JRCXZ
            | Opcode::CALL
            | Opcode::CALLF
            | Opcode::JMP
            | Opcode::JMPF
            | Opcode::JO
            | Opcode::JNO
            | Opcode::JB
            | Opcode::JNB
            | Opcode::JZ
            | Opcode::JNZ
            | Opcode::JA
            | Opcode::JNA
            | Opcode::JS
            | Opcode::JNS
            | Opcode::JP
            | Opcode::JNP
            | Opcode::JL
            | Opcode::JGE
            | Opcode::JLE
            | Opcode::JG => {
                write!(out, "{}", colors.control_flow_op(self))
            }

            /* Data transfer */
            Opcode::PI2FW
            | Opcode::PI2FD
            | Opcode::PF2ID
            | Opcode::PF2IW
            | Opcode::VCVTDQ2PD
            | Opcode::VCVTDQ2PS
            | Opcode::VCVTPD2DQ
            | Opcode::VCVTPD2PS
            | Opcode::VCVTPH2PS
            | Opcode::VCVTPS2DQ
            | Opcode::VCVTPS2PD
            | Opcode::VCVTPS2PH
            | Opcode::VCVTTPD2DQ
            | Opcode::VCVTTPS2DQ
            | Opcode::VCVTSD2SI
            | Opcode::VCVTSD2SS
            | Opcode::VCVTSI2SD
            | Opcode::VCVTSI2SS
            | Opcode::VCVTSS2SD
            | Opcode::VCVTSS2SI
            | Opcode::VCVTTSD2SI
            | Opcode::VCVTTSS2SI
            | Opcode::VCVTPD2UDQ
            | Opcode::VCVTTPD2UDQ
            | Opcode::VCVTPS2UDQ
            | Opcode::VCVTTPS2UDQ
            | Opcode::VCVTQQ2PD
            | Opcode::VCVTQQ2PS
            | Opcode::VCVTSD2USI
            | Opcode::VCVTTSD2USI
            | Opcode::VCVTSS2USI
            | Opcode::VCVTTSS2USI
            | Opcode::VCVTUDQ2PD
            | Opcode::VCVTUDQ2PS
            | Opcode::VCVTUSI2USD
            | Opcode::VCVTUSI2USS
            | Opcode::VCVTTPD2QQ
            | Opcode::VCVTPD2QQ
            | Opcode::VCVTTPD2UQQ
            | Opcode::VCVTPD2UQQ
            | Opcode::VCVTTPS2QQ
            | Opcode::VCVTPS2QQ
            | Opcode::VCVTTPS2UQQ
            | Opcode::VCVTPS2UQQ
            | Opcode::VCVTUQQ2PD
            | Opcode::VCVTUQQ2PS
            | Opcode::VMOVDDUP
            | Opcode::VPSHUFLW
            | Opcode::VPSHUFHW
            | Opcode::VBLENDMPD
            | Opcode::VBLENDMPS
            | Opcode::VPBLENDMD
            | Opcode::VPBLENDMQ
            | Opcode::VBLENDPD
            | Opcode::VBLENDPS
            | Opcode::VBLENDVPD
            | Opcode::VBLENDVPS
            | Opcode::VPBLENDMB
            | Opcode::VPBLENDMW
            | Opcode::PBLENDVB
            | Opcode::PBLENDW
            | Opcode::BLENDPD
            | Opcode::BLENDPS
            | Opcode::BLENDVPD
            | Opcode::BLENDVPS
            | Opcode::BLENDW
            | Opcode::VBROADCASTF128
            | Opcode::VBROADCASTI128
            | Opcode::VBROADCASTSD
            | Opcode::VBROADCASTSS
            | Opcode::VPBROADCASTM
            | Opcode::VEXTRACTF128
            | Opcode::VEXTRACTI128
            | Opcode::VEXTRACTPS
            | Opcode::EXTRACTPS
            | Opcode::VGATHERDPD
            | Opcode::VGATHERDPS
            | Opcode::VGATHERQPD
            | Opcode::VGATHERQPS
            | Opcode::VGATHERPF0DPD
            | Opcode::VGATHERPF0DPS
            | Opcode::VGATHERPF0QPD
            | Opcode::VGATHERPF0QPS
            | Opcode::VGATHERPF1DPD
            | Opcode::VGATHERPF1DPS
            | Opcode::VGATHERPF1QPD
            | Opcode::VGATHERPF1QPS
            | Opcode::VSCATTERDD
            | Opcode::VSCATTERDQ
            | Opcode::VSCATTERQD
            | Opcode::VSCATTERQQ
            | Opcode::VPSCATTERDD
            | Opcode::VPSCATTERDQ
            | Opcode::VPSCATTERQD
            | Opcode::VPSCATTERQQ
            | Opcode::VSCATTERPF0DPD
            | Opcode::VSCATTERPF0DPS
            | Opcode::VSCATTERPF0QPD
            | Opcode::VSCATTERPF0QPS
            | Opcode::VSCATTERPF1DPD
            | Opcode::VSCATTERPF1DPS
            | Opcode::VSCATTERPF1QPD
            | Opcode::VSCATTERPF1QPS
            | Opcode::VINSERTF128
            | Opcode::VINSERTI128
            | Opcode::VINSERTPS
            | Opcode::INSERTPS
            | Opcode::VEXTRACTF32X4
            | Opcode::VEXTRACTF64X2
            | Opcode::VEXTRACTF64X4
            | Opcode::VEXTRACTI32X4
            | Opcode::VEXTRACTI64X2
            | Opcode::VEXTRACTI64X4
            | Opcode::VINSERTF32X4
            | Opcode::VINSERTF64X2
            | Opcode::VINSERTF64X4
            | Opcode::VINSERTI64X2
            | Opcode::VINSERTI64X4
            | Opcode::VSHUFF32X4
            | Opcode::VSHUFF64X2
            | Opcode::VSHUFI32X4
            | Opcode::VSHUFI64X2
            | Opcode::VMASKMOVDQU
            | Opcode::VMASKMOVPD
            | Opcode::VMASKMOVPS
            | Opcode::VMOVAPD
            | Opcode::VMOVAPS
            | Opcode::VMOVD
            | Opcode::VMOVDQA
            | Opcode::VMOVDQU
            | Opcode::VMOVHLPS
            | Opcode::VMOVHPD
            | Opcode::VMOVHPS
            | Opcode::VMOVLHPS
            | Opcode::VMOVLPD
            | Opcode::VMOVLPS
            | Opcode::VMOVMSKPD
            | Opcode::VMOVMSKPS
            | Opcode::VMOVNTDQ
            | Opcode::VMOVNTDQA
            | Opcode::VMOVNTPD
            | Opcode::VMOVNTPS
            | Opcode::MOVDIR64B
            | Opcode::MOVDIRI
            | Opcode::MOVNTDQA
            | Opcode::VMOVQ
            | Opcode::VMOVSHDUP
            | Opcode::VMOVSLDUP
            | Opcode::VMOVUPD
            | Opcode::VMOVUPS
            | Opcode::VMOVSD
            | Opcode::VMOVSS
            | Opcode::VMOVDQA32
            | Opcode::VMOVDQA64
            | Opcode::VMOVDQU32
            | Opcode::VMOVDQU64
            | Opcode::VPMOVM2B
            | Opcode::VPMOVM2W
            | Opcode::VPMOVB2M
            | Opcode::VPMOVW2M
            | Opcode::VPMOVSWB
            | Opcode::VPMOVUSWB
            | Opcode::VPMOVSQB
            | Opcode::VPMOVUSQB
            | Opcode::VPMOVSQW
            | Opcode::VPMOVUSQW
            | Opcode::VPMOVSQD
            | Opcode::VPMOVUSQD
            | Opcode::VPMOVSDB
            | Opcode::VPMOVUSDB
            | Opcode::VPMOVSDW
            | Opcode::VPMOVUSDW
            | Opcode::VPMOVM2D
            | Opcode::VPMOVM2Q
            | Opcode::VPMOVB2D
            | Opcode::VPMOVQ2M
            | Opcode::VMOVDQU8
            | Opcode::VMOVDQU16
            | Opcode::VPBLENDD
            | Opcode::VPBLENDVB
            | Opcode::VPBLENDW
            | Opcode::VPBROADCASTB
            | Opcode::VPBROADCASTD
            | Opcode::VPBROADCASTQ
            | Opcode::VPBROADCASTW
            | Opcode::VPGATHERDD
            | Opcode::VPGATHERDQ
            | Opcode::VPGATHERQD
            | Opcode::VPGATHERQQ
            | Opcode::VPCLMULQDQ
            | Opcode::VPMOVMSKB
            | Opcode::VPMOVSXBD
            | Opcode::VPMOVSXBQ
            | Opcode::VPMOVSXBW
            | Opcode::VPMOVSXDQ
            | Opcode::VPMOVSXWD
            | Opcode::VPMOVSXWQ
            | Opcode::VPMOVZXBD
            | Opcode::VPMOVZXBQ
            | Opcode::VPMOVZXBW
            | Opcode::VPMOVZXDQ
            | Opcode::VPMOVZXWD
            | Opcode::VPMOVZXWQ
            | Opcode::PMOVSXBD
            | Opcode::PMOVSXBQ
            | Opcode::PMOVSXBW
            | Opcode::PMOVSXDQ
            | Opcode::PMOVSXWD
            | Opcode::PMOVSXWQ
            | Opcode::PMOVZXBD
            | Opcode::PMOVZXBQ
            | Opcode::PMOVZXBW
            | Opcode::PMOVZXDQ
            | Opcode::PMOVZXWD
            | Opcode::PMOVZXWQ
            | Opcode::KUNPCKBW
            | Opcode::KUNPCKWD
            | Opcode::KUNPCKDQ
            | Opcode::VUNPCKHPD
            | Opcode::VUNPCKHPS
            | Opcode::VUNPCKLPD
            | Opcode::VUNPCKLPS
            | Opcode::VPUNPCKHBW
            | Opcode::VPUNPCKHDQ
            | Opcode::VPUNPCKHQDQ
            | Opcode::VPUNPCKHWD
            | Opcode::VPUNPCKLBW
            | Opcode::VPUNPCKLDQ
            | Opcode::VPUNPCKLQDQ
            | Opcode::VPUNPCKLWD
            | Opcode::VSHUFPD
            | Opcode::VSHUFPS
            | Opcode::VPACKSSDW
            | Opcode::VPACKUSDW
            | Opcode::PACKUSDW
            | Opcode::VPACKSSWB
            | Opcode::VPACKUSWB
            | Opcode::VALIGND
            | Opcode::VALIGNQ
            | Opcode::VPALIGNR
            | Opcode::PALIGNR
            | Opcode::VPERM2F128
            | Opcode::VPERM2I128
            | Opcode::VPERMD
            | Opcode::VPERMILPD
            | Opcode::VPERMILPS
            | Opcode::VPERMPD
            | Opcode::VPERMPS
            | Opcode::VPERMQ
            | Opcode::VPERMI2D
            | Opcode::VPERMI2Q
            | Opcode::VPERMI2PD
            | Opcode::VPERMI2PS
            | Opcode::VPERMT2D
            | Opcode::VPERMT2Q
            | Opcode::VPERMT2PD
            | Opcode::VPERMT2PS
            | Opcode::VPERMI2B
            | Opcode::VPERMI2W
            | Opcode::VPERMW
            | Opcode::VPEXTRB
            | Opcode::VPEXTRD
            | Opcode::VPEXTRQ
            | Opcode::VPEXTRW
            | Opcode::PEXTRB
            | Opcode::PEXTRD
            | Opcode::PEXTRQ
            | Opcode::EXTRQ
            | Opcode::PINSRB
            | Opcode::PINSRD
            | Opcode::PINSRQ
            | Opcode::INSERTQ
            | Opcode::VPINSRB
            | Opcode::VPINSRD
            | Opcode::VPINSRQ
            | Opcode::VPINSRW
            | Opcode::VPMASKMOVD
            | Opcode::VPMASKMOVQ
            | Opcode::VCOMPRESSPD
            | Opcode::VCOMPRESSPS
            | Opcode::VPCOMPRESSQ
            | Opcode::VPCOMPRESSD
            | Opcode::VEXPANDPD
            | Opcode::VEXPANDPS
            | Opcode::VPSHUFB
            | Opcode::VPSHUFD
            | Opcode::VPHMINPOSUW
            | Opcode::PHMINPOSUW
            | Opcode::VZEROUPPER
            | Opcode::VZEROALL
            | Opcode::VFIXUPIMMPD
            | Opcode::VFIXUPIMMPS
            | Opcode::VFIXUPIMMSD
            | Opcode::VFIXUPIMMSS
            | Opcode::VREDUCEPD
            | Opcode::VREDUCEPS
            | Opcode::VREDUCESD
            | Opcode::VREDUCESS
            | Opcode::VGETEXPPD
            | Opcode::VGETEXPPS
            | Opcode::VGETEXPSD
            | Opcode::VGETEXPSS
            | Opcode::VGETMANTPD
            | Opcode::VGETMANTPS
            | Opcode::VGETMANTSD
            | Opcode::VGETMANTSS
            | Opcode::VLDDQU
            | Opcode::BSWAP
            | Opcode::CVTDQ2PD
            | Opcode::CVTDQ2PS
            | Opcode::CVTPS2DQ
            | Opcode::CVTPD2DQ
            | Opcode::CVTPI2PS
            | Opcode::CVTPI2PD
            | Opcode::CVTPS2PD
            | Opcode::CVTPD2PS
            | Opcode::CVTPS2PI
            | Opcode::CVTPD2PI
            | Opcode::CVTSD2SI
            | Opcode::CVTSD2SS
            | Opcode::CVTSI2SD
            | Opcode::CVTSI2SS
            | Opcode::CVTSS2SD
            | Opcode::CVTSS2SI
            | Opcode::CVTTPD2DQ
            | Opcode::CVTTPS2DQ
            | Opcode::CVTTPS2PI
            | Opcode::CVTTPD2PI
            | Opcode::CVTTSD2SI
            | Opcode::CVTTSS2SI
            | Opcode::MASKMOVQ
            | Opcode::MASKMOVDQU
            | Opcode::MOVAPS
            | Opcode::MOVAPD
            | Opcode::MOVD
            | Opcode::MOVHPS
            | Opcode::MOVHPD
            | Opcode::MOVHLPS
            | Opcode::MOVLPS
            | Opcode::MOVLPD
            | Opcode::MOVLHPS
            | Opcode::MOVMSKPS
            | Opcode::MOVMSKPD
            | Opcode::MOVNTI
            | Opcode::MOVNTPS
            | Opcode::MOVNTPD
            | Opcode::MOVNTSS
            | Opcode::MOVNTSD
            | Opcode::MOVNTQ
            | Opcode::MOVNTDQ
            | Opcode::MOVSD
            | Opcode::MOVSS
            | Opcode::MOVUPD
            | Opcode::PSHUFHW
            | Opcode::PSHUFLW
            | Opcode::PUNPCKHBW
            | Opcode::PUNPCKHDQ
            | Opcode::PUNPCKHWD
            | Opcode::PUNPCKLBW
            | Opcode::PUNPCKLDQ
            | Opcode::PUNPCKLWD
            | Opcode::PUNPCKLQDQ
            | Opcode::PUNPCKHQDQ
            | Opcode::PACKSSDW
            | Opcode::PACKSSWB
            | Opcode::PACKUSWB
            | Opcode::UNPCKHPS
            | Opcode::UNPCKHPD
            | Opcode::UNPCKLPS
            | Opcode::UNPCKLPD
            | Opcode::SHUFPD
            | Opcode::SHUFPS
            | Opcode::PMOVMSKB
            | Opcode::KMOVB
            | Opcode::KMOVW
            | Opcode::KMOVD
            | Opcode::KMOVQ
            | Opcode::BNDMOV
            | Opcode::LDDQU
            | Opcode::CMC
            | Opcode::CLC
            | Opcode::CLI
            | Opcode::CLD
            | Opcode::STC
            | Opcode::STI
            | Opcode::STD
            | Opcode::CBW
            | Opcode::CWDE
            | Opcode::CDQE
            | Opcode::CWD
            | Opcode::CDQ
            | Opcode::CQO
            | Opcode::MOVDDUP
            | Opcode::MOVSLDUP
            | Opcode::MOVDQ2Q
            | Opcode::MOVDQU
            | Opcode::MOVDQA
            | Opcode::MOVQ
            | Opcode::MOVQ2DQ
            | Opcode::MOVSHDUP
            | Opcode::MOVUPS
            | Opcode::PEXTRW
            | Opcode::PINSRW
            | Opcode::MOV
            | Opcode::MOVBE
            | Opcode::LODS
            | Opcode::STOS
            | Opcode::LAHF
            | Opcode::SAHF
            | Opcode::MOVS
            | Opcode::INS
            | Opcode::IN
            | Opcode::OUTS
            | Opcode::OUT
            | Opcode::MOVZX
            | Opcode::MOVSX
            | Opcode::MOVSXD
            | Opcode::FILD
            | Opcode::FBLD
            | Opcode::FBSTP
            | Opcode::FIST
            | Opcode::FISTP
            | Opcode::FISTTP
            | Opcode::FLD
            | Opcode::FLD1
            | Opcode::FLDCW
            | Opcode::FLDENV
            | Opcode::FLDL2E
            | Opcode::FLDL2T
            | Opcode::FLDLG2
            | Opcode::FLDLN2
            | Opcode::FLDPI
            | Opcode::FLDZ
            | Opcode::FST
            | Opcode::FSTP
            | Opcode::FSTPNCE
            | Opcode::FNSAVE
            | Opcode::FNSTCW
            | Opcode::FNSTENV
            | Opcode::FNSTOR
            | Opcode::FNSTSW
            | Opcode::FRSTOR
            | Opcode::FXCH
            | Opcode::XCHG
            | Opcode::XLAT
            | Opcode::CMOVA
            | Opcode::CMOVB
            | Opcode::CMOVG
            | Opcode::CMOVGE
            | Opcode::CMOVL
            | Opcode::CMOVLE
            | Opcode::CMOVNA
            | Opcode::CMOVNB
            | Opcode::CMOVNO
            | Opcode::CMOVNP
            | Opcode::CMOVNS
            | Opcode::CMOVNZ
            | Opcode::CMOVO
            | Opcode::CMOVP
            | Opcode::CMOVS
            | Opcode::CMOVZ
            | Opcode::FCMOVB
            | Opcode::FCMOVBE
            | Opcode::FCMOVE
            | Opcode::FCMOVNB
            | Opcode::FCMOVNBE
            | Opcode::FCMOVNE
            | Opcode::FCMOVNU
            | Opcode::FCMOVU
            | Opcode::SALC
            | Opcode::SETO
            | Opcode::SETNO
            | Opcode::SETB
            | Opcode::SETAE
            | Opcode::SETZ
            | Opcode::SETNZ
            | Opcode::SETBE
            | Opcode::SETA
            | Opcode::SETS
            | Opcode::SETNS
            | Opcode::SETP
            | Opcode::SETNP
            | Opcode::SETL
            | Opcode::SETGE
            | Opcode::SETLE
            | Opcode::SETG => {
                write!(out, "{}", colors.data_op(self))
            }

            Opcode::VCOMISD
            | Opcode::VCOMISS
            | Opcode::VUCOMISD
            | Opcode::VUCOMISS
            | Opcode::KORTESTB
            | Opcode::KTESTB
            | Opcode::KORTESTW
            | Opcode::KTESTW
            | Opcode::KORTESTD
            | Opcode::KTESTD
            | Opcode::KORTESTQ
            | Opcode::KTESTQ
            | Opcode::VPTESTNMD
            | Opcode::VPTESTNMQ
            | Opcode::VPTERNLOGD
            | Opcode::VPTERNLOGQ
            | Opcode::VPTESTMD
            | Opcode::VPTESTMQ
            | Opcode::VPTESTNMB
            | Opcode::VPTESTNMW
            | Opcode::VPTESTMB
            | Opcode::VPTESTMW
            | Opcode::VPCMPD
            | Opcode::VPCMPUD
            | Opcode::VPCMPQ
            | Opcode::VPCMPUQ
            | Opcode::VPCMPB
            | Opcode::VPCMPUB
            | Opcode::VPCMPW
            | Opcode::VPCMPUW
            | Opcode::VCMPPD
            | Opcode::VCMPPS
            | Opcode::VCMPSD
            | Opcode::VCMPSS
            | Opcode::VMAXPD
            | Opcode::VMAXPS
            | Opcode::VMAXSD
            | Opcode::VMAXSS
            | Opcode::VPMAXSQ
            | Opcode::VPMAXUQ
            | Opcode::VPMINSQ
            | Opcode::VPMINUQ
            | Opcode::VMINPD
            | Opcode::VMINPS
            | Opcode::VMINSD
            | Opcode::VMINSS
            | Opcode::VPCMPEQB
            | Opcode::VPCMPEQD
            | Opcode::VPCMPEQQ
            | Opcode::VPCMPEQW
            | Opcode::VPCMPGTB
            | Opcode::VPCMPGTD
            | Opcode::VPCMPGTQ
            | Opcode::VPCMPGTW
            | Opcode::VPCMPESTRI
            | Opcode::VPCMPESTRM
            | Opcode::VPCMPISTRI
            | Opcode::VPCMPISTRM
            | Opcode::VPMAXSB
            | Opcode::VPMAXSD
            | Opcode::VPMAXSW
            | Opcode::VPMAXUB
            | Opcode::VPMAXUW
            | Opcode::VPMAXUD
            | Opcode::VPMINSB
            | Opcode::VPMINSW
            | Opcode::VPMINSD
            | Opcode::VPMINUB
            | Opcode::VPMINUW
            | Opcode::VPMINUD
            | Opcode::VFPCLASSPD
            | Opcode::VFPCLASSPS
            | Opcode::VFPCLASSSD
            | Opcode::VFPCLASSSS
            | Opcode::VRANGEPD
            | Opcode::VRANGEPS
            | Opcode::VRANGESD
            | Opcode::VRANGESS
            | Opcode::VPCONFLICTD
            | Opcode::VPCONFLICTQ
            | Opcode::VPTEST
            | Opcode::VTESTPD
            | Opcode::VTESTPS
            | Opcode::PCMPEQB
            | Opcode::PCMPEQD
            | Opcode::PCMPEQQ
            | Opcode::PCMPEQW
            | Opcode::PCMPESTRI
            | Opcode::PCMPESTRM
            | Opcode::PCMPGTB
            | Opcode::PCMPGTD
            | Opcode::PCMPGTQ
            | Opcode::PCMPGTW
            | Opcode::PCMPISTRI
            | Opcode::PCMPISTRM
            | Opcode::PTEST
            | Opcode::MAXPD
            | Opcode::MAXPS
            | Opcode::MAXSD
            | Opcode::MAXSS
            | Opcode::MINPD
            | Opcode::MINPS
            | Opcode::MINSD
            | Opcode::MINSS
            | Opcode::PMAXSB
            | Opcode::PMAXSD
            | Opcode::PMAXSW
            | Opcode::PMAXUB
            | Opcode::PMAXUD
            | Opcode::PMAXUW
            | Opcode::PMINSB
            | Opcode::PMINSD
            | Opcode::PMINSW
            | Opcode::PMINUB
            | Opcode::PMINUD
            | Opcode::PMINUW
            | Opcode::PFCMPGE
            | Opcode::PFMIN
            | Opcode::PFCMPGT
            | Opcode::PFMAX
            | Opcode::PFCMPEQ
            | Opcode::CMPS
            | Opcode::SCAS
            | Opcode::TEST
            | Opcode::FTST
            | Opcode::FXAM
            | Opcode::FUCOM
            | Opcode::FUCOMI
            | Opcode::FUCOMIP
            | Opcode::FUCOMP
            | Opcode::FUCOMPP
            | Opcode::FCOM
            | Opcode::FCOMI
            | Opcode::FCOMIP
            | Opcode::FCOMP
            | Opcode::FCOMPP
            | Opcode::FICOM
            | Opcode::FICOMP
            | Opcode::CMPSD
            | Opcode::CMPSS
            | Opcode::CMP
            | Opcode::CMPPS
            | Opcode::CMPPD
            | Opcode::CMPXCHG8B
            | Opcode::CMPXCHG16B
            | Opcode::CMPXCHG => {
                write!(out, "{}", colors.comparison_op(self))
            }

            Opcode::WRMSR
            | Opcode::RDMSR
            | Opcode::RDTSC
            | Opcode::RDPMC
            | Opcode::RDPID
            | Opcode::RDFSBASE
            | Opcode::RDGSBASE
            | Opcode::WRFSBASE
            | Opcode::WRGSBASE
            | Opcode::FXSAVE
            | Opcode::FXRSTOR
            | Opcode::LDMXCSR
            | Opcode::STMXCSR
            | Opcode::VLDMXCSR
            | Opcode::VSTMXCSR
            | Opcode::XSAVE
            | Opcode::XSAVEC
            | Opcode::XSAVES
            | Opcode::XSAVEC64
            | Opcode::XSAVES64
            | Opcode::XRSTOR
            | Opcode::XRSTORS
            | Opcode::XRSTORS64
            | Opcode::XSAVEOPT
            | Opcode::LFENCE
            | Opcode::MFENCE
            | Opcode::SFENCE
            | Opcode::CLFLUSH
            | Opcode::CLFLUSHOPT
            | Opcode::CLWB
            | Opcode::LDS
            | Opcode::LES
            | Opcode::SGDT
            | Opcode::SIDT
            | Opcode::LGDT
            | Opcode::LIDT
            | Opcode::SMSW
            | Opcode::LMSW
            | Opcode::SWAPGS
            | Opcode::RDTSCP
            | Opcode::INVEPT
            | Opcode::INVVPID
            | Opcode::INVPCID
            | Opcode::INVLPG
            | Opcode::INVLPGA
            | Opcode::INVLPGB
            | Opcode::TLBSYNC
            | Opcode::PSMASH
            | Opcode::PVALIDATE
            | Opcode::RMPADJUST
            | Opcode::RMPUPDATE
            | Opcode::CPUID
            | Opcode::WBINVD
            | Opcode::INVD
            | Opcode::SYSRET
            | Opcode::CLTS
            | Opcode::SYSCALL
            | Opcode::TDCALL
            | Opcode::SEAMRET
            | Opcode::SEAMOPS
            | Opcode::SEAMCALL
            | Opcode::TPAUSE
            | Opcode::UMONITOR
            | Opcode::UMWAIT
            | Opcode::LSL
            | Opcode::SLDT
            | Opcode::STR
            | Opcode::LLDT
            | Opcode::LTR
            | Opcode::VERR
            | Opcode::VERW
            | Opcode::JMPE
            | Opcode::EMMS
            | Opcode::FEMMS
            | Opcode::GETSEC
            | Opcode::LFS
            | Opcode::LGS
            | Opcode::LSS
            | Opcode::RSM
            | Opcode::SYSENTER
            | Opcode::SYSEXIT
            | Opcode::VMREAD
            | Opcode::VMWRITE
            | Opcode::VMCLEAR
            | Opcode::VMPTRLD
            | Opcode::VMPTRST
            | Opcode::VMXON
            | Opcode::VMCALL
            | Opcode::VMLAUNCH
            | Opcode::VMRESUME
            | Opcode::VMLOAD
            | Opcode::VMMCALL
            | Opcode::VMSAVE
            | Opcode::VMRUN
            | Opcode::VMXOFF
            | Opcode::PCONFIG
            | Opcode::MONITOR
            | Opcode::MWAIT
            | Opcode::MONITORX
            | Opcode::MWAITX
            | Opcode::SKINIT
            | Opcode::CLGI
            | Opcode::STGI
            | Opcode::CLAC
            | Opcode::STAC
            | Opcode::ENCLS
            | Opcode::ENCLV
            | Opcode::XGETBV
            | Opcode::XSETBV
            | Opcode::VMFUNC
            | Opcode::XEND
            | Opcode::XTEST
            | Opcode::XABORT
            | Opcode::XBEGIN
            | Opcode::ENCLU
            | Opcode::RDPKRU
            | Opcode::WRPKRU
            | Opcode::RDPRU
            | Opcode::CLZERO
            | Opcode::ENQCMD
            | Opcode::ENQCMDS
            | Opcode::PTWRITE
            | Opcode::UIRET
            | Opcode::TESTUI
            | Opcode::CLUI
            | Opcode::STUI
            | Opcode::SENDUIPI
            | Opcode::XSUSLDTRK
            | Opcode::XRESLDTRK
            | Opcode::BOUND
            | Opcode::ARPL
            | Opcode::BNDMK
            | Opcode::BNDCL
            | Opcode::BNDCU
            | Opcode::BNDCN
            | Opcode::BNDLDX
            | Opcode::BNDSTX
            | Opcode::LAR => {
                write!(out, "{}", colors.platform_op(self))
            }

            Opcode::CRC32
            | Opcode::RDSEED
            | Opcode::RDRAND
            | Opcode::SHA1RNDS4
            | Opcode::SHA1NEXTE
            | Opcode::SHA1MSG1
            | Opcode::SHA1MSG2
            | Opcode::SHA256RNDS2
            | Opcode::SHA256MSG1
            | Opcode::SHA256MSG2
            | Opcode::FFREE
            | Opcode::FFREEP
            | Opcode::FDECSTP
            | Opcode::FINCSTP
            | Opcode::GF2P8MULB
            | Opcode::GF2P8AFFINEQB
            | Opcode::GF2P8AFFINEINVQB
            | Opcode::AESDEC128KL
            | Opcode::AESDEC256KL
            | Opcode::AESDECWIDE128KL
            | Opcode::AESDECWIDE256KL
            | Opcode::AESENC128KL
            | Opcode::AESENC256KL
            | Opcode::AESENCWIDE128KL
            | Opcode::AESENCWIDE256KL
            | Opcode::ENCODEKEY128
            | Opcode::ENCODEKEY256
            | Opcode::LOADIWKEY
            | Opcode::HRESET
            | Opcode::WRUSS
            | Opcode::WRSS
            | Opcode::INCSSP
            | Opcode::SAVEPREVSSP
            | Opcode::SETSSBSY
            | Opcode::CLRSSBSY
            | Opcode::RSTORSSP
            | Opcode::ENDBR64
            | Opcode::ENDBR32
            | Opcode::AESDEC
            | Opcode::AESDECLAST
            | Opcode::AESENC
            | Opcode::AESENCLAST
            | Opcode::AESIMC
            | Opcode::AESKEYGENASSIST
            | Opcode::VAESDEC
            | Opcode::VAESDECLAST
            | Opcode::VAESENC
            | Opcode::VAESENCLAST
            | Opcode::VAESIMC
            | Opcode::VAESKEYGENASSIST => {
                write!(out, "{}", colors.misc_op(self))
            }

            Opcode::UD0 | Opcode::UD1 | Opcode::UD2 | Opcode::Invalid => {
                write!(out, "{}", colors.invalid_op(self))
            }
        }
    }
}
