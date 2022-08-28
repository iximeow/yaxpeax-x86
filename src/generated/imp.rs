use yaxpeax_arch::{Colorize, YaxColors};
use core::fmt;
use crate::generated::opcode::Opcode;

impl fmt::Display for Opcode {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(self.name())
  }
}

impl<T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Opcode {
  fn colorize(&self, colors: &Y, out: &mut T) -> fmt::Result {
    match self {
      Opcode::ARPL |
      Opcode::BNDCL |
      Opcode::BNDCN |
      Opcode::BNDCU |
      Opcode::BNDLDX |
      Opcode::BNDMK |
      Opcode::BNDSTX |
      Opcode::BOUND |
      Opcode::CLAC |
      Opcode::CLFLUSH |
      Opcode::CLFLUSHOPT |
      Opcode::CLGI |
      Opcode::CLTS |
      Opcode::CLUI |
      Opcode::CLWB |
      Opcode::CLZERO |
      Opcode::CPUID |
      Opcode::EMMS |
      Opcode::ENCLS |
      Opcode::ENCLU |
      Opcode::ENCLV |
      Opcode::ENQCMD |
      Opcode::ENQCMDS |
      Opcode::FEMMS |
      Opcode::FXRSTOR |
      Opcode::FXSAVE |
      Opcode::GETSEC |
      Opcode::INVD |
      Opcode::INVEPT |
      Opcode::INVLPG |
      Opcode::INVLPGA |
      Opcode::INVLPGB |
      Opcode::INVPCID |
      Opcode::INVVPID |
      Opcode::JMPE |
      Opcode::LAR |
      Opcode::LDMXCSR |
      Opcode::LDS |
      Opcode::LES |
      Opcode::LFENCE |
      Opcode::LFS |
      Opcode::LGDT |
      Opcode::LGS |
      Opcode::LIDT |
      Opcode::LLDT |
      Opcode::LMSW |
      Opcode::LSL |
      Opcode::LSS |
      Opcode::LTR |
      Opcode::MFENCE |
      Opcode::MONITOR |
      Opcode::MONITORX |
      Opcode::MWAIT |
      Opcode::MWAITX |
      Opcode::PCONFIG |
      Opcode::PSMASH |
      Opcode::PTWRITE |
      Opcode::PVALIDATE |
      Opcode::RDFSBASE |
      Opcode::RDGSBASE |
      Opcode::RDMSR |
      Opcode::RDPID |
      Opcode::RDPKRU |
      Opcode::RDPMC |
      Opcode::RDPRU |
      Opcode::RDTSC |
      Opcode::RDTSCP |
      Opcode::RMPADJUST |
      Opcode::RMPUPDATE |
      Opcode::RSM |
      Opcode::SEAMCALL |
      Opcode::SEAMOPS |
      Opcode::SEAMRET |
      Opcode::SENDUIPI |
      Opcode::SFENCE |
      Opcode::SGDT |
      Opcode::SIDT |
      Opcode::SKINIT |
      Opcode::SLDT |
      Opcode::SMSW |
      Opcode::STAC |
      Opcode::STGI |
      Opcode::STMXCSR |
      Opcode::STR |
      Opcode::STUI |
      Opcode::SWAPGS |
      Opcode::SYSCALL |
      Opcode::SYSENTER |
      Opcode::SYSEXIT |
      Opcode::SYSRET |
      Opcode::TDCALL |
      Opcode::TESTUI |
      Opcode::TLBSYNC |
      Opcode::TPAUSE |
      Opcode::UIRET |
      Opcode::UMONITOR |
      Opcode::UMWAIT |
      Opcode::VERR |
      Opcode::VERW |
      Opcode::VLDMXCSR |
      Opcode::VMCALL |
      Opcode::VMCLEAR |
      Opcode::VMFUNC |
      Opcode::VMLAUNCH |
      Opcode::VMLOAD |
      Opcode::VMMCALL |
      Opcode::VMPTRLD |
      Opcode::VMPTRST |
      Opcode::VMREAD |
      Opcode::VMRESUME |
      Opcode::VMRUN |
      Opcode::VMSAVE |
      Opcode::VMWRITE |
      Opcode::VMXOFF |
      Opcode::VMXON |
      Opcode::VSTMXCSR |
      Opcode::WBINVD |
      Opcode::WRFSBASE |
      Opcode::WRGSBASE |
      Opcode::WRMSR |
      Opcode::WRPKRU |
      Opcode::XABORT |
      Opcode::XBEGIN |
      Opcode::XEND |
      Opcode::XGETBV |
      Opcode::XRESLDTRK |
      Opcode::XRSTOR |
      Opcode::XRSTORS |
      Opcode::XRSTORS64 |
      Opcode::XSAVE |
      Opcode::XSAVEC |
      Opcode::XSAVEC64 |
      Opcode::XSAVEOPT |
      Opcode::XSAVES |
      Opcode::XSAVES64 |
      Opcode::XSETBV |
      Opcode::XSUSLDTRK |
      Opcode::XTEST => {
        write!(out, "{}", colors.platform_op(self))
      }
      Opcode::FDISI8087_NOP |
      Opcode::FENI8087_NOP |
      Opcode::FNOP |
      Opcode::FSETPM287_NOP |
      Opcode::NOP |
      Opcode::PREFETCH0 |
      Opcode::PREFETCH1 |
      Opcode::PREFETCH2 |
      Opcode::PREFETCHNTA |
      Opcode::PREFETCHW |
      Opcode::WAIT => {
        write!(out, "{}", colors.nop_op(self))
      }
      Opcode::ENTER |
      Opcode::LEAVE |
      Opcode::POP |
      Opcode::POPA |
      Opcode::POPF |
      Opcode::PUSH |
      Opcode::PUSHA |
      Opcode::PUSHF => {
        write!(out, "{}", colors.stack_op(self))
      }
      Opcode::BLENDPD |
      Opcode::BLENDPS |
      Opcode::BLENDVPD |
      Opcode::BLENDVPS |
      Opcode::BLENDW |
      Opcode::BNDMOV |
      Opcode::BSWAP |
      Opcode::CBW |
      Opcode::CDQ |
      Opcode::CDQE |
      Opcode::CLC |
      Opcode::CLD |
      Opcode::CLI |
      Opcode::CMC |
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
      Opcode::CQO |
      Opcode::CVTDQ2PD |
      Opcode::CVTDQ2PS |
      Opcode::CVTPD2DQ |
      Opcode::CVTPD2PI |
      Opcode::CVTPD2PS |
      Opcode::CVTPI2PD |
      Opcode::CVTPI2PS |
      Opcode::CVTPS2DQ |
      Opcode::CVTPS2PD |
      Opcode::CVTPS2PI |
      Opcode::CVTSD2SI |
      Opcode::CVTSD2SS |
      Opcode::CVTSI2SD |
      Opcode::CVTSI2SS |
      Opcode::CVTSS2SD |
      Opcode::CVTSS2SI |
      Opcode::CVTTPD2DQ |
      Opcode::CVTTPD2PI |
      Opcode::CVTTPS2DQ |
      Opcode::CVTTPS2PI |
      Opcode::CVTTSD2SI |
      Opcode::CVTTSS2SI |
      Opcode::CWD |
      Opcode::CWDE |
      Opcode::EXTRACTPS |
      Opcode::EXTRQ |
      Opcode::FBLD |
      Opcode::FBSTP |
      Opcode::FCMOVB |
      Opcode::FCMOVBE |
      Opcode::FCMOVE |
      Opcode::FCMOVNB |
      Opcode::FCMOVNBE |
      Opcode::FCMOVNE |
      Opcode::FCMOVNU |
      Opcode::FCMOVU |
      Opcode::FILD |
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
      Opcode::FNSAVE |
      Opcode::FNSTCW |
      Opcode::FNSTENV |
      Opcode::FNSTOR |
      Opcode::FNSTSW |
      Opcode::FRSTOR |
      Opcode::FST |
      Opcode::FSTP |
      Opcode::FSTPNCE |
      Opcode::FXCH |
      Opcode::IN |
      Opcode::INS |
      Opcode::INSERTPS |
      Opcode::INSERTQ |
      Opcode::KMOVB |
      Opcode::KMOVD |
      Opcode::KMOVQ |
      Opcode::KMOVW |
      Opcode::KUNPCKBW |
      Opcode::KUNPCKDQ |
      Opcode::KUNPCKWD |
      Opcode::LAHF |
      Opcode::LDDQU |
      Opcode::LODS |
      Opcode::MASKMOVDQU |
      Opcode::MASKMOVQ |
      Opcode::MOV |
      Opcode::MOVAPD |
      Opcode::MOVAPS |
      Opcode::MOVBE |
      Opcode::MOVD |
      Opcode::MOVDDUP |
      Opcode::MOVDIR64B |
      Opcode::MOVDIRI |
      Opcode::MOVDQ2Q |
      Opcode::MOVDQA |
      Opcode::MOVDQU |
      Opcode::MOVHLPS |
      Opcode::MOVHPD |
      Opcode::MOVHPS |
      Opcode::MOVLHPS |
      Opcode::MOVLPD |
      Opcode::MOVLPS |
      Opcode::MOVMSKPD |
      Opcode::MOVMSKPS |
      Opcode::MOVNTDQ |
      Opcode::MOVNTDQA |
      Opcode::MOVNTI |
      Opcode::MOVNTPD |
      Opcode::MOVNTPS |
      Opcode::MOVNTQ |
      Opcode::MOVNTSD |
      Opcode::MOVNTSS |
      Opcode::MOVQ |
      Opcode::MOVQ2DQ |
      Opcode::MOVS |
      Opcode::MOVSD |
      Opcode::MOVSHDUP |
      Opcode::MOVSLDUP |
      Opcode::MOVSS |
      Opcode::MOVSX |
      Opcode::MOVSXD |
      Opcode::MOVUPD |
      Opcode::MOVUPS |
      Opcode::MOVZX |
      Opcode::OUT |
      Opcode::OUTS |
      Opcode::PACKSSDW |
      Opcode::PACKSSWB |
      Opcode::PACKUSDW |
      Opcode::PACKUSWB |
      Opcode::PALIGNR |
      Opcode::PBLENDVB |
      Opcode::PBLENDW |
      Opcode::PEXTRB |
      Opcode::PEXTRD |
      Opcode::PEXTRQ |
      Opcode::PEXTRW |
      Opcode::PF2ID |
      Opcode::PF2IW |
      Opcode::PHMINPOSUW |
      Opcode::PI2FD |
      Opcode::PI2FW |
      Opcode::PINSRB |
      Opcode::PINSRD |
      Opcode::PINSRQ |
      Opcode::PINSRW |
      Opcode::PMOVMSKB |
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
      Opcode::PSHUFHW |
      Opcode::PSHUFLW |
      Opcode::PUNPCKHBW |
      Opcode::PUNPCKHDQ |
      Opcode::PUNPCKHQDQ |
      Opcode::PUNPCKHWD |
      Opcode::PUNPCKLBW |
      Opcode::PUNPCKLDQ |
      Opcode::PUNPCKLQDQ |
      Opcode::PUNPCKLWD |
      Opcode::SAHF |
      Opcode::SALC |
      Opcode::SETA |
      Opcode::SETAE |
      Opcode::SETB |
      Opcode::SETBE |
      Opcode::SETG |
      Opcode::SETGE |
      Opcode::SETL |
      Opcode::SETLE |
      Opcode::SETNO |
      Opcode::SETNP |
      Opcode::SETNS |
      Opcode::SETNZ |
      Opcode::SETO |
      Opcode::SETP |
      Opcode::SETS |
      Opcode::SETZ |
      Opcode::SHUFPD |
      Opcode::SHUFPS |
      Opcode::STC |
      Opcode::STD |
      Opcode::STI |
      Opcode::STOS |
      Opcode::UNPCKHPD |
      Opcode::UNPCKHPS |
      Opcode::UNPCKLPD |
      Opcode::UNPCKLPS |
      Opcode::VALIGND |
      Opcode::VALIGNQ |
      Opcode::VBLENDMPD |
      Opcode::VBLENDMPS |
      Opcode::VBLENDPD |
      Opcode::VBLENDPS |
      Opcode::VBLENDVPD |
      Opcode::VBLENDVPS |
      Opcode::VBROADCASTF128 |
      Opcode::VBROADCASTI128 |
      Opcode::VBROADCASTSD |
      Opcode::VBROADCASTSS |
      Opcode::VCOMPRESSD |
      Opcode::VCOMPRESSPD |
      Opcode::VCOMPRESSPS |
      Opcode::VCOMPRESSQ |
      Opcode::VCVTDQ2PD |
      Opcode::VCVTDQ2PS |
      Opcode::VCVTPD2DQ |
      Opcode::VCVTPD2PS |
      Opcode::VCVTPD2QQ |
      Opcode::VCVTPD2UDQ |
      Opcode::VCVTPD2UQQ |
      Opcode::VCVTPH2PS |
      Opcode::VCVTPS2DQ |
      Opcode::VCVTPS2PD |
      Opcode::VCVTPS2PH |
      Opcode::VCVTPS2QQ |
      Opcode::VCVTPS2UDQ |
      Opcode::VCVTPS2UQQ |
      Opcode::VCVTQQ2PD |
      Opcode::VCVTQQ2PS |
      Opcode::VCVTSD2SI |
      Opcode::VCVTSD2SS |
      Opcode::VCVTSD2USI |
      Opcode::VCVTSI2SD |
      Opcode::VCVTSI2SS |
      Opcode::VCVTSS2SD |
      Opcode::VCVTSS2SI |
      Opcode::VCVTSS2USI |
      Opcode::VCVTTPD2DQ |
      Opcode::VCVTTPD2QQ |
      Opcode::VCVTTPD2UDQ |
      Opcode::VCVTTPD2UQQ |
      Opcode::VCVTTPS2DQ |
      Opcode::VCVTTPS2QQ |
      Opcode::VCVTTPS2UDQ |
      Opcode::VCVTTPS2UQQ |
      Opcode::VCVTTSD2SI |
      Opcode::VCVTTSD2USI |
      Opcode::VCVTTSS2SI |
      Opcode::VCVTTSS2USI |
      Opcode::VCVTUDQ2PD |
      Opcode::VCVTUDQ2PS |
      Opcode::VCVTUQQ2PD |
      Opcode::VCVTUQQ2PS |
      Opcode::VCVTUSI2USD |
      Opcode::VCVTUSI2USS |
      Opcode::VEXPANDPD |
      Opcode::VEXPANDPS |
      Opcode::VEXTRACTF128 |
      Opcode::VEXTRACTF32X4 |
      Opcode::VEXTRACTF64X2 |
      Opcode::VEXTRACTF64X4 |
      Opcode::VEXTRACTI128 |
      Opcode::VEXTRACTI32X4 |
      Opcode::VEXTRACTI64X2 |
      Opcode::VEXTRACTI64X4 |
      Opcode::VEXTRACTPS |
      Opcode::VFIXUPIMMPD |
      Opcode::VFIXUPIMMPS |
      Opcode::VFIXUPIMMSD |
      Opcode::VFIXUPIMMSS |
      Opcode::VGATHERDPD |
      Opcode::VGATHERDPS |
      Opcode::VGATHERPF0DPD |
      Opcode::VGATHERPF0DPS |
      Opcode::VGATHERPF0QPD |
      Opcode::VGATHERPF0QPS |
      Opcode::VGATHERPF1DPD |
      Opcode::VGATHERPF1DPS |
      Opcode::VGATHERPF1QPD |
      Opcode::VGATHERPF1QPS |
      Opcode::VGATHERQPD |
      Opcode::VGATHERQPS |
      Opcode::VGETEXPPD |
      Opcode::VGETEXPPS |
      Opcode::VGETEXPSD |
      Opcode::VGETEXPSS |
      Opcode::VGETMANTPD |
      Opcode::VGETMANTPS |
      Opcode::VGETMANTSD |
      Opcode::VGETMANTSS |
      Opcode::VINSERTF128 |
      Opcode::VINSERTF32X4 |
      Opcode::VINSERTF64X2 |
      Opcode::VINSERTF64X4 |
      Opcode::VINSERTI128 |
      Opcode::VINSERTI64X2 |
      Opcode::VINSERTI64X4 |
      Opcode::VINSERTPS |
      Opcode::VLDDQU |
      Opcode::VMASKMOVDQU |
      Opcode::VMASKMOVPD |
      Opcode::VMASKMOVPS |
      Opcode::VMOVAPD |
      Opcode::VMOVAPS |
      Opcode::VMOVD |
      Opcode::VMOVDDUP |
      Opcode::VMOVDQA |
      Opcode::VMOVDQA32 |
      Opcode::VMOVDQA64 |
      Opcode::VMOVDQU |
      Opcode::VMOVDQU16 |
      Opcode::VMOVDQU32 |
      Opcode::VMOVDQU64 |
      Opcode::VMOVDQU8 |
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
      Opcode::VMOVQ |
      Opcode::VMOVSD |
      Opcode::VMOVSHDUP |
      Opcode::VMOVSLDUP |
      Opcode::VMOVSS |
      Opcode::VMOVUPD |
      Opcode::VMOVUPS |
      Opcode::VPACKSSDW |
      Opcode::VPACKSSWB |
      Opcode::VPACKUSDW |
      Opcode::VPACKUSWB |
      Opcode::VPALIGNR |
      Opcode::VPBLENDD |
      Opcode::VPBLENDMB |
      Opcode::VPBLENDMD |
      Opcode::VPBLENDMQ |
      Opcode::VPBLENDMW |
      Opcode::VPBLENDVB |
      Opcode::VPBLENDW |
      Opcode::VPBROADCASTB |
      Opcode::VPBROADCASTD |
      Opcode::VPBROADCASTM |
      Opcode::VPBROADCASTQ |
      Opcode::VPBROADCASTW |
      Opcode::VPCLMULQDQ |
      Opcode::VPCOMPRESSD |
      Opcode::VPCOMPRESSQ |
      Opcode::VPERM2F128 |
      Opcode::VPERM2I128 |
      Opcode::VPERMD |
      Opcode::VPERMI2B |
      Opcode::VPERMI2D |
      Opcode::VPERMI2PD |
      Opcode::VPERMI2PS |
      Opcode::VPERMI2Q |
      Opcode::VPERMI2W |
      Opcode::VPERMILPD |
      Opcode::VPERMILPS |
      Opcode::VPERMPD |
      Opcode::VPERMPS |
      Opcode::VPERMQ |
      Opcode::VPERMT2D |
      Opcode::VPERMT2PD |
      Opcode::VPERMT2PS |
      Opcode::VPERMT2Q |
      Opcode::VPERMW |
      Opcode::VPEXTRB |
      Opcode::VPEXTRD |
      Opcode::VPEXTRQ |
      Opcode::VPEXTRW |
      Opcode::VPGATHERDD |
      Opcode::VPGATHERDQ |
      Opcode::VPGATHERQD |
      Opcode::VPGATHERQQ |
      Opcode::VPHMINPOSUW |
      Opcode::VPINSRB |
      Opcode::VPINSRD |
      Opcode::VPINSRQ |
      Opcode::VPINSRW |
      Opcode::VPMASKMOVD |
      Opcode::VPMASKMOVQ |
      Opcode::VPMOVB2D |
      Opcode::VPMOVB2M |
      Opcode::VPMOVM2B |
      Opcode::VPMOVM2D |
      Opcode::VPMOVM2Q |
      Opcode::VPMOVM2W |
      Opcode::VPMOVMSKB |
      Opcode::VPMOVQ2M |
      Opcode::VPMOVSDB |
      Opcode::VPMOVSDW |
      Opcode::VPMOVSQB |
      Opcode::VPMOVSQD |
      Opcode::VPMOVSQW |
      Opcode::VPMOVSWB |
      Opcode::VPMOVSXBD |
      Opcode::VPMOVSXBQ |
      Opcode::VPMOVSXBW |
      Opcode::VPMOVSXDQ |
      Opcode::VPMOVSXWD |
      Opcode::VPMOVSXWQ |
      Opcode::VPMOVUSDB |
      Opcode::VPMOVUSDW |
      Opcode::VPMOVUSQB |
      Opcode::VPMOVUSQD |
      Opcode::VPMOVUSQW |
      Opcode::VPMOVUSWB |
      Opcode::VPMOVW2M |
      Opcode::VPMOVZXBD |
      Opcode::VPMOVZXBQ |
      Opcode::VPMOVZXBW |
      Opcode::VPMOVZXDQ |
      Opcode::VPMOVZXWD |
      Opcode::VPMOVZXWQ |
      Opcode::VPSCATTERDD |
      Opcode::VPSCATTERDQ |
      Opcode::VPSCATTERQD |
      Opcode::VPSCATTERQQ |
      Opcode::VPSHUFB |
      Opcode::VPSHUFD |
      Opcode::VPSHUFHW |
      Opcode::VPSHUFLW |
      Opcode::VPUNPCKHBW |
      Opcode::VPUNPCKHDQ |
      Opcode::VPUNPCKHQDQ |
      Opcode::VPUNPCKHWD |
      Opcode::VPUNPCKLBW |
      Opcode::VPUNPCKLDQ |
      Opcode::VPUNPCKLQDQ |
      Opcode::VPUNPCKLWD |
      Opcode::VREDUCEPD |
      Opcode::VREDUCEPS |
      Opcode::VREDUCESD |
      Opcode::VREDUCESS |
      Opcode::VSCATTERDD |
      Opcode::VSCATTERDPD |
      Opcode::VSCATTERDPS |
      Opcode::VSCATTERDQ |
      Opcode::VSCATTERPF0DPD |
      Opcode::VSCATTERPF0DPS |
      Opcode::VSCATTERPF0QPD |
      Opcode::VSCATTERPF0QPS |
      Opcode::VSCATTERPF1DPD |
      Opcode::VSCATTERPF1DPS |
      Opcode::VSCATTERPF1QPD |
      Opcode::VSCATTERPF1QPS |
      Opcode::VSCATTERQD |
      Opcode::VSCATTERQPD |
      Opcode::VSCATTERQPS |
      Opcode::VSCATTERQQ |
      Opcode::VSHUFF32X4 |
      Opcode::VSHUFF64X2 |
      Opcode::VSHUFI32X4 |
      Opcode::VSHUFI64X2 |
      Opcode::VSHUFPD |
      Opcode::VSHUFPS |
      Opcode::VUNPCKHPD |
      Opcode::VUNPCKHPS |
      Opcode::VUNPCKLPD |
      Opcode::VUNPCKLPS |
      Opcode::VZEROALL |
      Opcode::VZEROUPPER |
      Opcode::XCHG |
      Opcode::XLAT => {
        write!(out, "{}", colors.data_op(self))
      }
      Opcode::CMP |
      Opcode::CMPPD |
      Opcode::CMPPS |
      Opcode::CMPS |
      Opcode::CMPSD |
      Opcode::CMPSS |
      Opcode::CMPXCHG |
      Opcode::CMPXCHG16B |
      Opcode::CMPXCHG8B |
      Opcode::FCOM |
      Opcode::FCOMI |
      Opcode::FCOMIP |
      Opcode::FCOMP |
      Opcode::FCOMPP |
      Opcode::FICOM |
      Opcode::FICOMP |
      Opcode::FTST |
      Opcode::FUCOM |
      Opcode::FUCOMI |
      Opcode::FUCOMIP |
      Opcode::FUCOMP |
      Opcode::FUCOMPP |
      Opcode::FXAM |
      Opcode::KORTESTB |
      Opcode::KORTESTD |
      Opcode::KORTESTQ |
      Opcode::KORTESTW |
      Opcode::KTESTB |
      Opcode::KTESTD |
      Opcode::KTESTQ |
      Opcode::KTESTW |
      Opcode::MAXPD |
      Opcode::MAXPS |
      Opcode::MAXSD |
      Opcode::MAXSS |
      Opcode::MINPD |
      Opcode::MINPS |
      Opcode::MINSD |
      Opcode::MINSS |
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
      Opcode::PFCMPEQ |
      Opcode::PFCMPGE |
      Opcode::PFCMPGT |
      Opcode::PFMAX |
      Opcode::PFMIN |
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
      Opcode::PTEST |
      Opcode::SCAS |
      Opcode::TEST |
      Opcode::VCMPPD |
      Opcode::VCMPPS |
      Opcode::VCMPSD |
      Opcode::VCMPSS |
      Opcode::VCOMISD |
      Opcode::VCOMISS |
      Opcode::VFPCLASSPD |
      Opcode::VFPCLASSPS |
      Opcode::VFPCLASSSD |
      Opcode::VFPCLASSSS |
      Opcode::VMAXPD |
      Opcode::VMAXPS |
      Opcode::VMAXSD |
      Opcode::VMAXSS |
      Opcode::VMINPD |
      Opcode::VMINPS |
      Opcode::VMINSD |
      Opcode::VMINSS |
      Opcode::VPCMPB |
      Opcode::VPCMPD |
      Opcode::VPCMPEQB |
      Opcode::VPCMPEQD |
      Opcode::VPCMPEQQ |
      Opcode::VPCMPEQW |
      Opcode::VPCMPESTRI |
      Opcode::VPCMPESTRM |
      Opcode::VPCMPGTB |
      Opcode::VPCMPGTD |
      Opcode::VPCMPGTQ |
      Opcode::VPCMPGTW |
      Opcode::VPCMPISTRI |
      Opcode::VPCMPISTRM |
      Opcode::VPCMPQ |
      Opcode::VPCMPUB |
      Opcode::VPCMPUD |
      Opcode::VPCMPUQ |
      Opcode::VPCMPUW |
      Opcode::VPCMPW |
      Opcode::VPCONFLICTD |
      Opcode::VPCONFLICTQ |
      Opcode::VPMAXSB |
      Opcode::VPMAXSD |
      Opcode::VPMAXSQ |
      Opcode::VPMAXSW |
      Opcode::VPMAXUB |
      Opcode::VPMAXUD |
      Opcode::VPMAXUQ |
      Opcode::VPMAXUW |
      Opcode::VPMINSB |
      Opcode::VPMINSD |
      Opcode::VPMINSQ |
      Opcode::VPMINSW |
      Opcode::VPMINUB |
      Opcode::VPMINUD |
      Opcode::VPMINUQ |
      Opcode::VPMINUW |
      Opcode::VPTERNLOGD |
      Opcode::VPTERNLOGQ |
      Opcode::VPTEST |
      Opcode::VPTESTMB |
      Opcode::VPTESTMD |
      Opcode::VPTESTMQ |
      Opcode::VPTESTMW |
      Opcode::VPTESTNMB |
      Opcode::VPTESTNMD |
      Opcode::VPTESTNMQ |
      Opcode::VPTESTNMW |
      Opcode::VRANGEPD |
      Opcode::VRANGEPS |
      Opcode::VRANGESD |
      Opcode::VRANGESS |
      Opcode::VTESTPD |
      Opcode::VTESTPS |
      Opcode::VUCOMISD |
      Opcode::VUCOMISS => {
        write!(out, "{}", colors.comparison_op(self))
      }
      Opcode::AAA |
      Opcode::AAD |
      Opcode::AAM |
      Opcode::AAS |
      Opcode::ADC |
      Opcode::ADCX |
      Opcode::ADD |
      Opcode::ADDPD |
      Opcode::ADDPS |
      Opcode::ADDSD |
      Opcode::ADDSS |
      Opcode::ADDSUBPD |
      Opcode::ADDSUBPS |
      Opcode::ADOX |
      Opcode::AND |
      Opcode::ANDN |
      Opcode::ANDNPD |
      Opcode::ANDNPS |
      Opcode::ANDPD |
      Opcode::ANDPS |
      Opcode::BEXTR |
      Opcode::BLSI |
      Opcode::BLSMSK |
      Opcode::BLSR |
      Opcode::BSF |
      Opcode::BSR |
      Opcode::BT |
      Opcode::BTC |
      Opcode::BTR |
      Opcode::BTS |
      Opcode::BZHI |
      Opcode::COMISD |
      Opcode::COMISS |
      Opcode::DAA |
      Opcode::DAS |
      Opcode::DEC |
      Opcode::DIV |
      Opcode::DIVPD |
      Opcode::DIVPS |
      Opcode::DIVSD |
      Opcode::DIVSS |
      Opcode::DPPD |
      Opcode::DPPS |
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
      Opcode::HADDPD |
      Opcode::HADDPS |
      Opcode::HSUBPD |
      Opcode::HSUBPS |
      Opcode::IDIV |
      Opcode::IMUL |
      Opcode::INC |
      Opcode::KADDB |
      Opcode::KADDD |
      Opcode::KADDQ |
      Opcode::KADDW |
      Opcode::KANDB |
      Opcode::KANDD |
      Opcode::KANDNB |
      Opcode::KANDND |
      Opcode::KANDNQ |
      Opcode::KANDNW |
      Opcode::KANDQ |
      Opcode::KANDW |
      Opcode::KNOTB |
      Opcode::KNOTD |
      Opcode::KNOTQ |
      Opcode::KNOTW |
      Opcode::KORB |
      Opcode::KORD |
      Opcode::KORQ |
      Opcode::KORW |
      Opcode::KSHIFTLB |
      Opcode::KSHIFTLD |
      Opcode::KSHIFTLQ |
      Opcode::KSHIFTLW |
      Opcode::KSHIFTRB |
      Opcode::KSHIFTRD |
      Opcode::KSHIFTRQ |
      Opcode::KSHIFTRW |
      Opcode::KXNORB |
      Opcode::KXNORD |
      Opcode::KXNORQ |
      Opcode::KXNORW |
      Opcode::KXORB |
      Opcode::KXORD |
      Opcode::KXORQ |
      Opcode::KXORW |
      Opcode::LEA |
      Opcode::LZCNT |
      Opcode::MPSADBW |
      Opcode::MUL |
      Opcode::MULPD |
      Opcode::MULPS |
      Opcode::MULSD |
      Opcode::MULSS |
      Opcode::MULX |
      Opcode::NEG |
      Opcode::NOT |
      Opcode::OR |
      Opcode::ORPD |
      Opcode::ORPS |
      Opcode::PABSB |
      Opcode::PABSD |
      Opcode::PABSW |
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
      Opcode::PAVGUSB |
      Opcode::PAVGW |
      Opcode::PCLMULQDQ |
      Opcode::PDEP |
      Opcode::PEXT |
      Opcode::PFACC |
      Opcode::PFADD |
      Opcode::PFMUL |
      Opcode::PFMULHRW |
      Opcode::PFNACC |
      Opcode::PFPNACC |
      Opcode::PFRCP |
      Opcode::PFRCPIT1 |
      Opcode::PFRCPIT2 |
      Opcode::PFRSQIT1 |
      Opcode::PFRSQRT |
      Opcode::PFSUB |
      Opcode::PFSUBR |
      Opcode::PHADDD |
      Opcode::PHADDSW |
      Opcode::PHADDW |
      Opcode::PHSUBD |
      Opcode::PHSUBSW |
      Opcode::PHSUBW |
      Opcode::PMADDUBSW |
      Opcode::PMADDWD |
      Opcode::PMULDQ |
      Opcode::PMULHRSW |
      Opcode::PMULHRW |
      Opcode::PMULHUW |
      Opcode::PMULHW |
      Opcode::PMULLD |
      Opcode::PMULLW |
      Opcode::PMULUDQ |
      Opcode::POPCNT |
      Opcode::POR |
      Opcode::PSADBW |
      Opcode::PSHUFB |
      Opcode::PSHUFD |
      Opcode::PSHUFW |
      Opcode::PSIGNB |
      Opcode::PSIGND |
      Opcode::PSIGNW |
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
      Opcode::PSWAPD |
      Opcode::PXOR |
      Opcode::RCL |
      Opcode::RCPPS |
      Opcode::RCPSS |
      Opcode::RCR |
      Opcode::ROL |
      Opcode::ROR |
      Opcode::RORX |
      Opcode::ROUNDPD |
      Opcode::ROUNDPS |
      Opcode::ROUNDSD |
      Opcode::ROUNDSS |
      Opcode::RSQRTPS |
      Opcode::RSQRTSS |
      Opcode::SAL |
      Opcode::SAR |
      Opcode::SARX |
      Opcode::SBB |
      Opcode::SHL |
      Opcode::SHLD |
      Opcode::SHLX |
      Opcode::SHR |
      Opcode::SHRD |
      Opcode::SHRX |
      Opcode::SLHD |
      Opcode::SQRTPD |
      Opcode::SQRTPS |
      Opcode::SQRTSD |
      Opcode::SQRTSS |
      Opcode::SUB |
      Opcode::SUBPD |
      Opcode::SUBPS |
      Opcode::SUBSD |
      Opcode::SUBSS |
      Opcode::TZCNT |
      Opcode::UCOMISD |
      Opcode::UCOMISS |
      Opcode::V4FMADDPS |
      Opcode::V4FMADDSS |
      Opcode::V4FNMADDPS |
      Opcode::V4FNMADDSS |
      Opcode::VADDPD |
      Opcode::VADDPS |
      Opcode::VADDSD |
      Opcode::VADDSS |
      Opcode::VADDSUBPD |
      Opcode::VADDSUBPS |
      Opcode::VANDNPD |
      Opcode::VANDNPS |
      Opcode::VANDPD |
      Opcode::VANDPS |
      Opcode::VBROADCASTF32X2 |
      Opcode::VBROADCASTF32X4 |
      Opcode::VBROADCASTF32X8 |
      Opcode::VBROADCASTF64X2 |
      Opcode::VBROADCASTF64X4 |
      Opcode::VBROADCASTI32X2 |
      Opcode::VBROADCASTI32X4 |
      Opcode::VBROADCASTI32X8 |
      Opcode::VBROADCASTI64X2 |
      Opcode::VBROADCASTI64X4 |
      Opcode::VCVTNE2PS2BF16 |
      Opcode::VCVTNEPS2BF16 |
      Opcode::VCVTUSI2SD |
      Opcode::VCVTUSI2SS |
      Opcode::VDBPSADBW |
      Opcode::VDIVPD |
      Opcode::VDIVPS |
      Opcode::VDIVSD |
      Opcode::VDIVSS |
      Opcode::VDPBF16PS |
      Opcode::VDPPD |
      Opcode::VDPPS |
      Opcode::VEXP2PD |
      Opcode::VEXP2PS |
      Opcode::VEXP2SD |
      Opcode::VEXP2SS |
      Opcode::VEXTRACTF32X8 |
      Opcode::VEXTRACTI32X8 |
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
      Opcode::VGF2P8AFFINEINVQB |
      Opcode::VGF2P8AFFINEQB |
      Opcode::VGF2P8MULB |
      Opcode::VHADDPD |
      Opcode::VHADDPS |
      Opcode::VHSUBPD |
      Opcode::VHSUBPS |
      Opcode::VINSERTF32X8 |
      Opcode::VINSERTI32X4 |
      Opcode::VINSERTI32X8 |
      Opcode::VMPSADBW |
      Opcode::VMULPD |
      Opcode::VMULPS |
      Opcode::VMULSD |
      Opcode::VMULSS |
      Opcode::VORPD |
      Opcode::VORPS |
      Opcode::VP2INTERSECTD |
      Opcode::VP2INTERSECTQ |
      Opcode::VP4DPWSSD |
      Opcode::VP4DPWSSDS |
      Opcode::VPABSB |
      Opcode::VPABSD |
      Opcode::VPABSQ |
      Opcode::VPABSW |
      Opcode::VPADDB |
      Opcode::VPADDD |
      Opcode::VPADDQ |
      Opcode::VPADDSB |
      Opcode::VPADDSW |
      Opcode::VPADDUSB |
      Opcode::VPADDUSW |
      Opcode::VPADDW |
      Opcode::VPAND |
      Opcode::VPANDD |
      Opcode::VPANDN |
      Opcode::VPANDND |
      Opcode::VPANDNQ |
      Opcode::VPANDQ |
      Opcode::VPAVGB |
      Opcode::VPAVGW |
      Opcode::VPBROADCASTMB2Q |
      Opcode::VPBROADCASTMW2D |
      Opcode::VPCOMPRESSB |
      Opcode::VPCOMPRESSW |
      Opcode::VPDPBUSD |
      Opcode::VPDPBUSDS |
      Opcode::VPDPWSSD |
      Opcode::VPDPWSSDS |
      Opcode::VPERMB |
      Opcode::VPERMT2B |
      Opcode::VPERMT2W |
      Opcode::VPEXPANDB |
      Opcode::VPEXPANDD |
      Opcode::VPEXPANDQ |
      Opcode::VPEXPANDW |
      Opcode::VPHADDD |
      Opcode::VPHADDSW |
      Opcode::VPHADDW |
      Opcode::VPHSUBD |
      Opcode::VPHSUBSW |
      Opcode::VPHSUBW |
      Opcode::VPLZCNTD |
      Opcode::VPLZCNTQ |
      Opcode::VPMADD52HUQ |
      Opcode::VPMADD52LUQ |
      Opcode::VPMADDUBSW |
      Opcode::VPMADDWD |
      Opcode::VPMOVD2M |
      Opcode::VPMOVDB |
      Opcode::VPMOVDW |
      Opcode::VPMOVQB |
      Opcode::VPMOVQD |
      Opcode::VPMOVQW |
      Opcode::VPMOVWB |
      Opcode::VPMULDQ |
      Opcode::VPMULHRSW |
      Opcode::VPMULHUW |
      Opcode::VPMULHW |
      Opcode::VPMULLD |
      Opcode::VPMULLQ |
      Opcode::VPMULLW |
      Opcode::VPMULTISHIFTQB |
      Opcode::VPMULUDQ |
      Opcode::VPOPCNTB |
      Opcode::VPOPCNTD |
      Opcode::VPOPCNTQ |
      Opcode::VPOPCNTW |
      Opcode::VPOR |
      Opcode::VPORD |
      Opcode::VPORQ |
      Opcode::VPROLD |
      Opcode::VPROLQ |
      Opcode::VPROLVD |
      Opcode::VPROLVQ |
      Opcode::VPRORD |
      Opcode::VPRORQ |
      Opcode::VPRORRD |
      Opcode::VPRORRQ |
      Opcode::VPRORVD |
      Opcode::VPRORVQ |
      Opcode::VPSADBW |
      Opcode::VPSHLDD |
      Opcode::VPSHLDQ |
      Opcode::VPSHLDVD |
      Opcode::VPSHLDVQ |
      Opcode::VPSHLDVW |
      Opcode::VPSHLDW |
      Opcode::VPSHRDD |
      Opcode::VPSHRDQ |
      Opcode::VPSHRDVD |
      Opcode::VPSHRDVQ |
      Opcode::VPSHRDVW |
      Opcode::VPSHRDW |
      Opcode::VPSHUFBITQMB |
      Opcode::VPSIGNB |
      Opcode::VPSIGND |
      Opcode::VPSIGNW |
      Opcode::VPSLLD |
      Opcode::VPSLLDQ |
      Opcode::VPSLLQ |
      Opcode::VPSLLVD |
      Opcode::VPSLLVQ |
      Opcode::VPSLLVW |
      Opcode::VPSLLW |
      Opcode::VPSRAD |
      Opcode::VPSRAQ |
      Opcode::VPSRAVD |
      Opcode::VPSRAVQ |
      Opcode::VPSRAVW |
      Opcode::VPSRAW |
      Opcode::VPSRLD |
      Opcode::VPSRLDQ |
      Opcode::VPSRLQ |
      Opcode::VPSRLVD |
      Opcode::VPSRLVQ |
      Opcode::VPSRLVW |
      Opcode::VPSRLW |
      Opcode::VPSUBB |
      Opcode::VPSUBD |
      Opcode::VPSUBQ |
      Opcode::VPSUBSB |
      Opcode::VPSUBSW |
      Opcode::VPSUBUSB |
      Opcode::VPSUBUSW |
      Opcode::VPSUBW |
      Opcode::VPXOR |
      Opcode::VPXORD |
      Opcode::VPXORQ |
      Opcode::VRCP14PD |
      Opcode::VRCP14PS |
      Opcode::VRCP14SD |
      Opcode::VRCP14SS |
      Opcode::VRCP28PD |
      Opcode::VRCP28PS |
      Opcode::VRCP28SD |
      Opcode::VRCP28SS |
      Opcode::VRCPPS |
      Opcode::VRCPSS |
      Opcode::VRNDSCALEPD |
      Opcode::VRNDSCALEPS |
      Opcode::VRNDSCALESD |
      Opcode::VRNDSCALESS |
      Opcode::VROUNDPD |
      Opcode::VROUNDPS |
      Opcode::VROUNDSD |
      Opcode::VROUNDSS |
      Opcode::VRSQRT14PD |
      Opcode::VRSQRT14PS |
      Opcode::VRSQRT14SD |
      Opcode::VRSQRT14SS |
      Opcode::VRSQRT28PD |
      Opcode::VRSQRT28PS |
      Opcode::VRSQRT28SD |
      Opcode::VRSQRT28SS |
      Opcode::VRSQRTPS |
      Opcode::VRSQRTSS |
      Opcode::VSCALEDPD |
      Opcode::VSCALEDPS |
      Opcode::VSCALEDSD |
      Opcode::VSCALEDSS |
      Opcode::VSCALEFPD |
      Opcode::VSCALEFPS |
      Opcode::VSCALEFSD |
      Opcode::VSCALEFSS |
      Opcode::VSQRTPD |
      Opcode::VSQRTPS |
      Opcode::VSQRTSD |
      Opcode::VSQRTSS |
      Opcode::VSUBPD |
      Opcode::VSUBPS |
      Opcode::VSUBSD |
      Opcode::VSUBSS |
      Opcode::VXORPD |
      Opcode::VXORPS |
      Opcode::XADD |
      Opcode::XOR |
      Opcode::XORPD |
      Opcode::XORPS => {
        write!(out, "{}", colors.arithmetic_op(self))
      }
      Opcode::Invalid |
      Opcode::UD0 |
      Opcode::UD1 |
      Opcode::UD2 => {
        write!(out, "{}", colors.invalid_op(self))
      }
      Opcode::HLT |
      Opcode::INT |
      Opcode::INTO |
      Opcode::IRET |
      Opcode::IRETD |
      Opcode::IRETQ |
      Opcode::RETF |
      Opcode::RETURN => {
        write!(out, "{}", colors.stop_op(self))
      }
      Opcode::AESDEC |
      Opcode::AESDEC128KL |
      Opcode::AESDEC256KL |
      Opcode::AESDECLAST |
      Opcode::AESDECWIDE128KL |
      Opcode::AESDECWIDE256KL |
      Opcode::AESENC |
      Opcode::AESENC128KL |
      Opcode::AESENC256KL |
      Opcode::AESENCLAST |
      Opcode::AESENCWIDE128KL |
      Opcode::AESENCWIDE256KL |
      Opcode::AESIMC |
      Opcode::AESKEYGENASSIST |
      Opcode::CLRSSBSY |
      Opcode::CRC32 |
      Opcode::ENCODEKEY128 |
      Opcode::ENCODEKEY256 |
      Opcode::ENDBR32 |
      Opcode::ENDBR64 |
      Opcode::FDECSTP |
      Opcode::FFREE |
      Opcode::FFREEP |
      Opcode::FINCSTP |
      Opcode::GF2P8AFFINEINVQB |
      Opcode::GF2P8AFFINEQB |
      Opcode::GF2P8MULB |
      Opcode::HRESET |
      Opcode::INCSSP |
      Opcode::LOADIWKEY |
      Opcode::RDRAND |
      Opcode::RDSEED |
      Opcode::RSTORSSP |
      Opcode::SAVEPREVSSP |
      Opcode::SETSSBSY |
      Opcode::SHA1MSG1 |
      Opcode::SHA1MSG2 |
      Opcode::SHA1NEXTE |
      Opcode::SHA1RNDS4 |
      Opcode::SHA256MSG1 |
      Opcode::SHA256MSG2 |
      Opcode::SHA256RNDS2 |
      Opcode::VAESDEC |
      Opcode::VAESDECLAST |
      Opcode::VAESENC |
      Opcode::VAESENCLAST |
      Opcode::VAESIMC |
      Opcode::VAESKEYGENASSIST |
      Opcode::WRSS |
      Opcode::WRUSS => {
        write!(out, "{}", colors.misc_op(self))
      }
      Opcode::CALL |
      Opcode::CALLF |
      Opcode::JA |
      Opcode::JB |
      Opcode::JCXZ |
      Opcode::JECXZ |
      Opcode::JG |
      Opcode::JGE |
      Opcode::JL |
      Opcode::JLE |
      Opcode::JMP |
      Opcode::JMPF |
      Opcode::JNA |
      Opcode::JNB |
      Opcode::JNO |
      Opcode::JNP |
      Opcode::JNS |
      Opcode::JNZ |
      Opcode::JO |
      Opcode::JP |
      Opcode::JRCXZ |
      Opcode::JS |
      Opcode::JZ |
      Opcode::LOOP |
      Opcode::LOOPNZ |
      Opcode::LOOPZ => {
        write!(out, "{}", colors.control_flow_op(self))
      }
    }
  }
}

pub(crate) mod real_mode {
  use crate::generated::real_mode::Opcode;
  use crate::real_mode::{InstDecoder, Instruction, DecodeError};

  use yaxpeax_arch::{Colorize, YaxColors};
  use core::fmt;
  impl InstDecoder {
    fn feature_sgx(&self) -> bool {
      true
    }
    fn feature_smap(&self) -> bool {
      true
    }
    fn feature_avx_unimplemented(&self) -> bool {
      true
    }
    fn feature_uintr(&self) -> bool {
      true
    }
    fn feature_pentium(&self) -> bool {
      true
    }
    fn feature_aesni(&self) -> bool {
      true
    }
    fn feature_invlpgb(&self) -> bool {
      true
    }
    fn feature_fsgsbase(&self) -> bool {
      true
    }
    fn feature_vmx(&self) -> bool {
      true
    }
    fn feature_simd(&self) -> bool {
      true
    }
    fn feature_avx(&self) -> bool {
      true
    }
    fn feature_hreset(&self) -> bool {
      true
    }
    fn feature_avx512_vp2intersect(&self) -> bool {
      true
    }
    fn feature_rdpru(&self) -> bool {
      true
    }
    fn feature_sse4_2(&self) -> bool {
      true
    }
    fn feature_monitor(&self) -> bool {
      true
    }
    fn feature_avx512_f_typo(&self) -> bool {
      true
    }
    fn feature_sse4_1(&self) -> bool {
      true
    }
    fn feature_avx512_dq(&self) -> bool {
      true
    }
    fn feature_adx(&self) -> bool {
      true
    }
    fn feature_fxsr(&self) -> bool {
      true
    }
    fn feature_tsx(&self) -> bool {
      true
    }
    fn feature_waitpkg(&self) -> bool {
      true
    }
    fn feature_80286(&self) -> bool {
      true
    }
    fn feature_sysenter(&self) -> bool {
      true
    }
    fn feature_invpcid_unimplemented(&self) -> bool {
      true
    }
    fn feature_avx512_4vnniw(&self) -> bool {
      true
    }
    fn feature_avx512_vnni(&self) -> bool {
      true
    }
    fn feature_avx512_pf(&self) -> bool {
      true
    }
    fn feature_avx512_cd(&self) -> bool {
      true
    }
    fn feature_avx512_bf16(&self) -> bool {
      true
    }
    fn feature_avx512bw_unimplemented(&self) -> bool {
      true
    }
    fn feature_mpk(&self) -> bool {
      true
    }
    fn feature_avx512_f__vl_unimplemented(&self) -> bool {
      true
    }
    fn feature_sha(&self) -> bool {
      true
    }
    fn feature_cet(&self) -> bool {
      true
    }
    fn feature_80486(&self) -> bool {
      true
    }
    fn feature_sse(&self) -> bool {
      true
    }
    fn feature_pentium_pro(&self) -> bool {
      true
    }
    fn feature_itanium(&self) -> bool {
      true
    }
    fn feature_mpx(&self) -> bool {
      true
    }
    fn feature_extra_instructions(&self) -> bool {
      true
    }
    fn feature_rdtscp(&self) -> bool {
      true
    }
    fn feature_avx2(&self) -> bool {
      true
    }
    fn feature_pconfig(&self) -> bool {
      true
    }
    fn feature_avx512_ifma(&self) -> bool {
      true
    }
    fn feature_gfni(&self) -> bool {
      true
    }
    fn feature_vpclmulqdq(&self) -> bool {
      true
    }
    fn feature_80386(&self) -> bool {
      true
    }
    fn feature_movbe(&self) -> bool {
      true
    }
    fn feature_avx512_f(&self) -> bool {
      true
    }
    fn feature_emx(&self) -> bool {
      true
    }
    fn feature_pclmulqdq(&self) -> bool {
      true
    }
    fn feature_fma3(&self) -> bool {
      true
    }
    fn feature_fma4(&self) -> bool {
      true
    }
    fn feature_xsaveopt(&self) -> bool {
      true
    }
    fn feature_syscall(&self) -> bool {
      true
    }
    fn feature_sse4a(&self) -> bool {
      true
    }
    fn feature_avx512_vpopcntdq(&self) -> bool {
      true
    }
    fn feature_invpcid(&self) -> bool {
      true
    }
    fn feature_80186_bound(&self) -> bool {
      true
    }
    fn feature_tsxldtrk(&self) -> bool {
      true
    }
    fn feature_cmov(&self) -> bool {
      true
    }
    fn feature_avx512_bw(&self) -> bool {
      true
    }
    fn feature_ssse3(&self) -> bool {
      true
    }
    fn feature_clflushopt(&self) -> bool {
      true
    }
    fn feature_80186_pusha(&self) -> bool {
      true
    }
    fn feature_smx(&self) -> bool {
      true
    }
    fn feature_8086_bcd(&self) -> bool {
      true
    }
    fn feature_abm(&self) -> bool {
      true
    }
    fn feature_3dnowprefetch(&self) -> bool {
      true
    }
    fn feature_rdpid(&self) -> bool {
      true
    }
    fn feature_lahfsahf(&self) -> bool {
      true
    }
    fn feature_jcxz(&self) -> bool {
      true
    }
    fn feature_enqcmd(&self) -> bool {
      true
    }
    fn feature_new(&self) -> bool {
      true
    }
    fn feature_ptwrite(&self) -> bool {
      true
    }
    fn feature_avx512_vbmi(&self) -> bool {
      true
    }
    fn feature_x87(&self) -> bool {
      true
    }
    fn feature_mmx(&self) -> bool {
      true
    }
    fn feature_avx512_bitalg(&self) -> bool {
      true
    }
    fn feature_keylocker(&self) -> bool {
      true
    }
    fn feature_80186(&self) -> bool {
      true
    }
    fn feature_clwb(&self) -> bool {
      true
    }
    fn feature_avx512_4fmaps(&self) -> bool {
      true
    }
    fn feature_tdx(&self) -> bool {
      true
    }
    fn feature_popcnt(&self) -> bool {
      true
    }
    fn feature_snp(&self) -> bool {
      true
    }
    fn feature_avx512_gfni(&self) -> bool {
      true
    }
    fn feature_3dnow(&self) -> bool {
      true
    }
    fn feature_avx512_vbmi2(&self) -> bool {
      true
    }
    fn feature_8086(&self) -> bool {
      true
    }
    fn feature_svm(&self) -> bool {
      true
    }
    fn feature_sse3(&self) -> bool {
      true
    }
    fn feature_sse2(&self) -> bool {
      true
    }
    fn feature_avx512_er(&self) -> bool {
      true
    }
    fn feature_xsave64(&self) -> bool {
      true
    }
    fn feature_80286_arpl(&self) -> bool {
      true
    }
    fn feature_bmi1(&self) -> bool {
      true
    }
    fn feature_bmi2(&self) -> bool {
      true
    }
    fn feature_movdir(&self) -> bool {
      true
    }
    fn feature_vaes(&self) -> bool {
      true
    }
    fn feature_xsave(&self) -> bool {
      true
    }
    fn feature_rdrand(&self) -> bool {
      true
    }
    fn feature_clzero(&self) -> bool {
      true
    }
    fn feature_rdseed(&self) -> bool {
      true
    }
  }
  impl<T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Opcode {
    fn colorize(&self, colors: &Y, out: &mut T) -> fmt::Result {
      self.to_generic().colorize(colors, out)
    }
  }
  impl Opcode {
    pub fn name(&self) -> &'static str {
      self.to_generic().name()
    }
  }
  pub(crate) fn revise_instruction(decoder: &InstDecoder, inst: &mut Instruction) -> Result<(), DecodeError> {
    if inst.prefixes.evex().is_some() {
      if !decoder.avx512() {
        return Err(DecodeError::InvalidOpcode);
      } else {
        return Ok(());
      }
    }

    // for some instructions (tzcnt), not having an extension means the instruction is
    // interpreted as another, rather than being simply rejected.
    // we might still reject the alternate instruction later, if the extension adding *it*
    // is also not supported.
    if inst.opcode == Opcode::TZCNT {
      if !decoder.bmi1() {
        // tzcnt is only supported if bmi1 is enabled. without bmi1, this decodes as bsf.
        inst.opcode = Opcode::BSF;
      }
    }

    match inst.opcode {
      // we'll never be rejecting the instruction `Invalid`
      Opcode::Invalid => {}
      Opcode::ENCLS |
      Opcode::ENCLU |
      Opcode::ENCLV => {
        if !decoder.feature_sgx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLAC |
      Opcode::STAC => {
        if !decoder.feature_smap() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMAXUB => {
        if !decoder.feature_avx_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::UIRET |
      Opcode::TESTUI |
      Opcode::CLUI |
      Opcode::STUI |
      Opcode::SENDUIPI => {
        if !decoder.feature_uintr() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CPUID |
      Opcode::WRMSR |
      Opcode::RDTSC |
      Opcode::RDMSR |
      Opcode::RSM |
      Opcode::CMPXCHG8B => {
        if !decoder.feature_pentium() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::AESKEYGENASSIST |
      Opcode::AESIMC |
      Opcode::AESENC |
      Opcode::AESENCLAST |
      Opcode::AESDEC |
      Opcode::AESDECLAST => {
        if !decoder.feature_aesni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVLPGB |
      Opcode::TLBSYNC => {
        if !decoder.feature_invlpgb() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDFSBASE |
      Opcode::RDGSBASE |
      Opcode::WRFSBASE |
      Opcode::WRGSBASE => {
        if !decoder.feature_fsgsbase() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VMPTRLD |
      Opcode::VMPTRST |
      Opcode::VMCLEAR |
      Opcode::VMREAD |
      Opcode::VMWRITE |
      Opcode::VMCALL |
      Opcode::VMLAUNCH |
      Opcode::VMRESUME |
      Opcode::VMXOFF |
      Opcode::VMXON |
      Opcode::INVEPT |
      Opcode::INVVPID |
      Opcode::VMFUNC => {
        if !decoder.feature_vmx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VLDDQU |
      Opcode::VPADDB |
      Opcode::VPADDD |
      Opcode::VPADDQ |
      Opcode::VPADDW |
      Opcode::VPABSB |
      Opcode::VPABSW |
      Opcode::VPABSD |
      Opcode::VMAXSD |
      Opcode::VMAXSS |
      Opcode::VMINSD |
      Opcode::VMINSS |
      Opcode::VMULSD |
      Opcode::VMULSS |
      Opcode::VRCPSS |
      Opcode::VPSUBUSB |
      Opcode::VPSUBUSW |
      Opcode::VXORPD |
      Opcode::VXORPS |
      Opcode::VPAVGB |
      Opcode::VPAVGW |
      Opcode::VANDNPD |
      Opcode::VANDNPS |
      Opcode::VPHADDD |
      Opcode::VPHADDSW |
      Opcode::VPHADDW |
      Opcode::VANDPD |
      Opcode::VANDPS |
      Opcode::VPADDSB |
      Opcode::VPADDSW |
      Opcode::VPADDUSB |
      Opcode::VPADDUSW |
      Opcode::VPHSUBD |
      Opcode::VPHSUBSW |
      Opcode::VPHSUBW |
      Opcode::VHADDPD |
      Opcode::VHADDPS |
      Opcode::VHSUBPD |
      Opcode::VHSUBPS |
      Opcode::VORPD |
      Opcode::VORPS |
      Opcode::VPBLENDVB |
      Opcode::VPBLENDW |
      Opcode::VADDSUBPD |
      Opcode::VADDSUBPS |
      Opcode::VRSQRTPS |
      Opcode::VRSQRTSS |
      Opcode::VPALIGNR |
      Opcode::VPSUBSB |
      Opcode::VPSUBSW |
      Opcode::VPMULHUW |
      Opcode::VPMULHW |
      Opcode::VPMULLW |
      Opcode::VPSHUFB |
      Opcode::VPSHUFHW |
      Opcode::VPSHUFLW |
      Opcode::VPHMINPOSUW |
      Opcode::VRCPPS |
      Opcode::VMPSADBW |
      Opcode::VPMADDUBSW |
      Opcode::VPMADDWD |
      Opcode::VMASKMOVDQU |
      Opcode::VPMOVMSKB |
      Opcode::VPSADBW |
      Opcode::VPSLLDQ |
      Opcode::VPSRLDQ |
      Opcode::VDPPD |
      Opcode::VDPPS |
      Opcode::VLDMXCSR |
      Opcode::VSTMXCSR |
      Opcode::VMOVMSKPD |
      Opcode::VMOVMSKPS |
      Opcode::VPTEST |
      Opcode::VTESTPD |
      Opcode::VTESTPS |
      Opcode::VPEXTRB |
      Opcode::VPEXTRW |
      Opcode::VPEXTRD |
      Opcode::VPEXTRQ |
      Opcode::VPACKSSDW |
      Opcode::VPACKUSDW |
      Opcode::VPACKSSWB |
      Opcode::VPACKUSWB |
      Opcode::VBLENDPD |
      Opcode::VBLENDPS |
      Opcode::VBLENDVPD |
      Opcode::VBLENDVPS |
      Opcode::VPMULHRSW |
      Opcode::VPAND |
      Opcode::VPANDN |
      Opcode::VPOR |
      Opcode::VPCMPEQB |
      Opcode::VPCMPEQD |
      Opcode::VPCMPEQQ |
      Opcode::VPCMPEQW |
      Opcode::VPCMPGTB |
      Opcode::VPCMPGTD |
      Opcode::VPCMPGTQ |
      Opcode::VPCMPGTW |
      Opcode::VPMAXSB |
      Opcode::VPMAXSD |
      Opcode::VPMAXSW |
      Opcode::VPMAXUB |
      Opcode::VPMAXUW |
      Opcode::VPMINSB |
      Opcode::VPMINSW |
      Opcode::VPMINUB |
      Opcode::VPMINUW |
      Opcode::VPMAXUD |
      Opcode::VPMINSD |
      Opcode::VPSIGNB |
      Opcode::VPSIGNW |
      Opcode::VPSIGND |
      Opcode::VPINSRB |
      Opcode::VPINSRW |
      Opcode::VPINSRD |
      Opcode::VPINSRQ |
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
      Opcode::VPMULDQ |
      Opcode::VPMULLQ |
      Opcode::VPMULLD |
      Opcode::VPMULUDQ |
      Opcode::VPSHUFD |
      Opcode::VSHUFPD |
      Opcode::VSHUFPS |
      Opcode::VSQRTPD |
      Opcode::VSQRTPS |
      Opcode::VSQRTSS |
      Opcode::VSQRTSD |
      Opcode::VSUBPD |
      Opcode::VSUBPS |
      Opcode::VSUBSD |
      Opcode::VSUBSS |
      Opcode::VUCOMISD |
      Opcode::VUCOMISS |
      Opcode::VROUNDPD |
      Opcode::VROUNDPS |
      Opcode::VROUNDSD |
      Opcode::VROUNDSS |
      Opcode::VPSLLD |
      Opcode::VPSLLQ |
      Opcode::VPSLLW |
      Opcode::VPSRAD |
      Opcode::VPSRAW |
      Opcode::VPSRLD |
      Opcode::VPSRLQ |
      Opcode::VPSRLW |
      Opcode::VPSUBB |
      Opcode::VPSUBW |
      Opcode::VPSUBD |
      Opcode::VPSUBQ |
      Opcode::VPXOR |
      Opcode::VPUNPCKHBW |
      Opcode::VPUNPCKHWD |
      Opcode::VPUNPCKHDQ |
      Opcode::VPUNPCKHQDQ |
      Opcode::VPUNPCKLBW |
      Opcode::VPUNPCKLWD |
      Opcode::VPUNPCKLDQ |
      Opcode::VPUNPCKLQDQ |
      Opcode::VUNPCKHPD |
      Opcode::VUNPCKHPS |
      Opcode::VUNPCKLPD |
      Opcode::VUNPCKLPS |
      Opcode::VPCMPESTRI |
      Opcode::VPCMPESTRM |
      Opcode::VPCMPISTRI |
      Opcode::VPCMPISTRM |
      Opcode::VBROADCASTSS |
      Opcode::VBROADCASTSD |
      Opcode::VBROADCASTF128 |
      Opcode::VBROADCASTSD |
      Opcode::VBROADCASTSS |
      Opcode::VINSERTF128 |
      Opcode::VEXTRACTF128 |
      Opcode::VMASKMOVPD |
      Opcode::VMASKMOVPS |
      Opcode::VPERMILPD |
      Opcode::VPERMILPS |
      Opcode::VPERM2F128 |
      Opcode::VZEROUPPER |
      Opcode::VZEROALL => {
        if !decoder.feature_avx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::HRESET => {
        if !decoder.feature_hreset() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VP2INTERSECTD |
      Opcode::VP2INTERSECTQ => {
        if !decoder.feature_avx512_vp2intersect() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPRU => {
        if !decoder.feature_rdpru() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CRC32 |
      Opcode::PCMPESTRI |
      Opcode::PCMPESTRM |
      Opcode::PCMPISTRI |
      Opcode::PCMPISTRM |
      Opcode::PCMPGTQ => {
        if !decoder.feature_sse4_2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MONITOR |
      Opcode::MWAIT => {
        if !decoder.feature_monitor() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPRORRD |
      Opcode::VPRORRQ => {
        if !decoder.feature_avx512_f_typo() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PHMINPOSUW |
      Opcode::PMULDQ |
      Opcode::PMULLD |
      Opcode::DPPS |
      Opcode::DPPD |
      Opcode::PACKUSDW |
      Opcode::PCMPEQQ |
      Opcode::PTEST |
      Opcode::MOVNTDQA |
      Opcode::ROUNDSS |
      Opcode::ROUNDSD |
      Opcode::ROUNDPS |
      Opcode::ROUNDPD |
      Opcode::PMAXSB |
      Opcode::PMAXSD |
      Opcode::PMAXUW |
      Opcode::PMAXUD |
      Opcode::PMINSD |
      Opcode::PMINSB |
      Opcode::PMINUD |
      Opcode::PMINUW |
      Opcode::BLENDW |
      Opcode::PBLENDW |
      Opcode::BLENDVPS |
      Opcode::BLENDVPD |
      Opcode::PBLENDVB |
      Opcode::BLENDPS |
      Opcode::BLENDPD |
      Opcode::MPSADBW |
      Opcode::PMOVZXDQ |
      Opcode::PMOVSXDQ |
      Opcode::PMOVZXBD |
      Opcode::PMOVSXBD |
      Opcode::PMOVZXWQ |
      Opcode::PMOVSXWQ |
      Opcode::PMOVZXBQ |
      Opcode::PMOVSXBQ |
      Opcode::PMOVSXWD |
      Opcode::PMOVZXWD |
      Opcode::PEXTRQ |
      Opcode::PEXTRD |
      Opcode::PEXTRB |
      Opcode::PMOVSXBW |
      Opcode::PMOVZXBW |
      Opcode::PINSRQ |
      Opcode::PINSRD |
      Opcode::PINSRB |
      Opcode::EXTRACTPS |
      Opcode::INSERTPS => {
        if !decoder.feature_sse4_1() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::KANDB |
      Opcode::KANDNB |
      Opcode::KADDB |
      Opcode::KTESTB |
      Opcode::KADDW |
      Opcode::KTESTW |
      Opcode::KMOVB |
      Opcode::KNOTB |
      Opcode::KORB |
      Opcode::KORTESTB |
      Opcode::KSHIFTLB |
      Opcode::KSHIFTRB |
      Opcode::KXNORB |
      Opcode::KXORB |
      Opcode::VBROADCASTF32X2 |
      Opcode::VBROADCASTF64X2 |
      Opcode::VBROADCASTF32X8 |
      Opcode::VBROADCASTI32X8 |
      Opcode::VBROADCASTI64X2 |
      Opcode::VBROADCASTI32X2 |
      Opcode::VEXTRACTF32X8 |
      Opcode::VEXTRACTI32X8 |
      Opcode::VGETEXPSD |
      Opcode::VGETEXPSS |
      Opcode::VXORPD |
      Opcode::VXORPS |
      Opcode::VPEXTRD |
      Opcode::VPEXTRQ |
      Opcode::VPINSRD |
      Opcode::VPINSRQ |
      Opcode::VANDNPD |
      Opcode::VANDNPS |
      Opcode::VANDPD |
      Opcode::VANDPS |
      Opcode::VORPD |
      Opcode::VORPS |
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
      Opcode::VEXTRACTF64X2 |
      Opcode::VEXTRACTI64X2 |
      Opcode::VFPCLASSPD |
      Opcode::VFPCLASSPS |
      Opcode::VFPCLASSSD |
      Opcode::VFPCLASSSS |
      Opcode::VINSERTF64X2 |
      Opcode::VINSERTF32X8 |
      Opcode::VINSERTI32X8 |
      Opcode::VINSERTI64X2 |
      Opcode::VPMOVM2D |
      Opcode::VPMOVM2Q |
      Opcode::VPMOVB2D |
      Opcode::VPMOVQ2M |
      Opcode::VRANGEPD |
      Opcode::VRANGEPS |
      Opcode::VRANGESD |
      Opcode::VRANGESS |
      Opcode::VREDUCEPD |
      Opcode::VREDUCEPS |
      Opcode::VREDUCESD |
      Opcode::VREDUCESS => {
        if !decoder.feature_avx512_dq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ADCX |
      Opcode::ADOX => {
        if !decoder.feature_adx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::FXSAVE |
      Opcode::FXRSTOR => {
        if !decoder.feature_fxsr() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XABORT |
      Opcode::XBEGIN |
      Opcode::XEND |
      Opcode::XTEST => {
        if !decoder.feature_tsx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::TPAUSE |
      Opcode::UMONITOR |
      Opcode::UMWAIT => {
        if !decoder.feature_waitpkg() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLTS |
      Opcode::LAR |
      Opcode::LGDT |
      Opcode::LIDT |
      Opcode::LLDT |
      Opcode::LMSW |
      Opcode::LSL |
      Opcode::SGDT |
      Opcode::SIDT |
      Opcode::SLDT |
      Opcode::SMSW |
      Opcode::STR |
      Opcode::LTR |
      Opcode::VERR |
      Opcode::VERW => {
        if !decoder.feature_80286() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SYSENTER |
      Opcode::SYSEXIT => {
        if !decoder.feature_sysenter() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVVPID => {
        if !decoder.feature_invpcid_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VP4DPWSSDS |
      Opcode::VP4DPWSSD => {
        if !decoder.feature_avx512_4vnniw() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPDPBUSDS |
      Opcode::VPDPBUSD |
      Opcode::VPDPWSSDS |
      Opcode::VPDPWSSD => {
        if !decoder.feature_avx512_vnni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VGATHERPF0DPD |
      Opcode::VGATHERPF0DPS |
      Opcode::VGATHERPF0QPD |
      Opcode::VGATHERPF0QPS |
      Opcode::VGATHERPF1DPD |
      Opcode::VGATHERPF1DPS |
      Opcode::VGATHERPF1QPD |
      Opcode::VGATHERPF1QPS |
      Opcode::VSCATTERPF0DPD |
      Opcode::VSCATTERPF0DPS |
      Opcode::VSCATTERPF0QPD |
      Opcode::VSCATTERPF0QPS |
      Opcode::VSCATTERPF1DPD |
      Opcode::VSCATTERPF1DPS |
      Opcode::VSCATTERPF1QPD |
      Opcode::VSCATTERPF1QPS => {
        if !decoder.feature_avx512_pf() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPBROADCASTMW2D |
      Opcode::VPBROADCASTMB2Q |
      Opcode::VPBROADCASTM |
      Opcode::VPCONFLICTD |
      Opcode::VPCONFLICTQ |
      Opcode::VPLZCNTD |
      Opcode::VPLZCNTQ => {
        if !decoder.feature_avx512_cd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VCVTNE2PS2BF16 |
      Opcode::VCVTNEPS2BF16 |
      Opcode::VDPBF16PS => {
        if !decoder.feature_avx512_bf16() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMOVQ2M => {
        if !decoder.feature_avx512bw_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPKRU |
      Opcode::WRPKRU => {
        if !decoder.feature_mpk() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VSCATTERDPS |
      Opcode::VSCATTERDPD |
      Opcode::VSCATTERQPS |
      Opcode::VSCATTERQPD => {
        if !decoder.feature_avx512_f__vl_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SHA1RNDS4 |
      Opcode::SHA1NEXTE |
      Opcode::SHA1MSG1 |
      Opcode::SHA1MSG2 |
      Opcode::SHA256RNDS2 |
      Opcode::SHA256MSG1 |
      Opcode::SHA256MSG2 => {
        if !decoder.feature_sha() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::WRUSS |
      Opcode::WRSS |
      Opcode::INCSSP |
      Opcode::SAVEPREVSSP |
      Opcode::SETSSBSY |
      Opcode::CLRSSBSY |
      Opcode::RSTORSSP |
      Opcode::ENDBR64 |
      Opcode::ENDBR32 => {
        if !decoder.feature_cet() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BSWAP |
      Opcode::CMPXCHG |
      Opcode::INVD |
      Opcode::WBINVD |
      Opcode::INVLPG |
      Opcode::XADD => {
        if !decoder.feature_80486() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVSS |
      Opcode::ADDSS |
      Opcode::SUBSS |
      Opcode::MULSS |
      Opcode::DIVSS |
      Opcode::MINSS |
      Opcode::MAXSS |
      Opcode::SQRTSS |
      Opcode::MOVUPS |
      Opcode::MOVHLPS |
      Opcode::MOVLPS |
      Opcode::MOVHPS |
      Opcode::MOVLHPS |
      Opcode::UNPCKLPS |
      Opcode::UNPCKHPS |
      Opcode::PREFETCHNTA |
      Opcode::PREFETCH0 |
      Opcode::PREFETCH1 |
      Opcode::PREFETCH2 |
      Opcode::MOVAPS |
      Opcode::CVTPI2PS |
      Opcode::CVTSI2SS |
      Opcode::MOVNTPS |
      Opcode::CVTTSS2SI |
      Opcode::CVTTPS2PI |
      Opcode::CVTSS2SI |
      Opcode::CVTPS2PI |
      Opcode::UCOMISS |
      Opcode::COMISS |
      Opcode::SQRTPS |
      Opcode::MOVMSKPS |
      Opcode::RSQRTSS |
      Opcode::RSQRTPS |
      Opcode::RCPPS |
      Opcode::RCPSS |
      Opcode::ANDPS |
      Opcode::ANDNPS |
      Opcode::XORPS |
      Opcode::ORPS |
      Opcode::ADDPS |
      Opcode::MULPS |
      Opcode::SUBPS |
      Opcode::MINPS |
      Opcode::DIVPS |
      Opcode::MAXPS |
      Opcode::PSHUFW |
      Opcode::LDMXCSR |
      Opcode::STMXCSR |
      Opcode::SFENCE |
      Opcode::CMPPS |
      Opcode::CMPSS |
      Opcode::PINSRW |
      Opcode::PEXTRW |
      Opcode::SHUFPS |
      Opcode::PMOVMSKB |
      Opcode::PMINUB |
      Opcode::PMAXUB |
      Opcode::PAVGB |
      Opcode::PAVGW |
      Opcode::PMULHUW |
      Opcode::MOVNTQ |
      Opcode::PMINSW |
      Opcode::PMAXSW |
      Opcode::PSADBW |
      Opcode::MASKMOVQ |
      Opcode::LDMXCSR |
      Opcode::STMXCSR => {
        if !decoder.feature_sse() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::UD2 => {
        if !decoder.feature_pentium_pro() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::JMPE => {
        if !decoder.feature_itanium() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BNDMK |
      Opcode::BNDCL |
      Opcode::BNDCU |
      Opcode::BNDCN |
      Opcode::BNDMOV |
      Opcode::BNDLDX |
      Opcode::BNDSTX => {
        if !decoder.feature_mpx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SWAPGS |
      Opcode::SLHD |
      Opcode::CDQE |
      Opcode::MOVSXD |
      Opcode::CMPXCHG16B => {
        if !decoder.feature_extra_instructions() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDTSCP => {
        if !decoder.feature_rdtscp() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VBROADCASTI128 |
      Opcode::VPBROADCASTB |
      Opcode::VPBROADCASTW |
      Opcode::VPBROADCASTD |
      Opcode::VPBROADCASTQ |
      Opcode::VINSERTI128 |
      Opcode::VEXTRACTI128 |
      Opcode::VPMASKMOVD |
      Opcode::VPMASKMOVQ |
      Opcode::VPERMPS |
      Opcode::VPERMD |
      Opcode::VPERMPD |
      Opcode::VPERMQ |
      Opcode::VPERM2I128 |
      Opcode::VPBLENDD |
      Opcode::VPSLLVD |
      Opcode::VPSLLVQ |
      Opcode::VPSRLVD |
      Opcode::VPSRLVQ |
      Opcode::VPSRAVD => {
        if !decoder.feature_avx2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PCONFIG => {
        if !decoder.feature_pconfig() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMADD52HUQ |
      Opcode::VPMADD52LUQ => {
        if !decoder.feature_avx512_ifma() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::GF2P8AFFINEQB |
      Opcode::GF2P8AFFINEINVQB |
      Opcode::GF2P8MULB => {
        if !decoder.feature_gfni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPCLMULQDQ => {
        if !decoder.feature_vpclmulqdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BSF |
      Opcode::BSR |
      Opcode::BT |
      Opcode::BTS |
      Opcode::BTC |
      Opcode::BTR |
      Opcode::CDQ |
      Opcode::CWDE |
      Opcode::LDS |
      Opcode::LES |
      Opcode::LFS |
      Opcode::LGS |
      Opcode::LSS |
      Opcode::MOVZX |
      Opcode::MOVSX |
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
      Opcode::SETG |
      Opcode::SHLD |
      Opcode::SHRD => {
        if !decoder.feature_80386() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVBE => {
        if !decoder.feature_movbe() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VCVTQQ2PD |
      Opcode::VCVTQQ2PS |
      Opcode::VCVTUSI2USD |
      Opcode::VCVTUSI2USS |
      Opcode::VEXTRACTF32X4 |
      Opcode::VEXTRACTF64X4 |
      Opcode::VEXTRACTI32X4 |
      Opcode::VEXTRACTI64X4 |
      Opcode::VFIXUPIMMSD |
      Opcode::VFIXUPIMMSS |
      Opcode::VINSERTI64X4 |
      Opcode::VMOVDQA32 |
      Opcode::VMOVDQA64 |
      Opcode::VMOVDQU32 |
      Opcode::VMOVDQU64 |
      Opcode::VPCOMPRESSQ |
      Opcode::VPCOMPRESSD |
      Opcode::VSCALEDPD |
      Opcode::VSCALEDPS |
      Opcode::VSCALEDSD |
      Opcode::VSCALEDSS |
      Opcode::VSCATTERDD |
      Opcode::VSCATTERDQ |
      Opcode::VSCATTERQD |
      Opcode::VSCATTERQQ |
      Opcode::VADDPD |
      Opcode::VADDPS |
      Opcode::VCMPPD |
      Opcode::VCMPPS |
      Opcode::VCVTDQ2PD |
      Opcode::VCVTDQ2PS |
      Opcode::VCVTPD2DQ |
      Opcode::VCVTPD2PS |
      Opcode::VCVTPS2DQ |
      Opcode::VCVTPS2PD |
      Opcode::VCVTTPD2DQ |
      Opcode::VCVTTPS2DQ |
      Opcode::VDIVPD |
      Opcode::VDIVPS |
      Opcode::VMAXPD |
      Opcode::VMAXPS |
      Opcode::VMINPD |
      Opcode::VMINPS |
      Opcode::VMOVAPD |
      Opcode::VMOVAPS |
      Opcode::VMOVDDUP |
      Opcode::VMOVDQA |
      Opcode::VMOVDQU |
      Opcode::VMOVNTDQA |
      Opcode::VMOVNTDQ |
      Opcode::VMOVNTPD |
      Opcode::VMOVNTPS |
      Opcode::VMOVSHDUP |
      Opcode::VMOVSLDUP |
      Opcode::VMOVUPD |
      Opcode::VMOVUPS |
      Opcode::VMULPD |
      Opcode::VMULPS |
      Opcode::VPANDD |
      Opcode::VPANDQ |
      Opcode::VPANDND |
      Opcode::VPANDNQ |
      Opcode::VPCMPEQD |
      Opcode::VPCMPEQQ |
      Opcode::VPCMPGTD |
      Opcode::VPCMPGTQ |
      Opcode::VPMAXSD |
      Opcode::VPMAXSQ |
      Opcode::VPMAXUD |
      Opcode::VPMAXUQ |
      Opcode::VPMINSD |
      Opcode::VPMINSQ |
      Opcode::VPMINUD |
      Opcode::VPMINUQ |
      Opcode::VPCMPD |
      Opcode::VPCMPQ |
      Opcode::VPCMPUD |
      Opcode::VPCMPUQ |
      Opcode::VPORD |
      Opcode::VPORQ |
      Opcode::VPXORD |
      Opcode::VPXORQ |
      Opcode::VPSLLD |
      Opcode::VPSLLQ |
      Opcode::VPSLLW |
      Opcode::VPSRAD |
      Opcode::VPSRAQ |
      Opcode::VALIGND |
      Opcode::VALIGNQ |
      Opcode::VBLENDMPD |
      Opcode::VBLENDMPS |
      Opcode::VPROLD |
      Opcode::VPROLQ |
      Opcode::VPROLVD |
      Opcode::VPROLVQ |
      Opcode::VPRORD |
      Opcode::VPRORQ |
      Opcode::VPRORVD |
      Opcode::VPRORVQ |
      Opcode::VPUNPCKHDQ |
      Opcode::VPUNPCKHQDQ |
      Opcode::VPUNPCKLDQ |
      Opcode::VPUNPCKLQDQ |
      Opcode::VUNPCKHPD |
      Opcode::VUNPCKHPS |
      Opcode::VUNPCKLPD |
      Opcode::VUNPCKLPS |
      Opcode::VBROADCASTF32X4 |
      Opcode::VBROADCASTF64X4 |
      Opcode::VBROADCASTI64X4 |
      Opcode::VBROADCASTI32X4 |
      Opcode::VINSERTF32X4 |
      Opcode::VINSERTI32X4 |
      Opcode::VSHUFF32X4 |
      Opcode::VSHUFF64X2 |
      Opcode::VSHUFI32X4 |
      Opcode::VSHUFI64X2 |
      Opcode::VCOMPRESSD |
      Opcode::VCOMPRESSQ |
      Opcode::VCOMPRESSPD |
      Opcode::VCOMPRESSPS |
      Opcode::VEXPANDPD |
      Opcode::VEXPANDPS |
      Opcode::VCVTPD2UDQ |
      Opcode::VCVTPS2UDQ |
      Opcode::VCVTUDQ2PD |
      Opcode::VCVTUDQ2PS |
      Opcode::VCVTTPD2UDQ |
      Opcode::VCVTTPS2UDQ |
      Opcode::VFIXUPIMMPD |
      Opcode::VFIXUPIMMPS |
      Opcode::VCVTPH2PS |
      Opcode::VCVTPS2PH |
      Opcode::VFMADD132PD |
      Opcode::VFMADD132PS |
      Opcode::VFMADD213PD |
      Opcode::VFMADD213PS |
      Opcode::VFMADD231PD |
      Opcode::VFMADD231PS |
      Opcode::VFMADDSUB132PD |
      Opcode::VFMADDSUB132PS |
      Opcode::VFMADDSUB213PD |
      Opcode::VFMADDSUB213PS |
      Opcode::VFMADDSUB231PD |
      Opcode::VFMADDSUB231PS |
      Opcode::VFMSUB132PD |
      Opcode::VFMSUB132PS |
      Opcode::VFMSUB213PD |
      Opcode::VFMSUB213PS |
      Opcode::VFMSUB231PD |
      Opcode::VFMSUB231PS |
      Opcode::VFMSUBADD132PD |
      Opcode::VFMSUBADD132PS |
      Opcode::VFMSUBADD213PD |
      Opcode::VFMSUBADD213PS |
      Opcode::VFMSUBADD231PD |
      Opcode::VFMSUBADD231PS |
      Opcode::VFNMADD132PD |
      Opcode::VFNMADD132PS |
      Opcode::VFNMADD213PD |
      Opcode::VFNMADD213PS |
      Opcode::VFNMADD231PD |
      Opcode::VFNMADD231PS |
      Opcode::VFNMSUB132PD |
      Opcode::VFNMSUB132PS |
      Opcode::VFNMSUB213PD |
      Opcode::VFNMSUB213PS |
      Opcode::VFNMSUB231PD |
      Opcode::VFNMSUB231PS |
      Opcode::VSCATTERDPS |
      Opcode::VSCATTERDPD |
      Opcode::VSCATTERQPS |
      Opcode::VSCATTERQPD |
      Opcode::VGATHERDPD |
      Opcode::VGATHERDPS |
      Opcode::VGATHERQPD |
      Opcode::VGATHERQPS |
      Opcode::VGETEXPPD |
      Opcode::VGETEXPPS |
      Opcode::VGETMANTPD |
      Opcode::VGETMANTPS |
      Opcode::VPBLENDMD |
      Opcode::VPBLENDMQ |
      Opcode::VPERMD |
      Opcode::VPERMQ |
      Opcode::VPERMI2D |
      Opcode::VPERMI2Q |
      Opcode::VPERMI2PD |
      Opcode::VPERMI2PS |
      Opcode::VPERMT2D |
      Opcode::VPERMT2Q |
      Opcode::VPERMT2PD |
      Opcode::VPERMT2PS |
      Opcode::VPEXPANDD |
      Opcode::VPEXPANDQ |
      Opcode::VPGATHERDD |
      Opcode::VPGATHERDQ |
      Opcode::VPGATHERQD |
      Opcode::VPGATHERQQ |
      Opcode::VPSCATTERDD |
      Opcode::VPSCATTERDQ |
      Opcode::VPSCATTERQD |
      Opcode::VPSCATTERQQ |
      Opcode::VPMOVDB |
      Opcode::VPMOVSDB |
      Opcode::VPMOVUSDB |
      Opcode::VPMOVDW |
      Opcode::VPMOVSDW |
      Opcode::VPMOVUSDW |
      Opcode::VPMOVQB |
      Opcode::VPMOVSQB |
      Opcode::VPMOVUSQB |
      Opcode::VPMOVQD |
      Opcode::VPMOVSQD |
      Opcode::VPMOVUSQD |
      Opcode::VPMOVQW |
      Opcode::VPMOVSQW |
      Opcode::VPMOVUSQW |
      Opcode::VPSRAVQ |
      Opcode::VPTERNLOGD |
      Opcode::VPTERNLOGQ |
      Opcode::VPTESTMD |
      Opcode::VPTESTMQ |
      Opcode::VPTESTNMD |
      Opcode::VPTESTNMQ |
      Opcode::VRCP14PD |
      Opcode::VRCP14PS |
      Opcode::VRNDSCALEPD |
      Opcode::VRNDSCALEPS |
      Opcode::VRSQRT14PD |
      Opcode::VRSQRT14PS |
      Opcode::VSCALEFPS |
      Opcode::VSCALEFPD |
      Opcode::VPABSQ |
      Opcode::VADDSD |
      Opcode::VADDSS |
      Opcode::VCMPSD |
      Opcode::VCMPSS |
      Opcode::VCOMISD |
      Opcode::VCOMISS |
      Opcode::VCVTSD2SI |
      Opcode::VCVTSD2SS |
      Opcode::VCVTSI2SS |
      Opcode::VCVTSI2SD |
      Opcode::VCVTSS2SD |
      Opcode::VCVTSS2SI |
      Opcode::VCVTTSS2SI |
      Opcode::VCVTTSD2SI |
      Opcode::VDIVSD |
      Opcode::VDIVSS |
      Opcode::VEXTRACTPS |
      Opcode::VINSERTPS |
      Opcode::VGETMANTSD |
      Opcode::VGETMANTSS |
      Opcode::VMOVD |
      Opcode::VMOVQ |
      Opcode::VMOVHLPS |
      Opcode::VMOVHPD |
      Opcode::VMOVHPS |
      Opcode::VMOVLHPS |
      Opcode::VMOVLPD |
      Opcode::VMOVLPS |
      Opcode::VMOVSS |
      Opcode::VMOVSD |
      Opcode::VSQRTSS |
      Opcode::VSQRTSD |
      Opcode::VSUBSD |
      Opcode::VSUBSS |
      Opcode::VUCOMISD |
      Opcode::VUCOMISS |
      Opcode::VRCP14SD |
      Opcode::VRCP14SS |
      Opcode::VRNDSCALESD |
      Opcode::VRNDSCALESS |
      Opcode::VRSQRT14SD |
      Opcode::VRSQRT14SS |
      Opcode::VSCALEFSS |
      Opcode::VSCALEFSD |
      Opcode::VINSERTF64X4 |
      Opcode::VCVTUSI2SD |
      Opcode::VCVTUSI2SS |
      Opcode::VCVTSD2USI |
      Opcode::VCVTSS2USI |
      Opcode::VCVTTSD2USI |
      Opcode::VCVTTSS2USI |
      Opcode::KANDW |
      Opcode::KANDNW |
      Opcode::KMOVW |
      Opcode::KNOTW |
      Opcode::KORW |
      Opcode::KORTESTW |
      Opcode::KSHIFTLW |
      Opcode::KSHIFTRW |
      Opcode::KUNPCKBW |
      Opcode::KXNORW |
      Opcode::KXORW => {
        if !decoder.feature_avx512_f() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MONITORX |
      Opcode::MWAITX => {
        if !decoder.feature_emx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PCLMULQDQ => {
        if !decoder.feature_pclmulqdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::VFNMSUB231SS => {
        if !decoder.feature_fma3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XSAVEOPT => {
        if !decoder.feature_xsaveopt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SYSCALL |
      Opcode::SYSRET => {
        if !decoder.feature_syscall() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::EXTRQ |
      Opcode::INSERTQ |
      Opcode::MOVNTSD |
      Opcode::MOVNTSS => {
        if !decoder.feature_sse4a() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPOPCNTD |
      Opcode::VPOPCNTQ => {
        if !decoder.feature_avx512_vpopcntdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVEPT |
      Opcode::INVVPID |
      Opcode::INVPCID => {
        if !decoder.feature_invpcid() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BOUND => {
        if !decoder.feature_80186_bound() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XSUSLDTRK |
      Opcode::XRESLDTRK => {
        if !decoder.feature_tsxldtrk() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::CMOVZ => {
        if !decoder.feature_cmov() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::KANDQ |
      Opcode::KANDD |
      Opcode::KANDNQ |
      Opcode::KANDND |
      Opcode::KMOVD |
      Opcode::KMOVQ |
      Opcode::KNOTD |
      Opcode::KNOTQ |
      Opcode::KORD |
      Opcode::KORQ |
      Opcode::KADDD |
      Opcode::KTESTD |
      Opcode::KADDQ |
      Opcode::KTESTQ |
      Opcode::KORTESTD |
      Opcode::KORTESTQ |
      Opcode::KSHIFTLD |
      Opcode::KSHIFTRD |
      Opcode::KSHIFTLQ |
      Opcode::KSHIFTRQ |
      Opcode::KUNPCKWD |
      Opcode::KUNPCKDQ |
      Opcode::KXNORD |
      Opcode::KXNORQ |
      Opcode::KXORD |
      Opcode::KXORQ |
      Opcode::VPCMPEQB |
      Opcode::VPCMPEQW |
      Opcode::VPCMPGTB |
      Opcode::VPCMPGTW |
      Opcode::VPSUBUSB |
      Opcode::VPSUBUSW |
      Opcode::VPERMW |
      Opcode::VPERMI2W |
      Opcode::VPERMT2W |
      Opcode::VPSLLVW |
      Opcode::VPSRAVW |
      Opcode::VPSRLVW |
      Opcode::VPEXTRB |
      Opcode::VPEXTRW |
      Opcode::VPINSRB |
      Opcode::VPINSRW |
      Opcode::VPMULHUW |
      Opcode::VPMULHRSW |
      Opcode::VPADDSB |
      Opcode::VPADDSW |
      Opcode::VPADDUSB |
      Opcode::VPADDUSW |
      Opcode::VPALIGNR |
      Opcode::VPMOVD2M |
      Opcode::VPMOVQ2M |
      Opcode::VPMOVWB |
      Opcode::VDBPSADBW |
      Opcode::VMOVDQU8 |
      Opcode::VMOVDQU16 |
      Opcode::VPBLENDMB |
      Opcode::VPBLENDMW |
      Opcode::VPCMPB |
      Opcode::VPCMPUB |
      Opcode::VPCMPW |
      Opcode::VPCMPUW |
      Opcode::VPERMW |
      Opcode::VPERMI2B |
      Opcode::VPERMI2W |
      Opcode::VPMOVM2B |
      Opcode::VPMOVM2W |
      Opcode::VPMOVB2M |
      Opcode::VPMOVW2M |
      Opcode::VPMOVSWB |
      Opcode::VPMOVUSWB |
      Opcode::VPSLLVW |
      Opcode::VPSRAVW |
      Opcode::VPSRLVW |
      Opcode::VPTESTNMB |
      Opcode::VPTESTNMW |
      Opcode::VPTESTMB |
      Opcode::VPTESTMW => {
        if !decoder.feature_avx512_bw() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PALIGNR |
      Opcode::PSIGNW |
      Opcode::PSIGND |
      Opcode::PSIGNB |
      Opcode::PSHUFB |
      Opcode::PMULHRSW |
      Opcode::PMADDUBSW |
      Opcode::PABSD |
      Opcode::PABSW |
      Opcode::PABSB |
      Opcode::PHSUBSW |
      Opcode::PHSUBW |
      Opcode::PHSUBD |
      Opcode::PHADDD |
      Opcode::PHADDSW |
      Opcode::PHADDW => {
        if !decoder.feature_ssse3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLFLUSHOPT => {
        if !decoder.feature_clflushopt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PUSHA |
      Opcode::POPA => {
        if !decoder.feature_80186_pusha() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::GETSEC => {
        if !decoder.feature_smx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::AAA |
      Opcode::AAD |
      Opcode::AAM |
      Opcode::AAS |
      Opcode::DAA |
      Opcode::DAS => {
        if !decoder.feature_8086_bcd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::POPCNT |
      Opcode::LZCNT => {
        if !decoder.feature_abm() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PREFETCHW => {
        if !decoder.feature_3dnowprefetch() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPID => {
        if !decoder.feature_rdpid() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::LAHF |
      Opcode::SAHF => {
        if !decoder.feature_lahfsahf() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::JCXZ => {
        if !decoder.feature_jcxz() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ENQCMD |
      Opcode::ENQCMDS => {
        if !decoder.feature_enqcmd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PTWRITE => {
        if !decoder.feature_ptwrite() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPERMT2B |
      Opcode::VPERMB => {
        if !decoder.feature_avx512_vbmi() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::F2XM1 |
      Opcode::FABS |
      Opcode::FADD |
      Opcode::FADDP |
      Opcode::FBLD |
      Opcode::FBSTP |
      Opcode::FCHS |
      Opcode::FCMOVB |
      Opcode::FCMOVBE |
      Opcode::FCMOVE |
      Opcode::FCMOVNB |
      Opcode::FCMOVNBE |
      Opcode::FCMOVNE |
      Opcode::FCMOVNU |
      Opcode::FCMOVU |
      Opcode::FCOM |
      Opcode::FCOMI |
      Opcode::FCOMIP |
      Opcode::FCOMP |
      Opcode::FCOMPP |
      Opcode::FCOS |
      Opcode::FDECSTP |
      Opcode::FDISI8087_NOP |
      Opcode::FDIV |
      Opcode::FDIVP |
      Opcode::FDIVR |
      Opcode::FDIVRP |
      Opcode::FENI8087_NOP |
      Opcode::FFREE |
      Opcode::FFREEP |
      Opcode::FIADD |
      Opcode::FICOM |
      Opcode::FICOMP |
      Opcode::FIDIV |
      Opcode::FIDIVR |
      Opcode::FILD |
      Opcode::FIMUL |
      Opcode::FINCSTP |
      Opcode::FIST |
      Opcode::FISTP |
      Opcode::FISTTP |
      Opcode::FISUB |
      Opcode::FISUBR |
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
      Opcode::FMUL |
      Opcode::FMULP |
      Opcode::FNCLEX |
      Opcode::FNINIT |
      Opcode::FNOP |
      Opcode::FNSAVE |
      Opcode::FNSTCW |
      Opcode::FNSTENV |
      Opcode::FNSTOR |
      Opcode::FNSTSW |
      Opcode::FPATAN |
      Opcode::FPREM |
      Opcode::FPREM1 |
      Opcode::FPTAN |
      Opcode::FRNDINT |
      Opcode::FRSTOR |
      Opcode::FSCALE |
      Opcode::FSETPM287_NOP |
      Opcode::FSIN |
      Opcode::FSINCOS |
      Opcode::FSQRT |
      Opcode::FST |
      Opcode::FSTP |
      Opcode::FSTPNCE |
      Opcode::FSUB |
      Opcode::FSUBP |
      Opcode::FSUBR |
      Opcode::FSUBRP |
      Opcode::FTST |
      Opcode::FUCOM |
      Opcode::FUCOMI |
      Opcode::FUCOMIP |
      Opcode::FUCOMP |
      Opcode::FUCOMPP |
      Opcode::FXAM |
      Opcode::FXCH |
      Opcode::FXTRACT |
      Opcode::FYL2X |
      Opcode::FYL2XP1 => {
        if !decoder.feature_x87() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPMC |
      Opcode::PUNPCKLBW |
      Opcode::PUNPCKLWD |
      Opcode::PUNPCKLDQ |
      Opcode::PACKSSWB |
      Opcode::PCMPGTB |
      Opcode::PCMPGTD |
      Opcode::PCMPGTW |
      Opcode::PACKUSWB |
      Opcode::PUNPCKHBW |
      Opcode::PUNPCKHWD |
      Opcode::PUNPCKHDQ |
      Opcode::PACKSSDW |
      Opcode::MOVD |
      Opcode::MOVQ |
      Opcode::PCMPEQB |
      Opcode::PCMPEQD |
      Opcode::PCMPEQW |
      Opcode::PSRLW |
      Opcode::PSRLD |
      Opcode::PSRLQ |
      Opcode::PMULLW |
      Opcode::PSUBUSB |
      Opcode::PSUBUSW |
      Opcode::PAND |
      Opcode::PADDUSB |
      Opcode::PADDUSW |
      Opcode::PANDN |
      Opcode::PSRAW |
      Opcode::PSRAD |
      Opcode::PMULHW |
      Opcode::PSUBSB |
      Opcode::PSUBSW |
      Opcode::POR |
      Opcode::PADDSB |
      Opcode::PADDSW |
      Opcode::PXOR |
      Opcode::PSLLW |
      Opcode::PSLLD |
      Opcode::PSLLQ |
      Opcode::PMADDWD |
      Opcode::PSUBB |
      Opcode::PSUBW |
      Opcode::PSUBD |
      Opcode::PADDB |
      Opcode::PADDW |
      Opcode::PADDD |
      Opcode::EMMS => {
        if !decoder.feature_mmx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPOPCNTD |
      Opcode::VPOPCNTQ |
      Opcode::VPOPCNTB |
      Opcode::VPOPCNTW |
      Opcode::VPSHUFBITQMB |
      Opcode::VPMULTISHIFTQB => {
        if !decoder.feature_avx512_bitalg() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::LOADIWKEY => {
        if !decoder.feature_keylocker() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ENTER |
      Opcode::LEAVE |
      Opcode::INS |
      Opcode::OUTS => {
        if !decoder.feature_80186() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLWB => {
        if !decoder.feature_clwb() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::V4FNMADDSS |
      Opcode::V4FNMADDPS |
      Opcode::V4FMADDSS |
      Opcode::V4FMADDPS => {
        if !decoder.feature_avx512_4fmaps() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::TDCALL |
      Opcode::SEAMRET |
      Opcode::SEAMOPS |
      Opcode::SEAMCALL => {
        if !decoder.feature_tdx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::POPCNT => {
        if !decoder.feature_popcnt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PSMASH |
      Opcode::PVALIDATE |
      Opcode::RMPADJUST |
      Opcode::RMPUPDATE => {
        if !decoder.feature_snp() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VGF2P8AFFINEQB |
      Opcode::VGF2P8AFFINEINVQB |
      Opcode::VGF2P8MULB => {
        if !decoder.feature_avx512_gfni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::FEMMS |
      Opcode::PI2FW |
      Opcode::PI2FD |
      Opcode::PF2IW |
      Opcode::PF2ID |
      Opcode::PMULHRW |
      Opcode::PFCMPGE |
      Opcode::PFMIN |
      Opcode::PFRCP |
      Opcode::PFRSQRT |
      Opcode::PFSUB |
      Opcode::PFADD |
      Opcode::PFCMPGT |
      Opcode::PFMAX |
      Opcode::PFRCPIT1 |
      Opcode::PFRSQIT1 |
      Opcode::PFSUBR |
      Opcode::PFACC |
      Opcode::PFCMPEQ |
      Opcode::PFMUL |
      Opcode::PFMULHRW |
      Opcode::PFRCPIT2 |
      Opcode::PFNACC |
      Opcode::PFPNACC |
      Opcode::PSWAPD |
      Opcode::PAVGUSB => {
        if !decoder.feature_3dnow() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPCOMPRESSB |
      Opcode::VPCOMPRESSW |
      Opcode::VPSHLDVW |
      Opcode::VPSHLDW |
      Opcode::VPEXPANDB |
      Opcode::VPEXPANDW |
      Opcode::VPSHRDVW |
      Opcode::VPSHRDW |
      Opcode::VPSHLDVQ |
      Opcode::VPSHLDVD |
      Opcode::VPSHLDQ |
      Opcode::VPSHLDD |
      Opcode::VPSHRDQ |
      Opcode::VPSHRDD |
      Opcode::VPSHRDVQ |
      Opcode::VPSHRDVD => {
        if !decoder.feature_avx512_vbmi2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CMC |
      Opcode::CLC |
      Opcode::STC |
      Opcode::CLI |
      Opcode::STI |
      Opcode::CLD |
      Opcode::STD |
      Opcode::ADD |
      Opcode::OR |
      Opcode::ADC |
      Opcode::SBB |
      Opcode::AND |
      Opcode::XOR |
      Opcode::SUB |
      Opcode::CMP |
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
      Opcode::HLT |
      Opcode::CALL |
      Opcode::CALLF |
      Opcode::JMP |
      Opcode::JMPF |
      Opcode::PUSH |
      Opcode::POP |
      Opcode::LEA |
      Opcode::NOP |
      Opcode::XCHG |
      Opcode::POPF |
      Opcode::INT |
      Opcode::INTO |
      Opcode::IRET |
      Opcode::IRETD |
      Opcode::IRETQ |
      Opcode::RETF |
      Opcode::ENTER |
      Opcode::LEAVE |
      Opcode::MOV |
      Opcode::RETURN |
      Opcode::PUSHF |
      Opcode::WAIT |
      Opcode::CBW |
      Opcode::CWD |
      Opcode::CQO |
      Opcode::LODS |
      Opcode::STOS |
      Opcode::CMPS |
      Opcode::SCAS |
      Opcode::MOVS |
      Opcode::TEST |
      Opcode::IN |
      Opcode::OUT |
      Opcode::IMUL |
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
      Opcode::JG |
      Opcode::UD0 |
      Opcode::UD1 |
      Opcode::UD2 |
      Opcode::DIV |
      Opcode::IDIV |
      Opcode::MUL |
      Opcode::NEG |
      Opcode::NOT |
      Opcode::XLAT |
      Opcode::LOOPNZ |
      Opcode::LOOPZ |
      Opcode::LOOP |
      Opcode::SALC => {
        if !decoder.feature_8086() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLGI |
      Opcode::STGI |
      Opcode::SKINIT |
      Opcode::VMLOAD |
      Opcode::VMMCALL |
      Opcode::VMSAVE |
      Opcode::VMRUN |
      Opcode::INVLPGA => {
        if !decoder.feature_svm() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ADDSUBPD |
      Opcode::ADDSUBPS |
      Opcode::HSUBPD |
      Opcode::HADDPD |
      Opcode::MOVSLDUP |
      Opcode::MOVSHDUP |
      Opcode::MOVDDUP |
      Opcode::HADDPS |
      Opcode::HSUBPS |
      Opcode::LDDQU => {
        if !decoder.feature_sse3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVUPD |
      Opcode::PSRLDQ |
      Opcode::PSLLDQ |
      Opcode::MOVSD |
      Opcode::MOVLPD |
      Opcode::UNPCKLPD |
      Opcode::UNPCKHPD |
      Opcode::MOVHPD |
      Opcode::MOVAPD |
      Opcode::MOVMSKPD |
      Opcode::CVTPI2PD |
      Opcode::CVTSI2SD |
      Opcode::MOVNTPD |
      Opcode::MOVNTI |
      Opcode::MOVNTDQ |
      Opcode::CVTTPD2PI |
      Opcode::CVTTSD2SI |
      Opcode::CVTPD2PI |
      Opcode::CVTSD2SI |
      Opcode::UCOMISD |
      Opcode::COMISD |
      Opcode::SQRTPD |
      Opcode::SQRTSD |
      Opcode::ANDPD |
      Opcode::ANDNPD |
      Opcode::ORPD |
      Opcode::XORPD |
      Opcode::ADDPD |
      Opcode::ADDSD |
      Opcode::MULSD |
      Opcode::MULPD |
      Opcode::CVTPS2PD |
      Opcode::CVTPD2PS |
      Opcode::CVTSS2SD |
      Opcode::CVTSD2SS |
      Opcode::CVTPS2DQ |
      Opcode::CVTDQ2PS |
      Opcode::CVTTPS2DQ |
      Opcode::SUBSD |
      Opcode::SUBPD |
      Opcode::MINPD |
      Opcode::MINSD |
      Opcode::DIVPD |
      Opcode::DIVSD |
      Opcode::MAXPD |
      Opcode::MAXSD |
      Opcode::PUNPCKLQDQ |
      Opcode::PUNPCKHQDQ |
      Opcode::MOVDQA |
      Opcode::MOVDQU |
      Opcode::PSHUFHW |
      Opcode::PSHUFLW |
      Opcode::PSHUFD |
      Opcode::LFENCE |
      Opcode::MFENCE |
      Opcode::CLFLUSH |
      Opcode::CMPPD |
      Opcode::CMPPS |
      Opcode::CMPSD |
      Opcode::SHUFPD |
      Opcode::PADDQ |
      Opcode::MOVQ2DQ |
      Opcode::MOVDQ2Q |
      Opcode::CVTPD2DQ |
      Opcode::CVTTPD2DQ |
      Opcode::CVTDQ2PD |
      Opcode::PMULUDQ |
      Opcode::MASKMOVDQU |
      Opcode::PSUBQ => {
        if !decoder.feature_sse2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VEXP2PD |
      Opcode::VEXP2PS |
      Opcode::VEXP2SD |
      Opcode::VEXP2SS |
      Opcode::VRCP28PD |
      Opcode::VRCP28PS |
      Opcode::VRCP28SD |
      Opcode::VRCP28SS |
      Opcode::VRSQRT28PD |
      Opcode::VRSQRT28PS |
      Opcode::VRSQRT28SD |
      Opcode::VRSQRT28SS => {
        if !decoder.feature_avx512_er() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XRSTORS64 |
      Opcode::XSAVEC64 |
      Opcode::XSAVES64 => {
        if !decoder.feature_xsave64() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ARPL => {
        if !decoder.feature_80286_arpl() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ANDN |
      Opcode::BEXTR |
      Opcode::BLSI |
      Opcode::BLSMSK |
      Opcode::BLSR |
      Opcode::TZCNT => {
        if !decoder.feature_bmi1() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BZHI |
      Opcode::MULX |
      Opcode::PDEP |
      Opcode::PEXT |
      Opcode::RORX |
      Opcode::SARX |
      Opcode::SHRX |
      Opcode::SHLX => {
        if !decoder.feature_bmi2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVDIRI |
      Opcode::MOVDIR64B => {
        if !decoder.feature_movdir() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VAESDEC |
      Opcode::VAESDECLAST |
      Opcode::VAESENC |
      Opcode::VAESENCLAST |
      Opcode::VAESIMC |
      Opcode::VAESKEYGENASSIST => {
        if !decoder.feature_vaes() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XGETBV |
      Opcode::XRSTOR |
      Opcode::XRSTORS |
      Opcode::XSAVE |
      Opcode::XSAVEC |
      Opcode::XSAVES |
      Opcode::XSETBV => {
        if !decoder.feature_xsave() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDRAND => {
        if !decoder.feature_rdrand() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLZERO => {
        if !decoder.feature_clzero() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDSEED => {
        if !decoder.feature_rdseed() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
    }
    Ok(())
  }
}

pub(crate) mod protected_mode {
  use crate::generated::protected_mode::Opcode;
  use crate::protected_mode::{InstDecoder, Instruction, DecodeError};

  use yaxpeax_arch::{Colorize, YaxColors};
  use core::fmt;
  impl InstDecoder {
    fn feature_sgx(&self) -> bool {
      true
    }
    fn feature_smap(&self) -> bool {
      true
    }
    fn feature_avx_unimplemented(&self) -> bool {
      true
    }
    fn feature_uintr(&self) -> bool {
      true
    }
    fn feature_pentium(&self) -> bool {
      true
    }
    fn feature_aesni(&self) -> bool {
      true
    }
    fn feature_invlpgb(&self) -> bool {
      true
    }
    fn feature_fsgsbase(&self) -> bool {
      true
    }
    fn feature_vmx(&self) -> bool {
      true
    }
    fn feature_simd(&self) -> bool {
      true
    }
    fn feature_avx(&self) -> bool {
      true
    }
    fn feature_hreset(&self) -> bool {
      true
    }
    fn feature_avx512_vp2intersect(&self) -> bool {
      true
    }
    fn feature_rdpru(&self) -> bool {
      true
    }
    fn feature_sse4_2(&self) -> bool {
      true
    }
    fn feature_monitor(&self) -> bool {
      true
    }
    fn feature_avx512_f_typo(&self) -> bool {
      true
    }
    fn feature_sse4_1(&self) -> bool {
      true
    }
    fn feature_avx512_dq(&self) -> bool {
      true
    }
    fn feature_adx(&self) -> bool {
      true
    }
    fn feature_fxsr(&self) -> bool {
      true
    }
    fn feature_tsx(&self) -> bool {
      true
    }
    fn feature_waitpkg(&self) -> bool {
      true
    }
    fn feature_80286(&self) -> bool {
      true
    }
    fn feature_sysenter(&self) -> bool {
      true
    }
    fn feature_invpcid_unimplemented(&self) -> bool {
      true
    }
    fn feature_avx512_4vnniw(&self) -> bool {
      true
    }
    fn feature_avx512_vnni(&self) -> bool {
      true
    }
    fn feature_avx512_pf(&self) -> bool {
      true
    }
    fn feature_avx512_cd(&self) -> bool {
      true
    }
    fn feature_avx512_bf16(&self) -> bool {
      true
    }
    fn feature_avx512bw_unimplemented(&self) -> bool {
      true
    }
    fn feature_mpk(&self) -> bool {
      true
    }
    fn feature_avx512_f__vl_unimplemented(&self) -> bool {
      true
    }
    fn feature_sha(&self) -> bool {
      true
    }
    fn feature_cet(&self) -> bool {
      true
    }
    fn feature_80486(&self) -> bool {
      true
    }
    fn feature_sse(&self) -> bool {
      true
    }
    fn feature_pentium_pro(&self) -> bool {
      true
    }
    fn feature_itanium(&self) -> bool {
      true
    }
    fn feature_mpx(&self) -> bool {
      true
    }
    fn feature_extra_instructions(&self) -> bool {
      true
    }
    fn feature_rdtscp(&self) -> bool {
      true
    }
    fn feature_avx2(&self) -> bool {
      true
    }
    fn feature_pconfig(&self) -> bool {
      true
    }
    fn feature_avx512_ifma(&self) -> bool {
      true
    }
    fn feature_gfni(&self) -> bool {
      true
    }
    fn feature_vpclmulqdq(&self) -> bool {
      true
    }
    fn feature_80386(&self) -> bool {
      true
    }
    fn feature_movbe(&self) -> bool {
      true
    }
    fn feature_avx512_f(&self) -> bool {
      true
    }
    fn feature_emx(&self) -> bool {
      true
    }
    fn feature_pclmulqdq(&self) -> bool {
      true
    }
    fn feature_fma3(&self) -> bool {
      true
    }
    fn feature_fma4(&self) -> bool {
      true
    }
    fn feature_xsaveopt(&self) -> bool {
      true
    }
    fn feature_syscall(&self) -> bool {
      true
    }
    fn feature_sse4a(&self) -> bool {
      true
    }
    fn feature_avx512_vpopcntdq(&self) -> bool {
      true
    }
    fn feature_invpcid(&self) -> bool {
      true
    }
    fn feature_80186_bound(&self) -> bool {
      true
    }
    fn feature_tsxldtrk(&self) -> bool {
      true
    }
    fn feature_cmov(&self) -> bool {
      true
    }
    fn feature_avx512_bw(&self) -> bool {
      true
    }
    fn feature_ssse3(&self) -> bool {
      true
    }
    fn feature_clflushopt(&self) -> bool {
      true
    }
    fn feature_80186_pusha(&self) -> bool {
      true
    }
    fn feature_smx(&self) -> bool {
      true
    }
    fn feature_8086_bcd(&self) -> bool {
      true
    }
    fn feature_abm(&self) -> bool {
      true
    }
    fn feature_3dnowprefetch(&self) -> bool {
      true
    }
    fn feature_rdpid(&self) -> bool {
      true
    }
    fn feature_lahfsahf(&self) -> bool {
      true
    }
    fn feature_enqcmd(&self) -> bool {
      true
    }
    fn feature_jecxz(&self) -> bool {
      true
    }
    fn feature_new(&self) -> bool {
      true
    }
    fn feature_ptwrite(&self) -> bool {
      true
    }
    fn feature_avx512_vbmi(&self) -> bool {
      true
    }
    fn feature_x87(&self) -> bool {
      true
    }
    fn feature_mmx(&self) -> bool {
      true
    }
    fn feature_avx512_bitalg(&self) -> bool {
      true
    }
    fn feature_keylocker(&self) -> bool {
      true
    }
    fn feature_80186(&self) -> bool {
      true
    }
    fn feature_clwb(&self) -> bool {
      true
    }
    fn feature_avx512_4fmaps(&self) -> bool {
      true
    }
    fn feature_tdx(&self) -> bool {
      true
    }
    fn feature_popcnt(&self) -> bool {
      true
    }
    fn feature_snp(&self) -> bool {
      true
    }
    fn feature_avx512_gfni(&self) -> bool {
      true
    }
    fn feature_3dnow(&self) -> bool {
      true
    }
    fn feature_avx512_vbmi2(&self) -> bool {
      true
    }
    fn feature_8086(&self) -> bool {
      true
    }
    fn feature_svm(&self) -> bool {
      true
    }
    fn feature_sse3(&self) -> bool {
      true
    }
    fn feature_sse2(&self) -> bool {
      true
    }
    fn feature_avx512_er(&self) -> bool {
      true
    }
    fn feature_xsave64(&self) -> bool {
      true
    }
    fn feature_80286_arpl(&self) -> bool {
      true
    }
    fn feature_bmi1(&self) -> bool {
      true
    }
    fn feature_bmi2(&self) -> bool {
      true
    }
    fn feature_movdir(&self) -> bool {
      true
    }
    fn feature_vaes(&self) -> bool {
      true
    }
    fn feature_xsave(&self) -> bool {
      true
    }
    fn feature_rdrand(&self) -> bool {
      true
    }
    fn feature_clzero(&self) -> bool {
      true
    }
    fn feature_rdseed(&self) -> bool {
      true
    }
  }
  impl<T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Opcode {
    fn colorize(&self, colors: &Y, out: &mut T) -> fmt::Result {
      self.to_generic().colorize(colors, out)
    }
  }
  impl Opcode {
    pub fn name(&self) -> &'static str {
      self.to_generic().name()
    }
  }
  pub(crate) fn revise_instruction(decoder: &InstDecoder, inst: &mut Instruction) -> Result<(), DecodeError> {
    if inst.prefixes.evex().is_some() {
      if !decoder.avx512() {
        return Err(DecodeError::InvalidOpcode);
      } else {
        return Ok(());
      }
    }

    // for some instructions (tzcnt), not having an extension means the instruction is
    // interpreted as another, rather than being simply rejected.
    // we might still reject the alternate instruction later, if the extension adding *it*
    // is also not supported.
    if inst.opcode == Opcode::TZCNT {
      if !decoder.bmi1() {
        // tzcnt is only supported if bmi1 is enabled. without bmi1, this decodes as bsf.
        inst.opcode = Opcode::BSF;
      }
    }

    match inst.opcode {
      // we'll never be rejecting the instruction `Invalid`
      Opcode::Invalid => {}
      Opcode::ENCLS |
      Opcode::ENCLU |
      Opcode::ENCLV => {
        if !decoder.feature_sgx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLAC |
      Opcode::STAC => {
        if !decoder.feature_smap() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMAXUB => {
        if !decoder.feature_avx_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::UIRET |
      Opcode::TESTUI |
      Opcode::CLUI |
      Opcode::STUI |
      Opcode::SENDUIPI => {
        if !decoder.feature_uintr() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CPUID |
      Opcode::WRMSR |
      Opcode::RDTSC |
      Opcode::RDMSR |
      Opcode::RSM |
      Opcode::CMPXCHG8B => {
        if !decoder.feature_pentium() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::AESKEYGENASSIST |
      Opcode::AESIMC |
      Opcode::AESENC |
      Opcode::AESENCLAST |
      Opcode::AESDEC |
      Opcode::AESDECLAST => {
        if !decoder.feature_aesni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVLPGB |
      Opcode::TLBSYNC => {
        if !decoder.feature_invlpgb() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDFSBASE |
      Opcode::RDGSBASE |
      Opcode::WRFSBASE |
      Opcode::WRGSBASE => {
        if !decoder.feature_fsgsbase() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VMPTRLD |
      Opcode::VMPTRST |
      Opcode::VMCLEAR |
      Opcode::VMREAD |
      Opcode::VMWRITE |
      Opcode::VMCALL |
      Opcode::VMLAUNCH |
      Opcode::VMRESUME |
      Opcode::VMXOFF |
      Opcode::VMXON |
      Opcode::INVEPT |
      Opcode::INVVPID |
      Opcode::VMFUNC => {
        if !decoder.feature_vmx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VLDDQU |
      Opcode::VPADDB |
      Opcode::VPADDD |
      Opcode::VPADDQ |
      Opcode::VPADDW |
      Opcode::VPABSB |
      Opcode::VPABSW |
      Opcode::VPABSD |
      Opcode::VMAXSD |
      Opcode::VMAXSS |
      Opcode::VMINSD |
      Opcode::VMINSS |
      Opcode::VMULSD |
      Opcode::VMULSS |
      Opcode::VRCPSS |
      Opcode::VPSUBUSB |
      Opcode::VPSUBUSW |
      Opcode::VXORPD |
      Opcode::VXORPS |
      Opcode::VPAVGB |
      Opcode::VPAVGW |
      Opcode::VANDNPD |
      Opcode::VANDNPS |
      Opcode::VPHADDD |
      Opcode::VPHADDSW |
      Opcode::VPHADDW |
      Opcode::VANDPD |
      Opcode::VANDPS |
      Opcode::VPADDSB |
      Opcode::VPADDSW |
      Opcode::VPADDUSB |
      Opcode::VPADDUSW |
      Opcode::VPHSUBD |
      Opcode::VPHSUBSW |
      Opcode::VPHSUBW |
      Opcode::VHADDPD |
      Opcode::VHADDPS |
      Opcode::VHSUBPD |
      Opcode::VHSUBPS |
      Opcode::VORPD |
      Opcode::VORPS |
      Opcode::VPBLENDVB |
      Opcode::VPBLENDW |
      Opcode::VADDSUBPD |
      Opcode::VADDSUBPS |
      Opcode::VRSQRTPS |
      Opcode::VRSQRTSS |
      Opcode::VPALIGNR |
      Opcode::VPSUBSB |
      Opcode::VPSUBSW |
      Opcode::VPMULHUW |
      Opcode::VPMULHW |
      Opcode::VPMULLW |
      Opcode::VPSHUFB |
      Opcode::VPSHUFHW |
      Opcode::VPSHUFLW |
      Opcode::VPHMINPOSUW |
      Opcode::VRCPPS |
      Opcode::VMPSADBW |
      Opcode::VPMADDUBSW |
      Opcode::VPMADDWD |
      Opcode::VMASKMOVDQU |
      Opcode::VPMOVMSKB |
      Opcode::VPSADBW |
      Opcode::VPSLLDQ |
      Opcode::VPSRLDQ |
      Opcode::VDPPD |
      Opcode::VDPPS |
      Opcode::VLDMXCSR |
      Opcode::VSTMXCSR |
      Opcode::VMOVMSKPD |
      Opcode::VMOVMSKPS |
      Opcode::VPTEST |
      Opcode::VTESTPD |
      Opcode::VTESTPS |
      Opcode::VPEXTRB |
      Opcode::VPEXTRW |
      Opcode::VPEXTRD |
      Opcode::VPEXTRQ |
      Opcode::VPACKSSDW |
      Opcode::VPACKUSDW |
      Opcode::VPACKSSWB |
      Opcode::VPACKUSWB |
      Opcode::VBLENDPD |
      Opcode::VBLENDPS |
      Opcode::VBLENDVPD |
      Opcode::VBLENDVPS |
      Opcode::VPMULHRSW |
      Opcode::VPAND |
      Opcode::VPANDN |
      Opcode::VPOR |
      Opcode::VPCMPEQB |
      Opcode::VPCMPEQD |
      Opcode::VPCMPEQQ |
      Opcode::VPCMPEQW |
      Opcode::VPCMPGTB |
      Opcode::VPCMPGTD |
      Opcode::VPCMPGTQ |
      Opcode::VPCMPGTW |
      Opcode::VPMAXSB |
      Opcode::VPMAXSD |
      Opcode::VPMAXSW |
      Opcode::VPMAXUB |
      Opcode::VPMAXUW |
      Opcode::VPMINSB |
      Opcode::VPMINSW |
      Opcode::VPMINUB |
      Opcode::VPMINUW |
      Opcode::VPMAXUD |
      Opcode::VPMINSD |
      Opcode::VPSIGNB |
      Opcode::VPSIGNW |
      Opcode::VPSIGND |
      Opcode::VPINSRB |
      Opcode::VPINSRW |
      Opcode::VPINSRD |
      Opcode::VPINSRQ |
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
      Opcode::VPMULDQ |
      Opcode::VPMULLQ |
      Opcode::VPMULLD |
      Opcode::VPMULUDQ |
      Opcode::VPSHUFD |
      Opcode::VSHUFPD |
      Opcode::VSHUFPS |
      Opcode::VSQRTPD |
      Opcode::VSQRTPS |
      Opcode::VSQRTSS |
      Opcode::VSQRTSD |
      Opcode::VSUBPD |
      Opcode::VSUBPS |
      Opcode::VSUBSD |
      Opcode::VSUBSS |
      Opcode::VUCOMISD |
      Opcode::VUCOMISS |
      Opcode::VROUNDPD |
      Opcode::VROUNDPS |
      Opcode::VROUNDSD |
      Opcode::VROUNDSS |
      Opcode::VPSLLD |
      Opcode::VPSLLQ |
      Opcode::VPSLLW |
      Opcode::VPSRAD |
      Opcode::VPSRAW |
      Opcode::VPSRLD |
      Opcode::VPSRLQ |
      Opcode::VPSRLW |
      Opcode::VPSUBB |
      Opcode::VPSUBW |
      Opcode::VPSUBD |
      Opcode::VPSUBQ |
      Opcode::VPXOR |
      Opcode::VPUNPCKHBW |
      Opcode::VPUNPCKHWD |
      Opcode::VPUNPCKHDQ |
      Opcode::VPUNPCKHQDQ |
      Opcode::VPUNPCKLBW |
      Opcode::VPUNPCKLWD |
      Opcode::VPUNPCKLDQ |
      Opcode::VPUNPCKLQDQ |
      Opcode::VUNPCKHPD |
      Opcode::VUNPCKHPS |
      Opcode::VUNPCKLPD |
      Opcode::VUNPCKLPS |
      Opcode::VPCMPESTRI |
      Opcode::VPCMPESTRM |
      Opcode::VPCMPISTRI |
      Opcode::VPCMPISTRM |
      Opcode::VBROADCASTSS |
      Opcode::VBROADCASTSD |
      Opcode::VBROADCASTF128 |
      Opcode::VBROADCASTSD |
      Opcode::VBROADCASTSS |
      Opcode::VINSERTF128 |
      Opcode::VEXTRACTF128 |
      Opcode::VMASKMOVPD |
      Opcode::VMASKMOVPS |
      Opcode::VPERMILPD |
      Opcode::VPERMILPS |
      Opcode::VPERM2F128 |
      Opcode::VZEROUPPER |
      Opcode::VZEROALL => {
        if !decoder.feature_avx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::HRESET => {
        if !decoder.feature_hreset() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VP2INTERSECTD |
      Opcode::VP2INTERSECTQ => {
        if !decoder.feature_avx512_vp2intersect() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPRU => {
        if !decoder.feature_rdpru() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CRC32 |
      Opcode::PCMPESTRI |
      Opcode::PCMPESTRM |
      Opcode::PCMPISTRI |
      Opcode::PCMPISTRM |
      Opcode::PCMPGTQ => {
        if !decoder.feature_sse4_2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MONITOR |
      Opcode::MWAIT => {
        if !decoder.feature_monitor() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPRORRD |
      Opcode::VPRORRQ => {
        if !decoder.feature_avx512_f_typo() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PHMINPOSUW |
      Opcode::PMULDQ |
      Opcode::PMULLD |
      Opcode::DPPS |
      Opcode::DPPD |
      Opcode::PACKUSDW |
      Opcode::PCMPEQQ |
      Opcode::PTEST |
      Opcode::MOVNTDQA |
      Opcode::ROUNDSS |
      Opcode::ROUNDSD |
      Opcode::ROUNDPS |
      Opcode::ROUNDPD |
      Opcode::PMAXSB |
      Opcode::PMAXSD |
      Opcode::PMAXUW |
      Opcode::PMAXUD |
      Opcode::PMINSD |
      Opcode::PMINSB |
      Opcode::PMINUD |
      Opcode::PMINUW |
      Opcode::BLENDW |
      Opcode::PBLENDW |
      Opcode::BLENDVPS |
      Opcode::BLENDVPD |
      Opcode::PBLENDVB |
      Opcode::BLENDPS |
      Opcode::BLENDPD |
      Opcode::MPSADBW |
      Opcode::PMOVZXDQ |
      Opcode::PMOVSXDQ |
      Opcode::PMOVZXBD |
      Opcode::PMOVSXBD |
      Opcode::PMOVZXWQ |
      Opcode::PMOVSXWQ |
      Opcode::PMOVZXBQ |
      Opcode::PMOVSXBQ |
      Opcode::PMOVSXWD |
      Opcode::PMOVZXWD |
      Opcode::PEXTRQ |
      Opcode::PEXTRD |
      Opcode::PEXTRB |
      Opcode::PMOVSXBW |
      Opcode::PMOVZXBW |
      Opcode::PINSRQ |
      Opcode::PINSRD |
      Opcode::PINSRB |
      Opcode::EXTRACTPS |
      Opcode::INSERTPS => {
        if !decoder.feature_sse4_1() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::KANDB |
      Opcode::KANDNB |
      Opcode::KADDB |
      Opcode::KTESTB |
      Opcode::KADDW |
      Opcode::KTESTW |
      Opcode::KMOVB |
      Opcode::KNOTB |
      Opcode::KORB |
      Opcode::KORTESTB |
      Opcode::KSHIFTLB |
      Opcode::KSHIFTRB |
      Opcode::KXNORB |
      Opcode::KXORB |
      Opcode::VBROADCASTF32X2 |
      Opcode::VBROADCASTF64X2 |
      Opcode::VBROADCASTF32X8 |
      Opcode::VBROADCASTI32X8 |
      Opcode::VBROADCASTI64X2 |
      Opcode::VBROADCASTI32X2 |
      Opcode::VEXTRACTF32X8 |
      Opcode::VEXTRACTI32X8 |
      Opcode::VGETEXPSD |
      Opcode::VGETEXPSS |
      Opcode::VXORPD |
      Opcode::VXORPS |
      Opcode::VPEXTRD |
      Opcode::VPEXTRQ |
      Opcode::VPINSRD |
      Opcode::VPINSRQ |
      Opcode::VANDNPD |
      Opcode::VANDNPS |
      Opcode::VANDPD |
      Opcode::VANDPS |
      Opcode::VORPD |
      Opcode::VORPS |
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
      Opcode::VEXTRACTF64X2 |
      Opcode::VEXTRACTI64X2 |
      Opcode::VFPCLASSPD |
      Opcode::VFPCLASSPS |
      Opcode::VFPCLASSSD |
      Opcode::VFPCLASSSS |
      Opcode::VINSERTF64X2 |
      Opcode::VINSERTF32X8 |
      Opcode::VINSERTI32X8 |
      Opcode::VINSERTI64X2 |
      Opcode::VPMOVM2D |
      Opcode::VPMOVM2Q |
      Opcode::VPMOVB2D |
      Opcode::VPMOVQ2M |
      Opcode::VRANGEPD |
      Opcode::VRANGEPS |
      Opcode::VRANGESD |
      Opcode::VRANGESS |
      Opcode::VREDUCEPD |
      Opcode::VREDUCEPS |
      Opcode::VREDUCESD |
      Opcode::VREDUCESS => {
        if !decoder.feature_avx512_dq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ADCX |
      Opcode::ADOX => {
        if !decoder.feature_adx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::FXSAVE |
      Opcode::FXRSTOR => {
        if !decoder.feature_fxsr() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XABORT |
      Opcode::XBEGIN |
      Opcode::XEND |
      Opcode::XTEST => {
        if !decoder.feature_tsx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::TPAUSE |
      Opcode::UMONITOR |
      Opcode::UMWAIT => {
        if !decoder.feature_waitpkg() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLTS |
      Opcode::LAR |
      Opcode::LGDT |
      Opcode::LIDT |
      Opcode::LLDT |
      Opcode::LMSW |
      Opcode::LSL |
      Opcode::SGDT |
      Opcode::SIDT |
      Opcode::SLDT |
      Opcode::SMSW |
      Opcode::STR |
      Opcode::LTR |
      Opcode::VERR |
      Opcode::VERW => {
        if !decoder.feature_80286() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SYSENTER |
      Opcode::SYSEXIT => {
        if !decoder.feature_sysenter() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVVPID => {
        if !decoder.feature_invpcid_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VP4DPWSSDS |
      Opcode::VP4DPWSSD => {
        if !decoder.feature_avx512_4vnniw() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPDPBUSDS |
      Opcode::VPDPBUSD |
      Opcode::VPDPWSSDS |
      Opcode::VPDPWSSD => {
        if !decoder.feature_avx512_vnni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VGATHERPF0DPD |
      Opcode::VGATHERPF0DPS |
      Opcode::VGATHERPF0QPD |
      Opcode::VGATHERPF0QPS |
      Opcode::VGATHERPF1DPD |
      Opcode::VGATHERPF1DPS |
      Opcode::VGATHERPF1QPD |
      Opcode::VGATHERPF1QPS |
      Opcode::VSCATTERPF0DPD |
      Opcode::VSCATTERPF0DPS |
      Opcode::VSCATTERPF0QPD |
      Opcode::VSCATTERPF0QPS |
      Opcode::VSCATTERPF1DPD |
      Opcode::VSCATTERPF1DPS |
      Opcode::VSCATTERPF1QPD |
      Opcode::VSCATTERPF1QPS => {
        if !decoder.feature_avx512_pf() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPBROADCASTMW2D |
      Opcode::VPBROADCASTMB2Q |
      Opcode::VPBROADCASTM |
      Opcode::VPCONFLICTD |
      Opcode::VPCONFLICTQ |
      Opcode::VPLZCNTD |
      Opcode::VPLZCNTQ => {
        if !decoder.feature_avx512_cd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VCVTNE2PS2BF16 |
      Opcode::VCVTNEPS2BF16 |
      Opcode::VDPBF16PS => {
        if !decoder.feature_avx512_bf16() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMOVQ2M => {
        if !decoder.feature_avx512bw_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPKRU |
      Opcode::WRPKRU => {
        if !decoder.feature_mpk() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VSCATTERDPS |
      Opcode::VSCATTERDPD |
      Opcode::VSCATTERQPS |
      Opcode::VSCATTERQPD => {
        if !decoder.feature_avx512_f__vl_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SHA1RNDS4 |
      Opcode::SHA1NEXTE |
      Opcode::SHA1MSG1 |
      Opcode::SHA1MSG2 |
      Opcode::SHA256RNDS2 |
      Opcode::SHA256MSG1 |
      Opcode::SHA256MSG2 => {
        if !decoder.feature_sha() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::WRUSS |
      Opcode::WRSS |
      Opcode::INCSSP |
      Opcode::SAVEPREVSSP |
      Opcode::SETSSBSY |
      Opcode::CLRSSBSY |
      Opcode::RSTORSSP |
      Opcode::ENDBR64 |
      Opcode::ENDBR32 => {
        if !decoder.feature_cet() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BSWAP |
      Opcode::CMPXCHG |
      Opcode::INVD |
      Opcode::WBINVD |
      Opcode::INVLPG |
      Opcode::XADD => {
        if !decoder.feature_80486() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVSS |
      Opcode::ADDSS |
      Opcode::SUBSS |
      Opcode::MULSS |
      Opcode::DIVSS |
      Opcode::MINSS |
      Opcode::MAXSS |
      Opcode::SQRTSS |
      Opcode::MOVUPS |
      Opcode::MOVHLPS |
      Opcode::MOVLPS |
      Opcode::MOVHPS |
      Opcode::MOVLHPS |
      Opcode::UNPCKLPS |
      Opcode::UNPCKHPS |
      Opcode::PREFETCHNTA |
      Opcode::PREFETCH0 |
      Opcode::PREFETCH1 |
      Opcode::PREFETCH2 |
      Opcode::MOVAPS |
      Opcode::CVTPI2PS |
      Opcode::CVTSI2SS |
      Opcode::MOVNTPS |
      Opcode::CVTTSS2SI |
      Opcode::CVTTPS2PI |
      Opcode::CVTSS2SI |
      Opcode::CVTPS2PI |
      Opcode::UCOMISS |
      Opcode::COMISS |
      Opcode::SQRTPS |
      Opcode::MOVMSKPS |
      Opcode::RSQRTSS |
      Opcode::RSQRTPS |
      Opcode::RCPPS |
      Opcode::RCPSS |
      Opcode::ANDPS |
      Opcode::ANDNPS |
      Opcode::XORPS |
      Opcode::ORPS |
      Opcode::ADDPS |
      Opcode::MULPS |
      Opcode::SUBPS |
      Opcode::MINPS |
      Opcode::DIVPS |
      Opcode::MAXPS |
      Opcode::PSHUFW |
      Opcode::LDMXCSR |
      Opcode::STMXCSR |
      Opcode::SFENCE |
      Opcode::CMPPS |
      Opcode::CMPSS |
      Opcode::PINSRW |
      Opcode::PEXTRW |
      Opcode::SHUFPS |
      Opcode::PMOVMSKB |
      Opcode::PMINUB |
      Opcode::PMAXUB |
      Opcode::PAVGB |
      Opcode::PAVGW |
      Opcode::PMULHUW |
      Opcode::MOVNTQ |
      Opcode::PMINSW |
      Opcode::PMAXSW |
      Opcode::PSADBW |
      Opcode::MASKMOVQ |
      Opcode::LDMXCSR |
      Opcode::STMXCSR => {
        if !decoder.feature_sse() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::UD2 => {
        if !decoder.feature_pentium_pro() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::JMPE => {
        if !decoder.feature_itanium() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BNDMK |
      Opcode::BNDCL |
      Opcode::BNDCU |
      Opcode::BNDCN |
      Opcode::BNDMOV |
      Opcode::BNDLDX |
      Opcode::BNDSTX => {
        if !decoder.feature_mpx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SWAPGS |
      Opcode::SLHD |
      Opcode::CDQE |
      Opcode::MOVSXD |
      Opcode::CMPXCHG16B => {
        if !decoder.feature_extra_instructions() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDTSCP => {
        if !decoder.feature_rdtscp() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VBROADCASTI128 |
      Opcode::VPBROADCASTB |
      Opcode::VPBROADCASTW |
      Opcode::VPBROADCASTD |
      Opcode::VPBROADCASTQ |
      Opcode::VINSERTI128 |
      Opcode::VEXTRACTI128 |
      Opcode::VPMASKMOVD |
      Opcode::VPMASKMOVQ |
      Opcode::VPERMPS |
      Opcode::VPERMD |
      Opcode::VPERMPD |
      Opcode::VPERMQ |
      Opcode::VPERM2I128 |
      Opcode::VPBLENDD |
      Opcode::VPSLLVD |
      Opcode::VPSLLVQ |
      Opcode::VPSRLVD |
      Opcode::VPSRLVQ |
      Opcode::VPSRAVD => {
        if !decoder.feature_avx2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PCONFIG => {
        if !decoder.feature_pconfig() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMADD52HUQ |
      Opcode::VPMADD52LUQ => {
        if !decoder.feature_avx512_ifma() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::GF2P8AFFINEQB |
      Opcode::GF2P8AFFINEINVQB |
      Opcode::GF2P8MULB => {
        if !decoder.feature_gfni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPCLMULQDQ => {
        if !decoder.feature_vpclmulqdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BSF |
      Opcode::BSR |
      Opcode::BT |
      Opcode::BTS |
      Opcode::BTC |
      Opcode::BTR |
      Opcode::CDQ |
      Opcode::CWDE |
      Opcode::LDS |
      Opcode::LES |
      Opcode::LFS |
      Opcode::LGS |
      Opcode::LSS |
      Opcode::MOVZX |
      Opcode::MOVSX |
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
      Opcode::SETG |
      Opcode::SHLD |
      Opcode::SHRD => {
        if !decoder.feature_80386() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVBE => {
        if !decoder.feature_movbe() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VCVTQQ2PD |
      Opcode::VCVTQQ2PS |
      Opcode::VCVTUSI2USD |
      Opcode::VCVTUSI2USS |
      Opcode::VEXTRACTF32X4 |
      Opcode::VEXTRACTF64X4 |
      Opcode::VEXTRACTI32X4 |
      Opcode::VEXTRACTI64X4 |
      Opcode::VFIXUPIMMSD |
      Opcode::VFIXUPIMMSS |
      Opcode::VINSERTI64X4 |
      Opcode::VMOVDQA32 |
      Opcode::VMOVDQA64 |
      Opcode::VMOVDQU32 |
      Opcode::VMOVDQU64 |
      Opcode::VPCOMPRESSQ |
      Opcode::VPCOMPRESSD |
      Opcode::VSCALEDPD |
      Opcode::VSCALEDPS |
      Opcode::VSCALEDSD |
      Opcode::VSCALEDSS |
      Opcode::VSCATTERDD |
      Opcode::VSCATTERDQ |
      Opcode::VSCATTERQD |
      Opcode::VSCATTERQQ |
      Opcode::VADDPD |
      Opcode::VADDPS |
      Opcode::VCMPPD |
      Opcode::VCMPPS |
      Opcode::VCVTDQ2PD |
      Opcode::VCVTDQ2PS |
      Opcode::VCVTPD2DQ |
      Opcode::VCVTPD2PS |
      Opcode::VCVTPS2DQ |
      Opcode::VCVTPS2PD |
      Opcode::VCVTTPD2DQ |
      Opcode::VCVTTPS2DQ |
      Opcode::VDIVPD |
      Opcode::VDIVPS |
      Opcode::VMAXPD |
      Opcode::VMAXPS |
      Opcode::VMINPD |
      Opcode::VMINPS |
      Opcode::VMOVAPD |
      Opcode::VMOVAPS |
      Opcode::VMOVDDUP |
      Opcode::VMOVDQA |
      Opcode::VMOVDQU |
      Opcode::VMOVNTDQA |
      Opcode::VMOVNTDQ |
      Opcode::VMOVNTPD |
      Opcode::VMOVNTPS |
      Opcode::VMOVSHDUP |
      Opcode::VMOVSLDUP |
      Opcode::VMOVUPD |
      Opcode::VMOVUPS |
      Opcode::VMULPD |
      Opcode::VMULPS |
      Opcode::VPANDD |
      Opcode::VPANDQ |
      Opcode::VPANDND |
      Opcode::VPANDNQ |
      Opcode::VPCMPEQD |
      Opcode::VPCMPEQQ |
      Opcode::VPCMPGTD |
      Opcode::VPCMPGTQ |
      Opcode::VPMAXSD |
      Opcode::VPMAXSQ |
      Opcode::VPMAXUD |
      Opcode::VPMAXUQ |
      Opcode::VPMINSD |
      Opcode::VPMINSQ |
      Opcode::VPMINUD |
      Opcode::VPMINUQ |
      Opcode::VPCMPD |
      Opcode::VPCMPQ |
      Opcode::VPCMPUD |
      Opcode::VPCMPUQ |
      Opcode::VPORD |
      Opcode::VPORQ |
      Opcode::VPXORD |
      Opcode::VPXORQ |
      Opcode::VPSLLD |
      Opcode::VPSLLQ |
      Opcode::VPSLLW |
      Opcode::VPSRAD |
      Opcode::VPSRAQ |
      Opcode::VALIGND |
      Opcode::VALIGNQ |
      Opcode::VBLENDMPD |
      Opcode::VBLENDMPS |
      Opcode::VPROLD |
      Opcode::VPROLQ |
      Opcode::VPROLVD |
      Opcode::VPROLVQ |
      Opcode::VPRORD |
      Opcode::VPRORQ |
      Opcode::VPRORVD |
      Opcode::VPRORVQ |
      Opcode::VPUNPCKHDQ |
      Opcode::VPUNPCKHQDQ |
      Opcode::VPUNPCKLDQ |
      Opcode::VPUNPCKLQDQ |
      Opcode::VUNPCKHPD |
      Opcode::VUNPCKHPS |
      Opcode::VUNPCKLPD |
      Opcode::VUNPCKLPS |
      Opcode::VBROADCASTF32X4 |
      Opcode::VBROADCASTF64X4 |
      Opcode::VBROADCASTI64X4 |
      Opcode::VBROADCASTI32X4 |
      Opcode::VINSERTF32X4 |
      Opcode::VINSERTI32X4 |
      Opcode::VSHUFF32X4 |
      Opcode::VSHUFF64X2 |
      Opcode::VSHUFI32X4 |
      Opcode::VSHUFI64X2 |
      Opcode::VCOMPRESSD |
      Opcode::VCOMPRESSQ |
      Opcode::VCOMPRESSPD |
      Opcode::VCOMPRESSPS |
      Opcode::VEXPANDPD |
      Opcode::VEXPANDPS |
      Opcode::VCVTPD2UDQ |
      Opcode::VCVTPS2UDQ |
      Opcode::VCVTUDQ2PD |
      Opcode::VCVTUDQ2PS |
      Opcode::VCVTTPD2UDQ |
      Opcode::VCVTTPS2UDQ |
      Opcode::VFIXUPIMMPD |
      Opcode::VFIXUPIMMPS |
      Opcode::VCVTPH2PS |
      Opcode::VCVTPS2PH |
      Opcode::VFMADD132PD |
      Opcode::VFMADD132PS |
      Opcode::VFMADD213PD |
      Opcode::VFMADD213PS |
      Opcode::VFMADD231PD |
      Opcode::VFMADD231PS |
      Opcode::VFMADDSUB132PD |
      Opcode::VFMADDSUB132PS |
      Opcode::VFMADDSUB213PD |
      Opcode::VFMADDSUB213PS |
      Opcode::VFMADDSUB231PD |
      Opcode::VFMADDSUB231PS |
      Opcode::VFMSUB132PD |
      Opcode::VFMSUB132PS |
      Opcode::VFMSUB213PD |
      Opcode::VFMSUB213PS |
      Opcode::VFMSUB231PD |
      Opcode::VFMSUB231PS |
      Opcode::VFMSUBADD132PD |
      Opcode::VFMSUBADD132PS |
      Opcode::VFMSUBADD213PD |
      Opcode::VFMSUBADD213PS |
      Opcode::VFMSUBADD231PD |
      Opcode::VFMSUBADD231PS |
      Opcode::VFNMADD132PD |
      Opcode::VFNMADD132PS |
      Opcode::VFNMADD213PD |
      Opcode::VFNMADD213PS |
      Opcode::VFNMADD231PD |
      Opcode::VFNMADD231PS |
      Opcode::VFNMSUB132PD |
      Opcode::VFNMSUB132PS |
      Opcode::VFNMSUB213PD |
      Opcode::VFNMSUB213PS |
      Opcode::VFNMSUB231PD |
      Opcode::VFNMSUB231PS |
      Opcode::VSCATTERDPS |
      Opcode::VSCATTERDPD |
      Opcode::VSCATTERQPS |
      Opcode::VSCATTERQPD |
      Opcode::VGATHERDPD |
      Opcode::VGATHERDPS |
      Opcode::VGATHERQPD |
      Opcode::VGATHERQPS |
      Opcode::VGETEXPPD |
      Opcode::VGETEXPPS |
      Opcode::VGETMANTPD |
      Opcode::VGETMANTPS |
      Opcode::VPBLENDMD |
      Opcode::VPBLENDMQ |
      Opcode::VPERMD |
      Opcode::VPERMQ |
      Opcode::VPERMI2D |
      Opcode::VPERMI2Q |
      Opcode::VPERMI2PD |
      Opcode::VPERMI2PS |
      Opcode::VPERMT2D |
      Opcode::VPERMT2Q |
      Opcode::VPERMT2PD |
      Opcode::VPERMT2PS |
      Opcode::VPEXPANDD |
      Opcode::VPEXPANDQ |
      Opcode::VPGATHERDD |
      Opcode::VPGATHERDQ |
      Opcode::VPGATHERQD |
      Opcode::VPGATHERQQ |
      Opcode::VPSCATTERDD |
      Opcode::VPSCATTERDQ |
      Opcode::VPSCATTERQD |
      Opcode::VPSCATTERQQ |
      Opcode::VPMOVDB |
      Opcode::VPMOVSDB |
      Opcode::VPMOVUSDB |
      Opcode::VPMOVDW |
      Opcode::VPMOVSDW |
      Opcode::VPMOVUSDW |
      Opcode::VPMOVQB |
      Opcode::VPMOVSQB |
      Opcode::VPMOVUSQB |
      Opcode::VPMOVQD |
      Opcode::VPMOVSQD |
      Opcode::VPMOVUSQD |
      Opcode::VPMOVQW |
      Opcode::VPMOVSQW |
      Opcode::VPMOVUSQW |
      Opcode::VPSRAVQ |
      Opcode::VPTERNLOGD |
      Opcode::VPTERNLOGQ |
      Opcode::VPTESTMD |
      Opcode::VPTESTMQ |
      Opcode::VPTESTNMD |
      Opcode::VPTESTNMQ |
      Opcode::VRCP14PD |
      Opcode::VRCP14PS |
      Opcode::VRNDSCALEPD |
      Opcode::VRNDSCALEPS |
      Opcode::VRSQRT14PD |
      Opcode::VRSQRT14PS |
      Opcode::VSCALEFPS |
      Opcode::VSCALEFPD |
      Opcode::VPABSQ |
      Opcode::VADDSD |
      Opcode::VADDSS |
      Opcode::VCMPSD |
      Opcode::VCMPSS |
      Opcode::VCOMISD |
      Opcode::VCOMISS |
      Opcode::VCVTSD2SI |
      Opcode::VCVTSD2SS |
      Opcode::VCVTSI2SS |
      Opcode::VCVTSI2SD |
      Opcode::VCVTSS2SD |
      Opcode::VCVTSS2SI |
      Opcode::VCVTTSS2SI |
      Opcode::VCVTTSD2SI |
      Opcode::VDIVSD |
      Opcode::VDIVSS |
      Opcode::VEXTRACTPS |
      Opcode::VINSERTPS |
      Opcode::VGETMANTSD |
      Opcode::VGETMANTSS |
      Opcode::VMOVD |
      Opcode::VMOVQ |
      Opcode::VMOVHLPS |
      Opcode::VMOVHPD |
      Opcode::VMOVHPS |
      Opcode::VMOVLHPS |
      Opcode::VMOVLPD |
      Opcode::VMOVLPS |
      Opcode::VMOVSS |
      Opcode::VMOVSD |
      Opcode::VSQRTSS |
      Opcode::VSQRTSD |
      Opcode::VSUBSD |
      Opcode::VSUBSS |
      Opcode::VUCOMISD |
      Opcode::VUCOMISS |
      Opcode::VRCP14SD |
      Opcode::VRCP14SS |
      Opcode::VRNDSCALESD |
      Opcode::VRNDSCALESS |
      Opcode::VRSQRT14SD |
      Opcode::VRSQRT14SS |
      Opcode::VSCALEFSS |
      Opcode::VSCALEFSD |
      Opcode::VINSERTF64X4 |
      Opcode::VCVTUSI2SD |
      Opcode::VCVTUSI2SS |
      Opcode::VCVTSD2USI |
      Opcode::VCVTSS2USI |
      Opcode::VCVTTSD2USI |
      Opcode::VCVTTSS2USI |
      Opcode::KANDW |
      Opcode::KANDNW |
      Opcode::KMOVW |
      Opcode::KNOTW |
      Opcode::KORW |
      Opcode::KORTESTW |
      Opcode::KSHIFTLW |
      Opcode::KSHIFTRW |
      Opcode::KUNPCKBW |
      Opcode::KXNORW |
      Opcode::KXORW => {
        if !decoder.feature_avx512_f() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MONITORX |
      Opcode::MWAITX => {
        if !decoder.feature_emx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PCLMULQDQ => {
        if !decoder.feature_pclmulqdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::VFNMSUB231SS => {
        if !decoder.feature_fma3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XSAVEOPT => {
        if !decoder.feature_xsaveopt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SYSCALL |
      Opcode::SYSRET => {
        if !decoder.feature_syscall() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::EXTRQ |
      Opcode::INSERTQ |
      Opcode::MOVNTSD |
      Opcode::MOVNTSS => {
        if !decoder.feature_sse4a() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPOPCNTD |
      Opcode::VPOPCNTQ => {
        if !decoder.feature_avx512_vpopcntdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVEPT |
      Opcode::INVVPID |
      Opcode::INVPCID => {
        if !decoder.feature_invpcid() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BOUND => {
        if !decoder.feature_80186_bound() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XSUSLDTRK |
      Opcode::XRESLDTRK => {
        if !decoder.feature_tsxldtrk() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::CMOVZ => {
        if !decoder.feature_cmov() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::KANDQ |
      Opcode::KANDD |
      Opcode::KANDNQ |
      Opcode::KANDND |
      Opcode::KMOVD |
      Opcode::KMOVQ |
      Opcode::KNOTD |
      Opcode::KNOTQ |
      Opcode::KORD |
      Opcode::KORQ |
      Opcode::KADDD |
      Opcode::KTESTD |
      Opcode::KADDQ |
      Opcode::KTESTQ |
      Opcode::KORTESTD |
      Opcode::KORTESTQ |
      Opcode::KSHIFTLD |
      Opcode::KSHIFTRD |
      Opcode::KSHIFTLQ |
      Opcode::KSHIFTRQ |
      Opcode::KUNPCKWD |
      Opcode::KUNPCKDQ |
      Opcode::KXNORD |
      Opcode::KXNORQ |
      Opcode::KXORD |
      Opcode::KXORQ |
      Opcode::VPCMPEQB |
      Opcode::VPCMPEQW |
      Opcode::VPCMPGTB |
      Opcode::VPCMPGTW |
      Opcode::VPSUBUSB |
      Opcode::VPSUBUSW |
      Opcode::VPERMW |
      Opcode::VPERMI2W |
      Opcode::VPERMT2W |
      Opcode::VPSLLVW |
      Opcode::VPSRAVW |
      Opcode::VPSRLVW |
      Opcode::VPEXTRB |
      Opcode::VPEXTRW |
      Opcode::VPINSRB |
      Opcode::VPINSRW |
      Opcode::VPMULHUW |
      Opcode::VPMULHRSW |
      Opcode::VPADDSB |
      Opcode::VPADDSW |
      Opcode::VPADDUSB |
      Opcode::VPADDUSW |
      Opcode::VPALIGNR |
      Opcode::VPMOVD2M |
      Opcode::VPMOVQ2M |
      Opcode::VPMOVWB |
      Opcode::VDBPSADBW |
      Opcode::VMOVDQU8 |
      Opcode::VMOVDQU16 |
      Opcode::VPBLENDMB |
      Opcode::VPBLENDMW |
      Opcode::VPCMPB |
      Opcode::VPCMPUB |
      Opcode::VPCMPW |
      Opcode::VPCMPUW |
      Opcode::VPERMW |
      Opcode::VPERMI2B |
      Opcode::VPERMI2W |
      Opcode::VPMOVM2B |
      Opcode::VPMOVM2W |
      Opcode::VPMOVB2M |
      Opcode::VPMOVW2M |
      Opcode::VPMOVSWB |
      Opcode::VPMOVUSWB |
      Opcode::VPSLLVW |
      Opcode::VPSRAVW |
      Opcode::VPSRLVW |
      Opcode::VPTESTNMB |
      Opcode::VPTESTNMW |
      Opcode::VPTESTMB |
      Opcode::VPTESTMW => {
        if !decoder.feature_avx512_bw() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PALIGNR |
      Opcode::PSIGNW |
      Opcode::PSIGND |
      Opcode::PSIGNB |
      Opcode::PSHUFB |
      Opcode::PMULHRSW |
      Opcode::PMADDUBSW |
      Opcode::PABSD |
      Opcode::PABSW |
      Opcode::PABSB |
      Opcode::PHSUBSW |
      Opcode::PHSUBW |
      Opcode::PHSUBD |
      Opcode::PHADDD |
      Opcode::PHADDSW |
      Opcode::PHADDW => {
        if !decoder.feature_ssse3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLFLUSHOPT => {
        if !decoder.feature_clflushopt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PUSHA |
      Opcode::POPA => {
        if !decoder.feature_80186_pusha() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::GETSEC => {
        if !decoder.feature_smx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::AAA |
      Opcode::AAD |
      Opcode::AAM |
      Opcode::AAS |
      Opcode::DAA |
      Opcode::DAS => {
        if !decoder.feature_8086_bcd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::POPCNT |
      Opcode::LZCNT => {
        if !decoder.feature_abm() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PREFETCHW => {
        if !decoder.feature_3dnowprefetch() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPID => {
        if !decoder.feature_rdpid() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::LAHF |
      Opcode::SAHF => {
        if !decoder.feature_lahfsahf() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ENQCMD |
      Opcode::ENQCMDS => {
        if !decoder.feature_enqcmd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::JECXZ => {
        if !decoder.feature_jecxz() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PTWRITE => {
        if !decoder.feature_ptwrite() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPERMT2B |
      Opcode::VPERMB => {
        if !decoder.feature_avx512_vbmi() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::F2XM1 |
      Opcode::FABS |
      Opcode::FADD |
      Opcode::FADDP |
      Opcode::FBLD |
      Opcode::FBSTP |
      Opcode::FCHS |
      Opcode::FCMOVB |
      Opcode::FCMOVBE |
      Opcode::FCMOVE |
      Opcode::FCMOVNB |
      Opcode::FCMOVNBE |
      Opcode::FCMOVNE |
      Opcode::FCMOVNU |
      Opcode::FCMOVU |
      Opcode::FCOM |
      Opcode::FCOMI |
      Opcode::FCOMIP |
      Opcode::FCOMP |
      Opcode::FCOMPP |
      Opcode::FCOS |
      Opcode::FDECSTP |
      Opcode::FDISI8087_NOP |
      Opcode::FDIV |
      Opcode::FDIVP |
      Opcode::FDIVR |
      Opcode::FDIVRP |
      Opcode::FENI8087_NOP |
      Opcode::FFREE |
      Opcode::FFREEP |
      Opcode::FIADD |
      Opcode::FICOM |
      Opcode::FICOMP |
      Opcode::FIDIV |
      Opcode::FIDIVR |
      Opcode::FILD |
      Opcode::FIMUL |
      Opcode::FINCSTP |
      Opcode::FIST |
      Opcode::FISTP |
      Opcode::FISTTP |
      Opcode::FISUB |
      Opcode::FISUBR |
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
      Opcode::FMUL |
      Opcode::FMULP |
      Opcode::FNCLEX |
      Opcode::FNINIT |
      Opcode::FNOP |
      Opcode::FNSAVE |
      Opcode::FNSTCW |
      Opcode::FNSTENV |
      Opcode::FNSTOR |
      Opcode::FNSTSW |
      Opcode::FPATAN |
      Opcode::FPREM |
      Opcode::FPREM1 |
      Opcode::FPTAN |
      Opcode::FRNDINT |
      Opcode::FRSTOR |
      Opcode::FSCALE |
      Opcode::FSETPM287_NOP |
      Opcode::FSIN |
      Opcode::FSINCOS |
      Opcode::FSQRT |
      Opcode::FST |
      Opcode::FSTP |
      Opcode::FSTPNCE |
      Opcode::FSUB |
      Opcode::FSUBP |
      Opcode::FSUBR |
      Opcode::FSUBRP |
      Opcode::FTST |
      Opcode::FUCOM |
      Opcode::FUCOMI |
      Opcode::FUCOMIP |
      Opcode::FUCOMP |
      Opcode::FUCOMPP |
      Opcode::FXAM |
      Opcode::FXCH |
      Opcode::FXTRACT |
      Opcode::FYL2X |
      Opcode::FYL2XP1 => {
        if !decoder.feature_x87() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPMC |
      Opcode::PUNPCKLBW |
      Opcode::PUNPCKLWD |
      Opcode::PUNPCKLDQ |
      Opcode::PACKSSWB |
      Opcode::PCMPGTB |
      Opcode::PCMPGTD |
      Opcode::PCMPGTW |
      Opcode::PACKUSWB |
      Opcode::PUNPCKHBW |
      Opcode::PUNPCKHWD |
      Opcode::PUNPCKHDQ |
      Opcode::PACKSSDW |
      Opcode::MOVD |
      Opcode::MOVQ |
      Opcode::PCMPEQB |
      Opcode::PCMPEQD |
      Opcode::PCMPEQW |
      Opcode::PSRLW |
      Opcode::PSRLD |
      Opcode::PSRLQ |
      Opcode::PMULLW |
      Opcode::PSUBUSB |
      Opcode::PSUBUSW |
      Opcode::PAND |
      Opcode::PADDUSB |
      Opcode::PADDUSW |
      Opcode::PANDN |
      Opcode::PSRAW |
      Opcode::PSRAD |
      Opcode::PMULHW |
      Opcode::PSUBSB |
      Opcode::PSUBSW |
      Opcode::POR |
      Opcode::PADDSB |
      Opcode::PADDSW |
      Opcode::PXOR |
      Opcode::PSLLW |
      Opcode::PSLLD |
      Opcode::PSLLQ |
      Opcode::PMADDWD |
      Opcode::PSUBB |
      Opcode::PSUBW |
      Opcode::PSUBD |
      Opcode::PADDB |
      Opcode::PADDW |
      Opcode::PADDD |
      Opcode::EMMS => {
        if !decoder.feature_mmx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPOPCNTD |
      Opcode::VPOPCNTQ |
      Opcode::VPOPCNTB |
      Opcode::VPOPCNTW |
      Opcode::VPSHUFBITQMB |
      Opcode::VPMULTISHIFTQB => {
        if !decoder.feature_avx512_bitalg() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::LOADIWKEY => {
        if !decoder.feature_keylocker() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ENTER |
      Opcode::LEAVE |
      Opcode::INS |
      Opcode::OUTS => {
        if !decoder.feature_80186() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLWB => {
        if !decoder.feature_clwb() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::V4FNMADDSS |
      Opcode::V4FNMADDPS |
      Opcode::V4FMADDSS |
      Opcode::V4FMADDPS => {
        if !decoder.feature_avx512_4fmaps() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::TDCALL |
      Opcode::SEAMRET |
      Opcode::SEAMOPS |
      Opcode::SEAMCALL => {
        if !decoder.feature_tdx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::POPCNT => {
        if !decoder.feature_popcnt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PSMASH |
      Opcode::PVALIDATE |
      Opcode::RMPADJUST |
      Opcode::RMPUPDATE => {
        if !decoder.feature_snp() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VGF2P8AFFINEQB |
      Opcode::VGF2P8AFFINEINVQB |
      Opcode::VGF2P8MULB => {
        if !decoder.feature_avx512_gfni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::FEMMS |
      Opcode::PI2FW |
      Opcode::PI2FD |
      Opcode::PF2IW |
      Opcode::PF2ID |
      Opcode::PMULHRW |
      Opcode::PFCMPGE |
      Opcode::PFMIN |
      Opcode::PFRCP |
      Opcode::PFRSQRT |
      Opcode::PFSUB |
      Opcode::PFADD |
      Opcode::PFCMPGT |
      Opcode::PFMAX |
      Opcode::PFRCPIT1 |
      Opcode::PFRSQIT1 |
      Opcode::PFSUBR |
      Opcode::PFACC |
      Opcode::PFCMPEQ |
      Opcode::PFMUL |
      Opcode::PFMULHRW |
      Opcode::PFRCPIT2 |
      Opcode::PFNACC |
      Opcode::PFPNACC |
      Opcode::PSWAPD |
      Opcode::PAVGUSB => {
        if !decoder.feature_3dnow() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPCOMPRESSB |
      Opcode::VPCOMPRESSW |
      Opcode::VPSHLDVW |
      Opcode::VPSHLDW |
      Opcode::VPEXPANDB |
      Opcode::VPEXPANDW |
      Opcode::VPSHRDVW |
      Opcode::VPSHRDW |
      Opcode::VPSHLDVQ |
      Opcode::VPSHLDVD |
      Opcode::VPSHLDQ |
      Opcode::VPSHLDD |
      Opcode::VPSHRDQ |
      Opcode::VPSHRDD |
      Opcode::VPSHRDVQ |
      Opcode::VPSHRDVD => {
        if !decoder.feature_avx512_vbmi2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CMC |
      Opcode::CLC |
      Opcode::STC |
      Opcode::CLI |
      Opcode::STI |
      Opcode::CLD |
      Opcode::STD |
      Opcode::ADD |
      Opcode::OR |
      Opcode::ADC |
      Opcode::SBB |
      Opcode::AND |
      Opcode::XOR |
      Opcode::SUB |
      Opcode::CMP |
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
      Opcode::HLT |
      Opcode::CALL |
      Opcode::CALLF |
      Opcode::JMP |
      Opcode::JMPF |
      Opcode::PUSH |
      Opcode::POP |
      Opcode::LEA |
      Opcode::NOP |
      Opcode::XCHG |
      Opcode::POPF |
      Opcode::INT |
      Opcode::INTO |
      Opcode::IRET |
      Opcode::IRETD |
      Opcode::IRETQ |
      Opcode::RETF |
      Opcode::ENTER |
      Opcode::LEAVE |
      Opcode::MOV |
      Opcode::RETURN |
      Opcode::PUSHF |
      Opcode::WAIT |
      Opcode::CBW |
      Opcode::CWD |
      Opcode::CQO |
      Opcode::LODS |
      Opcode::STOS |
      Opcode::CMPS |
      Opcode::SCAS |
      Opcode::MOVS |
      Opcode::TEST |
      Opcode::IN |
      Opcode::OUT |
      Opcode::IMUL |
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
      Opcode::JG |
      Opcode::UD0 |
      Opcode::UD1 |
      Opcode::UD2 |
      Opcode::DIV |
      Opcode::IDIV |
      Opcode::MUL |
      Opcode::NEG |
      Opcode::NOT |
      Opcode::XLAT |
      Opcode::LOOPNZ |
      Opcode::LOOPZ |
      Opcode::LOOP |
      Opcode::SALC => {
        if !decoder.feature_8086() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLGI |
      Opcode::STGI |
      Opcode::SKINIT |
      Opcode::VMLOAD |
      Opcode::VMMCALL |
      Opcode::VMSAVE |
      Opcode::VMRUN |
      Opcode::INVLPGA => {
        if !decoder.feature_svm() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ADDSUBPD |
      Opcode::ADDSUBPS |
      Opcode::HSUBPD |
      Opcode::HADDPD |
      Opcode::MOVSLDUP |
      Opcode::MOVSHDUP |
      Opcode::MOVDDUP |
      Opcode::HADDPS |
      Opcode::HSUBPS |
      Opcode::LDDQU => {
        if !decoder.feature_sse3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVUPD |
      Opcode::PSRLDQ |
      Opcode::PSLLDQ |
      Opcode::MOVSD |
      Opcode::MOVLPD |
      Opcode::UNPCKLPD |
      Opcode::UNPCKHPD |
      Opcode::MOVHPD |
      Opcode::MOVAPD |
      Opcode::MOVMSKPD |
      Opcode::CVTPI2PD |
      Opcode::CVTSI2SD |
      Opcode::MOVNTPD |
      Opcode::MOVNTI |
      Opcode::MOVNTDQ |
      Opcode::CVTTPD2PI |
      Opcode::CVTTSD2SI |
      Opcode::CVTPD2PI |
      Opcode::CVTSD2SI |
      Opcode::UCOMISD |
      Opcode::COMISD |
      Opcode::SQRTPD |
      Opcode::SQRTSD |
      Opcode::ANDPD |
      Opcode::ANDNPD |
      Opcode::ORPD |
      Opcode::XORPD |
      Opcode::ADDPD |
      Opcode::ADDSD |
      Opcode::MULSD |
      Opcode::MULPD |
      Opcode::CVTPS2PD |
      Opcode::CVTPD2PS |
      Opcode::CVTSS2SD |
      Opcode::CVTSD2SS |
      Opcode::CVTPS2DQ |
      Opcode::CVTDQ2PS |
      Opcode::CVTTPS2DQ |
      Opcode::SUBSD |
      Opcode::SUBPD |
      Opcode::MINPD |
      Opcode::MINSD |
      Opcode::DIVPD |
      Opcode::DIVSD |
      Opcode::MAXPD |
      Opcode::MAXSD |
      Opcode::PUNPCKLQDQ |
      Opcode::PUNPCKHQDQ |
      Opcode::MOVDQA |
      Opcode::MOVDQU |
      Opcode::PSHUFHW |
      Opcode::PSHUFLW |
      Opcode::PSHUFD |
      Opcode::LFENCE |
      Opcode::MFENCE |
      Opcode::CLFLUSH |
      Opcode::CMPPD |
      Opcode::CMPPS |
      Opcode::CMPSD |
      Opcode::SHUFPD |
      Opcode::PADDQ |
      Opcode::MOVQ2DQ |
      Opcode::MOVDQ2Q |
      Opcode::CVTPD2DQ |
      Opcode::CVTTPD2DQ |
      Opcode::CVTDQ2PD |
      Opcode::PMULUDQ |
      Opcode::MASKMOVDQU |
      Opcode::PSUBQ => {
        if !decoder.feature_sse2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VEXP2PD |
      Opcode::VEXP2PS |
      Opcode::VEXP2SD |
      Opcode::VEXP2SS |
      Opcode::VRCP28PD |
      Opcode::VRCP28PS |
      Opcode::VRCP28SD |
      Opcode::VRCP28SS |
      Opcode::VRSQRT28PD |
      Opcode::VRSQRT28PS |
      Opcode::VRSQRT28SD |
      Opcode::VRSQRT28SS => {
        if !decoder.feature_avx512_er() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XRSTORS64 |
      Opcode::XSAVEC64 |
      Opcode::XSAVES64 => {
        if !decoder.feature_xsave64() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ARPL => {
        if !decoder.feature_80286_arpl() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ANDN |
      Opcode::BEXTR |
      Opcode::BLSI |
      Opcode::BLSMSK |
      Opcode::BLSR |
      Opcode::TZCNT => {
        if !decoder.feature_bmi1() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BZHI |
      Opcode::MULX |
      Opcode::PDEP |
      Opcode::PEXT |
      Opcode::RORX |
      Opcode::SARX |
      Opcode::SHRX |
      Opcode::SHLX => {
        if !decoder.feature_bmi2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVDIRI |
      Opcode::MOVDIR64B => {
        if !decoder.feature_movdir() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VAESDEC |
      Opcode::VAESDECLAST |
      Opcode::VAESENC |
      Opcode::VAESENCLAST |
      Opcode::VAESIMC |
      Opcode::VAESKEYGENASSIST => {
        if !decoder.feature_vaes() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XGETBV |
      Opcode::XRSTOR |
      Opcode::XRSTORS |
      Opcode::XSAVE |
      Opcode::XSAVEC |
      Opcode::XSAVES |
      Opcode::XSETBV => {
        if !decoder.feature_xsave() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDRAND => {
        if !decoder.feature_rdrand() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLZERO => {
        if !decoder.feature_clzero() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDSEED => {
        if !decoder.feature_rdseed() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
    }
    Ok(())
  }
}

pub(crate) mod long_mode {
  use crate::generated::long_mode::Opcode;
  use crate::long_mode::{InstDecoder, Instruction, DecodeError};

  use yaxpeax_arch::{Colorize, YaxColors};
  use core::fmt;
  impl InstDecoder {
    fn feature_sgx(&self) -> bool {
      true
    }
    fn feature_smap(&self) -> bool {
      true
    }
    fn feature_avx_unimplemented(&self) -> bool {
      true
    }
    fn feature_uintr(&self) -> bool {
      true
    }
    fn feature_pentium(&self) -> bool {
      true
    }
    fn feature_aesni(&self) -> bool {
      true
    }
    fn feature_invlpgb(&self) -> bool {
      true
    }
    fn feature_sse(&self) -> bool {
      true
    }
    fn feature_xsaveopt(&self) -> bool {
      true
    }
    fn feature_simd(&self) -> bool {
      true
    }
    fn feature_avx(&self) -> bool {
      true
    }
    fn feature_hreset(&self) -> bool {
      true
    }
    fn feature_avx512_vp2intersect(&self) -> bool {
      true
    }
    fn feature_itanium(&self) -> bool {
      true
    }
    fn feature_rdpru(&self) -> bool {
      true
    }
    fn feature_sse4_2(&self) -> bool {
      true
    }
    fn feature_monitor(&self) -> bool {
      true
    }
    fn feature_avx512_f_typo(&self) -> bool {
      true
    }
    fn feature_sse4_1(&self) -> bool {
      true
    }
    fn feature_avx512_dq(&self) -> bool {
      true
    }
    fn feature_adx(&self) -> bool {
      true
    }
    fn feature_fxsr(&self) -> bool {
      true
    }
    fn feature_typo_instructions(&self) -> bool {
      true
    }
    fn feature_tsx(&self) -> bool {
      true
    }
    fn feature_cmpxchg16b(&self) -> bool {
      true
    }
    fn feature_waitpkg(&self) -> bool {
      true
    }
    fn feature_80286(&self) -> bool {
      true
    }
    fn feature_sysenter(&self) -> bool {
      true
    }
    fn feature_invpcid_unimplemented(&self) -> bool {
      true
    }
    fn feature_avx512_4vnniw(&self) -> bool {
      true
    }
    fn feature_avx512_vnni(&self) -> bool {
      true
    }
    fn feature_avx512_pf(&self) -> bool {
      true
    }
    fn feature_avx512_cd(&self) -> bool {
      true
    }
    fn feature_prefetchw(&self) -> bool {
      true
    }
    fn feature_avx512_bf16(&self) -> bool {
      true
    }
    fn feature_avx512bw_unimplemented(&self) -> bool {
      true
    }
    fn feature_pclmulqdq(&self) -> bool {
      true
    }
    fn feature_avx512_f__vl_unimplemented(&self) -> bool {
      true
    }
    fn feature_cet(&self) -> bool {
      true
    }
    fn feature_rdseed(&self) -> bool {
      true
    }
    fn feature_tbm(&self) -> bool {
      true
    }
    fn feature_80486(&self) -> bool {
      true
    }
    fn feature_fsgsbase(&self) -> bool {
      true
    }
    fn feature_pentium_pro(&self) -> bool {
      true
    }
    fn feature_f16c(&self) -> bool {
      true
    }
    fn feature_mpx(&self) -> bool {
      true
    }
    fn feature_rdtscp(&self) -> bool {
      true
    }
    fn feature_avx2(&self) -> bool {
      true
    }
    fn feature_pconfig(&self) -> bool {
      true
    }
    fn feature_avx512_ifma(&self) -> bool {
      true
    }
    fn feature_avx512_er(&self) -> bool {
      true
    }
    fn feature_vpclmulqdq(&self) -> bool {
      true
    }
    fn feature_80386(&self) -> bool {
      true
    }
    fn feature_movbe(&self) -> bool {
      true
    }
    fn feature_avx512_f(&self) -> bool {
      true
    }
    fn feature_emx(&self) -> bool {
      true
    }
    fn feature_fma3(&self) -> bool {
      true
    }
    fn feature_fma4(&self) -> bool {
      true
    }
    fn feature_vmx(&self) -> bool {
      true
    }
    fn feature_tsxldtrk(&self) -> bool {
      true
    }
    fn feature_syscall(&self) -> bool {
      true
    }
    fn feature_sse4a(&self) -> bool {
      true
    }
    fn feature_avx512_vpopcntdq(&self) -> bool {
      true
    }
    fn feature_invpcid(&self) -> bool {
      true
    }
    fn feature_xsave(&self) -> bool {
      true
    }
    fn feature_cmov(&self) -> bool {
      true
    }
    fn feature_avx512_bw(&self) -> bool {
      true
    }
    fn feature_ssse3(&self) -> bool {
      true
    }
    fn feature_clflushopt(&self) -> bool {
      true
    }
    fn feature_xop(&self) -> bool {
      true
    }
    fn feature_sha(&self) -> bool {
      true
    }
    fn feature_64bit(&self) -> bool {
      true
    }
    fn feature_x86_64_baseline(&self) -> bool {
      true
    }
    fn feature_abm(&self) -> bool {
      true
    }
    fn feature_3dnowprefetch(&self) -> bool {
      true
    }
    fn feature_rdpid(&self) -> bool {
      true
    }
    fn feature_lahfsahf(&self) -> bool {
      true
    }
    fn feature_enqcmd(&self) -> bool {
      true
    }
    fn feature_new(&self) -> bool {
      true
    }
    fn feature_ptwrite(&self) -> bool {
      true
    }
    fn feature_avx512_vbmi(&self) -> bool {
      true
    }
    fn feature_x87(&self) -> bool {
      true
    }
    fn feature_mmx(&self) -> bool {
      true
    }
    fn feature_avx512_bitalg(&self) -> bool {
      true
    }
    fn feature_keylocker(&self) -> bool {
      true
    }
    fn feature_80186(&self) -> bool {
      true
    }
    fn feature_clwb(&self) -> bool {
      true
    }
    fn feature_mpk(&self) -> bool {
      true
    }
    fn feature_avx512_4fmaps(&self) -> bool {
      true
    }
    fn feature_tdx(&self) -> bool {
      true
    }
    fn feature_popcnt(&self) -> bool {
      true
    }
    fn feature_snp(&self) -> bool {
      true
    }
    fn feature_avx512_gfni(&self) -> bool {
      true
    }
    fn feature_3dnow(&self) -> bool {
      true
    }
    fn feature_avx512_vbmi2(&self) -> bool {
      true
    }
    fn feature_8086(&self) -> bool {
      true
    }
    fn feature_svm(&self) -> bool {
      true
    }
    fn feature_sse3(&self) -> bool {
      true
    }
    fn feature_sse2(&self) -> bool {
      true
    }
    fn feature_gfni(&self) -> bool {
      true
    }
    fn feature_xsave64(&self) -> bool {
      true
    }
    fn feature_smx(&self) -> bool {
      true
    }
    fn feature_bmi1(&self) -> bool {
      true
    }
    fn feature_bmi2(&self) -> bool {
      true
    }
    fn feature_movdir(&self) -> bool {
      true
    }
    fn feature_vaes(&self) -> bool {
      true
    }
    fn feature_rdrand(&self) -> bool {
      true
    }
    fn feature_clzero(&self) -> bool {
      true
    }
  }
  impl<T: fmt::Write, Y: YaxColors> Colorize<T, Y> for Opcode {
    fn colorize(&self, colors: &Y, out: &mut T) -> fmt::Result {
      self.to_generic().colorize(colors, out)
    }
  }
  impl Opcode {
    pub fn name(&self) -> &'static str {
      self.to_generic().name()
    }
  }
  pub(crate) fn revise_instruction(decoder: &InstDecoder, inst: &mut Instruction) -> Result<(), DecodeError> {
    if inst.prefixes.evex().is_some() {
      if !decoder.avx512() {
        return Err(DecodeError::InvalidOpcode);
      } else {
        return Ok(());
      }
    }

    // for some instructions (tzcnt), not having an extension means the instruction is
    // interpreted as another, rather than being simply rejected.
    // we might still reject the alternate instruction later, if the extension adding *it*
    // is also not supported.
    if inst.opcode == Opcode::TZCNT {
      if !decoder.bmi1() {
        // tzcnt is only supported if bmi1 is enabled. without bmi1, this decodes as bsf.
        inst.opcode = Opcode::BSF;
      }
    }

    match inst.opcode {
      // we'll never be rejecting the instruction `Invalid`
      Opcode::Invalid => {}
      Opcode::ENCLS |
      Opcode::ENCLU |
      Opcode::ENCLV => {
        if !decoder.feature_sgx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLAC |
      Opcode::STAC => {
        if !decoder.feature_smap() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMAXUB => {
        if !decoder.feature_avx_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::UIRET |
      Opcode::TESTUI |
      Opcode::CLUI |
      Opcode::STUI |
      Opcode::SENDUIPI => {
        if !decoder.feature_uintr() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CPUID |
      Opcode::WRMSR |
      Opcode::RDTSC |
      Opcode::RDMSR |
      Opcode::RSM |
      Opcode::CMPXCHG8B => {
        if !decoder.feature_pentium() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::AESKEYGENASSIST |
      Opcode::AESIMC |
      Opcode::AESENC |
      Opcode::AESENCLAST |
      Opcode::AESDEC |
      Opcode::AESDECLAST => {
        if !decoder.feature_aesni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVLPGB |
      Opcode::TLBSYNC => {
        if !decoder.feature_invlpgb() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVSS |
      Opcode::ADDSS |
      Opcode::SUBSS |
      Opcode::MULSS |
      Opcode::DIVSS |
      Opcode::MINSS |
      Opcode::MAXSS |
      Opcode::SQRTSS |
      Opcode::MOVUPS |
      Opcode::MOVHLPS |
      Opcode::MOVLPS |
      Opcode::MOVHPS |
      Opcode::MOVLHPS |
      Opcode::UNPCKLPS |
      Opcode::UNPCKHPS |
      Opcode::PREFETCHNTA |
      Opcode::PREFETCH0 |
      Opcode::PREFETCH1 |
      Opcode::PREFETCH2 |
      Opcode::MOVAPS |
      Opcode::CVTPI2PS |
      Opcode::CVTSI2SS |
      Opcode::MOVNTPS |
      Opcode::CVTTSS2SI |
      Opcode::CVTTPS2PI |
      Opcode::CVTSS2SI |
      Opcode::CVTPS2PI |
      Opcode::UCOMISS |
      Opcode::COMISS |
      Opcode::SQRTPS |
      Opcode::MOVMSKPS |
      Opcode::RSQRTSS |
      Opcode::RSQRTPS |
      Opcode::RCPPS |
      Opcode::RCPSS |
      Opcode::ANDPS |
      Opcode::ANDNPS |
      Opcode::XORPS |
      Opcode::ORPS |
      Opcode::ADDPS |
      Opcode::MULPS |
      Opcode::SUBPS |
      Opcode::MINPS |
      Opcode::DIVPS |
      Opcode::MAXPS |
      Opcode::PSHUFW |
      Opcode::LDMXCSR |
      Opcode::STMXCSR |
      Opcode::SFENCE |
      Opcode::CMPPS |
      Opcode::CMPSS |
      Opcode::PINSRW |
      Opcode::PEXTRW |
      Opcode::SHUFPS |
      Opcode::PMOVMSKB |
      Opcode::PMINUB |
      Opcode::PMAXUB |
      Opcode::PAVGB |
      Opcode::PAVGW |
      Opcode::PMULHUW |
      Opcode::MOVNTQ |
      Opcode::PMINSW |
      Opcode::PMAXSW |
      Opcode::PSADBW |
      Opcode::MASKMOVQ |
      Opcode::LDMXCSR |
      Opcode::STMXCSR => {
        if !decoder.feature_sse() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XSAVEOPT => {
        if !decoder.feature_xsaveopt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VLDDQU |
      Opcode::VPADDB |
      Opcode::VPADDD |
      Opcode::VPADDQ |
      Opcode::VPADDW |
      Opcode::VPABSB |
      Opcode::VPABSW |
      Opcode::VPABSD |
      Opcode::VMAXSD |
      Opcode::VMAXSS |
      Opcode::VMINSD |
      Opcode::VMINSS |
      Opcode::VMULSD |
      Opcode::VMULSS |
      Opcode::VRCPSS |
      Opcode::VPSUBUSB |
      Opcode::VPSUBUSW |
      Opcode::VXORPD |
      Opcode::VXORPS |
      Opcode::VPAVGB |
      Opcode::VPAVGW |
      Opcode::VANDNPD |
      Opcode::VANDNPS |
      Opcode::VPHADDD |
      Opcode::VPHADDSW |
      Opcode::VPHADDW |
      Opcode::VANDPD |
      Opcode::VANDPS |
      Opcode::VPADDSB |
      Opcode::VPADDSW |
      Opcode::VPADDUSB |
      Opcode::VPADDUSW |
      Opcode::VPHSUBD |
      Opcode::VPHSUBSW |
      Opcode::VPHSUBW |
      Opcode::VHADDPD |
      Opcode::VHADDPS |
      Opcode::VHSUBPD |
      Opcode::VHSUBPS |
      Opcode::VORPD |
      Opcode::VORPS |
      Opcode::VPBLENDVB |
      Opcode::VPBLENDW |
      Opcode::VADDSUBPD |
      Opcode::VADDSUBPS |
      Opcode::VRSQRTPS |
      Opcode::VRSQRTSS |
      Opcode::VPALIGNR |
      Opcode::VPSUBSB |
      Opcode::VPSUBSW |
      Opcode::VPMULHUW |
      Opcode::VPMULHW |
      Opcode::VPMULLW |
      Opcode::VPSHUFB |
      Opcode::VPSHUFHW |
      Opcode::VPSHUFLW |
      Opcode::VPHMINPOSUW |
      Opcode::VRCPPS |
      Opcode::VMPSADBW |
      Opcode::VPMADDUBSW |
      Opcode::VPMADDWD |
      Opcode::VMASKMOVDQU |
      Opcode::VPMOVMSKB |
      Opcode::VPSADBW |
      Opcode::VPSLLDQ |
      Opcode::VPSRLDQ |
      Opcode::VDPPD |
      Opcode::VDPPS |
      Opcode::VLDMXCSR |
      Opcode::VSTMXCSR |
      Opcode::VMOVMSKPD |
      Opcode::VMOVMSKPS |
      Opcode::VPTEST |
      Opcode::VTESTPD |
      Opcode::VTESTPS |
      Opcode::VPEXTRB |
      Opcode::VPEXTRW |
      Opcode::VPEXTRD |
      Opcode::VPEXTRQ |
      Opcode::VPACKSSDW |
      Opcode::VPACKUSDW |
      Opcode::VPACKSSWB |
      Opcode::VPACKUSWB |
      Opcode::VBLENDPD |
      Opcode::VBLENDPS |
      Opcode::VBLENDVPD |
      Opcode::VBLENDVPS |
      Opcode::VPMULHRSW |
      Opcode::VPAND |
      Opcode::VPANDN |
      Opcode::VPOR |
      Opcode::VPCMPEQB |
      Opcode::VPCMPEQD |
      Opcode::VPCMPEQQ |
      Opcode::VPCMPEQW |
      Opcode::VPCMPGTB |
      Opcode::VPCMPGTD |
      Opcode::VPCMPGTQ |
      Opcode::VPCMPGTW |
      Opcode::VPMAXSB |
      Opcode::VPMAXSD |
      Opcode::VPMAXSW |
      Opcode::VPMAXUB |
      Opcode::VPMAXUW |
      Opcode::VPMINSB |
      Opcode::VPMINSW |
      Opcode::VPMINUB |
      Opcode::VPMINUW |
      Opcode::VPMAXUD |
      Opcode::VPMINSD |
      Opcode::VPSIGNB |
      Opcode::VPSIGNW |
      Opcode::VPSIGND |
      Opcode::VPINSRB |
      Opcode::VPINSRW |
      Opcode::VPINSRD |
      Opcode::VPINSRQ |
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
      Opcode::VPMULDQ |
      Opcode::VPMULLQ |
      Opcode::VPMULLD |
      Opcode::VPMULUDQ |
      Opcode::VPSHUFD |
      Opcode::VSHUFPD |
      Opcode::VSHUFPS |
      Opcode::VSQRTPD |
      Opcode::VSQRTPS |
      Opcode::VSQRTSS |
      Opcode::VSQRTSD |
      Opcode::VSUBPD |
      Opcode::VSUBPS |
      Opcode::VSUBSD |
      Opcode::VSUBSS |
      Opcode::VUCOMISD |
      Opcode::VUCOMISS |
      Opcode::VROUNDPD |
      Opcode::VROUNDPS |
      Opcode::VROUNDSD |
      Opcode::VROUNDSS |
      Opcode::VPSLLD |
      Opcode::VPSLLQ |
      Opcode::VPSLLW |
      Opcode::VPSRAD |
      Opcode::VPSRAW |
      Opcode::VPSRLD |
      Opcode::VPSRLQ |
      Opcode::VPSRLW |
      Opcode::VPSUBB |
      Opcode::VPSUBW |
      Opcode::VPSUBD |
      Opcode::VPSUBQ |
      Opcode::VPXOR |
      Opcode::VPUNPCKHBW |
      Opcode::VPUNPCKHWD |
      Opcode::VPUNPCKHDQ |
      Opcode::VPUNPCKHQDQ |
      Opcode::VPUNPCKLBW |
      Opcode::VPUNPCKLWD |
      Opcode::VPUNPCKLDQ |
      Opcode::VPUNPCKLQDQ |
      Opcode::VUNPCKHPD |
      Opcode::VUNPCKHPS |
      Opcode::VUNPCKLPD |
      Opcode::VUNPCKLPS |
      Opcode::VPCMPESTRI |
      Opcode::VPCMPESTRM |
      Opcode::VPCMPISTRI |
      Opcode::VPCMPISTRM |
      Opcode::VBROADCASTSS |
      Opcode::VBROADCASTSD |
      Opcode::VBROADCASTF128 |
      Opcode::VBROADCASTSD |
      Opcode::VBROADCASTSS |
      Opcode::VINSERTF128 |
      Opcode::VEXTRACTF128 |
      Opcode::VMASKMOVPD |
      Opcode::VMASKMOVPS |
      Opcode::VPERMILPD |
      Opcode::VPERMILPS |
      Opcode::VPERM2F128 |
      Opcode::VZEROUPPER |
      Opcode::VZEROALL => {
        if !decoder.feature_avx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::HRESET => {
        if !decoder.feature_hreset() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VP2INTERSECTD |
      Opcode::VP2INTERSECTQ => {
        if !decoder.feature_avx512_vp2intersect() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::JMPE => {
        if !decoder.feature_itanium() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPRU => {
        if !decoder.feature_rdpru() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CRC32 |
      Opcode::PCMPESTRI |
      Opcode::PCMPESTRM |
      Opcode::PCMPISTRI |
      Opcode::PCMPISTRM |
      Opcode::PCMPGTQ => {
        if !decoder.feature_sse4_2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MONITOR |
      Opcode::MWAIT => {
        if !decoder.feature_monitor() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPRORRD |
      Opcode::VPRORRQ => {
        if !decoder.feature_avx512_f_typo() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PHMINPOSUW |
      Opcode::PMULDQ |
      Opcode::PMULLD |
      Opcode::DPPS |
      Opcode::DPPD |
      Opcode::PACKUSDW |
      Opcode::PCMPEQQ |
      Opcode::PTEST |
      Opcode::MOVNTDQA |
      Opcode::ROUNDSS |
      Opcode::ROUNDSD |
      Opcode::ROUNDPS |
      Opcode::ROUNDPD |
      Opcode::PMAXSB |
      Opcode::PMAXSD |
      Opcode::PMAXUW |
      Opcode::PMAXUD |
      Opcode::PMINSD |
      Opcode::PMINSB |
      Opcode::PMINUD |
      Opcode::PMINUW |
      Opcode::BLENDW |
      Opcode::PBLENDW |
      Opcode::BLENDVPS |
      Opcode::BLENDVPD |
      Opcode::PBLENDVB |
      Opcode::BLENDPS |
      Opcode::BLENDPD |
      Opcode::MPSADBW |
      Opcode::PMOVZXDQ |
      Opcode::PMOVSXDQ |
      Opcode::PMOVZXBD |
      Opcode::PMOVSXBD |
      Opcode::PMOVZXWQ |
      Opcode::PMOVSXWQ |
      Opcode::PMOVZXBQ |
      Opcode::PMOVSXBQ |
      Opcode::PMOVSXWD |
      Opcode::PMOVZXWD |
      Opcode::PEXTRQ |
      Opcode::PEXTRD |
      Opcode::PEXTRB |
      Opcode::PMOVSXBW |
      Opcode::PMOVZXBW |
      Opcode::PINSRQ |
      Opcode::PINSRD |
      Opcode::PINSRB |
      Opcode::EXTRACTPS |
      Opcode::INSERTPS => {
        if !decoder.feature_sse4_1() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::KANDB |
      Opcode::KANDNB |
      Opcode::KADDB |
      Opcode::KTESTB |
      Opcode::KADDW |
      Opcode::KTESTW |
      Opcode::KMOVB |
      Opcode::KNOTB |
      Opcode::KORB |
      Opcode::KORTESTB |
      Opcode::KSHIFTLB |
      Opcode::KSHIFTRB |
      Opcode::KXNORB |
      Opcode::KXORB |
      Opcode::VBROADCASTF32X2 |
      Opcode::VBROADCASTF64X2 |
      Opcode::VBROADCASTF32X8 |
      Opcode::VBROADCASTI32X8 |
      Opcode::VBROADCASTI64X2 |
      Opcode::VBROADCASTI32X2 |
      Opcode::VEXTRACTF32X8 |
      Opcode::VEXTRACTI32X8 |
      Opcode::VGETEXPSD |
      Opcode::VGETEXPSS |
      Opcode::VXORPD |
      Opcode::VXORPS |
      Opcode::VPEXTRD |
      Opcode::VPEXTRQ |
      Opcode::VPINSRD |
      Opcode::VPINSRQ |
      Opcode::VANDNPD |
      Opcode::VANDNPS |
      Opcode::VANDPD |
      Opcode::VANDPS |
      Opcode::VORPD |
      Opcode::VORPS |
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
      Opcode::VEXTRACTF64X2 |
      Opcode::VEXTRACTI64X2 |
      Opcode::VFPCLASSPD |
      Opcode::VFPCLASSPS |
      Opcode::VFPCLASSSD |
      Opcode::VFPCLASSSS |
      Opcode::VINSERTF64X2 |
      Opcode::VINSERTF32X8 |
      Opcode::VINSERTI32X8 |
      Opcode::VINSERTI64X2 |
      Opcode::VPMOVM2D |
      Opcode::VPMOVM2Q |
      Opcode::VPMOVB2D |
      Opcode::VPMOVQ2M |
      Opcode::VRANGEPD |
      Opcode::VRANGEPS |
      Opcode::VRANGESD |
      Opcode::VRANGESS |
      Opcode::VREDUCEPD |
      Opcode::VREDUCEPS |
      Opcode::VREDUCESD |
      Opcode::VREDUCESS => {
        if !decoder.feature_avx512_dq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ADCX |
      Opcode::ADOX => {
        if !decoder.feature_adx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::FXSAVE |
      Opcode::FXRSTOR => {
        if !decoder.feature_fxsr() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SLHD => {
        if !decoder.feature_typo_instructions() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XABORT |
      Opcode::XBEGIN |
      Opcode::XEND |
      Opcode::XTEST => {
        if !decoder.feature_tsx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CMPXCHG16B => {
        if !decoder.feature_cmpxchg16b() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::TPAUSE |
      Opcode::UMONITOR |
      Opcode::UMWAIT => {
        if !decoder.feature_waitpkg() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLTS |
      Opcode::LAR |
      Opcode::LGDT |
      Opcode::LIDT |
      Opcode::LLDT |
      Opcode::LMSW |
      Opcode::LSL |
      Opcode::SGDT |
      Opcode::SIDT |
      Opcode::SLDT |
      Opcode::SMSW |
      Opcode::STR |
      Opcode::LTR |
      Opcode::VERR |
      Opcode::VERW => {
        if !decoder.feature_80286() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SYSENTER |
      Opcode::SYSEXIT => {
        if !decoder.feature_sysenter() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVVPID => {
        if !decoder.feature_invpcid_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VP4DPWSSDS |
      Opcode::VP4DPWSSD => {
        if !decoder.feature_avx512_4vnniw() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPDPBUSDS |
      Opcode::VPDPBUSD |
      Opcode::VPDPWSSDS |
      Opcode::VPDPWSSD => {
        if !decoder.feature_avx512_vnni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VGATHERPF0DPD |
      Opcode::VGATHERPF0DPS |
      Opcode::VGATHERPF0QPD |
      Opcode::VGATHERPF0QPS |
      Opcode::VGATHERPF1DPD |
      Opcode::VGATHERPF1DPS |
      Opcode::VGATHERPF1QPD |
      Opcode::VGATHERPF1QPS |
      Opcode::VSCATTERPF0DPD |
      Opcode::VSCATTERPF0DPS |
      Opcode::VSCATTERPF0QPD |
      Opcode::VSCATTERPF0QPS |
      Opcode::VSCATTERPF1DPD |
      Opcode::VSCATTERPF1DPS |
      Opcode::VSCATTERPF1QPD |
      Opcode::VSCATTERPF1QPS => {
        if !decoder.feature_avx512_pf() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPBROADCASTMW2D |
      Opcode::VPBROADCASTMB2Q |
      Opcode::VPBROADCASTM |
      Opcode::VPCONFLICTD |
      Opcode::VPCONFLICTQ |
      Opcode::VPLZCNTD |
      Opcode::VPLZCNTQ => {
        if !decoder.feature_avx512_cd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VCVTNE2PS2BF16 |
      Opcode::VCVTNEPS2BF16 |
      Opcode::VDPBF16PS => {
        if !decoder.feature_avx512_bf16() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMOVQ2M => {
        if !decoder.feature_avx512bw_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PCLMULQDQ => {
        if !decoder.feature_pclmulqdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VSCATTERDPS |
      Opcode::VSCATTERDPD |
      Opcode::VSCATTERQPS |
      Opcode::VSCATTERQPD => {
        if !decoder.feature_avx512_f__vl_unimplemented() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::WRUSS |
      Opcode::WRSS |
      Opcode::INCSSP |
      Opcode::SAVEPREVSSP |
      Opcode::SETSSBSY |
      Opcode::CLRSSBSY |
      Opcode::RSTORSSP |
      Opcode::ENDBR64 |
      Opcode::ENDBR32 => {
        if !decoder.feature_cet() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDSEED => {
        if !decoder.feature_rdseed() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BSWAP |
      Opcode::CMPXCHG |
      Opcode::INVD |
      Opcode::WBINVD |
      Opcode::INVLPG |
      Opcode::XADD => {
        if !decoder.feature_80486() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDFSBASE |
      Opcode::RDGSBASE |
      Opcode::WRFSBASE |
      Opcode::WRGSBASE => {
        if !decoder.feature_fsgsbase() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::UD2 => {
        if !decoder.feature_pentium_pro() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VCVTPH2PS |
      Opcode::VCVTPS2PH => {
        if !decoder.feature_f16c() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BNDMK |
      Opcode::BNDCL |
      Opcode::BNDCU |
      Opcode::BNDCN |
      Opcode::BNDMOV |
      Opcode::BNDLDX |
      Opcode::BNDSTX => {
        if !decoder.feature_mpx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDTSCP => {
        if !decoder.feature_rdtscp() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VBROADCASTI128 |
      Opcode::VPBROADCASTB |
      Opcode::VPBROADCASTW |
      Opcode::VPBROADCASTD |
      Opcode::VPBROADCASTQ |
      Opcode::VINSERTI128 |
      Opcode::VEXTRACTI128 |
      Opcode::VPMASKMOVD |
      Opcode::VPMASKMOVQ |
      Opcode::VPERMPS |
      Opcode::VPERMD |
      Opcode::VPERMPD |
      Opcode::VPERMQ |
      Opcode::VPERM2I128 |
      Opcode::VPBLENDD |
      Opcode::VPSLLVD |
      Opcode::VPSLLVQ |
      Opcode::VPSRLVD |
      Opcode::VPSRLVQ |
      Opcode::VPSRAVD => {
        if !decoder.feature_avx2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PCONFIG => {
        if !decoder.feature_pconfig() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPMADD52HUQ |
      Opcode::VPMADD52LUQ => {
        if !decoder.feature_avx512_ifma() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VEXP2PD |
      Opcode::VEXP2PS |
      Opcode::VEXP2SD |
      Opcode::VEXP2SS |
      Opcode::VRCP28PD |
      Opcode::VRCP28PS |
      Opcode::VRCP28SD |
      Opcode::VRCP28SS |
      Opcode::VRSQRT28PD |
      Opcode::VRSQRT28PS |
      Opcode::VRSQRT28SD |
      Opcode::VRSQRT28SS => {
        if !decoder.feature_avx512_er() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPCLMULQDQ => {
        if !decoder.feature_vpclmulqdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BSF |
      Opcode::BSR |
      Opcode::BT |
      Opcode::BTS |
      Opcode::BTC |
      Opcode::BTR |
      Opcode::CDQ |
      Opcode::CWDE |
      Opcode::LDS |
      Opcode::LES |
      Opcode::LFS |
      Opcode::LGS |
      Opcode::LSS |
      Opcode::MOVZX |
      Opcode::MOVSX |
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
      Opcode::SETG |
      Opcode::SHLD |
      Opcode::SHRD => {
        if !decoder.feature_80386() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVBE => {
        if !decoder.feature_movbe() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VCVTQQ2PD |
      Opcode::VCVTQQ2PS |
      Opcode::VCVTUSI2USD |
      Opcode::VCVTUSI2USS |
      Opcode::VEXTRACTF32X4 |
      Opcode::VEXTRACTF64X4 |
      Opcode::VEXTRACTI32X4 |
      Opcode::VEXTRACTI64X4 |
      Opcode::VFIXUPIMMSD |
      Opcode::VFIXUPIMMSS |
      Opcode::VINSERTI64X4 |
      Opcode::VMOVDQA32 |
      Opcode::VMOVDQA64 |
      Opcode::VMOVDQU32 |
      Opcode::VMOVDQU64 |
      Opcode::VPCOMPRESSQ |
      Opcode::VPCOMPRESSD |
      Opcode::VSCALEDPD |
      Opcode::VSCALEDPS |
      Opcode::VSCALEDSD |
      Opcode::VSCALEDSS |
      Opcode::VSCATTERDD |
      Opcode::VSCATTERDQ |
      Opcode::VSCATTERQD |
      Opcode::VSCATTERQQ |
      Opcode::VADDPD |
      Opcode::VADDPS |
      Opcode::VCMPPD |
      Opcode::VCMPPS |
      Opcode::VCVTDQ2PD |
      Opcode::VCVTDQ2PS |
      Opcode::VCVTPD2DQ |
      Opcode::VCVTPD2PS |
      Opcode::VCVTPS2DQ |
      Opcode::VCVTPS2PD |
      Opcode::VCVTTPD2DQ |
      Opcode::VCVTTPS2DQ |
      Opcode::VDIVPD |
      Opcode::VDIVPS |
      Opcode::VMAXPD |
      Opcode::VMAXPS |
      Opcode::VMINPD |
      Opcode::VMINPS |
      Opcode::VMOVAPD |
      Opcode::VMOVAPS |
      Opcode::VMOVDDUP |
      Opcode::VMOVDQA |
      Opcode::VMOVDQU |
      Opcode::VMOVNTDQA |
      Opcode::VMOVNTDQ |
      Opcode::VMOVNTPD |
      Opcode::VMOVNTPS |
      Opcode::VMOVSHDUP |
      Opcode::VMOVSLDUP |
      Opcode::VMOVUPD |
      Opcode::VMOVUPS |
      Opcode::VMULPD |
      Opcode::VMULPS |
      Opcode::VPANDD |
      Opcode::VPANDQ |
      Opcode::VPANDND |
      Opcode::VPANDNQ |
      Opcode::VPCMPEQD |
      Opcode::VPCMPEQQ |
      Opcode::VPCMPGTD |
      Opcode::VPCMPGTQ |
      Opcode::VPMAXSD |
      Opcode::VPMAXSQ |
      Opcode::VPMAXUD |
      Opcode::VPMAXUQ |
      Opcode::VPMINSD |
      Opcode::VPMINSQ |
      Opcode::VPMINUD |
      Opcode::VPMINUQ |
      Opcode::VPCMPD |
      Opcode::VPCMPQ |
      Opcode::VPCMPUD |
      Opcode::VPCMPUQ |
      Opcode::VPORD |
      Opcode::VPORQ |
      Opcode::VPXORD |
      Opcode::VPXORQ |
      Opcode::VPSLLD |
      Opcode::VPSLLQ |
      Opcode::VPSLLW |
      Opcode::VPSRAD |
      Opcode::VPSRAQ |
      Opcode::VALIGND |
      Opcode::VALIGNQ |
      Opcode::VBLENDMPD |
      Opcode::VBLENDMPS |
      Opcode::VPROLD |
      Opcode::VPROLQ |
      Opcode::VPROLVD |
      Opcode::VPROLVQ |
      Opcode::VPRORD |
      Opcode::VPRORQ |
      Opcode::VPRORVD |
      Opcode::VPRORVQ |
      Opcode::VPUNPCKHDQ |
      Opcode::VPUNPCKHQDQ |
      Opcode::VPUNPCKLDQ |
      Opcode::VPUNPCKLQDQ |
      Opcode::VUNPCKHPD |
      Opcode::VUNPCKHPS |
      Opcode::VUNPCKLPD |
      Opcode::VUNPCKLPS |
      Opcode::VBROADCASTF32X4 |
      Opcode::VBROADCASTF64X4 |
      Opcode::VBROADCASTI64X4 |
      Opcode::VBROADCASTI32X4 |
      Opcode::VINSERTF32X4 |
      Opcode::VINSERTI32X4 |
      Opcode::VSHUFF32X4 |
      Opcode::VSHUFF64X2 |
      Opcode::VSHUFI32X4 |
      Opcode::VSHUFI64X2 |
      Opcode::VCOMPRESSD |
      Opcode::VCOMPRESSQ |
      Opcode::VCOMPRESSPD |
      Opcode::VCOMPRESSPS |
      Opcode::VEXPANDPD |
      Opcode::VEXPANDPS |
      Opcode::VCVTPD2UDQ |
      Opcode::VCVTPS2UDQ |
      Opcode::VCVTUDQ2PD |
      Opcode::VCVTUDQ2PS |
      Opcode::VCVTTPD2UDQ |
      Opcode::VCVTTPS2UDQ |
      Opcode::VFIXUPIMMPD |
      Opcode::VFIXUPIMMPS |
      Opcode::VCVTPH2PS |
      Opcode::VCVTPS2PH |
      Opcode::VFMADD132PD |
      Opcode::VFMADD132PS |
      Opcode::VFMADD213PD |
      Opcode::VFMADD213PS |
      Opcode::VFMADD231PD |
      Opcode::VFMADD231PS |
      Opcode::VFMADDSUB132PD |
      Opcode::VFMADDSUB132PS |
      Opcode::VFMADDSUB213PD |
      Opcode::VFMADDSUB213PS |
      Opcode::VFMADDSUB231PD |
      Opcode::VFMADDSUB231PS |
      Opcode::VFMSUB132PD |
      Opcode::VFMSUB132PS |
      Opcode::VFMSUB213PD |
      Opcode::VFMSUB213PS |
      Opcode::VFMSUB231PD |
      Opcode::VFMSUB231PS |
      Opcode::VFMSUBADD132PD |
      Opcode::VFMSUBADD132PS |
      Opcode::VFMSUBADD213PD |
      Opcode::VFMSUBADD213PS |
      Opcode::VFMSUBADD231PD |
      Opcode::VFMSUBADD231PS |
      Opcode::VFNMADD132PD |
      Opcode::VFNMADD132PS |
      Opcode::VFNMADD213PD |
      Opcode::VFNMADD213PS |
      Opcode::VFNMADD231PD |
      Opcode::VFNMADD231PS |
      Opcode::VFNMSUB132PD |
      Opcode::VFNMSUB132PS |
      Opcode::VFNMSUB213PD |
      Opcode::VFNMSUB213PS |
      Opcode::VFNMSUB231PD |
      Opcode::VFNMSUB231PS |
      Opcode::VSCATTERDPS |
      Opcode::VSCATTERDPD |
      Opcode::VSCATTERQPS |
      Opcode::VSCATTERQPD |
      Opcode::VGATHERDPD |
      Opcode::VGATHERDPS |
      Opcode::VGATHERQPD |
      Opcode::VGATHERQPS |
      Opcode::VGETEXPPD |
      Opcode::VGETEXPPS |
      Opcode::VGETMANTPD |
      Opcode::VGETMANTPS |
      Opcode::VPBLENDMD |
      Opcode::VPBLENDMQ |
      Opcode::VPERMD |
      Opcode::VPERMQ |
      Opcode::VPERMI2D |
      Opcode::VPERMI2Q |
      Opcode::VPERMI2PD |
      Opcode::VPERMI2PS |
      Opcode::VPERMT2D |
      Opcode::VPERMT2Q |
      Opcode::VPERMT2PD |
      Opcode::VPERMT2PS |
      Opcode::VPEXPANDD |
      Opcode::VPEXPANDQ |
      Opcode::VPGATHERDD |
      Opcode::VPGATHERDQ |
      Opcode::VPGATHERQD |
      Opcode::VPGATHERQQ |
      Opcode::VPSCATTERDD |
      Opcode::VPSCATTERDQ |
      Opcode::VPSCATTERQD |
      Opcode::VPSCATTERQQ |
      Opcode::VPMOVDB |
      Opcode::VPMOVSDB |
      Opcode::VPMOVUSDB |
      Opcode::VPMOVDW |
      Opcode::VPMOVSDW |
      Opcode::VPMOVUSDW |
      Opcode::VPMOVQB |
      Opcode::VPMOVSQB |
      Opcode::VPMOVUSQB |
      Opcode::VPMOVQD |
      Opcode::VPMOVSQD |
      Opcode::VPMOVUSQD |
      Opcode::VPMOVQW |
      Opcode::VPMOVSQW |
      Opcode::VPMOVUSQW |
      Opcode::VPSRAVQ |
      Opcode::VPTERNLOGD |
      Opcode::VPTERNLOGQ |
      Opcode::VPTESTMD |
      Opcode::VPTESTMQ |
      Opcode::VPTESTNMD |
      Opcode::VPTESTNMQ |
      Opcode::VRCP14PD |
      Opcode::VRCP14PS |
      Opcode::VRNDSCALEPD |
      Opcode::VRNDSCALEPS |
      Opcode::VRSQRT14PD |
      Opcode::VRSQRT14PS |
      Opcode::VSCALEFPS |
      Opcode::VSCALEFPD |
      Opcode::VPABSQ |
      Opcode::VADDSD |
      Opcode::VADDSS |
      Opcode::VCMPSD |
      Opcode::VCMPSS |
      Opcode::VCOMISD |
      Opcode::VCOMISS |
      Opcode::VCVTSD2SI |
      Opcode::VCVTSD2SS |
      Opcode::VCVTSI2SS |
      Opcode::VCVTSI2SD |
      Opcode::VCVTSS2SD |
      Opcode::VCVTSS2SI |
      Opcode::VCVTTSS2SI |
      Opcode::VCVTTSD2SI |
      Opcode::VDIVSD |
      Opcode::VDIVSS |
      Opcode::VEXTRACTPS |
      Opcode::VINSERTPS |
      Opcode::VGETMANTSD |
      Opcode::VGETMANTSS |
      Opcode::VMOVD |
      Opcode::VMOVQ |
      Opcode::VMOVHLPS |
      Opcode::VMOVHPD |
      Opcode::VMOVHPS |
      Opcode::VMOVLHPS |
      Opcode::VMOVLPD |
      Opcode::VMOVLPS |
      Opcode::VMOVSS |
      Opcode::VMOVSD |
      Opcode::VSQRTSS |
      Opcode::VSQRTSD |
      Opcode::VSUBSD |
      Opcode::VSUBSS |
      Opcode::VUCOMISD |
      Opcode::VUCOMISS |
      Opcode::VRCP14SD |
      Opcode::VRCP14SS |
      Opcode::VRNDSCALESD |
      Opcode::VRNDSCALESS |
      Opcode::VRSQRT14SD |
      Opcode::VRSQRT14SS |
      Opcode::VSCALEFSS |
      Opcode::VSCALEFSD |
      Opcode::VINSERTF64X4 |
      Opcode::VCVTUSI2SD |
      Opcode::VCVTUSI2SS |
      Opcode::VCVTSD2USI |
      Opcode::VCVTSS2USI |
      Opcode::VCVTTSD2USI |
      Opcode::VCVTTSS2USI |
      Opcode::KANDW |
      Opcode::KANDNW |
      Opcode::KMOVW |
      Opcode::KNOTW |
      Opcode::KORW |
      Opcode::KORTESTW |
      Opcode::KSHIFTLW |
      Opcode::KSHIFTRW |
      Opcode::KUNPCKBW |
      Opcode::KXNORW |
      Opcode::KXORW => {
        if !decoder.feature_avx512_f() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MONITORX |
      Opcode::MWAITX => {
        if !decoder.feature_emx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::VFNMSUB231SS => {
        if !decoder.feature_fma3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VMPTRLD |
      Opcode::VMPTRST |
      Opcode::VMCLEAR |
      Opcode::VMREAD |
      Opcode::VMWRITE |
      Opcode::VMCALL |
      Opcode::VMLAUNCH |
      Opcode::VMRESUME |
      Opcode::VMXOFF |
      Opcode::VMXON |
      Opcode::INVEPT |
      Opcode::INVVPID |
      Opcode::VMFUNC => {
        if !decoder.feature_vmx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XSUSLDTRK |
      Opcode::XRESLDTRK => {
        if !decoder.feature_tsxldtrk() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SYSCALL |
      Opcode::SYSRET => {
        if !decoder.feature_syscall() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::EXTRQ |
      Opcode::INSERTQ |
      Opcode::MOVNTSD |
      Opcode::MOVNTSS => {
        if !decoder.feature_sse4a() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPOPCNTD |
      Opcode::VPOPCNTQ => {
        if !decoder.feature_avx512_vpopcntdq() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::INVEPT |
      Opcode::INVVPID |
      Opcode::INVPCID => {
        if !decoder.feature_invpcid() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XGETBV |
      Opcode::XRSTOR |
      Opcode::XRSTORS |
      Opcode::XSAVE |
      Opcode::XSAVEC |
      Opcode::XSAVES |
      Opcode::XSETBV => {
        if !decoder.feature_xsave() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::CMOVZ => {
        if !decoder.feature_cmov() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::KANDQ |
      Opcode::KANDD |
      Opcode::KANDNQ |
      Opcode::KANDND |
      Opcode::KMOVD |
      Opcode::KMOVQ |
      Opcode::KNOTD |
      Opcode::KNOTQ |
      Opcode::KORD |
      Opcode::KORQ |
      Opcode::KADDD |
      Opcode::KTESTD |
      Opcode::KADDQ |
      Opcode::KTESTQ |
      Opcode::KORTESTD |
      Opcode::KORTESTQ |
      Opcode::KSHIFTLD |
      Opcode::KSHIFTRD |
      Opcode::KSHIFTLQ |
      Opcode::KSHIFTRQ |
      Opcode::KUNPCKWD |
      Opcode::KUNPCKDQ |
      Opcode::KXNORD |
      Opcode::KXNORQ |
      Opcode::KXORD |
      Opcode::KXORQ |
      Opcode::VPCMPEQB |
      Opcode::VPCMPEQW |
      Opcode::VPCMPGTB |
      Opcode::VPCMPGTW |
      Opcode::VPSUBUSB |
      Opcode::VPSUBUSW |
      Opcode::VPERMW |
      Opcode::VPERMI2W |
      Opcode::VPERMT2W |
      Opcode::VPSLLVW |
      Opcode::VPSRAVW |
      Opcode::VPSRLVW |
      Opcode::VPEXTRB |
      Opcode::VPEXTRW |
      Opcode::VPINSRB |
      Opcode::VPINSRW |
      Opcode::VPMULHUW |
      Opcode::VPMULHRSW |
      Opcode::VPADDSB |
      Opcode::VPADDSW |
      Opcode::VPADDUSB |
      Opcode::VPADDUSW |
      Opcode::VPALIGNR |
      Opcode::VPMOVD2M |
      Opcode::VPMOVQ2M |
      Opcode::VPMOVWB |
      Opcode::VDBPSADBW |
      Opcode::VMOVDQU8 |
      Opcode::VMOVDQU16 |
      Opcode::VPBLENDMB |
      Opcode::VPBLENDMW |
      Opcode::VPCMPB |
      Opcode::VPCMPUB |
      Opcode::VPCMPW |
      Opcode::VPCMPUW |
      Opcode::VPERMW |
      Opcode::VPERMI2B |
      Opcode::VPERMI2W |
      Opcode::VPMOVM2B |
      Opcode::VPMOVM2W |
      Opcode::VPMOVB2M |
      Opcode::VPMOVW2M |
      Opcode::VPMOVSWB |
      Opcode::VPMOVUSWB |
      Opcode::VPSLLVW |
      Opcode::VPSRAVW |
      Opcode::VPSRLVW |
      Opcode::VPTESTNMB |
      Opcode::VPTESTNMW |
      Opcode::VPTESTMB |
      Opcode::VPTESTMW => {
        if !decoder.feature_avx512_bw() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PALIGNR |
      Opcode::PSIGNW |
      Opcode::PSIGND |
      Opcode::PSIGNB |
      Opcode::PSHUFB |
      Opcode::PMULHRSW |
      Opcode::PMADDUBSW |
      Opcode::PABSD |
      Opcode::PABSW |
      Opcode::PABSB |
      Opcode::PHSUBSW |
      Opcode::PHSUBW |
      Opcode::PHSUBD |
      Opcode::PHADDD |
      Opcode::PHADDSW |
      Opcode::PHADDW => {
        if !decoder.feature_ssse3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLFLUSHOPT => {
        if !decoder.feature_clflushopt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::SHA1RNDS4 |
      Opcode::SHA1NEXTE |
      Opcode::SHA1MSG1 |
      Opcode::SHA1MSG2 |
      Opcode::SHA256RNDS2 |
      Opcode::SHA256MSG1 |
      Opcode::SHA256MSG2 => {
        if !decoder.feature_sha() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::JRCXZ |
      Opcode::CDQE |
      Opcode::MOVZX |
      Opcode::SWAPGS |
      Opcode::MOVSXD => {
        if !decoder.feature_64bit() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::POPCNT |
      Opcode::LZCNT => {
        if !decoder.feature_abm() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PREFETCHW => {
        if !decoder.feature_3dnowprefetch() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPID => {
        if !decoder.feature_rdpid() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::LAHF |
      Opcode::SAHF => {
        if !decoder.feature_lahfsahf() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ENQCMD |
      Opcode::ENQCMDS => {
        if !decoder.feature_enqcmd() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PTWRITE => {
        if !decoder.feature_ptwrite() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPERMT2B |
      Opcode::VPERMB => {
        if !decoder.feature_avx512_vbmi() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::F2XM1 |
      Opcode::FABS |
      Opcode::FADD |
      Opcode::FADDP |
      Opcode::FBLD |
      Opcode::FBSTP |
      Opcode::FCHS |
      Opcode::FCMOVB |
      Opcode::FCMOVBE |
      Opcode::FCMOVE |
      Opcode::FCMOVNB |
      Opcode::FCMOVNBE |
      Opcode::FCMOVNE |
      Opcode::FCMOVNU |
      Opcode::FCMOVU |
      Opcode::FCOM |
      Opcode::FCOMI |
      Opcode::FCOMIP |
      Opcode::FCOMP |
      Opcode::FCOMPP |
      Opcode::FCOS |
      Opcode::FDECSTP |
      Opcode::FDISI8087_NOP |
      Opcode::FDIV |
      Opcode::FDIVP |
      Opcode::FDIVR |
      Opcode::FDIVRP |
      Opcode::FENI8087_NOP |
      Opcode::FFREE |
      Opcode::FFREEP |
      Opcode::FIADD |
      Opcode::FICOM |
      Opcode::FICOMP |
      Opcode::FIDIV |
      Opcode::FIDIVR |
      Opcode::FILD |
      Opcode::FIMUL |
      Opcode::FINCSTP |
      Opcode::FIST |
      Opcode::FISTP |
      Opcode::FISTTP |
      Opcode::FISUB |
      Opcode::FISUBR |
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
      Opcode::FMUL |
      Opcode::FMULP |
      Opcode::FNCLEX |
      Opcode::FNINIT |
      Opcode::FNOP |
      Opcode::FNSAVE |
      Opcode::FNSTCW |
      Opcode::FNSTENV |
      Opcode::FNSTOR |
      Opcode::FNSTSW |
      Opcode::FPATAN |
      Opcode::FPREM |
      Opcode::FPREM1 |
      Opcode::FPTAN |
      Opcode::FRNDINT |
      Opcode::FRSTOR |
      Opcode::FSCALE |
      Opcode::FSETPM287_NOP |
      Opcode::FSIN |
      Opcode::FSINCOS |
      Opcode::FSQRT |
      Opcode::FST |
      Opcode::FSTP |
      Opcode::FSTPNCE |
      Opcode::FSUB |
      Opcode::FSUBP |
      Opcode::FSUBR |
      Opcode::FSUBRP |
      Opcode::FTST |
      Opcode::FUCOM |
      Opcode::FUCOMI |
      Opcode::FUCOMIP |
      Opcode::FUCOMP |
      Opcode::FUCOMPP |
      Opcode::FXAM |
      Opcode::FXCH |
      Opcode::FXTRACT |
      Opcode::FYL2X |
      Opcode::FYL2XP1 => {
        if !decoder.feature_x87() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPMC |
      Opcode::PUNPCKLBW |
      Opcode::PUNPCKLWD |
      Opcode::PUNPCKLDQ |
      Opcode::PACKSSWB |
      Opcode::PCMPGTB |
      Opcode::PCMPGTD |
      Opcode::PCMPGTW |
      Opcode::PACKUSWB |
      Opcode::PUNPCKHBW |
      Opcode::PUNPCKHWD |
      Opcode::PUNPCKHDQ |
      Opcode::PACKSSDW |
      Opcode::MOVD |
      Opcode::MOVQ |
      Opcode::PCMPEQB |
      Opcode::PCMPEQD |
      Opcode::PCMPEQW |
      Opcode::PSRLW |
      Opcode::PSRLD |
      Opcode::PSRLQ |
      Opcode::PMULLW |
      Opcode::PSUBUSB |
      Opcode::PSUBUSW |
      Opcode::PAND |
      Opcode::PADDUSB |
      Opcode::PADDUSW |
      Opcode::PANDN |
      Opcode::PSRAW |
      Opcode::PSRAD |
      Opcode::PMULHW |
      Opcode::PSUBSB |
      Opcode::PSUBSW |
      Opcode::POR |
      Opcode::PADDSB |
      Opcode::PADDSW |
      Opcode::PXOR |
      Opcode::PSLLW |
      Opcode::PSLLD |
      Opcode::PSLLQ |
      Opcode::PMADDWD |
      Opcode::PSUBB |
      Opcode::PSUBW |
      Opcode::PSUBD |
      Opcode::PADDB |
      Opcode::PADDW |
      Opcode::PADDD |
      Opcode::EMMS => {
        if !decoder.feature_mmx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPOPCNTD |
      Opcode::VPOPCNTQ |
      Opcode::VPOPCNTB |
      Opcode::VPOPCNTW |
      Opcode::VPSHUFBITQMB |
      Opcode::VPMULTISHIFTQB => {
        if !decoder.feature_avx512_bitalg() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
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
      Opcode::LOADIWKEY => {
        if !decoder.feature_keylocker() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ENTER |
      Opcode::LEAVE |
      Opcode::INS |
      Opcode::OUTS => {
        if !decoder.feature_80186() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLWB => {
        if !decoder.feature_clwb() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDPKRU |
      Opcode::WRPKRU => {
        if !decoder.feature_mpk() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::V4FNMADDSS |
      Opcode::V4FNMADDPS |
      Opcode::V4FMADDSS |
      Opcode::V4FMADDPS => {
        if !decoder.feature_avx512_4fmaps() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::TDCALL |
      Opcode::SEAMRET |
      Opcode::SEAMOPS |
      Opcode::SEAMCALL => {
        if !decoder.feature_tdx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::POPCNT => {
        if !decoder.feature_popcnt() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::PSMASH |
      Opcode::PVALIDATE |
      Opcode::RMPADJUST |
      Opcode::RMPUPDATE => {
        if !decoder.feature_snp() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VGF2P8AFFINEQB |
      Opcode::VGF2P8AFFINEINVQB |
      Opcode::VGF2P8MULB => {
        if !decoder.feature_avx512_gfni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::FEMMS |
      Opcode::PI2FW |
      Opcode::PI2FD |
      Opcode::PF2IW |
      Opcode::PF2ID |
      Opcode::PMULHRW |
      Opcode::PFCMPGE |
      Opcode::PFMIN |
      Opcode::PFRCP |
      Opcode::PFRSQRT |
      Opcode::PFSUB |
      Opcode::PFADD |
      Opcode::PFCMPGT |
      Opcode::PFMAX |
      Opcode::PFRCPIT1 |
      Opcode::PFRSQIT1 |
      Opcode::PFSUBR |
      Opcode::PFACC |
      Opcode::PFCMPEQ |
      Opcode::PFMUL |
      Opcode::PFMULHRW |
      Opcode::PFRCPIT2 |
      Opcode::PFNACC |
      Opcode::PFPNACC |
      Opcode::PSWAPD |
      Opcode::PAVGUSB => {
        if !decoder.feature_3dnow() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VPCOMPRESSB |
      Opcode::VPCOMPRESSW |
      Opcode::VPSHLDVW |
      Opcode::VPSHLDW |
      Opcode::VPEXPANDB |
      Opcode::VPEXPANDW |
      Opcode::VPSHRDVW |
      Opcode::VPSHRDW |
      Opcode::VPSHLDVQ |
      Opcode::VPSHLDVD |
      Opcode::VPSHLDQ |
      Opcode::VPSHLDD |
      Opcode::VPSHRDQ |
      Opcode::VPSHRDD |
      Opcode::VPSHRDVQ |
      Opcode::VPSHRDVD => {
        if !decoder.feature_avx512_vbmi2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CMC |
      Opcode::CLC |
      Opcode::STC |
      Opcode::CLI |
      Opcode::STI |
      Opcode::CLD |
      Opcode::STD |
      Opcode::ADD |
      Opcode::OR |
      Opcode::ADC |
      Opcode::SBB |
      Opcode::AND |
      Opcode::XOR |
      Opcode::SUB |
      Opcode::CMP |
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
      Opcode::HLT |
      Opcode::CALL |
      Opcode::CALLF |
      Opcode::JMP |
      Opcode::JMPF |
      Opcode::PUSH |
      Opcode::POP |
      Opcode::LEA |
      Opcode::NOP |
      Opcode::XCHG |
      Opcode::POPF |
      Opcode::INT |
      Opcode::INTO |
      Opcode::IRET |
      Opcode::IRETD |
      Opcode::IRETQ |
      Opcode::RETF |
      Opcode::ENTER |
      Opcode::LEAVE |
      Opcode::MOV |
      Opcode::RETURN |
      Opcode::PUSHF |
      Opcode::WAIT |
      Opcode::CBW |
      Opcode::CWD |
      Opcode::CQO |
      Opcode::LODS |
      Opcode::STOS |
      Opcode::CMPS |
      Opcode::SCAS |
      Opcode::MOVS |
      Opcode::TEST |
      Opcode::IN |
      Opcode::OUT |
      Opcode::IMUL |
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
      Opcode::JG |
      Opcode::UD0 |
      Opcode::UD1 |
      Opcode::UD2 |
      Opcode::DIV |
      Opcode::IDIV |
      Opcode::MUL |
      Opcode::NEG |
      Opcode::NOT |
      Opcode::XLAT |
      Opcode::LOOPNZ |
      Opcode::LOOPZ |
      Opcode::LOOP |
      Opcode::SALC => {
        if !decoder.feature_8086() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLGI |
      Opcode::STGI |
      Opcode::SKINIT |
      Opcode::VMLOAD |
      Opcode::VMMCALL |
      Opcode::VMSAVE |
      Opcode::VMRUN |
      Opcode::INVLPGA => {
        if !decoder.feature_svm() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ADDSUBPD |
      Opcode::ADDSUBPS |
      Opcode::HSUBPD |
      Opcode::HADDPD |
      Opcode::MOVSLDUP |
      Opcode::MOVSHDUP |
      Opcode::MOVDDUP |
      Opcode::HADDPS |
      Opcode::HSUBPS |
      Opcode::LDDQU => {
        if !decoder.feature_sse3() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVUPD |
      Opcode::PSRLDQ |
      Opcode::PSLLDQ |
      Opcode::MOVSD |
      Opcode::MOVLPD |
      Opcode::UNPCKLPD |
      Opcode::UNPCKHPD |
      Opcode::MOVHPD |
      Opcode::MOVAPD |
      Opcode::MOVMSKPD |
      Opcode::CVTPI2PD |
      Opcode::CVTSI2SD |
      Opcode::MOVNTPD |
      Opcode::MOVNTI |
      Opcode::MOVNTDQ |
      Opcode::CVTTPD2PI |
      Opcode::CVTTSD2SI |
      Opcode::CVTPD2PI |
      Opcode::CVTSD2SI |
      Opcode::UCOMISD |
      Opcode::COMISD |
      Opcode::SQRTPD |
      Opcode::SQRTSD |
      Opcode::ANDPD |
      Opcode::ANDNPD |
      Opcode::ORPD |
      Opcode::XORPD |
      Opcode::ADDPD |
      Opcode::ADDSD |
      Opcode::MULSD |
      Opcode::MULPD |
      Opcode::CVTPS2PD |
      Opcode::CVTPD2PS |
      Opcode::CVTSS2SD |
      Opcode::CVTSD2SS |
      Opcode::CVTPS2DQ |
      Opcode::CVTDQ2PS |
      Opcode::CVTTPS2DQ |
      Opcode::SUBSD |
      Opcode::SUBPD |
      Opcode::MINPD |
      Opcode::MINSD |
      Opcode::DIVPD |
      Opcode::DIVSD |
      Opcode::MAXPD |
      Opcode::MAXSD |
      Opcode::PUNPCKLQDQ |
      Opcode::PUNPCKHQDQ |
      Opcode::MOVDQA |
      Opcode::MOVDQU |
      Opcode::PSHUFHW |
      Opcode::PSHUFLW |
      Opcode::PSHUFD |
      Opcode::LFENCE |
      Opcode::MFENCE |
      Opcode::CLFLUSH |
      Opcode::CMPPD |
      Opcode::CMPPS |
      Opcode::CMPSD |
      Opcode::SHUFPD |
      Opcode::PADDQ |
      Opcode::MOVQ2DQ |
      Opcode::MOVDQ2Q |
      Opcode::CVTPD2DQ |
      Opcode::CVTTPD2DQ |
      Opcode::CVTDQ2PD |
      Opcode::PMULUDQ |
      Opcode::MASKMOVDQU |
      Opcode::PSUBQ => {
        if !decoder.feature_sse2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::GF2P8AFFINEQB |
      Opcode::GF2P8AFFINEINVQB |
      Opcode::GF2P8MULB => {
        if !decoder.feature_gfni() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::XRSTORS64 |
      Opcode::XSAVEC64 |
      Opcode::XSAVES64 => {
        if !decoder.feature_xsave64() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::GETSEC => {
        if !decoder.feature_smx() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::ANDN |
      Opcode::BEXTR |
      Opcode::BLSI |
      Opcode::BLSMSK |
      Opcode::BLSR |
      Opcode::TZCNT => {
        if !decoder.feature_bmi1() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::BZHI |
      Opcode::MULX |
      Opcode::PDEP |
      Opcode::PEXT |
      Opcode::RORX |
      Opcode::SARX |
      Opcode::SHRX |
      Opcode::SHLX => {
        if !decoder.feature_bmi2() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::MOVDIRI |
      Opcode::MOVDIR64B => {
        if !decoder.feature_movdir() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::VAESDEC |
      Opcode::VAESDECLAST |
      Opcode::VAESENC |
      Opcode::VAESENCLAST |
      Opcode::VAESIMC |
      Opcode::VAESKEYGENASSIST => {
        if !decoder.feature_vaes() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::RDRAND => {
        if !decoder.feature_rdrand() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
      Opcode::CLZERO => {
        if !decoder.feature_clzero() {
          return Err(DecodeError::InvalidOpcode);

        }
      }
    }
    Ok(())
  }
}

