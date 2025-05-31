use super::{BMI1, BMI2, DecodeError, DecodeEverything, InstDecoder, Instruction, Opcode};

macro_rules! gen_isa_settings {
    (
        $(
            $(#[$doc:meta])*
            $feature:ident,
            $(#[$set_doc:item])*
            $set_feature:ident = $idx:expr;
        )+

        {
            $(
                $(#[$composite_doc:meta])*
                $composite_feature:ident = {
                    $first_inner_feature:ident
                    $(,$($inner_feature:ident$(,)?)+)?
                }$(,)?
            )*
        }

        {
            $(
                $(#[$composite_set_doc:meta])*
                $composite_set_feature:ident = {
                    $set_first_inner_feature:ident
                    $(,$($set_inner_feature:ident$(,)?)+)?
                }$(,)?
            )*
        }
    ) => {
        /// specific decode settings controlling how an x86 byte sequence is interpreted.
        ///
        /// this currently exists to specify which extensions are to be accepted or rejected. the two
        /// implementations provided by `yaxpeax-x86` are:
        /// * [`InstDecoder`], providing configurable enablement or disablement per-extension
        /// * [`DecodeEverything`], which allows all extensions supported by `yaxpeax-x86`
        ///
        /// notably, `InstDecoder::default()` and `DecodeEverything` are functionally equivalent in that
        /// they accept all extensions supported by the decoder.
        ///
        /// TODO: many additional extension support flags.
        /// * extended MMX (see `sha256:daee4e23dac983f1744126352d40cc71d47b4a9283a2a1e473837728ca9c51ac`)
        /// * lots of others... tile extensions...
        pub trait IsaSettings {
            $(
                $(#[$doc])*
                fn $feature(&self) -> bool;
            )+

            $(
                $(#[$composite_doc])*
                fn $composite_feature(&self) -> bool {
                    self.$first_inner_feature()
                        $($(&& self.$inner_feature())+)?
                }
            )*

            fn revise_instruction(&self, inst: &mut Instruction) -> Result<(), DecodeError> {
                revise_instruction(self, inst)
            }
        }

        impl IsaSettings for DecodeEverything {
            $(fn $feature(&self) -> bool { true })+

            fn revise_instruction(&self, _inst: &mut Instruction) -> Result<(), DecodeError> {
                Ok(())
            }
        }

        impl IsaSettings for InstDecoder {
            $(
                fn $feature(&self) -> bool {
                    let i = $idx as usize;
                    self.flags[i / 64] & (1 << (i % 64)) != 0
                }
            )+
        }

        impl InstDecoder {
            $(
                $(#[$set_doc])*
                pub fn $set_feature(mut self) -> Self {
                    let i = $idx as usize;
                    self.flags[i / 64] |= 1 << (i % 64);
                    self
                }
            )+

            $(
                $(#[$composite_set_doc])*
                pub fn $composite_set_feature(&self) -> Self {
                    self.$set_first_inner_feature()
                        $($(.$set_inner_feature())+)?
                }
            )*

            $(
                $(#[$doc])*
                pub fn $feature(&self) -> bool {
                    <Self as IsaSettings>::$feature(self)
                }
            )+

            $(
                $(#[$composite_doc])*
                pub fn $composite_feature(&self) -> bool {
                    <Self as IsaSettings>::$composite_feature(self)
                }
            )*
        }
    }
}
gen_isa_settings!(
    _3dnow, with_3dnow = 1;
    _3dnowprefetch, with_3dnowprefetch = 2;
    abm, with_abm = 3;
    adx, with_adx = 4;
    aesni, with_aesni = 5;
    amd_quirks, with_amd_quirks = 6;
    avx, with_avx = 7;
    avx2, with_avx2 = 8;
    avx512_4fmaps, with_avx512_4fmaps = 10;
    avx512_4vnniw, with_avx512_4vnniw = 11;
    avx512_bitalg, with_avx512_bitalg = 12;
    avx512_bw, with_avx512_bw = 13;
    avx512_cd, with_avx512_cd = 14;
    avx512_dq, with_avx512_dq = 15;
    avx512_er, with_avx512_er = 16;
    avx512_f, with_avx512_f = 17;
    avx512_fma, with_avx512_fma = 18;
    avx512_pf, with_avx512_pf = 19;
    avx512_vbmi, with_avx512_vbmi = 20;
    avx512_vbmi2, with_avx512_vbmi2 = 21;
    avx512_vl, with_avx512_vl = 22;
    avx512_vpopcntdq, with_avx512_vpopcntdq = 23;
    avx_vnni, with_avx_vnni = 24;
    bmi1, with_bmi1 = 25;
    #[doc="`bmi2` indicates support for the `BZHI`, `MULX`, `PDEP`, `PEXT`, `RORX`, `SARX`, `SHRX`, "]
    #[doc="and `SHLX` instructions. `bmi2` is implemented in all x86_64 chips that implement `bmi`, "]
    #[doc="except the amd `piledriver` and `steamroller` microarchitectures."]
    bmi2, with_bmi2 = 26;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0007_EBX_x0`: \"CLFLUSHOPT instruction support.\""]
    clflushopt, with_clflushopt = 27;
    clwb, with_clwb = 28;
    cmov, with_cmov = 29;
    cmpxchg16b, with_cmpxchg16b = 30;
    cx8, with_cx8 = 31;
    f16c, with_f16c = 32;
    fma3, with_fma3 = 33;
    fma4, with_fma4 = 34;
    gfni, with_gfni = 35;
    intel_quirks, with_intel_quirks = 36;
    invpcid, with_invpcid = 37;
    #[doc="`lahfsahf` is only unset for early revisions of 64-bit amd and intel chips. unfortunately"]
    #[doc="the clearest documentation on when these instructions were reintroduced into 64-bit"]
    #[doc="architectures seems to be"]
    #[doc="[wikipedia](https://en.wikipedia.org/wiki/X86-64#Older_implementations):"]
    #[doc="```text"]
    #[doc="Early AMD64 and Intel 64 CPUs lacked LAHF and SAHF instructions in 64-bit mode. AMD"]
    #[doc="introduced these instructions (also in 64-bit mode) with their Athlon 64, Opteron and"]
    #[doc="Turion 64 revision D processors in March 2005[48][49][50] while Intel introduced the"]
    #[doc="instructions with the Pentium 4 G1 stepping in December 2005. The 64-bit version of Windows"]
    #[doc="8.1 requires this feature.[47]"]
    #[doc="```"]
    #[doc=""]
    #[doc="this puts reintroduction of these instructions somewhere in the middle of prescott and k8"]
    #[doc="lifecycles, for intel and amd respectively. because there is no specific uarch where these"]
    #[doc="features become enabled, prescott and k8 default to not supporting these instructions,"]
    #[doc="where later uarches support these instructions."]
    lahfsahf, with_lahfsahf = 38;
    lzcnt, with_lzcnt = 39;
    monitor, with_monitor = 40;
    movbe, with_movbe = 41;
    mpx, with_mpx = 42;
    pclmulqdq, with_pclmulqdq = 43;
    pcommit, with_pcommit = 44;
    popcnt, with_popcnt = 45;
    prefetchw, with_prefetchw = 46;
    prefetchwt1, with_prefetchwt1 = 47;
    rdrand, with_rdrand = 48;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0007_EBX_x0`: \"RDSEED instruction support.\""]
    rdseed, with_rdseed = 49;
    rdtscp, with_rdtscp = 50;
    sgx, with_sgx = 51;
    sha, with_sha = 52;
    skinit, with_skinit = 53;
    sse3, with_sse3 = 54;
    sse4_1, with_sse4_1 = 55;
    sse4_2, with_sse4_2 = 56;
    sse4a, with_sse4a = 57;
    ssse3, with_ssse3 = 58;
    svm, with_svm = 59;
    syscall, with_syscall = 60;
    tbm, with_tbm = 61;
    tsx, with_tsx = 62;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0007_ECX_x0`: \"VAES 256-bit instruction support.\""]
    vaes, with_vaes = 63;
    vmx, with_vmx = 64;
    xop, with_xop = 65;
    xsave, with_xsave = 66;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_000D_EAX_x1`: \"XSAVEC and compact XRSTOR supported.\""]
    xsavec, with_xsavec = 67;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_000D_EAX_x1`: \"XSAVES, XRSTOR, and XSS are supported.\""]
    xsaves, with_xsaves = 68;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_000D_EAX_x1`: \"XSAVEOPT is available.\""]
    xsaveopt, with_xsaveopt = 69;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0007_EAX_x0`: \"FS and GS base read/write instruction support.\""]
    fsgsbase, with_fsgsbase = 70;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0001_ECX`: \"Support for MWAITX and MONITORX instructions.\""]
    monitorx, with_monitorx = 71;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0008_EBX`: \"WBNOINVD instruction supported.\""]
    wbnoinvd, with_wbnoinvd = 72;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0008_EBX`: \"CLZERO instruction supported.\""]
    clzero, with_clzero = 72;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0007_ECX_x0`: \"RDPID instruction and TSC_AUX MSR support.\""]
    rdpid, with_rdpid = 73;
    #[doc="from AMD APM Vol 3 `CPUID Fn0000_0007_ECX_x0`: \"VPCLMULQDQ 256-bit instruction support.\""]
    vpclmulqdq, with_vpclmulqdq = 74;
    #[doc="supported in Zen 5, but not mentioned in the AMD APM as of revision 3.36."]
    movdir64b, with_movdir64b = 75;
    #[doc="supported in Zen 5, but not mentioned in the AMD APM as of revision 3.36."]
    enqcmd, with_enqcmd = 76;

    {
        sse4 = {
            sse4_1,
            sse4_2,
        }

        #[doc = "returns `true` if this `InstDecoder` has **all** `avx512` features enabled."]
        avx512 = {
            avx512_4fmaps,
            avx512_4vnniw,
            avx512_bitalg,
            avx512_bw,
            avx512_cd,
            avx512_dq,
            avx512_er,
            avx512_f,
            avx512_fma,
            avx512_pf,
            avx512_vbmi,
            avx512_vbmi2,
            avx512_vl,
            avx512_vpopcntdq,
        },
    }

    {
        with_sse4 = {
            with_sse4_1,
            with_sse4_2,
        }

        with_avx512 = {
            with_avx512_4fmaps,
            with_avx512_4vnniw,
            with_avx512_bitalg,
            with_avx512_bw,
            with_avx512_cd,
            with_avx512_dq,
            with_avx512_er,
            with_avx512_f,
            with_avx512_fma,
            with_avx512_pf,
            with_avx512_vbmi,
            with_avx512_vbmi2,
            with_avx512_vl,
            with_avx512_vpopcntdq,
        }
    }
);

/// optionally reject or reinterpret instruction according to settings for this decode
/// operation.
fn revise_instruction<D: IsaSettings + ?Sized>(settings: &D, inst: &mut Instruction) -> Result<(), DecodeError> {
    if inst.prefixes.evex().is_some() {
        if !settings.avx512() {
            return Err(DecodeError::InvalidOpcode);
        } else {
            return Ok(());
        }
    }
    match inst.opcode {
        // original 3dnow instructions. see also
        // `3DNow-Technology-Manual.pdf`
        // * sha256: daee4e23dac983f1744126352d40cc71d47b4a9283a2a1e473837728ca9c51ac
        // * ref: https://www.amd.com/content/dam/amd/en/documents/archived-tech-docs/programmer-references/21928.pdf
        // * order# 21928
        Opcode::FEMMS |
        Opcode::PAVGUSB |
        Opcode::PFADD |
        Opcode::PFSUB |
        Opcode::PFSUBR |
        Opcode::PFACC |
        Opcode::PFCMPGE |
        Opcode::PFCMPGT |
        Opcode::PFCMPEQ |
        Opcode::PFMAX |
        Opcode::PFMIN |
        Opcode::PI2FD |
        Opcode::PF2ID |
        Opcode::PFRCP |
        Opcode::PFRSQRT |
        Opcode::PFMUL |
        Opcode::PFRCPIT1 |
        Opcode::PFRCPIT2 |
        Opcode::PFRSQIT1 |
        Opcode::PMULHRW => {
            if !settings._3dnow() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        // later extension to 3dnow. see also
        // `AMD-Extensions-to-the-3DNow-and-MMX-Instruction-Sets.pdf`
        // * sha256: ad847bd6877a682296fc584b4bbee354bf84c57bb97ba57e9c9adfc63cc5f465
        // * ref: https://refspecs.linuxfoundation.org/AMD-extensions.pdf
        // * order# 22466
        Opcode::PF2IW |
        Opcode::PFNACC |
        Opcode::PFPNACC |
        Opcode::PI2FW |
        Opcode::PSWAPD => {
            if !settings._3dnow() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::TZCNT => {
            if !settings.bmi1() {
                // tzcnt is only supported if bmi1 is enabled. without bmi1, this decodes as
                // bsf.
                inst.opcode = Opcode::BSF;
            }
        }
        Opcode::LDDQU |
        Opcode::ADDSUBPS |
        Opcode::ADDSUBPD |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::HADDPD |
        Opcode::HSUBPD |
        Opcode::MOVSHDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVDDUP |
        Opcode::MONITOR |
        Opcode::MWAIT => {
            // via Intel section 5.7, SSE3 Instructions
            if !settings.sse3() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::PHADDW |
        Opcode::PHADDSW |
        Opcode::PHADDD |
        Opcode::PHSUBW |
        Opcode::PHSUBSW |
        Opcode::PHSUBD |
        Opcode::PABSB |
        Opcode::PABSW |
        Opcode::PABSD |
        Opcode::PMADDUBSW |
        Opcode::PMULHRSW |
        Opcode::PSHUFB |
        Opcode::PSIGNB |
        Opcode::PSIGNW |
        Opcode::PSIGND |
        Opcode::PALIGNR => {
            // via Intel section 5.8, SSSE3 Instructions
            if !settings.ssse3() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::PMULLD |
        Opcode::PMULDQ |
        Opcode::MOVNTDQA |
        Opcode::BLENDPD |
        Opcode::BLENDPS |
        Opcode::BLENDVPD |
        Opcode::BLENDVPS |
        Opcode::PBLENDVB |
        Opcode::BLENDW |
        Opcode::PMINUW |
        Opcode::PMINUD |
        Opcode::PMINSB |
        Opcode::PMINSD |
        Opcode::PMAXUW |
        Opcode::PMAXUD |
        Opcode::PMAXSB |
        Opcode::PMAXSD |
        Opcode::ROUNDPS |
        Opcode::ROUNDPD |
        Opcode::ROUNDSS |
        Opcode::ROUNDSD |
        Opcode::PBLENDW |
        Opcode::EXTRACTPS |
        Opcode::INSERTPS |
        Opcode::PINSRB |
        Opcode::PINSRD |
        Opcode::PINSRQ |
        Opcode::PMOVSXBW |
        Opcode::PMOVZXBW |
        Opcode::PMOVSXBD |
        Opcode::PMOVZXBD |
        Opcode::PMOVSXWD |
        Opcode::PMOVZXWD |
        Opcode::PMOVSXBQ |
        Opcode::PMOVZXBQ |
        Opcode::PMOVSXWQ |
        Opcode::PMOVZXWQ |
        Opcode::PMOVSXDQ |
        Opcode::PMOVZXDQ |
        Opcode::DPPS |
        Opcode::DPPD |
        Opcode::MPSADBW |
        Opcode::PHMINPOSUW |
        Opcode::PTEST |
        Opcode::PCMPEQQ |
        Opcode::PEXTRB |
        Opcode::PEXTRW |
        Opcode::PEXTRD |
        Opcode::PEXTRQ |
        Opcode::PACKUSDW => {
            // via Intel section 5.10, SSE4.1 Instructions
            if !settings.sse4_1() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::EXTRQ |
        Opcode::INSERTQ |
        Opcode::MOVNTSS |
        Opcode::MOVNTSD => {
            if !settings.sse4a() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::CRC32 |
        Opcode::PCMPESTRI |
        Opcode::PCMPESTRM |
        Opcode::PCMPISTRI |
        Opcode::PCMPISTRM |
        Opcode::PCMPGTQ => {
            // via Intel section 5.11, SSE4.2 Instructions
            if !settings.sse4_2() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::AESDEC |
        Opcode::AESDECLAST |
        Opcode::AESENC |
        Opcode::AESENCLAST |
        Opcode::AESIMC |
        Opcode::AESKEYGENASSIST => {
            // via Intel section 5.12. AESNI AND PCLMULQDQ
            if !settings.aesni() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::PCLMULQDQ => {
            // via Intel section 5.12. AESNI AND PCLMULQDQ
            if !settings.pclmulqdq() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::XABORT |
        Opcode::XBEGIN |
        Opcode::XEND |
        Opcode::XTEST => {
            if !settings.tsx() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::SHA1MSG1 |
        Opcode::SHA1MSG2 |
        Opcode::SHA1NEXTE |
        Opcode::SHA1RNDS4 |
        Opcode::SHA256MSG1 |
        Opcode::SHA256MSG2 |
        Opcode::SHA256RNDS2 => {
            if !settings.sha() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::ENCLV |
        Opcode::ENCLS |
        Opcode::ENCLU => {
            if !settings.sgx() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        // AVX...
        Opcode::VMOVDDUP |
        Opcode::VPSHUFLW |
        Opcode::VPSHUFHW |
        Opcode::VHADDPS |
        Opcode::VHSUBPS |
        Opcode::VADDSUBPS |
        Opcode::VCVTPD2DQ |
        Opcode::VLDDQU |
        Opcode::VCOMISD |
        Opcode::VCOMISS |
        Opcode::VUCOMISD |
        Opcode::VUCOMISS |
        Opcode::VADDPD |
        Opcode::VADDPS |
        Opcode::VADDSD |
        Opcode::VADDSS |
        Opcode::VADDSUBPD |
        Opcode::VBLENDPD |
        Opcode::VBLENDPS |
        Opcode::VBLENDVPD |
        Opcode::VBLENDVPS |
        Opcode::VBROADCASTF128 |
        Opcode::VBROADCASTI128 |
        Opcode::VBROADCASTSD |
        Opcode::VBROADCASTSS |
        Opcode::VCMPSD |
        Opcode::VCMPSS |
        Opcode::VCMPPD |
        Opcode::VCMPPS |
        Opcode::VCVTDQ2PD |
        Opcode::VCVTDQ2PS |
        Opcode::VCVTPD2PS |
        Opcode::VCVTPS2DQ |
        Opcode::VCVTPS2PD |
        Opcode::VCVTSS2SD |
        Opcode::VCVTSI2SS |
        Opcode::VCVTSI2SD |
        Opcode::VCVTSD2SI |
        Opcode::VCVTSD2SS |
        Opcode::VCVTSS2SI |
        Opcode::VCVTTPD2DQ |
        Opcode::VCVTTPS2DQ |
        Opcode::VCVTTSS2SI |
        Opcode::VCVTTSD2SI |
        Opcode::VDIVPD |
        Opcode::VDIVPS |
        Opcode::VDIVSD |
        Opcode::VDIVSS |
        Opcode::VDPPD |
        Opcode::VDPPS |
        Opcode::VEXTRACTF128 |
        Opcode::VEXTRACTI128 |
        Opcode::VEXTRACTPS |
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
        Opcode::VGATHERDPD |
        Opcode::VGATHERDPS |
        Opcode::VGATHERQPD |
        Opcode::VGATHERQPS |
        Opcode::VHADDPD |
        Opcode::VHSUBPD |
        Opcode::VINSERTF128 |
        Opcode::VINSERTI128 |
        Opcode::VINSERTPS |
        Opcode::VMASKMOVDQU |
        Opcode::VMASKMOVPD |
        Opcode::VMASKMOVPS |
        Opcode::VMAXPD |
        Opcode::VMAXPS |
        Opcode::VMAXSD |
        Opcode::VMAXSS |
        Opcode::VMINPD |
        Opcode::VMINPS |
        Opcode::VMINSD |
        Opcode::VMINSS |
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
        Opcode::VMOVQ |
        Opcode::VMOVSS |
        Opcode::VMOVSD |
        Opcode::VMOVSHDUP |
        Opcode::VMOVSLDUP |
        Opcode::VMOVUPD |
        Opcode::VMOVUPS |
        Opcode::VMPSADBW |
        Opcode::VMULPD |
        Opcode::VMULPS |
        Opcode::VMULSD |
        Opcode::VMULSS |
        Opcode::VPABSB |
        Opcode::VPABSD |
        Opcode::VPABSW |
        Opcode::VPACKSSDW |
        Opcode::VPACKUSDW |
        Opcode::VPACKSSWB |
        Opcode::VPACKUSWB |
        Opcode::VPADDB |
        Opcode::VPADDD |
        Opcode::VPADDQ |
        Opcode::VPADDSB |
        Opcode::VPADDSW |
        Opcode::VPADDUSB |
        Opcode::VPADDUSW |
        Opcode::VPADDW |
        Opcode::VPALIGNR |
        Opcode::VPAND |
        Opcode::VANDPD |
        Opcode::VANDPS |
        Opcode::VANDNPD |
        Opcode::VANDNPS |
        Opcode::VORPD |
        Opcode::VORPS |
        Opcode::VPANDN |
        Opcode::VPAVGB |
        Opcode::VPAVGW |
        Opcode::VPBLENDD |
        Opcode::VPBLENDVB |
        Opcode::VPBLENDW |
        Opcode::VPBROADCASTB |
        Opcode::VPBROADCASTD |
        Opcode::VPBROADCASTQ |
        Opcode::VPBROADCASTW |
        Opcode::VPCLMULQDQ |
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
        Opcode::VPERM2F128 |
        Opcode::VPERM2I128 |
        Opcode::VPERMD |
        Opcode::VPERMILPD |
        Opcode::VPERMILPS |
        Opcode::VPERMPD |
        Opcode::VPERMPS |
        Opcode::VPERMQ |
        Opcode::VPEXTRB |
        Opcode::VPEXTRD |
        Opcode::VPEXTRQ |
        Opcode::VPEXTRW |
        Opcode::VPGATHERDD |
        Opcode::VPGATHERDQ |
        Opcode::VPGATHERQD |
        Opcode::VPGATHERQQ |
        Opcode::VPHADDD |
        Opcode::VPHADDSW |
        Opcode::VPHADDW |
        Opcode::VPMADDUBSW |
        Opcode::VPHMINPOSUW |
        Opcode::VPHSUBD |
        Opcode::VPHSUBSW |
        Opcode::VPHSUBW |
        Opcode::VPINSRB |
        Opcode::VPINSRD |
        Opcode::VPINSRQ |
        Opcode::VPINSRW |
        Opcode::VPMADDWD |
        Opcode::VPMASKMOVD |
        Opcode::VPMASKMOVQ |
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
        Opcode::VPMULDQ |
        Opcode::VPMULHRSW |
        Opcode::VPMULHUW |
        Opcode::VPMULHW |
        Opcode::VPMULLQ |
        Opcode::VPMULLD |
        Opcode::VPMULLW |
        Opcode::VPMULUDQ |
        Opcode::VPOR |
        Opcode::VPSADBW |
        Opcode::VPSHUFB |
        Opcode::VPSHUFD |
        Opcode::VPSIGNB |
        Opcode::VPSIGND |
        Opcode::VPSIGNW |
        Opcode::VPSLLD |
        Opcode::VPSLLDQ |
        Opcode::VPSLLQ |
        Opcode::VPSLLVD |
        Opcode::VPSLLVQ |
        Opcode::VPSLLW |
        Opcode::VPSRAD |
        Opcode::VPSRAVD |
        Opcode::VPSRAW |
        Opcode::VPSRLD |
        Opcode::VPSRLDQ |
        Opcode::VPSRLQ |
        Opcode::VPSRLVD |
        Opcode::VPSRLVQ |
        Opcode::VPSRLW |
        Opcode::VPSUBB |
        Opcode::VPSUBD |
        Opcode::VPSUBQ |
        Opcode::VPSUBSB |
        Opcode::VPSUBSW |
        Opcode::VPSUBUSB |
        Opcode::VPSUBUSW |
        Opcode::VPSUBW |
        Opcode::VPTEST |
        Opcode::VPUNPCKHBW |
        Opcode::VPUNPCKHDQ |
        Opcode::VPUNPCKHQDQ |
        Opcode::VPUNPCKHWD |
        Opcode::VPUNPCKLBW |
        Opcode::VPUNPCKLDQ |
        Opcode::VPUNPCKLQDQ |
        Opcode::VPUNPCKLWD |
        Opcode::VPXOR |
        Opcode::VRCPPS |
        Opcode::VROUNDPD |
        Opcode::VROUNDPS |
        Opcode::VROUNDSD |
        Opcode::VROUNDSS |
        Opcode::VRSQRTPS |
        Opcode::VRSQRTSS |
        Opcode::VRCPSS |
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
        Opcode::VTESTPD |
        Opcode::VTESTPS |
        Opcode::VUNPCKHPD |
        Opcode::VUNPCKHPS |
        Opcode::VUNPCKLPD |
        Opcode::VUNPCKLPS |
        Opcode::VXORPD |
        Opcode::VXORPS |
        Opcode::VZEROUPPER |
        Opcode::VZEROALL |
        Opcode::VLDMXCSR |
        Opcode::VSTMXCSR => {
            // TODO: check a table for these
            if !settings.avx() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::VAESDEC |
        Opcode::VAESDECLAST |
        Opcode::VAESENC |
        Opcode::VAESENCLAST |
        Opcode::VAESIMC |
        Opcode::VAESKEYGENASSIST => {
            // TODO: check a table for these
            if !settings.avx() || !settings.aesni() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::MOVBE => {
            if !settings.movbe() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::POPCNT => {
            /*
             * from the intel SDM:
             * ```
             * Before an application attempts to use the POPCNT instruction, it must check that
             * the processor supports SSE4.2 (if CPUID.01H:ECX.SSE4_2[bit 20] = 1) and POPCNT
             * (if CPUID.01H:ECX.POPCNT[bit 23] = 1).
             * ```
             */
            if settings.intel_quirks() && (settings.sse4_2() || settings.popcnt()) {
                return Ok(());
            } else if !settings.popcnt() {
                /*
                 * elsewhere from the amd APM:
                 * `Instruction Subsets and CPUID Feature Flags` on page 507 indicates that
                 * popcnt is present when the popcnt bit is reported by cpuid. this seems to be
                 * the less quirky default, so `intel_quirks` is considered the outlier, and
                 * before this default.
                 * */
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::LZCNT => {
            /*
             * amd APM, `LZCNT` page 212:
             * LZCNT is an Advanced Bit Manipulation (ABM) instruction. Support for the LZCNT
             * instruction is indicated by CPUID Fn8000_0001_ECX[ABM] = 1.
             *
             * meanwhile the intel SDM simply states:
             * ```
             * CPUID.EAX=80000001H:ECX.LZCNT[bit 5]: if 1 indicates the processor supports the
             * LZCNT instruction.
             * ```
             *
             * so that's considered the less-quirky (default) case here.
             * */
            if settings.amd_quirks() && !settings.abm() {
                return Err(DecodeError::InvalidOpcode);
            } else if !settings.lzcnt() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::ADCX |
        Opcode::ADOX => {
            if !settings.adx() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::VMRUN |
        Opcode::VMLOAD |
        Opcode::VMSAVE |
        Opcode::CLGI |
        Opcode::VMMCALL |
        Opcode::INVLPGA => {
            if !settings.svm() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::STGI |
        Opcode::SKINIT => {
            if !settings.svm() || !settings.skinit() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::LAHF |
        Opcode::SAHF => {
            if !settings.lahfsahf() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::VCVTPS2PH |
        Opcode::VCVTPH2PS => {
            /*
             * from intel SDM:
             * ```
             * 14.4.1 Detection of F16C Instructions Application using float 16 instruction
             *    must follow a detection sequence similar to AVX to ensure: • The OS has
             *    enabled YMM state management support, • The processor support AVX as
             *    indicated by the CPUID feature flag, i.e. CPUID.01H:ECX.AVX[bit 28] = 1.  •
             *    The processor support 16-bit floating-point conversion instructions via a
             *    CPUID feature flag (CPUID.01H:ECX.F16C[bit 29] = 1).
             * ```
             *
             * TODO: only the VEX-coded variant of this instruction should be gated on `f16c`.
             * the EVEX-coded variant should be gated on `avx512f` or `avx512vl` if not
             * EVEX.512-coded.
             */
            if !settings.avx() || !settings.f16c() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::RDRAND => {
            if !settings.rdrand() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::RDSEED => {
            if !settings.rdseed() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        Opcode::MONITORX | Opcode::MWAITX | // these are gated on the `monitorx` and `mwaitx` cpuid bits, but are AMD-only.
        Opcode::CLZERO | Opcode::RDPRU => { // again, gated on specific cpuid bits, but AMD-only.
            if !settings.amd_quirks() {
                return Err(DecodeError::InvalidOpcode);
            }
        }
        other => {
            if !settings.bmi1() {
                if BMI1.contains(&other) {
                    return Err(DecodeError::InvalidOpcode);
                }
            }
            if !settings.bmi2() {
                if BMI2.contains(&other) {
                    return Err(DecodeError::InvalidOpcode);
                }
            }
        }
    }
    Ok(())
}
