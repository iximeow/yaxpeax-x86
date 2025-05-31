// x86 feature flags are the same for all instruction bitnesses (16, 32, 64-bit).
// in fact, the per-uarch predefined settings are also the same across bitnesses, though not *all*
// features are present on all bitnesses - at least one feature bit is hidden in non-64-bit
// execution modes.
//
// this is one big macro because the instruction and decoder types are not unified across
// bitnesses, so the whole thing is generic over those identifiers.
// as-is, `gen_arch_isa_settings!(the, module, types)` should do, though.

macro_rules! gen_isa_settings {
    (
        ($inst_ty:ty, $decode_err:ty, $featureful_decoder:ty, $unconditional_decoder:ty, $revise_inst_fn:ident),
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

            fn revise_instruction(&self, inst: &mut $inst_ty) -> Result<(), $decode_err> {
                $revise_inst_fn(self, inst)
            }
        }

        impl IsaSettings for $unconditional_decoder {
            $(fn $feature(&self) -> bool { true })+

            fn revise_instruction(&self, _inst: &mut $inst_ty) -> Result<(), $decode_err> {
                Ok(())
            }
        }

        impl IsaSettings for $featureful_decoder {
            $(
                fn $feature(&self) -> bool {
                    let i = $idx as usize;
                    self.flags[i / 64] & (1 << (i % 64)) != 0
                }
            )+
        }

        impl $featureful_decoder {
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

macro_rules! gen_arch_isa_settings {
    ($inst_ty:ty, $decode_err:ty, $featureful_decoder:ty, $unconditional_decoder:ty, $revise_inst_fn:ident) => {
        gen_isa_settings!(
            ($inst_ty, $decode_err, $featureful_decoder, $unconditional_decoder, $revise_inst_fn),
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

    }
}

pub(crate) use gen_arch_isa_settings;
