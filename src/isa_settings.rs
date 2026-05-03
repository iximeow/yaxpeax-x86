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
        ($inst_ty:ty, $opcode:ty, $decode_err:ty, $featureful_decoder:ty),
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
        /// TODO: many additional extension support flags.
        /// * extended MMX (see `sha256:daee4e23dac983f1744126352d40cc71d47b4a9283a2a1e473837728ca9c51ac`)
        /// * lots of others... tile extensions...
        impl $featureful_decoder {
            $(
                $(#[$doc])*
                pub fn $feature(&self) -> bool {
                    let i = $idx as usize;
                    self.flags[i / 64] & (1 << (i % 64)) != 0
                }
            )+

            $(
                $(#[$composite_doc])*
                pub fn $composite_feature(&self) -> bool {
                    self.$first_inner_feature()
                        $($(&& self.$inner_feature())+)?
                }
            )*

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
        }

        /// optionally reject or reinterpret instruction according to settings for this decode
        /// operation.
        pub(crate) fn revise_instruction(settings: &$featureful_decoder, inst: &mut $inst_ty) -> Result<(), $decode_err> {
            if inst.prefixes.evex().is_some() {
                if !settings.avx512() {
                    return Err(<$decode_err>::InvalidOpcode);
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
                <$opcode>::FEMMS |
                <$opcode>::PAVGUSB |
                <$opcode>::PFADD |
                <$opcode>::PFSUB |
                <$opcode>::PFSUBR |
                <$opcode>::PFACC |
                <$opcode>::PFCMPGE |
                <$opcode>::PFCMPGT |
                <$opcode>::PFCMPEQ |
                <$opcode>::PFMAX |
                <$opcode>::PFMIN |
                <$opcode>::PI2FD |
                <$opcode>::PF2ID |
                <$opcode>::PFRCP |
                <$opcode>::PFRSQRT |
                <$opcode>::PFMUL |
                <$opcode>::PFRCPIT1 |
                <$opcode>::PFRCPIT2 |
                <$opcode>::PFRSQIT1 |
                <$opcode>::PMULHRW => {
                    if !settings._3dnow() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                // later extension to 3dnow. see also
                // `AMD-Extensions-to-the-3DNow-and-MMX-Instruction-Sets.pdf`
                // * sha256: ad847bd6877a682296fc584b4bbee354bf84c57bb97ba57e9c9adfc63cc5f465
                // * ref: https://refspecs.linuxfoundation.org/AMD-extensions.pdf
                // * order# 22466
                <$opcode>::PF2IW |
                <$opcode>::PFNACC |
                <$opcode>::PFPNACC |
                <$opcode>::PI2FW |
                <$opcode>::PSWAPD => {
                    if !settings._3dnow() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::TZCNT => {
                    if !settings.bmi1() {
                        // tzcnt is only supported if bmi1 is enabled. without bmi1, this decodes as
                        // bsf.
                        inst.opcode = <$opcode>::BSF;
                    }
                }
                <$opcode>::LDDQU |
                <$opcode>::ADDSUBPS |
                <$opcode>::ADDSUBPD |
                <$opcode>::HADDPS |
                <$opcode>::HSUBPS |
                <$opcode>::HADDPD |
                <$opcode>::HSUBPD |
                <$opcode>::MOVSHDUP |
                <$opcode>::MOVSLDUP |
                <$opcode>::MOVDDUP |
                <$opcode>::MONITOR |
                <$opcode>::MWAIT => {
                    // via Intel section 5.7, SSE3 Instructions
                    if !settings.sse3() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::PHADDW |
                <$opcode>::PHADDSW |
                <$opcode>::PHADDD |
                <$opcode>::PHSUBW |
                <$opcode>::PHSUBSW |
                <$opcode>::PHSUBD |
                <$opcode>::PABSB |
                <$opcode>::PABSW |
                <$opcode>::PABSD |
                <$opcode>::PMADDUBSW |
                <$opcode>::PMULHRSW |
                <$opcode>::PSHUFB |
                <$opcode>::PSIGNB |
                <$opcode>::PSIGNW |
                <$opcode>::PSIGND |
                <$opcode>::PALIGNR => {
                    // via Intel section 5.8, SSSE3 Instructions
                    if !settings.ssse3() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::PMULLD |
                <$opcode>::PMULDQ |
                <$opcode>::MOVNTDQA |
                <$opcode>::BLENDPD |
                <$opcode>::BLENDPS |
                <$opcode>::BLENDVPD |
                <$opcode>::BLENDVPS |
                <$opcode>::PBLENDVB |
                <$opcode>::BLENDW |
                <$opcode>::PMINUW |
                <$opcode>::PMINUD |
                <$opcode>::PMINSB |
                <$opcode>::PMINSD |
                <$opcode>::PMAXUW |
                <$opcode>::PMAXUD |
                <$opcode>::PMAXSB |
                <$opcode>::PMAXSD |
                <$opcode>::ROUNDPS |
                <$opcode>::ROUNDPD |
                <$opcode>::ROUNDSS |
                <$opcode>::ROUNDSD |
                <$opcode>::PBLENDW |
                <$opcode>::EXTRACTPS |
                <$opcode>::INSERTPS |
                <$opcode>::PINSRB |
                <$opcode>::PINSRD |
                <$opcode>::PINSRQ |
                <$opcode>::PMOVSXBW |
                <$opcode>::PMOVZXBW |
                <$opcode>::PMOVSXBD |
                <$opcode>::PMOVZXBD |
                <$opcode>::PMOVSXWD |
                <$opcode>::PMOVZXWD |
                <$opcode>::PMOVSXBQ |
                <$opcode>::PMOVZXBQ |
                <$opcode>::PMOVSXWQ |
                <$opcode>::PMOVZXWQ |
                <$opcode>::PMOVSXDQ |
                <$opcode>::PMOVZXDQ |
                <$opcode>::DPPS |
                <$opcode>::DPPD |
                <$opcode>::MPSADBW |
                <$opcode>::PHMINPOSUW |
                <$opcode>::PTEST |
                <$opcode>::PCMPEQQ |
                <$opcode>::PEXTRB |
                <$opcode>::PEXTRW |
                <$opcode>::PEXTRD |
                <$opcode>::PEXTRQ |
                <$opcode>::PACKUSDW => {
                    // via Intel section 5.10, SSE4.1 Instructions
                    if !settings.sse4_1() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::EXTRQ |
                <$opcode>::INSERTQ |
                <$opcode>::MOVNTSS |
                <$opcode>::MOVNTSD => {
                    if !settings.sse4a() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::CRC32 |
                <$opcode>::PCMPESTRI |
                <$opcode>::PCMPESTRM |
                <$opcode>::PCMPISTRI |
                <$opcode>::PCMPISTRM |
                <$opcode>::PCMPGTQ => {
                    // via Intel section 5.11, SSE4.2 Instructions
                    if !settings.sse4_2() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::AESDEC |
                <$opcode>::AESDECLAST |
                <$opcode>::AESENC |
                <$opcode>::AESENCLAST |
                <$opcode>::AESIMC |
                <$opcode>::AESKEYGENASSIST => {
                    // via Intel section 5.12. AESNI AND PCLMULQDQ
                    if !settings.aesni() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::PCLMULQDQ => {
                    // via Intel section 5.12. AESNI AND PCLMULQDQ
                    if !settings.pclmulqdq() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::XABORT |
                <$opcode>::XBEGIN |
                <$opcode>::XEND |
                <$opcode>::XTEST => {
                    if !settings.tsx() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::SHA1MSG1 |
                <$opcode>::SHA1MSG2 |
                <$opcode>::SHA1NEXTE |
                <$opcode>::SHA1RNDS4 |
                <$opcode>::SHA256MSG1 |
                <$opcode>::SHA256MSG2 |
                <$opcode>::SHA256RNDS2 => {
                    if !settings.sha() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::ENCLV |
                <$opcode>::ENCLS |
                <$opcode>::ENCLU => {
                    if !settings.sgx() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                // AVX...
                <$opcode>::VMOVDDUP |
                <$opcode>::VPSHUFLW |
                <$opcode>::VPSHUFHW |
                <$opcode>::VHADDPS |
                <$opcode>::VHSUBPS |
                <$opcode>::VADDSUBPS |
                <$opcode>::VCVTPD2DQ |
                <$opcode>::VLDDQU |
                <$opcode>::VCOMISD |
                <$opcode>::VCOMISS |
                <$opcode>::VUCOMISD |
                <$opcode>::VUCOMISS |
                <$opcode>::VADDPD |
                <$opcode>::VADDPS |
                <$opcode>::VADDSD |
                <$opcode>::VADDSS |
                <$opcode>::VADDSUBPD |
                <$opcode>::VBLENDPD |
                <$opcode>::VBLENDPS |
                <$opcode>::VBLENDVPD |
                <$opcode>::VBLENDVPS |
                <$opcode>::VBROADCASTF128 |
                <$opcode>::VBROADCASTI128 |
                <$opcode>::VBROADCASTSD |
                <$opcode>::VBROADCASTSS |
                <$opcode>::VCMPSD |
                <$opcode>::VCMPSS |
                <$opcode>::VCMPPD |
                <$opcode>::VCMPPS |
                <$opcode>::VCVTDQ2PD |
                <$opcode>::VCVTDQ2PS |
                <$opcode>::VCVTPD2PS |
                <$opcode>::VCVTPS2DQ |
                <$opcode>::VCVTPS2PD |
                <$opcode>::VCVTSS2SD |
                <$opcode>::VCVTSI2SS |
                <$opcode>::VCVTSI2SD |
                <$opcode>::VCVTSD2SI |
                <$opcode>::VCVTSD2SS |
                <$opcode>::VCVTSS2SI |
                <$opcode>::VCVTTPD2DQ |
                <$opcode>::VCVTTPS2DQ |
                <$opcode>::VCVTTSS2SI |
                <$opcode>::VCVTTSD2SI |
                <$opcode>::VDIVPD |
                <$opcode>::VDIVPS |
                <$opcode>::VDIVSD |
                <$opcode>::VDIVSS |
                <$opcode>::VDPPD |
                <$opcode>::VDPPS |
                <$opcode>::VEXTRACTF128 |
                <$opcode>::VEXTRACTI128 |
                <$opcode>::VEXTRACTPS |
                <$opcode>::VFMADD132PD |
                <$opcode>::VFMADD132PS |
                <$opcode>::VFMADD132SD |
                <$opcode>::VFMADD132SS |
                <$opcode>::VFMADD213PD |
                <$opcode>::VFMADD213PS |
                <$opcode>::VFMADD213SD |
                <$opcode>::VFMADD213SS |
                <$opcode>::VFMADD231PD |
                <$opcode>::VFMADD231PS |
                <$opcode>::VFMADD231SD |
                <$opcode>::VFMADD231SS |
                <$opcode>::VFMADDSUB132PD |
                <$opcode>::VFMADDSUB132PS |
                <$opcode>::VFMADDSUB213PD |
                <$opcode>::VFMADDSUB213PS |
                <$opcode>::VFMADDSUB231PD |
                <$opcode>::VFMADDSUB231PS |
                <$opcode>::VFMSUB132PD |
                <$opcode>::VFMSUB132PS |
                <$opcode>::VFMSUB132SD |
                <$opcode>::VFMSUB132SS |
                <$opcode>::VFMSUB213PD |
                <$opcode>::VFMSUB213PS |
                <$opcode>::VFMSUB213SD |
                <$opcode>::VFMSUB213SS |
                <$opcode>::VFMSUB231PD |
                <$opcode>::VFMSUB231PS |
                <$opcode>::VFMSUB231SD |
                <$opcode>::VFMSUB231SS |
                <$opcode>::VFMSUBADD132PD |
                <$opcode>::VFMSUBADD132PS |
                <$opcode>::VFMSUBADD213PD |
                <$opcode>::VFMSUBADD213PS |
                <$opcode>::VFMSUBADD231PD |
                <$opcode>::VFMSUBADD231PS |
                <$opcode>::VFNMADD132PD |
                <$opcode>::VFNMADD132PS |
                <$opcode>::VFNMADD132SD |
                <$opcode>::VFNMADD132SS |
                <$opcode>::VFNMADD213PD |
                <$opcode>::VFNMADD213PS |
                <$opcode>::VFNMADD213SD |
                <$opcode>::VFNMADD213SS |
                <$opcode>::VFNMADD231PD |
                <$opcode>::VFNMADD231PS |
                <$opcode>::VFNMADD231SD |
                <$opcode>::VFNMADD231SS |
                <$opcode>::VFNMSUB132PD |
                <$opcode>::VFNMSUB132PS |
                <$opcode>::VFNMSUB132SD |
                <$opcode>::VFNMSUB132SS |
                <$opcode>::VFNMSUB213PD |
                <$opcode>::VFNMSUB213PS |
                <$opcode>::VFNMSUB213SD |
                <$opcode>::VFNMSUB213SS |
                <$opcode>::VFNMSUB231PD |
                <$opcode>::VFNMSUB231PS |
                <$opcode>::VFNMSUB231SD |
                <$opcode>::VFNMSUB231SS |
                <$opcode>::VGATHERDPD |
                <$opcode>::VGATHERDPS |
                <$opcode>::VGATHERQPD |
                <$opcode>::VGATHERQPS |
                <$opcode>::VHADDPD |
                <$opcode>::VHSUBPD |
                <$opcode>::VINSERTF128 |
                <$opcode>::VINSERTI128 |
                <$opcode>::VINSERTPS |
                <$opcode>::VMASKMOVDQU |
                <$opcode>::VMASKMOVPD |
                <$opcode>::VMASKMOVPS |
                <$opcode>::VMAXPD |
                <$opcode>::VMAXPS |
                <$opcode>::VMAXSD |
                <$opcode>::VMAXSS |
                <$opcode>::VMINPD |
                <$opcode>::VMINPS |
                <$opcode>::VMINSD |
                <$opcode>::VMINSS |
                <$opcode>::VMOVAPD |
                <$opcode>::VMOVAPS |
                <$opcode>::VMOVD |
                <$opcode>::VMOVDQA |
                <$opcode>::VMOVDQU |
                <$opcode>::VMOVHLPS |
                <$opcode>::VMOVHPD |
                <$opcode>::VMOVHPS |
                <$opcode>::VMOVLHPS |
                <$opcode>::VMOVLPD |
                <$opcode>::VMOVLPS |
                <$opcode>::VMOVMSKPD |
                <$opcode>::VMOVMSKPS |
                <$opcode>::VMOVNTDQ |
                <$opcode>::VMOVNTDQA |
                <$opcode>::VMOVNTPD |
                <$opcode>::VMOVNTPS |
                <$opcode>::VMOVQ |
                <$opcode>::VMOVSS |
                <$opcode>::VMOVSD |
                <$opcode>::VMOVSHDUP |
                <$opcode>::VMOVSLDUP |
                <$opcode>::VMOVUPD |
                <$opcode>::VMOVUPS |
                <$opcode>::VMPSADBW |
                <$opcode>::VMULPD |
                <$opcode>::VMULPS |
                <$opcode>::VMULSD |
                <$opcode>::VMULSS |
                <$opcode>::VPABSB |
                <$opcode>::VPABSD |
                <$opcode>::VPABSW |
                <$opcode>::VPACKSSDW |
                <$opcode>::VPACKUSDW |
                <$opcode>::VPACKSSWB |
                <$opcode>::VPACKUSWB |
                <$opcode>::VPADDB |
                <$opcode>::VPADDD |
                <$opcode>::VPADDQ |
                <$opcode>::VPADDSB |
                <$opcode>::VPADDSW |
                <$opcode>::VPADDUSB |
                <$opcode>::VPADDUSW |
                <$opcode>::VPADDW |
                <$opcode>::VPALIGNR |
                <$opcode>::VPAND |
                <$opcode>::VANDPD |
                <$opcode>::VANDPS |
                <$opcode>::VANDNPD |
                <$opcode>::VANDNPS |
                <$opcode>::VORPD |
                <$opcode>::VORPS |
                <$opcode>::VPANDN |
                <$opcode>::VPAVGB |
                <$opcode>::VPAVGW |
                <$opcode>::VPBLENDD |
                <$opcode>::VPBLENDVB |
                <$opcode>::VPBLENDW |
                <$opcode>::VPBROADCASTB |
                <$opcode>::VPBROADCASTD |
                <$opcode>::VPBROADCASTQ |
                <$opcode>::VPBROADCASTW |
                <$opcode>::VPCLMULQDQ |
                <$opcode>::VPCMPEQB |
                <$opcode>::VPCMPEQD |
                <$opcode>::VPCMPEQQ |
                <$opcode>::VPCMPEQW |
                <$opcode>::VPCMPGTB |
                <$opcode>::VPCMPGTD |
                <$opcode>::VPCMPGTQ |
                <$opcode>::VPCMPGTW |
                <$opcode>::VPCMPESTRI |
                <$opcode>::VPCMPESTRM |
                <$opcode>::VPCMPISTRI |
                <$opcode>::VPCMPISTRM |
                <$opcode>::VPERM2F128 |
                <$opcode>::VPERM2I128 |
                <$opcode>::VPERMD |
                <$opcode>::VPERMILPD |
                <$opcode>::VPERMILPS |
                <$opcode>::VPERMPD |
                <$opcode>::VPERMPS |
                <$opcode>::VPERMQ |
                <$opcode>::VPEXTRB |
                <$opcode>::VPEXTRD |
                <$opcode>::VPEXTRQ |
                <$opcode>::VPEXTRW |
                <$opcode>::VPGATHERDD |
                <$opcode>::VPGATHERDQ |
                <$opcode>::VPGATHERQD |
                <$opcode>::VPGATHERQQ |
                <$opcode>::VPHADDD |
                <$opcode>::VPHADDSW |
                <$opcode>::VPHADDW |
                <$opcode>::VPMADDUBSW |
                <$opcode>::VPHMINPOSUW |
                <$opcode>::VPHSUBD |
                <$opcode>::VPHSUBSW |
                <$opcode>::VPHSUBW |
                <$opcode>::VPINSRB |
                <$opcode>::VPINSRD |
                <$opcode>::VPINSRQ |
                <$opcode>::VPINSRW |
                <$opcode>::VPMADDWD |
                <$opcode>::VPMASKMOVD |
                <$opcode>::VPMASKMOVQ |
                <$opcode>::VPMAXSB |
                <$opcode>::VPMAXSD |
                <$opcode>::VPMAXSW |
                <$opcode>::VPMAXUB |
                <$opcode>::VPMAXUW |
                <$opcode>::VPMAXUD |
                <$opcode>::VPMINSB |
                <$opcode>::VPMINSW |
                <$opcode>::VPMINSD |
                <$opcode>::VPMINUB |
                <$opcode>::VPMINUW |
                <$opcode>::VPMINUD |
                <$opcode>::VPMOVMSKB |
                <$opcode>::VPMOVSXBD |
                <$opcode>::VPMOVSXBQ |
                <$opcode>::VPMOVSXBW |
                <$opcode>::VPMOVSXDQ |
                <$opcode>::VPMOVSXWD |
                <$opcode>::VPMOVSXWQ |
                <$opcode>::VPMOVZXBD |
                <$opcode>::VPMOVZXBQ |
                <$opcode>::VPMOVZXBW |
                <$opcode>::VPMOVZXDQ |
                <$opcode>::VPMOVZXWD |
                <$opcode>::VPMOVZXWQ |
                <$opcode>::VPMULDQ |
                <$opcode>::VPMULHRSW |
                <$opcode>::VPMULHUW |
                <$opcode>::VPMULHW |
                <$opcode>::VPMULLQ |
                <$opcode>::VPMULLD |
                <$opcode>::VPMULLW |
                <$opcode>::VPMULUDQ |
                <$opcode>::VPOR |
                <$opcode>::VPSADBW |
                <$opcode>::VPSHUFB |
                <$opcode>::VPSHUFD |
                <$opcode>::VPSIGNB |
                <$opcode>::VPSIGND |
                <$opcode>::VPSIGNW |
                <$opcode>::VPSLLD |
                <$opcode>::VPSLLDQ |
                <$opcode>::VPSLLQ |
                <$opcode>::VPSLLVD |
                <$opcode>::VPSLLVQ |
                <$opcode>::VPSLLW |
                <$opcode>::VPSRAD |
                <$opcode>::VPSRAVD |
                <$opcode>::VPSRAW |
                <$opcode>::VPSRLD |
                <$opcode>::VPSRLDQ |
                <$opcode>::VPSRLQ |
                <$opcode>::VPSRLVD |
                <$opcode>::VPSRLVQ |
                <$opcode>::VPSRLW |
                <$opcode>::VPSUBB |
                <$opcode>::VPSUBD |
                <$opcode>::VPSUBQ |
                <$opcode>::VPSUBSB |
                <$opcode>::VPSUBSW |
                <$opcode>::VPSUBUSB |
                <$opcode>::VPSUBUSW |
                <$opcode>::VPSUBW |
                <$opcode>::VPTEST |
                <$opcode>::VPUNPCKHBW |
                <$opcode>::VPUNPCKHDQ |
                <$opcode>::VPUNPCKHQDQ |
                <$opcode>::VPUNPCKHWD |
                <$opcode>::VPUNPCKLBW |
                <$opcode>::VPUNPCKLDQ |
                <$opcode>::VPUNPCKLQDQ |
                <$opcode>::VPUNPCKLWD |
                <$opcode>::VPXOR |
                <$opcode>::VRCPPS |
                <$opcode>::VROUNDPD |
                <$opcode>::VROUNDPS |
                <$opcode>::VROUNDSD |
                <$opcode>::VROUNDSS |
                <$opcode>::VRSQRTPS |
                <$opcode>::VRSQRTSS |
                <$opcode>::VRCPSS |
                <$opcode>::VSHUFPD |
                <$opcode>::VSHUFPS |
                <$opcode>::VSQRTPD |
                <$opcode>::VSQRTPS |
                <$opcode>::VSQRTSS |
                <$opcode>::VSQRTSD |
                <$opcode>::VSUBPD |
                <$opcode>::VSUBPS |
                <$opcode>::VSUBSD |
                <$opcode>::VSUBSS |
                <$opcode>::VTESTPD |
                <$opcode>::VTESTPS |
                <$opcode>::VUNPCKHPD |
                <$opcode>::VUNPCKHPS |
                <$opcode>::VUNPCKLPD |
                <$opcode>::VUNPCKLPS |
                <$opcode>::VXORPD |
                <$opcode>::VXORPS |
                <$opcode>::VZEROUPPER |
                <$opcode>::VZEROALL |
                <$opcode>::VLDMXCSR |
                <$opcode>::VSTMXCSR => {
                    // TODO: check a table for these
                    if !settings.avx() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::VAESDEC |
                <$opcode>::VAESDECLAST |
                <$opcode>::VAESENC |
                <$opcode>::VAESENCLAST |
                <$opcode>::VAESIMC |
                <$opcode>::VAESKEYGENASSIST => {
                    // TODO: check a table for these
                    if !settings.avx() || !settings.aesni() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::MOVBE => {
                    if !settings.movbe() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::POPCNT => {
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
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::LZCNT => {
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
                        return Err(<$decode_err>::InvalidOpcode);
                    } else if !settings.lzcnt() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::ADCX |
                <$opcode>::ADOX => {
                    if !settings.adx() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::VMRUN |
                <$opcode>::VMLOAD |
                <$opcode>::VMSAVE |
                <$opcode>::CLGI |
                <$opcode>::VMMCALL |
                <$opcode>::INVLPGA => {
                    if !settings.svm() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::STGI |
                <$opcode>::SKINIT => {
                    if !settings.svm() || !settings.skinit() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::LAHF |
                <$opcode>::SAHF => {
                    if !settings.lahfsahf() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::VCVTPS2PH |
                <$opcode>::VCVTPH2PS => {
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
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::RDRAND => {
                    if !settings.rdrand() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::RDSEED => {
                    if !settings.rdseed() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::MONITORX | <$opcode>::MWAITX | // these are gated on the `monitorx` and `mwaitx` cpuid bits, but are AMD-only.
                <$opcode>::CLZERO | <$opcode>::RDPRU => { // again, gated on specific cpuid bits, but AMD-only.
                    if !settings.amd_quirks() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }
                <$opcode>::INVEPT |
                <$opcode>::INVVPID => {
                    if !settings.vmx() {
                        return Err(<$decode_err>::InvalidOpcode);
                    }
                }

                other => {
                    if !settings.bmi1() {
                        if BMI1.contains(&other) {
                            return Err(<$decode_err>::InvalidOpcode);
                        }
                    }
                    if !settings.bmi2() {
                        if BMI2.contains(&other) {
                            return Err(<$decode_err>::InvalidOpcode);
                        }
                    }
                }
            }
            Ok(())
        }
    }
}

macro_rules! gen_arch_isa_settings {
    ($inst_ty:ty, $opcode:ty, $decode_err:ty, $featureful_decoder:ty) => {
        gen_isa_settings!(
            ($inst_ty, $opcode, $decode_err, $featureful_decoder),
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
            #[doc="`lahfsahf` is only unset for early revisions of 64-bit amd and Intel chips. unfortunately"]
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
            #[doc="this puts reintroduction of these instructions somewhere in the middle of Prescott and K8"]
            #[doc="lifecycles, for Intel and AMD respectively. because there is no specific uarch where these"]
            #[doc="features become enabled, Prescott and K8 default to not supporting these instructions,"]
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

            amx, with_amx = 77;
            amx_bf16, with_amx_bf16 = 78;
            amx_tile, with_amx_tile = 79;
            amx_int8, with_amx_int8 = 80;
            amx_fp16, with_amx_fp16 = 81;
            amx_ifma, with_amx_ifma = 82;
            amx_ne_convert, with_amx_ne_convert = 83;
            amx_vnni_int8, with_amx_vnni_int8 = 84;
            amx_complex, with_amx_complex = 84;
            amx_vnni_int16, with_amx_vnni_int16 = 84;
            amx_movrs, with_avx_movrs = 85;
            amx_fp8, with_amx_fp8 = 86;
            amx_tf32, with_amx_tf32 = 87;

            sm3, with_sm3 = 90;
            sm4, with_sm4 = 91;
            sm4_evex, with_sm4_evex = 92;

            apx, with_apx = 99;
            // no avx10 top-level bit because avx10.1 is a rollup of many avx512 extensions to
            // date. avx10.2 and later will have bits for their additions plus similar top-level
            // avx10_2, avx10_3, ... as appropriate.
            fred, with_fred = 101;
            urdmsr, with_uwrmsr = 101;
            // avx10_2
            /// immediate encodings for `rdmsr` and `wrmsrns`
            immediate_rdmsr, with_immediate_rdmsr = 102;
            /// `movrs` and the `prefetchrst2`
            movrs, with_movrs = 103;

            avx512_fp16, with_avx512_fp16 = 104;
            avx512_vaes, with_avx512_vaes = 105;
            avx512_gfni, with_avx512_gfni = 106;
            avx512_vpclmulqdq, with_avx512_vpclmulqdq = 107;
            avx512_bf16, with_avx512_bf16 = 108;
            avx512_vnni, with_avx512_vnni = 109;
            avx512_ifma, with_avx512_ifma = 110;

            {
                sse4 = {
                    sse4_1,
                    sse4_2,
                }

                /// returns `true` if this `InstDecoder` has **all** `avx512` features enabled.
                /// this does not correspond to any particular processor architecture that has ever
                /// been shipped.
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

                /// returns `true` if this `InstDecoder` has all extensions since defined as part
                /// of AVX10.1. this is consistent with AVX10.1 as defined in Granite Rapids and
                /// later.
                avx10_1 = {
                    avx512_fp16,
                    avx512_vpopcntdq,
                    avx512_vbmi2,
                    avx512_vaes,
                    avx512_gfni,
                    avx512_vpclmulqdq,
                    avx512_bitalg,
                    avx512_bf16,
                    avx512_vnni,
                    avx512_vbmi,
                    avx512_ifma,
                    avx512_f,
                    avx512_cd,
                    avx512_bw,
                    avx512_dq,
                }
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

                /// controls support for decoding all extensions since defined as part of AVX10.1.
                /// this is consistent with AVX10.1 as defined in Granite Rapids and later.
                with_avx10_1 = {
                    with_avx512_fp16,
                    with_avx512_vpopcntdq,
                    with_avx512_vbmi2,
                    with_avx512_vaes,
                    with_avx512_gfni,
                    with_avx512_vpclmulqdq,
                    with_avx512_bitalg,
                    with_avx512_bf16,
                    with_avx512_vnni,
                    with_avx512_vbmi,
                    with_avx512_ifma,
                    with_avx512_f,
                    with_avx512_cd,
                    with_avx512_bw,
                    with_avx512_dq,
                }
            }
        );

    }
}

pub(crate) use gen_arch_isa_settings;
