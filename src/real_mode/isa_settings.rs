use super::{BMI1, BMI2, DecodeError, DecodeEverything, InstDecoder, Instruction, Opcode};

crate::isa_settings::gen_arch_isa_settings!(
    Instruction, DecodeError, InstDecoder, DecodeEverything,
    revise_instruction
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
