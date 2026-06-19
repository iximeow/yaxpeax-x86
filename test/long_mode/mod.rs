extern crate rand;

mod regspec;
mod operand;
mod opcode;
#[cfg(feature="fmt")]
mod display;
#[cfg(all(feature="std", feature="fmt"))]
mod descriptions;
mod evex_generated;
mod reuse_test;
#[cfg(feature="behavior")]
mod behavior;

use std::fmt::Write;

use yaxpeax_arch::{AddressBase, Decoder, LengthedInstruction};
use yaxpeax_x86::long_mode::InstDecoder;
use yaxpeax_x86::long_mode::DisplayStyle;

use crate::tools::{self, CodeModel};

/*
#[cfg(feature="std")]
fn test_write_hex_specialization() {
    use crate::yaxpeax_x86::long_mode::DisplaySink;
    for i in 0..0xffu8 {
        let mut out = yaxpeax_x86::long_mode::InstructionFormatter::new();
        out.write_char('0').unwrap();
        out.write_char('x').unwrap();
        out.write_u8(i).unwrap();
        assert_eq!(out.into_inner(), format!("0x{:x}", i));
    }
}
*/

fn test_invalid(data: &[u8]) {
    test_invalid_under(&InstDecoder::default(), data);
}

fn test_invalid_under(decoder: &InstDecoder, data: &[u8]) {
    let mut reader = yaxpeax_arch::U8Reader::new(data);
    if let Ok(inst) = decoder.decode(&mut reader)  {
        // realistically, the chances an error only shows up under non-fmt builds seems unlikely,
        // but try to report *something* in such cases.
        cfg_if::cfg_if! {
            if #[cfg(feature="fmt")] {
                panic!("decoded {:?} from {:02x?} under decoder {}", inst.opcode(), data, decoder);
            } else {
                // don't warn about the unused inst here
                let _ = inst;
                panic!("decoded instruction <non-fmt build> from {:02x?} under decoder <non-fmt build>", data);
            }
        }
    } else {
        // this is fine
    }
}

fn test_display_under(decoder: &InstDecoder, data: &[u8], expected: &'static str) {
    test_display_format(decoder, data, expected, DisplayStyle::Intel);
}

fn test_display_format(decoder: &InstDecoder, data: &[u8], expected: &'static str, style: DisplayStyle) {
    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }
    let mut reader = yaxpeax_arch::U8Reader::new(data);
    match decoder.decode(&mut reader) {
        Ok(instr) => {
            cfg_if::cfg_if! {
                if #[cfg(feature="fmt")] {
                    match style {
                        DisplayStyle::Intel => {
                            let text = format!("{}", instr.display_with(DisplayStyle::Intel));
                            assert!(
                                text == expected,
                                "display error for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                                hex,
                                instr,
                                decoder,
                                text,
                                expected
                            );

                            let mut text2 = String::new();
                            let mut out = yaxpeax_arch::display::FmtSink::new(&mut text2);
                            instr.write_to(&mut out).expect("printing succeeds");

                            assert!(
                                text2 == text,
                                "display error through FmtSink for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                                hex,
                                instr,
                                decoder,
                                text2,
                                text,
                            );

                            #[cfg(feature="alloc")]
                            let mut formatter = yaxpeax_x86::long_mode::InstructionTextBuffer::new();
                            #[cfg(feature="alloc")]
                            let text3 = formatter.format_inst(&instr.display_with(DisplayStyle::Intel)).expect("printing succeeds");

                            #[cfg(feature="alloc")]
                            assert!(
                                text3 == text,
                                "display error through InstructionTextBuffer for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                                hex,
                                instr,
                                decoder,
                                text3,
                                text,
                            );

                            let mut text4 = String::new();
                            instr.write_to(&mut text4).expect("printing succeeds");

                            assert!(
                                text4 == text,
                                "display error through String for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                                hex,
                                instr,
                                decoder,
                                text4,
                                text,
                            );
                        }
                        DisplayStyle::Masm => {
                            let text = format!("{}", instr.display_with(DisplayStyle::Masm));
                            assert!(
                                text == expected,
                                "display error for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                                hex,
                                instr,
                                decoder,
                                text,
                                expected
                            );

                            #[cfg(feature="alloc")]
                            let mut formatter = yaxpeax_x86::long_mode::InstructionTextBuffer::new();
                            #[cfg(feature="alloc")]
                            let text3 = formatter.format_inst(&instr.display_with(DisplayStyle::Masm)).expect("printing succeeds");

                            #[cfg(feature="alloc")]
                            assert!(
                                text3 == text,
                                "display error through InstructionTextBuffer for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                                hex,
                                instr,
                                decoder,
                                text3,
                                text,
                            );

                            // no `instr.display_with(DisplayStyle::Masm)` tests involving write_to
                            // since write_to unconditionally uses DisplayStyle::Intel
                        }
                        DisplayStyle::C => {
                            panic!("no support for C-style display in testcases yet");
                        }
                        other => {
                            panic!("unsupported style: {:?}", other);
                        }
                    }
                } else {
                    eprintln!("non-fmt build cannot compare text equality")
                }
            }
            // while we're at it, test that the instruction is as long, and no longer, than its
            // input
            assert_eq!((0u64.wrapping_offset(instr.len()).to_linear()) as usize, data.len(), "instruction length is incorrect, wanted instruction {}", expected);
        },
        Err(e) => {
            cfg_if::cfg_if! {
                if #[cfg(feature="fmt")] {
                    assert!(false, "decode error ({}) for {} under decoder {}:\n  expected: {}\n", e, hex, decoder, expected);
                } else {
                    // avoid the unused `e` warning
                    let _ = e;
                    assert!(false, "decode error (<non-fmt build>) for {} under decoder <non-fmt build>:\n  expected: {}\n", hex, expected);
                }
            }
        }
    }
}

#[allow(non_camel_case_types)]
enum FeatureSet {
    Default,
    Minimal,
    Amd,
    Intel,
    DecoderK8,
    DecoderBulldozer,
    DecoderNetburst,
    // SSE and SSE2 are part of baseline x86_64, always enabled
    SSE,
    SSE2,
    SSE3,
    SSSE3,
    SSE4_1,
    SSE4_2,
    SSE4_2_Intel,
    SSE4a,
    BMI1,
    BMI2,
    AVX,
    AESNI,
    F16C,
    AVX_AESNI,
    AVX_F16C,
    AVX2,
    VMX,
    Popcnt,
}

impl FeatureSet {
    fn into_decoder(&self) -> InstDecoder {
        match self {
            FeatureSet::Default => {
                yaxpeax_x86::long_mode::InstDecoder::default()
            }
            FeatureSet::Minimal => {
                yaxpeax_x86::long_mode::InstDecoder::minimal()
            }
            FeatureSet::Intel => {
                yaxpeax_x86::long_mode::InstDecoder::minimal().with_intel_quirks()
            }
            FeatureSet::Amd => {
                yaxpeax_x86::long_mode::InstDecoder::minimal().with_amd_quirks()
            }
            FeatureSet::DecoderK8 => {
                yaxpeax_x86::long_mode::uarch::amd::k8()
            }
            FeatureSet::DecoderBulldozer => {
                yaxpeax_x86::long_mode::uarch::amd::bulldozer()
            }
            FeatureSet::DecoderNetburst => {
                yaxpeax_x86::long_mode::uarch::intel::netburst()
            }
            // SSE and SSE2 are part of baseline x86_64, always enabled
            FeatureSet::SSE => {
                InstDecoder::minimal()
            }
            FeatureSet::SSE2 => {
                InstDecoder::minimal()
            }
            FeatureSet::SSE3 => {
                InstDecoder::minimal().with_sse3()
            }
            FeatureSet::SSSE3 => {
                InstDecoder::minimal().with_ssse3()
            }
            FeatureSet::SSE4_1 => {
                InstDecoder::minimal().with_sse4_1()
            }
            FeatureSet::SSE4_2 => {
                InstDecoder::minimal().with_sse4_2()
            }
            FeatureSet::SSE4_2_Intel => {
                InstDecoder::minimal().with_intel_quirks().with_sse4_2()
            }
            FeatureSet::SSE4a => {
                InstDecoder::minimal().with_sse4a()
            }
            FeatureSet::BMI1 => {
                InstDecoder::minimal().with_bmi1()
            }
            FeatureSet::BMI2 => {
                InstDecoder::minimal().with_bmi2()
            }
            FeatureSet::AVX => {
                InstDecoder::minimal().with_avx()
            }
            FeatureSet::AESNI => {
                InstDecoder::minimal().with_aesni()
            }
            FeatureSet::F16C => {
                InstDecoder::minimal().with_f16c()
            }
            FeatureSet::AVX_AESNI => {
                InstDecoder::minimal().with_avx().with_aesni()
            }
            FeatureSet::AVX_F16C => {
                InstDecoder::minimal().with_avx().with_f16c()
            }
            FeatureSet::AVX2 => {
                InstDecoder::minimal().with_avx().with_avx2()
            }
            FeatureSet::VMX => {
                InstDecoder::minimal().with_vmx()
            }
            FeatureSet::Popcnt => {
                InstDecoder::minimal().with_popcnt()
            }
        }
    }
}

struct Disasm {
    display: &'static str,
    c: Option<&'static str>,
    masm: Option<&'static str>,
}

struct TestCase {
    bytes: &'static [u8],
    featuresets: Option<&'static [(FeatureSet, bool)]>,
    #[cfg(feature="fmt")]
    decodes: Option<Disasm>,
}

fn check_decodes(decoder: &InstDecoder, decode_ok: bool, bytes: &[u8], disasm: &Disasm) {
    if decode_ok {
        test_display_under(&decoder, bytes, disasm.display);
        if let Some(c_style) = disasm.c.as_ref() {
            test_display_format(&decoder, bytes, c_style, DisplayStyle::C);
        }

        // if EXTERNAL_MASM is set we actually want to validate decodes against masm/dumpbin. this
        // is a bit convoluted. otherwise we're testing against in-tree "gold output". in the
        // EXTERNAL_MASM case we actually distrust this too, and validate the in-tree expected
        // output is what masm actually wants.
        if std::env::var_os("EXTERNAL_MASM").is_some() {
            eprintln!("==== running testcase: bytes={:x?}, expected_display={}", bytes, disasm.display);
            // OK: EXTERNAL_MASM is set, we'll expect that there's `../tools/` which has `wibo`,
            // `mlexe`, and `dumpbin.exe`.

            // match against some testcases that are known to be wrong by MASM/dumpbin.
            let external_masm_ish = match bytes {
                &[0xf1] => "int 1".to_string(),  // dumpbin does not know how to decode f1...
                &[0x4f, 0xe5, 0x99] => "in eax, 99h".to_string(), // this is a MASM/dumpbin bug. see notes on testcase.
                &[0x4f, 0xe7, 0x99] => "out 99h, eax".to_string(), // this is a MASM/dumpbin bug. see notes on testcase.
                // dumpbin prints the instruction as if it was encoded in 32-bit form regardless of object file, so overrule it.
                &[0xf3, 0x0f, 0xc7, 0xfd] => "rdpid rbp".to_string(),
                &[0x0f, 0x18, 0xc0] => "nop eax".to_string(), // dumpbin would love to call this "prefetchnta rax" ???
                &[0x0f, 0x18, 0xcc] => "nop esp".to_string(), // dumpbin would love to call this "prefetchnta rsp" ???
                &[0x0f, 0x18, 0x20] => "nop zmmword ptr [rax]".to_string(), // getting around dumpbin knowing about prefetchrst2..
                &[0x4f, 0x0f, 0x18, 0x20] => "nop zmmword ptr [r8]".to_string(), // getting around dumpbin knowing about prefetchrst2..
                &[0x2e, 0x36, 0x47, 0x0f, 0x18, 0xe7] => "nop r15d".to_string(), // getting around dumpbin knowing about prefetchrst2..
                &[0x0f, 0x19, 0x20] => "nop dword ptr [rax]".to_string(), // dumpbin doesn't know about 0f19..
                &[0x0f, 0x1a, 0x20] => "nop dword ptr [rax]".to_string(), // dumpbin wants to call this bndldx, yax doesn't do MPX yet
                &[0x0f, 0x1b, 0x20] => "nop dword ptr [rax]".to_string(), // dumpbin wants to call this bndstx, yax doesn't do MPX yet
                &[0x0f, 0x1c, 0x20] => "nop dword ptr [rax]".to_string(), // dumpbin doesn't know about 0f1c..
                &[0x0f, 0x1d, 0x20] => "nop dword ptr [rax]".to_string(), // dumpbin doesn't know about 0f1d..
                &[0x0f, 0x1e, 0x20] => "nop dword ptr [rax]".to_string(), // dumpbin doesn't know about 0f1e..
                &[0xf2, 0x66, 0x66, 0x4d, 0x0f, 0x10, 0xc0] => "movsd xmm8, xmm8".to_string(), // dumpbin does not love the prefixes
                &[0x4f, 0x66, 0x0f, 0x28, 0x00] => "movapd xmm0, xmmword ptr [rax]".to_string(), // dumpbin does not love the prefixes
                &[0x67, 0x4f, 0x66, 0x0f, 0x28, 0x00] => "movapd xmm0, xmmword ptr [eax]".to_string(), // dumpbin does not love the prefixes
                &[0xf3, 0x0f, 0x1e, 0xfc] => "nop".to_string(), // dumpbin does not tolerate this at all, redirect into a boring nop.
                &[0x4d, 0x0f, 0x43, 0xec] => "cmovnb r13, r12".to_string(), // dumpbin writes it "cmovae" instead of yax's cmovnb.
                &[0x65, 0x4c, 0x89, 0x04, 0x25, 0xa8, 0x01, 0x00, 0x00] => {
                    "mov qword ptr gs:[000001A8h], r8".to_string() // dumpbin uses %016 formatting, masm happily accepts shorter.
                },
                &[0x0f, 0xbe, 0x83, 0xb4, 0x00, 0x00, 0x00] => {
                    "movsx eax, byte ptr [rbx + 0B4h]".to_string() // dumpbin uses %016 formatting, masm happily accepts shorter.
                },
                &[0x46, 0x63, 0xc1] => "movsxd r8, ecx".to_string(), // dumpbin writes 32-bit destinations for this, but masm accepts either?
                &[0x62, 0xd2, 0x7e, 0x28, 0x3a, 0xca] => {
                    "vpbroadcastmw2d ymm1, k2".to_string() // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                },
                &[0x62, 0xd2, 0x7e, 0x08, 0x28, 0xc2] => {
                    "vpmovm2b xmm0, k2".to_string() // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                },
                &[0x0f, 0x01, 0x51, 0xff] => {
                    "lgdt fword ptr [rcx - 1]".to_string() // dumpbin prints this as "tbyte", which masm does not accept.
                },
                &[0x0f, 0x01, 0x59, 0xff] => {
                    "lidt fword ptr [rcx - 1]".to_string() // dumpbin prints this as "tbyte", which masm does not accept.
                },
                &[0x2e, 0x67, 0x65, 0x2e, 0x46, 0x0f, 0x01, 0xff] => {
                    "tlbsync".to_string() // dumpbin does not exactly tolerate the extra prefixes.
                },
                &[0x0f, 0x0d, 0x00] => {
                    // dumpbin interprets this as the 3DNow!-style PREFETCH instruction, but we're definitely not 3dnow..
                    "nop zmmword ptr [rax]".to_string()
                }
                &[0xf2, 0x41, 0x0f, 0xbc, 0xd3] => {
                    // masm doesn't like the extra prefix
                    "bsf edx, r11d".to_string()
                }
                &[0x4f, 0x4e, 0x00, 0xcc] => {
                    // masm doesn't like the extra prefix
                    "add spl, r9b".to_string()
                }
                &[0xc4, 0x03, 0x3d, 0x0a, 0xca, 0x77] => {
                    // dumpbin can't deal with this instruction..
                    "vroundss xmm9, xmm8, xmm10, 77h".to_string()
                }
                &[0xc4, 0x03, 0x3d, 0x0b, 0xca, 0x77] => {
                    // dumpbin can't deal with this instruction..
                    "vroundsd xmm9, xmm8, xmm10, 77h".to_string()
                }
                &[0x66, 0x4f, 0x0f, 0x6e, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc] => {
                    // dumpbin really wants to use mmword here, but i really don't.
                    "movq xmm11, qword ptr [r12 + r11 * 4 - 334455CCh]".to_string()
                }
                &[0x66, 0x0f, 0xd6, 0x01] => {
                    // dumpbin really wants to use mmword here, but i really don't.
                    "movq qword ptr [rcx], xmm0".to_string()
                }
                &[0x66, 0x4f, 0x0f, 0xd7, 0xc1] => {
                    // yax bug? default operand size is 64-bit in 64-bit mode, so the register should be r8?
                    "pmovmskb r8d, xmm9".to_string()
                }
                // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                &[0x66, 0x0f, 0xff, 0xc1] => "ud0 eax, ecx".to_string(),
                &[0xf2, 0x0f, 0xff, 0xc1] => "ud0 eax, ecx".to_string(),
                &[0xf3, 0x0f, 0xff, 0xc1] => "ud0 eax, ecx".to_string(),
                &[0x66, 0x0f, 0xff, 0x01] => "ud0 eax, dword ptr [rcx]".to_string(),
                &[0x66, 0x4f, 0x0f, 0xff, 0xc1] => "ud0 r8d, r9d".to_string(),
                &[0x4c, 0x0f, 0xff, 0x6b, 0xac] => "ud0 r13d, dword ptr [rbx - 54h]".to_string(),
                // dumpbin does not tolerate the pointless rex prefix.
                &[0x4f, 0x66, 0x0f, 0x2a, 0xcf] => "cvtpi2pd xmm1, mm7".to_string(),
                // dumpbin does not tolerate the pointless rex prefix.
                &[0x4f, 0xf3, 0x0f, 0x2a, 0xcf] => "cvtsi2ss xmm1, edi".to_string(),
                // dumpbin does not tolerate the pointless rex prefix.
                &[0x4f, 0xf2, 0x0f, 0x2a, 0xcf] => "cvtsi2sd xmm1, edi".to_string(),
                // dumpbin does not tolerate the pointless rex prefix.
                &[0x4f, 0xf2, 0x0f, 0x2a, 0x00] => "cvtsi2sd xmm0, dword ptr [rax]".to_string(),
                // dumpbin does not tolerate the pointless rex prefix.
                &[0x4f, 0xf3, 0x0f, 0x2a, 0x00] => "cvtsi2ss xmm0, dword ptr [rax]".to_string(),
                // dumpbin does not tolerate the pointless rex prefix.
                &[0x4f, 0x66, 0x0f, 0x2a, 0x00] => "cvtpi2pd xmm0, mmword ptr [rax]".to_string(),
                // dumpbin does not tolerate the pointless prefixes.
                &[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e] => "movdiri dword ptr [rbp + 3Eh], edx".to_string(),
                // dumpbin does not tolerate the pointless prefixes.
                &[0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07] => "movdir64b rbp, zmmword ptr [rbp + 729080Bh]".to_string(),
                // dumpbin does not tolerate the pointless prefixes.
                &[0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07] => "movdir64b ebp, zmmword ptr [ebp + 729080Bh]".to_string(),
                // dumpbin is super confused about the prefixing.
                &[0xf2, 0xf2, 0x2e, 0x36, 0x47, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f] => "enqcmd r8, zmmword ptr [r11 + 3F9D1C09h]".to_string(),
                // and again.
                &[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54] => "enqcmds rsi, zmmword ptr fs:[rdx + 54h]".to_string(),
                // dumpbin shows a ds prefix; this is tolerated by masm but is kinda incorrect in x86_64. either way masm accepts it though.
                &[0x3e, 0x4f, 0x0f, 0x38, 0xf6, 0x23] => "wrssq qword ptr [r11], r12".to_string(),
                // prefixes confuse dumpbin again
                &[0x66, 0xf3, 0x0f, 0x01, 0xe8] => "setssbsy".to_string(),
                // prefixes confuse dumpbin again
                &[0x66, 0xf3, 0x0f, 0x01, 0xea] => "saveprevssp".to_string(),
                // prefixes confuse dumpbin again
                &[0xf3, 0x66, 0x0f, 0x01, 0xe8] => "setssbsy".to_string(), // TODO: yax does not support `serialize` (yet)
                // prefixes confuse dumpbin again
                &[0xf3, 0x66, 0x0f, 0x01, 0xea] => "saveprevssp".to_string(),
                // prefixes confuse dumpbin again
                &[0xf3, 0x66, 0x0f, 0x01, 0x29] => "rstorssp qword ptr [rcx]".to_string(),
                // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                &[0xf2, 0x0f, 0xc0, 0xcc] => "xadd ah, cl".to_string(),
                // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                &[0xf3, 0x0f, 0xc0, 0xcc] => "xadd ah, cl".to_string(),
                // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                &[0xf2, 0x0f, 0xc1, 0xcc] => "xadd esp, ecx".to_string(),
                // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                &[0xf3, 0x0f, 0xc1, 0xcc] => "xadd esp, ecx".to_string(),
                // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                &[0xf2, 0x0f, 0xc7, 0x0f] => "cmpxchg8b qword ptr [rdi]".to_string(),
                // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                &[0xf3, 0x0f, 0xc7, 0x0f] => "cmpxchg8b qword ptr [rdi]".to_string(),
                // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                &[0x4f, 0x0f, 0xc7, 0x0f] => "cmpxchg16b xmmword ptr [r15]".to_string(),
                // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                &[0x66, 0x4f, 0x0f, 0xc7, 0x0f] => "cmpxchg16b xmmword ptr [r15]".to_string(),
                // dumpbin prints out repne prefix, which does not round-trip.
                &[0xf2, 0x4f, 0x0f, 0xc7, 0x0f] => "cmpxchg16b xmmword ptr [r15]".to_string(),
                // dumpbin prints out rep prefix, which does not round-trip.
                &[0xf3, 0x4f, 0x0f, 0xc7, 0x0f] => "cmpxchg16b xmmword ptr [r15]".to_string(),
                // prefixes again..
                &[0x3e, 0x64, 0x64, 0x66, 0x4e, 0x0f, 0x3a, 0xcf, 0xba, 0x13, 0x23, 0x04, 0xba, 0x6b] => "gf2p8affineinvqb xmm15, xmmword ptr fs:[rdx - 45FBDCEDh], 6Bh".to_string(),
                &[0x66, 0x36, 0x0f, 0x3a, 0xce, 0x8c, 0x56, 0x9e, 0x82, 0xd1, 0xbe, 0xad] => "gf2p8affineqb xmm1, xmmword ptr [rsi + rdx * 2 - 412E7D62h], 0ADh".to_string(),
                &[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8] => "loadiwkey xmm5, xmm0".to_string(),
                // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                &[0x66, 0x0f, 0x38, 0x80, 0x01] => "invept rax, xmmword ptr [rcx]".to_string(),
                // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                &[0x66, 0x0f, 0x38, 0x81, 0x01] => "invvpid rax, xmmword ptr [rcx]".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                // (and we print jnb instead of jae)
                &[0x73, 0x31] => "jnb $+33h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x72, 0x5a] => "jb $+5Ch".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x72, 0xf0] => "jb $-0Eh".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe8, 0x01, 0x00, 0x00, 0x00] => "call $+6".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe8, 0x80, 0x00, 0x00, 0x00] => "call near ptr $+85h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe8, 0xff, 0xff, 0xff, 0xff] => "call $+4".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe9, 0x01, 0x00, 0x00, 0x00] => "jmp $+6".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative. there's also the near ptr nonsense..
                &[0xe9, 0x80, 0x00, 0x00, 0x00] => "jmp near ptr $+85h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe9, 0xff, 0xff, 0xff, 0xff] => "jmp $+4".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x0f, 0x86, 0x8b, 0x01, 0x00, 0x00] => "jna $+191h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x0f, 0x85, 0x3b, 0x25, 0x00, 0x00] => "jnz $+2541h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x74, 0x47] => "jz $+49h".to_string(),
                // dumpbin invents a label for laughs.
                &[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00] => "call qword ptr [$ + 24727Eh]".to_string(),
                // dumpbin uses a really wide displacement .. for laughs..
                &[0xff, 0x24, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => "jmp qword ptr [rcx * 8 + 1BCA070h]".to_string(),
                // dumpbin uses a really wide displacement .. for laughs..
                &[0xff, 0x14, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => "call qword ptr [rcx * 8 + 1BCA070h]".to_string(),
                // dumpbin bug: 66-prefixed jmp/call does not pick 16-bit registers
                &[0x66, 0xff, 0xe0] => "jmp rax".to_string(),
                // dumpbin bug: 66-prefixed jmp/call does not pick 16-bit registers
                &[0x66, 0xff, 0xd0] => "call rax".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe0, 0x12] => "loopnz $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe1, 0x12] => "loopz $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe2, 0x12] => "loop $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe3, 0x12] => "jrcxz $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe3, 0xf0] => "jrcxz $-0Eh".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x67, 0xe3, 0x12] => "jecxz $+15h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x67, 0xe3, 0xf0] => "jecxz $-0Dh".to_string(),
                // dumpbin dislikes prefixes.
                &[0x66, 0xf2, 0x0f, 0x79, 0xcf] => "insertq xmm1, xmm7".to_string(),
                // rip-rel: oh dear
                &[0xf6, 0x05, 0x2c, 0x9b, 0xff, 0xff, 0x01] => "test byte ptr [$ - 64D4h], 1".to_string(),
                // yax uses wider immediates
                &[0x3d, 0x01, 0xf0, 0xff, 0xff] => "cmp eax, 0FFFFFFFFFFFFF001h".to_string(),
                // dumpbin doesn't print the $ of rip-rel :(
                &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => "xor eax, dword ptr [$ + 12345678h]".to_string(),
                &[0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => "xor eax, dword ptr [rcx + 43650123h]".to_string(),
                &[0x48, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => "xor rax, qword ptr [$ + 12345678h]".to_string(),
                &[0x48, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => "xor rax, qword ptr [rcx + 43650123h]".to_string(),
                &[0x44, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => "xor r8d, dword ptr [$ + 12345678h]".to_string(),
                &[0x44, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => "xor r8d, dword ptr [rcx + 43650123h]".to_string(),
                &[0x45, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => "xor r8d, dword ptr [$ + 12345678h]".to_string(),
                &[0x45, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => "xor r8d, dword ptr [r9 + 43650123h]".to_string(),
                &[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [44332211h]".to_string(),
                &[0x41, 0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [44332211h]".to_string(),
                &[0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [rbp + 44332211h]".to_string(),
                &[0x41, 0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [r13 + 44332211h]".to_string(),
                &[0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [44332211h]".to_string(),
                &[0x41, 0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [44332211h]".to_string(),
                &[0x42, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50] => "xor esi, dword ptr [r12 + 50403020h]".to_string(),
                &[0x43, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50] => "xor esi, dword ptr [r12 + 50403020h]".to_string(),
                &[0x42, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50] => "xor esi, dword ptr [rbp + r12 + 50403020h]".to_string(),
                &[0x43, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50] => "xor esi, dword ptr [r13 + r12 + 50403020h]".to_string(),
                // dumpbin gets the size wrong
                &[0x62, 0xf2, 0xfd, 0x0f, 0x8a, 0x62, 0xf2] => "vcompresspd xmmword ptr [rdx - 70h]{k7}, xmm4".to_string(),
                // TODO: yax doesn't know about rdssp{d,q}?
                &[0xf3, 0x0f, 0x1e, 0x0f] => "nop".to_string(),
                // yax won't mention the pointless repne prefix
                &[0xf2, 0x0f, 0x06] => "clts".to_string(),
                // yax won't mention the pointless repne prefix
                &[0xf2, 0x0f, 0x07] => "sysret".to_string(),
                // dumpbin spells this mmword
                &[0x0f, 0x6f, 0x00] => "movq mm0, qword ptr [rax]".to_string(),
                &[0x66, 0x2e, 0xf2, 0xf0, 0x0f, 0xbb, 0x13] => "xacquire lock btc word ptr [rbx], dx".to_string(),
                // dumpbin handles this right (like this!) but the output is weird to parse
                &[0x45, 0x66, 0x0f, 0x21, 0xc8] => "mov rax, dr1".to_string(),
                // dumpbin says repne, but that doesn't round-trip.
                &[0x45, 0xf2, 0x0f, 0x21, 0xc8] => "mov rax, dr1".to_string(),
                // dumpbin says rep, but that doesn't round-trip.
                &[0x45, 0xf3, 0x0f, 0x21, 0xc8] => "mov rax, dr1".to_string(),
                // dumpbin prints with more.. flourish
                &[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00] => "nop word ptr [rax + rax]".to_string(),
                // disp is wider from dumpbin
                &[0x48, 0x8d, 0xa4, 0xc7, 0x20, 0x00, 0x00, 0x12] => "lea rsp, [rdi + rax * 8 + 12000020h]".to_string(),
                &[0x0f, 0xfc, 0xaf, 0x40, 0x38, 0x25, 0xbf] => "paddb mm5, mmword ptr [rdi - 40DAC7C0h]".to_string(),
                &[0xc7, 0xf8, 0x10, 0x12, 0x34, 0x56] => "xbegin $+56341216h".to_string(),
                &[0x66, 0xc7, 0xf8, 0x10, 0x12] => "xbegin $+1215h".to_string(),
                &[0xf2, 0xf3, 0x66, 0x65, 0x4f, 0x25, 0x9b, 0x5e, 0xda, 0x44] => "and rax, 44DA5E9Bh".to_string(),
                &[0x65, 0x66, 0x66, 0x64, 0x48, 0x0f, 0x38, 0xdb, 0x0f] => "aesimc xmm1, xmmword ptr fs:[rdi]".to_string(),
                &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c] => "pi2fw mm6, qword ptr [rax - 5]".to_string(), // more prefix confusion..
                // prefixes confuse dumpbin, and dumpbin says "qword" where we use mmword. masm accepts either
                &[0x3e, 0xf3, 0x2e, 0xf2, 0x0f, 0x0f, 0x64, 0x93, 0x93, 0xa4] => "pfmax mm4, mmword ptr [rbx + rdx * 4 - 6Dh]".to_string(),
                // dumpbin calls this movq?
                &[0x4f, 0x0f, 0x7e, 0xcf] => "movd r15, mm1".to_string(),
                // dumpbin shows this as a wide register but it doesn't *really* matter and yax uses 32-bit always.
                &[0x4f, 0x0f, 0xd7, 0xcf] => "pmovmskb r9d, mm7".to_string(),
                // dumpbin shows this as a non-rip-rel offset :(
                &[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77] => "pmulhw mm7, qword ptr [$ + 77CCBBAAh]".to_string(),
                // dumpbin confused about prefixes once again
                &[0x66, 0x3e, 0x26, 0x2e, 0x2e, 0x0f, 0x38, 0x2a, 0x2b] => "movntdqa xmm5, xmmword ptr [rbx]".to_string(),
                // prefixes.. cs: isn't real in 64-bit mode
                &[0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c, 0xf0, 0x63] => "blendpd xmm7, xmmword ptr [eax - 0F83D010h], 63h".to_string(),
                // prefixes confuse dumpbin
                &[0x66, 0x66, 0x64, 0x3e, 0x0f, 0x38, 0x23, 0x9d, 0x69, 0x0f, 0xa8, 0x2d] => "pmovsxwd xmm3, qword ptr fs:[rbp + 2DA80F69h]".to_string(),
                // prefixes confuse dumpbin
                &[0x2e, 0x66, 0x26, 0x64, 0x49, 0x0f, 0x3a, 0x21, 0x0b, 0xb1] => "insertps xmm1, dword ptr fs:[r11], 0FFFFFFFFFFFFFFB1h".to_string(),
                // prefixes confuse dumpbin
                &[0x66, 0x26, 0x45, 0x0f, 0x3a, 0x42, 0x96, 0x74, 0x29, 0x96, 0xf9, 0x6a] => "mpsadbw xmm10, xmmword ptr [r14 - 669D68Ch], 6Ah".to_string(),
                // prefixes confuse dumpbin
                &[0x67, 0x26, 0x66, 0x65, 0x0f, 0x38, 0x3f, 0x9d, 0xcc, 0x03, 0xb3, 0xfa] => "pmaxud xmm3, xmmword ptr gs:[ebp - 54CFC34h]".to_string(),
                // prefixes confuse dumpbin
                &[0x67, 0x66, 0x65, 0x3e, 0x0f, 0x6d, 0xd1] => "punpckhqdq xmm2, xmm1".to_string(),
                // prefixes confuse dumpbin
                &[0x2e, 0x66, 0x40, 0x0f, 0x3a, 0x0d, 0x40, 0x2d, 0x57] => "blendpd xmm0, xmmword ptr [rax + 2Dh], 57h".to_string(),
                // prefixes confuse dumpbin
                &[0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f, 0xcd, 0xd7] => "lddqu xmm4, xmmword ptr [eax - 2832A0E5h]".to_string(),
                // prefixes confuse dumpbin
                &[0x2e, 0x3e, 0x66, 0x3e, 0x49, 0x0f, 0x3a, 0x41, 0x30, 0x48] => "dppd xmm6, xmmword ptr [r8], 48h".to_string(),
                // dumpbin prints the order backwards =|
                &[0x65, 0xf0, 0x87, 0x0f] => "lock xchg dword ptr gs:[rdi], ecx".to_string(),
                // displacement gets a bunch of extra zeroes
                &[0x66, 0x4e, 0x0f, 0x3a, 0x44, 0x88, 0xb3, 0xad, 0x26, 0x35, 0x75] => "pclmulqdq xmm9, xmmword ptr [rax + 3526ADB3h], 75h".to_string(),
                // dumpbin knows about "fstpnce" as "fstp1", but masm does not.
                // since this is an undocumented instruction anyway, decode it ourselves..
                &[0xd9, 0xdb] => "fstpnce st(3), st(0)".to_string(),
                // dumpbin calls this "fcom2", but it's just an undocumented fcom alias. this round-trips to a different instruction but it's at least.. kinda right.
                &[0xdc, 0xd3] => "fcom st(3)".to_string(),
                // dumpbin calls this "fcomp3", but it's just an undocumented fcomp alias. this round-trips to a different instruction but it's at least.. kinda right.
                &[0xdc, 0xdb] => "fcomp st(3)".to_string(),
                // dumpbin calls this "fxch4", but it's just an undocumented fxch alias. this round-trips to a different instruction but it's at least.. kinda right.
                &[0xdd, 0xcb] => "fxch st(3)".to_string(),
                // dumpbin calls this "fcomp5", but it's just an undocumented fcomp alias. this round-trips to a different instruction but it's at least.. kinda right.
                &[0xde, 0xd3] => "fcomp st(3)".to_string(),
                // dumpbin calls this "fxch7", but it's just an undocumented fxch alias. this round-trips to a different instruction but it's at least.. kinda right.
                &[0xdf, 0xcb] => "fxch st(3)".to_string(),
                // dumpbin calls this "fstp8", but it's just an undocumented fstp alias. this round-trips to a different instruction but it's at least.. kinda right.
                &[0xdf, 0xd3] => "fstp st(3)".to_string(),
                // dumpbin calls this "fstp9", but it's just an undocumented fstp alias. this round-trips to a different instruction but it's at least.. kinda right.
                &[0xdf, 0xdb] => "fstp st(3)".to_string(),
                // dunno why dumpbin doesn't like this one..
                &[0xc5, 0b0_1111_100, 0x2e, 0b00_001_010] => "vucomiss xmm9, dword ptr [rdx]".to_string(),
                &[0xc5, 0b0_1111_100, 0x2f, 0b00_001_010] => "vcomiss xmm9, dword ptr [rdx]".to_string(),
                other => {
                    tools::dumpbin(other, CodeModel::Bits64).unwrap_or_else(|e| {
                        panic!("{}: {e:?}", format!("could not get an instruction after dumpbining {other:x?}"));
                    })
                }
            };
            let displayed_masm = decoder.decode_slice(bytes).expect("can decode").display_with(DisplayStyle::Masm).to_string();
            let masm_as_bytes = match displayed_masm.as_str() {
                "nop zmmword ptr [rax]" => vec![0x0f, 0x18, 0x20], // MASM doesn't accept `nop zmmword ..`, no way to round trip 0f1820
                "nop zmmword ptr [r8]" => vec![0x41, 0x0f, 0x18, 0x20], // MASM doesn't accept `nop zmmword ..`, no way to round trip 410f1820
                "sysenter" => vec![0x0f, 0x34], // MASM doesn't accept sysenter, but dumpbin prints it.
                "sysexit" => vec![0x0f, 0x35], // MASM doesn't accept sysexit, but dumpbin prints it.
                "vpscatterdd dword ptr [r15 + xmm29]{k6}, xmm8" => vec![0x62, 0x12, 0x7d, 0x06, 0xa0, 0x04, 0x2f], // MASM ...??? assembles vpscatter wrong???
                "vpscatterdd dword ptr [r15 + xmm25]{k6}, xmm10" => vec![0x62, 0x12, 0x7d, 0x06, 0xa0, 0x14, 0x0f], // MASM ...??? assembles vpscatter wrong???
                "vpscatterdd dword ptr [r15 + ymm25]{k6}, ymm10" => vec![0x62, 0x12, 0x7d, 0x26, 0xa0, 0x14, 0x0f], // MASM ...??? assembles vpscatter wrong???
                "vpscatterdd dword ptr [r15 + zmm25]{k6}, zmm10" => vec![0x62, 0x12, 0x7d, 0x46, 0xa0, 0x14, 0x0f], // MASM ...??? assembles vpscatter wrong???
                "vpscatterdq qword ptr [r15 + xmm25]{k6}, xmm10" => vec![0x62, 0x12, 0xfd, 0x46, 0xa0, 0x14, 0x0f], // MASM ...??? assembles vpscatter wrong???
                "vpscatterqd dword ptr [r15 + ymm25]{k6}, ymm10" => vec![0x62, 0x12, 0x7d, 0x46, 0xa1, 0x14, 0x0f], // MASM ...??? assembles vpscatter wrong???
                "vpscatterqq qword ptr [r15 + zmm25]{k6}, zmm10" => vec![0x62, 0x12, 0xfd, 0x46, 0xa1, 0x14, 0x0f], // MASM ...??? assembles vpscatter wrong???
/*
                "vpinsrb xmm9, xmm8, r10d, 77h" => vec![0xc4, 0x03, 0x39, 0x20, 0xca, 0x77], // MASM ...??? assembles the extra register number bit wrong???
                "vpinsrb xmm9, xmm8, byte ptr [r10], 77h" => vec![0xc4, 0x03, 0x39, 0x20, 0x0a, 0x77], // MASM ...??? assembles the extra register number bit wrong???
                "vpinsrd xmm9, xmm8, r10d, 77h" => vec![0xc4, 0x03, 0x39, 0x22, 0xca, 0x77], // MASM ...??? assembles the extra register number bit wrong???
                "vpinsrd xmm9, xmm8, dword ptr [r10], 77h" => vec![0xc4, 0x03, 0x39, 0x22, 0x0a, 0x77], // MASM ...??? assembles the extra register number bit wrong???
                "vpinsrq xmm9, xmm8, r10, 77h" => vec![0xc4, 0x03, 0xb9, 0x22, 0xca, 0x77], // MASM ...??? assembles the extra register number bit wrong???
                "vpinsrq xmm9, xmm8, qword ptr [r10], 77h" => vec![0xc4, 0x03, 0xb9, 0x22, 0x0a, 0x77], // MASM ...??? assembles the extra register number bit wrong???
                "vpblendvb xmm9, xmm8, xmm10, xmm7" => vec![0xc4, 0x03, 0x39, 0x4c, 0xca, 0x77], // MASM ...??? assembles the extra register number bit wrong???
                "vpblendvb ymm9, ymm8, ymm10, ymm7" => vec![0xc4, 0x03, 0x3d, 0x4c, 0xca, 0x77], // MASM ...??? assembles the extra register number bit wrong???
*/
                // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                "ud0 eax, ecx" => vec![0x66, 0x0f, 0xff, 0xc1],
                "ud0 eax, dword ptr [rcx]" => vec![0x66, 0x0f, 0xff, 0x01],
                "ud0 r8d, r9d" => vec![0x66, 0x4f, 0x0f, 0xff, 0xc1],
                "ud0 r13d, dword ptr [rbx - 54h]" => vec![0x4c, 0x0f, 0xff, 0x6b, 0xac],
                // masm seems to not know about fstpnce/fstp1 at all. since this is an undocumented instruction anyway, assemble it ourselves..
                "fstpnce st(3), st(0)" => vec![0xd9, 0xdb],
                // masm inserts a wait prefix here..
                "feni" => vec![0xdb, 0xe0],
                "fdisi" => vec![0xdb, 0xe1],
                "fsetpm" => vec![0xdb, 0xe4],
                _other => { tools::masm(&displayed_masm, CodeModel::Bits64).expect("can assemble") }
            };
            let masm_roundtrip = decoder.decode_slice(&masm_as_bytes).expect("can decode").display_with(DisplayStyle::Masm).to_string();
            // chasing down differences in how dumpbin/yax write "qword" is not useful to anyone..
            let external_masm_ish = external_masm_ish.replace(" mmword ", " qword ");
            let masm_roundtrip = masm_roundtrip.replace(" mmword ", " qword ");
            if external_masm_ish.starts_with("tzcnt") && masm_roundtrip.starts_with("bsf") {
                // this is ok, we support "decode as if without bmi1" but dumpbin does not, so dumpbin always says tzcnt.
                // masm accepts either and does the right thing.
            } else {
                assert_eq!(external_masm_ish, masm_roundtrip);
            }
            if let Some(masm_style) = disasm.masm.as_ref() {
                assert_eq!(masm_style, &masm_roundtrip);
            }
        } else {
            if let Some(masm_style) = disasm.masm.as_ref() {
                test_display_format(&decoder, bytes, masm_style, DisplayStyle::Masm);
            }
        }
    } else {
        test_invalid_under(&decoder, bytes);
    }
}

fn run_test(cases: &[TestCase]) {
    for tc in cases {
        if let Some(decodes) = tc.decodes.as_ref() {
            // if there are explicit feature sets, run only those decodes; the default decoder is
            // in the list if the test cares about it, and describes if it should work or not.
            let featuresets = if let Some(featuresets) = tc.featuresets {
                featuresets
            } else {
                &[(FeatureSet::Default, true)]
            };

            for (featureset, decode_ok) in featuresets {
                let decoder = featureset.into_decoder();
                check_decodes(&decoder, *decode_ok, tc.bytes, decodes);
            }
        } else {
            // similar to above:
            if let Some(featuresets) = tc.featuresets {
                for (featureset, decode_ok) in featuresets {
                    assert!(!decode_ok);
                    let decoder = featureset.into_decoder();

                    test_invalid_under(&decoder, tc.bytes);
                }
            } else {
                test_invalid(tc.bytes);
            }
        }
    }
}

// the extra { } in these arms are load-bearing to keep Rust from opening thousands of scopes for
// each lifetime. at around 32k lifetimes it'll stack overflow.
macro_rules! testcase {
    (invalid: features nodefault { $($feature:ident: $decode:expr$(,)?)+ } $bytes:expr) => {
        {
            use crate::long_mode::{TestCase, FeatureSet};

            let bytes: &'static [u8] = $bytes;
            let featuresets: &'static [(FeatureSet, bool)] = &[
                $((FeatureSet::$feature, $decode),)*
            ];
            TestCase {
                bytes,
                featuresets: Some(featuresets),
                decodes: None,
            }
        }
    };

    (invalid: features { $($feature:ident: $decode:expr$(,)?)+ } $bytes:expr) => {
        {
            use crate::long_mode::{TestCase, FeatureSet};

            let bytes: &'static [u8] = $bytes;
            let featuresets: &'static [(FeatureSet, bool)] = &[
                (FeatureSet::Minimal, false),
                (FeatureSet::Default, false),
                $((FeatureSet::$feature, $decode),)*
            ];
            TestCase {
                bytes,
                featuresets: Some(featuresets),
                decodes: None,
            }
        }
    };

    (invalid: $bytes:expr) => {
        {
            use crate::long_mode::TestCase;

            let bytes: &'static [u8] = $bytes;
            TestCase {
                bytes,
                featuresets: None,
                decodes: None,
            }
        }
    };

    (features nodefault { $($feature:ident: $decode:expr$(,)?)+ } $bytes:expr, $text:expr, masm: $masm_text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm, FeatureSet};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            let featuresets: &'static [(FeatureSet, bool)] = &[
                $((FeatureSet::$feature, $decode),)*
            ];
            TestCase {
                bytes,
                featuresets: Some(featuresets),
                decodes: Some(Disasm { display: text, c: None, masm: Some($masm_text) })
            }
        }
    };

    (features nodefault { $($feature:ident: $decode:expr$(,)?)+ } $bytes:expr, $text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm, FeatureSet};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            let featuresets: &'static [(FeatureSet, bool)] = &[
                $((FeatureSet::$feature, $decode),)*
            ];
            TestCase {
                bytes,
                featuresets: Some(featuresets),
                decodes: Some(Disasm { display: text, c: None, masm: None })
            }
        }
    };

    // need this above `($bytes:expr, $test:expr)` below to keep that case from
    // matching inappropriately early.
    (features { $($feature:ident: $decode:expr$(,)?)+ } $bytes:expr, $text:expr, masm: $masm_text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm, FeatureSet};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            let featuresets: &'static [(FeatureSet, bool)] = &[
                (FeatureSet::Minimal, false),
                (FeatureSet::Default, true),
                $((FeatureSet::$feature, $decode),)*
            ];
            TestCase {
                bytes,
                featuresets: Some(featuresets),
                decodes: Some(Disasm { display: text, c: None, masm: Some($masm_text) })
            }
        }
    };

    // need this above `($bytes:expr, $test:expr)` below to keep that case from
    // matching inappropriately early.
    (features { $($feature:ident: $decode:expr$(,)?)+ } $bytes:expr, $text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm, FeatureSet};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            let featuresets: &'static [(FeatureSet, bool)] = &[
                (FeatureSet::Minimal, false),
                (FeatureSet::Default, true),
                $((FeatureSet::$feature, $decode),)*
            ];
            TestCase {
                bytes,
                featuresets: Some(featuresets),
                decodes: Some(Disasm { display: text, c: None, masm: None })
            }
        }
    };

    ({ $($feature:ident: $decode:expr)+ } $bytes:expr, $text:expr, c: $c_text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm, FeatureSet};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            let c: &'static str = $c_text;
            let featuresets: &'static [(FeatureSet, bool)] = &[
                (FeatureSet::Minimal, false),
                (FeatureSet::Default, true),
                $((FeatureSet::$feature, $decode))*
            ];
            TestCase {
                bytes,
                featuresets: Some(featuresets),
                decodes: Some(Disasm { display: text, c: Some(c), masm: None })
            }
        }
    };

    ($bytes:expr, $text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            TestCase {
                bytes,
                featuresets: None,
                decodes: Some(Disasm { display: text, c: None, masm: None })
            }
        }
    };

    ($bytes:expr, $text:expr, c: $c_text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            let c: &'static str = $c_text;
            TestCase {
                bytes,
                featuresets: None,
                decodes: Some(Disasm { display: text, c: Some(c) })
            }
        }
    };

    ($bytes:expr, $text:expr, masm: $masm_text:expr) => {
        {
            use crate::long_mode::{TestCase, Disasm};

            let bytes: &'static [u8] = $bytes;
            let text: &'static str = $text;
            let masm: &'static str = $masm_text;
            TestCase {
                bytes,
                featuresets: None,
                decodes: Some(Disasm { display: text, c: None, masm: Some(masm) })
            }
        }
    };
}

mod modrm_decode {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // just modr
        testcase!(&[0x33, 0x08], "xor ecx, dword [rax]"),
        testcase!(&[0x33, 0x20], "xor esp, dword [rax]"),
        testcase!(&[0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor eax, dword [rip + 0x12345678]"),
        testcase!(&[0x33, 0x41, 0x23], "xor eax, dword [rcx + 0x23]"),
        testcase!(&[0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor eax, dword [rcx + 0x43650123]"),
        testcase!(&[0x33, 0xc1], "xor eax, ecx"),

        // modrm + rex.w
        testcase!(&[0x48, 0x33, 0x08], "xor rcx, qword [rax]"),
        testcase!(&[0x48, 0x33, 0x20], "xor rsp, qword [rax]"),
        testcase!(&[0x48, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor rax, qword [rip + 0x12345678]"),
        testcase!(&[0x48, 0x33, 0x41, 0x23], "xor rax, qword [rcx + 0x23]"),
        testcase!(&[0x48, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor rax, qword [rcx + 0x43650123]"),
        testcase!(&[0x48, 0x33, 0xc1], "xor rax, rcx"),

        // modrm + rex.r
        testcase!(&[0x44, 0x33, 0x08], "xor r9d, dword [rax]"),
        testcase!(&[0x44, 0x33, 0x20], "xor r12d, dword [rax]"),
        testcase!(&[0x44, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor r8d, dword [rip + 0x12345678]"),
        testcase!(&[0x44, 0x33, 0x41, 0x23], "xor r8d, dword [rcx + 0x23]"),
        testcase!(&[0x44, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor r8d, dword [rcx + 0x43650123]"),
        testcase!(&[0x44, 0x33, 0xc1], "xor r8d, ecx"),

        // modrm + rex.rb
        testcase!(&[0x45, 0x33, 0x08], "xor r9d, dword [r8]"),
        testcase!(&[0x45, 0x33, 0x20], "xor r12d, dword [r8]"),
        testcase!(&[0x45, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor r8d, dword [rip + 0x12345678]"),
        testcase!(&[0x45, 0x33, 0x41, 0x23], "xor r8d, dword [r9 + 0x23]"),
        testcase!(&[0x45, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor r8d, dword [r9 + 0x43650123]"),
        testcase!(&[0x45, 0x33, 0xc1], "xor r8d, r9d"),

        // sib
        testcase!(&[0x33, 0x04, 0x0a], "xor eax, dword [rdx + rcx * 1]", masm: "xor eax, dword ptr [rdx + rcx]"),
        testcase!(&[0x33, 0x04, 0x4a], "xor eax, dword [rdx + rcx * 2]"),
        testcase!(&[0x33, 0x04, 0x8a], "xor eax, dword [rdx + rcx * 4]"),
        testcase!(&[0x33, 0x04, 0xca], "xor eax, dword [rdx + rcx * 8]"),
        testcase!(&[0x33, 0x04, 0x20], "xor eax, dword [rax]"),
        testcase!(&[0x33, 0x04, 0x60], "xor eax, dword [rax]"),
        testcase!(&[0x33, 0x04, 0xa0], "xor eax, dword [rax]"),
        testcase!(&[0x33, 0x04, 0xe0], "xor eax, dword [rax]"),
        testcase!(&[0x42, 0x33, 0x04, 0x20], "xor eax, dword [rax + r12 * 1]", masm: "xor eax, dword ptr [rax + r12]"),
        testcase!(&[0x42, 0x33, 0x04, 0x60], "xor eax, dword [rax + r12 * 2]"),
        testcase!(&[0x42, 0x33, 0x04, 0xa0], "xor eax, dword [rax + r12 * 4]"),
        testcase!(&[0x42, 0x33, 0x04, 0xe0], "xor eax, dword [rax + r12 * 8]"),
        testcase!(&[0x43, 0x33, 0x04, 0x20], "xor eax, dword [r8 + r12 * 1]"),
        testcase!(&[0x43, 0x33, 0x04, 0x60], "xor eax, dword [r8 + r12 * 2]"),
        testcase!(&[0x43, 0x33, 0x04, 0xa0], "xor eax, dword [r8 + r12 * 4]"),
        testcase!(&[0x43, 0x33, 0x04, 0xe0], "xor eax, dword [r8 + r12 * 8]"),
        testcase!(&[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [0x44332211]"),
        testcase!(&[0x41, 0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [0x44332211]"),

        testcase!(&[0x33, 0x44, 0x65, 0x11], "xor eax, dword [rbp + 0x11]"),
        testcase!(&[0x41, 0x33, 0x44, 0x65, 0x11], "xor eax, dword [r13 + 0x11]"),
        testcase!(&[0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [rbp + 0x44332211]"),
        testcase!(&[0x41, 0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [r13 + 0x44332211]"),
        testcase!(&[0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [0x44332211]", masm: "xor eax, dword ptr [44332211h]"),
        testcase!(&[0x41, 0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [0x44332211]"),

        // specifically sib with base == 0b101
        // mod bits 00
        testcase!(&[0x42, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [r12 * 1 + 0x50403020]"),
        testcase!(&[0x43, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [r12 * 1 + 0x50403020]"),
        // mod bits 01
        testcase!(&[0x42, 0x33, 0x74, 0x25, 0x20], "xor esi, dword [rbp + r12 * 1 + 0x20]"),
        testcase!(&[0x43, 0x33, 0x74, 0x25, 0x20], "xor esi, dword [r13 + r12 * 1 + 0x20]"),
        // mod bits 10
        testcase!(&[0x42, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [rbp + r12 * 1 + 0x50403020]"),
        testcase!(&[0x43, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [r13 + r12 * 1 + 0x50403020]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod mmx {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x4f, 0x0f, 0xf7, 0xc1], "maskmovq mm0, mm1"),
        testcase!(&[0x0f, 0xf7, 0xc1], "maskmovq mm0, mm1"),
        testcase!(invalid: &[0x0f, 0xf7, 0x01]),

        testcase!(&[0x4f, 0x0f, 0xe7, 0x03], "movntq qword [r11], mm0"),
        testcase!(&[0x0f, 0xe7, 0x03], "movntq qword [rbx], mm0"),
        testcase!(invalid: &[0x0f, 0xe7, 0xc3]),

        testcase!(&[0x4f, 0x0f, 0xc3, 0x03], "movnti qword [r11], r8"),
        testcase!(invalid: &[0x66, 0x0f, 0xc3, 0x03]),
        testcase!(&[0x0f, 0xc3, 0x03], "movnti dword [rbx], eax"),
        testcase!(invalid: &[0x0f, 0xc3, 0xc3]),

        testcase!(&[0x4f, 0x0f, 0x7e, 0xcf], "movd r15, mm1"),
        testcase!(&[0x41, 0x0f, 0x7e, 0xcf], "movd r15d, mm1"),
        testcase!(&[0x4f, 0x0f, 0x7f, 0xcf], "movq mm7, mm1"),
        testcase!(&[0x4f, 0x0f, 0x7f, 0x0f], "movq qword [r15], mm1"),
        testcase!(&[0x0f, 0xc4, 0xc0, 0x14], "pinsrw mm0, eax, 0x14"),
        testcase!(&[0x4f, 0x0f, 0xc4, 0xc0, 0x14], "pinsrw mm0, r8d, 0x14"),
        testcase!(&[0x4f, 0x0f, 0xc4, 0x00, 0x14], "pinsrw mm0, word [r8], 0x14"),
        testcase!(&[0x4f, 0x0f, 0xd1, 0xcf], "psrlw mm1, mm7"),
        testcase!(&[0x4f, 0x0f, 0xd1, 0x00], "psrlw mm0, qword [r8]"),
        testcase!(invalid: &[0x4f, 0x0f, 0xd7, 0x00]),
        testcase!(&[0x4f, 0x0f, 0xd7, 0xcf], "pmovmskb r9d, mm7"),
        testcase!(&[0x0f, 0x3a, 0x0f, 0xc1, 0x23], "palignr mm0, mm1, 0x23"),
        testcase!(&[0x0f, 0xf9, 0xc2], "psubw mm0, mm2"),
        testcase!(&[0x0f, 0xfd, 0xd2], "paddw mm2, mm2"),
        testcase!(&[0x0f, 0x6f, 0xe9], "movq mm5, mm1"),
        testcase!(&[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77], "pmulhw mm7, qword [rip + 0x77ccbbaa]"),

        testcase!(&[0x0f, 0x38, 0x00, 0xda], "pshufb mm3, mm2"),

        testcase!(&[0x0f, 0x74, 0xc2], "pcmpeqb mm0, mm2"),
        testcase!(&[0x0f, 0x75, 0xc2], "pcmpeqw mm0, mm2"),
        testcase!(&[0x0f, 0x76, 0xc2], "pcmpeqd mm0, mm2"),

        testcase!(&[0x66, 0x0f, 0xc5, 0xd8, 0xff], "pextrw ebx, xmm0, 0xff"),
        testcase!(invalid: &[0x66, 0x0f, 0xc5, 0x08, 0xff]),

        testcase!(&[0x0f, 0xc5, 0xd1, 0x00], "pextrw edx, mm1, 0x0"),
        testcase!(invalid: &[0x0f, 0xc5, 0x01, 0x00]),

        testcase!(&[0x0f, 0xd8, 0xc2], "psubusb mm0, mm2"),
        testcase!(&[0x0f, 0xd9, 0xc2], "psubusw mm0, mm2"),
        testcase!(&[0x0f, 0xda, 0xc2], "pminub mm0, mm2"),
        testcase!(&[0x0f, 0xdb, 0xc2], "pand mm0, mm2"),
        testcase!(&[0x0f, 0xdc, 0xc2], "paddusb mm0, mm2"),
        testcase!(&[0x0f, 0xdd, 0xc2], "paddusw mm0, mm2"),
        testcase!(&[0x0f, 0xde, 0xc2], "pmaxub mm0, mm2"),
        testcase!(&[0x0f, 0xdf, 0xc2], "pandn mm0, mm2"),

        testcase!(&[0x0f, 0xe8, 0xc2], "psubsb mm0, mm2"),
        testcase!(&[0x0f, 0xe9, 0xc2], "psubsw mm0, mm2"),
        testcase!(&[0x0f, 0xea, 0xc2], "pminsw mm0, mm2"),
        testcase!(&[0x0f, 0xeb, 0xc2], "por mm0, mm2"),
        testcase!(&[0x0f, 0xec, 0xc2], "paddsb mm0, mm2"),
        testcase!(&[0x0f, 0xed, 0xc2], "paddsw mm0, mm2"),
        testcase!(&[0x0f, 0xee, 0xc2], "pmaxsw mm0, mm2"),
        testcase!(&[0x0f, 0xef, 0xc2], "pxor mm0, mm2"),

        testcase!(invalid: &[0x0f, 0xf0, 0xc2]),
        testcase!(&[0x0f, 0xf1, 0xc2], "psllw mm0, mm2"),
        testcase!(&[0x0f, 0xf2, 0xc2], "pslld mm0, mm2"),
        testcase!(&[0x0f, 0xf3, 0xc2], "psllq mm0, mm2"),
        testcase!(&[0x0f, 0xf4, 0xc2], "pmuludq mm0, mm2"),
        testcase!(&[0x0f, 0xf5, 0xc2], "pmaddwd mm0, mm2"),
        testcase!(&[0x0f, 0xf6, 0xc2], "psadbw mm0, mm2"),
        testcase!(&[0x0f, 0xf8, 0xc2], "psubb mm0, mm2"),
        testcase!(&[0x0f, 0xf9, 0xc2], "psubw mm0, mm2"),
        testcase!(&[0x0f, 0xfa, 0xc2], "psubd mm0, mm2"),
        testcase!(&[0x0f, 0xfb, 0xc2], "psubq mm0, mm2"),
        testcase!(&[0x0f, 0xfc, 0xc2], "paddb mm0, mm2"),
        testcase!(&[0x0f, 0xfd, 0xc2], "paddw mm0, mm2"),
        testcase!(&[0x0f, 0xfe, 0xc2], "paddd mm0, mm2"),

        testcase!(&[0x0f, 0xf1, 0x02], "psllw mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xf2, 0x02], "pslld mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xf3, 0x02], "psllq mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xf4, 0x02], "pmuludq mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xf5, 0x02], "pmaddwd mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xf6, 0x02], "psadbw mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xf8, 0x02], "psubb mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xf9, 0x02], "psubw mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xfa, 0x02], "psubd mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xfb, 0x02], "psubq mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xfc, 0x02], "paddb mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xfd, 0x02], "paddw mm0, qword [rdx]"),
        testcase!(&[0x0f, 0xfe, 0x02], "paddd mm0, qword [rdx]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod cvt {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x2c, 0xcf], "cvttps2pi mm1, xmm7"),
        testcase!(&[0x48, 0x0f, 0x2c, 0xcf], "cvttps2pi mm1, xmm7"),
        testcase!(&[0x4f, 0x0f, 0x2c, 0xcf], "cvttps2pi mm1, xmm15"),
        testcase!(&[0x4f, 0x0f, 0x2a, 0xcf], "cvtpi2ps xmm9, mm7"),
        testcase!(&[0x4f, 0x0f, 0x2a, 0x00], "cvtpi2ps xmm8, qword [r8]"),
        testcase!(&[0x0f, 0x2a, 0x00], "cvtpi2ps xmm0, qword [rax]"),
        testcase!(&[0x4f, 0x66, 0x0f, 0x2a, 0xcf], "cvtpi2pd xmm1, mm7"),
        testcase!(&[0x66, 0x4f, 0x0f, 0x2a, 0xcf], "cvtpi2pd xmm9, mm7"),
        testcase!(&[0x4f, 0xf3, 0x0f, 0x2a, 0xcf], "cvtsi2ss xmm1, edi"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2a, 0xcf], "cvtsi2ss xmm9, r15"),
        testcase!(&[0x4f, 0xf2, 0x0f, 0x2a, 0xcf], "cvtsi2sd xmm1, edi"),
        testcase!(&[0xf2, 0x4f, 0x0f, 0x2a, 0xcf], "cvtsi2sd xmm9, r15"),
        testcase!(&[0x4f, 0xf2, 0x0f, 0x2a, 0x00], "cvtsi2sd xmm0, dword [rax]"),
        testcase!(&[0xf2, 0x4f, 0x0f, 0x2a, 0x00], "cvtsi2sd xmm8, qword [r8]"),
        testcase!(&[0x4f, 0xf3, 0x0f, 0x2a, 0x00], "cvtsi2ss xmm0, dword [rax]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2a, 0x00], "cvtsi2ss xmm8, qword [r8]"),
        testcase!(&[0x4f, 0x66, 0x0f, 0x2a, 0x00], "cvtpi2pd xmm0, qword [rax]"),
        testcase!(&[0x66, 0x4f, 0x0f, 0x2a, 0x00], "cvtpi2pd xmm8, qword [r8]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod aesni {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdb, 0x0f], "aesimc xmm1, xmmword [rdi]"),
        testcase!(features { AESNI: true } &[0x66, 0x4f, 0x0f, 0x38, 0xdb, 0xcf], "aesimc xmm9, xmm15"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdc, 0x0f], "aesenc xmm1, xmmword [rdi]"),
        testcase!(features { AESNI: true } &[0x66, 0x4f, 0x0f, 0x38, 0xdc, 0xcf], "aesenc xmm9, xmm15"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdd, 0x0f], "aesenclast xmm1, xmmword [rdi]"),
        testcase!(features { AESNI: true } &[0x66, 0x4f, 0x0f, 0x38, 0xdd, 0xcf], "aesenclast xmm9, xmm15"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xde, 0x0f], "aesdec xmm1, xmmword [rdi]"),
        testcase!(features { AESNI: true } &[0x66, 0x4f, 0x0f, 0x38, 0xde, 0xcf], "aesdec xmm9, xmm15"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdf, 0x0f], "aesdeclast xmm1, xmmword [rdi]"),
        testcase!(features { AESNI: true } &[0x66, 0x4f, 0x0f, 0x38, 0xdf, 0xcf], "aesdeclast xmm9, xmm15"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x3a, 0xdf, 0x0f, 0xaa], "aeskeygenassist xmm1, xmmword [rdi], 0xaa"),
        testcase!(features { AESNI: true } &[0x66, 0x4f, 0x0f, 0x3a, 0xdf, 0xcf, 0xaa], "aeskeygenassist xmm9, xmm15, 0xaa"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sse2 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x10, 0x0c, 0xc7], "movsd xmm1, qword [rdi + rax * 8]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x11, 0x0c, 0xc7], "movsd qword [rdi + rax * 8], xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x11, 0x0c, 0xc7], "movupd xmmword [rdi + rax * 8], xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x12, 0x03], "movlpd xmm8, qword [r11]"), // reg-mem is movlpd
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x13, 0x03], "movlpd qword [r11], xmm8"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x13, 0xc3]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x14, 0x03], "unpcklpd xmm8, xmmword [r11]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x14, 0xc3], "unpcklpd xmm8, xmm11"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x15, 0x03], "unpckhpd xmm8, xmmword [r11]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x15, 0xc3], "unpckhpd xmm8, xmm11"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x16, 0x03], "movhpd xmm8, qword [r11]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x17, 0x03], "movhpd qword [r11], xmm8"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x17, 0xc3]),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x28, 0xd0], "movapd xmm10, xmm8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x28, 0x00], "movapd xmm8, xmmword [r8]"),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2a, 0xcf], "cvtpi2pd xmm9, mm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2a, 0x0f], "cvtpi2pd xmm9, qword [r15]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x2a, 0xcf], "cvtsi2sd xmm9, r15"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x2a, 0x0f], "cvtsi2sd xmm9, qword [r15]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2b, 0x0f], "movntpd xmmword [r15], xmm9"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2c, 0xcf], "cvttpd2pi mm1, xmm15"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2c, 0x0f], "cvttpd2pi mm1, xmmword [r15]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x2c, 0xcf], "cvttsd2si r9, xmm15"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x2c, 0x0f], "cvttsd2si r9, qword [r15]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x40, 0x0f, 0x2c, 0xcf], "cvttsd2si ecx, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x40, 0x0f, 0x2c, 0x0f], "cvttsd2si ecx, qword [rdi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2d, 0xcf], "cvtpd2pi mm1, xmm15"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2d, 0x0f], "cvtpd2pi mm1, xmmword [r15]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x2d, 0xcf], "cvtsd2si r9, xmm15"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x2d, 0x0f], "cvtsd2si r9, qword [r15]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x40, 0x0f, 0x2d, 0xcf], "cvtsd2si ecx, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x40, 0x0f, 0x2d, 0x0f], "cvtsd2si ecx, qword [rdi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2e, 0xcf], "ucomisd xmm9, xmm15"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2e, 0x0f], "ucomisd xmm9, qword [r15]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2f, 0xcf], "comisd xmm9, xmm15"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x2f, 0x0f], "comisd xmm9, qword [r15]"),

        /*
         * .... 660f38
         * .... 660f7f
         */

        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x50, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x50, 0xc1], "movmskpd r8d, xmm9"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x51, 0x01], "sqrtpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x51, 0x01], "sqrtsd xmm8, qword [r9]"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x52, 0x01]),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x53, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x54, 0x01], "andpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x55, 0x01], "andnpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x56, 0x01], "orpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x57, 0x01], "xorpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x58, 0x01], "addpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x58, 0x01], "addsd xmm8, qword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x59, 0x01], "mulpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x59, 0x01], "mulsd xmm8, qword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x5a, 0x01], "cvtpd2ps xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x5a, 0x01], "cvtsd2ss xmm8, qword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x5b, 0x01], "cvtps2dq xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x5c, 0x01], "subpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x5c, 0x01], "subsd xmm8, qword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x5d, 0x01], "minpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x5d, 0x01], "minsd xmm8, qword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x5e, 0x01], "divpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x5e, 0x01], "divsd xmm8, qword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x5f, 0x01], "maxpd xmm8, xmmword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0x5f, 0x01], "maxsd xmm8, qword [r9]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x60, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpcklbw xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x61, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpcklwd xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x62, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckldq xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x63, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "packsswb xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x64, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "pcmpgtb xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x65, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "pcmpgtw xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x66, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "pcmpgtd xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x67, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "packuswb xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x68, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhbw xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x69, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhwd xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x6a, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhdq xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x6b, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "packssdw xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x6c, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpcklqdq xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x6d, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhqdq xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x6e, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movq xmm11, qword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x6f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqa xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x48, 0x0f, 0x6e, 0xc0], "movq xmm0, rax"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x70, 0xc0, 0x4e], "pshufd xmm0, xmm0, 0x4e"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x70, 0xc0, 0x4e], "pshuflw xmm0, xmm0, 0x4e"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf3, 0x0f, 0x70, 0xc0, 0x4e], "pshufhw xmm0, xmm0, 0x4e"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x71, 0x10, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x71, 0xd0, 0x8f], "psrlw xmm8, 0x8f"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x71, 0x20, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x71, 0xe0, 0x8f], "psraw xmm8, 0x8f"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x71, 0x30, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x71, 0xf0, 0x8f], "psllw xmm8, 0x8f"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x72, 0x10, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x72, 0xd0, 0x8f], "psrld xmm8, 0x8f"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x72, 0x20, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x72, 0xe0, 0x8f], "psrad xmm8, 0x8f"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x72, 0x30, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x72, 0xf0, 0x8f], "pslld xmm8, 0x8f"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x73, 0x10, 0x8f]),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x73, 0x18, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x73, 0xd0, 0x8f], "psrlq xmm8, 0x8f"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x73, 0xd8, 0x8f], "psrldq xmm8, 0x8f"),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x73, 0x30, 0x8f]),
        testcase!(invalid: &[0x66, 0x4f, 0x0f, 0x73, 0x38, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x73, 0xf0, 0x8f], "psllq xmm8, 0x8f"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x73, 0xf8, 0x8f], "pslldq xmm8, 0x8f"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x7e, 0xc1], "movd ecx, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x48, 0x0f, 0x7e, 0xc1], "movq rcx, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x48, 0x0f, 0x7e, 0x01], "movq qword [rcx], xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0x7e, 0xc1], "movq r9, xmm8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true }
            &[0x66, 0x4f, 0x0f, 0x7f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqa xmmword [r12 + r11 * 4 - 0x334455cc], xmm11"
        ),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc2, 0xc3, 0x08], "cmppd xmm0, xmm3, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xc2, 0xc3, 0x08], "cmppd xmm8, xmm11, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xc2, 0x03, 0x08], "cmppd xmm8, xmmword [r11], 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0xc2, 0xc3, 0x08], "cmpsd xmm0, xmm3, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0xc2, 0xc3, 0x08], "cmpsd xmm8, xmm11, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0xc2, 0x03, 0x08], "cmpsd xmm8, qword [r11], 0x8"),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc4, 0xc3, 0x08], "pinsrw xmm0, ebx, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xc4, 0xc3, 0x08], "pinsrw xmm8, r11d, 0x8"),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc4, 0x03, 0x08], "pinsrw xmm0, word [rbx], 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xc4, 0x03, 0x08], "pinsrw xmm8, word [r11], 0x8"),

    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0xc3, 0x08], "pextrw eax, xmm3, 0x8"),
    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xc5, 0xc3, 0x08], "pextrw r8d, xmm11, 0x8"),
    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0x03, 0x08]),
    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0x40, 0x08]),
    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0x80, 0x08]),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xc6, 0x03, 0x08], "shufpd xmm8, xmmword [r11], 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc6, 0x03, 0x08], "shufpd xmm0, xmmword [rbx], 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc6, 0xc3, 0x08], "shufpd xmm0, xmm3, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd1, 0xc1], "psrlw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd1, 0x01], "psrlw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd2, 0xc1], "psrld xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd2, 0x01], "psrld xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd3, 0xc1], "psrlq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd3, 0x01], "psrlq xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd4, 0xc1], "paddq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd4, 0x01], "paddq xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd5, 0xc1], "pmullw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd5, 0x01], "pmullw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd6, 0xc1], "movq xmm1, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd6, 0x01], "movq qword [rcx], xmm0"),
        testcase!(invalid: &[0xf3, 0x4f, 0x0f, 0xd6, 0x03]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf3, 0x4f, 0x0f, 0xd6, 0xc3], "movq2dq xmm8, mm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x4f, 0x0f, 0xd6, 0xc3], "movdq2q mm0, xmm11"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd7, 0xc1], "pmovmskb eax, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xd7, 0xc1], "pmovmskb r8d, xmm9"),
        testcase!(invalid: &[0x66, 0x0f, 0xd7, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd8, 0xc1], "psubusb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd8, 0x01], "psubusb xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd9, 0xc1], "psubusw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd9, 0x01], "psubusw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xda, 0xc1], "pminub xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xda, 0x01], "pminub xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdb, 0xc1], "pand xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdb, 0x01], "pand xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdc, 0xc1], "paddusb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdc, 0x01], "paddusb xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdd, 0xc1], "paddusw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdd, 0x01], "paddusw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xde, 0xc1], "pmaxub xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xde, 0x01], "pmaxub xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdf, 0xc1], "pandn xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdf, 0x01], "pandn xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe0, 0xc1], "pavgb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe0, 0x01], "pavgb xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe1, 0xc1], "psraw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe1, 0x01], "psraw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe2, 0xc1], "psrad xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe2, 0x01], "psrad xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe3, 0xc1], "pavgw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe3, 0x01], "pavgw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe4, 0xc1], "pmulhuw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe4, 0x01], "pmulhuw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe5, 0xc1], "pmulhw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe5, 0x01], "pmulhw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe6, 0xc1], "cvttpd2dq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe6, 0x01], "cvttpd2dq xmm0, xmmword [rcx]"),
        testcase!(invalid: &[0x66, 0x0f, 0xe7, 0xc1]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe7, 0x01], "movntdq xmmword [rcx], xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe8, 0xc1], "psubsb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe8, 0x01], "psubsb xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe9, 0xc1], "psubsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe9, 0x01], "psubsw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xea, 0xc1], "pminsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xea, 0x01], "pminsw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xc3], "por xmm0, xmm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xc4], "por xmm0, xmm4"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xd3], "por xmm2, xmm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0x12], "por xmm2, xmmword [rdx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xc1], "por xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0x01], "por xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xec, 0xc1], "paddsb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xec, 0x01], "paddsb xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xed, 0xc1], "paddsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xed, 0x01], "paddsw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xee, 0xc1], "pmaxsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xee, 0x01], "pmaxsw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xef, 0xc1], "pxor xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xef, 0x01], "pxor xmm0, xmmword [rcx]"),
        testcase!(invalid: &[0x66, 0x0f, 0xf0, 0xc1]),
        testcase!(invalid: &[0x66, 0x0f, 0xf0, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf1, 0xc1], "psllw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf1, 0x01], "psllw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf2, 0xc1], "pslld xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf2, 0x01], "pslld xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf3, 0xc1], "psllq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf3, 0x01], "psllq xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf4, 0xc1], "pmuludq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf4, 0x01], "pmuludq xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf5, 0xc1], "pmaddwd xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf5, 0x01], "pmaddwd xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf6, 0xc1], "psadbw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf6, 0x01], "psadbw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf7, 0xc1], "maskmovdqu xmm0, xmm1"),
        testcase!(invalid: &[0x66, 0x0f, 0xf7, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0xc1], "psubb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0x01], "psubb xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf9, 0xc1], "psubw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf9, 0x01], "psubw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfa, 0xc1], "psubd xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfa, 0x01], "psubd xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfb, 0xc1], "psubq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfb, 0x01], "psubq xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfc, 0xc1], "paddb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfc, 0x01], "paddb xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfd, 0xc1], "paddw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfd, 0x01], "paddw xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfe, 0xc1], "paddd xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfe, 0x01], "paddd xmm0, xmmword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xff, 0xc1], "ud0 eax, ecx"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0xff, 0xc1], "ud0 eax, ecx"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf3, 0x0f, 0xff, 0xc1], "ud0 eax, ecx"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xff, 0x01], "ud0 eax, dword [rcx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x4f, 0x0f, 0xff, 0xc1], "ud0 r8d, r9d"),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x74, 0xc1], "pcmpeqb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x74, 0x12], "pcmpeqb xmm2, xmmword [rdx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0xc8], "psubb xmm1, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0xd0], "psubb xmm2, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0x12], "psubb xmm2, xmmword [rdx]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sse3 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0xf0, 0x0f], "lddqu xmm1, xmmword [rdi]"),
        testcase!(invalid: &[0xf2, 0x0f, 0xf0, 0xcf]),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0xd0, 0x0f], "addsubps xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0xd0, 0xcf], "addsubps xmm1, xmm7"),
        testcase!(invalid: &[0xf3, 0x0f, 0xd0, 0x0f]),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x4f, 0x0f, 0xd0, 0xcf], "addsubps xmm9, xmm15"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0xd0, 0x0f], "addsubpd xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0xd0, 0xcf], "addsubpd xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x4f, 0x0f, 0xd0, 0xcf], "addsubpd xmm9, xmm15"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7c, 0x0f], "haddps xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7c, 0xcf], "haddps xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x4f, 0x0f, 0x7c, 0xcf], "haddps xmm9, xmm15"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7c, 0x0f], "haddpd xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7c, 0xcf], "haddpd xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x4f, 0x0f, 0x7c, 0xcf], "haddpd xmm9, xmm15"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7d, 0x0f], "hsubps xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7d, 0xcf], "hsubps xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x4f, 0x0f, 0x7d, 0xcf], "hsubps xmm9, xmm15"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7d, 0x0f], "hsubpd xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7d, 0xcf], "hsubpd xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x4f, 0x0f, 0x7d, 0xcf], "hsubpd xmm9, xmm15"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x12, 0x0f], "movsldup xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x12, 0xcf], "movsldup xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x4f, 0x0f, 0x12, 0xcf], "movsldup xmm9, xmm15"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x16, 0x0f], "movshdup xmm1, xmmword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x16, 0xcf], "movshdup xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x4f, 0x0f, 0x16, 0xcf], "movshdup xmm9, xmm15"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x12, 0x0f], "movddup xmm1, qword [rdi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x12, 0xcf], "movddup xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x4f, 0x0f, 0x12, 0xcf], "movddup xmm9, xmm15"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x0f, 0x01, 0xc8], "monitor", masm: "monitor rax, rcx, rdx"),
        testcase!(invalid: &[0x66, 0x0f, 0x01, 0xc8]),
        testcase!(invalid: &[0xf3, 0x0f, 0x01, 0xc8]),
        testcase!(invalid: &[0xf2, 0x0f, 0x01, 0xc8]),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x0f, 0x01, 0xc9], "mwait"),
        testcase!(invalid: &[0x66, 0x0f, 0x01, 0xc9]),
        testcase!(invalid: &[0xf2, 0x0f, 0x01, 0xc9]),
        testcase!(invalid: &[0xf3, 0x0f, 0x01, 0xc9]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sse4_2 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x38, 0x37, 0x03], "pcmpgtq xmm0, xmmword [rbx]"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x38, 0x37, 0xc3], "pcmpgtq xmm0, xmm3"),

        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf0, 0x06], "crc32 eax, byte [rsi]"),
        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf0, 0xc6], "crc32 eax, dh"),
        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf1, 0x06], "crc32 eax, dword [rsi]"),
        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf1, 0xc6], "crc32 eax, esi"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0xf2, 0x0f, 0x38, 0xf1, 0xc6], "crc32 eax, si"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0xf2, 0x48, 0x0f, 0x38, 0xf1, 0xc6], "crc32 rax, rsi"),

        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x60, 0xc6, 0x54], "pcmpestrm xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x60, 0x06, 0x54], "pcmpestrm xmm0, xmmword [rsi], 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x61, 0xc6, 0x54], "pcmpestri xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x61, 0x06, 0x54], "pcmpestri xmm0, xmmword [rsi], 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x62, 0xc6, 0x54], "pcmpistrm xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x62, 0x06, 0x54], "pcmpistrm xmm0, xmmword [rsi], 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x63, 0xc6, 0x54], "pcmpistri xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x63, 0x06, 0x54], "pcmpistri xmm0, xmmword [rsi], 0x54"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sse4_1 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0c, 0x11, 0x22], "blendps xmm2, xmmword [rcx], 0x22"),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0c, 0xc1, 0x22], "blendps xmm0, xmm1, 0x22"),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0d, 0x11, 0x22], "blendpd xmm2, xmmword [rcx], 0x22"),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0d, 0xc1, 0x22], "blendpd xmm0, xmm1, 0x22"),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x10, 0x06], "pblendvb xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x10, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x14, 0x06], "blendvps xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x14, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x15, 0x06], "blendvpd xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x15, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x17, 0x06], "ptest xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x17, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x20, 0x06], "pmovsxbw xmm0, qword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x20, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x21, 0x06], "pmovsxbd xmm0, dword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x21, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x22, 0x06], "pmovsxbq xmm0, word [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x22, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x23, 0x06], "pmovsxwd xmm0, qword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x23, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x24, 0x06], "pmovsxwq xmm0, dword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x24, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x25, 0x06], "pmovsxdq xmm0, qword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x25, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x28, 0x06], "pmuldq xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x28, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x29, 0x06], "pcmpeqq xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x29, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x2a, 0x06], "movntdqa xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x66, 0x0f, 0x38, 0x2a, 0xc6]),
        testcase!(invalid: &[0x0f, 0x38, 0x2a, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x2b, 0x06], "packusdw xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x2b, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x30, 0x06], "pmovzxbw xmm0, qword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x30, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x31, 0x06], "pmovzxbd xmm0, dword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x31, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x32, 0x06], "pmovzxbq xmm0, word [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x32, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x33, 0x06], "pmovzxwd xmm0, qword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x33, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x34, 0x06], "pmovzxwq xmm0, dword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x34, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x35, 0x06], "pmovzxdq xmm0, qword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x35, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x38, 0x06], "pminsb xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x38, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x39, 0x06], "pminsd xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x39, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3a, 0x06], "pminuw xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3a, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3b, 0x06], "pminud xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3b, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3c, 0x06], "pmaxsb xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3c, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3d, 0x06], "pmaxsd xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3d, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3e, 0x06], "pmaxuw xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3e, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3f, 0x06], "pmaxud xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3f, 0x06]),


        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x40, 0x06], "pmulld xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x40, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x41, 0x06], "phminposuw xmm0, xmmword [rsi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x41, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x08, 0x06, 0x31], "roundps xmm0, xmmword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x08, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x09, 0x06, 0x31], "roundpd xmm0, xmmword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x09, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0a, 0x06, 0x31], "roundss xmm0, dword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x0a, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0b, 0x06, 0x31], "roundsd xmm0, qword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x0b, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0e, 0x06, 0x31], "pblendw xmm0, xmmword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x0e, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x14, 0x06, 0x31], "pextrb byte [rsi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x14, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x15, 0x06, 0x31], "pextrw word [rsi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x15, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x16, 0x06, 0x31], "pextrd dword [rsi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x16, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x48, 0x0f, 0x3a, 0x16, 0x06, 0x31], "pextrq qword [rsi], xmm0, 0x31"),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x17, 0x06, 0x31], "extractps dword [rsi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x17, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x20, 0x06, 0x31], "pinsrb xmm0, byte [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x20, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x21, 0x06, 0x31], "insertps xmm0, dword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x21, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x22, 0x06, 0x31], "pinsrd xmm0, dword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x22, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x48, 0x0f, 0x3a, 0x22, 0x06, 0x31], "pinsrq xmm0, qword [rsi], 0x31"),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x40, 0x06, 0x31], "dpps xmm0, xmmword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x40, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x41, 0x06, 0x31], "dppd xmm0, xmmword [rsi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x41, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x42, 0x06, 0x44], "mpsadbw xmm0, xmmword [rsi], 0x44"),
        testcase!(invalid: &[0x0f, 0x3a, 0x42, 0x06]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod ssse3 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x00, 0xda], "pshufb xmm3, xmm2"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x00, 0x06], "pshufb xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x00, 0x06], "pshufb mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x01, 0x06], "phaddw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x01, 0x06], "phaddw mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x02, 0x06], "phaddd xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x02, 0x06], "phaddd mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x03, 0x06], "phaddsw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x03, 0x06], "phaddsw mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x04, 0x06], "pmaddubsw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x04, 0x06], "pmaddubsw mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x05, 0x06], "phsubw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x05, 0x06], "phsubw mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x06, 0x06], "phsubd xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x06, 0x06], "phsubd mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x07, 0x06], "phsubsw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x07, 0x06], "phsubsw mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x08, 0x06], "psignb xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x08, 0x06], "psignb mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x09, 0x06], "psignw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x09, 0x06], "psignw mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x0a, 0x06], "psignd xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x0a, 0x06], "psignd mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x0b, 0x06], "pmulhrsw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x0b, 0x06], "pmulhrsw mm0, qword [rsi]"),

        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x1c, 0x06], "pabsb xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x1c, 0x06], "pabsb mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x1d, 0x06], "pabsw xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x1d, 0x06], "pabsw mm0, qword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x1e, 0x06], "pabsd xmm0, xmmword [rsi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x1e, 0x06], "pabsd mm0, qword [rsi]"),

        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0f, 0x06, 0x30], "palignr xmm0, xmmword [rsi], 0x30"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x3a, 0x0f, 0x06, 0x30], "palignr mm0, qword [rsi], 0x30"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod _0f01 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // drawn heavily from "Table A-6.  Opcode Extensions for One- and Two-byte Opcodes by Group
        // Number"
        testcase!(invalid: &[0x0f, 0x01, 0x28]),
        testcase!(invalid: &[0x0f, 0x01, 0x29]),
        testcase!(invalid: &[0x0f, 0x01, 0x2a]),
        testcase!(invalid: &[0x0f, 0x01, 0x2b]),
        testcase!(invalid: &[0x0f, 0x01, 0x2c]),
        testcase!(invalid: &[0x0f, 0x01, 0x2d]),
        testcase!(invalid: &[0x0f, 0x01, 0x2e]),
        testcase!(invalid: &[0x0f, 0x01, 0x2f]),
        testcase!(&[0x0f, 0x01, 0x38], "invlpg byte [rax]"),
        testcase!(&[0x0f, 0x01, 0x3f], "invlpg byte [rdi]"),
        testcase!(&[0x0f, 0x01, 0x40, 0xff], "sgdt mword [rax - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x41, 0xff], "sgdt mword [rcx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x49, 0xff], "sidt mword [rcx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x51, 0xff], "lgdt mword [rcx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x59, 0xff], "lidt mword [rcx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x61, 0xff], "smsw word [rcx - 0x1]"),
        testcase!(invalid: &[0x0f, 0x01, 0x69, 0xff]),
        testcase!(&[0x0f, 0x01, 0x71, 0xff], "lmsw word [rcx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x79, 0xff], "invlpg byte [rcx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0xc0], "enclv"),
        testcase!(&[0x0f, 0x01, 0xc1], "vmcall"),
        testcase!(&[0x0f, 0x01, 0xc2], "vmlaunch"),
        testcase!(&[0x0f, 0x01, 0xc3], "vmresume"),
        testcase!(&[0x0f, 0x01, 0xc4], "vmxoff"),
        testcase!(&[0x0f, 0x01, 0xc5], "pconfig"),
        testcase!(invalid: &[0x0f, 0x01, 0xc6]),
        testcase!(invalid: &[0x0f, 0x01, 0xc7]),
        testcase!(&[0x0f, 0x01, 0xc8], "monitor"),
        testcase!(&[0x0f, 0x01, 0xc9], "mwait"),
        testcase!(&[0x0f, 0x01, 0xca], "clac"),
        testcase!(&[0x0f, 0x01, 0xcb], "stac"),
        testcase!(invalid: &[0x0f, 0x01, 0xcc]),
        testcase!(invalid: &[0x0f, 0x01, 0xcd]),
        testcase!(invalid: &[0x0f, 0x01, 0xce]),
        testcase!(&[0x0f, 0x01, 0xcf], "encls"),
        testcase!(&[0x0f, 0x01, 0xd0], "xgetbv"),
        testcase!(&[0x0f, 0x01, 0xd1], "xsetbv"),
        testcase!(invalid: &[0x0f, 0x01, 0xd2]),
        testcase!(invalid: &[0x0f, 0x01, 0xd3]),
        testcase!(&[0x0f, 0x01, 0xd4], "vmfunc"),
        testcase!(&[0x0f, 0x01, 0xd5], "xend"),
        testcase!(&[0x0f, 0x01, 0xd6], "xtest"),
        testcase!(&[0x0f, 0x01, 0xd7], "enclu"),
        testcase!(&[0x0f, 0x01, 0xd8], "vmrun rax"),
        testcase!(&[0x0f, 0x01, 0xd9], "vmmcall"),
        testcase!(&[0x0f, 0x01, 0xda], "vmload rax"),
        testcase!(&[0x0f, 0x01, 0xdb], "vmsave rax"),
        testcase!(&[0x0f, 0x01, 0xdc], "stgi"),
        testcase!(&[0x0f, 0x01, 0xdd], "clgi"),
        testcase!(&[0x0f, 0x01, 0xde], "skinit eax"),
        testcase!(&[0x0f, 0x01, 0xdf], "invlpga rax, ecx"),
        testcase!(&[0x0f, 0x01, 0xfe], "invlpgb rax, edx, ecx"),
        testcase!(&[0x0f, 0x01, 0xff], "tlbsync"),
        testcase!(&[0x2e, 0x67, 0x65, 0x2e, 0x46, 0x0f, 0x01, 0xff], "tlbsync"),
        testcase!(&[0x4f, 0x0f, 0x01, 0xe0], "smsw r8"),
        testcase!(&[0x0f, 0x01, 0xe0], "smsw eax"),
        testcase!(&[0x0f, 0x01, 0xe1], "smsw ecx"),
        testcase!(&[0x0f, 0x01, 0xe2], "smsw edx"),
        testcase!(&[0x0f, 0x01, 0xe3], "smsw ebx"),
        testcase!(&[0x0f, 0x01, 0xe4], "smsw esp"),
        testcase!(&[0x0f, 0x01, 0xe5], "smsw ebp"),
        testcase!(&[0x0f, 0x01, 0xe6], "smsw esi"),
        testcase!(&[0x0f, 0x01, 0xe7], "smsw edi"),
        testcase!(invalid: &[0x0f, 0x01, 0xe8]),
        testcase!(invalid: &[0x0f, 0x01, 0xe8]),
        testcase!(invalid: &[0x0f, 0x01, 0xe9]),
        testcase!(invalid: &[0x0f, 0x01, 0xea]),
        testcase!(invalid: &[0x0f, 0x01, 0xeb]),
        testcase!(&[0x0f, 0x01, 0xee], "rdpkru"),
        testcase!(&[0x0f, 0x01, 0xef], "wrpkru"),
        testcase!(invalid: &[0xf2, 0x0f, 0x01, 0xee]),
        testcase!(invalid: &[0xf2, 0x0f, 0x01, 0xef]),
        testcase!(&[0x4f, 0x0f, 0x01, 0xf0], "lmsw r8w"),
        testcase!(&[0x0f, 0x01, 0xf0], "lmsw ax"),
        testcase!(&[0x0f, 0x01, 0xf1], "lmsw cx"),
        testcase!(&[0x0f, 0x01, 0xf2], "lmsw dx"),
        testcase!(&[0x0f, 0x01, 0xf3], "lmsw bx"),
        testcase!(&[0x0f, 0x01, 0xf4], "lmsw sp"),
        testcase!(&[0x0f, 0x01, 0xf5], "lmsw bp"),
        testcase!(&[0x0f, 0x01, 0xf6], "lmsw si"),
        testcase!(&[0x0f, 0x01, 0xf7], "lmsw di"),
        testcase!(&[0x0f, 0x01, 0xf8], "swapgs"),
        testcase!(&[0x0f, 0x01, 0xf9], "rdtscp"),
        testcase!(&[0x0f, 0x01, 0xfa], "monitorx"),
        testcase!(&[0x0f, 0x01, 0xfb], "mwaitx"),
        testcase!(&[0x0f, 0x01, 0xfc], "clzero"),
        testcase!(&[0x0f, 0x01, 0xfd], "rdpru ecx"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod _0fae {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &[TestCase] = &[
        // drawn heavily from "Table A-6.  Opcode Extensions for One- and Two-byte Opcodes by Group
        // Number"
        testcase!(invalid: &[0xf3, 0x0f, 0xae, 0x87]),
        testcase!(invalid: &[0xf3, 0x0f, 0xae, 0x04, 0x4f]),
        testcase!(&[0x0f, 0xae, 0x04, 0x4f], "fxsave ptr [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xae, 0x04, 0x4f], "fxsave ptr [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xae, 0x0c, 0x4f], "fxrstor ptr [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xae, 0x14, 0x4f], "ldmxcsr dword [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xae, 0x1c, 0x4f], "stmxcsr dword [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xae, 0x24, 0x4f], "xsave ptr [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xc7, 0x5c, 0x24, 0x40], "xrstors ptr [rsp + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x64, 0x24, 0x40], "xsavec ptr [rsp + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x6c, 0x24, 0x40], "xsaves ptr [rsp + 0x40]"),
        testcase!(&[0x4f, 0x0f, 0xc7, 0x5c, 0x24, 0x40], "xrstors64 ptr [r12 + r12 * 1 + 0x40]"),
        testcase!(&[0x4f, 0x0f, 0xc7, 0x64, 0x24, 0x40], "xsavec64 ptr [r12 + r12 * 1 + 0x40]"),
        testcase!(&[0x4f, 0x0f, 0xc7, 0x6c, 0x24, 0x40], "xsaves64 ptr [r12 + r12 * 1 + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x74, 0x24, 0x40], "vmptrld qword [rsp + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x7c, 0x24, 0x40], "vmptrst qword [rsp + 0x40]"),
        testcase!(&[0x0f, 0xae, 0x2c, 0x4f], "xrstor ptr [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xae, 0x34, 0x4f], "xsaveopt ptr [rdi + rcx * 2]"),
        testcase!(&[0x0f, 0xae, 0x3c, 0x4f], "clflush zmmword [rdi + rcx * 2]"),

        testcase!(features nodefault { Intel: true, Amd: true, Minimal: true, Default: true } &[0x0f, 0xae, 0xe8], "lfence"),
        // it turns out intel and amd accept m != 0 for {l,m,s}fence:
        // from intel:
        // ```
        // Specification of the instruction's opcode above indicates a ModR/M byte of F0. For this
        // instruction, the processor ignores the r/m field of the ModR/M byte. Thus, MFENCE is encoded
        // by any opcode of the form 0F AE Fx, where x is in the range 0-7.
        // ```
        // whereas amd does not discuss the r/m field at all. at least as of zen, amd also accepts
        // these encodings.
        //
        // similar for mfence and sfence, below.
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xe8 | 1], "lfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xe8 | 2], "lfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xe8 | 3], "lfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xe8 | 4], "lfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xe8 | 5], "lfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xe8 | 6], "lfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xe8 | 7], "lfence"),

        testcase!(features nodefault { Intel: true, Amd: true, Minimal: true, Default: true } &[0x0f, 0xae, 0xf0], "mfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf0 | 1], "mfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf0 | 2], "mfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf0 | 3], "mfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf0 | 4], "mfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf0 | 5], "mfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf0 | 6], "mfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf0 | 7], "mfence"),

        testcase!(features nodefault { Intel: true, Amd: true, Minimal: true, Default: true } &[0x0f, 0xae, 0xf8], "sfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf8 | 1], "sfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf8 | 2], "sfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf8 | 3], "sfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf8 | 4], "sfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf8 | 5], "sfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf8 | 6], "sfence"),
        testcase!(features { Intel: true, Amd: true } &[0x0f, 0xae, 0xf8 | 7], "sfence"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod system {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x4f, 0x0f, 0xb2, 0x00], "lss r8, mword [r8]"),
        testcase!(&[0x67, 0x4f, 0x0f, 0xb2, 0x00], "lss r8, mword [r8d]"),
        testcase!(&[0x4f, 0x0f, 0xb2, 0x00], "lss r8, mword [r8]"),
        testcase!(&[0x0f, 0xb2, 0x00], "lss eax, far [rax]"),
        testcase!(&[0x66, 0x0f, 0xb2, 0x00], "lss ax, dword [rax]"),
        testcase!(invalid: &[0x45, 0x0f, 0x22, 0xc8]),
        testcase!(invalid: &[0x45, 0x0f, 0x20, 0xc8]),
        testcase!(&[0x40, 0x0f, 0x22, 0xd0], "mov cr2, rax"),
        testcase!(&[0x0f, 0x22, 0xd0], "mov cr2, rax"),
        testcase!(invalid: &[0x44, 0x0f, 0x22, 0xcf]),
        testcase!(&[0x0f, 0x22, 0xd7], "mov cr2, rdi"),
        testcase!(&[0x0f, 0x20, 0xd0], "mov rax, cr2"),

        testcase!(invalid: &[0x45, 0x0f, 0x23, 0xc8]),
        testcase!(invalid: &[0x45, 0x0f, 0x21, 0xc8]),
        testcase!(&[0x40, 0x0f, 0x23, 0xc8], "mov dr1, rax"),
        testcase!(&[0x0f, 0x23, 0xc8], "mov dr1, rax"),
        testcase!(&[0x0f, 0x21, 0xc8], "mov rax, dr1"),
        testcase!(&[0x0f, 0x06], "clts"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod arithmetic {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x81, 0xec, 0x10, 0x03, 0x00, 0x00], "sub esp, 0x310"),
        testcase!(&[0x0f, 0xaf, 0xc2], "imul eax, edx"),
        testcase!(&[0x4b, 0x69, 0x43, 0x6f, 0x6d, 0x70, 0x6c, 0x65], "imul rax, qword [r11 + 0x6f], 0x656c706d"),
        testcase!(&[0x66, 0x0f, 0xaf, 0xd1], "imul dx, cx"),
        testcase!(&[0xf6, 0xe8], "imul al"),
        testcase!(&[0xf6, 0x28], "imul byte [rax]"),
        testcase!(&[0x4b, 0x6b, 0x43, 0x6f, 0x6d], "imul rax, qword [r11 + 0x6f], 0x6d"),
        testcase!(&[0x4f, 0x4e, 0x00, 0xcc], "add spl, r9b"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

#[allow(non_snake_case)]
mod E_decode {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xff, 0x75, 0xb8], "push qword [rbp - 0x48]"),
        testcase!(&[0xff, 0x75, 0x08], "push qword [rbp + 0x8]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sse {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x10, 0x0c, 0xc7], "movss xmm1, dword [rdi + rax * 8]"),
        testcase!(&[0xf3, 0x0f, 0x11, 0x0c, 0xc7], "movss dword [rdi + rax * 8], xmm1"),
        testcase!(&[0x4f, 0x0f, 0x28, 0x00], "movaps xmm8, xmmword [r8]"),
        testcase!(&[0x4f, 0x0f, 0x29, 0x00], "movaps xmmword [r8], xmm8"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2a, 0xc1], "cvtsi2ss xmm8, r9"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2a, 0x01], "cvtsi2ss xmm8, qword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x2b, 0x00], "movntps xmmword [r8], xmm8"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2c, 0xc1], "cvttss2si r8, xmm9"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2c, 0x01], "cvttss2si r8, dword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2d, 0xc1], "cvtss2si r8, xmm9"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x2d, 0x01], "cvtss2si r8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x2e, 0x00], "ucomiss xmm8, dword [r8]"),
        testcase!(&[0x4f, 0x0f, 0x2f, 0x00], "comiss xmm8, dword [r8]"),
        testcase!(&[0x0f, 0x28, 0xd0], "movaps xmm2, xmm0"),
        testcase!(&[0x66, 0x0f, 0x28, 0xd0], "movapd xmm2, xmm0"),
        testcase!(&[0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [rax]"),
        testcase!(&[0x4f, 0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [rax]"),
        testcase!(&[0x66, 0x4f, 0x0f, 0x28, 0x00], "movapd xmm8, xmmword [r8]"),
        testcase!(&[0x66, 0x4f, 0x0f, 0x28, 0x00], "movapd xmm8, xmmword [r8]"),
        testcase!(&[0x67, 0x4f, 0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [eax]"),
        testcase!(&[0x67, 0x66, 0x4f, 0x0f, 0x28, 0x00], "movapd xmm8, xmmword [r8d]"),
        testcase!(&[0x66, 0x0f, 0x29, 0x00], "movapd xmmword [rax], xmm0"),
        testcase!(invalid: &[0x4f, 0x0f, 0x50, 0x00]),
        testcase!(&[0x4f, 0x0f, 0x50, 0xc1], "movmskps r8d, xmm9"),
        testcase!(&[0x4f, 0x0f, 0x51, 0x01], "sqrtps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x51, 0x01], "sqrtss xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x52, 0x01], "rsqrtps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x52, 0x01], "rsqrtss xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x53, 0x01], "rcpps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x53, 0x01], "rcpss xmm8, dword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x53, 0xc1], "rcpss xmm8, xmm9"),
        testcase!(&[0x4f, 0x0f, 0x54, 0x01], "andps xmm8, xmmword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x55, 0x01], "andnps xmm8, xmmword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x56, 0x01], "orps xmm8, xmmword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x57, 0x01], "xorps xmm8, xmmword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x58, 0x01], "addps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x58, 0x01], "addss xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x59, 0x01], "mulps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x59, 0x01], "mulss xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x5a, 0x01], "cvtps2pd xmm8, qword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x5a, 0x01], "cvtss2sd xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x5b, 0x01], "cvtdq2ps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x5b, 0x01], "cvttps2dq xmm8, xmmword [r9]"),
        testcase!(&[0x67, 0x4f, 0x0f, 0x5b, 0x01], "cvtdq2ps xmm8, xmmword [r9d]"),
        testcase!(&[0x4f, 0x0f, 0x5c, 0x01], "subps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x5c, 0x01], "subss xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x5d, 0x01], "minps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x5d, 0x01], "minss xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x5e, 0x01], "divps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x5e, 0x01], "divss xmm8, dword [r9]"),
        testcase!(&[0x4f, 0x0f, 0x5f, 0x01], "maxps xmm8, xmmword [r9]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x5f, 0x01], "maxss xmm8, dword [r9]"),

        testcase!(&[0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [rax], 0x7f"),
        testcase!(&[0x4f, 0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [r8], 0x7f"),

        testcase!(&[0x66, 0x0f, 0xef, 0xc0], "pxor xmm0, xmm0"),
        testcase!(&[0x66, 0x4f, 0x0f, 0xef, 0xc0], "pxor xmm8, xmm8"),
        testcase!(&[0xf2, 0x0f, 0x10, 0x0c, 0xc6], "movsd xmm1, qword [rsi + rax * 8]"),
        testcase!(&[0xf3, 0x0f, 0x10, 0x04, 0x86], "movss xmm0, dword [rsi + rax * 4]"),
        testcase!(&[0xf2, 0x0f, 0x59, 0xc8], "mulsd xmm1, xmm0"),
        testcase!(&[0xf3, 0x0f, 0x59, 0xc8], "mulss xmm1, xmm0"),
        testcase!(&[0xf2, 0x4f, 0x0f, 0x59, 0xc8], "mulsd xmm9, xmm8"),

        testcase!(
            &[0xf3, 0x4f, 0x0f, 0x6f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqu xmm11, xmmword [r12 + r11 * 4 - 0x334455cc]"
        ),
        testcase!(&[0xf3, 0x0f, 0x70, 0xc0, 0x4e], "pshufhw xmm0, xmm0, 0x4e"),
        testcase!(&[0xf3, 0x0f, 0x7e, 0xc1], "movq xmm0, xmm1"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x7e, 0xc1], "movq xmm8, xmm9"),
        testcase!(&[0xf3, 0x40, 0x0f, 0x7e, 0xc1], "movq xmm0, xmm1"),
        testcase!(&[0xf3, 0x41, 0x0f, 0x7e, 0xc1], "movq xmm0, xmm9"),
        testcase!(&[0xf3, 0x42, 0x0f, 0x7e, 0xc1], "movq xmm0, xmm1"),
        testcase!(&[0xf3, 0x44, 0x0f, 0x7e, 0xc1], "movq xmm8, xmm1"),
        testcase!(&[0xf3, 0x48, 0x0f, 0x7e, 0xc1], "movq xmm0, xmm1"),
        testcase!(
            &[0xf3, 0x4f, 0x0f, 0x7f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqu xmmword [r12 + r11 * 4 - 0x334455cc], xmm11"
        ),

        testcase!(&[0xf3, 0x0f, 0xc2, 0xc3, 0x08], "cmpss xmm0, xmm3, 0x8"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xc2, 0xc3, 0x08], "cmpss xmm8, xmm11, 0x8"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xc2, 0x03, 0x08], "cmpss xmm8, dword [r11], 0x8"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

// SETLE, SETNG, ...

mod mov {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xa0, 0x93, 0x62, 0xc4, 0x00, 0x12, 0x34, 0x12, 0x34], "mov al, byte [0x3412341200c46293]"),
        testcase!(&[0x67, 0xa0, 0x93, 0x62, 0xc4, 0x00], "mov al, byte [0xc46293]"),
        testcase!(&[0xa1, 0x93, 0x62, 0xc4, 0x00, 0x12, 0x34, 0x12, 0x34], "mov eax, dword [0x3412341200c46293]"),
        testcase!(&[0x67, 0xa1, 0x93, 0x62, 0xc4, 0x00], "mov eax, dword [0xc46293]"),
        testcase!(&[0xa2, 0x93, 0x62, 0xc4, 0x00, 0x12, 0x34, 0x12, 0x34], "mov byte [0x3412341200c46293], al"),
        testcase!(&[0x67, 0xa2, 0x93, 0x62, 0xc4, 0x00], "mov byte [0xc46293], al"),
        testcase!(&[0xa3, 0x93, 0x62, 0xc4, 0x00, 0x12, 0x34, 0x12, 0x34], "mov dword [0x3412341200c46293], eax"),
        testcase!(&[0x67, 0xa3, 0x93, 0x62, 0xc4, 0x00], "mov dword [0xc46293], eax"),
        testcase!(&[0xba, 0x01, 0x00, 0x00, 0x00], "mov edx, 0x1"),
        testcase!(&[0x48, 0xc7, 0x04, 0x24, 0x00, 0x00, 0x00, 0x00], "mov qword [rsp], 0x0"),
        testcase!(&[0x48, 0x89, 0x44, 0x24, 0x08], "mov qword [rsp + 0x8], rax"),
        testcase!(&[0x48, 0x89, 0x43, 0x18], "mov qword [rbx + 0x18], rax"),
        testcase!(&[0x48, 0xc7, 0x43, 0x10, 0x00, 0x00, 0x00, 0x00], "mov qword [rbx + 0x10], 0x0"),
        testcase!(&[0x49, 0x89, 0x4e, 0x08], "mov qword [r14 + 0x8], rcx"),
        testcase!(&[0x48, 0x8b, 0x32], "mov rsi, qword [rdx]"),
        testcase!(&[0x4d, 0x8b, 0x4c, 0x10, 0xf8], "mov r9, qword [r8 + rdx * 1 - 0x8]"),
        testcase!(&[0x49, 0x89, 0x46, 0x10], "mov qword [r14 + 0x10], rax"),
        testcase!(&[0x4d, 0x0f, 0x43, 0xec], "cmovnb r13, r12"),
        testcase!(&[0x0f, 0xb6, 0x06], "movzx eax, byte [rsi]"),
        testcase!(&[0x0f, 0xb7, 0x06], "movzx eax, word [rsi]"),
        testcase!(&[0x89, 0x55, 0x94], "mov dword [rbp - 0x6c], edx"),
        testcase!(&[0x65, 0x4c, 0x89, 0x04, 0x25, 0xa8, 0x01, 0x00, 0x00], "mov qword gs:[0x1a8], r8"),
        testcase!(&[0x0f, 0xbe, 0x83, 0xb4, 0x00, 0x00, 0x00], "movsx eax, byte [rbx + 0xb4]"),
        testcase!(&[0x46, 0x63, 0xc1], "movsxd r8, ecx"),
        testcase!(&[0x48, 0x63, 0x04, 0xba], "movsxd rax, dword [rdx + rdi * 4]"),
        testcase!(&[0xf3, 0x0f, 0x6f, 0x07], "movdqu xmm0, xmmword [rdi]"),
        testcase!(&[0xf3, 0x0f, 0x7f, 0x45, 0x00], "movdqu xmmword [rbp], xmm0"),

        testcase!(&[0x0f, 0x97, 0xc0], "seta al"),
        testcase!(&[0x0f, 0x97, 0xc8], "seta al"),
        testcase!(&[0x0f, 0x97, 0x00], "seta byte [rax]"),
        testcase!(&[0x0f, 0x97, 0x08], "seta byte [rax]"),
    //    testcase!(&[0xd6], "salc"),
        testcase!(&[0x8e, 0x00], "mov es, word [rax]"),
        testcase!(&[0x8e, 0xc0], "mov es, ax"),
        testcase!(&[0x8c, 0xc0], "mov eax, es"),
        // cs is not an allowed destination
        testcase!(invalid: &[0x8e, 0x08]),
        testcase!(&[0x8e, 0x10], "mov ss, word [rax]"),
        testcase!(&[0x8e, 0xd0], "mov ss, ax"),
        testcase!(&[0x8c, 0xd0], "mov eax, ss"),
        testcase!(&[0x8e, 0x18], "mov ds, word [rax]"),
        testcase!(&[0x8e, 0xd8], "mov ds, ax"),
        testcase!(&[0x8c, 0xd8], "mov eax, ds"),
        testcase!(&[0x8e, 0x20], "mov fs, word [rax]"),
        testcase!(&[0x8e, 0xe0], "mov fs, ax"),
        testcase!(&[0x8c, 0xe0], "mov eax, fs"),
        testcase!(&[0x8e, 0x28], "mov gs, word [rax]"),
        testcase!(&[0x8e, 0xe8], "mov gs, ax"),
        testcase!(&[0x8c, 0xe8], "mov eax, gs"),
        testcase!(invalid: &[0x8e, 0x30]),
        testcase!(invalid: &[0x8e, 0x38]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod xchg {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x90], "nop"),
        testcase!(&[0x91], "xchg eax, ecx"),
        testcase!(&[0x4f, 0x91], "xchg rax, r9"),
        testcase!(&[0x66, 0x91], "xchg ax, cx"),
        testcase!(&[0x4f, 0x90], "xchg rax, r8"),
        testcase!(&[0x41, 0x90], "xchg eax, r8d"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod stack {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x41, 0x50], "push r8w"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod prefixes {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x41, 0x31, 0xc0], "xor r8w, ax"),
        testcase!(&[0x66, 0x41, 0x32, 0xc0], "xor al, r8b"),
        testcase!(&[0x40, 0x32, 0xc5], "xor al, bpl"),
        testcase!(invalid: &[0xf0, 0x33, 0xc0]),
        testcase!(&[0xf0, 0x31, 0x00], "lock xor dword [rax], eax"),
        testcase!(&[0xf0, 0x80, 0x30, 0x00], "lock xor byte [rax], 0x0"),
        testcase!(&[0xf0, 0x0f, 0xbb, 0x17], "lock btc dword [rdi], edx"),
        testcase!(&[0x66, 0x2e, 0xf2, 0xf0, 0x0f, 0xbb, 0x13], "xacquire lock btc word [rbx], dx"),
        testcase!(invalid: &[0xf0, 0xc7, 0x00, 0x00, 0x00, 0x00]),
        testcase!(&[0x0f, 0xc1, 0xcc], "xadd esp, ecx"),
        testcase!(&[0x66, 0x0f, 0xc1, 0xcc], "xadd sp, cx"),
        testcase!(&[0xf2, 0x0f, 0xc1, 0xcc], "xadd esp, ecx"),
        testcase!(&[0xf3, 0x0f, 0xc1, 0xcc], "xadd esp, ecx"),
        testcase!(&[0x0f, 0xc0, 0xcc], "xadd ah, cl"),
        testcase!(&[0x66, 0x0f, 0xc0, 0xcc], "xadd ah, cl"),
        testcase!(&[0xf2, 0x0f, 0xc0, 0xcc], "xadd ah, cl"),
        testcase!(&[0xf3, 0x0f, 0xc0, 0xcc], "xadd ah, cl"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod control_flow {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x73, 0x31], "jnb $+0x31"),
        testcase!(&[0x72, 0x5a], "jb $+0x5a"),
        testcase!(&[0x72, 0xf0], "jb $-0x10"),
        testcase!(&[0xe8, 0x01, 0x00, 0x00, 0x00], "call $+0x1"),
        testcase!(&[0xe8, 0x80, 0x00, 0x00, 0x00], "call $+0x80", masm: "call near ptr $+85h"),
        testcase!(&[0xe8, 0xff, 0xff, 0xff, 0xff], "call $-0x1"),
        testcase!(&[0xe9, 0x01, 0x00, 0x00, 0x00], "jmp $+0x1"),
        testcase!(&[0xe9, 0x80, 0x00, 0x00, 0x00], "jmp $+0x80", masm: "jmp near ptr $+85h"),
        testcase!(&[0xe9, 0xff, 0xff, 0xff, 0xff], "jmp $-0x1"),
        testcase!(&[0x0f, 0x86, 0x8b, 0x01, 0x00, 0x00], "jna $+0x18b"),
        testcase!(&[0x0f, 0x85, 0x3b, 0x25, 0x00, 0x00], "jnz $+0x253b"),
        testcase!(&[0x74, 0x47], "jz $+0x47"),
        testcase!(&[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00], "call qword [rip + 0x24727e]", masm: "call qword ptr [$ + 24727Eh]"),
        testcase!(&[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00], "call qword [rip + 0x24727e]", masm: "call qword ptr [$ + 24727Eh]"),
        testcase!(&[0xff, 0x24, 0xcd, 0x70, 0xa0, 0xbc, 0x01], "jmp qword [rcx * 8 + 0x1bca070]"),
        testcase!(&[0xff, 0x14, 0xcd, 0x70, 0xa0, 0xbc, 0x01], "call qword [rcx * 8 + 0x1bca070]"),
        testcase!(&[0xff, 0xe0], "jmp rax"),
        testcase!(&[0xff, 0xd0], "call rax"),
        testcase!(&[0x66, 0xff, 0xe0], "jmp rax"),
        testcase!(&[0x67, 0xff, 0xe0], "jmp rax"),
        testcase!(&[0x66, 0xff, 0xd0], "call rax"),
        testcase!(&[0x67, 0xff, 0xd0], "call rax"),
        testcase!(invalid: &[0xff, 0xd8]),
        testcase!(&[0xff, 0x18], "callf mword [rax]", masm: "call fword ptr [rax]"),
        testcase!(&[0xe0, 0x12], "loopnz $+0x12"),
        testcase!(&[0xe1, 0x12], "loopz $+0x12"),
        testcase!(&[0xe2, 0x12], "loop $+0x12"),
        testcase!(&[0xe3, 0x12], "jrcxz $+0x12"),
        testcase!(&[0xe3, 0xf0], "jrcxz $-0x10"),
        testcase!(&[0x67, 0xe3, 0x12], "jecxz $+0x12"),
        testcase!(&[0x67, 0xe3, 0xf0], "jecxz $-0x10"),
        testcase!(&[0xc3], "ret"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod bad_instructions {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // too long
        testcase!(invalid: &[
             0x2e, 0x2e, 0x2e, 0x2e,
             0x2e, 0x2e, 0x2e, 0x2e,
             0x2e, 0x2e, 0x2e, 0x2e,
             0x2e, 0x2e, 0x2e, 0x2e,
             0x33, 0xc0,
        ]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod test_cmp {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf6, 0x05, 0x2c, 0x9b, 0xff, 0xff, 0x01], "test byte [rip - 0x64d4], 0x1"),
        testcase!(&[0x48, 0x3d, 0x01, 0xf0, 0xff, 0xff], "cmp rax, -0xfff"),
        testcase!(&[0x3d, 0x01, 0xf0, 0xff, 0xff], "cmp eax, -0xfff"),
        testcase!(&[0x48, 0x83, 0xf8, 0xff], "cmp rax, -0x1"),
        testcase!(&[0x48, 0x39, 0xc6], "cmp rsi, rax"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod push_pop {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x5b], "pop rbx"),
        testcase!(&[0x41, 0x5e], "pop r14"),
        testcase!(&[0x68, 0x7f, 0x63, 0xc4, 0x00],
            "push 0xc4637f",
            masm: "push 0C4637Fh"),
        testcase!(&[0x66, 0x8f, 0x00],
            "pop word [rax]",
            masm: "pop word ptr [rax]"),
        testcase!(&[0x8f, 0x00],
            "pop qword [rax]",
            masm: "pop qword ptr [rax]"),
        testcase!(&[0x48, 0x8f, 0x00],
            "pop qword [rax]",
            masm: "pop qword ptr [rax]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod bmi1 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // 0fbc changes from bsf to tzcnt when BMI1 is present, so we have to be a bit careful
        // about the expected decodes..
        testcase!(features nodefault { BMI1: true } &[0xf3, 0x41, 0x0f, 0xbc, 0xd3], "tzcnt edx, r11d"),
        testcase!(features nodefault { Minimal: true } &[0xf3, 0x41, 0x0f, 0xbc, 0xd3], "bsf edx, r11d"),

        testcase!(features nodefault { Minimal: true } &[0xf2, 0x41, 0x0f, 0xbc, 0xd3], "bsf edx, r11d"),
        testcase!(features nodefault { BMI1: true } &[0x41, 0x0f, 0xbc, 0xd3], "bsf edx, r11d"),

        // just 0f38
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0x60, 0xf2, 0x01], "andn eax, ebx, dword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0xe0, 0xf2, 0x01], "andn rax, rbx, qword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0x78, 0xf3, 0x09], "blsr eax, dword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0xf8, 0xf3, 0x09], "blsr rax, qword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0x78, 0xf3, 0x11], "blsmsk eax, dword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0xf8, 0xf3, 0x11], "blsmsk rax, qword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0x78, 0xf3, 0x19], "blsi eax, dword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0xf8, 0xf3, 0x19], "blsi rax, qword [r9]"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0x60, 0xf7, 0x01], "bextr eax, dword [r9], ebx"),
        testcase!(features { BMI1: true } &[0xc4, 0xc2, 0xe0, 0xf7, 0x01], "bextr rax, qword [r9], rbx"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod bmi2 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // f2 0f3a
        testcase!(features { BMI2: true } &[0xc4, 0xc3, 0x7b, 0xf0, 0x01, 0x05], "rorx eax, dword [r9], 0x5"),
        testcase!(features { BMI2: true } &[0xc4, 0xc3, 0xfb, 0xf0, 0x01, 0x05], "rorx rax, qword [r9], 0x5"),

        // f2 0f38 map
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x63, 0xf5, 0x07], "pdep eax, ebx, dword [rdi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe3, 0xf5, 0x07], "pdep rax, rbx, qword [rdi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x63, 0xf6, 0x07], "mulx eax, ebx, dword [rdi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe3, 0xf6, 0x07], "mulx rax, rbx, qword [rdi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xc2, 0x63, 0xf7, 0x01], "shrx eax, dword [r9], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xc2, 0xe3, 0xf7, 0x01], "shrx rax, qword [r9], rbx"),

        // f3 0f38 map
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x62, 0xf5, 0x07], "pext eax, ebx, dword [rdi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe2, 0xf5, 0x07], "pext rax, rbx, qword [rdi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xc2, 0x62, 0xf7, 0x01], "sarx eax, dword [r9], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xc2, 0xe2, 0xf7, 0x01], "sarx rax, qword [r9], rbx"),

        // just 0f38
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x60, 0xf5, 0x07], "bzhi eax, dword [rdi], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe0, 0xf5, 0x07], "bzhi rax, qword [rdi], rbx"),

        // 66 0f38
        testcase!(features { BMI2: true } &[0xc4, 0xc2, 0x61, 0xf7, 0x01], "shlx eax, dword [r9], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xc2, 0xe1, 0xf7, 0x01], "shlx rax, qword [r9], rbx"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod popcnt {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { Popcnt: true } &[0xf3, 0x0f, 0xb8, 0xc1], "popcnt eax, ecx"),
        testcase!(features { SSE4_2_Intel: true } &[0xf3, 0x0f, 0xb8, 0xc1], "popcnt eax, ecx"),
        testcase!(features { Popcnt: true } &[0xf3, 0x4f, 0x0f, 0xb8, 0xc1], "popcnt r8, r9"),
        testcase!(features { SSE4_2_Intel: true } &[0xf3, 0x4f, 0x0f, 0xb8, 0xc1], "popcnt r8, r9"),
        testcase!(invalid: features nodefault { Minimal: false } &[0xf3, 0x4f, 0x0f, 0xb8, 0xc1]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod bitwise {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features nodefault { Minimal: true } &[0x41, 0x0f, 0xbc, 0xd3], "bsf edx, r11d"),
        testcase!(features nodefault { Minimal: true } &[0x0f, 0xbb, 0x17], "btc dword [rdi], edx"),
        testcase!(features nodefault { Minimal: true } &[0xf0, 0x0f, 0xbb, 0x17], "lock btc dword [rdi], edx"),
        testcase!(features nodefault { Minimal: true } &[0xf0, 0x0f, 0xbb, 0x17], "lock btc dword [rdi], edx"),
        testcase!(&[0x48, 0x0f, 0xa3, 0xd0], "bt rax, rdx"),
        testcase!(&[0x48, 0x0f, 0xab, 0xd0], "bts rax, rdx"),
        testcase!(&[0x48, 0x0f, 0xb3, 0xd0], "btr rax, rdx"),
        testcase!(&[0x0f, 0xb3, 0xd0], "btr eax, edx"),
        testcase!(&[0x66, 0x41, 0x0f, 0xb3, 0xc0], "btr r8w, ax"),
        testcase!(&[0xd2, 0xe0], "shl al, cl"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod misc {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf1], "int 0x1"),
        testcase!(&[0xf5], "cmc"),
        testcase!(&[0xc8, 0x01, 0x02, 0x03], "enter 0x201, 0x3"),
        testcase!(&[0xc9], "leave"),
        testcase!(&[0xca, 0x12, 0x34], "retf 0x3412"),
        testcase!(&[0xcb], "retf"),
        testcase!(&[0x66, 0xcf], "iret"),
        testcase!(&[0xcf], "iretd"),
        testcase!(&[0x48, 0xcf], "iretq"),
        testcase!(&[0x66, 0x4f, 0xcf], "iretq"),
        testcase!(&[0xf2, 0x0f, 0x38, 0xf0, 0xc1], "crc32 eax, cl"),
        testcase!(&[0xf2, 0x0f, 0x38, 0xf1, 0xc1], "crc32 eax, ecx"),
        testcase!(&[0xfe, 0x00], "inc byte [rax]"),
        testcase!(&[0xfe, 0x08], "dec byte [rax]"),
        testcase!(&[0xff, 0x00], "inc dword [rax]"),
        testcase!(&[0x48, 0xff, 0x00], "inc qword [rax]"),
        testcase!(&[0xe4, 0x99], "in al, 0x99"),
        testcase!(&[0xe5, 0x99], "in eax, 0x99"),
        testcase!(&[0x67, 0xe5, 0x99], "in eax, 0x99"),
        // dumpbin prints this as "in rax, 99h", but this is wrong! MASM also accepts "in rax, 99h" to produce these bytes, which is wrong too.
        // this mismatch is special-cased in the MASM comparative harness.
        testcase!(&[0x4f, 0xe5, 0x99], "in eax, 0x99", masm: "in eax, 99h"),
        testcase!(&[0xe6, 0x99], "out 0x99, al"),
        // dumpbin prints this as "out 99h, rax", but this is wrong! MASM also accepts "out 99h, rax" to produce these bytes, which is wrong too.
        // this mismatch is special-cased in the MASM comparative harness.
        testcase!(&[0x4f, 0xe7, 0x99], "out 0x99, eax", masm: "out 99h, eax"),
        testcase!(&[0xec], "in al, dx"),
        testcase!(&[0xed], "in eax, dx"),
        testcase!(&[0xee], "out dx, al"),
        testcase!(&[0xef], "out dx, eax"),
        testcase!(&[0xcd, 0x00], "int 0x0"),
        testcase!(&[0xcd, 0xff], "int 0xff"),
        testcase!(&[0x9c], "pushf"),
        testcase!(&[0x48, 0x98], "cdqe"),
        testcase!(&[0x98], "cwde"),
        testcase!(&[0x66, 0x99], "cwd"),
        testcase!(&[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00], "nop word [rax + rax * 1]"),
        testcase!(&[0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00], "nop word [rax + rax * 1]"),
        testcase!(&[0x48, 0x8d, 0xa4, 0xc7, 0x20, 0x00, 0x00, 0x12], "lea rsp, qword [rdi + rax * 8 + 0x12000020]"),
        testcase!(&[0x33, 0xc0], "xor eax, eax"),
        testcase!(&[0x48, 0x8d, 0x53, 0x08], "lea rdx, qword [rbx + 0x8]"),
        testcase!(invalid: &[0x8d, 0xdd]),
        testcase!(&[0x31, 0xc9], "xor ecx, ecx"),
        testcase!(&[0x48, 0x29, 0xc8], "sub rax, rcx"),
        testcase!(&[0x48, 0x03, 0x0b], "add rcx, qword [rbx]"),
        testcase!(&[0x48, 0x8d, 0x0c, 0x12], "lea rcx, qword [rdx + rdx * 1]"),
        testcase!(&[0xf6, 0xc2, 0x18], "test dl, 0x18"),
        testcase!(&[0x41, 0xf6, 0xc7, 0x03], "test r15b, 0x3"),
        testcase!(&[0xf3, 0x48, 0xab], "rep stos qword es:[rdi], rax"),
        testcase!(&[0xf3, 0x48, 0xa5], "rep movs qword es:[rdi], qword ds:[rsi]"),
        testcase!(&[0xf3, 0x45, 0x0f, 0xbc, 0xd7], "tzcnt r10d, r15d"),

        testcase!(&[0xf3, 0x0f, 0xae, 0x26], "ptwrite dword [rsi]"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xe6], "ptwrite esi"),
        testcase!(invalid: &[0x66, 0xf3, 0x0f, 0xae, 0xe6]),
        testcase!(&[0xf3, 0x49, 0x0f, 0xae, 0x26], "ptwrite qword [r14]"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xc4], "rdfsbase esp"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xae, 0xc4], "rdfsbase r12"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xcc], "rdgsbase esp"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xae, 0xcc], "rdgsbase r12"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xd4], "wrfsbase esp"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xae, 0xd4], "wrfsbase r12"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xdc], "wrgsbase esp"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xae, 0xdc], "wrgsbase r12"),
        testcase!(&[0x66, 0x0f, 0xae, 0x3f], "clflushopt zmmword [rdi]"),
        testcase!(&[0x0f, 0xae, 0x3f], "clflush zmmword [rdi]"),
        testcase!(invalid: &[0x66, 0x0f, 0xae, 0xff]),
        testcase!(&[0x66, 0x0f, 0xae, 0x37], "clwb zmmword [rdi]"),
        testcase!(&[0x66, 0x0f, 0xae, 0xf7], "tpause edi"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xf1], "umonitor rcx"),
        testcase!(&[0xf2, 0x0f, 0xae, 0xf1], "umwait ecx"),
        testcase!(&[0xf2, 0x4f, 0x0f, 0xae, 0xf1], "umwait r9d"),
        testcase!(&[0x66, 0x0f, 0x38, 0x80, 0x2f], "invept rbp, xmmword [rdi]"),
        testcase!(&[0x66, 0x49, 0x0f, 0x38, 0x80, 0x2f], "invept rbp, xmmword [r15]"),
        testcase!(invalid: &[0x0f, 0x38, 0x80, 0x2f]),
        testcase!(invalid: &[0x43, 0x0f, 0x38, 0x80, 0x2f]),
        testcase!(&[0x66, 0x0f, 0x38, 0x81, 0x2f], "invvpid rbp, xmmword [rdi]"),
        testcase!(&[0x66, 0x49, 0x0f, 0x38, 0x81, 0x2f], "invvpid rbp, xmmword [r15]"),
        testcase!(invalid: &[0x0f, 0x38, 0x81, 0x2f]),
        testcase!(invalid: &[0x43, 0x0f, 0x38, 0x81, 0x2f]),
        testcase!(&[0x66, 0x0f, 0x38, 0x82, 0x2f], "invpcid rbp, xmmword [rdi]"),
        testcase!(&[0x66, 0x49, 0x0f, 0x38, 0x82, 0x2f], "invpcid rbp, xmmword [r15]"),
        testcase!(invalid: &[0x0f, 0x38, 0x82, 0x2f]),
        testcase!(invalid: &[0x43, 0x0f, 0x38, 0x82, 0x2f]),
        testcase!(&[0x66, 0x0f, 0xae, 0xf1], "tpause ecx"),
        testcase!(&[0x66, 0x4f, 0x0f, 0xae, 0xf1], "tpause r9d"),
        testcase!(&[0xc4, 0b000_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm10, ymm9, 0x77"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod evex {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x62, 0xe1, 0x7c, 0x00, 0x14, 0x0a], "vunpcklps xmm17, xmm16, xmmword [rdx]"),
        testcase!(&[0x62, 0xe1, 0x7c, 0x08, 0x14, 0x0a], "vunpcklps xmm17, xmm0, xmmword [rdx]"),
        testcase!(&[0x62, 0x01, 0x7c, 0x00, 0x14, 0xca], "vunpcklps xmm25, xmm16, xmm26"),
        testcase!(&[0x62, 0xf2, 0x7d, 0x48, 0x2a, 0x44, 0x40, 0x01], "vmovntdqa zmm0, zmmword [rax + rax * 2 + 0x40]"),
        testcase!(&[0x62, 0xf2, 0x7d, 0x08, 0x2a, 0x44, 0x40, 0x01], "vmovntdqa xmm0, xmmword [rax + rax * 2 + 0x10]"),
        testcase!(invalid: &[0x62, 0xf2, 0x7d, 0x1d, 0x66, 0x50, 0x01, 0x11]),

        // vpbroadcastmw2d. similar to `vpmovm2*`, out-of-range `k` are just masked down.
        testcase!(&[0x62, 0xd2, 0x7e, 0x28, 0x3a, 0xca], "vpbroadcastmw2d ymm1, k2"),
        // vpmovm2b (and larger forms). for some reason the source operand is a mask register but uses
        // modrm bits as a register selector. out-of-range `k` seem to just get masked down..
        testcase!(&[0x62, 0xd2, 0x7e, 0x08, 0x28, 0xc2], "vpmovm2b xmm0, k2"),
        testcase!(&[0x62, 0xf2, 0x7e, 0x08, 0x28, 0xc1], "vpmovm2b xmm0, k1"),
        // vpmovb2m (and larger forms). out-of-range `k` are invalid in 64-bit mode, are part of the
        // `bound` instruction for 32- and 16-bit modes.
        testcase!(invalid: &[0x62, 0x72, 0x7e, 0x28, 0x29, 0xfd]),
        testcase!(&[0x62, 0xf2, 0x7e, 0x28, 0x29, 0xfd], "vpmovb2m k7, ymm5"),

        testcase!(&[0x62, 0x12, 0x7d, 0x06, 0xa0, 0x04, 0x2f], "vpscatterdd dword [r15 + xmm29 * 1], k6, xmm8"),
        testcase!(&[0x62, 0x12, 0x7d, 0x06, 0xa0, 0x14, 0x0f], "vpscatterdd dword [r15 + xmm25 * 1], k6, xmm10"),
        testcase!(&[0x62, 0x12, 0x7d, 0x26, 0xa0, 0x14, 0x0f], "vpscatterdd dword [r15 + ymm25 * 1], k6, ymm10"),
        testcase!(&[0x62, 0x12, 0x7d, 0x46, 0xa0, 0x14, 0x0f], "vpscatterdd dword [r15 + zmm25 * 1], k6, zmm10"),
        testcase!(&[0x62, 0x12, 0xfd, 0x46, 0xa0, 0x14, 0x0f], "vpscatterdq qword [r15 + zmm25 * 1], k6, zmm10"),
        testcase!(&[0x62, 0x12, 0x7d, 0x46, 0xa1, 0x14, 0x0f], "vpscatterqd dword [r15 + zmm25 * 1], k6, zmm10"),
        testcase!(&[0x62, 0x12, 0xfd, 0x46, 0xa1, 0x14, 0x0f], "vpscatterqq qword [r15 + zmm25 * 1], k6, zmm10"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod vex {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &[TestCase] = &[
        // prefix 03
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_001, 0x00, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x00, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b1_1111_101, 0x00, 0b11_001_010, 0x77], "vpermq ymm9, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_001, 0x01, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x01, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b1_1111_101, 0x01, 0b11_001_010, 0x77], "vpermpd ymm9, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_001, 0x02, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x02, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x02, 0b11_001_010, 0x77], "vpblendd xmm9, xmm0, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x02, 0b00_001_010, 0x77], "vpblendd xmm9, xmm0, xmmword [r10], 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x02, 0b11_001_010, 0x77], "vpblendd ymm9, ymm0, ymm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x02, 0b00_001_010, 0x77], "vpblendd ymm9, ymm0, ymmword [r10], 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x04, 0b11_001_010, 0x77], "vpermilps xmm9, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x04, 0b11_001_010, 0x77], "vpermilps ymm9, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x05, 0b11_001_010, 0x77], "vpermilpd xmm9, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x05, 0b11_001_010, 0x77], "vpermilpd ymm9, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x06, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x06, 0b11_001_010, 0x77], "vperm2f128 ymm9, ymm0, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x06, 0b00_001_010, 0x77], "vperm2f128 ymm9, ymm0, ymmword [r10], 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0c, 0b11_001_010, 0x77], "vblendps xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0c, 0b11_001_010, 0x77], "vblendps ymm9, ymm8, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0d, 0b11_001_010, 0x77], "vblendpd xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0d, 0b11_001_010, 0x77], "vblendpd ymm9, ymm8, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0e, 0b11_001_010, 0x77], "vpblendw xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0e, 0b11_001_010, 0x77], "vpblendw ymm9, ymm8, ymm10, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x08, 0b11_001_010, 0x77], "vroundps xmm9, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x08, 0b11_001_010, 0x77], "vroundps ymm9, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x08, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x08, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x09, 0b11_001_010, 0x77], "vroundpd xmm9, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x09, 0b11_001_010, 0x77], "vroundpd ymm9, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x09, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x09, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0a, 0b11_001_010, 0x77], "vroundss xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0a, 0b11_001_010, 0x77], "vroundss xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0b, 0b11_001_010, 0x77], "vroundsd xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0b, 0b11_001_010, 0x77], "vroundsd xmm9, xmm8, xmm10, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_001, 0x0f, 0b11_001_010, 0x77], "vpalignr xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_101, 0x0f, 0b11_001_010, 0x77], "vpalignr ymm9, ymm8, ymm10, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x14, 0b11_001_010, 0x77], "vpextrb r10d, xmm9, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x14, 0b00_001_010, 0x77], "vpextrb byte [r10], xmm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x14, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x14, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x15, 0b11_001_010, 0x77], "vpextrw r10d, xmm9, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x15, 0b00_001_010, 0x77], "vpextrw word [r10], xmm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x15, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x15, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x16, 0b11_001_010, 0x77], "vpextrd r10d, xmm9, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x16, 0b00_001_010, 0x77], "vpextrd dword [r10], xmm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x16, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x16, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_1111_001, 0x16, 0b11_001_010, 0x77], "vpextrq r10, xmm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x16, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_1111_001, 0x16, 0b00_001_010, 0x77], "vpextrq qword [r10], xmm9, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x17, 0b11_001_010, 0x77], "vextractps r10d, xmm9, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x17, 0b00_001_010, 0x77], "vextractps dword [r10], xmm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x17, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x17, 0b00_001_010, 0x77]),

        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x18, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x18, 0b11_001_010, 0x77], "vinsertf128 ymm9, ymm8, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0x19, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x19, 0b11_001_010, 0x77], "vextractf128 xmm10, ymm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]),

        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x38, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x38, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x38, 0b11_001_010, 0x77], "vinserti128 ymm9, ymm8, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x39, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x39, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x39, 0b11_001_010, 0x77], "vextracti128 xmm10, ymm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]),

        testcase!(features { AVX_F16C: true, AVX: false, F16C: false } &[0xc4, 0b000_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm10, ymm9, 0x77"),
        testcase!(features { AVX_F16C: true, AVX: false, F16C: false } &[0xc4, 0b000_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm10, ymm9, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x1d, 0b11_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x20, 0b11_001_010, 0x77], "vpinsrb xmm9, xmm8, r10d, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x20, 0b00_001_010, 0x77], "vpinsrb xmm9, xmm8, byte [r10], 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x20, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x21, 0b11_001_010, 0x77], "vinsertps xmm9, xmm8, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x21, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x22, 0b11_001_010, 0x77], "vpinsrd xmm9, xmm8, r10d, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x22, 0b00_001_010, 0x77], "vpinsrd xmm9, xmm8, dword [r10], 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x22, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_001, 0x22, 0b11_001_010, 0x77], "vpinsrq xmm9, xmm8, r10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_001, 0x22, 0b00_001_010, 0x77], "vpinsrq xmm9, xmm8, qword [r10], 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0x22, 0b00_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x40, 0b11_001_010, 0x77], "vdpps xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x40, 0b11_001_010, 0x77], "vdpps ymm9, ymm8, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x41, 0b11_001_010, 0x77], "vdppd xmm9, xmm8, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x41, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x42, 0b11_001_010, 0x77], "vmpsadbw xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x42, 0b11_001_010, 0x77], "vmpsadbw ymm9, ymm8, ymm10, 0x77"),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x46, 0b11_001_010, 0x77], "vperm2i128 ymm9, ymm0, ymm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x46, 0b00_001_010, 0x77], "vperm2i128 ymm9, ymm0, ymmword [r10], 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x46, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x46, 0b11_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x4c, 0b11_001_010, 0x77], "vpblendvb xmm9, xmm8, xmm10, xmm7"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x4c, 0b11_001_010, 0x77], "vpblendvb ymm9, ymm8, ymm10, ymm7"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x4c, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0x4c, 0b11_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x60, 0b11_001_010, 0x77], "vpcmpestrm xmm9, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x60, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x60, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x60, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x61, 0b11_001_010, 0x77], "vpcmpestri xmm9, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x61, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x61, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x61, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x62, 0b11_001_010, 0x77], "vpcmpistrm xmm9, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x62, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x62, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x62, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x63, 0b11_001_010, 0x77], "vpcmpistri xmm9, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x63, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x63, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x63, 0b11_001_010, 0x77]),

        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00011, 0b1_1111_001, 0xdf, 0b11_001_010, 0x77], "vaeskeygenassist xmm9, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0xdf, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0xdf, 0b11_001_010, 0x77]),

        // prefix 02
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x00, 0b11_001_010], "vpshufb xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x00, 0b11_001_010], "vpshufb ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x01, 0b11_001_010], "vphaddw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x01, 0b11_001_010], "vphaddw ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x02, 0b11_001_010], "vphaddd xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x02, 0b11_001_010], "vphaddd ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x03, 0b11_001_010], "vphaddsw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x03, 0b11_001_010], "vphaddsw ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x04, 0b11_001_010], "vpmaddubsw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x04, 0b11_001_010], "vpmaddubsw ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x05, 0b11_001_010], "vphsubw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x05, 0b11_001_010], "vphsubw ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x06, 0b11_001_010], "vphsubd xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x06, 0b11_001_010], "vphsubd ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x07, 0b11_001_010], "vphsubsw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x07, 0b11_001_010], "vphsubsw ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x08, 0b11_001_010], "vpsignb xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x08, 0b11_001_010], "vpsignb ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x09, 0b11_001_010], "vpsignw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x09, 0b11_001_010], "vpsignw ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0a, 0b11_001_010], "vpsignd xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0a, 0b11_001_010], "vpsignd ymm9, ymm0, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0b, 0b11_001_010], "vpmulhrsw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0b, 0b11_001_010], "vpmulhrsw ymm9, ymm0, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x0c, 0b11_001_010], "vpermilps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x0c, 0b11_001_010], "vpermilps ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x0d, 0b11_001_010], "vpermilpd xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x0d, 0b11_001_010], "vpermilpd ymm9, ymm8, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x0d, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x0d, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0e, 0b11_001_010], "vtestps xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0e, 0b11_001_010], "vtestps ymm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x0e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0f, 0b11_001_010], "vtestpd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0f, 0b11_001_010], "vtestpd ymm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x0f, 0b11_001_010]),
        testcase!(features { AVX_F16C: true, AVX: false F16C: false } &[0xc4, 0b111_00010, 0b0_1111_001, 0x13, 0b11_001_010], "vcvtph2ps xmm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x13, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x16, 0b11_001_010], "vpermps ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x16, 0b00_001_010], "vpermps ymm9, ymm0, ymmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x16, 0b00_011_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x16, 0b00_011_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x17, 0b11_001_010], "vptest xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x17, 0b11_001_010], "vptest ymm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x17, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x17, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x18, 0b00_001_010], "vbroadcastss xmm9, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x18, 0b00_001_010], "vbroadcastss ymm9, dword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_001, 0x18, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x18, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x19, 0b00_001_010], "vbroadcastsd ymm9, qword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x19, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x19, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1a, 0b00_001_010], "vbroadcastf128 ymm9, xmmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x1a, 0b00_001_010]), // vex.w=1 is invalid
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x1a, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x1a, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x5a, 0b00_001_010], "vbroadcasti128 ymm9, xmmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_101, 0x5a, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x5a, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x5a, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x18, 0b11_001_010], "vbroadcastss xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x18, 0b11_001_010], "vbroadcastss ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x18, 0b00_001_010], "vbroadcastss ymm9, dword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x18, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x18, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b111_00010, 0b0_1111_001, 0x19, 0b11_001_010]), // "vbroadcastsd xmm, xmm" is not legal (L!=0)
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x19, 0b11_001_010], "vbroadcastsd ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x19, 0b00_001_010], "vbroadcastsd ymm9, qword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x19, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x19, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x19, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x1a, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x1a, 0b11_001_010]),


        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x1c, 0b11_001_010], "vpabsb xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1c, 0b11_001_010], "vpabsb ymm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x1c, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x1d, 0b11_001_010], "vpabsw xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1d, 0b11_001_010], "vpabsw ymm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x1d, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x1e, 0b11_001_010], "vpabsd xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1e, 0b11_001_010], "vpabsd ymm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x1e, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x20, 0b11_001_010], "vpmovsxbw xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x20, 0b11_001_010], "vpmovsxbw ymm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x20, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x21, 0b11_001_010], "vpmovsxbd xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x21, 0b11_001_010], "vpmovsxbd ymm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x21, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x22, 0b11_001_010], "vpmovsxbq xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x22, 0b11_001_010], "vpmovsxbq ymm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x22, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x23, 0b11_001_010], "vpmovsxwd xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x23, 0b11_001_010], "vpmovsxwd ymm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x23, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x24, 0b11_001_010], "vpmovsxwq xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x24, 0b11_001_010], "vpmovsxwq ymm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x24, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x25, 0b11_001_010], "vpmovsxdq xmm9, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x25, 0b11_001_010], "vpmovsxdq ymm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x25, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x28, 0b11_001_010], "vpmuldq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x28, 0b11_001_010], "vpmuldq ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x29, 0b11_001_010], "vpcmpeqq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x29, 0b11_001_010], "vpcmpeqq ymm9, ymm8, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x2a, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x2a, 0b00_001_010], "vmovntdqa xmm9, xmmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x2a, 0b00_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x2a, 0b00_001_010], "vmovntdqa ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2b, 0b11_001_010], "vpackusdw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2b, 0b11_001_010], "vpackusdw ymm9, ymm8, ymm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2b, 0b00_001_010], "vpackusdw ymm9, ymm8, ymmword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2c, 0b00_001_010], "vmaskmovps xmm9, xmm8, xmmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2c, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2c, 0b00_001_010], "vmaskmovps ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2d, 0b00_001_010], "vmaskmovpd xmm9, xmm8, xmmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2d, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2d, 0b00_001_010], "vmaskmovpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2e, 0b00_001_010], "vmaskmovps xmmword [r10], xmm8, xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2e, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2e, 0b00_001_010], "vmaskmovps ymmword [r10], ymm8, ymm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2f, 0b00_001_010], "vmaskmovpd xmmword [r10], xmm8, xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2f, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2f, 0b00_001_010], "vmaskmovpd ymmword [r10], ymm8, ymm9"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x30, 0b11_001_010], "vpmovzxbw xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x30, 0b11_001_010], "vpmovzxbw ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x31, 0b11_001_010], "vpmovzxbd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x31, 0b11_001_010], "vpmovzxbd ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x32, 0b11_001_010], "vpmovzxbq xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x32, 0b11_001_010], "vpmovzxbq ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x33, 0b11_001_010], "vpmovzxwd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x33, 0b11_001_010], "vpmovzxwd ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x34, 0b11_001_010], "vpmovzxwq xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x34, 0b11_001_010], "vpmovzxwq ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x35, 0b11_001_010], "vpmovzxdq xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x35, 0b11_001_010], "vpmovzxdq ymm9, xmm10"),

        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x30, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x30, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x31, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x31, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x32, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x32, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x33, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x33, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x34, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x34, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x35, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x35, 0b11_001_010]),

        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x36, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x36, 0b11_001_010], "vpermd ymm9, ymm8, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x36, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x37, 0b11_001_010], "vpcmpgtq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x37, 0b11_001_010], "vpcmpgtq ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x38, 0b11_001_010], "vpminsb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x38, 0b11_001_010], "vpminsb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x39, 0b11_001_010], "vpminsd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x39, 0b11_001_010], "vpminsd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3a, 0b11_001_010], "vpminuw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3a, 0b11_001_010], "vpminuw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3b, 0b11_001_010], "vpminud xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3b, 0b11_001_010], "vpminud ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3c, 0b11_001_010], "vpmaxsb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3c, 0b11_001_010], "vpmaxsb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3d, 0b11_001_010], "vpmaxsd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3d, 0b11_001_010], "vpmaxsd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3e, 0b11_001_010], "vpmaxuw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3e, 0b11_001_010], "vpmaxuw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3f, 0b11_001_010], "vpmaxud xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3f, 0b11_001_010], "vpmaxud ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x40, 0b11_001_010], "vpmulld xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x40, 0b11_001_010], "vpmulld ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x41, 0b11_001_010], "vphminposuw xmm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x41, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x41, 0b11_001_010]),
    // TODO: should something be at opcode 42 here?
    //    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b1_0111_001, 0x42, 0b11_001_010], "vphminposuw xmm"),
    //    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x41, 0b11_001_010]),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x45, 0b00_001_010], "vpsrlvd xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x45, 0b00_001_010], "vpsrlvd ymm9, ymm0, ymmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x45, 0b11_001_010], "vpsrlvd xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x45, 0b11_001_010], "vpsrlvd ymm9, ymm0, ymm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x45, 0b00_001_010], "vpsrlvq xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x45, 0b00_001_010], "vpsrlvq ymm9, ymm0, ymmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x45, 0b11_001_010], "vpsrlvq xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x45, 0b11_001_010], "vpsrlvq ymm9, ymm0, ymm10"),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x46, 0b00_001_010], "vpsravd xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x46, 0b00_001_010], "vpsravd ymm9, ymm0, ymmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_001, 0x46, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x46, 0b00_001_010]),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x47, 0b00_001_010], "vpsllvd xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x47, 0b00_001_010], "vpsllvd ymm9, ymm0, ymmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x47, 0b11_001_010], "vpsllvd xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x47, 0b11_001_010], "vpsllvd ymm9, ymm0, ymm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x47, 0b00_001_010], "vpsllvq xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x47, 0b00_001_010], "vpsllvq ymm9, ymm0, ymmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x47, 0b11_001_010], "vpsllvq xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x47, 0b11_001_010], "vpsllvq ymm9, ymm0, ymm10"),

        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x58, 0b11_000_001], "vpbroadcastd xmm0, xmm1"),
        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x58, 0b11_000_001], "vpbroadcastd ymm0, xmm1"),
        testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x58, 0b11_000_001]),
        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x59, 0b11_000_001], "vpbroadcastq xmm0, xmm1"),
        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x59, 0b11_000_001], "vpbroadcastq ymm0, xmm1"),
        testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x59, 0b11_000_001]),

        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x78, 0b11_000_001], "vpbroadcastb xmm0, xmm1"),
        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x78, 0b11_000_001], "vpbroadcastb ymm0, xmm1"),
        testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x78, 0b11_000_001]),
        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x79, 0b11_000_001], "vpbroadcastw xmm0, xmm1"),
        testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x79, 0b11_000_001], "vpbroadcastw ymm0, xmm1"),
        testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x79, 0b11_000_001]),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x8c, 0b00_001_010], "vpmaskmovd xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x8c, 0b00_001_010], "vpmaskmovd ymm9, ymm0, ymmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8c, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x8c, 0b00_001_010], "vpmaskmovq xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x8c, 0b00_001_010], "vpmaskmovq ymm9, ymm0, ymmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8c, 0b11_001_010]),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x8e, 0b00_001_010], "vpmaskmovd xmmword [r10], xmm0, xmm9"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x8e, 0b00_001_010], "vpmaskmovd ymmword [r10], ymm0, ymm9"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8e, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x8e, 0b00_001_010], "vpmaskmovq xmmword [r10], xmm0, xmm9"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x8e, 0b00_001_010], "vpmaskmovq ymmword [r10], ymm0, ymm9"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8e, 0b11_001_010]),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdd xmm8, dword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdd ymm8, dword [r9 + ymm12 * 4], ymm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdq xmm8, qword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdq ymm8, qword [r9 + xmm12 * 4], ymm0"),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm8, dword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm8, dword [r9 + ymm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqq xmm8, qword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqq ymm8, qword [r9 + ymm12 * 4], ymm0"),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdps xmm8, dword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdps ymm8, dword [r9 + ymm12 * 4], ymm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdpd xmm8, qword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdpd ymm8, qword [r9 + xmm12 * 4], ymm0"),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqps xmm8, dword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqps xmm8, dword [r9 + ymm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqpd xmm8, qword [r9 + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqpd ymm8, qword [r9 + ymm12 * 4], ymm0"),

        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b0_1111_001, 0xdb, 0b11_001_010], "vaesimc xmm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0xdb, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0xdb, 0b11_001_010]),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xdc, 0b11_001_010], "vaesenc xmm9, xmm8, xmm10"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xdc, 0b11_001_010], "vaesenc ymm9, ymm8, ymm10"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xdd, 0b11_001_010], "vaesenclast xmm9, xmm8, xmm10"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xdd, 0b11_001_010], "vaesenclast ymm9, ymm8, ymm10"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xde, 0b11_001_010], "vaesdec xmm9, xmm8, xmm10"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xde, 0b11_001_010], "vaesdec ymm9, ymm8, ymm10"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xdf, 0b11_001_010], "vaesdeclast xmm9, xmm8, xmm10"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xdf, 0b11_001_010], "vaesdeclast ymm9, ymm8, ymm10"),

        // prefix 01
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x10, 0b00_001_010], "vmovsd xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x10, 0b00_001_010], "vmovsd xmm9, qword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_011, 0x10, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0x10, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x10, 0b00_001_010], "vmovupd xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x10, 0b00_001_010], "vmovupd ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x11, 0b00_001_010], "vmovupd xmmword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x11, 0b00_001_010], "vmovupd ymmword [r10], ymm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x11, 0b00_001_010], "vmovsd qword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x11, 0b00_001_010], "vmovsd qword [r10], xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_011, 0x11, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0x11, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x10, 0b00_001_010], "vmovupd xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x10, 0b00_001_010], "vmovupd ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x11, 0b00_001_010], "vmovupd xmmword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x11, 0b00_001_010], "vmovupd ymmword [r10], ymm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x10, 0b00_001_010], "vmovss xmm9, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x10, 0b00_001_010], "vmovss xmm9, dword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_010, 0x10, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_110, 0x10, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x10, 0b00_001_010], "vmovups xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x10, 0b00_001_010], "vmovups ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x11, 0b00_001_010], "vmovups xmmword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x11, 0b00_001_010], "vmovups ymmword [r10], ymm9"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x11, 0b11_001_010], "vmovsd xmm10, xmm8, xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x11, 0b11_001_010], "vmovsd xmm10, xmm8, xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x11, 0b00_001_010], "vmovss dword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x11, 0b00_001_010], "vmovss dword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x11, 0b00_001_010], "vmovups xmmword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x11, 0b00_001_010], "vmovups ymmword [r10], ymm9"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x12, 0b00_001_010], "vmovddup xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x12, 0b00_001_010], "vmovddup ymm9, ymmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_011, 0x12, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_111, 0x12, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x12, 0b11_001_010], "vmovhlps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x12, 0b00_001_010], "vmovlps xmm9, xmm8, qword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_100, 0x12, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0x12, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x12, 0b00_001_010], "vmovsldup xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x12, 0b00_001_010], "vmovsldup ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x12, 0b00_001_010], "vmovsldup xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x12, 0b00_001_010], "vmovsldup ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x12, 0b00_001_010], "vmovlpd xmm9, xmm8, qword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x12, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x13, 0b00_001_010], "vmovlpd qword [r10], xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x13, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x13, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_000, 0x14, 0b00_001_010], "vunpcklps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_100, 0x14, 0b00_001_010], "vunpcklps ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x14, 0b00_001_010], "vunpcklpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x14, 0b00_001_010], "vunpcklpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_000, 0x15, 0b00_001_010], "vunpckhps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_100, 0x15, 0b00_001_010], "vunpckhps ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x15, 0b00_001_010], "vunpckhpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x15, 0b00_001_010], "vunpckhpd ymm9, ymm8, ymmword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x16, 0b11_001_010], "vmovshdup xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x16, 0b11_001_010], "vmovshdup ymm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_010, 0x16, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_110, 0x16, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x16, 0b00_001_010], "vmovhps xmm9, xmm8, qword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_100, 0x16, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x16, 0b00_001_010], "vmovhpd xmm9, xmm8, qword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x16, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x16, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x17, 0b00_001_010], "vmovhps qword [r10], xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_100, 0x17, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_000, 0x17, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_100, 0x17, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x17, 0b00_001_010], "vmovhpd qword [r10], xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x17, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x17, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x28, 0b11_001_010], "vmovaps xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x28, 0b11_001_010], "vmovaps ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x29, 0b11_001_010], "vmovaps xmm10, xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x29, 0b11_001_010], "vmovaps ymm10, ymm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x28, 0b11_001_010], "vmovapd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x28, 0b11_001_010], "vmovapd ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x29, 0b11_001_010], "vmovapd xmm10, xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x29, 0b11_001_010], "vmovapd ymm10, ymm9"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2a, 0b11_001_010], "vcvtsi2ss xmm9, xmm0, r10d"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2a, 0b00_001_010], "vcvtsi2ss xmm9, xmm0, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x2a, 0b11_001_010], "vcvtsi2ss xmm9, xmm0, r10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x2a, 0b00_001_010], "vcvtsi2ss xmm9, xmm0, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2a, 0b11_001_010], "vcvtsi2ss xmm9, xmm0, r10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, r10d"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, r10d"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, r10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2a, 0b00_001_010], "vcvtsi2sd xmm9, xmm0, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x2a, 0b00_001_010], "vcvtsi2sd xmm9, xmm0, qword [r10]"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_011, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, edx"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x2b, 0b00_001_010], "vmovntps xmmword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x2b, 0b00_001_010], "vmovntps ymmword [r10], ymm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_000, 0x2b, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_100, 0x2b, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2b, 0b00_001_010], "vmovntpd xmmword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x2b, 0b00_001_010], "vmovntpd ymmword [r10], ymm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0x2b, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x2b, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2c, 0b11_001_010], "vcvttss2si r9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2c, 0b00_001_010], "vcvttss2si r9d, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2c, 0b00_001_010], "vcvttss2si r9, dword [r10]"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_010, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_010, 0x2c, 0b00_001_010], "vcvttss2si r9d, dword [rdx]"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_110, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si r9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si r9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si r9d, qword [r10]"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_011, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si r9d, qword [rdx]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2d, 0b00_001_010], "vcvtss2si r9d, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2d, 0b00_001_010], "vcvtss2si r9, dword [r10]"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_010, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_110, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2d, 0b00_001_010], "vcvtsd2si r9d, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2d, 0b00_001_010], "vcvtsd2si r9d, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si r9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x2d, 0b00_001_010], "vcvtsd2si r9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_011, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm2"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2e, 0b00_001_010], "vucomisd xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2e, 0b00_001_010], "vucomisd xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2e, 0b11_001_010], "vucomisd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2e, 0b11_001_010], "vucomisd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2f, 0b00_001_010], "vcomisd xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2f, 0b00_001_010], "vcomisd xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2f, 0b11_001_010], "vcomisd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2f, 0b11_001_010], "vcomisd xmm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2e, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2e, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2e, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2e, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2f, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2f, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2f, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2f, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc5, 0b0_1111_000, 0x2e, 0b11_001_010], "vucomiss xmm9, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_100, 0x2e, 0b00_001_010], "vucomiss xmm9, dword [rdx]"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_000, 0x2f, 0b11_001_010], "vcomiss xmm9, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b0_1111_100, 0x2f, 0b00_001_010], "vcomiss xmm9, dword [rdx]"),
        testcase!(invalid: &[0xc5, 0b0_1111_111, 0x2f, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x50, 0b11_001_010], "vmovmskps r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x50, 0b11_001_010], "vmovmskps r9d, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_000, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_100, 0x50, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x50, 0b11_001_010], "vmovmskpd r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x50, 0b11_001_010], "vmovmskpd r9d, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x50, 0b11_001_010], "vmovmskpd r9d, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x50, 0b11_001_010], "vmovmskpd r9d, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_101, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x50, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x51, 0b00_001_010], "vsqrtpd xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x51, 0b00_001_010], "vsqrtpd ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_011, 0x51, 0b00_001_010], "vsqrtsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_111, 0x51, 0b00_001_010], "vsqrtsd xmm9, xmm8, qword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x51, 0b00_001_010], "vsqrtps xmm9, xmmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_000, 0x51, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x51, 0b00_001_010], "vsqrtps ymm9, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x51, 0b00_001_010], "vsqrtss xmm9, xmm0, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x51, 0b00_001_010], "vsqrtss xmm9, xmm0, dword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x52, 0b11_001_010], "vrsqrtps xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x52, 0b11_001_010], "vrsqrtps ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x52, 0b11_001_010], "vrsqrtss xmm9, xmm0, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x52, 0b11_001_010], "vrsqrtss xmm9, xmm0, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x53, 0b11_001_010], "vrcpps xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x53, 0b11_001_010], "vrcpps ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x53, 0b11_001_010], "vrcpss xmm9, xmm0, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x53, 0b11_001_010], "vrcpss xmm9, xmm0, xmm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x54, 0b11_001_010], "vandps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x54, 0b11_001_010], "vandps ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x55, 0b11_001_010], "vandnps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x55, 0b11_001_010], "vandnps ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x54, 0b00_001_010], "vandpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x54, 0b00_001_010], "vandpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x55, 0b00_001_010], "vandnpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x55, 0b00_001_010], "vandnpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x56, 0b00_001_010], "vorpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x56, 0b00_001_010], "vorpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x56, 0b00_001_010], "vorps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x56, 0b00_001_010], "vorps ymm9, ymm8, ymmword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x57, 0b11_001_010], "vxorps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x57, 0b11_001_010], "vxorps ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x57, 0b11_001_010], "vxorpd xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x57, 0b11_001_010], "vxorpd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x58, 0b11_001_010], "vaddps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x58, 0b11_001_010], "vaddps ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x58, 0b11_001_010], "vaddss xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x58, 0b11_001_010], "vaddss xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x58, 0b00_001_010], "vaddss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x58, 0b00_001_010], "vaddss xmm9, xmm8, dword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x58, 0b00_001_010], "vaddpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x58, 0b00_001_010], "vaddpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x58, 0b00_001_010], "vaddsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x58, 0b00_001_010], "vaddsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x59, 0b00_001_010], "vmulps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x59, 0b00_001_010], "vmulps ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x59, 0b00_001_010], "vmulpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x59, 0b00_001_010], "vmulpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x59, 0b00_001_010], "vmulss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x59, 0b00_001_010], "vmulss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x59, 0b00_001_010], "vmulsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x59, 0b00_001_010], "vmulsd xmm9, xmm8, qword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5a, 0b11_001_010], "vcvtps2pd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5a, 0b11_001_010], "vcvtps2pd ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5a, 0b00_001_010], "vcvtps2pd xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5a, 0b00_001_010], "vcvtps2pd ymm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x5a, 0b11_001_010], "vcvtpd2ps xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x5a, 0b11_001_010], "vcvtpd2ps xmm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x5a, 0b11_001_010], "vcvtsd2ss xmm9, xmm0, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x5a, 0b11_001_010], "vcvtsd2ss xmm9, xmm0, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x5a, 0b11_001_010], "vcvtss2sd xmm9, xmm0, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x5a, 0b11_001_010], "vcvtss2sd xmm9, xmm0, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x5b, 0b11_001_010], "vcvtps2dq xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x5b, 0b11_001_010], "vcvtps2dq ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x5b, 0b11_001_010], "vcvttps2dq xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x5b, 0b11_001_010], "vcvttps2dq ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5b, 0b11_001_010], "vcvtdq2ps xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5b, 0b00_001_010], "vcvtdq2ps xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5b, 0b11_001_010], "vcvtdq2ps ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5b, 0b00_001_010], "vcvtdq2ps ymm9, ymmword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x5c, 0b00_001_010], "vsubps xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_100, 0x5c, 0b00_001_010], "vsubps ymm9, ymm0, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_010, 0x5c, 0b00_001_010], "vsubss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_110, 0x5c, 0b00_001_010], "vsubss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x5c, 0b00_001_010], "vsubpd xmm9, xmm0, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x5c, 0b00_001_010], "vsubpd ymm9, ymm0, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_011, 0x5c, 0b00_001_010], "vsubsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_111, 0x5c, 0b00_001_010], "vsubsd xmm9, xmm8, qword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5d, 0b00_001_010], "vminps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x5d, 0b00_001_010], "vminps ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x5d, 0b00_001_010], "vminss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x5d, 0b00_001_010], "vminss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x5d, 0b00_001_010], "vminpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x5d, 0b00_001_010], "vminpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x5d, 0b00_001_010], "vminsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x5d, 0b00_001_010], "vminsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5e, 0b00_001_010], "vdivps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5e, 0b00_001_010], "vdivps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x5e, 0b00_001_010], "vdivpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x5e, 0b00_001_010], "vdivss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x5e, 0b00_001_010], "vdivsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x5e, 0b00_001_010], "vdivps ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x5e, 0b00_001_010], "vdivpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x5e, 0b00_001_010], "vdivss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x5e, 0b00_001_010], "vdivsd xmm9, xmm8, qword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5f, 0b00_001_010], "vmaxps xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x5f, 0b00_001_010], "vmaxpd xmm9, xmm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x5f, 0b00_001_010], "vmaxss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x5f, 0b00_001_010], "vmaxsd xmm9, xmm8, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x5f, 0b00_001_010], "vmaxps ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x5f, 0b00_001_010], "vmaxpd ymm9, ymm8, ymmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x5f, 0b00_001_010], "vmaxss xmm9, xmm8, dword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x5f, 0b00_001_010], "vmaxsd xmm9, xmm8, qword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x60, 0b11_001_010], "vpunpcklbw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x60, 0b11_001_010], "vpunpcklbw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x61, 0b11_001_010], "vpunpcklwd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x61, 0b11_001_010], "vpunpcklwd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x62, 0b11_001_010], "vpunpckldq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x62, 0b11_001_010], "vpunpckldq ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x63, 0b11_001_010], "vpacksswb xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x63, 0b11_001_010], "vpacksswb ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x64, 0b11_001_010], "vpcmpgtb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x64, 0b11_001_010], "vpcmpgtb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x65, 0b11_001_010], "vpcmpgtw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x65, 0b11_001_010], "vpcmpgtw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x66, 0b11_001_010], "vpcmpgtd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x66, 0b11_001_010], "vpcmpgtd ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x67, 0b11_001_010], "vpackuswb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x67, 0b11_001_010], "vpackuswb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x68, 0b11_001_010], "vpunpckhbw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x68, 0b11_001_010], "vpunpckhbw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x69, 0b11_001_010], "vpunpckhwd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x69, 0b11_001_010], "vpunpckhwd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x6a, 0b11_001_010], "vpunpckhdq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x6a, 0b11_001_010], "vpunpckhdq ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x6b, 0b11_001_010], "vpackssdw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x6b, 0b11_001_010], "vpackssdw ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6c, 0b11_001_010], "vpunpcklqdq xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x6c, 0b11_001_010], "vpunpcklqdq ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6d, 0b11_001_010], "vpunpckhqdq xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x6d, 0b11_001_010], "vpunpckhqdq ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6e, 0b11_001_010], "vmovq xmm9, r10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6e, 0b00_001_010], "vmovq xmm9, qword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xd6, 0b00_001_010], "vmovq qword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xd6, 0b11_001_010], "vmovq xmm10, xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x6e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x6f, 0b11_001_010], "vmovdqa xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x6f, 0b11_001_010], "vmovdqa ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x6f, 0b11_001_010], "vmovdqu xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x6f, 0b11_001_010], "vmovdqu ymm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x70, 0b11_001_010, 0x77], "vpshufd xmm9, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x70, 0b11_001_010, 0x77], "vpshufd ymm9, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x70, 0b11_001_010, 0x77], "vpshufhw xmm9, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x70, 0b11_001_010, 0x77], "vpshufhw ymm9, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x70, 0b11_001_010, 0x77], "vpshuflw xmm9, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x70, 0b11_001_010, 0x77], "vpshuflw ymm9, ymm10, 0x77"),


        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_010_010, 0x77], "vpsrlw xmm0, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x71, 0b11_010_010, 0x77], "vpsrlw xmm8, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x71, 0b11_010_010, 0x77], "vpsrlw ymm0, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b00_011_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_100_010, 0x77], "vpsraw xmm0, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x71, 0b11_100_010, 0x77], "vpsraw ymm0, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_101_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_110_010, 0x77], "vpsllw xmm0, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x71, 0b11_110_010, 0x77], "vpsllw ymm0, ymm10, 0x77"),

        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_000_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b11_010_010, 0x77], "vpsrld xmm0, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_010_010, 0x77], "vpsrld ymm0, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_011_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b11_100_010, 0x77], "vpsrad xmm0, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_100_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_100_010, 0x77], "vpsrad ymm0, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_101_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b11_110_010, 0x77], "vpslld xmm0, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_110_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_110_010, 0x77], "vpslld ymm0, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_111_010, 0x77]),

        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_000_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_010_010, 0x77], "vpsrlq xmm0, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_011_010, 0x77], "vpsrldq xmm0, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_010_010, 0x77], "vpsrlq ymm0, ymm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_011_010, 0x77], "vpsrldq ymm0, ymm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_100_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_101_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_110_010, 0x77], "vpsllq xmm0, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_111_010, 0x77], "vpslldq xmm0, xmm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_110_010, 0x77], "vpsllq ymm0, ymm10, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_111_010, 0x77], "vpslldq ymm0, ymm10, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x74, 0b11_001_010], "vpcmpeqb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x74, 0b11_001_010], "vpcmpeqb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x75, 0b11_001_010], "vpcmpeqw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x75, 0b11_001_010], "vpcmpeqw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x76, 0b11_001_010], "vpcmpeqd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x76, 0b11_001_010], "vpcmpeqd ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x7c, 0b11_001_010], "vhaddpd xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x7c, 0b11_001_010], "vhaddpd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x7c, 0b11_001_010], "vhaddps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x7c, 0b11_001_010], "vhaddps ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x7d, 0b11_001_010], "vhsubpd xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x7d, 0b11_001_010], "vhsubpd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x7d, 0b11_001_010], "vhsubps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x7d, 0b11_001_010], "vhsubps ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x7e, 0b11_001_010], "vmovd r10d, xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_101, 0x7e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x7e, 0b11_001_010], "vmovq r10, xmm9"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x7e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x7f, 0b11_001_010], "vmovdqa xmm10, xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x7f, 0b11_001_010], "vmovdqa ymm10, ymm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x7f, 0b11_001_010], "vmovdqu xmm10, xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x7f, 0b11_001_010], "vmovdqu ymm10, ymm9"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b00_010_001], "vldmxcsr dword [r9]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_100, 0xae, 0b00_010_001]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b11_010_001]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b00_011_001], "vstmxcsr dword [r9]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_100, 0xae, 0b00_011_001]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b11_011_001]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0xc2, 0b11_001_010, 0x77], "vcmpps xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0xc2, 0b11_001_010, 0x77], "vcmpps ymm9, ymm8, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xc2, 0b11_001_010, 0x77], "vcmppd xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xc2, 0b11_001_010, 0x77], "vcmppd ymm9, ymm8, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0xc2, 0b11_001_010, 0x77], "vcmpsd xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0xc2, 0b11_001_010, 0x77], "vcmpsd xmm9, xmm8, xmm10, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xc4, 0b11_001_010, 0x77], "vpinsrw xmm9, xmm8, r10d, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xc4, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0xc5, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0xc5, 0b11_001_010, 0x77], "vpextrw r9d, xmm10, 0x77"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0xc5, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xc5, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xc6, 0b11_001_010, 0x77], "vshufpd xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xc6, 0b11_001_010, 0x77], "vshufpd ymm9, ymm8, ymm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_000, 0xc6, 0b11_001_010, 0x77], "vshufps xmm9, xmm8, xmm10, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_100, 0xc6, 0b11_001_010, 0x77], "vshufps ymm9, ymm8, ymm10, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd0, 0b11_001_010], "vaddsubpd xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd0, 0b11_001_010], "vaddsubpd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0xd0, 0b11_001_010], "vaddsubps xmm9, xmm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0xd0, 0b11_001_010], "vaddsubps ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd1, 0b11_001_010], "vpsrlw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd1, 0b11_001_010], "vpsrlw ymm9, ymm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd1, 0b00_001_010], "vpsrlw ymm9, ymm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd2, 0b11_001_010], "vpsrld xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd2, 0b11_001_010], "vpsrld ymm9, ymm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd2, 0b00_001_010], "vpsrld ymm9, ymm8, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd3, 0b11_001_010], "vpsrlq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd3, 0b11_001_010], "vpsrlq ymm9, ymm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd3, 0b00_001_010], "vpsrlq ymm9, ymm8, xmmword [r10]"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd4, 0b11_001_010], "vpaddq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd4, 0b11_001_010], "vpaddq ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xd5, 0b11_001_010], "vpmullw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xd5, 0b11_001_010], "vpmullw ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0xd7, 0b11_001_010], "vpmovmskb r9d, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0xd7, 0b00_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0xd7, 0b11_001_010], "vpmovmskb r9d, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd8, 0b11_001_010], "vpsubusb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd8, 0b11_001_010], "vpsubusb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd9, 0b11_001_010], "vpsubusw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd9, 0b11_001_010], "vpsubusw ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xda, 0b11_001_010], "vpminub xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xda, 0b11_001_010], "vpminub ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdb, 0b11_001_010], "vpand xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdb, 0b11_001_010], "vpand ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdc, 0b11_001_010], "vpaddusb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdc, 0b11_001_010], "vpaddusb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdd, 0b11_001_010], "vpaddusw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdd, 0b11_001_010], "vpaddusw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xde, 0b11_001_010], "vpmaxub xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xde, 0b11_001_010], "vpmaxub ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdf, 0b11_001_010], "vpandn xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdf, 0b11_001_010], "vpandn ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe0, 0b11_001_010], "vpavgb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe0, 0b11_001_010], "vpavgb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe1, 0b11_001_010], "vpsraw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe1, 0b11_001_010], "vpsraw ymm9, ymm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe2, 0b11_001_010], "vpsrad xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe2, 0b11_001_010], "vpsrad ymm9, ymm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe3, 0b11_001_010], "vpavgw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe3, 0b11_001_010], "vpavgw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe4, 0b11_001_010], "vpmulhuw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe4, 0b11_001_010], "vpmulhuw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe5, 0b11_001_010], "vpmulhw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe5, 0b11_001_010], "vpmulhw ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xe6, 0b11_001_010], "vcvttpd2dq xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xe6, 0b11_001_010], "vcvttpd2dq xmm9, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0xe6, 0b11_001_010], "vcvtdq2pd xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0xe6, 0b11_001_010], "vcvtdq2pd ymm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0xe6, 0b11_001_010], "vcvtpd2dq xmm9, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0xe6, 0b11_001_010], "vcvtpd2dq xmm9, ymm10"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0xe7, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xe7, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xe7, 0b00_001_010], "vmovntdq xmmword [r10], xmm9"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xe7, 0b00_001_010], "vmovntdq ymmword [r10], ymm9"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe8, 0b11_001_010], "vpsubsb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe8, 0b11_001_010], "vpsubsb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe9, 0b11_001_010], "vpsubsw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe9, 0b11_001_010], "vpsubsw ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xea, 0b11_001_010], "vpminsw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xea, 0b11_001_010], "vpminsw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xeb, 0b11_001_010], "vpor xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xeb, 0b11_001_010], "vpor ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xec, 0b11_001_010], "vpaddsb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xec, 0b11_001_010], "vpaddsb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xed, 0b11_001_010], "vpaddsw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xed, 0b11_001_010], "vpaddsw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xee, 0b11_001_010], "vpmaxsw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xee, 0b11_001_010], "vpmaxsw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xef, 0b11_001_010], "vpxor xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xef, 0b11_001_010], "vpxor ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0xf0, 0b00_001_010], "vlddqu xmm9, xmmword [r10]"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0xf0, 0b00_001_010], "vlddqu ymm9, ymmword [r10]"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_011, 0xf0, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_011, 0xf0, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0xf0, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_111, 0xf0, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf1, 0b11_001_010], "vpsllw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf1, 0b11_001_010], "vpsllw ymm9, ymm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf2, 0b11_001_010], "vpslld xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf2, 0b11_001_010], "vpslld ymm9, ymm8, xmm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf3, 0b11_001_010], "vpsllq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf3, 0b11_001_010], "vpsllq ymm9, ymm8, xmm10"),


        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf4, 0b11_001_010], "vpmuludq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf4, 0b11_001_010], "vpmuludq ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xf5, 0b11_001_010], "vpmaddwd xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xf5, 0b11_001_010], "vpmaddwd ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xf6, 0b11_001_010], "vpsadbw xmm9, xmm0, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xf6, 0b11_001_010], "vpsadbw ymm9, ymm0, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xf7, 0b11_001_010], "vmaskmovdqu xmm9, xmm10"),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0xf7, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xf7, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf8, 0b11_001_010], "vpsubb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf8, 0b11_001_010], "vpsubb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf9, 0b11_001_010], "vpsubw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf9, 0b11_001_010], "vpsubw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfa, 0b11_001_010], "vpsubd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfa, 0b11_001_010], "vpsubd ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfb, 0b11_001_010], "vpsubq xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfb, 0b11_001_010], "vpsubq ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfc, 0b11_001_010], "vpaddb xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfc, 0b11_001_010], "vpaddb ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfd, 0b11_001_010], "vpaddw xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfd, 0b11_001_010], "vpaddw ymm9, ymm8, ymm10"),
        testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfe, 0b11_001_010], "vpaddd xmm9, xmm8, xmm10"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfe, 0b11_001_010], "vpaddd ymm9, ymm8, ymm10"),

        testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0x00], "vmovups xmm0, xmmword [rax]"),
        testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0x01], "vmovups xmm0, xmmword [rcx]"),
        testcase!(features { AVX: true } &[0xc5, 0x78, 0x10, 0x0f], "vmovups xmm9, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0xcf], "vmovups xmm1, xmm7"),
        testcase!(features { AVX: true } &[0xc5, 0xf9, 0x10, 0x0f], "vmovupd xmm1, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xfa, 0x7e, 0x10], "vmovq xmm2, qword [rax]"),
        testcase!(features { AVX: true } &[0xc5, 0xfc, 0x10, 0x0f], "vmovups ymm1, ymmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xfd, 0x10, 0x0f], "vmovupd ymm1, ymmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xfe, 0x10, 0x0f], "vmovss xmm1, dword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xff, 0x10, 0xcf], "vmovsd xmm1, xmm0, xmm7"),
        testcase!(features { AVX: true } &[0xc5, 0xff, 0x10, 0x01], "vmovsd xmm0, qword [rcx]"),
        testcase!(features { AVX: true } &[0xc5, 0xf9, 0x6e, 0xc6], "vmovd xmm0, esi"),
        testcase!(features { AVX: true } &[0xc5, 0xf9, 0x6e, 0x13], "vmovd xmm2, dword [rbx]"),
        testcase!(features { AVX: true } &[0xc5, 0xf9, 0x7e, 0xc6], "vmovd esi, xmm0"),
        testcase!(features { AVX: true } &[0xc5, 0xf9, 0x7e, 0x13], "vmovd dword [rbx], xmm2"),
        testcase!(invalid: features { AVX: false } &[0x4f, 0xc5, 0xf8, 0x10, 0x00]),
        testcase!(invalid: features { AVX: false } &[0xf0, 0xc5, 0xf8, 0x10, 0x00]),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x00, 0x0f], "vpshufb xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x00, 0x0f], "vpshufb ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x00, 0xcd], "vpshufb xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x00, 0xcd], "vpshufb ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x01, 0x0f], "vphaddw xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x01, 0x0f], "vphaddw ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x01, 0xcd], "vphaddw xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x01, 0xcd], "vphaddw ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x02, 0x0f], "vphaddd xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x02, 0x0f], "vphaddd ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x02, 0xcd], "vphaddd xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x02, 0xcd], "vphaddd ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x03, 0x0f], "vphaddsw xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x03, 0x0f], "vphaddsw ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x03, 0xcd], "vphaddsw xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x03, 0xcd], "vphaddsw ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x04, 0x0f], "vpmaddubsw xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x04, 0x0f], "vpmaddubsw ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x04, 0xcd], "vpmaddubsw xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x04, 0xcd], "vpmaddubsw ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x05, 0x0f], "vphsubw xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x05, 0x0f], "vphsubw ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x05, 0xcd], "vphsubw xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x05, 0xcd], "vphsubw ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x06, 0x0f], "vphsubd xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x06, 0x0f], "vphsubd ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x06, 0xcd], "vphsubd xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x06, 0xcd], "vphsubd ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x07, 0x0f], "vphsubsw xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x07, 0x0f], "vphsubsw ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x07, 0xcd], "vphsubsw xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x07, 0xcd], "vphsubsw ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x08, 0x0f], "vpsignb xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x08, 0x0f], "vpsignb ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x08, 0xcd], "vpsignb xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x08, 0xcd], "vpsignb ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x09, 0x0f], "vpsignw xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x09, 0x0f], "vpsignw ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x09, 0xcd], "vpsignw xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x09, 0xcd], "vpsignw ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0a, 0x0f], "vpsignd xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0a, 0x0f], "vpsignd ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0a, 0xcd], "vpsignd xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0a, 0xcd], "vpsignd ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0b, 0x0f], "vpmulhrsw xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0b, 0x0f], "vpmulhrsw ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0b, 0xcd], "vpmulhrsw xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0b, 0xcd], "vpmulhrsw ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0c, 0x0f], "vpermilps xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0c, 0x0f], "vpermilps ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0c, 0xcd], "vpermilps xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0c, 0xcd], "vpermilps ymm9, ymm1, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0d, 0x0f], "vpermilpd xmm9, xmm1, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0d, 0x0f], "vpermilpd ymm9, ymm1, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0d, 0xcd], "vpermilpd xmm9, xmm1, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0d, 0xcd], "vpermilpd ymm9, ymm1, ymm13"),
        testcase!(invalid: features { AVX: false } &[0xc4, 0x02, 0x71, 0x0e, 0x00]),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0e, 0x0f], "vtestps xmm9, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0e, 0x0f], "vtestps ymm9, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0e, 0xcd], "vtestps xmm9, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0e, 0xcd], "vtestps ymm9, ymm13"),
        testcase!(invalid: features { AVX: false } &[0xc4, 0x02, 0x71, 0x0f, 0x00]),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0f, 0x0f], "vtestpd xmm9, xmmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0f, 0x0f], "vtestpd ymm9, ymmword [r15]"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0f, 0xcd], "vtestpd xmm9, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0f, 0xcd], "vtestpd ymm9, ymm13"),
        testcase!(features { AVX: true } &[0xc4, 0xe2, 0x65, 0x90, 0x04, 0x51], "vpgatherdd ymm0, dword [rcx + ymm2 * 2], ymm3"),
        testcase!(features { AVX: true } &[0xc4, 0xe2, 0xe5, 0x90, 0x04, 0x51], "vpgatherdq ymm0, qword [rcx + xmm2 * 2], ymm3"),
        testcase!(features { AVX: true } &[0xc4, 0xe2, 0x65, 0x91, 0x04, 0x51], "vpgatherqd xmm0, dword [rcx + ymm2 * 2], xmm3"),
        testcase!(features { AVX: true } &[0xc4, 0xe2, 0xe5, 0x91, 0x04, 0x51], "vpgatherqq ymm0, qword [rcx + ymm2 * 2], ymm3"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x09, 0x9d, 0xcd], "vfnmadd132ss xmm9, xmm14, xmm13"),
        testcase!(features { AVX: true } &[0xc4, 0x02, 0x89, 0x9d, 0xcd], "vfnmadd132sd xmm9, xmm14, xmm13"),
    // ...
        testcase!(features { AVX: true } &[0xc4, 0xe3, 0x79, 0x14, 0xd0, 0x0a], "vpextrb eax, xmm2, 0xa"),
        testcase!(features { AVX: true } &[0xc4, 0xe3, 0x79, 0x14, 0x10, 0x0a], "vpextrb byte [rax], xmm2, 0xa"),
        testcase!(invalid: features { AVX: false } &[0xc4, 0xe3, 0xf9, 0x14, 0x00, 0xd0]),
        testcase!(invalid: features { AVX: false } &[0xc4, 0xe3, 0xf9, 0x14, 0x00, 0x0a]),
        testcase!(features { AVX: true } &[0xc5, 0xed, 0x71, 0xd0, 0x04], "vpsrlw ymm2, ymm0, 0x4"),
        testcase!(features { AVX: true } &[0xc5, 0xed, 0x73, 0xd4, 0x20], "vpsrlq ymm2, ymm4, 0x20"),
        testcase!(features { AVX: true } &[0xc4, 0xe3, 0xfd, 0x00, 0xc1, 0xa8], "vpermq ymm0, ymm1, 0xa8"),
        testcase!(features { AVX: true } &[0xc5, 0xfd, 0xea, 0xd1], "vpminsw ymm2, ymm0, ymm1"),
        testcase!(features { AVX: true } &[0xc5, 0xfd, 0xee, 0xd9], "vpmaxsw ymm3, ymm0, ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0xe3, 0x7d, 0x19, 0xd1, 0x01], "vextractf128 xmm1, ymm2, 0x1"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x38, 0x7c, 0x12, 0x05, 0x01], "vinserti128 ymm7, ymm1, xmmword [r10 + rdx * 1 + 0x5], 0x1"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x42, 0x7c, 0x12, 0x05, 0x61], "vmpsadbw ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], 0x61"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x46, 0x7c, 0x12, 0x05, 0x61], "vperm2i128 ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], 0x61"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x4a, 0x7c, 0x12, 0x05, 0x61], "vblendvps ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], ymm6"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4a, 0x7c, 0x12, 0x05, 0x61], "vblendvps xmm7, xmm1, xmmword [r10 + rdx * 1 + 0x5], xmm6"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4a, 0xdc, 0x61], "vblendvps xmm3, xmm1, xmm12, xmm6"),
        testcase!(invalid: &[0xc4, 0xc3, 0xf1, 0x4a, 0xdc, 0x61]),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x4b, 0x7c, 0x12, 0x05, 0x61], "vblendvpd ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], ymm6"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4b, 0x7c, 0x12, 0x05, 0x61], "vblendvpd xmm7, xmm1, xmmword [r10 + rdx * 1 + 0x5], xmm6"),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4b, 0xdc, 0x61], "vblendvpd xmm3, xmm1, xmm12, xmm6"),
        testcase!(invalid: &[0xc4, 0xc3, 0xf1, 0x4b, 0xdc, 0x61]),
        testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4c, 0x7c, 0x12, 0x05, 0x61], "vpblendvb xmm7, xmm1, xmmword [r10 + rdx * 1 + 0x5], xmm6"),

        testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf1, 0x0f], "vpsllw xmm1, xmm6, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf1, 0xcf], "vpsllw xmm1, xmm6, xmm7"),
        testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf1, 0x0f], "vpsllw ymm1, ymm6, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf1, 0xcf], "vpsllw ymm1, ymm6, xmm7"),
        testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf2, 0x0f], "vpslld xmm1, xmm6, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf2, 0xcf], "vpslld xmm1, xmm6, xmm7"),
        testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf2, 0x0f], "vpslld ymm1, ymm6, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf2, 0xcf], "vpslld ymm1, ymm6, xmm7"),
        testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf3, 0x0f], "vpsllq xmm1, xmm6, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf3, 0xcf], "vpsllq xmm1, xmm6, xmm7"),
        testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf3, 0x0f], "vpsllq ymm1, ymm6, xmmword [rdi]"),
        testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf3, 0xcf], "vpsllq ymm1, ymm6, xmm7"),

        testcase!(features { AVX: true } &[0xc5, 0xf1, 0xc4, 0xd8, 0x78], "vpinsrw xmm3, xmm1, eax, 0x78"),
        testcase!(features { AVX: true } &[0xc5, 0xf1, 0xc4, 0x18, 0x78], "vpinsrw xmm3, xmm1, word [rax], 0x78"),

        testcase!(features { AVX: true } &[0xc5, 0xe0, 0x54, 0x03], "vandps xmm0, xmm3, xmmword [rbx]"),
        testcase!(features { AVX: true } &[0xc5, 0xe1, 0x54, 0x03], "vandpd xmm0, xmm3, xmmword [rbx]"),
        testcase!(features { AVX: true } &[0xc5, 0xe0, 0x55, 0x03], "vandnps xmm0, xmm3, xmmword [rbx]"),
        testcase!(features { AVX: true } &[0xc5, 0xe1, 0x55, 0x03], "vandnpd xmm0, xmm3, xmmword [rbx]"),
        testcase!(features { AVX: true } &[0xc5, 0xe0, 0x56, 0x03], "vorps xmm0, xmm3, xmmword [rbx]"),
        testcase!(features { AVX: true } &[0xc5, 0xe1, 0x56, 0x03], "vorpd xmm0, xmm3, xmmword [rbx]"),
        testcase!(features { AVX: true } &[0xc4, 0xa2, 0x15, 0x3e, 0x14, 0xb9], "vpmaxuw ymm2, ymm13, ymmword [rcx + r15 * 4]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod strange_prefixing {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x45, 0x66, 0x0f, 0x21, 0xc8], "mov rax, dr1"),
        testcase!(&[0x45, 0xf2, 0x0f, 0x21, 0xc8], "mov rax, dr1"),
        testcase!(&[0x45, 0xf3, 0x0f, 0x21, 0xc8], "mov rax, dr1"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod prefixed_0f {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x02, 0x01], "lar eax, word [rcx]"),
        testcase!(&[0x0f, 0x02, 0xc1], "lar eax, ecx"),
        testcase!(&[0x4f, 0x0f, 0x02, 0x01], "lar r8, word [r9]"),
        testcase!(&[0x4f, 0x0f, 0x02, 0xc1], "lar r8, r9"),
        testcase!(&[0x66, 0x0f, 0x02, 0x01], "lar ax, word [rcx]"),
        testcase!(&[0x66, 0x0f, 0x02, 0xc1], "lar ax, cx"),
        testcase!(&[0x0f, 0x03, 0x01], "lsl eax, word [rcx]"),
        testcase!(&[0x0f, 0x03, 0xc1], "lsl eax, ecx"),
        testcase!(&[0x48, 0x0f, 0x03, 0x01], "lsl rax, word [rcx]"),
        // capstone says `lsl rax, rcx`, but xed says `rax, ecx`. intel docs also say second reg should
        // be dword.
        testcase!(&[0x48, 0x0f, 0x03, 0xc1], "lsl rax, ecx", masm: "lsl rax, rcx"),
        testcase!(&[0x66, 0x0f, 0x03, 0x01], "lsl ax, word [rcx]"),
        testcase!(&[0x66, 0x0f, 0x03, 0xc1], "lsl ax, cx"),
        testcase!(&[0x0f, 0x05], "syscall"),
        testcase!(&[0x48, 0x0f, 0x05], "syscall"),
        testcase!(&[0x66, 0x0f, 0x05], "syscall"),
        testcase!(&[0x0f, 0x06], "clts"),
        testcase!(&[0xf2, 0x0f, 0x06], "clts", masm: "clts"),
        testcase!(&[0x0f, 0x07], "sysret"),
        testcase!(&[0xf2, 0x0f, 0x07], "sysret", masm: "sysret"),
        testcase!(&[0x0f, 0x12, 0x0f], "movlps xmm1, qword [rdi]"),
        testcase!(&[0x0f, 0x12, 0xcf], "movhlps xmm1, xmm7"),
        testcase!(&[0x0f, 0x16, 0x0f], "movhps xmm1, qword [rdi]"),
        testcase!(&[0x0f, 0x16, 0xcf], "movlhps xmm1, xmm7"),
        testcase!(&[0x0f, 0x12, 0xc0], "movhlps xmm0, xmm0"),
        testcase!(invalid: &[0x0f, 0x13, 0xc0]),
        testcase!(&[0x0f, 0x13, 0x00], "movlps qword [rax], xmm0"),
        testcase!(&[0x0f, 0x14, 0x08], "unpcklps xmm1, xmmword [rax]"),
        testcase!(&[0x0f, 0x15, 0x08], "unpckhps xmm1, xmmword [rax]"),
        testcase!(&[0x0f, 0x16, 0x0f], "movhps xmm1, qword [rdi]"),
        testcase!(&[0x0f, 0x16, 0xc0], "movlhps xmm0, xmm0"),
        testcase!(invalid: &[0x0f, 0x17, 0xc0]),
        testcase!(&[0x0f, 0x17, 0x00], "movhps qword [rax], xmm0"),
        testcase!(&[0x0f, 0x18, 0xc0], "nop eax"), // capstone says invalid, xed says nop, masm would call it prefetchnta rax..?
        testcase!(&[0x0f, 0x18, 0x00], "prefetchnta zmmword [rax]", masm: "prefetchnta [rax]"), // masm elides the memory size here..?
        testcase!(&[0x0f, 0x18, 0x08], "prefetcht0 zmmword [rax]", masm: "prefetcht0 [rax]"),
        testcase!(&[0x0f, 0x18, 0x10], "prefetcht1 zmmword [rax]", masm: "prefetcht1 [rax]"),
        testcase!(&[0x0f, 0x18, 0x18], "prefetcht2 zmmword [rax]", masm: "prefetcht2 [rax]"),
        testcase!(&[0x0f, 0x18, 0x20], "nop zmmword [rax]", masm: "nop zmmword ptr [rax]"), // TODO: this is now prefetchrst2
        testcase!(&[0x4f, 0x0f, 0x18, 0x20], "nop zmmword [r8]", masm: "nop zmmword ptr [r8]"), // TODO: this is now prefetchrst2
        testcase!(&[0x0f, 0x18, 0xcc], "nop esp"),
        testcase!(&[0x0f, 0x19, 0x20], "nop dword [rax]"),
        testcase!(&[0x0f, 0x1a, 0x20], "nop dword [rax]"),
        testcase!(&[0x0f, 0x1b, 0x20], "nop dword [rax]"),
        testcase!(&[0x0f, 0x1c, 0x20], "nop dword [rax]"),
        testcase!(&[0x0f, 0x1d, 0x20], "nop dword [rax]"),
        testcase!(&[0x0f, 0x1e, 0x20], "nop dword [rax]"),
        testcase!(&[0x0f, 0x1f, 0x20], "nop dword [rax]"),
        testcase!(invalid: &[0x45, 0x0f, 0x20, 0xc8]),
        testcase!(&[0x45, 0x0f, 0x20, 0xc0], "mov r8, cr8"),
        testcase!(invalid: &[0x0f, 0x20, 0xc8]),
        testcase!(invalid: &[0x45, 0x0f, 0x21, 0xc8]),
        testcase!(&[0x0f, 0x21, 0xc8], "mov rax, dr1"),
        testcase!(invalid: &[0x45, 0x0f, 0x22, 0xc8]),
        testcase!(&[0x45, 0x0f, 0x22, 0xc0], "mov cr8, r8"),
        testcase!(invalid: &[0x40, 0x0f, 0x22, 0xc8]),
        testcase!(invalid: &[0x0f, 0x22, 0xc8]),
        testcase!(&[0x0f, 0x22, 0xc0], "mov cr0, rax"),
        testcase!(invalid: &[0x44, 0x0f, 0x22, 0xcf]),
        testcase!(&[0x0f, 0x22, 0xc7], "mov cr0, rdi"),
        testcase!(invalid: &[0x0f, 0x22, 0xcf]),
        testcase!(invalid: &[0x45, 0x0f, 0x23, 0xc8]),
        testcase!(&[0x40, 0x0f, 0x23, 0xc8], "mov dr1, rax"),
        testcase!(&[0x0f, 0x23, 0xc8], "mov dr1, rax"),
        testcase!(invalid: &[0x44, 0x0f, 0x23, 0xcf]),
        testcase!(&[0x0f, 0x23, 0xcf], "mov dr1, rdi"),
        testcase!(&[0x0f, 0x30], "wrmsr"),
        testcase!(&[0x0f, 0x31], "rdtsc"),
        testcase!(&[0x0f, 0x32], "rdmsr"),
        testcase!(&[0x0f, 0x33], "rdpmc"),
        testcase!(&[0x0f, 0x34], "sysenter"),
        testcase!(&[0x0f, 0x35], "sysexit"),
        testcase!(invalid: &[0x0f, 0x36]),
        testcase!(&[0x0f, 0x37], "getsec"),
        testcase!(invalid: &[0x66, 0x0f, 0x37]),
        testcase!(invalid: &[0xf2, 0x0f, 0x37]),
        testcase!(invalid: &[0xf3, 0x0f, 0x37]),
        testcase!(&[0x0f, 0x60, 0x00], "punpcklbw mm0, dword [rax]"),
        testcase!(&[0x0f, 0x60, 0xc2], "punpcklbw mm0, mm2"),
        testcase!(&[0x0f, 0x61, 0x00], "punpcklwd mm0, dword [rax]"),
        testcase!(&[0x0f, 0x61, 0xc2], "punpcklwd mm0, mm2"),
        testcase!(&[0x0f, 0x62, 0x00], "punpckldq mm0, dword [rax]"),
        testcase!(&[0x0f, 0x62, 0xc2], "punpckldq mm0, mm2"),
        testcase!(&[0x0f, 0x63, 0x00], "packsswb mm0, qword [rax]"),
        testcase!(&[0x0f, 0x63, 0xc2], "packsswb mm0, mm2"),
        testcase!(&[0x0f, 0x64, 0x00], "pcmpgtb mm0, qword [rax]"),
        testcase!(&[0x0f, 0x64, 0xc2], "pcmpgtb mm0, mm2"),
        testcase!(&[0x0f, 0x65, 0x00], "pcmpgtw mm0, qword [rax]"),
        testcase!(&[0x0f, 0x65, 0xc2], "pcmpgtw mm0, mm2"),
        testcase!(&[0x0f, 0x66, 0x00], "pcmpgtd mm0, qword [rax]"),
        testcase!(&[0x0f, 0x66, 0xc2], "pcmpgtd mm0, mm2"),
        testcase!(&[0x0f, 0x67, 0x00], "packuswb mm0, qword [rax]"),
        testcase!(&[0x0f, 0x67, 0xc2], "packuswb mm0, mm2"),
        testcase!(&[0x0f, 0x68, 0x00], "punpckhbw mm0, qword [rax]"),
        testcase!(&[0x0f, 0x68, 0xc2], "punpckhbw mm0, mm2"),
        testcase!(&[0x0f, 0x69, 0x00], "punpckhwd mm0, qword [rax]"),
        testcase!(&[0x0f, 0x69, 0xc2], "punpckhwd mm0, mm2"),
        testcase!(&[0x0f, 0x6a, 0x00], "punpckhdq mm0, qword [rax]"),
        testcase!(&[0x0f, 0x6a, 0xc2], "punpckhdq mm0, mm2"),
        testcase!(&[0x0f, 0x6b, 0x00], "packssdw mm0, qword [rax]"),
        testcase!(&[0x0f, 0x6b, 0xc2], "packssdw mm0, mm2"),
        testcase!(invalid: &[0x0f, 0x6c]),
        testcase!(invalid: &[0x0f, 0x6d]),
        testcase!(&[0x0f, 0x6e, 0x00], "movd mm0, dword [rax]"),
        testcase!(&[0x0f, 0x6e, 0xc2], "movd mm0, edx"),
        testcase!(&[0x0f, 0x6f, 0x00], "movq mm0, qword [rax]"),
        testcase!(&[0x0f, 0x6f, 0xc2], "movq mm0, mm2"),
        testcase!(&[0x0f, 0x6f, 0xfb], "movq mm7, mm3"),
        testcase!(&[0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [rax], 0x7f"),
        testcase!(&[0x4f, 0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [r8], 0x7f"),
        testcase!(invalid: &[0x0f, 0x71, 0x00, 0x7f]),
        testcase!(invalid: &[0x0f, 0x71, 0xc0, 0x7f]),
        testcase!(&[0x0f, 0x71, 0xd0, 0x7f], "psrlw mm0, 0x7f"),
        testcase!(&[0x0f, 0x71, 0xe0, 0x7f], "psraw mm0, 0x7f"),
        testcase!(&[0x0f, 0x71, 0xf0, 0x7f], "psllw mm0, 0x7f"),
        testcase!(invalid: &[0x0f, 0x72, 0x00, 0x7f]),
        testcase!(invalid: &[0x0f, 0x72, 0xc0, 0x7f]),
        testcase!(&[0x0f, 0x72, 0xd0, 0x7f], "psrld mm0, 0x7f"),
        testcase!(&[0x0f, 0x72, 0xe0, 0x7f], "psrad mm0, 0x7f"),
        testcase!(&[0x0f, 0x72, 0xf0, 0x7f], "pslld mm0, 0x7f"),
        testcase!(invalid: &[0x0f, 0x73, 0x00, 0x7f]),
        testcase!(invalid: &[0x0f, 0x73, 0xc0, 0x7f]),
        testcase!(&[0x0f, 0x73, 0xd0, 0x7f], "psrlq mm0, 0x7f"),
        testcase!(invalid: &[0x0f, 0x73, 0xe0, 0x7f]),
        testcase!(&[0x0f, 0x73, 0xf0, 0x7f], "psllq mm0, 0x7f"),
        testcase!(&[0x0f, 0xa0], "push fs"),
        testcase!(&[0x0f, 0xa1], "pop fs"),
        testcase!(&[0x0f, 0xa2], "cpuid"),
        testcase!(&[0x0f, 0xa4, 0xc0, 0x11], "shld eax, eax, 0x11"),
        testcase!(&[0x66, 0x0f, 0xa4, 0xcf, 0x11], "shld di, cx, 0x11"),
        testcase!(&[0x66, 0x45, 0x0f, 0xa4, 0xcf, 0x11], "shld r15w, r9w, 0x11"),
        testcase!(&[0x0f, 0xa5, 0xc0], "shld eax, eax, cl"),
        testcase!(&[0x0f, 0xa5, 0xc9], "shld ecx, ecx, cl"),
        testcase!(&[0x0f, 0xac, 0xc0, 0x11], "shrd eax, eax, 0x11"),
        testcase!(&[0x66, 0x0f, 0xac, 0xcf, 0x11], "shrd di, cx, 0x11"),
        testcase!(&[0x66, 0x45, 0x0f, 0xac, 0xcf, 0x11], "shrd r15w, r9w, 0x11"),
        testcase!(&[0x0f, 0xad, 0xc9], "shrd ecx, ecx, cl"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod prefixed_660f {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x0f, 0x10, 0xc0], "movupd xmm0, xmm0"),
        testcase!(&[0x66, 0x48, 0x0f, 0x10, 0xc0], "movupd xmm0, xmm0"),
        testcase!(&[0x66, 0x4a, 0x0f, 0x10, 0xc0], "movupd xmm0, xmm0"),
        testcase!(&[0x66, 0x4b, 0x0f, 0x10, 0xc0], "movupd xmm0, xmm8"),
        testcase!(&[0x66, 0x4c, 0x0f, 0x10, 0xc0], "movupd xmm8, xmm0"),
        testcase!(&[0x66, 0x4d, 0x0f, 0x10, 0xc0], "movupd xmm8, xmm8"),
        testcase!(&[0xf2, 0x66, 0x66, 0x4d, 0x0f, 0x10, 0xc0], "movsd xmm8, xmm8"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod prefixed_f20f {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(invalid: &[0xf2, 0x0f, 0x16, 0xcf]),
        testcase!(invalid: &[0xf2, 0x4d, 0x0f, 0x16, 0xcf]),
        testcase!(invalid: &[0x40, 0x66, 0xf2, 0x66, 0x4d, 0x0f, 0x16, 0xcf]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod prefixed_f30f {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x16, 0xcf], "movshdup xmm1, xmm7"),
        testcase!(&[0xf3, 0x4d, 0x0f, 0x16, 0xcf], "movshdup xmm9, xmm15"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0xfa], "endbr64"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0xfb], "endbr32"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0xfc], "nop esp, edi"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod only_64bit {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xae], "scas byte es:[rdi], al"),
        testcase!(&[0xaf], "scas dword es:[rdi], eax"),
        testcase!(&[0x67, 0xaf], "scas dword es:[edi], eax"),
        testcase!(&[0x67, 0xac], "lods al, byte [esi]"),
        testcase!(&[0x67, 0xaa], "stos byte es:[edi], al"),
        // note that rax.b does *not* change the register
        testcase!(&[0x4f, 0xa0, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34], "mov al, byte [0x3412341234123412]"),
        testcase!(&[0x4f, 0xa1, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34], "mov rax, qword [0x3412341234123412]"),
        testcase!(&[0x4f, 0xa2, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34], "mov byte [0x3412341234123412], al"),
        testcase!(&[0x4f, 0xa3, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34], "mov qword [0x3412341234123412], rax"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod adx {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x0f, 0x38, 0xf6, 0xc1], "adcx eax, ecx"),
        testcase!(&[0x66, 0x0f, 0x38, 0xf6, 0x01], "adcx eax, dword [rcx]"),
        testcase!(&[0x66, 0x4f, 0x0f, 0x38, 0xf6, 0x01], "adcx r8, qword [r9]"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xf6, 0xc1], "adox eax, ecx"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xf6, 0x01], "adox eax, dword [rcx]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0x38, 0xf6, 0x01], "adox r8, qword [r9]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod prefetchw {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x0d, 0x08], "prefetchw zmmword [rax]"),
        testcase!(&[0x0f, 0x0d, 0x00], "nop zmmword [rax]"),
        testcase!(invalid: &[0x0f, 0x0d, 0xc0]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod lzcnt {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0xf3, 0x0f, 0xbd, 0xc1], "lzcnt ax, cx"),
        testcase!(&[0xf3, 0x0f, 0xbd, 0xc1], "lzcnt eax, ecx"),
        testcase!(&[0xf3, 0x48, 0x0f, 0xbd, 0xc1], "lzcnt rax, rcx"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod svm {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x01, 0xdf], "invlpga rax, ecx"),
        testcase!(&[0x0f, 0x01, 0xde], "skinit eax"),
        testcase!(&[0x0f, 0x01, 0xdd], "clgi"),
        testcase!(&[0x0f, 0x01, 0xdc], "stgi"),
        testcase!(&[0x0f, 0x01, 0xdb], "vmsave rax"),
        testcase!(&[0x0f, 0x01, 0xda], "vmload rax"),
        testcase!(&[0x0f, 0x01, 0xd9], "vmmcall"),
        testcase!(&[0x0f, 0x01, 0xd8], "vmrun rax"),
        testcase!(&[0x0f, 0x78, 0xc4], "vmread rsp, rax"),
        testcase!(&[0x0f, 0x79, 0xc5], "vmwrite rax, rbp"),
        testcase!(&[0x0f, 0x78, 0x0b], "vmread qword [rbx], rcx"),
        testcase!(invalid: &[0x66, 0x0f, 0x78, 0x03]),
        testcase!(&[0x0f, 0x79, 0x0b], "vmwrite rcx, qword [rbx]"),
        testcase!(invalid: &[0x66, 0x0f, 0x79, 0x03]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod movbe {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x38, 0xf0, 0x06], "movbe eax, dword [rsi]"),
        testcase!(&[0x4f, 0x0f, 0x38, 0xf0, 0x06], "movbe r8, qword [r14]"),
        testcase!(invalid: &[0x4f, 0x0f, 0x38, 0xf0, 0xc6]),
        testcase!(&[0x0f, 0x38, 0xf1, 0x06], "movbe dword [rsi], eax"),
        testcase!(&[0x4f, 0x0f, 0x38, 0xf1, 0x06], "movbe qword [r14], r8"),
        testcase!(&[0x66, 0x0f, 0x38, 0xf1, 0x06], "movbe word [rsi], ax"),
        testcase!(invalid: &[0x66, 0x0f, 0x38, 0xf1, 0xc6]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod tsx {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xc6, 0xf8, 0x10], "xabort 0x10"),
        testcase!(&[0xc7, 0xf8, 0x10, 0x12, 0x34, 0x56], "xbegin $+0x56341210"),
        testcase!(&[0x66, 0xc7, 0xf8, 0x10, 0x12], "xbegin $+0x1210"),
        testcase!(&[0x0f, 0x01, 0xd5], "xend"),
        testcase!(&[0x0f, 0x01, 0xd6], "xtest"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod x86_rand {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0xc7, 0xfd], "rdseed ebp"),
        testcase!(&[0x66, 0x0f, 0xc7, 0xfd], "rdseed bp"),
        testcase!(&[0x48, 0x0f, 0xc7, 0xfd], "rdseed rbp"),
        testcase!(&[0x0f, 0xc7, 0xf5], "rdrand ebp"),
        testcase!(&[0x66, 0x0f, 0xc7, 0xf5], "rdrand bp"),
        testcase!(&[0x48, 0x0f, 0xc7, 0xf5], "rdrand rbp"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sha {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x3a, 0xcc, 0x12, 0x40], "sha1rnds4 xmm2, xmmword [rdx], 0x40"),
        testcase!(&[0x0f, 0x3a, 0xcc, 0x12, 0xff], "sha1rnds4 xmm2, xmmword [rdx], 0xff"),
        testcase!(&[0x0f, 0x38, 0xc8, 0x12], "sha1nexte xmm2, xmmword [rdx]"),
        testcase!(&[0x0f, 0x38, 0xc9, 0x12], "sha1msg1 xmm2, xmmword [rdx]"),
        testcase!(&[0x0f, 0x38, 0xca, 0x12], "sha1msg2 xmm2, xmmword [rdx]"),
        testcase!(&[0x0f, 0x38, 0xcb, 0x12], "sha256rnds2 xmm2, xmmword [rdx]"),
        testcase!(&[0x0f, 0x38, 0xcc, 0x12], "sha256msg1 xmm2, xmmword [rdx]"),
        testcase!(&[0x0f, 0x38, 0xcd, 0x12], "sha256msg2 xmm2, xmmword [rdx]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod vmx {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0xc7, 0x3f], "vmptrst qword [rdi]"),
        testcase!(&[0x0f, 0xc7, 0x37], "vmptrld qword [rdi]"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0x37], "vmxon qword [rdi]"),
        testcase!(&[0x66, 0x0f, 0xc7, 0xf7], "rdrand di"),
        testcase!(&[0x66, 0x0f, 0xc7, 0x37], "vmclear qword [rdi]"),

        // this is actually vmx
        // testcase!(invalid: &[0x66, 0x0f, 0xc7, 0x03]),
        testcase!(&[0x66, 0x4f, 0x0f, 0xc7, 0x33], "vmclear qword [r11]"),
        testcase!(&[0x66, 0x0f, 0xc7, 0x33], "vmclear qword [rbx]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xc7, 0x33], "vmxon qword [r11]"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0x33], "vmxon qword [rbx]"),

        // these need vmx and invept features
        testcase!(features { VMX: true } &[0x66, 0x0f, 0x38, 0x80, 0x01], "invept rax, xmmword [rcx]"),
        testcase!(features { VMX: true } &[0x66, 0x0f, 0x38, 0x81, 0x01], "invvpid rax, xmmword [rcx]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod rdpid {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0xc7, 0xfd], "rdpid rbp"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod cmpxchg8b {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0xc7, 0x0f], "cmpxchg8b qword [rdi]"),
        testcase!(&[0xf2, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [rdi]"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [rdi]"),
        testcase!(&[0x66, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [rdi]"),
        testcase!(&[0x4f, 0x0f, 0xc7, 0x0f], "cmpxchg16b xmmword [r15]"),
        testcase!(&[0xf2, 0x4f, 0x0f, 0xc7, 0x0f], "cmpxchg16b xmmword [r15]"),
        testcase!(&[0xf3, 0x4f, 0x0f, 0xc7, 0x0f], "cmpxchg16b xmmword [r15]"),
        testcase!(&[0x66, 0x4f, 0x0f, 0xc7, 0x0f], "cmpxchg16b xmmword [r15]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod x87 {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
    //    testcase!(&[0xd8, 0x03], "fadd st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x03], "fadd st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0x0b], "fmul st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x0b], "fmul st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0x13], "fcom st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x13], "fcom st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0x1b], "fcomp st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x1b], "fcomp st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0x23], "fsub st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x23], "fsub st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0x2b], "fsubr st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x2b], "fsubr st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0x33], "fdiv st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x33], "fdiv st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0x3b], "fdivr st, dword ptr [rbx]"),
        testcase!(&[0xd8, 0x3b], "fdivr st(0), dword [rbx]"),
    //    testcase!(&[0xd8, 0xc3], "fadd st, st(3)"),
        testcase!(&[0xd8, 0xc3], "fadd st(0), st(3)", masm: "fadd st, st(3)"),
    //    testcase!(&[0xd8, 0xcb], "fmul st, st(3)"),
        testcase!(&[0xd8, 0xcb], "fmul st(0), st(3)"),
    //    testcase!(&[0xd8, 0xd3], "fcom st, st(3)"),
        testcase!(&[0xd8, 0xd3], "fcom st(0), st(3)"),
    //    testcase!(&[0xd8, 0xdb], "fcomp st, st(3)"),
        testcase!(&[0xd8, 0xdb], "fcomp st(0), st(3)"),
    //    testcase!(&[0xd8, 0xe3], "fsub st, st(3)"),
        testcase!(&[0xd8, 0xe3], "fsub st(0), st(3)"),
    //    testcase!(&[0xd8, 0xeb], "fsubr st, st(3)"),
        testcase!(&[0xd8, 0xeb], "fsubr st(0), st(3)"),
    //    testcase!(&[0xd8, 0xf3], "fdiv st, st(3)"),
        testcase!(&[0xd8, 0xf3], "fdiv st(0), st(3)"),
    //    testcase!(&[0xd8, 0xfb], "fdivr st, st(3)"),
        testcase!(&[0xd8, 0xfb], "fdivr st(0), st(3)"),
    //    testcase!(&[0xd9, 0x03], "fld st, dword ptr [rbx]"),
        testcase!(&[0xd9, 0x03], "fld st(0), dword [rbx]"),
        testcase!(invalid: &[0xd9, 0x08]),
        testcase!(invalid: &[0xd9, 0x09]),
        testcase!(invalid: &[0xd9, 0x0a]),
        testcase!(invalid: &[0xd9, 0x0b]),
        testcase!(invalid: &[0xd9, 0x0c]),
        testcase!(invalid: &[0xd9, 0x0d]),
        testcase!(invalid: &[0xd9, 0x0e]),
        testcase!(invalid: &[0xd9, 0x0f]),
    //    testcase!(&[0xd9, 0x13], "fst dword ptr [rbx], st"),
        testcase!(&[0xd9, 0x13], "fst dword [rbx], st(0)"),
    //    testcase!(&[0xd9, 0x1b], "fstp dword ptr [rbx], st"),
        testcase!(&[0xd9, 0x1b], "fstp dword [rbx], st(0)"),
    //    testcase!(&[0xd9, 0x23], "fldenv ptr [rbx]"),
        testcase!(&[0xd9, 0x23], "fldenv ptr [rbx]"),
    //    testcase!(&[0xd9, 0x2b], "fldcw word ptr [rbx]"),
        testcase!(&[0xd9, 0x2b], "fldcw word [rbx]"),
    //    testcase!(&[0xd9, 0x33], "fnstenv ptr [rbx]"),
        testcase!(&[0xd9, 0x33], "fnstenv ptr [rbx]"),
    //    testcase!(&[0xd9, 0x3b], "fnstcw word ptr [rbx]"),
        testcase!(&[0xd9, 0x3b], "fnstcw word [rbx]"),
    //    testcase!(&[0xd9, 0xc3], "fld st, st(3)"),
        testcase!(&[0xd9, 0xc3], "fld st(0), st(3)"),
    //    testcase!(&[0xd9, 0xcb], "fxch st, st(3)"),
        testcase!(&[0xd9, 0xcb], "fxch st(0), st(3)"),
        testcase!(&[0xd9, 0xd0], "fnop"),
        testcase!(invalid: &[0xd9, 0xd1]),
        testcase!(invalid: &[0xd9, 0xd2]),
        testcase!(invalid: &[0xd9, 0xd3]),
        testcase!(invalid: &[0xd9, 0xd4]),
        testcase!(invalid: &[0xd9, 0xd5]),
        testcase!(invalid: &[0xd9, 0xd6]),
        testcase!(invalid: &[0xd9, 0xd7]),
        // undocumented save for intel xed
    //    testcase!(&[0xd9, 0xdb], "fstpnce st(3), st"),
        testcase!(&[0xd9, 0xdb], "fstpnce st(3), st(0)"),
        testcase!(&[0xd9, 0xe0], "fchs"),
        testcase!(&[0xd9, 0xe1], "fabs"),
        testcase!(invalid: &[0xd9, 0xe2]),
        testcase!(invalid: &[0xd9, 0xe3]),
        testcase!(&[0xd9, 0xe4], "ftst"),
        testcase!(&[0xd9, 0xe5], "fxam"),
        testcase!(invalid: &[0xd9, 0xe6]),
        testcase!(invalid: &[0xd9, 0xe7]),
        testcase!(&[0xd9, 0xe8], "fld1"),
        testcase!(&[0xd9, 0xe9], "fldl2t"),
        testcase!(&[0xd9, 0xea], "fldl2e"),
        testcase!(&[0xd9, 0xeb], "fldpi"),
        testcase!(&[0xd9, 0xec], "fldlg2"),
        testcase!(&[0xd9, 0xed], "fldln2"),
        testcase!(&[0xd9, 0xee], "fldz"),
        testcase!(invalid: &[0xd9, 0xef]),
        testcase!(&[0xd9, 0xf0], "f2xm1"),
        testcase!(&[0xd9, 0xf1], "fyl2x"),
        testcase!(&[0xd9, 0xf2], "fptan"),
        testcase!(&[0xd9, 0xf3], "fpatan"),
        testcase!(&[0xd9, 0xf4], "fxtract"),
        testcase!(&[0xd9, 0xf5], "fprem1"),
        testcase!(&[0xd9, 0xf6], "fdecstp"),
        testcase!(&[0xd9, 0xf7], "fincstp"),
        testcase!(&[0xd9, 0xf8], "fprem"),
        testcase!(&[0xd9, 0xf9], "fyl2xp1"),
        testcase!(&[0xd9, 0xfa], "fsqrt"),
        testcase!(&[0xd9, 0xfb], "fsincos"),
        testcase!(&[0xd9, 0xfc], "frndint"),
        testcase!(&[0xd9, 0xfd], "fscale"),
        testcase!(&[0xd9, 0xfe], "fsin"),
        testcase!(&[0xd9, 0xff], "fcos"),
    //    testcase!(&[0xda, 0x03], "fiadd st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x03], "fiadd st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0x0b], "fimul st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x0b], "fimul st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0x13], "ficom st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x13], "ficom st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0x1b], "ficomp st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x1b], "ficomp st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0x23], "fisub st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x23], "fisub st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0x2b], "fisubr st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x2b], "fisubr st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0x33], "fidiv st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x33], "fidiv st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0x3b], "fidivr st, dword ptr [rbx]"),
        testcase!(&[0xda, 0x3b], "fidivr st(0), dword [rbx]"),
    //    testcase!(&[0xda, 0xc3], "fcmovb st, st(3)"),
        testcase!(&[0xda, 0xc3], "fcmovb st(0), st(3)"),
    //    testcase!(&[0xda, 0xcb], "fcmove st, st(3)"),
        testcase!(&[0xda, 0xcb], "fcmove st(0), st(3)"),
    //    testcase!(&[0xda, 0xd3], "fcmovbe st, st(3)"),
        testcase!(&[0xda, 0xd3], "fcmovbe st(0), st(3)"),
    //    testcase!(&[0xda, 0xdb], "fcmovu st, st(3)"),
        testcase!(&[0xda, 0xdb], "fcmovu st(0), st(3)"),
        testcase!(invalid: &[0xda, 0xe0]),
        testcase!(invalid: &[0xda, 0xe1]),
        testcase!(invalid: &[0xda, 0xe2]),
        testcase!(invalid: &[0xda, 0xe3]),
        testcase!(invalid: &[0xda, 0xe4]),
        testcase!(invalid: &[0xda, 0xe5]),
        testcase!(invalid: &[0xda, 0xe6]),
        testcase!(invalid: &[0xda, 0xe7]),
        testcase!(invalid: &[0xda, 0xe8]),
        testcase!(&[0xda, 0xe9], "fucompp"),
        testcase!(invalid: &[0xda, 0xea]),
        testcase!(invalid: &[0xda, 0xeb]),
        testcase!(invalid: &[0xda, 0xec]),
        testcase!(invalid: &[0xda, 0xed]),
        testcase!(invalid: &[0xda, 0xee]),
        testcase!(invalid: &[0xda, 0xef]),
        testcase!(invalid: &[0xda, 0xf0]),
        testcase!(invalid: &[0xda, 0xf1]),
        testcase!(invalid: &[0xda, 0xf2]),
        testcase!(invalid: &[0xda, 0xf3]),
        testcase!(invalid: &[0xda, 0xf4]),
        testcase!(invalid: &[0xda, 0xf5]),
        testcase!(invalid: &[0xda, 0xf6]),
        testcase!(invalid: &[0xda, 0xf7]),
        testcase!(invalid: &[0xda, 0xf8]),
        testcase!(invalid: &[0xda, 0xf9]),
        testcase!(invalid: &[0xda, 0xfa]),
        testcase!(invalid: &[0xda, 0xfb]),
        testcase!(invalid: &[0xda, 0xfc]),
        testcase!(invalid: &[0xda, 0xfd]),
        testcase!(invalid: &[0xda, 0xfe]),
        testcase!(invalid: &[0xda, 0xff]),
    //    testcase!(&[0xdb, 0x03], "fild st, dword ptr [rbx]"),
        testcase!(&[0xdb, 0x03], "fild st(0), dword [rbx]"),
    //    testcase!(&[0xdb, 0x0b], "fisttp dword ptr [rbx], st"),
        testcase!(&[0xdb, 0x0b], "fisttp dword [rbx], st(0)"),
    //    testcase!(&[0xdb, 0x13], "fist dword ptr [rbx], st"),
        testcase!(&[0xdb, 0x13], "fist dword [rbx], st(0)"),
    //    testcase!(&[0xdb, 0x1b], "fistp dword ptr [rbx], st"),
        testcase!(&[0xdb, 0x1b], "fistp dword [rbx], st(0)"),
        testcase!(invalid: &[0xdb, 0x20]),
        testcase!(invalid: &[0xdb, 0x21]),
        testcase!(invalid: &[0xdb, 0x22]),
        testcase!(invalid: &[0xdb, 0x23]),
        testcase!(invalid: &[0xdb, 0x24]),
        testcase!(invalid: &[0xdb, 0x25]),
        testcase!(invalid: &[0xdb, 0x26]),
        testcase!(invalid: &[0xdb, 0x27]),
    //    testcase!(&[0xdb, 0x2b], "fld st, ptr [rbx]"),
        testcase!(&[0xdb, 0x2b], "fld st(0), mword [rbx]"),
        testcase!(invalid: &[0xdb, 0x30]),
        testcase!(invalid: &[0xdb, 0x31]),
        testcase!(invalid: &[0xdb, 0x32]),
        testcase!(invalid: &[0xdb, 0x33]),
        testcase!(invalid: &[0xdb, 0x34]),
        testcase!(invalid: &[0xdb, 0x35]),
        testcase!(invalid: &[0xdb, 0x36]),
        testcase!(invalid: &[0xdb, 0x37]),
    //    testcase!(&[0xdb, 0x3b], "fstp ptr [rbx], st"),
        testcase!(&[0xdb, 0x3b], "fstp mword [rbx], st(0)"),
    //    testcase!(&[0xdb, 0xc3], "fcmovnb st, st(3)"),
        testcase!(&[0xdb, 0xc3], "fcmovnb st(0), st(3)"),
    //    testcase!(&[0xdb, 0xcb], "fcmovne st, st(3)"),
        testcase!(&[0xdb, 0xcb], "fcmovne st(0), st(3)"),
    //    testcase!(&[0xdb, 0xd3], "fcmovnbe st, st(3)"),
        testcase!(&[0xdb, 0xd3], "fcmovnbe st(0), st(3)"),
    //    testcase!(&[0xdb, 0xdb], "fcmovnu st, st(3)"),
        testcase!(&[0xdb, 0xdb], "fcmovnu st(0), st(3)"),
        testcase!(&[0xdb, 0xe0], "feni8087_nop", masm: "feni"),
        testcase!(&[0xdb, 0xe1], "fdisi8087_nop", masm: "fdisi"),
        testcase!(&[0xdb, 0xe2], "fnclex"),
        testcase!(&[0xdb, 0xe3], "fninit"),
        testcase!(&[0xdb, 0xe4], "fsetpm287_nop", masm: "fsetpm"),
        testcase!(invalid: &[0xdb, 0xe5]),
        testcase!(invalid: &[0xdb, 0xe6]),
        testcase!(invalid: &[0xdb, 0xe7]),
    //    testcase!(&[0xdb, 0xeb], "fucomi st, st(3)"),
        testcase!(&[0xdb, 0xeb], "fucomi st(0), st(3)"),
    //    testcase!(&[0xdb, 0xf3], "fcomi st, st(3)"),
        testcase!(&[0xdb, 0xf3], "fcomi st(0), st(3)"),
        testcase!(invalid: &[0xdb, 0xf8]),
        testcase!(invalid: &[0xdb, 0xf9]),
        testcase!(invalid: &[0xdb, 0xfa]),
        testcase!(invalid: &[0xdb, 0xfb]),
        testcase!(invalid: &[0xdb, 0xfc]),
        testcase!(invalid: &[0xdb, 0xfd]),
        testcase!(invalid: &[0xdb, 0xfe]),
        testcase!(invalid: &[0xdb, 0xff]),
    //    testcase!(&[0xdc, 0x03], "fadd st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x03], "fadd st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0x0b], "fmul st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x0b], "fmul st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0x13], "fcom st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x13], "fcom st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0x1b], "fcomp st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x1b], "fcomp st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0x23], "fsub st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x23], "fsub st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0x2b], "fsubr st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x2b], "fsubr st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0x33], "fdiv st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x33], "fdiv st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0x3b], "fdivr st, qword ptr [rbx]"),
        testcase!(&[0xdc, 0x3b], "fdivr st(0), qword [rbx]"),
    //    testcase!(&[0xdc, 0xc3], "fadd st(3), st"),
        testcase!(&[0xdc, 0xc3], "fadd st(3), st(0)"),
    //    testcase!(&[0xdc, 0xcb], "fmul st(3), st"),
        testcase!(&[0xdc, 0xcb], "fmul st(3), st(0)"),
    //    testcase!(&[0xdc, 0xd3], "fcom st, st(3)"),
        testcase!(&[0xdc, 0xd3], "fcom st(0), st(3)"),
    //    testcase!(&[0xdc, 0xdb], "fcomp st, st(3)"),
        testcase!(&[0xdc, 0xdb], "fcomp st(0), st(3)"),
    //    testcase!(&[0xdc, 0xe3], "fsubr st(3), st"),
        testcase!(&[0xdc, 0xe3], "fsubr st(3), st(0)"),
    //    testcase!(&[0xdc, 0xeb], "fsub st(3), st"),
        testcase!(&[0xdc, 0xeb], "fsub st(3), st(0)"),
    //    testcase!(&[0xdc, 0xf3], "fdivr st(3), st"),
        testcase!(&[0xdc, 0xf3], "fdivr st(3), st(0)"),
    //    testcase!(&[0xdc, 0xfb], "fdiv st(3), st"),
        testcase!(&[0xdc, 0xfb], "fdiv st(3), st(0)"),
    //    testcase!(&[0xdd, 0x03], "fld st, qword ptr [rbx]"),
        testcase!(&[0xdd, 0x03], "fld st(0), qword [rbx]"),
    //    testcase!(&[0xdd, 0x0b], "fisttp qword ptr [rbx], st"),
        testcase!(&[0xdd, 0x0b], "fisttp qword [rbx], st(0)"),
    //    testcase!(&[0xdd, 0x13], "fst qword ptr [rbx], st"),
        testcase!(&[0xdd, 0x13], "fst qword [rbx], st(0)"),
    //    testcase!(&[0xdd, 0x1b], "fstp qword ptr [rbx], st"),
        testcase!(&[0xdd, 0x1b], "fstp qword [rbx], st(0)"),
    //    testcase!(&[0xdd, 0x23], "frstor ptr [rbx]"),
        testcase!(&[0xdd, 0x23], "frstor ptr [rbx]"),
        testcase!(invalid: &[0xdd, 0x28]),
        testcase!(invalid: &[0xdd, 0x29]),
        testcase!(invalid: &[0xdd, 0x2a]),
        testcase!(invalid: &[0xdd, 0x2b]),
        testcase!(invalid: &[0xdd, 0x2c]),
        testcase!(invalid: &[0xdd, 0x2d]),
        testcase!(invalid: &[0xdd, 0x2e]),
        testcase!(invalid: &[0xdd, 0x2f]),
    //    testcase!(&[0xdd, 0x33], "fnsave ptr [rbx]"),
        testcase!(&[0xdd, 0x33], "fnsave ptr [rbx]"),
    //    testcase!(&[0xdd, 0x3b], "fnstsw word ptr [rbx]"),
        testcase!(&[0xdd, 0x3b], "fnstsw word [rbx]"),
        testcase!(&[0xdd, 0xc3], "ffree st(3)"),
    //    testcase!(&[0xdd, 0xcb], "fxch st, st(3)"),
        testcase!(&[0xdd, 0xcb], "fxch st(0), st(3)"),
    //    testcase!(&[0xdd, 0xd3], "fst st(3), st"),
        testcase!(&[0xdd, 0xd3], "fst st(3), st(0)"),
    //    testcase!(&[0xdd, 0xdb], "fstp st(3), st"),
        testcase!(&[0xdd, 0xdb], "fstp st(3), st(0)"),
    //    testcase!(&[0xdd, 0xe3], "fucom st, st(3)"),
        testcase!(&[0xdd, 0xe3], "fucom st(0), st(3)"),
    //    testcase!(&[0xdd, 0xeb], "fucomp st, st(3)"),
        testcase!(&[0xdd, 0xeb], "fucomp st(0), st(3)"),
        testcase!(invalid: &[0xdd, 0xf0]),
        testcase!(invalid: &[0xdd, 0xf1]),
        testcase!(invalid: &[0xdd, 0xf2]),
        testcase!(invalid: &[0xdd, 0xf3]),
        testcase!(invalid: &[0xdd, 0xf4]),
        testcase!(invalid: &[0xdd, 0xf5]),
        testcase!(invalid: &[0xdd, 0xf6]),
        testcase!(invalid: &[0xdd, 0xf7]),
        testcase!(invalid: &[0xdd, 0xf8]),
        testcase!(invalid: &[0xdd, 0xf9]),
        testcase!(invalid: &[0xdd, 0xfa]),
        testcase!(invalid: &[0xdd, 0xfb]),
        testcase!(invalid: &[0xdd, 0xfc]),
        testcase!(invalid: &[0xdd, 0xfd]),
        testcase!(invalid: &[0xdd, 0xfe]),
        testcase!(invalid: &[0xdd, 0xff]),
    //    testcase!(&[0xde, 0x03], "fiadd st, word ptr [rbx]"),
        testcase!(&[0xde, 0x03], "fiadd st(0), word [rbx]"),
    //    testcase!(&[0xde, 0x0b], "fimul st, word ptr [rbx]"),
        testcase!(&[0xde, 0x0b], "fimul st(0), word [rbx]"),
    //    testcase!(&[0xde, 0x13], "ficom st, word ptr [rbx]"),
        testcase!(&[0xde, 0x13], "ficom st(0), word [rbx]"),
    //    testcase!(&[0xde, 0x1b], "ficomp st, word ptr [rbx]"),
        testcase!(&[0xde, 0x1b], "ficomp st(0), word [rbx]"),
    //    testcase!(&[0xde, 0x23], "fisub st, word ptr [rbx]"),
        testcase!(&[0xde, 0x23], "fisub st(0), word [rbx]"),
    //    testcase!(&[0xde, 0x2b], "fisubr st, word ptr [rbx]"),
        testcase!(&[0xde, 0x2b], "fisubr st(0), word [rbx]"),
    //    testcase!(&[0xde, 0x33], "fidiv st, word ptr [rbx]"),
        testcase!(&[0xde, 0x33], "fidiv st(0), word [rbx]"),
    //    testcase!(&[0xde, 0x3b], "fidivr st, word ptr [rbx]"),
        testcase!(&[0xde, 0x3b], "fidivr st(0), word [rbx]"),
    //    testcase!(&[0xde, 0xc3], "faddp st(3), st"),
        testcase!(&[0xde, 0xc3], "faddp st(3), st(0)"),
    //    testcase!(&[0xde, 0xcb], "fmulp st(3), st"),
        testcase!(&[0xde, 0xcb], "fmulp st(3), st(0)"),
    //    testcase!(&[0xde, 0xd3], "fcomp st, st(3)"),
        testcase!(&[0xde, 0xd3], "fcomp st(0), st(3)"),
        testcase!(invalid: &[0xde, 0xd8]),
        testcase!(&[0xde, 0xd9], "fcompp"),
        testcase!(invalid: &[0xde, 0xda]),
        testcase!(invalid: &[0xde, 0xdb]),
        testcase!(invalid: &[0xde, 0xdc]),
        testcase!(invalid: &[0xde, 0xdd]),
        testcase!(invalid: &[0xde, 0xde]),
        testcase!(invalid: &[0xde, 0xdf]),
    //    testcase!(&[0xde, 0xe3], "fsubrp st(3), st"),
        testcase!(&[0xde, 0xe3], "fsubrp st(3), st(0)"),
    //    testcase!(&[0xde, 0xeb], "fsubp st(3), st"),
        testcase!(&[0xde, 0xeb], "fsubp st(3), st(0)"),
    //    testcase!(&[0xde, 0xf3], "fdivrp st(3), st"),
        testcase!(&[0xde, 0xf3], "fdivrp st(3), st(0)"),
    //    testcase!(&[0xde, 0xfb], "fdivp st(3), st"),
        testcase!(&[0xde, 0xfb], "fdivp st(3), st(0)"),
    //    testcase!(&[0xdf, 0x03], "fild st, word ptr [rbx]"),
        testcase!(&[0xdf, 0x03], "fild st(0), word [rbx]"),
    //    testcase!(&[0xdf, 0x0b], "fisttp word ptr [rbx], st"),
        testcase!(&[0xdf, 0x0b], "fisttp word [rbx], st(0)"),
    //    testcase!(&[0xdf, 0x13], "fist word ptr [rbx], st"),
        testcase!(&[0xdf, 0x13], "fist word [rbx], st(0)"),
    //    testcase!(&[0xdf, 0x1b], "fistp word ptr [rbx], st"),
        testcase!(&[0xdf, 0x1b], "fistp word [rbx], st(0)"),
    //    testcase!(&[0xdf, 0x23], "fbld st, ptr [rbx]"),
        testcase!(&[0xdf, 0x23], "fbld st(0), mword [rbx]"),
    //    testcase!(&[0xdf, 0x2b], "fild st, qword ptr [rbx]"),
        testcase!(&[0xdf, 0x2b], "fild st(0), qword [rbx]"),
    //    testcase!(&[0xdf, 0x33], "fbstp ptr [rbx], st"),
        testcase!(&[0xdf, 0x33], "fbstp mword [rbx], st(0)"),
    //    testcase!(&[0xdf, 0x3b], "fistp qword ptr [rbx], st"),
        testcase!(&[0xdf, 0x3b], "fistp qword [rbx], st(0)"),
    //    testcase!(&[0xdf, 0xc3], "ffreep st(3)"),
        testcase!(&[0xdf, 0xc3], "ffreep st(3)"),
    //    testcase!(&[0xdf, 0xcb], "fxch st, st(3)"),
        testcase!(&[0xdf, 0xcb], "fxch st(0), st(3)"),
    //    testcase!(&[0xdf, 0xd3], "fstp st(3), st"),
        testcase!(&[0xdf, 0xd3], "fstp st(3), st(0)"),
    //    testcase!(&[0xdf, 0xdb], "fstp st(3), st"),
        testcase!(&[0xdf, 0xdb], "fstp st(3), st(0)"),
        testcase!(&[0xdf, 0xe0], "fnstsw ax"),
        testcase!(invalid: &[0xdf, 0xe1]),
        testcase!(invalid: &[0xdf, 0xe2]),
        testcase!(invalid: &[0xdf, 0xe3]),
        testcase!(invalid: &[0xdf, 0xe4]),
        testcase!(invalid: &[0xdf, 0xe5]),
        testcase!(invalid: &[0xdf, 0xe6]),
        testcase!(invalid: &[0xdf, 0xe7]),
    //    testcase!(&[0xdf, 0xeb], "fucomip st, st(3)"),
        testcase!(&[0xdf, 0xeb], "fucomip st(0), st(3)"),
    //    testcase!(&[0xdf, 0xf3], "fcomip st, st(3)"),
        testcase!(&[0xdf, 0xf3], "fcomip st(0), st(3)"),
        testcase!(invalid: &[0xdf, 0xf8]),
        testcase!(invalid: &[0xdf, 0xf9]),
        testcase!(invalid: &[0xdf, 0xfa]),
        testcase!(invalid: &[0xdf, 0xfb]),
        testcase!(invalid: &[0xdf, 0xfc]),
        testcase!(invalid: &[0xdf, 0xfd]),
        testcase!(invalid: &[0xdf, 0xfe]),
        testcase!(invalid: &[0xdf, 0xff]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod mishegos_finds {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(invalid: &[0xc5, 0x8c, 0x77]),
        testcase!(&[0x0f, 0xfc, 0xaf, 0x40, 0x38, 0x25, 0xbf], "paddb mm5, qword [rdi - 0x40dac7c0]"),
        testcase!(invalid: &[0xc5, 0x4d, 0x16, 0x0f]),
        testcase!(invalid: &[0xf3, 0x67, 0x0f, 0x3a, 0xf0, 0xfb, 0xb4]),
    // XOP is still not supported
    //    testcase!(&[0xc4, 0x63, 0x91, 0x7f, 0x2f, 0x2e], "vfnmsubsd xmm13, xmm13, xmm2, qword ptr [rdi]"),
        testcase!(invalid: &[0x62, 0xf1, 0x56, 0xfe, 0x58, 0x04, 0xca]),
        testcase!(invalid: &[0x66, 0xf3, 0x36, 0x65, 0x0f, 0x3a, 0xf0, 0xee, 0x7a]),
        testcase!(&[0x62, 0x42, 0xd5, 0x9d, 0x97, 0xf6], "vfmsubadd132pd zmm30{k5}{z}{rn-sae}, zmm5, zmm14"),
        testcase!(invalid: &[0x67, 0x66, 0x42, 0x0f, 0x01, 0xfe]),
        testcase!(&[0x62, 0x52, 0x05, 0xff, 0xad, 0xfd], "vfnmadd213ss xmm15{k7}{z}{rz-sae}, xmm15, xmm13"),
        testcase!(invalid: &[0xf2, 0x67, 0x4a, 0x0f, 0x01, 0xd6]),
        testcase!(invalid: &[0x36, 0x64, 0x62, 0x33, 0x39, 0xef, 0x55, 0xc2, 0x68]),
        testcase!(invalid: &[0x36, 0x66, 0x67, 0xf3, 0x0f, 0x01, 0xce]),
        testcase!(invalid: &[0x62, 0x0f, 0xc1, 0x35, 0x38, 0xf8, 0xc8]),
        testcase!(invalid: &[0x66, 0x2e, 0x64, 0x26, 0x0f, 0x01, 0xc1]),
        testcase!(invalid: &[0x2e, 0x2e, 0xf2, 0x36, 0x40, 0x0e, 0xb2, 0xdb, 0x42, 0xd6, 0xa3, 0x16]),
        testcase!(invalid: &[0x2e, 0xf2, 0x36, 0x40, 0x0f, 0xb2, 0xdb, 0x42, 0xd6, 0xa3, 0x16]),
        testcase!(invalid: &[0x3e, 0x3e, 0x3e, 0x66, 0x4b, 0x35, 0x58, 0x3e]),
        testcase!(&[0xf2, 0xf3, 0x66, 0x65, 0x4f, 0x25, 0x9b, 0x5e, 0xda, 0x44], "and rax, 0x44da5e9b"),
        testcase!(&[0x65, 0x67, 0x65, 0x65, 0x0f, 0x0e], "femms"),
        testcase!(&[0x26, 0x66, 0x67, 0x41, 0x0f, 0x38, 0xdf, 0xe4], "aesdeclast xmm4, xmm12"),
        testcase!(&[0x65, 0x66, 0x66, 0x64, 0x48, 0x0f, 0x38, 0xdb, 0x0f], "aesimc xmm1, xmmword fs:[rdi]"),
        testcase!(invalid: &[0xf3, 0xf2, 0x41, 0x0f, 0xae, 0x8f, 0x54, 0x3c, 0x58, 0xb7]),
        /*
        testcase!(&[652e662e0f3814ff], "blendvps"),
        testcase!(&[66666565450f3acf2b4b], "gf2 "),
        */

        // might just be yax trying to do a f20f decode when it should not be f2
        // impossible instruction if operands could be read: lock is illegal here.
        // testcase!(&[f06565f2640f16], "???"),
    //    testcase!(&[0x0f, 0x38, 0xf6, 0x8c, 0x98, 0x4d, 0x33, 0xf5, 0xd3, ], "wrssd"),
        testcase!(&[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c], "pi2fw mm6, qword [rax - 0x5]"),
        testcase!(&[0x0f, 0xc7, 0x0f], "cmpxchg8b qword [rdi]"),
        testcase!(&[0x4f, 0x0f, 0xc7, 0x0f], "cmpxchg16b xmmword [r15]"),
        testcase!(&[0x66, 0x3e, 0x26, 0x2e, 0x2e, 0x0f, 0x38, 0x2a, 0x2b, ], "movntdqa xmm5, xmmword [rbx]"),
        testcase!(&[0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c, 0xf0, 0x63, ], "blendpd xmm7, xmmword [eax - 0xf83d010], 0x63"),
        testcase!(&[0x66, 0x66, 0x64, 0x3e, 0x0f, 0x38, 0x23, 0x9d, 0x69, 0x0f, 0xa8, 0x2d, ], "pmovsxwd xmm3, qword fs:[rbp + 0x2da80f69]"),
        testcase!(&[0x2e, 0x66, 0x26, 0x64, 0x49, 0x0f, 0x3a, 0x21, 0x0b, 0xb1, ], "insertps xmm1, dword fs:[r11], 0xb1"),
        testcase!(&[0x66, 0x26, 0x45, 0x0f, 0x3a, 0x42, 0x96, 0x74, 0x29, 0x96, 0xf9, 0x6a], "mpsadbw xmm10, xmmword [r14 - 0x669d68c], 0x6a"),
        testcase!(&[0x67, 0x26, 0x66, 0x65, 0x0f, 0x38, 0x3f, 0x9d, 0xcc, 0x03, 0xb3, 0xfa], "pmaxud xmm3, xmmword gs:[ebp - 0x54cfc34]"),
        testcase!(&[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e, ], "movdiri dword [rbp + 0x3e], edx", masm: "movdiri dword ptr [rbp + 3Eh], edx"),
        testcase!(&[0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07], "movdir64b rbp, zmmword [rbp + 0x729080b]"),
        testcase!(invalid: &[0x66, 0x2e, 0x64, 0x66, 0x46, 0x0f, 0x38, 0xf8, 0xe2]),
        testcase!(&[0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07], "movdir64b ebp, zmmword [ebp + 0x729080b]"),
        testcase!(&[0x67, 0x66, 0x65, 0x3e, 0x0f, 0x6d, 0xd1], "punpckhqdq xmm2, xmm1"),
        testcase!(&[0x2e, 0x66, 0x40, 0x0f, 0x3a, 0x0d, 0x40, 0x2d, 0x57], "blendpd xmm0, xmmword [rax + 0x2d], 0x57"),
        testcase!(&[0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f, 0xcd, 0xd7], "lddqu xmm4, xmmword [eax - 0x2832a0e5]"),
        testcase!(&[0x2e, 0x3e, 0x66, 0x3e, 0x49, 0x0f, 0x3a, 0x41, 0x30, 0x48], "dppd xmm6, xmmword [r8], 0x48"),

        testcase!(&[0x2e, 0x36, 0x47, 0x0f, 0x18, 0xe7], "nop r15d"),
        testcase!(&[0x65, 0xf0, 0x87, 0x0f], "lock xchg dword gs:[rdi], ecx"),
        testcase!(&[0x66, 0x4e, 0x0f, 0x3a, 0x44, 0x88, 0xb3, 0xad, 0x26, 0x35, 0x75], "pclmulqdq xmm9, xmmword [rax + 0x3526adb3], 0x75"),
        testcase!(&[0x4c, 0x0f, 0xff, 0x6b, 0xac], "ud0 r13d, dword [rbx - 0x54]"),

        testcase!(&[0xf2, 0xf2, 0x2e, 0x36, 0x47, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f], "enqcmd r8, zmmword [r11 + 0x3f9d1c09]"),
        testcase!(&[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54], "enqcmds rsi, zmmword fs:[rdx + 0x54]"),
        testcase!(invalid: &[0xf3, 0x0f, 0x38, 0xf8, 0xf3]),

        testcase!(&[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8], "loadiwkey xmm5, xmm0"),

        testcase!(invalid: &[0xf3, 0x2e, 0x0f, 0x6a, 0x18]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod cet {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // see
        // https://software.intel.com/sites/default/files/managed/4d/2a/control-flow-enforcement-technology-preview.pdf
        // includes encodings:
        // wruss{d,q} 066 f 38 f5
        // wrss{d,q} 0f 38 f6
        // rstorssp f3 0f 01 /5
        // saveprevssp f3 0f 01 ea
        // rdssp{d,q} f3 0f 1e
        // incssp{d,q} f3 0f ae /5
        // testcase!(&[0x0f, 0x38, 0xf6, 0x8c, 0x98, 0x4d, 0x33, 0xf5, 0xd3, ], "wrssd [rax + rbx * 4 - 0x2c0accb3], ecx"),
        // setssbsy f3 0f 01 e8
        // clrssbsy f3 0f ae /6
        // endbr64 f3 0f ae fa
        // endbr32 f3 0f ae fb
        testcase!(&[0xf3, 0x4f, 0x0f, 0xae, 0xe9], "incssp r9"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xe9], "incssp ecx"),
        testcase!(&[0x3e, 0x4f, 0x0f, 0x38, 0xf6, 0x23], "wrss qword [r11], r12"),
        testcase!(&[0x66, 0x0f, 0x38, 0xf5, 0x47, 0xe9], "wruss dword [rdi - 0x17], eax"),
        testcase!(invalid: &[0x0f, 0x38, 0xf5, 0x47, 0xe9]),
        testcase!(invalid: &[0x66, 0x3e, 0x65, 0x3e, 0x0f, 0x38, 0xf5, 0xf0]),
        testcase!(&[0xf3, 0x0f, 0x01, 0xe8], "setssbsy"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xea], "saveprevssp"),
        testcase!(&[0x66, 0xf3, 0x0f, 0x01, 0xe8], "setssbsy"),
        testcase!(&[0x66, 0xf3, 0x0f, 0x01, 0xea], "saveprevssp"),
        testcase!(&[0xf3, 0x66, 0x0f, 0x01, 0xe8], "setssbsy"),
        testcase!(&[0xf3, 0x66, 0x0f, 0x01, 0xea], "saveprevssp"),
        testcase!(&[0xf3, 0x0f, 0x01, 0x29], "rstorssp qword [rcx]"),
        testcase!(&[0xf3, 0x66, 0x0f, 0x01, 0x29], "rstorssp qword [rcx]"),
        testcase!(&[0xf3, 0x0f, 0xae, 0x30], "clrssbsy qword [rax]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sse4a {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE4a: true } &[0xf2, 0x0f, 0x2b, 0x06], "movntsd qword [rsi], xmm0"),
        testcase!(invalid: &[0xf2, 0x0f, 0x2b, 0xc6]),
        testcase!(features { SSE4a: true } &[0xf3, 0x0f, 0x2b, 0x06], "movntss dword [rsi], xmm0"),
        testcase!(invalid: &[0xf3, 0x0f, 0xba, 0xc6]),
        testcase!(features { SSE4a: true } &[0x66, 0xf2, 0x0f, 0x79, 0xcf], "insertq xmm1, xmm7"),
        testcase!(invalid: &[0x66, 0xf2, 0x0f, 0x79, 0x0f]),
        testcase!(features { SSE4a: true } &[0xf2, 0x0f, 0x79, 0xcf], "insertq xmm1, xmm7"),
        testcase!(features { SSE4a: true } &[0xf2, 0x0f, 0x78, 0xf1, 0x4e, 0x76], "insertq xmm6, xmm1, 0x4e, 0x76"),
        testcase!(invalid: &[0xf2, 0x0f, 0x79, 0x0f]),
        testcase!(features { SSE4a: true } &[0x66, 0x0f, 0x79, 0xcf], "extrq xmm1, xmm7"),
        testcase!(invalid: &[0x66, 0x0f, 0x79, 0x0f]),
        testcase!(features { SSE4a: true } &[0x66, 0x0f, 0x78, 0xc1, 0x4e, 0x76], "extrq xmm1, 0x4e, 0x76"),
        testcase!(invalid: &[0x66, 0x0f, 0x78, 0xc9, 0x4e, 0x76]),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod _3dnow {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0f, 0xe0, 0x8a], "pfnacc mm4, mm0"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0f, 0x38, 0x8e], "pfpnacc mm7, qword [rax]"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x65, 0x67, 0x65, 0x65, 0x0f, 0x0e], "femms"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x3e, 0xf3, 0x2e, 0xf2, 0x0f, 0x0f, 0x64, 0x93, 0x93, 0xa4], "pfmax mm4, qword [rbx + rdx * 4 - 0x6d]"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c], "pi2fw mm6, qword [rax - 0x5]"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x66, 0x0f, 0x0f, 0xc6, 0xb7], "pmulhrw mm0, mm6"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0f, 0xc6, 0xb7], "pmulhrw mm0, mm6"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0e], "femms"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

// first appeared in tremont
mod direct_stores {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e, ], "movdiri dword [rbp + 0x3e], edx"),
        testcase!(&[0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07], "movdir64b rbp, zmmword [rbp + 0x729080b]"),
        testcase!(&[0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07], "movdir64b ebp, zmmword [ebp + 0x729080b]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod key_locker {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8], "loadiwkey xmm5, xmm0"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xfa, 0xde], "encodekey128 ebx, esi"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xfb, 0xde], "encodekey256 ebx, esi"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

// these uinter test cases come from llvm:
// https://reviews.llvm.org/differential/changeset/?ref=2226860
mod uintr {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x01, 0xec], "uiret"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xed], "testui"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xee], "clui"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xef], "stui"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0xf0], "senduipi rax"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0xf2], "senduipi rdx"),
        testcase!(&[0xf3, 0x41, 0x0f, 0xc7, 0xf0], "senduipi r8"),
        testcase!(&[0xf3, 0x41, 0x0f, 0xc7, 0xf5], "senduipi r13"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

// started shipping in sapphire rapids
mod enqcmd {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf2, 0xf2, 0x2e, 0x36, 0x47, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f], "enqcmd r8, zmmword [r11 + 0x3f9d1c09]"),
        testcase!(&[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54], "enqcmds rsi, zmmword fs:[rdx + 0x54]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod gfni {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x3e, 0x64, 0x64, 0x66, 0x4e, 0x0f, 0x3a, 0xcf, 0xba, 0x13, 0x23, 0x04, 0xba, 0x6b], "gf2p8affineinvqb xmm15, xmmword fs:[rdx - 0x45fbdced], 0x6b"),
        testcase!(&[0x66, 0x36, 0x0f, 0x3a, 0xce, 0x8c, 0x56, 0x9e, 0x82, 0xd1, 0xbe, 0xad], "gf2p8affineqb xmm1, xmmword [rsi + rdx * 2 - 0x412e7d62], 0xad"),
        testcase!(&[0x66, 0x4e, 0x0f, 0x38, 0xcf, 0x1c, 0x54], "gf2p8mulb xmm11, xmmword [rsp + r10 * 2]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod tdx {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x0f, 0x01, 0xcc], "tdcall"),
        testcase!(&[0x66, 0x0f, 0x01, 0xcd], "seamret"),
        testcase!(&[0x66, 0x0f, 0x01, 0xce], "seamops"),
        testcase!(&[0x66, 0x0f, 0x01, 0xcf], "seamcall"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod tsxldtrk {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf2, 0x0f, 0x01, 0xe8], "xsusldtrk"),
        testcase!(&[0xf2, 0x0f, 0x01, 0xe9], "xresldtrk"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod sevsnp {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x01, 0xff], "psmash"),
        testcase!(&[0xf2, 0x0f, 0x01, 0xff], "pvalidate"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xfe], "rmpadjust"),
        testcase!(&[0xf2, 0x0f, 0x01, 0xfe], "rmpupdate"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod keylocker {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x38, 0xdd, 0x03], "aesdec128kl xmm0, m384b [rbx]", masm: "aesdec128kl xmm0, [rbx]"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

// some test cases are best just lifted from llvm or gcc.
mod from_llvm {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x3a, 0xf0, 0xc0, 0x01], "hreset 0x1", masm: "hreset 1, eax"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

mod from_reports {
    use crate::long_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // negative compressed evex displacements should not overflow and panic
        testcase!(&[0x62, 0xf2, 0x6d, 0xac, 0x00, 0x59, 0xa7], "vpshufb ymm3{k4}{z}, ymm2, ymmword [rcx - 0xb20]"),
        testcase!(&[0x62, 0xf2, 0xfd, 0x0f, 0x8a, 0x62, 0xf2], "vcompresspd xmmword [rdx - 0x70]{k7}, xmm4"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0x0f], "nop dword [rdi], ecx"),
    ];

    #[test]
    fn test() {
        run_test(&CASES);
    }
}

/// the first four single byte registers (`al`, `cl`, `dl`, `bl`) can be described as either the
/// first four registers of the old `B` bank (`al..bh`) or the first four registers of the new `rB`
/// bank, used when `rex.w` is present on the instruction. `RegSpec` relies on the bank matching to
/// compare equality, as well as for `Hash` and `Ord` to work correctly, so having two spellings of
/// the register `al` (and friends) can be a correctness issue for dependent code.
#[test]
fn register_synonyms_use_old_bank() {
    // a few instructions where adding a rex.w prefix would yield a synonymous byte-size register
    let cases: &'static [&'static [u8]] = &[
        &[0x30, 0x00],
        &[0x30, 0xc1],
        &[0x0f, 0xb6, 0xc0],
        &[0x0f, 0xbe, 0xc0],
        &[0xb2, 0xff], // from `does_not_decode_invalid_registers` fuzz target 🎉
    ];

    for case in cases.iter() {
        let mut case_with_rexw: Vec<u8> = vec![0x48];
        case_with_rexw.extend_from_slice(case);

        let mut reader = yaxpeax_arch::U8Reader::new(case);
        let no_rexw = InstDecoder::default().decode(&mut reader).expect("can disassemble test instruction");

        let mut reader = yaxpeax_arch::U8Reader::new(case);
        let with_rexw = InstDecoder::default().decode(&mut reader).expect("can disassemble test instruction");

        assert_eq!(no_rexw, with_rexw);
    }
}

mod reg_specs {
    use yaxpeax_x86::long_mode::RegSpec;

    #[test]
    fn reg_specs_are_correct() {
        #[cfg(feature = "fmt")]
        {
            let cases: Vec<(RegSpec, &'static str)> = vec![
                (RegSpec::q(0), "rax"), (RegSpec::rax(), "rax"),
                (RegSpec::q(1), "rcx"), (RegSpec::rcx(), "rcx"),
                (RegSpec::q(2), "rdx"), (RegSpec::rdx(), "rdx"),
                (RegSpec::q(3), "rbx"), (RegSpec::rbx(), "rbx"),
                (RegSpec::q(4), "rsp"), (RegSpec::rsp(), "rsp"),
                (RegSpec::q(5), "rbp"), (RegSpec::rbp(), "rbp"),
                (RegSpec::q(6), "rsi"), (RegSpec::rsi(), "rsi"),
                (RegSpec::q(7), "rdi"), (RegSpec::rdi(), "rdi"),
                (RegSpec::q(8), "r8"), (RegSpec::r8(), "r8"),
                (RegSpec::q(9), "r9"), (RegSpec::r9(), "r9"),
                (RegSpec::q(10), "r10"), (RegSpec::r10(), "r10"),
                (RegSpec::q(11), "r11"), (RegSpec::r11(), "r11"),
                (RegSpec::q(12), "r12"), (RegSpec::r12(), "r12"),
                (RegSpec::q(13), "r13"), (RegSpec::r13(), "r13"),
                (RegSpec::q(14), "r14"), (RegSpec::r14(), "r14"),
                (RegSpec::q(15), "r15"), (RegSpec::r15(), "r15"),

                (RegSpec::d(0), "eax"), (RegSpec::eax(), "eax"),
                (RegSpec::d(1), "ecx"), (RegSpec::ecx(), "ecx"),
                (RegSpec::d(2), "edx"), (RegSpec::edx(), "edx"),
                (RegSpec::d(3), "ebx"), (RegSpec::ebx(), "ebx"),
                (RegSpec::d(4), "esp"), (RegSpec::esp(), "esp"),
                (RegSpec::d(5), "ebp"), (RegSpec::ebp(), "ebp"),
                (RegSpec::d(6), "esi"), (RegSpec::esi(), "esi"),
                (RegSpec::d(7), "edi"), (RegSpec::edi(), "edi"),
                (RegSpec::d(8), "r8d"), (RegSpec::r8d(), "r8d"),
                (RegSpec::d(9), "r9d"), (RegSpec::r9d(), "r9d"),
                (RegSpec::d(10), "r10d"), (RegSpec::r10d(), "r10d"),
                (RegSpec::d(11), "r11d"), (RegSpec::r11d(), "r11d"),
                (RegSpec::d(12), "r12d"), (RegSpec::r12d(), "r12d"),
                (RegSpec::d(13), "r13d"), (RegSpec::r13d(), "r13d"),
                (RegSpec::d(14), "r14d"), (RegSpec::r14d(), "r14d"),
                (RegSpec::d(15), "r15d"), (RegSpec::r15d(), "r15d"),

                (RegSpec::w(0), "ax"), (RegSpec::ax(), "ax"),
                (RegSpec::w(1), "cx"), (RegSpec::cx(), "cx"),
                (RegSpec::w(2), "dx"), (RegSpec::dx(), "dx"),
                (RegSpec::w(3), "bx"), (RegSpec::bx(), "bx"),
                (RegSpec::w(4), "sp"), (RegSpec::sp(), "sp"),
                (RegSpec::w(5), "bp"), (RegSpec::bp(), "bp"),
                (RegSpec::w(6), "si"), (RegSpec::si(), "si"),
                (RegSpec::w(7), "di"), (RegSpec::di(), "di"),
                (RegSpec::w(8), "r8w"), (RegSpec::r8w(), "r8w"),
                (RegSpec::w(9), "r9w"), (RegSpec::r9w(), "r9w"),
                (RegSpec::w(10), "r10w"), (RegSpec::r10w(), "r10w"),
                (RegSpec::w(11), "r11w"), (RegSpec::r11w(), "r11w"),
                (RegSpec::w(12), "r12w"), (RegSpec::r12w(), "r12w"),
                (RegSpec::w(13), "r13w"), (RegSpec::r13w(), "r13w"),
                (RegSpec::w(14), "r14w"), (RegSpec::r14w(), "r14w"),
                (RegSpec::w(15), "r15w"), (RegSpec::r15w(), "r15w"),

                (RegSpec::b(0), "al"), (RegSpec::al(), "al"),
                (RegSpec::b(1), "cl"), (RegSpec::cl(), "cl"),
                (RegSpec::b(2), "dl"), (RegSpec::dl(), "dl"),
                (RegSpec::b(3), "bl"), (RegSpec::bl(), "bl"),
                (RegSpec::b(4), "ah"), (RegSpec::ah(), "ah"),
                (RegSpec::b(5), "ch"), (RegSpec::ch(), "ch"),
                (RegSpec::b(6), "dh"), (RegSpec::dh(), "dh"),
                (RegSpec::b(7), "bh"), (RegSpec::bh(), "bh"),

                (RegSpec::rb(0), "al"), (RegSpec::al(), "al"),
                (RegSpec::rb(1), "cl"), (RegSpec::cl(), "cl"),
                (RegSpec::rb(2), "dl"), (RegSpec::dl(), "dl"),
                (RegSpec::rb(3), "bl"), (RegSpec::bl(), "bl"),
                (RegSpec::rb(4), "spl"), (RegSpec::spl(), "spl"),
                (RegSpec::rb(5), "bpl"), (RegSpec::bpl(), "bpl"),
                (RegSpec::rb(6), "sil"), (RegSpec::sil(), "sil"),
                (RegSpec::rb(7), "dil"), (RegSpec::dil(), "dil"),
                (RegSpec::rb(8), "r8b"), (RegSpec::r8b(), "r8b"),
                (RegSpec::rb(9), "r9b"), (RegSpec::r9b(), "r9b"),
                (RegSpec::rb(10), "r10b"), (RegSpec::r10b(), "r10b"),
                (RegSpec::rb(11), "r11b"), (RegSpec::r11b(), "r11b"),
                (RegSpec::rb(12), "r12b"), (RegSpec::r12b(), "r12b"),
                (RegSpec::rb(13), "r13b"), (RegSpec::r13b(), "r13b"),
                (RegSpec::rb(14), "r14b"), (RegSpec::r14b(), "r14b"),
                (RegSpec::rb(15), "r15b"), (RegSpec::r15b(), "r15b"),

                (RegSpec::rip(), "rip"), (RegSpec::eip(), "eip"),
                (RegSpec::rflags(), "rflags"), (RegSpec::eflags(), "eflags"),
                (RegSpec::es(), "es"), (RegSpec::cs(), "cs"),
                (RegSpec::ss(), "ss"), (RegSpec::ds(), "ds"),
                (RegSpec::fs(), "fs"), (RegSpec::gs(), "gs"),

                (RegSpec::mask(0), "k0"),
                (RegSpec::mask(1), "k1"),
                (RegSpec::mask(2), "k2"),
                (RegSpec::mask(3), "k3"),
                (RegSpec::mask(4), "k4"),
                (RegSpec::mask(5), "k5"),
                (RegSpec::mask(6), "k6"),
                (RegSpec::mask(7), "k7"),
            ];

            for (reg, name) in cases.iter() {
                assert_eq!(reg.name(), *name);
            }
        }

        let cases: Vec<(RegSpec, RegSpec)> = vec![
            (RegSpec::q(0), RegSpec::rax()),
            (RegSpec::q(1), RegSpec::rcx()),
            (RegSpec::q(2), RegSpec::rdx()),
            (RegSpec::q(3), RegSpec::rbx()),
            (RegSpec::q(4), RegSpec::rsp()),
            (RegSpec::q(5), RegSpec::rbp()),
            (RegSpec::q(6), RegSpec::rsi()),
            (RegSpec::q(7), RegSpec::rdi()),
            (RegSpec::q(8), RegSpec::r8()),
            (RegSpec::q(9), RegSpec::r9()),
            (RegSpec::q(10), RegSpec::r10()),
            (RegSpec::q(11), RegSpec::r11()),
            (RegSpec::q(12), RegSpec::r12()),
            (RegSpec::q(13), RegSpec::r13()),
            (RegSpec::q(14), RegSpec::r14()),
            (RegSpec::q(15), RegSpec::r15()),

            (RegSpec::d(0), RegSpec::eax()),
            (RegSpec::d(1), RegSpec::ecx()),
            (RegSpec::d(2), RegSpec::edx()),
            (RegSpec::d(3), RegSpec::ebx()),
            (RegSpec::d(4), RegSpec::esp()),
            (RegSpec::d(5), RegSpec::ebp()),
            (RegSpec::d(6), RegSpec::esi()),
            (RegSpec::d(7), RegSpec::edi()),
            (RegSpec::d(8), RegSpec::r8d()),
            (RegSpec::d(9), RegSpec::r9d()),
            (RegSpec::d(10), RegSpec::r10d()),
            (RegSpec::d(11), RegSpec::r11d()),
            (RegSpec::d(12), RegSpec::r12d()),
            (RegSpec::d(13), RegSpec::r13d()),
            (RegSpec::d(14), RegSpec::r14d()),
            (RegSpec::d(15), RegSpec::r15d()),

            (RegSpec::w(0), RegSpec::ax()),
            (RegSpec::w(1), RegSpec::cx()),
            (RegSpec::w(2), RegSpec::dx()),
            (RegSpec::w(3), RegSpec::bx()),
            (RegSpec::w(4), RegSpec::sp()),
            (RegSpec::w(5), RegSpec::bp()),
            (RegSpec::w(6), RegSpec::si()),
            (RegSpec::w(7), RegSpec::di()),
            (RegSpec::w(8), RegSpec::r8w()),
            (RegSpec::w(9), RegSpec::r9w()),
            (RegSpec::w(10), RegSpec::r10w()),
            (RegSpec::w(11), RegSpec::r11w()),
            (RegSpec::w(12), RegSpec::r12w()),
            (RegSpec::w(13), RegSpec::r13w()),
            (RegSpec::w(14), RegSpec::r14w()),
            (RegSpec::w(15), RegSpec::r15w()),

            (RegSpec::b(0), RegSpec::al()),
            (RegSpec::b(1), RegSpec::cl()),
            (RegSpec::b(2), RegSpec::dl()),
            (RegSpec::b(3), RegSpec::bl()),
            (RegSpec::b(4), RegSpec::ah()),
            (RegSpec::b(5), RegSpec::ch()),
            (RegSpec::b(6), RegSpec::dh()),
            (RegSpec::b(7), RegSpec::bh()),

            (RegSpec::rb(0), RegSpec::al()),
            (RegSpec::rb(1), RegSpec::cl()),
            (RegSpec::rb(2), RegSpec::dl()),
            (RegSpec::rb(3), RegSpec::bl()),
            (RegSpec::rb(4), RegSpec::spl()),
            (RegSpec::rb(5), RegSpec::bpl()),
            (RegSpec::rb(6), RegSpec::sil()),
            (RegSpec::rb(7), RegSpec::dil()),
            (RegSpec::rb(8), RegSpec::r8b()),
            (RegSpec::rb(9), RegSpec::r9b()),
            (RegSpec::rb(10), RegSpec::r10b()),
            (RegSpec::rb(11), RegSpec::r11b()),
            (RegSpec::rb(12), RegSpec::r12b()),
            (RegSpec::rb(13), RegSpec::r13b()),
            (RegSpec::rb(14), RegSpec::r14b()),
            (RegSpec::rb(15), RegSpec::r15b()),
        ];

        for (reg1, reg2) in cases.iter() {
            assert_eq!(reg1, reg2);
        }
    }

    #[test]
    #[should_panic]
    fn invalid_mask_reg_panics() {
        RegSpec::mask(8);
    }

    #[test]
    #[should_panic]
    fn invalid_qword_reg_panics() {
        RegSpec::q(16);
    }

    #[test]
    #[should_panic]
    fn invalid_dword_reg_panics() {
        RegSpec::d(16);
    }

    #[test]
    #[should_panic]
    fn invalid_word_reg_panics() {
        RegSpec::w(16);
    }

    #[test]
    #[should_panic]
    fn invalid_byte_reg_panics() {
        RegSpec::b(8);
    }

    #[test]
    #[should_panic]
    fn invalid_rex_byte_reg_panics() {
        RegSpec::rb(16);
    }

    #[test]
    #[should_panic]
    fn invalid_x87_reg_panics() {
        RegSpec::st(8);
    }

    #[test]
    #[should_panic]
    fn invalid_xmm_reg_panics() {
        RegSpec::xmm(32);
    }

    #[test]
    #[should_panic]
    fn invalid_ymm_reg_panics() {
        RegSpec::ymm(32);
    }

    #[test]
    #[should_panic]
    fn invalid_zmm_reg_panics() {
        RegSpec::zmm(32);
    }
}
