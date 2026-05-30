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

fn test_display(data: &[u8], expected: &'static str) {
    test_display_under(&InstDecoder::default(), data, expected);
}

fn test_display_under(decoder: &InstDecoder, data: &[u8], expected: &'static str) {
    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }
    let mut reader = yaxpeax_arch::U8Reader::new(data);
    match decoder.decode(&mut reader) {
        Ok(instr) => {
            cfg_if::cfg_if! {
                if #[cfg(feature="fmt")] {
                    let text = format!("{}", instr);
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
                    let text3 = formatter.format_inst(&instr.display_with(yaxpeax_x86::long_mode::DisplayStyle::Intel)).expect("printing succeeds");

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
    SSE4a,
    AVX,
    AESNI,
    F16C,
    AVX_AESNI,
    AVX_F16C,
    AVX2,
}

impl FeatureSet {
    fn into_decoder(&self) -> InstDecoder {
        match self {
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
            FeatureSet::SSE4a => {
                InstDecoder::minimal().with_sse4a()
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
        }
    }
}

struct Disasm {
    display: &'static str,
    c: Option<&'static str>,
}

struct TestCase {
    bytes: &'static [u8],
    featuresets: Option<&'static [(FeatureSet, bool)]>,
    #[cfg(feature="fmt")]
    decodes: Option<Disasm>,
}

fn run_test(cases: &[TestCase]) {
    for tc in cases {
        if let Some(decodes) = tc.decodes.as_ref() {
            test_display(tc.bytes, decodes.display);

            if let Some(featuresets) = tc.featuresets {
                for (featureset, decode_ok) in featuresets {
                    if *decode_ok {
                        test_display_under(&featureset.into_decoder(), tc.bytes, decodes.display);
                    } else {
                        test_invalid_under(&featureset.into_decoder(), tc.bytes);
                    }
                }
            }
        } else {
            test_invalid(tc.bytes);
        }
    }
}

macro_rules! testcase {
    (invalid: $bytes:expr) => {
        let bytes: &'static [u8] = $bytes;
        let tc = TestCase {
            bytes,
            featuresets: None,
            decodes: None,
        };
        run_test(&[tc]);
    };

    // need this above `($bytes:expr, $test:expr)` below to keep that case from
    // matching inappropriately early.
    (features { $($feature:ident: $decode:expr$(,)?)+ } $bytes:expr, $text:expr) => {
        let bytes: &'static [u8] = $bytes;
        let text: &'static str = $text;
        let featuresets: &'static [(FeatureSet, bool)] = &[
            $((FeatureSet::$feature, $decode),)*
        ];
        let tc = TestCase {
            bytes,
            featuresets: Some(featuresets),
            decodes: Some(Disasm { display: text, c: None })
        };
        run_test(&[tc]);
    };

    ({ $($feature:ident: $decode:expr)+ } $bytes:expr, $text:expr, c: $c_text:expr) => {
        let bytes: &'static [u8] = $bytes;
        let text: &'static str = $text;
        let c: &'static str = $c_text;
        let featuresets: &'static [(FeatureSet, bool)] = &[
            $((FeatureSet::$feature, $decode))*
        ];
        let tc = TestCase {
            bytes,
            featuresets: Some(featuresets),
            decodes: Some(Disasm { display: text, c: Some(c) })
        };
        run_test(&[tc]);
    };

    ($bytes:expr, $text:expr) => {
        let bytes: &'static [u8] = $bytes;
        let text: &'static str = $text;
        let tc = TestCase {
            bytes,
            featuresets: None,
            decodes: Some(Disasm { display: text, c: None })
        };
        run_test(&[tc]);
    };

    ($bytes:expr, $text:expr, c: $c_text:expr) => {
        let bytes: &'static [u8] = $bytes;
        let text: &'static str = $text;
        let c: &'static str = $c_text;
        let tc = TestCase {
            bytes,
            featuresets: None,
            decodes: Some(Disasm { display: text, c: Some(c) })
        };
        run_test(&[tc]);
    };
}


#[test]
fn test_vex() {
    fn test_instr_invalid(bytes: &[u8]) {
        test_invalid_under(&InstDecoder::minimal().with_avx(), bytes);
        test_invalid_under(&InstDecoder::default(), bytes);
    }

    // prefix 03
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_001, 0x00, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x00, 0b11_001_010, 0x77]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b1_1111_101, 0x00, 0b11_001_010, 0x77], "vpermq ymm9, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_001, 0x01, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x01, 0b11_001_010, 0x77]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b1_1111_101, 0x01, 0b11_001_010, 0x77], "vpermpd ymm9, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_001, 0x02, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x02, 0b11_001_010, 0x77]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x02, 0b11_001_010, 0x77], "vpblendd xmm9, xmm0, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x02, 0b00_001_010, 0x77], "vpblendd xmm9, xmm0, xmmword [r10], 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x02, 0b11_001_010, 0x77], "vpblendd ymm9, ymm0, ymm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x02, 0b00_001_010, 0x77], "vpblendd ymm9, ymm0, ymmword [r10], 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x04, 0b11_001_010, 0x77], "vpermilps xmm9, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x04, 0b11_001_010, 0x77], "vpermilps ymm9, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x05, 0b11_001_010, 0x77], "vpermilpd xmm9, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x05, 0b11_001_010, 0x77], "vpermilpd ymm9, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x06, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x06, 0b11_001_010, 0x77], "vperm2f128 ymm9, ymm0, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x06, 0b00_001_010, 0x77], "vperm2f128 ymm9, ymm0, ymmword [r10], 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0c, 0b11_001_010, 0x77], "vblendps xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0c, 0b11_001_010, 0x77], "vblendps ymm9, ymm8, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0d, 0b11_001_010, 0x77], "vblendpd xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0d, 0b11_001_010, 0x77], "vblendpd ymm9, ymm8, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0e, 0b11_001_010, 0x77], "vpblendw xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0e, 0b11_001_010, 0x77], "vpblendw ymm9, ymm8, ymm10, 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x08, 0b11_001_010, 0x77], "vroundps xmm9, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x08, 0b11_001_010, 0x77], "vroundps ymm9, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x08, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x08, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x09, 0b11_001_010, 0x77], "vroundpd xmm9, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x09, 0b11_001_010, 0x77], "vroundpd ymm9, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x09, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x09, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0a, 0b11_001_010, 0x77], "vroundss xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0a, 0b11_001_010, 0x77], "vroundss xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x0b, 0b11_001_010, 0x77], "vroundsd xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x0b, 0b11_001_010, 0x77], "vroundsd xmm9, xmm8, xmm10, 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_001, 0x0f, 0b11_001_010, 0x77], "vpalignr xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_101, 0x0f, 0b11_001_010, 0x77], "vpalignr ymm9, ymm8, ymm10, 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x14, 0b11_001_010, 0x77], "vpextrb r10d, xmm9, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x14, 0b00_001_010, 0x77], "vpextrb byte [r10], xmm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x14, 0b00_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x14, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x15, 0b11_001_010, 0x77], "vpextrw r10d, xmm9, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x15, 0b00_001_010, 0x77], "vpextrw word [r10], xmm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x15, 0b00_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x15, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x16, 0b11_001_010, 0x77], "vpextrd r10d, xmm9, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x16, 0b00_001_010, 0x77], "vpextrd dword [r10], xmm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x16, 0b00_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x16, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_1111_001, 0x16, 0b11_001_010, 0x77], "vpextrq r10, xmm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x16, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_1111_001, 0x16, 0b00_001_010, 0x77], "vpextrq qword [r10], xmm9, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x17, 0b11_001_010, 0x77], "vextractps r10d, xmm9, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x17, 0b00_001_010, 0x77], "vextractps dword [r10], xmm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x17, 0b00_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x17, 0b00_001_010, 0x77]);

    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x18, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x18, 0b11_001_010, 0x77], "vinsertf128 ymm9, ymm8, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0x19, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x19, 0b11_001_010, 0x77], "vextractf128 xmm10, ymm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]);

    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x38, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x38, 0b11_001_010, 0x77]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x38, 0b11_001_010, 0x77], "vinserti128 ymm9, ymm8, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x39, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x39, 0b11_001_010, 0x77]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x39, 0b11_001_010, 0x77], "vextracti128 xmm10, ymm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]);

    testcase!(features { AVX_F16C: true, AVX: false, F16C: false } &[0xc4, 0b000_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm10, ymm9, 0x77");
    testcase!(features { AVX_F16C: true, AVX: false, F16C: false } &[0xc4, 0b000_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm10, ymm9, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x1d, 0b11_001_010, 0x77]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x20, 0b11_001_010, 0x77], "vpinsrb xmm9, xmm8, r10d, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x20, 0b00_001_010, 0x77], "vpinsrb xmm9, xmm8, byte [r10], 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x20, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x21, 0b11_001_010, 0x77], "vinsertps xmm9, xmm8, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x21, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x22, 0b11_001_010, 0x77], "vpinsrd xmm9, xmm8, r10d, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x22, 0b00_001_010, 0x77], "vpinsrd xmm9, xmm8, dword [r10], 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x22, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_001, 0x22, 0b11_001_010, 0x77], "vpinsrq xmm9, xmm8, r10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b1_0111_001, 0x22, 0b00_001_010, 0x77], "vpinsrq xmm9, xmm8, qword [r10], 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0x22, 0b00_001_010, 0x77]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x40, 0b11_001_010, 0x77], "vdpps xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x40, 0b11_001_010, 0x77], "vdpps ymm9, ymm8, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x41, 0b11_001_010, 0x77], "vdppd xmm9, xmm8, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x41, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x42, 0b11_001_010, 0x77], "vmpsadbw xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x42, 0b11_001_010, 0x77], "vmpsadbw ymm9, ymm8, ymm10, 0x77");

    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x46, 0b11_001_010, 0x77], "vperm2i128 ymm9, ymm0, ymm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_1111_101, 0x46, 0b00_001_010, 0x77], "vperm2i128 ymm9, ymm0, ymmword [r10], 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_001, 0x46, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_1111_101, 0x46, 0b11_001_010, 0x77]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_0111_001, 0x4c, 0b11_001_010, 0x77], "vpblendvb xmm9, xmm8, xmm10, xmm7");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00011, 0b0_0111_101, 0x4c, 0b11_001_010, 0x77], "vpblendvb ymm9, ymm8, ymm10, ymm7");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0x4c, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0x4c, 0b11_001_010, 0x77]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x60, 0b11_001_010, 0x77], "vpcmpestrm xmm9, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x60, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x60, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x60, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x61, 0b11_001_010, 0x77], "vpcmpestri xmm9, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x61, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x61, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x61, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x62, 0b11_001_010, 0x77], "vpcmpistrm xmm9, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x62, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x62, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x62, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00011, 0b0_1111_001, 0x63, 0b11_001_010, 0x77], "vpcmpistri xmm9, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_1111_101, 0x63, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_001, 0x63, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b0_0111_101, 0x63, 0b11_001_010, 0x77]);

    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00011, 0b1_1111_001, 0xdf, 0b11_001_010, 0x77], "vaeskeygenassist xmm9, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_001, 0xdf, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00011, 0b1_0111_101, 0xdf, 0b11_001_010, 0x77]);

    // prefix 02
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x00, 0b11_001_010], "vpshufb xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x00, 0b11_001_010], "vpshufb ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x01, 0b11_001_010], "vphaddw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x01, 0b11_001_010], "vphaddw ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x02, 0b11_001_010], "vphaddd xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x02, 0b11_001_010], "vphaddd ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x03, 0b11_001_010], "vphaddsw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x03, 0b11_001_010], "vphaddsw ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x04, 0b11_001_010], "vpmaddubsw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x04, 0b11_001_010], "vpmaddubsw ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x05, 0b11_001_010], "vphsubw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x05, 0b11_001_010], "vphsubw ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x06, 0b11_001_010], "vphsubd xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x06, 0b11_001_010], "vphsubd ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x07, 0b11_001_010], "vphsubsw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x07, 0b11_001_010], "vphsubsw ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x08, 0b11_001_010], "vpsignb xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x08, 0b11_001_010], "vpsignb ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x09, 0b11_001_010], "vpsignw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x09, 0b11_001_010], "vpsignw ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0a, 0b11_001_010], "vpsignd xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0a, 0b11_001_010], "vpsignd ymm9, ymm0, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0b, 0b11_001_010], "vpmulhrsw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0b, 0b11_001_010], "vpmulhrsw ymm9, ymm0, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x0c, 0b11_001_010], "vpermilps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x0c, 0b11_001_010], "vpermilps ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x0d, 0b11_001_010], "vpermilpd xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x0d, 0b11_001_010], "vpermilpd ymm9, ymm8, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x0d, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x0d, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0e, 0b11_001_010], "vtestps xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0e, 0b11_001_010], "vtestps ymm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x0e, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x0f, 0b11_001_010], "vtestpd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x0f, 0b11_001_010], "vtestpd ymm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x0f, 0b11_001_010]);
    testcase!(features { AVX_F16C: true, AVX: false F16C: false } &[0xc4, 0b111_00010, 0b0_1111_001, 0x13, 0b11_001_010], "vcvtph2ps xmm1, xmm2");
    testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x13, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x16, 0b11_001_010], "vpermps ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x16, 0b00_001_010], "vpermps ymm9, ymm0, ymmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x16, 0b00_011_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x16, 0b00_011_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x17, 0b11_001_010], "vptest xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x17, 0b11_001_010], "vptest ymm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x17, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x17, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x18, 0b00_001_010], "vbroadcastss xmm9, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x18, 0b00_001_010], "vbroadcastss ymm9, dword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_001, 0x18, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x18, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x19, 0b00_001_010], "vbroadcastsd ymm9, qword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x19, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x19, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1a, 0b00_001_010], "vbroadcastf128 ymm9, xmmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x1a, 0b00_001_010]); // vex.w=1 is invalid
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x1a, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x1a, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x5a, 0b00_001_010], "vbroadcasti128 ymm9, xmmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_101, 0x5a, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x5a, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x5a, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x18, 0b11_001_010], "vbroadcastss xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x18, 0b11_001_010], "vbroadcastss ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x18, 0b00_001_010], "vbroadcastss ymm9, dword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x18, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x18, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b111_00010, 0b0_1111_001, 0x19, 0b11_001_010]); // "vbroadcastsd xmm, xmm" is not legal (L!=0)
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x19, 0b11_001_010], "vbroadcastsd ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x19, 0b00_001_010], "vbroadcastsd ymm9, qword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x19, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x19, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x19, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_001, 0x1a, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x1a, 0b11_001_010]);


    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x1c, 0b11_001_010], "vpabsb xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1c, 0b11_001_010], "vpabsb ymm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x1c, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x1d, 0b11_001_010], "vpabsw xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1d, 0b11_001_010], "vpabsw ymm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x1d, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x1e, 0b11_001_010], "vpabsd xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x1e, 0b11_001_010], "vpabsd ymm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x1e, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x20, 0b11_001_010], "vpmovsxbw xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x20, 0b11_001_010], "vpmovsxbw ymm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x20, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x21, 0b11_001_010], "vpmovsxbd xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x21, 0b11_001_010], "vpmovsxbd ymm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x21, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x22, 0b11_001_010], "vpmovsxbq xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x22, 0b11_001_010], "vpmovsxbq ymm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x22, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x23, 0b11_001_010], "vpmovsxwd xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x23, 0b11_001_010], "vpmovsxwd ymm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x23, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x24, 0b11_001_010], "vpmovsxwq xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x24, 0b11_001_010], "vpmovsxwq ymm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x24, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x25, 0b11_001_010], "vpmovsxdq xmm9, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x25, 0b11_001_010], "vpmovsxdq ymm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x25, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x28, 0b11_001_010], "vpmuldq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x28, 0b11_001_010], "vpmuldq ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x29, 0b11_001_010], "vpcmpeqq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x29, 0b11_001_010], "vpcmpeqq ymm9, ymm8, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x2a, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x2a, 0b00_001_010], "vmovntdqa xmm9, xmmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x2a, 0b00_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x2a, 0b00_001_010], "vmovntdqa ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2b, 0b11_001_010], "vpackusdw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2b, 0b11_001_010], "vpackusdw ymm9, ymm8, ymm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2b, 0b00_001_010], "vpackusdw ymm9, ymm8, ymmword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2c, 0b00_001_010], "vmaskmovps xmm9, xmm8, xmmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2c, 0b11_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2c, 0b00_001_010], "vmaskmovps ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2d, 0b00_001_010], "vmaskmovpd xmm9, xmm8, xmmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2d, 0b11_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2d, 0b00_001_010], "vmaskmovpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2e, 0b00_001_010], "vmaskmovps xmmword [r10], xmm8, xmm9");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2e, 0b11_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2e, 0b00_001_010], "vmaskmovps ymmword [r10], ymm8, ymm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x2f, 0b00_001_010], "vmaskmovpd xmmword [r10], xmm8, xmm9");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x2f, 0b11_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x2f, 0b00_001_010], "vmaskmovpd ymmword [r10], ymm8, ymm9");

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x30, 0b11_001_010], "vpmovzxbw xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x30, 0b11_001_010], "vpmovzxbw ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x31, 0b11_001_010], "vpmovzxbd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x31, 0b11_001_010], "vpmovzxbd ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x32, 0b11_001_010], "vpmovzxbq xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x32, 0b11_001_010], "vpmovzxbq ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x33, 0b11_001_010], "vpmovzxwd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x33, 0b11_001_010], "vpmovzxwd ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x34, 0b11_001_010], "vpmovzxwq xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x34, 0b11_001_010], "vpmovzxwq ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x35, 0b11_001_010], "vpmovzxdq xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x35, 0b11_001_010], "vpmovzxdq ymm9, xmm10");

    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x30, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x30, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x31, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x31, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x32, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x32, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x33, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x33, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x34, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x34, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x35, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x35, 0b11_001_010]);

    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x36, 0b11_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x36, 0b11_001_010], "vpermd ymm9, ymm8, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x36, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x37, 0b11_001_010], "vpcmpgtq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x37, 0b11_001_010], "vpcmpgtq ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x38, 0b11_001_010], "vpminsb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x38, 0b11_001_010], "vpminsb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x39, 0b11_001_010], "vpminsd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x39, 0b11_001_010], "vpminsd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3a, 0b11_001_010], "vpminuw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3a, 0b11_001_010], "vpminuw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3b, 0b11_001_010], "vpminud xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3b, 0b11_001_010], "vpminud ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3c, 0b11_001_010], "vpmaxsb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3c, 0b11_001_010], "vpmaxsb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3d, 0b11_001_010], "vpmaxsd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3d, 0b11_001_010], "vpmaxsd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3e, 0b11_001_010], "vpmaxuw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3e, 0b11_001_010], "vpmaxuw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x3f, 0b11_001_010], "vpmaxud xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x3f, 0b11_001_010], "vpmaxud ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_0111_001, 0x40, 0b11_001_010], "vpmulld xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_0111_101, 0x40, 0b11_001_010], "vpmulld ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x41, 0b11_001_010], "vphminposuw xmm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_001, 0x41, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0x41, 0b11_001_010]);
// TODO: should something be at opcode 42 here?
//    testcase!(features { AVX: true } &[0xc4, 0b000_00010, 0b1_0111_001, 0x42, 0b11_001_010], "vphminposuw xmm");
//    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0x41, 0b11_001_010]);

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x45, 0b00_001_010], "vpsrlvd xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x45, 0b00_001_010], "vpsrlvd ymm9, ymm0, ymmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x45, 0b11_001_010], "vpsrlvd xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x45, 0b11_001_010], "vpsrlvd ymm9, ymm0, ymm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x45, 0b00_001_010], "vpsrlvq xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x45, 0b00_001_010], "vpsrlvq ymm9, ymm0, ymmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x45, 0b11_001_010], "vpsrlvq xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x45, 0b11_001_010], "vpsrlvq ymm9, ymm0, ymm10");

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x46, 0b00_001_010], "vpsravd xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x46, 0b00_001_010], "vpsravd ymm9, ymm0, ymmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_001, 0x46, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_1111_101, 0x46, 0b00_001_010]);

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x47, 0b00_001_010], "vpsllvd xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x47, 0b00_001_010], "vpsllvd ymm9, ymm0, ymmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x47, 0b11_001_010], "vpsllvd xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x47, 0b11_001_010], "vpsllvd ymm9, ymm0, ymm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x47, 0b00_001_010], "vpsllvq xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x47, 0b00_001_010], "vpsllvq ymm9, ymm0, ymmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x47, 0b11_001_010], "vpsllvq xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x47, 0b11_001_010], "vpsllvq ymm9, ymm0, ymm10");

    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x58, 0b11_000_001], "vpbroadcastd xmm0, xmm1");
    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x58, 0b11_000_001], "vpbroadcastd ymm0, ymm1");
    testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x58, 0b11_000_001]);
    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x59, 0b11_000_001], "vpbroadcastq xmm0, xmm1");
    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x59, 0b11_000_001], "vpbroadcastq ymm0, ymm1");
    testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x59, 0b11_000_001]);

    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x78, 0b11_000_001], "vpbroadcastb xmm0, xmm1");
    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x78, 0b11_000_001], "vpbroadcastb ymm0, ymm1");
    testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x78, 0b11_000_001]);
    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_001, 0x79, 0b11_000_001], "vpbroadcastw xmm0, xmm1");
    testcase!(features { AVX2: true } &[0xc4, 0b111_00010, 0b0_1111_101, 0x79, 0b11_000_001], "vpbroadcastw ymm0, ymm1");
    testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x79, 0b11_000_001]);

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x8c, 0b00_001_010], "vpmaskmovd xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x8c, 0b00_001_010], "vpmaskmovd ymm9, ymm0, ymmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8c, 0b11_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x8c, 0b00_001_010], "vpmaskmovq xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x8c, 0b00_001_010], "vpmaskmovq ymm9, ymm0, ymmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8c, 0b11_001_010]);

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x8e, 0b00_001_010], "vpmaskmovd xmmword [r10], xmm0, xmm9");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x8e, 0b00_001_010], "vpmaskmovd ymmword [r10], ymm0, ymm9");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8e, 0b11_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x8e, 0b00_001_010], "vpmaskmovq xmmword [r10], xmm0, xmm9");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x8e, 0b00_001_010], "vpmaskmovq ymmword [r10], ymm0, ymm9");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_1111_001, 0x8e, 0b11_001_010]);

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdd xmm8, dword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdd ymm8, dword [r9 + ymm12 * 4], ymm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdq xmm8, qword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdq ymm8, qword [r9 + xmm12 * 4], ymm0");

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm8, dword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm8, dword [r9 + ymm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqq xmm8, qword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqq ymm8, qword [r9 + ymm12 * 4], ymm0");

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdps xmm8, dword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdps ymm8, dword [r9 + ymm12 * 4], ymm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdpd xmm8, qword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdpd ymm8, qword [r9 + ymm12 * 4], ymm0");

    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqps xmm8, dword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqps xmm8, dword [r9 + ymm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqpd xmm8, qword [r9 + xmm12 * 4], xmm0");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqpd ymm8, qword [r9 + ymm12 * 4], ymm0");

    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b0_1111_001, 0xdb, 0b11_001_010], "vaesimc xmm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00010, 0b0_0111_101, 0xdb, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00010, 0b1_0111_101, 0xdb, 0b11_001_010]);
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xdc, 0b11_001_010], "vaesenc xmm9, xmm8, xmm10");
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xdc, 0b11_001_010], "vaesenc ymm9, ymm8, ymm10");
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xdd, 0b11_001_010], "vaesenclast xmm9, xmm8, xmm10");
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xdd, 0b11_001_010], "vaesenclast ymm9, ymm8, ymm10");
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xde, 0b11_001_010], "vaesdec xmm9, xmm8, xmm10");
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xde, 0b11_001_010], "vaesdec ymm9, ymm8, ymm10");
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_001, 0xdf, 0b11_001_010], "vaesdeclast xmm9, xmm8, xmm10");
    testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b000_00010, 0b1_0111_101, 0xdf, 0b11_001_010], "vaesdeclast ymm9, ymm8, ymm10");

    // prefix 01
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x10, 0b00_001_010], "vmovsd xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x10, 0b00_001_010], "vmovsd xmm9, qword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_011, 0x10, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0x10, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x10, 0b00_001_010], "vmovupd xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x10, 0b00_001_010], "vmovupd ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x11, 0b00_001_010], "vmovupd xmmword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x11, 0b00_001_010], "vmovupd ymmword [r10], ymm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x11, 0b00_001_010], "vmovsd qword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x11, 0b00_001_010], "vmovsd qword [r10], xmm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_011, 0x11, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0x11, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x10, 0b00_001_010], "vmovupd xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x10, 0b00_001_010], "vmovupd ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x11, 0b00_001_010], "vmovupd xmmword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x11, 0b00_001_010], "vmovupd ymmword [r10], ymm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x10, 0b00_001_010], "vmovss xmm9, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x10, 0b00_001_010], "vmovss xmm9, dword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_010, 0x10, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_110, 0x10, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x10, 0b00_001_010], "vmovups xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x10, 0b00_001_010], "vmovups ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x11, 0b00_001_010], "vmovups xmmword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x11, 0b00_001_010], "vmovups ymmword [r10], ymm9");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x11, 0b11_001_010], "vmovsd xmm10, xmm8, xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x11, 0b11_001_010], "vmovsd xmm10, xmm8, xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x11, 0b00_001_010], "vmovss dword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x11, 0b00_001_010], "vmovss dword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x11, 0b00_001_010], "vmovups xmmword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x11, 0b00_001_010], "vmovups ymmword [r10], ymm9");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x12, 0b00_001_010], "vmovddup xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x12, 0b00_001_010], "vmovddup ymm9, ymmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_011, 0x12, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_111, 0x12, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x12, 0b11_001_010], "vmovhlps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x12, 0b00_001_010], "vmovlps xmm9, xmm8, qword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_100, 0x12, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0x12, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x12, 0b00_001_010], "vmovsldup xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x12, 0b00_001_010], "vmovsldup ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x12, 0b00_001_010], "vmovsldup xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x12, 0b00_001_010], "vmovsldup ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x12, 0b00_001_010], "vmovlpd xmm9, xmm8, qword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x12, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x13, 0b00_001_010], "vmovlpd qword [r10], xmm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x13, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x13, 0b00_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_000, 0x14, 0b00_001_010], "vunpcklps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_100, 0x14, 0b00_001_010], "vunpcklps ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x14, 0b00_001_010], "vunpcklpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x14, 0b00_001_010], "vunpcklpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_000, 0x15, 0b00_001_010], "vunpckhps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_100, 0x15, 0b00_001_010], "vunpckhps ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x15, 0b00_001_010], "vunpckhpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x15, 0b00_001_010], "vunpckhpd ymm9, ymm8, ymmword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x16, 0b11_001_010], "vmovshdup xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x16, 0b11_001_010], "vmovshdup ymm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_010, 0x16, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_110, 0x16, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x16, 0b00_001_010], "vmovhps xmm9, xmm8, qword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_100, 0x16, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x16, 0b00_001_010], "vmovhpd xmm9, xmm8, qword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x16, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x16, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x17, 0b00_001_010], "vmovhps qword [r10], xmm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_100, 0x17, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_000, 0x17, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_100, 0x17, 0b00_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x17, 0b00_001_010], "vmovhpd qword [r10], xmm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x17, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x17, 0b00_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x28, 0b11_001_010], "vmovaps xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x28, 0b11_001_010], "vmovaps ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x29, 0b11_001_010], "vmovaps xmm10, xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x29, 0b11_001_010], "vmovaps ymm10, ymm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x28, 0b11_001_010], "vmovapd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x28, 0b11_001_010], "vmovapd ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x29, 0b11_001_010], "vmovapd xmm10, xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x29, 0b11_001_010], "vmovapd ymm10, ymm9");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2a, 0b11_001_010], "vcvtsi2ss xmm9, xmm0, r10d");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2a, 0b00_001_010], "vcvtsi2ss xmm9, xmm0, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x2a, 0b11_001_010], "vcvtsi2ss xmm9, xmm0, r10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x2a, 0b00_001_010], "vcvtsi2ss xmm9, xmm0, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2a, 0b11_001_010], "vcvtsi2ss xmm9, xmm0, r10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, r10d");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, r10d");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, r10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2a, 0b00_001_010], "vcvtsi2sd xmm9, xmm0, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x2a, 0b00_001_010], "vcvtsi2sd xmm9, xmm0, qword [r10]");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_011, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, edx");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm9, xmm0, edx");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x2b, 0b00_001_010], "vmovntps xmmword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x2b, 0b00_001_010], "vmovntps ymmword [r10], ymm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_000, 0x2b, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_100, 0x2b, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2b, 0b00_001_010], "vmovntpd xmmword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x2b, 0b00_001_010], "vmovntpd ymmword [r10], ymm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0x2b, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x2b, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2c, 0b11_001_010], "vcvttss2si r9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2c, 0b00_001_010], "vcvttss2si r9d, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2c, 0b00_001_010], "vcvttss2si r9, dword [r10]");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_010, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm2");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_010, 0x2c, 0b00_001_010], "vcvttss2si r9d, dword [rdx]");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_110, 0x2c, 0b11_001_010], "vcvttss2si r9d, xmm2");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si r9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si r9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si r9d, qword [r10]");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_011, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm2");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si r9d, xmm2");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si r9d, qword [rdx]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x2d, 0b00_001_010], "vcvtss2si r9d, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x2d, 0b00_001_010], "vcvtss2si r9, dword [r10]");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_010, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm2");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_110, 0x2d, 0b11_001_010], "vcvtss2si r9d, xmm2");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x2d, 0b00_001_010], "vcvtsd2si r9d, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x2d, 0b00_001_010], "vcvtsd2si r9d, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si r9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x2d, 0b00_001_010], "vcvtsd2si r9, qword [r10]");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_011, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm2");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si r9d, xmm2");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2e, 0b00_001_010], "vucomisd xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2e, 0b00_001_010], "vucomisd xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2e, 0b11_001_010], "vucomisd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2e, 0b11_001_010], "vucomisd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2f, 0b00_001_010], "vcomisd xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2f, 0b00_001_010], "vcomisd xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x2f, 0b11_001_010], "vcomisd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x2f, 0b11_001_010], "vcomisd xmm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2e, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2e, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2e, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2e, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2f, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2f, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_001, 0x2f, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_0111_101, 0x2f, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc5, 0b0_1111_000, 0x2e, 0b11_001_010], "vucomiss xmm9, xmm2");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_100, 0x2e, 0b00_001_010], "vucomiss xmm9, dword [rdx]");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_000, 0x2f, 0b11_001_010], "vcomiss xmm9, xmm2");
    testcase!(features { AVX: true } &[0xc5, 0b0_1111_100, 0x2f, 0b00_001_010], "vcomiss xmm9, dword [rdx]");
    testcase!(invalid: &[0xc5, 0b0_1111_111, 0x2f, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x50, 0b11_001_010], "vmovmskps r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x50, 0b11_001_010], "vmovmskps r9d, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_000, 0x50, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_100, 0x50, 0b00_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x50, 0b11_001_010], "vmovmskpd r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x50, 0b11_001_010], "vmovmskpd r9d, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x50, 0b11_001_010], "vmovmskpd r9d, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x50, 0b11_001_010], "vmovmskpd r9d, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0x50, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_101, 0x50, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_001, 0x50, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_101, 0x50, 0b00_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x51, 0b00_001_010], "vsqrtpd xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x51, 0b00_001_010], "vsqrtpd ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_011, 0x51, 0b00_001_010], "vsqrtsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_111, 0x51, 0b00_001_010], "vsqrtsd xmm9, xmm8, qword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x51, 0b00_001_010], "vsqrtps xmm9, xmmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_000, 0x51, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x51, 0b00_001_010], "vsqrtps ymm9, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x51, 0b00_001_010], "vsqrtss xmm9, xmm0, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x51, 0b00_001_010], "vsqrtss xmm9, xmm0, dword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x52, 0b11_001_010], "vrsqrtps xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x52, 0b11_001_010], "vrsqrtps ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x52, 0b11_001_010], "vrsqrtss xmm9, xmm0, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x52, 0b11_001_010], "vrsqrtss xmm9, xmm0, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x53, 0b11_001_010], "vrcpps xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x53, 0b11_001_010], "vrcpps ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x53, 0b11_001_010], "vrcpss xmm9, xmm0, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x53, 0b11_001_010], "vrcpss xmm9, xmm0, xmm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x54, 0b11_001_010], "vandps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x54, 0b11_001_010], "vandps ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x55, 0b11_001_010], "vandnps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x55, 0b11_001_010], "vandnps ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x54, 0b00_001_010], "vandpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x54, 0b00_001_010], "vandpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x55, 0b00_001_010], "vandnpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x55, 0b00_001_010], "vandnpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x56, 0b00_001_010], "vorpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x56, 0b00_001_010], "vorpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x56, 0b00_001_010], "vorps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x56, 0b00_001_010], "vorps ymm9, ymm8, ymmword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x57, 0b11_001_010], "vxorps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x57, 0b11_001_010], "vxorps ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x57, 0b11_001_010], "vxorpd xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x57, 0b11_001_010], "vxorpd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x58, 0b11_001_010], "vaddps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x58, 0b11_001_010], "vaddps ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x58, 0b11_001_010], "vaddss xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x58, 0b11_001_010], "vaddss xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x58, 0b00_001_010], "vaddss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x58, 0b00_001_010], "vaddss xmm9, xmm8, dword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x58, 0b00_001_010], "vaddpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x58, 0b00_001_010], "vaddpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x58, 0b00_001_010], "vaddsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x58, 0b00_001_010], "vaddsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x59, 0b00_001_010], "vmulps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x59, 0b00_001_010], "vmulps ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x59, 0b00_001_010], "vmulpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x59, 0b00_001_010], "vmulpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x59, 0b00_001_010], "vmulss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x59, 0b00_001_010], "vmulss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x59, 0b00_001_010], "vmulsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x59, 0b00_001_010], "vmulsd xmm9, xmm8, qword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5a, 0b11_001_010], "vcvtps2pd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5a, 0b11_001_010], "vcvtps2pd ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5a, 0b00_001_010], "vcvtps2pd xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5a, 0b00_001_010], "vcvtps2pd ymm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x5a, 0b11_001_010], "vcvtpd2ps xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x5a, 0b11_001_010], "vcvtpd2ps xmm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x5a, 0b11_001_010], "vcvtsd2ss xmm9, xmm0, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x5a, 0b11_001_010], "vcvtsd2ss xmm9, xmm0, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_011, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_111, 0x5a, 0b00_001_010], "vcvtsd2ss xmm9, xmm0, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x5a, 0b11_001_010], "vcvtss2sd xmm9, xmm0, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x5a, 0b11_001_010], "vcvtss2sd xmm9, xmm0, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x5a, 0b00_001_010], "vcvtss2sd xmm9, xmm0, dword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x5b, 0b11_001_010], "vcvtps2dq xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x5b, 0b11_001_010], "vcvtps2dq ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x5b, 0b11_001_010], "vcvttps2dq xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x5b, 0b11_001_010], "vcvttps2dq ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5b, 0b11_001_010], "vcvtdq2ps xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_000, 0x5b, 0b00_001_010], "vcvtdq2ps xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5b, 0b11_001_010], "vcvtdq2ps ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_100, 0x5b, 0b00_001_010], "vcvtdq2ps ymm9, ymmword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0x5c, 0b00_001_010], "vsubps xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_100, 0x5c, 0b00_001_010], "vsubps ymm9, ymm0, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_010, 0x5c, 0b00_001_010], "vsubss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_110, 0x5c, 0b00_001_010], "vsubss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x5c, 0b00_001_010], "vsubpd xmm9, xmm0, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x5c, 0b00_001_010], "vsubpd ymm9, ymm0, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_011, 0x5c, 0b00_001_010], "vsubsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_111, 0x5c, 0b00_001_010], "vsubsd xmm9, xmm8, qword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5d, 0b00_001_010], "vminps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x5d, 0b00_001_010], "vminps ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x5d, 0b00_001_010], "vminss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x5d, 0b00_001_010], "vminss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x5d, 0b00_001_010], "vminpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x5d, 0b00_001_010], "vminpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x5d, 0b00_001_010], "vminsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x5d, 0b00_001_010], "vminsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5e, 0b00_001_010], "vdivps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5e, 0b00_001_010], "vdivps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x5e, 0b00_001_010], "vdivpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x5e, 0b00_001_010], "vdivss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x5e, 0b00_001_010], "vdivsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x5e, 0b00_001_010], "vdivps ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x5e, 0b00_001_010], "vdivpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x5e, 0b00_001_010], "vdivss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x5e, 0b00_001_010], "vdivsd xmm9, xmm8, qword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0x5f, 0b00_001_010], "vmaxps xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x5f, 0b00_001_010], "vmaxpd xmm9, xmm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_010, 0x5f, 0b00_001_010], "vmaxss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x5f, 0b00_001_010], "vmaxsd xmm9, xmm8, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0x5f, 0b00_001_010], "vmaxps ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x5f, 0b00_001_010], "vmaxpd ymm9, ymm8, ymmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_110, 0x5f, 0b00_001_010], "vmaxss xmm9, xmm8, dword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x5f, 0b00_001_010], "vmaxsd xmm9, xmm8, qword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x60, 0b11_001_010], "vpunpcklbw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x60, 0b11_001_010], "vpunpcklbw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x61, 0b11_001_010], "vpunpcklwd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x61, 0b11_001_010], "vpunpcklwd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x62, 0b11_001_010], "vpunpckldq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x62, 0b11_001_010], "vpunpckldq ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x63, 0b11_001_010], "vpacksswb xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x63, 0b11_001_010], "vpacksswb ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x64, 0b11_001_010], "vpcmpgtb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x64, 0b11_001_010], "vpcmpgtb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x65, 0b11_001_010], "vpcmpgtw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x65, 0b11_001_010], "vpcmpgtw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x66, 0b11_001_010], "vpcmpgtd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x66, 0b11_001_010], "vpcmpgtd ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x67, 0b11_001_010], "vpackuswb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x67, 0b11_001_010], "vpackuswb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x68, 0b11_001_010], "vpunpckhbw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x68, 0b11_001_010], "vpunpckhbw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x69, 0b11_001_010], "vpunpckhwd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x69, 0b11_001_010], "vpunpckhwd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x6a, 0b11_001_010], "vpunpckhdq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x6a, 0b11_001_010], "vpunpckhdq ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0x6b, 0b11_001_010], "vpackssdw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0x6b, 0b11_001_010], "vpackssdw ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6c, 0b11_001_010], "vpunpcklqdq xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x6c, 0b11_001_010], "vpunpcklqdq ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6d, 0b11_001_010], "vpunpckhqdq xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x6d, 0b11_001_010], "vpunpckhqdq ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6e, 0b11_001_010], "vmovq xmm9, r10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x6e, 0b00_001_010], "vmovq xmm9, qword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xd6, 0b00_001_010], "vmovq qword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xd6, 0b11_001_010], "vmovq xmm10, xmm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x6e, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x6f, 0b11_001_010], "vmovdqa xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x6f, 0b11_001_010], "vmovdqa ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x6f, 0b11_001_010], "vmovdqu xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x6f, 0b11_001_010], "vmovdqu ymm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x70, 0b11_001_010, 0x77], "vpshufd xmm9, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x70, 0b11_001_010, 0x77], "vpshufd ymm9, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0x70, 0b11_001_010, 0x77], "vpshufhw xmm9, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0x70, 0b11_001_010, 0x77], "vpshufhw ymm9, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0x70, 0b11_001_010, 0x77], "vpshuflw xmm9, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0x70, 0b11_001_010, 0x77], "vpshuflw ymm9, ymm10, 0x77");


    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_010_010, 0x77], "vpsrlw xmm0, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x71, 0b11_010_010, 0x77], "vpsrlw xmm8, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x71, 0b11_010_010, 0x77], "vpsrlw ymm0, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b00_011_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_100_010, 0x77], "vpsraw xmm0, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x71, 0b11_100_010, 0x77], "vpsraw ymm0, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_101_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x71, 0b11_110_010, 0x77], "vpsllw xmm0, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x71, 0b11_110_010, 0x77], "vpsllw ymm0, ymm10, 0x77");

    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_000_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b11_010_010, 0x77], "vpsrld xmm0, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_010_010, 0x77], "vpsrld ymm0, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_011_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b11_100_010, 0x77], "vpsrad xmm0, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_100_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_100_010, 0x77], "vpsrad ymm0, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_101_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b11_110_010, 0x77], "vpslld xmm0, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_110_010, 0x77]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x72, 0b11_110_010, 0x77], "vpslld ymm0, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x72, 0b00_111_010, 0x77]);

    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_000_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_010_010, 0x77], "vpsrlq xmm0, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_011_010, 0x77], "vpsrldq xmm0, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_010_010, 0x77], "vpsrlq ymm0, ymm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_011_010, 0x77], "vpsrldq ymm0, ymm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_100_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_101_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_110_010, 0x77], "vpsllq xmm0, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x73, 0b11_111_010, 0x77], "vpslldq xmm0, xmm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_110_010, 0x77], "vpsllq ymm0, ymm10, 0x77");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0x73, 0b11_111_010, 0x77], "vpslldq ymm0, ymm10, 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x74, 0b11_001_010], "vpcmpeqb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x74, 0b11_001_010], "vpcmpeqb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x75, 0b11_001_010], "vpcmpeqw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x75, 0b11_001_010], "vpcmpeqw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x76, 0b11_001_010], "vpcmpeqd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x76, 0b11_001_010], "vpcmpeqd ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x7c, 0b11_001_010], "vhaddpd xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x7c, 0b11_001_010], "vhaddpd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x7c, 0b11_001_010], "vhaddps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x7c, 0b11_001_010], "vhaddps ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0x7d, 0b11_001_010], "vhsubpd xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0x7d, 0b11_001_010], "vhsubpd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0x7d, 0b11_001_010], "vhsubps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0x7d, 0b11_001_010], "vhsubps ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x7e, 0b11_001_010], "vmovd r10d, xmm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_101, 0x7e, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0x7e, 0b11_001_010], "vmovq r10, xmm9");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0x7e, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0x7f, 0b11_001_010], "vmovdqa xmm10, xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0x7f, 0b11_001_010], "vmovdqa ymm10, ymm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_010, 0x7f, 0b11_001_010], "vmovdqu xmm10, xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_110, 0x7f, 0b11_001_010], "vmovdqu ymm10, ymm9");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b00_010_001], "vldmxcsr dword [r9]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_100, 0xae, 0b00_010_001]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b11_010_001]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b00_011_001], "vstmxcsr dword [r9]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_100, 0xae, 0b00_011_001]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_000, 0xae, 0b11_011_001]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_000, 0xc2, 0b11_001_010, 0x77], "vcmpps xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_100, 0xc2, 0b11_001_010, 0x77], "vcmpps ymm9, ymm8, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xc2, 0b11_001_010, 0x77], "vcmppd xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xc2, 0b11_001_010, 0x77], "vcmppd ymm9, ymm8, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0xc2, 0b11_001_010, 0x77], "vcmpsd xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0xc2, 0b11_001_010, 0x77], "vcmpsd xmm9, xmm8, xmm10, 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xc4, 0b11_001_010, 0x77], "vpinsrw xmm9, xmm8, r10d, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xc4, 0b11_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0xc5, 0b00_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0xc5, 0b11_001_010, 0x77], "vpextrw r9d, xmm10, 0x77");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0xc5, 0b00_001_010, 0x77]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xc5, 0b11_001_010, 0x77]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xc6, 0b11_001_010, 0x77], "vshufpd xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xc6, 0b11_001_010, 0x77], "vshufpd ymm9, ymm8, ymm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_000, 0xc6, 0b11_001_010, 0x77], "vshufps xmm9, xmm8, xmm10, 0x77");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_100, 0xc6, 0b11_001_010, 0x77], "vshufps ymm9, ymm8, ymm10, 0x77");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd0, 0b11_001_010], "vaddsubpd xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd0, 0b11_001_010], "vaddsubpd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_011, 0xd0, 0b11_001_010], "vaddsubps xmm9, xmm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_111, 0xd0, 0b11_001_010], "vaddsubps ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd1, 0b11_001_010], "vpsrlw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd1, 0b11_001_010], "vpsrlw ymm9, ymm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd1, 0b00_001_010], "vpsrlw ymm9, ymm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd2, 0b11_001_010], "vpsrld xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd2, 0b11_001_010], "vpsrld ymm9, ymm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd2, 0b00_001_010], "vpsrld ymm9, ymm8, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd3, 0b11_001_010], "vpsrlq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd3, 0b11_001_010], "vpsrlq ymm9, ymm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd3, 0b00_001_010], "vpsrlq ymm9, ymm8, xmmword [r10]");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd4, 0b11_001_010], "vpaddq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd4, 0b11_001_010], "vpaddq ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xd5, 0b11_001_010], "vpmullw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xd5, 0b11_001_010], "vpmullw ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_1111_001, 0xd7, 0b11_001_010], "vpmovmskb r9d, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b0_1111_001, 0xd7, 0b00_001_010]);
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_1111_101, 0xd7, 0b11_001_010], "vpmovmskb r9d, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd8, 0b11_001_010], "vpsubusb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd8, 0b11_001_010], "vpsubusb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xd9, 0b11_001_010], "vpsubusw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xd9, 0b11_001_010], "vpsubusw ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xda, 0b11_001_010], "vpminub xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xda, 0b11_001_010], "vpminub ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdb, 0b11_001_010], "vpand xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdb, 0b11_001_010], "vpand ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdc, 0b11_001_010], "vpaddusb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdc, 0b11_001_010], "vpaddusb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdd, 0b11_001_010], "vpaddusw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdd, 0b11_001_010], "vpaddusw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xde, 0b11_001_010], "vpmaxub xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xde, 0b11_001_010], "vpmaxub ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xdf, 0b11_001_010], "vpandn xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xdf, 0b11_001_010], "vpandn ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe0, 0b11_001_010], "vpavgb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe0, 0b11_001_010], "vpavgb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe1, 0b11_001_010], "vpsraw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe1, 0b11_001_010], "vpsraw ymm9, ymm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe2, 0b11_001_010], "vpsrad xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe2, 0b11_001_010], "vpsrad ymm9, ymm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe3, 0b11_001_010], "vpavgw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe3, 0b11_001_010], "vpavgw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe4, 0b11_001_010], "vpmulhuw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe4, 0b11_001_010], "vpmulhuw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe5, 0b11_001_010], "vpmulhw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe5, 0b11_001_010], "vpmulhw ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xe6, 0b11_001_010], "vcvttpd2dq xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xe6, 0b11_001_010], "vcvttpd2dq xmm9, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_010, 0xe6, 0b11_001_010], "vcvtdq2pd xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_110, 0xe6, 0b11_001_010], "vcvtdq2pd ymm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0xe6, 0b11_001_010], "vcvtpd2dq xmm9, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0xe6, 0b11_001_010], "vcvtpd2dq xmm9, ymm10");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0xe7, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xe7, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xe7, 0b00_001_010], "vmovntdq xmmword [r10], xmm9");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xe7, 0b00_001_010], "vmovntdq ymmword [r10], ymm9");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe8, 0b11_001_010], "vpsubsb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe8, 0b11_001_010], "vpsubsb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xe9, 0b11_001_010], "vpsubsw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xe9, 0b11_001_010], "vpsubsw ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xea, 0b11_001_010], "vpminsw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xea, 0b11_001_010], "vpminsw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xeb, 0b11_001_010], "vpor xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xeb, 0b11_001_010], "vpor ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xec, 0b11_001_010], "vpaddsb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xec, 0b11_001_010], "vpaddsb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xed, 0b11_001_010], "vpaddsw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xed, 0b11_001_010], "vpaddsw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xee, 0b11_001_010], "vpmaxsw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xee, 0b11_001_010], "vpmaxsw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b0_0111_001, 0xef, 0b11_001_010], "vpxor xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b0_0111_101, 0xef, 0b11_001_010], "vpxor ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_011, 0xf0, 0b00_001_010], "vlddqu xmm9, xmmword [r10]");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_111, 0xf0, 0b00_001_010], "vlddqu ymm9, ymmword [r10]");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_011, 0xf0, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_011, 0xf0, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_0111_111, 0xf0, 0b11_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_111, 0xf0, 0b11_001_010]);
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf1, 0b11_001_010], "vpsllw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf1, 0b11_001_010], "vpsllw ymm9, ymm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf2, 0b11_001_010], "vpslld xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf2, 0b11_001_010], "vpslld ymm9, ymm8, xmm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf3, 0b11_001_010], "vpsllq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf3, 0b11_001_010], "vpsllq ymm9, ymm8, xmm10");


    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf4, 0b11_001_010], "vpmuludq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf4, 0b11_001_010], "vpmuludq ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xf5, 0b11_001_010], "vpmaddwd xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xf5, 0b11_001_010], "vpmaddwd ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xf6, 0b11_001_010], "vpsadbw xmm9, xmm0, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_1111_101, 0xf6, 0b11_001_010], "vpsadbw ymm9, ymm0, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_1111_001, 0xf7, 0b11_001_010], "vmaskmovdqu xmm9, xmm10");
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_001, 0xf7, 0b00_001_010]);
    testcase!(invalid: &[0xc4, 0b000_00001, 0b1_1111_101, 0xf7, 0b11_001_010]);

    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf8, 0b11_001_010], "vpsubb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf8, 0b11_001_010], "vpsubb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xf9, 0b11_001_010], "vpsubw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xf9, 0b11_001_010], "vpsubw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfa, 0b11_001_010], "vpsubd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfa, 0b11_001_010], "vpsubd ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfb, 0b11_001_010], "vpsubq xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfb, 0b11_001_010], "vpsubq ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfc, 0b11_001_010], "vpaddb xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfc, 0b11_001_010], "vpaddb ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfd, 0b11_001_010], "vpaddw xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfd, 0b11_001_010], "vpaddw ymm9, ymm8, ymm10");
    testcase!(features { AVX: true } &[0xc4, 0b000_00001, 0b1_0111_001, 0xfe, 0b11_001_010], "vpaddd xmm9, xmm8, xmm10");
    testcase!(features { AVX2: true } &[0xc4, 0b000_00001, 0b1_0111_101, 0xfe, 0b11_001_010], "vpaddd ymm9, ymm8, ymm10");

    testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0x00], "vmovups xmm0, xmmword [rax]");
    testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0x01], "vmovups xmm0, xmmword [rcx]");
    testcase!(features { AVX: true } &[0xc5, 0x78, 0x10, 0x0f], "vmovups xmm9, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0xcf], "vmovups xmm1, xmm7");
    testcase!(features { AVX: true } &[0xc5, 0xf9, 0x10, 0x0f], "vmovupd xmm1, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xfa, 0x7e, 0x10], "vmovq xmm2, qword [rax]");
    testcase!(features { AVX: true } &[0xc5, 0xfc, 0x10, 0x0f], "vmovups ymm1, ymmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xfd, 0x10, 0x0f], "vmovupd ymm1, ymmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xfe, 0x10, 0x0f], "vmovss xmm1, dword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xff, 0x10, 0xcf], "vmovsd xmm1, xmm0, xmm7");
    testcase!(features { AVX: true } &[0xc5, 0xff, 0x10, 0x01], "vmovsd xmm0, qword [rcx]");
    testcase!(features { AVX: true } &[0xc5, 0xf9, 0x6e, 0xc6], "vmovd xmm0, esi");
    testcase!(features { AVX: true } &[0xc5, 0xf9, 0x6e, 0x13], "vmovd xmm2, dword [rbx]");
    testcase!(features { AVX: true } &[0xc5, 0xf9, 0x7e, 0xc6], "vmovd esi, xmm0");
    testcase!(features { AVX: true } &[0xc5, 0xf9, 0x7e, 0x13], "vmovd dword [rbx], xmm2");
    test_instr_invalid(&[0x4f, 0xc5, 0xf8, 0x10, 0x00]);
    test_instr_invalid(&[0xf0, 0xc5, 0xf8, 0x10, 0x00]);
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x00, 0x0f], "vpshufb xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x00, 0x0f], "vpshufb ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x00, 0xcd], "vpshufb xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x00, 0xcd], "vpshufb ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x01, 0x0f], "vphaddw xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x01, 0x0f], "vphaddw ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x01, 0xcd], "vphaddw xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x01, 0xcd], "vphaddw ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x02, 0x0f], "vphaddd xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x02, 0x0f], "vphaddd ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x02, 0xcd], "vphaddd xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x02, 0xcd], "vphaddd ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x03, 0x0f], "vphaddsw xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x03, 0x0f], "vphaddsw ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x03, 0xcd], "vphaddsw xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x03, 0xcd], "vphaddsw ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x04, 0x0f], "vpmaddubsw xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x04, 0x0f], "vpmaddubsw ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x04, 0xcd], "vpmaddubsw xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x04, 0xcd], "vpmaddubsw ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x05, 0x0f], "vphsubw xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x05, 0x0f], "vphsubw ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x05, 0xcd], "vphsubw xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x05, 0xcd], "vphsubw ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x06, 0x0f], "vphsubd xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x06, 0x0f], "vphsubd ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x06, 0xcd], "vphsubd xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x06, 0xcd], "vphsubd ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x07, 0x0f], "vphsubsw xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x07, 0x0f], "vphsubsw ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x07, 0xcd], "vphsubsw xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x07, 0xcd], "vphsubsw ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x08, 0x0f], "vpsignb xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x08, 0x0f], "vpsignb ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x08, 0xcd], "vpsignb xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x08, 0xcd], "vpsignb ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x09, 0x0f], "vpsignw xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x09, 0x0f], "vpsignw ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x09, 0xcd], "vpsignw xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x09, 0xcd], "vpsignw ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0a, 0x0f], "vpsignd xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0a, 0x0f], "vpsignd ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0a, 0xcd], "vpsignd xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0a, 0xcd], "vpsignd ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0b, 0x0f], "vpmulhrsw xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0b, 0x0f], "vpmulhrsw ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0b, 0xcd], "vpmulhrsw xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0b, 0xcd], "vpmulhrsw ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0c, 0x0f], "vpermilps xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0c, 0x0f], "vpermilps ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0c, 0xcd], "vpermilps xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0c, 0xcd], "vpermilps ymm9, ymm1, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0d, 0x0f], "vpermilpd xmm9, xmm1, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0d, 0x0f], "vpermilpd ymm9, ymm1, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x71, 0x0d, 0xcd], "vpermilpd xmm9, xmm1, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x75, 0x0d, 0xcd], "vpermilpd ymm9, ymm1, ymm13");
    test_instr_invalid(&[0xc4, 0x02, 0x71, 0x0e, 0x00]);
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0e, 0x0f], "vtestps xmm9, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0e, 0x0f], "vtestps ymm9, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0e, 0xcd], "vtestps xmm9, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0e, 0xcd], "vtestps ymm9, ymm13");
    test_instr_invalid(&[0xc4, 0x02, 0x71, 0x0f, 0x00]);
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0f, 0x0f], "vtestpd xmm9, xmmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0f, 0x0f], "vtestpd ymm9, ymmword [r15]");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x79, 0x0f, 0xcd], "vtestpd xmm9, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x7d, 0x0f, 0xcd], "vtestpd ymm9, ymm13");
    testcase!(features { AVX: true } &[0xc4, 0xe2, 0x65, 0x90, 0x04, 0x51], "vpgatherdd ymm0, dword [rcx + ymm2 * 2], ymm3");
    testcase!(features { AVX: true } &[0xc4, 0xe2, 0xe5, 0x90, 0x04, 0x51], "vpgatherdq ymm0, qword [rcx + xmm2 * 2], ymm3");
    testcase!(features { AVX: true } &[0xc4, 0xe2, 0x65, 0x91, 0x04, 0x51], "vpgatherqd xmm0, dword [rcx + ymm2 * 2], xmm3");
    testcase!(features { AVX: true } &[0xc4, 0xe2, 0xe5, 0x91, 0x04, 0x51], "vpgatherqq ymm0, qword [rcx + ymm2 * 2], ymm3");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x09, 0x9d, 0xcd], "vfnmadd132ss xmm9, xmm14, xmm13");
    testcase!(features { AVX: true } &[0xc4, 0x02, 0x89, 0x9d, 0xcd], "vfnmadd132sd xmm9, xmm14, xmm13");
// ...
    testcase!(features { AVX: true } &[0xc4, 0xe3, 0x79, 0x14, 0xd0, 0x0a], "vpextrb eax, xmm2, 0xa");
    testcase!(features { AVX: true } &[0xc4, 0xe3, 0x79, 0x14, 0x10, 0x0a], "vpextrb byte [rax], xmm2, 0xa");
    test_instr_invalid(&[0xc4, 0xe3, 0xf9, 0x14, 0x00, 0xd0]);
    test_instr_invalid(&[0xc4, 0xe3, 0xf9, 0x14, 0x00, 0x0a]);
    testcase!(features { AVX: true } &[0xc5, 0xed, 0x71, 0xd0, 0x04], "vpsrlw ymm2, ymm0, 0x4");
    testcase!(features { AVX: true } &[0xc5, 0xed, 0x73, 0xd4, 0x20], "vpsrlq ymm2, ymm4, 0x20");
    testcase!(features { AVX: true } &[0xc4, 0xe3, 0xfd, 0x00, 0xc1, 0xa8], "vpermq ymm0, ymm1, 0xa8");
    testcase!(features { AVX: true } &[0xc5, 0xfd, 0xea, 0xd1], "vpminsw ymm2, ymm0, ymm1");
    testcase!(features { AVX: true } &[0xc5, 0xfd, 0xee, 0xd9], "vpmaxsw ymm3, ymm0, ymm1");
    testcase!(features { AVX: true } &[0xc4, 0xe3, 0x7d, 0x19, 0xd1, 0x01], "vextractf128 xmm1, ymm2, 0x1");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x38, 0x7c, 0x12, 0x05, 0x01], "vinserti128 ymm7, ymm1, xmmword [r10 + rdx * 1 + 0x5], 0x1");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x42, 0x7c, 0x12, 0x05, 0x61], "vmpsadbw ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], 0x61");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x46, 0x7c, 0x12, 0x05, 0x61], "vperm2i128 ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], 0x61");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x4a, 0x7c, 0x12, 0x05, 0x61], "vblendvps ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], ymm6");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4a, 0x7c, 0x12, 0x05, 0x61], "vblendvps xmm7, xmm1, xmmword [r10 + rdx * 1 + 0x5], xmm6");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4a, 0xdc, 0x61], "vblendvps xmm3, xmm1, xmm12, xmm6");
    testcase!(invalid: &[0xc4, 0xc3, 0xf1, 0x4a, 0xdc, 0x61]);
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x75, 0x4b, 0x7c, 0x12, 0x05, 0x61], "vblendvpd ymm7, ymm1, ymmword [r10 + rdx * 1 + 0x5], ymm6");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4b, 0x7c, 0x12, 0x05, 0x61], "vblendvpd xmm7, xmm1, xmmword [r10 + rdx * 1 + 0x5], xmm6");
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4b, 0xdc, 0x61], "vblendvpd xmm3, xmm1, xmm12, xmm6");
    testcase!(invalid: &[0xc4, 0xc3, 0xf1, 0x4b, 0xdc, 0x61]);
    testcase!(features { AVX: true } &[0xc4, 0xc3, 0x71, 0x4c, 0x7c, 0x12, 0x05, 0x61], "vpblendvb xmm7, xmm1, xmmword [r10 + rdx * 1 + 0x5], xmm6");

    testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf1, 0x0f], "vpsllw xmm1, xmm6, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf1, 0xcf], "vpsllw xmm1, xmm6, xmm7");
    testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf1, 0x0f], "vpsllw ymm1, ymm6, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf1, 0xcf], "vpsllw ymm1, ymm6, xmm7");
    testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf2, 0x0f], "vpslld xmm1, xmm6, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf2, 0xcf], "vpslld xmm1, xmm6, xmm7");
    testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf2, 0x0f], "vpslld ymm1, ymm6, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf2, 0xcf], "vpslld ymm1, ymm6, xmm7");
    testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf3, 0x0f], "vpsllq xmm1, xmm6, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xc9, 0xf3, 0xcf], "vpsllq xmm1, xmm6, xmm7");
    testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf3, 0x0f], "vpsllq ymm1, ymm6, xmmword [rdi]");
    testcase!(features { AVX: true } &[0xc5, 0xcd, 0xf3, 0xcf], "vpsllq ymm1, ymm6, xmm7");

    testcase!(features { AVX: true } &[0xc5, 0xf1, 0xc4, 0xd8, 0x78], "vpinsrw xmm3, xmm1, eax, 0x78");
    testcase!(features { AVX: true } &[0xc5, 0xf1, 0xc4, 0x18, 0x78], "vpinsrw xmm3, xmm1, word [rax], 0x78");

    testcase!(features { AVX: true } &[0xc5, 0xe0, 0x54, 0x03], "vandps xmm0, xmm3, xmmword [rbx]");
    testcase!(features { AVX: true } &[0xc5, 0xe1, 0x54, 0x03], "vandpd xmm0, xmm3, xmmword [rbx]");
    testcase!(features { AVX: true } &[0xc5, 0xe0, 0x55, 0x03], "vandnps xmm0, xmm3, xmmword [rbx]");
    testcase!(features { AVX: true } &[0xc5, 0xe1, 0x55, 0x03], "vandnpd xmm0, xmm3, xmmword [rbx]");
    testcase!(features { AVX: true } &[0xc5, 0xe0, 0x56, 0x03], "vorps xmm0, xmm3, xmmword [rbx]");
    testcase!(features { AVX: true } &[0xc5, 0xe1, 0x56, 0x03], "vorpd xmm0, xmm3, xmmword [rbx]");
    testcase!(features { AVX: true } &[0xc4, 0xa2, 0x15, 0x3e, 0x14, 0xb9], "vpmaxuw ymm2, ymm13, ymmword [rcx + r15 * 4]");
}
