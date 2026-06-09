mod regspec;
mod operand;
mod opcode;
#[cfg(feature="fmt")]
mod display;
mod evex_generated;
#[cfg(feature="behavior")]
mod behavior;

use std::fmt::Write;

use yaxpeax_arch::{Decoder, LengthedInstruction};
use yaxpeax_x86::protected_mode::{Instruction, InstDecoder};
#[cfg(feature="fmt")]
use yaxpeax_x86::protected_mode::DisplayStyle;

use crate::tools::{self, CodeModel};

fn test_invalid(data: &[u8]) {
    test_invalid_under(&InstDecoder::default(), data);
}

fn test_invalid_under(decoder: &InstDecoder, data: &[u8]) {
    let mut reader = yaxpeax_arch::U8Reader::new(data);
    if let Ok(inst) = decoder.decode(&mut reader) {
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

fn test_decode_under(decoder: &InstDecoder, data: &[u8], expected: &'static str) -> Instruction {
    let mut reader = yaxpeax_arch::U8Reader::new(data);
    let instr = match decoder.decode(&mut reader) {
        Ok(instr) => {
            assert_eq!(instr.len().to_const(), data.len() as u32, "instruction length is incorrect");
            instr
        },
        Err(e) => {
            let mut hex = String::new();
            for b in data {
                write!(hex, "{:02x}", b).unwrap();
            }
            cfg_if::cfg_if! {
                if #[cfg(feature="fmt")] {
                    panic!("decode error ({}) for {} under decoder {}:\n  expected: {}\n", e, hex, decoder, expected);
                } else {
                    // avoid the unused `e` warning
                    let _ = e;
                    panic!("decode error (<non-fmt build>) for {} under decoder <non-fmt build>:\n  expected: {}\n", hex, expected);
                }
            }
        }
    };
    instr
}

fn test_display_under(decoder: &InstDecoder, data: &[u8], expected: &'static str) {
    // testing that the instruction displays doesn't work if formatting is disabled, but we can
    // test that it at least decodes..
    let _instr = test_decode_under(decoder, data, expected);

    #[cfg(feature="fmt")]
    test_display_format(decoder, data, expected, DisplayStyle::Intel);
}

#[cfg(feature="fmt")]
fn test_display_format(decoder: &InstDecoder, data: &[u8], expected: &'static str, style: DisplayStyle) {
    let instr = test_decode_under(decoder, data, expected);

    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }

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
            let mut formatter = yaxpeax_x86::protected_mode::InstructionTextBuffer::new();
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
            let mut formatter = yaxpeax_x86::protected_mode::InstructionTextBuffer::new();
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
                yaxpeax_x86::protected_mode::InstDecoder::default()
            }
            FeatureSet::Minimal => {
                yaxpeax_x86::protected_mode::InstDecoder::minimal()
            }
            FeatureSet::Intel => {
                yaxpeax_x86::protected_mode::InstDecoder::minimal().with_intel_quirks()
            }
            FeatureSet::Amd => {
                yaxpeax_x86::protected_mode::InstDecoder::minimal().with_amd_quirks()
            }
            FeatureSet::DecoderK8 => {
                yaxpeax_x86::protected_mode::uarch::amd::k8()
            }
            FeatureSet::DecoderBulldozer => {
                yaxpeax_x86::protected_mode::uarch::amd::bulldozer()
            }
            FeatureSet::DecoderNetburst => {
                yaxpeax_x86::protected_mode::uarch::intel::netburst()
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
    decodes: Option<Disasm>,
}

fn check_decodes(decoder: &InstDecoder, decode_ok: bool, bytes: &[u8], disasm: &Disasm) {
    if decode_ok {
        test_display_under(&decoder, bytes, disasm.display);
        #[cfg(feature = "fmt")]
        if let Some(c_style) = disasm.c.as_ref() {
            test_display_format(&decoder, bytes, c_style, DisplayStyle::C);
        }

        // if EXTERNAL_MASM is set we actually want to validate decodes against masm/dumpbin. this
        // is a bit convoluted. otherwise we're testing against in-tree "gold output". in the
        // EXTERNAL_MASM case we actually distrust this too, and validate the in-tree expected
        // output is what masm actually wants.
        #[cfg(feature = "fmt")]
        if std::env::var_os("EXTERNAL_MASM").is_some() {
            eprintln!("==== running testcase: bytes={:x?}, expected_display={}", bytes, disasm.display);
            // OK: EXTERNAL_MASM is set, we'll expect that there's `../tools/` which has `wibo`,
            // `mlexe`, and `dumpbin.exe`.

            // outside long mode, dumpbin behaves poorly in the face of vex/evex-encoded instructions. for instructions that include scalar registers,
            // vex.w is often respected and extends dword instructions into qword instructions on rax and friends. wild! for SIMD registers, the
            // register number extension bits are often respected, resulting in xmm8, ymm9, etc, appearing in protected mode. also wild!
            // further, some instructions are overly-strict and will treat VEX.LIG as VEX.L0 in protected mode, so we a whole mess to deal with.
            // lots of "successful" incorrect decodes, some "unsuccessful" decodes of valid-to-hardware instructions. what to do?
            // if we fail to decode, and there's a c4/c5 up front, allow it in this specific branch because it might be a dumpbin bug.
            // if we successfully decode and there's a rax/rcx/rdx/rbx/etc or "qword ptr" and it doesn't match the expectation, log it and move on because it might be a dumpbin bug.
            // everything else is .. probably fine?
            let vex_prefixed = bytes[0] == 0xc4 || bytes[0] == 0xc5;

            // match against some testcases that are known to be wrong by MASM/dumpbin.
            let external_masm_ish = match bytes {
                &[0xf1] => "int 1".to_string(),  // dumpbin does not know how to decode f1...
                &[0xe5, 0x99] => "in eax, 99h".to_string(), // this is a MASM/dumpbin bug. see notes on testcase.
                &[0xe7, 0x99] => "out 99h, eax".to_string(), // this is a MASM/dumpbin bug. see notes on testcase.
                // dumpbin prints the instruction as if it was encoded in 32-bit form regardless of object file, so overrule it.
                &[0xf3, 0x0f, 0xc7, 0xfd] => "rdpid ebp".to_string(),
                &[0x0f, 0x18, 0xc0] => "nop eax".to_string(), // dumpbin would love to call this "prefetchnta eax" ???
                &[0x0f, 0x18, 0xcc] => "nop esp".to_string(), // dumpbin would love to call this "prefetchnta esp" ???
                &[0x0f, 0x18, 0x20] => "nop zmmword ptr [eax]".to_string(), // getting around dumpbin knowing about prefetchrst2..
                &[0x0f, 0x19, 0x20] => "nop dword ptr [eax]".to_string(), // dumpbin doesn't know about 0f19..
                &[0x0f, 0x1a, 0x20] => "nop dword ptr [eax]".to_string(), // dumpbin wants to call this bndldx, yax doesn't do MPX yet
                &[0x0f, 0x1b, 0x20] => "nop dword ptr [eax]".to_string(), // dumpbin wants to call this bndstx, yax doesn't do MPX yet
                &[0x0f, 0x1c, 0x20] => "nop dword ptr [eax]".to_string(), // dumpbin doesn't know about 0f1c..
                &[0x0f, 0x1d, 0x20] => "nop dword ptr [eax]".to_string(), // dumpbin doesn't know about 0f1d..
                &[0x0f, 0x1e, 0x20] => "nop dword ptr [eax]".to_string(), // dumpbin doesn't know about 0f1e..
                &[0xf2, 0x66, 0x66, 0x0f, 0x10, 0xc0] => "movsd xmm0, xmm0".to_string(), // dumpbin does not love the prefixes
                &[0xf3, 0x0f, 0x1e, 0xfc] => "nop".to_string(), // dumpbin does not tolerate this at all, redirect into a boring nop.
                &[0x0f, 0x43, 0xec] => "cmovnb ebp, esp".to_string(), // dumpbin writes it "cmovae" instead of yax's cmovnb.
                &[0x2e, 0x36, 0x0f, 0x18, 0xe7] => "nop edi".to_string(), // dumpbin reports a mildly-confused prefetchrst2 rdi (even in 32-bit mode!)
                &[0x0f, 0xbe, 0x83, 0xb4, 0x00, 0x00, 0x00] => {
                    "movsx eax, byte ptr [ebx + 0B4h]".to_string() // dumpbin uses %016 formatting, masm happily accepts shorter.
                },
                &[0x62, 0xd2, 0x7e, 0x28, 0x3a, 0xca] => {
                    "vpbroadcastmw2d ymm1, k2".to_string() // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                },
                &[0x62, 0xd2, 0x7e, 0x08, 0x28, 0xc2] => {
                    "vpmovm2b xmm0, k2".to_string() // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                },
                &[0x0f, 0x0d, 0x00] => {
                    // dumpbin interprets this as the 3DNow!-style PREFETCH instruction, but we're definitely not 3dnow..
                    "nop zmmword ptr [eax]".to_string()
                }
                &[0xc4, 0x03, 0x3d, 0x0a, 0xca, 0x77] => {
                    // dumpbin can't deal with this instruction..
                    "vroundss xmm9, xmm8, xmm10, 77h".to_string()
                }
                &[0xc4, 0x03, 0x3d, 0x0b, 0xca, 0x77] => {
                    // dumpbin can't deal with this instruction..
                    "vroundsd xmm9, xmm8, xmm10, 77h".to_string()
                }
                &[0x66, 0x0f, 0xd6, 0x01] => {
                    // dumpbin really wants to use mmword here, but i really don't.
                    "movq qword ptr [ecx], xmm0".to_string()
                }
                // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                &[0x66, 0x0f, 0xff, 0xc1] => "ud0 eax, ecx".to_string(),
                &[0xf2, 0x0f, 0xff, 0xc1] => "ud0 eax, ecx".to_string(),
                &[0xf3, 0x0f, 0xff, 0xc1] => "ud0 eax, ecx".to_string(),
                &[0x66, 0x0f, 0xff, 0x01] => "ud0 eax, dword ptr [ecx]".to_string(),
                &[0x0f, 0xff, 0x6b, 0xac] => "ud0 ebp, dword ptr [ebx - 54h]".to_string(),
                // dumpbin does not tolerate the pointless prefixes.
                &[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e] => "movdiri dword ptr cs:[ebp + 3Eh], edx".to_string(),
                // dumpbin does not tolerate the pointless prefixes.
                &[0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07] => "movdir64b ebp, zmmword ptr es:[ebp + 729080Bh]".to_string(),
                // dumpbin does not tolerate the pointless prefixes.
                &[0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08] => "movdir64b bp, zmmword ptr es:[di + 80Bh]".to_string(),
                // and again
                 &[0xf2, 0xf2, 0x2e, 0x36, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f] => "enqcmd eax, zmmword ptr ss:[ebx + 3F9D1C09h]".to_string(),
                // and again.
                &[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54] => "enqcmds esi, zmmword ptr fs:[edx + 54h]".to_string(),
                // prefixes confuse dumpbin again
                &[0x66, 0xf3, 0x0f, 0x01, 0xe8] => "setssbsy".to_string(),
                // prefixes confuse dumpbin again
                &[0x66, 0xf3, 0x0f, 0x01, 0xea] => "saveprevssp".to_string(),
                // prefixes confuse dumpbin again
                &[0xf3, 0x66, 0x0f, 0x01, 0xe8] => "setssbsy".to_string(), // TODO: yax does not support `serialize` (yet)
                // prefixes confuse dumpbin again
                &[0xf3, 0x66, 0x0f, 0x01, 0xea] => "saveprevssp".to_string(),
                // prefixes confuse dumpbin again
                &[0xf3, 0x66, 0x0f, 0x01, 0x29] => "rstorssp qword ptr [ecx]".to_string(),
                // dumpbin writes the repne, but it doesn't do anything..
                &[0xf2, 0x0f, 0x21, 0xc8] => "mov eax, dr1".to_string(),
                // dumpbin writes the rep, but it doesn't do anything..
                &[0xf3, 0x0f, 0x21, 0xc8] => "mov eax, dr1".to_string(),
                // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                &[0xf2, 0x0f, 0xc0, 0xcc] => "xadd ah, cl".to_string(),
                // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                &[0xf3, 0x0f, 0xc0, 0xcc] => "xadd ah, cl".to_string(),
                // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                &[0xf2, 0x0f, 0xc1, 0xcc] => "xadd esp, ecx".to_string(),
                // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                &[0xf3, 0x0f, 0xc1, 0xcc] => "xadd esp, ecx".to_string(),
                // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                &[0xf2, 0x0f, 0xc7, 0x0f] => "cmpxchg8b qword ptr [edi]".to_string(),
                // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                &[0xf3, 0x0f, 0xc7, 0x0f] => "cmpxchg8b qword ptr [edi]".to_string(),
                // prefixes again..
                &[0x66, 0x36, 0x0f, 0x3a, 0xce, 0x8c, 0x56, 0x9e, 0x82, 0xd1, 0xbe, 0xad] => "gf2p8affineqb xmm1, xmmword ptr ss:[esi + edx * 2 - 412E7D62h], 0ADh".to_string(),
                &[0x3e, 0x64, 0x64, 0x66, 0x0f, 0x3a, 0xcf, 0xba, 0x13, 0x23, 0x04, 0xba, 0x6b] => "gf2p8affineinvqb xmm7, xmmword ptr fs:[edx - 45FBDCEDh], 6Bh".to_string(),
                &[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8] => "loadiwkey xmm5, xmm0".to_string(),
                // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                &[0x66, 0x0f, 0x38, 0x80, 0x01] => "invept eax, xmmword ptr [ecx]".to_string(),
                // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                &[0x66, 0x0f, 0x38, 0x81, 0x01] => "invvpid eax, xmmword ptr [ecx]".to_string(),
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
                // dumpbin prints a ds: since this is an absolute address..
                &[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00] => "call dword ptr [0024727Eh]".to_string(),
                // dumpbin uses a really wide displacement .. for laughs..
                &[0xff, 0x24, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => "jmp dword ptr [ecx * 8 + 1BCA070h]".to_string(),
                // dumpbin uses a really wide displacement .. for laughs..
                &[0xff, 0x14, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => "call dword ptr [ecx * 8 + 1BCA070h]".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe0, 0x12] => "loopnz $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe1, 0x12] => "loopz $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe2, 0x12] => "loop $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe3, 0x12] => "jecxz $+14h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0xe3, 0xf0] => "jecxz $-0Eh".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x67, 0xe3, 0x12] => "jcxz $+15h".to_string(),
                // dumpbin uses absolute branch destinations, but yax uses relative.
                &[0x67, 0xe3, 0xf0] => "jcxz $-0Dh".to_string(),
                // dumpbin dislikes prefixes.
                &[0x66, 0xf2, 0x0f, 0x79, 0xcf] => "insertq xmm1, xmm7".to_string(),
                &[0xf6, 0x05, 0x2c, 0x9b, 0xff, 0xff, 0x01] => "test byte ptr [0FFFF9B2Ch], 1".to_string(),
                // yax uses wider immediates
                &[0x3d, 0x01, 0xf0, 0xff, 0xff] => "cmp eax, 0FFFFF001h".to_string(),
                // dumpbin gets the size wrong
                &[0x62, 0xf2, 0xfd, 0x0f, 0x8a, 0x62, 0xf2] => "vcompresspd xmmword ptr [edx - 70h]{k7}, xmm4".to_string(),
                // TODO: yax doesn't know about rdssp{d,q}?
                &[0xf3, 0x0f, 0x1e, 0x0f] => "nop".to_string(),
                // yax won't mention the pointless repne prefix
                &[0xf2, 0x0f, 0x06] => "clts".to_string(),
                // yax won't mention the pointless repne prefix
                &[0xf2, 0x0f, 0x07] => "sysret".to_string(),
                // dumpbin spells this mmword
                &[0x0f, 0x6f, 0x00] => "movq mm0, qword ptr [eax]".to_string(),
                &[0x66, 0x2e, 0xf2, 0xf0, 0x0f, 0xbb, 0x13] => "xacquire lock btc word ptr cs:[ebx], dx".to_string(),
                // dumpbin prints with more.. flourish
                &[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00] => "nop word ptr cs:[eax + eax]".to_string(),
                // disp is wider from dumpbin
                &[0x0f, 0xfc, 0xaf, 0x40, 0x38, 0x25, 0xbf] => "paddb mm5, mmword ptr [edi - 40DAC7C0h]".to_string(),
                &[0xc7, 0xf8, 0x10, 0x12, 0x34, 0x56] => "xbegin $+56341216h".to_string(),
                &[0x66, 0xc7, 0xf8, 0x10, 0x12] => "xbegin $+1215h".to_string(),
                &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c] => "pi2fw mm6, qword ptr ss:[eax - 5]".to_string(), // more prefix confusion..
                // prefixes confuse dumpbin, and dumpbin says "qword" where we use mmword. masm accepts either
                &[0x3e, 0xf3, 0x2e, 0xf2, 0x0f, 0x0f, 0x64, 0x93, 0x93, 0xa4] => "pfmax mm4, mmword ptr cs:[ebx + edx * 4 - 6Dh]".to_string(),
                // dumpbin shows this as a non-rip-rel offset :(
                &[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77] => "pmulhw mm7, qword ptr [77CCBBAAh]".to_string(),
                // dumpbin confused about prefixes once again
                &[0x66, 0x3e, 0x26, 0x2e, 0x2e, 0x0f, 0x38, 0x2a, 0x2b] => "movntdqa xmm5, xmmword ptr cs:[ebx]".to_string(),
                // prefixes.. cs: isn't real in 64-bit mode
                &[0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c] => "blendpd xmm7, xmmword ptr cs:[bx + si + 2FF0h], 7Ch".to_string(),
                // prefixes confuse dumpbin
                &[0x66, 0x66, 0x64, 0x3e, 0x0f, 0x38, 0x23, 0x9d, 0x69, 0x0f, 0xa8, 0x2d] => "pmovsxwd xmm3, qword ptr [ebp + 2DA80F69h]".to_string(),
                // prefixes confuse dumpbin
                &[0x2e, 0x66, 0x26, 0x64, 0x0f, 0x3a, 0x21, 0x0b, 0xb1] => "insertps xmm1, dword ptr fs:[ebx], 0B1h".to_string(),
                // prefixes confuse dumpbin
                &[0x66, 0x26, 0x0f, 0x3a, 0x42, 0x96, 0x74, 0x29, 0x96, 0xf9, 0x6a] => "mpsadbw xmm2, xmmword ptr es:[esi - 669D68Ch], 6Ah".to_string(),
                // prefixes confuse dumpbin
                &[0x67, 0x26, 0x66, 0x65, 0x0f, 0x38, 0x3f, 0x9d, 0xcc, 0x03] => "pmaxud xmm3, xmmword ptr gs:[di + 3CCh]".to_string(),
                // prefixes confuse dumpbin
                &[0x67, 0x66, 0x65, 0x3e, 0x0f, 0x6d, 0xd1] => "punpckhqdq xmm2, xmm1".to_string(),
                // prefixes confuse dumpbin
                &[0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f] => "lddqu xmm4, xmmword ptr es:[bx + si + 5F1Bh]".to_string(),
                // prefixes confuse dumpbin
                &[0x2e, 0x3e, 0x66, 0x3e, 0x0f, 0x3a, 0x41, 0x30, 0x48] => "dppd xmm6, xmmword ptr [eax], 48h".to_string(),
                // again prefixes confuse dumpbin
                &[0x65, 0x66, 0x66, 0x64, 0x0f, 0x38, 0xdb, 0x0f] => "aesimc xmm1, xmmword ptr fs:[edi]".to_string(),
                // dumpbin prints the order backwards =|
                &[0x65, 0xf0, 0x87, 0x0f] => "lock xchg dword ptr gs:[edi], ecx".to_string(),
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
                &[0xf2, 0x0f, 0xbc, 0xd3] => "bsf edx, ebx".to_string(),
                // mov abs in 32-bit mode gets a ds: prefix even though that's the default. masm does not need this prefix, so we round-trip fine without it.
                &[0xa0, 0x93, 0x62, 0xc4, 0x00] => "mov al, byte ptr [00C46293h]".to_string(),
                &[0x67, 0xa0, 0x93, 0x62] => "mov al, byte ptr [00006293h]".to_string(),
                &[0xa1, 0x93, 0x62, 0xc4, 0x00] => "mov eax, dword ptr [00C46293h]".to_string(),
                &[0x67, 0xa1, 0x93, 0x62] => "mov eax, dword ptr [00006293h]".to_string(),
                &[0xa2, 0x93, 0x62, 0xc4, 0x00] => "mov byte ptr [00C46293h], al".to_string(),
                &[0x67, 0xa2, 0x93, 0x62] => "mov byte ptr [00006293h], al".to_string(),
                &[0xa3, 0x93, 0x62, 0xc4, 0x00] => "mov dword ptr [00C46293h], eax".to_string(),
                &[0x67, 0xa3, 0x93, 0x62] => "mov dword ptr [00006293h], eax".to_string(),
                &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => "xor eax, dword ptr [12345678h]".to_string(),
                &[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [44332211h]".to_string(),
                &[0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44] => "xor eax, dword ptr [44332211h]".to_string(),
                &[0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50] => "xor esi, dword ptr [50403020h]".to_string(),
                &[0xa0, 0xc0, 0xb0, 0xa0, 0x90] => "mov al, byte ptr [90A0B0C0h]".to_string(),
                &[0x67, 0xa0, 0xc0, 0xb0] => "mov al, byte ptr [0B0C0h]".to_string(),
                &[0x67, 0xa1, 0xc0, 0xb0] => "mov eax, dword ptr [0000B0C0h]".to_string(),
                &[0x66, 0x67, 0xa1, 0xc0, 0xb0] => "mov ax, word ptr [0000B0C0h]".to_string(),
                // same for wrssd
                &[0x3e, 0x0f, 0x38, 0xf6, 0x23] => "wrssd dword ptr [ebx], esp".to_string(),
                // dumpbin believes that rex.w works even in 32-bit code, thus prints `rorx rax, ..`. haha what a dingus
                &[0xc4, 0xe3, 0xfb, 0xf0, 0x01, 0x05] => "rorx eax, dword ptr [ecx], 5".to_string(),
                &[0xc4, 0xe2, 0xe3, 0xf5, 0x07] => "pdep eax, ebx, dword ptr [edi]".to_string(),
                &[0xc4, 0xe2, 0xe3, 0xf6, 0x07] => "mulx eax, ebx, dword ptr [edi]".to_string(),
                &[0xc4, 0xe2, 0xe3, 0xf7, 0x01] => "shrx eax, dword ptr [ecx], ebx".to_string(),
                &[0xc4, 0xe2, 0xe2, 0xf5, 0x07] => "pext eax, ebx, dword ptr [edi]".to_string(),
                &[0xc4, 0xe2, 0xe2, 0xf7, 0x01] => "sarx eax, dword ptr [ecx], ebx".to_string(),
                &[0xc4, 0xe2, 0xe0, 0xf5, 0x07] => "bzhi eax, dword ptr [edi], ebx".to_string(),
                &[0xc4, 0xe2, 0xe1, 0xf7, 0x01] => "shlx eax, dword ptr [ecx], ebx".to_string(),
                &[0xc4, 0xe2, 0xe0, 0xf2, 0x01] => "andn eax, ebx, dword ptr [ecx]".to_string(),
                &[0xc4, 0xe2, 0xf8, 0xf3, 0x09] => "blsr eax, dword ptr [ecx]".to_string(),
                &[0xc4, 0xe2, 0xf8, 0xf3, 0x11] => "blsmsk eax, dword ptr [ecx]".to_string(),
                &[0xc4, 0xe2, 0xf8, 0xf3, 0x19] => "blsi eax, dword ptr [ecx]".to_string(),
                &[0xc4, 0xe2, 0xe0, 0xf7, 0x01] => "bextr eax, dword ptr [ecx], ebx".to_string(),
                &[0xc4, 0xc3, 0x39, 0x0c, 0xca, 0x77] => "vblendps xmm1, xmm0, xmm2, 77h".to_string(),
                // just have to decide we know better than dumpbin: masm does not accept an absolute far call/far jump destination,
                // so we definitely can't round-trip by following dumpbin. dumpbin doesn't use hex suffixes here, instead printing
                // "6655:44332211" as the destination. this is technically not ambiguous since `:` is a hint that this is a absolute
                // far address and that both numbers are base 16, but that's ... subtle and easy to miss. so add some h's.
                &[0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66] => "call 6655h:44332211h".to_string(),
                &[0x66, 0x9a, 0x11, 0x22, 0x33, 0x44] => "call 4433h:2211h".to_string(),
                // terribly unfortunate: masm reasonably encodes this instruction as a 32-bit offset, which causes yax to spell the offset
                // as 0000AA55 instead of AA55. override dumpbin to use the (worse) encoding for the sake of matching with the test.
                &[0x66, 0x67, 0x8b, 0x0e, 0x55, 0xaa] => "mov cx, word ptr [0AA55h]".to_string(),
                // inexplicably, dumpbin spells this "aamb", for .. ascii adjust after multiplcation (byte) ???
                // additionally, masm does not accept an integer operand: it only supports `aam 10` as in d4 0a. so.. bummer.
                &[0xd4, 0x01] => "aam 1".to_string(),
                // same as above
                &[0xd5, 0x01] => "aad 1".to_string(),
                // dunno why dumpbin doesn't like this one..
                &[0xc5, 0b1_1111_100, 0x2e, 0b00_001_010] => "vucomiss xmm1, dword ptr [edx]".to_string(),
                &[0xc5, 0b1_1111_100, 0x2f, 0b00_001_010] => "vcomiss xmm1, dword ptr [edx]".to_string(),
                other => {
                    let dumpbin_res = tools::dumpbin(other, CodeModel::Bits32);
                    match dumpbin_res {
                        Ok(text) => text,
                        Err(e) => {
                            if vex_prefixed {
                                // this might be an instance of dumpbin not being great: consider vucomiss, as in "c5f82eca".
                                return;
                            }

                            // otherwise: unexpected, what da heck.
                            panic!("{}: {e:?}", format!("could not get an instruction after dumpbining {other:x?}"));
                        }
                    }
                }
            };

            // anguish, misery, etc: dumpbin will process register extension bits in protected mode, even though they are ignored.
            // if we see a register like this, just.. bail. otherwise this will have to have exceptions for hundreds of test cases.
            // note this skips x/y/z in the register name since mm8..mm31 will do the job.
            for reg in [
                "mm8", "mm9", "mm10", "mm11", "mm12", "mm13", "mm14", "mm15",
                "mm16", "mm17", "mm18", "mm19", "mm20", "mm21", "mm22", "mm23",
                "mm24", "mm25", "mm26", "mm27", "mm28", "mm29", "mm30", "mm31",
            ] {
                if external_masm_ish.contains(reg) {
                    // TODO: EXTRACT THIS TO SOME OTHER MASM-SPECIFIC FUNCTION: THIS BAILS OUT OF THE REST OF THE TEST ENTIRELY.
                    return;
                }
            }

            if vex_prefixed {
                for reg in [
                    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
                ] {
                    if external_masm_ish.contains(reg) {
                        // TODO: EXTRACT THIS TO SOME OTHER MASM-SPECIFIC FUNCTION: THIS BAILS OUT OF THE REST OF THE TEST ENTIRELY.
                        return;
                    }
                }
            }

            let displayed_masm = decoder.decode_slice(bytes).expect("can decode").display_with(DisplayStyle::Masm).to_string();
            let masm_as_bytes = match displayed_masm.as_str() {
                "nop zmmword ptr [eax]" => vec![0x0f, 0x18, 0x20], // MASM doesn't accept `nop zmmword ..`, no way to round trip 0f1820
                "sysenter" => vec![0x0f, 0x34], // MASM doesn't accept sysenter, but dumpbin prints it.
                "sysexit" => vec![0x0f, 0x35], // MASM doesn't accept sysexit, but dumpbin prints it.
                // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                "ud0 eax, ecx" => vec![0x66, 0x0f, 0xff, 0xc1],
                "ud0 eax, dword ptr [ecx]" => vec![0x66, 0x0f, 0xff, 0x01],
                "ud0 ebp, dword ptr [ebx - 54h]" => vec![0x0f, 0xff, 0x6b, 0xac],
                // masm seems to not know about fstpnce/fstp1 at all. since this is an undocumented instruction anyway, assemble it ourselves..
                "fstpnce st(3), st(0)" => vec![0xd9, 0xdb],
                // masm inserts a wait prefix here..
                "feni" => vec![0xdb, 0xe0],
                "fdisi" => vec![0xdb, 0xe1],
                "fsetpm" => vec![0xdb, 0xe4],
                // masm doesn't know how to assemble address-size overrides..?
                // > cannot use 16-bit register with a 32-bit address
                "aesimc xmm1, xmmword ptr [bx]" => vec![0x67, 0x66, 0x0f, 0x38, 0xdb, 0x0f],
                "aesenc xmm1, xmmword ptr [bx]" => vec![0x67, 0x66, 0x0f, 0x38, 0xdc, 0x0f],
                "aesenclast xmm1, xmmword ptr [bx]" => vec![0x67, 0x66, 0x0f, 0x38, 0xdd, 0x0f],
                "aesdec xmm1, xmmword ptr [bx]" => vec![0x67, 0x66, 0x0f, 0x38, 0xde, 0x0f],
                "aesdeclast xmm1, xmmword ptr [bx]" => vec![0x67, 0x66, 0x0f, 0x38, 0xdf, 0x0f],
                "blendpd xmm7, xmmword ptr cs:[bx + si + 2FF0h], 7Ch" => vec![0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c],
                // more
                "movdir64b bp, zmmword ptr es:[di + 80Bh]" => vec![0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08],
                "lss eax, fword ptr [bx + si]" => vec![0x67, 0x0f, 0xb2, 0x00],
                "lddqu xmm4, xmmword ptr es:[bx + si + 5F1Bh]" => vec![0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f],
                "lods byte ptr [si]" => vec![0x67, 0xac],
                "scas byte ptr es:[di]" => vec![0x67, 0xae],
                "rep movs byte ptr es:[di], byte ptr [si]" => vec![0x67, 0xf3, 0xa4],
                "rep movs dword ptr es:[di], dword ptr [si]" => vec![0x67, 0xf3, 0xa5],
                "movapd xmm0, xmmword ptr [bx + si]" => vec![0x67, 0x66, 0x0f, 0x28, 0x00],
                "cvtdq2ps xmm0, xmmword ptr [bx + di]" => vec![0x67, 0x0f, 0x5b, 0x01],
                // i tried really hard to find a MASM syntax for absolute far call/jump destinations! i turned up a bunch of blanks.
                // https://mirrors.nycbug.org/pub/The_Unix_Archive/Unix_Usenet/comp.unix.xenix/1989-February/001910.html is the funniest,
                // given that it is OS hackers experiencing the same issue and concluding they should emit the bytes themselves.
                // so yax will emit something like bindump would, and we'll just swallow the text as if masm worked like i'd hope..
                "call 6655h:44332211h" => vec![0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66],
                "call 4433h:2211h" => vec![0x66, 0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66],
                // terribly unfortunate: masm reasonably encodes this instruction as a 32-bit offset, which causes yax to spell the offset
                // as 0000AA55 instead of AA55. override dumpbin to use the (worse) encoding for the sake of matching with the test.
                "mov cx, word ptr [0AA55h]" => vec![0x66, 0x67, 0x8b, 0x0e, 0x55, 0xaa],
                // same deal, different instruction.
                "mov al, byte ptr [0B0C0h]" => vec![0x67, 0xa0, 0xc0, 0xb0],
                "mov eax, dword ptr [0000B0C0h]" => vec![0x67, 0xa1, 0xc0, 0xb0],
                // if you operand-size override pushad/popad you get the 16-bit forms, pusha/popa. dumpbin reflects this, but in 32-bit mode
                // accepts either as a way of spelling pushad/popad. override it here for tests to match up, but this is an unfortunately
                // disastrous difference in round-tripping..
                "pusha" => vec![0x66, 0x60],
                "popa" => vec![0x66, 0x61],
                // masm does not accept an integer operand: it only supports `aam 10` as in d4 0a. so.. bummer.
                "aam 1" => vec![0xd4, 0x01],
                // same as above
                "aad 1" => vec![0xd5, 0x01],
                _other => { tools::masm(&displayed_masm, CodeModel::Bits32).expect("can assemble") }
            };
            let masm_roundtrip = decoder.decode_slice(&masm_as_bytes).expect("can decode").display_with(DisplayStyle::Masm).to_string();
            // chasing down differences in how dumpbin/yax write "qword" is not useful to anyone..
            let external_masm_ish = external_masm_ish.replace(" mmword ", " qword ");
            let masm_roundtrip = masm_roundtrip.replace(" mmword ", " qword ");
            if external_masm_ish.starts_with("tzcnt") && masm_roundtrip.starts_with("bsf") {
                // this is ok, we support "decode as if without bmi1" but dumpbin does not, so dumpbin always says tzcnt.
                // masm accepts either and does the right thing.
            } else if external_masm_ish.contains(" qword ") && masm_roundtrip.contains(" dword ") {
                // this might be "dumpbin thinks the instruction works on qwords, but we know it's dwords. let it through? :/
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
            use crate::protected_mode::{TestCase, FeatureSet};

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
            use crate::protected_mode::{TestCase, FeatureSet};

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
            use crate::protected_mode::TestCase;

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
            use crate::protected_mode::{TestCase, Disasm, FeatureSet};

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
            use crate::protected_mode::{TestCase, Disasm, FeatureSet};

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
            use crate::protected_mode::{TestCase, Disasm, FeatureSet};

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
            use crate::protected_mode::{TestCase, Disasm, FeatureSet};

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
            use crate::protected_mode::{TestCase, Disasm, FeatureSet};

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
            use crate::protected_mode::{TestCase, Disasm};

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
            use crate::protected_mode::{TestCase, Disasm};

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
            use crate::protected_mode::{TestCase, Disasm};

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
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // just modrm
        testcase!(&[0x33, 0x08], "xor ecx, dword [eax]"),
        testcase!(&[0x33, 0x20], "xor esp, dword [eax]"),
        testcase!(&[0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor eax, dword [0x12345678]"),
        testcase!(&[0x33, 0x41, 0x23], "xor eax, dword [ecx + 0x23]"),
        testcase!(&[0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor eax, dword [ecx + 0x43650123]"),
        testcase!(&[0x33, 0xc1], "xor eax, ecx"),

        // sib
        testcase!(&[0x33, 0x04, 0x0a], "xor eax, dword [edx + ecx * 1]", masm: "xor eax, dword ptr [edx + ecx]"),
        testcase!(&[0x33, 0x04, 0x4a], "xor eax, dword [edx + ecx * 2]"),
        testcase!(&[0x33, 0x04, 0x8a], "xor eax, dword [edx + ecx * 4]"),
        testcase!(&[0x33, 0x04, 0xca], "xor eax, dword [edx + ecx * 8]"),
        testcase!(&[0x33, 0x04, 0x20], "xor eax, dword [eax]"),
        testcase!(&[0x33, 0x04, 0x60], "xor eax, dword [eax]"),
        testcase!(&[0x33, 0x04, 0xa0], "xor eax, dword [eax]"),
        testcase!(&[0x33, 0x04, 0xe0], "xor eax, dword [eax]"),
        testcase!(&[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [0x44332211]", masm: "xor eax, dword ptr [44332211h]"),

        testcase!(&[0x33, 0x44, 0x65, 0x11], "xor eax, dword [ebp + 0x11]"),
        testcase!(&[0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [ebp + 0x44332211]"),
        testcase!(&[0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44], "xor eax, dword [0x44332211]"),

        // specifically sib with base == 0b101
        // mod bits 00
        testcase!(&[0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [0x50403020]"),
        // mod bits 01
        testcase!(&[0x33, 0x74, 0x25, 0x20], "xor esi, dword [ebp + 0x20]"),
        // mod bits 10
        testcase!(&[0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [ebp + 0x50403020]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod mmx {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0xf7, 0xc1], "maskmovq mm0, mm1"),
        testcase!(invalid: &[0x0f, 0xf7, 0x01]),

        testcase!(&[0x0f, 0xe7, 0x03], "movntq qword [ebx], mm0"),
        testcase!(invalid: &[0x0f, 0xe7, 0xc3]),

        testcase!(invalid: &[0x66, 0x0f, 0xc3, 0x03]),
        testcase!(&[0x0f, 0xc3, 0x03], "movnti dword [ebx], eax"),
        testcase!(invalid: &[0x0f, 0xc3, 0xc3]),

        testcase!(&[0x0f, 0x7e, 0xcf], "movd edi, mm1"),
        testcase!(&[0x0f, 0x7f, 0xcf], "movq mm7, mm1"),
        testcase!(&[0x0f, 0x7f, 0x0f], "movq qword [edi], mm1"),
        testcase!(&[0x0f, 0xc4, 0xc0, 0x14], "pinsrw mm0, eax, 0x14"),
        testcase!(&[0x0f, 0xc4, 0x00, 0x14], "pinsrw mm0, word [eax], 0x14"),
        testcase!(&[0x0f, 0xd1, 0xcf], "psrlw mm1, mm7"),
        testcase!(&[0x0f, 0xd1, 0x00], "psrlw mm0, qword [eax]"),
        testcase!(invalid: &[0x0f, 0xd7, 0x00]),
        testcase!(&[0x0f, 0xd7, 0xcf], "pmovmskb ecx, mm7"),
        testcase!(&[0x0f, 0x3a, 0x0f, 0xc1, 0x23], "palignr mm0, mm1, 0x23"),
        testcase!(&[0x0f, 0xf9, 0xc2], "psubw mm0, mm2"),
        testcase!(&[0x0f, 0xfd, 0xd2], "paddw mm2, mm2"),
        testcase!(&[0x0f, 0x6f, 0xe9], "movq mm5, mm1"),
        testcase!(&[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77], "pmulhw mm7, qword [0x77ccbbaa]"),

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
        testcase!(&[0x0f, 0xfc, 0x02], "paddb mm0, qword [edx]"),
        testcase!(&[0x0f, 0xfd, 0xc2], "paddw mm0, mm2"),
        testcase!(&[0x0f, 0xfe, 0xc2], "paddd mm0, mm2"),

        testcase!(&[0x0f, 0xf1, 0x02], "psllw mm0, qword [edx]"),
        testcase!(&[0x0f, 0xf2, 0x02], "pslld mm0, qword [edx]"),
        testcase!(&[0x0f, 0xf3, 0x02], "psllq mm0, qword [edx]"),
        testcase!(&[0x0f, 0xf4, 0x02], "pmuludq mm0, qword [edx]"),
        testcase!(&[0x0f, 0xf5, 0x02], "pmaddwd mm0, qword [edx]"),
        testcase!(&[0x0f, 0xf6, 0x02], "psadbw mm0, qword [edx]"),
        testcase!(&[0x0f, 0xf8, 0x02], "psubb mm0, qword [edx]"),
        testcase!(&[0x0f, 0xf9, 0x02], "psubw mm0, qword [edx]"),
        testcase!(&[0x0f, 0xfa, 0x02], "psubd mm0, qword [edx]"),
        testcase!(&[0x0f, 0xfb, 0x02], "psubq mm0, qword [edx]"),
        testcase!(&[0x0f, 0xfc, 0x02], "paddb mm0, qword [edx]"),
        testcase!(&[0x0f, 0xfd, 0x02], "paddw mm0, qword [edx]"),
        testcase!(&[0x0f, 0xfe, 0x02], "paddd mm0, qword [edx]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod cvt {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x2c, 0xcf], "cvttps2pi mm1, xmm7"),
        testcase!(&[0x0f, 0x2a, 0xcf], "cvtpi2ps xmm1, mm7"),
        testcase!(&[0x0f, 0x2a, 0x00], "cvtpi2ps xmm0, qword [eax]"),
        testcase!(&[0x66, 0x0f, 0x2a, 0x00], "cvtpi2pd xmm0, qword [eax]"),
        testcase!(&[0x66, 0x0f, 0x2a, 0xcf], "cvtpi2pd xmm1, mm7"),
        testcase!(&[0xf2, 0x0f, 0x2a, 0x00], "cvtsi2sd xmm0, dword [eax]"),
        testcase!(&[0xf2, 0x0f, 0x2a, 0xcf], "cvtsi2sd xmm1, edi"),
        testcase!(&[0xf3, 0x0f, 0x2a, 0x00], "cvtsi2ss xmm0, dword [eax]"),
        testcase!(&[0xf3, 0x0f, 0x2a, 0xcf], "cvtsi2ss xmm1, edi"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod aesni {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdb, 0x0f], "aesimc xmm1, xmmword [edi]"),
        testcase!(features { AESNI: true } &[0x67, 0x66, 0x0f, 0x38, 0xdb, 0x0f], "aesimc xmm1, xmmword [bx]"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdc, 0x0f], "aesenc xmm1, xmmword [edi]"),
        testcase!(features { AESNI: true } &[0x67, 0x66, 0x0f, 0x38, 0xdc, 0x0f], "aesenc xmm1, xmmword [bx]"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdd, 0x0f], "aesenclast xmm1, xmmword [edi]"),
        testcase!(features { AESNI: true } &[0x67, 0x66, 0x0f, 0x38, 0xdd, 0x0f], "aesenclast xmm1, xmmword [bx]"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xde, 0x0f], "aesdec xmm1, xmmword [edi]"),
        testcase!(features { AESNI: true } &[0x67, 0x66, 0x0f, 0x38, 0xde, 0x0f], "aesdec xmm1, xmmword [bx]"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x38, 0xdf, 0x0f], "aesdeclast xmm1, xmmword [edi]"),
        testcase!(features { AESNI: true } &[0x67, 0x66, 0x0f, 0x38, 0xdf, 0x0f], "aesdeclast xmm1, xmmword [bx]"),

        testcase!(features { AESNI: true } &[0x66, 0x0f, 0x3a, 0xdf, 0x0f, 0xaa], "aeskeygenassist xmm1, xmmword [edi], 0xaa"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod sse2 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x10, 0x0c, 0xc7], "movsd xmm1, qword [edi + eax * 8]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x11, 0x0c, 0xc7], "movsd qword [edi + eax * 8], xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x11, 0x0c, 0xc7], "movupd xmmword [edi + eax * 8], xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x12, 0x03], "movlpd xmm0, qword [ebx]"), // reg-mem is movlpd
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x13, 0x03], "movlpd qword [ebx], xmm0"),
        testcase!(invalid: &[0x66, 0x0f, 0x13, 0xc3]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x14, 0x03], "unpcklpd xmm0, xmmword [ebx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x14, 0xc3], "unpcklpd xmm0, xmm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x15, 0x03], "unpckhpd xmm0, xmmword [ebx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x15, 0xc3], "unpckhpd xmm0, xmm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x16, 0x03], "movhpd xmm0, qword [ebx]"),
        testcase!(invalid: &[0x66, 0x0f, 0x16, 0xc3]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x17, 0x03], "movhpd qword [ebx], xmm0"),
        testcase!(invalid: &[0x66, 0x0f, 0x17, 0xc3]),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x28, 0xd0], "movapd xmm2, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [eax]"),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2a, 0xcf], "cvtpi2pd xmm1, mm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2a, 0x0f], "cvtpi2pd xmm1, qword [edi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x2a, 0xcf], "cvtsi2sd xmm1, edi"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x2a, 0x0f], "cvtsi2sd xmm1, dword [edi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2b, 0x0f], "movntpd xmmword [edi], xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2c, 0xcf], "cvttpd2pi mm1, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2c, 0x0f], "cvttpd2pi mm1, xmmword [edi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x2c, 0xcf], "cvttsd2si ecx, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x2c, 0x0f], "cvttsd2si ecx, qword [edi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2d, 0xcf], "cvtpd2pi mm1, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2d, 0x0f], "cvtpd2pi mm1, xmmword [edi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x2d, 0xcf], "cvtsd2si ecx, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x2d, 0x0f], "cvtsd2si ecx, qword [edi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2e, 0xcf], "ucomisd xmm1, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2e, 0x0f], "ucomisd xmm1, qword [edi]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2f, 0xcf], "comisd xmm1, xmm7"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x2f, 0x0f], "comisd xmm1, qword [edi]"),

        /*
         * .... 660f38
         * .... 660f7f
         */

        testcase!(invalid: &[0x66, 0x0f, 0x50, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x50, 0xc1], "movmskpd eax, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x51, 0x01], "sqrtpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x51, 0x01], "sqrtsd xmm0, qword [ecx]"),
        testcase!(invalid: &[0x66, 0x0f, 0x52, 0x01]),
        testcase!(invalid: &[0x66, 0x0f, 0x53, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x54, 0x01], "andpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x55, 0x01], "andnpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x56, 0x01], "orpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x57, 0x01], "xorpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x58, 0x01], "addpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x58, 0x01], "addsd xmm0, qword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x59, 0x01], "mulpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x59, 0x01], "mulsd xmm0, qword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x5a, 0x01], "cvtpd2ps xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x5a, 0x01], "cvtsd2ss xmm0, qword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x5b, 0x01], "cvtps2dq xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x5c, 0x01], "subpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x5c, 0x01], "subsd xmm0, qword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x5d, 0x01], "minpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x5d, 0x01], "minsd xmm0, qword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x5e, 0x01], "divpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x5e, 0x01], "divsd xmm0, qword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x5f, 0x01], "maxpd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x5f, 0x01], "maxsd xmm0, qword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x60, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpcklbw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x61, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpcklwd xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x62, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckldq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x63, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "packsswb xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x64, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "pcmpgtb xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x65, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "pcmpgtw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x66, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "pcmpgtd xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x67, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "packuswb xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x68, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhbw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x69, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhwd xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x6a, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhdq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x6b, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "packssdw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x6c, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpcklqdq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x6d, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "punpckhqdq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x6e, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movd xmm3, dword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x6f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqa xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x6e, 0xc0], "movd xmm0, eax"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x70, 0xc0, 0x4e], "pshufd xmm0, xmm0, 0x4e"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0x70, 0xc0, 0x4e], "pshuflw xmm0, xmm0, 0x4e"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf3, 0x0f, 0x70, 0xc0, 0x4e], "pshufhw xmm0, xmm0, 0x4e"),
        testcase!(invalid: &[0x66, 0x0f, 0x71, 0x10, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x71, 0xd0, 0x8f], "psrlw xmm0, 0x8f"),
        testcase!(invalid: &[0x66, 0x0f, 0x71, 0x20, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x71, 0xe0, 0x8f], "psraw xmm0, 0x8f"),
        testcase!(invalid: &[0x66, 0x0f, 0x71, 0x30, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x71, 0xf0, 0x8f], "psllw xmm0, 0x8f"),
        testcase!(invalid: &[0x66, 0x0f, 0x72, 0x10, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x72, 0xd0, 0x8f], "psrld xmm0, 0x8f"),
        testcase!(invalid: &[0x66, 0x0f, 0x72, 0x20, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x72, 0xe0, 0x8f], "psrad xmm0, 0x8f"),
        testcase!(invalid: &[0x66, 0x0f, 0x72, 0x30, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x72, 0xf0, 0x8f], "pslld xmm0, 0x8f"),
        testcase!(invalid: &[0x66, 0x0f, 0x73, 0x10, 0x8f]),
        testcase!(invalid: &[0x66, 0x0f, 0x73, 0x18, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x73, 0xd0, 0x8f], "psrlq xmm0, 0x8f"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x73, 0xd8, 0x8f], "psrldq xmm0, 0x8f"),
        testcase!(invalid: &[0x66, 0x0f, 0x73, 0x30, 0x8f]),
        testcase!(invalid: &[0x66, 0x0f, 0x73, 0x38, 0x8f]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x73, 0xf0, 0x8f], "psllq xmm0, 0x8f"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x73, 0xf8, 0x8f], "pslldq xmm0, 0x8f"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x7e, 0xc1], "movd ecx, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x7e, 0x01], "movd dword [ecx], xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } 
            &[0x66, 0x0f, 0x7f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqa xmmword [esp + ebx * 4 - 0x334455cc], xmm3"
        ),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc2, 0xc3, 0x08], "cmppd xmm0, xmm3, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc2, 0x03, 0x08], "cmppd xmm0, xmmword [ebx], 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0xc2, 0xc3, 0x08], "cmpsd xmm0, xmm3, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0xc2, 0x03, 0x08], "cmpsd xmm0, qword [ebx], 0x8"),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc4, 0xc3, 0x08], "pinsrw xmm0, ebx, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc4, 0x03, 0x08], "pinsrw xmm0, word [ebx], 0x8"),

    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0xc3, 0x08], "pextrw eax, xmm3, 0x8"),
    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0x03, 0x08]),
    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0x40, 0x08]),
    //    testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc5, 0x80, 0x08]),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc6, 0x03, 0x08], "shufpd xmm0, xmmword [ebx], 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xc6, 0xc3, 0x08], "shufpd xmm0, xmm3, 0x8"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd1, 0xc1], "psrlw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd1, 0x01], "psrlw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd2, 0xc1], "psrld xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd2, 0x01], "psrld xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd3, 0xc1], "psrlq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd3, 0x01], "psrlq xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd4, 0xc1], "paddq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd4, 0x01], "paddq xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd5, 0xc1], "pmullw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd5, 0x01], "pmullw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd6, 0xc1], "movq xmm1, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd6, 0x01], "movq qword [ecx], xmm0"),
        testcase!(invalid: &[0xf3, 0x0f, 0xd6, 0x03]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf3, 0x0f, 0xd6, 0xc3], "movq2dq xmm0, mm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0xd6, 0xc3], "movdq2q mm0, xmm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd7, 0xc1], "pmovmskb eax, xmm1"),
        testcase!(invalid: &[0x66, 0x0f, 0xd7, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd8, 0xc1], "psubusb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd8, 0x01], "psubusb xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd9, 0xc1], "psubusw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xd9, 0x01], "psubusw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xda, 0xc1], "pminub xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xda, 0x01], "pminub xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdb, 0xc1], "pand xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdb, 0x01], "pand xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdc, 0xc1], "paddusb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdc, 0x01], "paddusb xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdd, 0xc1], "paddusw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdd, 0x01], "paddusw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xde, 0xc1], "pmaxub xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xde, 0x01], "pmaxub xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdf, 0xc1], "pandn xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xdf, 0x01], "pandn xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe0, 0xc1], "pavgb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe0, 0x01], "pavgb xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe1, 0xc1], "psraw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe1, 0x01], "psraw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe2, 0xc1], "psrad xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe2, 0x01], "psrad xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe3, 0xc1], "pavgw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe3, 0x01], "pavgw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe4, 0xc1], "pmulhuw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe4, 0x01], "pmulhuw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe5, 0xc1], "pmulhw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe5, 0x01], "pmulhw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe6, 0xc1], "cvttpd2dq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe6, 0x01], "cvttpd2dq xmm0, xmmword [ecx]"),
        testcase!(invalid: &[0x66, 0x0f, 0xe7, 0xc1]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe7, 0x01], "movntdq xmmword [ecx], xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe8, 0xc1], "psubsb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe8, 0x01], "psubsb xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe9, 0xc1], "psubsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xe9, 0x01], "psubsw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xea, 0xc1], "pminsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xea, 0x01], "pminsw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xc3], "por xmm0, xmm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xc4], "por xmm0, xmm4"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xd3], "por xmm2, xmm3"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0x12], "por xmm2, xmmword [edx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0xc1], "por xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xeb, 0x01], "por xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xec, 0xc1], "paddsb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xec, 0x01], "paddsb xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xed, 0xc1], "paddsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xed, 0x01], "paddsw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xee, 0xc1], "pmaxsw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xee, 0x01], "pmaxsw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xef, 0xc1], "pxor xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xef, 0x01], "pxor xmm0, xmmword [ecx]"),
        testcase!(invalid: &[0x66, 0x0f, 0xf0, 0xc1]),
        testcase!(invalid: &[0x66, 0x0f, 0xf0, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf1, 0xc1], "psllw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf1, 0x01], "psllw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf2, 0xc1], "pslld xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf2, 0x01], "pslld xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf3, 0xc1], "psllq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf3, 0x01], "psllq xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf4, 0xc1], "pmuludq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf4, 0x01], "pmuludq xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf5, 0xc1], "pmaddwd xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf5, 0x01], "pmaddwd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf6, 0xc1], "psadbw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf6, 0x01], "psadbw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf7, 0xc1], "maskmovdqu xmm0, xmm1"),
        testcase!(invalid: &[0x66, 0x0f, 0xf7, 0x01]),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0xc1], "psubb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0x01], "psubb xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf9, 0xc1], "psubw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf9, 0x01], "psubw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfa, 0xc1], "psubd xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfa, 0x01], "psubd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfb, 0xc1], "psubq xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfb, 0x01], "psubq xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfc, 0xc1], "paddb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfc, 0x01], "paddb xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfd, 0xc1], "paddw xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfd, 0x01], "paddw xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfe, 0xc1], "paddd xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xfe, 0x01], "paddd xmm0, xmmword [ecx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xff, 0xc1], "ud0 eax, ecx"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf2, 0x0f, 0xff, 0xc1], "ud0 eax, ecx"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0xf3, 0x0f, 0xff, 0xc1], "ud0 eax, ecx"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xff, 0x01], "ud0 eax, dword [ecx]"),

        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x74, 0xc1], "pcmpeqb xmm0, xmm1"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0x74, 0x12], "pcmpeqb xmm2, xmmword [edx]"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0xc8], "psubb xmm1, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0xd0], "psubb xmm2, xmm0"),
        testcase!(features nodefault { Minimal: true, SSE: true, SSE2: true } &[0x66, 0x0f, 0xf8, 0x12], "psubb xmm2, xmmword [edx]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod sse3 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0xf0, 0x0f], "lddqu xmm1, xmmword [edi]"),
        testcase!(invalid: &[0xf2, 0x0f, 0xf0, 0xcf]),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0xd0, 0x0f], "addsubps xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0xd0, 0xcf], "addsubps xmm1, xmm7"),
        testcase!(invalid: &[0xf3, 0x0f, 0xd0, 0x0f]),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0xd0, 0x0f], "addsubpd xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0xd0, 0xcf], "addsubpd xmm1, xmm7"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7c, 0x0f], "haddps xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7c, 0xcf], "haddps xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7c, 0x0f], "haddpd xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7c, 0xcf], "haddpd xmm1, xmm7"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7d, 0x0f], "hsubps xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x7d, 0xcf], "hsubps xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7d, 0x0f], "hsubpd xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x66, 0x0f, 0x7d, 0xcf], "hsubpd xmm1, xmm7"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x12, 0x0f], "movsldup xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x12, 0xcf], "movsldup xmm1, xmm7"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x16, 0x0f], "movshdup xmm1, xmmword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf3, 0x0f, 0x16, 0xcf], "movshdup xmm1, xmm7"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x12, 0x0f], "movddup xmm1, qword [edi]"),
        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0xf2, 0x0f, 0x12, 0xcf], "movddup xmm1, xmm7"),

        testcase!(features { SSE3: true, SSE4_1: false, SSE4_2: false, AVX: false } &[0x0f, 0x01, 0xc8], "monitor", masm: "monitor eax, ecx, edx"),
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
        run_test(CASES);
    }
}

mod sse4_2 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x38, 0x37, 0x03], "pcmpgtq xmm0, xmmword [ebx]"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x38, 0x37, 0xc3], "pcmpgtq xmm0, xmm3"),

        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf0, 0x06], "crc32 eax, byte [esi]"),
        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf0, 0xc6], "crc32 eax, dh"),
        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf1, 0x06], "crc32 eax, dword [esi]"),
        testcase!(features { SSE4_2: true, AVX: false } &[0xf2, 0x0f, 0x38, 0xf1, 0xc6], "crc32 eax, esi"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0xf2, 0x0f, 0x38, 0xf1, 0xc6], "crc32 eax, si"),

        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x60, 0xc6, 0x54], "pcmpestrm xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x60, 0x06, 0x54], "pcmpestrm xmm0, xmmword [esi], 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x61, 0xc6, 0x54], "pcmpestri xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x61, 0x06, 0x54], "pcmpestri xmm0, xmmword [esi], 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x62, 0xc6, 0x54], "pcmpistrm xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x62, 0x06, 0x54], "pcmpistrm xmm0, xmmword [esi], 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x63, 0xc6, 0x54], "pcmpistri xmm0, xmm6, 0x54"),
        testcase!(features { SSE4_2: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x63, 0x06, 0x54], "pcmpistri xmm0, xmmword [esi], 0x54"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod sse4_1 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0c, 0x11, 0x22], "blendps xmm2, xmmword [ecx], 0x22"),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0c, 0xc1, 0x22], "blendps xmm0, xmm1, 0x22"),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0d, 0x11, 0x22], "blendpd xmm2, xmmword [ecx], 0x22"),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0d, 0xc1, 0x22], "blendpd xmm0, xmm1, 0x22"),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x10, 0x06], "pblendvb xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x10, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x14, 0x06], "blendvps xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x14, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x15, 0x06], "blendvpd xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x15, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x17, 0x06], "ptest xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x17, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x20, 0x06], "pmovsxbw xmm0, qword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x20, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x21, 0x06], "pmovsxbd xmm0, dword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x21, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x22, 0x06], "pmovsxbq xmm0, word [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x22, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x23, 0x06], "pmovsxwd xmm0, qword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x23, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x24, 0x06], "pmovsxwq xmm0, dword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x24, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x25, 0x06], "pmovsxdq xmm0, qword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x25, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x28, 0x06], "pmuldq xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x28, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x29, 0x06], "pcmpeqq xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x29, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x2a, 0x06], "movntdqa xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x66, 0x0f, 0x38, 0x2a, 0xc6]),
        testcase!(invalid: &[0x0f, 0x38, 0x2a, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x2b, 0x06], "packusdw xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x2b, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x30, 0x06], "pmovzxbw xmm0, qword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x30, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x31, 0x06], "pmovzxbd xmm0, dword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x31, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x32, 0x06], "pmovzxbq xmm0, word [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x32, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x33, 0x06], "pmovzxwd xmm0, qword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x33, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x34, 0x06], "pmovzxwq xmm0, dword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x34, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x35, 0x06], "pmovzxdq xmm0, qword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x35, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x38, 0x06], "pminsb xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x38, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x39, 0x06], "pminsd xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x39, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3a, 0x06], "pminuw xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3a, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3b, 0x06], "pminud xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3b, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3c, 0x06], "pmaxsb xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3c, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3d, 0x06], "pmaxsd xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3d, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3e, 0x06], "pmaxuw xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3e, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x3f, 0x06], "pmaxud xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x3f, 0x06]),


        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x40, 0x06], "pmulld xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x40, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x38, 0x41, 0x06], "phminposuw xmm0, xmmword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x41, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x08, 0x06, 0x31], "roundps xmm0, xmmword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x08, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x09, 0x06, 0x31], "roundpd xmm0, xmmword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x09, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0a, 0x06, 0x31], "roundss xmm0, dword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x0a, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0b, 0x06, 0x31], "roundsd xmm0, qword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x0b, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0e, 0x06, 0x31], "pblendw xmm0, xmmword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x0e, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x14, 0x06, 0x31], "pextrb byte [esi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x14, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x15, 0x06, 0x31], "pextrw word [esi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x15, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x16, 0x06, 0x31], "pextrd dword [esi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x16, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x17, 0x06, 0x31], "extractps dword [esi], xmm0, 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x17, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x20, 0x06, 0x31], "pinsrb xmm0, byte [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x20, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x21, 0x06, 0x31], "insertps xmm0, dword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x21, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x22, 0x06, 0x31], "pinsrd xmm0, dword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x22, 0x06]),

        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x40, 0x06, 0x31], "dpps xmm0, xmmword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x40, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x41, 0x06, 0x31], "dppd xmm0, xmmword [esi], 0x31"),
        testcase!(invalid: &[0x0f, 0x3a, 0x41, 0x06]),
        testcase!(features { SSE4_1: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x42, 0x06, 0x44], "mpsadbw xmm0, xmmword [esi], 0x44"),
        testcase!(invalid: &[0x0f, 0x3a, 0x42, 0x06]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod ssse3 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x00, 0xda], "pshufb xmm3, xmm2"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x00, 0x06], "pshufb xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x00, 0x06], "pshufb mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x01, 0x06], "phaddw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x01, 0x06], "phaddw mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x02, 0x06], "phaddd xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x02, 0x06], "phaddd mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x03, 0x06], "phaddsw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x03, 0x06], "phaddsw mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x04, 0x06], "pmaddubsw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x04, 0x06], "pmaddubsw mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x05, 0x06], "phsubw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x05, 0x06], "phsubw mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x06, 0x06], "phsubd xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x06, 0x06], "phsubd mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x07, 0x06], "phsubsw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x07, 0x06], "phsubsw mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x08, 0x06], "psignb xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x08, 0x06], "psignb mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x09, 0x06], "psignw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x09, 0x06], "psignw mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x0a, 0x06], "psignd xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x0a, 0x06], "psignd mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x0b, 0x06], "pmulhrsw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x0b, 0x06], "pmulhrsw mm0, qword [esi]"),

        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x1c, 0x06], "pabsb xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x1c, 0x06], "pabsb mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x1d, 0x06], "pabsw xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x1d, 0x06], "pabsw mm0, qword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x38, 0x1e, 0x06], "pabsd xmm0, xmmword [esi]"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x38, 0x1e, 0x06], "pabsd mm0, qword [esi]"),

        testcase!(features { SSSE3: true, AVX: false } &[0x66, 0x0f, 0x3a, 0x0f, 0x06, 0x30], "palignr xmm0, xmmword [esi], 0x30"),
        testcase!(features { SSSE3: true, AVX: false } &[0x0f, 0x3a, 0x0f, 0x06, 0x30], "palignr mm0, qword [esi], 0x30"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod _0f01 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // drawn heavily from "Table A-6.  Opcode Extensions for One- and Two-byte Opcodes by Group
        // Number"
        testcase!(&[0x0f, 0x01, 0x38], "invlpg byte [eax]"),
        testcase!(&[0x0f, 0x01, 0x3f], "invlpg byte [edi]"),
        testcase!(&[0x0f, 0x01, 0x40, 0xff], "sgdt far [eax - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x41, 0xff], "sgdt far [ecx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x49, 0xff], "sidt far [ecx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x51, 0xff], "lgdt far [ecx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x59, 0xff], "lidt far [ecx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x61, 0xff], "smsw word [ecx - 0x1]"),
        testcase!(invalid: &[0x0f, 0x01, 0x69, 0xff]),
        testcase!(&[0x0f, 0x01, 0x71, 0xff], "lmsw word [ecx - 0x1]"),
        testcase!(&[0x0f, 0x01, 0x79, 0xff], "invlpg byte [ecx - 0x1]"),
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
        testcase!(&[0x0f, 0x01, 0xd8], "vmrun eax"),
        testcase!(&[0x0f, 0x01, 0xd9], "vmmcall"),
        testcase!(&[0x0f, 0x01, 0xda], "vmload eax"),
        testcase!(&[0x0f, 0x01, 0xdb], "vmsave eax"),
        testcase!(&[0x0f, 0x01, 0xdc], "stgi"),
        testcase!(&[0x0f, 0x01, 0xdd], "clgi"),
        testcase!(&[0x0f, 0x01, 0xde], "skinit eax"),
        testcase!(&[0x0f, 0x01, 0xdf], "invlpga eax, ecx"),
    // TODO: not clear what SHOULD be reported for invlpgb. certainly not a `eax` operand. xed claims
    // that this is UD in protected mode. the AMD manual explicitly says this does not #UD in protected
    // mode. same for tlbsync.
    //    testcase!(&[0x0f, 0x01, 0xfe], "invlpgb eax, edx, ecx"),
    //    testcase!(&[0x0f, 0x01, 0xff], "tlbsync"),
    //    testcase!(&[0x2e, 0x67, 0x65, 0x2e, 0x46, 0x0f, 0x01, 0xff], "tlbsync"),
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
        testcase!(&[0x0f, 0x01, 0xf0], "lmsw ax"),
        testcase!(&[0x0f, 0x01, 0xf1], "lmsw cx"),
        testcase!(&[0x0f, 0x01, 0xf2], "lmsw dx"),
        testcase!(&[0x0f, 0x01, 0xf3], "lmsw bx"),
        testcase!(&[0x0f, 0x01, 0xf4], "lmsw sp"),
        testcase!(&[0x0f, 0x01, 0xf5], "lmsw bp"),
        testcase!(&[0x0f, 0x01, 0xf6], "lmsw si"),
        testcase!(&[0x0f, 0x01, 0xf7], "lmsw di"),
        testcase!(invalid: &[0x0f, 0x01, 0xf8]),
        testcase!(&[0x0f, 0x01, 0xf9], "rdtscp"),
        testcase!(&[0x0f, 0x01, 0xfa], "monitorx"),
        testcase!(&[0x0f, 0x01, 0xfb], "mwaitx"),
        testcase!(&[0x0f, 0x01, 0xfc], "clzero"),
        testcase!(&[0x0f, 0x01, 0xfd], "rdpru ecx"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod _0fae {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // drawn heavily from "Table A-6.  Opcode Extensions for One- and Two-byte Opcodes by Group
        // Number"
        testcase!(invalid: &[0xf3, 0x0f, 0xae, 0x87]),
        testcase!(invalid: &[0xf3, 0x0f, 0xae, 0x04, 0x4f]),
        testcase!(&[0x0f, 0xae, 0x04, 0x4f], "fxsave ptr [edi + ecx * 2]"),
        testcase!(&[0x0f, 0xae, 0x0c, 0x4f], "fxrstor ptr [edi + ecx * 2]"),
        testcase!(&[0x0f, 0xae, 0x14, 0x4f], "ldmxcsr dword [edi + ecx * 2]"),
        testcase!(&[0x0f, 0xae, 0x1c, 0x4f], "stmxcsr dword [edi + ecx * 2]"),
        testcase!(&[0x0f, 0xae, 0x24, 0x4f], "xsave ptr [edi + ecx * 2]"),
        testcase!(&[0x0f, 0xc7, 0x5c, 0x24, 0x40], "xrstors ptr [esp + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x64, 0x24, 0x40], "xsavec ptr [esp + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x6c, 0x24, 0x40], "xsaves ptr [esp + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x74, 0x24, 0x40], "vmptrld qword [esp + 0x40]"),
        testcase!(&[0x0f, 0xc7, 0x7c, 0x24, 0x40], "vmptrst qword [esp + 0x40]"),
        testcase!(&[0x0f, 0xae, 0x2c, 0x4f], "xrstor ptr [edi + ecx * 2]"),
        testcase!(&[0x0f, 0xae, 0x34, 0x4f], "xsaveopt ptr [edi + ecx * 2]"),
        testcase!(&[0x0f, 0xae, 0x3c, 0x4f], "clflush zmmword [edi + ecx * 2]"),

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
        run_test(CASES);
    }
}

mod system {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x63, 0xc1], "arpl cx, ax"),
        testcase!(&[0x63, 0x04, 0xba], "arpl word [edx + edi * 4], ax"),
        testcase!(&[0x66, 0x0f, 0xb2, 0x00], "lss ax, word [eax]"),
        testcase!(&[0x67, 0x0f, 0xb2, 0x00], "lss eax, far [bx + si * 1]"),
        testcase!(&[0x0f, 0xb2, 0x00], "lss eax, far [eax]"),
        testcase!(&[0x66, 0x0f, 0xb2, 0x00], "lss ax, word [eax]"),
        testcase!(invalid: &[0x0f, 0x22, 0xc8]),
        testcase!(invalid: &[0x0f, 0x20, 0xc8]),
        testcase!(&[0x0f, 0x22, 0xd0], "mov cr2, eax"),
        testcase!(invalid: &[0x0f, 0x22, 0xcf]),
        testcase!(&[0x0f, 0x22, 0xd7], "mov cr2, edi"),
        testcase!(&[0x0f, 0x20, 0xd0], "mov eax, cr2"),

        testcase!(&[0x0f, 0x23, 0xc8], "mov dr1, eax"),
        testcase!(&[0x0f, 0x21, 0xc8], "mov eax, dr1"),
        testcase!(&[0x0f, 0x06], "clts"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod arithmetic {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x81, 0xec, 0x10, 0x03, 0x00, 0x00], "sub esp, 0x310"),
        testcase!(&[0x0f, 0xaf, 0xc2], "imul eax, edx"),
        testcase!(&[0x69, 0x43, 0x6f, 0x6d, 0x70, 0x6c, 0x65], "imul eax, dword [ebx + 0x6f], 0x656c706d"),
        testcase!(&[0x66, 0x0f, 0xaf, 0xd1], "imul dx, cx"),
        testcase!(&[0xf6, 0xe8], "imul al"),
        testcase!(&[0xf6, 0x28], "imul byte [eax]"),
        testcase!(&[0x6b, 0x43, 0x6f, 0x6d], "imul eax, dword [ebx + 0x6f], 0x6d"),
        testcase!(&[0x00, 0xcc], "add ah, cl"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

#[allow(non_snake_case)]
mod E_decode {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xff, 0x75, 0xb8], "push dword [ebp - 0x48]"),
        testcase!(&[0xff, 0x75, 0x08], "push dword [ebp + 0x8]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod sse {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x10, 0x0c, 0xc7], "movss xmm1, dword [edi + eax * 8]"),
        testcase!(&[0xf3, 0x0f, 0x11, 0x0c, 0xc7], "movss dword [edi + eax * 8], xmm1"),
        testcase!(&[0x0f, 0x28, 0x00], "movaps xmm0, xmmword [eax]"),
        testcase!(&[0x0f, 0x29, 0x00], "movaps xmmword [eax], xmm0"),
        testcase!(&[0xf3, 0x0f, 0x2a, 0xc1], "cvtsi2ss xmm0, ecx"),
        testcase!(&[0xf3, 0x0f, 0x2a, 0x01], "cvtsi2ss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x2b, 0x00], "movntps xmmword [eax], xmm0"),
        testcase!(&[0xf3, 0x0f, 0x2c, 0xc1], "cvttss2si eax, xmm1"),
        testcase!(&[0xf3, 0x0f, 0x2c, 0x01], "cvttss2si eax, dword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x2d, 0xc1], "cvtss2si eax, xmm1"),
        testcase!(&[0xf3, 0x0f, 0x2d, 0x01], "cvtss2si eax, dword [ecx]"),
        testcase!(&[0x0f, 0x2e, 0x00], "ucomiss xmm0, dword [eax]"),
        testcase!(&[0x0f, 0x2f, 0x00], "comiss xmm0, dword [eax]"),
        testcase!(&[0x0f, 0x28, 0xd0], "movaps xmm2, xmm0"),
        testcase!(&[0x66, 0x0f, 0x28, 0xd0], "movapd xmm2, xmm0"),
        testcase!(&[0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [eax]"),
        testcase!(&[0x67, 0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [bx + si * 1]"),
        testcase!(&[0x66, 0x0f, 0x29, 0x00], "movapd xmmword [eax], xmm0"),
        testcase!(invalid: &[0x0f, 0x50, 0x00]),
        testcase!(&[0x0f, 0x50, 0xc1], "movmskps eax, xmm1"),
        testcase!(&[0x0f, 0x51, 0x01], "sqrtps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x51, 0x01], "sqrtss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x52, 0x01], "rsqrtps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x52, 0x01], "rsqrtss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x53, 0x01], "rcpps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x53, 0x01], "rcpss xmm0, dword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x53, 0xc1], "rcpss xmm0, xmm1"),
        testcase!(&[0x0f, 0x54, 0x01], "andps xmm0, xmmword [ecx]"),
        testcase!(&[0x0f, 0x55, 0x01], "andnps xmm0, xmmword [ecx]"),
        testcase!(&[0x0f, 0x56, 0x01], "orps xmm0, xmmword [ecx]"),
        testcase!(&[0x0f, 0x57, 0x01], "xorps xmm0, xmmword [ecx]"),
        testcase!(&[0x0f, 0x58, 0x01], "addps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x58, 0x01], "addss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x59, 0x01], "mulps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x59, 0x01], "mulss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x5a, 0x01], "cvtps2pd xmm0, qword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x5a, 0x01], "cvtss2sd xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x5b, 0x01], "cvtdq2ps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x5b, 0x01], "cvttps2dq xmm0, xmmword [ecx]"),
        testcase!(&[0x67, 0x0f, 0x5b, 0x01], "cvtdq2ps xmm0, xmmword [bx + di * 1]"),
        testcase!(&[0x0f, 0x5c, 0x01], "subps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x5c, 0x01], "subss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x5d, 0x01], "minps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x5d, 0x01], "minss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x5e, 0x01], "divps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x5e, 0x01], "divss xmm0, dword [ecx]"),
        testcase!(&[0x0f, 0x5f, 0x01], "maxps xmm0, xmmword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x5f, 0x01], "maxss xmm0, dword [ecx]"),

        testcase!(&[0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [eax], 0x7f"),

        testcase!(&[0x66, 0x0f, 0xef, 0xc0], "pxor xmm0, xmm0"),
        testcase!(&[0x66, 0x0f, 0xef, 0xc0], "pxor xmm0, xmm0"),
        testcase!(&[0xf2, 0x0f, 0x10, 0x0c, 0xc6], "movsd xmm1, qword [esi + eax * 8]"),
        testcase!(&[0xf3, 0x0f, 0x10, 0x04, 0x86], "movss xmm0, dword [esi + eax * 4]"),
        testcase!(&[0xf2, 0x0f, 0x59, 0xc8], "mulsd xmm1, xmm0"),
        testcase!(&[0xf3, 0x0f, 0x59, 0xc8], "mulss xmm1, xmm0"),

        testcase!(
            &[0xf3, 0x0f, 0x6f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqu xmm3, xmmword [esp + ebx * 4 - 0x334455cc]"
        ),
        testcase!(&[0xf3, 0x0f, 0x70, 0xc0, 0x4e], "pshufhw xmm0, xmm0, 0x4e"),
        testcase!(&[0xf3, 0x0f, 0x7e, 0xc1], "movq xmm0, xmm1"),
        testcase!(
            &[0xf3, 0x0f, 0x7f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
            "movdqu xmmword [esp + ebx * 4 - 0x334455cc], xmm3"
        ),

        testcase!(&[0xf3, 0x0f, 0xc2, 0xc3, 0x08], "cmpss xmm0, xmm3, 0x8"),
        testcase!(&[0xf3, 0x0f, 0xc2, 0x03, 0x08], "cmpss xmm0, dword [ebx], 0x8"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

// SETLE, SETNG, ...

mod mov {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xa0, 0x93, 0x62, 0xc4, 0x00], "mov al, byte [0xc46293]"),
        testcase!(&[0x67, 0xa0, 0x93, 0x62], "mov al, byte [0x6293]"),
        testcase!(&[0xa1, 0x93, 0x62, 0xc4, 0x00], "mov eax, dword [0xc46293]"),
        testcase!(&[0x67, 0xa1, 0x93, 0x62], "mov eax, dword [0x6293]"),
        testcase!(&[0xa2, 0x93, 0x62, 0xc4, 0x00], "mov byte [0xc46293], al"),
        testcase!(&[0x67, 0xa2, 0x93, 0x62], "mov byte [0x6293], al"),
        testcase!(&[0xa3, 0x93, 0x62, 0xc4, 0x00], "mov dword [0xc46293], eax"),
        testcase!(&[0x67, 0xa3, 0x93, 0x62], "mov dword [0x6293], eax"),
        testcase!(&[0xba, 0x01, 0x00, 0x00, 0x00], "mov edx, 0x1"),
        testcase!(&[0xc7, 0x04, 0x24, 0x00, 0x00, 0x00, 0x00], "mov dword [esp], 0x0"),
        testcase!(&[0x89, 0x44, 0x24, 0x08], "mov dword [esp + 0x8], eax"),
        testcase!(&[0x89, 0x43, 0x18], "mov dword [ebx + 0x18], eax"),
        testcase!(&[0xc7, 0x43, 0x10, 0x00, 0x00, 0x00, 0x00], "mov dword [ebx + 0x10], 0x0"),
        testcase!(&[0x89, 0x4e, 0x08], "mov dword [esi + 0x8], ecx"),
        testcase!(&[0x8b, 0x32], "mov esi, dword [edx]"),
        testcase!(&[0x8b, 0x4c, 0x10, 0xf8], "mov ecx, dword [eax + edx * 1 - 0x8]"),
        testcase!(&[0x89, 0x46, 0x10], "mov dword [esi + 0x10], eax"),
        testcase!(&[0x0f, 0x43, 0xec], "cmovnb ebp, esp"),
        testcase!(&[0x0f, 0xb6, 0x06], "movzx eax, byte [esi]"),
        testcase!(&[0x0f, 0xb7, 0x06], "movzx eax, word [esi]"),
        testcase!(&[0x89, 0x55, 0x94], "mov dword [ebp - 0x6c], edx"),
        testcase!(&[0x65, 0x89, 0x04, 0x25, 0xa8, 0x01, 0x00, 0x00], "mov dword gs:[0x1a8], eax"),
        testcase!(&[0x0f, 0xbe, 0x83, 0xb4, 0x00, 0x00, 0x00], "movsx eax, byte [ebx + 0xb4]"),
        testcase!(&[0xf3, 0x0f, 0x6f, 0x07], "movdqu xmm0, xmmword [edi]"),
        testcase!(&[0xf3, 0x0f, 0x7f, 0x45, 0x00], "movdqu xmmword [ebp], xmm0"),

        testcase!(&[0x0f, 0x97, 0xc0], "seta al"),
        testcase!(&[0x0f, 0x97, 0xc8], "seta al"),
        testcase!(&[0x0f, 0x97, 0x00], "seta byte [eax]"),
        testcase!(&[0x0f, 0x97, 0x08], "seta byte [eax]"),
    //    testcase!(&[0xd6], "salc"),
        testcase!(&[0x8e, 0x00], "mov es, word [eax]"),
        testcase!(&[0x8e, 0xc0], "mov es, ax"),
        testcase!(&[0x8c, 0xc0], "mov eax, es"),
        // cs is not an allowed destination
        testcase!(invalid: &[0x8e, 0x08]),
        testcase!(&[0x8e, 0x10], "mov ss, word [eax]"),
        testcase!(&[0x8e, 0xd0], "mov ss, ax"),
        testcase!(&[0x8c, 0xd0], "mov eax, ss"),
        testcase!(&[0x8e, 0x18], "mov ds, word [eax]"),
        testcase!(&[0x8e, 0xd8], "mov ds, ax"),
        testcase!(&[0x8c, 0xd8], "mov eax, ds"),
        testcase!(&[0x8e, 0x20], "mov fs, word [eax]"),
        testcase!(&[0x8e, 0xe0], "mov fs, ax"),
        testcase!(&[0x8c, 0xe0], "mov eax, fs"),
        testcase!(&[0x8e, 0x28], "mov gs, word [eax]"),
        testcase!(&[0x8e, 0xe8], "mov gs, ax"),
        testcase!(&[0x8c, 0xe8], "mov eax, gs"),
        testcase!(invalid: &[0x8e, 0x30]),
        testcase!(invalid: &[0x8e, 0x38]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod xchg {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x90], "nop"),
        testcase!(&[0x91], "xchg eax, ecx"),
        testcase!(&[0x66, 0x91], "xchg ax, cx"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod stack {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x50], "push ax"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod prefixes {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x31, 0xc0], "xor ax, ax"),
        testcase!(&[0x66, 0x32, 0xc0], "xor al, al"),
        testcase!(&[0x66, 0x32, 0xc5], "xor al, ch"),
        testcase!(invalid: &[0xf0, 0x33, 0xc0]),
        testcase!(&[0xf0, 0x31, 0x00], "lock xor dword [eax], eax"),
        testcase!(&[0xf0, 0x80, 0x30, 0x00], "lock xor byte [eax], 0x0"),
        testcase!(&[0xf0, 0x0f, 0xbb, 0x17], "lock btc dword [edi], edx"),
        testcase!(&[0x66, 0x2e, 0xf2, 0xf0, 0x0f, 0xbb, 0x13], "xacquire lock btc word cs:[ebx], dx"),
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
        run_test(CASES);
    }
}

mod control_flow {
    use crate::protected_mode::{TestCase, run_test};

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
        testcase!(&[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00], "call dword [0x24727e]", masm: "call dword ptr [0024727Eh]"),
        testcase!(&[0xff, 0x24, 0xcd, 0x70, 0xa0, 0xbc, 0x01], "jmp dword [ecx * 8 + 0x1bca070]"),
        testcase!(&[0xff, 0x14, 0xcd, 0x70, 0xa0, 0xbc, 0x01], "call dword [ecx * 8 + 0x1bca070]"),
        testcase!(&[0xff, 0xe0], "jmp eax"),
        testcase!(&[0xff, 0xd0], "call eax"),
        testcase!(&[0x66, 0xff, 0xe0], "jmp ax"),
        testcase!(&[0x67, 0xff, 0xe0], "jmp eax"),
        testcase!(&[0x66, 0xff, 0xd0], "call ax"),
        testcase!(&[0x67, 0xff, 0xd0], "call eax"),
        testcase!(invalid: &[0xff, 0xd8]),
        testcase!(&[0xff, 0x18], "callf far [eax]", masm: "call fword ptr [eax]"),
        testcase!(&[0xe0, 0x12], "loopnz $+0x12"),
        testcase!(&[0xe1, 0x12], "loopz $+0x12"),
        testcase!(&[0xe2, 0x12], "loop $+0x12"),
        testcase!(&[0xe3, 0x12], "jecxz $+0x12"),
        testcase!(&[0xe3, 0xf0], "jecxz $-0x10"),
        testcase!(&[0x67, 0xe3, 0x12], "jcxz $+0x12"),
        testcase!(&[0x67, 0xe3, 0xf0], "jcxz $-0x10"),
        testcase!(&[0xc3], "ret"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}
 
mod bad_instructions {
    use crate::protected_mode::{TestCase, run_test};

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
        run_test(CASES);
    }
}


mod test_cmp {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf6, 0x05, 0x2c, 0x9b, 0xff, 0xff, 0x01], "test byte [0xffff9b2c], 0x1"),
        testcase!(&[0x3d, 0x01, 0xf0, 0xff, 0xff], "cmp eax, -0xfff"),
        testcase!(&[0x83, 0xf8, 0xff], "cmp eax, -0x1"),
        testcase!(&[0x39, 0xc6], "cmp esi, eax"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod push_pop {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x5b], "pop ebx"),
        testcase!(&[0x5e], "pop esi"),
        testcase!(&[0x68, 0x7f, 0x63, 0xc4, 0x00], "push 0xc4637f", masm: "push 0C4637Fh"),
        testcase!(&[0x66, 0x8f, 0x00], "pop word [eax]", masm: "pop word ptr [eax]"),
        testcase!(&[0x8f, 0x00], "pop dword [eax]", masm: "pop dword ptr [eax]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod bmi1 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features nodefault { BMI1: true } &[0xf3, 0x0f, 0xbc, 0xd3], "tzcnt edx, ebx"),
        testcase!(features nodefault { Minimal: true } &[0xf3, 0x0f, 0xbc, 0xd3], "bsf edx, ebx"),
        testcase!(features nodefault { Minimal: true } &[0xf2, 0x0f, 0xbc, 0xd3], "bsf edx, ebx"),
        testcase!(features nodefault { BMI1: true } &[0x0f, 0xbc, 0xd3], "bsf edx, ebx"),

        // from the intel manual [`ANDN`, though this is true for `BMI1` generally]:
        // ```
        // VEX.W1 is ignored in non-64-bit modes.
        // ```

        // just 0f38
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0x60, 0xf2, 0x01], "andn eax, ebx, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0xe0, 0xf2, 0x01], "andn eax, ebx, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0x78, 0xf3, 0x09], "blsr eax, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0xf8, 0xf3, 0x09], "blsr eax, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0x78, 0xf3, 0x11], "blsmsk eax, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0xf8, 0xf3, 0x11], "blsmsk eax, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0x78, 0xf3, 0x19], "blsi eax, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0xf8, 0xf3, 0x19], "blsi eax, dword [ecx]"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0x60, 0xf7, 0x01], "bextr eax, dword [ecx], ebx"),
        testcase!(features { BMI1: true } &[0xc4, 0xe2, 0xe0, 0xf7, 0x01], "bextr eax, dword [ecx], ebx"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod bmi2 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // from the intel manual [`PDEP`, though this is true for `BMI2` generally]:
        // ```
        // VEX.W1 is ignored in non-64-bit modes.
        // ```

        // f2 0f3a
        testcase!(features { BMI2: true } &[0xc4, 0xe3, 0x7b, 0xf0, 0x01, 0x05], "rorx eax, dword [ecx], 0x5"),
        testcase!(features { BMI2: true } &[0xc4, 0xe3, 0xfb, 0xf0, 0x01, 0x05], "rorx eax, dword [ecx], 0x5"),

        // f2 0f38 map
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x63, 0xf5, 0x07], "pdep eax, ebx, dword [edi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe3, 0xf5, 0x07], "pdep eax, ebx, dword [edi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x63, 0xf6, 0x07], "mulx eax, ebx, dword [edi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe3, 0xf6, 0x07], "mulx eax, ebx, dword [edi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x63, 0xf7, 0x01], "shrx eax, dword [ecx], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe3, 0xf7, 0x01], "shrx eax, dword [ecx], ebx"),

        // f3 0f38 map
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x62, 0xf5, 0x07], "pext eax, ebx, dword [edi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe2, 0xf5, 0x07], "pext eax, ebx, dword [edi]"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x62, 0xf7, 0x01], "sarx eax, dword [ecx], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe2, 0xf7, 0x01], "sarx eax, dword [ecx], ebx"),

        // just 0f38
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x60, 0xf5, 0x07], "bzhi eax, dword [edi], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe0, 0xf5, 0x07], "bzhi eax, dword [edi], ebx"),

        // 66 0f38
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0x61, 0xf7, 0x01], "shlx eax, dword [ecx], ebx"),
        testcase!(features { BMI2: true } &[0xc4, 0xe2, 0xe1, 0xf7, 0x01], "shlx eax, dword [ecx], ebx"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod popcnt {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { Popcnt: true } &[0xf3, 0x0f, 0xb8, 0xc1], "popcnt eax, ecx"),
        testcase!(features { SSE4_2_Intel: true } &[0xf3, 0x0f, 0xb8, 0xc1], "popcnt eax, ecx"),
        testcase!(invalid: features nodefault { Minimal: false } &[0xf3, 0x0f, 0xb8, 0xc1]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod bitwise {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features nodefault { Minimal: true } &[0x0f, 0xbc, 0xd3], "bsf edx, ebx"),
        testcase!(features nodefault { Minimal: true } &[0x0f, 0xbb, 0x17], "btc dword [edi], edx"),
        testcase!(features nodefault { Minimal: true } &[0xf0, 0x0f, 0xbb, 0x17], "lock btc dword [edi], edx"),
        testcase!(&[0x0f, 0xa3, 0xd0], "bt eax, edx"),
        testcase!(&[0x0f, 0xab, 0xd0], "bts eax, edx"),
        testcase!(&[0x0f, 0xb3, 0xd0], "btr eax, edx"),
        testcase!(&[0x66, 0x0f, 0xb3, 0xc0], "btr ax, ax"),
        testcase!(&[0xd2, 0xe0], "shl al, cl"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod misc {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf1], "int 0x1"),
        testcase!(&[0xf5], "cmc"),
        testcase!(&[0xc8, 0x01, 0x02, 0x03], "enter 0x201, 0x3"),
        testcase!(&[0xc9], "leave"),
        testcase!(&[0xca, 0x12, 0x34], "retf 0x3412"),
        testcase!(&[0xcb], "retf"),
        testcase!(&[0x66, 0xcf], "iret"),
        testcase!(&[0xcf], "iretd"),
        testcase!(&[0xf2, 0x0f, 0x38, 0xf0, 0xc1], "crc32 eax, cl"),
        testcase!(&[0xf2, 0x0f, 0x38, 0xf1, 0xc1], "crc32 eax, ecx"),
        testcase!(&[0xfe, 0x00], "inc byte [eax]"),
        testcase!(&[0xfe, 0x08], "dec byte [eax]"),
        testcase!(&[0xff, 0x00], "inc dword [eax]"),
        testcase!(&[0xff, 0x08], "dec dword [eax]"),
        testcase!(&[0xe4, 0x99], "in al, 0x99"),
        testcase!(&[0xe5, 0x99], "in eax, 0x99", masm: "in eax, 99h"),
        testcase!(&[0x67, 0xe5, 0x99], "in eax, 0x99"),
        testcase!(&[0xe5, 0x99], "in eax, 0x99"),
        testcase!(&[0xe6, 0x99], "out 0x99, al"),
        testcase!(&[0xe7, 0x99], "out 0x99, eax", masm: "out 99h, eax"),
        testcase!(&[0xec], "in al, dx"),
        testcase!(&[0xed], "in eax, dx"),
        testcase!(&[0xee], "out dx, al"),
        testcase!(&[0xef], "out dx, eax"),
        testcase!(&[0xcd, 0x00], "int 0x0"),
        testcase!(&[0xcd, 0xff], "int 0xff"),
        testcase!(&[0x9c], "pushf", masm: "pushfd"),
        testcase!(&[0x98], "cwde"),
        testcase!(&[0x66, 0x99], "cwd"),
        testcase!(&[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00], "nop word cs:[eax + eax * 1]"),
        testcase!(&[0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00], "nop word [eax + eax * 1]"),
        testcase!(&[0x8d, 0xa4, 0xc7, 0x20, 0x00, 0x00, 0x12], "lea esp, dword [edi + eax * 8 + 0x12000020]"),
        testcase!(&[0x33, 0xc0], "xor eax, eax"),
        testcase!(&[0x8d, 0x53, 0x08], "lea edx, dword [ebx + 0x8]"),
        testcase!(invalid: &[0x8d, 0xdd]),
        testcase!(&[0x31, 0xc9], "xor ecx, ecx"),
        testcase!(&[0x29, 0xc8], "sub eax, ecx"),
        testcase!(&[0x03, 0x0b], "add ecx, dword [ebx]"),
        testcase!(&[0x8d, 0x0c, 0x12], "lea ecx, dword [edx + edx * 1]"),
        testcase!(&[0xf6, 0xc2, 0x18], "test dl, 0x18"),
        testcase!(&[0xf3, 0xab], "rep stos dword es:[edi], eax"),
        testcase!(&[0xf3, 0xa5], "rep movs dword es:[edi], dword ds:[esi]"),
        testcase!(&[0xf3, 0x0f, 0xbc, 0xd7], "tzcnt edx, edi"),

        // TODO:
        // this is actually vmx
        // testcase!(invalid: &[0x66, 0x0f, 0xc7, 0x03]),
        testcase!(&[0x66, 0x0f, 0xc7, 0x33], "vmclear qword [ebx]"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0x33], "vmxon qword [ebx]"),

        testcase!(&[0xf3, 0x0f, 0xae, 0x26], "ptwrite dword [esi]"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xe6], "ptwrite esi"),
        testcase!(invalid: &[0x66, 0xf3, 0x0f, 0xae, 0xe6]),
        testcase!(&[0xf3, 0x0f, 0xae, 0xc4], "rdfsbase esp"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xcc], "rdgsbase esp"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xd4], "wrfsbase esp"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xdc], "wrgsbase esp"),
        testcase!(&[0x66, 0x0f, 0xae, 0x3f], "clflushopt zmmword [edi]"), // or clflush without 66
        testcase!(invalid: &[0x66, 0x0f, 0xae, 0xff]),
        testcase!(&[0x66, 0x0f, 0xae, 0x37], "clwb zmmword [edi]"),
        testcase!(&[0x66, 0x0f, 0xae, 0xf7], "tpause edi"),
        testcase!(&[0xf3, 0x0f, 0xae, 0xf1], "umonitor ecx"),
        testcase!(&[0x67, 0xf3, 0x0f, 0xae, 0xf1], "umonitor cx"),
        testcase!(&[0xf2, 0x0f, 0xae, 0xf1], "umwait ecx"),
        testcase!(&[0x66, 0x0f, 0x38, 0x80, 0x2f], "invept ebp, xmmword [edi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x80, 0x2f]),
        testcase!(&[0x66, 0x0f, 0x38, 0x81, 0x2f], "invvpid ebp, xmmword [edi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x81, 0x2f]),
        testcase!(&[0x66, 0x0f, 0x38, 0x82, 0x2f], "invpcid ebp, xmmword [edi]"),
        testcase!(invalid: &[0x0f, 0x38, 0x82, 0x2f]),
        testcase!(&[0x66, 0x0f, 0xae, 0xf1], "tpause ecx"),
        testcase!(&[0xc4, 0b111_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm2, ymm1, 0x77"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod evex {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // vpbroadcastmw2d. similar to `vpmovm2*`, out-of-range `k` are just masked down.
        testcase!(&[0x62, 0xd2, 0x7e, 0x28, 0x3a, 0xca], "vpbroadcastmw2d ymm1, k2"),
        // vpmovm2b (and larger forms). for some reason the source operand is a mask register but uses
        // modrm bits as a register selector. out-of-range `k` seem to just get masked down..
        testcase!(&[0x62, 0xd2, 0x7e, 0x08, 0x28, 0xc2], "vpmovm2b xmm0, k2"),
        testcase!(&[0x62, 0xf2, 0x7e, 0x08, 0x28, 0xc1], "vpmovm2b xmm0, k1"),
        // vpmovb2m (and larger forms). out-of-range `k` are invalid in 64-bit mode, are part of the
        // `bound` instruction for 32- and 16-bit modes.
        testcase!(&[0x62, 0x72, 0x7e /* , 0x28, 0x29, 0xfd */], "bound esi, qword [edx + 0x7e]"),
        testcase!(&[0x62, 0xf2, 0x7e, 0x28, 0x29, 0xfd], "vpmovb2m k7, ymm5"),

        testcase!(&[0x62, 0xf2, 0x7d, 0x48, 0x2a, 0x44, 0x40, 0x01], "vmovntdqa zmm0, zmmword [eax + eax * 2 + 0x40]"),
        testcase!(&[0x62, 0xf2, 0x7d, 0x08, 0x2a, 0x44, 0x40, 0x01], "vmovntdqa xmm0, xmmword [eax + eax * 2 + 0x10]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod vex {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // prefix 03
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_001, 0x00, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x00, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b1_1111_101, 0x00, 0b11_001_010, 0x77], "vpermq ymm1, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_001, 0x01, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x01, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b1_1111_101, 0x01, 0b11_001_010, 0x77], "vpermpd ymm1, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_001, 0x02, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_101, 0x02, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x02, 0b11_001_010, 0x77], "vpblendd xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x02, 0b00_001_010, 0x77], "vpblendd xmm1, xmm0, xmmword [edx], 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x02, 0b11_001_010, 0x77], "vpblendd ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x02, 0b00_001_010, 0x77], "vpblendd ymm1, ymm0, ymmword [edx], 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x04, 0b11_001_010, 0x77], "vpermilps xmm1, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x04, 0b11_001_010, 0x77], "vpermilps ymm1, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x05, 0b11_001_010, 0x77], "vpermilpd xmm1, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x05, 0b11_001_010, 0x77], "vpermilpd ymm1, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_001, 0x06, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x06, 0b11_001_010, 0x77], "vperm2f128 ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x06, 0b00_001_010, 0x77], "vperm2f128 ymm1, ymm0, ymmword [edx], 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x0c, 0b11_001_010, 0x77], "vblendps xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x0c, 0b11_001_010, 0x77], "vblendps ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x0d, 0b11_001_010, 0x77], "vblendpd xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x0d, 0b11_001_010, 0x77], "vblendpd ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x0e, 0b11_001_010, 0x77], "vpblendw xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x0e, 0b11_001_010, 0x77], "vpblendw ymm1, ymm0, ymm2, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x08, 0b11_001_010, 0x77], "vroundps xmm1, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x08, 0b11_001_010, 0x77], "vroundps ymm1, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x08, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x08, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x09, 0b11_001_010, 0x77], "vroundpd xmm1, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x09, 0b11_001_010, 0x77], "vroundpd ymm1, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x09, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x09, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x0a, 0b11_001_010, 0x77], "vroundss xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x0a, 0b11_001_010, 0x77], "vroundss xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x0b, 0b11_001_010, 0x77], "vroundsd xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x0b, 0b11_001_010, 0x77], "vroundsd xmm1, xmm0, xmm2, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b1_0111_001, 0x0f, 0b11_001_010, 0x77], "vpalignr xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b1_0111_101, 0x0f, 0b11_001_010, 0x77], "vpalignr ymm1, ymm0, ymm2, 0x77"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x14, 0b11_001_010, 0x77], "vpextrb edx, xmm1, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x14, 0b00_001_010, 0x77], "vpextrb byte [edx], xmm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x14, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x14, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x15, 0b11_001_010, 0x77], "vpextrw edx, xmm1, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x15, 0b00_001_010, 0x77], "vpextrw word [edx], xmm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x15, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x15, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x16, 0b11_001_010, 0x77], "vpextrd edx, xmm1, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x16, 0b00_001_010, 0x77], "vpextrd dword [edx], xmm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x16, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x16, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b1_1111_001, 0x16, 0b11_001_010, 0x77], "vpextrd edx, xmm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_001, 0x16, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b1_1111_001, 0x16, 0b00_001_010, 0x77], "vpextrd dword [edx], xmm1, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x17, 0b11_001_010, 0x77], "vextractps edx, xmm1, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x17, 0b00_001_010, 0x77], "vextractps dword [edx], xmm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x17, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x17, 0b00_001_010, 0x77]),

        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_001, 0x18, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x18, 0b11_001_010, 0x77], "vinsertf128 ymm1, ymm0, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_101, 0x19, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x19, 0b11_001_010, 0x77], "vextractf128 xmm2, ymm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]),

        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_001, 0x38, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x38, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x38, 0b11_001_010, 0x77], "vinserti128 ymm1, ymm0, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_101, 0x39, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x39, 0b11_001_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x39, 0b11_001_010, 0x77], "vextracti128 xmm2, ymm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]),

        testcase!(features { AVX_F16C: true, AVX: false, F16C: false } &[0xc4, 0b110_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm2, ymm1, 0x77"),
        testcase!(features { AVX_F16C: true, AVX: false, F16C: false } &[0xc4, 0b110_00011, 0b0_1111_101, 0x1d, 0b11_001_010, 0x77], "vcvtps2ph xmm2, ymm1, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_101, 0x1d, 0b11_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x20, 0b11_001_010, 0x77], "vpinsrb xmm1, xmm0, edx, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x20, 0b00_001_010, 0x77], "vpinsrb xmm1, xmm0, byte [edx], 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x20, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x21, 0b11_001_010, 0x77], "vinsertps xmm1, xmm0, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x21, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x22, 0b11_001_010, 0x77], "vpinsrd xmm1, xmm0, edx, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x22, 0b00_001_010, 0x77], "vpinsrd xmm1, xmm0, dword [edx], 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x22, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b1_0111_001, 0x22, 0b11_001_010, 0x77], "vpinsrd xmm1, xmm0, edx, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b1_0111_001, 0x22, 0b00_001_010, 0x77], "vpinsrd xmm1, xmm0, dword [edx], 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_101, 0x22, 0b00_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x40, 0b11_001_010, 0x77], "vdpps xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x40, 0b11_001_010, 0x77], "vdpps ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x41, 0b11_001_010, 0x77], "vdppd xmm1, xmm0, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x41, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x42, 0b11_001_010, 0x77], "vmpsadbw xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x42, 0b11_001_010, 0x77], "vmpsadbw ymm1, ymm0, ymm2, 0x77"),

        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x46, 0b11_001_010, 0x77], "vperm2i128 ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_1111_101, 0x46, 0b00_001_010, 0x77], "vperm2i128 ymm1, ymm0, ymmword [edx], 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_001, 0x46, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_1111_101, 0x46, 0b11_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_0111_001, 0x4c, 0b11_001_010, 0x77], "vpblendvb xmm1, xmm0, xmm2, xmm7"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00011, 0b0_0111_101, 0x4c, 0b11_001_010, 0x77], "vpblendvb ymm1, ymm0, ymm2, ymm7"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_001, 0x4c, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_101, 0x4c, 0b11_001_010, 0x77]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x60, 0b11_001_010, 0x77], "vpcmpestrm xmm1, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x60, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x60, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x60, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x61, 0b11_001_010, 0x77], "vpcmpestri xmm1, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x61, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x61, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x61, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x62, 0b11_001_010, 0x77], "vpcmpistrm xmm1, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x62, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x62, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x62, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00011, 0b0_1111_001, 0x63, 0b11_001_010, 0x77], "vpcmpistri xmm1, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_1111_101, 0x63, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_001, 0x63, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b0_0111_101, 0x63, 0b11_001_010, 0x77]),

        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00011, 0b1_1111_001, 0xdf, 0b11_001_010, 0x77], "vaeskeygenassist xmm1, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_001, 0xdf, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00011, 0b1_0111_101, 0xdf, 0b11_001_010, 0x77]),

        // prefix 02
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x00, 0b11_001_010], "vpshufb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x00, 0b11_001_010], "vpshufb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x01, 0b11_001_010], "vphaddw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x01, 0b11_001_010], "vphaddw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x02, 0b11_001_010], "vphaddd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x02, 0b11_001_010], "vphaddd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x03, 0b11_001_010], "vphaddsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x03, 0b11_001_010], "vphaddsw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x04, 0b11_001_010], "vpmaddubsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x04, 0b11_001_010], "vpmaddubsw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x05, 0b11_001_010], "vphsubw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x05, 0b11_001_010], "vphsubw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x06, 0b11_001_010], "vphsubd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x06, 0b11_001_010], "vphsubd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x07, 0b11_001_010], "vphsubsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x07, 0b11_001_010], "vphsubsw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x08, 0b11_001_010], "vpsignb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x08, 0b11_001_010], "vpsignb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x09, 0b11_001_010], "vpsignw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x09, 0b11_001_010], "vpsignw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x0a, 0b11_001_010], "vpsignd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x0a, 0b11_001_010], "vpsignd ymm1, ymm0, ymm2"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x0b, 0b11_001_010], "vpmulhrsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x0b, 0b11_001_010], "vpmulhrsw ymm1, ymm0, ymm2"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x0c, 0b11_001_010], "vpermilps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x0c, 0b11_001_010], "vpermilps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x0d, 0b11_001_010], "vpermilpd xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x0d, 0b11_001_010], "vpermilpd ymm1, ymm0, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_001, 0x0d, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0x0d, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x0e, 0b11_001_010], "vtestps xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x0e, 0b11_001_010], "vtestps ymm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x0e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x0f, 0b11_001_010], "vtestpd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x0f, 0b11_001_010], "vtestpd ymm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x0f, 0b11_001_010]),
        testcase!(features { AVX_F16C: true, AVX: false, F16C: false } &[0xc4, 0b111_00010, 0b0_1111_001, 0x13, 0b11_001_010], "vcvtph2ps xmm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b111_00010, 0b1_1111_001, 0x13, 0b11_001_010]),


        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x16, 0b11_001_010], "vpermps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x16, 0b00_001_010], "vpermps ymm1, ymm0, ymmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_001, 0x16, 0b00_011_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x16, 0b00_011_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x17, 0b11_001_010], "vptest xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x17, 0b11_001_010], "vptest ymm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x17, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x17, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x18, 0b00_001_010], "vbroadcastss xmm1, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x18, 0b00_001_010], "vbroadcastss ymm1, dword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_001, 0x18, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x18, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x19, 0b00_001_010], "vbroadcastsd ymm1, qword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x19, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0x19, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x1a, 0b00_001_010], "vbroadcastf128 ymm1, xmmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x1a, 0b00_001_010]), // vex.w=1 is invalid
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0x1a, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_001, 0x1a, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x5a, 0b00_001_010], "vbroadcasti128 ymm1, xmmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_101, 0x5a, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_001, 0x5a, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x5a, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x18, 0b11_001_010], "vbroadcastss xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x18, 0b11_001_010], "vbroadcastss ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x18, 0b00_001_010], "vbroadcastss ymm1, dword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_001, 0x18, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0x18, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_001, 0x19, 0b11_001_010]), // "vbroadcastsd xmm, xmm" is not legal (L!=0)
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x19, 0b11_001_010], "vbroadcastsd ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x19, 0b00_001_010], "vbroadcastsd ymm1, qword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x19, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0x19, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_001, 0x19, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_001, 0x1a, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0x1a, 0b11_001_010]),


        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x1c, 0b11_001_010], "vpabsb xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x1c, 0b11_001_010], "vpabsb ymm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x1c, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x1d, 0b11_001_010], "vpabsw xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x1d, 0b11_001_010], "vpabsw ymm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x1d, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x1e, 0b11_001_010], "vpabsd xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x1e, 0b11_001_010], "vpabsd ymm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x1e, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x20, 0b11_001_010], "vpmovsxbw xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x20, 0b11_001_010], "vpmovsxbw ymm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x20, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x21, 0b11_001_010], "vpmovsxbd xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x21, 0b11_001_010], "vpmovsxbd ymm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x21, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x22, 0b11_001_010], "vpmovsxbq xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x22, 0b11_001_010], "vpmovsxbq ymm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x22, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x23, 0b11_001_010], "vpmovsxwd xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x23, 0b11_001_010], "vpmovsxwd ymm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x23, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x24, 0b11_001_010], "vpmovsxwq xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x24, 0b11_001_010], "vpmovsxwq ymm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x24, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x25, 0b11_001_010], "vpmovsxdq xmm1, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x25, 0b11_001_010], "vpmovsxdq ymm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x25, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x28, 0b11_001_010], "vpmuldq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x28, 0b11_001_010], "vpmuldq ymm1, ymm0, ymm2"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x29, 0b11_001_010], "vpcmpeqq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x29, 0b11_001_010], "vpcmpeqq ymm1, ymm0, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x2a, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x2a, 0b00_001_010], "vmovntdqa xmm1, xmmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x2a, 0b00_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x2a, 0b00_001_010], "vmovntdqa ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x2b, 0b11_001_010], "vpackusdw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x2b, 0b11_001_010], "vpackusdw ymm1, ymm0, ymm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x2b, 0b00_001_010], "vpackusdw ymm1, ymm0, ymmword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x2c, 0b00_001_010], "vmaskmovps xmm1, xmm0, xmmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x2c, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x2c, 0b00_001_010], "vmaskmovps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x2d, 0b00_001_010], "vmaskmovpd xmm1, xmm0, xmmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x2d, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x2d, 0b00_001_010], "vmaskmovpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x2e, 0b00_001_010], "vmaskmovps xmmword [edx], xmm0, xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x2e, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x2e, 0b00_001_010], "vmaskmovps ymmword [edx], ymm0, ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x2f, 0b00_001_010], "vmaskmovpd xmmword [edx], xmm0, xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x2f, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x2f, 0b00_001_010], "vmaskmovpd ymmword [edx], ymm0, ymm1"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x30, 0b11_001_010], "vpmovzxbw xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x30, 0b11_001_010], "vpmovzxbw ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x31, 0b11_001_010], "vpmovzxbd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x31, 0b11_001_010], "vpmovzxbd ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x32, 0b11_001_010], "vpmovzxbq xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x32, 0b11_001_010], "vpmovzxbq ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x33, 0b11_001_010], "vpmovzxwd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x33, 0b11_001_010], "vpmovzxwd ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x34, 0b11_001_010], "vpmovzxwq xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x34, 0b11_001_010], "vpmovzxwq ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x35, 0b11_001_010], "vpmovzxdq xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x35, 0b11_001_010], "vpmovzxdq ymm1, xmm2"),

        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x30, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x30, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x31, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x31, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x32, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x32, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x33, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x33, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x34, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x34, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x35, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x35, 0b11_001_010]),

        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x36, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x36, 0b11_001_010], "vpermd ymm1, ymm0, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x36, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x37, 0b11_001_010], "vpcmpgtq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x37, 0b11_001_010], "vpcmpgtq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x38, 0b11_001_010], "vpminsb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x38, 0b11_001_010], "vpminsb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x39, 0b11_001_010], "vpminsd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x39, 0b11_001_010], "vpminsd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x3a, 0b11_001_010], "vpminuw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x3a, 0b11_001_010], "vpminuw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x3b, 0b11_001_010], "vpminud xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x3b, 0b11_001_010], "vpminud ymm1, ymm0, ymm2"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x3c, 0b11_001_010], "vpmaxsb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x3c, 0b11_001_010], "vpmaxsb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x3d, 0b11_001_010], "vpmaxsd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x3d, 0b11_001_010], "vpmaxsd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x3e, 0b11_001_010], "vpmaxuw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x3e, 0b11_001_010], "vpmaxuw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x3f, 0b11_001_010], "vpmaxud xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x3f, 0b11_001_010], "vpmaxud ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_0111_001, 0x40, 0b11_001_010], "vpmulld xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_0111_101, 0x40, 0b11_001_010], "vpmulld ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x41, 0b11_001_010], "vphminposuw xmm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_001, 0x41, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0x41, 0b11_001_010]),
    // TODO: should something b11at opcode 42 here?
    //    testcase!(features { AVX: true } &[0xc4, 0b110_00010, 0b1_0111_001, 0x42, 0b11_001_010], "vphminposuw xmm"),
    //    testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0x41, 0b11_001_010]),

        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x45, 0b00_001_010], "vpsrlvd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x45, 0b00_001_010], "vpsrlvd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x45, 0b11_001_010], "vpsrlvd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x45, 0b11_001_010], "vpsrlvd ymm1, ymm0, ymm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x45, 0b00_001_010], "vpsrlvq xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x45, 0b00_001_010], "vpsrlvq ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x45, 0b11_001_010], "vpsrlvq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x45, 0b11_001_010], "vpsrlvq ymm1, ymm0, ymm2"),

        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x46, 0b00_001_010], "vpsravd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x46, 0b00_001_010], "vpsravd ymm1, ymm0, ymmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_001, 0x46, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_1111_101, 0x46, 0b00_001_010]),

        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x47, 0b00_001_010], "vpsllvd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x47, 0b00_001_010], "vpsllvd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x47, 0b11_001_010], "vpsllvd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x47, 0b11_001_010], "vpsllvd ymm1, ymm0, ymm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x47, 0b00_001_010], "vpsllvq xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x47, 0b00_001_010], "vpsllvq ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x47, 0b11_001_010], "vpsllvq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x47, 0b11_001_010], "vpsllvq ymm1, ymm0, ymm2"),

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

        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x8c, 0b00_001_010], "vpmaskmovd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x8c, 0b00_001_010], "vpmaskmovd ymm1, ymm0, ymmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_001, 0x8c, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x8c, 0b00_001_010], "vpmaskmovq xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x8c, 0b00_001_010], "vpmaskmovq ymm1, ymm0, ymmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_001, 0x8c, 0b11_001_010]),

        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x8e, 0b00_001_010], "vpmaskmovd xmmword [edx], xmm0, xmm1"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x8e, 0b00_001_010], "vpmaskmovd ymmword [edx], ymm0, ymm1"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_001, 0x8e, 0b11_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x8e, 0b00_001_010], "vpmaskmovq xmmword [edx], xmm0, xmm1"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x8e, 0b00_001_010], "vpmaskmovq ymmword [edx], ymm0, ymm1"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_1111_001, 0x8e, 0b11_001_010]),

        /*
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdd xmm0, dword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdd ymm0, dword [ecx + ymm12 * 4], ymm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdq xmm0, dword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdq ymm0, qword [ecx + ymm12 * 4], ymm0"),

        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm0, dword xmmword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b0_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm0, dword xmmword [ecx + ymm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqq xmm0, dword xmmword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b000_00010, 0b1_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqq ymm0, qword xmmword [ecx + ymm12 * 4], ymm0"),

        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdps xmm0, dword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdps ymm0, qword [ecx + ymm12 * 4], ymm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdpd xmm0, dword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdpd ymm0, qword [ecx + xmm12 * 4], ymm0"),

        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqps xmm0, dword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b0_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqps ymm0, qword [ecx + ymm12 * 4], ymm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqpd xmm0, dword [ecx + xmm12 * 4], xmm0"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00010, 0b1_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqpd ymm0, qword [ecx + ymm12 * 4], ymm0"),
        */

        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b0_1111_001, 0xdb, 0b11_001_010], "vaesimc xmm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b0_0111_101, 0xdb, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00010, 0b1_0111_101, 0xdb, 0b11_001_010]),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_001, 0xdc, 0b11_001_010], "vaesenc xmm1, xmm0, xmm2"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_101, 0xdc, 0b11_001_010], "vaesenc ymm1, ymm0, ymm2"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_001, 0xdd, 0b11_001_010], "vaesenclast xmm1, xmm0, xmm2"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_101, 0xdd, 0b11_001_010], "vaesenclast ymm1, ymm0, ymm2"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_001, 0xde, 0b11_001_010], "vaesdec xmm1, xmm0, xmm2"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_101, 0xde, 0b11_001_010], "vaesdec ymm1, ymm0, ymm2"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_001, 0xdf, 0b11_001_010], "vaesdeclast xmm1, xmm0, xmm2"),
        testcase!(features { AVX_AESNI: true, AVX: false, AESNI: false } &[0xc4, 0b110_00010, 0b1_0111_101, 0xdf, 0b11_001_010], "vaesdeclast ymm1, ymm0, ymm2"),

        // prefix 01
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0x10, 0b00_001_010], "vmovsd xmm1, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x10, 0b00_001_010], "vmovsd xmm1, qword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_011, 0x10, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_111, 0x10, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x10, 0b00_001_010], "vmovupd xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x10, 0b00_001_010], "vmovupd ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x11, 0b00_001_010], "vmovupd xmmword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x11, 0b00_001_010], "vmovupd ymmword [edx], ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0x11, 0b00_001_010], "vmovsd qword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x11, 0b00_001_010], "vmovsd qword [edx], xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_011, 0x11, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_111, 0x11, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x10, 0b00_001_010], "vmovupd xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x10, 0b00_001_010], "vmovupd ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x11, 0b00_001_010], "vmovupd xmmword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x11, 0b00_001_010], "vmovupd ymmword [edx], ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x10, 0b00_001_010], "vmovss xmm1, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x10, 0b00_001_010], "vmovss xmm1, dword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_010, 0x10, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_110, 0x10, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x10, 0b00_001_010], "vmovups xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x10, 0b00_001_010], "vmovups ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x11, 0b00_001_010], "vmovups xmmword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x11, 0b00_001_010], "vmovups ymmword [edx], ymm1"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x11, 0b11_001_010], "vmovsd xmm2, xmm0, xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x11, 0b11_001_010], "vmovsd xmm2, xmm0, xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x11, 0b00_001_010], "vmovss dword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x11, 0b00_001_010], "vmovss dword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x11, 0b00_001_010], "vmovups xmmword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x11, 0b00_001_010], "vmovups ymmword [edx], ymm1"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_011, 0x12, 0b00_001_010], "vmovddup xmm1, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_111, 0x12, 0b00_001_010], "vmovddup ymm1, ymmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_011, 0x12, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_111, 0x12, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x12, 0b11_001_010], "vmovhlps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x12, 0b00_001_010], "vmovlps xmm1, xmm0, qword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_100, 0x12, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_111, 0x12, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x12, 0b00_001_010], "vmovsldup xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x12, 0b00_001_010], "vmovsldup ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_010, 0x12, 0b00_001_010], "vmovsldup xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x12, 0b00_001_010], "vmovsldup ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x12, 0b00_001_010], "vmovlpd xmm1, xmm0, qword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_101, 0x12, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x13, 0b00_001_010], "vmovlpd qword [edx], xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_001, 0x13, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_101, 0x13, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_000, 0x14, 0b00_001_010], "vunpcklps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_100, 0x14, 0b00_001_010], "vunpcklps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x14, 0b00_001_010], "vunpcklpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x14, 0b00_001_010], "vunpcklpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_000, 0x15, 0b00_001_010], "vunpckhps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_100, 0x15, 0b00_001_010], "vunpckhps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x15, 0b00_001_010], "vunpckhpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x15, 0b00_001_010], "vunpckhpd ymm1, ymm0, ymmword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x16, 0b11_001_010], "vmovshdup xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x16, 0b11_001_010], "vmovshdup ymm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_010, 0x16, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_110, 0x16, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x16, 0b00_001_010], "vmovhps xmm1, xmm0, qword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_100, 0x16, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x16, 0b00_001_010], "vmovhpd xmm1, xmm0, qword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_101, 0x16, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_001, 0x16, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x17, 0b00_001_010], "vmovhps qword [edx], xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_100, 0x17, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_000, 0x17, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_100, 0x17, 0b00_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x17, 0b00_001_010], "vmovhpd qword [edx], xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_001, 0x17, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0x17, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_000, 0x28, 0b11_001_010], "vmovaps xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x28, 0b11_001_010], "vmovaps ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_000, 0x29, 0b11_001_010], "vmovaps xmm2, xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x29, 0b11_001_010], "vmovaps ymm2, ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x28, 0b11_001_010], "vmovapd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x28, 0b11_001_010], "vmovapd ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x29, 0b11_001_010], "vmovapd xmm2, xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x29, 0b11_001_010], "vmovapd ymm2, ymm1"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_010, 0x2a, 0b11_001_010], "vcvtsi2ss xmm1, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_010, 0x2a, 0b00_001_010], "vcvtsi2ss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x2a, 0b11_001_010], "vcvtsi2ss xmm1, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x2a, 0b00_001_010], "vcvtsi2ss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x2a, 0b11_001_010], "vcvtsi2ss xmm1, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_011, 0x2a, 0b11_001_010], "vcvtsi2sd xmm1, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm1, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm1, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_011, 0x2a, 0b00_001_010], "vcvtsi2sd xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0x2a, 0b00_001_010], "vcvtsi2sd xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_011, 0x2a, 0b11_001_010], "vcvtsi2sd xmm1, xmm0, edx"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_111, 0x2a, 0b11_001_010], "vcvtsi2sd xmm1, xmm0, edx"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_000, 0x2b, 0b00_001_010], "vmovntps xmmword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x2b, 0b00_001_010], "vmovntps ymmword [edx], ymm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_000, 0x2b, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_100, 0x2b, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x2b, 0b00_001_010], "vmovntpd xmmword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x2b, 0b00_001_010], "vmovntpd ymmword [edx], ymm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_001, 0x2b, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0x2b, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_010, 0x2c, 0b11_001_010], "vcvttss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x2c, 0b11_001_010], "vcvttss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x2c, 0b11_001_010], "vcvttss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x2c, 0b00_001_010], "vcvttss2si ecx, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x2c, 0b00_001_010], "vcvttss2si ecx, dword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_010, 0x2c, 0b11_001_010], "vcvttss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_010, 0x2c, 0b00_001_010], "vcvttss2si ecx, dword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_110, 0x2c, 0b11_001_010], "vcvttss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_011, 0x2c, 0b11_001_010], "vcvttsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si ecx, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si ecx, qword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_011, 0x2c, 0b11_001_010], "vcvttsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_111, 0x2c, 0b11_001_010], "vcvttsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_111, 0x2c, 0b00_001_010], "vcvttsd2si ecx, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_010, 0x2d, 0b11_001_010], "vcvtss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x2d, 0b11_001_010], "vcvtss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x2d, 0b00_001_010], "vcvtss2si ecx, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x2d, 0b00_001_010], "vcvtss2si ecx, dword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_010, 0x2d, 0b11_001_010], "vcvtss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_010, 0x2d, 0b00_001_010], "vcvtss2si ecx, dword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_110, 0x2d, 0b11_001_010], "vcvtss2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_011, 0x2d, 0b11_001_010], "vcvtsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_011, 0x2d, 0b00_001_010], "vcvtsd2si ecx, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_111, 0x2d, 0b00_001_010], "vcvtsd2si ecx, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0x2d, 0b00_001_010], "vcvtsd2si ecx, qword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_011, 0x2d, 0b11_001_010], "vcvtsd2si ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_111, 0x2d, 0b11_001_010], "vcvtsd2si ecx, xmm2"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x2e, 0b00_001_010], "vucomisd xmm1, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x2e, 0b00_001_010], "vucomisd xmm1, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x2e, 0b11_001_010], "vucomisd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x2e, 0b11_001_010], "vucomisd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x2f, 0b00_001_010], "vcomisd xmm1, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x2f, 0b00_001_010], "vcomisd xmm1, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x2f, 0b11_001_010], "vcomisd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x2f, 0b11_001_010], "vcomisd xmm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_001, 0x2e, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_101, 0x2e, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_001, 0x2e, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_101, 0x2e, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_001, 0x2f, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_101, 0x2f, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_001, 0x2f, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_0111_101, 0x2f, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc5, 0b1_1111_000, 0x2e, 0b11_001_010], "vucomiss xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_100, 0x2e, 0b00_001_010], "vucomiss xmm1, dword [edx]"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_000, 0x2f, 0b11_001_010], "vcomiss xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc5, 0b1_1111_100, 0x2f, 0b00_001_010], "vcomiss xmm1, dword [edx]"),
        testcase!(invalid: &[0xc5, 0b1_1111_111, 0x2f, 0b11_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x50, 0b11_001_010], "vmovmskps ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x50, 0b11_001_010], "vmovmskps ecx, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_000, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_100, 0x50, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x50, 0b11_001_010], "vmovmskpd ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x50, 0b11_001_010], "vmovmskpd ecx, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x50, 0b11_001_010], "vmovmskpd ecx, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x50, 0b11_001_010], "vmovmskpd ecx, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_001, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_101, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_001, 0x50, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_101, 0x50, 0b00_001_010]),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x51, 0b00_001_010], "vsqrtpd xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x51, 0b00_001_010], "vsqrtpd ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_011, 0x51, 0b00_001_010], "vsqrtsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_111, 0x51, 0b00_001_010], "vsqrtsd xmm1, xmm0, qword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x51, 0b00_001_010], "vsqrtps xmm1, xmmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_000, 0x51, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x51, 0b00_001_010], "vsqrtps ymm1, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x51, 0b00_001_010], "vsqrtss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x51, 0b00_001_010], "vsqrtss xmm1, xmm0, dword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x52, 0b11_001_010], "vrsqrtps xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x52, 0b11_001_010], "vrsqrtps ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x52, 0b11_001_010], "vrsqrtss xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x52, 0b11_001_010], "vrsqrtss xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x53, 0b11_001_010], "vrcpps xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x53, 0b11_001_010], "vrcpps ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x53, 0b11_001_010], "vrcpss xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x53, 0b11_001_010], "vrcpss xmm1, xmm0, xmm2"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x54, 0b11_001_010], "vandps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x54, 0b11_001_010], "vandps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x55, 0b11_001_010], "vandnps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x55, 0b11_001_010], "vandnps ymm1, ymm0, ymm2"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x54, 0b00_001_010], "vandpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x54, 0b00_001_010], "vandpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x55, 0b00_001_010], "vandnpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x55, 0b00_001_010], "vandnpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x56, 0b00_001_010], "vorpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x56, 0b00_001_010], "vorpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x56, 0b00_001_010], "vorps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x56, 0b00_001_010], "vorps ymm1, ymm0, ymmword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x57, 0b11_001_010], "vxorps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x57, 0b11_001_010], "vxorps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x57, 0b11_001_010], "vxorpd xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x57, 0b11_001_010], "vxorpd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x58, 0b11_001_010], "vaddps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x58, 0b11_001_010], "vaddps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_010, 0x58, 0b11_001_010], "vaddss xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_110, 0x58, 0b11_001_010], "vaddss xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_010, 0x58, 0b00_001_010], "vaddss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_110, 0x58, 0b00_001_010], "vaddss xmm1, xmm0, dword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x58, 0b00_001_010], "vaddpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x58, 0b00_001_010], "vaddpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x58, 0b00_001_010], "vaddsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x58, 0b00_001_010], "vaddsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x59, 0b00_001_010], "vmulps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x59, 0b00_001_010], "vmulps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x59, 0b00_001_010], "vmulpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x59, 0b00_001_010], "vmulpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_010, 0x59, 0b00_001_010], "vmulss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_110, 0x59, 0b00_001_010], "vmulss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x59, 0b00_001_010], "vmulsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x59, 0b00_001_010], "vmulsd xmm1, xmm0, qword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x5a, 0b11_001_010], "vcvtps2pd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x5a, 0b11_001_010], "vcvtps2pd ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x5a, 0b00_001_010], "vcvtps2pd xmm1, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x5a, 0b00_001_010], "vcvtps2pd ymm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x5a, 0b11_001_010], "vcvtpd2ps xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x5a, 0b11_001_010], "vcvtpd2ps xmm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0x5a, 0b11_001_010], "vcvtsd2ss xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x5a, 0b11_001_010], "vcvtsd2ss xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0x5a, 0b00_001_010], "vcvtsd2ss xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x5a, 0b00_001_010], "vcvtsd2ss xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_011, 0x5a, 0b00_001_010], "vcvtsd2ss xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_111, 0x5a, 0b00_001_010], "vcvtsd2ss xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x5a, 0b11_001_010], "vcvtss2sd xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x5a, 0b11_001_010], "vcvtss2sd xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x5a, 0b00_001_010], "vcvtss2sd xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x5a, 0b00_001_010], "vcvtss2sd xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x5a, 0b00_001_010], "vcvtss2sd xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x5a, 0b00_001_010], "vcvtss2sd xmm1, xmm0, dword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x5b, 0b11_001_010], "vcvtps2dq xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x5b, 0b11_001_010], "vcvtps2dq ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x5b, 0b11_001_010], "vcvttps2dq xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x5b, 0b11_001_010], "vcvttps2dq ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x5b, 0b11_001_010], "vcvtdq2ps xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_000, 0x5b, 0b00_001_010], "vcvtdq2ps xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x5b, 0b11_001_010], "vcvtdq2ps ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_100, 0x5b, 0b00_001_010], "vcvtdq2ps ymm1, ymmword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_000, 0x5c, 0b00_001_010], "vsubps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_100, 0x5c, 0b00_001_010], "vsubps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_010, 0x5c, 0b00_001_010], "vsubss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_110, 0x5c, 0b00_001_010], "vsubss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x5c, 0b00_001_010], "vsubpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x5c, 0b00_001_010], "vsubpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_011, 0x5c, 0b00_001_010], "vsubsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_111, 0x5c, 0b00_001_010], "vsubsd xmm1, xmm0, qword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x5d, 0b00_001_010], "vminps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x5d, 0b00_001_010], "vminps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_010, 0x5d, 0b00_001_010], "vminss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_110, 0x5d, 0b00_001_010], "vminss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x5d, 0b00_001_010], "vminpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x5d, 0b00_001_010], "vminpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x5d, 0b00_001_010], "vminsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x5d, 0b00_001_010], "vminsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x5e, 0b00_001_010], "vdivps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x5e, 0b00_001_010], "vdivps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x5e, 0b00_001_010], "vdivpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_010, 0x5e, 0b00_001_010], "vdivss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x5e, 0b00_001_010], "vdivsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x5e, 0b00_001_010], "vdivps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x5e, 0b00_001_010], "vdivpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_110, 0x5e, 0b00_001_010], "vdivss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x5e, 0b00_001_010], "vdivsd xmm1, xmm0, qword [edx]"),

        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0x5f, 0b00_001_010], "vmaxps xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x5f, 0b00_001_010], "vmaxpd xmm1, xmm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_010, 0x5f, 0b00_001_010], "vmaxss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x5f, 0b00_001_010], "vmaxsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0x5f, 0b00_001_010], "vmaxps ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x5f, 0b00_001_010], "vmaxpd ymm1, ymm0, ymmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_110, 0x5f, 0b00_001_010], "vmaxss xmm1, xmm0, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x5f, 0b00_001_010], "vmaxsd xmm1, xmm0, qword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x60, 0b11_001_010], "vpunpcklbw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x60, 0b11_001_010], "vpunpcklbw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x61, 0b11_001_010], "vpunpcklwd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x61, 0b11_001_010], "vpunpcklwd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x62, 0b11_001_010], "vpunpckldq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x62, 0b11_001_010], "vpunpckldq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x63, 0b11_001_010], "vpacksswb xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x63, 0b11_001_010], "vpacksswb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x64, 0b11_001_010], "vpcmpgtb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x64, 0b11_001_010], "vpcmpgtb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x65, 0b11_001_010], "vpcmpgtw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x65, 0b11_001_010], "vpcmpgtw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x66, 0b11_001_010], "vpcmpgtd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x66, 0b11_001_010], "vpcmpgtd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x67, 0b11_001_010], "vpackuswb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x67, 0b11_001_010], "vpackuswb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x68, 0b11_001_010], "vpunpckhbw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x68, 0b11_001_010], "vpunpckhbw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x69, 0b11_001_010], "vpunpckhwd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x69, 0b11_001_010], "vpunpckhwd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x6a, 0b11_001_010], "vpunpckhdq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x6a, 0b11_001_010], "vpunpckhdq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0x6b, 0b11_001_010], "vpackssdw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0x6b, 0b11_001_010], "vpackssdw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x6c, 0b11_001_010], "vpunpcklqdq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x6c, 0b11_001_010], "vpunpcklqdq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x6d, 0b11_001_010], "vpunpckhqdq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x6d, 0b11_001_010], "vpunpckhqdq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x6e, 0b11_001_010], "vmovd xmm1, edx"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x6e, 0b00_001_010], "vmovd xmm1, dword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0xd6, 0b00_001_010], "vmovq qword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0xd6, 0b11_001_010], "vmovq xmm2, xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0x6e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x6f, 0b11_001_010], "vmovdqa xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x6f, 0b11_001_010], "vmovdqa ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_010, 0x6f, 0b11_001_010], "vmovdqu xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x6f, 0b11_001_010], "vmovdqu ymm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x70, 0b11_001_010, 0x77], "vpshufd xmm1, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x70, 0b11_001_010, 0x77], "vpshufd ymm1, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0x70, 0b11_001_010, 0x77], "vpshufhw xmm1, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0x70, 0b11_001_010, 0x77], "vpshufhw ymm1, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0x70, 0b11_001_010, 0x77], "vpshuflw xmm1, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0x70, 0b11_001_010, 0x77], "vpshuflw ymm1, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_010_010, 0x77], "vpsrlw xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x71, 0b11_010_010, 0x77], "vpsrlw xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x71, 0b11_010_010, 0x77], "vpsrlw ymm0, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b00_011_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_100_010, 0x77], "vpsraw xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x71, 0b11_100_010, 0x77], "vpsraw ymm0, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_101_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_110_010, 0x77], "vpsllw xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x71, 0b11_110_010, 0x77], "vpsllw ymm0, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_000_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b11_010_010, 0x77], "vpsrld xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_010_010, 0x77], "vpsrld ymm0, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_011_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b11_100_010, 0x77], "vpsrad xmm0, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_100_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_100_010, 0x77], "vpsrad ymm0, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_101_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b11_110_010, 0x77], "vpslld xmm0, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_110_010, 0x77]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_110_010, 0x77], "vpslld ymm0, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_111_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_000_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_010_010, 0x77], "vpsrlq xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_011_010, 0x77], "vpsrldq xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_010_010, 0x77], "vpsrlq ymm0, ymm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_011_010, 0x77], "vpsrldq ymm0, ymm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_100_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_101_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_110_010, 0x77], "vpsllq xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_111_010, 0x77], "vpslldq xmm0, xmm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_110_010, 0x77], "vpsllq ymm0, ymm2, 0x77"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_111_010, 0x77], "vpslldq ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x74, 0b11_001_010], "vpcmpeqb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x74, 0b11_001_010], "vpcmpeqb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x75, 0b11_001_010], "vpcmpeqw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x75, 0b11_001_010], "vpcmpeqw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x76, 0b11_001_010], "vpcmpeqd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x76, 0b11_001_010], "vpcmpeqd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x7c, 0b11_001_010], "vhaddpd xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x7c, 0b11_001_010], "vhaddpd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x7c, 0b11_001_010], "vhaddps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x7c, 0b11_001_010], "vhaddps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0x7d, 0b11_001_010], "vhsubpd xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0x7d, 0b11_001_010], "vhsubpd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0x7d, 0b11_001_010], "vhsubps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0x7d, 0b11_001_010], "vhsubps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x7e, 0b11_001_010], "vmovd edx, xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_101, 0x7e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0x7e, 0b11_001_010], "vmovd edx, xmm1"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0x7e, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0x7f, 0b11_001_010], "vmovdqa xmm2, xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0x7f, 0b11_001_010], "vmovdqa ymm2, ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_010, 0x7f, 0b11_001_010], "vmovdqu xmm2, xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_110, 0x7f, 0b11_001_010], "vmovdqu ymm2, ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b00_010_001], "vldmxcsr dword [ecx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_100, 0xae, 0b00_010_001]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b11_010_001]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b00_011_001], "vstmxcsr dword [ecx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_100, 0xae, 0b00_011_001]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b11_011_001]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_000, 0xc2, 0b11_001_010, 0x77], "vcmpps xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_100, 0xc2, 0b11_001_010, 0x77], "vcmpps ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xc2, 0b11_001_010, 0x77], "vcmppd xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xc2, 0b11_001_010, 0x77], "vcmppd ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0xc2, 0b11_001_010, 0x77], "vcmpsd xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0xc2, 0b11_001_010, 0x77], "vcmpsd xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xc4, 0b11_001_010, 0x77], "vpinsrw xmm1, xmm0, edx, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0xc4, 0b11_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_001, 0xc5, 0b00_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0xc5, 0b11_001_010, 0x77], "vpextrw ecx, xmm2, 0x77"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0xc5, 0b00_001_010, 0x77]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0xc5, 0b11_001_010, 0x77]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xc6, 0b11_001_010, 0x77], "vshufpd xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xc6, 0b11_001_010, 0x77], "vshufpd ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_000, 0xc6, 0b11_001_010, 0x77], "vshufps xmm1, xmm0, xmm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_100, 0xc6, 0b11_001_010, 0x77], "vshufps ymm1, ymm0, ymm2, 0x77"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xd0, 0b11_001_010], "vaddsubpd xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd0, 0b11_001_010], "vaddsubpd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_011, 0xd0, 0b11_001_010], "vaddsubps xmm1, xmm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_111, 0xd0, 0b11_001_010], "vaddsubps ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xd1, 0b11_001_010], "vpsrlw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd1, 0b11_001_010], "vpsrlw ymm1, ymm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd1, 0b00_001_010], "vpsrlw ymm1, ymm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xd2, 0b11_001_010], "vpsrld xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd2, 0b11_001_010], "vpsrld ymm1, ymm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd2, 0b00_001_010], "vpsrld ymm1, ymm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xd3, 0b11_001_010], "vpsrlq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd3, 0b11_001_010], "vpsrlq ymm1, ymm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd3, 0b00_001_010], "vpsrlq ymm1, ymm0, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xd4, 0b11_001_010], "vpaddq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd4, 0b11_001_010], "vpaddq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xd5, 0b11_001_010], "vpmullw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xd5, 0b11_001_010], "vpmullw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_1111_001, 0xd7, 0b11_001_010], "vpmovmskb ecx, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b0_1111_001, 0xd7, 0b00_001_010]),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_1111_101, 0xd7, 0b11_001_010], "vpmovmskb ecx, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xd8, 0b11_001_010], "vpsubusb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd8, 0b11_001_010], "vpsubusb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xd9, 0b11_001_010], "vpsubusw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xd9, 0b11_001_010], "vpsubusw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xda, 0b11_001_010], "vpminub xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xda, 0b11_001_010], "vpminub ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xdb, 0b11_001_010], "vpand xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xdb, 0b11_001_010], "vpand ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xdc, 0b11_001_010], "vpaddusb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xdc, 0b11_001_010], "vpaddusb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xdd, 0b11_001_010], "vpaddusw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xdd, 0b11_001_010], "vpaddusw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xde, 0b11_001_010], "vpmaxub xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xde, 0b11_001_010], "vpmaxub ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xdf, 0b11_001_010], "vpandn xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xdf, 0b11_001_010], "vpandn ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe0, 0b11_001_010], "vpavgb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe0, 0b11_001_010], "vpavgb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe1, 0b11_001_010], "vpsraw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe1, 0b11_001_010], "vpsraw ymm1, ymm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe2, 0b11_001_010], "vpsrad xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe2, 0b11_001_010], "vpsrad ymm1, ymm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe3, 0b11_001_010], "vpavgw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe3, 0b11_001_010], "vpavgw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe4, 0b11_001_010], "vpmulhuw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe4, 0b11_001_010], "vpmulhuw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe5, 0b11_001_010], "vpmulhw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe5, 0b11_001_010], "vpmulhw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0xe6, 0b11_001_010], "vcvttpd2dq xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0xe6, 0b11_001_010], "vcvttpd2dq xmm1, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_010, 0xe6, 0b11_001_010], "vcvtdq2pd xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_110, 0xe6, 0b11_001_010], "vcvtdq2pd ymm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0xe6, 0b11_001_010], "vcvtpd2dq xmm1, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0xe6, 0b11_001_010], "vcvtpd2dq xmm1, ymm2"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0xe7, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0xe7, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0xe7, 0b00_001_010], "vmovntdq xmmword [edx], xmm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0xe7, 0b00_001_010], "vmovntdq ymmword [edx], ymm1"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe8, 0b11_001_010], "vpsubsb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe8, 0b11_001_010], "vpsubsb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xe9, 0b11_001_010], "vpsubsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xe9, 0b11_001_010], "vpsubsw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xea, 0b11_001_010], "vpminsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xea, 0b11_001_010], "vpminsw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xeb, 0b11_001_010], "vpor xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xeb, 0b11_001_010], "vpor ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xec, 0b11_001_010], "vpaddsb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xec, 0b11_001_010], "vpaddsb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xed, 0b11_001_010], "vpaddsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xed, 0b11_001_010], "vpaddsw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xee, 0b11_001_010], "vpmaxsw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xee, 0b11_001_010], "vpmaxsw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b0_0111_001, 0xef, 0b11_001_010], "vpxor xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b0_0111_101, 0xef, 0b11_001_010], "vpxor ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_011, 0xf0, 0b00_001_010], "vlddqu xmm1, xmmword [edx]"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_111, 0xf0, 0b00_001_010], "vlddqu ymm1, ymmword [edx]"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_011, 0xf0, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_011, 0xf0, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_0111_111, 0xf0, 0b11_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_111, 0xf0, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xf1, 0b11_001_010], "vpsllw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xf1, 0b11_001_010], "vpsllw ymm1, ymm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xf2, 0b11_001_010], "vpslld xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xf2, 0b11_001_010], "vpslld ymm1, ymm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xf3, 0b11_001_010], "vpsllq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xf3, 0b11_001_010], "vpsllq ymm1, ymm0, xmm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xf4, 0b11_001_010], "vpmuludq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xf4, 0b11_001_010], "vpmuludq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0xf5, 0b11_001_010], "vpmaddwd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0xf5, 0b11_001_010], "vpmaddwd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0xf6, 0b11_001_010], "vpsadbw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_1111_101, 0xf6, 0b11_001_010], "vpsadbw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_1111_001, 0xf7, 0b11_001_010], "vmaskmovdqu xmm1, xmm2"),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_001, 0xf7, 0b00_001_010]),
        testcase!(invalid: &[0xc4, 0b110_00001, 0b1_1111_101, 0xf7, 0b11_001_010]),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xf8, 0b11_001_010], "vpsubb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xf8, 0b11_001_010], "vpsubb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xf9, 0b11_001_010], "vpsubw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xf9, 0b11_001_010], "vpsubw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xfa, 0b11_001_010], "vpsubd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xfa, 0b11_001_010], "vpsubd ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xfb, 0b11_001_010], "vpsubq xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xfb, 0b11_001_010], "vpsubq ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xfc, 0b11_001_010], "vpaddb xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xfc, 0b11_001_010], "vpaddb ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xfd, 0b11_001_010], "vpaddw xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xfd, 0b11_001_010], "vpaddw ymm1, ymm0, ymm2"),
        testcase!(features { AVX: true } &[0xc4, 0b110_00001, 0b1_0111_001, 0xfe, 0b11_001_010], "vpaddd xmm1, xmm0, xmm2"),
        testcase!(features { AVX2: true } &[0xc4, 0b110_00001, 0b1_0111_101, 0xfe, 0b11_001_010], "vpaddd ymm1, ymm0, ymm2"),

        testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0x00], "vmovups xmm0, xmmword [eax]"),
        testcase!(features { AVX: true } &[0xc5, 0xf8, 0x10, 0x01], "vmovups xmm0, xmmword [ecx]"),
    ];

        #[test]
        fn test() {
            run_test(CASES);
        }
}

mod strange_prefixing {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x0f, 0x21, 0xc8], "mov eax, dr1"),
        testcase!(&[0xf2, 0x0f, 0x21, 0xc8], "mov eax, dr1"),
        testcase!(&[0xf3, 0x0f, 0x21, 0xc8], "mov eax, dr1"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod prefixed_0f {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x02, 0x01], "lar eax, word [ecx]"),
        testcase!(&[0x0f, 0x02, 0xc1], "lar eax, ecx"),
        testcase!(&[0x66, 0x0f, 0x02, 0x01], "lar ax, word [ecx]"),
        testcase!(&[0x66, 0x0f, 0x02, 0xc1], "lar ax, cx"),
        testcase!(&[0x0f, 0x03, 0x01], "lsl eax, word [ecx]"),
        testcase!(&[0x0f, 0x03, 0xc1], "lsl eax, ecx"),
        testcase!(&[0x66, 0x0f, 0x03, 0x01], "lsl ax, word [ecx]"),
        testcase!(&[0x66, 0x0f, 0x03, 0xc1], "lsl ax, cx"),
        testcase!(&[0x0f, 0x05], "syscall"),
        testcase!(&[0x66, 0x0f, 0x05], "syscall"),
        testcase!(&[0x0f, 0x06], "clts"),
        testcase!(&[0xf2, 0x0f, 0x06], "clts", masm: "clts"),
        testcase!(&[0x0f, 0x07], "sysret"),
        testcase!(&[0xf2, 0x0f, 0x07], "sysret", masm: "sysret"),
        testcase!(&[0x0f, 0x12, 0x0f], "movlps xmm1, qword [edi]"),
        testcase!(&[0x0f, 0x12, 0xcf], "movhlps xmm1, xmm7"),
        testcase!(&[0x0f, 0x16, 0x0f], "movhps xmm1, qword [edi]"),
        testcase!(&[0x0f, 0x16, 0xcf], "movlhps xmm1, xmm7"),
        testcase!(&[0x0f, 0x12, 0xc0], "movhlps xmm0, xmm0"),
        testcase!(invalid: &[0x0f, 0x13, 0xc0]),
        testcase!(&[0x0f, 0x13, 0x00], "movlps qword [eax], xmm0"),
        testcase!(&[0x0f, 0x14, 0x08], "unpcklps xmm1, xmmword [eax]"),
        testcase!(&[0x0f, 0x15, 0x08], "unpckhps xmm1, xmmword [eax]"),
        testcase!(&[0x0f, 0x16, 0x0f], "movhps xmm1, qword [edi]"),
        testcase!(&[0x0f, 0x16, 0xc0], "movlhps xmm0, xmm0"),
        testcase!(invalid: &[0x0f, 0x17, 0xc0]),
        testcase!(&[0x0f, 0x17, 0x00], "movhps qword [eax], xmm0"),
        testcase!(&[0x0f, 0x18, 0xc0], "nop eax"), // capstone says invalid, xed says nop
        testcase!(&[0x0f, 0x18, 0x00], "prefetchnta zmmword [eax]", masm: "prefetchnta [eax]"),
        testcase!(&[0x0f, 0x18, 0x08], "prefetcht0 zmmword [eax]", masm: "prefetcht0 [eax]"),
        testcase!(&[0x0f, 0x18, 0x10], "prefetcht1 zmmword [eax]", masm: "prefetcht1 [eax]"),
        testcase!(&[0x0f, 0x18, 0x18], "prefetcht2 zmmword [eax]", masm: "prefetcht2 [eax]"),
        testcase!(&[0x0f, 0x18, 0x20], "nop zmmword [eax]"),
        testcase!(&[0x0f, 0x18, 0xcc], "nop esp"),
        testcase!(&[0x0f, 0x19, 0x20], "nop dword [eax]"),
        testcase!(&[0x0f, 0x1a, 0x20], "nop dword [eax]"),
        testcase!(&[0x0f, 0x1b, 0x20], "nop dword [eax]"),
        testcase!(&[0x0f, 0x1c, 0x20], "nop dword [eax]"),
        testcase!(&[0x0f, 0x1d, 0x20], "nop dword [eax]"),
        testcase!(&[0x0f, 0x1e, 0x20], "nop dword [eax]"),
        testcase!(&[0x0f, 0x1f, 0x20], "nop dword [eax]"),
        testcase!(&[0x0f, 0x20, 0xc0], "mov eax, cr0"),
        testcase!(invalid: &[0x0f, 0x20, 0xc8]),
        testcase!(&[0x0f, 0x21, 0xc8], "mov eax, dr1"),
        testcase!(&[0x0f, 0x22, 0xc0], "mov cr0, eax"),
        testcase!(invalid: &[0x0f, 0x22, 0xc8]),
        testcase!(&[0x0f, 0x22, 0xc7], "mov cr0, edi"),
        testcase!(&[0x0f, 0x23, 0xc8], "mov dr1, eax"),
        testcase!(&[0x0f, 0x23, 0xcf], "mov dr1, edi"),
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
        testcase!(&[0x0f, 0x60, 0x00], "punpcklbw mm0, dword [eax]"),
        testcase!(&[0x0f, 0x60, 0xc2], "punpcklbw mm0, mm2"),
        testcase!(&[0x0f, 0x61, 0x00], "punpcklwd mm0, dword [eax]"),
        testcase!(&[0x0f, 0x61, 0xc2], "punpcklwd mm0, mm2"),
        testcase!(&[0x0f, 0x62, 0x00], "punpckldq mm0, dword [eax]"),
        testcase!(&[0x0f, 0x62, 0xc2], "punpckldq mm0, mm2"),
        testcase!(&[0x0f, 0x63, 0x00], "packsswb mm0, qword [eax]"),
        testcase!(&[0x0f, 0x63, 0xc2], "packsswb mm0, mm2"),
        testcase!(&[0x0f, 0x64, 0x00], "pcmpgtb mm0, qword [eax]"),
        testcase!(&[0x0f, 0x64, 0xc2], "pcmpgtb mm0, mm2"),
        testcase!(&[0x0f, 0x65, 0x00], "pcmpgtw mm0, qword [eax]"),
        testcase!(&[0x0f, 0x65, 0xc2], "pcmpgtw mm0, mm2"),
        testcase!(&[0x0f, 0x66, 0x00], "pcmpgtd mm0, qword [eax]"),
        testcase!(&[0x0f, 0x66, 0xc2], "pcmpgtd mm0, mm2"),
        testcase!(&[0x0f, 0x67, 0x00], "packuswb mm0, qword [eax]"),
        testcase!(&[0x0f, 0x67, 0xc2], "packuswb mm0, mm2"),
        testcase!(&[0x0f, 0x68, 0x00], "punpckhbw mm0, qword [eax]"),
        testcase!(&[0x0f, 0x68, 0xc2], "punpckhbw mm0, mm2"),
        testcase!(&[0x0f, 0x69, 0x00], "punpckhwd mm0, qword [eax]"),
        testcase!(&[0x0f, 0x69, 0xc2], "punpckhwd mm0, mm2"),
        testcase!(&[0x0f, 0x6a, 0x00], "punpckhdq mm0, qword [eax]"),
        testcase!(&[0x0f, 0x6a, 0xc2], "punpckhdq mm0, mm2"),
        testcase!(&[0x0f, 0x6b, 0x00], "packssdw mm0, qword [eax]"),
        testcase!(&[0x0f, 0x6b, 0xc2], "packssdw mm0, mm2"),
        testcase!(invalid: &[0x0f, 0x6c]),
        testcase!(invalid: &[0x0f, 0x6d]),
        testcase!(&[0x0f, 0x6e, 0x00], "movd mm0, dword [eax]"),
        testcase!(&[0x0f, 0x6e, 0xc2], "movd mm0, edx"),
        testcase!(&[0x0f, 0x6f, 0x00], "movq mm0, qword [eax]"),
        testcase!(&[0x0f, 0x6f, 0xc2], "movq mm0, mm2"),
        testcase!(&[0x0f, 0x6f, 0xfb], "movq mm7, mm3"),
        testcase!(&[0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [eax], 0x7f"),
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
        testcase!(&[0x0f, 0xa5, 0xc0], "shld eax, eax, cl"),
        testcase!(&[0x0f, 0xa5, 0xc9], "shld ecx, ecx, cl"),
        testcase!(&[0x0f, 0xac, 0xc0, 0x11], "shrd eax, eax, 0x11"),
        testcase!(&[0x66, 0x0f, 0xac, 0xcf, 0x11], "shrd di, cx, 0x11"),
        testcase!(&[0x0f, 0xad, 0xc9], "shrd ecx, ecx, cl"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod prefixed_660f {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x0f, 0x10, 0xc0], "movupd xmm0, xmm0"),
        testcase!(&[0xf2, 0x66, 0x66, 0x0f, 0x10, 0xc0], "movsd xmm0, xmm0"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod prefixed_f20f {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(invalid: &[0xf2, 0x0f, 0x16, 0xcf]),
        testcase!(invalid: &[0x66, 0xf2, 0x66, 0x0f, 0x16, 0xcf]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod prefixed_f30f {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x16, 0xcf], "movshdup xmm1, xmm7"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0xfa], "endbr64"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0xfb], "endbr32"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0xfc], "nop esp, edi"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod only_32bit {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66], "callf 0x6655:0x44332211"),
        testcase!(&[0x66, 0x9a, 0x11, 0x22, 0x33, 0x44], "callf 0x4433:0x2211"),
        testcase!(&[0x67, 0xac], "lods al, byte ds:[si]"),
        testcase!(&[0x67, 0xae], "scas byte es:[di], al"),
        testcase!(&[0xac], "lods al, byte ds:[esi]"),
        testcase!(&[0xae], "scas byte es:[edi], al"),
        testcase!(&[0x67, 0xf3, 0xa4], "rep movs byte es:[di], byte ds:[si]"),
        testcase!(&[0xf3, 0xa4], "rep movs byte es:[edi], byte ds:[esi]"),
        testcase!(&[0x67, 0xf3, 0xa5], "rep movs dword es:[di], dword ds:[si]"),
        testcase!(&[0xf3, 0xa5], "rep movs dword es:[edi], dword ds:[esi]"),
        testcase!(&[0x66, 0x67, 0x8b, 0x0e, 0x55, 0xaa], "mov cx, word [0xaa55]"),
        testcase!(&[0x66, 0x8b, 0x0e], "mov cx, word [esi]"),
        testcase!(&[0x40], "inc eax"),
        testcase!(&[0x41], "inc ecx"),
        testcase!(&[0x47], "inc edi"),
        testcase!(&[0x48], "dec eax"),
        testcase!(&[0x4f], "dec edi"),

        testcase!(&[0xa0, 0xc0, 0xb0, 0xa0, 0x90], "mov al, byte [0x90a0b0c0]"),
        testcase!(&[0x67, 0xa0, 0xc0, 0xb0], "mov al, byte [0xb0c0]"),
        testcase!(&[0x67, 0xa1, 0xc0, 0xb0], "mov eax, dword [0xb0c0]"),
        testcase!(&[0x66, 0x67, 0xa1, 0xc0, 0xb0], "mov ax, word [0xb0c0]"),

        testcase!(&[0x60], "pushad"),
        testcase!(&[0x61], "popad"),
        testcase!(&[0x66, 0x60], "pusha"),
        testcase!(&[0x66, 0x61], "popa"),
        testcase!(&[0xce], "into"),
        testcase!(&[0x06], "push es"),
        testcase!(&[0x07], "pop es"),
        testcase!(&[0x0e], "push cs"),
        testcase!(&[0x16], "push ss"),
        testcase!(&[0x17], "pop ss"),
        testcase!(&[0x1e], "push ds"),
        testcase!(&[0x1f], "pop ds"),
        testcase!(&[0x27], "daa"),
        testcase!(&[0x2f], "das"),
        testcase!(&[0x37], "aaa"),
        testcase!(&[0x3f], "aas"),
        testcase!(&[0xd4, 0x01], "aam 0x1", masm: "aam 1"),
        testcase!(&[0xd4, 0x0a], "aam 0xa", masm: "aam"),
        testcase!(&[0xd5, 0x01], "aad 0x1", masm: "aad 1"),
        testcase!(&[0xd5, 0x0a], "aad 0xa", masm: "aad"),

        testcase!(&[0xc5, 0x78, 0x10], "lds edi, far [eax + 0x10]"),
        testcase!(&[0x66, 0xc5, 0x78, 0x10], "lds di, dword [eax + 0x10]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod adx {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x0f, 0x38, 0xf6, 0xc1], "adcx eax, ecx"),
        testcase!(&[0x66, 0x0f, 0x38, 0xf6, 0x01], "adcx eax, dword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xf6, 0xc1], "adox eax, ecx"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xf6, 0x01], "adox eax, dword [ecx]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod prefetchw {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x0d, 0x08], "prefetchw zmmword [eax]"),
        testcase!(&[0x0f, 0x0d, 0x00], "nop zmmword [eax]"),
        testcase!(invalid: &[0x0f, 0x0d, 0xc0]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod lzcnt {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0xf3, 0x0f, 0xbd, 0xc1], "lzcnt ax, cx"),
        testcase!(&[0xf3, 0x0f, 0xbd, 0xc1], "lzcnt eax, ecx"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod svm {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x01, 0xdf], "invlpga eax, ecx"),
        testcase!(&[0x0f, 0x01, 0xde], "skinit eax"),
        testcase!(&[0x0f, 0x01, 0xdd], "clgi"),
        testcase!(&[0x0f, 0x01, 0xdc], "stgi"),
        testcase!(&[0x0f, 0x01, 0xdb], "vmsave eax"),
        testcase!(&[0x0f, 0x01, 0xda], "vmload eax"),
        testcase!(&[0x0f, 0x01, 0xd9], "vmmcall"),
        testcase!(&[0x0f, 0x01, 0xd8], "vmrun eax"),
        testcase!(&[0x0f, 0x78, 0xc4], "vmread esp, eax"),
        testcase!(&[0x0f, 0x79, 0xc5], "vmwrite eax, ebp"),
        testcase!(&[0x0f, 0x78, 0x0b], "vmread qword [ebx], ecx"),
        testcase!(invalid: &[0x66, 0x0f, 0x78, 0x03]),
        testcase!(&[0x0f, 0x79, 0x0b], "vmwrite ecx, qword [ebx]"),
        testcase!(invalid: &[0x66, 0x0f, 0x79, 0x03]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod movbe {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x38, 0xf0, 0x06], "movbe eax, dword [esi]"),
        testcase!(invalid: &[0x0f, 0x38, 0xf0, 0xc6]),
        testcase!(&[0x0f, 0x38, 0xf1, 0x06], "movbe dword [esi], eax"),
        testcase!(&[0x66, 0x0f, 0x38, 0xf1, 0x06], "movbe word [esi], ax"),
        testcase!(invalid: &[0x66, 0x0f, 0x38, 0xf1, 0xc6]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod tsx {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xc6, 0xf8, 0x10], "xabort 0x10"),
        testcase!(&[0xc7, 0xf8, 0x10, 0x12, 0x34, 0x56], "xbegin $+0x56341210"),
        testcase!(&[0x66, 0xc7, 0xf8, 0x10, 0x12], "xbegin $+0x1210"),
        testcase!(&[0x0f, 0x01, 0xd5], "xend"),
        testcase!(&[0x0f, 0x01, 0xd6], "xtest"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod rand {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0xc7, 0xfd], "rdseed ebp"),
        testcase!(&[0x66, 0x0f, 0xc7, 0xfd], "rdseed bp"),
        testcase!(&[0x0f, 0xc7, 0xf5], "rdrand ebp"),
        testcase!(&[0x66, 0x0f, 0xc7, 0xf5], "rdrand bp"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod sha {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0x3a, 0xcc, 0x12, 0x40], "sha1rnds4 xmm2, xmmword [edx], 0x40"),
        testcase!(&[0x0f, 0x3a, 0xcc, 0x12, 0xff], "sha1rnds4 xmm2, xmmword [edx], 0xff"),
        // with astonishing dismay: 66-prefixed sha1rnds4 is #UD only in 32-bit and 16-bit mode.
        testcase!(invalid: &[0x66, 0x0f, 0x3a, 0xcc, 0x12, 0xff]),
        testcase!(&[0x0f, 0x38, 0xc8, 0x12], "sha1nexte xmm2, xmmword [edx]"),
        testcase!(&[0x0f, 0x38, 0xc9, 0x12], "sha1msg1 xmm2, xmmword [edx]"),
        testcase!(&[0x0f, 0x38, 0xca, 0x12], "sha1msg2 xmm2, xmmword [edx]"),
        testcase!(&[0x0f, 0x38, 0xcb, 0x12], "sha256rnds2 xmm2, xmmword [edx]"),
        testcase!(&[0x0f, 0x38, 0xcc, 0x12], "sha256msg1 xmm2, xmmword [edx]"),
        testcase!(&[0x0f, 0x38, 0xcd, 0x12], "sha256msg2 xmm2, xmmword [edx]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod vmx {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0xc7, 0x3f], "vmptrst qword [edi]"),
        testcase!(&[0x0f, 0xc7, 0x37], "vmptrld qword [edi]"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0x37], "vmxon qword [edi]"),
        testcase!(&[0x66, 0x0f, 0xc7, 0xf7], "rdrand di"),
        testcase!(&[0x66, 0x0f, 0xc7, 0x37], "vmclear qword [edi]"),

        // this is actually vmx
        // testcase!(invalid: &[0x66, 0x0f, 0xc7, 0x03]),
        testcase!(&[0x66, 0x0f, 0xc7, 0x33], "vmclear qword [ebx]"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0x33], "vmxon qword [ebx]"),

        // these need vmx and invept features
        testcase!(features { VMX: true } &[0x66, 0x0f, 0x38, 0x80, 0x01], "invept eax, xmmword [ecx]"),
        testcase!(features { VMX: true } &[0x66, 0x0f, 0x38, 0x81, 0x01], "invvpid eax, xmmword [ecx]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod rdpid {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0xc7, 0xfd], "rdpid ebp"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod cmpxchg8b {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]"),
        testcase!(&[0xf2, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]"),
        testcase!(&[0x66, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod x87 {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
    //    testcase!(&[0xd8, 0x03], "fadd st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x03], "fadd st(0), dword [ebx]"),
    //    testcase!(&[0xd8, 0x0b], "fmul st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x0b], "fmul st(0), dword [ebx]"),
    //    testcase!(&[0xd8, 0x13], "fcom st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x13], "fcom st(0), dword [ebx]"),
    //    testcase!(&[0xd8, 0x1b], "fcomp st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x1b], "fcomp st(0), dword [ebx]"),
    //    testcase!(&[0xd8, 0x23], "fsub st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x23], "fsub st(0), dword [ebx]"),
    //    testcase!(&[0xd8, 0x2b], "fsubr st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x2b], "fsubr st(0), dword [ebx]"),
    //    testcase!(&[0xd8, 0x33], "fdiv st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x33], "fdiv st(0), dword [ebx]"),
    //    testcase!(&[0xd8, 0x3b], "fdivr st, dword ptr [ebx]"),
        testcase!(&[0xd8, 0x3b], "fdivr st(0), dword [ebx]"),
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
    //    testcase!(&[0xd9, 0x03], "fld st, dword ptr [ebx]"),
        testcase!(&[0xd9, 0x03], "fld st(0), dword [ebx]"),
        testcase!(invalid: &[0xd9, 0x08]),
        testcase!(invalid: &[0xd9, 0x09]),
        testcase!(invalid: &[0xd9, 0x0a]),
        testcase!(invalid: &[0xd9, 0x0b]),
        testcase!(invalid: &[0xd9, 0x0c]),
        testcase!(invalid: &[0xd9, 0x0d]),
        testcase!(invalid: &[0xd9, 0x0e]),
        testcase!(invalid: &[0xd9, 0x0f]),
    //    testcase!(&[0xd9, 0x13], "fst dword ptr [ebx], st"),
        testcase!(&[0xd9, 0x13], "fst dword [ebx], st(0)"),
    //    testcase!(&[0xd9, 0x1b], "fstp dword ptr [ebx], st"),
        testcase!(&[0xd9, 0x1b], "fstp dword [ebx], st(0)"),
    //    testcase!(&[0xd9, 0x23], "fldenv ptr [ebx]"),
        testcase!(&[0xd9, 0x23], "fldenv ptr [ebx]"),
    //    testcase!(&[0xd9, 0x2b], "fldcw word ptr [ebx]"),
        testcase!(&[0xd9, 0x2b], "fldcw word [ebx]"),
    //    testcase!(&[0xd9, 0x33], "fnstenv ptr [ebx]"),
        testcase!(&[0xd9, 0x33], "fnstenv ptr [ebx]"),
    //    testcase!(&[0xd9, 0x3b], "fnstcw word ptr [ebx]"),
        testcase!(&[0xd9, 0x3b], "fnstcw word [ebx]"),
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
    //    testcase!(&[0xda, 0x03], "fiadd st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x03], "fiadd st(0), dword [ebx]"),
    //    testcase!(&[0xda, 0x0b], "fimul st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x0b], "fimul st(0), dword [ebx]"),
    //    testcase!(&[0xda, 0x13], "ficom st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x13], "ficom st(0), dword [ebx]"),
    //    testcase!(&[0xda, 0x1b], "ficomp st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x1b], "ficomp st(0), dword [ebx]"),
    //    testcase!(&[0xda, 0x23], "fisub st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x23], "fisub st(0), dword [ebx]"),
    //    testcase!(&[0xda, 0x2b], "fisubr st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x2b], "fisubr st(0), dword [ebx]"),
    //    testcase!(&[0xda, 0x33], "fidiv st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x33], "fidiv st(0), dword [ebx]"),
    //    testcase!(&[0xda, 0x3b], "fidivr st, dword ptr [ebx]"),
        testcase!(&[0xda, 0x3b], "fidivr st(0), dword [ebx]"),
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
    //    testcase!(&[0xdb, 0x03], "fild st, dword ptr [ebx]"),
        testcase!(&[0xdb, 0x03], "fild st(0), dword [ebx]"),
    //    testcase!(&[0xdb, 0x0b], "fisttp dword ptr [ebx], st"),
        testcase!(&[0xdb, 0x0b], "fisttp dword [ebx], st(0)"),
    //    testcase!(&[0xdb, 0x13], "fist dword ptr [ebx], st"),
        testcase!(&[0xdb, 0x13], "fist dword [ebx], st(0)"),
    //    testcase!(&[0xdb, 0x1b], "fistp dword ptr [ebx], st"),
        testcase!(&[0xdb, 0x1b], "fistp dword [ebx], st(0)"),
        testcase!(invalid: &[0xdb, 0x20]),
        testcase!(invalid: &[0xdb, 0x21]),
        testcase!(invalid: &[0xdb, 0x22]),
        testcase!(invalid: &[0xdb, 0x23]),
        testcase!(invalid: &[0xdb, 0x24]),
        testcase!(invalid: &[0xdb, 0x25]),
        testcase!(invalid: &[0xdb, 0x26]),
        testcase!(invalid: &[0xdb, 0x27]),
    //    testcase!(&[0xdb, 0x2b], "fld st, ptr [ebx]"),
        testcase!(&[0xdb, 0x2b], "fld st(0), mword [ebx]"),
        testcase!(invalid: &[0xdb, 0x30]),
        testcase!(invalid: &[0xdb, 0x31]),
        testcase!(invalid: &[0xdb, 0x32]),
        testcase!(invalid: &[0xdb, 0x33]),
        testcase!(invalid: &[0xdb, 0x34]),
        testcase!(invalid: &[0xdb, 0x35]),
        testcase!(invalid: &[0xdb, 0x36]),
        testcase!(invalid: &[0xdb, 0x37]),
    //    testcase!(&[0xdb, 0x3b], "fstp ptr [ebx], st"),
        testcase!(&[0xdb, 0x3b], "fstp mword [ebx], st(0)"),
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
    //    testcase!(&[0xdc, 0x03], "fadd st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x03], "fadd st(0), qword [ebx]"),
    //    testcase!(&[0xdc, 0x0b], "fmul st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x0b], "fmul st(0), qword [ebx]"),
    //    testcase!(&[0xdc, 0x13], "fcom st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x13], "fcom st(0), qword [ebx]"),
    //    testcase!(&[0xdc, 0x1b], "fcomp st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x1b], "fcomp st(0), qword [ebx]"),
    //    testcase!(&[0xdc, 0x23], "fsub st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x23], "fsub st(0), qword [ebx]"),
    //    testcase!(&[0xdc, 0x2b], "fsubr st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x2b], "fsubr st(0), qword [ebx]"),
    //    testcase!(&[0xdc, 0x33], "fdiv st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x33], "fdiv st(0), qword [ebx]"),
    //    testcase!(&[0xdc, 0x3b], "fdivr st, qword ptr [ebx]"),
        testcase!(&[0xdc, 0x3b], "fdivr st(0), qword [ebx]"),
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
    //    testcase!(&[0xdd, 0x03], "fld st, qword ptr [ebx]"),
        testcase!(&[0xdd, 0x03], "fld st(0), qword [ebx]"),
    //    testcase!(&[0xdd, 0x0b], "fisttp qword ptr [ebx], st"),
        testcase!(&[0xdd, 0x0b], "fisttp qword [ebx], st(0)"),
    //    testcase!(&[0xdd, 0x13], "fst qword ptr [ebx], st"),
        testcase!(&[0xdd, 0x13], "fst qword [ebx], st(0)"),
    //    testcase!(&[0xdd, 0x1b], "fstp qword ptr [ebx], st"),
        testcase!(&[0xdd, 0x1b], "fstp qword [ebx], st(0)"),
    //    testcase!(&[0xdd, 0x23], "frstor ptr [ebx]"),
        testcase!(&[0xdd, 0x23], "frstor ptr [ebx]"),
        testcase!(invalid: &[0xdd, 0x28]),
        testcase!(invalid: &[0xdd, 0x29]),
        testcase!(invalid: &[0xdd, 0x2a]),
        testcase!(invalid: &[0xdd, 0x2b]),
        testcase!(invalid: &[0xdd, 0x2c]),
        testcase!(invalid: &[0xdd, 0x2d]),
        testcase!(invalid: &[0xdd, 0x2e]),
        testcase!(invalid: &[0xdd, 0x2f]),
    //    testcase!(&[0xdd, 0x33], "fnsave ptr [ebx]"),
        testcase!(&[0xdd, 0x33], "fnsave ptr [ebx]"),
    //    testcase!(&[0xdd, 0x3b], "fnstsw word ptr [ebx]"),
        testcase!(&[0xdd, 0x3b], "fnstsw word [ebx]"),
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
    //    testcase!(&[0xde, 0x03], "fiadd st, word ptr [ebx]"),
        testcase!(&[0xde, 0x03], "fiadd st(0), word [ebx]"),
    //    testcase!(&[0xde, 0x0b], "fimul st, word ptr [ebx]"),
        testcase!(&[0xde, 0x0b], "fimul st(0), word [ebx]"),
    //    testcase!(&[0xde, 0x13], "ficom st, word ptr [ebx]"),
        testcase!(&[0xde, 0x13], "ficom st(0), word [ebx]"),
    //    testcase!(&[0xde, 0x1b], "ficomp st, word ptr [ebx]"),
        testcase!(&[0xde, 0x1b], "ficomp st(0), word [ebx]"),
    //    testcase!(&[0xde, 0x23], "fisub st, word ptr [ebx]"),
        testcase!(&[0xde, 0x23], "fisub st(0), word [ebx]"),
    //    testcase!(&[0xde, 0x2b], "fisubr st, word ptr [ebx]"),
        testcase!(&[0xde, 0x2b], "fisubr st(0), word [ebx]"),
    //    testcase!(&[0xde, 0x33], "fidiv st, word ptr [ebx]"),
        testcase!(&[0xde, 0x33], "fidiv st(0), word [ebx]"),
    //    testcase!(&[0xde, 0x3b], "fidivr st, word ptr [ebx]"),
        testcase!(&[0xde, 0x3b], "fidivr st(0), word [ebx]"),
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
    //    testcase!(&[0xdf, 0x03], "fild st, word ptr [ebx]"),
        testcase!(&[0xdf, 0x03], "fild st(0), word [ebx]"),
    //    testcase!(&[0xdf, 0x0b], "fisttp word ptr [ebx], st"),
        testcase!(&[0xdf, 0x0b], "fisttp word [ebx], st(0)"),
    //    testcase!(&[0xdf, 0x13], "fist word ptr [ebx], st"),
        testcase!(&[0xdf, 0x13], "fist word [ebx], st(0)"),
    //    testcase!(&[0xdf, 0x1b], "fistp word ptr [ebx], st"),
        testcase!(&[0xdf, 0x1b], "fistp word [ebx], st(0)"),
    //    testcase!(&[0xdf, 0x23], "fbld st, ptr [ebx]"),
        testcase!(&[0xdf, 0x23], "fbld st(0), mword [ebx]"),
    //    testcase!(&[0xdf, 0x2b], "fild st, qword ptr [ebx]"),
        testcase!(&[0xdf, 0x2b], "fild st(0), qword [ebx]"),
    //    testcase!(&[0xdf, 0x33], "fbstp ptr [ebx], st"),
        testcase!(&[0xdf, 0x33], "fbstp mword [ebx], st(0)"),
    //    testcase!(&[0xdf, 0x3b], "fistp qword ptr [ebx], st"),
        testcase!(&[0xdf, 0x3b], "fistp qword [ebx], st(0)"),
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
        run_test(CASES);
    }
}

mod mishegos_finds {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(invalid: &[0xc5, 0x8c, 0x77]),
        testcase!(&[0x0f, 0xfc, 0xaf, 0x40, 0x38, 0x25, 0xbf], "paddb mm5, qword [edi - 0x40dac7c0]"),
        testcase!(invalid: &[0xf3, 0x67, 0x0f, 0x3a, 0xf0, 0xfb, 0xb4]),
        testcase!(&[0x65, 0x66, 0x0f, 0x01, 0xdc], "stgi"),
        testcase!(&[0x66, 0x0f, 0x01, 0xd8], "vmrun eax"),
        testcase!(invalid: &[0x2e, 0x2e, 0xf2, 0x36, 0x0f, 0xb2, 0xdb, 0x42, 0xd6, 0xa3, 0x16]),
        testcase!(&[0x65, 0x67, 0x65, 0x65, 0x0f, 0x0e], "femms"),
        testcase!(&[0x26, 0x66, 0x67, 0x0f, 0x38, 0xdf, 0xe4], "aesdeclast xmm4, xmm4"),
        testcase!(&[0x65, 0x66, 0x66, 0x64, 0x0f, 0x38, 0xdb, 0x0f], "aesimc xmm1, xmmword fs:[edi]"),
        testcase!(invalid: &[0xf3, 0xf2, 0x0f, 0xae, 0x8f, 0x54, 0x3c, 0x58, 0xb7]),
        /*
        testcase!(&[652e662e0f3814ff], "blendvps"),
        testcase!(&[66666565450f3acf2b4b], "gf2 "),
        */

        // might just be yax trying to do a f20f decode when it should not be f2
        // impossible instruction if operands could be read: lock is illegal here.
        // testcase!(&[f06565f2640f16], "???"),
    //    testcase!(&[0x0f, 0x38, 0xf6, 0x8c, 0x98, 0x4d, 0x33, 0xf5, 0xd3, ], "wrssd"),
        testcase!(&[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c], "pi2fw mm6, qword ss:[eax - 0x5]"),
        testcase!(&[0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]"),
        testcase!(&[0x66, 0x3e, 0x26, 0x2e, 0x2e, 0x0f, 0x38, 0x2a, 0x2b], "movntdqa xmm5, xmmword cs:[ebx]"),
        testcase!(&[0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c], "blendpd xmm7, xmmword cs:[bx + si * 1 + 0x2ff0], 0x7c"),
        testcase!(&[0x66, 0x66, 0x64, 0x3e, 0x0f, 0x38, 0x23, 0x9d, 0x69, 0x0f, 0xa8, 0x2d], "pmovsxwd xmm3, qword [ebp + 0x2da80f69]"),
        testcase!(&[0x2e, 0x66, 0x26, 0x64, 0x0f, 0x3a, 0x21, 0x0b, 0xb1], "insertps xmm1, dword fs:[ebx], -0x4f"),
        testcase!(&[0x66, 0x26, 0x0f, 0x3a, 0x42, 0x96, 0x74, 0x29, 0x96, 0xf9, 0x6a], "mpsadbw xmm2, xmmword es:[esi - 0x669d68c], 0x6a"),
        testcase!(&[0x67, 0x26, 0x66, 0x65, 0x0f, 0x38, 0x3f, 0x9d, 0xcc, 0x03], "pmaxud xmm3, xmmword gs:[di + 0x3cc]"),
        testcase!(&[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e], "movdiri dword cs:[ebp + 0x3e], edx"),
        testcase!(invalid: &[0x66, 0x2e, 0x64, 0x66, 0x0f, 0x38, 0xf8, 0xe2]),
        testcase!(&[0x67, 0x66, 0x65, 0x3e, 0x0f, 0x6d, 0xd1], "punpckhqdq xmm2, xmm1"),
        testcase!(&[0x2e, 0x66, 0x0f, 0x3a, 0x0d, 0x40, 0x2d, 0x57], "blendpd xmm0, xmmword cs:[eax + 0x2d], 0x57"),
        testcase!(&[0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f], "lddqu xmm4, xmmword es:[bx + si * 1 + 0x5f1b]"),
        testcase!(&[0x2e, 0x3e, 0x66, 0x3e, 0x0f, 0x3a, 0x41, 0x30, 0x48], "dppd xmm6, xmmword [eax], 0x48"),

        testcase!(&[0x2e, 0x36, 0x0f, 0x18, 0xe7], "nop edi"),
        testcase!(&[0x65, 0xf0, 0x87, 0x0f], "lock xchg dword gs:[edi], ecx"),
        testcase!(&[0x66, 0x0f, 0x3a, 0x44, 0x88, 0xb3, 0xad, 0x26, 0x35, 0x75], "pclmulqdq xmm1, xmmword [eax + 0x3526adb3], 0x75"),
        testcase!(&[0x0f, 0xff, 0x6b, 0xac], "ud0 ebp, dword [ebx - 0x54]"),

        testcase!(&[0xf2, 0xf2, 0x2e, 0x36, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f], "enqcmd eax, zmmword ss:[ebx + 0x3f9d1c09]"),
        testcase!(&[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54], "enqcmds esi, zmmword fs:[edx + 0x54]"),
        testcase!(invalid: &[0xf3, 0x0f, 0x38, 0xf8, 0xf3]),

        testcase!(&[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8], "loadiwkey xmm5, xmm0"),

        testcase!(invalid: &[0xf3, 0x2e, 0x0f, 0x6a, 0x18]),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod cet {
    use crate::protected_mode::{TestCase, run_test};

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
        // testcase!(&[0x0f, 0x38, 0xf6, 0x8c, 0x98, 0x4d, 0x33, 0xf5, 0xd3, ], "wrssd [eax + ebx * 4 - 0x2c0accb3], ecx"),
        // setssbsy f3 0f 01 e8
        // clrssbsy f3 0f ae /6
        // endbr64 f3 0f ae fa
        // endbr32 f3 0f ae fb
        testcase!(&[0xf3, 0x0f, 0xae, 0xe9], "incssp ecx"),
        testcase!(&[0x3e, 0x0f, 0x38, 0xf6, 0x23], "wrss dword [ebx], esp"),
        testcase!(&[0x66, 0x0f, 0x38, 0xf5, 0x47, 0xe9], "wruss dword [edi - 0x17], eax"),
        testcase!(invalid: &[0x0f, 0x38, 0xf5, 0x47, 0xe9]),
        testcase!(invalid: &[0x66, 0x3e, 0x65, 0x3e, 0x0f, 0x38, 0xf5, 0xf0]),
        testcase!(&[0xf3, 0x0f, 0x01, 0xe8], "setssbsy"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xea], "saveprevssp"),
        testcase!(&[0x66, 0xf3, 0x0f, 0x01, 0xe8], "setssbsy"),
        testcase!(&[0x66, 0xf3, 0x0f, 0x01, 0xea], "saveprevssp"),
        testcase!(&[0xf3, 0x66, 0x0f, 0x01, 0xe8], "setssbsy"),
        testcase!(&[0xf3, 0x66, 0x0f, 0x01, 0xea], "saveprevssp"),
        testcase!(&[0xf3, 0x0f, 0x01, 0x29], "rstorssp qword [ecx]"),
        testcase!(&[0xf3, 0x66, 0x0f, 0x01, 0x29], "rstorssp qword [ecx]"),
        testcase!(&[0xf3, 0x0f, 0xae, 0x30], "clrssbsy qword [eax]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod sse4a {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { SSE4a: true } &[0xf2, 0x0f, 0x2b, 0x06], "movntsd qword [esi], xmm0"),
        testcase!(invalid: &[0xf2, 0x0f, 0x2b, 0xc6]),
        testcase!(features { SSE4a: true } &[0xf3, 0x0f, 0x2b, 0x06], "movntss dword [esi], xmm0"),
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
            run_test(CASES);
        }
}

mod _3dnow {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0f, 0xe0, 0x8a], "pfnacc mm4, mm0"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0f, 0x38, 0x8e], "pfpnacc mm7, qword [eax]"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x65, 0x67, 0x65, 0x65, 0x0f, 0x0e], "femms"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x3e, 0xf3, 0x2e, 0xf2, 0x0f, 0x0f, 0x64, 0x93, 0x93, 0xa4], "pfmax mm4, qword cs:[ebx + edx * 4 - 0x6d]"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c], "pi2fw mm6, qword ss:[eax - 0x5]"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x66, 0x0f, 0x0f, 0xc6, 0xb7], "pmulhrw mm0, mm6"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0f, 0xc6, 0xb7], "pmulhrw mm0, mm6"),
        testcase!(features { DecoderK8: true, DecoderBulldozer: false, DecoderNetburst: false } &[0x0f, 0x0e], "femms"),
    ];

        #[test]
        fn test() {
            run_test(CASES);
        }
}

// first appeared in tremont
mod direct_stores {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e, ], "movdiri dword cs:[ebp + 0x3e], edx"),
        testcase!(&[0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08], "movdir64b bp, zmmword es:[di + 0x80b]"),
        testcase!(&[0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07], "movdir64b ebp, zmmword es:[ebp + 0x729080b]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod key_locker {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8], "loadiwkey xmm5, xmm0"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xfa, 0xde], "encodekey128 ebx, esi"),
        testcase!(&[0xf3, 0x0f, 0x38, 0xfb, 0xde], "encodekey256 ebx, esi"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

// these uinter test cases come from llvm:
// https://reviews.llvm.org/differential/changeset/?ref=2226860
mod uintr {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x01, 0xec], "uiret"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xed], "testui"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xee], "clui"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xef], "stui"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0xf0], "senduipi eax"),
        testcase!(&[0xf3, 0x0f, 0xc7, 0xf2], "senduipi edx"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

// started shipping in sapphire rapids
mod enqcmd {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf2, 0xf2, 0x2e, 0x36, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f], "enqcmd eax, zmmword ss:[ebx + 0x3f9d1c09]"),
        testcase!(&[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54], "enqcmds esi, zmmword fs:[edx + 0x54]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod gfni {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x3e, 0x64, 0x64, 0x66, 0x0f, 0x3a, 0xcf, 0xba, 0x13, 0x23, 0x04, 0xba, 0x6b], "gf2p8affineinvqb xmm7, xmmword fs:[edx - 0x45fbdced], 0x6b"),
        testcase!(&[0x66, 0x36, 0x0f, 0x3a, 0xce, 0x8c, 0x56, 0x9e, 0x82, 0xd1, 0xbe, 0xad], "gf2p8affineqb xmm1, xmmword ss:[esi + edx * 2 - 0x412e7d62], 0xad"),
        testcase!(&[0x66, 0x0f, 0x38, 0xcf, 0x1c, 0x54], "gf2p8mulb xmm3, xmmword [esp + edx * 2]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod tdx {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0x66, 0x0f, 0x01, 0xcc], "tdcall"),
        testcase!(&[0x66, 0x0f, 0x01, 0xcd], "seamret"),
        testcase!(&[0x66, 0x0f, 0x01, 0xce], "seamops"),
        testcase!(&[0x66, 0x0f, 0x01, 0xcf], "seamcall"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod tsxldtrk {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf2, 0x0f, 0x01, 0xe8], "xsusldtrk"),
        testcase!(&[0xf2, 0x0f, 0x01, 0xe9], "xresldtrk"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod sevsnp {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x01, 0xff], "psmash"),
        testcase!(&[0xf2, 0x0f, 0x01, 0xff], "pvalidate"),
        testcase!(&[0xf3, 0x0f, 0x01, 0xfe], "rmpadjust"),
        testcase!(&[0xf2, 0x0f, 0x01, 0xfe], "rmpupdate"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod keylocker {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x38, 0xdd, 0x03], "aesdec128kl xmm0, m384b [ebx]"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

// some test cases are best just lifted from llvm or gcc.
mod llvm {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        testcase!(&[0xf3, 0x0f, 0x3a, 0xf0, 0xc0, 0x01], "hreset 0x1", masm: "hreset 1, eax"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod from_reports {
    use crate::protected_mode::{TestCase, run_test};

    const CASES: &'static [TestCase] = &[
        // negative compressed evex displacements should not overflow and panic
        testcase!(&[0x62, 0xf2, 0x6d, 0xac, 0x00, 0x59, 0xa7], "vpshufb ymm3{k4}{z}, ymm2, ymmword [ecx - 0xb20]"),
        testcase!(&[0x62, 0xf2, 0xfd, 0x0f, 0x8a, 0x62, 0xf2], "vcompresspd xmmword [edx - 0x70]{k7}, xmm4"),
        testcase!(&[0xf3, 0x0f, 0x1e, 0x0f], "nop dword [edi], ecx"),
    ];

    #[test]
    fn test() {
        run_test(CASES);
    }
}

mod reg_specs {
    use yaxpeax_x86::protected_mode::RegSpec;

    #[test]
    fn reg_specs_are_correct() {
        #[cfg(feature = "fmt")]
        {
            let cases: Vec<(RegSpec, &'static str)> = vec![
                (RegSpec::d(0), "eax"), (RegSpec::eax(), "eax"),
                (RegSpec::d(1), "ecx"), (RegSpec::ecx(), "ecx"),
                (RegSpec::d(2), "edx"), (RegSpec::edx(), "edx"),
                (RegSpec::d(3), "ebx"), (RegSpec::ebx(), "ebx"),
                (RegSpec::d(4), "esp"), (RegSpec::esp(), "esp"),
                (RegSpec::d(5), "ebp"), (RegSpec::ebp(), "ebp"),
                (RegSpec::d(6), "esi"), (RegSpec::esi(), "esi"),
                (RegSpec::d(7), "edi"), (RegSpec::edi(), "edi"),

                (RegSpec::w(0), "ax"), (RegSpec::ax(), "ax"),
                (RegSpec::w(1), "cx"), (RegSpec::cx(), "cx"),
                (RegSpec::w(2), "dx"), (RegSpec::dx(), "dx"),
                (RegSpec::w(3), "bx"), (RegSpec::bx(), "bx"),
                (RegSpec::w(4), "sp"), (RegSpec::sp(), "sp"),
                (RegSpec::w(5), "bp"), (RegSpec::bp(), "bp"),
                (RegSpec::w(6), "si"), (RegSpec::si(), "si"),
                (RegSpec::w(7), "di"), (RegSpec::di(), "di"),

                (RegSpec::b(0), "al"), (RegSpec::al(), "al"),
                (RegSpec::b(1), "cl"), (RegSpec::cl(), "cl"),
                (RegSpec::b(2), "dl"), (RegSpec::dl(), "dl"),
                (RegSpec::b(3), "bl"), (RegSpec::bl(), "bl"),
                (RegSpec::b(4), "ah"), (RegSpec::ah(), "ah"),
                (RegSpec::b(5), "ch"), (RegSpec::ch(), "ch"),
                (RegSpec::b(6), "dh"), (RegSpec::dh(), "dh"),
                (RegSpec::b(7), "bh"), (RegSpec::bh(), "bh"),

                (RegSpec::eip(), "eip"),
                (RegSpec::eflags(), "eflags"),
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
            (RegSpec::d(0), RegSpec::eax()),
            (RegSpec::d(1), RegSpec::ecx()),
            (RegSpec::d(2), RegSpec::edx()),
            (RegSpec::d(3), RegSpec::ebx()),
            (RegSpec::d(4), RegSpec::esp()),
            (RegSpec::d(5), RegSpec::ebp()),
            (RegSpec::d(6), RegSpec::esi()),
            (RegSpec::d(7), RegSpec::edi()),

            (RegSpec::w(0), RegSpec::ax()),
            (RegSpec::w(1), RegSpec::cx()),
            (RegSpec::w(2), RegSpec::dx()),
            (RegSpec::w(3), RegSpec::bx()),
            (RegSpec::w(4), RegSpec::sp()),
            (RegSpec::w(5), RegSpec::bp()),
            (RegSpec::w(6), RegSpec::si()),
            (RegSpec::w(7), RegSpec::di()),

            (RegSpec::b(0), RegSpec::al()),
            (RegSpec::b(1), RegSpec::cl()),
            (RegSpec::b(2), RegSpec::dl()),
            (RegSpec::b(3), RegSpec::bl()),
            (RegSpec::b(4), RegSpec::ah()),
            (RegSpec::b(5), RegSpec::ch()),
            (RegSpec::b(6), RegSpec::dh()),
            (RegSpec::b(7), RegSpec::bh()),
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
    fn invalid_dword_reg_panics() {
        RegSpec::d(8);
    }

    #[test]
    #[should_panic]
    fn invalid_word_reg_panics() {
        RegSpec::w(8);
    }

    #[test]
    #[should_panic]
    fn invalid_byte_reg_panics() {
        RegSpec::b(8);
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
