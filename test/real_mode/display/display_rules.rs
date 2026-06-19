use std::fmt::Write;

use yaxpeax_arch::{Decoder, LengthedInstruction};
use yaxpeax_arch::display::DisplaySink;
use yaxpeax_x86::real_mode::InstDecoder;
use yaxpeax_x86::real_mode::{DisplayRules, DisplayStyle};
use yaxpeax_x86::real_mode::AbsoluteAddressFormatter;

use std::fmt;

fn test_display_rule<Rules: for<'f, 'g> DisplayRules<yaxpeax_arch::display::FmtSink<'f, fmt::Formatter<'g>>>>(
    decoder: &InstDecoder, testcase: &'static TestCase, rules: [&Rules; 3]
) {
    let mut reader = yaxpeax_arch::U8Reader::new(testcase.bytes);
    let instr = decoder.decode(&mut reader).expect("display rule tests only run on instructions that decode");
    assert_eq!(instr.len().to_const(), testcase.bytes.len() as u32, "instruction length is incorrect");

    let intel_match = {
        let actual = instr.display_rules(rules[0]).to_string();
        if testcase.intel == actual {
            Ok(())
        } else {
            Err(actual)
        }
    };

    let c_match = {
        let actual = instr.display_rules(rules[1]).to_string();
        if let Some(c) = testcase.c.as_ref() {
            if *c == actual.as_str() {
                Ok(())
            } else {
                Err(actual)
            }
        } else {
            Ok(())
        }
    };

    let masm_match = {
        let actual = instr.display_rules(rules[2]).to_string();
        if let Some(masm) = testcase.masm.as_ref() {
            if *masm == actual.as_str() {
                Ok(())
            } else {
                Err(actual)
            }
        } else {
            Ok(())
        }
    };

    if intel_match.is_err() || c_match.is_err() || masm_match.is_err() {
        let mut hex = String::new();
        for b in testcase.bytes {
            write!(hex, "{:02x}", b).unwrap();
        }

        eprintln!("rule-based formatting test failed for a covered style. \n  \
               instr: {}\n  \
               hex: {}", instr, hex);

        if let Err(actual) = intel_match {
            eprintln!("  [-] intel style: {}, wanted {}", actual, testcase.intel);
        } else {
            eprintln!("  [+] intel style: OK ({})", testcase.intel);
        }

        if let Err(actual) = c_match {
            eprintln!("  [-] c style    : {}, wanted {}", actual, testcase.c.unwrap());
        } else {
            eprintln!("  [+] c style    : OK ({})", testcase.c.unwrap());
        }

        if let Err(actual) = masm_match {
            eprintln!("  [-] masm style : {}, wanted {}", actual, testcase.masm.unwrap());
        } else {
            eprintln!("  [+] masm style : OK ({})", testcase.masm.unwrap());
        }

        panic!("rule-based formatting failed for {}", instr);
    }
}

struct TestCase {
    bytes: &'static [u8],
    intel: &'static str,
    c: Option<&'static str>,
    masm: Option<&'static str>,
}

impl TestCase {
    const fn new(bytes: &'static [u8], intel: &'static str, c: Option<&'static str>, masm: Option<&'static str>) -> Self {
        Self { bytes, intel, c, masm }
    }
}

#[test]
fn test_absolute_addr() {
    let rules = AbsoluteAddressFormatter::new(0x3800);
    let rules_c = rules.with_style(DisplayStyle::C);
    let rules_masm = rules.with_style(DisplayStyle::Masm);

    let decoder = InstDecoder::default();

    static CASES: &'static [TestCase] = &[
        TestCase::new(
            &[0x33, 0xc0],
            "xor ax, ax", Some("ax ^= ax"), Some("xor ax, ax")
        ),
        TestCase::new(
            &[0x33, 0x45, 0x78],
            "xor ax, word [di + 0x78]", Some("ax ^= [di + 0x78]"), Some("xor ax, word ptr [di + 78h]"),
        ),
        TestCase::new(
            &[0xeb, 0x10],
            "jmp 0x3812", Some("jmp 0x3812"), Some("jmp 3812h"),
        ),
        TestCase::new(
            &[0xe9, 0x00, 0xd0],
            "jmp 0x803", Some("jmp 0x803"), Some("jmp 803h"),
        ),
    ];

    for case in CASES.iter() {
        test_display_rule(&decoder, case, [&rules, &rules_c, &rules_masm]);
    }
}

/// a simple DisplayRules impl to test that addresses directly at symbols are printed reasonably.
/// if a symbol is not directly at an address, the address is printed instead.
#[derive(Copy, Clone)]
struct SimpleSymbolicator<'syms> {
    eip: u32,
    style: DisplayStyle,
    symbols: &'syms [(u32, &'static str)],
}

impl<'syms> SimpleSymbolicator<'syms> {
    pub fn new(eip: u32, symbols: &'syms [(u32, &'static str)]) -> Self {
        SimpleSymbolicator {
            eip,
            style: DisplayStyle::Intel,
            symbols,
        }
    }

    pub fn with_style(mut self, style: DisplayStyle) -> Self {
        self.style = style;
        self
    }
}

impl<'syms, S: DisplaySink> DisplayRules<S> for SimpleSymbolicator<'syms> {
    fn display_style(&self) -> DisplayStyle {
        self.style
    }

    fn instr_addr(&self) -> Option<u32> {
        Some(self.eip)
    }

    fn emit_address(&self, addr: u32, s: &mut S) -> Result<bool, fmt::Error> {
        for (sym_addr, sym) in self.symbols {
            if *sym_addr == addr {
                s.write_str(sym)?;
                return Ok(true);
            }
        }

        Ok(false)
    }
}

#[test]
fn test_symbolication() {
    let syms = &[
        (0x0000_9000, "memcpy"),
        (0x0000_c000, "global"),
        (0x0000_8002, "branch_target"),
        (0x0000_8003, "wide_target"),
    ];
    let rules = SimpleSymbolicator::new(0x0000_8000, syms);
    let rules_c = rules.with_style(DisplayStyle::C);
    let rules_masm = rules.with_style(DisplayStyle::Masm);

    let decoder = InstDecoder::default();

    static CASES: &'static [TestCase] = &[
        TestCase::new(
            &[0xe8, 0xfd, 0x0f],
            "call memcpy", Some("call memcpy"), Some("call memcpy")
        ),
        TestCase::new(
            &[0x33, 0x45, 0x78],
            "xor ax, word [di + 0x78]", Some("ax ^= [di + 0x78]"), Some("xor ax, word ptr [di + 78h]"),
        ),
        TestCase::new(
            &[0xa2, 0x00, 0xc0],
            "mov byte [global], al", Some("[global] = al"), Some("mov byte ptr [global], al"),
        ),
        TestCase::new(
            &[0xeb, 0x00],
            "jmp branch_target", Some("jmp branch_target"), Some("jmp branch_target"),
        ),
        TestCase::new(
            &[0xe9, 0x00, 0x00],
            "jmp wide_target", Some("jmp wide_target"), Some("jmp wide_target"),
        ),
    ];

    for case in CASES.iter() {
        test_display_rule(&decoder, case, [&rules, &rules_c, &rules_masm]);
    }
}
