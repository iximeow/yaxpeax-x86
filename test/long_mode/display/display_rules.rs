use std::fmt::Write;

use yaxpeax_arch::{Decoder, LengthedInstruction};
use yaxpeax_arch::display::DisplaySink;
use yaxpeax_x86::long_mode::InstDecoder;
use yaxpeax_x86::long_mode::{DisplayRules, DisplayStyle};
use yaxpeax_x86::long_mode::AbsoluteAddressFormatter;

use std::fmt;

fn test_display_rule<Rules: for<'f, 'g> DisplayRules<yaxpeax_arch::display::FmtSink<'f, fmt::Formatter<'g>>>>(
    decoder: &InstDecoder, testcase: &'static TestCase, rules: [&Rules; 3]
) {
    let mut reader = yaxpeax_arch::U8Reader::new(testcase.bytes);
    let instr = decoder.decode(&mut reader).expect("display rule tests only run on instructions that decode");
    assert_eq!(instr.len().to_const(), testcase.bytes.len() as u64, "instruction length is incorrect");

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
    let rules = AbsoluteAddressFormatter::new(0x0380_0000);
    let rules_c = rules.with_style(DisplayStyle::C);
    let rules_masm = rules.with_style(DisplayStyle::Masm);

    let decoder = InstDecoder::default();

    static CASES: &'static [TestCase] = &[
        TestCase::new(
            &[0x33, 0xc0],
            "xor eax, eax", Some("eax ^= eax"), Some("xor eax, eax")
        ),
        TestCase::new(
            &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12],
            "xor eax, dword [0x15b45678]", Some("eax ^= [0x15b45678]"), Some("xor eax, dword ptr [15B45678h]"),
        ),
        TestCase::new(
            &[0xeb, 0x10],
            "jmp 0x3800012", Some("jmp 0x3800012"), Some("jmp 3800012h"),
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
    rip: u64,
    style: DisplayStyle,
    symbols: &'syms [(u64, &'static str)],
}

impl<'syms> SimpleSymbolicator<'syms> {
    pub fn new(rip: u64, symbols: &'syms [(u64, &'static str)]) -> Self {
        SimpleSymbolicator {
            rip,
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

    fn instr_addr(&self) -> Option<u64> {
        Some(self.rip)
    }

    fn emit_address(&self, addr: u64, s: &mut S) -> Result<bool, fmt::Error> {
        for (sym_addr, sym) in self.symbols {
            eprintln!("does symbol {} at {:04x} match {:04x}?", sym, sym_addr, addr);
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
        (0x0002_0000, "memcpy"),
        (0x0020_0000, "global"),
        (0x0021_0006, "global_riprel"),
        (0x0001_0002, "branch_target"),
        (0x0001_0005, "wide_target"),
    ];
    let rules = SimpleSymbolicator::new(0x0001_0000, syms);
    let rules_c = rules.with_style(DisplayStyle::C);
    let rules_masm = rules.with_style(DisplayStyle::Masm);

    let decoder = InstDecoder::default();

    static CASES: &'static [TestCase] = &[
        TestCase::new(
            &[0xe8, 0xfb, 0xff, 0x00, 0x00],
            "call memcpy", Some("call memcpy"), Some("call memcpy")
        ),
        TestCase::new(
            &[0x33, 0x05, 0x00, 0x00, 0x20, 0x00],
            "xor eax, dword [global_riprel]", Some("eax ^= [global_riprel]"), Some("xor eax, dword ptr [global_riprel]"),
        ),
        TestCase::new(
            &[0xa2, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00],
            "mov byte [global], al", Some("[global] = al"), Some("mov byte ptr [global], al"),
        ),
        TestCase::new(
            &[0xeb, 0x00],
            "jmp branch_target", Some("jmp branch_target"), Some("jmp branch_target"),
        ),
        TestCase::new(
            &[0xe9, 0x00, 0x00, 0x00, 0x00],
            "jmp wide_target", Some("jmp wide_target"), Some("jmp wide_target"),
        ),
    ];

    for case in CASES.iter() {
        test_display_rule(&decoder, case, [&rules, &rules_c, &rules_masm]);
    }
}
