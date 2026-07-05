#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate yaxpeax_x86;
extern crate yaxpeax_arch;

use std::fmt::Write;

fuzz_target!(|data: &[u8]| {
    let x86_64_decoder = yaxpeax_x86::long_mode::InstDecoder::default();
    let x86_32_decoder = yaxpeax_x86::protected_mode::InstDecoder::default();
    let x86_16_decoder = yaxpeax_x86::real_mode::InstDecoder::default();

    if let Ok(inst) = x86_64_decoder.decode_slice(data) {
        use yaxpeax_x86::long_mode::DisplayStyle;

        let mut s = String::new();
        write!(s, "{}", inst.display_with(DisplayStyle::Intel)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
        s.clear();
        write!(s, "{}", inst.display_with(DisplayStyle::C)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
        s.clear();
        write!(s, "{}", inst.display_with(DisplayStyle::Masm)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
    };

    if let Ok(inst) = x86_32_decoder.decode_slice(data) {
        use yaxpeax_x86::protected_mode::DisplayStyle;

        let mut s = String::new();
        write!(s, "{}", inst.display_with(DisplayStyle::Intel)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
        s.clear();
        write!(s, "{}", inst.display_with(DisplayStyle::C)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
        s.clear();
        write!(s, "{}", inst.display_with(DisplayStyle::Masm)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
    };

    if let Ok(inst) = x86_16_decoder.decode_slice(data) {
        use yaxpeax_x86::real_mode::DisplayStyle;

        let mut s = String::new();
        write!(s, "{}", inst.display_with(DisplayStyle::Intel)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
        s.clear();
        write!(s, "{}", inst.display_with(DisplayStyle::C)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
        s.clear();
        write!(s, "{}", inst.display_with(DisplayStyle::Masm)).expect("can write");
        // MAX_INSTRUCTION_LEN is not a public crate item yet...
        assert!(s.len() < 512);
    };
});
