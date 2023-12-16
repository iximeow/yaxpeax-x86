//! instruction text should never include the word BUG - this is a symptom of selecting an invalid
//! RegSpec while disassembling.

#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate yaxpeax_x86;

fuzz_target!(|data: &[u8]| {
    let x86_64_decoder = yaxpeax_x86::long_mode::InstDecoder::default();
    let x86_32_decoder = yaxpeax_x86::protected_mode::InstDecoder::default();
    let x86_16_decoder = yaxpeax_x86::real_mode::InstDecoder::default();

    if let Ok(inst) = x86_64_decoder.decode_slice(data) {
        use yaxpeax_x86::long_mode::Operand;
        let mut res = String::new();
        inst.write_to(&mut res).expect("format does not panic");
        assert!(!res.contains("BUG"));
        for i in 0..inst.operand_count() {
            match inst.operand(i) {
                Operand::Register(reg) => {
                    assert!(!reg.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMerge(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSae(r1, r2, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSaeNoround(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDeref(r1) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegDisp(r1, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegScale(r1, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegIndexBase(r1, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseDisp(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegScaleDisp(r1, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScale(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleDisp(r1, r2, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDerefMasked(r1, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDispMasked(r1, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegScaleMasked(r1, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseMasked(r1, r2, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseDispMasked(r1, r2, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegScaleDispMasked(r1, _, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleMasked(r1, r2, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleDispMasked(r1, r2, _, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::Nothing => {
                    panic!("`Nothing` should not be an operand listed in `inst.operand_count()`");
                }
                _ => {
                    /* operands with no register - immediates or a non-rip-relative displacement */
                }
            }
        }
    };

    if let Ok(inst) = x86_32_decoder.decode_slice(data) {
        use yaxpeax_x86::protected_mode::Operand;
        let mut res = String::new();
        inst.write_to(&mut res).expect("format does not panic");
        assert!(!res.contains("BUG"));
        for i in 0..inst.operand_count() {
            match inst.operand(i) {
                Operand::Register(reg) => {
                    assert!(!reg.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMerge(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSae(r1, r2, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSaeNoround(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDeref(r1) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegDisp(r1, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegScale(r1, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegIndexBase(r1, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseDisp(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegScaleDisp(r1, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScale(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleDisp(r1, r2, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDerefMasked(r1, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDispMasked(r1, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegScaleMasked(r1, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseMasked(r1, r2, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseDispMasked(r1, r2, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegScaleDispMasked(r1, _, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleMasked(r1, r2, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleDispMasked(r1, r2, _, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::Nothing => {
                    panic!("`Nothing` should not be an operand listed in `inst.operand_count()`");
                }
                _ => {
                    /* operands with no register - immediates or a non-rip-relative displacement */
                }
            }
        }
    };

    if let Ok(inst) = x86_16_decoder.decode_slice(data) {
        use yaxpeax_x86::real_mode::Operand;
        let mut res = String::new();
        inst.write_to(&mut res).expect("format does not panic");
        assert!(!res.contains("BUG"));
        for i in 0..inst.operand_count() {
            match inst.operand(i) {
                Operand::Register(reg) => {
                    assert!(!reg.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMerge(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSae(r1, r2, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSaeNoround(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDeref(r1) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegDisp(r1, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegScale(r1, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegIndexBase(r1, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseDisp(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegScaleDisp(r1, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScale(r1, r2, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleDisp(r1, r2, _, _) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDerefMasked(r1, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegDispMasked(r1, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegScaleMasked(r1, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseMasked(r1, r2, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseDispMasked(r1, r2, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegScaleDispMasked(r1, _, _, r2) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleMasked(r1, r2, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::RegIndexBaseScaleDispMasked(r1, r2, _, _, r3) => {
                    assert!(!r1.class().name().contains("BUG"));
                    assert!(!r2.class().name().contains("BUG"));
                    assert!(!r3.class().name().contains("BUG"));
                }
                Operand::Nothing => {
                    panic!("`Nothing` should not be an operand listed in `inst.operand_count()`");
                }
                _ => {
                    /* operands with no register - immediates or a non-rip-relative displacement */
                }
            }
        }
    };
});
