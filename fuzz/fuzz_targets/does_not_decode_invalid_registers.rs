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
                Operand::Register { reg } => {
                    assert!(!reg.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMerge { reg, mask, merge: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSae { reg, mask, merge: _, sae: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSaeNoround { reg, mask, merge: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemDeref { base } => {
                    assert!(!base.class().name().contains("BUG"));
                }
                Operand::Disp { base, disp: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                }
                Operand::MemIndexScale { index, scale: _ } => {
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScale { base, index, scale: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleDisp { base, index, scale: _, disp: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemDerefMasked { base, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::DispMasked { base, disp: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleMasked { base, index, scale: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemIndexScaleDispMasked { index, scale: _, disp: _, mask } => {
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleDispMasked { base, index, scale: _, disp: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
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
                Operand::Register { reg } => {
                    assert!(!reg.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMerge { reg, mask, merge: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSae { reg, mask, merge: _, sae: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSaeNoround { reg, mask, merge: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemDeref { base } => {
                    assert!(!base.class().name().contains("BUG"));
                }
                Operand::Disp { base, disp: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                }
                Operand::MemIndexScale { index, scale: _ } => {
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScale { base, index, scale: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleDisp { base, index, scale: _, disp: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemDerefMasked { base, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::DispMasked { base, disp: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleMasked { base, index, scale: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemIndexScaleDispMasked { index, scale: _, disp: _, mask } => {
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleDispMasked { base, index, scale: _, disp: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
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
                Operand::Register { reg } => {
                    assert!(!reg.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMerge { reg, mask, merge: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSae { reg, mask, merge: _, sae: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::RegisterMaskMergeSaeNoround { reg, mask, merge: _ } => {
                    assert!(!reg.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemDeref { base } => {
                    assert!(!base.class().name().contains("BUG"));
                }
                Operand::MemDisp { base, disp: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                }
                Operand::MemIndexScale { index, scale: _ } => {
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemIndexScaleDisp { index, scale: _, disp: _ } => {
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleDisp { base, index, scale: _, disp: _ } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                }
                Operand::MemDerefMasked { base, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemDispMasked { base, disp: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleMasked { base, index, scale: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemIndexScaleDispMasked { index, scale: _, disp: _, mask } => {
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
                }
                Operand::MemBaseIndexScaleDispMasked { base, index, scale: _, disp: _, mask } => {
                    assert!(!base.class().name().contains("BUG"));
                    assert!(!index.class().name().contains("BUG"));
                    assert!(!mask.class().name().contains("BUG"));
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
