//! if a register has a single-byte register operand, and it's one of `al`, `bl`, `cl`, or `dl`, it
//! should compare equal to the `RegSpec` produced by `RegSpec::al()` and so on.
//!
//! at one point this was a bug; `RegSpec::al()` would use `RegisterBank::B`, but an instruction
//! with `rex.w` set could get an `al` backed by a `RegSpec` in `RegisterBank::rB`.

#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate yaxpeax_x86;

// this test is not meaningful for 32-bit or 16-bit modes, there are no register synonyms in those
// cases. leaving them in for fuzz targets to match other cases, and In Case Of Future Change.
fuzz_target!(|data: &[u8]| {
    let x86_64_decoder = yaxpeax_x86::long_mode::InstDecoder::default();
    let x86_32_decoder = yaxpeax_x86::protected_mode::InstDecoder::default();
    let x86_16_decoder = yaxpeax_x86::real_mode::InstDecoder::default();

    if let Ok(inst) = x86_64_decoder.decode_slice(data) {
        for i in 0..inst.operand_count() {
            match inst.operand(i) {
                yaxpeax_x86::long_mode::Operand::Register(reg) => {
                    if reg.num() < 4 && reg.class() == yaxpeax_x86::long_mode::register_class::RB {
                        assert!(false, "instruction has rex.w register that aliases old byte registers");
                    } else {
                        /* not a potentially-unwanted register */
                    }
                },
                _ => { /* not a relevant operand kind. immediate or memory of some kind. */ }
            }
        }
    };

    /*
    if let Ok(inst) = x86_32_decoder.decode_slice(data) {
        for i in 0..inst.operand_count() {
            match inst.operand(i) {
                Operand::Register(_reg) => {
                    /* not a potentially-unwanted register */
                },
                _ => { /* not a relevant operand kind. immediate or memory of some kind. */ }
            }
        }
    };

    if let Ok(inst) = x86_16_decoder.decode_slice(data) {
        for i in 0..inst.operand_count() {
            match inst.operand(i) {
                Operand::Register(_reg) => {
                    /* not a potentially-unwanted register */
                },
                _ => { /* not a relevant operand kind. immediate or memory of some kind. */ }
            }
        }
    };
    */
});
