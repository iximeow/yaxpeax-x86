#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate yaxpeax_x86;

use yaxpeax_arch::{Decoder, LengthedInstruction};

fuzz_target!(|data: &[u8]| {
    let x86_64_decoder = yaxpeax_x86::long_mode::InstDecoder::default();
    let x86_32_decoder = yaxpeax_x86::protected_mode::InstDecoder::default();
    let x86_16_decoder = yaxpeax_x86::real_mode::InstDecoder::default();

    {
        use yaxpeax_x86::long_mode::{DisplayStyle, InstructionTextBuffer};
        let decoder = x86_64_decoder;

        if let Ok(mut inst) = decoder.decode_slice(data) {
            let remainder_slice = &data[inst.len().to_const() as usize..];

            let mut initial_inst_buf = InstructionTextBuffer::new();
            initial_inst_buf.format_inst(&inst.display_with(DisplayStyle::Intel)).expect("can format");

            let mut reader = yaxpeax_arch::U8Reader::new(data);
            decoder.decode_into(&mut inst, &mut reader).expect("can redecode");

            let mut re_inst_buf = InstructionTextBuffer::new();
            re_inst_buf.format_inst(&inst.display_with(DisplayStyle::Intel)).expect("can format");

            assert_eq!(initial_inst_buf.text_str(), re_inst_buf.text_str());

            let first_next_inst = decoder.decode_slice(remainder_slice);
            let mut rest_reader = yaxpeax_arch::U8Reader::new(remainder_slice);
            let rest_res = decoder.decode_into(&mut inst, &mut rest_reader);
            match (first_next_inst, rest_res) {
                (Ok(first_next_inst), Ok(())) => {
                    assert_eq!(first_next_inst, inst, "on second pass of {:x?}", remainder_slice);
                }
                (Err(left), Err(right)) => {
                    assert_eq!(left, right, "on second pass of {:x?}", remainder_slice);
                }
                other => {
                    panic!("mismatched results when reusing instruction: {:?}", other);
                }
            }
        };
    }

    {
        use yaxpeax_x86::protected_mode::{DisplayStyle, InstructionTextBuffer};
        let decoder = x86_32_decoder;

        if let Ok(mut inst) = decoder.decode_slice(data) {
            let remainder_slice = &data[inst.len().to_const() as usize..];

            let mut initial_inst_buf = InstructionTextBuffer::new();
            initial_inst_buf.format_inst(&inst.display_with(DisplayStyle::Intel)).expect("can format");

            let mut reader = yaxpeax_arch::U8Reader::new(data);
            decoder.decode_into(&mut inst, &mut reader).expect("can redecode");

            let mut re_inst_buf = InstructionTextBuffer::new();
            re_inst_buf.format_inst(&inst.display_with(DisplayStyle::Intel)).expect("can format");

            assert_eq!(initial_inst_buf.text_str(), re_inst_buf.text_str());

            let first_next_inst = decoder.decode_slice(remainder_slice);
            let mut rest_reader = yaxpeax_arch::U8Reader::new(remainder_slice);
            let rest_res = decoder.decode_into(&mut inst, &mut rest_reader);
            match (first_next_inst, rest_res) {
                (Ok(first_next_inst), Ok(())) => {
                    assert_eq!(first_next_inst, inst, "on second pass of {:x?}", remainder_slice);
                }
                (Err(left), Err(right)) => {
                    assert_eq!(left, right, "on second pass of {:x?}", remainder_slice);
                }
                other => {
                    panic!("mismatched results when reusing instruction: {:?}", other);
                }
            }
        };
    }

    {
        use yaxpeax_x86::real_mode::{DisplayStyle, InstructionTextBuffer};
        let decoder = x86_16_decoder;

        if let Ok(mut inst) = decoder.decode_slice(data) {
            let remainder_slice = &data[inst.len().to_const() as usize..];

            let mut initial_inst_buf = InstructionTextBuffer::new();
            initial_inst_buf.format_inst(&inst.display_with(DisplayStyle::Intel)).expect("can format");

            let mut reader = yaxpeax_arch::U8Reader::new(data);
            decoder.decode_into(&mut inst, &mut reader).expect("can redecode");

            let mut re_inst_buf = InstructionTextBuffer::new();
            re_inst_buf.format_inst(&inst.display_with(DisplayStyle::Intel)).expect("can format");

            assert_eq!(initial_inst_buf.text_str(), re_inst_buf.text_str());

            let first_next_inst = decoder.decode_slice(remainder_slice);
            let mut rest_reader = yaxpeax_arch::U8Reader::new(remainder_slice);
            let rest_res = decoder.decode_into(&mut inst, &mut rest_reader);
            match (first_next_inst, rest_res) {
                (Ok(first_next_inst), Ok(())) => {
                    assert_eq!(first_next_inst, inst, "on second pass of {:x?}", remainder_slice);
                }
                (Err(left), Err(right)) => {
                    assert_eq!(left, right, "on second pass of {:x?}", remainder_slice);
                }
                other => {
                    panic!("mismatched results when reusing instruction: {:?}", other);
                }
            }
        };
    }
});
