use yaxpeax_arch::{Arch, Decoder, LengthedInstruction, U8Reader, AddressBase};
use yaxpeax_x86::real_mode;

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_x86_16_decode(data: *const u8, length: u64, inst: *mut real_mode::Instruction) -> bool {
    let inst: &mut real_mode::Instruction = core::mem::transmute(inst);
    let mut reader = U8Reader::new(core::slice::from_raw_parts(data as *const u8, length as usize));
    <real_mode::Arch as Arch>::Decoder::default().decode_into(inst, &mut reader).is_err()
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_x86_16_instr_length(inst: *mut real_mode::Instruction) -> usize {
    let inst: &mut real_mode::Instruction = core::mem::transmute(inst);
    0.wrapping_offset(inst.len()).to_linear()
}

#[cfg(feature = "fmt")]
mod write_sink;

#[cfg(feature = "fmt")]
mod fmt {
    use super::write_sink::InstructionSink;

    use core::fmt::Write;

    use yaxpeax_x86::real_mode;

    #[no_mangle]
    pub unsafe extern "C" fn yaxpeax_x86_16_fmt(inst: *mut real_mode::Instruction, text: *mut u8, len: usize) {
        let inst: &mut real_mode::Instruction = core::mem::transmute(inst);
        let res = core::slice::from_raw_parts_mut(text, len);

        write!(InstructionSink { buf: res, offs: 0 }, "{}", inst).unwrap();
    }
}

#[cfg(feature = "fmt")]
pub use fmt::yaxpeax_x86_16_fmt;
