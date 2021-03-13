#![no_std]
#![feature(lang_items)]

#[panic_handler]
#[cold]
fn panic(_panic: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[lang = "eh_personality"] extern fn eh_personality() {}

use yaxpeax_arch::{Arch, Decoder, LengthedInstruction, AddressBase};
use yaxpeax_x86::long_mode as amd64;

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_decode_x86_64_optimistic(data: *const u8, length: u64, inst: *mut amd64::Instruction) -> bool {
    let inst: &mut amd64::Instruction = core::mem::transmute(inst);
    <amd64::Arch as Arch>::Decoder::default().decode_into(inst, core::slice::from_raw_parts(data as *const u8, length as usize).iter().cloned()).is_err()
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_instr_length_x86_64(inst: *mut amd64::Instruction) -> usize {
    let inst: &mut amd64::Instruction = core::mem::transmute(inst);
    0.wrapping_offset(inst.len()).to_linear()
}

#[cfg(fmt)]
mod write_sink;

#[cfg(fmt)]
mod fmt {
    use write_sink::InstructionSink;

    use core::fmt::Write;

    #[no_mangle]
    pub unsafe extern "C" fn yaxpeax_instr_fmt(inst: *mut amd64::Instruction, text: *mut u8, len: usize) {
        let inst: &mut amd64::Instruction = core::mem::transmute(inst);
        let res = core::slice::from_raw_parts_mut(text, len);

        write!(InstructionSink { buf: res, offs: 0 }, "{}", inst).unwrap();
    }
}
