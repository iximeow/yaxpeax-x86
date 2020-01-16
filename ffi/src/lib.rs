#![no_std]
#![feature(lang_items)]

#[panic_handler]
#[cold]
fn panic(panic: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[lang = "eh_personality"] extern fn eh_personality() {}

use yaxpeax_arch::{Arch, Decoder, LengthedInstruction, Address, AddressBase};
use yaxpeax_x86::x86_64;

use core::fmt::Write;

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_decode_x86_64_optimistic(data: *const u8, length: u64, inst: *mut yaxpeax_x86::Instruction) -> bool {
    let inst: &mut yaxpeax_x86::Instruction = core::mem::transmute(inst);
    <x86_64 as Arch>::Decoder::default().decode_into(inst, core::slice::from_raw_parts(data as *const u8, length as usize).iter().cloned()).is_err()
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_instr_length_x86_64(inst: *mut yaxpeax_x86::Instruction) -> usize {
    let inst: &mut yaxpeax_x86::Instruction = core::mem::transmute(inst);
    inst.len().to_linear()
}

struct InstructionSink<'buf> {
    buf: &'buf mut [u8],
    offs: usize,
}

impl<'a> core::fmt::Write for InstructionSink<'a> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.bytes() {
            if self.offs < self.buf.len() {
                self.buf[self.offs] = b;
                self.offs += 1;
            } else {
                break;
            }
        }

        Ok(())
    }
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_instr_fmt(inst: *mut yaxpeax_x86::Instruction, text: *mut u8, len: usize) {
    let inst: &mut yaxpeax_x86::Instruction = core::mem::transmute(inst);
    let mut res = core::slice::from_raw_parts_mut(text, len);

    write!(InstructionSink { buf: res, offs: 0 }, "{}", inst).unwrap();
}
