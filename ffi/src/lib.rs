use yaxpeax_arch::{Arch, Decoder, LengthedInstruction, Address};
use yaxpeax_x86::x86_64;

use std::fmt::Write;

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_decode_x86_64_optimistic(data: *const std::os::raw::c_char, length: std::os::raw::c_longlong, inst: *mut yaxpeax_x86::Instruction) -> bool {
    let inst: &mut yaxpeax_x86::Instruction = std::mem::transmute(inst);
    <x86_64 as Arch>::Decoder::default().decode_into(inst, std::slice::from_raw_parts(data as *const u8, length as usize).iter().cloned()).is_err()
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_instr_length_x86_64(inst: *mut yaxpeax_x86::Instruction) -> usize {
    let inst: &mut yaxpeax_x86::Instruction = std::mem::transmute(inst);
    inst.len().to_linear()
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_instr_fmt(inst: *mut yaxpeax_x86::Instruction, size: *mut u64, capacity: *mut u64) -> *mut u8 {
    let inst: &mut yaxpeax_x86::Instruction = std::mem::transmute(inst);
    let mut res = String::with_capacity(128);

    write!(res, "{}", inst).unwrap();

    let mut bytes = res.into_bytes();
    let ptr = bytes.as_mut_ptr();
    *capacity = bytes.capacity() as u64;
    *size = bytes.len() as u64;
    std::mem::forget(bytes);
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_instr_x86_64_blank() -> *mut yaxpeax_x86::Instruction {
    Box::leak(Box::new(yaxpeax_x86::Instruction::invalid()))
}

#[no_mangle]
pub unsafe extern "C" fn yaxpeax_str_drop(data: *mut u8, size: u64, capacity: u64) {
    std::mem::drop(
        Vec::from_raw_parts(data, size as usize, capacity as usize)
    );
}
