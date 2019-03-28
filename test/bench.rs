#![feature(test)]

extern crate test;
extern crate yaxpeax_x86;

use std::ffi::c_void;

use std::io::Write;

use test::Bencher;

use yaxpeax_x86::{Instruction, Opcode, decode_one};

fn decode(bytes: &[u8]) -> Option<Instruction> {
    let mut instr = Instruction::invalid();
    match decode_one(bytes.iter().map(|x| *x), &mut instr) {
        Some(()) => Some(instr),
        None => None
    }
}

#[bench]
fn bench_102000_instrs(b: &mut Bencher) {
    b.iter(|| {
        for i in (0..3000) {
            test::black_box(do_decode_swathe());
        }
    })
}

#[cfg(feature = "capstone_bench")]
#[bench]
fn bench_102000_intrs_capstone(b: &mut Bencher) {
    let handle = get_cs_handle();
//    panic!("Allocating..");
    let mut instr: *mut c_void = unsafe { cs_malloc(handle) };
//    panic!("Allocated...");
    b.iter(|| {
        for i in (0..3000) {
            test::black_box(do_capstone_decode_swathe(handle, instr));
        }
    })
}

const decode_data: [u8; 130] = [
    0x48, 0xc7, 0x04, 0x24, 0x00, 0x00, 0x00, 0x00,
    0x48, 0x89, 0x44, 0x24, 0x08,
    0x48, 0x89, 0x43, 0x18,
    0x48, 0xc7, 0x43, 0x10, 0x00, 0x00, 0x00, 0x00,
    0x49, 0x89, 0x4e, 0x08,
    0x48, 0x8b, 0x32,
    0x49, 0x89, 0x46, 0x10,
    0x4d, 0x0f, 0x43, 0xec, 0x49,
    0x0f, 0xb6, 0x06,
    0x0f, 0xb7, 0x06,
    0x66, 0x41, 0x50,
    0x66, 0x41, 0x31, 0xc0,
    0x66, 0x41, 0x32, 0xc0,
    0x40, 0x32, 0xc5,
    0x73, 0x31,
    0x72, 0x5a,
    0x0f, 0x86, 0x8b, 0x01, 0x00, 0x00,
    0x74, 0x47,
    0xff, 0x15, 0x7e, 0x72, 0x24, 0x00,
    0xc3,
    0x48, 0x3d, 0x01, 0xf0, 0xff, 0xff,
    0x3d, 0x01, 0xf0, 0xff, 0xff,
    0x48, 0x83, 0xf8, 0xff,
    0x48, 0x39, 0xc6,
    0x48, 0x8d, 0xa4, 0xc7, 0x20, 0x00, 0x00, 0x12,
    0x33, 0xc0,
    0x48, 0x8d, 0x53, 0x08,
    0x31, 0xc9,
    0x48, 0x29, 0xc8,
    0x48, 0x03, 0x0b,
    0x5b,
    0x41, 0x5e,
    0x48, 0x8d, 0x0c, 0x12,
    0xf6, 0xc2, 0x18
];

fn do_decode_swathe() {
    let mut buf = [0u8; 128];
    let mut iter = decode_data.iter().map(|x| *x);
    let mut result = yaxpeax_x86::Instruction::invalid();
    loop {
        match yaxpeax_x86::decode_one(&mut iter, &mut result) {
            Some(_) => {
                #[cfg(feature = "capstone_bench")]
                test::black_box(write!(&mut buf[..], "{}", result));
                test::black_box(&result);
            },
            None => {
                println!("done.");
                break;
            }
        }
    }
}

#[cfg(feature = "capstone_bench")]
extern "C" {
    pub fn cs_open(arch: u32, mode: u32, handle: *mut usize) -> usize;
}

#[cfg(feature = "capstone_bench")]
extern "C" {
    pub fn cs_malloc(handle: usize) -> *mut c_void;
}

#[cfg(feature = "capstone_bench")]
extern "C" {
    pub fn cs_disasm_iter(
        arch: usize,
        code: *mut *const u8,
        size: *mut usize,
        address: *mut u64,
        insn: *mut c_void
    ) -> bool;
}

#[cfg(feature = "capstone_bench")]
fn get_cs_handle() -> usize {
    let mut handle: usize = 0;
    let res = unsafe { cs_open(3, 4, &mut handle as *mut usize) };
    handle
}

#[cfg(feature = "capstone_bench")]
fn get_instr(handle: usize) -> *mut c_void {
    unsafe { cs_malloc(handle) as *mut c_void }
}

#[cfg(feature = "capstone_bench")]
fn do_capstone_decode_swathe(cs: usize, instr: *mut c_void) {
    unsafe {
        let mut code = &decode_data as *const u8;
        let mut len = decode_data.len();
        let mut addr = 0u64;
        loop {
            let result = cs_disasm_iter(
                cs,
                &mut code as *mut *const u8,
                &mut len as *mut usize,
                &mut addr as *mut u64,
                instr
            );
            //panic!("at least one succeeded");
            if result == false {
                return;
            }
        }
    }
}

//#[bench]
//#[ignore]
// VEX prefixes are not supported at the moment, in any form
//fn test_avx() {
//    assert_eq!(&format!("{}", decode(
//        &[0xc5, 0xf8, 0x10, 0x00]
//    ).unwrap()), "vmovups xmm0, xmmword [rax]");
//}
