extern crate x86_rs;

use x86_rs::{Instruction, Opcode, decode_one};

fn decode(bytes: &[u8]) -> Option<Instruction> {
    let mut instr = Instruction::invalid();
    match decode_one(bytes, &mut instr) {
        Some(()) => Some(instr),
        None => None
    }
}

#[test]
fn test_mov() {
    assert_eq!(&format!("{}", decode(
        &[0x48, 0xc7, 0x04, 0x24, 0x00, 0x00, 0x00, 0x00]
    ).unwrap()), "mov [rsp], 0x0");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x89, 0x44, 0x24, 0x08]
    ).unwrap()), "mov [rsp + 0x8], rax");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x89, 0x43, 0x18]
    ).unwrap()), "mov [rbx + 0x18], rax");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0xc7, 0x43, 0x10, 0x00, 0x00, 0x00, 0x00]
    ).unwrap()), "mov [rbx + 0x10], 0x0");
    assert_eq!(&format!("{}", decode(
        &[0x49, 0x89, 0x4e, 0x08]
    ).unwrap()), "mov [r14 + 0x8], rcx");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x8b, 0x32]
    ).unwrap()), "mov rsi, [rdx]");
    assert_eq!(&format!("{}", decode(
        &[0x49, 0x89, 0x46, 0x10]
    ).unwrap()), "mov [r14 + 0x10], rax");
    assert_eq!(&format!("{}", decode(
        &[0x4d, 0x0f, 0x43, 0xec, 0x49]
    ).unwrap()), "cmovnb r13, r12");
    assert_eq!(&format!("{}", decode(
        &[0x0f, 0xb6, 0x06]
    ).unwrap()), "movzx eax, byte [rsi]");
    assert_eq!(&format!("{}", decode(
        &[0x0f, 0xb7, 0x06]
    ).unwrap()), "movzx eax, word [rsi]");
}

#[test]
fn test_stack() {
    assert_eq!(&format!("{}", decode(
        &[0x66, 0x41, 0x50]
    ).unwrap()), "push r8w");
}

#[test]
fn test_prefixes() {
    assert_eq!(&format!("{}", decode(
        &[0x66, 0x41, 0x31, 0xc0]
    ).unwrap()), "xor r8w, ax");
    assert_eq!(&format!("{}", decode(
        &[0x66, 0x41, 0x32, 0xc0]
    ).unwrap()), "xor al, r8b");
    assert_eq!(&format!("{}", decode(
        &[0x40, 0x32, 0xc5]
    ).unwrap()), "xor al, bpl");
}

#[test]
fn test_control_flow() {
    assert_eq!(&format!("{}", decode(
        &[0x73, 0x31]
    ).unwrap()), "jnb 0x31");
    assert_eq!(&format!("{}", decode(
        &[0x72, 0x5a]
    ).unwrap()), "jb 0x5a");
    assert_eq!(&format!("{}", decode(
        &[0x0f, 0x86, 0x8b, 0x01, 0x00, 0x00]
    ).unwrap()), "jna 0x18b");
    assert_eq!(&format!("{}", decode(
        &[0x74, 0x47]
    ).unwrap()), "jz 0x47");
    assert_eq!(&format!("{}", decode(
        &[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00]
    ).unwrap()), "call [rip + 0x24727e]");
    assert_eq!(&format!("{}", decode(
        &[0xc3]
    ).unwrap()), "ret");
}

#[test]
fn test_test_cmp() {
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x3d, 0x01, 0xf0, 0xff, 0xff]
    ).unwrap()), "cmp rax, 0xfffffffffffff001");
    assert_eq!(&format!("{}", decode(
        &[0x3d, 0x01, 0xf0, 0xff, 0xff]
    ).unwrap()), "cmp eax, 0xfffff001");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x83, 0xf8, 0xff]
    ).unwrap()), "cmp rax, 0xffffffffffffffff");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x39, 0xc6]
    ).unwrap()), "cmp rsi, rax");
}

#[test]
fn test_misc() {
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x8d, 0xa4, 0xc7, 0x20, 0x00, 0x00, 0x12]
    ).unwrap()), "lea rsp, [rdi + rax * 8 + 0x12000020]");
    assert_eq!(&format!("{}", decode(
        &[0x33, 0xc0]
    ).unwrap()), "xor eax, eax");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x8d, 0x53, 0x08]
    ).unwrap()), "lea rdx, [rbx + 0x8]");
    assert_eq!(&format!("{}", decode(
        &[0x31, 0xc9]
    ).unwrap()), "xor ecx, ecx");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x29, 0xc8]
    ).unwrap()), "sub rax, rcx");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x03, 0x0b]
    ).unwrap()), "add rcx, [rbx]");
    assert_eq!(&format!("{}", decode(
        &[0x5b]
    ).unwrap()), "pop rbx");
    assert_eq!(&format!("{}", decode(
        &[0x41, 0x5e]
    ).unwrap()), "pop r14");
    assert_eq!(&format!("{}", decode(
        &[0x48, 0x8d, 0x0c, 0x12]
    ).unwrap()), "lea rcx, [rdx + rdx]");
    assert_eq!(&format!("{}", decode(
        &[0xf6, 0xc2, 0x18]
    ).unwrap()), "test dl, 0x18");
}
