#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate yaxpeax_x86;

fuzz_target!(|data: &[u8]| {
    if data.len() > 15 {
        return;
    }
    let x86_64_decoder = yaxpeax_x86::long_mode::InstDecoder::default();
    let x86_32_decoder = yaxpeax_x86::protected_mode::InstDecoder::default();
    let x86_16_decoder = yaxpeax_x86::real_mode::InstDecoder::default();
    /*
    let inst_32b = x86_32_decoder.decode_slice(data).expect("is ok");
    let inst_16b = x86_16_decoder.decode_slice(data).expect("is ok");
    */

    if let Ok(inst_64b) = x86_64_decoder.decode_slice(data) {
        /*
        for b in data {
            eprint!("{:02x}", b);
        }
        eprintln!(": {}", inst_64b);
        */
        let behavior_64b = inst_64b.behavior();
        let _ = behavior_64b.privilege_level();
        let _ = behavior_64b.exceptions();
        let _ = behavior_64b.implicit_oplist();
        for i in 0..5 {
            let _ = behavior_64b.operand_access(i);
        }

        let mut opcount_64b = 0;
        use yaxpeax_x86::long_mode::{Opcode as b64Opcode, Operand as b64Operand};

        if let Ok(ops) = behavior_64b.all_operands() {
            // eprintln!("checking instr {}", inst_64b);
            for (op, acc) in ops.iter() {
                match op {
                    b64Operand::ImmediateI8 { .. } |
                    b64Operand::ImmediateU8 { .. } |
                    b64Operand::ImmediateI16 { .. } |
                    b64Operand::ImmediateU16 { .. } |
                    b64Operand::ImmediateI32 { .. } |
                    b64Operand::ImmediateU32 { .. } |
                    b64Operand::ImmediateI64 { .. } => {
                        // immediates are not reported as "accessed" below, as they are not really
                        // architectural state to be "accessed".. so skip them here to make the
                        // counters line up.
                        continue;
                    },
                    _ => {}
                }
//                eprintln!("saw {:?}, {:?}", op, acc);
                let mut accs = 0;
                if inst_64b.opcode() == b64Opcode::LEA && op.is_memory() {
                    // the access-visiting interface below does not report a memory access for lea
                    // because lea does not access memory. skip it to make the counters line up.
                    continue;
                }
                match op {
                    b64Operand::AbsoluteU32 { .. } |
                    b64Operand::AbsoluteU64 { .. } |
                    b64Operand::Register { .. } |
                    b64Operand::MemDeref { .. } |
                    b64Operand::Disp { .. } |
                    b64Operand::MemIndexScale { .. } |
                    b64Operand::MemIndexScaleDisp { .. } |
                    b64Operand::MemBaseIndexScale { .. } |
                    b64Operand::MemBaseIndexScaleDisp { .. } => {
                        accs = 1;
                    }
                    b64Operand::RegisterMaskMerge { mask, .. } |
                    b64Operand::RegisterMaskMergeSae { mask, .. } |
                    b64Operand::RegisterMaskMergeSaeNoround { mask, .. } |
                    b64Operand::MemDerefMasked { mask, .. } |
                    b64Operand::DispMasked { mask, .. } |
                    b64Operand::MemIndexScaleMasked { mask, .. } |
                    b64Operand::MemIndexScaleDispMasked { mask, .. } |
                    b64Operand::MemBaseIndexScaleMasked { mask, .. } |
                    b64Operand::MemBaseIndexScaleDispMasked { mask, .. } => {
                        accs = 1;
                        if mask.num() != 0 {
                            // the variants producing RegisterMaskMerge* are not sufficiently
                            // careful..
                            accs += 1;
                        }
                    }
                    _ => {
                        // immediates don't produce a register/memory read/write
                        accs = 0;
                    }
                }
                // read-write accesses are reported as two accesses in the visitor interface below.
                // count such cases twice here to make the counters line up.
                if acc.is_read() {
                    opcount_64b += accs;
                }
                if acc.is_write() {
                    opcount_64b += accs;
                }
            }
        }

        struct AccessCounter<'ctr> {
            counter: &'ctr mut usize,
        }

        impl<'ctr> yaxpeax_x86::long_mode::behavior::AccessVisitor for AccessCounter<'ctr> {
            fn register_read(&mut self, _reg: yaxpeax_x86::long_mode::RegSpec) {
//                eprintln!("saw read {:?}", _reg);
                *self.counter += 1;
            }
            fn register_write(&mut self, _reg: yaxpeax_x86::long_mode::RegSpec) {
//                eprintln!("saw write {:?}", _reg);
                *self.counter += 1;
            }
            fn get_register(&mut self, _reg: yaxpeax_x86::long_mode::RegSpec) -> Option<u64> { None }
            fn memory_read(&mut self, _address: Option<u64>, _size: u32) {
//                eprintln!("saw read {:?}", _size);
                *self.counter += 1;
            }
            fn memory_write(&mut self, _address: Option<u64>, _size: u32) {
//                eprintln!("saw write {:?}", _size);
                *self.counter += 1;
            }
        }

        let mut acc_seen_64b = 0;
        let mut visitor = AccessCounter {
            counter: &mut acc_seen_64b,
        };

        let visit_res = behavior_64b.visit_accesses(&mut visitor);
        if visit_res.is_ok() {
            assert_eq!(opcount_64b, acc_seen_64b);
        }
    }
});
