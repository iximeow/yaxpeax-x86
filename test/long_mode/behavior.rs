
#[cfg(target_arch = "x86_64")]
mod kvm {
    use asmlinator::x86_64::{GuestAddress, Vm, VcpuExit, kvm_regs, kvm_sregs};

    use yaxpeax_x86::long_mode;
    use yaxpeax_x86::long_mode::behavior::Exception;

    use yaxpeax_arch::LengthedInstruction;
    use yaxpeax_x86::long_mode::Instruction;

    use rand::prelude::*;

    use std::fmt::Write;

    #[derive(Debug, Copy, Clone)]
    struct ExpectedMemAccess {
        write: bool,
        addr: u64,
        size: u32,
    }

    #[derive(Debug, Copy, Clone)]
    struct ExpectedRegAccess {
        write: bool,
        reg: RegSpec,
    }

    #[derive(Debug, Copy, Clone)]
    struct UnexpectedRegChange {
        reg: RegSpec,
        before: u64,
        after: u64,
    }

    #[derive(Debug, Clone)]
    struct MemPatch {
        addr: u64,
        bytes: Vec<u8>,
    }

    struct AccessTestCtx<'a> {
        regs: &'a mut kvm_regs,
        vm: &'a Vm,
        preserve_rsp: bool,
        used_regs: [bool; 16],
        expected_reg: Vec<ExpectedRegAccess>,
        expected_mem: Vec<ExpectedMemAccess>,
    }

    impl<'a> AccessTestCtx<'a> {
        fn into_expectations(self) -> (Vec<ExpectedRegAccess>, Vec<ExpectedMemAccess>) {
            let AccessTestCtx {
                expected_reg,
                expected_mem,
                ..
            } = self;
            (expected_reg, expected_mem)
        }
    }

    use yaxpeax_arch::AddressBase;
    use yaxpeax_x86::long_mode::{RegSpec, behavior::AccessVisitor};
    use yaxpeax_x86::long_mode::register_class;

    impl<'a> AccessVisitor for AccessTestCtx<'a> {
        fn register_read(&mut self, reg: RegSpec) {
            self.expected_reg.push(ExpectedRegAccess {
                write: false,
                reg,
            });
        }
        fn register_write(&mut self, reg: RegSpec) {
            self.expected_reg.push(ExpectedRegAccess {
                write: true,
                reg,
            });
        }
        fn get_register(&mut self, reg: RegSpec) -> Option<u64> {
            self.register_read(reg);

            let cls = reg.class();
            match cls {
                register_class::B | register_class::W | register_class::D | register_class::Q => {
                    static KVM_REG_LUT: [usize; 16] = [
                        0, 2, 3, 1, 6, 7, 4, 5,
                        8, 9, 10, 11, 12, 13, 14, 15,
                    ];
                    let kvm_reg_nr = KVM_REG_LUT[reg.num() as usize];

                    // some ridiculous circumstances require us to not permute rsp, even
                    // though we *would* set it to a mapped address.
                    let allocated = self.used_regs[reg.num() as usize] ||
                        (reg.num() == RegSpec::rsp().num() && self.preserve_rsp);

                    if allocated {
                        let value = unsafe {
                            (self.regs as *mut _ as *mut u64).offset(kvm_reg_nr as isize).read()
                        };
                        Some(value)
                    } else {
                        // register value allocation is done .. carefully.
                        //
                        // see the comment on `map_test_mem` about why these numbers make any
                        // sense.
                        let value = 0x1_0000_0000 + (kvm_reg_nr as u64 + 1) * 0x0200;
                        unsafe {
                            (self.regs as *mut _ as *mut u64).offset(kvm_reg_nr as isize).write(value);
                        }
                        self.used_regs[reg.num() as usize] = true;
                        Some(value)
                    }
                }
                other => {
                    panic!("unexpected VcpuExit: {:?}", other);
                }
            }
        }
        fn memory_read(&mut self, address: Option<u64>, size: u32) {
            let acc = ExpectedMemAccess {
                write: false,
                addr: address.expect("can compute expected address"),
                size,
            };
            self.expected_mem.push(acc);
        }
        fn memory_write(&mut self, address: Option<u64>, size: u32) {
            let acc = ExpectedMemAccess {
                write: true,
                addr: address.expect("can compute expected address"),
                size,
            };
            self.expected_mem.push(acc);
        }
    }

    fn run(vm: &mut Vm) {
        let mut exits = 0;
        let end_pc = loop {
//            eprintln!("about to run! here's some state:");
/*
            let regs = vm.get_regs().unwrap();
            unsafe {
            let bytes = vm.host_ptr(GuestAddress(regs.rip));
            let slc = std::slice::from_raw_parts(bytes, 15);
            let decoded = yaxpeax_x86::long_mode::InstDecoder::default()
                .decode_slice(slc);
            if let Ok(decoded) = decoded {
                eprintln!("step. next: {:06x}: {}", regs.rip, decoded);
            }
            }
*/

//            dump_regs(&regs);
//            let sregs = vm.get_sregs().unwrap();
//            eprintln!("sregs: {:?}", sregs);
            let exit = vm.run().expect("can run vcpu");
            exits += 1;
            match exit {
                VcpuExit::MmioRead { addr, buf } => {
                    eprintln!("mmio: [{:08x}:{}] <- ..", addr, buf.len());
                    // TODO: with expected memory accesses we should be able to perhaps pick some
                    // values ahead of time and permute them (so as to tickle flags changes in
                    // `add [rcx], rdi` for example.
                    buf.fill(1);
                }
                VcpuExit::MmioWrite { addr, buf } => {
                    eprintln!("mmio: .. -> [{:08x}:{}]", addr, buf.len());
                }
                VcpuExit::Debug { pc, info } => {
                    let regs = vm.get_regs().unwrap();
                    dump_regs(&regs);
                    unsafe {
                        let bytes = vm.host_ptr(GuestAddress(pc));
                        let slc = std::slice::from_raw_parts(bytes, 15);
                        let decoded = yaxpeax_x86::long_mode::InstDecoder::default()
                            .decode_slice(slc);
                        if let Ok(decoded) = decoded {
                            eprintln!("step. next: {:06x}: {}", info.pc, decoded);
                        } else {
                            eprintln!("garbage @ {:06x}", info.pc);
                        }
                    }
                    // break info.pc;
                }
                VcpuExit::Hlt => {
                    // eprintln!("hit hlt");
                    let regs = vm.get_regs().unwrap();
                    // dump_regs(&regs);
                    break regs.rip;
                }
                other => {
                    eprintln!("unhandled exit: {:?} ... after {}", other, exits);
                    let regs = vm.get_regs().unwrap();
                    dump_regs(&regs);
                    let sregs = vm.get_sregs().unwrap();
                    eprintln!("sregs: {:?}", sregs);
                    panic!("stop");
                }
            }
        };

        eprintln!("run exits at {:08x}", end_pc);
        return;
    }

    fn dump_regs(regs: &kvm_regs) {
        eprintln!("rip              flags            ");
        eprintln!("{:016x} {:016x}", regs.rip, regs.rflags);
        eprintln!("rax              rcx              rdx              rbx");
        eprintln!("{:016x} {:016x} {:016x} {:016x}", regs.rax, regs.rcx, regs.rdx, regs.rbx);
        eprintln!("rsp              rbp              rsi              rdi");
        eprintln!("{:016x} {:016x} {:016x} {:016x}", regs.rsp, regs.rbp, regs.rsi, regs.rdi);
        eprintln!("r8               r9               r10              r11");
        eprintln!("{:016x} {:016x} {:016x} {:016x}", regs.r8, regs.r9, regs.r10, regs.r11);
        eprintln!("r12              r13              r14              r15");
        eprintln!("{:016x} {:016x} {:016x} {:016x}", regs.r12, regs.r13, regs.r14, regs.r15);
    }

    fn run_with_mem_checks(vm: &mut Vm, expected_end: u64, mem_patches: &[MemPatch]) -> Result<(), Exception> {
        for chunk in 0..=8 {
            let base = TEST_MEM_BASE.0 + 0x1_0000_0000 * chunk;
            vm.mem_slice_mut(GuestAddress(base), TEST_MEM_SIZE).fill(0xaa);
        }
        // test environments may require constants in memory at known locations (say, in support of
        // an LGDT test). apply those patches now that we've initialized all extra memory.
        for patch in mem_patches {
            let slice = vm.mem_slice_mut(GuestAddress(patch.addr), patch.bytes.len() as u64);
            slice.copy_from_slice(patch.bytes.as_slice());
        }
        let mut exits = 0;
        let end_pc = loop {
//            eprintln!("about to run! here's some state:");
            let regs = vm.get_regs().unwrap();
//            dump_regs(&regs);
//            let sregs = vm.get_sregs().unwrap();
//            eprintln!("sregs: {:?}", sregs);
            let exit = vm.run().expect("can run vcpu");
            exits += 1;
            match exit {
                VcpuExit::MmioRead { addr, buf } => {
                    panic!("shoud not be mmio accesses anymore");
                }
                VcpuExit::MmioWrite { addr, buf } => {
                    panic!("shoud not be mmio accesses anymore");
                }
                VcpuExit::Debug { pc, info } => {
                    break pc;
                }
                VcpuExit::Exception { nr } => {
                    return Err(Exception::vector(nr));
                }
                VcpuExit::Hlt => {
                    let regs = vm.get_regs().unwrap();
                    break regs.rip;
                }
                other => {
                    eprintln!("unhandled exit: {:?} ... after {}", other, exits);
                    let regs = vm.get_regs().unwrap();
                    eprintln!("regs: {:?}", regs);
//                    let sregs = vm.get_sregs().unwrap();
//                    eprintln!("sregs: {:?}", sregs);
                    panic!("stop");
                }
            }
        };

        if end_pc != expected_end - 1 && end_pc != expected_end {
            panic!("single-step ended at {:08x}, expected {:08x}", end_pc, expected_end);
        }

        /*
        if !unexpected_mem.is_empty() {
            eprintln!("memory access surprise!");
            if expected_mem.is_empty() {
                eprintln!("expected none");
            } else {
                eprintln!("expected:");
                for acc in expected_mem.iter() {
                    let rw = if acc.write { "write:" } else { " read:" };
                    eprintln!("  {} {} bytes at {:08x}", rw, acc.size, acc.addr);
                }
            }
            eprintln!("unexpected:");
            for (write, addr, size) in unexpected_mem {
                let rw = if write { "write:" } else { " read:" };
                eprintln!("  {} {} bytes at {:08x}", rw, size, addr);
            }
            panic!("stop");
        }
        */
        return Ok(());
    }

    fn check_contains(larger: RegSpec, smaller: RegSpec) -> bool {
        if larger == smaller {
            return true;
        } else if larger.class() == smaller.class() {
            // no registers in the same class alias
            return false;
        } else {
            match (larger.class(), smaller.class()) {
                (register_class::Q, register_class::Q) |
                (register_class::Q, register_class::D) |
                (register_class::Q, register_class::W) |
                (register_class::Q, register_class::RB) |
                (register_class::D, register_class::D) |
                (register_class::D, register_class::W) |
                (register_class::D, register_class::RB) |
                (register_class::W, register_class::W) |
                (register_class::W, register_class::RB) |
                (register_class::RB, register_class::RB) => {
                    larger.num() == smaller.num()
                }
                (register_class::Q, register_class::B) |
                (register_class::D, register_class::B) |
                (register_class::W, register_class::B) => {
                    // top bit selects high/low half of *x registers, so mask it and compare
                    smaller.num() & 0b11 == larger.num()
                }
                (register_class::RFLAGS, _) |
                (_, register_class::RFLAGS) => {
                    false
                }
                (register_class::RIP, _) |
                (_, register_class::RIP) => {
                    false
                }
                (register_class::S, _) |
                (_, register_class::S) => {
                    false
                }
                (l, s) => {
                    panic!("unhandled register-contains test: {:?}/{:?}", l, s);
                }
            }
        }
    }
    fn reg_mask(reg: RegSpec) -> u64 {
        match reg.class() {
            register_class::B => {
                // non-rex byte regs are al, cl, dl, bl, ah, ch, dh, bh
                let mask = if reg.num() < 4 {
                    0xff
                } else {
                    0xff00
                };
                mask
            },
            // but rex byte regs are all low-byte
            register_class::RB => 0xff,
            register_class::W => 0xffff,
            // x86_64 zero-extends 32-bit writes to 64-bit, so writes to "32-bit" registers still
            // are fully-clobbers.
            register_class::D => 0xffffffff_ffffffff,
            register_class::Q => 0xffffffff_ffffffff,
            register_class::RFLAGS => 0xffffffff_ffffffff,
            register_class::S => 0xffff,
            other => {
                panic!("unhandled register class: {:?}", other);
            }
        }
    }

    fn verify_seg(
        unexpected_regs: &mut Vec<UnexpectedRegChange>, expected_regs: &[ExpectedRegAccess],
        changed_reg: RegSpec, before: u16, after: u16,
    ) {
        verify_reg(unexpected_regs, expected_regs, changed_reg, before as u64, after as u64)
    }

    fn verify_reg(
        unexpected_regs: &mut Vec<UnexpectedRegChange>, expected_regs: &[ExpectedRegAccess],
        changed_reg: RegSpec, before: u64, after: u64,
    ) {
        // the same GPR may appear by different names in `expected_regs`, like as in `xchg ah, al`.
        // so, compute a diff here and poke out bits as the diff can be accounted for.
        let mut diff = before ^ after;
        if diff != 0 {
            // could be a write. full write? maybe!
            for e in expected_regs.iter() {
                if !e.write {
                    continue;
                }

                if !check_contains(changed_reg, e.reg) {
                    continue;
                }

                diff &= !reg_mask(e.reg);
            }

            if diff != 0 {
                unexpected_regs.push(UnexpectedRegChange {
                    reg: changed_reg,
                    before,
                    after,
                });
            }
        }
    }

    fn verify_dontcares(written_regs: &[RegSpec], initial_after_regs: &kvm_regs, now_after_regs: &kvm_regs) {
        let mut bad = false;

        for reg in written_regs.iter() {
            assert_eq!(reg.class(), register_class::Q);

            static KVM_REG_LUT: [usize; 16] = [
                0, 2, 3, 1, 6, 7, 4, 5,
                8, 9, 10, 11, 12, 13, 14, 15,
            ];
            let kvm_reg_nr = KVM_REG_LUT[reg.num() as usize];

            let initial_after = unsafe {
                (initial_after_regs as *const _ as *const u64).offset(kvm_reg_nr as isize).read()
            };

            let now_after = unsafe {
                (now_after_regs as *const _ as *const u64).offset(kvm_reg_nr as isize).read()
            };

            if initial_after != now_after {
                eprintln!("register {} changed after permuting dontcares: {:016x} => {:016x}",
                    reg, initial_after, now_after);
                bad = true;
            }
        }

        if bad {
            panic!("cared about dontcares");
        }
    }

        fn compute_dontcares(vm: &Vm, accesses: &[ExpectedRegAccess]) -> Vec<RegSpec> {
            // use a bitmap for dontcares, mask out bits as registers are seen to be read.
            let mut reg_bitmap: u32 = 0xffffffff;

            fn reg_to_gpr(reg: RegSpec) -> Option<u8> {
                match reg.class() {
                    register_class::Q |
                    register_class::D |
                    register_class::W |
                    register_class::RB => {
                        Some(reg.num())
                    }
                    register_class::B => {
                        Some(reg.num() & 0b11)
                    }
                    _ => {
                        None
                    }
                }
            }

            if vm.idt_configured() {
                reg_bitmap &= !(1 << (RegSpec::rsp().num()));
            }

            for acc in accesses.iter() {
                if acc.write && acc.reg.class().width() >= 4 {
                    // TODO: if a write goes to a subset of a GPR, the dontcare part is *only* the
                    // written part. currently dontcares are reported as the full width, so
                    // subsequent steps permute the non-written part and trip over it in
                    // verify_dontcares.
                    //
                    // for now, only dontcare if the written register would be a full write (4
                    // bytes, zero-extended to 8, or actually a write to all 8 bytes).
                    continue;
                }

                if let Some(gpr_num) = reg_to_gpr(acc.reg) {
                    reg_bitmap &= !(1 << gpr_num);
                }
            }

            let mut regs = Vec::new();

            for i in 0..16 {
                if reg_bitmap & (1 << i) != 0 {
                    regs.push(RegSpec::q(i));
                }
            }

            regs
        }

        fn compute_writes(accesses: &[ExpectedRegAccess]) -> Vec<RegSpec> {
            // same as dontcares, isk
            let mut reg_bitmap: u32 = 0x00000000;

            fn reg_to_gpr(reg: RegSpec) -> Option<u8> {
                match reg.class() {
                    register_class::Q |
                    register_class::D |
                    register_class::W |
                    register_class::RB => {
                        Some(reg.num())
                    }
                    register_class::B => {
                        Some(reg.num() & 0b11)
                    }
                    _ => {
                        None
                    }
                }
            }

            for acc in accesses.iter() {
                if !acc.write {
                    continue;
                }

                if let Some(gpr_num) = reg_to_gpr(acc.reg) {
                    reg_bitmap |= 1 << gpr_num;
                }
            }

            let mut regs = Vec::new();

            for i in 0..16 {
                if reg_bitmap & (1 << i) != 0 {
                    regs.push(RegSpec::q(i));
                }
            }

            regs
        }

        fn permute_dontcares(dontcare_regs: &[RegSpec], regs: &mut kvm_regs) {
            let mut rng = rand::rng();

            for reg in dontcare_regs {
                assert_eq!(reg.class(), register_class::Q);

                static KVM_REG_LUT: [usize; 16] = [
                    0, 2, 3, 1, 6, 7, 4, 5,
                    8, 9, 10, 11, 12, 13, 14, 15,
                ];
                let kvm_reg_nr = KVM_REG_LUT[reg.num() as usize];
                let rand = rng.next_u64();
                unsafe {
                    (regs as *mut _ as *mut u64).offset(kvm_reg_nr as isize).write(rand);
                }
            }
        }

        fn permute_memdontcare(expected_mem: &[ExpectedMemAccess], vm: &mut Vm) {
            for acc in expected_mem.iter() {
                if acc.write {
                    continue;
                }

                /*
                 * WRONG
                let mut buf = vec![0; acc.size as usize];
                let mut rng = rand::rng();
                rng.fill(&mut buf);

                if acc.addr >= 0x1_0000_0000 {
                    vm.write_testmem(GuestAddress(acc.addr), buf.as_slice());
                } else {
                    // check we're not going to "permute" page tables or something.
                    // instruction text might get clobbered, which would be Weird, but..
                    assert!(acc.addr > vm.page_table_addr().0 + 2 * 0x1000);
                    vm.write_mem(GuestAddress(acc.addr), buf.as_slice());
                }
                */
            }
        }

    fn verify_mem_changes(
        expected_mem: &[ExpectedMemAccess],
        vm: &mut Vm,
    ) {
        // test the expected writes by process of elimination: reset any expected-to-be-written
        // areas to the initial pattern. then, anything in test memory that is not the default
        // pattern must have been an unexpected write.
        for acc in expected_mem {
            if !acc.write {
                continue;
            }

            unsafe {
                let slice = vm.mem_slice_mut(GuestAddress(acc.addr), acc.size as u64);
                slice.fill(0xaa);
            }
        }

        struct MemoryDiff {
            addr: GuestAddress,
            bytes: Vec<u8>,
        }

        use std::fmt;

        impl fmt::Display for MemoryDiff {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "diff at 0x{:08x}: ", self.addr.0)?;
                for b in self.bytes.iter() {
                    write!(f, "{:02x}", b)?;
                }
                Ok(())
            }
        }

        let mut unexpected_acc = Vec::new();
        let mut current_diff: Option<MemoryDiff> = None;

        for mem_hunk in 0..=8 {
            let base = GuestAddress(TEST_MEM_BASE.0 * (mem_hunk + 1));
            let test_mem = unsafe { vm.mem_slice(base, TEST_MEM_SIZE) };
            for i in 0..test_mem.len() {
                if test_mem[i] != 0xaa {
                    if let Some(mut diff) = current_diff.take() {
                        const CHUNK_SIZE: u64 = 128 * 1024;

                        let prev_diff_start = diff.addr.0 % CHUNK_SIZE;
                        let prev_diff_tail = prev_diff_start + diff.bytes.len() as u64;
                        let continuation = i as u64 == prev_diff_tail + 1;
                        if continuation {
                            diff.bytes.push(test_mem[i]);
                        } else {
                            unexpected_acc.push(diff);

                            let guest_addr = (mem_hunk + 1) * 0x1_0000_0000 + i as u64;
                            current_diff = Some(MemoryDiff {
                                addr: GuestAddress(guest_addr as u64),
                                bytes: vec![test_mem[i]],
                            });
                        }
                    } else {
                        let guest_addr = (mem_hunk + 1) * 0x1_0000_0000 + i as u64;
                        current_diff = Some(MemoryDiff {
                            addr: GuestAddress(guest_addr as u64),
                            bytes: vec![test_mem[i]],
                        });
                    }
                }
            }

            if let Some(diff) = current_diff.take() {
                unexpected_acc.push(diff);
            }
        }

        if !unexpected_acc.is_empty() {
            for diff in unexpected_acc {
                eprintln!("{}", diff);
            }
            panic!("unexpected memory accesses!");
        }
    }

    fn verify_reg_changes(
        expected_regs: &[ExpectedRegAccess],
        before_regs: &kvm_regs, after_regs: &kvm_regs,
        before_sregs: &kvm_sregs, after_sregs: &kvm_sregs
    ) {
        let mut unexpected_regs = Vec::new();

        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rax(), before_regs.rax, after_regs.rax);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rcx(), before_regs.rcx, after_regs.rcx);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rdx(), before_regs.rdx, after_regs.rdx);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rbx(), before_regs.rbx, after_regs.rbx);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rsp(), before_regs.rsp, after_regs.rsp);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rbp(), before_regs.rbp, after_regs.rbp);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rsi(), before_regs.rsi, after_regs.rsi);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rdi(), before_regs.rdi, after_regs.rdi);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r8(), before_regs.r8, after_regs.r8);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r9(), before_regs.r9, after_regs.r9);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r10(), before_regs.r10, after_regs.r10);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r11(), before_regs.r11, after_regs.r11);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r12(), before_regs.r12, after_regs.r12);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r13(), before_regs.r13, after_regs.r13);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r14(), before_regs.r14, after_regs.r14);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::r15(), before_regs.r15, after_regs.r15);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::rflags(), before_regs.rflags, after_regs.rflags);

        verify_seg(&mut unexpected_regs, &expected_regs, RegSpec::cs(), before_sregs.cs.selector, after_sregs.cs.selector);
        verify_seg(&mut unexpected_regs, &expected_regs, RegSpec::ds(), before_sregs.ds.selector, after_sregs.ds.selector);
        verify_seg(&mut unexpected_regs, &expected_regs, RegSpec::es(), before_sregs.es.selector, after_sregs.es.selector);
        verify_seg(&mut unexpected_regs, &expected_regs, RegSpec::fs(), before_sregs.fs.selector, after_sregs.fs.selector);
        verify_seg(&mut unexpected_regs, &expected_regs, RegSpec::gs(), before_sregs.gs.selector, after_sregs.gs.selector);
        verify_seg(&mut unexpected_regs, &expected_regs, RegSpec::ss(), before_sregs.ss.selector, after_sregs.ss.selector);

        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::cr0(), before_sregs.cr0, after_sregs.cr0);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::cr2(), before_sregs.cr2, after_sregs.cr2);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::cr3(), before_sregs.cr3, after_sregs.cr3);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::cr4(), before_sregs.cr4, after_sregs.cr4);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::cr8(), before_sregs.cr8, after_sregs.cr8);

        if !unexpected_regs.is_empty() {
            eprintln!("unexpected reg changes:");
            for change in unexpected_regs {
                eprintln!("  {}: {:08x} -> {:08x}", change.reg.name(), change.before, change.after);
            }
            panic!("stop");
        }
    }

    // check the side effects of the instruction that `regs.rip` points to. the side effects are
    // enumerated across `expected_reg` and `expected_mem`. if this instruction instead raises an
    // exception, return that instead.
    //
    // TODO: it's possible that this instruction permuts either the instruction bytes or vCPU
    // control structures (GDT, IDT, or page tables). these could be made read-only, but then we'd
    // need to verify that these structures are not modified via Weird Different Mapping or
    // whatever. such a mapping shouldn't exist anyway. but making these read-only also implies
    // moving the stack elsewhere, and the stack would have to be zeroed to not introduce Weirdness
    // across permutations too.
    fn check_side_effects(
        vm: &mut Vm, regs: &kvm_regs, sregs: &kvm_sregs,
        mem_patches: &[MemPatch],
        expected_end: u64, expected_reg: &[ExpectedRegAccess], expected_mem: &[ExpectedMemAccess]
    ) -> Result<(kvm_regs, kvm_sregs), Exception> {
        run_with_mem_checks(vm, expected_end, mem_patches)?;

        let after_regs = vm.get_regs().unwrap();
        let after_sregs = vm.get_sregs().unwrap();

        verify_reg_changes(&expected_reg, &regs, &after_regs, &sregs, &after_sregs);
        verify_mem_changes(&expected_mem, vm);

        Ok((after_regs, after_sregs))
    }

    // run the VM a few times permuting the "dontcare" registers each time and checking that we
    // really did not care about them. "4" steps is of course arbitrary, but makes for some kind of
    // confidence about flag registers in particular, probably.
    fn test_dontcares(
        vm: &mut Vm, regs: &mut kvm_regs, sregs: &kvm_sregs,
        mem_patches: &[MemPatch],
        expected_end: u64, expected_reg: &[ExpectedRegAccess], expected_mem: &[ExpectedMemAccess],
        dontcare_regs: &[RegSpec], written_regs: &[RegSpec],
        first_after_regs: &kvm_regs, _first_after_sregs: &kvm_sregs
    ) -> Result<(), Exception> {
        for _ in 0..4 {
            permute_dontcares(dontcare_regs, regs);
            // TODO:
            // permute_memread(expected_mem, vm);

            vm.set_regs(&regs).unwrap();

            let (after_regs, _after_sregs) = check_side_effects(
                vm, &regs, &sregs,
                mem_patches,
                expected_end, expected_reg, expected_mem
            )?;

            verify_dontcares(written_regs, &first_after_regs, &after_regs);
        }

        Ok(())
    }

    fn inrange_displacements(vm: &Vm, inst: &long_mode::Instruction) -> bool {
        // see comment on `map_test_mem`. this limit is partially used to figure out what memory
        // must be backed by real memory vs holes that can have mmio traps.
        let disp_lim = 511;

        let ops = match inst.behavior().all_operands() {
            Ok(ops) => ops,
            Err(e) => {
                // TODO: is it true that all ComplexOp do not have displacements?
                return true;
            }
        };
        for op in ops.iter().operands() {
            let disp = match op {
                long_mode::Operand::AbsoluteU32 { .. } |
                long_mode::Operand::AbsoluteU64 { .. } => {
                    return false;
                }
                long_mode::Operand::Disp { disp, .. } => disp,
                long_mode::Operand::MemIndexScaleDisp { disp, .. } => disp,
                long_mode::Operand::MemBaseIndexScaleDisp { disp, .. } => disp,
                long_mode::Operand::DispMasked { disp, .. } => disp,
                long_mode::Operand::MemIndexScaleDispMasked { disp, .. } => disp,
                long_mode::Operand::MemBaseIndexScaleDispMasked { disp, .. } => disp,
                _ => {
                    continue;
                }
            };

            if disp > disp_lim {
                return false;
            }
        }

        true
    }

    fn check_behavior(vm: &mut Vm, inst: &[u8]) {
        check_behavior_with_regs(vm, inst, [false; 16], &[])
    }

    // `reg_preserves` declares a set of registers, numbered by their *Linux KVM API number*, as in
    // the position in `kvm_regs`, that must be preserved by the test.
    fn check_behavior_with_regs(vm: &mut Vm, inst: &[u8], reg_preserves: [bool; 16], mem_patches: &[MemPatch]) {
        let mut insts = inst.to_vec();
        // cap things off with a `hlt` to work around single-step sometimes .. not? see comment on
        // set_single_step. this ensures that even if single-stepping doesn't do the needful, the
        // next address _will_ get the vCPU back out to us.
        //
        // this obviously doesn't work if code is overwritten (so really [TODO] the first page
        // should be made non-writable), and doesn't work if the one executed instruction is a
        // call, jump, etc. in those cases the instruction doesn't rmw memory .. .except for
        // call/ret, where the `rsp` access might. so we might have to just have to skip them?
        //
        // alternatively, probably should set up the IDT such that there's a handler for the
        // exception raised by `TF` that just executes hlt. then everything other than popf will
        // work out of the box and popf can be caught by kvm single-stepping.
        insts.push(0xf4);
        let decoded = yaxpeax_x86::long_mode::InstDecoder::default()
            .decode_slice(inst).expect("can decode");
        use yaxpeax_arch::LengthedInstruction;
        assert_eq!(insts.len(), 0.wrapping_offset(decoded.len()) as usize + 1);

        if !inrange_displacements(vm, &decoded) {
            panic!("unable to test '{}': displacement(s) are larger than test VM memory.", decoded);
        }

        let behavior = decoded.behavior();
        eprintln!("checking behavior of {}", decoded);

        // TODO: this does an infinite loop when run??
        if decoded.opcode() == long_mode::Opcode::FLDCW {
            return;
        }

        let sregs = vm.get_sregs().unwrap();
        let mut regs = vm.get_regs().unwrap();
        // vm.set_single_step(true).expect("can enable single-step");
        vm.program(insts.as_slice(), &mut regs);

        let mut rng = rand::rng();

        // a set of registers whose values we are directed to care about. these are subtracted from
        // dontcares, later.
        let mut cares = Vec::new();

        if !reg_preserves[0] {
            regs.rax = rng.next_u64();
        } else {
            cares.push(RegSpec::rax());
        }
        if !reg_preserves[1] {
            regs.rbx = rng.next_u64();
        } else {
            cares.push(RegSpec::rbx());
        }
        if !reg_preserves[2] {
            regs.rcx = rng.next_u64();
        } else {
            cares.push(RegSpec::rcx());
        }
        if !reg_preserves[3] {
            regs.rdx = rng.next_u64();
        } else {
            cares.push(RegSpec::rdx());
        }
        if !reg_preserves[4] {
            regs.rsi = rng.next_u64();
        } else {
            cares.push(RegSpec::rsi());
        }
        if !reg_preserves[5] {
            regs.rdi = rng.next_u64();
        } else {
            cares.push(RegSpec::rdi());
        }
        if !vm.idt_configured() {
            if !reg_preserves[6] {
                regs.rsp = rng.next_u64();
            } else {
                cares.push(RegSpec::rsp());
            }
        }
        if !reg_preserves[7] {
            regs.rbp = rng.next_u64();
        } else {
            cares.push(RegSpec::rbp());
        }

        regs.r8 = rng.next_u64();
        regs.r9 = rng.next_u64();
        regs.r10 = rng.next_u64();
        regs.r11 = rng.next_u64();
        regs.r12 = rng.next_u64();
        regs.r13 = rng.next_u64();
        regs.r14 = rng.next_u64();
        regs.r15 = rng.next_u64();

        let mut ctx = AccessTestCtx {
            regs: &mut regs,
            vm,
            // if an interrupt handler is initialized with rsp pointing to addresses that cause
            // MMIO exits the vcpu ends up in a loop doing nothing particularly interesting
            // (seemingly in a loop trying to raise #UD after resetting?). this is a Linux issue
            // i'm not tracking down right now. instead, if the IDT is initialized then keep the
            // rsp pointed somewhere "normal" so that exceptions still work right.
            //
            // to reproduce this issue, set this to `false` unconditionally, then run
            // `kvm_verify_popmem`. it will infinite loop in the kernel and you'll see
            // x86_decode_emulated_instruction failing over and over and over and ...
            preserve_rsp: vm.idt_configured(),
            used_regs: reg_preserves,
            expected_reg: Vec::new(),
            expected_mem: Vec::new(),
        };
        behavior.visit_accesses(&mut ctx).expect("can visit accesses");
        let (expected_reg, mut expected_mem) = ctx.into_expectations();
        for patch in mem_patches.iter() {
            expected_mem.push(ExpectedMemAccess {
                addr: patch.addr,
                size: patch.bytes.len() as u32,
                write: true,
            });
        }

        let mut dontcare_regs = compute_dontcares(&vm, &expected_reg);
        dontcare_regs.retain(|reg| {
            !cares.iter().any(|care| check_contains(*care, *reg))
        });
        let written_regs = compute_writes(&expected_reg);

        permute_dontcares(dontcare_regs.as_slice(), &mut regs);

//        eprintln!("setting regs to: {:?}", regs);
        vm.set_regs(&regs).unwrap();

        let expected_end = regs.rip + insts.len() as u64;

        let (after_regs, after_sregs) = match check_side_effects(
            vm, &regs, &sregs,
            mem_patches,
            expected_end, &expected_reg, &expected_mem
        ) {
            Ok((a, b)) => (a, b),
            Err(other) => {
                let vm_regs = vm.get_regs().unwrap();
                let vm_sregs = vm.get_sregs().unwrap();
                let mut prev_rip = [0u8; 8];
                vm.read_mem(GuestAddress(vm_regs.rsp + 8), &mut prev_rip[..]);
                let mut buf = [0u8; 8];
                vm.read_mem(GuestAddress(vm_regs.rsp), &mut buf[..]);
                if other == Exception::PF {
                    eprintln!(
                        "error code: {:#08x} accessing {:016x} @ rip={:#016x} (cr3={:016x})",
                        u64::from_le_bytes(buf), vm_sregs.cr2,
                        u64::from_le_bytes(prev_rip), vm_sregs.cr3
                    );
                    let mut pdpt = [0u8; 4096];
                    vm.read_mem(vm.page_tables().pdpt_addr(), &mut pdpt[..]);
                    eprintln!("pdpt: {:x?}", &pdpt[..8]);
                } else if other == Exception::GP {
                    if decoded.opcode() == long_mode::Opcode::MOV {
                        // TODO: should be in the exception list
                        if let long_mode::Operand::Register { reg } = decoded.operand(0) {
                            if reg.class() == long_mode::register_class::S {
                                // mov to segment selector can #GP if the selector is invalid:
                                // > If the DS, ES, FS, or GS register is being loaded and the
                                // > segment pointed to is not a data or readable code segment.
                                return;
                            }
                        }
                    }
                }
                dump_regs(&vm_regs);
                panic!("TODO: handle exceptions ({:?})", other);
            }
        };

        let res = test_dontcares(
            vm, &mut regs, &sregs,
            mem_patches,
            expected_end, expected_reg.as_slice(), expected_mem.as_slice(),
            dontcare_regs.as_slice(), written_regs.as_slice(),
            &after_regs, &after_sregs
        );

        match res {
            Ok(()) => {
            }
            Err(Exception::PF) => {
            }
            Err(other) => {
                panic!("TODO: handle exceptions ({:?})", other);
            }
        }
    }

    const TEST_MEM_BASE: GuestAddress = GuestAddress(0x1_0000_0000);
    const TEST_MEM_SIZE: u64 = 128 * 1024;

    // we need to keep accesses from falling into mapped-but-not-backed regions
    // of guest memory, so we don't get MMIO exits (which would just test
    // Linux's x86 emulation). control structures are at in the low 1G (really 1M)
    // of memory, which memory references under test should not touch.
    //
    // we'll limit displacements to 511 (arbitrary), which means that 512-byte
    // increments of 1..16 can distinguish registers. given SIB addressing the
    // highest address that can be formed is something like...
    //
    // > (1G + 15 * 512) + (1G + 16 * 512) * 8 + 512
    //
    // or just under 9G + 16k. that access *could* be a wide AVX-512 situation,
    // so the highest byte addressed can be a few bytes later.
    //
    // this can be read as "the first 32k at each 1G may be accessed", but only
    // GB boundaries at 1, 2, 3, 5, and 9 can be accessed in this way (non-SIB,
    // then SIB with scale = 1, 2, 4, 8).
    //
    // while memory is Yikes Expensive, setting up 128k at each 1G offset that might be
    // accessed is only 1M 128K, so that's what we'll do  here.
    fn map_test_mem(vm: &mut asmlinator::x86_64::Vm) {
        let mut base = TEST_MEM_BASE.0;
        for _ in 0..=8 {
            vm.add_memory(GuestAddress(base), TEST_MEM_SIZE).expect("can add test mem region");
            base += 0x1_0000_0000;
        }
    }

    fn create_test_vm() -> asmlinator::x86_64::Vm {
        let mut vm = Vm::create(1024 * 1024).expect("can create vm");

        map_test_mem(&mut vm);
        unsafe {
            vm.configure_identity_paging(None);
        }

        vm
    }

    #[test]
    fn kvm_verify_xor_reg_mem() {
        let mut vm = create_test_vm();

        // `xor rax, [rcx]`. this works. great!
        let inst: &'static [u8] = &[0x33, 0x01];
        check_behavior(&mut vm, inst);

        // `xor al, [rcx]`. also works. cool!
        let inst: &'static [u8] = &[0x32, 0x01];
        check_behavior(&mut vm, inst);

        // `xor [rcx], al`. this runs until the VM starts executing in MMIO space and
        // VcpuExit::Shutdown. what.
        let inst: &'static [u8] = &[0x30, 0x01];
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn kvm_verify_inc() {
        let mut vm = create_test_vm();

        // `inc eax`
        let inst: &'static [u8] = &[0xff, 0xc0];
        check_behavior(&mut vm, inst);

        // `inc dword [rax]`
        let inst: &'static [u8] = &[0xff, 0x00];
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn kvm_verify_push() {
        let mut vm = create_test_vm();

        // `push rax`
        let inst: &'static [u8] = &[0x50];
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn kvm_verify_popmem() {
        let mut vm = create_test_vm();

        // `pop [rax]`
        let inst: &'static [u8] = &[0x8f, 0x00];
        check_behavior(&mut vm, &inst[0..2]);
    }

    // #[test]
    fn kvm_hugepage_bug() {
        let mut vm = create_test_vm();

        // `add [rsp], al; add [rcx], al; pop [rcx]; hlt`
        // the first instruction runs fine. the second instruction runs fine.
        // the third instruction gets a page fault at 0xf800? which worked fine for the add.
        // this turns out to be an issue in linux' paging64_gva_to_gpa() when the va is mapped with
        // huge pages.
        let inst: &'static [u8] = &[0x00, 0x04, 0x24, 0x00, 0x01, 0x8f, 0x01, 0xf4];
        let mut regs = vm.get_regs().unwrap();
        regs.rax = 0x00000002_00100000;
        regs.rcx = 0x00000002_00100000;
        vm.program(inst, &mut regs);
        vm.set_regs(&regs).unwrap();
        vm.set_single_step(true).expect("can enable single-step");
        run(&mut vm);

        let vm_regs = vm.get_regs().unwrap();
        let vm_sregs = vm.get_sregs().unwrap();
        let mut prev_rip = [0u8; 8];
        vm.read_mem(GuestAddress(vm_regs.rsp + 8), &mut prev_rip[..]);
        let mut buf = [0u8; 8];
        vm.read_mem(GuestAddress(vm_regs.rsp), &mut buf[..]);
        eprintln!(
            "error code: {:#08x} accessing {:016x} @ rip={:#016x} (cr3={:016x})",
            u64::from_le_bytes(buf), vm_sregs.cr2,
            u64::from_le_bytes(prev_rip), vm_sregs.cr3
        );
        if vm_regs.rip == 0x300f {
            let mut pdpt = [0u8; 4096];
            vm.read_mem(vm.page_tables().pdpt_addr(), &mut pdpt[..]);
            eprintln!("pdpt: {:x?}", &pdpt[..8]);
        }
        panic!("no");
    }

    #[test]
    fn kvm_verify_ret() {
        let mut vm = create_test_vm();

        // `ret`
        let inst: &'static [u8] = &[0xc3];
        // TODO: set up ret test to return to some other address. check_behavior() doesn't tolerate
        // this (yet).
        vm.write_mem(vm.stack_addr(), &0xff001u64.to_le_bytes());
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn kvm_verify_ins() {
        let mut vm = create_test_vm();

        // `ins byte [rdi], dl`
        let inst: &'static [u8] = &[0x6c];
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn test_pf() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::long_mode::InstDecoder;

        let mut vm = create_test_vm();

        let decoder = InstDecoder::default();

        let inst = &[
            // verr dx
            0x0f, 0x00, 0xe2,
            // verw dx
            0x0f, 0x00, 0xea,
            // jmpf mword [rcx]
            //
            // because there is no operand override prefix, this is am m16:32
            // (!  not 64 !) operand.
            0xff, 0x29,
            // hlt (unreached)
            0xf4,
        ];

        let do_fault = &[
            // nop then int3 to have clear evidence that the vcpu took the jmpf
            // and the GDT works, before using the IDT.
            0x90, 0xcc,
        ];

        let before_sregs = vm.get_sregs().unwrap();
        let mut regs = vm.get_regs().unwrap();

        vm.program(inst.as_slice(), &mut regs);

        vm.write_mem(GuestAddress(0x83000), do_fault);

        let int1_far_addr = &mut [0; 6];
        int1_far_addr[..4].copy_from_slice(&0x83000u32.to_le_bytes());
        int1_far_addr[4..].copy_from_slice(&before_sregs.cs.selector.to_le_bytes());
        vm.write_mem(GuestAddress(0x80000), int1_far_addr);

        regs.rdx = vm.selector_cs() as u64;
        regs.rcx = 0x80000;

        let mut reader = U8Reader::new(inst.as_slice());
        eprintln!("going to run...");
        let mut offset = regs.rip;
        loop {
            let decoded = decoder.decode(&mut reader).expect("can decode");
            use yaxpeax_arch::LengthedInstruction;
            eprintln!("{:04x}: {}", offset, decoded);
            offset = offset.wrapping_offset(decoded.len());
            if decoded.opcode() == yaxpeax_x86::long_mode::Opcode::HLT {
                break;
            }
        }

        vm.set_regs(&regs).unwrap();

        vm.set_single_step(true).expect("can enable single-step");

        run(&mut vm);

// TODO: capture the VcpuExit::Exception
//        let intr_exit = exception_exit(&vm);
//        assert_eq!(intr_exit, Some(Exception::BP));
    }

    #[test]
    fn behavior_verify_kvm() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::long_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        // TODO: happen to be testing on a zen 5 system, so i picked a zen 5 decoder.
        let decoder = long_mode::uarch::amd::zen5();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        for word in 0..u16::MAX {
            let inst = word.to_le_bytes();
            let mut reader = U8Reader::new(&inst);
            if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                if [Opcode::FLDENV, Opcode::FNSTENV, Opcode::FRSTOR, Opcode::FNSAVE, Opcode::FNSTCW, Opcode::FNSTSW].contains(&buf.opcode()) {
                    // this needs a more targeted test
                    continue;
                }

                if [Opcode::INS, Opcode::MOVS, Opcode::OUTS, Opcode::LODS, Opcode::STOS, Opcode::CMPS, Opcode::SCAS].contains(&buf.opcode()) {
                    if buf.prefixes.rep_any() {
                        // `repnz cmps` will carry on for however long memory allows,
                        // `rep movs` runs `rcx`-many times, etc
                        continue;
                    }
                }

                if buf.opcode() == Opcode::RSM {
                    // SMM is kinda not our problem for now..
                    continue;
                }

                if buf.opcode() == Opcode::GETSEC {
                    // oh dear
                    continue;
                }

                if buf.opcode() == Opcode::RDTSC {
                    // the TSC keeps ticking so eax will change across runs and trip the
                    // "cared about dontcares" check.
                    continue;
                }

                if buf.opcode() == Opcode::RDPMC {
                    // reading a bogus PMC will just #GP so this needs a more specific test.
                    continue;
                }

                if buf.opcode() == Opcode::DIV || buf.opcode() == Opcode::IDIV {
                    // if the operand is in memory we're not currently permuting memory, so it
                    // might be zero and just #DE.
                    continue;
                }

                if buf.opcode() == Opcode::WRMSR || buf.opcode() == Opcode::RDMSR {
                    // TODO: ... okay come on.
                    continue;
                }
                if buf.opcode() == Opcode::RETURN {
                    // hard to handle generically here; see `verify_ret`.
                    continue;
                }
                if buf.opcode() == Opcode::LEAVE {
                    // TODO: trying to generically handle leave typically gets #SS from popping a
                    // non-canonical address. needs more specific test.
                    continue;
                }
                if buf.opcode() == Opcode::JMPF || buf.opcode() == Opcode::RETF || buf.opcode() == Opcode::CALLF {
                    // TODO: trying to is harder. needs more specific test.
                    continue;
                }
                if buf.opcode() == Opcode::INT {
                    // TODO: int is complex, but check_behavior() does not tolerate those yet
                    continue;
                }
                if buf.opcode() == Opcode::JMP || buf.opcode() == Opcode::CALL {
                    // TODO: needs more specific testing
                    continue;
                }
                if buf.opcode() == Opcode::JRCXZ || buf.opcode() == Opcode::LOOP || buf.opcode() == Opcode::LOOPZ || buf.opcode() == Opcode::LOOPNZ {
                    // TODO: also complex
                    continue;
                }
                if buf.opcode() == Opcode::IRET || buf.opcode() == Opcode::IRETD || buf.opcode() == Opcode::IRETQ {
                    // TODO: oh dear
                    continue;
                }
                if [Opcode::JO, Opcode::JNO, Opcode::JB, Opcode::JNB, Opcode::JZ, Opcode::JNZ, Opcode::JA, Opcode::JNA, Opcode::JS, Opcode::JNS, Opcode::JP, Opcode::JNP, Opcode::JL, Opcode::JGE, Opcode::JLE, Opcode::JG].contains(&buf.opcode()) {
                    // TODO: jmp-related tests that tolerate rip changing.
                    continue;
                }
                if [Opcode::SYSCALL, Opcode::SYSRET, Opcode::SYSENTER, Opcode::SYSEXIT].contains(&buf.opcode()) {
                    // TODO: syscall tests
                    continue;
                }
                if [Opcode::UD0, Opcode::UD1, Opcode::UD2].contains(&buf.opcode()) {
                    // TODO: ud tests, etc
                    continue;
                }

                if buf.opcode() == Opcode::CLTS {
                    // what happens here, access 0xff000?
                    continue;
                }
                // some instructions may just be one byte, so figure out the length and only check
                // that many bytes of instructions for specific behavior..
                use yaxpeax_arch::LengthedInstruction;
                let inst_len = 0.wrapping_offset(buf.len()) as usize;
                if inst_len == 1 {
                    eprintln!("checking behavior of {:02x}: {}", inst[0], buf);
                } else {
                    eprintln!("checking behavior of {:02x} {:02x}: {}", inst[0], inst[1], buf);
                }
                use yaxpeax_x86::long_mode::Opcode;
                // mov es, word [rax]
                // does an inf loop too...?
                if [Opcode::INS, Opcode::OUTS, Opcode::IN, Opcode::OUT].contains(&buf.opcode()) {
                    eprintln!("skipping {}", buf.opcode());
                    continue;
                }
                vm.set_regs(&initial_regs).unwrap();
                check_behavior(&mut vm, &inst[..inst_len]);
            }
        }
    }

    fn test_prelude(bytes: &[u8], instr: &Instruction) {
        let inst_len = 0.wrapping_offset(instr.len()) as usize;
        assert_eq!(inst_len, bytes.len());

        let mut bytes_text = String::new();
        for b in bytes.iter() {
            if !bytes_text.is_empty() {
                bytes_text.push_str(" ");
            }
            write!(bytes_text, "{:02x}", b).expect("can format");
        }
        eprintln!("checking behavior of {bytes_text}: {instr}");
    }

    #[test]
    fn behavior_verify_kvm_0f_() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::long_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        // TODO: happen to be testing on a zen 5 system, so i picked a zen 5 decoder.
        let decoder = long_mode::uarch::amd::zen5();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        for word in 0..u16::MAX {
            let inst = word.to_le_bytes();
            let bytes = [0x0f, inst[0], inst[1]];
            let mut reader = U8Reader::new(&bytes);
            if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                // two byte instructions were covered by `verify_kvm`, novel instructions are three
                // bytes (or longer..?)
                use yaxpeax_arch::LengthedInstruction;
                let inst_len = 0.wrapping_offset(buf.len()) as usize;
                if inst_len == 2 {
                    continue;
                }

                eprintln!("checking behavior of {:02x} {:02x}: {}", inst[0], inst[1], buf);

                /*
                use yaxpeax_x86::long_mode::Opcode;
                // mov es, word [rax]
                // does an inf loop too...?
                if [Opcode::INS, Opcode::OUTS, Opcode::IN, Opcode::OUT].contains(&buf.opcode()) {
                    eprintln!("skipping {}", buf.opcode());
                    continue;
                }
                */
                vm.set_regs(&initial_regs).unwrap();
                check_behavior(&mut vm, &bytes[..inst_len]);
            }
        }
    }

    // check the collection of {l,s}{g,i,l}dt. these instructions are at the combination of
    // "interesting memory size" and "interesting [non]interaction with prefixes"
    //
    // because these modify VM control structures, the VM is not retained across checks in a test.
    mod table_instrs {
        use super::create_test_vm;
        use super::test_prelude;
        use super::MemPatch;
        use super::check_behavior;
        use super::check_behavior_with_regs;

        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::long_mode::Instruction;
        use yaxpeax_x86::long_mode;

        #[test]
        fn verify_lgdt() {
            // TODO: happen to be testing on a zen 5 system, so i picked a zen 5 decoder.
            let decoder = long_mode::uarch::amd::zen5();
            let mut buf = Instruction::default();

            static INSTS: &'static [&'static [u8]] = &[
                // lgdt
                &[0x0f, 0x01, 0x10],
                &[0x66, 0x0f, 0x01, 0x10],
                &[0x67, 0x0f, 0x01, 0x10],
            ];

            for inst in INSTS {
                let mut vm = create_test_vm();
                vm.set_single_step(true).expect("can enable single-step");

                // set up lgdt to re-load the same gdt.
                let mut patch_bytes = Vec::new();
                patch_bytes.extend_from_slice(&4095u16.to_le_bytes());
                patch_bytes.extend_from_slice(&vm.gdt_addr().0.to_le_bytes());
                let patch = MemPatch {
                    addr: 0x1_0000_0000,
                    bytes: patch_bytes,
                };
                let mut regs = vm.get_regs().expect("can get regs");
                regs.rax = patch.addr;
                vm.set_regs(&regs).expect("can set regs");
                // keep rax as-is
                let mut preserves = [false; 16];
                preserves[0] = true;

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                test_prelude(inst, &buf);

                check_behavior_with_regs(&mut vm, &inst, preserves, &[patch]);
            }
        }

        #[test]
        fn verify_lidt() {
            // TODO: happen to be testing on a zen 5 system, so i picked a zen 5 decoder.
            let decoder = long_mode::uarch::amd::zen5();
            let mut buf = Instruction::default();

            static INSTS: &'static [&'static [u8]] = &[
                // lidt
                &[0x0f, 0x01, 0x18],
                &[0x66, 0x0f, 0x01, 0x18],
                &[0x67, 0x0f, 0x01, 0x18],
            ];

            for inst in INSTS {
                let mut vm = create_test_vm();
                vm.set_single_step(true).expect("can enable single-step");

                // set up lidt to re-load the same idt.
                let mut patch_bytes = Vec::new();
                patch_bytes.extend_from_slice(&4095u16.to_le_bytes());
                patch_bytes.extend_from_slice(&vm.idt_addr().0.to_le_bytes());
                let patch = MemPatch {
                    addr: 0x1_0000_0000,
                    bytes: patch_bytes,
                };
                let mut regs = vm.get_regs().expect("can get regs");
                regs.rax = patch.addr;
                vm.set_regs(&regs).expect("can set regs");
                // keep rax as-is
                let mut preserves = [false; 16];
                preserves[0] = true;

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                test_prelude(inst, &buf);

                check_behavior_with_regs(&mut vm, &inst, preserves, &[patch]);
            }
        }

        #[test]
        fn verify_lldt() {
            // TODO: happen to be testing on a zen 5 system, so i picked a zen 5 decoder.
            let decoder = long_mode::uarch::amd::zen5();
            let mut buf = Instruction::default();

            static INSTS: &'static [&'static [u8]] = &[
                // lldt
                &[0x0f, 0x00, 0x10],
                &[0x66, 0x0f, 0x00, 0x10],
                &[0x67, 0x0f, 0x00, 0x10],
            ];

            for inst in INSTS {
                let mut vm = create_test_vm();
                vm.set_single_step(true).expect("can enable single-step");

                // quoth SDM:
                // > If bits 2-15 of the source operand are 0, LDTR is marked invalid and the LLDT
                // > instruction completes silently. However, all subsequent references to
                // > descriptors in the LDT (except by the LAR, VERR, VERW or LSL instructions) cause
                // > a general protection exception (#GP).
                let mut patch_bytes = Vec::new();
                patch_bytes.extend_from_slice(&0u16.to_le_bytes());
                let patch = MemPatch {
                    addr: 0x1_0000_0000,
                    bytes: patch_bytes,
                };
                let mut regs = vm.get_regs().expect("can get regs");
                regs.rax = patch.addr;
                vm.set_regs(&regs).expect("can set regs");
                // keep rax as-is
                let mut preserves = [false; 16];
                preserves[0] = true;

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                test_prelude(inst, &buf);

                check_behavior_with_regs(&mut vm, &inst, preserves, &[patch]);
            }
        }

        #[test]
        fn verify_table_stores() {
            // TODO: happen to be testing on a zen 5 system, so i picked a zen 5 decoder.
            let decoder = long_mode::uarch::amd::zen5();
            let mut buf = Instruction::default();

            static INSTS: &'static [&'static [u8]] = &[
                // sgdt
                &[0x0f, 0x01, 0x00],
                &[0x66, 0x0f, 0x01, 0x00],
                &[0x67, 0x0f, 0x01, 0x00],
                // sidt
                &[0x0f, 0x01, 0x08],
                &[0x66, 0x0f, 0x01, 0x08],
                &[0x67, 0x0f, 0x01, 0x08],
                // sldt
                &[0x0f, 0x00, 0x00],
                &[0x66, 0x0f, 0x00, 0x00],
                &[0x67, 0x0f, 0x00, 0x00],
            ];

            for inst in INSTS {
                let mut vm = create_test_vm();
                vm.set_single_step(true).expect("can enable single-step");

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                test_prelude(inst, &buf);

                check_behavior(&mut vm, &inst);
            }
        }
    }
}
