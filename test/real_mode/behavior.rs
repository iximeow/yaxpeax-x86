#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
mod kvm {
    use asmlinator::x86_64::{
        IsaMode, GuestAddress,
        VmSettings, Vm, VcpuExit,
        kvm_regs, kvm_sregs
    };

    use yaxpeax_x86::real_mode;
    use yaxpeax_x86::real_mode::Instruction;
    use yaxpeax_x86::Exception;

    use rand::prelude::*;

    fn host_decoder() -> real_mode::InstDecoder {
        // Safety: it's cpuid, everything supports leaf eax=1.
        let leaf1 = core::arch::x86_64::__cpuid(1);
        match leaf1.eax {
            0x00b40f40 => {
                // zen 5 (my 9950x)
                real_mode::uarch::amd::zen5()
            }
            0x00870f10 => {
                // zen 2 (my 3950x)
                real_mode::uarch::amd::zen2()
            }
            0x00050657 => {
                // 10980xe (according to instlatx86)
                // this is actually "cascade lake" but there's no uarch for that yet
                real_mode::uarch::intel::skylake()
            }
            _ => {
                // some kind of assumed baseline, haswell-or-later is a total guess on compat.
                real_mode::uarch::intel::haswell()
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    struct ExpectedMemAccess {
        write: bool,
        addr: u16,
        size: u32,
    }

    #[derive(Debug, Copy, Clone)]
    struct ExpectedRegAccess {
        write: bool,
        reg: RegSpec,
    }

    // this is only actually *used* when printing detailed failure information. otherwise the fact
    // that any of these exist is all that's used to fail tests.
    #[cfg_attr(not(feature = "fmt"), allow(dead_code))]
    #[derive(Debug, Copy, Clone)]
    struct UnexpectedRegChange {
        reg: RegSpec,
        before: u64,
        after: u64,
    }

    #[derive(Debug, Clone)]
    struct MemPatch {
        addr: u16,
        bytes: Vec<u8>,
    }

    #[derive(Debug)]
    enum CheckErr {
        ComplexOp(real_mode::behavior::ComplexOp),
    }

    struct TestAccesses {
        preserve_rsp: bool,
        used_regs: [bool; 16],
        expected_reg: Vec<ExpectedRegAccess>,
        expected_mem: Vec<ExpectedMemAccess>,
    }

    struct AccessTestCtx<'a> {
        regs: &'a mut kvm_regs,
        accs: TestAccesses,
    }

    impl<'a> AccessTestCtx<'a> {
        fn into_expectations(self) -> (Vec<ExpectedRegAccess>, Vec<ExpectedMemAccess>) {
            let TestAccesses {
                expected_reg,
                expected_mem,
                ..
            } = self.accs;
            (expected_reg, expected_mem)
        }

        // randomize initial test VM state as described by this AccessTestCtx; it's possible that
        // after this point a `visit_accesses` pass will reset some of these registers to different
        // values as they may be used for address calculations.
        //
        // along the way, deposit any registers this test ctx declares to be "used" as an explicit
        // entry in `cares`. later testing should not permute these registers, even if they are not
        // directly declared as read by any operand, implicit or explicit.
        fn randomize_unused(&mut self, cares: &mut Vec<RegSpec>) {
            let mut rng = rand::rng();

            if !self.accs.used_regs[0] {
                self.regs.rax = rng.next_u64();
            } else {
                cares.push(RegSpec::eax());
            }
            if !self.accs.used_regs[1] {
                self.regs.rbx = rng.next_u64();
            } else {
                cares.push(RegSpec::ebx());
            }
            if !self.accs.used_regs[2] {
                self.regs.rcx = rng.next_u64();
            } else {
                cares.push(RegSpec::ecx());
            }
            if !self.accs.used_regs[3] {
                self.regs.rdx = rng.next_u64();
            } else {
                cares.push(RegSpec::edx());
            }
            if !self.accs.used_regs[4] {
                self.regs.rsi = rng.next_u64();
            } else {
                cares.push(RegSpec::esi());
            }
            if !self.accs.used_regs[5] {
                self.regs.rdi = rng.next_u64();
            } else {
                cares.push(RegSpec::edi());
            }
            if !self.accs.preserve_rsp {
                if !self.accs.used_regs[6] {
                    self.regs.rsp = rng.next_u64();
                } else {
                    cares.push(RegSpec::esp());
                }
            }
            if !self.accs.used_regs[7] {
                self.regs.rbp = rng.next_u64();
            } else {
                cares.push(RegSpec::ebp());
            }

            self.regs.r8 = rng.next_u64();
            self.regs.r9 = rng.next_u64();
            self.regs.r10 = rng.next_u64();
            self.regs.r11 = rng.next_u64();
            self.regs.r12 = rng.next_u64();
            self.regs.r13 = rng.next_u64();
            self.regs.r14 = rng.next_u64();
            self.regs.r15 = rng.next_u64();
        }
    }

    use yaxpeax_arch::AddressBase;
    use yaxpeax_x86::real_mode::{RegSpec, behavior::AccessVisitor};
    use yaxpeax_x86::real_mode::register_class;

    impl<'a> AccessVisitor for AccessTestCtx<'a> {
        fn register_read(&mut self, reg: RegSpec) {
            self.accs.expected_reg.push(ExpectedRegAccess {
                write: false,
                reg,
            });
        }
        fn register_write(&mut self, reg: RegSpec) {
            self.accs.expected_reg.push(ExpectedRegAccess {
                write: true,
                reg,
            });
        }
        fn get_register(&mut self, reg: RegSpec) -> Option<u16> {
            self.register_read(reg);

            let cls = reg.class();
            match cls {
                register_class::B | register_class::W | register_class::D => {
                    static KVM_REG_LUT: [usize; 16] = [
                        0, 2, 3, 1, 6, 7, 4, 5,
                        8, 9, 10, 11, 12, 13, 14, 15,
                    ];
                    let kvm_reg_nr = KVM_REG_LUT[reg.num() as usize];

                    // some ridiculous circumstances require us to not permute rsp, even
                    // though we *would* set it to a mapped address.
                    let allocated = self.accs.used_regs[reg.num() as usize] ||
                        (reg.num() == RegSpec::esp().num() && self.accs.preserve_rsp);

                    if allocated {
                        let value = unsafe {
                            (self.regs as *mut _ as *mut u64).offset(kvm_reg_nr as isize).read()
                        };
                        Some(value as u16)
                    } else {
                        // register value allocation is done .. carefully.
                        //
                        // unlike 64-bit and 32-bit test harnesses, we don't have gobs of memory
                        // to work with. everything gets packed in pretty tight, right after
                        // placing code.
                        let value = 0x10 + (kvm_reg_nr as u64 + 1) * 0x10;
                        unsafe {
                            (self.regs as *mut _ as *mut u64).offset(kvm_reg_nr as isize).write(value);
                        }
                        self.accs.used_regs[reg.num() as usize] = true;
                        Some(value as u16)
                    }
                }
                other => {
                    panic!("unexpected VcpuExit: {:?}", other);
                }
            }
        }
        fn memory_read(&mut self, address: Option<u16>, size: u32) {
            let acc = ExpectedMemAccess {
                write: false,
                addr: address.expect("can compute expected address"),
                size,
            };
            self.accs.expected_mem.push(acc);
        }
        fn memory_write(&mut self, address: Option<u16>, size: u32) {
            let acc = ExpectedMemAccess {
                write: true,
                addr: address.expect("can compute expected address"),
                size,
            };
            self.accs.expected_mem.push(acc);
        }
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
        vm.mem_slice_mut(TEST_MEM_BASE, TEST_MEM_SIZE).fill(0xaa);
        // test environments may require constants in memory at known locations (say, in support of
        // an LGDT test). apply those patches now that we've initialized all extra memory.
        for patch in mem_patches {
            let slice = vm.mem_slice_mut(GuestAddress(patch.addr as u64), patch.bytes.len() as u64);
            slice.copy_from_slice(patch.bytes.as_slice());
        }
        let mut exits = 0;
        let end_pc = loop {
            let exit = vm.run().expect("can run vcpu");
            exits += 1;
            match exit {
                VcpuExit::MmioRead { addr, .. } |
                VcpuExit::MmioWrite { addr, .. } => {
                    panic!("should not be mmio accesses anymore, but one at {:08x}", addr);
                }
                VcpuExit::Debug { pc, info: _ } => {
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
                (register_class::D, register_class::D) |
                (register_class::D, register_class::W) |
                (register_class::W, register_class::W) |
                (register_class::B, register_class::B) => {
                    larger.num() == smaller.num()
                }
                (register_class::D, register_class::B) |
                (register_class::W, register_class::B) => {
                    // top bit selects high/low half of *x registers, so mask it and compare
                    smaller.num() & 0b11 == larger.num()
                }
                (register_class::EFLAGS, _) |
                (_, register_class::EFLAGS) => {
                    false
                }
                (register_class::EIP, _) |
                (_, register_class::EIP) => {
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
            register_class::W => 0xffff,
            // x86_64 zero-extends 32-bit writes to 64-bit, so writes to "32-bit" registers still
            // are fully-clobbers.
            register_class::D => 0xffffffff_ffffffff,
            register_class::EFLAGS => 0xffffffff_ffffffff,
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
            assert_eq!(reg.class(), register_class::D);

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
                #[cfg(feature = "fmt")]
                eprintln!("register {} changed after permuting dontcares: {:016x} => {:016x}",
                    reg, initial_after, now_after);
                bad = true;
            }
        }

        if bad {
            eprintln!("after:");
            dump_regs(&now_after_regs);
            eprintln!("initial_after:");
            dump_regs(&initial_after_regs);
            panic!("cared about dontcares");
        }
    }

        fn compute_dontcares(vm: &Vm, accesses: &[ExpectedRegAccess]) -> Vec<RegSpec> {
            // use a bitmap for dontcares, mask out bits as registers are seen to be read.
            let mut reg_bitmap: u32 = 0xffff;

            fn reg_to_gpr(reg: RegSpec) -> Option<u8> {
                match reg.class() {
                    register_class::D |
                    register_class::W => {
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
                reg_bitmap &= !(1 << (RegSpec::esp().num()));
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

            for i in 0..8 {
                if reg_bitmap & (1 << i) != 0 {
                    regs.push(RegSpec::d(i));
                }
            }

            regs
        }

        fn compute_writes(accesses: &[ExpectedRegAccess]) -> Vec<RegSpec> {
            // same as dontcares, isk
            let mut reg_bitmap: u32 = 0x00000000;

            fn reg_to_gpr(reg: RegSpec) -> Option<u8> {
                match reg.class() {
                    register_class::D |
                    register_class::W => {
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

            for i in 0..8 {
                if reg_bitmap & (1 << i) != 0 {
                    regs.push(RegSpec::d(i));
                }
            }

            regs
        }

        fn permute_dontcares(dontcare_regs: &[RegSpec], regs: &mut kvm_regs) {
            let mut rng = rand::rng();

            for reg in dontcare_regs {
                assert_eq!(reg.class(), register_class::D);

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

        // TODO: this needs some rethinking. see commented-out caller.
        #[allow(dead_code)]
        fn permute_memread(expected_mem: &[ExpectedMemAccess], vm: &mut Vm) {
            for acc in expected_mem.iter() {
                if acc.write {
                    continue;
                }

                let mut buf = vec![0; acc.size as usize];
                let mut rng = rand::rng();
                rng.fill(&mut buf);

                vm.write_mem(GuestAddress(DATA_BASE + acc.addr as u64), buf.as_slice());
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

            let slice = vm.mem_slice_mut(GuestAddress(DATA_BASE + acc.addr as u64), acc.size as u64);
            slice.fill(0xaa);
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

        let test_mem = vm.mem_slice(TEST_MEM_BASE, TEST_MEM_SIZE);
        for i in 0..test_mem.len() {
            if test_mem[i] != 0xaa {
                if let Some(mut diff) = current_diff.take() {
                    let prev_diff_start = diff.addr.0;
                    let prev_diff_tail = prev_diff_start + diff.bytes.len() as u64;
                    let continuation = TEST_MEM_BASE.0 + i as u64 == prev_diff_tail + 1;
                    if continuation {
                        diff.bytes.push(test_mem[i]);
                    } else {
                        unexpected_acc.push(diff);

                        let guest_addr = TEST_MEM_BASE.0 + i as u64;
                        current_diff = Some(MemoryDiff {
                            addr: GuestAddress(guest_addr as u64),
                            bytes: vec![test_mem[i]],
                        });
                    }
                } else {
                    let guest_addr = TEST_MEM_BASE.0 + i as u64;
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

        if !unexpected_acc.is_empty() {
            for diff in unexpected_acc {
                eprintln!("{}", diff);
            }
            let regs = vm.get_regs().unwrap();
            dump_regs(&regs);
            panic!("unexpected memory accesses!");
        }
    }

    fn verify_reg_changes(
        expected_regs: &[ExpectedRegAccess],
        before_regs: &kvm_regs, after_regs: &kvm_regs,
        before_sregs: &kvm_sregs, after_sregs: &kvm_sregs
    ) {
        let mut unexpected_regs = Vec::new();

        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::eax(), before_regs.rax, after_regs.rax);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::ecx(), before_regs.rcx, after_regs.rcx);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::edx(), before_regs.rdx, after_regs.rdx);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::ebx(), before_regs.rbx, after_regs.rbx);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::esp(), before_regs.rsp, after_regs.rsp);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::ebp(), before_regs.rbp, after_regs.rbp);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::esi(), before_regs.rsi, after_regs.rsi);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::edi(), before_regs.rdi, after_regs.rdi);
        verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::eflags(), before_regs.rflags, after_regs.rflags);

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
// not outside 64-bit mode!
//         verify_reg(&mut unexpected_regs, &expected_regs, RegSpec::cr8(), before_sregs.cr8, after_sregs.cr8);

        if !unexpected_regs.is_empty() {
            eprintln!("unexpected reg changes:");
            for change in unexpected_regs {
                let _ = change;
                #[cfg(feature = "fmt")]
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
            // TODO: really need to permute memory dont-care here, rather than reads. it'd probably
            // be sufficient to pick any other default pattern than 0xaa and pass that selected
            // pattern to verify..?
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

    fn check_behavior(vm: &mut Vm, inst: &[u8]) -> Result<(), CheckErr> {
        check_behavior_with_regs(vm, inst, None, &[])
    }

    // `reg_preserves` declares a set of registers, numbered by their *Linux KVM API number*, as in
    // the position in `kvm_regs`, that must be preserved by the test.
    fn check_behavior_with_regs(vm: &mut Vm, inst: &[u8], expect_accs: Option<TestAccesses>, mem_patches: &[MemPatch]) -> Result<(), CheckErr> {
        let decoded = yaxpeax_x86::real_mode::InstDecoder::default()
            .decode_slice(inst).expect("can decode");

        eprint!("checking behavior of ");
        for b in inst.iter() {
            eprint!("{:02x}", b);
        }
        #[cfg(feature = "fmt")]
        eprint!(": {}", decoded);
        eprint!("\n");

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
        use yaxpeax_arch::LengthedInstruction;
        assert_eq!(insts.len(), 0.wrapping_offset(decoded.len()) as usize + 1);

        let behavior = decoded.behavior();

        // TODO: this does an infinite loop when run??
        if decoded.opcode() == real_mode::Opcode::FLDCW {
            return Ok(());
        }

        let sregs = vm.get_sregs().unwrap();
        let mut regs = vm.get_regs().unwrap();
        vm.program(insts.as_slice(), &mut regs);
        regs.rcx = 0x00f000;
        vm.set_regs(&regs).unwrap();

        // a set of registers whose values we are directed to care about. these are subtracted from
        // dontcares, later.
        let mut cares = Vec::new();

        let ctx = match expect_accs {
            Some(mut accs) => {
                accs.preserve_rsp = vm.idt_configured();
                let mut ctx = AccessTestCtx {
                    regs: &mut regs,
                    accs,
                };
                ctx.randomize_unused(&mut cares);
                ctx
            }
            None => {
                let accs = TestAccesses {
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
                    used_regs: [false; 16],
                    expected_reg: Vec::new(),
                    expected_mem: Vec::new(),
                };
                let mut ctx = AccessTestCtx {
                    regs: &mut regs,
                    accs,
                };
                ctx.randomize_unused(&mut cares);
                behavior.visit_accesses(&mut ctx).map_err(|e| CheckErr::ComplexOp(e))?;

                ctx
            }
        };

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
                    if decoded.opcode() == real_mode::Opcode::MOV {
                        // TODO: should be in the exception list
                        if let real_mode::Operand::Register { reg } = decoded.operand(0) {
                            if reg.class() == real_mode::register_class::S {
                                // mov to segment selector can #GP if the selector is invalid:
                                // > If the DS, ES, FS, or GS register is being loaded and the
                                // > segment pointed to is not a data or readable code segment.
                                return Ok(());
                            }
                        }
                    }
                }
                dump_regs(&vm_regs);

                #[cfg(feature = "fmt")]
                {
                    panic!("TODO: handle exceptions ({:?})", other);
                }
                #[cfg(not(feature = "fmt"))]
                {
                    let _ = other;
                    panic!("TODO: handle exceptions");
                }
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
                return Ok(());
            }
            Err(Exception::PF) => {
                // TODO: probably should handle `#PF` more precisely?
                return Ok(());
            }
            Err(other) => {
                #[cfg(feature = "fmt")]
                {
                    panic!("TODO: handle exceptions ({:?})", other);
                }
                #[cfg(not(feature = "fmt"))]
                {
                    let _ = other;
                    panic!("TODO: handle exceptions");
                }
            }
        }
    }

    const DATA_BASE: u64 = 1024 * 1024 - 0x1000;
    const TEST_MEM_BASE: GuestAddress = GuestAddress(DATA_BASE + 0x10);
    const TEST_MEM_SIZE: u64 = 0x1000 - 0x10;

    fn create_test_vm() -> asmlinator::x86_64::Vm {
        let settings = VmSettings::new(1024 * 1024, IsaMode::Real);
        Vm::create_by_settings(settings).expect("can create vm")
    }

    // see the comment in this test about why these vex()-related tests are not run..
    #[ignore]
    #[test]
    fn kvm_run_vex() {
        // check for our own benefit that vex instructions... might work?
        //
        // zen 5 does not support vex-prefixed instructions, seemingly. then, linux (at least
        // kernel 6.8) tries to emulate the #UD (arch/x86/kvm/x86.c `handle_ud()`), decodes
        // *something*, and advances the vCPU into something quite bogus. if you're lucky, this
        // results in an MMIO access which `check_behavior()` rejects. if you're unlucky, the vCPU
        // is left running an RMW instruction on an MMIO address and hangs out in the kernel for
        // ever until you signal the process.
        //
        // so, just run one instruction ("vcmpps xmm0, xmm0, xmm1, 0") to see if we're really
        // doomed in the other "run everything" tests in this module.
        //
        // the following bpftrace was helpful in figuring out what in the world was happening:
        // ```
        // kprobe:svm_invoke_exit_handler {
        //     /* recall exit codes from AMD APM Vol 2 Appendix C "SVM Intercept Exit Codes" */
        //     /* 0x46 => #UD, 0x78 => hlt, 0x400 => nested page fault */
        //     printf("exit. guest rip: %p, exit_code: %x\n",
        //         ((struct kvm_vcpu*)arg0)->arch.regs[16], arg1
        //     );
        // }
        //
        // kprobe:npf_interception {
        //     printf("npf_interception\n");
        // }
        //
        // kprobe:kvm_mmu_page_fault {
        //     printf("kvm_mmu_page_fault gpa=%p\n", arg1);
        // }
        //
        // kprobe:x86_emulate_instruction {
        //     printf("we should not be emulating\n");
        // }
        // ```
        //
        // and behold, on the afflicted combination above, an instruction emulation happens!
        let mut vm = create_test_vm();
        let inst: &'static [u8] = &[0xc4, 0xe1, 0x78, 0xc2, 0xc1, 0x00];
        check_behavior(&mut vm, inst).expect("behavior check is ok");
    }

    #[test]
    fn kvm_verify_xor_reg_mem() {
        let mut vm = create_test_vm();

        let inst: &'static [u8] = &[0x8b, 0x01];
        check_behavior(&mut vm, inst).expect("behavior check is ok");

        // `xor rax, [rcx]`. this works. great!
        let inst: &'static [u8] = &[0x33, 0x01];
        check_behavior(&mut vm, inst).expect("behavior check is ok");

        // `xor al, [rcx]`. also works. cool!
        let inst: &'static [u8] = &[0x32, 0x01];
        check_behavior(&mut vm, inst).expect("behavior check is ok");

        // `xor [rcx], al`. this runs until the VM starts executing in MMIO space and
        // VcpuExit::Shutdown. what.
        let inst: &'static [u8] = &[0x30, 0x01];
        check_behavior(&mut vm, inst).expect("behavior check is ok");
    }

    #[test]
    fn kvm_verify_inc() {
        let mut vm = create_test_vm();

        // `inc eax`
        let inst: &'static [u8] = &[0xff, 0xc0];
        check_behavior(&mut vm, inst).expect("behavior check is ok");

        // `inc dword [rax]`
        let inst: &'static [u8] = &[0xff, 0x00];
        check_behavior(&mut vm, inst).expect("behavior check is ok");
    }

    #[test]
    fn kvm_verify_push() {
        let mut vm = create_test_vm();

        // `push rax`
        let inst: &'static [u8] = &[0x50];
        check_behavior(&mut vm, inst).expect("behavior check is ok");
    }

    #[test]
    fn kvm_verify_popmem() {
        let mut vm = create_test_vm();

        // `pop [rax]`
        let inst: &'static [u8] = &[0x8f, 0x00];
        check_behavior(&mut vm, &inst[0..2]).expect("behavior check is ok");
    }

    #[test]
    fn kvm_verify_ret() {
        let mut vm = create_test_vm();

        // `ret`
        let inst: &'static [u8] = &[0xc3];
        let guest_stack_base = 1024 * 1024 - 4096 + 8;
        // TODO: set up ret test to return to some other address. check_behavior() doesn't tolerate
        // this (yet).
        //
        // further, move the stack so low that the preparation for memory access checking (aka
        // "scribbling all over memory") doesn't clobber it. the stack normally would be at
        // 4096 - 0x80 here, but anything after 0x10 is scribbled. we're only using the low byte
        // for code, so there's some space here..
        let mut regs = vm.get_regs().unwrap();
        regs.rsp = 8;
        vm.set_regs(&regs).unwrap();
        vm.write_mem(GuestAddress(guest_stack_base), &0x01u64.to_le_bytes());
        check_behavior(&mut vm, inst).expect("behavior check is ok");
    }

    /*
     * TODO: this doesn't fit in the test framework really: `ins` will cause an I/O exit, which
     * immediately fails the test.
    #[test]
    fn kvm_verify_ins() {
        let mut vm = create_test_vm();

        // `ins byte [rdi], dl`
        let inst: &'static [u8] = &[0x6c];
        check_behavior(&mut vm, inst).expect("behavior check is ok");
    }
    */

    #[test]
    fn behavior_verify_kvm_general() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut inst = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        for prefix in [0x00, 0x66] {
            for opc in 0x00..=u8::MAX {
                for opers in [0x00,
                              0x01, 0x09, 0x11, 0x19, 0x21, 0x29, 0x31, 0x39,
                              0xc1, 0xc9, 0xd1, 0xd9, 0xe1, 0xe9, 0xf1, 0xf9] {
                    for imm in [0x00, 0x01, 0x80, 0x81, 0xc0, 0xc1] {
                        let mut inst_len = 0;
                        let mut buf = [0u8; 8];

                        match prefix {
                            0x00 => {},
                            o => {
                                buf[inst_len] = o;
                                inst_len += 1;
                            }
                        }

                        buf[inst_len] = opc;
                        inst_len += 1;

                        if opers != 0x00 {
                            buf[inst_len] = opers;
                            inst_len += 1;
                        }

                        // disp16 means oper+imm is interpreted as very large displacements, into
                        // mmio space..
                        if imm != 0x00 && (opc < 0xa0 || opc >= 0xa4) {
                            buf[inst_len] = imm;
                            inst_len += 1;
                        }

                        let mut reader = U8Reader::new(&buf[..inst_len]);
                        if decoder.decode_into(&mut inst, &mut reader).is_ok() {
                            if not_generic(&inst) {
                                continue;
                            }
                            vm.set_regs(&initial_regs).unwrap();
                            use yaxpeax_arch::LengthedInstruction;
                            let inst_len = 0.wrapping_offset(inst.len()) as usize;
                            let res = check_behavior(&mut vm, &buf[..inst_len]);
                            if matches!(res, Err(CheckErr::ComplexOp(_))) {
                                continue;
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn behavior_verify_kvm_0f_() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        for opc in 0xf0..=u8::MAX {
            for bits in 0..=0x7f {
                let mut instlen = 0;
                let suffix = bits & 3;
                let prefix = (bits >> 2) & 3;
                let imm = (bits >> 4) & 3;
                let opers = (bits >> 6) & 1;

                let mut bytes = [0; 6]; // 0x66, 0x0f, inst[0], inst[1]];

                match prefix {
                    0b00 => { },
                    0b01 => {
                        bytes[instlen] = 0x66;
                        instlen += 1;
                    }
                    0b10 => {
                        bytes[instlen] = 0xf2;
                        instlen += 1;
                    }
                    0b11 => {
                        bytes[instlen] = 0xf3;
                        instlen += 1;
                    }
                    _ => {}
                }

                bytes[instlen] = 0x0f;
                instlen += 1;

                match suffix {
                    0b00 => { },
                    0b01 => {
                        bytes[instlen] = 0x38;
                        instlen += 1;
                    }
                    0b10 => {
                        bytes[instlen] = 0x3a;
                        instlen += 1;
                    }
                    _ => {}
                }

                bytes[instlen] = opc;
                bytes[instlen + 1] = if opers == 0 {
                    0x01
                } else {
                    0xc1
                };
                instlen += 2;

                match imm {
                    0b00 => { },
                    0b01 => {
                        bytes[instlen] = 0x00;
                        instlen += 1;
                    },
                    0b10 => {
                        bytes[instlen] = 0x01;
                        instlen += 1;
                    },
                    _ => {
                        bytes[instlen] = 0xff;
                        instlen += 1;
                    },
                }

                let mut reader = U8Reader::new(&bytes);
                if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                    // two byte instructions were covered by `verify_kvm`, novel instructions are three
                    // bytes (or longer..?)
                    use yaxpeax_arch::LengthedInstruction;
                    let decoded_len = 0.wrapping_offset(buf.len()) as usize;
                    if decoded_len != instlen {
                        continue;
                    }

                    if not_generic(&buf) {
                        continue;
                    }

                    vm.set_regs(&initial_regs).unwrap();
                    check_behavior(&mut vm, &bytes[..instlen]).expect("behavior check is ok");
                }
            }
        }
    }

    #[test]
    fn behavior_verify_kvm_avx() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        for opc in 0xf0..=255 {
            for prefix in [0x00, 0x66, 0xf2, 0xf3] {
                for map in 0..3 {
                    for operands in [0x01, 0xc1] {
                        let mut len = 0;
                        let mut bytes = [0; 8];

                        if prefix != 0x00 {
                            bytes[len] = prefix;
                            len += 1;
                        }

                        bytes[len] = 0x0f;
                        len += 1;

                        if map == 1 {
                            bytes[len] = 0x38;
                            len += 1;
                        } else if map == 2 {
                            bytes[len] = 0x3a;
                            len += 1;
                        }

                        bytes[len] = opc;
                        len += 1;

                        bytes[len] = operands;
                        len += 1;

                        let bytes = &bytes[..len];
                        let mut reader = U8Reader::new(&bytes);
                        if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                            use yaxpeax_arch::LengthedInstruction;
                            let inst_len = 0.wrapping_offset(buf.len()) as usize;
                            if inst_len != bytes.len() {
                                continue;
                            }

                            if not_generic(&buf) {
                                continue;
                            }

                            vm.set_regs(&initial_regs).unwrap();
                            check_behavior(&mut vm, &bytes).expect("behavior check is ok");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn behavior_verify_kvm_avx_imm8() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        for opc in 0x10..=255 {
            for prefix in [0x00, 0x66, 0xf2, 0xf3] {
                for map in 0..3 {
                    for imm in [0u8, 1u8, 2u8, 4u8, 8u8, 16u8, 32u8, 64u8, 128u8, 255u8] {
                        for operands in [0x01, 0xc1] {
                            let mut len = 0;
                            let mut bytes = [0; 8];

                            if prefix != 0x00 {
                                bytes[len] = prefix;
                                len += 1;
                            }

                            bytes[len] = 0x0f;
                            len += 1;

                            if map == 1 {
                                bytes[len] = 0x38;
                                len += 1;
                            } else if map == 2 {
                                bytes[len] = 0x3a;
                                len += 1;
                            }

                            bytes[len] = opc;
                            len += 1;

                            bytes[len] = operands;
                            len += 1;

                            bytes[len] = imm;
                            len += 1;

                            let bytes = &bytes[..len];
                            let mut reader = U8Reader::new(&bytes);
                            if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                                use yaxpeax_arch::LengthedInstruction;
                                let inst_len = 0.wrapping_offset(buf.len()) as usize;
                                if inst_len != bytes.len() {
                                    continue;
                                }

                                if not_generic(&buf) {
                                    continue;
                                }

                                vm.set_regs(&initial_regs).unwrap();
                                check_behavior(&mut vm, &bytes).expect("behavior check is ok");
                            }
                        }
                    }
                }
            }
        }
    }

    // see `kvm_run_vex()` for why vex-related tests are ignored
    #[ignore]
    #[test]
    fn behavior_verify_kvm_vex() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        #[allow(non_snake_case)]
        for opcode in 0x00..=u8::MAX {
            for prefix_bits in 0x00..0x400u16 {
                let mmmmm = prefix_bits & 0b11111;
                let prefix_1 = (0xe0 | mmmmm) as u8;

                let pp = (prefix_bits >> 5) & 0b11;
                let W = (prefix_bits >> 7) & 1;
                let L = (prefix_bits >> 8) & 1;
                let prefix_2 = (0x78 | (W << 7) | (L << 2) | pp) as u8;

                let operands = (prefix_bits >> 9) & 0b1;
                static OPC_BYTE_TABLE: [u8; 2] = [0xc1, 0x01];

                let bytes: [u8; 5] = [0xc4, prefix_1, prefix_2, opcode, OPC_BYTE_TABLE[operands as usize]];
                let mut reader = U8Reader::new(&bytes);
                if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                    // two byte instructions were covered by `verify_kvm`, novel instructions are three
                    // bytes (or longer..?)
                    use yaxpeax_arch::LengthedInstruction;
                    let inst_len = 0.wrapping_offset(buf.len()) as usize;
                    if inst_len != bytes.len() {
                        continue;
                    }

                    if not_generic(&buf) {
                        continue;
                    }

                    vm.set_regs(&initial_regs).unwrap();
                    let res = check_behavior(&mut vm, &bytes[..inst_len]);
                    match res {
                        Ok(()) => {}
                        Err(CheckErr::ComplexOp(op)) => {
                            // uncheckable but not a failure
                            eprintln!("cannot check {:?}", op);
                        }
                    }
                }
            }
        }
    }

    // see `kvm_run_vex()` for why vex-related tests are ignored
    #[ignore]
    #[test]
    fn behavior_verify_kvm_vex_imm8() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        #[allow(non_snake_case)]
        for opcode in 0xc2..=u8::MAX {
            for prefix_bits in 0x00..0x400u16 {
                for imm in [0u8, 1u8, 2u8, 4u8, 8u8, 16u8, 32u8, 64u8, 128u8, 255u8] {
                    let mmmmm = prefix_bits & 0b11111;
                    let prefix_1 = (0xe0 | mmmmm) as u8;

                    let pp = (prefix_bits >> 5) & 0b11;
                    let W = (prefix_bits >> 7) & 1;
                    let L = (prefix_bits >> 8) & 1;
                    let prefix_2 = (0x78 | (W << 7) | (L << 2) | pp) as u8;

                    let operands = (prefix_bits >> 9) & 0b1;
                    static OPC_BYTE_TABLE: [u8; 2] = [0xc1, 0x01];

                    let bytes: [u8; 6] = [0xc4, prefix_1, prefix_2, opcode, OPC_BYTE_TABLE[operands as usize], imm];
                    let mut reader = U8Reader::new(&bytes);
                    if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                        // two byte instructions were covered by `verify_kvm`, novel instructions are three
                        // bytes (or longer..?)
                        use yaxpeax_arch::LengthedInstruction;
                        let inst_len = 0.wrapping_offset(buf.len()) as usize;
                        if inst_len != bytes.len() {
                            continue;
                        }

                        if not_generic(&buf) {
                            continue;
                        }

                        vm.set_regs(&initial_regs).unwrap();
                        let res = check_behavior(&mut vm, &bytes[..inst_len]);
                        match res {
                            Ok(()) => {}
                            Err(CheckErr::ComplexOp(op)) => {
                                // uncheckable but not a failure
                                eprintln!("cannot check {:?}", op);
                            }
                        }
                    }
                }
            }
        }
    }

    // see `kvm_run_vex()` for why vex-related tests are ignored
    #[ignore]
    #[test]
    fn behavior_verify_kvm_evex_noimm() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        #[allow(non_snake_case)]
        for opcode in 0x00..=u8::MAX {
            for prefix_bits in 0x00..0x1000u16 {
                let mmm = prefix_bits & 0b111;
                let prefix_1 = (0xf0 | mmm) as u8;

                let pp = (prefix_bits >> 3) & 0b11;
                let z = (prefix_bits >> 5) & 1;
                let b = (prefix_bits >> 6) & 1;
                let W = (prefix_bits >> 7) & 1;
                let LL = (prefix_bits >> 8) & 0b11;
                let k = (prefix_bits >> 10) & 11;

                let prefix_2 = (0x7c | (W << 7) | pp) as u8;

                let aaa = [0b000, 0b001, 0b010, 0b111][k as usize];
                let prefix_3 = (0x08 | aaa | b << 4 | LL << 5 | z << 7) as u8;

                let operands = (prefix_bits >> 9) & 0b1;
                static OPC_BYTE_TABLE: [u8; 2] = [0xc1, 0x01];

                let bytes: [u8; 6] = [0x62, prefix_1, prefix_2, prefix_3, opcode, OPC_BYTE_TABLE[operands as usize]];
                let mut reader = U8Reader::new(&bytes);
                if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                    // two byte instructions were covered by `verify_kvm`, novel instructions are three
                    // bytes (or longer..?)
                    use yaxpeax_arch::LengthedInstruction;
                    let inst_len = 0.wrapping_offset(buf.len()) as usize;
                    if inst_len != bytes.len() {
                        continue;
                    }

                    if not_generic(&buf) {
                        continue;
                    }

                    vm.set_regs(&initial_regs).unwrap();
                    let res = check_behavior(&mut vm, &bytes[..inst_len]);
                    match res {
                        Ok(()) => {}
                        Err(CheckErr::ComplexOp(op)) => {
                            // uncheckable but not a failure
                            eprintln!("cannot check {:?}", op);
                        }
                    }
                }
            }
        }
    }

    // see `kvm_run_vex()` for why vex-related tests are ignored
    #[ignore]
    #[test]
    fn behavior_verify_kvm_evex_imm() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        #[allow(non_snake_case)]
        for opcode in 0x00..=u8::MAX {
            for imm in [0x00, 0x01, 0x80, 0xff] {
                for prefix_bits in 0x00..0x1000u16 {
                    let mmm = prefix_bits & 0b111;
                    let prefix_1 = (0xf0 | mmm) as u8;

                    let pp = (prefix_bits >> 3) & 0b11;
                    let z = (prefix_bits >> 5) & 1;
                    let b = (prefix_bits >> 6) & 1;
                    let W = (prefix_bits >> 7) & 1;
                    let LL = (prefix_bits >> 8) & 0b11;
                    let k = (prefix_bits >> 10) & 11;

                    let prefix_2 = (0x7c | (W << 7) | pp) as u8;

                    let aaa = [0b000, 0b001, 0b010, 0b111][k as usize];
                    let prefix_3 = (0x08 | aaa | b << 4 | LL << 5 | z << 7) as u8;

                    let operands = (prefix_bits >> 9) & 0b1;
                    static OPC_BYTE_TABLE: [u8; 2] = [0xc1, 0x01];

                    let bytes: [u8; 7] = [0x62, prefix_1, prefix_2, prefix_3, opcode, OPC_BYTE_TABLE[operands as usize], imm];
                    let mut reader = U8Reader::new(&bytes);
                    if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                        // two byte instructions were covered by `verify_kvm`, novel instructions are three
                        // bytes (or longer..?)
                        use yaxpeax_arch::LengthedInstruction;
                        let inst_len = 0.wrapping_offset(buf.len()) as usize;
                        if inst_len != bytes.len() {
                            continue;
                        }

                        if not_generic(&buf) {
                            continue;
                        }

                        vm.set_regs(&initial_regs).unwrap();
                        let res = check_behavior(&mut vm, &bytes[..inst_len]);
                        match res {
                            Ok(()) => {}
                            Err(CheckErr::ComplexOp(op)) => {
                                // uncheckable but not a failure
                                eprintln!("cannot check {:?}", op);
                            }
                        }
                    }
                }
            }
        }
    }

    // use the generic test harness for a handful of instructions that don't get covered in the
    // general enumeration above
    #[test]
    fn behavior_verify_kvm_misc() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::Instruction;

        let mut vm = create_test_vm();
        vm.set_single_step(true).expect("can enable single-step");

        let decoder = host_decoder();
        let mut buf = Instruction::default();
        let initial_regs = vm.get_regs().unwrap();

        static MISC_INSTS: &'static [&'static [u8]] = &[
            // cmppd xmm0, xmmword [rcx], 0x75
            &[0x66, 0x0f, 0xc2, 0x01, 0x75],
            // cmpps xmm0, xmmword [rcx], 0x75
            &[0x0f, 0xc2, 0x01, 0x75],
            // shufpd xmm0, xmmword [rcx], 0x75
            &[0x66, 0x0f, 0xc6, 0x01, 0x75],
            // shufps xmm0, xmmword [rcx], 0x75
            &[0x0f, 0xc6, 0x01, 0x75],
            // lzcnt eax, dword [rcx]
            &[0xf3, 0x0f, 0xbd, 0x01],
            // adcx eax, dword [rcx]
            &[0x66, 0x0f, 0x38, 0xf6, 0x01],
            // adox eax, dword [rcx]
            &[0xf3, 0x0f, 0x38, 0xf6, 0x01],
            // crc32 eax, byte [rcx]
            &[0xf2, 0x0f, 0x38, 0xf0, 0xc1],
        ];
        for bytes in MISC_INSTS.iter() {
            let mut reader = U8Reader::new(&bytes);
            if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                eprint!("checking behavior of {:02x}", bytes[0]);
                for b in &bytes[1..] {
                    eprint!(" {:02x}", b);
                }
                eprint!("\n");

                vm.set_regs(&initial_regs).unwrap();
                check_behavior(&mut vm, bytes).expect("behavior check is ok");
            }
        }
    }

    use yaxpeax_x86::real_mode::Opcode;
    use yaxpeax_x86::real_mode::Operand;
    fn not_generic(instr: &Instruction) -> bool {
        if instr.prefixes.cs() {
            // writes to cs:[..] will #GP and this is the most straightforward way to avoid that
            // category of nonsense.
            return true;
        }
        if instr.opcode() == Opcode::POP {
            match instr.operand(0) {
                Operand::Register { reg } if reg.class() == register_class::S => {
                    // pop of segment registers does means any followup execution in the same VM is
                    // unpredictable and will probably #GP
                    return true;
                }
                _ => {}
            }
        }

        if instr.opcode() == Opcode::MOV {
            if let Operand::Register { reg } = instr.operand(0) {
                if reg.class() == register_class::S {
                    // mov to segment selector can #GP if the selector is invalid:
                    // > If the DS, ES, FS, or GS register is being loaded and the
                    // > segment pointed to is not a data or readable code segment.
                    //
                    // and even if the VM continues, results will be unpredictable with segment
                    // registers full of random data..
                    return true;
                }
            }
        }

        if instr.opcode() == Opcode::XCHG {
            // TODO: don't understand why this "cared about dontcares"s...
            return true;
        }
        if instr.opcode() == Opcode::XADD {
            // TODO: 660fc101 trips the "permuting dontcares" check?
            return true;
        }
        if instr.opcode() == Opcode::ARPL {
            // TODO: runs in real mode, but the SDM sure does say that it is not accepted in real
            // mode. so that's curious!
            return true;
        }
        if instr.opcode() == Opcode::BOUND {
            // when bound *does things*, it's a #BR exception.
            return true;
        }

        if [Opcode::LES, Opcode::LDS, Opcode::LFS, Opcode::LGS, Opcode::LSS].contains(&instr.opcode()) {
            // loading invalid segment selectors will #GP, so these are tricky to run generically.
            return true;
        }

        if [Opcode::FLDENV, Opcode::FNSTENV, Opcode::FRSTOR, Opcode::FNSAVE, Opcode::FNSTCW, Opcode::FNSTSW].contains(&instr.opcode()) {
            // this needs a more targeted test
            return true;
        }

        if [Opcode::INS, Opcode::MOVS, Opcode::OUTS, Opcode::LODS, Opcode::STOS, Opcode::CMPS, Opcode::SCAS].contains(&instr.opcode()) {
            if instr.prefixes.rep_any() {
                // `repnz cmps` will carry on for however long memory allows,
                // `rep movs` runs `rcx`-many times, etc
                return true;
            }
        }

        if instr.opcode() == Opcode::RSM {
            // SMM is kinda not our problem for now..
            return true;
        }

        if instr.opcode() == Opcode::GETSEC {
            // oh dear
            return true;
        }

        if instr.opcode() == Opcode::RDPID {
            // rdpid is a specialized rdmsr
            return true;
        }

        if instr.opcode() == Opcode::RDTSC {
            // the TSC keeps ticking so eax will change across runs and trip the
            // "cared about dontcares" check.
            return true;
        }

        if instr.opcode() == Opcode::RDPMC {
            // reading a bogus PMC will just #GP so this needs a more specific test.
            return true;
        }

        if instr.opcode() == Opcode::DIV || instr.opcode() == Opcode::IDIV {
            // if the operand is in memory we're not currently permuting memory, so it
            // might be zero and just #DE.
            return true;
        }

        if instr.opcode() == Opcode::WRMSR || instr.opcode() == Opcode::RDMSR {
            // TODO: ... okay come on.
            return true;
        }
        if instr.opcode() == Opcode::RETURN {
            // hard to handle generically here; see `verify_ret`.
            return true;
        }
        if instr.opcode() == Opcode::LEAVE {
            // TODO: trying to generically handle leave typically gets #SS from popping a
            // non-canonical address. needs more specific test.
            return true;
        }
        if instr.opcode() == Opcode::JMPF || instr.opcode() == Opcode::RETF || instr.opcode() == Opcode::CALLF {
            // TODO: trying to is harder. needs more specific test.
            return true;
        }
        if instr.opcode() == Opcode::INT {
            // TODO: int is complex, but check_behavior() does not tolerate those yet
            return true;
        }
        if instr.opcode() == Opcode::JMP || instr.opcode() == Opcode::CALL {
            // TODO: needs more specific testing
            return true;
        }
        if instr.opcode() == Opcode::JCXZ || instr.opcode() == Opcode::LOOP || instr.opcode() == Opcode::LOOPZ || instr.opcode() == Opcode::LOOPNZ {
            // TODO: also complex
            return true;
        }
        if instr.opcode() == Opcode::IRET || instr.opcode() == Opcode::IRETD || instr.opcode() == Opcode::IRETQ {
            // TODO: oh dear
            return true;
        }
        if [Opcode::JO, Opcode::JNO, Opcode::JB, Opcode::JNB, Opcode::JZ, Opcode::JNZ, Opcode::JA, Opcode::JNA, Opcode::JS, Opcode::JNS, Opcode::JP, Opcode::JNP, Opcode::JL, Opcode::JGE, Opcode::JLE, Opcode::JG].contains(&instr.opcode()) {
            // TODO: jmp-related tests that tolerate rip changing.
            return true;
        }

        if [Opcode::SYSCALL, Opcode::SYSRET, Opcode::SYSENTER, Opcode::SYSEXIT].contains(&instr.opcode()) {
            // TODO: syscall tests
            return true;
        }

        if undef::OPCODES.contains(&instr.opcode()) {
            // TODO: ud tests, etc
            return true;
        }

        if instr.opcode() == Opcode::CLTS {
            // what happens here, access 0xff000?
            return true;
        }

        // mov es, word [rax]
        // does an inf loop too...?
        if [Opcode::INS, Opcode::OUTS, Opcode::IN, Opcode::OUT].contains(&instr.opcode()) {
            return true;
        }

        if sse_gpr::OPCODES.contains(&instr.opcode()) {
            // not having reads for xmm registers yet, these don't fit well with the test harness..
            return true;
        }

        if table_instrs::OPCODES.contains(&instr.opcode()) {
            // tested under `mod table_instrs`.
            return true;
        }

        if ptwrite::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if vm_instrs::OPCODES.contains(&instr.opcode()) {
            // this generic testing facility is not appropriate for VM instructions.
            return true;
        }

        if ctrl_instrs::OPCODES.contains(&instr.opcode()) {
            // control registers complicate testing against permutations, since those reuse
            // the VM.
            return true;
        }

        if enqcmd::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        static COMPLEX: &'static [Opcode] = &[
            Opcode::SYSCALL,
            Opcode::SYSRET,
            Opcode::PREFETCHW,
            Opcode::PREFETCHNTA,
            Opcode::PREFETCH2,
            Opcode::PREFETCH1,
            Opcode::PREFETCH0,
            Opcode::MOVDIR64B,
        ];

        if COMPLEX.contains(&instr.opcode()) {
            return true;
        }

        if instr.opcode() == Opcode::INVPCID {
            // this #UDs in the VM? is it because i'm not setting invpcid in cpuid..
            return true;
        }

        if instr.opcode() == Opcode::RDPID {
            // rdpid is a specialized rdmsr
            return true;
        }

        if instr.opcode() == Opcode::RDTSCP {
            // raises #UD without CPUID leaf 80000001 edx.rdtscp (bit 27)
            return true;
        }

        if instr.opcode() == Opcode::INVLPGB {
            // guest is not configured to permit invlpgb
            return true;
        }

        if instr.opcode() == Opcode::TLBSYNC {
            // guest is not configured to permit tlbsync
            return true;
        }

        if cet::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if rands::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if xsave::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if pconfig::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if mpk::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if selector_load::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if undef::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if cmov::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if tdx::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if waitpkg::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if uintr::OPCODES.contains(&instr.opcode()) {
            return true;
        }

        if Opcode::MONITOR == instr.opcode() {
            return true;
        }

        if Opcode::MWAIT == instr.opcode() {
            return true;
        }

        if instr.opcode() == Opcode::LAR || instr.opcode() == Opcode::LSL {
            // TODO: needs explicit test (conditional write of dest..)
            return true;
        }

        if instr.operand_present(0) {
            if let Operand::Register { reg } = instr.operand(0) {
                if reg.class() == register_class::CR {
                    return true;
                }

                if reg.class() == register_class::DR {
                    return true;
                }
            }
        }

        if instr.mem_size().is_some() {
            if instr.opcode() == Opcode::BT ||
                instr.opcode() == Opcode::BTS ||
                instr.opcode() == Opcode::BTR ||
                instr.opcode() == Opcode::BTC {
                return true;
            }
        }

        return false;
    }

    mod cet {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::WRUSS,
            Opcode::WRSS,
            Opcode::INCSSP,
            Opcode::SAVEPREVSSP,
            Opcode::SETSSBSY,
            Opcode::CLRSSBSY,
            Opcode::RSTORSSP,
            Opcode::ENDBR64,
            Opcode::ENDBR32,
        ];
    }

    // TODO: these don't fit in the generic harness because the destination register is scrombled
    // and checking permutations will assume the instruction depends on some missed read (which
    // *is* kinda true...)
    mod rands {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::RDRAND,
            Opcode::RDSEED,
        ];
    }

    mod cmov {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::CMOVA,
            Opcode::CMOVNA,
            Opcode::CMOVB,
            Opcode::CMOVNB,
            Opcode::CMOVG,
            Opcode::CMOVLE,
            Opcode::CMOVL,
            Opcode::CMOVGE,
            Opcode::CMOVO,
            Opcode::CMOVNO,
            Opcode::CMOVP,
            Opcode::CMOVNP,
            Opcode::CMOVS,
            Opcode::CMOVNS,
            Opcode::CMOVZ,
            Opcode::CMOVNZ,
        ];

    }

    mod tdx {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::TDCALL,
            Opcode::SEAMRET,
            Opcode::SEAMOPS,
            Opcode::SEAMCALL,
        ];

    }

    mod waitpkg {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::TPAUSE,
            Opcode::UMONITOR,
            Opcode::UMWAIT,
        ];

    }

    mod uintr {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::UIRET,
            Opcode::SENDUIPI,
            Opcode::TESTUI,
            Opcode::CLUI,
            Opcode::STUI,
        ];

    }

    mod undef {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::UD0,
            Opcode::UD1,
            Opcode::UD2,
        ];

    }

    // these need standalone testing because loading a bogus selector produces #GP
    mod selector_load {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::LFS,
            Opcode::LGS,
            Opcode::LSS,
            Opcode::SWAPGS,
        ];

    }

    mod xsave {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::FXSAVE,
            Opcode::FXRSTOR,
            Opcode::XSAVE,
            Opcode::XSAVEOPT,
            Opcode::XSAVEC,
            Opcode::XSAVEC64,
            Opcode::XSAVES,
            Opcode::XSAVES64,
            Opcode::XRSTOR,
            Opcode::XRSTORS,
            Opcode::XRSTORS64,
        ];

    }

    mod pconfig {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::PCONFIG,
            Opcode::SKINIT,
        ];

    }

    mod ptwrite {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::PTWRITE,
        ];

    }

    mod mpk {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::RDPKRU,
            Opcode::WRPKRU,
        ];

    }

    mod ctrl_instrs {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::CLTS,
            Opcode::XGETBV,
            Opcode::XSETBV,
            Opcode::LDMXCSR,
            Opcode::STMXCSR,
            Opcode::LMSW,
            Opcode::SMSW,
            Opcode::SWAPGS,
            Opcode::RDFSBASE,
            Opcode::WRFSBASE,
            Opcode::RDGSBASE,
            Opcode::WRGSBASE,
        ];

    }

    mod enqcmd {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::ENQCMD,
            Opcode::ENQCMDS,
        ];

    }

    // instructions related to operating VT-x/SVM virtual machines.
    // TODO: these are not (yet) tested.
    mod vm_instrs {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::STGI,
            Opcode::CLGI,
            Opcode::VMREAD,
            Opcode::VMWRITE,
            Opcode::VMCLEAR,
            Opcode::VMLAUNCH,
            Opcode::VMRESUME,
            Opcode::VMXON,
            Opcode::VMXOFF,
            Opcode::VMFUNC,
            Opcode::VMPTRLD,
            Opcode::VMPTRST,
            Opcode::VMMCALL,
            Opcode::VMLOAD,
            Opcode::VMSAVE,
            Opcode::VMRUN,
            Opcode::VMCALL,
        ];
    }

    mod sse_gpr {
        use yaxpeax_x86::real_mode::Opcode;

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::PEXTRB,
            Opcode::PEXTRW,
            Opcode::PEXTRD,
            Opcode::EXTRACTPS,
        ];
    }

    // check the collection of {l,s}{g,i,l}dt. these instructions are at the combination of
    // "interesting memory size" and "interesting [non]interaction with prefixes"
    //
    // because these modify VM control structures, the VM is not retained across checks in a test.
    mod table_instrs {
        use super::create_test_vm;
        use super::MemPatch;
        use super::check_behavior_with_regs;
        use super::TestAccesses;
        use super::ExpectedRegAccess;
        use super::ExpectedMemAccess;

        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::real_mode::{Instruction, Opcode, RegSpec};

        pub static OPCODES: &'static [Opcode] = &[
            Opcode::LGDT,
            Opcode::SGDT,
            Opcode::LIDT,
            Opcode::SIDT,
            Opcode::LLDT,
            Opcode::SLDT,
            Opcode::LTR,
            Opcode::STR,
        ];

        #[test]
        fn verify_lgdt() {
            let decoder = super::host_decoder();
            let mut buf = Instruction::default();

            const PATCH_ADDR: u16 = 0x10;

            // the instructions below read `[rax]`, so set `rax` as used and declare it in
            // `used_regs` so randomization does not clobber.
            let mut used_regs = [false; 16];
            used_regs[0] = true;
            used_regs[1] = true;
            used_regs[4] = true;

            let tests: Vec<(&'static [u8], TestAccesses)> = vec![
                // lgdt mword [rax]
                (&[0x0f, 0x01, 0x10], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![ExpectedRegAccess { write: false, reg: RegSpec::eax() }],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 6 }],
                }),
                (&[0x66, 0x0f, 0x01, 0x10], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![ExpectedRegAccess { write: false, reg: RegSpec::eax() }],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 6 }],
                }),
                (&[0x67, 0x0f, 0x01, 0x10], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![ExpectedRegAccess { write: false, reg: RegSpec::eax() }],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 6 }],
                }),
            ];

            for (inst, accs) in tests.into_iter() {
                let mut vm = create_test_vm();
                vm.set_single_step(true).expect("can enable single-step");

                // set up lgdt to re-load the same gdt.
                let mut patch_bytes = Vec::new();
                patch_bytes.extend_from_slice(&4095u16.to_le_bytes());
                patch_bytes.extend_from_slice(&vm.gdt_addr().0.to_le_bytes());
                let patch = MemPatch {
                    addr: PATCH_ADDR,
                    bytes: patch_bytes,
                };
                let mut regs = vm.get_regs().expect("can get regs");
                regs.rax = patch.addr as u64;
                regs.rbx = patch.addr as u64;
                regs.rsi = 0;
                vm.set_regs(&regs).expect("can set regs");

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                check_behavior_with_regs(&mut vm, &inst, Some(accs), &[patch]).expect("behavior check is ok");
            }
        }

        #[test]
        fn verify_lidt() {
            let decoder = super::host_decoder();
            let mut buf = Instruction::default();

            const PATCH_ADDR: u16 = 0x600;

            // the instructions below read `[rax]`, so set `rax` as used and declare it in
            // `used_regs` so randomization does not clobber.
            let mut used_regs = [false; 16];
            used_regs[0] = true;
            used_regs[1] = true;
            used_regs[4] = true;

            let tests: Vec<(&'static [u8], TestAccesses)> = vec![
                // lidt mword [rax]
                (&[0x0f, 0x01, 0x18], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 6 }],
                }),
                (&[0x66, 0x0f, 0x01, 0x18], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 6 }],
                }),
                (&[0x67, 0x0f, 0x01, 0x18], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::eax() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 6 }],
                }),
            ];

            for (inst, accs) in tests.into_iter() {
                let mut vm = create_test_vm();
                vm.set_single_step(true).expect("can enable single-step");

                // set up lidt to re-load the same idt.
                let mut patch_bytes = Vec::new();
                patch_bytes.extend_from_slice(&4095u16.to_le_bytes());
                patch_bytes.extend_from_slice(&vm.idt_addr().0.to_le_bytes());
                let patch = MemPatch {
                    addr: PATCH_ADDR,
                    bytes: patch_bytes,
                };
                let mut regs = vm.get_regs().expect("can get regs");
                regs.rax = patch.addr as u64;
                regs.rbx = patch.addr as u64;
                regs.rsi = 0;
                vm.set_regs(&regs).expect("can set regs");

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                check_behavior_with_regs(&mut vm, &inst, Some(accs), &[patch]).expect("behavior check is ok");
            }
        }

        // TODO: this results in `single-step ended at 000fee04, expected 00000004`. `#[ignore]`
        // for the time being, because this seems like maybe i'm misunderstanding what to expect..?
        #[ignore]
        #[test]
        fn verify_lldt() {
            let decoder = super::host_decoder();
            let mut buf = Instruction::default();

            const PATCH_ADDR: u16 = 0x600;

            // the instructions below read `[rax]`, so set `rax` as used and declare it in
            // `used_regs` so randomization does not clobber.
            let mut used_regs = [false; 16];
            used_regs[0] = true;
            used_regs[1] = true;
            used_regs[4] = true;

            let tests: Vec<(&'static [u8], TestAccesses)> = vec![
                // lldt mword [rax]
                (&[0x0f, 0x00, 0x10], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 2 }],
                }),
                (&[0x66, 0x0f, 0x00, 0x10], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 2 }],
                }),
                (&[0x67, 0x0f, 0x00, 0x10], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![ExpectedRegAccess { write: false, reg: RegSpec::eax() }],
                    expected_mem: vec![ExpectedMemAccess { write: false, addr: PATCH_ADDR, size: 2 }],
                }),
            ];

            for (inst, accs) in tests.into_iter() {
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
                    addr: PATCH_ADDR,
                    bytes: patch_bytes,
                };
                let mut regs = vm.get_regs().expect("can get regs");
                regs.rax = patch.addr as u64;
                regs.rbx = patch.addr as u64;
                regs.rsi = 0;
                vm.set_regs(&regs).expect("can set regs");

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                check_behavior_with_regs(&mut vm, &inst, Some(accs), &[patch]).expect("behavior check is ok");
            }
        }

        #[test]
        fn verify_table_stores() {
            let decoder = super::host_decoder();
            let mut buf = Instruction::default();

            const PATCH_ADDR: u16 = 0x10;

            // the instructions below read `[rax]`, so set `rax` as used and declare it in
            // `used_regs` so randomization does not clobber.
            let mut used_regs = [false; 16];
            used_regs[0] = true;
            used_regs[1] = true;
            used_regs[4] = true;

            let tests: Vec<(&'static [u8], TestAccesses)> = vec![
                // sgdt
                (&[0x0f, 0x01, 0x00], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 6 }],
                }),
                (&[0x66, 0x0f, 0x01, 0x00], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 6 }],
                }),
                (&[0x67, 0x0f, 0x01, 0x00], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![ExpectedRegAccess { write: false, reg: RegSpec::eax() }],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 6 }],
                }),
                // sidt
                (&[0x0f, 0x01, 0x08], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 10 }],
                }),
                (&[0x66, 0x0f, 0x01, 0x08], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 6 }],
                }),
                (&[0x67, 0x0f, 0x01, 0x08], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![ExpectedRegAccess { write: false, reg: RegSpec::eax() }],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 6 }],
                }),
                // TODO: this results in `single-step ended at 000fee04, expected 00000004`.
                // `#[ignore]` for the time being, like verify_lldt`, because this seems like maybe
                // i'm misunderstanding what to expect..?
                // sldt
                /*
                (&[0x0f, 0x00, 0x00], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 2 }],
                }),
                (&[0x66, 0x0f, 0x00, 0x00], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![
                        ExpectedRegAccess { write: false, reg: RegSpec::ebx() },
                        ExpectedRegAccess { write: false, reg: RegSpec::esi() },
                    ],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 2 }],
                }),
                (&[0x67, 0x0f, 0x00, 0x00], TestAccesses {
                    preserve_rsp: true,
                    used_regs,
                    expected_reg: vec![ExpectedRegAccess { write: false, reg: RegSpec::eax() }],
                    expected_mem: vec![ExpectedMemAccess { write: true, addr: PATCH_ADDR, size: 2 }],
                }),
                */
            ];

            for (inst, accs) in tests.into_iter() {
                let mut vm = create_test_vm();
                vm.set_single_step(true).expect("can enable single-step");
                let mut regs = vm.get_regs().expect("can get regs");
                regs.rax = PATCH_ADDR as u64;
                regs.rbx = PATCH_ADDR as u64;
                regs.rsi = 0;
                vm.set_regs(&regs).expect("can set regs");

                let mut reader = U8Reader::new(&inst);
                assert!(decoder.decode_into(&mut buf, &mut reader).is_ok());

                check_behavior_with_regs(&mut vm, &inst, Some(accs), &[]).expect("behavior check is ok");
            }
        }
    }
}
