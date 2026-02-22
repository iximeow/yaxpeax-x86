
#[cfg(target_arch = "x86_64")]
mod kvm {
    use kvm_ioctls::{Kvm, VcpuFd, VmFd, VcpuExit};
    use kvm_bindings::{
        kvm_guest_debug, kvm_userspace_memory_region, kvm_segment, kvm_regs, kvm_sregs,
        KVM_GUESTDBG_ENABLE, KVM_GUESTDBG_SINGLESTEP,
    };

    use rand::prelude::*;

    /// a test VM for running arbitrary instructions.
    ///
    /// there is one CPU which is configured for long-mode execution. all memory is
    /// identity-mapped with 1GiB pages. page tables are configured to cover 512 GiB of memory, but
    /// much much lss than that is actually allocated and usable through `memory.`
    ///
    /// it is configured with `mem_size` bytes of memory at guest address 0, accessible through
    /// host pointer `memory`.
    #[allow(unused)]
    struct TestVm {
        vm: VmFd,
        vcpu: VcpuFd,
        memory: *mut u8,
        mem_size: usize,
    }

    const GB: u64 = 1 << 30;

    #[derive(Copy, Clone)]
    struct GuestAddress(u64);

    struct VmPageTables<'vm> {
        vm: &'vm TestVm,
        base: GuestAddress,
    }

    impl<'vm> VmPageTables<'vm> {
        fn pml4_addr(&self) -> GuestAddress {
            self.base
        }

        fn pdpt_addr(&self) -> GuestAddress {
            GuestAddress(self.base.0 + 0x1000)
        }

        fn pml4_mut(&self) -> *mut u64 {
            // SAFETY: creating VmPageTables implies we've asserted that we can form host pointers
            // for all addresses in the page tables.
            unsafe {
                self.vm.host_ptr(self.pml4_addr()) as *mut u64
            }
        }

        fn pdpt_mut(&self) -> *mut u64 {
            // SAFETY: creating VmPageTables implies we've asserted that we can form host pointers
            // for all addresses in the page tables.
            unsafe {
                self.vm.host_ptr(self.pdpt_addr()) as *mut u64
            }
        }
    }

    impl TestVm {
        fn create() -> TestVm {
            let kvm = Kvm::new().unwrap();

            let vm = kvm.create_vm().unwrap();

            let mem_size = 1024 * 1024;
            let mem_addr: *mut u8 = unsafe {
                libc::mmap(
                    core::ptr::null_mut(),
                    mem_size,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_ANONYMOUS | libc::MAP_SHARED | libc::MAP_NORESERVE,
                    -1,
                    0,
                ) as *mut u8
            };

            assert!(!mem_addr.is_null());
            // look, mmap should only be in the business of returning page-aligned addresses but i
            // just wanna see it, you know...
            assert!(mem_addr as usize % 4096 == 0);

            let region = kvm_userspace_memory_region {
                slot: 0,
                guest_phys_addr: 0x0000,
                memory_size: mem_size as u64,
                userspace_addr: mem_addr as u64,
                flags: 0,
            };
            unsafe { vm.set_user_memory_region(region).unwrap() };

            let vcpu = vm.create_vcpu(0).unwrap();

            let mut this = TestVm {
                vm,
                vcpu,
                memory: mem_addr,
                mem_size,
            };

            let mut vcpu_sregs = this.vcpu.get_sregs().unwrap();

            unsafe {
                this.configure_identity_paging(&mut vcpu_sregs);
                this.configure_selectors(&mut vcpu_sregs);
            }

            vcpu_sregs.efer = 0x0000_0500; // LME | LMA

            this.vcpu.set_sregs(&vcpu_sregs).unwrap();

            this
        }

        // TODO: seems like there's a KVM bug where if the VM is configured for single-step and the
        // single-stepped instruction is a read-modify-write to MMIO memory, the single-step
        // doesn't actually take effect. compare `0x33 0x00` and `0x31 0x00`. what the hell!
        fn set_single_step(&mut self, active: bool) {
            let mut guest_debug = kvm_guest_debug::default();

            if active {
                guest_debug.control = KVM_GUESTDBG_ENABLE | KVM_GUESTDBG_SINGLESTEP
            };

            self.vcpu.set_guest_debug(&guest_debug).unwrap();
        }

        fn run(&mut self) -> VcpuExit<'_> {
            self.vcpu.run().unwrap()
        }

        unsafe fn host_ptr(&self, address: GuestAddress) -> *mut u8 {
            self.memory.offset(address.0 as isize)
        }

        fn gdt_addr(&self) -> GuestAddress {
            GuestAddress(0)
        }

        fn page_table_addr(&self) -> GuestAddress {
            GuestAddress(0x10000)
        }

        fn code_addr(&self) -> GuestAddress {
            GuestAddress(self.mem_size as u64 - 4096)
        }

        fn guest_mem_size(&self) -> u64 {
            512 * (GB as u64)
        }

        fn check_range(&self, base: GuestAddress, size: u64) {
            let base = base.0;
            let end = base.checked_add(size).expect("no overflow");

            assert!(base < self.mem_size as u64);
            assert!(self.mem_size as u64 >= end);
        }

        pub fn program(&mut self, code: &[u8], regs: &mut kvm_regs) {
            let addr = self.code_addr();
            self.check_range(addr, code.len() as u64);

            // SAFETY: `check_range` above validates the range to copy, and... please do not
            // provide a slice of guest memory as what the guest should be programmed for...
            unsafe {
                std::ptr::copy_nonoverlapping(code.as_ptr(), self.host_ptr(addr), code.len());
            }

            regs.rip = self.code_addr().0;
        }

        fn gdt_entry_mut(&mut self, idx: u16) -> *mut u64 {
            // the GDT is set up at addresses 0..64k:
            //
            // > 3.5.1 Segment Descriptor Tables
            // > A segment descriptor table is an array of segment descriptors (see Figure 3-10). A
            // > descriptor table is variable in length and can contain up to 8192 (2^13) 8-byte
            // > descriptors.

            let addr = GuestAddress(self.gdt_addr().0 + (idx as u64 * 8));
            assert!(idx < 8192);
            self.check_range(addr, std::mem::size_of::<u64>() as u64);

            // SAFETY: idx * 8 can't overflow isize, and we've asserted the end of the pointer is
            // still inside the allocation (`self.memory`).
            unsafe {
                self.host_ptr(addr) as *mut u64
            }
        }

        fn page_tables(&self) -> VmPageTables<'_> {
            let base = self.page_table_addr();

            // the page tables are really just two pages: a PML4 and a PDPT for its first 512G of
            // address space.
            self.check_range(base, 0x2000);

            VmPageTables {
                vm: self,
                base,
            }
        }

        unsafe fn configure_identity_paging(&mut self, sregs: &mut kvm_sregs) {
            let pt = self.page_tables();

            // we're only setting up one PDPT, which can have up to 512 PDPTE covering 1G each.
            assert!(self.guest_mem_size() <= 512 * GB);

            // TODO: expects 1G page support

            pt.pml4_mut().write(
                1 << 0 | // P
                1 << 1 | // RW
                1 << 2 | // user access allowed. but no user code will run so not strictly needed.
                0 << 3 | // PWT (TODO: configure PAT explicitly, but PAT0 is sufficient)
                0 << 4 | // PCD (TODO: configure PAT explicitly, but PAT0 is sufficient)
                0 << 5 | // A
                0 << 6 | // ignored
                0 << 7 | // PS (reserved must-be-0)
                0 << 11 | // R (for ordinary paging, ignored; for HLAT ...)
                pt.pdpt_addr().0
            );

            let mut mapped: u64 = 0;
            // we've set up the first PML4 to point to a PDPT, so we should actually set it up!
            let pdpt = pt.pdpt_mut();
            // PDPTEs start at the start of PDPT..
            let mut pdpte = pdpt;
            let entry_bits: u64 =
                1 << 0 | // P
                1 << 1 | // RW
                1 << 2 | // user accesses allowed (everything is under privilege level 0 tho)
                0 << 3 | // PWT (TODO: configure PAT explicitly, but PAT0 is sufficient)
                0 << 4 | // PCD (TODO: configure PAT explicitly, but PAT0 is sufficient)
                0 << 5 | // Accessed
                0 << 6 | // Dirty
                1 << 7 | // Page size (1 implies 1G page)
                1 << 8 | // Global (if cr4.pge)
                0 << 9 |
                0 << 10 |
                0 << 11 | // for ordinary paging, ignored. for HLAT, ...
                0 << 12; // PAT (TODO: configure explicitly, but PAT0 is sufficient. verify MTRR sets PAT0 to WB?)

            while mapped < self.guest_mem_size() {
                let phys_num = mapped >> 30;
                let entry = entry_bits | (phys_num << 30);
                pdpte.write(entry);
                pdpte = pdpte.offset(1);
                // eprintln!("mapped 1g at {:08x}", mapped);
                mapped += 1 << 30;
            }

            sregs.cr0 = 0x8000_0001; // cr0.PE | cr0.PG
            sregs.cr3 = pt.pml4_addr().0 as u64;
            sregs.cr4 = 1 << 5; // enable PAE
        }

        unsafe fn configure_selectors(&mut self, sregs: &mut kvm_sregs) {
            // we have to set descriptor information directly. this avoids having to load selectors
            // as the first instructions on the vCPU, which is simplifying. but if we want the
            // information in these selectors to match with anything in a GDT (i do!) we'll have to
            // keep this initial state lined up with GDT entries ourselves.
            //
            // we could avoid setting up the GDT for the most part, but anything that might
            // legitimately load the "valid" current segment selector would instead clobber the
            // selector with zeroes.

            sregs.cs.base = 0;
            sregs.cs.limit = 0;
            sregs.cs.selector = 4 * 8;
            sregs.cs.type_ = 0b1010; // see SDM table 3-1 Code- and Data-Segment Types
            sregs.cs.present = 1;
            sregs.cs.dpl = 0;
            sregs.cs.db = 0;
            sregs.cs.s = 1;
            sregs.cs.l = 1;
            sregs.cs.g = 0;
            sregs.cs.avl = 1;

            sregs.ds.base = 0;
            sregs.ds.limit = 0;
            sregs.ds.selector = 5 * 8;
            sregs.ds.type_ = 0b0010; // see SDM table 3-1 Code- and Data-Segment Types
            sregs.ds.present = 1;
            sregs.ds.dpl = 0;
            sregs.ds.db = 1;
            sregs.ds.s = 1;
            sregs.ds.l = 0;
            sregs.ds.g = 1;
            sregs.ds.avl = 0;

            sregs.es = sregs.ds;
            sregs.fs = sregs.ds;
            sregs.gs = sregs.ds;
            sregs.ss = sregs.ds;

            fn encode_segment(seg: &kvm_segment) -> u64 {
                let base = seg.base as u64;
                let limit = seg.limit as u64;

                let lim_low = limit & 0xffff;
                let lim_high = (limit >> 16) & 0xf;
                let addr_low = base & 0xffff;
                let desc_low = lim_low | (addr_low << 16);

                let base_mid = (base >> 16) & 0xff;
                let base_high = (base >> 24) & 0xff;
                let desc_high = base_mid
                    | (seg.type_ as u64) << 8
                    | (seg.s as u64) << 12
                    | (seg.dpl as u64) << 13
                    | (seg.present as u64) << 15
                    | lim_high << 16
                    | (seg.avl as u64) << 20
                    | (seg.l as u64) << 21
                    | (seg.db as u64) << 22
                    | (seg.g as u64) << 23
                    | base_high << 24;

                desc_low | (desc_high << 32)
            }

            sregs.gdt.base = self.gdt_addr().0;
            sregs.gdt.limit = 0xffff;

            self.gdt_entry_mut(4).write(encode_segment(&sregs.cs));
            self.gdt_entry_mut(5).write(encode_segment(&sregs.ds));
        }
    }

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

    struct AccessTestCtx<'regs> {
        regs: &'regs mut kvm_regs,
        used_regs: [bool; 16],
        expected_reg: Vec<ExpectedRegAccess>,
        expected_mem: Vec<ExpectedMemAccess>,
    }

    impl<'regs> AccessTestCtx<'regs> {
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

    impl<'regs> AccessVisitor for AccessTestCtx<'regs> {
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
                    if self.used_regs[reg.num() as usize] {
                        let value = unsafe {
                            (self.regs as *mut _ as *mut u64).offset(kvm_reg_nr as isize).read()
                        };
                        Some(value)
                    } else {
                        // register value allocation is done carefully to keep memory accesses out
                        // of the first 1G of memory. that keeps test instructions from clobbering
                        // page tables. registers used for memory access are set to at least to 8G,
                        // so that disp32 offsets can cause an instruction to access only as early
                        // as 4G. SIB addressing means the highest address that may be accessed
                        // could be 8G + 0xf00_0000 + (8G + 0x1000_0000) * 4 + 2G, or somewhere
                        // around 42G.
                        //
                        // since the highest accessible address is (probably) 2^48 or 256T, even in
                        // the most convoluted case we're always going to be forming canonical
                        // (lower-half) addresses.
                        let value = 0x2_0000_0000 + (kvm_reg_nr as u64 + 1) * 0x100_0000;
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

    fn run_with_mem_checks(vm: &mut TestVm, expected_end: u64, expected_mem: &[ExpectedMemAccess]) {
        let mut expected_mem = expected_mem.to_vec();
        let mut unexpected_mem = Vec::new();
        let mut exits = 0;
        let end_pc = loop {
            let exit = vm.run();
            exits += 1;
            match exit {
                VcpuExit::MmioRead(addr, buf) => {
                    let position = expected_mem.iter().position(|e| {
                        e.addr == addr && e.size as usize == buf.len() && e.write == false
                    });

                    if let Some(position) = position {
                        expected_mem.swap_remove(position);
                    } else {
                        unexpected_mem.push((false, addr, buf.len()));
                    }
                    // TODO: better
                    buf.fill(1);
                }
                VcpuExit::MmioWrite(addr, buf) => {
                    let position = expected_mem.iter().position(|e| {
                        e.addr == addr && e.size as usize == buf.len() && e.write
                    });

                    if let Some(position) = position {
                        expected_mem.swap_remove(position);
                    } else {
                        unexpected_mem.push((true, addr, buf.len()));
                    }

                    // TODO: verify write? probably can't without full semantics.
                }
                VcpuExit::Debug(info) => {
                    break info.pc;
                }
                VcpuExit::Hlt => {
                    let regs = vm.vcpu.get_regs().unwrap();
                    break regs.rip;
                }
                other => {
                    panic!("unhandled exit: {:?} ... after {}", other, exits);
                }
            }
        };

        if end_pc != expected_end - 1 && end_pc != expected_end {
            panic!("single-step ended at {:08x}, expected {:08x}", end_pc, expected_end);
        }

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
        return;
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
                (l, s) => {
                    panic!("unhandled register-contains test: {:?}/{:?}", l, s);
                }
            }
        }
    }
    fn write_matches_reg(reg: RegSpec, diff: u64) -> bool {
        match reg.class() {
            register_class::B => {
                // non-rex byte regs are al, cl, dl, bl, ah, ch, dh, bh
                let mask = if reg.num() < 4 {
                    0xff
                } else {
                    0xff00
                };
                (diff & !mask) == 0
            },
            // but rex byte regs are all low-byte
            register_class::RB => (diff & !0xff) == 0,
            register_class::W => (diff & !0xffff) == 0,
            register_class::D => (diff & !0xffffffff) == 0,
            register_class::Q => (diff & !0xffffffff_ffffffff) == 0,
            register_class::RFLAGS => (diff & !0xffffffff_ffffffff) == 0,
            other => {
                panic!("unhandled register class: {:?}", other);
            }
        }
    }

    fn verify_reg(
        unexpected_regs: &mut Vec<UnexpectedRegChange>, expected_regs: &[ExpectedRegAccess],
        changed_reg: RegSpec, before: u64, after: u64,
    ) {
        let diff = before ^ after;
        if diff != 0 {
            // could be a write. full write? maybe!
            let position = expected_regs.iter().position(|e| {
                if !e.write {
                    return false;
                }

                if !check_contains(changed_reg, e.reg) {
                    return false;
                }

                write_matches_reg(e.reg, diff)
            });

            if let Some(_position) = position {
                // nothing to do with it right now
            } else {
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

    fn verify_reg_changes(
        expected_regs: &[ExpectedRegAccess],
        before_regs: kvm_regs, after_regs: kvm_regs,
        before_sregs: kvm_sregs, after_sregs: kvm_sregs
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

        if !unexpected_regs.is_empty() {
            eprintln!("unexpected reg changes:");
            for change in unexpected_regs {
                eprintln!("  {}: {:08x} -> {:08x}", change.reg.name(), change.before, change.after);
            }
            panic!("stop");
        }
    }

    fn check_behavior(vm: &mut TestVm, inst: &[u8]) {
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
        let behavior = decoded.behavior();
        eprintln!("checking behavior of {}", decoded);

        let before_sregs = vm.vcpu.get_sregs().unwrap();
        let mut regs = vm.vcpu.get_regs().unwrap();
        vm.set_single_step(true);
        vm.program(insts.as_slice(), &mut regs);

        let mut rng = rand::rng();

        regs.rax = rng.next_u64();
        regs.rbx = rng.next_u64();
        regs.rcx = rng.next_u64();
        regs.rdx = rng.next_u64();
        regs.rsp = rng.next_u64();
        regs.rbp = rng.next_u64();
        regs.rsi = rng.next_u64();
        regs.rdi = rng.next_u64();

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
            used_regs: [false; 16],
            expected_reg: Vec::new(),
            expected_mem: Vec::new(),
        };
        behavior.visit_accesses(&mut ctx).expect("can visit accesses");
        let (expected_reg, expected_mem) = ctx.into_expectations();

        fn compute_dontcares(accesses: &[ExpectedRegAccess]) -> Vec<RegSpec> {
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
                        Some(reg.num() & 0b111)
                    }
                    _ => {
                        None
                    }
                }
            }

            for acc in accesses.iter() {
                if acc.write {
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
                        Some(reg.num() & 0b111)
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

        let dontcare_regs = compute_dontcares(&expected_reg);
        let written_regs = compute_writes(&expected_reg);

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

        permute_dontcares(dontcare_regs.as_slice(), &mut regs);

        vm.vcpu.set_regs(&regs).unwrap();

        run_with_mem_checks(vm, regs.rip + insts.len() as u64, expected_mem.as_slice());

        let initial_after_regs = vm.vcpu.get_regs().unwrap();
        let after_sregs = vm.vcpu.get_sregs().unwrap();

        verify_reg_changes(&expected_reg, regs, initial_after_regs, before_sregs, after_sregs);

        for _ in 0..4 {
            permute_dontcares(dontcare_regs.as_slice(), &mut regs);

            vm.vcpu.set_regs(&regs).unwrap();

            run_with_mem_checks(vm, regs.rip + insts.len() as u64, expected_mem.as_slice());

            let after_regs = vm.vcpu.get_regs().unwrap();
            let after_sregs = vm.vcpu.get_sregs().unwrap();

            verify_reg_changes(&expected_reg, regs, after_regs, before_sregs, after_sregs);
            verify_dontcares(written_regs.as_slice(), &initial_after_regs, &after_regs);
        }
    }

    #[test]
    fn kvm_verify_xor_reg_mem() {
        let mut vm = TestVm::create();

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
        let mut vm = TestVm::create();

        // `inc eax`
        let inst: &'static [u8] = &[0xff, 0xc0];
        check_behavior(&mut vm, inst);

        // `inc dword [rax]`
        let inst: &'static [u8] = &[0xff, 0x00];
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn kvm_verify_push() {
        let mut vm = TestVm::create();

        // `push rax`
        let inst: &'static [u8] = &[0x50];
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn behavior_verify_kvm() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::long_mode::{Instruction, InstDecoder};

        let mut vm = TestVm::create();

        let decoder = InstDecoder::default();
        let mut buf = Instruction::default();

        for word in 0..u16::MAX {
            let inst = word.to_le_bytes();
            let mut reader = U8Reader::new(&inst);
            if decoder.decode_into(&mut buf, &mut reader).is_ok() {
                // some instructions may just be one byte, so figure out the length and only check
                // that many bytes of instructions for specific behavior..
                use yaxpeax_arch::LengthedInstruction;
                let inst_len = 0.wrapping_offset(buf.len()) as usize;
                if inst_len == 1 {
                    eprintln!("checking behavior of {:02x}: {}", inst[0], buf);
                } else {
                    eprintln!("checking behavior of {:02x} {:02x}: {}", inst[0], inst[1], buf);
                }
                check_behavior(&mut vm, &inst[..inst_len]);
            }
        }
    }
}
