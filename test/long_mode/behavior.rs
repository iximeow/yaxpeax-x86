
#[cfg(target_arch = "x86_64")]
mod kvm {
    use kvm_ioctls::{Kvm, VcpuFd, VmFd, VcpuExit};
    use kvm_bindings::{
        kvm_guest_debug, kvm_userspace_memory_region, kvm_segment, kvm_regs, kvm_sregs,
        KVM_GUESTDBG_ENABLE, KVM_GUESTDBG_SINGLESTEP,
    };

    /// a test VM for running arbitrary instructions.
    ///
    /// there is one CPU which is configured for long-mode execution. all memory is
    /// identity-mapped with 1GiB pages.
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

            const GB: usize = 1 << 30;

            // we're only setting up one PDPT, which can have up to 512 PDPTE covering 1G each.
            assert!(self.mem_size <= 512 * GB);

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

            while mapped < self.mem_size as u64 {
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

    #[test]
    fn behavior_verify_kvm() {
        let mut vm = TestVm::create();

        let inst: &'static [u8] = &[0x33, 0x01, 0xf4];

        let code_addr = 1024 * 1024 - 4096;

        {
            // safety: VM is not running so we are the only writers to this memory
            let slice = unsafe {
                std::slice::from_raw_parts_mut(
                    vm.memory.offset(code_addr),
                    0x1000
                )
            };
            slice[..inst.len()].copy_from_slice(&inst);
        }


        let before_sregs = vm.vcpu.get_sregs().unwrap();
        let mut regs = vm.vcpu.get_regs().unwrap();
        vm.program(inst, &mut regs);
        regs.rax = 0;
        regs.rcx = code_addr as u64;
        vm.vcpu.set_regs(&regs).unwrap();

        vm.set_single_step(true);

        let res = vm.run();
        eprintln!("exit: {:?}", res);

        let after_regs = vm.vcpu.get_regs().unwrap();
        eprintln!("rip is now {:08x}", after_regs.rip);
        eprintln!("rax is now {:08x}", after_regs.rax);
        eprintln!("rcx is now {:08x}", after_regs.rcx);
        let after_sregs = vm.vcpu.get_sregs().unwrap();

        fn get_reg_changes(
            before_regs: kvm_regs, after_regs: kvm_regs,
            before_sregs: kvm_sregs, after_sregs: kvm_sregs
        ) {
        }

        let changed_regs = get_reg_changes(regs, after_regs, before_sregs, after_sregs);
    }
}
