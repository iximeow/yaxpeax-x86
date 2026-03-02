
#[cfg(target_arch = "x86_64")]
mod kvm {
    use std::convert::TryInto;
    use kvm_ioctls::{Kvm, VcpuFd, VmFd, VcpuExit};
    use kvm_bindings::{
        kvm_guest_debug, kvm_userspace_memory_region, kvm_segment, kvm_regs, kvm_sregs,
        KVM_GUESTDBG_ENABLE, KVM_GUESTDBG_SINGLESTEP,
    };

    use yaxpeax_x86::long_mode::behavior::Exception;

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

    fn encode_segment(seg: &kvm_segment) -> u64 {
        let base = seg.base as u64;
        let limit = seg.limit as u64;

        let lim_low = limit & 0xffff;
        let lim_high = (limit >> 16) & 0xf;
        let addr_low = base & 0xffff;
        let desc_low = lim_low | (addr_low << 16);

        let base_mid = (base >> 16) & 0xff;
        let base_high = (base >> 24) & 0xff;
        let access_byte = (seg.type_ as u64)
            | (seg.s as u64) << 4
            | (seg.dpl as u64) << 5
            | (seg.present as u64) << 7;
        let flaglim_byte = lim_high
            | (seg.avl as u64) << 4
            | (seg.l as u64) << 5
            | (seg.db as u64) << 6
            | (seg.g as u64) << 7;
        let desc_high = base_mid
            | access_byte << 8
            | flaglim_byte << 16
            | base_high << 24;

        desc_low | (desc_high << 32)
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

            let mut vcpu_regs = this.vcpu.get_regs().unwrap();
            let mut vcpu_sregs = this.vcpu.get_sregs().unwrap();

            unsafe {
                this.configure_identity_paging(&mut vcpu_sregs);
                this.configure_selectors(&mut vcpu_sregs);
                this.configure_idt(&mut vcpu_regs, &mut vcpu_sregs);
            }

            vcpu_sregs.efer = 0x0000_0500; // LME | LMA

            this.vcpu.set_regs(&vcpu_regs).unwrap();
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
            GuestAddress(0x1000)
        }

        fn idt_addr(&self) -> GuestAddress {
            GuestAddress(0x2000)
        }

        fn interrupt_handlers_start(&self) -> GuestAddress {
            GuestAddress(0x3000)
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

        /// configuring the IDT implies the IDT might be used which means we want a stack pointer
        /// that can have at least 0x18 bytes pushed to it if an interrupt happens.
        fn stack_addr(&self) -> GuestAddress {
            // it would be nice to point the stack somewhere that we could get MMIO exits and see the
            // processor push words for the interrupt in real time, but this doesn't seem to... work
            // as one might hope. instead, you end up in a loop somewhere around svm_vcpu_run (which
            // you can ^C out of, thankfully).
            //
            // so this picks some guest memory lower down.
            //GuestAddress(0x1_0000_8000)

            // stack grows *down* but if someone pops a lot of bytes from rsp we'd go up and
            // clobber the page tables. so leave a bit of space.
            GuestAddress(0xf800)
        }

        /// selector 0x10 is used for code everywhere in these tests.
        fn selector_cs(&self) -> u16 {
            0x10
        }

        /// selector 0x18 is used for data (all segments; ss, ds, es, etc) everywhere in these tests.
        fn selector_ds(&self) -> u16 {
            0x18
        }

        fn check_range(&self, base: GuestAddress, size: u64) {
            let base = base.0;
            let end = base.checked_add(size).expect("no overflow");

            assert!(base < self.mem_size as u64);
            assert!(self.mem_size as u64 >= end);
        }

        pub fn write_mem(&mut self, addr: GuestAddress, data: &[u8]) {
            self.check_range(addr, data.len() as u64);

            // SAFETY: `check_range` above validates the range to copy, and... please do not
            // provide a slice of guest memory as what the guest should be programmed for...
            unsafe {
                std::ptr::copy_nonoverlapping(
                    data.as_ptr(),
                    self.host_ptr(addr),
                    data.len()
                );
            }
        }

        pub fn program(&mut self, code: &[u8], regs: &mut kvm_regs) {
            let addr = self.code_addr();
            self.write_mem(addr, code);

            regs.rip = addr.0;
        }

        fn gdt_entry_mut(&mut self, idx: u16) -> *mut u64 {
            // the GDT is set up at addresses 0..64k:
            //
            // > 3.5.1 Segment Descriptor Tables
            // > A segment descriptor table is an array of segment descriptors (see Figure 3-10). A
            // > descriptor table is variable in length and can contain up to 8192 (2^13) 8-byte
            // > descriptors.

            assert!(idx < 4096 / 8);
            let addr = GuestAddress(self.gdt_addr().0 + (idx as u64 * 8));
            self.check_range(addr, std::mem::size_of::<u64>() as u64);

            // SAFETY: idx * 8 can't overflow isize, and we've asserted the end of the pointer is
            // still inside the allocation (`self.memory`).
            unsafe {
                self.host_ptr(addr) as *mut u64
            }
        }

        // note this returns a u32, but an IDT is four u32. the u32 this points at is the first of
        // the four for the entry.
        fn idt_entry_mut(&mut self, idx: u8) -> *mut u32 {
            let addr = GuestAddress(self.idt_addr().0 + (idx as u64 * 16));
            self.check_range(addr, std::mem::size_of::<[u64; 2]>() as u64);

            unsafe {
                self.host_ptr(addr) as *mut u32
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
            sregs.cs.selector = self.selector_cs();
            sregs.cs.type_ = 0b1011; // see SDM table 3-1 Code- and Data-Segment Types
            sregs.cs.present = 1;
            sregs.cs.dpl = 0;
            sregs.cs.db = 0;
            sregs.cs.s = 1;
            sregs.cs.l = 1;
            sregs.cs.g = 0;
            sregs.cs.avl = 0;

            sregs.ds.base = 0;
            sregs.ds.limit = 0xffffffff;
            sregs.ds.selector = self.selector_ds();
            sregs.ds.type_ = 0b0011; // see SDM table 3-1 Code- and Data-Segment Types
            sregs.ds.present = 1;
            sregs.ds.dpl = 0;
            sregs.ds.db = 0;
            sregs.ds.s = 1;
            sregs.ds.l = 0;
            sregs.ds.g = 0;
            sregs.ds.avl = 0;

            sregs.es = sregs.ds;
            sregs.fs = sregs.ds;
            sregs.gs = sregs.ds;
            // linux populates the vmcb cpl field with whatever's in ss.dpl. what the hell???
            sregs.ss = sregs.ds;

            sregs.gdt.base = self.gdt_addr().0;
            sregs.gdt.limit = 256 * 8 - 1;

            self.gdt_entry_mut(self.selector_cs() >> 3).write(encode_segment(&sregs.cs));
            self.gdt_entry_mut(self.selector_ds() >> 3).write(encode_segment(&sregs.ds));
        }

        fn write_idt_entry(
            &mut self,
            intr_nr: u8,
            interrupt_handler_cs: u16,
            interrupt_handler_addr: GuestAddress
        ) {
            let idt_ptr = self.idt_entry_mut(intr_nr);

            // entries in the IDT, interrupt and trap descriptors (in the AMD APM, "interrupt-gate"
            // and "trap-gate" descriptors), are described (in the AMD APM) by
            // "Figure 4-24. Interrupt-Gate and Trap-Gate Descriptors—Long Mode". reproduced here:
            //
            //  3   2                 1        |          1                   0
            //  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
            // |---------------------------------------------------------------|
            // |                            res,ign                            | +12
            // |                      target offset[63:32]                     | +8
            // |     target offset[31:16]      |P|DPL|0| type  | res,ign | IST | +4
            // |     target selector           |    target offset[15:0]        | +0
            // |---------------------------------------------------------------|
            //
            // descriptors are encoded with P set, DPL at 0, and type set to 0b1110. TODO: frankly
            // i don't know the mechanical difference between type 0x0e and type 0x0f, but 0x0e
            // works for now.
            let idt_attr_bits = 0b1_00_0_1110_00000_000;
            let low_hi = (interrupt_handler_addr.0 as u32 & 0xffff_0000) | idt_attr_bits;
            let low_lo = (interrupt_handler_cs as u32) << 16 | (interrupt_handler_addr.0 as u32 & 0x0000_ffff);

            unsafe {
                idt_ptr.offset(0).write(low_lo);
                idt_ptr.offset(1).write(low_hi);
                idt_ptr.offset(2).write((interrupt_handler_addr.0 >> 32) as u32);
                idt_ptr.offset(3).write(0); // reserved
            }
        }

        fn configure_idt(&mut self, regs: &mut kvm_regs, sregs: &mut kvm_sregs) {
            const IDT_ENTRIES: u16 = 256;
            sregs.idt.base = self.idt_addr().0;
            sregs.idt.limit = IDT_ENTRIES * 16 - 1; // IDT is 256 entries of 16 bytes each

            for i in 0..IDT_ENTRIES {
                let interrupt_handler_addr = GuestAddress(self.interrupt_handlers_start().0 + i as u64);
                self.write_idt_entry(
                    i.try_into().expect("<u8::MAX interrupts"),
                    self.selector_cs(),
                    interrupt_handler_addr
                );
            }

            // all interrupt handlers are just `hlt`. their position is used to detect which
            // exception/interrupt occurred.
            unsafe {
                std::slice::from_raw_parts_mut(
                    self.host_ptr(self.interrupt_handlers_start()),
                    IDT_ENTRIES as usize
                ).fill(0xf4);
            }

            // finally, set `rsp` to a valid region so that the CPU can push necessary state (see
            // AMD APM section "8.9.3 Interrupt Stack Frame") to actually enter the interrupt
            // handler. if we didn't do this, rsp will probably be zero or something, underflow,
            // page fault on push to 0xffffffff_ffffffff, and just triple fault.
            //
            // TODO: this is our option in 16- and 32-bit modes, but in long mode all the interrupt
            // descriptors could set something in IST to switch stacks outright for exception
            // handling. this might be nice to test rsp permutations in 64-bit code? alternatively
            // we might just have to limit possible rsp permutations so as to be able to test in
            // 16- and 32-bit modes anyway.
            regs.rsp = self.stack_addr().0;
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

    fn run(vm: &mut TestVm) {
        let mut exits = 0;
        let end_pc = loop {
            eprintln!("about to run! here's some state:");
            let regs = vm.vcpu.get_regs().unwrap();
            dump_regs(&regs);
//            let sregs = vm.vcpu.get_sregs().unwrap();
//            eprintln!("sregs: {:?}", sregs);
            let exit = vm.run();
            exits += 1;
            match exit {
                VcpuExit::MmioRead(addr, buf) => {
                    eprintln!("mmio: [{:08x}:{}] <- ..", addr, buf.len());
                    // TODO: better
                    buf.fill(1);
                }
                VcpuExit::MmioWrite(addr, buf) => {
                    eprintln!("mmio: .. -> [{:08x}:{}]", addr, buf.len());
                }
                VcpuExit::Debug(info) => {
                    unsafe {
                        let bytes = vm.host_ptr(GuestAddress(info.pc));
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
                    eprintln!("hit hlt");
                    let regs = vm.vcpu.get_regs().unwrap();
                    dump_regs(&regs);
                    break regs.rip;
                }
                other => {
                    eprintln!("unhandled exit: {:?} ... after {}", other, exits);
                    let regs = vm.vcpu.get_regs().unwrap();
                    dump_regs(&regs);
                    let sregs = vm.vcpu.get_sregs().unwrap();
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

    fn run_with_mem_checks(vm: &mut TestVm, expected_end: u64, expected_mem: &[ExpectedMemAccess]) {
        let mut expected_mem = expected_mem.to_vec();
        let mut unexpected_mem = Vec::new();
        let mut exits = 0;
        let end_pc = loop {
            eprintln!("about to run! here's some state:");
            let regs = vm.vcpu.get_regs().unwrap();
            eprintln!("regs: {:?}", regs);
//            let sregs = vm.vcpu.get_sregs().unwrap();
//            eprintln!("sregs: {:?}", sregs);
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
                    eprintln!("unhandled exit: {:?} ... after {}", other, exits);
                    let regs = vm.vcpu.get_regs().unwrap();
                    eprintln!("regs: {:?}", regs);
//                    let sregs = vm.vcpu.get_sregs().unwrap();
//                    eprintln!("sregs: {:?}", sregs);
                    panic!("stop");
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

        eprintln!("setting regs to: {:?}", regs);
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
    fn kvm_verify_ins() {
        let mut vm = TestVm::create();

        // `ins byte [rdi], dl`
        let inst: &'static [u8] = &[0x6c];
        check_behavior(&mut vm, inst);
    }

    #[test]
    fn test_pf() {
        use yaxpeax_arch::{Decoder, U8Reader};
        use yaxpeax_x86::long_mode::InstDecoder;

        let mut vm = TestVm::create();

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

        let before_sregs = vm.vcpu.get_sregs().unwrap();
        let mut regs = vm.vcpu.get_regs().unwrap();

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

        vm.vcpu.set_regs(&regs).unwrap();

        vm.set_single_step(true);

        run(&mut vm);

        let after_regs = vm.vcpu.get_regs().unwrap();

        let bp_exit_addr: u64 = vm.interrupt_handlers_start().0 + Exception::BP.to_u8() as u64 + 1;
        assert_eq!(after_regs.rip, bp_exit_addr);
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
