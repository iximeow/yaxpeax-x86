use super::{Instruction, Opcode, Operand, OperandSpec};
use super::RegSpec;

/// an accessor for run-time characteristics of instructions.
///
/// ... TODO words ...
///
/// additionally, of note for x86:
///
/// * x86 has privilege levels, where some instructions raise an exception when executed in
///   inappropriate privilege levels. this is expressed by [`privilege_level()`] and
///   [`exceptions()`].
/// * x86 instructions have the familiar operands from textual disassembly, but many instructions
///   have other implied operands. in some cases the implied operand is a second memory-accessing
///   operation (consider `call qword [rcx]`; `qword [rcx]` is one memory access, but the implied
///   push of a return address is a second memory operation).
/// * `{,e,r}flags` is often written and sometimes read, but almost never as an explicit source or
///   destination operand. this can be queried with `flags_access()`, in addition to its inclusion
///   as an implicit operand.
///
/// it's also useful to know if implicit and explicit operands are reads, writes, or both, such as
/// when diagnosing a run-time fault. to iterate over this information, `operands().accesses()`. or
/// `visit_accesses(&mut ..)` to collect all operand/access information for this instruction.
#[derive(Copy, Clone)]
pub struct InstBehavior<'inst> {
    inst: &'inst Instruction,
    behavior: BehaviorDigest,
}

impl Instruction {
    pub fn behavior<'inst>(&'inst self) -> InstBehavior<'inst> {
        let behavior = if let Some(behavior) = opcode2behavior(&self.opcode) {
            behavior
        } else {
            // mul and imul are incredibly frustrating, with multiple behaviors corresponding to
            // different encodings with different opcode counts. fix up behaviors here..
            if self.opcode == Opcode::MUL || (self.opcode == Opcode::IMUL && self.operand_count == 1) {
                let op_width = if self.operands[0] == OperandSpec::RegMMM {
                    self.regs[1].width()
                } else {
                    self.mem_size
                };
                let ops_idx = match op_width {
                    1 => MUL_IDX_1OP_BYTE,
                    2 => MUL_IDX_1OP_WORD,
                    4 => MUL_IDX_1OP_DWORD,
                    _ /* 8 */ => MUL_IDX_1OP_QWORD,
                };
                BehaviorDigest::empty()
                    .set_pl_any()
                    .set_flags_access(Access::Write)
                    .set_operand(0, Access::Read)
                    .set_implicit_ops(ops_idx)
            } else if self.opcode == Opcode::IMUL {
                let base_digest = BehaviorDigest::empty()
                    .set_pl_any()
                    .set_flags_access(Access::Write);
                if self.operand_count == 2 {
                    base_digest
                        .set_operand(0, Access::ReadWrite)
                        .set_operand(1, Access::Read)
                } else {
                    base_digest
                        .set_operand(0, Access::Write)
                        .set_operand(1, Access::Read)
                        .set_operand(2, Access::Read)
                }
            } else if self.opcode == Opcode::DIV || self.opcode == Opcode::IDIV {
                let op_width = if self.operands[0] == OperandSpec::RegMMM {
                    self.regs[1].width()
                } else {
                    self.mem_size
                };
                let ops_idx = match op_width {
                    1 => DIV_IDX_1OP_BYTE,
                    2 => DIV_IDX_1OP_WORD,
                    4 => DIV_IDX_1OP_DWORD,
                    _ /* 8 */ => DIV_IDX_1OP_QWORD,
                };
                BehaviorDigest::empty()
                    .set_pl_any()
                    .set_flags_access(Access::Write)
                    .set_operand(0, Access::Read)
                    .set_implicit_ops(ops_idx)
            } else if self.opcode == Opcode::NOP {
                let mut digest = BehaviorDigest::empty()
                    .set_pl_any();
                if self.operand_count == 1 {
                    digest = digest
                        .set_operand(0, Access::None);
                }

                digest
            } else {
                // TODO: words
                unreachable!();
            }
        };
        InstBehavior {
            inst: self,
            behavior
        }
    }
}

/// a collection of possible exceptions an instruction can raise. this covers the handful of
/// well-defined exception vectors with bits matching to the exception vectors listed in SDM
/// chapter 6.5.1 "Call and Return Operation for Interrupt or Exception Handling Procedures"
/// specifically "Table 6-1. Exceptions and Interrupts".
pub struct ExceptionInfo {
    possible_vectors: u32,
}

/// an individual exception vector. these are just a tiny wrapper around `u8` to have some
/// associated constant definitions.
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Exception {
    vector: u8,
}

impl Exception {
    /// Divide Error
    pub const DE: Exception = Exception::vector(0);
    /// Debug
    pub const DB: Exception = Exception::vector(1);
    /// Non-Maskable Interrupt
    pub const NMI: Exception = Exception::vector(2);
    /// Breakpoint
    pub const BP: Exception = Exception::vector(3);
    /// Overflow
    pub const OF: Exception = Exception::vector(4);
    /// BOUND Range Exceeded
    pub const BR: Exception = Exception::vector(5);
    /// Invalid Opcode (Undefined Opcode)
    pub const UD: Exception = Exception::vector(6);
    /// Device Not Available (No Math Coprocessor)
    pub const NM: Exception = Exception::vector(7);
    /// Double Fault
    pub const DF: Exception = Exception::vector(8);
    // CoProcessor Segment Overrun (reserved)
    // from the SDM:
    // > IA-32 processors after the Intel386 processor do not generate this exception.
    //
    // and as the mnemonic has since been reused for exception vector 16,
    // `Floating-Point Error (Math Fault)`, we won't bother giving vector 9 a nice symbolic name.
    // const MF: Exception = Exception::vector(9);
    /// Invalid TSS
    pub const TS: Exception = Exception::vector(10);
    /// Segment Not Present
    pub const NP: Exception = Exception::vector(11);
    /// Stack Segment Fault
    pub const SS: Exception = Exception::vector(12);
    /// General Protection
    pub const GP: Exception = Exception::vector(13);
    /// Page Fault
    pub const PF: Exception = Exception::vector(14);
    // vector 15 is reserved
    /// Floating-Point Error (Math Fault)
    pub const MF: Exception = Exception::vector(16);
    /// Alignment Check
    pub const AC: Exception = Exception::vector(17);
    /// Machine Check
    pub const MC: Exception = Exception::vector(18);
    /// SIMD Floating-Point Exception
    pub const XM: Exception = Exception::vector(19);
    /// Virtualization Exception
    pub const VE: Exception = Exception::vector(20);
    /// Control Protection Exception
    pub const CP: Exception = Exception::vector(21);

    pub const fn vector(vector: u8) -> Self {
        Self { vector }
    }

    /// convert this `Exception` to an index into an x86 IDT.
    pub const fn to_u8(&self) -> u8 {
        self.vector
    }

    #[cfg(feature = "fmt")]
    pub fn name(&self) -> Option<&'static str> {
        static NAMES: [Option<&'static str>; 22] = [
            Some("DE"), Some("DB"), Some("NMI"), Some("BP"),
            Some("OF"), Some("BR"), Some("UD"), Some("NM"),
            Some("DF"), None, Some("TS"), Some("NP"),
            Some("SS"), Some("GP"), Some("PF"), None,
            Some("MF"), Some("AC"), Some("MC"), Some("XM"),
            Some("VE"), Some("CP")
        ];

        if let Some(maybe_name) = NAMES.get(self.vector as usize) {
            *maybe_name
        } else {
            None
        }
    }
}

#[cfg(feature = "fmt")]
use core::fmt;
#[cfg(feature = "fmt")]
impl fmt::Debug for Exception {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = self.name() {
            write!(f, "#{}", name)
        } else {
            write!(f, "#Int{}", self.to_u8())
        }
    }
}

impl ExceptionInfo {
    pub fn empty() -> Self {
        Self {
            possible_vectors: 0,
        }
    }

    pub fn any(&self) -> bool {
        self.possible_vectors != 0
    }

    pub fn none(&self) -> bool {
        !self.any()
    }

    pub fn may(&self, e: Exception) -> bool {
        (self.possible_vectors & (1 << e.vector)) != 0
    }

    /// record that exception `e` is or is not (`b`) possible in this `ExceptionInfo` record.
    pub const fn set(&mut self, e: Exception, b: bool) {
        let offset = e.vector;
        assert!(offset < 32);
        let mask = !(1 << offset);
        let bit = (b as u32) << offset;

        self.possible_vectors &= mask;
        self.possible_vectors |= bit;
    }

    pub const fn with(mut self, e: Exception, b: bool) -> Self {
        self.set(e, b);
        self
    }
}

#[test]
fn test_exception_info() {
    let mut info = ExceptionInfo::empty();
    info.set(Exception::MF, true);
    assert_eq!(info.possible_vectors, 0x10000);

    info.set(Exception::MF, true);
    assert_eq!(info.possible_vectors, 0x10000);

    info.set(Exception::MF, false);
    assert_eq!(info.possible_vectors, 0x00000);

    info.set(Exception::GP, false);
    assert_eq!(info.possible_vectors, 0x00000);

    info.set(Exception::GP, true);
    assert_eq!(info.possible_vectors, 0x02000);

    info.set(Exception::MF, true);
    assert_eq!(info.possible_vectors, 0x12000);
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrivilegeLevel {
    Any = 0b00,
    PL0 = 0b01,
    Special = 0b10,
}

#[derive(Copy, Clone)]
pub struct InstOperands<'inst> {
    inst: InstBehavior<'inst>,
    implicit_ops: &'static [(Operand, Access)],
}

impl<'inst> InstOperands<'inst> {
    pub fn iter(self) -> AccessIter<'inst> {
        AccessIter::new(self)
    }
}

pub struct AccessIter<'inst> {
    operands: InstOperands<'inst>,
    explicit: bool,
    next: u8,
}

impl<'inst> AccessIter<'inst> {
    fn new(operands: InstOperands<'inst>) -> Self {
        Self {
            operands,
            explicit: false,
            next: 0,
        }
    }

    pub fn operands(self) -> OperandIter<'inst> {
        OperandIter { inner: self }
    }
}

impl<'inst> Iterator for AccessIter<'inst> {
    type Item = (Operand, Access);

    fn next(&mut self) -> Option<Self::Item> {
        // iteration order is:
        // * if the instruction accesses flags, report that
        // * then, walk the implicit operand list
        // * finally, walk explicit operands
        // * fin

        // the implicit operand list might be empty, there may be no flags, etc. so we need to
        // consider the implicit operand iterator states up to two times before falling through to
        // the first is this the first reported case of buttelement of the explicit operand list (if there is one). using a loop here
        // seems like the least-gross way to go...
        while !self.explicit {
            if self.next == 0 {
                // we only consider flags at most once; either we're returning a value here or
                // we're going to keep searching through the loop.
                self.next += 1;
                if let Some(acc) = self.operands.inst.flags_access() {
                    return Some((Operand::Register { reg: RegSpec::rflags() }, acc));
                }
            } else {
                let implicit_idx = self.next - 1;
                self.next += 1;

                if let Some(entry) = self.operands.implicit_ops.get(implicit_idx as usize) {
                    return Some(entry.clone());
                } else {
                    // we've gotten to the end of implicit operands. flip to explicit operands,
                    // reset `next`, and continue searching.
                    self.explicit = true;
                    self.next = 0;
                }
            }
        }


        if self.next < self.operands.inst.inst.operand_count() {
            let op = self.operands.inst.inst.operand(self.next);
            let access = self.operands.inst.operand_access(self.next).expect("defined operand has defined access");
            debug_assert!(access != Access::None || self.operands.inst.inst.opcode == Opcode::NOP);
            let res = Some((op, access));
            self.next += 1;
            res
        } else {
            None
        }
    }
}

pub struct OperandIter<'inst> {
    inner: AccessIter<'inst>,
}

/// enough structure to describe any implicitly-present operand in an x86_64 instruction.
///
/// this is (maybe surprisingly, compared to the rest of the isa) relatively tiny: the only
/// implicit operands to date are register reads/writes, and simple dereference of a register (such
/// as `[rsp - 8] = ...` in a push).
// TODO: this needs accessors for the elements or something.
pub struct ImplicitOperand {
    // TODO: not suitable for public API!
    spec: OperandSpec,
    reg: RegSpec,
    disp: i32,
    write: bool,
}

impl<'inst> Iterator for OperandIter<'inst> {
    type Item = Operand;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(op, _acc)| op)
    }
}

impl<'inst> InstBehavior<'inst> {
    pub fn privilege_level(&self) -> Option<PrivilegeLevel> {
        let pl_bits = self.behavior.behavior & 0b11;
        const LUT: [Option<PrivilegeLevel>; 4] = [
            Some(PrivilegeLevel::Any), Some(PrivilegeLevel::PL0),
            Some(PrivilegeLevel::Special), None,
        ];

        LUT[pl_bits as usize]
    }

    pub fn exceptions(&self) -> ExceptionInfo {
        let mut exceptions = ExceptionInfo::empty();
        if self.privilege_level() != Some(PrivilegeLevel::Any) {
            // TODO: is it correct that executing with incorrect CPL always yields #GP?
            exceptions.set(Exception::GP, true);
        }

        match self.all_operands() {
            Ok(op_info) => {
                exceptions.set(Exception::PF, op_info.iter().operands().any(|x| x.is_memory()));
            }
            Err(_complex) => {
                // TODO: is it correct that all complex ops can page fault? no: wrmsr/rdmsr do not
                // #PF.
                exceptions.set(Exception::PF, true);
            }
        }

        exceptions
    }

    /// produce an `InstOperands` describing the explicit and implicit operands of this
    /// instruction.
    ///
    /// "explicit" operands are ones that are displayed as part of the instruction's textual
    /// assembly/disassembly, while "implicit" operands are operands describing the reset of the
    /// instruction's behavior.
    ///
    /// this notion of "implicit operands" does not map precisely to terminology from either the
    /// Intel SDM or AMD APM. instead, it is provided by `yaxpeax-x86` to try providing an answer
    /// to some common queries about instructions .
    pub fn all_operands(&self) -> Result<InstOperands<'inst>, ComplexOp> {
        Ok(InstOperands {
            inst: *self,
            // TODO: actually select an implicit operands array based on... something from the
            // instruction behavior, maybe?
            implicit_ops: &[],
        })
    }

    pub fn flags_access(&self) -> Option<Access> {
        let flag_acc = (self.behavior.behavior >> 2) & 0b11;
        Access::from_bits(flag_acc)
    }

    pub fn implicit_oplist(&self) -> Option<&'static [ImplicitOperand]> {
        let ops_idx = self.behavior.extra;
        if ops_idx == 0 {
            return None;
        }

        // TODO: ops_idx cannot be out of bounds, so maybe kinda-unchecked here..?
        Some(&IMPLICIT_OPS_LIST[ops_idx as usize])
    }

    pub fn operand_access(&self, idx: u8) -> Option<Access> {
        if idx >= 4 {
            return None;
        }

        let op_acc = (self.behavior.operand_access >> (2 * idx)) & 0b11;
        Access::from_bits(op_acc)
    }

    pub fn visit_accesses<T: AccessVisitor>(&self, v: &mut T) -> Result<(), ComplexOp> {
        if self.inst.opcode == Opcode::WRMSR {
            return Err(ComplexOp::WRMSR);
        } else if self.inst.opcode == Opcode::PREFETCHNTA {
            return Err(ComplexOp::PREFETCHNTA);
        } else if self.inst.opcode == Opcode::PREFETCH2 {
            return Err(ComplexOp::PREFETCHT2);
        } else if self.inst.opcode == Opcode::PREFETCH1 {
            return Err(ComplexOp::PREFETCHT1);
        } else if self.inst.opcode == Opcode::PREFETCH0 {
            return Err(ComplexOp::PREFETCHT0);
        } else if self.inst.opcode == Opcode::BT && self.inst.operands[0] != OperandSpec::RegMMM {
            return Err(ComplexOp::BT);
        } else if self.inst.opcode == Opcode::BTS && self.inst.operands[0] != OperandSpec::RegMMM {
            return Err(ComplexOp::BTS);
        } else if self.inst.opcode == Opcode::BTC && self.inst.operands[0] != OperandSpec::RegMMM {
            return Err(ComplexOp::BTC);
        }

        fn compute_addr<T: AccessVisitor>(v: &mut T, inst: &Instruction, op_spec: OperandSpec) -> Option<u64> {
            // TODO: test assertions feature?
            if !op_spec.is_memory() {
                panic!("expected memory operand but got {:?}", op_spec);
            }

            match op_spec {
                OperandSpec::Deref => {
                    v.get_register(inst.regs[1])
                }
                OperandSpec::Deref_rdi => {
                    v.get_register(RegSpec::rdi())
                }
                OperandSpec::Deref_rsi => {
                    v.get_register(RegSpec::rsi())
                }
                OperandSpec::Deref_edi => {
                    v.get_register(RegSpec::edi())
                }
                OperandSpec::Deref_esi => {
                    v.get_register(RegSpec::esi())
                }
                other => {
                    panic!("not-yet-handled memory operand: {:?}", other);
                }
            }
        }

        if let Some(implicit_oplist) = self.implicit_oplist() {
            for op in implicit_oplist.iter() {
                if op.spec == OperandSpec::RegRRR {
                    if op.write {
                        v.register_write(op.reg);
                    } else {
                        v.register_read(op.reg);
                    }
                } else if op.spec == OperandSpec::Deref_rdi {
                    // Deref_rdi is used for string instructions; operand-size overrides apply here
                    // and so the register that is incremented (or decremented!) depends on the
                    // operand-size prefix. the register is correct for the operands, so we'll
                    let reg = match self.inst.operands[op.disp as usize] {
                        OperandSpec::Deref_rdi => RegSpec::rdi(),
                        OperandSpec::Deref_rsi => RegSpec::rsi(),
                        OperandSpec::Deref_edi => RegSpec::edi(),
                        OperandSpec::Deref_esi => RegSpec::esi(),
                        OperandSpec::Deref => self.inst.regs[1],
                        other => { panic!("TODO: unreachable {:?}", other); }
                    };
                    if op.write {
                        v.register_write(reg);
                    } else {
                        v.register_read(reg);
                    }
                } else {
                    let addr = match op.spec {
                        OperandSpec::Deref => {
                            v.get_register(op.reg)
                        },
                        OperandSpec::Disp => {
                            if let Some(base) = v.get_register(op.reg) {
                                Some(base.wrapping_add(op.disp as i64 as u64))
                            } else {
                                None
                            }
                        }
                        OperandSpec::MemIndexScale => {
                            // HACK HACK HACK
                            let base = v.get_register(op.reg);
                            let index = v.get_register(RegSpec::al());
                            if let (Some(base), Some(index)) = (base, index) {
                                Some(base.wrapping_add(index as u64))
                            } else {
                                None
                            }
                        }
                        other => {
                            panic!("impossible operand spec {:?}", other);
                        }
                    };

                    let size = self.inst.mem_size().expect("memory operand implies memory access size")
                        .bytes_size().expect("non-complex instructions have well-defined bytes_size()");

                    if op.write {
                        v.memory_write(addr, size as u32);
                    } else {
                        v.memory_read(addr, size as u32);
                    }
                }
            }
        }

        if let Some(acc) = self.flags_access() {
            if acc.is_read() {
                v.register_read(RegSpec::rflags());
            }
            if acc.is_write() {
                v.register_write(RegSpec::rflags());
            }
        }

        let operand_access = self.behavior.operand_access;

        for i in 0..self.inst.operand_count {
            let access = Access::from_bits((operand_access >> (i * 2)) & 0b11)
                .expect("selected only low two bits");
            let op_spec = self.inst.operands[i as usize];

            if access.is_read() {
                match op_spec {
                    OperandSpec::RegRRR => {
                        v.register_read(self.inst.regs[0]);
                    }
                    OperandSpec::RegMMM => {
                        v.register_read(self.inst.regs[1]);
                    }
                    OperandSpec::RegVex => {
                        v.register_read(self.inst.regs[3]);
                    }
                    OperandSpec::ImmI8 |
                    OperandSpec::ImmU8 |
                    OperandSpec::ImmI16 |
                    OperandSpec::ImmU16 |
                    OperandSpec::ImmI32 |
                    OperandSpec::ImmI64 |
                    OperandSpec::ImmInDispField => {
                        // no register/memory access to report.
                    }
                    _other => {
                        // compute effective address...
                        let addr = compute_addr(v, &self.inst, op_spec);
                        let size = self.inst.mem_size().expect("memory operand implies memory access size")
                            .bytes_size().expect("non-complex instructions have well-defined bytes_size()");
                        // `lea` *just* computes the effective address, which we've done above.
                        // othrwise, the instruction will actually read this memory operand.
                        if self.inst.opcode != Opcode::LEA {
                            v.memory_read(addr, size as u32);
                        }
                    }
                }
            }

            if access.is_write() {
                // given a register `reg` that an instruction writes, expand it for the purposes of
                // tracking register writes. x86 zero-extends writes to 32-bit GPRs into 64-bit GPR
                // writes, so replicate that here.
                fn apply_x86_zext(mut reg: RegSpec) -> RegSpec {
                    use super::RegisterBank;
                    if reg.bank == RegisterBank::D {
                        reg.bank = RegisterBank::Q;
                    }
                    reg
                }
                match op_spec {
                    OperandSpec::RegRRR => {
                        v.register_write(apply_x86_zext(self.inst.regs[0]));
                    }
                    OperandSpec::RegMMM => {
                        v.register_write(apply_x86_zext(self.inst.regs[1]));
                    }
                    OperandSpec::RegVex => {
                        v.register_write(apply_x86_zext(self.inst.regs[3]));
                    }
                    OperandSpec::ImmI8 |
                    OperandSpec::ImmU8 |
                    OperandSpec::ImmI16 |
                    OperandSpec::ImmU16 |
                    OperandSpec::ImmI32 |
                    OperandSpec::ImmI64 |
                    OperandSpec::ImmInDispField => {
                        // no register/memory access to report.
                    }
                    _other => {
                        // compute effective address...
                        let addr = compute_addr(v, &self.inst, op_spec);
                        let size = self.inst.mem_size().expect("memory operand implies memory access size")
                            .bytes_size().expect("non-complex instructions have well-defined bytes_size()");
                        // no lea check necessary: the memory access is coded as a read and no
                        // instruction has a similar "fake" memory write.
                        v.memory_write(addr, size as u32);
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Access {
    /// the corresponding operand is read.
    ///
    /// for memory operands, this describes the referenced memory; implicitly the registers used in
    /// the operand's address calculation are also read.
    Read = 0b01,
    /// the corresponding operand is written.
    ///
    /// for memory operands, this describes the referenced memory; implicitly the registers used in
    /// the operand's address calculation are also read.
    Write = 0b10,
    /// the corresponding operand is read and written.
    ///
    /// for memory operands, this describes the referenced memory; implicitly the registers used in
    /// the operand's address calculation are also read.
    ReadWrite = 0b11,
    /// the corresponding operand is not actually accessed for reading or writing.
    ///
    /// this is only used to describe the operand of `nop` instructions.
    None = 0b00,
}

impl Access {
    // translate two bits to an `Access`. panics if the bit pattern has anything other than the low
    // two bits set. don't do that
    fn from_bits(bits: u8) -> Option<Access> {
        const LUT: [Option<Access>; 4] = [
            Some(Access::None), Some(Access::Read),
            Some(Access::Write), Some(Access::ReadWrite),
        ];

        assert!(bits <= 0b11);

        LUT[bits as usize]
    }

    fn is_read(&self) -> bool {
        *self as u8 & 0b01 != 0
    }

    fn is_write(&self) -> bool {
        *self as u8 & 0b10 != 0
    }
}

#[derive(Copy, Clone)]
pub struct BehaviorDigest {
    // laid out like:
    //
    // |7 6|5 4|3 2|1 0|
    // |imp_ops|FL |PL |
    //
    // imp_ops: selector for a `&'static [Operand]` of additional "implicit" operands for the
    //   instruction.
    // FL: access bits for {,e,r}flags
    // PL: privilege level this instruction can be executed.
    //   00 -> all levels
    //   01 -> CPL=0 only
    //   10 -> something more complicated (instruction-specific)
    //   11 -> reserved
    behavior: u8,
    // for the up-to four explicit operands in an x86 instruction.
    //
    // bits are pairs interpreted as described in `enum Access`. operand count on the instruction
    // describes validity of these bits: fields left `00` must not have a corresponding operand at
    // that offset. fields with no corresponding operand may have bits set.
    operand_access: u8,
    extra: u16,
}

// TODO: the various `set_pl*()` are not actually used yet..
#[allow(dead_code)]
impl BehaviorDigest {
    const fn empty() -> BehaviorDigest {
        BehaviorDigest {
            behavior: 0,
            operand_access: 0,
            extra: 0
        }
    }

    const fn set_pl0(mut self) -> Self {
        self.behavior &= 0b11_11_11_00;
        self.behavior |= 0b00_00_00_01;
        self
    }

    const fn set_pl_any(mut self) -> Self {
        self.behavior &= 0b11_11_11_00;
        self.behavior |= 0b00_00_00_00;
        self
    }

    const fn set_pl_special(mut self) -> Self {
        self.behavior &= 0b11_11_11_00;
        self.behavior |= 0b00_00_00_10;
        self
    }

    const fn set_flags_access(mut self, access: Access) -> Self {
        self.behavior &= 0b11_11_00_11;
        self.behavior |= (access as u8) << 2;
        self
    }

    const fn set_operand(mut self, idx: u8, access: Access) -> Self {
        assert!(idx < 4);
        let offset = idx * 2;
        self.operand_access &= !(0b11 << offset);
        self.operand_access |= (access as u8) << offset;
        self
    }

    const fn set_implicit_ops(mut self, ops_idx: u16) -> Self {
        // TODO: this needs much less than a full u16 (much less than |Opcode| even)
        self.extra = ops_idx;
        self
    }

    const fn set_complex(mut self, state: bool) -> Self {
        self.behavior &= 0b11_10_11_11;
        self.behavior |= (state as u8) << 4;
        self
    }
}

/// a subset of [`Opcode`] where access patterns cannot be expressed as a simple stream of reads or
/// writes. in many cases these instructions are only executable when `CPL=0` but notable
/// exceptions are `rep`-prefixed string instructions (`rep movs`, `rep stos`, etc).
///
/// complex instructions and appropriate handling are documented on a best-effort basis below.
///
/// ### the `xsave` family
///
/// this section applies for all of `xsave`, `xsaveopt`, `xsavec`, `xsavec64`, `xsaves`,
/// `xsaves64`, `xrstor`, `xrstors`, `xrstors64`.
///
/// these instructions are considered "complex" because the actual amount of data read or written
/// depends on dynamic processor state, specifically, bits in `xcr0`. further, the upper bound of
/// data read or written by these instructions is *also* processor-dependent - each architecture
/// extension that adds processor state tends to have a corresponding bit opting it in or out of
/// xsave state with these instructions.
///
/// gaps between enabled feature bits are possible and it would be legal for processors to change
/// xsave layout based on enabled feature bits as well. no processor *does* this, but i'm not
/// assuming this layout is fixed only to leave you holding the bag!
///
/// see the Intel SDM chapter `13.1 XSAVE-Supported Features And State-Component Bitmaps` for more
/// details.
///
/// ### rep-prefixed string instructions
///
/// this section applies for all of `rep movs`, `rep stos`, `rep lods`, `rep scas`, and `rep cmps`.
///
/// these instructions are considered "complex" for two reasons. first, while these are single
/// instructions, they do not execute atomically. then even if they were executed in a single
/// architectural state update, the size of the memory access is a function of `rcx`.
///
/// worse, different processors can execute these instructions somewhat differently. from the Intel
/// SDM:
///
/// > 7.3.9.3 Fast-String Operation
/// >
/// > [...] Instructions using fast-string operation effectively operate on the string in groups
/// > that may include multiple elements of the native data size (byte, word, doubleword, or
/// > quadword). With fast-string operation, the processor recognizes interrupts and data
/// > breakpoints only on boundaries between these groups. [...]
///
/// the number of multiple elements is not defined, nor is the unit size of a fast string operation
/// group, and that interrupts and breakpoints are only recognized on boundaries between these
/// groups of implementation-defined size. even considering `rep`-prefixed string instructions as a
/// series of the instruction data size is incorrect if the instruction's initial conditions are
/// eligible for `Fast-String Operation` acceleration.
///
/// correctly interpreting the access pattern of these instructions depends heavily on the
/// application needing this information.
///
/// ### AVX512 scatter/gather instructions
///
/// this section applies for all of `vpscatter{dd,dq,qd,qq}` and `vpgather{dd,dq,qd,qq}`.
///
/// these instructions are considered "complex" because their memory access characteristics are
/// actually to many memory addresses using the lanes of the vector register used as an index in
/// the memory operand. consider `vpscatterdd [r15 + ymm25], k6, ymm10`; this instruction has
/// accesses four memory locations, one for each dword lane in `ymm25` as an offset to `r15`.
///
/// `yaxpeax-x86` does not have a way to name individual lanes of vector registers and may not ever
/// add one if this is the only use. therefore there is no way to express these memory accesses and
/// the instructions are considered complex.
///
/// ### `monitor`, `monitorx`
///
/// these instructions reference memory but neither read nor write it. instead, `monitor` primes
/// hardware to watch for accesses to the specified address, while `mwait` waits for an access to
/// some earlier `monitor`-primed adddress.
///
/// arguably `monitor` could be described as a load; it sets the A-bit in page tables, is ordered
/// as a load, and is subject to the permission checking associated with a byte load. but is it
/// *actually* doing a load? it might just be translating the linear address to a physical address
/// for monitoring, which *only* requires the page table walk.
///
/// ### `syscall/sysret`, `sysenter/sysexit`
///
/// these instructions are considered "complex" because they include implicit reads and possibly
/// writes to various MSRs. further, depending on dynamic processor state (i.e. "is FRED enabled")
/// these instructions may behave quite differently than a "normal" shuffling of
/// `rip`/`rflags`/`cs`.
///
/// ### `vmread`, `vmwrite`, `vmrun`, `vmsave`, `vmload`
///
/// these instructions are considered "complex" because their actual operand use differs
/// substantially from their encoding.
///
/// for `vmread` and `vmwrite`, the memory operand may be `[rax]`, but it is implicitly an access
/// to the current VMCS - and, indeed, not even an access to "memory".
///
/// for `vmrun`, `vmsave`, and `vmload`, the operand is "`rax`", but expects `rax` to carry a
/// physical address to a VMCB which is then loaded from or stored into.
///
/// ### `vzeroupper`, `vzeroall`
///
/// these instructions are considered "complex" because their actual effect varies by processor
/// implementation. when AVX512 is supported, these operate on the `zmm*` registers, otherwise they
/// operate on `ymm*`.
///
/// ### `prefetchnta`, `prefetcht2`, `prefetcht1`, `prefetcht0`
///
/// these instructions are considered "complex" because they are hints, but have effects on
/// microarchitectural state. the memory operand is documented as a 1-byte access, reported as a
/// 32-byte access, but practically is implementation-defined and "a minimum of 32 bytes". the
/// memory operand needs not be a valid address, either, and if it is not a mapped address then the
/// `prefetch*` instructions do not raise a #PF.
///
/// in architectural terms, these instructions could have an operand access form of `Access::None`,
/// but due to the microarchitectural effects this would be misleading. so, these are "complex" and
/// should be handled by user code as a no-op, or read, or access hint, etc.
///
/// ### `bts`, `btc`, `bt`
///
/// these instructions are considered "complex" when the destination is a memory operand because
/// the effective address of the modified word/dword/qword is a function of both operands of the
/// instruction.
///
/// in particular, the accessed location is the word/dword/qword at the first operand's effective
/// address *plus* the second operand divided by the access size. as a worked example with a dword
/// access:
/// ```text
/// rax := 0x1_0000_0100
/// rcx := 0x203
///
/// // bts dword [rax], ecx
/// ptr = rax + (rcx / 32) => 0x1_0000_0303
/// bit = rcx % 32         => 3
/// cf := (*ptr >> bit) & 1
/// *ptr |= (1 << bit)
/// ```
#[non_exhaustive]
#[derive(Copy, Clone, Debug)]
pub enum ComplexOp {
    /// rdmsr/wrmsr are considered "complex" for reasons described in the enum doc comment.
    RDMSR,
    WRMSR,

    /// string instructions are considered "complex" for reasons described in the enum doc comment.
    MOVS,
    STOS,
    LODS,
    SCAS,
    CMPS,

    /// prefetch instructions are considered "complex" for reasons described in the enum doc
    /// comment.
    PREFETCHNTA,
    PREFETCHT2,
    PREFETCHT1,
    PREFETCHT0,

    /// bit test/set/clear instructions are conditionally comple depending on their destination
    /// operand form, as described in the enum doc comment..
    BT,
    BTC,
    BTS,
}

/// a visitor for collecting architectural accesses for an `Instruction`. used with
/// [`InstBehavior::visit_accesses`].
///
/// ## address calculation
///
/// [`memory_read()`] and [`memory_write()`] take an optional parameter for an effective address
/// that is either read or written. by default, the address provided is typically `None`, but with
/// appropriate implementations of this trait, `yaxpeax-x86` will calculate and report the
/// effective addresses of memory acceses. when visiting a memory operand, the library will call
/// [`get_register()`] on each register used in an operand's address calculation; if all calls
/// return a value, then the library will compute an address and provide it in the corresponding
/// `memory_read()` or `memory_write()`.
///
/// the default `get_register()` implementation does not return register values, but does call
/// `register_read()`. this means that `register_read()` is called for each register that may be
/// used by the instruction in question. if this is desirable and you are providing a custom
/// implementation of `get_register()`, be sure to include a `register_read()`! alternatively, if
/// `get_register()` is made to not call `register_read()`, then the other functions in this trait
/// (`{register,memory}_{read,write}`()) are called one-to-one for implicit or explicit operands of
/// this instruction.
pub trait AccessVisitor {
    /// record that the instruction reads a register. note that the default implementation of
    /// [`AccessVisitor::get_register`] also calls `register_read`; registers used as part of an
    /// address calculation for memory accesses are recorded via `register_read()` by default!
    fn register_read(&mut self, reg: RegSpec);
    /// record that the instruction writes a register.
    fn register_write(&mut self, reg: RegSpec);
    /// get a numeric value for `reg`, if possible. this is called as part of computing effective
    /// addresses used in [`AccessVisitor::memory_read`] and [`AccessVisitor::memory_write`], for
    /// each register involved in an address calculation.
    ///
    /// if any `get_register()` returns `None` in an address calculation, the subsequent
    /// `memory_read()` or `memory_write()` for that operand will be given an `address` of `None`.
    ///
    /// if `get_register` is implemented withhout calling `register_read`, the 
    /// if `get_register()` is given a custom implementation, be sure to either call `
    fn get_register(&mut self, reg: RegSpec) -> Option<u64> {
        self.register_read(reg);
        None
    }
    fn memory_read(&mut self, address: Option<u64>, size: u32);
    fn memory_write(&mut self, address: Option<u64>, size: u32);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::long_mode::InstDecoder;

    use alloc::vec;
    use alloc::vec::Vec;

    #[test]
    fn access_visitor_works() {
        // xor eax, dword [rcx]
        let bytes = &[0x33, 0x01];
        let inst = InstDecoder::default().decode_slice(bytes).expect("can decode trivial instructions");

        struct AccessCtx {
            rcx: u64,

            accesses: Vec<(RegSpec, Access)>,
            mem_accesses: Vec<((Option<u64>, u32), Access)>,
        }

        let mut ctx = AccessCtx {
            rcx: 0x10000,
            accesses: Vec::new(),
            mem_accesses: Vec::new(),
        };

        impl AccessVisitor for AccessCtx {
            fn register_read(&mut self, reg: RegSpec) {
                self.accesses.push((reg, Access::Read));
            }

            fn register_write(&mut self, reg: RegSpec) {
                self.accesses.push((reg, Access::Write));
            }

            fn get_register(&mut self, reg: RegSpec) -> Option<u64> {
                self.register_read(reg);

                if reg == RegSpec::rcx() {
                    Some(self.rcx)
                } else {
                    None
                }
            }

            fn memory_read(&mut self, address: Option<u64>, size: u32) {
                self.mem_accesses.push(((address, size), Access::Read));
            }

            fn memory_write(&mut self, address: Option<u64>, size: u32) {
                self.mem_accesses.push(((address, size), Access::Write));
            }
        }

        let behavior = inst.behavior();
        behavior.visit_accesses(&mut ctx).expect("xor eax, [rcx] is not complex");

        assert_eq!(ctx.accesses, vec![
               (RegSpec::rflags(), Access::Write),
               (RegSpec::eax(), Access::Read),
               (RegSpec::rax(), Access::Write),
               (RegSpec::rcx(), Access::Read)
        ]);
        assert_eq!(ctx.mem_accesses, vec![((Some(0x10000), 4), Access::Read)]);
    }

    #[test]
    fn operand_iter_basically_works() {
        // xor eax, eax
        let bytes = &[0x33, 0xc0];
        let inst = InstDecoder::default().decode_slice(bytes).expect("can decode trivial instructions");

        // uwu whats this...
        let behavior = inst.behavior();

        // owo hewwo there
        let operands = behavior.all_operands().expect("xor eax, eax is not complex");

        // OwO waowwww
        let collected: alloc::vec::Vec<(Operand, Access)> = operands.iter().collect();
        let expected = alloc::vec![
            (Operand::Register { reg: RegSpec::rflags() }, Access::Write),
            (Operand::Register { reg: RegSpec::eax() }, Access::ReadWrite),
            (Operand::Register { reg: RegSpec::eax() }, Access::Read),
        ];
        assert_eq!(collected, expected);

        assert_eq!(behavior.privilege_level(), Some(PrivilegeLevel::Any));
        let exceptions = behavior.exceptions();
        assert!(exceptions.none());

        // but if an operand does a memory access, that can fault:
        // xor eax, [rax]
        let bytes = &[0x33, 0x00];
        let inst = InstDecoder::default().decode_slice(bytes).expect("can decode trivial instructions");
        let behavior = inst.behavior();
        let operands = behavior.all_operands().expect("xor eax, eax is not complex");

        let collected: alloc::vec::Vec<(Operand, Access)> = operands.iter().collect();
        let expected = alloc::vec![
            (Operand::Register { reg: RegSpec::rflags() }, Access::Write),
            (Operand::Register { reg: RegSpec::eax() }, Access::ReadWrite),
            (Operand::MemDeref { base: RegSpec::rax() }, Access::Read),
        ];
        assert_eq!(collected, expected);

        assert_eq!(behavior.privilege_level(), Some(PrivilegeLevel::Any));
        let exceptions = behavior.exceptions();
        assert!(!exceptions.none());
        assert!(exceptions.may(Exception::PF));
    }
}

/// no operations, but you can run it anywhere.
const GENERAL: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any();

/// instructions that can execute at all privilege levels, have two operands, read/write the first,
/// and read the second.
const GENERAL_RW_R: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_operand(0, Access::ReadWrite)
    .set_operand(1, Access::Read);

/// same as above, but writes flags. this is most arithmetic instructions.
const GENERAL_RW_R_FLAGWRITE: BehaviorDigest = GENERAL_RW_R
    .set_flags_access(Access::Write);

/// popcnt and maybe others?
const GENERAL_W_R_FLAGWRITE: BehaviorDigest = GENERAL_RW_R
    .set_operand(0, Access::Write)
    .set_flags_access(Access::Write);

/// test, cmp, with no write but to flags.
const GENERAL_R_R_FLAGWRITE: BehaviorDigest = GENERAL_RW_R
    .set_operand(0, Access::Read)
    .set_flags_access(Access::Write);

/// `sbb`, `adc`, etc both read flags and write them.
const GENERAL_RW_R_FLAGRW: BehaviorDigest = GENERAL_RW_R
    .set_flags_access(Access::ReadWrite);

/// `xadd` reads everything and writes everything, even flags!
const GENERAL_RW_RW_FLAGRW: BehaviorDigest = GENERAL_RW_R_FLAGRW
    .set_operand(1, Access::ReadWrite);

/// setcc and friends read flags to maybe write their operand.
const GENERAL_RW_FLAGREAD: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_operand(0, Access::ReadWrite)
    .set_flags_access(Access::Read);

/// cmov reads from a second operand and (may) writes to the first.
const GENERAL_W_R_FLAGREAD: BehaviorDigest = GENERAL_RW_FLAGREAD
    .set_operand(0, Access::Write)
    .set_operand(1, Access::Read);

/// cmc, clc, sti, cli, etc that toggle individual bits in flags
const GENERAL_FLAGRW: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_flags_access(Access::ReadWrite);

/// `inc`, `dec`, and `neg` have one operand and modify flags.
const GENERAL_RW_FLAGWRITE: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_operand(0, Access::ReadWrite)
    .set_flags_access(Access::Write);

/// `inc`, `dec`, and `neg` have one operand and modify flags.
const GENERAL_RW: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_operand(0, Access::ReadWrite);

const GENERAL_R_R: BehaviorDigest = GENERAL_RW_R
    .set_operand(0, Access::Read);

/// `ins` writes to the memory operand, reads from `rdx` (second operand)
const GENERAL_W_R: BehaviorDigest = GENERAL_RW_R
    .set_operand(0, Access::Write);

/// many vex/evex-encoded instructions
const GENERAL_W_R_R: BehaviorDigest = GENERAL_W_R
    .set_operand(2, Access::Write);

const GENERAL_RW_RW: BehaviorDigest = GENERAL_RW_R
    .set_operand(1, Access::ReadWrite);

const GENERAL_RW_RW_FLAGWRITE: BehaviorDigest = GENERAL_RW_RW
    .set_flags_access(Access::Write);

const GENERAL_R: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_operand(0, Access::Read);

const GENERAL_W: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_operand(0, Access::Write);

const GENERAL_R_FLAGREAD: BehaviorDigest = GENERAL_R
    .set_flags_access(Access::Read);

// TODO: seems incredibly funky that jcc's operand is an immediate, when written like this..
const JCC: BehaviorDigest = BehaviorDigest::empty()
    .set_implicit_ops(JCC_OPS_IDX)
    .set_pl_any()
    .set_operand(0, Access::Read);

const CMOVCC: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_flags_access(Access::Read)
    .set_operand(0, Access::Write)
    .set_operand(1, Access::Read);

const SETCC: BehaviorDigest = BehaviorDigest::empty()
    .set_pl_any()
    .set_flags_access(Access::Read)
    .set_operand(0, Access::Write);

static PUSH_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Disp,
        reg: RegSpec::rsp(),
        disp: -8i32,
        write: true,
    },
    // push.. pushes the value (above), then does a RMW on rsp.
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

static POP_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rsp(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

static JCC_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rflags(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: true,
    }
];

static CBW_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::al(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0,
        write: true,
    }
];

static CWDE_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0,
        write: true,
    }
];

static CDQE_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0,
        write: true,
    }
];

// note CQD, CDQ, CQO:
//
// these are writes to dx/edx/rdx but *not* `*ax`. this is because while these registers "write"
// sign-extended *ax to *ax:*dx, "writes" to eax:edx do not modify the upper 32 bits of rax. that
// is to say, that if `rax` is 0x8000_1234_c000_0000 and a `cdq` is executed, the result is:
// ```
// rax = 0x8000_1234_c000_0000
// rdx = 0x0000_0000_ffff_ffff
// ```
//
// cool, huh!!
static CWD_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::dx(),
        disp: 0,
        write: true,
    }
];

static CDQ_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0,
        write: true,
    }
];

static CQO_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rdx(),
        disp: 0,
        write: true,
    }
];

static PUSHF_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rflags(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::Disp,
        reg: RegSpec::rsp(),
        disp: -8i32,
        write: true,
    },
    // push.. pushes the value (above), then does a RMW on rsp.
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

static POPF_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rsp(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rflags(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

static SAHF_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ah(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rflags(),
        disp: 0,
        write: true,
    }
];

static LAHF_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rflags(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ah(),
        disp: 0,
        write: true,
    }
];

static MOVS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 1,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 1,
        write: true,
    },
];

static LODS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 1,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 1,
        write: true,
    },
];

static STOS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 0,
        write: true,
    },
];

static SCAS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref_rdi,
        reg: RegSpec::eax(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rflags(),
        disp: 0,
        write: true,
    }
];

static RETURN_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rsp(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

static LEAVE_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rbp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rsp(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rbp(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

static XLAT_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        // xlat is the only implicit operand to use a base/index addressing scheme, so note the
        // base (rbx) and handle the implicit al index in code..?
        spec: OperandSpec::MemIndexScale,
        reg: RegSpec::rbx(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::al(),
        disp: 0,
        write: true,
    },
];

static CLTS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cr2(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cr2(),
        disp: 0,
        write: true,
    },
];

// the actual implicit operands of `{i,}mul` are broken out by operand count and operation size..
static MUL_OPS_1OP_BYTE: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::al(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: true,
    }
];
static MUL_OPS_1OP_WORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::dx(),
        disp: 0i32,
        write: true,
    }
];
static MUL_OPS_1OP_DWORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0i32,
        write: true,
    }
];
static MUL_OPS_1OP_QWORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rdx(),
        disp: 0i32,
        write: true,
    }
];

// the actual implicit operands of `{i,}div` are broken out by operation size..
static DIV_OPS_1OP_BYTE: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: true,
    },
];
static DIV_OPS_1OP_WORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::dx(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::dx(),
        disp: 0i32,
        write: true,
    }
];
static DIV_OPS_1OP_DWORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0i32,
        write: true,
    }
];
static DIV_OPS_1OP_QWORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rdx(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rdx(),
        disp: 0i32,
        write: true,
    }
];

static RDTSC_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0i32,
        write: true,
    }
];

static RDPMC_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0i32,
        write: true,
    }
];

static CPUID_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ebx(),
        disp: 0i32,
        write: true,
    },
];

static CALL_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::Disp,
        reg: RegSpec::rsp(),
        disp: -8i32,
        write: true,
    },
    // push.. pushes the value (above), then does a RMW on rsp.
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

static JMP_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: true,
    },
];

static JMPF_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cs(),
        disp: 0,
        write: true,
    },
];

static CALLF_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cs(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rip(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cs(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::Disp,
        reg: RegSpec::rsp(),
        disp: -8i32,
        write: true,
    },
    // push.. pushes the value (above), then does a RMW on rsp.
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    }
];

const PUSH_OPS_IDX: u16 = 1;
const POP_OPS_IDX: u16 = 2;
const JCC_OPS_IDX: u16 = 3;
const CBW_IDX: u16 = 4;
const CWDE_IDX: u16 = 5;
const CDQE_IDX: u16 = 6;
const CWD_IDX: u16 = 7;
const CDQ_IDX: u16 = 8;
const CQO_IDX: u16 = 9;
const PUSHF_IDX: u16 = 10;
const POPF_IDX: u16 = 11;
const SAHF_IDX: u16 = 12;
const LAHF_IDX: u16 = 13;
const MOVS_IDX: u16 = 14;
const LODS_IDX: u16 = 15;
const STOS_IDX: u16 = 16;
const SCAS_IDX: u16 = 17;
const RETURN_IDX: u16 = 18;
const LEAVE_IDX: u16 = 19;
const XLAT_IDX: u16 = 20;
const CLTS_IDX: u16 = 21;
const MUL_IDX_1OP_BYTE: u16 = 22;
const MUL_IDX_1OP_WORD: u16 = 23;
const MUL_IDX_1OP_DWORD: u16 = 24;
const MUL_IDX_1OP_QWORD: u16 = 25;
const DIV_IDX_1OP_BYTE: u16 = 26;
const DIV_IDX_1OP_WORD: u16 = 27;
const DIV_IDX_1OP_DWORD: u16 = 28;
const DIV_IDX_1OP_QWORD: u16 = 29;
const RDTSC_IDX: u16 = 30;
const RDPMC_IDX: u16 = 31;
const CPUID_IDX: u16 = 32;
const CALL_OPS_IDX: u16 = 33;
const JMP_OPS_IDX: u16 = 34;
const CALLF_OPS_IDX: u16 = 35;
const JMPF_OPS_IDX: u16 = 36;

static IMPLICIT_OPS_LIST: [&[ImplicitOperand]; 37] = [
    &[], // implicit ops list 0 is not used
    PUSH_OPS,
    POP_OPS,
    JCC_OPS,
    CBW_OPS,
    CWDE_OPS,
    CDQE_OPS,
    CWD_OPS,
    CDQ_OPS,
    CQO_OPS,
    PUSHF_OPS,
    POPF_OPS,
    SAHF_OPS,
    LAHF_OPS,
    MOVS_OPS,
    LODS_OPS,
    STOS_OPS,
    SCAS_OPS,
    RETURN_OPS,
    LEAVE_OPS,
    XLAT_OPS,
    CLTS_OPS,
    MUL_OPS_1OP_BYTE,
    MUL_OPS_1OP_WORD,
    MUL_OPS_1OP_DWORD,
    MUL_OPS_1OP_QWORD,
    DIV_OPS_1OP_BYTE,
    DIV_OPS_1OP_WORD,
    DIV_OPS_1OP_DWORD,
    DIV_OPS_1OP_QWORD,
    RDTSC_OPS,
    RDPMC_OPS,
    CPUID_OPS,
    CALL_OPS,
    JMP_OPS,
    CALLF_OPS,
    JMPF_OPS,
];

fn opcode2behavior(opc: &Opcode) -> Option<BehaviorDigest> {
    use Opcode::*;
    if opc == &MUL || opc == &IMUL || opc == &DIV || opc == &IDIV || opc == &NOP {
        return None;
    }
    let behavior = match opc {
        ADD => GENERAL_RW_R_FLAGWRITE,
        OR => GENERAL_RW_R_FLAGWRITE,
        ADC => GENERAL_RW_R_FLAGRW,
        SBB => GENERAL_RW_R_FLAGRW,
        AND => GENERAL_RW_R_FLAGWRITE,
        SUB => GENERAL_RW_R_FLAGWRITE,
        XOR => GENERAL_RW_R_FLAGWRITE,
        CMP => GENERAL_R_R_FLAGWRITE,
        ROL => GENERAL_RW_R_FLAGWRITE,
        ROR => GENERAL_RW_R_FLAGWRITE,
        RCL => GENERAL_RW_R_FLAGRW,
        RCR => GENERAL_RW_R_FLAGRW,
        SHL => GENERAL_RW_R_FLAGWRITE,
        SHR => GENERAL_RW_R_FLAGWRITE,
        SAL => GENERAL_RW_R_FLAGWRITE,
        SAR => GENERAL_RW_R_FLAGWRITE,
        BTC => GENERAL_RW_R_FLAGWRITE,
        BTR => GENERAL_RW_R_FLAGWRITE,
        BTS => GENERAL_RW_R_FLAGWRITE,
        CMPXCHG => GENERAL_RW_R_FLAGRW,
        CMPXCHG8B => GENERAL_RW_R_FLAGRW,
        CMPXCHG16B => GENERAL_RW_R_FLAGRW,
        DEC => GENERAL_RW_FLAGWRITE,
        INC => GENERAL_RW_FLAGWRITE,
        NEG => GENERAL_RW_FLAGWRITE,
        NOT => GENERAL_RW,
        XADD => GENERAL_RW_RW_FLAGRW,
        XCHG => GENERAL_RW_RW,

        CMPS => GENERAL_RW_RW_FLAGWRITE
            .set_implicit_ops(MOVS_IDX),
        SCAS => GENERAL_W_R_FLAGREAD
            .set_implicit_ops(SCAS_IDX), // TODO: second operand is `aX`, right?
        MOVS => GENERAL_W_R_FLAGREAD
            .set_implicit_ops(MOVS_IDX),
        LODS => GENERAL_W_R_FLAGREAD
            .set_implicit_ops(LODS_IDX),
        STOS => GENERAL_W_R_FLAGREAD
            .set_implicit_ops(STOS_IDX),
        INS => GENERAL_W_R,
        OUTS => GENERAL_R_R,

        Invalid => { panic!("todo: invalid"); },
        BT => GENERAL_R_R_FLAGWRITE,
        BSF => GENERAL_RW_R_FLAGWRITE,
        BSR => GENERAL_RW_R_FLAGWRITE,
        TZCNT => GENERAL_RW_R_FLAGWRITE,
        MOVSS => { panic!("todo: movss"); },
        ADDSS => { panic!("todo: addss"); },
        SUBSS => { panic!("todo: subss"); },
        MULSS => { panic!("todo: mulss"); },
        DIVSS => { panic!("todo: divss"); },
        MINSS => { panic!("todo: minss"); },
        MAXSS => { panic!("todo: maxss"); },
        SQRTSS => GENERAL_RW_R,
        MOVSD => { panic!("todo: movsd"); },
        SQRTSD => { panic!("todo: sqrtsd"); },
        ADDSD => { panic!("todo: addsd"); },
        SUBSD => { panic!("todo: subsd"); },
        MULSD => { panic!("todo: mulsd"); },
        DIVSD => { panic!("todo: divsd"); },
        MINSD => { panic!("todo: minsd"); },
        MAXSD => { panic!("todo: maxsd"); },
        MOVSLDUP => { panic!("todo: movsldup"); },
        MOVSHDUP => { panic!("todo: movshdup"); },
        MOVDDUP => { panic!("todo: movddup"); },
        HADDPS => GENERAL_RW_R,
        HSUBPS => GENERAL_RW_R,
        ADDSUBPD => { panic!("todo: addsubpd"); },
        ADDSUBPS => { panic!("todo: addsubps"); },
        CVTSI2SS => { panic!("todo: cvtsi2ss"); },
        CVTSI2SD => { panic!("todo: cvtsi2sd"); },
        CVTTSD2SI => { panic!("todo: cvttsd2si"); },
        CVTTPS2DQ => { panic!("todo: cvttps2dq"); },
        CVTPD2DQ => { panic!("todo: cvtpd2dq"); },
        CVTPD2PS => { panic!("todo: cvtpd2ps"); },
        CVTPS2DQ => { panic!("todo: cvtps2dq"); },
        CVTSD2SI => { panic!("todo: cvtsd2si"); },
        CVTSD2SS => { panic!("todo: cvtsd2ss"); },
        CVTTSS2SI => { panic!("todo: cvttss2si"); },
        CVTSS2SI => { panic!("todo: cvtss2si"); },
        CVTSS2SD => { panic!("todo: cvtss2sd"); },
        CVTDQ2PD => { panic!("todo: cvtdq2pd"); },
        LDDQU => { panic!("todo: lddqu"); },
        MOVZX => GENERAL_RW_R,
        MOVSX => GENERAL_RW_R,
        MOVSXD => GENERAL_RW_R,
        SHRD => { panic!("todo: shrd"); },
        // TODO: should be complex?
        HLT => BehaviorDigest::empty()
            .set_pl0(),
        CALL => BehaviorDigest::empty()
            .set_implicit_ops(CALL_OPS_IDX)
            .set_pl_any()
            .set_operand(0, Access::Read),
        CALLF => BehaviorDigest::empty()
            .set_implicit_ops(CALLF_OPS_IDX)
            .set_pl_any()
            .set_operand(0, Access::Read),
        JMP => BehaviorDigest::empty()
            .set_implicit_ops(JMP_OPS_IDX)
            .set_pl_any()
            .set_operand(0, Access::Read),
        JMPF => BehaviorDigest::empty()
            .set_implicit_ops(JMPF_OPS_IDX)
            .set_pl_any()
            .set_operand(0, Access::Read),
        PUSH => BehaviorDigest::empty()
            .set_implicit_ops(PUSH_OPS_IDX)
            .set_pl_any()
            .set_operand(0, Access::Read),
        POP => BehaviorDigest::empty()
            .set_implicit_ops(POP_OPS_IDX)
            .set_pl_any()
            .set_operand(0, Access::Write),
        LEA => GENERAL_W_R,
        NOP => GENERAL,
        PREFETCHNTA => GENERAL_R,
        PREFETCH0 => GENERAL_R,
        PREFETCH1 => GENERAL_R,
        PREFETCH2 => GENERAL_R,
        POPF => BehaviorDigest::empty()
            .set_implicit_ops(POPF_IDX)
            .set_pl_any(),
        INT => GENERAL_R,
        INTO => GENERAL_R_FLAGREAD,
        IRET => { panic!("todo: iret"); },
        IRETD => { panic!("todo: iretd"); },
        IRETQ => { panic!("todo: iretq"); },
        RETF => { panic!("todo: retf"); },
        ENTER => { panic!("todo: enter"); },
        LEAVE => BehaviorDigest::empty()
            .set_implicit_ops(LEAVE_IDX)
            .set_pl_any(),
        MOV => GENERAL_RW_R,
        RETURN => BehaviorDigest::empty()
            .set_implicit_ops(RETURN_IDX)
            .set_pl_any(),
        PUSHF => BehaviorDigest::empty()
            .set_implicit_ops(PUSHF_IDX)
            .set_pl_any(),
        WAIT => GENERAL,
        CBW => BehaviorDigest::empty()
            .set_implicit_ops(CBW_IDX)
            .set_pl_any(),
        CWDE => BehaviorDigest::empty()
            .set_implicit_ops(CWDE_IDX)
            .set_pl_any(),
        CDQE => BehaviorDigest::empty()
            .set_implicit_ops(CDQE_IDX)
            .set_pl_any(),
        CWD => BehaviorDigest::empty()
            .set_implicit_ops(CWD_IDX)
            .set_pl_any(),
        CDQ => BehaviorDigest::empty()
            .set_implicit_ops(CDQ_IDX)
            .set_pl_any(),
        CQO => BehaviorDigest::empty()
            .set_implicit_ops(CQO_IDX)
            .set_pl_any(),
        LAHF => BehaviorDigest::empty()
            .set_implicit_ops(LAHF_IDX)
            .set_pl_any(),
        SAHF => BehaviorDigest::empty()
            .set_implicit_ops(SAHF_IDX)
            .set_pl_any(),
        TEST => GENERAL_R_R_FLAGWRITE,
        IN => { panic!("todo: in"); },
        OUT => { panic!("todo: out"); },
        IMUL => BehaviorDigest::empty(), // unreachable due to branch above match
        JO => JCC,
        JNO => JCC,
        JB => JCC,
        JNB => JCC,
        JZ => JCC,
        JNZ => JCC,
        JA => JCC,
        JNA => JCC,
        JS => JCC,
        JNS => JCC,
        JP => JCC,
        JNP => JCC,
        JL => JCC,
        JGE => JCC,
        JLE => JCC,
        JG => JCC,
        CMOVA => CMOVCC,
        CMOVB => CMOVCC,
        CMOVG => CMOVCC,
        CMOVGE => CMOVCC,
        CMOVL => CMOVCC,
        CMOVLE => CMOVCC,
        CMOVNA => CMOVCC,
        CMOVNB => CMOVCC,
        CMOVNO => CMOVCC,
        CMOVNP => CMOVCC,
        CMOVNS => CMOVCC,
        CMOVNZ => CMOVCC,
        CMOVO => CMOVCC,
        CMOVP => CMOVCC,
        CMOVS => CMOVCC,
        CMOVZ => CMOVCC,
        DIV => BehaviorDigest::empty(), // unreachable due to branch above match
        IDIV => BehaviorDigest::empty(), // same as div
        MUL => BehaviorDigest::empty(), // same as div
        SETO => SETCC,
        SETNO => SETCC,
        SETB => SETCC,
        SETAE => SETCC,
        SETZ => SETCC,
        SETNZ => SETCC,
        SETBE => SETCC,
        SETA => SETCC,
        SETS => SETCC,
        SETNS => SETCC,
        SETP => SETCC,
        SETNP => SETCC,
        SETL => SETCC,
        SETGE => SETCC,
        SETLE => SETCC,
        SETG => SETCC,
        CPUID => BehaviorDigest::empty()
            .set_implicit_ops(CPUID_IDX)
            .set_pl_any(),
        UD0 => GENERAL,
        UD1 => GENERAL,
        UD2 => GENERAL,
        WBINVD => BehaviorDigest::empty()
            .set_pl0(),
        INVD =>  BehaviorDigest::empty()
            .set_pl0(),
        SYSRET => { panic!("todo: sysret"); },
        CLTS => BehaviorDigest::empty()
            .set_implicit_ops(CLTS_IDX)
            .set_pl0(),
        SYSCALL => { panic!("todo: syscall"); },
        LSL =>  BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_operand(1, Access::Read)
            .set_flags_access(Access::Write),
        LAR => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_operand(1, Access::Read)
            .set_flags_access(Access::Write),
        SGDT => BehaviorDigest::empty()
            .set_pl_special()
            .set_operand(0, Access::Write),
        SIDT => BehaviorDigest::empty()
            .set_pl_special()
            .set_operand(0, Access::Write),
        LGDT => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read),
        LIDT => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read),
        SMSW => { panic!("todo: smsw"); },
        LMSW => { panic!("todo: lmsw"); },
        SWAPGS => { panic!("todo: swapgs"); },
        RDTSCP => { panic!("todo: rdtscp"); },
        INVLPG => { panic!("todo: invlpg"); },
        FXSAVE => { panic!("todo: fxsave"); },
        FXRSTOR => { panic!("todo: fxrstor"); },
        LDMXCSR => { panic!("todo: ldmxcsr"); },
        STMXCSR => { panic!("todo: stmxcsr"); },
        XSAVE => { panic!("todo: xsave"); },
        XRSTOR => { panic!("todo: xrstor"); },
        XSAVEOPT => { panic!("todo: xsaveopt"); },
        LFENCE => GENERAL,
        MFENCE => GENERAL,
        SFENCE => GENERAL,
        // in almost all cases `clflush` does not "write" anything, but it is more of a write than
        // a read; from any other processor's perspective, the cache coherency protocol would
        // ensure that other processors' caches "are" memory and this would be a no-op for
        // architectural state. but for some kinds of memory (WC, for example), cache coherency is
        // more lax and the executing processor's cache is in fact writing up to 64 bytes of novel
        // data to main memory.
        CLFLUSH => GENERAL_W,
        // same argument as `clflush`.
        CLFLUSHOPT => GENERAL_W,
        // same argument as `clflush`.
        CLWB => GENERAL_W,
        WRMSR => { panic!("todo: wrmsr"); },
        RDTSC => BehaviorDigest::empty()
            .set_implicit_ops(RDTSC_IDX)
            .set_pl_special(),
        RDMSR => { panic!("todo: rdmsr"); },
        RDPMC => BehaviorDigest::empty()
            .set_implicit_ops(RDPMC_IDX)
            .set_pl_special(),
        SLDT => BehaviorDigest::empty()
            .set_pl_special()
            .set_operand(0, Access::Write),
        STR => { panic!("todo: str"); },
        LLDT => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read),
        LTR => { panic!("todo: ltr"); },
        VERR => { panic!("todo: verr"); },
        VERW => { panic!("todo: verw"); },
        CMC => GENERAL_FLAGRW,
        CLC => GENERAL_FLAGRW,
        STC => GENERAL_FLAGRW,
        CLI => GENERAL_FLAGRW
            .set_pl_special(),
        STI => GENERAL_FLAGRW
            .set_pl_special(),
        CLD => GENERAL_FLAGRW,
        STD => GENERAL_FLAGRW,
        JMPE => { panic!("todo: jmpe"); },
        POPCNT => GENERAL_W_R_FLAGWRITE,
        MOVDQU => { panic!("todo: movdqu"); },
        MOVDQA => { panic!("todo: movdqa"); },
        MOVQ => GENERAL_W_R,
        CMPSS => { panic!("todo: cmpss"); },
        CMPSD => { panic!("todo: cmpsd"); },
        UNPCKLPS => GENERAL_RW_R,
        UNPCKLPD => GENERAL_RW_R,
        UNPCKHPS => GENERAL_RW_R,
        UNPCKHPD => GENERAL_RW_R,
        PSHUFHW => { panic!("todo: pshufhw"); },
        PSHUFLW => { panic!("todo: pshuflw"); },
        MOVUPS => GENERAL_W_R,
        MOVQ2DQ => { panic!("todo: movq2dq"); },
        MOVDQ2Q => { panic!("todo: movdq2q"); },
        RSQRTSS => GENERAL_RW_R,
        RCPSS => { panic!("todo: rcpss"); },

        ANDN => { panic!("todo: andn"); },
        BEXTR => { panic!("todo: bextr"); },
        BLSI => { panic!("todo: blsi"); },
        BLSMSK => { panic!("todo: blsmsk"); },
        BLSR => { panic!("todo: blsr"); },
        VMCLEAR => { panic!("todo: vmclear"); },
        VMXON => { panic!("todo: vmxon"); },
        VMCALL => { panic!("todo: vmcall"); },
        VMLAUNCH => { panic!("todo: vmlaunch"); },
        VMRESUME => { panic!("todo: vmresume"); },
        VMXOFF => { panic!("todo: vmxoff"); },
        PCONFIG => { panic!("todo: pconfig"); },
        MONITOR => { panic!("todo: monitor"); },
        MWAIT => { panic!("todo: mwait"); },
        MONITORX => { panic!("todo: monitorx"); },
        MWAITX => { panic!("todo: mwaitx"); },
        CLAC => { panic!("todo: clac"); },
        STAC => { panic!("todo: stac"); },
        ENCLS => { panic!("todo: encls"); },
        ENCLV => { panic!("todo: enclv"); },
        XGETBV => { panic!("todo: xgetbv"); },
        XSETBV => { panic!("todo: xsetbv"); },
        VMFUNC => { panic!("todo: vmfunc"); },
        XABORT => { panic!("todo: xabort"); },
        XBEGIN => { panic!("todo: xbegin"); },
        XEND => { panic!("todo: xend"); },
        XTEST => { panic!("todo: xtest"); },
        ENCLU => { panic!("todo: enclu"); },
        RDPKRU => { panic!("todo: rdpkru"); },
        WRPKRU => { panic!("todo: wrpkru"); },

        RDPRU => { panic!("todo: rdpru"); },
        CLZERO => { panic!("todo: clzero"); },

        RDSEED => { panic!("todo: rdseed"); },
        RDRAND => { panic!("todo: rdrand"); },

        ADDPS => GENERAL_RW_R,
        ADDPD => GENERAL_RW_R,
        ANDNPS => GENERAL_RW_R,
        ANDNPD => GENERAL_RW_R,
        ANDPS => GENERAL_RW_R,
        ANDPD => GENERAL_RW_R,
        BSWAP => GENERAL_RW,
        CMPPD => { panic!("todo: cmppd"); },
        CMPPS => { panic!("todo: cmpps"); },
        COMISD => GENERAL_R_R_FLAGWRITE,
        COMISS => GENERAL_R_R_FLAGWRITE,
        CVTDQ2PS => GENERAL_W_R,
        CVTPI2PS => GENERAL_RW_R,
        // TODO: are these cvtp*2p* instructions targeting mmx actually read-write on the
        // destination? what happens to the top 16 bits of the destination?
        CVTPI2PD => GENERAL_W_R,
        CVTPS2PD => GENERAL_W_R,
        CVTPS2PI => GENERAL_W_R,
        CVTPD2PI => GENERAL_W_R,
        CVTTPS2PI => GENERAL_W_R,
        CVTTPD2PI => GENERAL_W_R,
        // exciting: zeroes the upper half of the xmm register, but leaves ymm/zmm unmodified
        CVTTPD2DQ => GENERAL_W_R,
        DIVPS => GENERAL_RW_R,
        DIVPD => GENERAL_RW_R,
        EMMS => GENERAL,
        GETSEC => { panic!("todo: getsec"); },
        LFS => { panic!("todo: lfs"); },
        LGS => { panic!("todo: lgs"); },
        LSS => { panic!("todo: lss"); },
        MASKMOVQ => { panic!("todo: maskmovq"); },
        MASKMOVDQU => { panic!("todo: maskmovdqu"); },
        MAXPS => GENERAL_RW_R,
        MAXPD => GENERAL_RW_R,
        MINPS => GENERAL_RW_R,
        MINPD => GENERAL_RW_R,
        MOVAPS => GENERAL_W_R,
        MOVAPD => GENERAL_W_R,
        MOVD => GENERAL_W_R,
        MOVLPS => GENERAL_RW_R,
        MOVLPD => GENERAL_RW_R,
        MOVHPS => GENERAL_RW_R,
        MOVHPD => GENERAL_RW_R,
        MOVLHPS => { panic!("todo: movlhps"); },
        MOVHLPS => { panic!("todo: movhlps"); },
        MOVUPD => GENERAL_W_R,
        MOVMSKPS => { panic!("todo: movmskps"); },
        MOVMSKPD => { panic!("todo: movmskpd"); },
        MOVNTI => { panic!("todo: movnti"); },
        MOVNTPS => GENERAL_W_R,
        MOVNTPD => GENERAL_W_R,
        EXTRQ => { panic!("todo: extrq"); },
        INSERTQ => { panic!("todo: insertq"); },
        MOVNTSS => { panic!("todo: movntss"); },
        MOVNTSD => { panic!("todo: movntsd"); },
        MOVNTQ => { panic!("todo: movntq"); },
        MOVNTDQ => { panic!("todo: movntdq"); },
        MULPS => GENERAL_RW_R,
        MULPD => GENERAL_RW_R,
        ORPS => GENERAL_RW_R,
        ORPD => GENERAL_RW_R,
        PACKSSDW => GENERAL_RW_R,
        PACKSSWB => GENERAL_RW_R,
        PACKUSWB => GENERAL_RW_R,
        PADDB => { panic!("todo: paddb"); },
        PADDD => { panic!("todo: paddd"); },
        PADDQ => { panic!("todo: paddq"); },
        PADDSB => { panic!("todo: paddsb"); },
        PADDSW => { panic!("todo: paddsw"); },
        PADDUSB => { panic!("todo: paddusb"); },
        PADDUSW => { panic!("todo: paddusw"); },
        PADDW => { panic!("todo: paddw"); },
        PAND => { panic!("todo: pand"); },
        PANDN => { panic!("todo: pandn"); },
        PAVGB => { panic!("todo: pavgb"); },
        PAVGW => { panic!("todo: pavgw"); },
        PCMPEQB => GENERAL_RW_R,
        PCMPEQD => GENERAL_RW_R,
        PCMPEQW => GENERAL_RW_R,
        PCMPGTB => GENERAL_RW_R,
        PCMPGTD => GENERAL_RW_R,
        PCMPGTW => GENERAL_RW_R,
        PINSRW => { panic!("todo: pinsrw"); },
        PMADDWD => { panic!("todo: pmaddwd"); },
        PMAXSW => { panic!("todo: pmaxsw"); },
        PMAXUB => { panic!("todo: pmaxub"); },
        PMINSW => { panic!("todo: pminsw"); },
        PMINUB => { panic!("todo: pminub"); },
        PMOVMSKB => { panic!("todo: pmovmskb"); },
        PMULHUW => { panic!("todo: pmulhuw"); },
        PMULHW => { panic!("todo: pmulhw"); },
        PMULLW => { panic!("todo: pmullw"); },
        PMULUDQ => { panic!("todo: pmuludq"); },
        POR => { panic!("todo: por"); },
        PSADBW => { panic!("todo: psadbw"); },
        PSHUFW => { panic!("todo: pshufw"); },
        PSHUFD => { panic!("todo: pshufd"); },
        PSLLD => { panic!("todo: pslld"); },
        PSLLDQ => { panic!("todo: pslldq"); },
        PSLLQ => { panic!("todo: psllq"); },
        PSLLW => { panic!("todo: psllw"); },
        PSRAD => { panic!("todo: psrad"); },
        PSRAW => { panic!("todo: psraw"); },
        PSRLD => { panic!("todo: psrld"); },
        PSRLDQ => { panic!("todo: psrldq"); },
        PSRLQ => { panic!("todo: psrlq"); },
        PSRLW => { panic!("todo: psrlw"); },
        PSUBB => { panic!("todo: psubb"); },
        PSUBD => { panic!("todo: psubd"); },
        PSUBQ => { panic!("todo: psubq"); },
        PSUBSB => { panic!("todo: psubsb"); },
        PSUBSW => { panic!("todo: psubsw"); },
        PSUBUSB => { panic!("todo: psubusb"); },
        PSUBUSW => { panic!("todo: psubusw"); },
        PSUBW => { panic!("todo: psubw"); },
        PUNPCKHBW => GENERAL_RW_R,
        PUNPCKHDQ => GENERAL_RW_R,
        PUNPCKHWD => GENERAL_RW_R,
        PUNPCKLBW => GENERAL_RW_R,
        PUNPCKLDQ => GENERAL_RW_R,
        PUNPCKLWD => GENERAL_RW_R,
        PUNPCKLQDQ => GENERAL_RW_R,
        PUNPCKHQDQ => GENERAL_RW_R,
        PXOR => { panic!("todo: pxor"); },
        RCPPS => GENERAL_W_R,
        RSM => { panic!("todo: rsm"); },
        RSQRTPS => GENERAL_W_R,
        SHLD => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        SHUFPD => { panic!("todo: shufpd"); },
        SHUFPS => { panic!("todo: shufps"); },
        SLHD => { panic!("todo: slhd"); },
        SQRTPS => GENERAL_W_R,
        SQRTPD => GENERAL_W_R,
        SUBPS => GENERAL_RW_R,
        SUBPD => GENERAL_RW_R,
        SYSENTER => { panic!("todo: sysenter"); },
        SYSEXIT => { panic!("todo: sysexit"); },
        UCOMISD => GENERAL_R_R_FLAGWRITE,
        UCOMISS => GENERAL_R_R_FLAGWRITE,
        VMREAD => { panic!("todo: vmread"); },
        VMWRITE => { panic!("todo: vmwrite"); },
        XORPS => GENERAL_RW_R,
        XORPD => GENERAL_RW_R,

        VMOVDDUP => { panic!("todo: vmovddup"); },
        VPSHUFLW => { panic!("todo: vpshuflw"); },
        VPSHUFHW => { panic!("todo: vpshufhw"); },
        VHADDPS => GENERAL_W_R_R,
        VHSUBPS => GENERAL_W_R_R,
        VADDSUBPS => { panic!("todo: vaddsubps"); },
        VCVTPD2DQ => { panic!("todo: vcvtpd2dq"); },
        VLDDQU => { panic!("todo: vlddqu"); },

        VCOMISD => GENERAL_R_R_FLAGWRITE,
        VCOMISS => GENERAL_R_R_FLAGWRITE,
        VUCOMISD => GENERAL_R_R_FLAGWRITE,
        VUCOMISS => GENERAL_R_R_FLAGWRITE,
        VADDPD => GENERAL_W_R_R,
        VADDPS => GENERAL_W_R_R,
        VADDSD => { panic!("todo: vaddsd"); },
        VADDSS => { panic!("todo: vaddss"); },
        VADDSUBPD => { panic!("todo: vaddsubpd"); },
        VAESDEC => { panic!("todo: vaesdec"); },
        VAESDECLAST => { panic!("todo: vaesdeclast"); },
        VAESENC => { panic!("todo: vaesenc"); },
        VAESENCLAST => { panic!("todo: vaesenclast"); },
        VAESIMC => { panic!("todo: vaesimc"); },
        VAESKEYGENASSIST => { panic!("todo: vaeskeygenassist"); },
        VBLENDPD => { panic!("todo: vblendpd"); },
        VBLENDPS => { panic!("todo: vblendps"); },
        VBLENDVPD => { panic!("todo: vblendvpd"); },
        VBLENDVPS => { panic!("todo: vblendvps"); },
        VBROADCASTF128 => { panic!("todo: vbroadcastf128"); },
        VBROADCASTI128 => { panic!("todo: vbroadcasti128"); },
        VBROADCASTSD => { panic!("todo: vbroadcastsd"); },
        VBROADCASTSS => { panic!("todo: vbroadcastss"); },
        VCMPSD => { panic!("todo: vcmpsd"); },
        VCMPSS => { panic!("todo: vcmpss"); },
        VCMPPD => { panic!("todo: vcmppd"); },
        VCMPPS => { panic!("todo: vcmpps"); },
        VCVTDQ2PD => { panic!("todo: vcvtdq2pd"); },
        VCVTDQ2PS => GENERAL_W_R,
        VCVTPD2PS => { panic!("todo: vcvtpd2ps"); },
        VCVTPH2PS => { panic!("todo: vcvtph2ps"); },
        VCVTPS2DQ => { panic!("todo: vcvtps2dq"); },
        VCVTPS2PD => { panic!("todo: vcvtps2pd"); },
        VCVTSS2SD => { panic!("todo: vcvtss2sd"); },
        VCVTSI2SS => { panic!("todo: vcvtsi2ss"); },
        VCVTSI2SD => { panic!("todo: vcvtsi2sd"); },
        VCVTSD2SI => { panic!("todo: vcvtsd2si"); },
        VCVTSD2SS => { panic!("todo: vcvtsd2ss"); },
        VCVTPS2PH => { panic!("todo: vcvtps2ph"); },
        VCVTSS2SI => { panic!("todo: vcvtss2si"); },
        VCVTTPD2DQ => { panic!("todo: vcvttpd2dq"); },
        VCVTTPS2DQ => { panic!("todo: vcvttps2dq"); },
        VCVTTSS2SI => { panic!("todo: vcvttss2si"); },
        VCVTTSD2SI => { panic!("todo: vcvttsd2si"); },
        VDIVPD => GENERAL_W_R_R,
        VDIVPS => GENERAL_W_R_R,
        VDIVSD => { panic!("todo: vdivsd"); },
        VDIVSS => { panic!("todo: vdivss"); },
        VDPPD => { panic!("todo: vdppd"); },
        VDPPS => { panic!("todo: vdpps"); },
        VEXTRACTF128 => { panic!("todo: vextractf128"); },
        VEXTRACTI128 => { panic!("todo: vextracti128"); },
        VEXTRACTPS => { panic!("todo: vextractps"); },
        VFMADD132PD => { panic!("todo: vfmadd132pd"); },
        VFMADD132PS => { panic!("todo: vfmadd132ps"); },
        VFMADD132SD => { panic!("todo: vfmadd132sd"); },
        VFMADD132SS => { panic!("todo: vfmadd132ss"); },
        VFMADD213PD => { panic!("todo: vfmadd213pd"); },
        VFMADD213PS => { panic!("todo: vfmadd213ps"); },
        VFMADD213SD => { panic!("todo: vfmadd213sd"); },
        VFMADD213SS => { panic!("todo: vfmadd213ss"); },
        VFMADD231PD => { panic!("todo: vfmadd231pd"); },
        VFMADD231PS => { panic!("todo: vfmadd231ps"); },
        VFMADD231SD => { panic!("todo: vfmadd231sd"); },
        VFMADD231SS => { panic!("todo: vfmadd231ss"); },
        VFMADDSUB132PD => { panic!("todo: vfmaddsub132pd"); },
        VFMADDSUB132PS => { panic!("todo: vfmaddsub132ps"); },
        VFMADDSUB213PD => { panic!("todo: vfmaddsub213pd"); },
        VFMADDSUB213PS => { panic!("todo: vfmaddsub213ps"); },
        VFMADDSUB231PD => { panic!("todo: vfmaddsub231pd"); },
        VFMADDSUB231PS => { panic!("todo: vfmaddsub231ps"); },
        VFMSUB132PD => { panic!("todo: vfmsub132pd"); },
        VFMSUB132PS => { panic!("todo: vfmsub132ps"); },
        VFMSUB132SD => { panic!("todo: vfmsub132sd"); },
        VFMSUB132SS => { panic!("todo: vfmsub132ss"); },
        VFMSUB213PD => { panic!("todo: vfmsub213pd"); },
        VFMSUB213PS => { panic!("todo: vfmsub213ps"); },
        VFMSUB213SD => { panic!("todo: vfmsub213sd"); },
        VFMSUB213SS => { panic!("todo: vfmsub213ss"); },
        VFMSUB231PD => { panic!("todo: vfmsub231pd"); },
        VFMSUB231PS => { panic!("todo: vfmsub231ps"); },
        VFMSUB231SD => { panic!("todo: vfmsub231sd"); },
        VFMSUB231SS => { panic!("todo: vfmsub231ss"); },
        VFMSUBADD132PD => { panic!("todo: vfmsubadd132pd"); },
        VFMSUBADD132PS => { panic!("todo: vfmsubadd132ps"); },
        VFMSUBADD213PD => { panic!("todo: vfmsubadd213pd"); },
        VFMSUBADD213PS => { panic!("todo: vfmsubadd213ps"); },
        VFMSUBADD231PD => { panic!("todo: vfmsubadd231pd"); },
        VFMSUBADD231PS => { panic!("todo: vfmsubadd231ps"); },
        VFNMADD132PD => { panic!("todo: vfnmadd132pd"); },
        VFNMADD132PS => { panic!("todo: vfnmadd132ps"); },
        VFNMADD132SD => { panic!("todo: vfnmadd132sd"); },
        VFNMADD132SS => { panic!("todo: vfnmadd132ss"); },
        VFNMADD213PD => { panic!("todo: vfnmadd213pd"); },
        VFNMADD213PS => { panic!("todo: vfnmadd213ps"); },
        VFNMADD213SD => { panic!("todo: vfnmadd213sd"); },
        VFNMADD213SS => { panic!("todo: vfnmadd213ss"); },
        VFNMADD231PD => { panic!("todo: vfnmadd231pd"); },
        VFNMADD231PS => { panic!("todo: vfnmadd231ps"); },
        VFNMADD231SD => { panic!("todo: vfnmadd231sd"); },
        VFNMADD231SS => { panic!("todo: vfnmadd231ss"); },
        VFNMSUB132PD => { panic!("todo: vfnmsub132pd"); },
        VFNMSUB132PS => { panic!("todo: vfnmsub132ps"); },
        VFNMSUB132SD => { panic!("todo: vfnmsub132sd"); },
        VFNMSUB132SS => { panic!("todo: vfnmsub132ss"); },
        VFNMSUB213PD => { panic!("todo: vfnmsub213pd"); },
        VFNMSUB213PS => { panic!("todo: vfnmsub213ps"); },
        VFNMSUB213SD => { panic!("todo: vfnmsub213sd"); },
        VFNMSUB213SS => { panic!("todo: vfnmsub213ss"); },
        VFNMSUB231PD => { panic!("todo: vfnmsub231pd"); },
        VFNMSUB231PS => { panic!("todo: vfnmsub231ps"); },
        VFNMSUB231SD => { panic!("todo: vfnmsub231sd"); },
        VFNMSUB231SS => { panic!("todo: vfnmsub231ss"); },
        VGATHERDPD => { panic!("todo: vgatherdpd"); },
        VGATHERDPS => { panic!("todo: vgatherdps"); },
        VGATHERQPD => { panic!("todo: vgatherqpd"); },
        VGATHERQPS => { panic!("todo: vgatherqps"); },
        VHADDPD => GENERAL_W_R_R,
        VHSUBPD => GENERAL_W_R_R,
        VINSERTF128 => { panic!("todo: vinsertf128"); },
        VINSERTI128 => { panic!("todo: vinserti128"); },
        VINSERTPS => { panic!("todo: vinsertps"); },
        VMASKMOVDQU => { panic!("todo: vmaskmovdqu"); },
        VMASKMOVPD => { panic!("todo: vmaskmovpd"); },
        VMASKMOVPS => { panic!("todo: vmaskmovps"); },
        VMAXPD => GENERAL_W_R_R,
        VMAXPS => GENERAL_W_R_R,
        VMAXSD => { panic!("todo: vmaxsd"); },
        VMAXSS => { panic!("todo: vmaxss"); },
        VMINPD => GENERAL_W_R_R,
        VMINPS => GENERAL_W_R_R,
        VMINSD => { panic!("todo: vminsd"); },
        VMINSS => { panic!("todo: vminss"); },
        VMOVAPD => { panic!("todo: vmovapd"); },
        VMOVAPS => { panic!("todo: vmovaps"); },
        VMOVD => GENERAL_W_R,
        VMOVDQA => { panic!("todo: vmovdqa"); },
        VMOVDQU => { panic!("todo: vmovdqu"); },
        VMOVHLPS => { panic!("todo: vmovhlps"); },
        VMOVHPD => { panic!("todo: vmovhpd"); },
        VMOVHPS => { panic!("todo: vmovhps"); },
        VMOVLHPS => { panic!("todo: vmovlhps"); },
        VMOVLPD => { panic!("todo: vmovlpd"); },
        VMOVLPS => { panic!("todo: vmovlps"); },
        VMOVMSKPD => { panic!("todo: vmovmskpd"); },
        VMOVMSKPS => { panic!("todo: vmovmskps"); },
        VMOVNTDQ => { panic!("todo: vmovntdq"); },
        VMOVNTDQA => { panic!("todo: vmovntdqa"); },
        VMOVNTPD => { panic!("todo: vmovntpd"); },
        VMOVNTPS => { panic!("todo: vmovntps"); },
        VMOVQ => GENERAL_W_R,
        VMOVSS => { panic!("todo: vmovss"); },
        VMOVSD => { panic!("todo: vmovsd"); },
        VMOVSHDUP => { panic!("todo: vmovshdup"); },
        VMOVSLDUP => { panic!("todo: vmovsldup"); },
        VMOVUPD => { panic!("todo: vmovupd"); },
        VMOVUPS => { panic!("todo: vmovups"); },
        VMPSADBW => { panic!("todo: vmpsadbw"); },
        VMULPD => GENERAL_W_R_R,
        VMULPS => GENERAL_W_R_R,
        VMULSD => { panic!("todo: vmulsd"); },
        VMULSS => { panic!("todo: vmulss"); },
        VPABSB => { panic!("todo: vpabsb"); },
        VPABSD => { panic!("todo: vpabsd"); },
        VPABSW => { panic!("todo: vpabsw"); },
        VPACKSSDW => GENERAL_W_R_R,
        VPACKUSDW => GENERAL_W_R_R,
        VPACKSSWB => GENERAL_W_R_R,
        VPACKUSWB => GENERAL_W_R_R,
        VPADDB => { panic!("todo: vpaddb"); },
        VPADDD => { panic!("todo: vpaddd"); },
        VPADDQ => { panic!("todo: vpaddq"); },
        VPADDSB => { panic!("todo: vpaddsb"); },
        VPADDSW => { panic!("todo: vpaddsw"); },
        VPADDUSB => { panic!("todo: vpaddusb"); },
        VPADDUSW => { panic!("todo: vpaddusw"); },
        VPADDW => { panic!("todo: vpaddw"); },
        VPALIGNR => { panic!("todo: vpalignr"); },
        VANDPD => GENERAL_W_R_R,
        VANDPS => GENERAL_W_R_R,
        VORPD => GENERAL_W_R_R,
        VORPS => GENERAL_W_R_R,
        VANDNPD => GENERAL_W_R_R,
        VANDNPS => GENERAL_W_R_R,
        VPAND => { panic!("todo: vpand"); },
        VPANDN => { panic!("todo: vpandn"); },
        VPAVGB => { panic!("todo: vpavgb"); },
        VPAVGW => { panic!("todo: vpavgw"); },
        VPBLENDD => { panic!("todo: vpblendd"); },
        VPBLENDVB => { panic!("todo: vpblendvb"); },
        VPBLENDW => { panic!("todo: vpblendw"); },
        VPBROADCASTB => { panic!("todo: vpbroadcastb"); },
        VPBROADCASTD => { panic!("todo: vpbroadcastd"); },
        VPBROADCASTQ => { panic!("todo: vpbroadcastq"); },
        VPBROADCASTW => { panic!("todo: vpbroadcastw"); },
        VPCLMULQDQ => { panic!("todo: vpclmulqdq"); },
        VPCMPEQB => GENERAL_W_R_R,
        VPCMPEQD => GENERAL_W_R_R,
        VPCMPEQQ => GENERAL_W_R_R,
        VPCMPEQW => GENERAL_W_R_R,
        VPCMPGTB => GENERAL_W_R_R,
        VPCMPGTD => GENERAL_W_R_R,
        VPCMPGTQ => GENERAL_W_R_R,
        VPCMPGTW => GENERAL_W_R_R,
        VPCMPESTRI => { panic!("todo: vpcmpestri"); },
        VPCMPESTRM => { panic!("todo: vpcmpestrm"); },
        VPCMPISTRI => { panic!("todo: vpcmpistri"); },
        VPCMPISTRM => { panic!("todo: vpcmpistrm"); },
        VPERM2F128 => { panic!("todo: vperm2f128"); },
        VPERM2I128 => { panic!("todo: vperm2i128"); },
        VPERMD => { panic!("todo: vpermd"); },
        VPERMILPD => { panic!("todo: vpermilpd"); },
        VPERMILPS => { panic!("todo: vpermilps"); },
        VPERMPD => { panic!("todo: vpermpd"); },
        VPERMPS => { panic!("todo: vpermps"); },
        VPERMQ => { panic!("todo: vpermq"); },
        VPEXTRB => { panic!("todo: vpextrb"); },
        VPEXTRD => { panic!("todo: vpextrd"); },
        VPEXTRQ => { panic!("todo: vpextrq"); },
        VPEXTRW => { panic!("todo: vpextrw"); },
        VPGATHERDD => { panic!("todo: vpgatherdd"); },
        VPGATHERDQ => { panic!("todo: vpgatherdq"); },
        VPGATHERQD => { panic!("todo: vpgatherqd"); },
        VPGATHERQQ => { panic!("todo: vpgatherqq"); },
        VPHADDD => { panic!("todo: vphaddd"); },
        VPHADDSW => { panic!("todo: vphaddsw"); },
        VPHADDW => { panic!("todo: vphaddw"); },
        VPMADDUBSW => { panic!("todo: vpmaddubsw"); },
        VPHMINPOSUW => { panic!("todo: vphminposuw"); },
        VPHSUBD => { panic!("todo: vphsubd"); },
        VPHSUBSW => { panic!("todo: vphsubsw"); },
        VPHSUBW => { panic!("todo: vphsubw"); },
        VPINSRB => { panic!("todo: vpinsrb"); },
        VPINSRD => { panic!("todo: vpinsrd"); },
        VPINSRQ => { panic!("todo: vpinsrq"); },
        VPINSRW => { panic!("todo: vpinsrw"); },
        VPMADDWD => { panic!("todo: vpmaddwd"); },
        VPMASKMOVD => { panic!("todo: vpmaskmovd"); },
        VPMASKMOVQ => { panic!("todo: vpmaskmovq"); },
        VPMAXSB => { panic!("todo: vpmaxsb"); },
        VPMAXSD => { panic!("todo: vpmaxsd"); },
        VPMAXSW => { panic!("todo: vpmaxsw"); },
        VPMAXUB => { panic!("todo: vpmaxub"); },
        VPMAXUW => { panic!("todo: vpmaxuw"); },
        VPMAXUD => { panic!("todo: vpmaxud"); },
        VPMINSB => { panic!("todo: vpminsb"); },
        VPMINSW => { panic!("todo: vpminsw"); },
        VPMINSD => { panic!("todo: vpminsd"); },
        VPMINUB => { panic!("todo: vpminub"); },
        VPMINUW => { panic!("todo: vpminuw"); },
        VPMINUD => { panic!("todo: vpminud"); },
        VPMOVMSKB => { panic!("todo: vpmovmskb"); },
        VPMOVSXBD => { panic!("todo: vpmovsxbd"); },
        VPMOVSXBQ => { panic!("todo: vpmovsxbq"); },
        VPMOVSXBW => { panic!("todo: vpmovsxbw"); },
        VPMOVSXDQ => { panic!("todo: vpmovsxdq"); },
        VPMOVSXWD => { panic!("todo: vpmovsxwd"); },
        VPMOVSXWQ => { panic!("todo: vpmovsxwq"); },
        VPMOVZXBD => { panic!("todo: vpmovzxbd"); },
        VPMOVZXBQ => { panic!("todo: vpmovzxbq"); },
        VPMOVZXBW => { panic!("todo: vpmovzxbw"); },
        VPMOVZXDQ => { panic!("todo: vpmovzxdq"); },
        VPMOVZXWD => { panic!("todo: vpmovzxwd"); },
        VPMOVZXWQ => { panic!("todo: vpmovzxwq"); },
        VPMULDQ => { panic!("todo: vpmuldq"); },
        VPMULHRSW => { panic!("todo: vpmulhrsw"); },
        VPMULHUW => { panic!("todo: vpmulhuw"); },
        VPMULHW => { panic!("todo: vpmulhw"); },
        VPMULLQ => { panic!("todo: vpmullq"); },
        VPMULLD => { panic!("todo: vpmulld"); },
        VPMULLW => { panic!("todo: vpmullw"); },
        VPMULUDQ => { panic!("todo: vpmuludq"); },
        VPOR => { panic!("todo: vpor"); },
        VPSADBW => { panic!("todo: vpsadbw"); },
        VPSHUFB => { panic!("todo: vpshufb"); },
        VPSHUFD => { panic!("todo: vpshufd"); },
        VPSIGNB => { panic!("todo: vpsignb"); },
        VPSIGND => { panic!("todo: vpsignd"); },
        VPSIGNW => { panic!("todo: vpsignw"); },
        VPSLLD => { panic!("todo: vpslld"); },
        VPSLLDQ => { panic!("todo: vpslldq"); },
        VPSLLQ => { panic!("todo: vpsllq"); },
        VPSLLVD => { panic!("todo: vpsllvd"); },
        VPSLLVQ => { panic!("todo: vpsllvq"); },
        VPSLLW => { panic!("todo: vpsllw"); },
        VPSRAD => { panic!("todo: vpsrad"); },
        VPSRAVD => { panic!("todo: vpsravd"); },
        VPSRAW => { panic!("todo: vpsraw"); },
        VPSRLD => { panic!("todo: vpsrld"); },
        VPSRLDQ => { panic!("todo: vpsrldq"); },
        VPSRLQ => { panic!("todo: vpsrlq"); },
        VPSRLVD => { panic!("todo: vpsrlvd"); },
        VPSRLVQ => { panic!("todo: vpsrlvq"); },
        VPSRLW => { panic!("todo: vpsrlw"); },
        VPSUBB => { panic!("todo: vpsubb"); },
        VPSUBD => { panic!("todo: vpsubd"); },
        VPSUBQ => { panic!("todo: vpsubq"); },
        VPSUBSB => { panic!("todo: vpsubsb"); },
        VPSUBSW => { panic!("todo: vpsubsw"); },
        VPSUBUSB => { panic!("todo: vpsubusb"); },
        VPSUBUSW => { panic!("todo: vpsubusw"); },
        VPSUBW => { panic!("todo: vpsubw"); },
        VPTEST => { panic!("todo: vptest"); },
        VPUNPCKHBW => GENERAL_W_R_R,
        VPUNPCKHDQ => GENERAL_W_R_R,
        VPUNPCKHQDQ => GENERAL_W_R_R,
        VPUNPCKHWD => GENERAL_W_R_R,
        VPUNPCKLBW => GENERAL_W_R_R,
        VPUNPCKLDQ => GENERAL_W_R_R,
        VPUNPCKLQDQ => GENERAL_W_R_R,
        VPUNPCKLWD => GENERAL_W_R_R,
        VPXOR => { panic!("todo: vpxor"); },
        VRCPPS => GENERAL_W_R,
        VROUNDPD => { panic!("todo: vroundpd"); },
        VROUNDPS => { panic!("todo: vroundps"); },
        VROUNDSD => { panic!("todo: vroundsd"); },
        VROUNDSS => { panic!("todo: vroundss"); },
        VRSQRTPS => GENERAL_W_R,
        VRSQRTSS => GENERAL_RW_R,
        VRCPSS => { panic!("todo: vrcpss"); },
        VSHUFPD => { panic!("todo: vshufpd"); },
        VSHUFPS => { panic!("todo: vshufps"); },
        VSQRTPD => GENERAL_W_R,
        VSQRTPS => GENERAL_W_R,
        VSQRTSS => GENERAL_RW_R,
        VSQRTSD => { panic!("todo: vsqrtsd"); },
        VSUBPD => { panic!("todo: vsubpd"); },
        VSUBPS => { panic!("todo: vsubps"); },
        VSUBSD => { panic!("todo: vsubsd"); },
        VSUBSS => { panic!("todo: vsubss"); },
        VTESTPD => { panic!("todo: vtestpd"); },
        VTESTPS => { panic!("todo: vtestps"); },
        VUNPCKHPD => { panic!("todo: vunpckhpd"); },
        VUNPCKHPS => { panic!("todo: vunpckhps"); },
        VUNPCKLPD => { panic!("todo: vunpcklpd"); },
        VUNPCKLPS => { panic!("todo: vunpcklps"); },
        VXORPD => GENERAL_W_R_R,
        VXORPS => GENERAL_W_R_R,
        VZEROUPPER => { panic!("todo: vzeroupper"); },
        VZEROALL => { panic!("todo: vzeroall"); },
        VLDMXCSR => { panic!("todo: vldmxcsr"); },
        VSTMXCSR => { panic!("todo: vstmxcsr"); },

        PCLMULQDQ => { panic!("todo: pclmulqdq"); },
        AESKEYGENASSIST => { panic!("todo: aeskeygenassist"); },
        AESIMC => { panic!("todo: aesimc"); },
        AESENC => { panic!("todo: aesenc"); },
        AESENCLAST => { panic!("todo: aesenclast"); },
        AESDEC => { panic!("todo: aesdec"); },
        AESDECLAST => { panic!("todo: aesdeclast"); },
        PCMPGTQ => GENERAL_RW_R,
        PCMPISTRM => { panic!("todo: pcmpistrm"); },
        PCMPISTRI => { panic!("todo: pcmpistri"); },
        PCMPESTRI => { panic!("todo: pcmpestri"); },
        PACKUSDW => { panic!("todo: packusdw"); },
        PCMPESTRM => { panic!("todo: pcmpestrm"); },
        PCMPEQQ => GENERAL_RW_R,
        PTEST => { panic!("todo: ptest"); },
        PHMINPOSUW => { panic!("todo: phminposuw"); },
        DPPS => { panic!("todo: dpps"); },
        DPPD => { panic!("todo: dppd"); },
        MPSADBW => { panic!("todo: mpsadbw"); },
        PMOVZXDQ => { panic!("todo: pmovzxdq"); },
        PMOVSXDQ => { panic!("todo: pmovsxdq"); },
        PMOVZXBD => { panic!("todo: pmovzxbd"); },
        PMOVSXBD => { panic!("todo: pmovsxbd"); },
        PMOVZXWQ => { panic!("todo: pmovzxwq"); },
        PMOVSXWQ => { panic!("todo: pmovsxwq"); },
        PMOVZXBQ => { panic!("todo: pmovzxbq"); },
        PMOVSXBQ => { panic!("todo: pmovsxbq"); },
        PMOVSXWD => { panic!("todo: pmovsxwd"); },
        PMOVZXWD => { panic!("todo: pmovzxwd"); },
        PEXTRQ => { panic!("todo: pextrq"); },
        PEXTRD => { panic!("todo: pextrd"); },
        PEXTRW => { panic!("todo: pextrw"); },
        PEXTRB => { panic!("todo: pextrb"); },
        PMOVSXBW => { panic!("todo: pmovsxbw"); },
        PMOVZXBW => { panic!("todo: pmovzxbw"); },
        PINSRQ => { panic!("todo: pinsrq"); },
        PINSRD => { panic!("todo: pinsrd"); },
        PINSRB => { panic!("todo: pinsrb"); },
        EXTRACTPS => { panic!("todo: extractps"); },
        INSERTPS => { panic!("todo: insertps"); },
        ROUNDSS => { panic!("todo: roundss"); },
        ROUNDSD => { panic!("todo: roundsd"); },
        ROUNDPS => { panic!("todo: roundps"); },
        ROUNDPD => { panic!("todo: roundpd"); },
        PMAXSB => { panic!("todo: pmaxsb"); },
        PMAXSD => { panic!("todo: pmaxsd"); },
        PMAXUW => { panic!("todo: pmaxuw"); },
        PMAXUD => { panic!("todo: pmaxud"); },
        PMINSD => { panic!("todo: pminsd"); },
        PMINSB => { panic!("todo: pminsb"); },
        PMINUD => { panic!("todo: pminud"); },
        PMINUW => { panic!("todo: pminuw"); },
        BLENDW => { panic!("todo: blendw"); },
        PBLENDVB => { panic!("todo: pblendvb"); },
        PBLENDW => { panic!("todo: pblendw"); },
        BLENDVPS => { panic!("todo: blendvps"); },
        BLENDVPD => { panic!("todo: blendvpd"); },
        BLENDPS => { panic!("todo: blendps"); },
        BLENDPD => { panic!("todo: blendpd"); },
        PMULDQ => { panic!("todo: pmuldq"); },
        MOVNTDQA => { panic!("todo: movntdqa"); },
        PMULLD => { panic!("todo: pmulld"); },
        PALIGNR => { panic!("todo: palignr"); },
        PSIGNW => { panic!("todo: psignw"); },
        PSIGND => { panic!("todo: psignd"); },
        PSIGNB => { panic!("todo: psignb"); },
        PSHUFB => { panic!("todo: pshufb"); },
        PMULHRSW => { panic!("todo: pmulhrsw"); },
        PMADDUBSW => { panic!("todo: pmaddubsw"); },
        PABSD => { panic!("todo: pabsd"); },
        PABSW => { panic!("todo: pabsw"); },
        PABSB => { panic!("todo: pabsb"); },
        PHSUBSW => { panic!("todo: phsubsw"); },
        PHSUBW => { panic!("todo: phsubw"); },
        PHSUBD => { panic!("todo: phsubd"); },
        PHADDD => { panic!("todo: phaddd"); },
        PHADDSW => { panic!("todo: phaddsw"); },
        PHADDW => { panic!("todo: phaddw"); },
        HSUBPD => GENERAL_RW_R,
        HADDPD => GENERAL_RW_R,

        SHA1RNDS4 => { panic!("todo: sha1rnds4"); },
        SHA1NEXTE => { panic!("todo: sha1nexte"); },
        SHA1MSG1 => { panic!("todo: sha1msg1"); },
        SHA1MSG2 => { panic!("todo: sha1msg2"); },
        SHA256RNDS2 => { panic!("todo: sha256rnds2"); },
        SHA256MSG1 => { panic!("todo: sha256msg1"); },
        SHA256MSG2 => { panic!("todo: sha256msg2"); },

        LZCNT => { panic!("todo: lzcnt"); },
        CLGI => { panic!("todo: clgi"); },
        STGI => { panic!("todo: stgi"); },
        SKINIT => { panic!("todo: skinit"); },
        VMLOAD => { panic!("todo: vmload"); },
        VMMCALL => { panic!("todo: vmmcall"); },
        VMSAVE => { panic!("todo: vmsave"); },
        VMRUN => { panic!("todo: vmrun"); },
        INVLPGA => { panic!("todo: invlpga"); },
        INVLPGB => { panic!("todo: invlpgb"); },
        TLBSYNC => { panic!("todo: tlbsync"); },

        MOVBE => { panic!("todo: movbe"); },

        ADCX => { panic!("todo: adcx"); },
        ADOX => { panic!("todo: adox"); },

        PREFETCHW => { panic!("todo: prefetchw"); },

        RDPID => { panic!("todo: rdpid"); },
        VMPTRLD => { panic!("todo: vmptrld"); },
        VMPTRST => { panic!("todo: vmptrst"); },

        BZHI => { panic!("todo: bzhi"); },
        MULX => { panic!("todo: mulx"); },
        SHLX => { panic!("todo: shlx"); },
        SHRX => { panic!("todo: shrx"); },
        SARX => { panic!("todo: sarx"); },
        PDEP => { panic!("todo: pdep"); },
        PEXT => { panic!("todo: pext"); },
        RORX => { panic!("todo: rorx"); },
        XRSTORS => { panic!("todo: xrstors"); },
        XRSTORS64 => { panic!("todo: xrstors64"); },
        XSAVEC => { panic!("todo: xsavec"); },
        XSAVEC64 => { panic!("todo: xsavec64"); },
        XSAVES => { panic!("todo: xsaves"); },
        XSAVES64 => { panic!("todo: xsaves64"); },

        RDFSBASE => { panic!("todo: rdfsbase"); },
        RDGSBASE => { panic!("todo: rdgsbase"); },
        WRFSBASE => { panic!("todo: wrfsbase"); },
        WRGSBASE => { panic!("todo: wrgsbase"); },

        CRC32 => { panic!("todo: crc32"); },
        SALC => { panic!("todo: salc"); },
        XLAT => BehaviorDigest::empty()
            .set_implicit_ops(XLAT_IDX)
            .set_pl_any(),

        // TODO: none of x87 is verified well.. and what about the bits in the FPU status word..
        // and what about pushes/pops from the x87 operand stack..
        // TODO: read st(0), write st(0)
        F2XM1 => BehaviorDigest::empty()
            .set_pl_any(),
        FABS => GENERAL, // TODO: this is really an implicit write to st(0)
        FADD => GENERAL_RW_R,
        FADDP => GENERAL_RW_R,
        FBLD => GENERAL_W_R,
        FBSTP => GENERAL_W_R,
        FCHS => GENERAL_W_R,
        FCMOVB => GENERAL_W_R_FLAGREAD,
        FCMOVBE => GENERAL_W_R_FLAGREAD,
        FCMOVE => GENERAL_W_R_FLAGREAD,
        FCMOVNB => GENERAL_W_R_FLAGREAD,
        FCMOVNBE => GENERAL_W_R_FLAGREAD,
        FCMOVNE => GENERAL_W_R_FLAGREAD,
        FCMOVNU => GENERAL_W_R_FLAGREAD,
        FCMOVU => GENERAL_W_R_FLAGREAD,
        FCOM => GENERAL_R_R,
        FCOMI => GENERAL_R_R_FLAGWRITE,
        FCOMIP => GENERAL_R_R_FLAGWRITE,
        FCOMP => GENERAL_R_R,
        FCOMPP => GENERAL_R_R,
        // TODO: st(0) -> st(0)
        FCOS => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: x87 stack pointer dec
        FDECSTP => BehaviorDigest::empty()
            .set_pl_any(),
        FDIV => GENERAL_RW_R,
        // TODO: x87 stack pop
        FDIVP => GENERAL_RW_R,
        FDIVR => GENERAL_RW_R,
        // TODO: x87 stack pop
        FDIVRP => GENERAL_RW_R,
        FENI8087_NOP => BehaviorDigest::empty()
            .set_pl_any(),
        FDISI8087_NOP => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: the behavior here is ... inaccurate. st(i) is not read, but state associated with
        // that register is modified. so it's kind of read?
        FFREE => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_pl_any(),
        // same as `ffree` above.
        FFREEP => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_pl_any(),
        FIADD => GENERAL_RW_R,
        FICOM => GENERAL_R_R,
        FICOMP => GENERAL_R_R,
        FIDIV => GENERAL_RW_R,
        FIDIVR => GENERAL_RW_R,
        // TODO: writing to st(0) is only kind of accurate, this *pushes* to the operand stack..
        FILD => GENERAL_W_R,
        FIMUL => GENERAL_RW_R,
        // TODO: x87 stack pointer inc
        FINCSTP => BehaviorDigest::empty()
            .set_pl_any(),
        FIST => GENERAL_W_R,
        FISTP => GENERAL_W_R,
        FISTTP => GENERAL_W_R,
        FISUB => GENERAL_RW_R,
        FISUBR => GENERAL_RW_R,
        // TODO: writing to st(0) is only kind of accurate, this *pushes* to the operand stack..
        FLD => GENERAL_W_R,
        // TODO: fpu stack write
        FLD1 => BehaviorDigest::empty()
            .set_pl_any(),
        FLDCW => GENERAL_R,
        FLDENV => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: fpu stack write
        FLDL2E => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: fpu stack write
        FLDL2T => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: fpu stack write
        FLDLG2 => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: fpu stack write
        FLDLN2 => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: fpu stack write
        FLDPI =>  BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: fpu stack write
        FLDZ =>  BehaviorDigest::empty()
            .set_pl_any(),
        FMUL => GENERAL_RW_R,
        FMULP => GENERAL_RW_R,
        // TODO: report change to x87 flags?
        FNCLEX => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: report change to x87 flags?
        FNINIT => BehaviorDigest::empty()
            .set_pl_any(),
        FNOP => BehaviorDigest::empty()
            .set_pl_any(),
        FNSAVE => { panic!("todo: fnsave"); },
        FNSTCW => { panic!("todo: fnstcw"); },
        FNSTENV => { panic!("todo: fnstenv"); },
        FNSTOR => { panic!("todo: fnstor"); },
        FNSTSW => { panic!("todo: fnstsw"); },
        // TODO: read st(1) with atan(st(1)/st(0)) and pop
        FPATAN => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: read st(0), st(1), write st(0)
        FPREM => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: read st(0), st(1), write st(0)
        FPREM1 => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: read st(0), write, push?
        FPTAN => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: read st(0), write, push?
        FRNDINT => BehaviorDigest::empty()
            .set_pl_any(),
        FRSTOR => { panic!("todo: frstor"); },
        // TODO: read st(0), st(1)
        FSCALE => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: report this as a complex instruction?
        FSETPM287_NOP => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: st(0) -> st(0)
        FSIN => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: st(0) -> st(0)
        FSINCOS => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: st(0) -> st(0)
        FSQRT => BehaviorDigest::empty()
            .set_pl_any(),
        FST => GENERAL_W_R,
        FSTP => GENERAL_W_R,
        FSTPNCE => GENERAL_W_R,
        FSUB => GENERAL_RW_R,
        FSUBP => GENERAL_RW_R,
        FSUBR => GENERAL_RW_R,
        FSUBRP => GENERAL_RW_R,
        // TODO: report change to x87 flags, read of st(0)?
        FTST => BehaviorDigest::empty()
            .set_pl_any(),
        FUCOM => GENERAL_R_R,
        FUCOMI => GENERAL_R_R_FLAGWRITE,
        FUCOMIP => GENERAL_R_R_FLAGWRITE,
        FUCOMP => GENERAL_R_R,
        FUCOMPP => GENERAL_R_R,
        // TODO: report change to x87 flags?
        FXAM => BehaviorDigest::empty()
            .set_pl_any(),
        FXCH => GENERAL_RW_RW,
        // TODO: read st(0), write st(0), x87 push
        FXTRACT => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: read st(0), write st(0)
        FYL2X => BehaviorDigest::empty()
            .set_pl_any(),
        // TODO: read st(0), write st(0)
        FYL2XP1 => BehaviorDigest::empty()
            .set_pl_any(),

        LOOPNZ => { panic!("todo: loopnz"); },
        LOOPZ => { panic!("todo: loopz"); },
        LOOP => { panic!("todo: loop"); },
        JRCXZ => { panic!("todo: jrcxz"); },

        // started shipping in Tremont, 2020 sept 23
        MOVDIR64B => { panic!("todo: movdir64b"); },
        MOVDIRI => { panic!("todo: movdiri"); },

        // started shipping in Tiger Lake, 2020 sept 2
        AESDEC128KL => { panic!("todo: aesdec128kl"); },
        AESDEC256KL => { panic!("todo: aesdec256kl"); },
        AESDECWIDE128KL => { panic!("todo: aesdecwide128kl"); },
        AESDECWIDE256KL => { panic!("todo: aesdecwide256kl"); },
        AESENC128KL => { panic!("todo: aesenc128kl"); },
        AESENC256KL => { panic!("todo: aesenc256kl"); },
        AESENCWIDE128KL => { panic!("todo: aesencwide128kl"); },
        AESENCWIDE256KL => { panic!("todo: aesencwide256kl"); },
        ENCODEKEY128 => { panic!("todo: encodekey128"); },
        ENCODEKEY256 => { panic!("todo: encodekey256"); },
        LOADIWKEY => { panic!("todo: loadiwkey"); },

        // unsure
        HRESET => { panic!("todo: hreset"); },

        // 3dnow. note these are yet untested!
        FEMMS => GENERAL,
        PI2FW => { panic!("todo: pi2fw"); },
        PI2FD => { panic!("todo: pi2fd"); },
        PF2IW => { panic!("todo: pf2iw"); },
        PF2ID => { panic!("todo: pf2id"); },
        PMULHRW => { panic!("todo: pmulhrw"); },
        PFCMPGE => { panic!("todo: pfcmpge"); },
        PFMIN => { panic!("todo: pfmin"); },
        PFRCP => { panic!("todo: pfrcp"); },
        PFRSQRT => { panic!("todo: pfrsqrt"); },
        PFSUB => { panic!("todo: pfsub"); },
        PFADD => { panic!("todo: pfadd"); },
        PFCMPGT => { panic!("todo: pfcmpgt"); },
        PFMAX => { panic!("todo: pfmax"); },
        PFRCPIT1 => { panic!("todo: pfrcpit1"); },
        PFRSQIT1 => { panic!("todo: pfrsqit1"); },
        PFSUBR => { panic!("todo: pfsubr"); },
        PFACC => { panic!("todo: pfacc"); },
        PFCMPEQ => { panic!("todo: pfcmpeq"); },
        PFMUL => { panic!("todo: pfmul"); },
        PFMULHRW => { panic!("todo: pfmulhrw"); },
        PFRCPIT2 => { panic!("todo: pfrcpit2"); },
        PFNACC => { panic!("todo: pfnacc"); },
        PFPNACC => { panic!("todo: pfpnacc"); },
        PSWAPD => { panic!("todo: pswapd"); },
        PAVGUSB => { panic!("todo: pavgusb"); },

        // ENQCMD
        ENQCMD => { panic!("todo: enqcmd"); },
        ENQCMDS => { panic!("todo: enqcmds"); },

        // INVPCID
        INVEPT => { panic!("todo: invept"); },
        INVVPID => { panic!("todo: invvpid"); },
        INVPCID => { panic!("todo: invpcid"); },

        // PTWRITE
        PTWRITE => { panic!("todo: ptwrite"); },

        // GFNI
        GF2P8AFFINEQB => { panic!("todo: gf2p8affineqb"); },
        GF2P8AFFINEINVQB => { panic!("todo: gf2p8affineinvqb"); },
        GF2P8MULB => { panic!("todo: gf2p8mulb"); },

        // CET
        WRUSS => { panic!("todo: wruss"); },
        WRSS => { panic!("todo: wrss"); },
        INCSSP => { panic!("todo: incssp"); },
        SAVEPREVSSP => { panic!("todo: saveprevssp"); },
        SETSSBSY => { panic!("todo: setssbsy"); },
        CLRSSBSY => { panic!("todo: clrssbsy"); },
        RSTORSSP => { panic!("todo: rstorssp"); },
        ENDBR64 => { panic!("todo: endbr64"); },
        ENDBR32 => { panic!("todo: endbr32"); },

        // TDX
        TDCALL => { panic!("todo: tdcall"); },
        SEAMRET => { panic!("todo: seamret"); },
        SEAMOPS => { panic!("todo: seamops"); },
        SEAMCALL => { panic!("todo: seamcall"); },

        // WAITPKG
        TPAUSE => { panic!("todo: tpause"); },
        UMONITOR => { panic!("todo: umonitor"); },
        UMWAIT => { panic!("todo: umwait"); },

        // UINTR
        UIRET => { panic!("todo: uiret"); },
        TESTUI => { panic!("todo: testui"); },
        CLUI => { panic!("todo: clui"); },
        STUI => { panic!("todo: stui"); },
        SENDUIPI => { panic!("todo: senduipi"); },

        // TSXLDTRK
        XSUSLDTRK => { panic!("todo: xsusldtrk"); },
        XRESLDTRK => { panic!("todo: xresldtrk"); },

        // AVX512F
        VALIGND => { panic!("todo: valignd"); },
        VALIGNQ => { panic!("todo: valignq"); },
        VBLENDMPD => { panic!("todo: vblendmpd"); },
        VBLENDMPS => { panic!("todo: vblendmps"); },
        VCOMPRESSPD => { panic!("todo: vcompresspd"); },
        VCOMPRESSPS => { panic!("todo: vcompressps"); },
        VCVTPD2UDQ => { panic!("todo: vcvtpd2udq"); },
        VCVTTPD2UDQ => { panic!("todo: vcvttpd2udq"); },
        VCVTPS2UDQ => { panic!("todo: vcvtps2udq"); },
        VCVTTPS2UDQ => { panic!("todo: vcvttps2udq"); },
        VCVTQQ2PD => { panic!("todo: vcvtqq2pd"); },
        VCVTQQ2PS => { panic!("todo: vcvtqq2ps"); },
        VCVTSD2USI => { panic!("todo: vcvtsd2usi"); },
        VCVTTSD2USI => { panic!("todo: vcvttsd2usi"); },
        VCVTSS2USI => { panic!("todo: vcvtss2usi"); },
        VCVTTSS2USI => { panic!("todo: vcvttss2usi"); },
        VCVTUDQ2PD => { panic!("todo: vcvtudq2pd"); },
        VCVTUDQ2PS => { panic!("todo: vcvtudq2ps"); },
        VCVTUSI2USD => { panic!("todo: vcvtusi2usd"); },
        VCVTUSI2USS => { panic!("todo: vcvtusi2uss"); },
        VEXPANDPD => { panic!("todo: vexpandpd"); },
        VEXPANDPS => { panic!("todo: vexpandps"); },
        VEXTRACTF32X4 => { panic!("todo: vextractf32x4"); },
        VEXTRACTF64X4 => { panic!("todo: vextractf64x4"); },
        VEXTRACTI32X4 => { panic!("todo: vextracti32x4"); },
        VEXTRACTI64X4 => { panic!("todo: vextracti64x4"); },
        VFIXUPIMMPD => { panic!("todo: vfixupimmpd"); },
        VFIXUPIMMPS => { panic!("todo: vfixupimmps"); },
        VFIXUPIMMSD => { panic!("todo: vfixupimmsd"); },
        VFIXUPIMMSS => { panic!("todo: vfixupimmss"); },
        VGETEXPPD => { panic!("todo: vgetexppd"); },
        VGETEXPPS => { panic!("todo: vgetexpps"); },
        VGETEXPSD => { panic!("todo: vgetexpsd"); },
        VGETEXPSS => { panic!("todo: vgetexpss"); },
        VGETMANTPD => { panic!("todo: vgetmantpd"); },
        VGETMANTPS => { panic!("todo: vgetmantps"); },
        VGETMANTSD => { panic!("todo: vgetmantsd"); },
        VGETMANTSS => { panic!("todo: vgetmantss"); },
        VINSERTF32X4 => { panic!("todo: vinsertf32x4"); },
        VINSERTF64X4 => { panic!("todo: vinsertf64x4"); },
        VINSERTI64X4 => { panic!("todo: vinserti64x4"); },
        VMOVDQA32 => { panic!("todo: vmovdqa32"); },
        VMOVDQA64 => { panic!("todo: vmovdqa64"); },
        VMOVDQU32 => { panic!("todo: vmovdqu32"); },
        VMOVDQU64 => { panic!("todo: vmovdqu64"); },
        VPBLENDMD => { panic!("todo: vpblendmd"); },
        VPBLENDMQ => { panic!("todo: vpblendmq"); },
        VPCMPD => { panic!("todo: vpcmpd"); },
        VPCMPUD => { panic!("todo: vpcmpud"); },
        VPCMPQ => { panic!("todo: vpcmpq"); },
        VPCMPUQ => { panic!("todo: vpcmpuq"); },
        VPCOMPRESSQ => { panic!("todo: vpcompressq"); },
        VPCOMPRESSD => { panic!("todo: vpcompressd"); },
        VPERMI2D => { panic!("todo: vpermi2d"); },
        VPERMI2Q => { panic!("todo: vpermi2q"); },
        VPERMI2PD => { panic!("todo: vpermi2pd"); },
        VPERMI2PS => { panic!("todo: vpermi2ps"); },
        VPERMT2D => { panic!("todo: vpermt2d"); },
        VPERMT2Q => { panic!("todo: vpermt2q"); },
        VPERMT2PD => { panic!("todo: vpermt2pd"); },
        VPERMT2PS => { panic!("todo: vpermt2ps"); },
        VPMAXSQ => { panic!("todo: vpmaxsq"); },
        VPMAXUQ => { panic!("todo: vpmaxuq"); },
        VPMINSQ => { panic!("todo: vpminsq"); },
        VPMINUQ => { panic!("todo: vpminuq"); },
        VPMOVSQB => { panic!("todo: vpmovsqb"); },
        VPMOVUSQB => { panic!("todo: vpmovusqb"); },
        VPMOVSQW => { panic!("todo: vpmovsqw"); },
        VPMOVUSQW => { panic!("todo: vpmovusqw"); },
        VPMOVSQD => { panic!("todo: vpmovsqd"); },
        VPMOVUSQD => { panic!("todo: vpmovusqd"); },
        VPMOVSDB => { panic!("todo: vpmovsdb"); },
        VPMOVUSDB => { panic!("todo: vpmovusdb"); },
        VPMOVSDW => { panic!("todo: vpmovsdw"); },
        VPMOVUSDW => { panic!("todo: vpmovusdw"); },
        VPROLD => { panic!("todo: vprold"); },
        VPROLQ => { panic!("todo: vprolq"); },
        VPROLVD => { panic!("todo: vprolvd"); },
        VPROLVQ => { panic!("todo: vprolvq"); },
        VPRORD => { panic!("todo: vprord"); },
        VPRORQ => { panic!("todo: vprorq"); },
        VPRORRD => { panic!("todo: vprorrd"); },
        VPRORRQ => { panic!("todo: vprorrq"); },
        VPSCATTERDD => { panic!("todo: vpscatterdd"); },
        VPSCATTERDQ => { panic!("todo: vpscatterdq"); },
        VPSCATTERQD => { panic!("todo: vpscatterqd"); },
        VPSCATTERQQ => { panic!("todo: vpscatterqq"); },
        VPSRAQ => { panic!("todo: vpsraq"); },
        VPSRAVQ => { panic!("todo: vpsravq"); },
        VPTESTNMD => { panic!("todo: vptestnmd"); },
        VPTESTNMQ => { panic!("todo: vptestnmq"); },
        VPTERNLOGD => { panic!("todo: vpternlogd"); },
        VPTERNLOGQ => { panic!("todo: vpternlogq"); },
        VPTESTMD => { panic!("todo: vptestmd"); },
        VPTESTMQ => { panic!("todo: vptestmq"); },
        VRCP14PD => { panic!("todo: vrcp14pd"); },
        VRCP14PS => { panic!("todo: vrcp14ps"); },
        VRCP14SD => { panic!("todo: vrcp14sd"); },
        VRCP14SS => { panic!("todo: vrcp14ss"); },
        VRNDSCALEPD => { panic!("todo: vrndscalepd"); },
        VRNDSCALEPS => { panic!("todo: vrndscaleps"); },
        VRNDSCALESD => { panic!("todo: vrndscalesd"); },
        VRNDSCALESS => { panic!("todo: vrndscaless"); },
        VRSQRT14PD => { panic!("todo: vrsqrt14pd"); },
        VRSQRT14PS => { panic!("todo: vrsqrt14ps"); },
        VRSQRT14SD => { panic!("todo: vrsqrt14sd"); },
        VRSQRT14SS => { panic!("todo: vrsqrt14ss"); },
        VSCALEDPD => { panic!("todo: vscaledpd"); },
        VSCALEDPS => { panic!("todo: vscaledps"); },
        VSCALEDSD => { panic!("todo: vscaledsd"); },
        VSCALEDSS => { panic!("todo: vscaledss"); },
        VSCATTERDD => { panic!("todo: vscatterdd"); },
        VSCATTERDQ => { panic!("todo: vscatterdq"); },
        VSCATTERQD => { panic!("todo: vscatterqd"); },
        VSCATTERQQ => { panic!("todo: vscatterqq"); },
        VSHUFF32X4 => { panic!("todo: vshuff32x4"); },
        VSHUFF64X2 => { panic!("todo: vshuff64x2"); },
        VSHUFI32X4 => { panic!("todo: vshufi32x4"); },
        VSHUFI64X2 => { panic!("todo: vshufi64x2"); },

        // AVX512DQ
        VCVTTPD2QQ => { panic!("todo: vcvttpd2qq"); },
        VCVTPD2QQ => { panic!("todo: vcvtpd2qq"); },
        VCVTTPD2UQQ => { panic!("todo: vcvttpd2uqq"); },
        VCVTPD2UQQ => { panic!("todo: vcvtpd2uqq"); },
        VCVTTPS2QQ => { panic!("todo: vcvttps2qq"); },
        VCVTPS2QQ => { panic!("todo: vcvtps2qq"); },
        VCVTTPS2UQQ => { panic!("todo: vcvttps2uqq"); },
        VCVTPS2UQQ => { panic!("todo: vcvtps2uqq"); },
        VCVTUQQ2PD => { panic!("todo: vcvtuqq2pd"); },
        VCVTUQQ2PS => { panic!("todo: vcvtuqq2ps"); },
        VEXTRACTF64X2 => { panic!("todo: vextractf64x2"); },
        VEXTRACTI64X2 => { panic!("todo: vextracti64x2"); },
        VFPCLASSPD => { panic!("todo: vfpclasspd"); },
        VFPCLASSPS => { panic!("todo: vfpclassps"); },
        VFPCLASSSD => { panic!("todo: vfpclasssd"); },
        VFPCLASSSS => { panic!("todo: vfpclassss"); },
        VINSERTF64X2 => { panic!("todo: vinsertf64x2"); },
        VINSERTI64X2 => { panic!("todo: vinserti64x2"); },
        VPMOVM2D => { panic!("todo: vpmovm2d"); },
        VPMOVM2Q => { panic!("todo: vpmovm2q"); },
        VPMOVB2D => { panic!("todo: vpmovb2d"); },
        VPMOVQ2M => { panic!("todo: vpmovq2m"); },
        VRANGEPD => { panic!("todo: vrangepd"); },
        VRANGEPS => { panic!("todo: vrangeps"); },
        VRANGESD => { panic!("todo: vrangesd"); },
        VRANGESS => { panic!("todo: vrangess"); },
        VREDUCEPD => { panic!("todo: vreducepd"); },
        VREDUCEPS => { panic!("todo: vreduceps"); },
        VREDUCESD => { panic!("todo: vreducesd"); },
        VREDUCESS => { panic!("todo: vreducess"); },

        // AVX512BW
        VDBPSADBW => { panic!("todo: vdbpsadbw"); },
        VMOVDQU8 => { panic!("todo: vmovdqu8"); },
        VMOVDQU16 => { panic!("todo: vmovdqu16"); },
        VPBLENDMB => { panic!("todo: vpblendmb"); },
        VPBLENDMW => { panic!("todo: vpblendmw"); },
        VPCMPB => { panic!("todo: vpcmpb"); },
        VPCMPUB => { panic!("todo: vpcmpub"); },
        VPCMPW => { panic!("todo: vpcmpw"); },
        VPCMPUW => { panic!("todo: vpcmpuw"); },
        VPERMW => { panic!("todo: vpermw"); },
        VPERMI2B => { panic!("todo: vpermi2b"); },
        VPERMI2W => { panic!("todo: vpermi2w"); },
        VPMOVM2B => { panic!("todo: vpmovm2b"); },
        VPMOVM2W => { panic!("todo: vpmovm2w"); },
        VPMOVB2M => { panic!("todo: vpmovb2m"); },
        VPMOVW2M => { panic!("todo: vpmovw2m"); },
        VPMOVSWB => { panic!("todo: vpmovswb"); },
        VPMOVUSWB => { panic!("todo: vpmovuswb"); },
        VPSLLVW => { panic!("todo: vpsllvw"); },
        VPSRAVW => { panic!("todo: vpsravw"); },
        VPSRLVW => { panic!("todo: vpsrlvw"); },
        VPTESTNMB => { panic!("todo: vptestnmb"); },
        VPTESTNMW => { panic!("todo: vptestnmw"); },
        VPTESTMB => { panic!("todo: vptestmb"); },
        VPTESTMW => { panic!("todo: vptestmw"); },

        // AVX512CD
        VPBROADCASTM => { panic!("todo: vpbroadcastm"); },
        VPCONFLICTD => { panic!("todo: vpconflictd"); },
        VPCONFLICTQ => { panic!("todo: vpconflictq"); },
        VPLZCNTD => { panic!("todo: vplzcntd"); },
        VPLZCNTQ => { panic!("todo: vplzcntq"); },

        KUNPCKBW => { panic!("todo: kunpckbw"); },
        KUNPCKWD => { panic!("todo: kunpckwd"); },
        KUNPCKDQ => { panic!("todo: kunpckdq"); },

        KADDB => { panic!("todo: kaddb"); },
        KANDB => { panic!("todo: kandb"); },
        KANDNB => { panic!("todo: kandnb"); },
        KMOVB => { panic!("todo: kmovb"); },
        KNOTB => { panic!("todo: knotb"); },
        KORB => { panic!("todo: korb"); },
        KORTESTB => { panic!("todo: kortestb"); },
        KSHIFTLB => { panic!("todo: kshiftlb"); },
        KSHIFTRB => { panic!("todo: kshiftrb"); },
        KTESTB => { panic!("todo: ktestb"); },
        KXNORB => { panic!("todo: kxnorb"); },
        KXORB => { panic!("todo: kxorb"); },
        KADDW => { panic!("todo: kaddw"); },
        KANDW => { panic!("todo: kandw"); },
        KANDNW => { panic!("todo: kandnw"); },
        KMOVW => { panic!("todo: kmovw"); },
        KNOTW => { panic!("todo: knotw"); },
        KORW => { panic!("todo: korw"); },
        KORTESTW => { panic!("todo: kortestw"); },
        KSHIFTLW => { panic!("todo: kshiftlw"); },
        KSHIFTRW => { panic!("todo: kshiftrw"); },
        KTESTW => { panic!("todo: ktestw"); },
        KXNORW => { panic!("todo: kxnorw"); },
        KXORW => { panic!("todo: kxorw"); },
        KADDD => { panic!("todo: kaddd"); },
        KANDD => { panic!("todo: kandd"); },
        KANDND => { panic!("todo: kandnd"); },
        KMOVD => { panic!("todo: kmovd"); },
        KNOTD => { panic!("todo: knotd"); },
        KORD => { panic!("todo: kord"); },
        KORTESTD => { panic!("todo: kortestd"); },
        KSHIFTLD => { panic!("todo: kshiftld"); },
        KSHIFTRD => { panic!("todo: kshiftrd"); },
        KTESTD => { panic!("todo: ktestd"); },
        KXNORD => { panic!("todo: kxnord"); },
        KXORD => { panic!("todo: kxord"); },
        KADDQ => { panic!("todo: kaddq"); },
        KANDQ => { panic!("todo: kandq"); },
        KANDNQ => { panic!("todo: kandnq"); },
        KMOVQ => { panic!("todo: kmovq"); },
        KNOTQ => { panic!("todo: knotq"); },
        KORQ => { panic!("todo: korq"); },
        KORTESTQ => { panic!("todo: kortestq"); },
        KSHIFTLQ => { panic!("todo: kshiftlq"); },
        KSHIFTRQ => { panic!("todo: kshiftrq"); },
        KTESTQ => { panic!("todo: ktestq"); },
        KXNORQ => { panic!("todo: kxnorq"); },
        KXORQ => { panic!("todo: kxorq"); },

        // AVX512ER
        VEXP2PD => { panic!("todo: vexp2pd"); },
        VEXP2PS => { panic!("todo: vexp2ps"); },
        VEXP2SD => { panic!("todo: vexp2sd"); },
        VEXP2SS => { panic!("todo: vexp2ss"); },
        VRCP28PD => { panic!("todo: vrcp28pd"); },
        VRCP28PS => { panic!("todo: vrcp28ps"); },
        VRCP28SD => { panic!("todo: vrcp28sd"); },
        VRCP28SS => { panic!("todo: vrcp28ss"); },
        VRSQRT28PD => { panic!("todo: vrsqrt28pd"); },
        VRSQRT28PS => { panic!("todo: vrsqrt28ps"); },
        VRSQRT28SD => { panic!("todo: vrsqrt28sd"); },
        VRSQRT28SS => { panic!("todo: vrsqrt28ss"); },

        // AVX512PF
        VGATHERPF0DPD => { panic!("todo: vgatherpf0dpd"); },
        VGATHERPF0DPS => { panic!("todo: vgatherpf0dps"); },
        VGATHERPF0QPD => { panic!("todo: vgatherpf0qpd"); },
        VGATHERPF0QPS => { panic!("todo: vgatherpf0qps"); },
        VGATHERPF1DPD => { panic!("todo: vgatherpf1dpd"); },
        VGATHERPF1DPS => { panic!("todo: vgatherpf1dps"); },
        VGATHERPF1QPD => { panic!("todo: vgatherpf1qpd"); },
        VGATHERPF1QPS => { panic!("todo: vgatherpf1qps"); },
        VSCATTERPF0DPD => { panic!("todo: vscatterpf0dpd"); },
        VSCATTERPF0DPS => { panic!("todo: vscatterpf0dps"); },
        VSCATTERPF0QPD => { panic!("todo: vscatterpf0qpd"); },
        VSCATTERPF0QPS => { panic!("todo: vscatterpf0qps"); },
        VSCATTERPF1DPD => { panic!("todo: vscatterpf1dpd"); },
        VSCATTERPF1DPS => { panic!("todo: vscatterpf1dps"); },
        VSCATTERPF1QPD => { panic!("todo: vscatterpf1qpd"); },
        VSCATTERPF1QPS => { panic!("todo: vscatterpf1qps"); },

        // MPX
        BNDMK => { panic!("todo: bndmk"); },
        BNDCL => { panic!("todo: bndcl"); },
        BNDCU => { panic!("todo: bndcu"); },
        BNDCN => { panic!("todo: bndcn"); },
        BNDMOV => { panic!("todo: bndmov"); },
        BNDLDX => { panic!("todo: bndldx"); },
        BNDSTX => { panic!("todo: bndstx"); },

        VGF2P8AFFINEQB => { panic!("todo: vgf2p8affineqb"); },
        VGF2P8AFFINEINVQB => { panic!("todo: vgf2p8affineinvqb"); },
        VPSHRDQ => { panic!("todo: vpshrdq"); },
        VPSHRDD => { panic!("todo: vpshrdd"); },
        VPSHRDW => { panic!("todo: vpshrdw"); },
        VPSHLDQ => { panic!("todo: vpshldq"); },
        VPSHLDD => { panic!("todo: vpshldd"); },
        VPSHLDW => { panic!("todo: vpshldw"); },
        VBROADCASTF32X8 => { panic!("todo: vbroadcastf32x8"); },
        VBROADCASTF64X4 => { panic!("todo: vbroadcastf64x4"); },
        VBROADCASTF32X4 => { panic!("todo: vbroadcastf32x4"); },
        VBROADCASTF64X2 => { panic!("todo: vbroadcastf64x2"); },
        VBROADCASTF32X2 => { panic!("todo: vbroadcastf32x2"); },
        VBROADCASTI32X8 => { panic!("todo: vbroadcasti32x8"); },
        VBROADCASTI64X4 => { panic!("todo: vbroadcasti64x4"); },
        VBROADCASTI32X4 => { panic!("todo: vbroadcasti32x4"); },
        VBROADCASTI64X2 => { panic!("todo: vbroadcasti64x2"); },
        VBROADCASTI32X2 => { panic!("todo: vbroadcasti32x2"); },
        VEXTRACTI32X8 => { panic!("todo: vextracti32x8"); },
        VEXTRACTF32X8 => { panic!("todo: vextractf32x8"); },
        VINSERTI32X8 => { panic!("todo: vinserti32x8"); },
        VINSERTF32X8 => { panic!("todo: vinsertf32x8"); },
        VINSERTI32X4 => { panic!("todo: vinserti32x4"); },
        V4FNMADDSS => { panic!("todo: v4fnmaddss"); },
        V4FNMADDPS => { panic!("todo: v4fnmaddps"); },
        VCVTNEPS2BF16 => { panic!("todo: vcvtneps2bf16"); },
        V4FMADDSS => { panic!("todo: v4fmaddss"); },
        V4FMADDPS => { panic!("todo: v4fmaddps"); },
        VCVTNE2PS2BF16 => { panic!("todo: vcvtne2ps2bf16"); },
        VP2INTERSECTD => { panic!("todo: vp2intersectd"); },
        VP2INTERSECTQ => { panic!("todo: vp2intersectq"); },
        VP4DPWSSDS => { panic!("todo: vp4dpwssds"); },
        VP4DPWSSD => { panic!("todo: vp4dpwssd"); },
        VPDPWSSDS => { panic!("todo: vpdpwssds"); },
        VPDPWSSD => { panic!("todo: vpdpwssd"); },
        VPDPBUSDS => { panic!("todo: vpdpbusds"); },
        VDPBF16PS => { panic!("todo: vdpbf16ps"); },
        VPBROADCASTMW2D => { panic!("todo: vpbroadcastmw2d"); },
        VPBROADCASTMB2Q => { panic!("todo: vpbroadcastmb2q"); },
        VPMOVD2M => { panic!("todo: vpmovd2m"); },
        VPMOVQD => { panic!("todo: vpmovqd"); },
        VPMOVWB => { panic!("todo: vpmovwb"); },
        VPMOVDB => { panic!("todo: vpmovdb"); },
        VPMOVDW => { panic!("todo: vpmovdw"); },
        VPMOVQB => { panic!("todo: vpmovqb"); },
        VPMOVQW => { panic!("todo: vpmovqw"); },
        VGF2P8MULB => { panic!("todo: vgf2p8mulb"); },
        VPMADD52HUQ => { panic!("todo: vpmadd52huq"); },
        VPMADD52LUQ => { panic!("todo: vpmadd52luq"); },
        VPSHUFBITQMB => { panic!("todo: vpshufbitqmb"); },
        VPERMB => { panic!("todo: vpermb"); },
        VPEXPANDD => { panic!("todo: vpexpandd"); },
        VPEXPANDQ => { panic!("todo: vpexpandq"); },
        VPABSQ => { panic!("todo: vpabsq"); },
        VPRORVD => { panic!("todo: vprorvd"); },
        VPRORVQ => { panic!("todo: vprorvq"); },
        VPMULTISHIFTQB => { panic!("todo: vpmultishiftqb"); },
        VPERMT2B => { panic!("todo: vpermt2b"); },
        VPERMT2W => { panic!("todo: vpermt2w"); },
        VPSHRDVQ => { panic!("todo: vpshrdvq"); },
        VPSHRDVD => { panic!("todo: vpshrdvd"); },
        VPSHRDVW => { panic!("todo: vpshrdvw"); },
        VPSHLDVQ => { panic!("todo: vpshldvq"); },
        VPSHLDVD => { panic!("todo: vpshldvd"); },
        VPSHLDVW => { panic!("todo: vpshldvw"); },
        VPCOMPRESSB => { panic!("todo: vpcompressb"); },
        VPCOMPRESSW => { panic!("todo: vpcompressw"); },
        VPEXPANDB => { panic!("todo: vpexpandb"); },
        VPEXPANDW => { panic!("todo: vpexpandw"); },
        VPOPCNTD => { panic!("todo: vpopcntd"); },
        VPOPCNTQ => { panic!("todo: vpopcntq"); },
        VPOPCNTB => { panic!("todo: vpopcntb"); },
        VPOPCNTW => { panic!("todo: vpopcntw"); },
        VSCALEFSS => { panic!("todo: vscalefss"); },
        VSCALEFSD => { panic!("todo: vscalefsd"); },
        VSCALEFPS => { panic!("todo: vscalefps"); },
        VSCALEFPD => { panic!("todo: vscalefpd"); },
        VPDPBUSD => { panic!("todo: vpdpbusd"); },
        VCVTUSI2SD => { panic!("todo: vcvtusi2sd"); },
        VCVTUSI2SS => { panic!("todo: vcvtusi2ss"); },
        VPXORD => { panic!("todo: vpxord"); },
        VPXORQ => { panic!("todo: vpxorq"); },
        VPORD => { panic!("todo: vpord"); },
        VPORQ => { panic!("todo: vporq"); },
        VPANDND => { panic!("todo: vpandnd"); },
        VPANDNQ => { panic!("todo: vpandnq"); },
        VPANDD => { panic!("todo: vpandd"); },
        VPANDQ => { panic!("todo: vpandq"); },

        PSMASH => { panic!("todo: psmash"); },
        PVALIDATE => { panic!("todo: pvalidate"); },
        RMPADJUST => { panic!("todo: rmpadjust"); },
        RMPUPDATE => { panic!("todo: rmpupdate"); },

    };
    Some(behavior)
}
