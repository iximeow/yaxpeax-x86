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
        let mut behavior = opcode2behavior(&self.opcode);

        if behavior.is_nontrivial() {
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
                behavior = behavior
                    .set_implicit_ops(ops_idx);
            } else if self.opcode == Opcode::IMUL {
                if self.operand_count == 2 {
                    behavior = behavior
                        .set_operand(0, Access::ReadWrite)
                        .set_operand(1, Access::Read);
                } else if self.operand_count == 3 {
                    behavior = behavior
                        .set_operand(0, Access::ReadWrite)
                        .set_operand(1, Access::Read)
                        .set_operand(2, Access::Read);
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
                behavior = behavior
                    .set_implicit_ops(ops_idx);
            } else if self.opcode == Opcode::NOP {
                if self.operand_count == 1 {
                    behavior = behavior
                        .set_operand(0, Access::None);
                }
            } else if self.opcode == Opcode::CMPXCHG {
                let op_width = if self.operands[0] == OperandSpec::RegMMM {
                    self.regs[1].width()
                } else {
                    self.mem_size
                };
                let ops_idx = match op_width {
                    1 => CMPXCHG_IDX_BYTE,
                    2 => CMPXCHG_IDX_WORD,
                    4 => CMPXCHG_IDX_DWORD,
                    _ /* 8 */ => CMPXCHG_IDX_QWORD,
                };
                behavior = behavior
                    .set_implicit_ops(ops_idx);
            } else if self.opcode == Opcode::VMOVLPS || self.opcode == Opcode::VMOVHPS ||
                      self.opcode == Opcode::VMOVLPD || self.opcode == Opcode::VMOVHPD {
                if self.operand_count == 2 {
                    behavior = behavior
                        .set_operand(0, Access::ReadWrite);
                } else {
                    behavior = behavior
                        .set_operand(0, Access::Write)
                        .set_operand(2, Access::Read);
                }
            } else if self.opcode() == Opcode::EXTRQ {
                if self.operand_count > 2 {
                    behavior = behavior
                        .set_operand(2, Access::Read);
                }
            } else if self.opcode() == Opcode::INSERTQ {
                if self.operand_count > 2 {
                    behavior = behavior
                        .set_operand(2, Access::Read)
                        .set_operand(3, Access::Read);
                }
            } else if self.opcode() == Opcode::RETURN {
                if self.operand_count != 0 {
                    behavior = behavior
                        .set_operand(0, Access::Read);
                }
            } else if self.opcode() == Opcode::RETF {
                if self.operand_count != 0 {
                    behavior = behavior
                        .set_operand(0, Access::Read);
                }
            } else if self.opcode() == Opcode::MULX {
                // `mulx` is always vex-encoded.
                if self.prefixes.vex_unchecked().w() {
                    behavior = behavior
                        .set_implicit_ops(MULX_64B_IDX);
                } else {
                    behavior = behavior
                        .set_implicit_ops(MULX_32B_IDX);
                }
            } else if self.opcode() == Opcode::VMASKMOVDQU {
                // in 64-bit mode, 67 overrides 64-bit addressing down to 32-bit.
                if self.prefixes.address_size() {
                    behavior = behavior
                        .set_implicit_ops(EDI_MEMWRITE_IDX);
                } else {
                    behavior = behavior
                        .set_implicit_ops(RDI_MEMWRITE_IDX);
                }
            } else if self.opcode() == Opcode::PCMPESTRI || self.opcode() == Opcode::VPCMPESTRI {
                if self.prefixes.vex_unchecked().w() {
                    behavior = behavior
                        .set_implicit_ops(PCMPESTRI_64B_IDX);
                } else {
                    behavior = behavior
                        .set_implicit_ops(PCMPESTRI_32B_IDX);
                }
            } else if self.opcode() == Opcode::PCMPESTRM || self.opcode() == Opcode::VPCMPESTRM {
                if self.prefixes.vex_unchecked().w() {
                    behavior = behavior
                        .set_implicit_ops(PCMPESTRM_64B_IDX);
                } else {
                    behavior = behavior
                        .set_implicit_ops(PCMPESTRM_32B_IDX);
                }
            } else if self.opcode() == Opcode::LOOPNZ
                || self.opcode() == Opcode::LOOPZ
                || self.opcode() == Opcode::LOOP
                || self.opcode() == Opcode::JRCXZ {
               if self.prefixes.rex_unchecked().w() {
                   behavior = behavior
                       .set_implicit_ops(RW_RCX_IDX);
               } else if self.prefixes.operand_size() {
                   behavior = behavior
                       .set_implicit_ops(RW_CX_IDX);
               } else {
                   behavior = behavior
                       .set_implicit_ops(RW_ECX_IDX);
               }
            } else {
                // there should never be an unhandled nontrivial opcode, but leave this in so
                // fuzzing and testing can make sure. this way in normal builds the branch is empty
                // and compilers can forget all about it for us.
                #[cfg(feature = "_debug_internal_asserts")]
                unreachable!();
            }
        }

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
    implicit_ops: &'static [ImplicitOperand],
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
                    if acc != Access::None {
                        return Some((Operand::Register { reg: RegSpec::rflags() }, acc));
                    }
                }
            } else {
                let implicit_idx = self.next - 1;
                self.next += 1;

                if let Some(entry) = self.operands.implicit_ops.get(implicit_idx as usize) {
                    let access = if entry.write {
                        Access::Write
                    } else {
                        Access::Read
                    };
                    return Some((entry.into_operand(), access));
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
            debug_assert!(
                access != Access::None || (
                    self.operands.inst.inst.opcode == Opcode::NOP ||
                    self.operands.inst.inst.opcode == Opcode::UD1 ||
                    self.operands.inst.inst.opcode == Opcode::UD0
                )
            );
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
#[derive(Copy, Clone)]
pub struct ImplicitOperand {
    // TODO: not suitable for public API!
    spec: OperandSpec,
    reg: RegSpec,
    disp: i32,
    write: bool,
}

impl ImplicitOperand {
    fn into_operand(self) -> Operand {
        match self.spec {
            OperandSpec::RegRRR => {
                Operand::Register { reg: self.reg }
            },
            OperandSpec::Deref => {
                Operand::MemDeref { base: self.reg }
            }
            OperandSpec::Disp => {
                Operand::Disp { base: self.reg, disp: self.disp }
            }
            OperandSpec::Deref_rdi => {
                Operand::MemDeref { base: RegSpec::rdi() }
            }
            // from `xlat` specifically... `base` specifies rbx, infer ax as the index here.
            OperandSpec::MemIndexScale => {
                Operand::MemBaseIndexScale {
                    base: self.reg,
                    index: RegSpec::al(),
                    scale: 1
                }
            }
            spec => {
                panic!("unexpected implicit op: {:?}", spec);
            }
        }
    }
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

    /// 
    #[cfg(feature = "unstable")]
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

    fn as_complex_op(&self) -> Option<ComplexOp> {
        // if the behavior is not complex, it is *definitely* not complex. if the behavior is
        // complex, it's really a "depending on the specific instruction and operands it might
        // be"...
        if !self.behavior.is_complex() {
            return None;
        }

        // TODO: all of these should be a `set_complex` bit.
        if self.inst.opcode == Opcode::BT {
            if self.inst.operands[0] != OperandSpec::RegMMM {
                Some(ComplexOp::BT)
            } else {
                None
            }
        } else if self.inst.opcode == Opcode::BTS {
            if self.inst.operands[0] != OperandSpec::RegMMM {
                Some(ComplexOp::BTS)
            } else {
                None
            }
        } else if self.inst.opcode == Opcode::BTR {
            if self.inst.operands[0] != OperandSpec::RegMMM {
                Some(ComplexOp::BTR)
            } else {
                None
            }
        } else if self.inst.opcode == Opcode::BTC {
            if self.inst.operands[0] != OperandSpec::RegMMM {
                Some(ComplexOp::BTC)
            } else {
                None
            }
        } else {
            // Safety: every `Opcode` with a `BehaviorDigest` that is `set_complex(true)` has a
            // corresponding `ComplexOp` variant set to the same integer value, and the two types
            // agree on repr.
            let comp: ComplexOp = unsafe { core::mem::transmute::<Opcode, ComplexOp>(self.inst.opcode) };
            Some(comp)
        }
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
        if let Some(op) = self.as_complex_op() {
            return Err(op);
        }

        let implicit_ops = if let Some(ops) = self.implicit_oplist() {
            ops
        } else {
            &[]
        };

        Ok(InstOperands {
            inst: *self,
            implicit_ops,
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
        if let Some(op) = self.as_complex_op() {
            return Err(op);
        }

        fn compute_addr<T: AccessVisitor>(v: &mut T, inst: &Instruction, op_spec: OperandSpec) -> Option<u64> {
            // TODO: test assertions feature?
            if !op_spec.is_memory() {
                panic!("expected memory operand but got {:?}", op_spec);
            }

            match op_spec {
                OperandSpec::Deref |
                OperandSpec::Deref_mask => {
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
                OperandSpec::Disp => {
                    let base = v.get_register(inst.regs[1]);
                    base.map(|addr| addr.wrapping_add(inst.disp as i32 as i64 as u64))
                }
                OperandSpec::Disp_mask => {
                    let base = v.get_register(inst.regs[1]);
                    base.map(|addr| addr.wrapping_add(inst.disp as i32 as i64 as u64))
                }
                OperandSpec::MemIndexScale => {
                    let index = v.get_register(inst.regs[2]);
                    index.map(|addr| {
                        addr
                            .wrapping_mul(inst.scale as u64)
                    })
                }
                OperandSpec::MemIndexScale_mask => {
                    let index = v.get_register(inst.regs[2]);
                    index.map(|addr| {
                        addr
                            .wrapping_mul(inst.scale as u64)
                    })
                }
                OperandSpec::MemIndexScaleDisp => {
                    let index = v.get_register(inst.regs[2]);
                    index.map(|addr| {
                        addr
                            .wrapping_mul(inst.scale as u64)
                            .wrapping_add(inst.disp as i32 as i64 as u64)
                    })
                }
                OperandSpec::MemIndexScaleDisp_mask => {
                    let index = v.get_register(inst.regs[2]);
                    index.map(|addr| {
                        addr
                            .wrapping_mul(inst.scale as u64)
                            .wrapping_add(inst.disp as i32 as i64 as u64)
                    })
                }
                OperandSpec::MemBaseIndexScale => {
                    let base = v.get_register(inst.regs[1]);
                    let index = v.get_register(inst.regs[2]);
                    base.and_then(|base| {
                        index.map(|index| {
                            base
                                .wrapping_add(index.wrapping_mul(inst.scale as u64))
                        })
                    })
                }
                OperandSpec::MemBaseIndexScale_mask => {
                    let base = v.get_register(inst.regs[1]);
                    let index = v.get_register(inst.regs[2]);
                    base.and_then(|base| {
                        index.map(|index| {
                            base
                                .wrapping_add(index.wrapping_mul(inst.scale as u64))
                        })
                    })
                }
                OperandSpec::MemBaseIndexScaleDisp => {
                    let base = v.get_register(inst.regs[1]);
                    let index = v.get_register(inst.regs[2]);
                    base.and_then(|base| {
                        index.map(|index| {
                            base
                                .wrapping_add(index.wrapping_mul(inst.scale as u64))
                                .wrapping_add(inst.disp as i32 as i64 as u64)
                        })
                    })
                }
                OperandSpec::MemBaseIndexScaleDisp_mask => {
                    let base = v.get_register(inst.regs[1]);
                    let index = v.get_register(inst.regs[2]);
                    base.and_then(|base| {
                        index.map(|index| {
                            base
                                .wrapping_add(index.wrapping_mul(inst.scale as u64))
                                .wrapping_add(inst.disp as i32 as i64 as u64)
                        })
                    })
                }
                OperandSpec::DispU64 => {
                    Some(inst.disp)
                }
                OperandSpec::DispU32 => {
                    Some(inst.disp as u32 as u64)
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
                    OperandSpec::Reg4 => {
                        let spec = RegSpec {
                            num: self.inst.imm as u8,
                            bank: self.inst.regs[3].bank,
                        };
                        v.register_read(spec);
                    }
                    OperandSpec::RegRRR_maskmerge |
                    OperandSpec::RegRRR_maskmerge_sae |
                    OperandSpec::RegRRR_maskmerge_sae_noround => {
                        v.register_read(self.inst.regs[0]);
                        if self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
                    }
                    OperandSpec::RegMMM_maskmerge |
                    OperandSpec::RegMMM_maskmerge_sae_noround => {
                        v.register_read(self.inst.regs[1]);
                        if self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
                    }
                    OperandSpec::RegVex_maskmerge => {
                        v.register_read(self.inst.regs[3]);
                        if self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
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
                    other => {
                        // compute effective address...
                        let addr = compute_addr(v, &self.inst, op_spec);
                        let size = self.inst.mem_size().expect("memory operand implies memory access size")
                            .bytes_size().expect("non-complex instructions have well-defined bytes_size()");
                        if other.is_masked() && self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
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
                    OperandSpec::Reg4 => {
                        let spec = RegSpec {
                            num: self.inst.imm as u8,
                            bank: self.inst.regs[3].bank,
                        };
                        v.register_write(apply_x86_zext(spec));
                    }
                    OperandSpec::RegRRR_maskmerge |
                    OperandSpec::RegRRR_maskmerge_sae |
                    OperandSpec::RegRRR_maskmerge_sae_noround => {
                        if self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
                        v.register_write(self.inst.regs[0]);
                    }
                    OperandSpec::RegMMM_maskmerge |
                    OperandSpec::RegMMM_maskmerge_sae_noround => {
                        if self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
                        v.register_write(self.inst.regs[1]);
                    }
                    OperandSpec::RegVex_maskmerge => {
                        if self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
                        v.register_write(self.inst.regs[3]);
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
                    other => {
                        // compute effective address...
                        let addr = compute_addr(v, &self.inst, op_spec);
                        let size = self.inst.mem_size().expect("memory operand implies memory access size")
                            .bytes_size().expect("non-complex instructions have well-defined bytes_size()");
                        if other.is_masked() && self.inst.prefixes.evex_unchecked().mask_reg() != 0 {
                            v.register_read(RegSpec::mask(self.inst.prefixes.evex_unchecked().mask_reg()));
                        }
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

/// a description of how an operand is used.
///
/// `Access::ReadWrite` can be processed in the same manner as that operand listed as
/// `Access::Read` followed by that same operand listed as `Access::Write`.
///
/// **important**: the meaning of `Access` is different for `flags`/`eflags`/`rflags` than other
/// operands! these differences are documented on enum variants below.
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
    ///
    /// for flags/eflags/rflags, "write" refers to some subset of flag bits as appropriate for the
    /// instruction, and implies that the instruction does not depend on the initial state of those
    /// bits. this is in contrast to `Write` for other operands, where it implies a full write of
    /// the corresponding operand. as a concrete example, `add` reports the flags register as a
    /// `Write` since the resulting flag bits are purely a function of the `add` register/memory
    /// operands.
    Write = 0b10,
    /// the corresponding operand is read and written.
    ///
    /// in some cases `Access::ReadWrite` is chosen in particular to represent a parital-write;
    /// this is especially true with SIMD instructions as `yaxpeax-x86` does not currently have the
    /// ability to express individual SIMD lane read/write operations. the `vmov{h,l}{ps,pd}`
    /// instructions are more common examples of this access form. this kind of partial-write
    /// access is reported as `Access::Write` for flags/eflags/rflags.
    ///
    /// for flags/eflags/rflags, "read-write" refers to some subset of flag bits as appropriate for the
    /// instruction, and implies that the instruction does depends on the initial state of those
    /// bits as well as modifying some (possibly different) bits in flags as a result.
    /// as a concrete example, `adc` reports the flags register as a `ReadWrite` because the
    /// initial state of `cf` is an input to the addition, and the normal arithmetic flags are
    /// written based on the result.
    ///
    /// for memory operands, this describes the referenced memory; implicitly the registers used in
    /// the operand's address calculation are also read.
    ReadWrite = 0b11,
    /// the corresponding operand is not actually accessed for reading or writing.
    ///
    /// this is only used to describe the operand of `nop` or `ud1` instructions.
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

    pub fn is_read(&self) -> bool {
        *self as u8 & 0b01 != 0
    }

    pub fn is_write(&self) -> bool {
        *self as u8 & 0b10 != 0
    }
}

#[derive(Copy, Clone)]
pub struct BehaviorDigest {
    // laid out like:
    //
    // |7 6|5 4|3 2|1 0|
    // |_ _ C N|FL |PL |
    //
    // C: complex (this instruction *may*  N: non-trivial (implementation detail of constructing
    // the instruction's behavior digest)
    // N: non-trivial (implementation detail of constructing the instruction's behavior digest)
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
    // selector for a `&'static [Operand]` of additional "implicit" operands for the instruction.
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

    const fn set_nontrivial(mut self, state: bool) -> Self {
        self.behavior &= 0b11_10_11_11;
        self.behavior |= (state as u8) << 4;
        self
    }

    const fn is_nontrivial(&self) -> bool {
        (self.behavior & (1 << 4)) != 0
    }

    const fn set_complex(mut self, state: bool) -> Self {
        self.behavior &= 0b11_01_11_11;
        self.behavior |= (state as u8) << 5;
        self
    }

    const fn is_complex(&self) -> bool {
        (self.behavior & (1 << 5)) != 0
    }
}

/// a subset of [`Opcode`] where access patterns cannot be expressed as a simple stream of reads or
/// writes. in many cases these instructions are only executable when `CPL=0` but notable
/// exceptions are `rep`-prefixed string instructions (`rep movs`, `rep stos`, etc).
///
/// complex instructions and appropriate handling are documented on a best-effort basis below.
///
/// ### `rdmsr`, `wrmsr`, and all other instructions that directly read or write MSRs
///
/// the library API has no way to express MSRs as read or written locations, and must defer to user
/// code to track reads or writes of this state.
///
/// "other instructions" include `wrfsbase`, `wrgsbase`, `rdfsbase`, `rdgsbase`, `syscall`,
/// `sysenter`, `rdpru`, `rdtsc`, and others.
///
/// ### the `xsave` family
///
/// this section applies for all of `xsave`, `xsaveopt`, `xsavec`, `xsavec64`, `xsaves`,
/// `xsaves64`, `xrstor`, `xrstors`, `xrstors64`, as well as many other instructions operating on
/// bulk processor state and `mxcsr`.
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
/// other related instructions, like `fnstenv`, `frstor`, and others, simply save and restore
/// architectural state that is not expressed in the library API and cannot be included in implicit
/// operand lists (such as `mxcsr`).
///
/// ### `in`, `out`, including rep-prefixed forms
///
/// port I/O instructions use a register or immedate to select an I/O port, meaning the literal
/// operand and architectural operation are totally distinct. the library API does not currently
/// have an operand form for I/O ports, so these instructions are "complex".
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
/// additionally, the vector scatter/gather prefetch instructions
/// `v{gather,scatter}pf0{dps,dpd,qps,qpd}` are complex in part for these reasons.
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
/// the vector scatter/gather instructions additionally are complex for the same reasons as
/// prefetch instructions described below.
///
/// ### `monitor`, `monitorx`, `mwait`, `mwaitx`
///
/// these instructions reference memory but neither read nor write it. instead, `monitor` primes
/// hardware to watch for accesses to the specified address, while `mwait` waits for an access to
/// some earlier `monitor`-primed adddress. this address-monitoring hardware is not expressed in
/// the library API and makes this family of instructions "complex" due to reading or writing
/// unrepresented state.
///
/// arguably `monitor` could be described as a load; it sets the A-bit in page tables, is ordered
/// as a load, and is subject to the permission checking associated with a byte load.
///
/// ### `syscall/sysret`, `sysenter/sysexit`
///
/// these instructions are considered "complex" because they include implicit reads and possibly
/// writes to various MSRs. further, depending on dynamic processor state (i.e. "is FRED enabled")
/// these instructions may behave quite differently than a "normal" shuffling of
/// `rip`/`rflags`/`cs`.
///
/// ### `vmread`, `vmwrite`, `vmrun`, `vmsave`, `vmload`, and SVM/VMX generally
///
/// for instructions that *have* an operand, their operand's semantics differs substantially from a
/// "normal" understanding of the literal operand.
///
/// for `vmread` and `vmwrite`, the memory operand may be `[rax]`, but it is implicitly an access
/// to the current VMCS - and, indeed, not even an access to "memory".
///
/// for `vmrun`, `vmsave`, and `vmload`, the operand is "`rax`", but expects `rax` to carry a
/// physical address to a VMCB which is then loaded from or stored into.
///
/// generally, SVM/VMX instructions operate on a hidden VMCB/VMCS structure and are "complex" for
/// interacting with architectural state that is not expressed in library APIs.
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
/// ### `clzero`, `clflush`, `clflushopt`, `clwb`
///
/// these instructions are "complex" because the amount of memory that is operated on is
/// processor-dependent and the accessed address is *not* simply the effective address of the
/// memory operand.
///
/// the size of an x86 cache line is _typically_ 64 bytes, but is reported per-processor in CPUID
/// information (leaf `eax=1`: `clflush line size`, AMD leaf `eax=8000_0005`: `cache line size`).
///
/// some x86 processors have had 32-byte cache lines.
///
/// `clflush`, `clflushopt`, and `clwb` are closer to a no-op in terms of architectural state. they
/// are included as "complex" for the reasons above and in support of library uses which want to
/// precisely model memory, such as in modeling the execution of multi-processor systems.
///
/// ### `bts`, `btc`, `bt`
///
/// these instructions are *conditionally* "complex". when the destination is a memory operand they
/// are complex because the effective address of the modified word/dword/qword is a function of
/// both operands of the instruction.
///
/// in particular, the accessed location is the word/dword/qword at the first operand's effective
/// address *plus* the second operand divided by the access size. as a worked example with a dword
/// access:
/// ```text
/// rax := 0x1_0000_0100
/// rcx := 0x203
///
/// // bts dword [rax], ecx
/// ptr = rax + (rcx / 32)  ; 0x1_0000_0303
/// bit = rcx % 32          ; 3
/// cf := (*ptr >> bit) & 1
/// *ptr |= (1 << bit)
///
/// this was very dismaying to learn! the library API has no hope of expressing this! but the fact
/// that the test harness detected this is strong evidence it works...
/// ```
///
/// ### `enqcmd`, `enqcmds`
///
/// these instructions use "enqueue stores" to write to what are expected to be "enqueue registers"
/// via MMIO. additionally, i do not have hardware to test these against "normal" memory, so these
/// are "complex" out of caution.
///
/// ### CET-related instructions (`wrss`, `incssp`, `clrssbsy`, etc)
///
/// CET-related instructions manipulate shadow stack state, which is a kind-of-hidden architectural
/// state that is not expressed in the library API. these instructions are considered "complex" due
/// to reading or writing that state.
///
/// ### `ltr`, `str`, `lldt`, `sldt`, `lidt`, `sidt`, `lgdt`, `sgdt`, (AMD: `clgi`, `stgi`)
///
/// these instructions all directly manage architectural state which is not expressed in the
/// library API.
///
/// ### `xgetbv`/`xsetbv`
///
/// these instructions operate on `xcr*` registers (namely, `xcr0`), which is not currently
/// expressible in the library API, so these are considered "complex".
///
/// ### `v4f*madd`
///
/// `v4f*` family multiply-add instructions operate on ranges of registers that are not (currently)
/// expressed precisely in the library API; the {x,y,z}mm register set these operate on is obtained by
/// "mask the low two bits of the SIMD register, the result is the base of and the next three are
/// the rest of the bank". this *could* be expressed in the library API but seems like it would be
/// awkward. the instructions seem uncommon, so they are "complex" for expediency.
///
/// ### `movdir64b`
///
/// movdir64b is considered complex primarily because it has two memory operands, but the
/// destination operand (first, in Intel syntax) is expressly *not* a memory operand so far as
/// syntax is concerned.
///
/// ### `hreset`
///
/// `hreset` manages microarchitectural processor history, but is considered "complex" somewhat
/// arbitrarily as its sole responsibility is to operate on state that is not expressed in the
/// library API.
///
/// ### `psmash`, `pvalidate`, `rmpadjust`, `rmpupdate`
///
/// `psmash`-related instructions depend on architectural state which is described in more depth
/// above, but not currently expressed in the library API, so they are "complex".
///
/// ### `ptwrite`
///
/// `ptwrite` modifies processor state that is not expressed in the library API currently, so it
/// is "complex".
///
/// ### Restricted Transactional Memory (RTM)
///
/// `xbegin`, `xend`, `xtest`, and `xabort` are all "complex" because the RTM instructions relate
/// to architectural state for memory transactions which are not expressed in the library API.
/// additionally, these instructions have consequences for control flow that are not easily
/// expressed in the library API.
///
/// ### `pconfig`
///
/// `pconfig` is "complex" because it alters architectural state and has complex semantics. the
/// instruction is similar to `getsec` or `cpuid` in intended breadth and like `cpuid` could
/// perhaps be made non-complex on the expectation that library users interested in *this
/// instruction* would look for the opcode instead.
///
/// out of caution, and because this is a CPL=0-only relatively-rare instruction, this is still
/// "complex".
///
/// ### `bndldx`, `bndstx`
///
/// these MPX instructions are "complex" because the interpretation of their operands differs
/// substantially from the typical meaning, and they interact with architectural state (bounds
/// table entries, "BTEs") that is not expressed in the library API.
///
/// ### `iret`, `iretd`, `iretq`
///
/// interrupt return instructions are considered "complex" purely for their semantics being, well,
/// complex. they interact with the current execution mode, privilege level, requested privilege
/// level of returned-to segments, and shadow stacks.
///
/// most architectural state they interact with is expressed in the library API. these are
/// difficult to consider "complex" by the general guidelines above. in truth, they are complex
/// mostly because they are uncommon, typically executed at CPL=0, and more difficult to
/// comprehensively test. these may stop being considered complex in a future release.
///
/// ### OSPKE
///
/// `rdpkru` and `wrpkru` are considered complex because these instructions operate on the `pkru`
/// register, which is not expressed in the library API today. these may stop being considered
/// complex in a future release, at which point `pkru` would be an implicit operand as appropriate.
///
/// ### `rsm`
///
/// this instruction is considered complex for a few related reasons:
///
/// * yours truly does not really know much about SMM at all, so it's not clear if there are
///   architectural state gotchas involved in transitioning to/from SMM
/// * yours truly is not sure how much state is covered by the processor state save/restore on SMM
///   transition, and has no way to validate if any implicit operand list describing the
///   reads/writes is correct.
///
/// you know how to test SMM transitions and returns, please write me!
///
/// ### WAITPKG
///
/// `tpause`, `umonitor`, and `umwait` are considered complex for different reasons:
///
/// * `umonitor` and `umwait` are complex in similar ways to `monitor` and `mwait`.
/// * `tpause` is considered "complex" because the implicit operands are compared with the TSC; one
///   might imagine the library would report an implicit read of the TSC MSR, but there is no
///   library API to describe MSR accesses yet.
///
/// ### UINTR
///
/// UINTR-related instructions are considered complex for varied reasons:
///
/// * `stui`, `clui`, `testui`: these instructions manipulate a bit in `rflags` and probably do not
///   need to be complex (similar to `sti`, `cli`). these may lose their "complex" status in a future
///   release.
/// * `senduipi`: this instruction is "complex" because the user-IPI mechanism involves the
///   user-interrupt target table (UITT) and referenced user posted-interrupt descriptor (UPID).
/// * `uiret`: this instruction is only "complex" because it is considered uncommon (for now?),
///   this author has no hardware to test it on, and it's not immediately clear how this relates to
///   a corresponding UPID (if i've even read the documentation correctly!)
///
/// ### TDX
///
/// TDX-related instructions are considered complex because they are not more precisely tested and
/// are assumed as-complex-as-VMX in the first place.
#[non_exhaustive]
#[repr(u32)] // same repr as `Opcode`
#[derive(Copy, Clone, Debug)]
pub enum ComplexOp {
    /// rdmsr/wrmsr are considered "complex" for reasons in the enum doc comment.
    RDMSR = (Opcode::RDMSR as u32),
    WRMSR = (Opcode::WRMSR as u32),

    /// `rdtsc` and `rdtscp` read MSRs and can be modeled as a special form of `rdmsr`; they are
    /// "complex" in the same way.
    RDTSC = (Opcode::RDTSC as u32),
    RDTSCP = (Opcode::RDTSCP as u32),

    /// `rdpru` reads MSRs and can be modeled as a special form of `rdmsr`; it is "complex" in the
    /// same way.
    RDPRU = (Opcode::RDPRU as u32),

    /// instructions interacting with MSRs, such as these (`IA32_FS_BASE`, `IA32_GS_BASE`,
    /// `IA32_KERNEL_GS_BASE`) are complex for the moment.
    SWAPGS = (Opcode::SWAPGS as u32),
    RDFSBASE = (Opcode::RDFSBASE as u32),
    WRFSBASE = (Opcode::WRFSBASE as u32),
    RDGSBASE = (Opcode::RDGSBASE as u32),
    WRGSBASE = (Opcode::WRGSBASE as u32),

    /// the bulk processor state save/restore instructions, as well as `mxcsr`-related
    /// instructions, are considered complex for reasons described under `fxsave` in the enum doc
    /// comment above.
    FRSTOR = (Opcode::FRSTOR as u32),
    FLDENV = (Opcode::FLDENV as u32),
    FNSTENV = (Opcode::FNSTENV as u32),
    FNSAVE = (Opcode::FNSAVE as u32),
    FNSTCW = (Opcode::FNSTCW as u32),
    FNSTSW = (Opcode::FNSTSW as u32),
    FXSAVE = (Opcode::FXSAVE as u32),
    FXRSTOR = (Opcode::FXRSTOR as u32),
    LDMXCSR = (Opcode::LDMXCSR as u32),
    VLDMXCSR = (Opcode::VLDMXCSR as u32),
    STMXCSR = (Opcode::STMXCSR as u32),
    VSTMXCSR = (Opcode::VSTMXCSR as u32),
    XSAVE = (Opcode::XSAVE as u32),
    XSAVEC = (Opcode::XSAVEC as u32),
    XSAVES = (Opcode::XSAVES as u32),
    XSAVEC64 = (Opcode::XSAVEC64 as u32),
    XSAVES64 = (Opcode::XSAVES64 as u32),
    XRSTOR = (Opcode::XRSTOR as u32),
    XRSTORS = (Opcode::XRSTORS as u32),
    XRSTORS64 = (Opcode::XRSTORS64 as u32),
    XSAVEOPT = (Opcode::XSAVEOPT as u32),

    /// in/out are considered "complex" for reasons in the enum doc comment.
    IN = (Opcode::IN as u32),
    OUT = (Opcode::OUT as u32),

    /// string instructions are considered "complex" for reasons in the enum doc comment.
    MOVS = (Opcode::MOVS as u32),
    STOS = (Opcode::STOS as u32),
    LODS = (Opcode::LODS as u32),
    SCAS = (Opcode::SCAS as u32),
    CMPS = (Opcode::CMPS as u32),

    /// scatter/gather instructions are considered "complex" for reasons in the enum doc comment.
    VPGATHERDD = (Opcode::VPGATHERDD as u32),
    VPGATHERDQ = (Opcode::VPGATHERDQ as u32),
    VPGATHERQD = (Opcode::VPGATHERQD as u32),
    VPGATHERQQ = (Opcode::VPGATHERQQ as u32),
    VGATHERDPD = (Opcode::VGATHERDPD as u32),
    VGATHERDPS = (Opcode::VGATHERDPS as u32),
    VGATHERQPD = (Opcode::VGATHERQPD as u32),
    VGATHERQPS = (Opcode::VGATHERQPS as u32),

    VPSCATTERDD = (Opcode::VPSCATTERDD as u32),
    VPSCATTERDQ = (Opcode::VPSCATTERDQ as u32),
    VPSCATTERQD = (Opcode::VPSCATTERQD as u32),
    VPSCATTERQQ = (Opcode::VPSCATTERQQ as u32),

    /// monitor/mwait instructions are considered "complex" for reasons in the enum doc comment.
    MONITOR = (Opcode::MONITOR as u32),
    MONITORX = (Opcode::MONITORX as u32),
    MWAIT = (Opcode::MWAIT as u32),
    MWAITX = (Opcode::MWAITX as u32),

    /// the syscall/systenter and sysexit/sysret instructions are considered complex because of
    /// their interaction with architectural state that is not expressible purely as register or
    /// memory accesses.
    SYSCALL = (Opcode::SYSCALL as u32),
    SYSRET = (Opcode::SYSRET as u32),
    SYSENTER = (Opcode::SYSENTER as u32),
    SYSEXIT = (Opcode::SYSEXIT as u32),

    /// SVM instructions generally are considered "complex" for reasons in the doc comment above.
    SKINIT = (Opcode::SKINIT as u32),
    VMLOAD = (Opcode::VMLOAD as u32),
    VMMCALL = (Opcode::VMMCALL as u32),
    VMSAVE = (Opcode::VMSAVE as u32),
    VMRUN = (Opcode::VMRUN as u32),
    VMPTRLD = (Opcode::VMPTRLD as u32),
    VMPTRST = (Opcode::VMPTRST as u32),

    /// VMX instructions, too, are considered "complex" for similar reasons as SVM.
    VMXON = (Opcode::VMXON as u32),
    VMXOFF = (Opcode::VMXOFF as u32),
    VMREAD = (Opcode::VMREAD as u32),
    VMWRITE = (Opcode::VMWRITE as u32),
    VMCLEAR = (Opcode::VMCLEAR as u32),
    VMCALL = (Opcode::VMCALL as u32),
    VMLAUNCH = (Opcode::VMLAUNCH as u32),
    VMRESUME = (Opcode::VMRESUME as u32),
    VMFUNC = (Opcode::VMFUNC as u32),

    /// vzeroupper/vzeroall are considered "complex" for reasons in the doc comment above.
    VZEROUPPER = (Opcode::VZEROUPPER as u32),
    VZEROALL = (Opcode::VZEROALL as u32),

    /// clzero, clflush, clflushopt, and clwb are considered "complex" for reasons in the enum doc
    /// comment.
    CLZERO = (Opcode::CLZERO as u32),
    CLFLUSH = (Opcode::CLFLUSH as u32),
    CLFLUSHOPT = (Opcode::CLFLUSHOPT as u32),
    CLWB = (Opcode::CLWB as u32),

    /// prefetch instructions are considered "complex" for reasons in the enum doc comment.
    PREFETCHNTA = (Opcode::PREFETCHNTA as u32),
    PREFETCHT2 = (Opcode::PREFETCH2 as u32),
    PREFETCHT1 = (Opcode::PREFETCH1 as u32),
    PREFETCHT0 = (Opcode::PREFETCH0 as u32),

    /// bit test/set/reset/complement instructions are conditionally complex depending on their
    /// destination operand form, as described in the enum doc comment.
    BT = (Opcode::BT as u32),
    BTC = (Opcode::BTC as u32),
    BTR = (Opcode::BTR as u32),
    BTS = (Opcode::BTS as u32),

    /// enqueue stores in an archtecturally interesting way, and write to
    /// architecturally-interesting non-memory locations, so they are "complex".
    ENQCMD = (Opcode::ENQCMD as u32),
    ENQCMDS = (Opcode::ENQCMDS as u32),

    /// shadow stacks and other CET machinery involve modifies processor state that cannot be
    /// expressed by `yaxpeax-x86` as any particular location currently, so it is "complex".
    WRUSS = (Opcode::WRUSS as u32),
    WRSS = (Opcode::WRSS as u32),
    INCSSP = (Opcode::INCSSP as u32),
    SAVEPREVSSP = (Opcode::SAVEPREVSSP as u32),
    SETSSBSY = (Opcode::SETSSBSY as u32),
    CLRSSBSY = (Opcode::CLRSSBSY as u32),
    RSTORSSP = (Opcode::RSTORSSP as u32),
    ENDBR64 = (Opcode::ENDBR64 as u32),
    ENDBR32 = (Opcode::ENDBR32 as u32),

    /// str/ldr and sldt/lldt are considered complex because of their interaction with
    /// architectural state that is not expressible purely as register or memory accesses.
    STR = (Opcode::STR as u32),
    LTR = (Opcode::LTR as u32),
    SLDT = (Opcode::SLDT as u32),
    LLDT = (Opcode::LLDT as u32),

    /// likewise, the AMD global interrupt flag (GIF) is not expressible as an architectural
    /// location by `yaxpeax-x86`, and so instructions operating on it are "complex".
    CLGI = (Opcode::CLGI as u32),
    STGI = (Opcode::STGI as u32),

    /// `xgetbv`/`xsetbv` are "complex" because the library API does not have a way to express
    /// extended control registers (xcr0 and the like).
    XGETBV = (Opcode::XGETBV as u32),
    XSETBV = (Opcode::XSETBV as u32),

    /// `v4f*` family multiply-add instructions operate on ranges of registers that are not
    /// (currently) expressed precisely in the library API
    V4FNMADDSS = (Opcode::V4FNMADDSS as u32),
    V4FNMADDPS = (Opcode::V4FNMADDPS as u32),
    V4FMADDSS = (Opcode::V4FMADDSS as u32),
    V4FMADDPS = (Opcode::V4FMADDPS as u32),

    /// movdir64b is considered complex primarily because it has two memory operands, but the
    /// destination operand (first, in Intel syntax) is expressly *not* a memory operand so far as
    /// syntax is concerned.
    MOVDIR64B = (Opcode::MOVDIR64B as u32),

    /// `hreset` manages microarchitectural processor history, but is considered "complex" somewhat
    /// arbitrarily as its sole responsibility is to operate on state that is not expressed in the
    /// library API.
    HRESET = (Opcode::HRESET as u32),

    /// `psmash`-related instructions depend on architectural state which is described in more depth
    /// above, but not currently expressed in the library API, so they are "complex".
    PSMASH = (Opcode::PSMASH as u32),
    PVALIDATE = (Opcode::PVALIDATE as u32),
    RMPADJUST = (Opcode::RMPADJUST as u32),
    RMPUPDATE = (Opcode::RMPUPDATE as u32),

    /// `ptwrite` modifies processor state that is not expressed in the library API currently, so it
    /// is "complex".
    PTWRITE = (Opcode::PTWRITE as u32),

    /// these instructions are all documented as complex for the reasons under `Restricted
    /// Transactional Memory` (RTM) above.
    XABORT = (Opcode::XABORT as u32),
    XBEGIN = (Opcode::XBEGIN as u32),
    XEND = (Opcode::XEND as u32),
    XTEST = (Opcode::XTEST as u32),

    /// `pconfig` is "complex" because it alters architectural state and has complex semantics.
    PCONFIG = (Opcode::PCONFIG as u32),

    /// some MPX-related instructions are considered complex for the reasons described in the enum
    /// doc comment above.
    BNDLDX = (Opcode::BNDLDX as u32),
    BNDSTX = (Opcode::BNDSTX as u32),

    /// `iret*` instructions are considered complex for the reasons described in the enum doc
    /// comment above.
    IRET = (Opcode::IRET as u32),
    IRETD = (Opcode::IRETD as u32),
    IRETQ = (Opcode::IRETQ as u32),

    /// enclave-related instructions are considered complex for the reasons described in the enum
    /// doc comment above.
    ENCLS = (Opcode::ENCLS as u32),
    ENCLV = (Opcode::ENCLV as u32),
    ENCLU = (Opcode::ENCLU as u32),

    /// OSPKE-related instructions are considered complex for the reasons described in the enum doc
    /// comment above.
    RDPKRU = (Opcode::RDPKRU as u32),
    WRPKRU = (Opcode::WRPKRU as u32),

    /// `rsm` is considered complex for the reasons related to SMM described in the enum doc
    /// comment above.
    RSM = (Opcode::RSM as u32),

    /// WAITPKG-related instructions are considered complex for the reasons described in the enum
    /// doc comment above.
    TPAUSE = (Opcode::TPAUSE as u32),
    UMONITOR = (Opcode::UMONITOR as u32),
    UMWAIT = (Opcode::UMWAIT as u32),

    /// UINTR-related instructions are considered complex for the reasons described in the enum
    /// doc comment above.
    UIRET = (Opcode::UIRET as u32),
    TESTUI = (Opcode::TESTUI as u32),
    CLUI = (Opcode::CLUI as u32),
    STUI = (Opcode::STUI as u32),
    SENDUIPI = (Opcode::SENDUIPI as u32),

    /// TDX-related instructions are considered complex for the reasons described in the enum
    /// doc comment above.
    TDCALL = (Opcode::TDCALL as u32),
    SEAMRET = (Opcode::SEAMRET as u32),
    SEAMOPS = (Opcode::SEAMOPS as u32),
    SEAMCALL = (Opcode::SEAMCALL as u32),

    /// vector scatter/gather prefetch instructions are considered complex for the reasons "normal"
    /// scatter/gather are complex, as well as the reasons "normal" prefetch instructions are
    /// complex.
    VGATHERPF0DPD = (Opcode::VGATHERPF0DPD as u32),
    VGATHERPF0DPS = (Opcode::VGATHERPF0DPS as u32),
    VGATHERPF0QPD = (Opcode::VGATHERPF0QPD as u32),
    VGATHERPF0QPS = (Opcode::VGATHERPF0QPS as u32),
    VGATHERPF1DPD = (Opcode::VGATHERPF1DPD as u32),
    VGATHERPF1DPS = (Opcode::VGATHERPF1DPS as u32),
    VGATHERPF1QPD = (Opcode::VGATHERPF1QPD as u32),
    VGATHERPF1QPS = (Opcode::VGATHERPF1QPS as u32),
    VSCATTERPF0DPD = (Opcode::VSCATTERPF0DPD as u32),
    VSCATTERPF0DPS = (Opcode::VSCATTERPF0DPS as u32),
    VSCATTERPF0QPD = (Opcode::VSCATTERPF0QPD as u32),
    VSCATTERPF0QPS = (Opcode::VSCATTERPF0QPS as u32),
    VSCATTERPF1DPD = (Opcode::VSCATTERPF1DPD as u32),
    VSCATTERPF1DPS = (Opcode::VSCATTERPF1DPS as u32),
    VSCATTERPF1QPD = (Opcode::VSCATTERPF1QPD as u32),
    VSCATTERPF1QPS = (Opcode::VSCATTERPF1QPS as u32),
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
    .set_operand(2, Access::Read);

/// and for vex/evex-encoded instructions with an imm8 suffix
///
/// this is not distinct from a `GENERAL_W_R_R_R`, but is named distinctly in case yaxpeax-x86
/// should report imm8 operands differently from "read" or "write".
const GENERAL_W_R_R_IMM8: BehaviorDigest = GENERAL_W_R_R
    .set_operand(3, Access::Read);

/// shld
const GENERAL_RW_R_R: BehaviorDigest = GENERAL_W_R_R
    .set_operand(0, Access::ReadWrite);

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

const GENERAL_R_FLAGWRITE: BehaviorDigest = GENERAL_R
    .set_flags_access(Access::Write);

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

static ENTER_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rsp(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rbp(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rbp(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::Disp,
        reg: RegSpec::rsp(),
        disp: -8i32,
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

static RDTSCP_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0i32,
        write: true,
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
        spec: OperandSpec::Disp,
        reg: RegSpec::rsp(),
        disp: -10i32,
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

static RETF_OPS: &'static [ImplicitOperand] = &[
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
        reg: RegSpec::cs(),
        disp: 0,
        write: true,
    },
    // pop.. pops the value (above), then does a RMW on rsp.
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

static LFS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::fs(),
        disp: 0,
        write: true,
    }
];

static LGS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::gs(),
        disp: 0,
        write: true,
    }
];

static LSS_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ss(),
        disp: 0,
        write: true,
    }
];

static CMPXCHG_OPS_BYTE: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::al(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::al(),
        disp: 0,
        write: true,
    },
];

static CMPXCHG_OPS_WORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ax(),
        disp: 0,
        write: true,
    },
];

static CMPXCHG_OPS_DWORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0,
        write: true,
    },
];

static CMPXCHG_OPS_QWORD: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0,
        write: true,
    },
];

static CMPXCHG8B_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ebx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0,
        write: true,
    },
];

static CMPXCHG16B_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rdx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rcx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rbx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rax(),
        disp: 0,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rdx(),
        disp: 0,
        write: true,
    },
];

// TODO: register size should be picked by memory access size, but defaulting to rdi for now.
static MASKMOVQ_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rdi(),
        disp: 0i32,
        write: true,
    },
];

static MONITOR_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rcx(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rdx(),
        disp: 0i32,
        write: false,
    },
];

static XMM0_READ_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm0(),
        disp: 0i32,
        write: false,
    },
];

static MULX_64B_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rdx(),
        disp: 0i32,
        write: false,
    },
];

static MULX_32B_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::edx(),
        disp: 0i32,
        write: false,
    },
];

static EDI_MEMWRITE_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::edi(),
        disp: 0i32,
        write: false,
    },
];

static RDI_MEMWRITE_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::Deref,
        reg: RegSpec::rdi(),
        disp: 0i32,
        write: false,
    },
];

static PCMPESTRI_64B_OPS: &'static [ImplicitOperand] = &[
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
        reg: RegSpec::ecx(),
        disp: 0i32,
        write: true,
    },
];

static PCMPESTRI_32B_OPS: &'static [ImplicitOperand] = &[
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
        reg: RegSpec::ecx(),
        disp: 0i32,
        write: true,
    },
];

static PCMPESTRM_64B_OPS: &'static [ImplicitOperand] = &[
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
        reg: RegSpec::xmm0(),
        disp: 0i32,
        write: true,
    },
];

static PCMPESTRM_32B_OPS: &'static [ImplicitOperand] = &[
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
        reg: RegSpec::xmm0(),
        disp: 0i32,
        write: true,
    },
];

static PCMPISTRI_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0i32,
        write: true,
    },
];

static PCMPISTRM_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm0(),
        disp: 0i32,
        write: true,
    },
];

static READ_EDX_EAX_OPS: &'static [ImplicitOperand] = &[
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
];

static LMSW_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cr0(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cr0(),
        disp: 0i32,
        write: true,
    },
];

static SMSW_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cr0(),
        disp: 0i32,
        write: false,
    },
];

static READ_EAX_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: false,
    },
];

static WRITE_AL_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::al(),
        disp: 0i32,
        write: true,
    },
];

static RW_XMM0TO7_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(0),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(1),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(2),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(3),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(4),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(5),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(6),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(7),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(0),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(1),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(2),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(3),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(4),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(5),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(6),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(7),
        disp: 0i32,
        write: true,
    },
];

static ENCODEKEY_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(0),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(0),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(1),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(2),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(4),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(5),
        disp: 0i32,
        write: true,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(6),
        disp: 0i32,
        write: true,
    },
];

static LOADIWKEY_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::eax(),
        disp: 0i32,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::xmm(0),
        disp: 0i32,
        write: false,
    },
];

static RW_RCX_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rcx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::rcx(),
        disp: 0,
        write: true,
    },
];

static RW_ECX_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::ecx(),
        disp: 0,
        write: true,
    },
];

static RW_CX_OPS: &'static [ImplicitOperand] = &[
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cx(),
        disp: 0,
        write: false,
    },
    ImplicitOperand {
        spec: OperandSpec::RegRRR,
        reg: RegSpec::cx(),
        disp: 0,
        write: true,
    },
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
const LFS_IDX: u16 = 37;
const LGS_IDX: u16 = 38;
const LSS_IDX: u16 = 39;
const CMPXCHG_IDX_BYTE: u16 = 40;
const CMPXCHG_IDX_WORD: u16 = 41;
const CMPXCHG_IDX_DWORD: u16 = 42;
const CMPXCHG_IDX_QWORD: u16 = 43;
const CMPXCHG8B_IDX: u16 = 44;
const CMPXCHG16B_IDX: u16 = 45;
const RDTSCP_IDX: u16 = 46;
const MASKMOVQ_IDX: u16 = 47;
const MONITOR_IDX: u16 = 48;
const XMM0_READ_IDX: u16 = 49;
const MULX_64B_IDX: u16 = 50;
const MULX_32B_IDX: u16 = 51;
const EDI_MEMWRITE_IDX: u16 = 52;
const RDI_MEMWRITE_IDX: u16 = 53;
const PCMPESTRI_64B_IDX: u16 = 54;
const PCMPESTRI_32B_IDX: u16 = 55;
const PCMPESTRM_64B_IDX: u16 = 56;
const PCMPESTRM_32B_IDX: u16 = 57;
const PCMPISTRI_IDX: u16 = 58;
const PCMPISTRM_IDX: u16 = 59;
const READ_EDX_EAX_IDX: u16 = 60;
const RETF_IDX: u16 = 61;
const LMSW_IDX: u16 = 62;
const SMSW_IDX: u16 = 63;
const READ_EAX_IDX: u16 = 64;
const WRITE_AL_IDX: u16 = 65;
const RW_XMM0TO7_IDX: u16 = 66;
const ENCODEKEY_IDX: u16 = 67;
const LOADIWKEY_IDX: u16 = 68;
const RW_RCX_IDX: u16 = 69;
const RW_ECX_IDX: u16 = 70;
const RW_CX_IDX: u16 = 71;
const ENTER_IDX: u16 = 72;

static IMPLICIT_OPS_LIST: [&[ImplicitOperand]; 73] = [
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
    LFS_OPS,
    LGS_OPS,
    LSS_OPS,
    CMPXCHG_OPS_BYTE,
    CMPXCHG_OPS_WORD,
    CMPXCHG_OPS_DWORD,
    CMPXCHG_OPS_QWORD,
    CMPXCHG8B_OPS,
    CMPXCHG16B_OPS,
    RDTSCP_OPS,
    MASKMOVQ_OPS,
    MONITOR_OPS,
    XMM0_READ_OPS,
    MULX_64B_OPS,
    MULX_32B_OPS,
    EDI_MEMWRITE_OPS,
    RDI_MEMWRITE_OPS,
    PCMPESTRI_64B_OPS,
    PCMPESTRI_32B_OPS,
    PCMPESTRM_64B_OPS,
    PCMPESTRM_32B_OPS,
    PCMPISTRI_OPS,
    PCMPISTRM_OPS,
    READ_EDX_EAX_OPS,
    RETF_OPS,
    LMSW_OPS,
    SMSW_OPS,
    READ_EAX_OPS,
    WRITE_AL_OPS,
    RW_XMM0TO7_OPS,
    ENCODEKEY_OPS,
    LOADIWKEY_OPS,
    RW_RCX_OPS,
    RW_ECX_OPS,
    RW_CX_OPS,
    ENTER_OPS,
];

#[inline(never)]
#[unsafe(no_mangle)]
fn opcode2behavior(opc: &Opcode) -> BehaviorDigest {
    use Opcode::*;
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
        BTC => GENERAL_RW_R_FLAGWRITE
            .set_complex(true),
        BTR => GENERAL_RW_R_FLAGWRITE
            .set_complex(true),
        BTS => GENERAL_RW_R_FLAGWRITE
            .set_complex(true),
        CMPXCHG => GENERAL_RW_R_FLAGWRITE
            .set_nontrivial(true),
        CMPXCHG8B => GENERAL_RW_R_FLAGWRITE
            .set_implicit_ops(CMPXCHG8B_IDX),
        CMPXCHG16B => GENERAL_RW_R_FLAGWRITE
            .set_implicit_ops(CMPXCHG16B_IDX),
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

        // "Invalid" should never be a publicly-visible Opcode variant..
        Invalid => BehaviorDigest::empty()
            .set_complex(true),
        BT => GENERAL_R_R_FLAGWRITE
            .set_complex(true),
        BSF => GENERAL_RW_R_FLAGWRITE,
        BSR => GENERAL_RW_R_FLAGWRITE,
        TZCNT => GENERAL_RW_R_FLAGWRITE,
        MOVSS => GENERAL_RW_R,
        ADDSS => GENERAL_RW_R,
        SUBSS => GENERAL_RW_R,
        MULSS => GENERAL_RW_R,
        DIVSS => GENERAL_RW_R,
        MINSS => GENERAL_RW_R,
        MAXSS => GENERAL_RW_R,
        SQRTSS => GENERAL_RW_R,
        MOVSD => GENERAL_RW_R,
        SQRTSD => GENERAL_RW_R,
        ADDSD => GENERAL_RW_R,
        SUBSD => GENERAL_RW_R,
        MULSD => GENERAL_RW_R,
        DIVSD => GENERAL_RW_R,
        MINSD => GENERAL_RW_R,
        MAXSD => GENERAL_RW_R,
        MOVSLDUP => GENERAL_W_R,
        MOVSHDUP => GENERAL_W_R,
        MOVDDUP => GENERAL_W_R,
        HADDPS => GENERAL_RW_R,
        HSUBPS => GENERAL_RW_R,
        ADDSUBPD => GENERAL_RW_R,
        ADDSUBPS => GENERAL_RW_R,
        CVTSI2SS => GENERAL_W_R,
        CVTSI2SD => GENERAL_W_R,
        CVTTSD2SI => GENERAL_W_R,
        CVTTPS2DQ => GENERAL_W_R,
        CVTPD2DQ => GENERAL_W_R,
        CVTPD2PS => GENERAL_W_R,
        CVTPS2DQ => GENERAL_W_R,
        CVTSD2SI => GENERAL_RW_R,
        CVTSD2SS => GENERAL_RW_R,
        CVTTSS2SI => GENERAL_RW_R,
        CVTSS2SI => GENERAL_RW_R,
        CVTSS2SD => GENERAL_RW_R,
        CVTDQ2PD => GENERAL_W_R,
        LDDQU => GENERAL_W_R,
        MOVZX => GENERAL_RW_R,
        MOVSX => GENERAL_RW_R,
        MOVSXD => GENERAL_RW_R,
        SHRD => GENERAL_RW_R_FLAGWRITE
            .set_operand(2, Access::Read),
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
        // TODO: should there be implicit operands for the iret instructions? they're complex
        // anyway..
        IRET => BehaviorDigest::empty()
            .set_pl_special()
            .set_complex(true),
        IRETD => BehaviorDigest::empty()
            .set_pl_special()
            .set_complex(true),
        IRETQ => BehaviorDigest::empty()
            .set_pl_special()
            .set_complex(true),
        RETF => BehaviorDigest::empty()
            .set_pl_any()
            .set_nontrivial(true)
            .set_implicit_ops(RETF_IDX),
        ENTER => BehaviorDigest::empty()
            .set_implicit_ops(ENTER_IDX)
            .set_operand(0, Access::Read)
            .set_operand(1, Access::Read)
            .set_pl_any(),
        LEAVE => BehaviorDigest::empty()
            .set_implicit_ops(LEAVE_IDX)
            .set_pl_any(),
        MOV => GENERAL_RW_R,
        RETURN => BehaviorDigest::empty()
            .set_implicit_ops(RETURN_IDX)
            .set_nontrivial(true)
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
        IN => BehaviorDigest::empty()
            .set_complex(true)
            .set_pl_special()
            .set_operand(0, Access::Write)
            .set_operand(1, Access::Read),
        OUT => BehaviorDigest::empty()
            .set_complex(true)
            .set_pl_special()
            .set_operand(0, Access::Read)
            .set_operand(1, Access::Read),
        IMUL => BehaviorDigest::empty()
            .set_pl_any()
            .set_flags_access(Access::Write)
            .set_operand(0, Access::Read) // operands are adjusted via non_trivial
            .set_nontrivial(true),
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
        DIV => BehaviorDigest::empty()
            .set_pl_any()
            .set_flags_access(Access::Write)
            .set_operand(0, Access::Read)
            .set_nontrivial(true),
        IDIV => BehaviorDigest::empty()
            .set_pl_any()
            .set_flags_access(Access::Write)
            .set_operand(0, Access::Read)
            .set_nontrivial(true),
        MUL => BehaviorDigest::empty()
            .set_pl_any()
            .set_flags_access(Access::Write)
            .set_operand(0, Access::Read)
            .set_nontrivial(true),
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
        UD1 => GENERAL
            .set_operand(0, Access::None)
            .set_operand(1, Access::None),
        UD2 => GENERAL,
        WBINVD => BehaviorDigest::empty()
            .set_pl0(),
        INVD =>  BehaviorDigest::empty()
            .set_pl0(),
        SYSRET => BehaviorDigest::empty()
            .set_pl0(),
        CLTS => BehaviorDigest::empty()
            .set_implicit_ops(CLTS_IDX)
            .set_pl0(),
        SYSCALL => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
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
        SMSW => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Write)
            .set_implicit_ops(SMSW_IDX),
        LMSW => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read)
            .set_implicit_ops(LMSW_IDX),
        SWAPGS => BehaviorDigest::empty()
            .set_pl0(),
        RDTSCP => BehaviorDigest::empty()
            .set_pl_special()
            .set_implicit_ops(RDTSCP_IDX)
            .set_complex(true),
        // TODO: invlpg does not generate a page fault, so it's "memory" only in generating an
        // address.
        INVLPG => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read),
        // TODO: this is only complex because while the memory access is 512 bytes,
        // `MemoryAccessSize::bytes_size()` does not report it as such.
        FXSAVE => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_complex(true),
        // TODO: this is only complex because while the memory access is 512 bytes,
        // `MemoryAccessSize::bytes_size()` does not report it as such.
        FXRSTOR => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read)
            .set_complex(true),
        LDMXCSR => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true)
            .set_pl_any(),
        STMXCSR => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_complex(true)
            .set_pl_any(),
        XSAVE => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_pl_any()
            .set_complex(true),
        XRSTOR => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_pl_any()
            .set_complex(true),
        XSAVEOPT => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_pl_any()
            .set_complex(true),
        LFENCE => GENERAL,
        MFENCE => GENERAL,
        SFENCE => GENERAL,
        // in almost all cases `clflush` does not "write" anything, but it is more of a write than
        // a read; from any other processor's perspective, the cache coherency protocol would
        // ensure that other processors' caches "are" memory and this would be a no-op for
        // architectural state. but for some kinds of memory (WC, for example), cache coherency is
        // more lax and the executing processor's cache is in fact writing up to 64 bytes of novel
        // data to main memory.
        CLFLUSH => GENERAL_W
            .set_complex(true),
        // same argument as `clflush`.
        CLFLUSHOPT => GENERAL_W
            .set_complex(true),
        // same argument as `clflush`.
        CLWB => GENERAL_W
            .set_complex(true),
        WRMSR => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        RDTSC => BehaviorDigest::empty()
            .set_implicit_ops(RDTSC_IDX)
            .set_pl_special()
            .set_complex(true),
        RDMSR => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        RDPMC => BehaviorDigest::empty()
            .set_implicit_ops(RDPMC_IDX)
            .set_pl_special(),
        SLDT => BehaviorDigest::empty()
            .set_pl_special()
            .set_operand(0, Access::Write)
            .set_complex(true),
        STR => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Write)
            .set_complex(true),
        LLDT => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read)
            .set_complex(true),
        LTR => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VERR => GENERAL_R_FLAGWRITE,
        VERW =>  GENERAL_R_FLAGWRITE,
        CMC => GENERAL_FLAGRW,
        CLC => GENERAL_FLAGRW,
        STC => GENERAL_FLAGRW,
        CLI => GENERAL_FLAGRW
            .set_pl_special(),
        STI => GENERAL_FLAGRW
            .set_pl_special(),
        CLD => GENERAL_FLAGRW,
        STD => GENERAL_FLAGRW,
        JMPE => BehaviorDigest::empty()
            .set_pl_any() // TODO: don't have a processor with jmpe to validate
            .set_operand(0, Access::Read)
            .set_implicit_ops(JMP_OPS_IDX),
        POPCNT => GENERAL_W_R_FLAGWRITE,
        MOVDQU => GENERAL_W_R,
        MOVDQA => GENERAL_W_R,
        MOVQ => GENERAL_W_R,
        CMPSS => GENERAL_RW_R
            .set_operand(2, Access::Read),
        CMPSD => GENERAL_RW_R
            .set_operand(2, Access::Read),
        UNPCKLPS => GENERAL_RW_R,
        UNPCKLPD => GENERAL_RW_R,
        UNPCKHPS => GENERAL_RW_R,
        UNPCKHPD => GENERAL_RW_R,
        PSHUFHW => GENERAL_W_R
            .set_operand(2, Access::Read),
        PSHUFLW => GENERAL_W_R
            .set_operand(2, Access::Read),
        MOVUPS => GENERAL_W_R,
        MOVQ2DQ => GENERAL_W_R,
        MOVDQ2Q => GENERAL_W_R,
        RSQRTSS => GENERAL_RW_R,
        RCPSS => GENERAL_RW_R,

        ANDN => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        BEXTR => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        BLSI => GENERAL_W_R_FLAGWRITE,
        BLSMSK => GENERAL_W_R_FLAGWRITE,
        BLSR => GENERAL_W_R_FLAGWRITE,
        VMCLEAR => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VMXON => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        VMCALL => BehaviorDigest::empty()
            .set_complex(true),
        VMLAUNCH => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        VMRESUME => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        VMXOFF => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        PCONFIG => BehaviorDigest::empty()
            .set_complex(true),
        MONITOR => BehaviorDigest::empty()
            .set_pl_special()
            .set_implicit_ops(MONITOR_IDX)
            .set_complex(true),
        MWAIT => BehaviorDigest::empty()
            .set_pl_special()
            .set_complex(true),
        MONITORX => BehaviorDigest::empty()
            .set_pl_any()
            .set_implicit_ops(MONITOR_IDX)
            .set_complex(true),
        MWAITX => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        CLAC => BehaviorDigest::empty()
            .set_pl0()
            .set_flags_access(Access::Write),
        STAC => BehaviorDigest::empty()
            .set_pl0()
            .set_flags_access(Access::Write),
        ENCLS => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        ENCLV => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        XGETBV => BehaviorDigest::empty()
            .set_complex(true),
        XSETBV =>  BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        VMFUNC => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        XABORT => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        XBEGIN => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        XEND => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        XTEST => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        ENCLU => BehaviorDigest::empty()
            .set_pl_special()
            .set_complex(true),
        RDPKRU => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        WRPKRU => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),

        RDPRU => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        CLZERO => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),

        RDSEED => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_flags_access(Access::Write)
            .set_pl_any(),
        RDRAND => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_flags_access(Access::Write)
            .set_pl_any(),

        ADDPS => GENERAL_RW_R,
        ADDPD => GENERAL_RW_R,
        ANDNPS => GENERAL_RW_R,
        ANDNPD => GENERAL_RW_R,
        ANDPS => GENERAL_RW_R,
        ANDPD => GENERAL_RW_R,
        BSWAP => GENERAL_RW,
        CMPPD => GENERAL_RW_R
            .set_operand(2, Access::Read),
        CMPPS =>  GENERAL_RW_R
            .set_operand(2, Access::Read),
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
        // TODO: untested, don't have relevant hardware..
        GETSEC => BehaviorDigest::empty()
            .set_pl_any(),
        LFS => GENERAL_W_R
            .set_implicit_ops(LFS_IDX),
        LGS => GENERAL_W_R
            .set_implicit_ops(LGS_IDX),
        LSS => GENERAL_W_R
            .set_implicit_ops(LSS_IDX),
        MASKMOVQ => GENERAL_R_R
            .set_implicit_ops(MASKMOVQ_IDX),
        MASKMOVDQU => GENERAL_R_R
            .set_implicit_ops(MASKMOVQ_IDX),
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
        MOVLHPS => GENERAL_RW_R,
        MOVHLPS => GENERAL_RW_R,
        MOVUPD => GENERAL_W_R,
        MOVMSKPS => GENERAL_RW_R,
        MOVMSKPD => GENERAL_RW_R,
        MOVNTI => GENERAL_W_R,
        MOVNTPS => GENERAL_W_R,
        MOVNTPD => GENERAL_W_R,
        EXTRQ => GENERAL_RW_R
            .set_nontrivial(true),
        INSERTQ => GENERAL_RW_R
            .set_nontrivial(true),
        MOVNTSS => GENERAL_W_R,
        MOVNTSD => GENERAL_W_R,
        MOVNTQ => GENERAL_W_R,
        MOVNTDQ => GENERAL_W_R,
        MULPS => GENERAL_RW_R,
        MULPD => GENERAL_RW_R,
        ORPS => GENERAL_RW_R,
        ORPD => GENERAL_RW_R,
        PACKSSDW => GENERAL_RW_R,
        PACKSSWB => GENERAL_RW_R,
        PACKUSWB => GENERAL_RW_R,
        PADDB => GENERAL_RW_R,
        PADDD => GENERAL_RW_R,
        PADDQ => GENERAL_RW_R,
        PADDSB => GENERAL_RW_R,
        PADDSW => GENERAL_RW_R,
        PADDUSB => GENERAL_RW_R,
        PADDUSW => GENERAL_RW_R,
        PADDW => GENERAL_RW_R,
        PAND => GENERAL_RW_R,
        PANDN => GENERAL_RW_R,
        PAVGB => GENERAL_RW_R,
        PAVGW => GENERAL_RW_R,
        PCMPEQB => GENERAL_RW_R,
        PCMPEQD => GENERAL_RW_R,
        PCMPEQW => GENERAL_RW_R,
        PCMPGTB => GENERAL_RW_R,
        PCMPGTD => GENERAL_RW_R,
        PCMPGTW => GENERAL_RW_R,
        PINSRW => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PMADDWD => GENERAL_RW_R,
        PMAXSW => GENERAL_RW_R,
        PMAXUB => GENERAL_RW_R,
        PMINSW => GENERAL_RW_R,
        PMINUB => GENERAL_RW_R,
        PMOVMSKB => GENERAL_RW_R,
        PMULHUW => GENERAL_RW_R,
        PMULHW => GENERAL_RW_R,
        PMULLW => GENERAL_RW_R,
        PMULUDQ => GENERAL_RW_R,
        POR => GENERAL_RW_R,
        PSADBW => GENERAL_RW_R,
        PSHUFW => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PSHUFD => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PSLLD => GENERAL_RW_R,
        PSLLDQ => GENERAL_RW_R,
        PSLLQ => GENERAL_RW_R,
        PSLLW => GENERAL_RW_R,
        PSRAD => GENERAL_RW_R,
        PSRAW => GENERAL_RW_R,
        PSRLD => GENERAL_RW_R,
        PSRLDQ => GENERAL_RW_R,
        PSRLQ => GENERAL_RW_R,
        PSRLW => GENERAL_RW_R,
        PSUBB => GENERAL_RW_R,
        PSUBD => GENERAL_RW_R,
        PSUBQ => GENERAL_RW_R,
        PSUBSB => GENERAL_RW_R,
        PSUBSW => GENERAL_RW_R,
        PSUBUSB => GENERAL_RW_R,
        PSUBUSW => GENERAL_RW_R,
        PSUBW => GENERAL_RW_R,
        PUNPCKHBW => GENERAL_RW_R,
        PUNPCKHDQ => GENERAL_RW_R,
        PUNPCKHWD => GENERAL_RW_R,
        PUNPCKLBW => GENERAL_RW_R,
        PUNPCKLDQ => GENERAL_RW_R,
        PUNPCKLWD => GENERAL_RW_R,
        PUNPCKLQDQ => GENERAL_RW_R,
        PUNPCKHQDQ => GENERAL_RW_R,
        PXOR => GENERAL_RW_R,
        RCPPS => GENERAL_W_R,
        RSM => BehaviorDigest::empty()
            .set_pl_special()
            .set_complex(true),
        RSQRTPS => GENERAL_W_R,
        SHLD => GENERAL_RW_R_R
            .set_flags_access(Access::Write),
        SHUFPD => GENERAL_RW_R_R,
        SHUFPS => GENERAL_RW_R_R,
        // TODO: slhd is not real, typo of shld
        SLHD => BehaviorDigest::empty(),
        SQRTPS => GENERAL_W_R,
        SQRTPD => GENERAL_W_R,
        SUBPS => GENERAL_RW_R,
        SUBPD => GENERAL_RW_R,
        SYSENTER => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        SYSEXIT => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        UCOMISD => GENERAL_R_R_FLAGWRITE,
        UCOMISS => GENERAL_R_R_FLAGWRITE,
        VMREAD => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_operand(1, Access::Read)
            .set_pl0()
            .set_complex(true),
        VMWRITE => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_operand(1, Access::Read)
            .set_pl0()
            .set_complex(true),
        XORPS => GENERAL_RW_R,
        XORPD => GENERAL_RW_R,

        VMOVDDUP => GENERAL_W_R,
        VPSHUFLW => GENERAL_W_R
            .set_operand(2, Access::Read),
        VPSHUFHW => GENERAL_W_R
            .set_operand(2, Access::Read),
        VHADDPS => GENERAL_W_R_R,
        VHSUBPS => GENERAL_W_R_R,
        VADDSUBPS => GENERAL_W_R_R,
        VCVTPD2DQ => GENERAL_W_R,
        VLDDQU => GENERAL_W_R,

        VCOMISD => GENERAL_R_R_FLAGWRITE,
        VCOMISS => GENERAL_R_R_FLAGWRITE,
        VUCOMISD => GENERAL_R_R_FLAGWRITE,
        VUCOMISS => GENERAL_R_R_FLAGWRITE,
        VADDPD => GENERAL_W_R_R,
        VADDPS => GENERAL_W_R_R,
        VADDSD => GENERAL_W_R_R,
        VADDSS => GENERAL_W_R_R,
        VADDSUBPD => GENERAL_W_R_R,
        VAESDEC => GENERAL_W_R_R,
        VAESDECLAST => GENERAL_W_R_R,
        VAESENC => GENERAL_W_R_R,
        VAESENCLAST => GENERAL_W_R_R,
        VAESIMC => GENERAL_W_R,
        VAESKEYGENASSIST => GENERAL_W_R_R,
        VBLENDPD => GENERAL_W_R_R_IMM8,
        VBLENDPS => GENERAL_W_R_R_IMM8,
        VBLENDVPD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VBLENDVPS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VBROADCASTF128 => GENERAL_W_R,
        VBROADCASTI128 => GENERAL_W_R,
        VBROADCASTSD => GENERAL_W_R,
        VBROADCASTSS => GENERAL_W_R,
        VCMPSD => GENERAL_W_R_R_IMM8,
        VCMPSS => GENERAL_W_R_R_IMM8,
        VCMPPD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VCMPPS =>  GENERAL_W_R_R
            .set_operand(3, Access::Read),
        // TODO: SUPER suspicious about the RW_R/W_R confusion here.
        // vcvtss2si says that dest[0:63] are written but omits bits 64 and up.
        // vcvtsd2si says taht dest[0:63] are written and upper bits are taken from src1.
        // is src1 used generally?
        VCVTDQ2PD => GENERAL_W_R,
        VCVTDQ2PS => GENERAL_W_R,
        VCVTPD2PS => GENERAL_W_R,
        VCVTPH2PS => GENERAL_W_R,
        VCVTPS2DQ => GENERAL_W_R,
        VCVTPS2PD => GENERAL_W_R,
        VCVTSS2SD => GENERAL_W_R_R,
        VCVTSI2SS => GENERAL_W_R_R,
        VCVTSI2SD => GENERAL_W_R_R,
        VCVTSD2SI => GENERAL_RW_R,
        VCVTSD2SS => GENERAL_W_R_R,
        VCVTPS2PH => GENERAL_W_R_R,
        VCVTSS2SI => GENERAL_RW_R,
        VCVTTPD2DQ => GENERAL_W_R,
        VCVTTPS2DQ => GENERAL_W_R,
        VCVTTSS2SI => GENERAL_RW_R,
        VCVTTSD2SI => GENERAL_RW_R,
        VDIVPD => GENERAL_W_R_R,
        VDIVPS => GENERAL_W_R_R,
        VDIVSD => GENERAL_W_R_R,
        VDIVSS => GENERAL_W_R_R,
        VDPPD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VDPPS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VEXTRACTF128 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VEXTRACTI128 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VEXTRACTPS => GENERAL_W_R
            .set_operand(2, Access::Read),
        VFMADD132PD => GENERAL_RW_R_R,
        VFMADD132PS => GENERAL_RW_R_R,
        VFMADD132SD => GENERAL_RW_R_R,
        VFMADD132SS => GENERAL_RW_R_R,
        VFMADD213PD => GENERAL_RW_R_R,
        VFMADD213PS => GENERAL_RW_R_R,
        VFMADD213SD => GENERAL_RW_R_R,
        VFMADD213SS => GENERAL_RW_R_R,
        VFMADD231PD => GENERAL_RW_R_R,
        VFMADD231PS => GENERAL_RW_R_R,
        VFMADD231SD => GENERAL_RW_R_R,
        VFMADD231SS => GENERAL_RW_R_R,
        VFMADDSUB132PD => GENERAL_RW_R_R,
        VFMADDSUB132PS => GENERAL_RW_R_R,
        VFMADDSUB213PD => GENERAL_RW_R_R,
        VFMADDSUB213PS => GENERAL_RW_R_R,
        VFMADDSUB231PD => GENERAL_RW_R_R,
        VFMADDSUB231PS => GENERAL_RW_R_R,
        VFMSUB132PD => GENERAL_RW_R_R,
        VFMSUB132PS => GENERAL_RW_R_R,
        VFMSUB132SD => GENERAL_RW_R_R,
        VFMSUB132SS => GENERAL_RW_R_R,
        VFMSUB213PD => GENERAL_RW_R_R,
        VFMSUB213PS => GENERAL_RW_R_R,
        VFMSUB213SD => GENERAL_RW_R_R,
        VFMSUB213SS => GENERAL_RW_R_R,
        VFMSUB231PD => GENERAL_RW_R_R,
        VFMSUB231PS => GENERAL_RW_R_R,
        VFMSUB231SD => GENERAL_RW_R_R,
        VFMSUB231SS => GENERAL_RW_R_R,
        VFMSUBADD132PD => GENERAL_RW_R_R,
        VFMSUBADD132PS => GENERAL_RW_R_R,
        VFMSUBADD213PD => GENERAL_RW_R_R,
        VFMSUBADD213PS => GENERAL_RW_R_R,
        VFMSUBADD231PD => GENERAL_RW_R_R,
        VFMSUBADD231PS => GENERAL_RW_R_R,
        VFNMADD132PD => GENERAL_RW_R_R,
        VFNMADD132PS => GENERAL_RW_R_R,
        VFNMADD132SD => GENERAL_RW_R_R,
        VFNMADD132SS => GENERAL_RW_R_R,
        VFNMADD213PD => GENERAL_RW_R_R,
        VFNMADD213PS => GENERAL_RW_R_R,
        VFNMADD213SD => GENERAL_RW_R_R,
        VFNMADD213SS => GENERAL_RW_R_R,
        VFNMADD231PD => GENERAL_RW_R_R,
        VFNMADD231PS => GENERAL_RW_R_R,
        VFNMADD231SD => GENERAL_RW_R_R,
        VFNMADD231SS => GENERAL_RW_R_R,
        VFNMSUB132PD => GENERAL_RW_R_R,
        VFNMSUB132PS => GENERAL_RW_R_R,
        VFNMSUB132SD => GENERAL_RW_R_R,
        VFNMSUB132SS => GENERAL_RW_R_R,
        VFNMSUB213PD => GENERAL_RW_R_R,
        VFNMSUB213PS => GENERAL_RW_R_R,
        VFNMSUB213SD => GENERAL_RW_R_R,
        VFNMSUB213SS => GENERAL_RW_R_R,
        VFNMSUB231PD => GENERAL_RW_R_R,
        VFNMSUB231PS => GENERAL_RW_R_R,
        VFNMSUB231SD => GENERAL_RW_R_R,
        VFNMSUB231SS => GENERAL_RW_R_R,
        VGATHERDPD => BehaviorDigest::empty()
            .set_complex(true),
        VGATHERDPS => BehaviorDigest::empty()
            .set_complex(true),
        VGATHERQPD => BehaviorDigest::empty()
            .set_complex(true),
        VGATHERQPS => BehaviorDigest::empty()
            .set_complex(true),
        VHADDPD => GENERAL_W_R_R,
        VHSUBPD => GENERAL_W_R_R,
        VINSERTF128 => GENERAL_W_R_R_IMM8,
        VINSERTI128 => GENERAL_W_R_R_IMM8,
        VINSERTPS => GENERAL_W_R_R_IMM8,
        VMASKMOVDQU => GENERAL_R_R
            .set_nontrivial(true),
        VMASKMOVPD => GENERAL_W_R_R,
        VMASKMOVPS => GENERAL_W_R_R,
        VMAXPD => GENERAL_W_R_R,
        VMAXPS => GENERAL_W_R_R,
        VMAXSD => GENERAL_W_R_R,
        VMAXSS => GENERAL_W_R_R,
        VMINPD => GENERAL_W_R_R,
        VMINPS => GENERAL_W_R_R,
        VMINSD => GENERAL_W_R_R,
        VMINSS => GENERAL_W_R_R,
        VMOVAPD => GENERAL_W_R,
        VMOVAPS => GENERAL_W_R,
        VMOVD => GENERAL_W_R,
        VMOVDQA => GENERAL_W_R,
        VMOVDQU => GENERAL_W_R,
        VMOVHLPS => GENERAL_W_R_R,
        VMOVLHPS => GENERAL_W_R_R,
        // these four are not actually reached due to check above
        VMOVHPD => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(1, Access::Read)
            .set_nontrivial(true),
        VMOVHPS => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(1, Access::Read)
            .set_nontrivial(true),
        VMOVLPD => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(1, Access::Read)
            .set_nontrivial(true),
        VMOVLPS => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(1, Access::Read)
            .set_nontrivial(true),
        VMOVMSKPD => GENERAL_W_R,
        VMOVMSKPS => GENERAL_W_R,
        VMOVNTDQ => GENERAL_W_R,
        VMOVNTDQA => GENERAL_W_R,
        VMOVNTPD => GENERAL_W_R,
        VMOVNTPS => GENERAL_W_R,
        VMOVQ => GENERAL_W_R,
        VMOVSS => GENERAL_W_R_R,
        VMOVSD => GENERAL_W_R_R,
        VMOVSHDUP => GENERAL_W_R,
        VMOVSLDUP => GENERAL_W_R,
        VMOVUPD => GENERAL_W_R,
        VMOVUPS => GENERAL_W_R,
        VMPSADBW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VMULPD => GENERAL_W_R_R,
        VMULPS => GENERAL_W_R_R,
        VMULSD => GENERAL_W_R_R,
        VMULSS => GENERAL_W_R_R,
        VPABSB => GENERAL_W_R,
        VPABSD => GENERAL_W_R,
        VPABSW => GENERAL_W_R,
        VPACKSSDW => GENERAL_W_R_R,
        VPACKUSDW => GENERAL_W_R_R,
        VPACKSSWB => GENERAL_W_R_R,
        VPACKUSWB => GENERAL_W_R_R,
        VPADDB => GENERAL_W_R_R,
        VPADDD => GENERAL_W_R_R,
        VPADDQ => GENERAL_W_R_R,
        VPADDSB => GENERAL_W_R_R,
        VPADDSW => GENERAL_W_R_R,
        VPADDUSB => GENERAL_W_R_R,
        VPADDUSW => GENERAL_W_R_R,
        VPADDW => GENERAL_W_R_R,
        VPALIGNR => GENERAL_W_R_R_IMM8,
        VANDPD => GENERAL_W_R_R,
        VANDPS => GENERAL_W_R_R,
        VORPD => GENERAL_W_R_R,
        VORPS => GENERAL_W_R_R,
        VANDNPD => GENERAL_W_R_R,
        VANDNPS => GENERAL_W_R_R,
        VPAND => GENERAL_W_R_R,
        VPANDN => GENERAL_W_R_R,
        VPAVGB => GENERAL_W_R_R,
        VPAVGW => GENERAL_W_R_R,
        VPBLENDD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPBLENDVB => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPBLENDW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPBROADCASTB => GENERAL_W_R,
        VPBROADCASTD => GENERAL_W_R,
        VPBROADCASTQ => GENERAL_W_R,
        VPBROADCASTW => GENERAL_W_R,
        VPCLMULQDQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCMPEQB => GENERAL_W_R_R,
        VPCMPEQD => GENERAL_W_R_R,
        VPCMPEQQ => GENERAL_W_R_R,
        VPCMPEQW => GENERAL_W_R_R,
        VPCMPGTB => GENERAL_W_R_R,
        VPCMPGTD => GENERAL_W_R_R,
        VPCMPGTQ => GENERAL_W_R_R,
        VPCMPGTW => GENERAL_W_R_R,
        VPCMPESTRI => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_nontrivial(true),
        VPCMPESTRM => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_nontrivial(true),
        VPCMPISTRI => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_implicit_ops(PCMPISTRI_IDX),
        VPCMPISTRM => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_implicit_ops(PCMPISTRM_IDX),
        VPERM2F128 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPERM2I128 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPERMD => GENERAL_W_R_R,
        VPERMILPD => GENERAL_W_R_R,
        VPERMILPS => GENERAL_W_R_R,
        VPERMPD => GENERAL_W_R_R,
        VPERMPS => GENERAL_W_R_R,
        VPERMQ => GENERAL_W_R_R,
        VPEXTRB => GENERAL_W_R
            .set_operand(2, Access::Read),
        VPEXTRD => GENERAL_W_R
            .set_operand(2, Access::Read),
        VPEXTRQ => GENERAL_W_R
            .set_operand(2, Access::Read),
        VPEXTRW => GENERAL_W_R
            .set_operand(2, Access::Read),
        // TODO: complex
        VPGATHERDD => BehaviorDigest::empty()
            .set_complex(true),
        VPGATHERDQ => BehaviorDigest::empty()
            .set_complex(true),
        VPGATHERQD => BehaviorDigest::empty()
            .set_complex(true),
        VPGATHERQQ => BehaviorDigest::empty()
            .set_complex(true),
        VPHADDD => GENERAL_W_R_R,
        VPHADDSW => GENERAL_W_R_R,
        VPHADDW => GENERAL_W_R_R,
        VPMADDUBSW => GENERAL_W_R_R,
        VPHMINPOSUW => GENERAL_W_R,
        VPHSUBD => GENERAL_W_R_R,
        VPHSUBSW => GENERAL_W_R_R,
        VPHSUBW => GENERAL_W_R_R,
        VPINSRB => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPINSRD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPINSRQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPINSRW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPMADDWD => GENERAL_W_R_R,
        VPMASKMOVD => GENERAL_W_R_R,
        VPMASKMOVQ => GENERAL_W_R_R,
        VPMAXSB => GENERAL_W_R_R,
        VPMAXSD => GENERAL_W_R_R,
        VPMAXSW => GENERAL_W_R_R,
        VPMAXUB => GENERAL_W_R_R,
        VPMAXUW => GENERAL_W_R_R,
        VPMAXUD => GENERAL_W_R_R,
        VPMINSB => GENERAL_W_R_R,
        VPMINSW => GENERAL_W_R_R,
        VPMINSD => GENERAL_W_R_R,
        VPMINUB => GENERAL_W_R_R,
        VPMINUW => GENERAL_W_R_R,
        VPMINUD => GENERAL_W_R_R,
        VPMOVMSKB => GENERAL_W_R,
        VPMOVSXBD => GENERAL_W_R,
        VPMOVSXBQ => GENERAL_W_R,
        VPMOVSXBW => GENERAL_W_R,
        VPMOVSXDQ => GENERAL_W_R,
        VPMOVSXWD => GENERAL_W_R,
        VPMOVSXWQ => GENERAL_W_R,
        VPMOVZXBD => GENERAL_W_R,
        VPMOVZXBQ => GENERAL_W_R,
        VPMOVZXBW => GENERAL_W_R,
        VPMOVZXDQ => GENERAL_W_R,
        VPMOVZXWD => GENERAL_W_R,
        VPMOVZXWQ => GENERAL_W_R,
        VPMULDQ => GENERAL_W_R_R,
        VPMULHRSW => GENERAL_W_R_R,
        VPMULHUW => GENERAL_W_R_R,
        VPMULHW => GENERAL_W_R_R,
        VPMULLQ => GENERAL_W_R_R,
        VPMULLD => GENERAL_W_R_R,
        VPMULLW => GENERAL_W_R_R,
        VPMULUDQ => GENERAL_W_R_R,
        VPOR => GENERAL_W_R_R,
        VPSADBW => GENERAL_W_R_R,
        VPSHUFB => GENERAL_W_R_R,
        VPSHUFD => GENERAL_W_R_R,
        VPSIGNB => GENERAL_W_R_R,
        VPSIGND => GENERAL_W_R_R,
        VPSIGNW => GENERAL_W_R_R,
        VPSLLD => GENERAL_W_R_R,
        VPSLLDQ => GENERAL_W_R_R,
        VPSLLQ => GENERAL_W_R_R,
        VPSLLVD => GENERAL_W_R_R,
        VPSLLVQ => GENERAL_W_R_R,
        VPSLLW => GENERAL_W_R_R,
        VPSRAD => GENERAL_W_R_R,
        VPSRAVD => GENERAL_W_R_R,
        VPSRAW => GENERAL_W_R_R,
        VPSRLD => GENERAL_W_R_R,
        VPSRLDQ => GENERAL_W_R_R,
        VPSRLQ => GENERAL_W_R_R,
        VPSRLVD => GENERAL_W_R_R,
        VPSRLVQ => GENERAL_W_R_R,
        VPSRLW => GENERAL_W_R_R,
        VPSUBB => GENERAL_W_R_R,
        VPSUBD => GENERAL_W_R_R,
        VPSUBQ => GENERAL_W_R_R,
        VPSUBSB => GENERAL_W_R_R,
        VPSUBSW => GENERAL_W_R_R,
        VPSUBUSB => GENERAL_W_R_R,
        VPSUBUSW => GENERAL_W_R_R,
        VPSUBW => GENERAL_W_R_R,
        VPTEST => GENERAL_R_R_FLAGWRITE,
        VPUNPCKHBW => GENERAL_W_R_R,
        VPUNPCKHDQ => GENERAL_W_R_R,
        VPUNPCKHQDQ => GENERAL_W_R_R,
        VPUNPCKHWD => GENERAL_W_R_R,
        VPUNPCKLBW => GENERAL_W_R_R,
        VPUNPCKLDQ => GENERAL_W_R_R,
        VPUNPCKLQDQ => GENERAL_W_R_R,
        VPUNPCKLWD => GENERAL_W_R_R,
        VPXOR => GENERAL_W_R_R,
        VRCPPS => GENERAL_W_R,
        VROUNDPD => GENERAL_W_R_R,
        VROUNDPS => GENERAL_W_R_R,
        VROUNDSD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VROUNDSS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VRSQRTPS => GENERAL_W_R,
        VRSQRTSS => GENERAL_W_R_R,
        VRCPSS => GENERAL_W_R_R,
        VSHUFPD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VSHUFPS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VSQRTPD => GENERAL_W_R,
        VSQRTPS => GENERAL_W_R,
        VSQRTSS => GENERAL_W_R_R,
        VSQRTSD => GENERAL_W_R_R,
        VSUBPD => GENERAL_W_R_R,
        VSUBPS => GENERAL_W_R_R,
        VSUBSD => GENERAL_W_R_R,
        VSUBSS => GENERAL_W_R_R,
        VTESTPD => GENERAL_R_R
            .set_flags_access(Access::Write),
        VTESTPS => GENERAL_R_R
            .set_flags_access(Access::Write),
        VUNPCKHPD => GENERAL_W_R_R,
        VUNPCKHPS => GENERAL_W_R_R,
        VUNPCKLPD => GENERAL_W_R_R,
        VUNPCKLPS => GENERAL_W_R_R,
        VXORPD => GENERAL_W_R_R,
        VXORPS => GENERAL_W_R_R,
        VZEROUPPER => BehaviorDigest::empty()
            .set_complex(true),
        VZEROALL => BehaviorDigest::empty()
            .set_complex(true),
        VLDMXCSR => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true)
            .set_pl_any(),
        VSTMXCSR => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_complex(true)
            .set_pl_any(),

        PCLMULQDQ => GENERAL_RW_R
            .set_operand(2, Access::Read),
        AESKEYGENASSIST => GENERAL_W_R
            .set_operand(2, Access::Read),
        AESIMC => GENERAL_W_R,
        AESENC => GENERAL_RW_R,
        AESENCLAST => GENERAL_RW_R,
        AESDEC => GENERAL_RW_R,
        AESDECLAST => GENERAL_RW_R,
        PCMPGTQ => GENERAL_RW_R,
        PCMPISTRM => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_implicit_ops(PCMPISTRM_IDX),
        PCMPISTRI => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_implicit_ops(PCMPISTRI_IDX),
        PCMPESTRI => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_nontrivial(true),
        PACKUSDW => GENERAL_RW_R,
        PCMPESTRM => GENERAL_R_R
            .set_operand(2, Access::Read)
            .set_flags_access(Access::Write)
            .set_nontrivial(true),
        PCMPEQQ => GENERAL_RW_R,
        PTEST => GENERAL_R_R
            .set_flags_access(Access::Write),
        PHMINPOSUW => GENERAL_W_R,
        DPPS => GENERAL_RW_R
            .set_operand(2, Access::Read),
        DPPD => GENERAL_RW_R
            .set_operand(2, Access::Read),
        MPSADBW => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PMOVZXDQ => GENERAL_RW_R,
        PMOVSXDQ => GENERAL_RW_R,
        PMOVZXBD => GENERAL_RW_R,
        PMOVSXBD => GENERAL_RW_R,
        PMOVZXWQ => GENERAL_RW_R,
        PMOVSXWQ => GENERAL_RW_R,
        PMOVZXBQ => GENERAL_RW_R,
        PMOVSXBQ => GENERAL_RW_R,
        PMOVSXWD => GENERAL_RW_R,
        PMOVZXWD => GENERAL_RW_R,
        PEXTRQ => GENERAL_W_R
            .set_operand(2, Access::Read),
        PEXTRD => GENERAL_W_R
            .set_operand(2, Access::Read),
        PEXTRW => GENERAL_W_R
            .set_operand(2, Access::Read),
        PEXTRB => GENERAL_W_R
            .set_operand(2, Access::Read),
        PMOVSXBW => GENERAL_RW_R,
        PMOVZXBW => GENERAL_RW_R,
        PINSRQ => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PINSRD => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PINSRB => GENERAL_RW_R
            .set_operand(2, Access::Read),
        EXTRACTPS => GENERAL_W_R
            .set_operand(2, Access::Read),
        INSERTPS => GENERAL_W_R
            .set_operand(2, Access::Read),
        ROUNDSS => GENERAL_RW_R
            .set_operand(2, Access::Read),
        ROUNDSD => GENERAL_RW_R
            .set_operand(2, Access::Read),
        ROUNDPS => GENERAL_W_R
            .set_operand(2, Access::Read),
        ROUNDPD => GENERAL_W_R
            .set_operand(2, Access::Read),
        PMAXSB => GENERAL_RW_R,
        PMAXSD => GENERAL_RW_R,
        PMAXUW => GENERAL_RW_R,
        PMAXUD => GENERAL_RW_R,
        PMINSD => GENERAL_RW_R,
        PMINSB => GENERAL_RW_R,
        PMINUD => GENERAL_RW_R,
        PMINUW => GENERAL_RW_R,
        // TODO: need to remove; doesn't exist
        BLENDW => BehaviorDigest::empty(),
        PBLENDVB => GENERAL_RW_R
            .set_implicit_ops(XMM0_READ_IDX),
        PBLENDW => GENERAL_RW_R
            .set_operand(2, Access::Read),
        BLENDVPS => GENERAL_RW_R
            .set_implicit_ops(XMM0_READ_IDX),
        BLENDVPD => GENERAL_RW_R
            .set_implicit_ops(XMM0_READ_IDX),
        BLENDPS => GENERAL_RW_R
            .set_operand(2, Access::Read),
        BLENDPD => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PMULDQ => GENERAL_RW_R,
        MOVNTDQA => GENERAL_W_R,
        PMULLD => GENERAL_RW_R,
        PALIGNR => GENERAL_RW_R
            .set_operand(2, Access::Read),
        PSIGNW => GENERAL_RW_R,
        PSIGND => GENERAL_RW_R,
        PSIGNB => GENERAL_RW_R,
        PSHUFB => GENERAL_RW_R,
        PMULHRSW => GENERAL_RW_R,
        PMADDUBSW => GENERAL_RW_R,
        PABSD => GENERAL_W_R,
        PABSW => GENERAL_W_R,
        PABSB => GENERAL_W_R,
        PHSUBSW => GENERAL_RW_R,
        PHSUBW => GENERAL_RW_R,
        PHSUBD => GENERAL_RW_R,
        PHADDD => GENERAL_RW_R,
        PHADDSW => GENERAL_RW_R,
        PHADDW => GENERAL_RW_R,
        HSUBPD => GENERAL_RW_R,
        HADDPD => GENERAL_RW_R,

        SHA1RNDS4 => GENERAL_RW_R_R,
        SHA1NEXTE => GENERAL_RW_R,
        SHA1MSG1 => GENERAL_RW_R,
        SHA1MSG2 => GENERAL_RW_R,
        SHA256RNDS2 => GENERAL_RW_R
            .set_implicit_ops(XMM0_READ_IDX),
        SHA256MSG1 => GENERAL_RW_R,
        SHA256MSG2 => GENERAL_RW_R,

        LZCNT => GENERAL_W_R
            .set_flags_access(Access::Write),
        CLGI => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        STGI => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        SKINIT => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Read),
        VMLOAD => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Read),
        VMMCALL => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        VMSAVE => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Read),
        VMRUN => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Read),
        INVLPGA => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read)
            .set_operand(1, Access::Read),
        INVLPGB => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read)
            .set_operand(1, Access::Read)
            .set_operand(2, Access::Read),
        TLBSYNC => BehaviorDigest::empty()
            .set_pl0(),

        MOVBE => GENERAL_W_R,

        ADCX => GENERAL_RW_R
            .set_flags_access(Access::ReadWrite),
        ADOX => GENERAL_RW_R
            .set_flags_access(Access::ReadWrite),

        PREFETCHW => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read),

        RDPID => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write),
        VMPTRLD => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VMPTRST => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::Write)
            .set_complex(true),

        BZHI => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        MULX => BehaviorDigest::empty()
            .set_operand(0, Access::Write)
            .set_operand(1, Access::Write)
            .set_operand(2, Access::Read)
            .set_nontrivial(true),
        SHLX => GENERAL_W_R_R,
        SHRX => GENERAL_W_R_R,
        SARX => GENERAL_W_R_R,
        PDEP => GENERAL_W_R_R,
        PEXT => GENERAL_W_R_R,
        RORX => GENERAL_W_R_R,
        XRSTORS => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Read)
            .set_implicit_ops(READ_EDX_EAX_IDX),
        XRSTORS64 => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Read)
            .set_implicit_ops(READ_EDX_EAX_IDX),
        XSAVEC => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true)
            .set_operand(0, Access::Write)
            .set_implicit_ops(READ_EDX_EAX_IDX),
        XSAVEC64 => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true)
            .set_operand(0, Access::Write)
            .set_implicit_ops(READ_EDX_EAX_IDX),
        XSAVES => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Write)
            .set_implicit_ops(READ_EDX_EAX_IDX),
        XSAVES64 => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true)
            .set_operand(0, Access::Write)
            .set_implicit_ops(READ_EDX_EAX_IDX),

        RDFSBASE => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write),
        RDGSBASE => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write),
        WRFSBASE => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read),
        WRGSBASE => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read),

        CRC32 => GENERAL_RW_R,
        SALC => BehaviorDigest::empty()
            .set_pl_any()
            .set_flags_access(Access::Read)
            .set_implicit_ops(WRITE_AL_IDX),
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
        FLDENV => GENERAL_R
            .set_pl_any()
            .set_complex(true),
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
        FNSAVE => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_complex(true),
        FNSTCW => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_complex(true),
        FNSTENV => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_complex(true),
        // TODO: never produced..
        FNSTOR => BehaviorDigest::empty(),
        FNSTSW => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_complex(true),
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
        FRSTOR => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Write)
            .set_complex(true),
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

        LOOPNZ => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read)
            .set_flags_access(Access::ReadWrite)
            .set_nontrivial(true),
        LOOPZ => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read)
            .set_flags_access(Access::ReadWrite)
            .set_nontrivial(true),
        LOOP => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read)
            .set_flags_access(Access::ReadWrite)
            .set_nontrivial(true),
        JRCXZ => BehaviorDigest::empty()
            .set_pl_any()
            .set_operand(0, Access::Read)
            .set_nontrivial(true),

        // started shipping in Tremont, 2020 sept 23
        // while this instruction is marked "write, read", the written first operand is a register
        // interpreteed as an address for a memory destination through the `es` selector.
        MOVDIR64B => GENERAL_W_R
            .set_complex(true),
        MOVDIRI => GENERAL_W_R,

        // started shipping in Tiger Lake, 2020 sept 2
        AESDEC128KL => GENERAL_RW_R
            .set_flags_access(Access::Write),
        AESDEC256KL => GENERAL_RW_R
            .set_flags_access(Access::Write),
        AESDECWIDE128KL => GENERAL_R
            .set_implicit_ops(RW_XMM0TO7_IDX)
            .set_flags_access(Access::Write),
        AESDECWIDE256KL => GENERAL_R
            .set_implicit_ops(RW_XMM0TO7_IDX)
            .set_flags_access(Access::Write),
        AESENC128KL => GENERAL_RW_R
            .set_flags_access(Access::Write),
        AESENC256KL => GENERAL_RW_R
            .set_flags_access(Access::Write),
        AESENCWIDE128KL => GENERAL_R
            .set_implicit_ops(RW_XMM0TO7_IDX)
            .set_flags_access(Access::Write),
        AESENCWIDE256KL => GENERAL_R
            .set_implicit_ops(RW_XMM0TO7_IDX)
            .set_flags_access(Access::Write),
        ENCODEKEY128 => GENERAL_W_R
            .set_implicit_ops(ENCODEKEY_IDX)
            .set_flags_access(Access::Write),
        ENCODEKEY256 => GENERAL_W_R
            .set_implicit_ops(ENCODEKEY_IDX)
            .set_flags_access(Access::Write),
        LOADIWKEY => GENERAL_R_R
            .set_implicit_ops(LOADIWKEY_IDX)
            .set_flags_access(Access::Write),

        // unsure
        HRESET => BehaviorDigest::empty()
            .set_operand(0, Access::Read) // but really, "explicit imm8 operand is ignored"
            .set_implicit_ops(READ_EAX_IDX)
            .set_complex(true),

        // 3dnow. note these are yet untested!
        // the 3dnow DSP instructions (pi2fw, pf2iw, pfnacc, pfpnacc, pswapd)
        // are even more untested.
        FEMMS => GENERAL,
        PI2FW => GENERAL_RW_R,
        PI2FD => GENERAL_W_R,
        PF2IW => GENERAL_RW_R,
        PF2ID => GENERAL_W_R,
        PMULHRW => GENERAL_RW_R,
        PFCMPGE => GENERAL_RW_R,
        PFMIN => GENERAL_RW_R,
        PFRCP => GENERAL_W_R,
        PFRSQRT => GENERAL_W_R,
        PFSUB => GENERAL_RW_R,
        PFADD => GENERAL_RW_R,
        PFCMPGT => GENERAL_RW_R,
        PFMAX => GENERAL_RW_R,
        PFRCPIT1 => GENERAL_RW_R,
        PFRSQIT1 => GENERAL_RW_R,
        PFSUBR => GENERAL_RW_R,
        PFACC => GENERAL_RW_R,
        PFCMPEQ => GENERAL_RW_R,
        PFMUL => GENERAL_RW_R,
        PFMULHRW => GENERAL_RW_R,
        PFRCPIT2 => GENERAL_RW_R,
        PFNACC => GENERAL_RW_R,
        PFPNACC => GENERAL_RW_R,
        PSWAPD => GENERAL_RW_RW,
        PAVGUSB => GENERAL_RW_R,

        // ENQCMD
        // similar to movdir64b, but more complex; the first operand is also an address for a
        // memory destination.
        ENQCMD =>  GENERAL_W_R
            .set_flags_access(Access::Write)
            .set_pl0()
            .set_complex(true),
        ENQCMDS => GENERAL_W_R
            .set_flags_access(Access::Write)
            .set_pl0()
            .set_complex(true),

        // INVPCID
        // this almost meets the bar to be "complex", given that it manages non-architectural
        // state not described by the operand iterator. but.. not quite, for now?
        INVEPT => GENERAL_R_R,
        // similar to above.
        INVVPID => GENERAL_R_R,
        // again, similar to `invept` above.
        INVPCID => GENERAL_R_R,

        // PTWRITE
        // TODO: untested
        PTWRITE => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),

        // GFNI
        GF2P8AFFINEQB => GENERAL_RW_R
            .set_operand(2, Access::Read),
        GF2P8AFFINEINVQB => GENERAL_RW_R
            .set_operand(2, Access::Read),
        GF2P8MULB => GENERAL_RW_R,

        // CET
        WRUSS => GENERAL_W_R
            .set_pl0()
            .set_complex(true),
        WRSS => GENERAL_W_R
            .set_pl_special()
            .set_complex(true),
        INCSSP => GENERAL_R
            .set_pl_special()
            .set_complex(true),
        SAVEPREVSSP => BehaviorDigest::empty()
            .set_pl_special()
            .set_complex(true),
        SETSSBSY => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        CLRSSBSY => BehaviorDigest::empty()
            .set_pl0()
            .set_operand(0, Access::ReadWrite)
            .set_flags_access(Access::Write)
            .set_complex(true),
        RSTORSSP => BehaviorDigest::empty()
            .set_pl_special()
            .set_operand(0, Access::ReadWrite)
            .set_flags_access(Access::Write)
            .set_complex(true),
        ENDBR64 => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        ENDBR32 => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),

        // TDX
        TDCALL => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        SEAMRET => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        SEAMOPS => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),
        SEAMCALL => BehaviorDigest::empty()
            .set_pl0()
            .set_complex(true),

        // WAITPKG
        TPAUSE => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        UMONITOR => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        UMWAIT => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),

        // UINTR
        UIRET => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        TESTUI => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        CLUI => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        STUI => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),
        SENDUIPI => BehaviorDigest::empty()
            .set_pl_any()
            .set_complex(true),

        // TSXLDTRK
        // arguably these should be considered "complex" similar to `uintr` instructions above, but
        // they are not (at this time). the arbitrary distinction here is that x{sus,res}ldtrk
        // operate on nothing *but* the trackedness of loads, so interactions with this processor
        // state can be easily determined by looking for these instructions. additionally, a user
        // interested in this state is probably already looking for these instructions, so
        // declaring them complex adds burden to all other use for no benefit.
        XSUSLDTRK => BehaviorDigest::empty()
            .set_pl_any(),
        XRESLDTRK => BehaviorDigest::empty()
            .set_pl_any(),

        // AVX512F
        VALIGND => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VALIGNQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VBLENDMPD => GENERAL_W_R_R,
        VBLENDMPS => GENERAL_W_R_R,
        VCOMPRESSPD => GENERAL_W_R,
        VCOMPRESSPS => GENERAL_W_R,
        VCVTPD2UDQ => GENERAL_W_R,
        VCVTTPD2UDQ => GENERAL_W_R,
        VCVTPS2UDQ => GENERAL_W_R,
        VCVTTPS2UDQ => GENERAL_W_R,
        VCVTQQ2PD => GENERAL_W_R,
        VCVTQQ2PS => GENERAL_W_R,
        VCVTSD2USI => GENERAL_W_R,
        VCVTTSD2USI => GENERAL_W_R,
        VCVTSS2USI => GENERAL_W_R,
        VCVTTSS2USI => GENERAL_W_R,
        VCVTUDQ2PD => GENERAL_W_R,
        VCVTUDQ2PS => GENERAL_W_R,
        VCVTUSI2USD => GENERAL_W_R,
        VCVTUSI2USS => GENERAL_W_R,
        VEXPANDPD => GENERAL_W_R,
        VEXPANDPS => GENERAL_W_R,
        VEXTRACTF32X4 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VEXTRACTF64X4 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VEXTRACTI32X4 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VEXTRACTI64X4 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VFIXUPIMMPD => GENERAL_RW_R_R
            .set_operand(3, Access::Read),
        VFIXUPIMMPS => GENERAL_RW_R_R
            .set_operand(3, Access::Read),
        VFIXUPIMMSD => GENERAL_RW_R_R
            .set_operand(3, Access::Read),
        VFIXUPIMMSS => GENERAL_RW_R_R
            .set_operand(3, Access::Read),
        VGETEXPPD => GENERAL_W_R,
        VGETEXPPS => GENERAL_W_R,
        VGETEXPSD => GENERAL_W_R_R,
        VGETEXPSS => GENERAL_W_R_R,
        VGETMANTPD => GENERAL_W_R
            .set_operand(2, Access::Read),
        VGETMANTPS => GENERAL_W_R
            .set_operand(2, Access::Read),
        VGETMANTSD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VGETMANTSS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VINSERTF32X4 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VINSERTF64X4 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VINSERTI64X4 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VMOVDQA32 => GENERAL_W_R,
        VMOVDQA64 => GENERAL_W_R,
        VMOVDQU32 => GENERAL_W_R,
        VMOVDQU64 => GENERAL_W_R,
        VPBLENDMD => GENERAL_W_R_R,
        VPBLENDMQ => GENERAL_W_R_R,
        VPCMPD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCMPUD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCMPQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCMPUQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCOMPRESSQ => GENERAL_W_R,
        VPCOMPRESSD => GENERAL_W_R,
        VPERMI2D => GENERAL_W_R_R,
        VPERMI2Q => GENERAL_W_R_R,
        VPERMI2PD => GENERAL_W_R_R,
        VPERMI2PS => GENERAL_W_R_R,
        VPERMT2D => GENERAL_W_R_R,
        VPERMT2Q => GENERAL_W_R_R,
        VPERMT2PD => GENERAL_W_R_R,
        VPERMT2PS => GENERAL_W_R_R,
        VPMAXSQ => GENERAL_W_R_R,
        VPMAXUQ => GENERAL_W_R_R,
        VPMINSQ => GENERAL_W_R_R,
        VPMINUQ => GENERAL_W_R_R,
        VPMOVSQB => GENERAL_W_R,
        VPMOVUSQB => GENERAL_W_R,
        VPMOVSQW => GENERAL_W_R,
        VPMOVUSQW => GENERAL_W_R,
        VPMOVSQD => GENERAL_W_R,
        VPMOVUSQD => GENERAL_W_R,
        VPMOVSDB => GENERAL_W_R,
        VPMOVUSDB => GENERAL_W_R,
        VPMOVSDW => GENERAL_W_R,
        VPMOVUSDW => GENERAL_W_R,
        VPROLD => GENERAL_W_R_R,
        VPROLQ => GENERAL_W_R_R,
        VPROLVD => GENERAL_W_R_R,
        VPROLVQ => GENERAL_W_R_R,
        VPRORD => GENERAL_W_R_R,
        VPRORQ => GENERAL_W_R_R,
        VPRORRD => GENERAL_W_R_R,
        VPRORRQ => GENERAL_W_R_R,
        // TODO: complex
        VPSCATTERDD => BehaviorDigest::empty()
            .set_complex(true),
        VPSCATTERDQ => BehaviorDigest::empty()
            .set_complex(true),
        VPSCATTERQD => BehaviorDigest::empty()
            .set_complex(true),
        VPSCATTERQQ => BehaviorDigest::empty()
            .set_complex(true),
        VPSRAQ => GENERAL_W_R_R,
        VPSRAVQ => GENERAL_W_R_R,
        VPTESTNMD => GENERAL_W_R_R,
        VPTESTNMQ => GENERAL_W_R_R,
        VPTERNLOGD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPTERNLOGQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPTESTMD => GENERAL_W_R_R,
        VPTESTMQ => GENERAL_W_R_R,
        VRCP14PD => GENERAL_W_R,
        VRCP14PS => GENERAL_W_R,
        VRCP14SD => GENERAL_W_R_R,
        VRCP14SS => GENERAL_W_R_R,
        VRNDSCALEPD => GENERAL_W_R
            .set_operand(2, Access::Read),
        VRNDSCALEPS => GENERAL_W_R
            .set_operand(2, Access::Read),
        VRNDSCALESD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VRNDSCALESS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VRSQRT14PD => GENERAL_W_R,
        VRSQRT14PS => GENERAL_W_R,
        VRSQRT14SD => GENERAL_W_R_R,
        VRSQRT14SS => GENERAL_W_R_R,
        // vvv --- these don't exist..
        VSCALEDPD => BehaviorDigest::empty(),
        VSCALEDPS => BehaviorDigest::empty(),
        VSCALEDSD => BehaviorDigest::empty(),
        VSCALEDSS => BehaviorDigest::empty(),
        VSCATTERDD => BehaviorDigest::empty(),
        VSCATTERDQ => BehaviorDigest::empty(),
        VSCATTERQD => BehaviorDigest::empty(),
        VSCATTERQQ => BehaviorDigest::empty(),
        // ^^^ --- these don't exist..
        VSHUFF32X4 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VSHUFF64X2 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VSHUFI32X4 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VSHUFI64X2 => GENERAL_W_R_R
            .set_operand(3, Access::Read),

        // AVX512DQ
        VCVTTPD2QQ => GENERAL_W_R,
        VCVTPD2QQ => GENERAL_W_R,
        VCVTTPD2UQQ => GENERAL_W_R,
        VCVTPD2UQQ => GENERAL_W_R,
        VCVTTPS2QQ => GENERAL_W_R,
        VCVTPS2QQ => GENERAL_W_R,
        VCVTTPS2UQQ => GENERAL_W_R,
        VCVTPS2UQQ => GENERAL_W_R,
        VCVTUQQ2PD => GENERAL_W_R,
        VCVTUQQ2PS => GENERAL_W_R,
        VEXTRACTF64X2 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VEXTRACTI64X2 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VFPCLASSPD => GENERAL_W_R
            .set_operand(2, Access::Read),
        VFPCLASSPS => GENERAL_W_R
            .set_operand(2, Access::Read),
        VFPCLASSSD => GENERAL_W_R
            .set_operand(2, Access::Read),
        VFPCLASSSS => GENERAL_W_R
            .set_operand(2, Access::Read),
        VINSERTF64X2 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VINSERTI64X2 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPMOVM2D => GENERAL_W_R,
        VPMOVM2Q => GENERAL_W_R,
        VPMOVB2D => GENERAL_W_R,
        VPMOVQ2M => GENERAL_W_R,
        VRANGEPD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VRANGEPS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VRANGESD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VRANGESS => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VREDUCEPD => GENERAL_W_R_R
            .set_operand(2, Access::Read),
        VREDUCEPS => GENERAL_W_R_R
            .set_operand(2, Access::Read),
        VREDUCESD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VREDUCESS => GENERAL_W_R_R
            .set_operand(3, Access::Read),

        // AVX512BW
        VDBPSADBW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VMOVDQU8 => GENERAL_W_R,
        VMOVDQU16 => GENERAL_W_R,
        VPBLENDMB => GENERAL_W_R_R,
        VPBLENDMW => GENERAL_W_R_R,
        VPCMPB => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCMPUB => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCMPW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPCMPUW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPERMW => GENERAL_W_R_R,
        VPERMI2B => GENERAL_W_R_R,
        VPERMI2W => GENERAL_W_R_R,
        VPMOVM2B => GENERAL_W_R,
        VPMOVM2W => GENERAL_W_R,
        VPMOVB2M => GENERAL_W_R,
        VPMOVW2M => GENERAL_W_R,
        VPMOVSWB => GENERAL_W_R,
        VPMOVUSWB => GENERAL_W_R,
        VPSLLVW => GENERAL_W_R_R,
        VPSRAVW => GENERAL_W_R_R,
        VPSRLVW => GENERAL_W_R_R,
        VPTESTNMB => GENERAL_W_R_R,
        VPTESTNMW => GENERAL_W_R_R,
        VPTESTMB => GENERAL_W_R_R,
        VPTESTMW => GENERAL_W_R_R,

        // AVX512CD
        // TODO: this one does not exist
        VPBROADCASTM => BehaviorDigest::empty(),
        VPCONFLICTD => GENERAL_W_R,
        VPCONFLICTQ => GENERAL_W_R,
        VPLZCNTD => GENERAL_W_R,
        VPLZCNTQ => GENERAL_W_R,

        KUNPCKBW => GENERAL_W_R_R,
        KUNPCKWD => GENERAL_W_R_R,
        KUNPCKDQ => GENERAL_W_R_R,

        KADDB => GENERAL_W_R_R,
        KANDB => GENERAL_W_R_R,
        KANDNB => GENERAL_W_R_R,
        KMOVB => GENERAL_W_R,
        KNOTB => GENERAL_W_R,
        KORB => GENERAL_W_R_R,
        KORTESTB => GENERAL_R_R
            .set_flags_access(Access::Write),
        KSHIFTLB => GENERAL_W_R_R,
        KSHIFTRB => GENERAL_W_R_R,
        KTESTB => GENERAL_R_R
            .set_flags_access(Access::Write),
        KXNORB => GENERAL_W_R_R,
        KXORB => GENERAL_W_R_R,
        KADDW => GENERAL_W_R_R,
        KANDW => GENERAL_W_R_R,
        KANDNW => GENERAL_W_R_R,
        KMOVW => GENERAL_W_R,
        KNOTW => GENERAL_W_R,
        KORW => GENERAL_W_R_R,
        KORTESTW => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        KSHIFTLW => GENERAL_W_R_R,
        KSHIFTRW => GENERAL_W_R_R,
        KTESTW => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        KXNORW => GENERAL_W_R_R,
        KXORW => GENERAL_W_R_R,
        KADDD => GENERAL_W_R_R,
        KANDD => GENERAL_W_R_R,
        KANDND => GENERAL_W_R_R,
        KMOVD => GENERAL_W_R,
        KNOTD => GENERAL_W_R,
        KORD => GENERAL_W_R_R,
        KORTESTD => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        KSHIFTLD => GENERAL_W_R_R,
        KSHIFTRD => GENERAL_W_R_R,
        KTESTD => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        KXNORD => GENERAL_W_R_R,
        KXORD => GENERAL_W_R_R,
        KADDQ => GENERAL_W_R_R,
        KANDQ => GENERAL_W_R_R,
        KANDNQ => GENERAL_W_R_R,
        KMOVQ => GENERAL_W_R,
        KNOTQ => GENERAL_W_R,
        KORQ => GENERAL_W_R_R,
        KORTESTQ => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        KSHIFTLQ => GENERAL_W_R_R,
        KSHIFTRQ => GENERAL_W_R_R,
        KTESTQ => GENERAL_W_R_R
            .set_flags_access(Access::Write),
        KXNORQ => GENERAL_W_R_R,
        KXORQ => GENERAL_W_R_R,

        // AVX512ER
        VEXP2PD => GENERAL_W_R,
        VEXP2PS => GENERAL_W_R,
        // TODO: well, this one isn't real.
        VEXP2SD => BehaviorDigest::empty(),
        // TODO: or this one.
        VEXP2SS => BehaviorDigest::empty(),
        VRCP28PD => GENERAL_W_R,
        VRCP28PS => GENERAL_W_R,
        VRCP28SD => GENERAL_W_R_R,
        VRCP28SS => GENERAL_W_R_R,
        VRSQRT28PD => GENERAL_W_R,
        VRSQRT28PS => GENERAL_W_R,
        VRSQRT28SD => GENERAL_W_R_R,
        VRSQRT28SS => GENERAL_W_R_R,

        // AVX512PF
        VGATHERPF0DPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VGATHERPF0DPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VGATHERPF0QPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VGATHERPF0QPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VGATHERPF1DPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VGATHERPF1DPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VGATHERPF1QPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VGATHERPF1QPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF0DPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF0DPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF0QPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF0QPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF1DPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF1DPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF1QPD => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),
        VSCATTERPF1QPS => BehaviorDigest::empty()
            .set_operand(0, Access::Read)
            .set_complex(true),

        // MPX
        BNDMK => GENERAL_W_R,
        BNDCL => GENERAL_R,
        BNDCU => GENERAL_R,
        BNDCN => GENERAL_R,
        BNDMOV => GENERAL_W_R,
        BNDLDX => GENERAL_W_R
            .set_complex(true),
        BNDSTX => GENERAL_W_R
            .set_complex(true),

        VGF2P8AFFINEQB => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VGF2P8AFFINEINVQB => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPSHRDQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPSHRDD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPSHRDW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPSHLDQ => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPSHLDD => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VPSHLDW => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VBROADCASTF32X8 => GENERAL_W_R,
        VBROADCASTF64X4 => GENERAL_W_R,
        VBROADCASTF32X4 => GENERAL_W_R,
        VBROADCASTF64X2 => GENERAL_W_R,
        VBROADCASTF32X2 => GENERAL_W_R,
        VBROADCASTI32X8 => GENERAL_W_R,
        VBROADCASTI64X4 => GENERAL_W_R,
        VBROADCASTI32X4 => GENERAL_W_R,
        VBROADCASTI64X2 => GENERAL_W_R,
        VBROADCASTI32X2 => GENERAL_W_R,
        VEXTRACTI32X8 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VEXTRACTF32X8 => GENERAL_W_R
            .set_operand(2, Access::Read),
        VINSERTI32X8 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VINSERTF32X8 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        VINSERTI32X4 => GENERAL_W_R_R
            .set_operand(3, Access::Read),
        V4FNMADDSS => GENERAL_RW_R_R
            .set_complex(true),
        V4FNMADDPS => GENERAL_RW_R_R
            .set_complex(true),
        VCVTNEPS2BF16 => GENERAL_W_R,
        V4FMADDSS => GENERAL_RW_R_R
            .set_complex(true),
        V4FMADDPS => GENERAL_RW_R_R
            .set_complex(true),
        VCVTNE2PS2BF16 => GENERAL_W_R_R,
        VP2INTERSECTD => GENERAL_W_R_R,
        VP2INTERSECTQ => GENERAL_W_R_R,
        VP4DPWSSDS => GENERAL_RW_R_R,
        VP4DPWSSD => GENERAL_RW_R_R,
        VPDPWSSDS => GENERAL_RW_R_R,
        VPDPWSSD => GENERAL_RW_R_R,
        VPDPBUSDS => GENERAL_RW_R_R,
        VDPBF16PS => GENERAL_RW_R_R,
        VPBROADCASTMW2D => GENERAL_W_R,
        VPBROADCASTMB2Q => GENERAL_W_R,
        VPMOVD2M => GENERAL_W_R,
        VPMOVQD => GENERAL_W_R,
        VPMOVWB => GENERAL_W_R,
        VPMOVDB => GENERAL_W_R,
        VPMOVDW => GENERAL_W_R,
        VPMOVQB => GENERAL_W_R,
        VPMOVQW => GENERAL_W_R,
        VGF2P8MULB => GENERAL_RW_R_R,
        VPMADD52HUQ => GENERAL_RW_R_R,
        VPMADD52LUQ => GENERAL_RW_R_R,
        VPSHUFBITQMB => GENERAL_W_R_R,
        VPERMB => GENERAL_W_R_R,
        VPEXPANDD => GENERAL_W_R,
        VPEXPANDQ => GENERAL_W_R,
        VPABSQ => GENERAL_W_R,
        VPRORVD => GENERAL_W_R_R,
        VPRORVQ => GENERAL_W_R_R,
        VPMULTISHIFTQB => GENERAL_W_R_R,
        VPERMT2B => GENERAL_RW_R_R,
        VPERMT2W => GENERAL_RW_R_R,
        VPSHRDVQ => GENERAL_RW_R_R,
        VPSHRDVD => GENERAL_RW_R_R,
        VPSHRDVW => GENERAL_RW_R_R,
        VPSHLDVQ => GENERAL_RW_R_R,
        VPSHLDVD => GENERAL_RW_R_R,
        VPSHLDVW => GENERAL_RW_R_R,
        VPCOMPRESSB => GENERAL_W_R,
        VPCOMPRESSW => GENERAL_W_R,
        VPEXPANDB => GENERAL_W_R,
        VPEXPANDW => GENERAL_W_R,
        VPOPCNTD => GENERAL_W_R,
        VPOPCNTQ => GENERAL_W_R,
        VPOPCNTB => GENERAL_W_R,
        VPOPCNTW => GENERAL_W_R,
        VSCALEFSS => GENERAL_W_R_R,
        VSCALEFSD => GENERAL_W_R_R,
        VSCALEFPS => GENERAL_W_R_R,
        VSCALEFPD => GENERAL_W_R_R,
        VPDPBUSD => GENERAL_W_R_R,
        VCVTUSI2SD => GENERAL_W_R_R,
        VCVTUSI2SS => GENERAL_W_R_R,
        VPXORD => GENERAL_W_R_R,
        VPXORQ => GENERAL_W_R_R,
        VPORD => GENERAL_W_R_R,
        VPORQ => GENERAL_W_R_R,
        VPANDND => GENERAL_W_R_R,
        VPANDNQ => GENERAL_W_R_R,
        VPANDD => GENERAL_W_R_R,
        VPANDQ => GENERAL_W_R_R,

        PSMASH => BehaviorDigest::empty()
            .set_pl0()
            .set_flags_access(Access::Write)
            .set_complex(true),
        PVALIDATE => BehaviorDigest::empty()
            .set_pl0()
            .set_flags_access(Access::Write)
            .set_complex(true),
        RMPADJUST => BehaviorDigest::empty()
            .set_pl0()
            .set_flags_access(Access::Write)
            .set_complex(true),
        RMPUPDATE => BehaviorDigest::empty()
            .set_pl0()
            .set_flags_access(Access::Write)
            .set_complex(true),

    };
    behavior
}
