use core::fmt;

use yaxpeax_arch::AddressBase;
use yaxpeax_arch::LengthedInstruction;

use crate::protected_mode::{
    DisplayRules,
    RegSpec, Opcode, Operand, OperandSpec,
    MergeMode, SaeMode,
    Instruction, RegisterBank,
    display::DisplaySinkExt,
};

use yaxpeax_arch::display::DisplaySink;
use yaxpeax_arch::safer_unchecked::GetSaferUnchecked as _;
use yaxpeax_arch::safer_unchecked::unreachable_kinda_unchecked as unreachable_unchecked;

struct DisplayingOperandVisitor<'a, 'rules, T, R> {
    f: &'a mut T,
    rules: &'rules R,
}

impl<T: DisplaySink, R: DisplayRules<T>> DisplayingOperandVisitor<'_, '_, T, R> {
    fn write_register(&mut self, reg: RegSpec) -> Result<(), core::fmt::Error> {
        if self.rules.emit_register(reg, &mut self.f)? {
            return Ok(());
        }

        self.f.write_reg(reg)
    }
}

fn needs_leading_0(imm: u64) -> bool {
    let mut rem = imm;
    let mut digit = 0;
    while rem > 0 {
        digit = rem & 0xf;
        rem = rem >> 4;
    }

    // digit is whatever the top non-zero hex digit was in the number
    digit >= 10
}

#[test]
fn test_leading_0() {
    assert!(needs_leading_0(0xa0));
    assert!(needs_leading_0(0xa00000));
    assert!(!needs_leading_0(0x900000));
    assert!(needs_leading_0(0xf12345));
}

fn hex_ambiguous(imm: u64) -> bool {
    imm >= 10
}

static RELATIVE_BRANCHES: [Opcode; 23] = [
    Opcode::JMP, Opcode::CALL,
    Opcode::JECXZ, Opcode::JCXZ,
    Opcode::LOOP, Opcode::LOOPZ, Opcode::LOOPNZ,
    Opcode::JO, Opcode::JNO,
    Opcode::JB, Opcode::JNB,
    Opcode::JZ, Opcode::JNZ,
    Opcode::JNA, Opcode::JA,
    Opcode::JS, Opcode::JNS,
    Opcode::JP, Opcode::JNP,
    Opcode::JL, Opcode::JGE,
    Opcode::JLE, Opcode::JG,
];

struct RelativeBranchPrinter<'a, F: DisplaySink, Rules: DisplayRules<F>> {
    inst: &'a Instruction,
    out: &'a mut F,
    rules: &'a Rules,
}

impl<'a, F: DisplaySink, Rules: DisplayRules<F>> crate::protected_mode::OperandVisitor for RelativeBranchPrinter<'a, F, Rules> {
    // return true if we printed a relative branch offset, false otherwise
    type Ok = bool;
    // but errors are errors
    type Error = fmt::Error;

    fn visit_reg(&mut self, _reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_deref(&mut self, _base: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_disp(&mut self, _base: RegSpec, _rel: i32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i8(&mut self, rel: i8) -> Result<Self::Ok, Self::Error> {
        if RELATIVE_BRANCHES.contains(&self.inst.opcode) {
            self.out.write_char('$')?;
            let rel = rel.wrapping_add(0u32.wrapping_offset(self.inst.len()).to_linear() as i8);
            let mut v = rel as u8;
            if rel < 0 {
                self.out.write_char('-')?;
                v = rel.unsigned_abs();
            } else {
                self.out.write_char('+')?;
            }
            if !self.rules.emit_signed_immediate(rel as i32, self.out)? {
                if needs_leading_0(v as u64) {
                    self.out.write_char('0')?;
                }
                write!(self.out, "{:X}", v)?;
                if hex_ambiguous(v as u64) {
                    self.out.write_char('h')?;
                }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i32(&mut self, rel: i32) -> Result<Self::Ok, Self::Error> {
        if RELATIVE_BRANCHES.contains(&self.inst.opcode) || self.inst.opcode == Opcode::XBEGIN {
            let rel = rel.wrapping_add(0u32.wrapping_offset(self.inst.len()).to_linear() as i32);
            let mut v = rel as u32;

            if v >= 128 && v < 256 {
                // we'll print two digits, but nasm will try to shorten the jump to a 1-byte offset form. then the
                // offset is out of range and nasm complains "jump destination too far : by <N> byte(s)".
                // "jmp near ptr " is the full name of "i mean it, use a 32-bit offset", so use that to ward away
                // the problematic "optimization".
                self.out.write_str("near ptr ")?;
            }

            self.out.write_char('$')?;
            if rel < 0 {
                self.out.write_char('-')?;
                v = rel.unsigned_abs();
            } else {
                self.out.write_char('+')?;
            }
            if !self.rules.emit_signed_immediate(rel as i32, self.out)? {
                if needs_leading_0(v as u64) {
                    self.out.write_char('0')?;
                }
                write!(self.out, "{:X}", v)?;
                if hex_ambiguous(v as u64) {
                    self.out.write_char('h')?;
                }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
    fn visit_u8(&mut self, _imm: u8) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_i16(&mut self, _imm: i16) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_u16(&mut self, _imm: u16) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_u32(&mut self, _imm: u32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_abs_u16(&mut self, _imm: u16) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_abs_u32(&mut self, _imm: u32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_absolute_far_address(&mut self, _segment: u16, _address: u32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale(&mut self, _index: RegSpec, _scale: u8) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale_disp(&mut self, _index: RegSpec, _scale: u8, _disp: i32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale_disp(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8, _disp: i32) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_other(&mut self) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_reg_mask_merge(&mut self, _spec: RegSpec, _mask: RegSpec, _merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_reg_mask_merge_sae(&mut self, _spec: RegSpec, _mask: RegSpec, _merge_mode: MergeMode, _sae_mode: SaeMode) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_reg_mask_merge_sae_noround(&mut self, _spec: RegSpec, _mask: RegSpec, _merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_disp_masked(&mut self, _base: RegSpec, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_deref_masked(&mut self, _base: RegSpec, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale_masked(&mut self, _index: RegSpec, _scale: u8, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_index_scale_disp_masked(&mut self, _index: RegSpec, _scale: u8, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_masked(&mut self, _base: RegSpec, _index: RegSpec, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_disp_masked(&mut self, _base: RegSpec, _index: RegSpec, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale_masked(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
    fn visit_base_index_scale_disp_masked(&mut self, _base: RegSpec, _index: RegSpec, _scale: u8, _disp: i32, _mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }
}

fn masm_displacement<T: core::fmt::Write>(f: &mut T, disp: i32) -> Result<(), core::fmt::Error> {
    let udisp = if disp >= 0 {
        write!(f, "+ ")?;
        disp as u64
    } else {
        write!(f, "- ")?;
        -(disp as i64) as u64
    };

    if needs_leading_0(udisp) {
        write!(f, "0")?;
    }
    write!(f, "{:X}", udisp)?;
    if hex_ambiguous(udisp) {
        write!(f, "h")?;
    }

    Ok(())
}

impl <T: DisplaySink, R: DisplayRules<T>> crate::protected_mode::OperandVisitor for DisplayingOperandVisitor<'_, '_, T, R> {
    type Ok = ();
    type Error = core::fmt::Error;

    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_u8(&mut self, imm: u8) -> Result<Self::Ok, Self::Error> {
        self.f.span_start_immediate();
        if needs_leading_0(imm as u64) {
            self.f.write_char('0')?;
        }
        write!(self.f, "{:X}", imm)?;
        if hex_ambiguous(imm as u64) {
            self.f.write_char('h')?;
        }
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i8(&mut self, imm: i8) -> Result<Self::Ok, Self::Error> {
        self.f.span_start_immediate();
        let imm = imm as i32 as u32;
        if needs_leading_0(imm as u64) {
            self.f.write_char('0')?;
        }
        write!(self.f, "{:X}", imm)?;
        if hex_ambiguous(imm as u64) {
            self.f.write_char('h')?;
        }
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_u16(&mut self, imm: u16) -> Result<Self::Ok, Self::Error> {
        self.f.span_start_immediate();
        if needs_leading_0(imm as u64) {
            self.f.write_char('0')?;
        }
        write!(self.f, "{:X}", imm)?;
        if hex_ambiguous(imm as u64) {
            self.f.write_char('h')?;
        }
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_i16(&mut self, imm: i16) -> Result<Self::Ok, Self::Error> {
        self.f.span_start_immediate();
        let imm = imm as i32 as u32;
        if needs_leading_0(imm as u64) {
            self.f.write_char('0')?;
        }
        write!(self.f, "{:X}", imm)?;
        if hex_ambiguous(imm as u64) {
            self.f.write_char('h')?;
        }
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_u32(&mut self, imm: u32) -> Result<Self::Ok, Self::Error> {
        self.f.span_start_immediate();
        if needs_leading_0(imm as u64) {
            self.f.write_char('0')?;
        }
        write!(self.f, "{:X}", imm)?;
        if hex_ambiguous(imm as u64) {
            self.f.write_char('h')?;
        }
        self.f.span_end_immediate();
        Ok(())
    }
    fn visit_i32(&mut self, imm: i32) -> Result<Self::Ok, Self::Error> {
        self.f.span_start_immediate();
        let imm = imm as u32;
        if needs_leading_0(imm as u64) {
            self.f.write_char('0')?;
        }
        write!(self.f, "{:X}", imm)?;
        if hex_ambiguous(imm as u64) {
            self.f.write_char('h')?;
        }
        self.f.span_end_immediate();
        Ok(())
    }
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_reg(&mut self, reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_reg(reg)?;
        Ok(())
    }
    fn visit_reg_mask_merge(&mut self, spec: RegSpec, mask: RegSpec, merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        self.f.write_reg(spec)?;
        if mask.num != 0 {
            self.f.write_fixed_size("{")?;
            self.f.write_reg(mask)?;
            self.f.write_fixed_size("}")?;
        }
        if let MergeMode::Zero = merge_mode {
            self.f.write_fixed_size("{z}")?;
        }
        Ok(())
    }
    fn visit_reg_mask_merge_sae(&mut self, spec: RegSpec, mask: RegSpec, merge_mode: MergeMode, _sae_mode: crate::protected_mode::SaeMode) -> Result<Self::Ok, Self::Error> {
        self.f.write_reg(spec)?;
        if mask.num != 0 {
            self.f.write_fixed_size("{")?;
            self.f.write_reg(mask)?;
            self.f.write_fixed_size("}")?;
        }
        if let MergeMode::Zero = merge_mode {
            self.f.write_fixed_size("{z}")?;
        }
        Ok(())
    }
    fn visit_reg_mask_merge_sae_noround(&mut self, spec: RegSpec, mask: RegSpec, merge_mode: MergeMode) -> Result<Self::Ok, Self::Error> {
        self.f.write_reg(spec)?;
        if mask.num != 0 {
            self.f.write_fixed_size("{")?;
            self.f.write_reg(mask)?;
            self.f.write_fixed_size("}")?;
        }
        if let MergeMode::Zero = merge_mode {
            self.f.write_fixed_size("{z}")?;
        }
        Ok(())
    }
    fn visit_abs_u16(&mut self, imm: u16) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        if !self.rules.emit_absolute_address(imm as u32, self.f)? {
            self.f.span_start_address();
            if imm >= 0x1000 && needs_leading_0(imm as u64) {
                self.f.write_char('0')?;
            }
            write!(self.f, "{:04X}", imm)?;
            self.f.write_char('h')?;
            self.f.span_end_address();
        }
        self.f.write_fixed_size("]")?;
        Ok(())
    }
    fn visit_abs_u32(&mut self, imm: u32) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        if !self.rules.emit_absolute_address(imm, self.f)? {
            self.f.span_start_address();
            if imm >= 0x1000_0000 && needs_leading_0(imm as u64) {
                self.f.write_char('0')?;
            }
            write!(self.f, "{:08X}", imm)?;
            self.f.write_char('h')?;
            self.f.span_end_address();
        }
        self.f.write_fixed_size("]")?;
        Ok(())
    }
    fn visit_absolute_far_address(&mut self, segment: u16, address: u32) -> Result<Self::Ok, Self::Error> {
        if !self.rules.emit_far_address(segment, address, self.f)? {
            if needs_leading_0(segment as u64) {
                self.f.write_char('0')?;
            }
            write!(self.f, "{:4X}", segment)?;
            self.f.write_char('h')?;
            self.f.write_fixed_size(":")?;
            if needs_leading_0(address as u64) {
                self.f.write_char('0')?;
            }
            write!(self.f, "{:4X}", address)?;
            self.f.write_char('h')?;
        }
        Ok(())
    }
    #[cfg_attr(not(feature="profiling"), inline(always))]
    #[cfg_attr(feature="profiling", inline(never))]
    fn visit_disp(&mut self, base: RegSpec, disp: i32) -> Result<Self::Ok, Self::Error> {
        self.f.write_char('[')?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size(" ")?;
        masm_displacement(&mut self.f, disp)?;
        self.f.write_fixed_size("]")
    }
    fn visit_deref(&mut self, base: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size("]")
    }
    fn visit_index_scale(&mut self, index: RegSpec, scale: u8) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_fixed_size("]")?;

        Ok(())
    }
    fn visit_index_scale_disp(&mut self, index: RegSpec, scale: u8, disp: i32) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_fixed_size(" ")?;
        masm_displacement(&mut self.f, disp)?;
        self.f.write_char(']')
    }
    fn visit_base_index_scale(&mut self, base: RegSpec, index: RegSpec, scale: u8) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size(" + ")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_fixed_size("]")
    }
    fn visit_base_index_scale_disp(&mut self, base: RegSpec, index: RegSpec, scale: u8, disp: i32) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size(" + ")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_fixed_size(" ")?;
        masm_displacement(&mut self.f, disp)?;
        self.f.write_fixed_size("]")
    }
    fn visit_disp_masked(&mut self, base: RegSpec, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_char('[')?;
        self.f.write_reg(base)?;
        self.f.write_char(' ')?;
        masm_displacement(&mut self.f, disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_deref_masked(&mut self, base: RegSpec, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_index_scale_masked(&mut self, index: RegSpec, scale: u8, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_index_scale_disp_masked(&mut self, index: RegSpec, scale: u8, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_fixed_size(" ")?;
        masm_displacement(&mut self.f, disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_masked(&mut self, base: RegSpec, index: RegSpec, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size(" + ")?;
        self.f.write_reg(index)?;
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_disp_masked(&mut self, base: RegSpec, index: RegSpec, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size(" + ")?;
        self.f.write_reg(index)?;
        self.f.write_fixed_size(" ")?;
        masm_displacement(&mut self.f, disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_scale_masked(&mut self, base: RegSpec, index: RegSpec, scale: u8, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size(" + ")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_fixed_size("]")?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }
    fn visit_base_index_scale_disp_masked(&mut self, base: RegSpec, index: RegSpec, scale: u8, disp: i32, mask_reg: RegSpec) -> Result<Self::Ok, Self::Error> {
        self.f.write_fixed_size("[")?;
        self.f.write_reg(base)?;
        self.f.write_fixed_size(" + ")?;
        self.f.write_reg(index)?;
        if scale > 1 {
            self.f.write_fixed_size(" * ")?;
            self.f.write_scale(scale)?;
        }
        self.f.write_char(' ')?;
        masm_displacement(&mut self.f, disp)?;
        self.f.write_char(']')?;
        self.f.write_char('{')?;
        self.f.write_reg(mask_reg)?;
        self.f.write_char('}')?;
        Ok(())
    }

    fn visit_other(&mut self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

#[cfg_attr(feature="profiling", inline(never))]
pub(crate) fn contextualize<T: DisplaySink, R: DisplayRules<T>>(instr: &Instruction, rules: &R, out: &mut T) -> fmt::Result {
    if instr.xacquire() {
        out.write_fixed_size("xacquire ")?;
    }
    if instr.xrelease() {
        out.write_fixed_size("xrelease ")?;
    }
    if instr.prefixes.lock() {
        out.write_fixed_size("lock ")?;
    }

    if instr.prefixes.rep_any() {
        // TODO: be more precise about when rep/repne 
        if instr.opcode.can_rep() {
            if instr.prefixes.rep() {
                out.write_fixed_size("rep ")?;
            } else if instr.prefixes.repnz() {
                out.write_fixed_size("repne ")?;
            }
        }
    }

    if instr.opcode == Opcode::JMPF {
        out.write_opcode(Opcode::JMP)?; // MASM puts the f on the operand size
    } else if instr.opcode == Opcode::CALLF {
        out.write_opcode(Opcode::CALL)?; // MASM puts the f on the operand size
    } else if instr.opcode == Opcode::FENI8087_NOP {
        out.write_fixed_size("feni")?;
        return Ok(());
    } else if instr.opcode == Opcode::FDISI8087_NOP {
        out.write_fixed_size("fdisi")?;
        return Ok(());
    } else if instr.opcode == Opcode::FSETPM287_NOP {
        out.write_fixed_size("fsetpm")?;
        return Ok(());
    } else {
        out.write_opcode(instr.opcode)?;
    }

    match instr.opcode {
        Opcode::HRESET => {
            // dumpbin shows, and MASM needs, the implicit "eax" operand as an explicit textual operand.
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            visitor.f.write_fixed_size(" ")?;
            if !rules.emit_operand(instr, 0, visitor.f)? {
                instr.visit_operand(0, &mut visitor)?;
            }
            visitor.f.write_fixed_size(", ")?;
            visitor.write_register(RegSpec::eax())?;
            return Ok(());
        }
        Opcode::LSL => {
            // dumpbin shows, and MASM needs, the first and second operands to match in size.
            // this means `lsl eax, edx` is actually shown as `lsl eax, edx`. fix that up here.
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            let Operand::Register { reg: dest } = instr.operand(0) else { panic!("impossible LSL dest"); };

            if let Operand::Register { reg: mut src } = instr.operand(1) {
                visitor.f.write_fixed_size(" ")?;
                if !rules.emit_operand(instr, 0, visitor.f)? {
                    instr.visit_operand(0, &mut visitor)?;
                }
                src.bank = dest.bank;
                visitor.f.write_fixed_size(", ")?;
                visitor.write_register(src)?;
                return Ok(());
            } else {
                // don't need to do anything about memory sources
            };
        }
        Opcode::PREFETCHNTA |
        Opcode::PREFETCH0 |
        Opcode::PREFETCH1 |
        Opcode::PREFETCH2 |
        Opcode::PREFETCHW |
        Opcode::INVLPG |
        Opcode::CLFLUSH |
        Opcode::CLFLUSHOPT |
        Opcode::CLWB => {
            // dumpbin doesn't bother with the memory size here, same for masm.
            out.write_char(' ')?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            if !rules.emit_operand(instr, 0, visitor.f)? {
                instr.visit_operand(0, &mut visitor)?;
            }

            return Ok(());
        }
        Opcode::SGDT | Opcode::SIDT => {
            // masm uses "tbyte" as a memory size here.
            out.write_char(' ')?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            if !rules.emit_operand(instr, 0, visitor.f)? {
                visitor.f.write_fixed_size("fword ptr ")?;
                instr.visit_operand(0, &mut visitor)?;
            }

            return Ok(());
        }
        Opcode::LSS => {
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            visitor.f.write_char(' ')?;
            if !rules.emit_operand(instr, 0, visitor.f)? {
                instr.visit_operand(0, &mut visitor)?;
            }
            visitor.f.write_fixed_size(", ")?;

            if !rules.emit_operand(instr, 1, visitor.f)? {
                match instr.mem_size {
                    4 => {
                        visitor.f.write_fixed_size("dword ptr ")?;
                    },
                    6 => {
                        visitor.f.write_fixed_size("fword ptr ")?;
                    },
                    o => { panic!("impossible memory size: {:?}", o); }
                }

                instr.visit_operand(1, &mut visitor)?;
            }

            return Ok(());
        }
        Opcode::LGDT | Opcode::LIDT => {
            // masm uses "fword" as a memory size here.
            out.write_char(' ')?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            if !rules.emit_operand(instr, 0, visitor.f)? {
                visitor.f.write_fixed_size("fword ptr ")?;
                instr.visit_operand(0, &mut visitor)?;
            }

            return Ok(());
        }
        Opcode::NOP => {
            // masm doesn't accept multi-operand nop, while x86 does have such instructions.
            // report no-operand nop here instead so it at least round-trips..
            if instr.operand_count > 1 {
                return Ok(());
            }
        },
        Opcode::VPSCATTERDD | Opcode::VPSCATTERQD => {
            // intel/xed/etc syntax has the mask register as an operand rather than normal memory masking. is xed wrong?
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            visitor.f.write_char(' ')?;
            if !rules.emit_operand(instr, 0, visitor.f)? {
                visitor.f.write_str("dword ptr ")?;
                instr.visit_operand(0, &mut visitor)?;
                visitor.f.write_char('{')?;
                if !rules.emit_operand(instr, 1, visitor.f)? {
                    visitor.f.write_reg(instr.regs[3])?;
                }
                visitor.f.write_char('}')?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_operand(instr, 2, visitor.f)? {
                instr.visit_operand(2, &mut visitor)?;
            }
            return Ok(());
        },
        Opcode::VPSCATTERDQ | Opcode::VPSCATTERQQ => {
            // intel/xed/etc syntax has the mask register as an operand rather than normal memory masking. is xed wrong?
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            visitor.f.write_char(' ')?;
            if !rules.emit_operand(instr, 0, visitor.f)? {
                visitor.f.write_str("qword ptr ")?;
                instr.visit_operand(0, &mut visitor)?;
                visitor.f.write_char('{')?;
                if !rules.emit_operand(instr, 1, visitor.f)? {
                    visitor.f.write_reg(instr.regs[3])?;
                }
                visitor.f.write_char('}')?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_operand(instr, 2, visitor.f)? {
                instr.visit_operand(2, &mut visitor)?;
            }
            return Ok(());
        },
        Opcode::MONITOR | Opcode::MONITORX | Opcode::MWAITX => {
            // masm wants the implicit registers to all be ... explicit.
            let visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            visitor.f.write_char(' ')?;
            if !rules.emit_register(RegSpec::eax(), visitor.f)? {
                visitor.f.write_reg(RegSpec::eax())?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_register(RegSpec::ecx(), visitor.f)? {
                visitor.f.write_reg(RegSpec::ecx())?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_register(RegSpec::edx(), visitor.f)? {
                visitor.f.write_reg(RegSpec::edx())?;
            }
            return Ok(());
        }
        Opcode::MWAIT => {
            // masm wants the implicit registers to all be ... explicit.
            let visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            visitor.f.write_char(' ')?;
            if !rules.emit_register(RegSpec::ecx(), visitor.f)? {
                visitor.f.write_reg(RegSpec::ecx())?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_register(RegSpec::ecx(), visitor.f)? {
                visitor.f.write_reg(RegSpec::ecx())?;
            }
            return Ok(());
        }
        Opcode::INVLPGB => {
            // masm bug: it doesn't tolerate the mention of the second operand?!
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            visitor.f.write_char(' ')?;
            if !rules.emit_operand(instr, 0, visitor.f)? {
                instr.visit_operand(0, &mut visitor)?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_operand(instr, 2, visitor.f)? {
                instr.visit_operand(2, &mut visitor)?;
            }
            return Ok(());
        }
        Opcode::RDPRU => {
            // masm wants no implicit registers this time.
            return Ok(());
        }
        Opcode::SCAS | Opcode::STOS => {
            // masm does not want the implicit r/e/ax
            out.write_fixed_size(" ")?;
            if !rules.emit_operand(instr, 0, out)? {
                out.write_mem_size_label(instr.mem_size)?;
                out.write_fixed_size(" ptr ")?;
                if let Some(prefix) = instr.segment_override_for_op(0) {
                    let name = prefix.name();
                    out.write_char(name[0] as char)?;
                    out.write_char(name[1] as char)?;
                    out.write_fixed_size(":")?;
                }
                let mut visitor = DisplayingOperandVisitor {
                    f: out,
                    rules,
                };
                instr.visit_operand(0, &mut visitor)?;
            }
            return Ok(());
        }
        Opcode::LODS => {
            // masm does not want the implicit r/e/ax
            out.write_fixed_size(" ")?;
            if !rules.emit_operand(instr, 1, out)? {
                out.write_mem_size_label(instr.mem_size)?;
                out.write_fixed_size(" ptr ")?;
                let mut visitor = DisplayingOperandVisitor {
                    f: out,
                    rules,
                };
                instr.visit_operand(1, &mut visitor)?;
            }
            return Ok(());
        }
        Opcode::PSMASH => {
            // masm wants the implicit eax operand
            out.write_fixed_size(" ")?;
            if !rules.emit_register(RegSpec::eax(), out)? {
                out.write_reg(RegSpec::eax())?;
            }
            return Ok(());
        }
        Opcode::PVALIDATE | Opcode::RMPADJUST | Opcode::RMPUPDATE => {
            // masm wants the implicit registers to all be ... explicit.
            let visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };
            visitor.f.write_char(' ')?;
            if !rules.emit_register(RegSpec::eax(), visitor.f)? {
                visitor.f.write_reg(RegSpec::eax())?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_register(RegSpec::ecx(), visitor.f)? {
                visitor.f.write_reg(RegSpec::ecx())?;
            }
            visitor.f.write_fixed_size(", ")?;
            if !rules.emit_register(RegSpec::edx(), visitor.f)? {
                visitor.f.write_reg(RegSpec::edx())?;
            }
            return Ok(());
        }
        Opcode::INCSSP => {
            // masm wants a d/q suffix on the mnemonic even though these are distinguished by register name..
            let Operand::Register { reg } = instr.operand(0) else {
                panic!("impossible operand");
            };
            if reg.bank == RegisterBank::D {
                out.write_char('d')?;
            } else {
                panic!("impossible register width");
            }
        }
        Opcode::WRSS | Opcode::WRUSS => {
            // masm wants a d/q suffix on the mnemonic, but other operands are the same.
            let Operand::Register { reg } = instr.operand(1) else {
                panic!("impossible operand");
            };
            if reg.bank == RegisterBank::D {
                out.write_char('d')?;
            } else {
                panic!("impossible register width");
            }
        }
        Opcode::PBLENDVB | Opcode::BLENDVPS | Opcode::BLENDVPD | Opcode::SHA256RNDS2 => {
            // masm wants the implicit xmm0 operand as ... explicit.
            out.write_fixed_size(" ")?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };
            if !rules.emit_operand(instr, 0, visitor.f)? {
                instr.visit_operand(0, &mut visitor)?;
            }
            visitor.f.write_str(", ")?;

            if !rules.emit_operand(instr, 1, visitor.f)? {
                if instr.operands[1].is_memory() {
                    visitor.f.write_mem_size_label(instr.mem_size)?;
                    visitor.f.write_fixed_size(" ptr")?;
                    visitor.f.write_char(' ')?;
                    if let Some(prefix) = instr.segment_override_for_op(1) {
                        let name = prefix.name();
                        visitor.f.write_char(name[0] as char)?;
                        visitor.f.write_char(name[1] as char)?;
                        visitor.f.write_fixed_size(":")?;
                    }
                }
                instr.visit_operand(1, &mut visitor)?;
            }
            visitor.f.write_str(", ")?;
            if !rules.emit_register(RegSpec::xmm0(), visitor.f)? {
                visitor.f.write_reg(RegSpec::xmm0())?;
            }

            return Ok(());
        }
        Opcode::LEA => {
            // dumpbin/masm don't want the `<word> ptr` prefix on the memory access here..
            out.write_fixed_size(" ")?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };
            if !rules.emit_operand(instr, 0, visitor.f)? {
                instr.visit_operand(0, &mut visitor)?;
            }
            visitor.f.write_str(", ")?;
            if !rules.emit_operand(instr, 1, visitor.f)? {
                instr.visit_operand(1, &mut visitor)?;
            }
            return Ok(());
        }
        Opcode::PUSHF => {
            if !instr.prefixes.operand_size() {
                out.write_char('d')?;
            }
            return Ok(());
        }
        Opcode::AAM | Opcode::AAD => {
            if instr.imm == 10 {
                // dumpbin doesn't bother with a base here, and this is the only form masm accepts.
                return Ok(());
            }
        }
        Opcode::FCOM | Opcode::FCOMP | Opcode::FICOM | Opcode::FICOMP => {
            // masm does not want the first operand *ever*?
            out.write_fixed_size(" ")?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            if !rules.emit_operand(instr, 1, visitor.f)? {
                if instr.operands[1].is_memory() {
                    visitor.f.write_mem_size_label(instr.mem_size)?;
                    visitor.f.write_fixed_size(" ptr")?;
                    visitor.f.write_char(' ')?;
                    if let Some(prefix) = instr.segment_override_for_op(1) {
                        let name = prefix.name();
                        visitor.f.write_char(name[0] as char)?;
                        visitor.f.write_char(name[1] as char)?;
                        visitor.f.write_fixed_size(":")?;
                    }
                }

                instr.visit_operand(1, &mut visitor)?;
            }
            return Ok(());
        }
        Opcode::FADD | Opcode::FMUL | Opcode::FSUB | Opcode::FSUBR | Opcode::FDIV | Opcode::FDIVR |
        Opcode::FADDP | Opcode::FMULP | Opcode::FSUBRP | Opcode::FSUBP | Opcode::FDIVP | Opcode::FDIVRP |
        Opcode::FIADD | Opcode::FIMUL | Opcode::FISUB | Opcode::FISUBR | Opcode::FIDIV | Opcode::FIDIVR |
        Opcode::FCMOVB | Opcode::FCMOVE | Opcode::FCMOVBE | Opcode::FCMOVU | Opcode::FCMOVNB | Opcode::FCMOVNE | Opcode::FCMOVNBE | Opcode::FCMOVNU |
        Opcode::FUCOMI | Opcode::FCOMI | Opcode::FUCOMIP | Opcode::FCOMIP => {
            out.write_fixed_size(" ")?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            if instr.operands[1].is_memory() {
                // masm does not want to see the implicit st(0).

                if !rules.emit_operand(instr, 1, visitor.f)? {
                    visitor.f.write_mem_size_label(instr.mem_size)?;
                    visitor.f.write_fixed_size(" ptr")?;
                    visitor.f.write_char(' ')?;
                    if let Some(prefix) = instr.segment_override_for_op(1) {
                        let name = prefix.name();
                        visitor.f.write_char(name[0] as char)?;
                        visitor.f.write_char(name[1] as char)?;
                        visitor.f.write_fixed_size(":")?;
                    }
                    instr.visit_operand(1, &mut visitor)?;
                }
            } else {
                // dumpbin writes `st` instead of `st(0)` as the first operand in reg-reg ops, replicate this. masm doesn't care.
                if instr.operands[0] == OperandSpec::RegRRR {
                    if !rules.emit_operand(instr, 0, visitor.f)? {
                        if instr.regs[0] == RegSpec::st0() {
                            visitor.f.write_fixed_size("st")?;
                        } else {
                            instr.visit_operand(0, &mut visitor)?;
                        }
                    }
                    visitor.f.write_fixed_size(", ")?;
                    if !rules.emit_operand(instr, 1, visitor.f)? {
                        instr.visit_operand(1, &mut visitor)?;
                    }
                } else {
                    debug_assert!(instr.operands[1] == OperandSpec::RegRRR);

                    if !rules.emit_operand(instr, 0, visitor.f)? {
                        instr.visit_operand(0, &mut visitor)?;
                    }
                    visitor.f.write_fixed_size(", ")?;
                    if !rules.emit_operand(instr, 1, visitor.f)? {
                        if instr.regs[0] == RegSpec::st0() {
                            visitor.f.write_fixed_size("st")?;
                        } else {
                            instr.visit_operand(1, &mut visitor)?;
                        }
                    }
                };
            }

            return Ok(());
        }
        Opcode::FBLD | Opcode::FLD | Opcode::FILD | Opcode::FXCH | Opcode::FUCOM | Opcode::FUCOMP => {
            // masm does not want to see the implicit st(0).
            out.write_fixed_size(" ")?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            if !rules.emit_operand(instr, 1, visitor.f)? {
                if instr.operands[1].is_memory() {
                    if instr.mem_size == 10 {
                        visitor.f.write_fixed_size("tbyte")?;
                    } else {
                        visitor.f.write_mem_size_label(instr.mem_size)?;
                    }
                    visitor.f.write_fixed_size(" ptr")?;
                    visitor.f.write_char(' ')?;
                    if let Some(prefix) = instr.segment_override_for_op(1) {
                        let name = prefix.name();
                        visitor.f.write_char(name[0] as char)?;
                        visitor.f.write_char(name[1] as char)?;
                        visitor.f.write_fixed_size(":")?;
                    }
                    instr.visit_operand(1, &mut visitor)?;
                } else {
                    if instr.regs[1] == RegSpec::st0() {
                        visitor.f.write_fixed_size("st")?;
                    } else {
                        instr.visit_operand(1, &mut visitor)?;
                    }
                }
            }
            return Ok(());
        }
        Opcode::FBSTP | Opcode::FST | Opcode::FSTP | Opcode::FIST | Opcode::FISTP | Opcode::FISTTP => {
            // masm does not want to see the implicit st(0).
            out.write_fixed_size(" ")?;
            let mut visitor = DisplayingOperandVisitor {
                f: out,
                rules,
            };

            if !rules.emit_operand(instr, 0, visitor.f)? {
                if instr.operands[0].is_memory() {
                    if instr.mem_size == 10 {
                        visitor.f.write_fixed_size("tbyte")?;
                    } else {
                        visitor.f.write_mem_size_label(instr.mem_size)?;
                    }
                    visitor.f.write_fixed_size(" ptr")?;
                    visitor.f.write_char(' ')?;
                    if let Some(prefix) = instr.segment_override_for_op(0) {
                        let name = prefix.name();
                        visitor.f.write_char(name[0] as char)?;
                        visitor.f.write_char(name[1] as char)?;
                        visitor.f.write_fixed_size(":")?;
                    }
                    instr.visit_operand(0, &mut visitor)?;
                } else {
                    instr.visit_operand(0, &mut visitor)?;
                }
            }
            return Ok(());
        }
        _ => {}
    }

    if instr.operand_count > 0 {
        out.write_fixed_size(" ")?;

        let mut size_is_mmword = false;
        for i in 0..instr.operand_count {
            if let Operand::Register { reg } = instr.operand(i) {
                if reg.bank == RegisterBank::MM || reg.bank == RegisterBank::X || reg.bank == RegisterBank::Y {
                    size_is_mmword = true;
                }
            }
        }

        if instr.opcode == Opcode::VPEXTRQ || instr.opcode == Opcode::PEXTRQ || instr.opcode == Opcode::VPINSRQ || instr.opcode == Opcode::PINSRQ || instr.opcode == Opcode::MOVLPD || instr.opcode == Opcode::MOVHPD || instr.opcode == Opcode::CVTSI2SD || instr.opcode == Opcode::MOVQ || instr.opcode == Opcode::CVTSI2SS || instr.opcode == Opcode::MOVNTSD || instr.opcode == Opcode::MOVLPS || instr.opcode == Opcode::MOVHPS {
            // movq: dumpbin is inconsistent on sizes (480f7e is qword, 0f7f is mmword), but masm accepts either.
            // use qword always to match xed etc.
            size_is_mmword = false;
        } else if instr.opcode == Opcode::CVTTSD2SI || instr.opcode == Opcode::CVTSD2SI {
            size_is_mmword = true;
        }

        if instr.operands[0] == OperandSpec::ImmI8 || instr.operands[0] == OperandSpec::ImmI32 {
            if RELATIVE_BRANCHES.contains(&instr.opcode) {
                // relative branch instructions have only one operand, so print this one and we're
                // done. relative branch instructions *also* have a ... relative branch ... as
                // their only operand, so don't `emit_operand()` which would confuse these for a
                // "normal" immediate.
                if rules.emit_branch_addr(instr, instr.imm as i32, out)? {
                    return Ok(());
                }
            }

            if instr.visit_operand(0, &mut RelativeBranchPrinter {
                inst: instr,
                rules,
                out,
            })? {
                return Ok(());
            }
        }

        let mut show_sae = false;
        let mut sae_mode = None;
        let mut displayer = DisplayingOperandVisitor {
            f: out,
            rules,
        };

        if instr.operands[0] == OperandSpec::RegRRR_maskmerge_sae ||
            instr.operands[0] == OperandSpec::RegRRR_maskmerge_sae_noround ||
            instr.operands[0] == OperandSpec::RegMMM_maskmerge_sae_noround {
            show_sae = true;
            if instr.operands[0] == OperandSpec::RegRRR_maskmerge_sae {
                let instr_evex = instr.prefixes.evex_unchecked();
                sae_mode = Some(SaeMode::from(instr_evex.vex().l(), instr_evex.lp()));
            }
        }

        if !rules.emit_operand(instr, 0, displayer.f)? {
            if instr.operands[0 as usize].is_memory() {
                // fxsave and friends get no "XXXword ptr" memory prefix, masm doesn't accept it
                if instr.mem_size != 63 && instr.mem_size != 48 { // masm does not print "m384b" labels.. 
                    if size_is_mmword && instr.mem_size == 8 {
                        displayer.f.write_fixed_size("mmword")?;
                    } else if instr.mem_size == 6 && (instr.opcode == Opcode::JMPF || instr.opcode == Opcode::CALLF) {
                        // "fword" in protected mode instead of "far"..
                        displayer.f.write_fixed_size("fword")?;
                    } else {
                        displayer.f.write_mem_size_label(instr.mem_size)?;
                    }
                    displayer.f.write_fixed_size(" ptr")?;
                    displayer.f.write_char(' ')?;
                }
                if let Some(prefix) = instr.segment_override_for_op(0) {
                    let name = prefix.name();
                    displayer.f.write_char(name[0] as char)?;
                    displayer.f.write_char(name[1] as char)?;
                    displayer.f.write_fixed_size(":")?;
                }
            }

            instr.visit_operand(0 as u8, &mut displayer)?;
        }

        for i in 1..instr.operand_count {
            // don't worry about checking for `instr.operands[i] != Nothing`, it would be a bug to
            // reach that while iterating only to `operand_count`..
            displayer.f.write_fixed_size(", ")?;
            // hint that accessing `inster.operands[i]` can't panic: this is useful for
            // `instr.operands` and the segment selector check after.
            if i >= 4 {
                // Safety: Instruction::operands is a four-element array; operand_count is always
                // low enough that 0..operand_count is a valid index.
                unsafe { unreachable_unchecked(); }
            }

            if rules.emit_operand(instr, i, displayer.f)? {
                // if the rule printed an operand out, continue on to the next one!
                continue;
            }

            if instr.operands[i as usize].is_memory() {
                // fxsave and friends get no "XXXword ptr" memory prefix, masm doesn't accept it
                if instr.mem_size != 63 && instr.mem_size != 48 { // masm does not print "m384b" labels.. 
                    if size_is_mmword && instr.mem_size == 8 {
                        displayer.f.write_fixed_size("mmword")?;
                    } else if instr.mem_size == 6 && (instr.opcode == Opcode::LDS || instr.opcode == Opcode::LES || instr.opcode == Opcode::LFS || instr.opcode == Opcode::LGS || instr.opcode == Opcode::LSS) {
                        // "fword" in protected mode instead of "far"..
                        displayer.f.write_fixed_size("fword")?;
                    } else {
                        displayer.f.write_mem_size_label(instr.mem_size)?;
                    }
                    displayer.f.write_fixed_size(" ptr")?;
                    displayer.f.write_char(' ')?;
                }
                if let Some(prefix) = instr.segment_override_for_op(i) {
                    let name = prefix.name();
                    if instr.opcode != Opcode::MOVS || name != b"ds" {
                        displayer.f.write_char(name[0] as char)?;
                        displayer.f.write_char(name[1] as char)?;
                        displayer.f.write_fixed_size(":")?;
                    }
                }
            }

            instr.visit_operand(i as u8, &mut displayer)?;
            if let Some(evex) = instr.prefixes.evex() {
                if evex.broadcast() && instr.operands[i as usize].is_memory() {
                    let scale = if instr.opcode == Opcode::VCVTPD2PS || instr.opcode == Opcode::VCVTTPD2UDQ || instr.opcode == Opcode::VCVTPD2UDQ || instr.opcode == Opcode::VCVTUDQ2PD || instr.opcode == Opcode::VCVTPS2PD || instr.opcode == Opcode::VCVTQQ2PS || instr.opcode == Opcode::VCVTDQ2PD || instr.opcode == Opcode::VCVTTPD2DQ || instr.opcode == Opcode::VFPCLASSPS || instr.opcode == Opcode::VFPCLASSPD || instr.opcode == Opcode::VCVTNEPS2BF16 || instr.opcode == Opcode::VCVTUQQ2PS || instr.opcode == Opcode::VCVTPD2DQ || instr.opcode == Opcode::VCVTTPS2UQQ || instr.opcode == Opcode::VCVTPS2UQQ || instr.opcode == Opcode::VCVTTPS2QQ || instr.opcode == Opcode::VCVTPS2QQ {
                        if instr.opcode == Opcode::VFPCLASSPS || instr.opcode ==  Opcode::VCVTNEPS2BF16 {
                            if evex.vex().l() {
                                8
                            } else if evex.lp() {
                                16
                            } else {
                                4
                            }
                        } else if instr.opcode == Opcode::VFPCLASSPD {
                            if evex.vex().l() {
                                4
                            } else if evex.lp() {
                                8
                            } else {
                                2
                            }
                        } else {
                            // vcvtpd2ps is "cool": in broadcast mode, it can read a
                            // double-precision float (qword), resize to single-precision,
                            // then broadcast that to the whole destination register. this
                            // means we need to show `xmm, qword [addr]{1to4}` if vector
                            // size is 256. likewise, scale of 8 for the same truncation
                            // reason if vector size is 512.
                            // vcvtudq2pd is the same story.
                            // vfpclassp{s,d} is a mystery to me.
                            if evex.vex().l() {
                                4
                            } else if evex.lp() {
                                8
                            } else {
                                2
                            }
                        }
                    } else {
                        // this should never be `None` - that would imply two
                        // memory operands for a broadcasted operation.
                        if let Some(width) = Operand::from_spec(instr, instr.operands[i as usize - 1]).width() {
                            width / instr.mem_size
                        } else {
                            0
                        }
                    };
                    displayer.f.write_fixed_size("{1to")?;
                    static STRING_LUT: &'static [&'static str] = &[
                        "0", "1", "2", "3", "4", "5", "6", "7", "8",
                        "9", "10", "11", "12", "13", "14", "15", "16",
                    ];
                    unsafe {
                        displayer.f.write_lt_16(STRING_LUT.get_kinda_unchecked(scale as usize))?;
                    }
                    displayer.f.write_char('}')?;
                }
            }
        }

        if show_sae {
            displayer.f.write_char(' ')?;
            if let Some(sae_mode) = sae_mode.as_ref() {
                displayer.f.write_sae_mode(*sae_mode)?;
            } else {
                displayer.f.write_str("{sae}")?;
            }
        }
    }
    Ok(())
}
