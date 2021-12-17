// use crate::long_mode::{OperandSpec, DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::long_mode::{Arch, DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::long_mode::{read_modrm, read_E_vex, read_imm_unsigned};
use yaxpeax_arch::Reader;

const DEFAULT_EVEX_REGISTER_SIZE: RegisterBank = RegisterBank::Q;
const DEFAULT_EVEX_REGISTER_WIDTH: u8 = 8;

fn isa_has_qwords() -> bool {
    true
}

fn apply_disp_scale(inst: &mut Instruction) {
    inst.disp = ((inst.disp as i64) * (inst.mem_size as i64)) as u64;
}

include!("../shared/generated_evex.in");
include!("../shared/evex.in");
