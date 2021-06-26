// use crate::long_mode::{OperandSpec, DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::long_mode::{DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::long_mode::{read_modrm, read_E, read_E_xmm, read_E_ymm, read_E_zmm, read_imm_unsigned};

include!("../shared/generated_evex.in");
include!("../shared/evex.in");
