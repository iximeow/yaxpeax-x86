// use crate::long_mode::{OperandSpec, DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::long_mode::{DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::long_mode::{read_modrm, read_E_vex, read_imm_unsigned};

include!("../shared/generated_evex.in");
include!("../shared/evex.in");
