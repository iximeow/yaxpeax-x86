use super::{BMI1, BMI2, DecodeError, InstDecoder, Instruction, Opcode};

crate::isa_settings::gen_arch_isa_settings!(Instruction, Opcode, DecodeError, InstDecoder);
