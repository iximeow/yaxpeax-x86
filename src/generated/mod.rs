pub(crate) mod opcode;
pub(crate) mod imp;

pub(crate) mod real_mode {
  pub(crate) use super::opcode::real_mode::Opcode as Opcode;
  pub(crate) use super::imp::real_mode::revise_instruction as revise_instruction;
}
pub(crate) mod protected_mode {
  pub(crate) use super::opcode::protected_mode::Opcode as Opcode;
  pub(crate) use super::imp::protected_mode::revise_instruction as revise_instruction;
}
pub(crate) mod long_mode {
  pub(crate) use super::opcode::long_mode::Opcode as Opcode;
  pub(crate) use super::imp::long_mode::revise_instruction as revise_instruction;
}
