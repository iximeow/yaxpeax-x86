use yaxpeax_arch::{Decoder, LengthedInstruction};
use yaxpeax_x86::long_mode::{DecodeError, InstDecoder, Opcode};

#[test]
fn register_widths() {
    assert_eq!(Operand::Register(RegSpec::esp()).width(), 4);
    assert_eq!(Operand::Register(RegSpec::sp()).width(), 2);
    assert_eq!(Operand::Register(RegSpec::cl()).width(), 1);
    assert_eq!(Operand::Register(RegSpec::ch()).width(), 1);
    assert_eq!(Operand::Register(RegSpec::gs()).width(), 2);
}

#[test]
fn memory_widths() {
    assert_eq!(Operand::RegDeref(RegSpec::rsp()).width(), 4);
}
