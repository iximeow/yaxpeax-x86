use yaxpeax_x86::long_mode::{Operand, RegSpec};

#[test]
fn register_widths() {
    assert_eq!(Operand::Register(RegSpec::rsp()).width(), Some(8));
    assert_eq!(Operand::Register(RegSpec::esp()).width(), Some(4));
    assert_eq!(Operand::Register(RegSpec::sp()).width(), Some(2));
    assert_eq!(Operand::Register(RegSpec::cl()).width(), Some(1));
    assert_eq!(Operand::Register(RegSpec::ch()).width(), Some(1));
    assert_eq!(Operand::Register(RegSpec::gs()).width(), Some(2));
}

#[test]
fn memory_widths() {
    assert_eq!(Operand::RegDeref(RegSpec::rsp()).width(), None);
}
