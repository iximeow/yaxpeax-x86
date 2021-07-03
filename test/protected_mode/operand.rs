use yaxpeax_x86::protected_mode::{InstDecoder, MemoryAccessSize, Operand, RegSpec};

#[test]
fn register_widths() {
    assert_eq!(Operand::Register(RegSpec::esp()).width(), Some(4));
    assert_eq!(Operand::Register(RegSpec::sp()).width(), Some(2));
    assert_eq!(Operand::Register(RegSpec::cl()).width(), Some(1));
    assert_eq!(Operand::Register(RegSpec::ch()).width(), Some(1));
    assert_eq!(Operand::Register(RegSpec::gs()).width(), Some(2));
}

#[test]
fn memory_widths() {
    // the register operand directly doesn't report a size - it comes from the `Instruction` for
    // which this is an operand.
    assert_eq!(Operand::RegDeref(RegSpec::esp()).width(), None);

    fn mem_size_of(data: &[u8]) -> MemoryAccessSize {
        let decoder = InstDecoder::default();
        decoder.decode_slice(data).unwrap().mem_size().unwrap()
    }

    // and checking the memory size direcly reports correct names
    assert_eq!(mem_size_of(&[0x32, 0x00]).size_name(), "byte");
    assert_eq!(mem_size_of(&[0x66, 0x33, 0x00]).size_name(), "word");
    assert_eq!(mem_size_of(&[0x33, 0x00]).size_name(), "dword");
}
