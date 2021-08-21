use yaxpeax_x86::real_mode::{InstDecoder, Operand, RegSpec};
use yaxpeax_x86::MemoryAccessSize;

#[test]
fn test_implied_memory_width() {
    fn mem_size_of(data: &[u8]) -> Option<u8> {
        let decoder = InstDecoder::default();
        decoder.decode_slice(data).unwrap().mem_size().unwrap().bytes_size()
    }

    // test push, pop, call, and ret
    assert_eq!(mem_size_of(&[0xc3]), Some(2));
    assert_eq!(mem_size_of(&[0xe8, 0x11, 0x22, 0x33, 0x44]), Some(2));
    assert_eq!(mem_size_of(&[0x50]), Some(2));
    assert_eq!(mem_size_of(&[0x58]), Some(2));
    assert_eq!(mem_size_of(&[0x66, 0x50]), Some(2));
    assert_eq!(mem_size_of(&[0x66, 0x58]), Some(2));
    assert_eq!(mem_size_of(&[0xff, 0xf0]), Some(2));
    assert_eq!(mem_size_of(&[0x66, 0xff, 0xf0]), Some(4));
}
