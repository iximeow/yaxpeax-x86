use yaxpeax_x86::long_mode::RegSpec;
use std::collections::{BTreeMap, HashMap};

#[test]
fn test_ord() {
    let _: BTreeMap<RegSpec, u64> = BTreeMap::new();
}

#[test]
fn test_hash() {
    let _: HashMap<RegSpec, u64> = HashMap::new();
}

#[test]
fn test_labels() {
    assert_eq!(RegSpec::rip().name(), "rip");
    assert_eq!(RegSpec::eip().name(), "eip");
    assert_eq!(RegSpec::rflags().name(), "rflags");
    assert_eq!(RegSpec::rbp().name(), "rbp");
    assert_eq!(RegSpec::gs().name(), "gs");
    assert_eq!(RegSpec::al().name(), "al");
}

#[test]
fn test_bank_names() {
    assert_eq!(RegSpec::al().class().name(), "byte");
    assert_eq!(RegSpec::r8b().class().name(), "rex-byte");
    assert_eq!(RegSpec::ax().class().name(), "word");
    assert_eq!(RegSpec::eax().class().name(), "dword");
    assert_eq!(RegSpec::rax().class().name(), "qword");
    assert_eq!(RegSpec::fs().class().name(), "segment");
    assert_eq!(RegSpec::eflags().class().name(), "eflags");
    assert_eq!(RegSpec::rflags().class().name(), "rflags");
    assert_eq!(RegSpec::eip().class().name(), "eip");
    assert_eq!(RegSpec::rip().class().name(), "rip");
    assert_eq!(RegSpec::st0().class().name(), "x87-stack");
    assert_eq!(RegSpec::mm0().class().name(), "mmx");
    assert_eq!(RegSpec::xmm0().class().name(), "xmm");
    assert_eq!(RegSpec::ymm0().class().name(), "ymm");
    assert_eq!(RegSpec::zmm0().class().name(), "zmm");
}
