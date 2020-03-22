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
