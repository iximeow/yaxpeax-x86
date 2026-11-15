use yaxpeax_x86::long_mode::{register_class, RegSpec};
use std::collections::{BTreeMap, HashMap};

#[test]
fn test_ord() {
    let _: BTreeMap<RegSpec, u64> = BTreeMap::new();
}

#[test]
fn test_hash() {
    let _: HashMap<RegSpec, u64> = HashMap::new();
}

#[cfg(feature="fmt")]
#[test]
fn test_labels() {
    assert_eq!(RegSpec::rip().name(), "rip");
    assert_eq!(RegSpec::eip().name(), "eip");
    assert_eq!(RegSpec::rflags().name(), "rflags");
    assert_eq!(RegSpec::rbp().name(), "rbp");
    assert_eq!(RegSpec::gs().name(), "gs");
    assert_eq!(RegSpec::al().name(), "al");
}

#[cfg(feature="fmt")]
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

// this should compile.
#[test]
fn match_bank_kind() {
    match RegSpec::al().class() {
        register_class::X => {
            panic!("al is an xmm register? don't think so");
        }
        register_class::B => {
            println!("al is a byte register");
        }
        other => {
            panic!("unknown register kind: {:?}", other);
        }
    }
}

#[cfg(feature="use-serde")]
#[test]
fn regspec_deserialize_is_invariant_preserving() {
    // RegisterBank is not public, so we can't use it in the test. instead, use the serialized form
    // of the enum and just splat it into the json by hand.
    struct TestCase {
        bank: &'static str,
        // the lowest valid register number for this bank
        low: u8,
        // the highest valid register number for this bank (inclusive)
        high: u8,
    }

    const DESERIALIZE_LIMITS: [TestCase; 16] = [
        TestCase { bank: "Q", low: 0, high: 15 },
        TestCase { bank: "D", low: 0, high: 15 },
        TestCase { bank: "W", low: 0, high: 15 },
        TestCase { bank: "rB", low: 4, high: 15 },
        TestCase { bank: "B", low: 0, high: 7 },

        TestCase { bank: "X", low: 0, high: 31 },
        TestCase { bank: "Y", low: 0, high: 31 },
        TestCase { bank: "Z", low: 0, high: 31 },

        TestCase { bank: "K", low: 0, high: 7 },

        TestCase { bank: "ST", low: 0, high: 7 },
        TestCase { bank: "MM", low: 0, high: 7 },

        TestCase { bank: "S", low: 0, high: 5 },

        TestCase { bank: "EIP", low: 0, high: 0 },
        TestCase { bank: "RIP", low: 0, high: 0 },
        TestCase { bank: "EFlags", low: 0, high: 0 },
        TestCase { bank: "RFlags", low: 0, high: 0 },
    ];

    for TestCase { bank, low, high } in DESERIALIZE_LIMITS.iter() {
        for i in 0..=255 {
            let json = format!("{{ \"bank\": \"{}\", \"num\": {} }}", bank, i);
            let res: Result<RegSpec, serde_json::Error> = serde_json::from_str(&json);
            if i >= *low && i <= *high {
                assert!(res.is_ok(), "deserializing bank {} register {} should succeed", bank, i);
            } else {
                assert!(res.is_err(), "deserializing bank {} register {} should fail", bank, i);
            }
        }
    }
}
