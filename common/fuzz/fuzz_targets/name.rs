#![no_main]

use libfuzzer_sys::fuzz_target;
use system_bibtex_tester::NameChecker;

fuzz_target!(|data: &[u8]| {
    // We don't currently support strings containing '"' or '\n'; see system_bibtex_tester.
    if data.is_empty() || data.iter().any(|&c| c == b'"' || c == b'\n' ||  !c.is_ascii()) {
        return;
    }

    let mut ch = NameChecker::new();
    ch.set_names(data);
    assert_eq!(ch.reference_output(), ch.our_output());
});
