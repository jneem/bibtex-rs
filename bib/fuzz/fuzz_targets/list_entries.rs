#![no_main]
use libfuzzer_sys::fuzz_target;

use tectonic_tester::BibtexRunner;

fuzz_target!(|data: &[u8]| {
    let mut runner = BibtexRunner::new();
    runner.set_bib_input(data);
    runner.check_ours();
});
