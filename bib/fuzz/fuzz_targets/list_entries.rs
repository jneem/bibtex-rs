#![no_main]
use libfuzzer_sys::fuzz_target;

use tectonic_tester::BibtexRunner;

fuzz_target!(|data: &[u8]| {
    // For now, we restrict to non-control ASCII input. This is because the tectonic version of
    // bibtex (presumably coming from xetex) and my system version of bibtex (from texlive) differ
    // in their handling of error messages containing weird characters.
    if data.iter().all(|c| c.is_ascii() && !c.is_ascii_control()) {
        let mut runner = BibtexRunner::new();
        runner.set_bib_input(data);
        runner.check_ours();
    }
});
