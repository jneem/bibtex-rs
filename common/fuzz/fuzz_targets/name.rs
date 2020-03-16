#![no_main]

use bstr::{BString, ByteSlice};
use libfuzzer_sys::fuzz_target;
use system_bibtex_tester::NameChecker;

fuzz_target!(|data: &[u8]| {
    // We don't currently support strings containing '"' or newlines; see system_bibtex_tester.
    if data.is_empty() || data.iter().any(|&c| c == b'"' || c == b'\n' || c == b'\r' ||  !c.is_ascii()) {
        return;
    }

    // Because of (what I believe is) a bug in bibtex, we don't support strings with unbalanced braces.
    let mut brace_level = 0;
    for &ch in data {
        match ch {
            b'{' => {
                brace_level += 1;
            }
            b'}' => {
                if brace_level == 0 {
                    return;
                } else {
                    brace_level -= 1;
                }
            }
            _ => {}
        }
    }
    if brace_level > 0 {
        return;
    }

    let mut ch = NameChecker::new();
    ch.set_names(data);

    // bibtex sometimes inserts ties instead of spaces between the tokens in part of the name (to
    // be precise, it does it between the last two tokens, and between the first two tokens if
    // the first token is "too short". We don't want to test this functionality here (only the
    // name-splitting part), and so we simply get rid of all ties.
    let reference = BString::from(ch.reference_output().replace("~", " "));
    let ours = BString::from(ch.our_output().replace("~", " "));
    assert_eq!(reference, ours);
});
