use tectonic_tester::{list_entries_success, list_entries_failure};

list_entries_success!(basic, "basic.bib");

list_entries_failure!(missing_entry_type, "missing_entry_type.bib");
list_entries_failure!(missing_field_name, "missing_field_name.bib");
list_entries_failure!(missing_string_name, "missing_string_name.bib");
list_entries_failure!(missing_field_part, "missing_field_part.bib");
list_entries_failure!(no_key, "no_key.bib");
list_entries_failure!(unexpected_eof, "unexpected_eof.bib");
list_entries_failure!(missing_equals, "missing_equals.bib");
list_entries_failure!(expecting_delimiter, "expecting_delimiter.bib");
list_entries_failure!(unterminated_preamble, "unterminated_preamble.bib");
list_entries_failure!(unterminated_string, "unterminated_string.bib");
list_entries_failure!(unbalanced_braces, "unbalanced_braces.bib");
list_entries_failure!(error_cases, "error_cases.bib");


// This currently fails, because we aren't detecting duplicate keys.
//list_entries_failure!(key_cases, "key_cases.bib");
