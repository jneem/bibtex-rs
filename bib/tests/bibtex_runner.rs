use system_bibtex_tester::{list_entries_success, list_entries_failure};

list_entries_success!(basic, "basic.bib");
list_entries_success!(field_warnings, "field_warnings.bib");
list_entries_success!(string_warnings, "string_warnings.bib");

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
list_entries_failure!(key_cases, "key_cases.bib");

list_entries_failure!(with_citation_list, "with_citation_list.bib", vec![&b"key1"[..], b"key2", b"key3"]);

list_entries_success!(crossref_filtering, "crossref_filtering.bib", vec![&b"key1"[..], b"key2", b"key3"]);
list_entries_success!(entry_order, "entry_order.bib", vec![&b"key1"[..], b"key2", b"*", b"key3"]);

