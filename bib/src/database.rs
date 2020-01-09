use std::collections::HashMap;

use crate::{BString, BStringLc, Entry, Parser};
use crate::citation_list::{CitationList, CrossrefList};
use crate::error::{ErrorKind, Problem, ProblemReporter, WarningKind};

/// A Database is an ordered collection of [`Entry`]'s.
///
/// TODO: redo the docs
/// TODO: some of these fields (i.e. entries and extra_entries) are only used for parsers that are
/// storing their own data. Maybe split out a separate struct?
#[derive(Debug)]
pub struct DatabaseBuilder {
    /// This field will not be modified in the course of parsing a database file. It lists the
    /// citations as contained in the .aux file (possibly including an "all citations marker",
    /// introduced by "\citation{*}").
    citation_list: CitationList,

    /// Entries can contain a `crossref` field, like
    /// @article{article_key, title="Blah", crossref=book_key}
    /// @book{book_key, editor="Me"}
    ///
    /// While processing the bibtex file, we keep track of all crossreferenced entries (i.e.
    /// book_key in the example above). Those that are referenced more than `min_crossrefs` times
    /// will appear in the bibliography even if they were not otherwise included.
    min_crossrefs: u8,

    /// This lists the entries that are to be included, but which weren't explicitly on
    /// `citation_list'. There are two possibilities: either `citation_list' contains a * (in which case
    /// everything not on `citation_list' goes here), or `citation_list' doesn't contain a * (in which
    /// case this contains entries that were crossreferenced).
    extra_list: CrossrefList,

    /// The list of all entries that we are interested in, in the order that we encountered them in
    /// the `.bib` file.
    entries: Vec<Entry>,

    /// A map going from (lower-cased) keys to indices in `entries`.
    entry_index: HashMap<BStringLc, usize>,

    /// The string substitutions defined so far. Note that bibtex compresses adjacent whitespace
    /// whenever it does a string substitution. We don't do this, so any spaces should already be
    /// compressed here.
    pub strings: HashMap<BString, BString>,
}

impl DatabaseBuilder {
    fn new() -> DatabaseBuilder {
        DatabaseBuilder {
            citation_list: Default::default(),
            min_crossrefs: 0,
            extra_list: Default::default(),
            entries: Vec::new(),
            entry_index: HashMap::new(),
            strings: HashMap::new(),
        }
    }

    pub fn from_citation_list<I: AsRef<[u8]>, T: IntoIterator<Item=I>>(list: T, min_crossrefs: u8) -> Self {
        let list: Vec<_> = list.into_iter().map(|xs| xs.as_ref().to_vec()).collect();
        let citation_list = CitationList::new(list);

        DatabaseBuilder {
            citation_list,
            min_crossrefs,
            ..DatabaseBuilder::new()
        }
    }

    pub fn with_all_citations() -> Self {
        DatabaseBuilder {
            citation_list: CitationList::with_all_citations(),
            ..DatabaseBuilder::new()
        }
    }

    pub fn cite_list(&self) -> &CitationList {
        &self.citation_list
    }

    pub fn with_initial_strings<T: IntoIterator<Item=(BString, BString)>>(&mut self, strings: T) -> &mut Self {
        self.strings = strings.into_iter().collect();
        self
    }

    pub fn merge_bib_file<R, E>(&mut self, read: R, report: E)
    where
        R: std::io::BufRead,
        E: crate::error::ProblemReporter,
    {
        let mut dummy = DatabaseBuilder::new();
        std::mem::swap(self, &mut dummy);
        let mut parser = Parser::with_db_builder(read, report, dummy);
        // Drive the parser to completion, so that it modifies the DatabaseBuilder that we passed to it.
        parser.entries().count();
        *self = parser.into_db_builder();
    }

    /// Checks whether we have already stored the entry for a given key.
    pub fn contains_entry(&self, key: &BStringLc) -> bool {
        self.entry_index.contains_key(key)
    }

    /// Checks whether a key is an interesting citation for us.
    ///
    /// There are two ways that a key could be an interesting citation: it could be on the original
    /// `cite_list` of interesting citations (this includes the case that `cite_list` contains
    /// `*`), or it could be some key that was crossreferenced.
    pub fn contains_citation(&self, key: &BStringLc) -> bool {
        self.cite_list().contains(key) || self.extra_list.list.contains(key)
    }

    pub fn add_crossref(&mut self, key: &[u8]) {
        debug_assert!(!self.cite_list().has_all());

        self.extra_list.add(key);
    }

    pub fn store_entry(&mut self, entry: &Entry, lc_key: &BStringLc) {
        debug_assert!(!self.entry_index.contains_key(lc_key));

        self.entry_index.insert(lc_key.clone(), self.entries.len());
        self.entries.push(entry.clone());

        if self.cite_list().has_all() {
            self.extra_list.list.insert(lc_key.to_vec());
        } else {
            debug_assert!(self.contains_citation(lc_key));
        }
    }

    // TODO: provide with_entry_type_checker and with_field_name_checker

    // Rearranges the entries so that the appear in the same order as they do in bibtex: first, we
    // include the entries in the order they appear `citation_list`. This continues until either we
    // get to the end of `citation_list` or we hit the `all_marker`, at which point we switch to
    // including entries in the order we found them in the `.bib` file.
    fn rearrange_entries(&mut self) {
        let mut rearranged = Vec::with_capacity(self.entries.len());

        // We remove elements of self.entries one-by-one, and this vec tells of which of them were
        // taken already.
        let mut taken = vec![false; self.entries.len()];
        let end_idx = self.cite_list().all_marker.unwrap_or(self.cite_list().len());
        for key in &self.citation_list.keys[..end_idx] {
            let entry_idx = self.entry_index[&BStringLc::from_bytes(key)];
            let mut entry = Entry::empty();
            std::mem::swap(&mut entry, &mut self.entries[entry_idx]);
            rearranged.push(entry);
            taken[entry_idx] = true;
        }

        for (entry, gone) in self.entries.drain(..).zip(taken.into_iter()) {
            if !gone {
                rearranged.push(entry);
            }
        }

        std::mem::swap(&mut rearranged, &mut self.entries);
    }

    // Whenever an entry (call it the child entry) has a crossref field (pointing to a parent
    // entry), copy to the child all the fields that are present in the parent but missing in the
    // child.
    //
    // Although it is technically illegal to nest cross-references, we don't try to detect that
    // here: we just go over all the entries in order. So for example, if a parent has its own
    // parent, then the grandparent's fields will get copied to the child if and only if the parent
    // appears before the child.
    //
    // We also don't complain about crossrefs to parents that are missing; that is taken care of in
    // `subtract_crossref_info`.
    fn add_crossref_info(&mut self) {
        for i in 0..self.entries.len() {
            if let Some(parent_key) = self.entries[i].fields.get(&b"crossref"[..]) {
                if let Some(&parent_idx) = self.entry_index.get(&BStringLc::from_bytes(parent_key)) {
                    // Swap out the parent so that we can modify the child.
                    let mut parent = Entry::empty();
                    std::mem::swap(&mut parent, &mut self.entries[parent_idx]);

                    let child = &mut self.entries[i];
                    for (key, val) in &parent.fields {
                        if !child.fields.contains_key(key) {
                            child.fields.insert(key.clone(), val.clone());
                        }
                    }

                    std::mem::swap(&mut parent, &mut self.entries[parent_idx]);
                }
            }
        }
    }

    // Delete every crossref field if we're not planning to include the entry that it references.
    // (If we never found the entry that it references, also issue a warning.)
    fn subtract_crossref_info(&mut self, report: &mut impl ProblemReporter) {
        for child_idx in 0..self.entries.len() {
            if let Some(parent_key) = self.entries[child_idx].fields.get(&b"crossref"[..]) {
                let child_key = &self.entries[child_idx].key;
                let parent_lc = BStringLc::from_bytes(parent_key);

                // For us to want to include the parent, first of all we need to have found it in
                // the database. In addition, it either needs to have been cited enough times, or
                // it needs to have been something that was in our original cite list anyway.
                let parent_found = self.entry_index.contains_key(&parent_lc);
                let enough_cites = self.extra_list.count(parent_key) >= self.min_crossrefs;
                let on_cite_list = self.citation_list.contains(&parent_lc);

                if !parent_found {
                    report.report(&Problem::from_error_no_input(ErrorKind::BadCrossReference { child: child_key.clone(), parent: parent_key.clone() }));
                }

                if let Some(&parent_idx) = self.entry_index.get(&parent_lc) {
                    if self.entries[parent_idx].fields.contains_key(&b"crossref"[..]) {
                        report.report(&Problem::from_warning_no_input(WarningKind::NestedCrossRef { child: child_key.clone(), parent: parent_key.clone() }));
                    }
                }

                if !parent_found || (!enough_cites && !on_cite_list) {
                    self.entries[child_idx].fields.remove(&b"crossref"[..]);
                }
            }
        }
    }

    fn complain_about_missing_entries(&self, report: &mut impl ProblemReporter) {
        for key in &self.citation_list.keys {
            if !self.contains_entry(&BStringLc::from_bytes(key)) {
                report.report(&Problem::from_warning_no_input(WarningKind::MissingEntry(key.clone())));
            }
        }
    }

    fn remove_insufficiently_cited_entries(&mut self) {
        let mut lc_buf = BStringLc::new();
        let lc_buf = &mut lc_buf;
        let cite_list = &self.citation_list;
        let extra_list = &self.extra_list;
        let min_crossrefs = self.min_crossrefs;
        let mut keep = |key: &BString| {
            lc_buf.set_from_bytes(key);
            cite_list.contains(&lc_buf) || extra_list.count(&lc_buf) >= min_crossrefs
        };

        // TODO: this could use drain_filter, once it stabilizes.
        let entries = self.entries.drain(..)
            .filter(|e| keep(&e.key))
            .collect::<Vec<_>>();
        self.entries = entries;
    }

    /// Consumes this `DatabaseBuilder`, producing an ordered list of entries.
    ///
    /// This function also performs some post-processing (e.g. substituting crossreferences) that
    /// have the potential to produce errors, hence it needs an `ErrorReporter`.
    pub fn into_entries<R: ProblemReporter>(mut self, mut report: R) -> Vec<Entry> {
        self.rearrange_entries();
        self.add_crossref_info();
        self.subtract_crossref_info(&mut report);
        self.complain_about_missing_entries(&mut report);
        self.remove_insufficiently_cited_entries();
        self.entries
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn database_merge() {
        let bib_data1 = r#"
        @book{Melville:51,
            title = "Moby Dick",
            author = "Herman Melville",
        }"#;
        let bib_data2 = r#"
        @article{Shannon:48,
            title = "A Mathematical Theory of Communication",
            author = "Shannon, C. E.",
        }"#;
        
        let mut db = DatabaseBuilder::with_all_citations();
        db.merge_bib_file(bib_data1.as_bytes(), ());
        db.merge_bib_file(bib_data2.as_bytes(), ());
        assert_eq!(&db.entries[0].fields[&b"author"[..]], b"Herman Melville");
        assert_eq!(&db.entries[1].fields[&b"author"[..]], b"Shannon, C. E.");
    }
}
