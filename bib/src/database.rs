use std::collections::HashMap;

use crate::{BString, BStringLc, Entry, Parser};
use crate::citation_list::{CitationList, CrossrefList};
use crate::error::{DatabaseWarningKind, ProblemReporter};

/// A `DatabaseBuilder` creates a collection of [`Entry`]s by reading one or more `bib` files.
/// Compared to the simple `bib`-reading interface provided by [`Parser`], this one is more
/// complete and correct, because
///
/// - it supports merging multiple `bib` files,
/// - it resolves cross-references, and
/// - it correctly reports warnings for missing entries.
///
/// ```
/// use bib::DatabaseBuilder;
/// let bib_data1 = r#"
/// @book{Melville:51,
///     title = "Moby Dick",
///     author = "Herman Melville",
/// }
///
/// @article{Shannon:48,
///     title = "A Mathematical Theory of Communication",
///     author = "Shannon, C. E.",
///     crossref = "BSTJ",
/// }"#;
/// let bib_data2 = r#"
/// @article{BSTJ,
///     journal = "Bell System Technical Journal"
/// }"#;
///
/// // One of two constructors. The other one, `DatabaseBuilder::from_citation_list`, allows for
/// // filtering out uninteresting citations.
/// let mut db = DatabaseBuilder::with_all_citations();
///
/// db.merge_bib_file(bib_data1.as_bytes(), ());
/// db.merge_bib_file(bib_data2.as_bytes(), ());
/// let entries = db.into_entries(());
///
/// assert_eq!(&entries[0].fields[&b"author"[..]], b"Herman Melville");
/// assert_eq!(&entries[1].fields[&b"author"[..]], b"Shannon, C. E.");
/// // The "journal" field gets merged in from BSTJ.
/// assert_eq!(&entries[1].fields[&b"journal"[..]], b"Bell System Technical Journal");
/// ```
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

    /// This is a callback providing some context that normally comes from `.bst` file: what are
    /// the admissible entry names? (Like article, book, etc.)
    pub(crate) entry_type_checker: Option<Box<dyn FnMut(&[u8]) -> bool>>,

    /// This is a callback providing some context that normally comes from `.bst` file: what are
    /// the admissible field names? (Like author, title, etc.)
    pub(crate) field_name_checker: Option<Box<dyn FnMut(&[u8]) -> bool>>,

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
            entry_type_checker: None,
            field_name_checker: None,
            strings: HashMap::new(),
        }
    }

    /// Creates a `DatabaseBuilder` from a list of citations. When merging in `bib` files with
    /// [`DatabaseBuilder::merge_bib_file`], it will filter out uninteresting entries. Note that
    /// "uninteresting" is a bit more complicated than "not on the list", for two reasons:
    ///
    ///  - The list of citations is allowed to include the special citation `"*"`, in which case
    ///    nothing will be filtered out.
    ///  - If entries corresponding to citations that are on the list crossreference other
    ///    entries, then those other entries will be included as long as they are crossreferenced
    ///    at least `min_crossrefs` times.
    ///
    /// In addition to filtering out uninteresting citations, the citation list also affects the
    /// order in which the citations appear (in the return value of
    /// [`DatabaseBuilder::into_entries`]). There are two cases, depending on whether the special
    /// citation `"*"` is present:
    ///
    ///  - If `"*"` is on the citation list, then all entries before the `"*"` come first, in the
    ///    same order that they appear on the citation list. After that come all the other entries,
    ///    in the order that we encountered them in the `bib` files.
    ///  - If `"*"` is not on the citation list, then the entries on the citation list come first
    ///    (in the order that they appear on the citation list). After that come all the entries
    ///    that are included because they were crossreferenced, in the order that we encountered
    ///    them in the `bib` files.
    ///
    /// ```
    /// use bib::DatabaseBuilder;
    ///
    ///
    /// let mut database = DatabaseBuilder::from_citation_list(&[&b"key1"[..], b"key2", b"*"], 2);
    /// let bib_data = r#"
    /// @article{other1, }
    /// @article{key2, }
    /// @article{key1, }
    /// @article{other2, }
    /// "#;
    /// database.merge_bib_file(bib_data.as_bytes(), ());
    ///
    /// // Because of the citation list we used above, key1 and key2 will come first, followed by
    /// // the other entries in order of appearance.
    /// let entries = database.into_entries(());
    /// assert_eq!(&entries[0].key, b"key1");
    /// assert_eq!(&entries[1].key, b"key2");
    /// assert_eq!(&entries[2].key, b"other1");
    /// assert_eq!(&entries[3].key, b"other2");
    /// ```
    ///
    /// The behavior of this function is designed to make it easy to replicate bibtex's behavior,
    /// which is achieved by taking all of the `\citation{..}` commands in the `.aux` file and
    /// passing them to this constructor in the same order that they appeared in that file.
    /// (Also, bibtex defaults to `2` for `min_crossrefs`.)
    pub fn from_citation_list<I: AsRef<[u8]>, T: IntoIterator<Item=I>>(list: T, min_crossrefs: u8) -> Self {
        let list: Vec<_> = list.into_iter().map(|xs| xs.as_ref().to_vec()).collect();
        let citation_list = CitationList::new(list);

        DatabaseBuilder {
            citation_list,
            min_crossrefs,
            ..DatabaseBuilder::new()
        }
    }

    /// Creates a `DatabaseBuilder` that doesn't filter out any entries. This is equivalent to
    /// `DatabaseBuilder::from_citation_list(&[&b"*"[..]])`.
    pub fn with_all_citations() -> Self {
        DatabaseBuilder {
            citation_list: CitationList::with_all_citations(),
            ..DatabaseBuilder::new()
        }
    }

    /// Registers a function for checking whether the entries are of a known type.
    ///
    /// The provided function `f` will be called on every entry type encountered in the `bib` file
    /// (common entry types include `"article"`, `"book"`, etc.), and if `f` returns false then the
    /// entry will be skipped and a warning will be issued.
    pub fn with_entry_type_checker<F: FnMut(&[u8]) -> bool + 'static>(&mut self, f: F) -> &mut Self {
        self.entry_type_checker = Some(Box::new(f));
        self
    }

    /// Registers a function for checking whether the field names are known.
    ///
    /// The provided function `f` will be called on every entry field name encountered in the `bib`
    /// file (common field names include `"author"`, `"title"`, etc.), and if `f` returns false
    /// then the field will be skipped.
    pub fn with_field_name_checker<F: FnMut(&[u8]) -> bool + 'static>(&mut self, f: F) -> &mut Self {
        self.field_name_checker = Some(Box::new(f));
        self
    }

    pub(crate) fn cite_list(&self) -> &CitationList {
        &self.citation_list
    }

    /// Registers a set of string replacements, like in the bibtex `@string` command.
    pub fn with_initial_strings<T: IntoIterator<Item=(BString, BString)>>(&mut self, strings: T) -> &mut Self {
        self.strings = strings.into_iter().collect();
        self
    }

    /// Merges a `bib` file into this database.
    ///
    /// Any interesting entries from the `bib` file will be added to the database, and any
    /// `@string` definitions will be remembered.
    ///
    /// `report` is used for reporting any errors or warnings encountered while reading the `bib`
    /// file.
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
    pub(crate) fn contains_entry(&self, key: &BStringLc) -> bool {
        self.entry_index.contains_key(key)
    }

    /// Checks whether a key is an interesting citation for us.
    ///
    /// There are two ways that a key could be an interesting citation: it could be on the original
    /// `cite_list` of interesting citations (this includes the case that `cite_list` contains
    /// `*`), or it could be some key that was crossreferenced.
    pub(crate) fn contains_citation(&self, key: &BStringLc) -> bool {
        self.cite_list().contains(key) || self.extra_list.list.contains(key)
    }

    pub(crate) fn add_crossref(&mut self, key: &[u8]) {
        debug_assert!(!self.cite_list().has_all());

        self.extra_list.add(key);
    }

    pub(crate) fn store_entry(&mut self, entry: &Entry, lc_key: &BStringLc) {
        debug_assert!(!self.entry_index.contains_key(lc_key));

        self.entry_index.insert(lc_key.clone(), self.entries.len());
        self.entries.push(entry.clone());

        if self.cite_list().has_all() {
            self.extra_list.add(lc_key);
        } else {
            debug_assert!(self.contains_citation(lc_key));
        }
    }

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
            if let Some(&entry_idx) = self.entry_index.get(&BStringLc::from_bytes(key)) {
                let mut entry = Entry::empty();
                std::mem::swap(&mut entry, &mut self.entries[entry_idx]);
                rearranged.push(entry);
                taken[entry_idx] = true;
            }
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
                let enough_cites = self.citation_list.has_all() || self.extra_list.count(parent_key) >= self.min_crossrefs;
                let on_cite_list = self.citation_list.contains(&parent_lc);

                if !parent_found {
                    report.crossref_error(&child_key, &parent_key);
                }

                if let Some(&parent_idx) = self.entry_index.get(&parent_lc) {
                    if self.entries[parent_idx].fields.contains_key(&b"crossref"[..]) {
                        report.database_warning(&DatabaseWarningKind::NestedCrossRef { child: child_key.clone(), parent: parent_key.clone() });
                    }
                }

                if !parent_found || (!enough_cites && !on_cite_list) {
                    self.entries[child_idx].fields.remove(&b"crossref"[..]);
                }
            }
        }
    }

    fn complain_about_missing_entries(&self, report: &mut impl ProblemReporter) {
        for key in self.citation_list.keys.iter().chain(self.extra_list.list.keys.iter()) {
            if !self.contains_entry(&BStringLc::from_bytes(key)) {
                report.database_warning(&DatabaseWarningKind::MissingEntry(key.clone()));
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
