use std::collections::HashMap;

#[derive(Default)]
pub struct CitationList {
    /// The list of all keys, in some possibly relevant order.
    pub keys: Vec<Vec<u8>>,
    /// A mapping from lower cased keys to the index in `keys`.
    pub key_indices: HashMap<Vec<u8>, usize>,
    /// A citation list can contain the key `*`, in which case all citations should be included.
    /// Note that this isn't just a boolean flag: in some situations (and depending on the
    /// style-file), the location of the `*` in the citation list makes a difference. `all_marker`,
    /// if set, stores the index of the first occurrence of `*` in the citation list.
    pub all_marker: Option<usize>,
}

impl CitationList {
    pub(crate) fn initialize_with(&mut self, keys: Vec<Vec<u8>>) {
        debug_assert!(self.keys.is_empty());

        self.keys = keys;
        for (i, k) in self.keys.iter().enumerate() {
            let lower = k.to_ascii_lowercase();
            self.key_indices.insert(lower, i);

            if self.all_marker.is_none() && k == b"*" {
                self.all_marker = Some(i);
            }
        }
    }

    pub fn contains<T: AsRef<[u8]>>(&self, key: T) -> bool {
        // TODO: we could reduce allocations by storing a reusable buffer for the lowercase
        // conversion.
        self.all_marker.is_some() || self.key_indices.contains_key(&key.as_ref().to_ascii_lowercase())
    }
}

/// `.bib` files can contain cross-references, so when parsing a `.bib` file we need to keep track
/// of which entries were cross-referenced, and how many times. This struct takes care of that
/// bookkeeping.
#[derive(Default)]
pub(crate) struct CrossrefList {
    /// The list of all crossref'ed keys, in order of appearance.
    pub list: CitationList,
    /// For each key in the list above, the number of times it has appeared as a cross-reference.
    /// Although this is very unlikely to happen anyway, these saturate at 255.
    pub crossref_counts: Vec<u8>,
}

