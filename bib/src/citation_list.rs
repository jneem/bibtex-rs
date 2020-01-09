use std::collections::HashMap;

use crate::{BString, BStringLc};

#[derive(Debug, Default)]
pub struct CitationList {
    /// The list of all keys, in some possibly relevant order. This is guaranteed not to contain
    /// the key `*` (see [`all_marker`] for an explanation).
    pub keys: Vec<BString>,

    /// A mapping from lower cased keys to the index in `keys`.
    pub key_indices: HashMap<BString, usize>,

    /// In bibtex, a citation list can contain the key `*`, in which case all citations should be
    /// included.  Note that this isn't just a boolean flag: in some situations (and depending on
    /// the style-file), the location of the `*` in the citation list makes a difference.
    ///
    /// In this struct, we remove all instances of `*` from `keys`, and we store the index of the
    /// first (former) occurrence of `*` here.
    pub all_marker: Option<usize>,
}

impl CitationList {
    pub(crate) fn new(keys: Vec<BString>) -> CitationList {
        let mut ret = CitationList::default();
        ret.initialize_with(keys);
        ret
    }


    pub(crate) fn with_all_citations() -> CitationList {
        let mut ret = CitationList::default();
        ret.all_marker = Some(0);
        ret
    }

    pub(crate) fn initialize_with(&mut self, keys: Vec<BString>) {
        debug_assert!(self.keys.is_empty());

        let idx = self.keys.len();
        for k in keys {
            if k == b"*" {
                if self.all_marker.is_none() {
                    self.all_marker = Some(idx);
                }
            } else {
                let lower = k.to_ascii_lowercase();
                self.key_indices.insert(lower, idx);
                self.keys.push(k);
            }
        }
    }

    pub fn contains(&self, key: &BStringLc) -> bool {
        self.all_marker.is_some() || self.key_indices.contains_key(key as &[u8])
    }

    pub fn get_idx(&self, key: &BStringLc) -> Option<usize> {
        self.key_indices.get(key as &[u8]).cloned()
    }

    /// Does this citation list include the `*' citation?
    pub fn has_all(&self) -> bool {
        self.all_marker.is_some()
    }

    pub fn insert(&mut self, key: BString) {
        debug_assert!(!self.contains(&BStringLc::from_bytes(&key)));

        self.key_indices.insert(key.clone(), self.key_indices.len());
        self.keys.push(key);
    }

    /// Not counting `*` if it's present, how many citations are there?
    pub fn len(&self) -> usize {
        self.keys.len()
    }
}

/// `.bib` files can contain cross-references, so when parsing a `.bib` file we need to keep track
/// of which entries were cross-referenced, and how many times. This struct takes care of that
/// bookkeeping.
#[derive(Debug, Default)]
pub struct CrossrefList {
    /// The list of all crossref'ed keys, in order of appearance.
    pub list: CitationList,
    /// For each key in the list above, the number of times it has appeared as a cross-reference.
    /// Although this is very unlikely to happen anyway, these saturate at 255.
    pub crossref_counts: Vec<u8>,
}

// FIXME: which of the parameters here need to be lowercase?
impl CrossrefList {
    pub fn add(&mut self, crossref: &[u8]) {
        if let Some(&idx) = self.list.key_indices.get(crossref) {
            self.crossref_counts[idx] = self.crossref_counts[idx].saturating_add(1);
        } else {
            self.list.insert(crossref.to_vec());
            self.crossref_counts.push(1);
        }
    }

    pub fn count(&self, crossref: &[u8]) -> u8 {
        if let Some(&idx) = self.list.key_indices.get(crossref) {
            self.crossref_counts[idx]
        } else {
            0
        }
    }
}
