use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InternedString(u32);

pub struct InternTable {
    next_id: u32,
    // This is for looking up the strings by id. This representation could be more compact (using a
    // single array with indices into it), but this is probably fine for now.
    strings: Vec<Vec<u8>>,
    // This is for looking up the id associated with a string.
    ids: HashMap<Vec<u8>, u32>,
}

impl InternTable {
    pub fn new() -> InternTable {
        InternTable {
            next_id: 0,
            strings: Vec::new(),
            ids: HashMap::new(),
        }
    }

    pub fn insert(&mut self, s: &[u8]) -> InternedString {
        // We're doing a double-lookup in the table. This isn't great, but using the entry API
        // requires cloning the string first, which also isn't great.
        if let Some(val) = self.ids.get(s) {
            InternedString(*val)
        } else {
            let ret = InternedString(self.next_id);
            self.next_id += 1;
            self.ids.insert(s.to_owned(), ret.0);
            self.strings.push(s.to_owned());
            ret
        }
    }

    pub fn string(&self, s: &InternedString) -> &[u8] {
        &self.strings[s.0 as usize]
    }
}

impl Default for InternTable {
    fn default() -> InternTable {
        InternTable::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert() {
        let mut tab = InternTable::new();
        let abc = tab.insert(b"abc");
        let _def = tab.insert(b"def");
        assert_eq!(tab.insert(b"abc"), abc);
        assert!(tab.insert(b"def") != abc);
    }
}
