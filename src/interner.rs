/// ! a very simple string interner
use std::borrow::Borrow;
use std::collections::HashSet;
use std::rc::Rc;
use std::hash::BuildHasherDefault;
use std::mem;
use fnv::FnvHasher;

#[cfg(test)]
use std::convert::From;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RcStr(Rc<String>);

impl Borrow<str> for RcStr {
    fn borrow(&self) -> &str {
        &self.0
    }
}

/// force compiler complaints when this is being done in production code
/// this is only for test code to allow `assert_eq`s
#[cfg(test)]
impl<T: Into<String>> From<T> for RcStr {
    fn from(t: T) -> RcStr {
        RcStr(Rc::new(t.into()))
    }
}

pub struct Interner {
    strs: HashSet<RcStr, BuildHasherDefault<FnvHasher>>,
}

impl Interner {
    pub fn new() -> Interner {
        Interner { strs: HashSet::default() }
    }

    /// checks if a string is already cached in an owned form, reeuse it or
    /// clone it if necessary
    /// returns the ref counted reference to the string
    pub fn intern(&mut self, s: &str) -> RcStr {
        if let Some(str_) = self.strs.get(s).cloned() {
            return str_;
        }
        let str_ = RcStr(Rc::new(s.to_owned()));
        self.strs.insert(str_.clone());
        str_
    }

    /// get a new interner instance only containing interned strings
    /// which are also referenced externally
    pub fn compact(self) -> Interner {
        let mut new = Interner::new();
        for key in self.strs {
            let weak_ref = Rc::downgrade(&key.0);
            mem::drop(key);
            if let Some(x) = weak_ref.upgrade() {
                let key = RcStr(x);
                new.strs.insert(key);
            }
        }
        new
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_interner() {
        let mut str2 = {
            let mut interner = Interner::new();
            let mut str1 = interner.intern("test");
            assert_eq!(Rc::get_mut(&mut str1.0), None);
            interner.intern("test")
        };
        assert_eq!(Rc::get_mut(&mut str2.0), Some(&mut "test".to_owned()));
    }
}
