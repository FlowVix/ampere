use std::{cell::RefCell, rc::Rc};

use ahash::AHashMap;

use super::value::{StoredValue, ValueRef};

#[derive(Debug, Clone)]
pub struct Scope {
    pub vars: AHashMap<String, ValueRef>,
    pub parent: Option<ScopeRef>,
}

#[derive(Debug, Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct ScopeRef(Rc<RefCell<Scope>>);

impl ScopeRef {
    pub fn new(v: Scope) -> Self {
        Self(Rc::new(RefCell::new(v)))
    }
    pub fn derived(&self) -> Self {
        Self::new(Scope {
            vars: AHashMap::new(),
            parent: Some(self.clone()),
        })
    }
}

impl Scope {
    pub fn get_var(&self, name: &str) -> Option<ValueRef> {
        match self.vars.get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.parent {
                Some(p) => p.borrow().get_var(name),
                None => None,
            },
        }
    }
}
