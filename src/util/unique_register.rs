use std::mem::MaybeUninit;

use ahash::AHashMap;

#[derive(Debug, Clone)]
pub struct UniqueRegister<T> {
    map: AHashMap<T, usize>,
}

impl<T: std::hash::Hash + Eq> UniqueRegister<T> {
    pub fn new() -> Self {
        Self {
            map: AHashMap::new(),
        }
    }

    pub fn insert(&mut self, value: T) -> usize {
        let len = self.map.len();
        *self.map.entry(value).or_insert(len)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn drain(&mut self) -> impl Iterator<Item = (usize, T)> + '_ {
        self.map.drain().map(|(k, v)| (v, k))
    }
}

impl<T: std::hash::Hash + Eq> UniqueRegister<T> {
    pub fn make_vec(&mut self) -> Vec<T> {
        let mut ve: Vec<MaybeUninit<T>> = (0..self.len()).map(|_| MaybeUninit::uninit()).collect();

        for (v, k) in self.drain() {
            ve[v].write(k);
        }

        unsafe { std::mem::transmute(ve) }
    }
}
