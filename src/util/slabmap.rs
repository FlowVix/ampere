use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use slab::Slab;

#[derive(Debug, Clone)]
pub struct SlabMap<K, V> {
    slab: Slab<V>,
    _p: PhantomData<K>,
}

impl<K, V> Default for SlabMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> SlabMap<K, V> {
    pub fn new() -> Self {
        Self {
            slab: Slab::new(),
            _p: PhantomData,
        }
    }
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            slab: Slab::with_capacity(capacity),
            _p: PhantomData,
        }
    }
}

impl<K, V> SlabMap<K, V>
where
    K: From<usize> + Debug,
    usize: From<K>,
{
    pub fn insert(&mut self, val: V) -> K {
        self.slab.insert(val).into()
    }
    pub fn remove(&mut self, k: K) -> Option<V> {
        self.slab.try_remove(k.into())
    }
    pub fn len(&mut self) -> usize {
        self.slab.len()
    }
    pub fn compact<F>(&mut self, mut rekey: F)
    where
        F: FnMut(&mut V, K, K) -> bool,
    {
        self.slab
            .compact(|v, curr, new| rekey(v, curr.into(), new.into()))
    }
    pub fn shrink_to_fit(&mut self) {
        self.slab.shrink_to_fit()
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.slab.get(key.into())
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.slab.get_mut(key.into())
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.slab.iter().map(|(k, v)| (k.into(), v))
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> {
        self.slab.iter_mut().map(|(k, v)| (k.into(), v))
    }

    pub fn into_iter(self) -> impl Iterator<Item = (K, V)> {
        self.slab.into_iter().map(|(k, v)| (k.into(), v))
    }
}

impl<K, V> Index<K> for SlabMap<K, V>
where
    usize: From<K>,
{
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.slab[index.into()]
    }
}
impl<K, V> IndexMut<K> for SlabMap<K, V>
where
    usize: From<K>,
{
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.slab[index.into()]
    }
}

impl<K, V> FromIterator<(K, V)> for SlabMap<K, V>
where
    usize: From<K>,
{
    fn from_iter<I>(iterable: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
    {
        Self {
            slab: Slab::from_iter(iterable.into_iter().map(|(k, v)| (k.into(), v))),
            _p: PhantomData,
        }
    }
}
