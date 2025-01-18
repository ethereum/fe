use std::{
    hash::{BuildHasher, Hash, RandomState},
    ops::{Deref, DerefMut},
};

use salsa::Update;

#[derive(Debug, Clone)]
pub struct IndexMap<K, V, S = RandomState>(indexmap::IndexMap<K, V, S>);

impl<K, V> IndexMap<K, V> {
    pub fn new() -> Self {
        Self(indexmap::IndexMap::new())
    }

    pub fn with_capacity(n: usize) -> Self {
        Self(indexmap::IndexMap::with_capacity(n))
    }
}

impl<K, V> Default for IndexMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V, S> IntoIterator for IndexMap<K, V, S> {
    type Item = <indexmap::IndexMap<K, V, S> as IntoIterator>::Item;
    type IntoIter = <indexmap::IndexMap<K, V, S> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K, V, S> IntoIterator for &'a IndexMap<K, V, S> {
    type Item = <&'a indexmap::IndexMap<K, V, S> as IntoIterator>::Item;
    type IntoIter = <&'a indexmap::IndexMap<K, V, S> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a, K, V, S> IntoIterator for &'a mut IndexMap<K, V, S> {
    type Item = <&'a mut indexmap::IndexMap<K, V, S> as IntoIterator>::Item;
    type IntoIter = <&'a mut indexmap::IndexMap<K, V, S> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl<K, V, S> PartialEq for IndexMap<K, V, S>
where
    K: Eq + Hash,
    V: PartialEq,
    S: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<K, V, S> Eq for IndexMap<K, V, S>
where
    K: Eq + Hash,
    V: Eq,
    S: BuildHasher,
{
}

impl<K, V, S> FromIterator<(K, V)> for IndexMap<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher + Default,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(indexmap::IndexMap::from_iter(iter))
    }
}

impl<K, V, S> Deref for IndexMap<K, V, S>
where
    S: BuildHasher,
{
    type Target = indexmap::IndexMap<K, V, S>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V, S> DerefMut for IndexMap<K, V, S>
where
    S: BuildHasher,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

unsafe impl<K, V, S> Update for IndexMap<K, V, S>
where
    K: Update + Eq + Hash,
    V: Update,
    S: BuildHasher,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_map: Self) -> bool {
        let old_map = unsafe { &mut *old_pointer };

        // Check if the keys in both maps are the same w.r.t the key order.
        let is_key_same = old_map.len() == new_map.len()
            && old_map
                .keys()
                .zip(new_map.keys())
                .all(|(old, new)| old == new);

        // If the keys are different, update entire map.
        if !is_key_same {
            old_map.clear();
            old_map.0.extend(new_map.0);
            return true;
        }

        // Update values if it's different.
        let mut changed = false;
        for (i, new_value) in new_map.0.into_values().enumerate() {
            let old_value = &mut old_map[i];
            changed |= V::maybe_update(old_value, new_value);
        }

        changed
    }
}

#[derive(Debug, Clone)]
pub struct IndexSet<V, S = RandomState>(indexmap::IndexSet<V, S>);

impl<V> IndexSet<V> {
    pub fn new() -> Self {
        Self(indexmap::IndexSet::new())
    }

    pub fn with_capacity(n: usize) -> Self {
        Self(indexmap::IndexSet::with_capacity(n))
    }
}

impl<V> Default for IndexSet<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, S> IntoIterator for IndexSet<V, S> {
    type Item = <indexmap::IndexSet<V, S> as IntoIterator>::Item;
    type IntoIter = <indexmap::IndexSet<V, S> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, V, S> IntoIterator for &'a IndexSet<V, S> {
    type Item = <&'a indexmap::IndexSet<V, S> as IntoIterator>::Item;
    type IntoIter = <&'a indexmap::IndexSet<V, S> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<V, S> PartialEq for IndexSet<V, S>
where
    V: Hash + Eq,
    S: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<V, S> Eq for IndexSet<V, S>
where
    V: Eq + Hash,
    S: BuildHasher,
{
}

impl<V, S> FromIterator<V> for IndexSet<V, S>
where
    V: Hash + Eq,
    S: BuildHasher + Default,
{
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self(indexmap::IndexSet::from_iter(iter))
    }
}

impl<V, S> Deref for IndexSet<V, S>
where
    S: BuildHasher,
{
    type Target = indexmap::IndexSet<V, S>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V, S> DerefMut for IndexSet<V, S>
where
    S: BuildHasher,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

unsafe impl<V, S> Update for IndexSet<V, S>
where
    V: Update + Eq + Hash,
    S: BuildHasher,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_set: Self) -> bool {
        let old_set = unsafe { &mut *old_pointer };
        if old_set == &new_set {
            false
        } else {
            old_set.clear();
            old_set.0.extend(new_set.0);
            true
        }
    }
}
