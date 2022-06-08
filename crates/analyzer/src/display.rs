use crate::AnalyzerDb;
use std::fmt;

pub trait Displayable: DisplayWithDb {
    fn display<'a, 'b>(&'a self, db: &'b dyn AnalyzerDb) -> DisplayableWrapper<'b, &'a Self> {
        DisplayableWrapper::new(db, self)
    }
}
impl<T: DisplayWithDb> Displayable for T {}

pub trait DisplayWithDb {
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<T: ?Sized> DisplayWithDb for &T
where
    T: DisplayWithDb,
{
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (*self).format(db, f)
    }
}

pub struct DisplayableWrapper<'a, T> {
    db: &'a dyn AnalyzerDb,
    inner: T,
}
impl<'a, T> DisplayableWrapper<'a, T> {
    pub fn new(db: &'a dyn AnalyzerDb, inner: T) -> Self {
        Self { db, inner }
    }
    pub fn child(&self, inner: T) -> Self {
        Self { db: self.db, inner }
    }
}

impl<T: DisplayWithDb> fmt::Display for DisplayableWrapper<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.format(self.db, f)
    }
}
