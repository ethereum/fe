use crate::files::{File, SourceFileId, Utf8Path};
use codespan_reporting as cs;
use salsa;
use smol_str::SmolStr;
use std::rc::Rc;

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

pub trait UpcastMut<T: ?Sized> {
    fn upcast_mut(&mut self) -> &mut T;
}

#[salsa::query_group(SourceDbStorage)]
pub trait SourceDb {
    #[salsa::interned]
    fn intern_file(&self, file: File) -> SourceFileId;

    /// Set with `fn set_file_content(&mut self, file: SourceFileId, content: Rc<str>)
    #[salsa::input]
    fn file_content(&self, file: SourceFileId) -> Rc<str>;

    #[salsa::invoke(file_line_starts_query)]
    fn file_line_starts(&self, file: SourceFileId) -> Rc<[usize]>;

    #[salsa::invoke(file_name_query)]
    fn file_name(&self, file: SourceFileId) -> SmolStr;
}

fn file_line_starts_query(db: &dyn SourceDb, file: SourceFileId) -> Rc<[usize]> {
    cs::files::line_starts(&file.content(db)).collect()
}

fn file_name_query(db: &dyn SourceDb, file: SourceFileId) -> SmolStr {
    let path = db.lookup_intern_file(file).path;
    Utf8Path::new(path.as_str())
        .file_name()
        .expect("path lacks file name")
        .into()
}

#[salsa::database(SourceDbStorage)]
#[derive(Default)]
pub struct TestDb {
    storage: salsa::Storage<TestDb>,
}
impl salsa::Database for TestDb {}
