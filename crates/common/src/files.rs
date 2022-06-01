use crate::db::SourceDb;
pub use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
pub use fe_library::include_dir;
use std::ops::Range;
use std::rc::Rc;

// NOTE: all file paths are stored as utf8 strings.
//  Non-utf8 paths (for user code) should be reported
//  as an error.
//  If include_dir paths aren't utf8, we panic and fix
//  our stdlib/test-file path names.

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct File {
    /// Differentiates between local source files and fe std lib
    /// files, which may have the same path (for salsa's sake).
    pub kind: FileKind,

    /// Path of the file. May include `src/` dir or longer prefix;
    /// this prefix will be stored in the `Ingot::src_path`, and stripped
    /// off as needed.
    pub path: Rc<Utf8PathBuf>,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum FileKind {
    /// User file; either part of the target project or an imported ingot
    Local,
    /// File is part of the fe standard library
    Std,
}

/// Returns the common *prefix* of two paths. If the paths are identical,
/// returns the path parent.
pub fn common_prefix(left: &Utf8Path, right: &Utf8Path) -> Utf8PathBuf {
    left.components()
        .zip(right.components())
        .take_while(|(l, r)| l == r)
        .map(|(l, _)| l)
        .collect()
}

// from rust-analyzer {
#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(v.as_u32())
            }
            fn as_intern_id(&self) -> salsa::InternId {
                salsa::InternId::from(self.0)
            }
        }
    };
}
// } from rust-analyzer

// TODO: rename to FileId
#[derive(Debug, serde::Deserialize, PartialEq, Eq, Hash, Copy, Clone)]
pub struct SourceFileId(pub(crate) u32);
impl_intern_key!(SourceFileId);

impl SourceFileId {
    pub fn new_local(db: &mut dyn SourceDb, path: &str, content: Rc<str>) -> Self {
        Self::new(db, FileKind::Std, path, content)
    }

    pub fn new_std(db: &mut dyn SourceDb, path: &str, content: Rc<str>) -> Self {
        Self::new(db, FileKind::Std, path, content)
    }

    pub fn new(db: &mut dyn SourceDb, kind: FileKind, path: &str, content: Rc<str>) -> Self {
        let id = db.intern_file(File {
            kind,
            path: Rc::new(path.into()),
        });
        db.set_file_content(id, content);
        id
    }

    pub fn path(&self, db: &dyn SourceDb) -> Rc<Utf8PathBuf> {
        db.lookup_intern_file(*self).path
    }

    pub fn content(&self, db: &dyn SourceDb) -> Rc<str> {
        db.file_content(*self)
    }

    pub fn line_index(&self, db: &dyn SourceDb, byte_index: usize) -> usize {
        db.file_line_starts(*self)
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1)
    }

    pub fn line_range(&self, db: &dyn SourceDb, line_index: usize) -> Option<Range<usize>> {
        let line_starts = db.file_line_starts(*self);
        let end = if line_index == line_starts.len() - 1 {
            self.content(db).len()
        } else {
            *line_starts.get(line_index + 1)?
        };
        Some(Range {
            start: *line_starts.get(line_index)?,
            end,
        })
    }

    pub fn dummy_file() -> Self {
        // Used by unit tests and benchmarks
        Self(u32::MAX)
    }
    pub fn is_dummy(self) -> bool {
        self == Self::dummy_file()
    }
}

#[test]
fn test_common_prefix() {
    assert_eq!(
        common_prefix(Utf8Path::new("a/b/c/d/e"), Utf8Path::new("a/b/d/e")),
        Utf8Path::new("a/b")
    );
    assert_eq!(
        common_prefix(Utf8Path::new("src/foo.x"), Utf8Path::new("tests/bar.fe")),
        Utf8Path::new("")
    );
    assert_eq!(
        common_prefix(Utf8Path::new("/src/foo.x"), Utf8Path::new("src/bar.fe")),
        Utf8Path::new("")
    );
}
