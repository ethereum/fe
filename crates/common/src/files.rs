use crate::utils::keccak;
use crate::Span;
use codespan_reporting as cs;
use cs::files::Error as CsError;
use include_dir::Dir;
use indexmap::indexmap;
use indexmap::IndexMap;
use smol_str::SmolStr;
use std::collections::BTreeMap;
use std::ops::Range;
use std::path::Path;
use std::{fs, io};

#[derive(PartialEq, Clone, Eq, Hash, Debug)]
pub struct SourceFile {
    pub id: SourceFileId,
    pub name: String,
    pub content: String,
    line_starts: Vec<usize>,
}

#[derive(PartialEq, Copy, Clone, Eq, Hash, Debug, PartialOrd, Ord, Default)]
pub struct SourceFileId(pub u128);

impl SourceFile {
    pub fn new(name: &str, content: &str) -> Self {
        // Canonicalize new line character for Windows.
        let content_canonicalized = content.replace("\r\n", "\n");
        let hash = keccak::full_as_bytes(content_canonicalized.as_bytes());

        let line_starts = cs::files::line_starts(content).collect();
        Self {
            id: SourceFileId(u128::from_be_bytes(hash[..16].try_into().unwrap())),
            name: name.to_string(),
            content: content.to_string(),
            line_starts,
        }
    }

    pub fn line_index(&self, byte_index: usize) -> usize {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1)
    }

    pub fn line_span(&self, line_index: usize) -> Option<Span> {
        let end = if line_index == self.line_starts.len() - 1 {
            self.content.len()
        } else {
            *self.line_starts.get(line_index + 1)?
        };
        Some(Span::new(self.id, *self.line_starts.get(line_index)?, end))
    }
}

pub trait FileLoader {
    fn load_file(&self, path: &Path) -> io::Result<String>;
}

pub struct OsFileLoader;

impl FileLoader for OsFileLoader {
    fn load_file(&self, path: &Path) -> io::Result<String> {
        fs::read_to_string(path)
    }
}

pub struct FileStore {
    pub files: BTreeMap<SourceFileId, SourceFile>,
    loader: Box<dyn FileLoader>,
}

impl FileStore {
    pub fn new() -> Self {
        Self {
            files: BTreeMap::new(),
            loader: Box::new(OsFileLoader),
        }
    }

    pub fn with_loader(loader: Box<dyn FileLoader>) -> Self {
        Self {
            files: BTreeMap::new(),
            loader,
        }
    }

    pub fn add_file(&mut self, path: &str, content: &str) -> SourceFileId {
        let file = SourceFile::new(path, content);
        let id = file.id;
        self.files.insert(id, file);
        id
    }

    /// Adds an included dir to the file store.
    pub fn add_included_dir(&mut self, dir: &Dir) -> Vec<SourceFileId> {
        let mut file_ids = vec![];

        for file in dir.files() {
            file_ids.push(
                self.add_file(
                    file.path()
                        .to_str()
                        .expect("cannot convert file path to string"),
                    file.contents_utf8()
                        .expect("could not get utf8 encoded file content"),
                ),
            );
        }

        for sub_dir in dir.dirs() {
            file_ids.extend(self.add_included_dir(sub_dir))
        }

        file_ids
    }

    /// Adds the included libraries to the file store and returns a mapping of
    /// library names to file ids.
    pub fn add_included_libraries(&mut self) -> IndexMap<SmolStr, Vec<SourceFileId>> {
        indexmap! {
            "std".into() => self.add_included_dir(&fe_library::STD)
        }
    }

    pub fn load_file(&mut self, path: &str) -> io::Result<(String, SourceFileId)> {
        let content = self.loader.load_file(Path::new(&path))?;
        let id = self.add_file(path, &content);
        Ok((content, id))
    }

    pub fn get_file(&self, id: SourceFileId) -> Option<&SourceFile> {
        self.files.get(&id)
    }

    pub fn all_files(&self) -> Vec<SourceFileId> {
        self.files.keys().copied().collect()
    }
}

impl<'a> cs::files::Files<'a> for FileStore {
    type FileId = SourceFileId;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&'a self, id: SourceFileId) -> Result<Self::Name, CsError> {
        self.get_file(id)
            .map(|file| file.name.as_str())
            .ok_or(CsError::FileMissing)
    }

    fn source(&'a self, id: SourceFileId) -> Result<Self::Source, CsError> {
        self.get_file(id)
            .map(|file| file.content.as_str())
            .ok_or(CsError::FileMissing)
    }

    fn line_index(&'a self, id: SourceFileId, byte_index: usize) -> Result<usize, CsError> {
        Ok(self
            .get_file(id)
            .ok_or(CsError::FileMissing)?
            .line_index(byte_index))
    }

    fn line_range(&'a self, id: SourceFileId, line_index: usize) -> Result<Range<usize>, CsError> {
        let file = self.get_file(id).ok_or(CsError::FileMissing)?;
        Ok(file
            .line_span(line_index)
            .ok_or(CsError::LineTooLarge {
                given: line_index,
                max: file.line_starts.len() - 1,
            })?
            .into())
    }
}

impl Default for FileStore {
    fn default() -> Self {
        Self::new()
    }
}
