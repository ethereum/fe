use crate::utils::keccak;
use crate::Span;
use codespan_reporting as cs;
use cs::files::Error as CsError;
use std::{
    collections::HashMap,
    convert::TryInto,
    fs,
    io,
    ops::Range,
    path::Path,
};

pub struct SourceFile {
    id: SourceFileId,
    name: String,
    content: String,
    line_starts: Vec<usize>,
}

#[derive(PartialEq, Copy, Clone, Eq, Hash)]
pub struct SourceFileId(u128);

impl SourceFile {
    pub fn new(name: String, content: String) -> Self {
        let hash = keccak::full_as_bytes(content.as_bytes());
        let line_starts = cs::files::line_starts(&content).collect();
        Self {
            id: SourceFileId(u128::from_be_bytes(hash[..16].try_into().unwrap())),
            name,
            content,
            line_starts,
        }
    }

    pub fn line_index(&self, byte_index: usize) -> usize {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1)
    }

    pub fn line_span(&self, line_index: usize) -> Option<Span> {
        Some(Span::new(
            *self.line_starts.get(line_index)?,
            *self.line_starts.get(line_index + 1)?,
        ))
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
    files: HashMap<SourceFileId, SourceFile>,
    loader: Box<dyn FileLoader>,
}

impl FileStore {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            loader: Box::new(OsFileLoader),
        }
    }

    pub fn with_loader(loader: Box<dyn FileLoader>) -> Self {
        Self {
            files: HashMap::new(),
            loader,
        }
    }

    pub fn add_file(&mut self, path: String, content: String) -> SourceFileId {
        let file = SourceFile::new(path, content);
        let id = file.id;
        self.files.insert(id, file);
        id
    }

    pub fn load_file(&mut self, path: String) -> io::Result<(SourceFileId, String)> {
        let content = self.loader.load_file(&Path::new(&path))?;
        let id = self.add_file(path, content.clone());
        Ok((id, content))
    }

    pub fn get_file(&self, id: SourceFileId) -> Option<&SourceFile> {
        self.files.get(&id)
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
