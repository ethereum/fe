use crate::Resolver;
use camino::Utf8PathBuf;
use common::indexmap::IndexMap;
use std::fs;
use std::io;

pub struct SourceFiles {
    pub files: IndexMap<Utf8PathBuf, Vec<u8>>,
}

#[derive(Debug)]
pub enum SourceFilesResolutionError {
    SourceFolderMissing,
    FileReadError(io::Error),
}

#[derive(Debug)]
pub enum SourceFilesResolutionDiagnostic {
    MainFileMissing,
    LibFileMissing,
}

pub struct SourceFilesResolver;

impl SourceFilesResolver {
    pub fn new() -> Self {
        Self
    }
}

impl Resolver for SourceFilesResolver {
    type Description = Utf8PathBuf;
    type Resource = SourceFiles;
    type Error = SourceFilesResolutionError;
    type Diagnostic = SourceFilesResolutionDiagnostic;

    fn resolve(
        &mut self,
        description: &Utf8PathBuf,
    ) -> Result<SourceFiles, SourceFilesResolutionError> {
        let src_dir = description.join("src");

        if !src_dir.exists() || !src_dir.is_dir() {
            return Err(SourceFilesResolutionError::SourceFolderMissing);
        }

        let mut files = IndexMap::new();

        collect_files_and_contents(&src_dir, &mut files)?;

        Ok(SourceFiles { files })
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}

fn collect_files_and_contents(
    dir: &Utf8PathBuf,
    file_map: &mut IndexMap<Utf8PathBuf, Vec<u8>>,
) -> Result<(), SourceFilesResolutionError> {
    for entry in fs::read_dir(dir).map_err(SourceFilesResolutionError::FileReadError)? {
        let entry = entry.map_err(SourceFilesResolutionError::FileReadError)?;
        let path = Utf8PathBuf::from_path_buf(entry.path()).map_err(|_| {
            SourceFilesResolutionError::FileReadError(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid UTF-8 path",
            ))
        })?;

        if path.is_dir() {
            collect_files_and_contents(&path, file_map)?;
        } else if path.is_file() {
            let content = fs::read(&path).map_err(SourceFilesResolutionError::FileReadError)?;
            file_map.insert(path, content);
        }
    }

    Ok(())
}
