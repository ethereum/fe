use core::panic;
use std::str::FromStr;

use crate::Resolver;
use camino::Utf8PathBuf;
use common::indexmap::IndexSet;
use glob::glob;

pub struct SourceFiles {
    pub root: Utf8PathBuf,
    pub files: IndexSet<Utf8PathBuf>,
}

#[derive(Debug)]
pub enum SourceFilesResolutionError {
    SourceFolderMissing,
    SourceFolderEmpty,
}

#[derive(Debug)]
pub enum SourceFilesResolutionDiagnostic {
    RootFileMissing,
    GlobError,
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
        ingot_path: &Utf8PathBuf,
    ) -> Result<SourceFiles, SourceFilesResolutionError> {
        let source_path = ingot_path.join("src");
        let root = source_path.join("lib.fe");
        let files = source_path.join("**/*.fe");

        let files = glob(&files.to_string())
            .expect("Failed to read glob pattern")
            .into_iter()
            .filter_map(|entry| match entry {
                Ok(path) => Some(Utf8PathBuf::from_str(path.to_str().unwrap()).unwrap()),
                Err(error) => None, // add diagnostic
            })
            .collect();

        if !root.exists() {
            // add diagn
        }

        Ok(SourceFiles { root, files })
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}
