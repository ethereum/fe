use crate::Resolver;
use camino::Utf8PathBuf;
use common::indexmap::IndexMap;
use common::indexmap::IndexSet;
use core::panic;
use std::io;

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
        todo!()
        // for entry in glob("/src/**/*.fe").expect("Failed to read glob pattern") {
        //     match entry {
        //         Ok(path) => println!("{:?}", path.display()),
        //         Err(e) => println!("{:?}", e),
        //     }
        // }
        //
        // Ok(SourceFiles { files })
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}
