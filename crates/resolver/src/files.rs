use camino::Utf8PathBuf;
use glob::glob;
use std::fs;
use std::io;
use std::path::PathBuf;
use url::Url;

use crate::Resolver;

#[derive(Default)]
pub struct FilesResolver {
    patterns: Vec<String>,
    diagnostics: Vec<FilesResolutionDiagnostic>,
}

#[derive(Debug)]
pub struct File {
    pub path: Utf8PathBuf,
    pub content: String,
}

#[derive(Debug)]
pub enum FilesResolutionError {
    FileUrlDoesNotExist(Url),
    GlobError(glob::GlobError),
    PatternError(glob::PatternError),
    IoError(io::Error),
}

#[derive(Debug)]
pub enum FilesResolutionDiagnostic {
    SkippedNonUtf8(PathBuf),
    FileIoError(Utf8PathBuf, io::Error),
}

impl FilesResolver {
    pub fn with_patterns(patterns: &[&str]) -> Self {
        Self {
            patterns: patterns.iter().map(|p| p.to_string()).collect(),
            diagnostics: vec![],
        }
    }

    pub fn with_pattern(pattern: &str) -> Self {
        Self {
            patterns: vec![pattern.to_string()],
            diagnostics: vec![],
        }
    }

    pub fn exact_file(path: Utf8PathBuf) -> Self {
        Self {
            patterns: vec![path.to_string()],
            diagnostics: vec![],
        }
    }
}

impl Resolver for FilesResolver {
    type Description = Url;
    type Resource = Vec<File>;
    type Error = FilesResolutionError;
    type Diagnostic = FilesResolutionDiagnostic;

    fn transient_resolve(&mut self, url: &Url) -> Result<Vec<File>, FilesResolutionError> {
        let mut results = vec![];

        // for remote ingots we can setup the local path here after resolving the git repo
        let ingot_path = Utf8PathBuf::from(url.path());

        for pattern in &self.patterns {
            let pattern = ingot_path.join(pattern);
            let entries = glob(pattern.as_str()).map_err(FilesResolutionError::PatternError)?;

            for entry in entries {
                match entry {
                    Ok(path) => {
                        if path.is_file() {
                            match Utf8PathBuf::from_path_buf(path) {
                                Ok(path) => match fs::read_to_string(&path) {
                                    Ok(content) => results.push(File { path, content }),
                                    Err(error) => self
                                        .diagnostics
                                        .push(FilesResolutionDiagnostic::FileIoError(path, error)),
                                },
                                Err(error) => {
                                    self.diagnostics
                                        .push(FilesResolutionDiagnostic::SkippedNonUtf8(error));
                                }
                            }
                        }
                    }
                    Err(e) => return Err(FilesResolutionError::GlobError(e)),
                }
            }
        }

        if results.is_empty() {
            return Err(FilesResolutionError::FileUrlDoesNotExist(url.clone()));
        }

        Ok(results)
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}
