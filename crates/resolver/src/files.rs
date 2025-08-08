use camino::Utf8PathBuf;
use glob::glob;
use std::io;
use std::path::PathBuf;
use std::{fmt, fs};
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

impl fmt::Display for FilesResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilesResolutionError::FileUrlDoesNotExist(url) => {
                write!(f, "File URL does not exist: {url}")
            }
            FilesResolutionError::GlobError(err) => {
                write!(f, "Glob pattern error: {err}")
            }
            FilesResolutionError::PatternError(err) => {
                write!(f, "Pattern error: {err}")
            }
            FilesResolutionError::IoError(err) => {
                write!(f, "IO error: {err}")
            }
        }
    }
}

impl std::error::Error for FilesResolutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FilesResolutionError::GlobError(err) => Some(err),
            FilesResolutionError::PatternError(err) => Some(err),
            FilesResolutionError::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl fmt::Display for FilesResolutionDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilesResolutionDiagnostic::SkippedNonUtf8(path) => {
                write!(f, "Skipped non-UTF8 file: {}", path.display())
            }
            FilesResolutionDiagnostic::FileIoError(path, err) => {
                write!(f, "IO error reading file {path}: {err}")
            }
        }
    }
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

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        url: &Url,
    ) -> Result<H::Item, Self::Error>
    where
        H: crate::ResolutionHandler<Self>,
    {
        tracing::trace!(target: "resolver", "Starting file resolution for URL: {}", url);
        let mut results = vec![];

        // for remote ingots we can setup the local path here after resolving the git repo
        let ingot_path = Utf8PathBuf::from(url.path());
        tracing::trace!(target: "resolver", "Resolving files in path: {}", ingot_path);

        for pattern in &self.patterns {
            let pattern = ingot_path.join(pattern);
            let entries = glob(pattern.as_str()).map_err(FilesResolutionError::PatternError)?;

            for entry in entries {
                match entry {
                    Ok(path) => {
                        if path.is_file() {
                            match Utf8PathBuf::from_path_buf(path) {
                                Ok(path) => match fs::read_to_string(&path) {
                                    Ok(content) => {
                                        tracing::trace!(target: "resolver", "Successfully read file: {}", path);
                                        results.push(File { path, content });
                                    }
                                    Err(error) => {
                                        tracing::warn!(target: "resolver", "Failed to read file {}: {}", path, error);
                                        self.diagnostics.push(
                                            FilesResolutionDiagnostic::FileIoError(path, error),
                                        );
                                    }
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
            tracing::warn!(target: "resolver", "No files found for URL: {}", url);
            return Err(FilesResolutionError::FileUrlDoesNotExist(url.clone()));
        }

        tracing::trace!(target: "resolver", "File resolution completed successfully, found {} files", results.len());
        Ok(handler.handle_resolution(url, results))
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}
