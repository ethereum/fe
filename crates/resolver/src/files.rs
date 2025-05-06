use camino::Utf8PathBuf;
use glob::glob;
use std::fs;
use std::io;
use std::path::PathBuf;

use crate::Resolver;

#[derive(Default)]
pub struct FilesResolver {
    patterns: Vec<String>,
    diagnostics: Vec<FilesResolverDiagnostic>,
}

#[derive(Debug)]
pub enum FilesResolverError {
    PathDoesNotExist(Utf8PathBuf),
    GlobError(glob::GlobError),
    PatternError(glob::PatternError),
    IoError(io::Error),
}

#[derive(Debug)]
pub enum FilesResolverDiagnostic {
    SkippedNonUtf8(PathBuf),
    IoError(Utf8PathBuf, io::Error),
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
    type Description = Utf8PathBuf;
    type Resource = Vec<(Utf8PathBuf, String)>;
    type Error = FilesResolverError;
    type Diagnostic = FilesResolverDiagnostic;

    fn transient_resolve(
        &mut self,
        base: &Self::Description,
    ) -> Result<Self::Resource, Self::Error> {
        let mut results = vec![];

        for pattern in &self.patterns {
            let combined = base.join(pattern);
            let pattern_str = combined.as_str();

            let entries = glob(pattern_str).map_err(FilesResolverError::PatternError)?;

            for entry in entries {
                match entry {
                    Ok(path) => {
                        if path.is_file() {
                            match Utf8PathBuf::from_path_buf(path.clone()) {
                                Ok(utf8_path) => match fs::read_to_string(&path) {
                                    Ok(content) => results.push((utf8_path, content)),
                                    Err(e) => {
                                        self.diagnostics.push(FilesResolverDiagnostic::IoError(
                                            Utf8PathBuf::from_path_buf(path).unwrap_or_default(),
                                            e,
                                        ))
                                    }
                                },
                                Err(_) => {
                                    self.diagnostics
                                        .push(FilesResolverDiagnostic::SkippedNonUtf8(path));
                                }
                            }
                        }
                    }
                    Err(e) => return Err(FilesResolverError::GlobError(e)),
                }
            }
        }

        if results.is_empty() {
            return Err(FilesResolverError::PathDoesNotExist(base.clone()));
        }

        Ok(results)
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}

