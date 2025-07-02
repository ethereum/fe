use std::{fmt, fs};

use camino::Utf8PathBuf;
use config::{Config, ConfigResolver};
use source_files::{SourceFiles, SourceFilesResolver};
use url::Url;

use crate::Resolver;

pub mod config;
pub mod source_files;

#[derive(Debug)]
pub enum Ingot {
    SingleFile {
        url: Url,
        content: String,
    },
    Folder {
        config: Option<Config>,
        source_files: Option<SourceFiles>,
    },
}

#[derive(Debug)]
pub enum Diagnostic {
    ConfigError(config::Error),
    SourceFilesError(source_files::Error),
    SourceFilesDiagnostics(Vec<source_files::Diagnostic>),
}

#[derive(Debug)]
pub enum Error {
    IngotUrlDoesNotExist(Url),
    IngotIsEmpty(Url),
    StandaloneFileReadError { url: Url, error: std::io::Error },
}

#[derive(Default)]
pub struct IngotResolver {
    diagnostics: Vec<Diagnostic>,
}

impl Resolver for IngotResolver {
    type Description = Url;
    type Resource = Ingot;
    type Error = Error;
    type Diagnostic = Diagnostic;

    fn resolve(&mut self, ingot_url: &Url) -> Result<Ingot, Error> {
        let ingot_path = Utf8PathBuf::from(ingot_url.path());

        if ingot_path.exists() {
            if ingot_path.is_dir() {
                let mut config_resolver = ConfigResolver;
                let mut source_files_resolver = SourceFilesResolver::default();

                let config = match config_resolver.resolve(ingot_url) {
                    Ok(config) => Some(config),
                    Err(error) => {
                        self.diagnostics.push(Diagnostic::ConfigError(error));
                        None
                    }
                };

                let source_files = match source_files_resolver.resolve(ingot_url) {
                    Ok(source_files) => Some(source_files),
                    Err(error) => {
                        self.diagnostics.push(Diagnostic::SourceFilesError(error));
                        None
                    }
                };

                let source_files_diags = source_files_resolver.take_diagnostics();

                if !source_files_diags.is_empty() {
                    self.diagnostics
                        .push(Diagnostic::SourceFilesDiagnostics(source_files_diags));
                }

                Ok(Ingot::Folder {
                    config,
                    source_files,
                })
            } else {
                match fs::read_to_string(ingot_path) {
                    Ok(content) => Ok(Ingot::SingleFile {
                        url: ingot_url.clone(),
                        content,
                    }),
                    Err(error) => Err(Error::StandaloneFileReadError {
                        url: ingot_url.clone(),
                        error,
                    }),
                }
            }
        } else {
            Err(Error::IngotUrlDoesNotExist(ingot_url.clone()))
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IngotUrlDoesNotExist(url) => write!(f, "the ingot url {url} does not exist"),
            Self::IngotIsEmpty(url) => write!(
                f,
                "the ingot url {url} exists, but does not contain configuration or source files"
            ),
            Self::StandaloneFileReadError { url, error } => {
                write!(f, "unable to read standalone ingot file {url}: {error}")
            }
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigError(error) => write!(f, "config resolution error: {error}"),
            Self::SourceFilesError(error) => write!(f, "source files resolution error: {error}"),
            Self::SourceFilesDiagnostics(diagnostics) => {
                writeln!(
                    f,
                    "the following errors were encountered during source file resolution:"
                )?;
                for diagnostic in diagnostics {
                    writeln!(f, " {diagnostic}")?
                }
                Ok(())
            }
        }
    }
}
