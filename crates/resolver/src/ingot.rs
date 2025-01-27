use std::{fmt, fs};

use camino::Utf8PathBuf;
use config::{Config, ConfigResolver};
use graph::{Graph, GraphResolver};
use source_files::{SourceFiles, SourceFilesResolver};

use crate::Resolver;

pub mod config;
pub mod graph;
pub mod source_files;

#[derive(Debug)]
pub enum Ingot {
    SingleFile {
        path: Utf8PathBuf,
        content: String,
    },
    Folder {
        config: Option<Config>,
        source_files: Option<SourceFiles>,
        dependency_graph: Option<Graph>,
    },
}

#[derive(Debug)]
pub enum Diagnostic {
    ConfigError(config::Error),
    ConfigDiagnostics(Vec<config::Diagnostic>),
    SourceFilesError(source_files::Error),
    SourceFilesDiagnostics(Vec<source_files::Diagnostic>),
}

#[derive(Debug)]
pub enum Error {
    IngotPathDoesNotExist,
    SingleFileReadError(std::io::Error),
}

#[derive(Default)]
pub struct IngotResolver {
    diagnostics: Vec<Diagnostic>,
}

impl Resolver for IngotResolver {
    type Description = Utf8PathBuf;
    type Resource = Ingot;
    type Error = Error;
    type Diagnostic = Diagnostic;

    fn resolve(&mut self, ingot_path: &Utf8PathBuf) -> Result<Ingot, Error> {
        if ingot_path.exists() {
            if ingot_path.is_dir() {
                let mut config_resolver = ConfigResolver::default();
                let mut source_files_resolver = SourceFilesResolver::default();
                let mut dep_graph_resolver = GraphResolver::default();

                let config = match config_resolver.resolve(ingot_path) {
                    Ok(config) => Some(config),
                    Err(error) => {
                        self.diagnostics.push(Diagnostic::ConfigError(error));
                        None
                    }
                };

                let source_files = match source_files_resolver.resolve(ingot_path) {
                    Ok(source_files) => Some(source_files),
                    Err(error) => {
                        self.diagnostics.push(Diagnostic::SourceFilesError(error));
                        None
                    }
                };

                let config_diags = config_resolver.take_diagnostics();
                let source_files_diags = source_files_resolver.take_diagnostics();

                if !config_diags.is_empty() {
                    self.diagnostics
                        .push(Diagnostic::ConfigDiagnostics(config_diags));
                }

                if !source_files_diags.is_empty() {
                    self.diagnostics
                        .push(Diagnostic::SourceFilesDiagnostics(source_files_diags));
                }

                let dependency_graph = Some(dep_graph_resolver.resolve(ingot_path).unwrap());

                Ok(Ingot::Folder {
                    config,
                    source_files,
                    dependency_graph,
                })
            } else {
                match fs::read_to_string(ingot_path) {
                    Ok(content) => Ok(Ingot::SingleFile {
                        path: ingot_path.clone(),
                        content,
                    }),
                    Err(error) => Err(Error::SingleFileReadError(error)),
                }
            }
        } else {
            Err(Error::IngotPathDoesNotExist)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IngotPathDoesNotExist => write!(f, "the ingot path does not exist"),
            Self::SingleFileReadError(error) => write!(f, "single file read error: {error}"),
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigError(error) => write!(f, "config resolution error: {error}"),
            Self::ConfigDiagnostics(diagnostics) => {
                writeln!(f, "unable to resolve config file due to the following:")?;
                for diagnostic in diagnostics {
                    writeln!(f, " {diagnostic}")?
                }
                Ok(())
            }
            Self::SourceFilesError(error) => write!(f, "source files resolution error: {error}"),
            Self::SourceFilesDiagnostics(diagnostics) => {
                writeln!(f, "unable to resolve source files due to the following:")?;
                for diagnostic in diagnostics {
                    writeln!(f, " {diagnostic}")?
                }
                Ok(())
            }
        }
    }
}
