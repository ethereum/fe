use std::{fmt, fs, mem, path::PathBuf};

use crate::Resolver;
use camino::Utf8PathBuf;
use glob::glob;
use url::Url;

#[derive(Debug)]
pub struct SourceFiles {
    pub root: Option<Url>,
    pub files: Vec<(Url, String)>,
}

#[derive(Debug)]
pub enum Error {
    SourceFolderDoesNotExist,
    SourcePathIsFile,
}

#[derive(Debug)]
pub enum Diagnostic {
    RootFileDoesNotExist,
    NonUtf8Path(PathBuf),
    GlobError(glob::GlobError),
    FileReadError(Utf8PathBuf, std::io::Error),
}

#[derive(Default)]
pub struct SourceFilesResolver {
    diagnostics: Vec<Diagnostic>,
}

impl Resolver for SourceFilesResolver {
    type Description = Url;
    type Resource = SourceFiles;
    type Error = Error;
    type Diagnostic = Diagnostic;

    fn resolve(&mut self, ingot_url: &Url) -> Result<SourceFiles, Error> {
        let ingot_path = Utf8PathBuf::from(ingot_url.path());
        let source_path = ingot_path.join("src");

        if !source_path.exists() {
            return Err(Error::SourceFolderDoesNotExist);
        }
        if !source_path.is_dir() {
            return Err(Error::SourcePathIsFile);
        }

        let root = source_path.join("lib.fe");
        let files = source_path.join("**/*.fe");

        let files = glob(files.as_str())
            .expect("failed to read glob pattern")
            .filter_map(|entry| match entry {
                Ok(path) => match Utf8PathBuf::from_path_buf(path.to_path_buf()) {
                    Ok(path) => match fs::read_to_string(&path) {
                        Ok(content) => Some((Url::from_file_path(path).unwrap(), content)),
                        Err(error) => {
                            self.diagnostics
                                .push(Diagnostic::FileReadError(path, error));
                            None
                        }
                    },
                    Err(path) => {
                        self.diagnostics.push(Diagnostic::NonUtf8Path(path));
                        None
                    }
                },
                Err(error) => {
                    self.diagnostics.push(Diagnostic::GlobError(error));
                    None
                }
            })
            .collect();

        let root = if root.exists() {
            Some(Url::from_file_path(root).unwrap())
        } else {
            self.diagnostics.push(Diagnostic::RootFileDoesNotExist);
            None
        };

        Ok(SourceFiles { root, files })
    }

    fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        mem::take(&mut self.diagnostics)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SourceFolderDoesNotExist => {
                write!(f, "`src` folder does not exist in the ingot directory")
            }
            Self::SourcePathIsFile => {
                write!(f, "`src` path is a file")
            }
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RootFileDoesNotExist => {
                write!(
                    f,
                    "a `lib.fe` file does not exist in the ingot `src` directory"
                )
            }
            Self::NonUtf8Path(path) => {
                write!(f, "the path `{}` is not utf8", path.to_string_lossy())
            }
            Self::GlobError(error) => write!(f, "glob error: {error}"),
            Self::FileReadError(path, error) => write!(f, "unable to read `{path}`: {error}"),
        }
    }
}
