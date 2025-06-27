use camino::Utf8PathBuf;
use core::panic;
use std::{fmt, fs};
use url::Url;

use crate::Resolver;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

#[derive(Debug)]
pub enum Error {
    ConfigFileDoesNotExist(Utf8PathBuf),
    FileReadError(std::io::Error),
}

#[derive(Default)]
pub struct ConfigResolver;

impl Resolver for ConfigResolver {
    type Description = Url;
    type Resource = String;
    type Error = Error;
    type Diagnostic = ();

    fn resolve(&mut self, ingot_url: &Url) -> Result<String, Error> {
        let config_path = Utf8PathBuf::from(ingot_url).join(FE_CONFIG_SUFFIX);

        if config_path.exists() {
            fs::read_to_string(&config_path).map_err(Error::FileReadError)
        } else {
            Err(Error::ConfigFileDoesNotExist(config_path))
        }
    }

    fn take_diagnostics(&mut self) -> Vec<()> {
        panic!()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigFileDoesNotExist(path) => {
                write!(f, "a `{path}` file does not exist in the ingot directory")
            }
            Self::FileReadError(error) => write!(f, "file read error: {error}"),
        }
    }
}

// impl fmt::Display for Diagnostic {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         if self.count == 1 {
//             write!(f, "there is 1 diagnostic in {}", self.path)
//         } else {
//             write!(f, "there are {} diagnostics in {}", self.count, self.path)
//         }
//     }
// }
