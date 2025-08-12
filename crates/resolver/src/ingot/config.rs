use camino::Utf8PathBuf;
use std::{fmt, fs};
use url::Url;

use crate::Resolver;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

#[derive(Debug, Clone)]
pub struct Config {
    pub url: Url,
    pub content: String,
}

#[derive(Debug)]
pub enum Error {
    ConfigFileDoesNotExist(Utf8PathBuf),
    FileReadError(std::io::Error),
}

#[derive(Default)]
pub struct ConfigResolver;

impl Resolver for ConfigResolver {
    type Description = Url;
    type Resource = Config;
    type Error = Error;
    type Diagnostic = ();

    fn resolve(&mut self, ingot_url: &Url) -> Result<Config, Error> {
        let config_path = Utf8PathBuf::from(ingot_url.path()).join(FE_CONFIG_SUFFIX);

        if config_path.exists() {
            Ok(Config {
                url: Url::from_file_path(&config_path).unwrap(),
                content: fs::read_to_string(&config_path).map_err(Error::FileReadError)?,
            })
        } else {
            Err(Error::ConfigFileDoesNotExist(config_path))
        }
    }

    fn take_diagnostics(&mut self) -> Vec<()> {
        vec![]
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigFileDoesNotExist(path) => {
                write!(f, "`{path}` does not exist in the ingot directory")
            }
            Self::FileReadError(error) => write!(f, "file read error: {error}"),
        }
    }
}
