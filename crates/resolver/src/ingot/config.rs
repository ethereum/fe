use camino::Utf8PathBuf;
use common::{config::IngotMetadata, ingot::Version};
use serde::Deserialize;
use smol_str::SmolStr;
use std::{fmt, fs, mem};
use toml::{self, Table};

use crate::Resolver;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

#[derive(Debug)]
pub enum Error {
    ConfigFileDoesNotExist,
    FileReadError(std::io::Error),
    TomlParseError(toml::de::Error),
}

#[derive(Debug)]
pub enum Diagnostic {
    MissingName,
    MissingVersion,
    InvalidName,
    InvalidVersion(serde_semver::semver::Error),
}

#[derive(Default)]
pub struct ConfigResolver {
    diagnostics: Vec<Diagnostic>,
}

impl Resolver for ConfigResolver {
    type Description = Utf8PathBuf;
    type Resource = IngotMetadata;
    type Error = Error;
    type Diagnostic = Diagnostic;

    fn resolve(&mut self, ingot_path: &Utf8PathBuf) -> Result<IngotMetadata, Error> {
        let config_path = ingot_path.join(FE_CONFIG_SUFFIX);

        if config_path.exists() {
            let file_content = fs::read_to_string(&config_path).map_err(Error::FileReadError)?;

            let raw_config = toml::from_str::<IngotConfig>(&file_content)
                .map_err(Error::TomlParseError)?
                .ingot;

            let version = match raw_config.version {
                Some(version) => match Version::parse(&version) {
                    Ok(version) => Some(version),
                    Err(error) => {
                        self.diagnostics.push(Diagnostic::InvalidVersion(error));
                        None
                    }
                },
                None => {
                    self.diagnostics.push(Diagnostic::MissingVersion);
                    None
                }
            };

            let name = match raw_config.name {
                Some(name) => {
                    if is_valid_name(&name) {
                        Some(name)
                    } else {
                        self.diagnostics.push(Diagnostic::InvalidName);
                        None
                    }
                }
                None => {
                    self.diagnostics.push(Diagnostic::MissingName);
                    None
                }
            };

            if raw_config.dependencies.is_some() {
                eprintln!("ingot dependencies are not yet supported")
            }

            Ok(IngotMetadata { name, version })
        } else {
            Err(Error::ConfigFileDoesNotExist)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        mem::take(&mut self.diagnostics)
    }
}

#[derive(Deserialize, Debug, Clone)]
struct IngotConfig {
    pub ingot: RawConfig,
}

#[derive(Deserialize, Debug, Clone)]
struct RawConfig {
    pub name: Option<SmolStr>,
    pub version: Option<SmolStr>,
    pub dependencies: Option<Table>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigFileDoesNotExist => write!(
                f,
                "a `{FE_CONFIG_SUFFIX}` file does not exist in the ingot directory"
            ),
            Self::FileReadError(error) => write!(f, "file read error: {error}"),
            Self::TomlParseError(error) => write!(f, "toml parse error: {error}"),
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingName => write!(f, "name is missing from the config"),
            Self::MissingVersion => write!(f, "version is missing from config"),
            Self::InvalidName => write!(f, "invalid name"),
            Self::InvalidVersion(error) => write!(f, "invalid semantic version: {error}"),
        }
    }
}

fn is_valid_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '-'
}

fn is_valid_name(s: &SmolStr) -> bool {
    s.chars().all(is_valid_name_char)
}
