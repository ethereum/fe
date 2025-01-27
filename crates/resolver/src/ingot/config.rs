use camino::Utf8PathBuf;
use common::{indexmap::IndexMap, input::Version};
use serde::Deserialize;
use smol_str::SmolStr;
use std::{fmt, fs, mem};
use toml::{self, Table};

use crate::Resolver;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

#[derive(Default, Debug, Clone)]
pub struct Config {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
    pub dependencies: Option<IndexMap<SmolStr, DependencyDescription>>,
}

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
    InvalidVersion(semver::Error),
}

#[derive(Default)]
pub struct ConfigResolver {
    diagnostics: Vec<Diagnostic>,
}

impl Resolver for ConfigResolver {
    type Description = Utf8PathBuf;
    type Resource = Config;
    type Error = Error;
    type Diagnostic = Diagnostic;

    fn resolve(&mut self, ingot_path: &Utf8PathBuf) -> Result<Config, Error> {
        let config_path = if ingot_path.ends_with(FE_CONFIG_SUFFIX) {
            ingot_path.clone()
        } else {
            ingot_path.join(FE_CONFIG_SUFFIX)
        };

        if config_path.exists() {
            let file_content = fs::read_to_string(&config_path).map_err(Error::FileReadError)?;

            let raw_config =
                toml::from_str::<RawConfig>(&file_content).map_err(Error::TomlParseError)?;

            let ingot = raw_config.ingot.unwrap();

            let version = match ingot.version {
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

            let name = match ingot.name {
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

            let dependencies = raw_config.dependencies.map(|dependencies| {
                dependencies
                    .iter()
                    .map(|(name, description)| {
                        (
                            SmolStr::from(name),
                            DependencyDescription {
                                ingot: IngotDescription {
                                    version: Version::new(0, 0, 0),
                                },
                                files: FilesDescription::Path(
                                    description
                                        .as_table()
                                        .expect(&config_path.to_string())
                                        .get("path")
                                        .unwrap()
                                        .as_str()
                                        .unwrap()
                                        .to_string(),
                                ),
                            },
                        )
                    })
                    .collect()
            });

            Ok(Config {
                name,
                version,
                dependencies,
            })
        } else {
            Err(Error::ConfigFileDoesNotExist)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        mem::take(&mut self.diagnostics)
    }
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

#[derive(Debug, Clone)]
pub enum FilesDescription {
    Path(String),
}

#[derive(Debug, Clone)]
pub struct IngotDescription {
    pub version: Version,
}

#[derive(Debug, Clone)]
pub struct DependencyDescription {
    pub ingot: IngotDescription,
    pub files: FilesDescription,
}

#[derive(Deserialize, Debug, Clone)]
struct RawConfig {
    pub ingot: Option<RawIngotConfig>,
    pub dependencies: Option<Table>,
}

#[derive(Deserialize, Debug, Clone)]
struct RawIngotConfig {
    pub name: Option<SmolStr>,
    pub version: Option<SmolStr>,
}

// #[derive(Deserialize, Debug, Clone)]
// pub struct RawPathDescription {
//     path: String,
// }
//
// #[derive(Deserialize, Debug, Clone)]
// #[serde(untagged)]
// pub(crate) enum RawFilesDescription {
//     Path(RawPathDescription),
//     Remote(GitDescription),
// }
//
// #[derive(Deserialize, Debug, Clone)]
// pub(crate) struct RawIngotDescription {
//     pub version: SmolStr,
// }
//
// #[derive(Deserialize, Debug, Clone)]
// pub(crate) struct RawDependencyDescription {
//     #[serde(flatten)]
//     pub ingot: RawIngotDescription,
//     #[serde(flatten)]
//     pub files: RawFilesDescription,
// }
