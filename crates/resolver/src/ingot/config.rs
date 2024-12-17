use camino::Utf8PathBuf;
use serde::Deserialize;
use smol_str::SmolStr;
use std::{collections::HashMap, fs};
use toml;

use crate::Resolver;

use super::AnyIngotDescription;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub name: SmolStr,
    pub version: SmolStr,
    pub dependencies: HashMap<SmolStr, AnyIngotDescription>,
}

pub enum InvalidConfigError {
    MissingVersion,
    MissingName,
    UnrecognizedField(String),
}

#[derive(Debug)]
pub enum ConfigResolutionError {
    ConfigNotFound,
    FileReadError(std::io::Error),
    TomlParseError(toml::de::Error),
    InvalidConfig(String),
}

pub struct ConfigResolver;

impl Resolver for ConfigResolver {
    type Description = Utf8PathBuf;
    type Resource = Config;
    type ResolutionError = ConfigResolutionError;

    fn resolve(&self, desc: &Utf8PathBuf) -> Result<Config, ConfigResolutionError> {
        let config_path = desc.join("fe.toml");

        let file_content =
            fs::read_to_string(&config_path).map_err(ConfigResolutionError::FileReadError)?;

        let config: Config =
            toml::from_str(&file_content).map_err(ConfigResolutionError::TomlParseError)?;

        if config.name.is_empty() {
            return Err(ConfigResolutionError::InvalidConfig(
                "Invalid configuration: 'name' field is missing or empty.".to_string(),
            ));
        }

        if config.version.is_empty() {
            return Err(ConfigResolutionError::InvalidConfig(
                "Invalid configuration: 'version' field is missing or empty.".to_string(),
            ));
        }

        Ok(config)
    }
}
