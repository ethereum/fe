use camino::Utf8PathBuf;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use smol_str::SmolStr;
use std::hash::Hash;
use std::{collections::HashMap, fs};
use toml;

use crate::Resolver;

use super::IngotDesc;

#[derive(Deserialize)]
pub struct Config<FD>
where
    FD: Hash,
{
    pub name: SmolStr,
    pub version: SmolStr,
    pub dependencies: HashMap<SmolStr, IngotDesc<FD>>,
}

#[derive(Debug)]
pub enum ConfigResolutionError {
    FileNotFound,
    FileReadError(std::io::Error),
    TomlParseError(toml::de::Error),
    InvalidConfig(String),
}

pub struct ConfigResolver<FR> {
    files_resolver: FR,
}

impl<FR> Resolver for ConfigResolver<FR>
where
    FR: Resolver,
    FR::ResourceDesc: Hash,
    FR::Resource: AsRef<Utf8PathBuf>,
    Config<FR::ResourceDesc>: DeserializeOwned,
{
    type Config = ();
    type ResourceDesc = IngotDesc<FR::ResourceDesc>;
    type Resource = Config<FR::ResourceDesc>;
    type ResolutionError = ConfigResolutionError;

    fn from_config(_config: &Self::Config) -> Self {
        todo!()
    }

    fn resolve(
        &self,
        desc: &IngotDesc<FR::ResourceDesc>,
    ) -> Result<Config<FR::ResourceDesc>, ConfigResolutionError> {
        let config_path = self
            .files_resolver
            .resolve(&desc.files_desc)
            .map_err(|_| ConfigResolutionError::FileNotFound)?
            .as_ref()
            .join("fe.toml");

        let file_content =
            fs::read_to_string(&config_path).map_err(ConfigResolutionError::FileReadError)?;

        let config: Config<FR::ResourceDesc> =
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
