use camino::Utf8PathBuf;
use common::{indexmap::IndexMap, input::Version};
use serde::Deserialize;
use smol_str::SmolStr;
use std::{collections::HashMap, fs, mem::take};
use toml;

use crate::{
    path::{FullPathDescription, PathDescription},
    remote::GitDescription,
    Resolver,
};

use super::dependency::DependencyDescription;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub name: SmolStr,
    version: SmolStr,
    dependencies: HashMap<SmolStr, IngotAndAnyFilesDescription>,
}

impl Config {
    pub fn version(&self) -> Option<Version> {
        Some(Version::new(0, 0, 0))
    }

    pub fn dependency_descriptions(
        &self,
        source_path: &Utf8PathBuf,
        source_path_description: &FullPathDescription,
    ) -> Vec<DependencyDescription> {
        self.dependencies
            .iter()
            .map(|(name, ingot_description)| {
                let (ingot_description, target_path_description) = match ingot_description {
                    IngotAndAnyFilesDescription::Path(description) => (
                        description.ingot_description.clone(),
                        source_path_description.push_sub_path(&description.files_description),
                    ),
                    // git repo should be resolved below
                    IngotAndAnyFilesDescription::Remote(description) => (
                        description.ingot_description.clone(),
                        FullPathDescription::new_remote(&description.files_description),
                    ),
                    IngotAndAnyFilesDescription::Registered(description) => todo!(),
                };

                DependencyDescription {
                    name: name.clone(),
                    source_path: source_path.clone(),
                    target_path_description,
                    ingot_description,
                }
            })
            .collect()
    }
}

#[derive(Debug)]
pub enum ConfigResolutionError {
    ConfigNotFound,
    FileReadError(std::io::Error),
    TomlParseError(toml::de::Error),
}

#[derive(Debug, Clone)]
pub enum ConfigResolutionDiagnostic {
    InvalidVersionFormat,
    InvalidNameFormat,
}

pub struct ConfigResolver {
    diagnostics: Vec<ConfigResolutionDiagnostic>,
}

impl ConfigResolver {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
        }
    }
}

impl Resolver for ConfigResolver {
    type Description = Utf8PathBuf;
    type Resource = Config;
    type Error = ConfigResolutionError;
    type Diagnostic = ConfigResolutionDiagnostic;

    fn resolve(&mut self, description: &Utf8PathBuf) -> Result<Config, ConfigResolutionError> {
        let config_path = description.join("config.toml");

        let file_content =
            fs::read_to_string(&config_path).map_err(ConfigResolutionError::FileReadError)?;

        let config: Config =
            toml::from_str(&file_content).map_err(ConfigResolutionError::TomlParseError)?;

        Ok(config)
    }

    fn take_diagnostics(&mut self) -> Vec<ConfigResolutionDiagnostic> {
        take(&mut self.diagnostics)
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IngotDescription {
    version: Option<SmolStr>,
    // other fields like "features" would go here
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IngotAndFilesDescription<FD>
where
    FD: Clone,
{
    #[serde(flatten)]
    pub ingot_description: IngotDescription,
    #[serde(flatten)]
    pub files_description: FD,
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(untagged)]
pub enum IngotAndAnyFilesDescription {
    Path(IngotAndFilesDescription<PathDescription>),
    Remote(IngotAndFilesDescription<GitDescription>),
    Registered(IngotAndFilesDescription<()>),
}
