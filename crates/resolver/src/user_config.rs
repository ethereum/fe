use std::{fs, str::FromStr};

use crate::{
    path::PathDescription,
    remote::{GitDescription, GitResolutionError},
    Resolver,
};
use camino::Utf8PathBuf;
use common::home_dir::HomeDir;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct UserConfig {
    // could also include things like ingot registries and settings for the language server.
    pub core: CoreIngotDescription,
}

pub struct UserConfigResolver;

impl UserConfigResolver {
    pub fn new() -> Self {
        Self
    }
}

#[derive(Debug)]
pub enum UserConfigResolutionError {
    HomePathDoesNotExist,
    UserConfigDoesNotExist,
    FileReadError(std::io::Error),
    TomlParseError(toml::de::Error),
}

#[derive(Debug)]
pub enum UserConfigResolutionDiagnostic {
    CoreNotSet,
}

impl Resolver for UserConfigResolver {
    type Description = HomeDir;
    type Resource = UserConfig;
    type Error = UserConfigResolutionError;
    type Diagnostic = UserConfigResolutionDiagnostic;

    fn resolve(&mut self, description: &HomeDir) -> Result<UserConfig, UserConfigResolutionError> {
        let home_path = description.path();
        let config_path = home_path.join("config.toml");

        let file_content =
            fs::read_to_string(&config_path).map_err(UserConfigResolutionError::FileReadError)?;

        let config: UserConfig =
            toml::from_str(&file_content).map_err(UserConfigResolutionError::TomlParseError)?;

        Ok(config)
    }

    fn take_diagnostics(&mut self) -> Vec<UserConfigResolutionDiagnostic> {
        todo!()
    }
}

#[derive(Debug, Deserialize)]
pub enum CoreIngotDescription {
    Local(PathDescription),
    Remote(GitDescription),
}

pub enum CoreIngotResolutionError {
    CoreIngotPathDoesNotExist,
    RemoteResolutionError(GitResolutionError),
}

/*
pub struct CoreIngotResolver {
    // ...
}

impl Resolver for CoreIngotResolver {
    type Description = CoreIngotDescription;
    type Resource = Utf8PathBuf;
    type Error = CoreIngotResolutionError;

    fn resolve(&self, description: &CoreIngotDescription) -> Result<Ingot, IngotResolutionError> {
        // ...
    }
}
*/
