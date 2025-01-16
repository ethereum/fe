use std::fmt::Debug;

use crate::Resolver;
use crate::{path::FullPathDescription, remote::GitResolutionError};
use camino::Utf8PathBuf;
use semver::Version;
use smol_str::SmolStr;

use std::mem::take;

use super::config::{
    ConfigResolutionDiagnostic, ConfigResolutionError, ConfigResolver, IngotDescription,
};

#[derive(Clone, Debug)]
pub struct DependencyDescription {
    pub name: SmolStr,
    pub source_path: Utf8PathBuf,
    pub target_path_description: FullPathDescription,
    pub ingot_description: IngotDescription,
}

#[derive(Debug)]
pub enum DependencyResolutionError {
    GitResolutionError(GitResolutionError),
    TargetPathDoesNotExist,
    TargetConfigResolutionError(ConfigResolutionError),
}

#[derive(Debug, Clone)]
pub enum DependencyResolutionDiagnostic {
    IncompatibleVersion,
    TargetConfigResolutionError(ConfigResolutionDiagnostic),
}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub name: SmolStr,
    pub version: Version,
    pub source_path: Utf8PathBuf,
    pub target_path: Utf8PathBuf,
    pub sub_dependencies: Vec<DependencyDescription>,
}

pub struct DependencyResolver {
    config_resolver: ConfigResolver,
    diagnostics: Vec<DependencyResolutionDiagnostic>,
}

impl DependencyResolver {
    pub fn new() -> Self {
        Self {
            config_resolver: ConfigResolver::new(),
            diagnostics: vec![],
        }
    }
}

impl Resolver for DependencyResolver {
    type Description = DependencyDescription;
    type Resource = Dependency;
    type Error = DependencyResolutionError;
    type Diagnostic = DependencyResolutionDiagnostic;

    fn resolve(
        &mut self,
        description: &DependencyDescription,
    ) -> Result<Dependency, DependencyResolutionError> {
        let target_path = description.target_path_description.path();

        if !target_path.exists() {
            Err(DependencyResolutionError::TargetPathDoesNotExist)
        } else {
            match self.config_resolver.resolve(&target_path) {
                Ok(target_config) => {
                    let version = target_config.version().unwrap();
                    let sub_dependencies = target_config.dependency_descriptions(
                        &target_path,
                        &description.target_path_description,
                    );

                    // compare description and target config

                    let dependency = Dependency {
                        name: description.name.clone(),
                        version,
                        source_path: description.source_path.clone(),
                        target_path,
                        sub_dependencies,
                    };

                    Ok(dependency)
                }
                Err(error) => Err(DependencyResolutionError::TargetConfigResolutionError(
                    error,
                )),
            }
        }
    }

    fn take_diagnostics(&mut self) -> Vec<DependencyResolutionDiagnostic> {
        self.diagnostics.append(
            &mut self
                .config_resolver
                .take_diagnostics()
                .into_iter()
                .map(DependencyResolutionDiagnostic::TargetConfigResolutionError)
                .collect(),
        );

        take(&mut self.diagnostics)
    }
}
