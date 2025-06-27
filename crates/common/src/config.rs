use std::str::FromStr;

use camino::Utf8PathBuf;
use smol_str::SmolStr;
use toml::Value;
use url::Url;

use crate::{ingot::Version, urlext::UrlExt};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Config {
    pub metadata: IngotMetadata,
    pub dependencies: Vec<Dependency>,
    pub diagnostics: Vec<ConfigDiagnostic>,
}

impl Config {
    pub fn parse(content: &str) -> Result<Self, <Value as FromStr>::Err> {
        let mut diagnostics = Vec::new();
        let mut metadata = IngotMetadata::default();
        let mut dependencies = Vec::new();

        let parsed: Value = content.parse()?;

        if let Some(table) = parsed.get("ingot").and_then(|value| value.as_table()) {
            if let Some(name) = table.get("name") {
                match name.as_str() {
                    Some(name) => metadata.name = Some(SmolStr::new(name)),
                    None => diagnostics.push(ConfigDiagnostic::InvalidName),
                }
            } else {
                diagnostics.push(ConfigDiagnostic::MissingName);
            }

            if let Some(version) = table.get("version") {
                match version.as_str().and_then(|value| value.parse().ok()) {
                    Some(version) => metadata.version = Some(version),
                    None => diagnostics.push(ConfigDiagnostic::InvalidVersion),
                }
            } else {
                diagnostics.push(ConfigDiagnostic::MissingVersion);
            }
        } else {
            diagnostics.push(ConfigDiagnostic::MissingName);
            diagnostics.push(ConfigDiagnostic::MissingVersion);
        }

        if let Some(table) = parsed
            .get("dependencies")
            .and_then(|value| value.as_table())
        {
            for (alias, value) in table {
                match value {
                    Value::String(path) => {
                        dependencies.push(Dependency::path(alias.into(), Utf8PathBuf::from(path)));
                    }
                    Value::Table(table) => {
                        let path = table.get("path").and_then(|value| value.as_str());
                        if let Some(path) = path {
                            let mut parameters = DependencyParameters::default();
                            if let Some(name) = table.get("name").and_then(|value| value.as_str()) {
                                parameters.name = Some(SmolStr::new(name));
                            }
                            if let Some(version) =
                                table.get("version").and_then(|value| value.as_str())
                            {
                                if let Ok(version) = version.parse() {
                                    parameters.version = Some(version);
                                } else {
                                    diagnostics.push(ConfigDiagnostic::InvalidVersion);
                                }
                            }
                            dependencies.push(Dependency::path_with_arguments(
                                alias.into(),
                                Utf8PathBuf::from(path),
                                parameters,
                            ));
                        } else {
                            diagnostics.push(ConfigDiagnostic::InvalidDependencyDescription);
                        }
                    }
                    _ => diagnostics.push(ConfigDiagnostic::InvalidDependencyDescription),
                }
            }
        }

        Ok(Self {
            metadata,
            dependencies,
            diagnostics,
        })
    }

    pub fn based_dependencies(&self, base_url: &Url) -> Vec<BasedDependency> {
        self.dependencies
            .iter()
            .map(|dependency| dependency.based(base_url))
            .collect()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct IngotMetadata {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DependencyDescription {
    Path(Utf8PathBuf),
    PathWithParameters {
        path: Utf8PathBuf,
        parameters: DependencyParameters,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Dependency {
    pub alias: SmolStr,
    pub description: DependencyDescription,
}

impl Dependency {
    pub fn path(alias: SmolStr, path: Utf8PathBuf) -> Self {
        Self {
            alias,
            description: DependencyDescription::Path(path),
        }
    }

    pub fn path_with_arguments(
        alias: SmolStr,
        path: Utf8PathBuf,
        parameters: DependencyParameters,
    ) -> Self {
        Self {
            alias,
            description: DependencyDescription::PathWithParameters { path, parameters },
        }
    }

    pub fn based(&self, base_url: &Url) -> BasedDependency {
        match &self.description {
            // base_url.join(path.as_str()).unwrap().directory().unwrap()
            DependencyDescription::Path(path) => BasedDependency {
                alias: self.alias.clone(),
                parameters: DependencyParameters::default(),
                url: base_url.join_directory(path).unwrap(),
            },
            DependencyDescription::PathWithParameters {
                path,
                parameters: _,
            } => BasedDependency {
                alias: self.alias.clone(),
                parameters: DependencyParameters::default(),
                url: base_url.join_directory(path).unwrap(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasedDependency {
    pub alias: SmolStr,
    pub parameters: DependencyParameters,
    pub url: Url,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct DependencyParameters {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConfigDiagnostic {
    MissingName,
    MissingVersion,
    InvalidName,
    InvalidVersion,
    InvalidDependencyAlias,
    InvalidDependencyDescription,
    UnrecognizedField,
}

// fn is_valid_name_char(c: char) -> bool {
//     c.is_alphanumeric() || c == '-'
// }
//
// fn is_valid_name(s: &SmolStr) -> bool {
//     s.chars().all(is_valid_name_char)
// }
