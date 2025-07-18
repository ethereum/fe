use std::{fmt::Display, str::FromStr};

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
                    Some(name) if is_valid_name(name) => metadata.name = Some(SmolStr::new(name)),
                    Some(name) => {
                        diagnostics.push(ConfigDiagnostic::InvalidName(SmolStr::new(name)))
                    }
                    None => diagnostics.push(ConfigDiagnostic::InvalidName(SmolStr::new(
                        name.to_string(),
                    ))),
                }
            } else {
                diagnostics.push(ConfigDiagnostic::MissingName);
            }

            if let Some(version) = table.get("version") {
                match version.as_str().and_then(|value| value.parse().ok()) {
                    Some(version) => metadata.version = Some(version),
                    None => diagnostics.push(ConfigDiagnostic::InvalidVersion(SmolStr::from(
                        version.to_string(),
                    ))),
                }
            } else {
                diagnostics.push(ConfigDiagnostic::MissingVersion);
            }
        } else {
            diagnostics.push(ConfigDiagnostic::MissingIngotMetadata);
        }

        if let Some(table) = parsed
            .get("dependencies")
            .and_then(|value| value.as_table())
        {
            diagnostics.push(ConfigDiagnostic::DependenciesNotYetSupported);
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
                                    diagnostics
                                        .push(ConfigDiagnostic::InvalidVersion(version.into()));
                                }
                            }
                            dependencies.push(Dependency::path_with_arguments(
                                alias.into(),
                                Utf8PathBuf::from(path),
                                parameters,
                            ));
                        } else {
                            diagnostics.push(ConfigDiagnostic::MissingDependencyPath {
                                alias: alias.into(),
                                description: value.to_string(),
                            });
                        }
                    }
                    value => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                        field: "dependencies".into(),
                        found: value.type_str().to_lowercase().into(),
                        expected: Some("table".into()),
                    }),
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

    pub fn formatted_diagnostics(&self) -> Option<String> {
        if self.diagnostics.is_empty() {
            None
        } else {
            Some(
                self.diagnostics
                    .iter()
                    .map(|diag| format!("  {diag}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        }
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
    MissingIngotMetadata,
    MissingName,
    MissingVersion,
    InvalidName(SmolStr),
    InvalidVersion(SmolStr),
    InvalidDependencyAlias(SmolStr),
    InvalidDependencyName(SmolStr),
    InvalidDependencyVersion(SmolStr),
    InvalidTomlSyntax(String),
    MissingDependencyPath {
        alias: SmolStr,
        description: String,
    },
    UnexpectedTomlData {
        field: SmolStr,
        found: SmolStr,
        expected: Option<SmolStr>,
    },
    /// TODO: push diagnostics for fields that should not exist
    // UnrecognizedField(SmolStr),
    DependenciesNotYetSupported,
}

impl Display for ConfigDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingIngotMetadata => write!(f, "missing ingot metadata"),
            Self::MissingName => write!(f, "missing ingot name"),
            Self::MissingVersion => write!(f, "missing ingot version"),
            Self::InvalidName(name) => write!(f, "invalid ingot name \"{name}\""),
            Self::InvalidVersion(version) => write!(f, "invalid ingot version \"{version}\""),
            Self::InvalidDependencyAlias(alias) => {
                write!(f, "invalid dependency alias \"{alias}\"")
            }
            Self::InvalidDependencyName(name) => {
                write!(f, "invalid dependency name \"{name}\"")
            }
            Self::InvalidDependencyVersion(version) => {
                write!(f, "invalid dependency version \"{version}\"")
            }
            Self::InvalidTomlSyntax(err) => write!(f, "invalid TOML syntax: {err}"),
            Self::MissingDependencyPath { alias, description } => write!(
                f,
                "the dependency \"{alias}\" is missing a path argument \"{description}\""
            ),
            Self::UnexpectedTomlData {
                field,
                found,
                expected,
            } => {
                if let Some(expected) = expected {
                    write!(
                        f,
                        "expected a {expected} in field {field}, but found a {found}"
                    )
                } else {
                    write!(f, "unexpected field {field}")
                }
            }
            Self::DependenciesNotYetSupported => write!(f, "dependencies are not yet supported"),
        }
    }
}

fn is_valid_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '-'
}

fn is_valid_name(s: &str) -> bool {
    s.chars().all(is_valid_name_char)
}
