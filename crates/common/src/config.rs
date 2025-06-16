use camino::Utf8PathBuf;
use smol_str::SmolStr;
use toml::Value;
use url::Url;

use crate::{ingot::Version, urlext::UrlExt};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Config {
    pub ingot: IngotMetadata,
    pub dependencies: Vec<Dependency>,
    pub diagnostics: Vec<ConfigDiagnostics>,
}

impl Config {
    pub fn from_string(content: String) -> Self {
        let mut diagnostics = Vec::new();
        let mut ingot = IngotMetadata::default();
        let mut dependencies = Vec::new();

        let parsed: Value = match content.parse() {
            Ok(v) => v,
            Err(_) => {
                diagnostics.push(ConfigDiagnostics::UnrecognizedField);
                return Self {
                    ingot,
                    dependencies,
                    diagnostics,
                };
            }
        };

        // Parse [ingot] section
        if let Some(table) = parsed.get("ingot").and_then(|v| v.as_table()) {
            if let Some(name_val) = table.get("name") {
                match name_val.as_str() {
                    Some(name) => ingot.name = Some(SmolStr::new(name)),
                    None => diagnostics.push(ConfigDiagnostics::InvalidName),
                }
            } else {
                diagnostics.push(ConfigDiagnostics::MissingName);
            }

            if let Some(version_val) = table.get("version") {
                match version_val.as_str().and_then(|v| v.parse().ok()) {
                    Some(ver) => ingot.version = Some(ver),
                    None => diagnostics.push(ConfigDiagnostics::InvalidVersion),
                }
            } else {
                diagnostics.push(ConfigDiagnostics::MissingVersion);
            }
        } else {
            diagnostics.push(ConfigDiagnostics::MissingName);
            diagnostics.push(ConfigDiagnostics::MissingVersion);
        }

        // Parse [dependencies]
        if let Some(deps) = parsed.get("dependencies").and_then(|v| v.as_table()) {
            for (alias, value) in deps {
                match value {
                    Value::String(path) => {
                        dependencies.push(Dependency::path(alias.into(), Utf8PathBuf::from(path)));
                    }
                    Value::Table(tbl) => {
                        let path = tbl.get("path").and_then(|v| v.as_str());
                        if let Some(path_str) = path {
                            let mut args = IngotArguments::default();
                            if let Some(name) = tbl.get("name").and_then(|v| v.as_str()) {
                                args.name = Some(SmolStr::new(name));
                            }
                            if let Some(version_str) = tbl.get("version").and_then(|v| v.as_str()) {
                                if let Ok(ver) = version_str.parse() {
                                    args.version = Some(ver);
                                } else {
                                    diagnostics.push(ConfigDiagnostics::InvalidVersion);
                                }
                            }
                            dependencies.push(Dependency::path_with_arguments(
                                alias.into(),
                                Utf8PathBuf::from(path.unwrap()),
                                args,
                            ));
                        } else {
                            diagnostics.push(ConfigDiagnostics::InvalidDependencyDescription);
                        }
                    }
                    _ => diagnostics.push(ConfigDiagnostics::InvalidDependencyDescription),
                }
            }
        }

        Config {
            ingot,
            dependencies,
            diagnostics,
        }
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
    PathWithArguments {
        path: Utf8PathBuf,
        arguments: IngotArguments,
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
        arguments: IngotArguments,
    ) -> Self {
        Self {
            alias,
            description: DependencyDescription::PathWithArguments { path, arguments },
        }
    }

    pub fn based(&self, base_url: &Url) -> BasedDependency {
        match &self.description {
            // base_url.join(path.as_str()).unwrap().directory().unwrap()
            DependencyDescription::Path(path) => BasedDependency {
                alias: self.alias.clone(),
                arguments: IngotArguments::default(),
                url: base_url.join_directory(path).unwrap(),
            },
            DependencyDescription::PathWithArguments { path, arguments } => BasedDependency {
                alias: self.alias.clone(),
                arguments: IngotArguments::default(),
                url: base_url.join_directory(path).unwrap(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasedDependency {
    pub alias: SmolStr,
    pub arguments: IngotArguments,
    pub url: Url,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct IngotArguments {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConfigDiagnostics {
    MissingName,
    MissingVersion,
    InvalidName,
    InvalidVersion,
    InvalidDependencyAlias,
    InvalidDependencyDescription,
    UnrecognizedField,
}
