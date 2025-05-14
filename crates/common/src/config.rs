use camino::Utf8PathBuf;
use smol_str::SmolStr;
use toml::Value;

use crate::input::Version;

#[derive(Debug, Clone)]
pub struct Config {
    pub ingot: Ingot,
    pub dependencies: Vec<Dependency>,
    pub diagnostics: Vec<ConfigDiagnostics>,
}

impl Config {
    pub fn from_string(content: String) -> Self {
        let mut diagnostics = Vec::new();
        let mut ingot = Ingot::default();
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
                        dependencies.push(Dependency::Path(Utf8PathBuf::from(path)));
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
                            dependencies.push(Dependency::PathWithArguments {
                                path: Utf8PathBuf::from(path_str),
                                arguments: args,
                            });
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
}

#[derive(Debug, Default, Clone)]
pub struct Ingot {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone)]
pub enum Dependency {
    Path(Utf8PathBuf),
    PathWithArguments {
        path: Utf8PathBuf,
        arguments: IngotArguments,
    },
}

#[derive(Debug, Default, Clone)]
pub struct IngotArguments {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone)]
pub enum ConfigDiagnostics {
    MissingName,
    MissingVersion,
    InvalidName,
    InvalidVersion,
    InvalidDependencyAlias,
    InvalidDependencyDescription,
    UnrecognizedField,
}
