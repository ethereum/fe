use serde::Deserialize;
use std::{fs, path::Path};
use toml::Table;

use indexmap::{indexmap, IndexMap};
use path_clean::PathClean;
use smol_str::SmolStr;
use walkdir::WalkDir;

enum FileLoader {
    Static(Vec<(&'static str, &'static str)>),
    Fs,
}

impl FileLoader {
    pub fn canonicalize_path(&self, path: &str) -> Result<SmolStr, String> {
        match self {
            FileLoader::Static(_) => Ok(SmolStr::new(
                Path::new(path).clean().to_str().expect("path clean failed"),
            )),
            FileLoader::Fs => Ok(SmolStr::new(
                fs::canonicalize(path)
                    .map_err(|err| format!("unable to canonicalize root project path.\n{err}"))?
                    .to_str()
                    .expect("could not convert path to string"),
            )),
        }
    }

    pub fn fe_files(&self, path: &str) -> Result<Vec<(String, String)>, String> {
        match self {
            FileLoader::Static(files) => Ok(files
                .iter()
                .filter_map(|(file_path, content)| {
                    if file_path.starts_with(path) && file_path.ends_with(".fe") {
                        Some((file_path.to_string(), content.to_string()))
                    } else {
                        None
                    }
                })
                .collect()),
            FileLoader::Fs => {
                let entries = WalkDir::new(path);
                let mut files = vec![];

                for entry in entries.into_iter() {
                    let entry =
                        entry.map_err(|err| format!("Error loading source files.\n{err}"))?;
                    let path = entry.path();

                    if path.is_file()
                        && path.extension().and_then(std::ffi::OsStr::to_str) == Some("fe")
                    {
                        let content = std::fs::read_to_string(path)
                            .map_err(|err| format!("Unable to read src file.\n{err}"))?;
                        files.push((path.to_string_lossy().to_string(), content));
                    }
                }

                Ok(files)
            }
        }
    }

    pub fn file_content(&self, path: &str) -> Result<String, String> {
        match self {
            FileLoader::Static(files) => {
                match files.iter().find(|(file_path, _)| file_path == &path) {
                    Some((_, content)) => Ok(content.to_string()),
                    None => Err(format!("could not load static file {}", path)),
                }
            }
            FileLoader::Fs => {
                std::fs::read_to_string(path).map_err(|err| format!("Unable to read file.\n{err}"))
            }
        }
    }
}

pub struct BuildFiles {
    pub root_project_path: SmolStr,
    pub project_files: IndexMap<SmolStr, ProjectFiles>,
}

impl BuildFiles {
    pub fn root_project_mode(&self) -> ProjectMode {
        self.project_files[&self.root_project_path].mode
    }

    /// Build files are loaded from the file system.
    pub fn load_fs(root_path: &str) -> Result<Self, String> {
        Self::load(&FileLoader::Fs, root_path)
    }

    /// Build files are loaded from static file vector.
    pub fn load_static(
        files: Vec<(&'static str, &'static str)>,
        root_path: &str,
    ) -> Result<Self, String> {
        Self::load(&FileLoader::Static(files), root_path)
    }

    fn load(loader: &FileLoader, root_project_path: &str) -> Result<Self, String> {
        let root_project_path = loader.canonicalize_path(root_project_path)?;

        // Map containing canonicalized project paths and their files.
        let mut project_files = indexmap! {
            root_project_path.clone() => ProjectFiles::load(loader, &root_project_path)?
        };

        // The root project is the first project to have unresolved dependencies.
        let mut unresolved_projects = vec![root_project_path.clone()];

        while let Some(unresolved_project_path) = unresolved_projects.pop() {
            // Iterate over each of `unresolved_projects` dependencies.
            for dependency in project_files[&unresolved_project_path].dependencies.clone() {
                if !project_files.contains_key(&dependency.canonicalized_path) {
                    // The dependency is being encountered for the first time.
                    let dep_project = dependency.resolve(loader)?;
                    project_files.insert(dependency.canonicalized_path.clone(), dep_project);
                    unresolved_projects.push(dependency.canonicalized_path);
                };
            }
        }

        Ok(Self {
            root_project_path,
            project_files,
        })
    }
}

pub struct ProjectFiles {
    pub name: SmolStr,
    pub version: SmolStr,
    pub mode: ProjectMode,
    pub dependencies: Vec<Dependency>,
    pub src: Vec<(String, String)>,
}

impl ProjectFiles {
    fn load(loader: &FileLoader, path: &str) -> Result<Self, String> {
        let manifest_path = Path::new(path)
            .join("fe.toml")
            .to_str()
            .expect("unable to convert path to &str")
            .to_owned();
        let manifest = Manifest::load(loader, &manifest_path)?;
        let name = manifest.name;
        let version = manifest.version;
        let dependencies = if let Some(dependencies) = &manifest.dependencies {
            dependencies
                .iter()
                .map(|(name, value)| Dependency::new(loader, name, path, value))
                .collect::<Result<_, _>>()?
        } else {
            vec![]
        };

        let src_path = Path::new(path)
            .join("src")
            .to_str()
            .expect("unable to convert path to &str")
            .to_owned();
        let src = loader.fe_files(&src_path)?;

        let mode = if src
            .iter()
            .any(|(file_path, _)| file_path.ends_with("main.fe"))
        {
            ProjectMode::Main
        } else if src
            .iter()
            .any(|(file_path, _)| file_path.ends_with("lib.fe"))
        {
            ProjectMode::Lib
        } else {
            return Err(format!("Unable to determine mode of {}. Consider adding src/main.fe or src/lib.fe file to the project.", name));
        };

        Ok(Self {
            name,
            version,
            mode,
            dependencies,
            src,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ProjectMode {
    Main,
    Lib,
}

#[derive(Clone)]
pub struct Dependency {
    pub name: SmolStr,
    pub version: Option<SmolStr>,
    pub canonicalized_path: SmolStr,
}

impl Dependency {
    fn new(
        loader: &FileLoader,
        name: &str,
        orig_path: &str,
        value: &toml::Value,
    ) -> Result<Self, String> {
        match value {
            toml::Value::String(dep_path) => Ok(Dependency {
                name: name.into(),
                version: None,
                canonicalized_path: loader.canonicalize_path(
                    Path::new(orig_path)
                        .join(dep_path)
                        .to_str()
                        .expect("unable to convert path to &str"),
                )?,
            }),
            toml::Value::Table(table) => {
                let dep_path = table.get("path").unwrap().as_str().unwrap();
                let version = table
                    .get("version")
                    .map(|version| version.as_str().unwrap().into());
                Ok(Dependency {
                    name: name.into(),
                    version,
                    canonicalized_path: loader.canonicalize_path(
                        Path::new(orig_path)
                            .join(dep_path)
                            .to_str()
                            .expect("unable to convert path to &str"),
                    )?,
                })
            }
            _ => Err("unsupported toml type".into()),
        }
    }

    fn resolve(&self, loader: &FileLoader) -> Result<ProjectFiles, String> {
        let project = ProjectFiles::load(loader, &self.canonicalized_path)?;

        let mut errors = vec![];

        if project.mode == ProjectMode::Main {
            errors.push(format!("{} is not a library", project.name));
        }

        if project.name != self.name {
            errors.push(format!("Name mismatch: {} =/= {}", project.name, self.name));
        }

        if let Some(version) = &self.version {
            if version != &project.version {
                errors.push(format!(
                    "Version mismatch: {} =/= {}",
                    project.version, version
                ));
            }
        }

        if errors.is_empty() {
            Ok(project)
        } else {
            Err(format!(
                "Unable to resolve {} at {} due to the following errors.\n{}",
                self.name,
                self.canonicalized_path,
                errors.join("\n")
            ))
        }
    }
}

#[derive(Deserialize)]
struct Manifest {
    pub name: SmolStr,
    pub version: SmolStr,
    dependencies: Option<Table>,
}

impl Manifest {
    pub fn load(loader: &FileLoader, path: &str) -> Result<Self, String> {
        let content = loader
            .file_content(path)
            .map_err(|err| format!("Failed to load manifest content from {path}.\n{err}"))?;
        let manifest: Manifest = toml::from_str(&content)
            .map_err(|err| format!("Failed to parse the content of {path}.\n{err}"))?;

        Ok(manifest)
    }
}
