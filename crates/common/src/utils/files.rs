use serde::Deserialize;
use std::{fs, path::Path};
use toml::Table;

use indexmap::{indexmap, IndexMap};
use path_clean::PathClean;
use smol_str::SmolStr;
use walkdir::WalkDir;

use crate::utils::dirs::get_fe_deps;
use crate::utils::git;

const FE_TOML: &str = "fe.toml";

pub enum FileLoader {
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
                    .map_err(|err| {
                        format!("unable to canonicalize root project path {path}.\n{err}")
                    })?
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
            .join(FE_TOML)
            .to_str()
            .expect("unable to convert path to &str")
            .to_owned();
        let manifest = Manifest::load(loader, &manifest_path)?;
        let name = manifest.name;
        let version = manifest.version;

        let mut dependencies = vec![];
        let mut errors = vec![];

        if let Some(deps) = &manifest.dependencies {
            for (name, value) in deps {
                match Dependency::new(loader, name, path, value) {
                    Ok(dep) => dependencies.push(dep),
                    Err(dep_err) => {
                        errors.push(format!("Misconfigured dependency {name}:\n{dep_err}"))
                    }
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors.join("\n"));
        }

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
    pub kind: DependencyKind,
}

pub trait DependencyResolver {
    fn resolve(dep: &Dependency, loader: &FileLoader) -> Result<ProjectFiles, String>;
}

#[derive(Clone)]
pub struct LocalDependency;

impl DependencyResolver for LocalDependency {
    fn resolve(dep: &Dependency, loader: &FileLoader) -> Result<ProjectFiles, String> {
        let project = ProjectFiles::load(loader, &dep.canonicalized_path)?;

        let mut errors = vec![];

        if project.mode == ProjectMode::Main {
            errors.push(format!("{} is not a library", project.name));
        }

        if project.name != dep.name {
            errors.push(format!("Name mismatch: {} =/= {}", project.name, dep.name));
        }

        if let Some(version) = &dep.version {
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
                dep.name,
                dep.canonicalized_path,
                errors.join("\n")
            ))
        }
    }
}

#[derive(Clone)]
pub struct GitDependency {
    source: String,
    rev: String,
}

impl DependencyResolver for GitDependency {
    fn resolve(dep: &Dependency, loader: &FileLoader) -> Result<ProjectFiles, String> {
        if let DependencyKind::Git(GitDependency { source, rev }) = &dep.kind {
            if let Err(e) = git::fetch_and_checkout(source, dep.canonicalized_path.as_str(), rev) {
                return Err(format!(
                    "Unable to clone git dependency {}.\n{}",
                    dep.name, e
                ));
            }

            // Load it like any local dependency which will include additional checks.
            return LocalDependency::resolve(dep, loader);
        }
        Err(format!("Could not resolve git dependency {}", dep.name))
    }
}

#[derive(Clone)]
pub enum DependencyKind {
    Local(LocalDependency),
    Git(GitDependency),
}

impl Dependency {
    fn new(
        loader: &FileLoader,
        name: &str,
        orig_path: &str,
        value: &toml::Value,
    ) -> Result<Self, String> {
        let join_path = |path: &str| {
            loader.canonicalize_path(
                Path::new(orig_path)
                    .join(path)
                    .to_str()
                    .expect("unable to convert path to &str"),
            )
        };

        match value {
            toml::Value::String(dep_path) => Ok(Dependency {
                name: name.into(),
                version: None,
                kind: DependencyKind::Local(LocalDependency),
                canonicalized_path: join_path(dep_path)?,
            }),
            toml::Value::Table(table) => {
                let version = table
                    .get("version")
                    .map(|version| version.as_str().unwrap().into());

                let return_local_dep = |path| {
                    Ok::<Dependency, String>(Dependency {
                        name: name.into(),
                        version: version.clone(),
                        canonicalized_path: join_path(path)?,

                        kind: DependencyKind::Local(LocalDependency),
                    })
                };

                match (table.get("source"), table.get("rev"), table.get("path")) {
                    (Some(toml::Value::String(source)), Some(toml::Value::String(rev)), None) => {
                        let dep_path = get_fe_deps().join(format!("{}-{}", name, rev));
                        if dep_path.exists() {
                            // Should we at least perform some kind of integrity check here? We currently treat an
                            // existing directory as a valid dependency no matter what.
                            return_local_dep(dep_path.to_str().unwrap())
                        } else {
                            fs::create_dir_all(&dep_path).unwrap();
                            Ok(Dependency {
                                name: name.into(),
                                version: version.clone(),
                                canonicalized_path: loader.canonicalize_path(
                                    Path::new(orig_path)
                                        .join(dep_path)
                                        .to_str()
                                        .expect("unable to convert path to &str"),
                                )?,
                                kind: DependencyKind::Git(GitDependency {
                                    source: source.into(),
                                    rev: rev.into(),
                                }),
                            })
                        }
                    }
                    (Some(_), Some(_), Some(_)) => {
                        Err("`path` can not be used together with `rev` and `source`".into())
                    }
                    (Some(_), None, _) => Err("`source` specified but no `rev` given".into()),
                    (None, Some(_), _) => Err("`rev` specified but no `source` given".into()),
                    (None, None, Some(toml::Value::String(path))) => return_local_dep(path),
                    _ => Err("Dependency isn't well formed".into()),
                }
            }
            _ => Err("unsupported toml type".into()),
        }
    }

    fn resolve(&self, loader: &FileLoader) -> Result<ProjectFiles, String> {
        match &self.kind {
            DependencyKind::Local(_) => LocalDependency::resolve(self, loader),
            DependencyKind::Git(_) => GitDependency::resolve(self, loader),
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

/// Returns the root path of the current Fe project
pub fn get_project_root() -> Option<String> {
    let current_dir = std::env::current_dir().expect("Unable to get current directory");

    let mut current_path = current_dir.clone();
    loop {
        let fe_toml_path = current_path.join(FE_TOML);
        if fe_toml_path.is_file() {
            return fe_toml_path
                .parent()
                .map(|val| val.to_string_lossy().to_string());
        }

        if !current_path.pop() {
            break;
        }
    }

    None
}
