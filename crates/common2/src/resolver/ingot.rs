use camino::Utf8PathBuf;
use indexmap::IndexSet;
use semver::Version;
use serde::Deserialize;
use smol_str::SmolStr;
use toml::Table;

use super::{fs::LazyDirResolver, git::GitResolver, Resolver};

#[derive(Deserialize)]
pub struct IngotDesc<FD> {
    // name: SmolStr,
    version: Option<String>,
    #[serde(flatten)]
    files_desc: FD,
}

#[derive(Deserialize)]
pub struct Config {
    pub name: SmolStr,
    pub version: SmolStr,
    dependencies: Table,
}

impl Config {
    fn dependencies<FD>(&self) -> IndexSet<IngotDesc<FD>> {
        todo!("dependency")
    }
}

trait IngotFiles {
    fn config(&self) -> Filestate<Config>;
    fn src(&self) -> IndexSet<(Utf8PathBuf, Vec<u8>)>;
    fn root_path(&self) -> Utf8PathBuf;
}

pub struct Ingot<F>
where
    F: IngotFiles,
{
    // desc_name: SmolStr,
    desc_version: Version,
    files: F,
}

impl<F> Ingot<F>
where
    F: IngotFiles,
{
    pub fn src_files(&self) -> IngotDesc<Utf8PathBuf> {
        self.files.root_path()
    }
}

pub struct IngotResolver<FR> {
    files_resolver: FR,
    diagnostics: (),
}

pub type IngotFsResolver = IngotResolver<LazyDirResolver>;
// my_dep = { version = "1.0.0", path = "path/to/dep" }
pub type IngotGitResolver = IngotResolver<GitResolver>;

impl<FR> Resolver for IngotResolver<FR>
where
    FR: Resolver,
    FR::Resource: IngotFiles,
{
    type ResourceDesc = IngotDesc<FR::ResourceDesc>;
    type Resource = Ingot<FR::Resource>;
    type ResolutionError = ();

    fn resolve(&self, desc: &IngotDesc<FR::ResourceDesc>) -> Result<Ingot<FR::Resource>, ()> {
        Err(())
    }
}
