use std::marker::PhantomData;

use camino::Utf8PathBuf;
use indexmap::IndexSet;
use semver::Version;
use serde::Deserialize;
use smol_str::SmolStr;
use toml::Table;

use super::{fs::LazyDirResolver, git::GitResolver, Resolver};

#[derive(Deserialize)]
pub struct IngotDesc<FD> {
    name: SmolStr,
    version: String,
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
    fn config(&self) -> Config;
    fn src(&self) -> IndexSet<(Utf8PathBuf, Vec<u8>)>;
    fn root_path(&self) -> Utf8PathBuf;
}

pub struct Ingot<F>
where
    F: IngotFiles,
{
    desc_name: SmolStr,
    desc_version: Version,
    files: F,
}

pub struct IngotResolver<FR> {
    files_resolver: FR,
}

pub type FsIngotResolver = IngotResolver<LazyDirResolver>;
pub type GitIngotResolver = IngotResolver<GitResolver>;

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
