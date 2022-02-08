use serde_derive::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub const MANIFEST_FILE_NAME: &str = "fe.toml";

pub struct IngotManifest {
    package: IngotPackage,
    dependencies: HashMap<String, IngotDependency>,
}

pub struct IngotPackage {
    name: String,
    source_dir: String,
}

pub struct IngotDependency {
    version: String,
    path: Option<String>,
    git: Option<String>,
    rev: Option<String>,
}

pub enum ProjectManifestError {}

// XXX remove file

// XXX error
fn find_manifest_in_dir(dir: &Path) -> Result<PathBuf, ()> {
    let mani = dir.join(MANIFEST_FILE_NAME);
    if mani.exists() {
        Ok(mani)
    } else if let Some(parent) = dir.parent() {
        find_manifest_in_dir(parent)
    } else {
        Err(())
    }
}
