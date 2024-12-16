use camino::Utf8PathBuf;
use git2::{Error as Git2Error, FetchOptions, Oid, Repository};
use serde::Deserialize;
use std::fs;

use crate::Resolver;

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct GitDescription {
    remote: String,
    refspec: String,
    local_path: Option<String>,
}

pub struct GitResolver {
    default_clone_path: Utf8PathBuf,
}

impl Default for GitResolver {
    fn default() -> Self {
        GitResolver {
            default_clone_path: Utf8PathBuf::from(""),
        }
    }
}

#[derive(Debug)]
pub enum GitResolutionError {
    GitError(Git2Error),
    IoError(std::io::Error),
    InvalidOid,
}

impl From<Git2Error> for GitResolutionError {
    fn from(error: Git2Error) -> Self {
        GitResolutionError::GitError(error)
    }
}

impl From<std::io::Error> for GitResolutionError {
    fn from(error: std::io::Error) -> Self {
        GitResolutionError::IoError(error)
    }
}

impl std::fmt::Display for GitResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GitResolutionError::GitError(e) => write!(f, "Git error: {}", e),
            GitResolutionError::IoError(e) => write!(f, "IO error: {}", e),
            GitResolutionError::InvalidOid => write!(f, "Invalid OID for refspec"),
        }
    }
}

impl Resolver for GitResolver {
    type Description = GitDescription;
    type Resource = Utf8PathBuf;
    type ResolutionError = GitResolutionError;

    fn resolve(&self, desc: &GitDescription) -> Result<Utf8PathBuf, GitResolutionError> {
        let clone_path = if let Some(ref local_path) = desc.local_path {
            Utf8PathBuf::from(local_path)
        } else {
            self.default_clone_path
                .join(format!("{}_{}", &desc.remote, &desc.refspec))
        };

        if clone_path.exists() {
            let repo = Repository::open(&clone_path).map_err(GitResolutionError::from)?;

            if !repo.find_reference(&desc.refspec).is_ok() {
                fs::remove_dir_all(&clone_path)?;
                self.clone_repo(&desc.remote, &clone_path, &desc.refspec)?;
            }
        } else {
            self.clone_repo(&desc.remote, &clone_path, &desc.refspec)?;
        }

        Ok(clone_path)
    }
}

impl GitResolver {
    fn clone_repo(
        &self,
        remote: &str,
        target_directory: &Utf8PathBuf,
        refspec: &str,
    ) -> Result<(), GitResolutionError> {
        let repo = Repository::init(target_directory)?;

        self.fetch_and_checkout(remote, &repo, refspec)?;

        Ok(())
    }

    fn fetch_and_checkout(
        &self,
        remote: &str,
        repo: &Repository,
        refspec: &str,
    ) -> Result<(), GitResolutionError> {
        let mut remote = repo.remote("origin", remote)?;

        let mut fetch_options = FetchOptions::new();
        fetch_options.depth(1);

        remote.fetch(&[refspec], Some(&mut fetch_options), None)?;

        let oid = Oid::from_str(refspec).map_err(|_| GitResolutionError::InvalidOid)?;
        let commit = repo.find_commit(oid)?;

        repo.checkout_tree(commit.as_object(), None)?;
        repo.set_head_detached(oid)?;

        Ok(())
    }
}
