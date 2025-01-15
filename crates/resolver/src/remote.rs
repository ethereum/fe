use camino::Utf8PathBuf;
use git2::{Error as Git2Error, FetchOptions, Oid, Repository};
use serde::Deserialize;
use std::fs;

use crate::Resolver;

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct GitDescription {
    remote: String,
    refspec: String,
    path: Option<String>,
}

impl GitDescription {
    pub fn relative_path(&self) -> Utf8PathBuf {
        if let Some(ref path) = self.path {
            Utf8PathBuf::from(path)
        } else {
            Utf8PathBuf::from(format!("{}_{}", &self.remote, &self.refspec))
        }
    }
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

#[derive(Debug)]
pub enum GitResolutionDiagnostic {
    RefspecDoesNotMatch(GitDescription),
    UncommittedChanges(GitDescription),
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
    type Error = GitResolutionError;
    type Diagnostic = GitResolutionDiagnostic;

    fn resolve(&mut self, description: &GitDescription) -> Result<Utf8PathBuf, GitResolutionError> {
        let clone_path = self.default_clone_path.join(description.relative_path());

        if clone_path.exists() {
            let repo = Repository::open(&clone_path).map_err(GitResolutionError::from)?;

            if !repo.find_reference(&description.refspec).is_ok() {
                fs::remove_dir_all(&clone_path)?;
                self.clone_repo(&description.remote, &clone_path, &description.refspec)?;
            }
        } else {
            self.clone_repo(&description.remote, &clone_path, &description.refspec)?;
        }

        Ok(clone_path)
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        vec![]
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
