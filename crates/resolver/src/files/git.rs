use camino::Utf8PathBuf;
use serde::Deserialize;

use crate::Resolver;

#[derive(Deserialize)]
pub struct GitDesc {
    remote: String,
    refspec: String,
    local_path: Option<String>,
}

pub struct GitResolver {
    pub default_clone_path: Utf8PathBuf,
}

pub struct GitResolutionError;

impl Resolver for GitResolver {
    type Config = ();
    type ResourceDesc = GitDesc;
    type Resource = Utf8PathBuf;
    type ResolutionError = GitResolutionError;

    fn from_config(_: &Self::Config) -> Self {
        todo!("")
    }

    fn resolve(&self, desc: &GitDesc) -> Result<Utf8PathBuf, GitResolutionError> {
        // check to see if the dep is already in the dep directory
        // check to see if the repo is already cached on the local fs
        // if it is, then check to see if the repo is valid with respect to the CID or refspec
        // if the repo is not cached, clone it
        // copy to dep directory
        // resolve the repo path
        todo!("")
    }
}

// use git2::{FetchOptions, Oid, Repository};
// use std::error::Error;
// use std::path::Path;

// /// Fetch and checkout the specified refspec from the remote repository without
// /// fetching any additional history.
// pub fn fetch_and_checkout<P: AsRef<Path>>(
//     remote: &str,
//     target_directory: P,
//     refspec: &str,
// ) -> Result<(), Box<dyn Error>> {
//     // We initialize the repo here so that we can be sure that we created a directory that
//     // needs to be clean up in case of an error. If the init fails, there won't be anything
//     // to clean up.
//     let repo = Repository::init(&target_directory)?;
//     let res = _fetch_and_checkout(remote, repo, refspec);
//     if res.is_err() {
//         std::fs::remove_dir_all(target_directory).expect("Failed to clean up directory");
//     }

//     res
// }

// fn _fetch_and_checkout(
//     remote: &str,
//     repo: Repository,
//     refspec: &str,
// ) -> Result<(), Box<dyn Error>> {
//     let mut remote = repo.remote("origin", remote)?;

//     let mut fetch_options = FetchOptions::new();

//     fetch_options.depth(1);

//     // Fetch the specified SHA1 with depth 1
//     if let Err(e) = remote.fetch(&[refspec], Some(&mut fetch_options), None) {
//         if let (git2::ErrorClass::Net, git2::ErrorCode::GenericError) = (e.class(), e.code()) {
//             // That's a pretty cryptic error for the common case of the refspec not existing.
//             // We keep the cryptic error (because it might have other causes) but add a hint.
//             return Err(format!("{}\nMake sure revision {} exists in remote", e, refspec).into());
//         } else {
//             return Err(e.into());
//         }
//     }

//     // Find the fetched commit by SHA1
//     let oid = Oid::from_str(refspec)?;
//     let commit = repo.find_commit(oid)?;

//     // Checkout the commit
//     repo.checkout_tree(commit.as_object(), None)?;
//     repo.set_head_detached(oid)?;

//     Ok(())
// }
