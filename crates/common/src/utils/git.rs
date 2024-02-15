#[cfg(not(target_arch = "wasm32"))]
use git2::{FetchOptions, Oid, Repository};
use std::error::Error;
#[cfg(not(target_arch = "wasm32"))]
use std::path::Path;

/// Fetch and checkout the specified refspec from the remote repository without
/// fetching any additional history.
#[cfg(not(target_arch = "wasm32"))]
pub fn fetch_and_checkout<P: AsRef<Path>>(
    remote: &str,
    target_directory: P,
    refspec: &str,
) -> Result<(), Box<dyn Error>> {
    // We initialize the repo here so that we can be sure that we created a directory that
    // needs to be clean up in case of an error. If the init fails, there won't be anything
    // to clean up.
    let repo = Repository::init(&target_directory)?;
    let res = _fetch_and_checkout(remote, repo, refspec);
    if res.is_err() {
        std::fs::remove_dir_all(target_directory).expect("Failed to clean up directory");
    }

    res
}

#[cfg(not(target_arch = "wasm32"))]
fn _fetch_and_checkout(
    remote: &str,
    repo: Repository,
    refspec: &str,
) -> Result<(), Box<dyn Error>> {
    let mut remote = repo.remote("origin", remote)?;

    let mut fetch_options = FetchOptions::new();

    fetch_options.depth(1);

    // Fetch the specified SHA1 with depth 1
    if let Err(e) = remote.fetch(&[refspec], Some(&mut fetch_options), None) {
        if let (git2::ErrorClass::Net, git2::ErrorCode::GenericError) = (e.class(), e.code()) {
            // That's a pretty cryptic error for the common case of the refspec not existing.
            // We keep the cryptic error (because it might have other causes) but add a hint.
            return Err(format!("{}\nMake sure revision {} exists in remote", e, refspec).into());
        } else {
            return Err(e.into());
        }
    }

    // Find the fetched commit by SHA1
    let oid = Oid::from_str(refspec)?;
    let commit = repo.find_commit(oid)?;

    // Checkout the commit
    repo.checkout_tree(commit.as_object(), None)?;
    repo.set_head_detached(oid)?;

    Ok(())
}

#[cfg(target_arch = "wasm32")]
pub fn fetch_and_checkout(
    _remote: &str,
    _target_directory: &str,
    _refspec: &str,
) -> Result<(), Box<dyn Error>> {
    Err("Not supported on WASM".into())
}
