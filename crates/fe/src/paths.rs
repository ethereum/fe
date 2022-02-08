use std::path::{Path, PathBuf};

// From https://github.com/rust-lang/cargo/blob/master/crates/cargo-util/src/paths.rs
// MIT/Apache2
// Copied here to remove cargo-specific env var check.

/// Returns an iterator that walks up the directory hierarchy towards the root.
///
/// Each item is a [`Path`]. It will start with the given path, finishing at
/// the root. If the `stop_root_at` parameter is given, it will stop at the
/// given path (which will be the last item).
pub fn ancestors<'a>(path: &'a Path, stop_root_at: Option<&Path>) -> PathAncestors<'a> {
    PathAncestors::new(path, stop_root_at)
}

pub struct PathAncestors<'a> {
    current: Option<&'a Path>,
    stop_at: Option<PathBuf>,
}

impl<'a> PathAncestors<'a> {
    fn new(path: &'a Path, stop_root_at: Option<&Path>) -> PathAncestors<'a> {
        let stop_at = stop_root_at.map(|p| p.to_path_buf());
        PathAncestors {
            current: Some(path),
            stop_at,
        }
    }
}

impl<'a> Iterator for PathAncestors<'a> {
    type Item = &'a Path;

    fn next(&mut self) -> Option<&'a Path> {
        if let Some(path) = self.current {
            self.current = path.parent();

            if let Some(ref stop_at) = self.stop_at {
                if path == stop_at {
                    self.current = None;
                }
            }

            Some(path)
        } else {
            None
        }
    }
}

/// Returns the path to the `file` in `pwd`, if it exists.
pub fn find_project_manifest_exact(pwd: &Path, file: &str) -> Option<PathBuf> {
    let manifest = pwd.join(file);

    if manifest.exists() {
        Some(manifest)
    } else {
        None
        // XXX anyhow::bail!("Could not find `{}` in `{}`", file, pwd.display())
    }
}

pub fn find_root_manifest_for_wd(cwd: &Path, file_name: &str) -> Option<PathBuf> {
    for current in ancestors(cwd, None) {
        let manifest = current.join(manifest_file_name);
        if manifest.exists() {
            return Some(manifest);
        }
    }
    None
}
