use camino::Utf8PathBuf;
use serde::Deserialize;

use crate::remote::GitDescription;

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct PathDescription {
    pub path: String,
}

#[derive(Clone, Debug)]
enum PathHead {
    Local(Utf8PathBuf),
    Remote(GitDescription),
}

#[derive(Clone, Debug)]
pub struct FullPathDescription {
    head: PathHead,
    sub_paths: Vec<PathDescription>,
}

impl FullPathDescription {
    pub fn new_local(local_path: &Utf8PathBuf) -> Self {
        FullPathDescription {
            head: PathHead::Local(local_path.clone()),
            sub_paths: vec![],
        }
    }

    pub fn new_remote(git_description: &GitDescription) -> Self {
        FullPathDescription {
            head: PathHead::Remote(git_description.clone()),
            sub_paths: vec![],
        }
    }

    pub fn path(&self) -> Utf8PathBuf {
        match &self.head {
            PathHead::Local(local_path) => {
                self.sub_paths
                    .iter()
                    .fold(local_path.clone(), |acc, path_description| {
                        acc.join(Utf8PathBuf::from(path_description.path.to_owned()))
                            .canonicalize_utf8()
                            .unwrap()
                    })
            }
            PathHead::Remote(git_description) => self.sub_paths.iter().fold(
                git_description.relative_path(),
                |acc, path_description| {
                    acc.join(Utf8PathBuf::from(path_description.path.to_owned()))
                        .canonicalize_utf8()
                        .unwrap()
                },
            ),
        }
    }

    pub fn push_sub_path(&self, path_description: &PathDescription) -> Self {
        let mut sub_paths = self.sub_paths.clone();
        sub_paths.push(path_description.clone());
        Self {
            head: self.head.clone(),
            sub_paths,
        }
    }
}
