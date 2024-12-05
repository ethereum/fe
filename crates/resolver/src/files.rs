use camino::Utf8PathBuf;
use git::{GitDesc, GitResolutionError};
use local::{LocalDesc, LocalResolutionError};
use serde::Deserialize;

use crate::Resolver;

mod git;
mod local;

#[derive(Deserialize, Hash, Debug)]
#[serde(untagged)]
pub enum AnyFilesDesc {
    Local(LocalDesc),
    Git(GitDesc),
}

pub struct AnyFilesResolver;

pub enum AnyFilesResolutionError {
    Local(LocalResolutionError),
    Git(GitResolutionError),
}

impl Resolver for AnyFilesResolver {
    type Config = ();
    type ResourceDesc = AnyFilesDesc;
    type Resource = Utf8PathBuf;
    type ResolutionError = AnyFilesResolutionError;

    fn from_config(config: &Self::Config) -> Self {
        todo!()
    }

    fn resolve(&self, desc: &AnyFilesDesc) -> Result<Utf8PathBuf, AnyFilesResolutionError> {
        todo!()
    }
}
