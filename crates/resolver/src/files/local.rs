use camino::Utf8PathBuf;
use serde::Deserialize;

use super::Resolver;

#[derive(Deserialize, Hash, Debug)]
pub struct LocalDesc {
    pub path: String,
}

pub struct LocalResolver;

pub enum LocalResolutionError {
    InvalidPath,
    PathDoesNotExist,
}

impl Resolver for LocalResolver {
    type Config = ();
    type ResourceDesc = LocalDesc;
    type Resource = Utf8PathBuf;
    type ResolutionError = LocalResolutionError;

    fn from_config(_: &Self::Config) -> Self {
        todo!()
    }

    fn resolve(&self, desc: &LocalDesc) -> Result<Utf8PathBuf, LocalResolutionError> {
        if let Ok(path) = Utf8PathBuf::try_from(desc.path.clone()) {
            if !path.exists() {
                Err(LocalResolutionError::PathDoesNotExist)
            } else {
                Ok(path)
            }
        } else {
            Err(LocalResolutionError::InvalidPath)
        }
    }
}
