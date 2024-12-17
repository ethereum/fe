use camino::Utf8PathBuf;
use serde::Deserialize;

use super::Resolver;

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct PathDescription {
    pub path: String,
}

pub struct PathResolver;

pub enum PathResolutionError {
    AbsolutePathDoesNotExist,
}

impl Resolver for PathResolver {
    type Description = Vec<PathDescription>;
    type Resource = Utf8PathBuf;
    type ResolutionError = PathResolutionError;

    fn resolve(&self, desc: &Vec<PathDescription>) -> Result<Utf8PathBuf, PathResolutionError> {
        todo!()
    }
}
