use camino::Utf8PathBuf;
use indexmap::IndexSet;

use crate::Resolver;

use super::IngotDesc;

pub struct SrcFilesResolver<FR> {
    files_resolver: FR,
}

pub enum SrcFilesResolutionError {
    MissingDirectory,
    MissingRootFile,
}

impl<FR> Resolver for SrcFilesResolver<FR>
where
    FR: Resolver,
{
    type Config = ();
    type ResourceDesc = IngotDesc<FR::ResourceDesc>;
    type Resource = IndexSet<Utf8PathBuf>;
    type ResolutionError = SrcFilesResolutionError;

    fn from_config(config: &Self::Config) -> Self {
        todo!()
    }

    fn resolve(
        &self,
        desc: &IngotDesc<FR::ResourceDesc>,
    ) -> Result<IndexSet<Utf8PathBuf>, SrcFilesResolutionError> {
        todo!()
    }
}
