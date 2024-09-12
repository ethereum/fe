use smol_str::SmolStr;

use crate::Resolver;

use super::IngotDesc;

pub struct Config<FD> {
    pub name: SmolStr,
    pub version: SmolStr,
    dependencies: Vec<IngotDesc<FD>>,
}

pub struct ConfigResolutionError;

pub struct ConfigResolver<FR> {
    files_resolver: FR,
}

impl<FR> Resolver for ConfigResolver<FR>
where
    FR: Resolver,
{
    type Config = ();
    type ResourceDesc = IngotDesc<FR::ResourceDesc>;
    type Resource = Config<FR::ResourceDesc>;
    type ResolutionError = ConfigResolutionError;

    fn from_config(config: &Self::Config) -> Self {
        todo!()
    }

    fn resolve(
        &self,
        desc: &IngotDesc<FR::ResourceDesc>,
    ) -> Result<Config<FR::ResourceDesc>, ConfigResolutionError> {
        // load the config file data
        // parse the toml
        // check toml content (version, features, etc)
        //
    }
}
