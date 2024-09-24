pub mod files;
pub mod ingot;

pub trait Resolver {
    type Config;
    type ResourceDesc;
    type Resource;
    type ResolutionError;

    fn from_config(config: &Self::Config) -> Self;
    fn resolve(&self, desc: &Self::ResourceDesc) -> Result<Self::Resource, Self::ResolutionError>;
}
