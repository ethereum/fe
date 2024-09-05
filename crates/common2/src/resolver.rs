pub mod fs;
pub mod git;
pub mod ingot;

pub trait Resolver {
    type Config;
    type ResourceDesc;
    type Resource;
    type ResolutionError;

    fn resolve(&self, desc: &Self::ResourceDesc) -> Result<Self::Resource, Self::ResolutionError>;
}
