pub mod fs;
pub mod git;
pub mod ingot;

pub trait Resolver {
    type ResourceDesc;
    type Resource;
    type ResolutionError;

    fn resolve(&self, id: &Self::ResourceDesc) -> Result<Self::Resource, Self::ResolutionError>;
}
