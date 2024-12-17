pub mod ingot;
pub mod path;
pub mod remote;

pub trait Resolver {
    type Description;
    type Resource;
    type ResolutionError;

    fn resolve(
        &self,
        description: &Self::Description,
    ) -> Result<Self::Resource, Self::ResolutionError>;
}
