pub mod ingot;
pub mod path;
pub mod remote;
pub mod user_config;

pub trait Resolver {
    type Description;
    type Resource;
    type Error;
    type Diagnostic;

    fn resolve(&mut self, description: &Self::Description) -> Result<Self::Resource, Self::Error>;
    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic>;
}
