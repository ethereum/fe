pub mod files;
pub mod graph;
pub mod ingot;

pub trait Resolver: Sized {
    type Description;
    type Resource;
    type Error;
    type Diagnostic;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        self.transient_resolve(description)
            .map(|resource| handler.handle_resolution(description, resource))
    }

    fn transient_resolve(
        &mut self,
        description: &Self::Description,
    ) -> Result<Self::Resource, Self::Error>;

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic>;
}

pub trait ResolutionHandler<R>
where
    R: Resolver,
{
    type Item;

    fn handle_resolution(
        &mut self,
        description: &R::Description,
        resource: R::Resource,
    ) -> Self::Item;
}
