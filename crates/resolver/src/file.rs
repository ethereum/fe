use camino::Utf8PathBuf;

use crate::{ResolutionHandler, Resolver};

#[derive(Default)]
pub struct FileResolver;

impl Resolver for FileResolver {
    type Description = Utf8PathBuf;
    type Resource = String;
    type Error = ();
    type Diagnostic = ();

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        todo!()
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}
