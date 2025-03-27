use camino::Utf8PathBuf;

use crate::Resolver;

struct IngotResolver;

enum Ingot {
    Standalone { path: () },
    Normal { config: (), src: () },
}

impl Resolver for IngotResolver {
    type Description = Utf8PathBuf;
    type Resource = Ingot;
    type Diagnostic = ();
    type Error = ();

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: crate::ResolutionHandler<Self>,
    {
        todo!()
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}
