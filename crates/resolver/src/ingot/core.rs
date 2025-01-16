// #[derive(Debug, Deserialize)]
// pub enum CoreIngotDescription {
//     Local(PathDescription),
//     Remote(GitDescription),
// }
//
// pub enum CoreIngotResolutionError {
//     CoreIngotPathDoesNotExist,
//     RemoteResolutionError(GitResolutionError),
// }
//
// pub struct CoreIngotResolver {
//     // ...
// }
//
// impl Resolver for CoreIngotResolver {
//     type Description = CoreIngotDescription;
//     type Resource = Utf8PathBuf;
//     type Error = CoreIngotResolutionError;
//
//     fn resolve(&self, description: &CoreIngotDescription) -> Result<Ingot, IngotResolutionError> {
//         // ...
//     }
// }
