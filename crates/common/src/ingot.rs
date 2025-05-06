use imbl::OrdMap;
use url::Url;

use crate::{workspace::IngotWorkspace, InputDb, InputFile};

#[salsa::tracked]
pub fn ingot_files(
    db: &dyn InputDb,
    workspace: IngotWorkspace,
    ingot_root: Url,
) -> OrdMap<Url, InputFile> {
    // let ingot_path = self.ingot_path(db, &ingot);
    // Match ingot_path and return early if None

    let upper_bound = ingot_root

    // Get files with paths between ingot_path and upper_bound
    let files = workspace
        .files(db)
        .split(&ingot_root)
        .1 // Get files with paths >= ingot_path
        .split(&upper_bound)
        .0; // Get files with paths < upper_bound

    files
}
