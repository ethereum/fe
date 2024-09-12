use serde::Deserialize;
use smol_str::SmolStr;

use crate::files::AnyFilesDesc;

mod config;
mod src_files;

#[derive(Deserialize)]
pub struct IngotDesc<FD> {
    name: SmolStr,
    version: Option<String>,
    #[serde(flatten)]
    files_desc: FD,
}

pub type AnyIngotDesc = IngotDesc<AnyFilesDesc>;
