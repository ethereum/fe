use serde::Deserialize;
use smol_str::SmolStr;
use std::hash::Hash;

use crate::files::AnyFilesDesc;

mod config;
mod dep_graph;
mod src_files;

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IngotDesc<FD>
where
    FD: Hash,
{
    pub version: Option<SmolStr>,
    #[serde(flatten)]
    pub files_desc: FD,
}

pub type AnyIngotDesc = IngotDesc<AnyFilesDesc>;
