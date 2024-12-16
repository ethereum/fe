use serde::Deserialize;
use smol_str::SmolStr;
use std::hash::Hash;

use crate::{path::PathDescription, remote::GitDescription};

pub mod config;
pub mod dependencies;
pub mod src_files;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IngotDescription<FD> {
    pub version: Option<SmolStr>,
    // pub fe_version: SmolStr,
    // pub features: Vec<SmolStr>,
    #[serde(flatten)]
    pub files_description: FD,
}

#[derive(Deserialize, PartialEq, Debug, Clone, Eq)]
pub enum AnyIngotDescription {
    Path(IngotDescription<PathDescription>),
    Remote(IngotDescription<GitDescription>),
    Registered(IngotDescription<()>),
}

// pub struct IngotPathResolver;

// pub enum IngotPathResolutionError {
//     RemotePathOutOfScope,
//     LocalPathDoesNotExist,
// }

// impl Resolver for IngotPathResolver {
//     type ResourceDesc = Vec<AnyIngotDesc>;
//     type Resource = Utf8PathBuf;
//     type ResolutionError = IngotPathResolutionError;

//     fn resolve(&self, desc: &Self::ResourceDesc) -> Result<Self::Resource, Self::ResolutionError> {
//         todo!()
//     }
// }
