pub mod core;
pub mod diagnostics;
pub mod indexmap;
pub mod ingot;
pub mod input;
pub use input::{InputFile, InputIngot};

#[derive(rust_embed::Embed)]
#[folder = "../../library/core"]
pub(crate) struct Core;

#[salsa::db]
pub trait InputDb: salsa::Database {}

#[salsa::db]
impl<T> InputDb for T where T: salsa::Database {}
