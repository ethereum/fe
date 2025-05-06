pub mod core;
// pub mod diagnostics;
pub mod indexmap;
// pub mod ingot;
// pub mod input;
// pub mod workspace;
// pub use input::InputFile;
pub mod urlext;

#[derive(Embed)]
#[folder = "../../library/core"]
pub(crate) struct Core;

#[salsa::db]
pub trait InputDb: salsa::Database {}

#[salsa::db]
impl<T> InputDb for T where T: salsa::Database {}

#[doc(hidden)]
pub use paste::paste;
use rust_embed::Embed;

#[macro_export]
macro_rules! impl_db_traits {
    ($db_type:ty, $($trait_name:ident),+ $(,)?) => {
        #[salsa::db]
        impl salsa::Database for $db_type {
            fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
        }
    };
}
