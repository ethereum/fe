pub mod config;
pub mod core;
pub mod diagnostics;
pub mod file;
pub mod indexmap;
pub mod ingot;
pub mod urlext;

use file::FileIndex;

#[salsa::db]
// Each database must implement InputDb explicitly with its own storage mechanism
pub trait InputDb: salsa::Database {
    fn file_index(&self) -> FileIndex;
}

#[doc(hidden)]
pub use paste::paste;

#[macro_export]
macro_rules! impl_db_traits {
    ($db_type:ty, $($trait_name:ident),+ $(,)?) => {
        #[salsa::db]
        impl salsa::Database for $db_type {
            fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
        }
    };
}
