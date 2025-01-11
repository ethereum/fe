pub mod diagnostics;
pub mod indexmap;
pub mod input;
pub use input::{InputFile, InputIngot};

#[salsa::db]
pub trait InputDb: salsa::Database {
    fn as_input_db(&self) -> &dyn InputDb;
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

        $(
            $crate::paste! {
                #[salsa::db]
                impl $trait_name for $db_type {
                    fn [<as_ $trait_name:snake>](&self) -> &dyn $trait_name {
                        self
                    }
                }
            }
        )+
    };
}
