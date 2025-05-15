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

/// Macro for implementing the InputDb trait for a Salsa database struct
///
/// This assumes the database has a field named `index` of type `Option<FileIndex>`.
///
/// # Example
///
/// ```
/// #[derive(Clone)]
/// #[salsa::db]
/// pub struct MyDatabase {
///     storage: salsa::Storage<Self>,
///     index: Option<FileIndex>,
/// }
///
/// impl_input_db!(MyDatabase);
/// ```
#[macro_export]
macro_rules! impl_input_db {
    ($db_type:ty) => {
        #[salsa::db]
        impl $crate::InputDb for $db_type {
            fn file_index(&self) -> $crate::file::FileIndex {
                self.index.clone().expect("FileIndex not initialized")
            }
        }
    };
}

/// Macro for implementing Default for a Salsa database with FileIndex
///
/// This assumes the database has a field named `index` of type `Option<FileIndex>`,
/// and will initialize it properly.
///
/// # Example
///
/// ```
/// #[derive(Clone)]
/// #[salsa::db]
/// pub struct MyDatabase {
///     storage: salsa::Storage<Self>,
///     index: Option<FileIndex>,
/// }
///
/// impl_db_default!(MyDatabase);
/// ```
#[macro_export]
macro_rules! impl_db_default {
    ($db_type:ty) => {
        impl Default for $db_type
        where
            $db_type: $crate::core::HasBuiltinCore,
        {
            fn default() -> Self {
                let mut db = Self {
                    storage: salsa::Storage::default(),
                    index: None,
                };
                let index = $crate::file::FileIndex::default(&db);
                db.index = Some(index);
                $crate::core::HasBuiltinCore::initialize_builtin_core(&mut db);
                db
            }
        }
    };
}

/// Macro for creating a standard Salsa database with FileIndex support
///
/// This creates a struct with:
/// - A `storage` field for Salsa
/// - An `index` field for the FileIndex
/// - Default implementation that initializes the FileIndex
/// - Implementation of InputDb via impl_input_db!
///
/// # Example
///
/// ```
/// define_input_db!(MyDatabase);
/// ```
#[macro_export]
macro_rules! define_input_db {
    ($db_name:ident) => {
        #[derive(Clone)]
        #[salsa::db]
        pub struct $db_name {
            storage: salsa::Storage<Self>,
            index: Option<$crate::file::FileIndex>,
        }

        #[salsa::db]
        impl salsa::Database for $db_name {
            fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
        }

        $crate::impl_input_db!($db_name);
        $crate::impl_db_default!($db_name);
    };
}

#[macro_export]
macro_rules! impl_db_traits {
    ($db_type:ty, $($trait_name:ident),+ $(,)?) => {
        #[salsa::db]
        impl salsa::Database for $db_type {
            fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
        }
    };
}
