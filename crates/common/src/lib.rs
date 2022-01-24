pub mod db;
pub mod diagnostics;
pub mod files;
pub mod numeric;
pub mod panic;
mod span;
pub mod utils;

pub use files::{File, FileKind, SourceFileId};
pub use span::{Span, Spanned};

#[macro_export]
#[cfg(target_arch = "wasm32")]
macro_rules! assert_snapshot_wasm {
    ($path:expr, $actual:expr) => {
        let snap = include_str!($path);
        let expected = snap.splitn(3, "---\n").last().unwrap();
        pretty_assertions::assert_eq!($actual.trim(), expected.trim());
    };
}

#[macro_export]
#[cfg(not(target_arch = "wasm32"))]
macro_rules! assert_snapshot_wasm {
    ($path:expr, $actual:expr) => {};
}
