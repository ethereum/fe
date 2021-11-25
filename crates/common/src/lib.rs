pub mod diagnostics;
pub mod files;
pub mod numeric;
mod span;
pub mod utils;
pub use span::{Span, Spanned};
pub mod panic;
mod upcast;
pub use upcast::Upcast;

#[macro_export]
#[cfg(target_arch = "wasm32")]
macro_rules! assert_snapshot_wasm {
    ($path:expr, $actual:expr) => {
        let snap = include_str!($path);
        let (_, expected) = snap.rsplit_once("---\n").unwrap();
        pretty_assertions::assert_eq!($actual.trim(), expected.trim());
    };
}

#[macro_export]
#[cfg(not(target_arch = "wasm32"))]
macro_rules! assert_snapshot_wasm {
    ($path:expr, $actual:expr) => {};
}
