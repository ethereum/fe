//! This module defines the diagnostics that can be accumulated inside salsa-db
//! with span-agnostic forms. All diagnostics accumulated in salsa-db should
//! implement [`DiagnosticVoucher`] which defines the conversion into
//! [`CompleteDiagnostics`].

use common::diagnostics::{CompleteDiagnostic, GlobalErrorCode};

use crate::span::db::SpannedHirDb;

/// All diagnostics accumulated in salsa-db should implement
/// [`DiagnosticVoucher`] which defines the conversion into
/// [`CompleteDiagnostic`].
///
/// All types that implement `DiagnosticVoucher` must NOT have a span
/// information which invalidates cache in salsa-db. Instead of it, the all
/// information is given by [`SpannedHirDB`] to allow evaluating span lazily.
///
/// The reason why we use `DiagnosticVoucher` is that we want to evaluate span
/// lazily to avoid invalidating cache in salsa-db.
///
/// To obtain a span from HIR nodes in a lazy manner, it's recommended to use
/// `[LazySpan]`(crate::span::LazySpan) and types that implement `LazySpan`.
pub trait DiagnosticVoucher: Send {
    fn error_code(&self) -> GlobalErrorCode;
    /// Consumes voucher and makes a [`CompleteDiagnostic`].
    fn to_complete(self, db: &dyn SpannedHirDb) -> CompleteDiagnostic;
}
