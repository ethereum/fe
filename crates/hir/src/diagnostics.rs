//! This module defines the diagnostics that can be accumulated inside salsa-db
//! with span-agnostic forms. All diagnostics accumulated in salsa-db should
//! implement [`DiagnosticVoucher`] which defines the conversion into
//! [`CompleteDiagnostics`].

use common::diagnostics::{CompleteDiagnostic, GlobalErrorCode};

use crate::span::db::SpannedHirDb;

/// All diagnostics accumulated in salsa-db should implement
/// [`DiagnosticVoucher`] which defines the conversion.
///
/// All types that implements `DiagnosticVoucher` must NOT have a span
/// information which invalidates cache in salsa-db. Instead of it, the all
/// information is given by [`SpannedHirDB`] to allow evaluating span lazily.
///
/// The utility structs for conversion from HIR-spanless types to nodes are
/// defined in [`crate::span`] module.
pub trait DiagnosticVoucher {
    fn pass(&self) -> GlobalErrorCode;
    /// Consumes voucher and makes a [`CompleteDiagnostic`].
    fn consume(self, db: &dyn SpannedHirDb) -> CompleteDiagnostic;
}

impl DiagnosticVoucher for CompleteDiagnostic {
    fn pass(&self) -> GlobalErrorCode {
        self.error_code.clone()
    }

    fn consume(self, _db: &dyn SpannedHirDb) -> CompleteDiagnostic {
        self
    }
}
