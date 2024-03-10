//! This module defines the diagnostics that can be accumulated inside salsa-db
//! with span-agnostic forms. All diagnostics accumulated in salsa-db should
//! implement [`DiagnosticVoucher`] which defines the conversion into
//! [`CompleteDiagnostic`].

use common::diagnostics::{CompleteDiagnostic, GlobalErrorCode};

use crate::SpannedHirDb;

/// All diagnostics accumulated in salsa-db should implement
/// [`DiagnosticVoucher`] which defines the conversion into
/// [`CompleteDiagnostic`].
///
/// All types that implement `DiagnosticVoucher` must NOT have a span
/// information which invalidates cache in salsa-db. Instead of it, the all
/// information is given by [`SpannedHirDb`] to allow evaluating span lazily.
///
/// The reason why we use `DiagnosticVoucher` is that we want to evaluate span
/// lazily to avoid invalidating cache in salsa-db.
///
/// To obtain a span from HIR nodes in a lazy manner, it's recommended to use
/// `[LazySpan]`(crate::span::LazySpan) and types that implement `LazySpan`.
pub trait DiagnosticVoucher: Send {
    fn error_code(&self) -> GlobalErrorCode;
    /// Makes a [`CompleteDiagnostic`].
    fn to_complete(&self, db: &dyn SpannedHirDb) -> CompleteDiagnostic;
    fn clone_box(&self) -> Box<dyn DiagnosticVoucher>;
}

impl DiagnosticVoucher for CompleteDiagnostic {
    fn error_code(&self) -> GlobalErrorCode {
        self.error_code.clone()
    }

    fn to_complete(&self, _db: &dyn SpannedHirDb) -> CompleteDiagnostic {
        self.clone()
    }

    fn clone_box(&self) -> Box<dyn DiagnosticVoucher> {
        Box::new(self.clone())
    }
}

impl DiagnosticVoucher for Box<dyn DiagnosticVoucher> {
    fn error_code(&self) -> GlobalErrorCode {
        self.as_ref().error_code()
    }

    fn to_complete(&self, db: &dyn SpannedHirDb) -> CompleteDiagnostic {
        self.as_ref().to_complete(db)
    }

    fn clone_box(&self) -> Box<dyn DiagnosticVoucher> {
        self.as_ref().clone_box()
    }
}

