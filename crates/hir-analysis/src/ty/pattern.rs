use crate::ty::ty_check::{RecordLike, TupleLike};
use crate::HirAnalysisDb;
use hir::hir_def::IdentId;

/// Represents a pattern in Fe
#[derive(Clone, Debug)]
pub enum Pattern<'db> {
    /// Wildcard pattern (_)
    Wildcard,

    /// Variable binding
    Binding(IdentId<'db>),

    /// Boolean literal pattern
    BoolLiteral(bool),

    /// Integer literal pattern
    IntLiteral(i128),

    /// Record pattern with named fields
    Record {
        record: RecordLike<'db>,
        fields: Vec<(IdentId<'db>, Box<Pattern<'db>>)>,
        rest: bool, // Whether ".." is present
    },

    /// Tuple pattern
    Tuple {
        tuple: Option<TupleLike<'db>>, // None for regular tuples
        elements: Vec<Pattern<'db>>,
    },

    /// Or pattern (p1 | p2 | ...)
    Or(Vec<Pattern<'db>>),

    /// Rest pattern (..)
    Rest,

    /// Invalid pattern (for error recovery)
    Invalid,
}

impl<'db> Pattern<'db> {
    /// Create a new wildcard pattern
    pub fn wildcard() -> Self {
        Pattern::Wildcard
    }

    /// Create a new binding pattern
    pub fn binding(name: IdentId<'db>) -> Self {
        Pattern::Binding(name)
    }

    /// Create a new boolean literal pattern
    pub fn bool_literal(value: bool) -> Self {
        Pattern::BoolLiteral(value)
    }

    /// Create a new integer literal pattern
    pub fn int_literal(value: i128) -> Self {
        Pattern::IntLiteral(value)
    }

    /// Create a new record pattern
    pub fn record(
        record: RecordLike<'db>,
        fields: Vec<(IdentId<'db>, Box<Pattern<'db>>)>,
        rest: bool,
    ) -> Self {
        Pattern::Record {
            record,
            fields,
            rest,
        }
    }

    /// Create a new tuple pattern
    pub fn tuple(tuple: Option<TupleLike<'db>>, elements: Vec<Pattern<'db>>) -> Self {
        Pattern::Tuple { tuple, elements }
    }

    /// Create a new or pattern
    pub fn or(patterns: Vec<Pattern<'db>>) -> Self {
        Pattern::Or(patterns)
    }

    /// Create a new rest pattern
    pub fn rest() -> Self {
        Pattern::Rest
    }

    /// Create a new invalid pattern
    pub fn invalid() -> Self {
        Pattern::Invalid
    }

    /// Check if this pattern is a wildcard
    pub fn is_wildcard(&self) -> bool {
        matches!(self, Pattern::Wildcard)
    }

    /// Check if this pattern is irrefutable (always matches)
    pub fn is_irrefutable(&self, _db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            Pattern::Wildcard | Pattern::Binding(_) | Pattern::Rest => true,
            Pattern::Or(patterns) => patterns.iter().any(|p| p.is_irrefutable(_db)),
            // Record and tuple patterns with rest are irrefutable
            Pattern::Record { rest, .. } => *rest,
            // Other patterns are refutable
            _ => false,
        }
    }
}
