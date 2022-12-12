use std::borrow::Cow;

pub(crate) mod cgu;
pub(crate) mod dependency_graph;
pub(crate) mod isel;
pub(crate) mod legalize;
pub(crate) mod runtime;

mod slot_size;

use yultsur::*;

/// A helper struct to abstract ident and expr.
struct YulVariable<'a>(Cow<'a, str>);

impl<'a> YulVariable<'a> {
    fn expr(&self) -> yul::Expression {
        identifier_expression! {(format!{"${}", self.0})}
    }

    fn ident(&self) -> yul::Identifier {
        identifier! {(format!{"${}", self.0})}
    }

    fn new(name: impl Into<Cow<'a, str>>) -> Self {
        Self(name.into())
    }
}
