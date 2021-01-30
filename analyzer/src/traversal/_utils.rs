use crate::namespace::types::FixedSize;
use crate::{
    ExpressionAttributes,
    Type,
};
use fe_parser::ast as fe;
use fe_parser::span::{
    Span,
    Spanned,
};

/// Creates a new spanned expression. Useful in cases where an `Expr` is nested
/// within the node of a `Spanned` object.
pub fn spanned_expression<'a>(span: &Span, exp: &fe::Expr<'a>) -> Spanned<fe::Expr<'a>> {
    Spanned {
        node: (*exp).clone(),
        span: (*span).to_owned(),
    }
}

pub fn expression_attributes_to_types(attributes: Vec<ExpressionAttributes>) -> Vec<Type> {
    attributes
        .iter()
        .map(|attributes| attributes.typ.clone())
        .collect()
}

pub fn fixed_sizes_to_types(sizes: Vec<FixedSize>) -> Vec<Type> {
    sizes.iter().map(|param| param.clone().into()).collect()
}
