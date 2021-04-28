use crate::errors::SemanticError;
use crate::namespace::types::FixedSize;
use crate::{ExpressionAttributes, Type};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::convert::TryInto;

pub fn call_arg_value(arg: &fe::CallArg) -> &Node<fe::Expr> {
    match arg {
        fe::CallArg::Arg(value) => value,
        fe::CallArg::Kwarg(fe::Kwarg { value, .. }) => value,
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

pub fn types_to_fixed_sizes(sizes: Vec<Type>) -> Result<Vec<FixedSize>, SemanticError> {
    sizes.iter().map(|param| param.clone().try_into()).collect()
}
