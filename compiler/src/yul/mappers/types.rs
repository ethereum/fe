use crate::errors::CompileError;
use crate::yul::namespace::scopes::Scope;
use crate::yul::namespace::types;
use crate::yul::namespace::types::{Array, Base, FixedSize, Type};

use vyper_parser::ast as vyp;

pub fn type_desc<'a>(scope: Scope, typ: &'a vyp::TypeDesc<'a>) -> Result<Type, CompileError> {
    types::type_desc(&scope.module_scope().borrow().defs, typ)
}

pub fn type_desc_base<'a>(scope: Scope, typ: &'a vyp::TypeDesc<'a>) -> Result<Base, CompileError> {
    types::type_desc_base(&scope.module_scope().borrow().defs, typ)
}

pub fn type_desc_array<'a>(
    scope: Scope,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<Array, CompileError> {
    types::type_desc_array(&scope.module_scope().borrow().defs, typ)
}

pub fn type_desc_fixed_size<'a>(
    scope: Scope,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<FixedSize, CompileError> {
    types::type_desc_fixed_size(&scope.module_scope().borrow().defs, typ)
}
