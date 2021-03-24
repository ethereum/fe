use fe_parser::{
    ast::StructStmt,
    node::Node,
};

use crate::errors::SemanticError;
use crate::namespace::scopes::{
    ModuleScope,
    Shared,
};
use crate::namespace::types::{
    type_desc,
    FixedSize,
    Struct,
    Type,
};

pub fn struct_def(
    module_scope: Shared<ModuleScope>,
    name: &str,
    struct_stmts: &[Node<StructStmt>],
) -> Result<(), SemanticError> {
    let mut val = Struct::new(name);
    for stmt in struct_stmts {
        let StructStmt::StructField { name, typ, .. } = &stmt.kind;
        let field_type = type_desc(&module_scope.borrow().type_defs, &typ.kind)?;
        if let Type::Base(base_typ) = field_type {
            val.add_field(&name.kind, &FixedSize::Base(base_typ))?;
        } else {
            todo!("Non-Base type fields aren't yet supported")
        }
    }
    module_scope
        .borrow_mut()
        .add_type_def(name, Type::Struct(val))?;
    Ok(())
}
