use fe_parser::{ast::Field, node::Node};

use crate::errors::SemanticError;
use crate::namespace::scopes::{ModuleScope, Scope, Shared};
use crate::namespace::types::{FixedSize, Struct, Type};
use crate::traversal::types::type_desc;
use crate::Context;
use std::rc::Rc;

pub fn struct_def(
    context: &mut Context,
    module_scope: Shared<ModuleScope>,
    name: &str,
    fields: &[Node<Field>],
) -> Result<(), SemanticError> {
    let mut val = Struct::new(name);
    for field in fields {
        let Field { name, typ, .. } = &field.kind;
        let field_type = type_desc(
            &Scope::Module(Rc::clone(&module_scope)),
            context,
            typ,
        )?;
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
