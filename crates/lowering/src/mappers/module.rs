use crate::context::ModuleContext;
use crate::mappers::{contracts, types};
use crate::names;
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::items::{ModuleId, TypeDefId};
use fe_analyzer::namespace::types::{Base, FixedSize, Tuple};
use fe_analyzer::AnalyzerDb;
use fe_parser::ast;
use fe_parser::node::Node;

/// Lowers a module.
pub fn module(db: &dyn AnalyzerDb, module: ModuleId) -> ast::Module {
    let mut context = ModuleContext::new(db);

    let mut lowered_body = module
        .data(db)
        .ast
        .body
        .iter()
        .filter_map(|stmt| match stmt {
            ast::ModuleStmt::Pragma(_) => Some(stmt.clone()),
            ast::ModuleStmt::Import(_) => Some(stmt.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    lowered_body.extend(module.all_type_defs(db).iter().map(|def| match def {
        TypeDefId::Alias(id) => {
            let node = &id.data(db).ast;
            let name = node.kind.name.clone();
            ast::ModuleStmt::TypeAlias(Node::new(
                ast::TypeAlias {
                    name,
                    typ: types::type_desc(
                        &mut context,
                        node.kind.typ.clone(),
                        &id.typ(db).expect("type alias error"),
                    ),
                },
                id.span(db),
            ))
        }
        TypeDefId::Struct(id) => ast::ModuleStmt::Struct(id.data(db).ast.clone()),
        TypeDefId::Contract(id) => {
            ast::ModuleStmt::Contract(contracts::contract_def(&mut context, *id))
        }
    }));

    let struct_defs_from_tuples = context
        .into_tuples()
        .iter()
        .map(|typ| ast::ModuleStmt::Struct(build_tuple_struct(typ).into_node()))
        .collect::<Vec<ast::ModuleStmt>>();

    ast::Module {
        body: [struct_defs_from_tuples, lowered_body].concat(),
    }
}

fn build_tuple_struct(tuple: &Tuple) -> ast::Struct {
    let fields = tuple
        .items
        .iter()
        .enumerate()
        .map(|(index, typ)| {
            build_struct_field(format!("item{}", index), build_type_desc(typ)).into_node()
        })
        .collect();

    ast::Struct {
        name: names::tuple_struct_name(tuple).into_node(),
        fields,
    }
}

fn build_struct_field(name: String, type_desc: ast::TypeDesc) -> ast::Field {
    ast::Field {
        is_pub: false,
        is_const: false,
        name: name.into_node(),
        typ: type_desc.into_node(),
        value: None,
    }
}

fn build_type_desc(typ: &FixedSize) -> ast::TypeDesc {
    match typ {
        FixedSize::Base(Base::Unit) => ast::TypeDesc::Unit,
        FixedSize::Base(base) => ast::TypeDesc::Base {
            base: names::base_type_name(base),
        },
        FixedSize::Array(array) => ast::TypeDesc::Array {
            dimension: array.size,
            typ: build_type_desc(&array.inner.into()).into_boxed_node(),
        },
        FixedSize::Tuple(tuple) => ast::TypeDesc::Base {
            base: names::tuple_struct_name(tuple),
        },
        FixedSize::String(string) => ast::TypeDesc::Generic {
            base: "String".to_string().into_node(),
            args: vec![ast::GenericArg::Int(string.max_size.into_node())].into_node(),
        },
        FixedSize::Contract(contract) => ast::TypeDesc::Base {
            base: contract.name.clone(),
        },
        FixedSize::Struct(strukt) => ast::TypeDesc::Base {
            base: strukt.name.clone(),
        },
    }
}
