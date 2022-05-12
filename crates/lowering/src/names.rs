use crate::names;
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::types::{Array, Base, SafeNames, Tuple, Type};
use fe_parser::ast::{self, SmolStr};

/// The name of a lowered list expression generator function.
pub fn list_expr_generator_fn_name(list_expr_type: &Array) -> SmolStr {
    format!("list_expr_{}", list_expr_type.lower_snake()).into()
}

/// The name of a lowered tuple struct definition.
pub fn tuple_struct_name(tuple: &Tuple) -> SmolStr {
    format!("${}", tuple.lower_snake()).into()
}

/// Maps a Type type to its type description.
pub fn build_type_desc(typ: &Type) -> ast::TypeDesc {
    match typ {
        Type::Base(Base::Unit) => ast::TypeDesc::Unit,
        Type::Base(base) => ast::TypeDesc::Base { base: base.name() },
        Type::Array(array) => ast::TypeDesc::Generic {
            base: SmolStr::new("Array").into_node(),
            args: vec![
                ast::GenericArg::TypeDesc(build_type_desc(&Type::Base(array.inner)).into_node()),
                ast::GenericArg::Int(array.size.into_node()),
            ]
            .into_node(),
        },
        Type::Tuple(tuple) => ast::TypeDesc::Base {
            base: names::tuple_struct_name(tuple),
        },
        Type::String(string) => ast::TypeDesc::Generic {
            base: SmolStr::new("String").into_node(),
            args: vec![ast::GenericArg::Int(string.max_size.into_node())].into_node(),
        },
        Type::Contract(contract) => ast::TypeDesc::Base {
            base: contract.name.clone(),
        },
        Type::Struct(strukt) => ast::TypeDesc::Base {
            base: strukt.name.clone(),
        },
        Type::Map(map) => ast::TypeDesc::Generic {
            base: SmolStr::new("Map").into_node(),
            args: vec![ast::GenericArg::TypeDesc(
                build_type_desc(&map.value).into_node(),
            )]
            .into_node(),
        },
        Type::SelfContract(contract) => ast::TypeDesc::Base {
            base: contract.name.clone(),
        },
    }
}
