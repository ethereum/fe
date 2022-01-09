use crate::names;
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::types::{Array, Base, FixedSize, SafeNames, Tuple};
use fe_parser::ast::{self, SmolStr};

/// The name of a lowered list expression generator function.
pub fn list_expr_generator_fn_name(list_expr_type: &Array) -> SmolStr {
    format!("list_expr_{}", list_expr_type.lower_snake()).into()
}

/// The name of a lowered tuple struct definition.
pub fn tuple_struct_name(tuple: &Tuple) -> SmolStr {
    format!("${}", tuple.lower_snake()).into()
}

/// Maps a FixedSize type to its type description.
pub fn fixed_size_type_desc(typ: &FixedSize) -> ast::TypeDesc {
    match typ {
        FixedSize::Base(Base::Unit) => ast::TypeDesc::Unit,
        FixedSize::Base(base) => ast::TypeDesc::Base { base: base.name() },
        FixedSize::Array(array) => ast::TypeDesc::Generic {
            base: SmolStr::new("Array").into_node(),
            args: vec![
                ast::GenericArg::TypeDesc(
                    fixed_size_type_desc(&FixedSize::Base(array.inner)).into_node(),
                ),
                ast::GenericArg::Int(array.size.into_node()),
            ]
            .into_node(),
        },
        FixedSize::Tuple(tuple) => ast::TypeDesc::Base {
            base: names::tuple_struct_name(tuple),
        },
        FixedSize::String(string) => ast::TypeDesc::Generic {
            base: SmolStr::new("String").into_node(),
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
