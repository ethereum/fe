use crate::lowering::utils::ZeroSpanNode;
use fe_analyzer::namespace::types::{Array, Base, FixedSize, Integer, SafeNames, Tuple};
use fe_parser::ast as fe;

/// The name of a lowered list expression generator function.
pub fn list_expr_generator_fn_name(list_expr_type: &Array) -> String {
    format!("list_expr_{}", list_expr_type.lower_snake())
}

/// The name of a lowered tuple struct definition.
pub fn tuple_struct_name(tuple: &Tuple) -> String {
    tuple.lower_snake()
}

/// Maps a FixedSize type to its type description.
pub fn fixed_size_type_desc(typ: &FixedSize) -> fe::TypeDesc {
    match typ {
        FixedSize::Base(Base::Unit) => fe::TypeDesc::Unit,
        FixedSize::Base(base) => fe::TypeDesc::Base {
            base: base_type_name(base),
        },
        FixedSize::Array(array) => fe::TypeDesc::Array {
            dimension: array.size,
            typ: fixed_size_type_desc(&array.inner.into()).into_boxed_node(),
        },
        FixedSize::Tuple(_) => todo!(),
        FixedSize::String(_) => todo!(),
        FixedSize::Contract(_) => todo!(),
        FixedSize::Struct(_) => todo!(),
    }
}

pub fn base_type_name(typ: &Base) -> String {
    match typ {
        Base::Numeric(number) => match number {
            Integer::U256 => "u256",
            Integer::U128 => "u128",
            Integer::U64 => "u64",
            Integer::U32 => "u32",
            Integer::U16 => "u16",
            Integer::U8 => "u8",
            Integer::I256 => "i256",
            Integer::I128 => "i128",
            Integer::I64 => "i64",
            Integer::I32 => "i32",
            Integer::I16 => "i16",
            Integer::I8 => "i8",
        },
        Base::Bool => "bool",
        Base::Byte => unimplemented!("byte should be removed"),
        Base::Address => "address",
        Base::Unit => "unit",
    }
    .to_string()
}
