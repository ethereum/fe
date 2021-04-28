use fe_analyzer::namespace::types::{Base, FixedSize, Integer, SafeNames, Tuple};
use fe_parser::ast as fe;

/// The name of a lowered tuple struct definition.
pub fn tuple_struct_string(tuple: &Tuple) -> String {
    tuple.lower_snake()
}

/// The type description of a lowered tuple struct.
pub fn tuple_struct_type_desc(tuple: &Tuple) -> fe::TypeDesc {
    fe::TypeDesc::Base {
        base: tuple_struct_string(tuple),
    }
}

/// The name of a lowered tuple struct definition as an expression.
pub fn tuple_struct_name(tuple: &Tuple) -> fe::Expr {
    fe::Expr::Name(tuple_struct_string(tuple))
}

/// Maps a FixedSize type to its type description.
pub fn fixed_size_type_desc(typ: &FixedSize) -> fe::TypeDesc {
    match typ {
        FixedSize::Base(base) => fe::TypeDesc::Base {
            base: base_type_name(base),
        },
        FixedSize::Array(_) => todo!(),
        FixedSize::Tuple(_) => todo!(),
        FixedSize::String(_) => todo!(),
        FixedSize::Contract(_) => todo!(),
        FixedSize::Struct(_) => todo!(),
    }
}

fn base_type_name(typ: &Base) -> String {
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
    }
    .to_string()
}
