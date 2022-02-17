use std::io::{self, Write};

use crate::{
    db::MirDb,
    ir::{
        function::BodyDataStore,
        types::{ArrayDef, TupleDef},
        Type, TypeId,
    },
};

use super::PrettyPrint;

impl PrettyPrint for TypeId {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> io::Result<()> {
        match self.data(db).as_ref() {
            Type::I8 => write!(w, "i8"),
            Type::I16 => write!(w, "i16"),
            Type::I32 => write!(w, "i32"),
            Type::I64 => write!(w, "i64"),
            Type::I128 => write!(w, "i128"),
            Type::I256 => write!(w, "i256"),
            Type::U8 => write!(w, "u8"),
            Type::U16 => write!(w, "u16"),
            Type::U32 => write!(w, "u32"),
            Type::U64 => write!(w, "u64"),
            Type::U128 => write!(w, "u128"),
            Type::U256 => write!(w, "u256"),
            Type::Bool => write!(w, "bool"),
            Type::Address => write!(w, "address"),
            Type::Unit => write!(w, "()"),
            Type::Array(ArrayDef { elem_ty, len }) => {
                write!(w, "[")?;
                elem_ty.pretty_print(db, store, w)?;
                write!(w, "; {}]", len)
            }
            Type::Tuple(TupleDef { items }) => {
                write!(w, "(")?;
                if items.is_empty() {
                    return write!(w, ")");
                }

                let len = items.len();
                for item in &items[0..len - 1] {
                    item.pretty_print(db, store, w)?;
                    write!(w, ", ")?;
                }
                items.last().unwrap().pretty_print(db, store, w)
            }
            Type::Struct(def) => {
                write!(w, "{}", def.name)
            }
            Type::Event(def) => {
                write!(w, "{}", def.name)
            }
            Type::Contract(def) => {
                write!(w, "{}", def.name)
            }
            Type::Map(def) => {
                write!(w, "Map<")?;
                def.key_ty.pretty_print(db, store, w)?;
                write!(w, ",")?;
                def.value_ty.pretty_print(db, store, w)?;
                write!(w, ">")
            }
        }
    }
}
