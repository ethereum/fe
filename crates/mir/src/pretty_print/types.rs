use std::fmt::{self, Write};

use crate::{
    db::MirDb,
    ir::{
        function::BodyDataStore,
        types::{ArrayDef, TupleDef, TypeKind},
        TypeId,
    },
};

use super::PrettyPrint;

impl PrettyPrint for TypeId {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        match &self.data(db).kind {
            TypeKind::I8 => write!(w, "i8"),
            TypeKind::I16 => write!(w, "i16"),
            TypeKind::I32 => write!(w, "i32"),
            TypeKind::I64 => write!(w, "i64"),
            TypeKind::I128 => write!(w, "i128"),
            TypeKind::I256 => write!(w, "i256"),
            TypeKind::U8 => write!(w, "u8"),
            TypeKind::U16 => write!(w, "u16"),
            TypeKind::U32 => write!(w, "u32"),
            TypeKind::U64 => write!(w, "u64"),
            TypeKind::U128 => write!(w, "u128"),
            TypeKind::U256 => write!(w, "u256"),
            TypeKind::Bool => write!(w, "bool"),
            TypeKind::Address => write!(w, "address"),
            TypeKind::Unit => write!(w, "()"),
            TypeKind::String(size) => write!(w, "Str<{}>", size),
            TypeKind::Array(ArrayDef { elem_ty, len }) => {
                write!(w, "[")?;
                elem_ty.pretty_print(db, store, w)?;
                write!(w, "; {}]", len)
            }
            TypeKind::Tuple(TupleDef { items }) => {
                write!(w, "(")?;
                if items.is_empty() {
                    return write!(w, ")");
                }

                let len = items.len();
                for item in &items[0..len - 1] {
                    item.pretty_print(db, store, w)?;
                    write!(w, ", ")?;
                }
                items.last().unwrap().pretty_print(db, store, w)?;
                write!(w, ")")
            }
            TypeKind::Struct(def) => {
                write!(w, "{}", def.name)
            }
            TypeKind::Event(def) => {
                write!(w, "{}", def.name)
            }
            TypeKind::Contract(def) => {
                write!(w, "{}", def.name)
            }
            TypeKind::Map(def) => {
                write!(w, "Map<")?;
                def.key_ty.pretty_print(db, store, w)?;
                write!(w, ",")?;
                def.value_ty.pretty_print(db, store, w)?;
                write!(w, ">")
            }
            TypeKind::MPtr(inner) => {
                write!(w, "*@s ")?;
                inner.pretty_print(db, store, w)
            }
            TypeKind::SPtr(inner) => {
                write!(w, "*@m ")?;
                inner.pretty_print(db, store, w)
            }
        }
    }
}
