use std::{rc::Rc, str::FromStr};

use fe_analyzer::namespace::{items as analyzer_items, types as analyzer_types};

use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::{
    db::MirDb,
    ir::{types::ArrayDef, value::Immediate, Type, TypeId, Value},
    lower::types::{lower_event_type, lower_type},
};

pub fn mir_lowered_type(db: &dyn MirDb, analyzer_type: analyzer_types::Type) -> TypeId {
    lower_type(db, &analyzer_type)
}

pub fn mir_lowered_event_type(db: &dyn MirDb, event: analyzer_items::EventId) -> TypeId {
    lower_event_type(db, event)
}

impl TypeId {
    pub fn data(self, db: &dyn MirDb) -> Rc<Type> {
        db.lookup_mir_intern_type(self)
    }

    pub fn projection_ty(self, db: &dyn MirDb, access: &Value) -> TypeId {
        match self.data(db).as_ref() {
            Type::Array(ArrayDef { elem_ty, .. }) => *elem_ty,
            Type::Tuple(def) => {
                let index = expect_projection_index(access);
                def.items[index]
            }
            Type::Struct(def) | Type::Contract(def) => {
                let index = expect_projection_index(access);
                def.fields[index].1
            }
            Type::Event(def) => {
                let index = expect_projection_index(access);
                def.fields[index].1
            }
            other => unreachable!("{:?} is not an aggregate type", other),
        }
    }

    pub fn index_from_fname(self, db: &dyn MirDb, fname: &str, index_ty: TypeId) -> Immediate {
        match self.data(db).as_ref() {
            Type::Tuple(_) => {
                // TODO: Fix this when the syntax for tuple access changes.
                let index_str = &fname[4..];
                Immediate {
                    value: BigInt::from_str(index_str).unwrap(),
                    ty: index_ty,
                }
            }

            Type::Struct(def) | Type::Contract(def) => {
                let index = def
                    .fields
                    .iter()
                    .enumerate()
                    .find_map(|(i, field)| (field.0 == fname).then(|| i.into()))
                    .unwrap();
                Immediate {
                    value: index,
                    ty: index_ty,
                }
            }

            Type::Event(def) => {
                let index = def
                    .fields
                    .iter()
                    .enumerate()
                    .find_map(|(i, field)| (field.0 == fname).then(|| i.into()))
                    .unwrap();
                Immediate {
                    value: index,
                    ty: index_ty,
                }
            }

            other => unreachable!("{:?} does not have fields", other),
        }
    }

    pub fn is_aggregate(self, db: &dyn MirDb) -> bool {
        matches!(
            self.data(db).as_ref(),
            Type::Array(_) | Type::Tuple(_) | Type::Struct(_) | Type::Contract(_) | Type::Event(_)
        )
    }
}

fn expect_projection_index(value: &Value) -> usize {
    match value {
        Value::Immediate(imm) => imm.value.to_usize().unwrap(),
        _ => panic!("given `value` is not an immediate"),
    }
}
