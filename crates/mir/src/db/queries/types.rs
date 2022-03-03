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

    pub fn projection_ty_imm(self, db: &dyn MirDb, index: usize) -> TypeId {
        debug_assert!(self.is_aggregate(db));

        match self.data(db).as_ref() {
            Type::Array(ArrayDef { elem_ty, .. }) => *elem_ty,
            Type::Tuple(def) => def.items[index],
            Type::Struct(def) | Type::Contract(def) => def.fields[index].1,
            Type::Event(def) => def.fields[index].1,
            _ => unreachable!(),
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

    pub fn is_primitive(self, db: &dyn MirDb) -> bool {
        matches!(
            self.data(db).as_ref(),
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::I128
                | Type::I256
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::U128
                | Type::U256
                | Type::Bool
                | Type::Address
                | Type::Unit
        )
    }

    /// Returns size of the type in bytes.
    pub fn size_of(self, db: &dyn MirDb, slot_size: usize) -> usize {
        match self.data(db).as_ref() {
            Type::Bool | Type::I8 | Type::U8 => 1,
            Type::I16 | Type::U16 => 2,
            Type::I32 | Type::U32 => 4,
            Type::I64 | Type::U64 => 8,
            Type::I128 | Type::U128 => 16,
            Type::I256 | Type::U256 | Type::Map(_) => 32,
            Type::Address => 20,
            Type::Unit => 0,

            Type::Array(def) => array_elem_size_imp(def, db, slot_size) * def.len,

            Type::Tuple(def) => {
                if def.items.is_empty() {
                    return 0;
                }
                let last_idx = def.items.len() - 1;
                self.aggregate_elem_offset(db, last_idx, slot_size)
                    + def.items[last_idx].size_of(db, slot_size)
            }

            Type::Struct(def) | Type::Contract(def) => {
                if def.fields.is_empty() {
                    return 0;
                }
                let last_idx = def.fields.len() - 1;
                self.aggregate_elem_offset(db, last_idx, slot_size)
                    + def.fields[last_idx].1.size_of(db, slot_size)
            }

            Type::Event(def) => {
                if def.fields.is_empty() {
                    return 0;
                }
                let last_idx = def.fields.len() - 1;
                self.aggregate_elem_offset(db, last_idx, slot_size)
                    + def.fields[last_idx].1.size_of(db, slot_size)
            }
        }
    }

    pub fn is_zero_sized(self, db: &dyn MirDb) -> bool {
        // It's ok to use 1 as a slot size because slot size doesn't affect whether a
        // type is zero sized or not.
        self.size_of(db, 1) == 0
    }

    pub fn align_of(self, db: &dyn MirDb, slot_size: usize) -> usize {
        if self.is_primitive(db) {
            1
        } else {
            // TODO: Too naive, we could implement more efficient layout for aggregate
            // types.
            slot_size
        }
    }

    /// Returns an offset of the element of aggregate type.
    pub fn aggregate_elem_offset(self, db: &dyn MirDb, elem_idx: usize, slot_size: usize) -> usize {
        debug_assert!(self.is_aggregate(db));

        if elem_idx == 0 {
            return 0;
        }

        if let Type::Array(def) = self.data(db).as_ref() {
            return array_elem_size_imp(def, db, slot_size) * elem_idx;
        }

        let mut offset = self.aggregate_elem_offset(db, elem_idx - 1, slot_size)
            + self
                .projection_ty_imm(db, elem_idx - 1)
                .size_of(db, slot_size);

        let elem_ty = self.projection_ty_imm(db, elem_idx);
        if (offset % slot_size + elem_ty.size_of(db, slot_size)) > slot_size {
            offset = round_up(offset, slot_size);
        }

        round_up(offset, elem_ty.align_of(db, slot_size))
    }

    pub fn is_aggregate(self, db: &dyn MirDb) -> bool {
        matches!(
            self.data(db).as_ref(),
            Type::Array(_) | Type::Tuple(_) | Type::Struct(_) | Type::Contract(_) | Type::Event(_)
        )
    }

    pub fn is_contract(self, db: &dyn MirDb) -> bool {
        matches!(self.data(db).as_ref(), Type::Contract(_))
    }

    pub fn array_elem_size(self, db: &dyn MirDb, slot_size: usize) -> usize {
        let data = self.data(db);
        if let Type::Array(def) = data.as_ref() {
            array_elem_size_imp(def, db, slot_size)
        } else {
            panic!("expected `Array` type; but got {:?}", data.as_ref())
        }
    }
}

fn array_elem_size_imp(arr: &ArrayDef, db: &dyn MirDb, slot_size: usize) -> usize {
    let elem_ty = arr.elem_ty;
    let elem = elem_ty.size_of(db, slot_size);
    round_up(elem, elem_ty.align_of(db, slot_size))
}

fn expect_projection_index(value: &Value) -> usize {
    match value {
        Value::Immediate(imm) => imm.value.to_usize().unwrap(),
        _ => panic!("given `value` is not an immediate"),
    }
}

fn round_up(value: usize, slot_size: usize) -> usize {
    ((value + slot_size - 1) / slot_size) * slot_size
}

#[cfg(test)]
mod tests {
    use fe_analyzer::namespace::items::ModuleId;
    use fe_common::Span;

    use super::*;
    use crate::{
        db::{MirDb, NewDb},
        ir::types::StructDef,
    };

    #[test]
    fn test_primitive_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::I8.into());
        let bool = db.mir_intern_type(Type::Bool.into());

        debug_assert_eq!(i8.size_of(&db, 1), 1);
        debug_assert_eq!(i8.size_of(&db, 32), 1);
        debug_assert_eq!(i8.align_of(&db, 1), 1);
        debug_assert_eq!(i8.align_of(&db, 32), 1);
        debug_assert_eq!(bool.size_of(&db, 1), 1);
        debug_assert_eq!(bool.size_of(&db, 32), 1);
        debug_assert_eq!(i8.align_of(&db, 32), 1);
        debug_assert_eq!(i8.align_of(&db, 32), 1);

        let u32 = db.mir_intern_type(Type::U32.into());
        debug_assert_eq!(u32.size_of(&db, 1), 4);
        debug_assert_eq!(u32.size_of(&db, 32), 4);
        debug_assert_eq!(u32.align_of(&db, 32), 1);

        let address = db.mir_intern_type(Type::Address.into());
        debug_assert_eq!(address.size_of(&db, 1), 20);
        debug_assert_eq!(address.size_of(&db, 32), 20);
        debug_assert_eq!(address.align_of(&db, 32), 1);
    }

    #[test]
    fn test_primitive_elem_array_type_info() {
        let db = NewDb::default();
        let i32 = db.mir_intern_type(Type::I32.into());

        let array_len = 10;
        let array_def = ArrayDef {
            elem_ty: i32,
            len: array_len,
        };
        let array = db.mir_intern_type(Type::Array(array_def).into());

        let elem_size = array.array_elem_size(&db, 1);
        debug_assert_eq!(elem_size, 4);
        debug_assert_eq!(array.array_elem_size(&db, 32), elem_size);

        debug_assert_eq!(array.size_of(&db, 1), elem_size * array_len);
        debug_assert_eq!(array.size_of(&db, 32), elem_size * array_len);
        debug_assert_eq!(array.align_of(&db, 1), 1);
        debug_assert_eq!(array.align_of(&db, 32), 32);

        debug_assert_eq!(array.aggregate_elem_offset(&db, 3, 32), elem_size * 3);
        debug_assert_eq!(array.aggregate_elem_offset(&db, 9, 1), elem_size * 9);
    }

    #[test]
    fn test_aggregate_elem_array_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::I8.into());
        let i64 = db.mir_intern_type(Type::I64.into());
        let i128 = db.mir_intern_type(Type::I128.into());

        let fields = vec![
            ("".into(), i64),
            ("".into(), i64),
            ("".into(), i8),
            ("".into(), i128),
            ("".into(), i8),
        ];

        let struct_def = StructDef {
            name: "".into(),
            fields,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate = db.mir_intern_type(Type::Struct(struct_def).into());

        let array_len = 10;
        let array_def = ArrayDef {
            elem_ty: aggregate,
            len: array_len,
        };
        let array = db.mir_intern_type(Type::Array(array_def).into());

        debug_assert_eq!(array.array_elem_size(&db, 1), 34);
        debug_assert_eq!(array.array_elem_size(&db, 32), 64);

        debug_assert_eq!(array.size_of(&db, 1), 34 * array_len);
        debug_assert_eq!(array.size_of(&db, 32), 64 * array_len);

        debug_assert_eq!(array.align_of(&db, 1), 1);
        debug_assert_eq!(array.align_of(&db, 32), 32);

        debug_assert_eq!(array.aggregate_elem_offset(&db, 3, 1), 102);
        debug_assert_eq!(array.aggregate_elem_offset(&db, 3, 32), 192);
    }

    #[test]
    fn test_primitive_elem_aggregate_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::I8.into());
        let i64 = db.mir_intern_type(Type::I64.into());
        let i128 = db.mir_intern_type(Type::I128.into());

        let fields = vec![
            ("".into(), i64),
            ("".into(), i64),
            ("".into(), i8),
            ("".into(), i128),
            ("".into(), i8),
        ];

        let struct_def = StructDef {
            name: "".into(),
            fields,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate = db.mir_intern_type(Type::Struct(struct_def).into());

        debug_assert_eq!(aggregate.size_of(&db, 1), 34);
        debug_assert_eq!(aggregate.size_of(&db, 32), 49);

        debug_assert_eq!(aggregate.align_of(&db, 1), 1);
        debug_assert_eq!(aggregate.align_of(&db, 32), 32);

        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 1), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 32), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 3, 1), 17);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 3, 32), 32);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 4, 1), 33);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 4, 32), 48);
    }

    #[test]
    fn test_aggregate_elem_aggregate_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::I8.into());
        let i64 = db.mir_intern_type(Type::I64.into());
        let i128 = db.mir_intern_type(Type::I128.into());

        let fields_inner = vec![
            ("".into(), i64),
            ("".into(), i64),
            ("".into(), i8),
            ("".into(), i128),
            ("".into(), i8),
        ];

        let struct_def_inner = StructDef {
            name: "".into(),
            fields: fields_inner,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate_inner = db.mir_intern_type(Type::Struct(struct_def_inner).into());

        let fields = vec![("".into(), i8), ("".into(), aggregate_inner)];
        let struct_def = StructDef {
            name: "".into(),
            fields,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate = db.mir_intern_type(Type::Struct(struct_def).into());

        debug_assert_eq!(aggregate.size_of(&db, 1), 35);
        debug_assert_eq!(aggregate.size_of(&db, 32), 81);

        debug_assert_eq!(aggregate.align_of(&db, 1), 1);
        debug_assert_eq!(aggregate.align_of(&db, 32), 32);

        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 1), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 32), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 1, 1), 1);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 1, 32), 32);
    }
}
