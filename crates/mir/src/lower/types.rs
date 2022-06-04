use crate::{
    db::MirDb,
    ir::{
        types::{ArrayDef, EventDef, MapDef, StructDef, TupleDef},
        Type, TypeId, TypeKind,
    },
};

use fe_analyzer::namespace::{items as analyzer_items, types as analyzer_types};

pub fn lower_type(db: &dyn MirDb, analyzer_ty: &analyzer_types::Type) -> TypeId {
    let ty_kind = match analyzer_ty {
        analyzer_types::Type::Base(base) => lower_base(base),
        analyzer_types::Type::Array(arr) => lower_array(db, arr),
        analyzer_types::Type::Map(map) => lower_map(db, map),
        analyzer_types::Type::Tuple(tup) => lower_tuple(db, tup),
        analyzer_types::Type::String(string) => TypeKind::String(string.max_size),
        analyzer_types::Type::Contract(_) => TypeKind::Address,
        analyzer_types::Type::SelfContract(contract) => lower_contract(db, contract),
        analyzer_types::Type::Struct(struct_) => lower_struct(db, struct_),
    };

    intern_type(db, ty_kind, Some(analyzer_ty))
}

pub fn lower_event_type(db: &dyn MirDb, event: analyzer_items::EventId) -> TypeId {
    let name = event.name(db.upcast());

    let analyzer_ty = event.typ(db.upcast());
    // Lower event fields.
    let fields = analyzer_ty
        .fields
        .iter()
        .map(|field| {
            let ty = db.mir_lowered_type(field.typ.clone().unwrap());
            (field.name.clone(), ty, field.is_indexed)
        })
        .collect();

    // Obtain span.
    let span = event.span(db.upcast());

    let module_id = event.module(db.upcast());

    let def = EventDef {
        name,
        fields,
        span,
        module_id,
    };
    let ty = TypeKind::Event(def);
    intern_type(db, ty, None)
}

fn lower_base(base: &analyzer_types::Base) -> TypeKind {
    use analyzer_types::{Base, Integer};

    match base {
        Base::Numeric(int_ty) => match int_ty {
            Integer::I8 => TypeKind::I8,
            Integer::I16 => TypeKind::I16,
            Integer::I32 => TypeKind::I32,
            Integer::I64 => TypeKind::I64,
            Integer::I128 => TypeKind::I128,
            Integer::I256 => TypeKind::I256,
            Integer::U8 => TypeKind::U8,
            Integer::U16 => TypeKind::U16,
            Integer::U32 => TypeKind::U32,
            Integer::U64 => TypeKind::U64,
            Integer::U128 => TypeKind::U128,
            Integer::U256 => TypeKind::U256,
        },

        Base::Bool => TypeKind::Bool,
        Base::Address => TypeKind::Address,
        Base::Unit => TypeKind::Unit,
    }
}

fn lower_array(db: &dyn MirDb, arr: &analyzer_types::Array) -> TypeKind {
    let len = arr.size;
    let elem_ty = db.mir_lowered_type(arr.inner.as_ref().clone());

    let def = ArrayDef { elem_ty, len };
    TypeKind::Array(def)
}

fn lower_map(db: &dyn MirDb, map: &analyzer_types::Map) -> TypeKind {
    let key_ty = db.mir_lowered_type(map.key.into());
    let value_ty = db.mir_lowered_type(*map.value.clone());

    let def = MapDef { key_ty, value_ty };
    TypeKind::Map(def)
}

fn lower_tuple(db: &dyn MirDb, tup: &analyzer_types::Tuple) -> TypeKind {
    let items = tup
        .items
        .iter()
        .map(|item| db.mir_lowered_type(item.clone()))
        .collect();

    let def = TupleDef { items };
    TypeKind::Tuple(def)
}

fn lower_contract(db: &dyn MirDb, contract: &analyzer_types::Contract) -> TypeKind {
    let id = contract.id;

    let name = id.name(db.upcast());

    // Lower contract fields.
    let fields = id
        .fields(db.upcast())
        .iter()
        .map(|(fname, fid)| {
            let analyzer_types = fid.typ(db.upcast()).unwrap();
            let ty = db.mir_lowered_type(analyzer_types);
            (fname.clone(), ty)
        })
        .collect();

    // Obtain span.
    let span = id.span(db.upcast());

    let module_id = id.module(db.upcast());

    let def = StructDef {
        name,
        fields,
        span,
        module_id,
    };
    TypeKind::Contract(def)
}

fn lower_struct(db: &dyn MirDb, struct_: &analyzer_types::Struct) -> TypeKind {
    let id = struct_.id;

    let name = id.name(db.upcast());

    // Lower struct fields.
    let fields = id
        .fields(db.upcast())
        .iter()
        .map(|(fname, fid)| {
            let analyzer_types = fid.typ(db.upcast()).unwrap();
            let ty = db.mir_lowered_type(analyzer_types);
            (fname.clone(), ty)
        })
        .collect();

    // obtain span.
    let span = id.span(db.upcast());

    let module_id = id.module(db.upcast());

    let def = StructDef {
        name,
        fields,
        span,
        module_id,
    };
    TypeKind::Struct(def)
}

fn intern_type(
    db: &dyn MirDb,
    ty_kind: TypeKind,
    analyzer_type: Option<&analyzer_types::Type>,
) -> TypeId {
    db.mir_intern_type(Type::new(ty_kind, analyzer_type.cloned()).into())
}
