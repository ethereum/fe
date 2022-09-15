use fe_mir::ir::{
    types::{ArrayDef, MapDef, StructDef, TupleDef},
    Type, TypeId, TypeKind,
};

use crate::db::CodegenDb;

pub fn legalized_type(db: &dyn CodegenDb, ty: TypeId) -> TypeId {
    let ty_data = ty.data(db.upcast());
    let ty_kind = match &ty.data(db.upcast()).kind {
        TypeKind::Tuple(def) => {
            let items = def
                .items
                .iter()
                .filter_map(|item| {
                    if item.is_zero_sized(db.upcast()) {
                        None
                    } else {
                        Some(legalized_type(db, *item))
                    }
                })
                .collect();
            let new_def = TupleDef { items };
            TypeKind::Tuple(new_def)
        }

        TypeKind::Array(def) => {
            let new_def = ArrayDef {
                elem_ty: legalized_type(db, def.elem_ty),
                len: def.len,
            };
            TypeKind::Array(new_def)
        }

        TypeKind::Struct(def) => {
            let fields = def
                .fields
                .iter()
                .cloned()
                .filter_map(|(name, ty)| {
                    if ty.is_zero_sized(db.upcast()) {
                        None
                    } else {
                        Some((name, legalized_type(db, ty)))
                    }
                })
                .collect();
            let new_def = StructDef {
                name: def.name.clone(),
                fields,
                span: def.span,
                module_id: def.module_id,
            };
            TypeKind::Struct(new_def)
        }

        TypeKind::Contract(def) => {
            let fields = def
                .fields
                .iter()
                .cloned()
                .filter_map(|(name, ty)| {
                    if ty.is_zero_sized(db.upcast()) {
                        None
                    } else {
                        Some((name, legalized_type(db, ty)))
                    }
                })
                .collect();
            let new_def = StructDef {
                name: def.name.clone(),
                fields,
                span: def.span,
                module_id: def.module_id,
            };
            TypeKind::Contract(new_def)
        }

        TypeKind::Map(def) => {
            let new_def = MapDef {
                key_ty: legalized_type(db, def.key_ty),
                value_ty: legalized_type(db, def.value_ty),
            };
            TypeKind::Map(new_def)
        }

        TypeKind::MPtr(ty) => {
            let new_ty = legalized_type(db, *ty);
            TypeKind::MPtr(new_ty)
        }

        TypeKind::SPtr(ty) => {
            let new_ty = legalized_type(db, *ty);
            TypeKind::SPtr(new_ty)
        }

        _ => return ty,
    };

    let analyzer_ty = ty_data.analyzer_ty;
    db.mir_intern_type(Type::new(ty_kind, analyzer_ty).into())
}
