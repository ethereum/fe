use fe_parser2::ast::{self, prelude::*};

use crate::{
    hir_def::{Body, GenericArgListId, PathId, TypeId, TypeKind},
    span::FileId,
    HirDb,
};

impl TypeId {
    pub(crate) fn from_ast(db: &dyn HirDb, fid: FileId, ast: Option<ast::Type>) -> Self {
        let Some(ty) = ast else {
            return TypeId::new(db, TypeKind::Invalid);
        };

        let kind = match ty.kind() {
            ast::TypeKind::Ptr(ptr_type) => {
                let inner = ptr_type.inner();
                let inner_id = TypeId::from_ast(db, fid, inner);
                TypeKind::Ptr(inner_id)
            }

            ast::TypeKind::Path(path_type) => {
                let path = path_type.path();
                let path_id = PathId::from_ast(db, path);
                if let Some(generic_args) = path_type.generic_args() {
                    let generic_args = GenericArgListId::from_ast(db, fid, generic_args);
                    TypeKind::Path(path_id, generic_args.into())
                } else {
                    TypeKind::Path(path_id, None)
                }
            }

            ast::TypeKind::SelfType(_) => TypeKind::SelfType,

            ast::TypeKind::Tuple(tuple_type) => {
                let mut elem_tys = Vec::new();
                for elem in tuple_type {
                    elem_tys.push(TypeId::from_ast(db, fid, elem.into()));
                }
                TypeKind::Tuple(elem_tys)
            }

            ast::TypeKind::Array(array_type) => {
                let elem = array_type.elem_ty();
                let elem_ty_id = TypeId::from_ast(db, fid, elem);

                let body = if let Some(body) = array_type.len() {
                    Body::from_ast(db, fid, body)
                } else {
                    Body::invalid(db, fid)
                };

                TypeKind::Array(elem_ty_id, body)
            }
        };

        TypeId::new(db, kind)
    }
}
