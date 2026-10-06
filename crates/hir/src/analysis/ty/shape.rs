//! Projection return shapes.
//!
//! `ref` and `mut` are not types. A function whose return mentions them is a
//! projection, and its return is a *shape*: owned components, access
//! components, tuples of shapes, and the sum shapes `Option<shape>` and
//! `Result<E, shape>`. The type checker sees a projection call's *erased*
//! type; the shape records which parts of that type the call grants as
//! accesses. Semantic lowering represents an access component by a carrier.
use salsa::Update;

use super::{
    corelib::resolve_lib_type_path,
    diagnostics::TyDiagCollection,
    fold::{TyFoldable, TyFolder},
    trait_resolution::PredicateListId,
    ty_def::{BorrowKind, InvalidCause, TyId},
    ty_error::collect_hir_ty_diags,
    ty_lower::lower_hir_ty,
    visitor::{TyVisitable, TyVisitor},
};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{
        GenericArg, Partial, PathId, TypeId as HirTyId, TypeKind, TypeMode, scope_graph::ScopeId,
    },
    span::types::LazyTySpan,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum Shape<'db> {
    Owned(TyId<'db>),
    Access(BorrowKind, TyId<'db>),
    Tuple(Vec<Shape<'db>>),
    /// `Option<shape>` or `Result<E, shape>`. Only `variant` carries the
    /// payload shape, as type argument `arg` of `ty`, the erased enum type.
    Sum {
        ty: TyId<'db>,
        variant: u16,
        arg: usize,
        payload: Box<Shape<'db>>,
    },
}

impl<'db> Shape<'db> {
    /// Whether this shape grants an access, i.e. whether a function returning
    /// it is a projection.
    pub fn has_access(&self) -> bool {
        match self {
            Self::Owned(_) => false,
            Self::Access(..) => true,
            Self::Tuple(elems) => elems.iter().any(Self::has_access),
            Self::Sum { payload, .. } => payload.has_access(),
        }
    }

    /// The type a projection call has in the type checker.
    pub fn erased_ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self {
            Self::Owned(ty) | Self::Access(_, ty) | Self::Sum { ty, .. } => *ty,
            Self::Tuple(elems) => TyId::tuple_with_elems(
                db,
                &elems
                    .iter()
                    .map(|elem| elem.erased_ty(db))
                    .collect::<Vec<_>>(),
            ),
        }
    }

    /// The semantic-IR representation of the shape: access components are
    /// carriers.
    pub fn carrier_ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self {
            Self::Owned(ty) => *ty,
            Self::Access(kind, ty) => TyId::borrow_of(db, *kind, *ty),
            Self::Tuple(elems) => TyId::tuple_with_elems(
                db,
                &elems
                    .iter()
                    .map(|elem| elem.carrier_ty(db))
                    .collect::<Vec<_>>(),
            ),
            Self::Sum {
                ty, arg, payload, ..
            } => {
                let (base, args) = ty.decompose_ty_app(db);
                args.iter().enumerate().fold(base, |acc, (idx, &ty_arg)| {
                    let ty_arg = if idx == *arg {
                        payload.carrier_ty(db)
                    } else {
                        ty_arg
                    };
                    TyId::app(db, acc, ty_arg)
                })
            }
        }
    }

    /// Whether a value granting `self` satisfies `required`: the same
    /// structure, with each access at least as strong.
    pub fn grants(&self, required: &Self) -> bool {
        match (self, required) {
            (Self::Owned(_), Self::Owned(_)) => true,
            (Self::Access(given, _), Self::Access(required, _)) => given >= required,
            (Self::Tuple(given), Self::Tuple(required)) => {
                given.len() == required.len()
                    && given
                        .iter()
                        .zip(required)
                        .all(|(given, required)| given.grants(required))
            }
            (
                Self::Sum {
                    variant, payload, ..
                },
                Self::Sum {
                    variant: required_variant,
                    payload: required_payload,
                    ..
                },
            ) => variant == required_variant && payload.grants(required_payload),
            _ => false,
        }
    }

    /// Whether two shapes grant the same accesses, ignoring the types.
    pub fn same_modes(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Owned(_), Self::Owned(_)) => true,
            (Self::Access(lhs, _), Self::Access(rhs, _)) => lhs == rhs,
            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.len() == rhs.len() && lhs.iter().zip(rhs).all(|(lhs, rhs)| lhs.same_modes(rhs))
            }
            (
                Self::Sum {
                    variant, payload, ..
                },
                Self::Sum {
                    variant: other_variant,
                    payload: other_payload,
                    ..
                },
            ) => variant == other_variant && payload.same_modes(other_payload),
            _ => false,
        }
    }

    pub fn pretty_print(&self, db: &'db dyn HirAnalysisDb) -> String {
        match self {
            Self::Owned(ty) => ty.pretty_print(db).to_string(),
            Self::Access(BorrowKind::Ref, ty) => format!("ref {}", ty.pretty_print(db)),
            Self::Access(BorrowKind::Mut, ty) => format!("mut {}", ty.pretty_print(db)),
            Self::Tuple(elems) => format!(
                "({})",
                elems
                    .iter()
                    .map(|elem| elem.pretty_print(db))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Sum {
                ty, arg, payload, ..
            } => {
                let (base, args) = ty.decompose_ty_app(db);
                let args = args
                    .iter()
                    .enumerate()
                    .map(|(idx, ty_arg)| {
                        if idx == *arg {
                            payload.pretty_print(db)
                        } else {
                            ty_arg.pretty_print(db).to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{args}>", base.pretty_print(db))
            }
        }
    }

    pub fn map_tys(&self, f: &mut impl FnMut(TyId<'db>) -> TyId<'db>) -> Self {
        match self {
            Self::Owned(ty) => Self::Owned(f(*ty)),
            Self::Access(kind, ty) => Self::Access(*kind, f(*ty)),
            Self::Tuple(elems) => Self::Tuple(elems.iter().map(|elem| elem.map_tys(f)).collect()),
            Self::Sum {
                ty,
                variant,
                arg,
                payload,
            } => Self::Sum {
                ty: f(*ty),
                variant: *variant,
                arg: *arg,
                payload: Box::new(payload.map_tys(f)),
            },
        }
    }
}

impl<'db> TyVisitable<'db> for Shape<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Owned(ty) | Self::Access(_, ty) => ty.visit_with(visitor),
            Self::Tuple(elems) => elems.iter().for_each(|elem| elem.visit_with(visitor)),
            Self::Sum { ty, payload, .. } => {
                ty.visit_with(visitor);
                payload.visit_with(visitor);
            }
        }
    }
}

impl<'db> TyFoldable<'db> for Shape<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            Self::Owned(ty) => Self::Owned(ty.fold_with(db, folder)),
            Self::Access(kind, ty) => Self::Access(kind, ty.fold_with(db, folder)),
            Self::Tuple(elems) => Self::Tuple(
                elems
                    .into_iter()
                    .map(|elem| elem.fold_with(db, folder))
                    .collect(),
            ),
            Self::Sum {
                ty,
                variant,
                arg,
                payload,
            } => Self::Sum {
                ty: ty.fold_with(db, folder),
                variant,
                arg,
                payload: Box::new(payload.fold_with(db, folder)),
            },
        }
    }
}

/// Whether a written type mentions a mode anywhere.
pub fn mentions_mode<'db>(db: &'db dyn HirAnalysisDb, ty: HirTyId<'db>) -> bool {
    let mentions = |ty: &Partial<HirTyId<'db>>| ty.to_opt().is_some_and(|ty| mentions_mode(db, ty));
    match ty.data(db) {
        TypeKind::Mode(..) => true,
        TypeKind::Ptr(inner) | TypeKind::Array(inner, _) => mentions(inner),
        TypeKind::Tuple(elems) => elems.data(db).iter().any(mentions),
        TypeKind::Path(path) => path.to_opt().is_some_and(|path| {
            (0..path.len(db)).any(|idx| {
                path.segment(db, idx).is_some_and(|segment| {
                    segment
                        .generic_args(db)
                        .data(db)
                        .iter()
                        .any(|arg| matches!(arg, GenericArg::Type(arg) if mentions(&arg.ty)))
                })
            })
        }),
        TypeKind::Never => false,
    }
}

/// Lowers a written return type to its shape.
pub fn lower_return_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Shape<'db> {
    let lower_opt = |ty: Partial<HirTyId<'db>>| {
        ty.to_opt().map_or_else(
            || TyId::invalid(db, InvalidCause::ParseError),
            |ty| lower_hir_ty(db, ty, scope, assumptions),
        )
    };
    if !mentions_mode(db, ty) {
        return Shape::Owned(lower_hir_ty(db, ty, scope, assumptions));
    }
    match ty.data(db) {
        TypeKind::Mode(TypeMode::Mut, inner) => Shape::Access(BorrowKind::Mut, lower_opt(*inner)),
        TypeKind::Mode(TypeMode::Ref, inner) => Shape::Access(BorrowKind::Ref, lower_opt(*inner)),
        TypeKind::Mode(TypeMode::Own, inner) => Shape::Owned(lower_opt(*inner)),
        TypeKind::Tuple(elems) => Shape::Tuple(
            elems
                .data(db)
                .iter()
                .map(|elem| match elem.to_opt() {
                    Some(elem) => lower_return_shape(db, elem, scope, assumptions),
                    None => Shape::Owned(TyId::invalid(db, InvalidCause::ParseError)),
                })
                .collect(),
        ),
        TypeKind::Path(Partial::Present(path)) => sum_shape(db, *path, scope, assumptions)
            .unwrap_or_else(|| Shape::Owned(lower_hir_ty(db, ty, scope, assumptions))),
        _ => Shape::Owned(lower_hir_ty(db, ty, scope, assumptions)),
    }
}

/// The variant and type argument of a sum type that carries a shape:
/// `Option::Some(T)` and `Result::Ok(T)` (`Result<E, T>`).
fn sum_shape_slot<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<(TyId<'db>, u16, usize)> {
    let ctor_hir = HirTyId::new(
        db,
        TypeKind::Path(Partial::Present(path.strip_generic_args(db))),
    );
    let ctor = lower_hir_ty(db, ctor_hir, scope, assumptions);
    let is = |lib_path| {
        resolve_lib_type_path(db, scope, lib_path)
            .is_some_and(|lib| lib.base_ty(db) == ctor.base_ty(db))
    };
    if is("core::option::Option") {
        Some((ctor, 0, 0))
    } else if is("core::result::Result") {
        Some((ctor, 1, 1))
    } else {
        None
    }
}

fn sum_type_args<'db>(db: &'db dyn HirAnalysisDb, path: PathId<'db>) -> Option<Vec<HirTyId<'db>>> {
    path.generic_args(db)
        .data(db)
        .iter()
        .map(|arg| match arg {
            GenericArg::Type(arg) => arg.ty.to_opt(),
            GenericArg::Const(_) | GenericArg::AssocType(_) => None,
        })
        .collect()
}

fn sum_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<Shape<'db>> {
    let (ctor, variant, arg) = sum_shape_slot(db, path, scope, assumptions)?;
    let args = sum_type_args(db, path)?;
    if args.len() != arg + 1 {
        return None;
    }
    let payload = lower_return_shape(db, args[arg], scope, assumptions);
    let ty = args.iter().enumerate().fold(ctor, |acc, (idx, &ty_arg)| {
        let ty_arg = if idx == arg {
            payload.erased_ty(db)
        } else {
            lower_hir_ty(db, ty_arg, scope, assumptions)
        };
        TyId::app(db, acc, ty_arg)
    });
    Some(Shape::Sum {
        ty,
        variant,
        arg,
        payload: Box::new(payload),
    })
}

/// Diagnostics for a written return type. Modes are accepted exactly where
/// the shape grammar allows them; every other position is checked as a type.
pub fn return_shape_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let opt_diags = |ty: Partial<HirTyId<'db>>, span: LazyTySpan<'db>| {
        ty.to_opt()
            .map(|ty| return_shape_diags(db, ty, scope, span, assumptions))
            .unwrap_or_default()
    };
    if !mentions_mode(db, ty) {
        return collect_hir_ty_diags(db, scope, ty, span, assumptions);
    }
    match ty.data(db) {
        TypeKind::Mode(TypeMode::Mut | TypeMode::Ref, inner) => inner
            .to_opt()
            .map(|inner| {
                collect_hir_ty_diags(db, scope, inner, span.into_mode_type().inner(), assumptions)
            })
            .unwrap_or_default(),
        TypeKind::Tuple(elems) => elems
            .data(db)
            .iter()
            .enumerate()
            .flat_map(|(idx, elem)| opt_diags(*elem, span.clone().into_tuple_type().elem_ty(idx)))
            .collect(),
        TypeKind::Path(Partial::Present(path))
            if let Some((_, _, arg)) = sum_shape_slot(db, *path, scope, assumptions)
                && let Some(args) = sum_type_args(db, *path)
                && args.len() == arg + 1 =>
        {
            let segment = path.segment_index(db);
            args.iter()
                .enumerate()
                .flat_map(|(idx, &ty_arg)| {
                    let span = span
                        .clone()
                        .into_path_type()
                        .path()
                        .segment(segment)
                        .generic_args()
                        .arg(idx)
                        .into_type_arg()
                        .ty();
                    if idx == arg {
                        return_shape_diags(db, ty_arg, scope, span, assumptions)
                    } else {
                        collect_hir_ty_diags(db, scope, ty_arg, span, assumptions)
                    }
                })
                .collect()
        }
        _ => collect_hir_ty_diags(db, scope, ty, span, assumptions),
    }
}
