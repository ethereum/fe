mod env;
mod expr;
mod pat;
mod stmt;

use env::TyCheckEnv;
use hir::{
    hir_def::{
        Body, Enum, ExprId, Func, IdentId, LitKind, PatId, PathId, TypeId as HirTyId,
        VariantKind as HirVariantKind,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_def::{AdtDef, AdtRef, AdtRefId, InvalidCause, Kind, Subst, TyId, TyVarUniverse},
    ty_lower::{lower_adt, lower_hir_ty},
    unify::{UnificationError, UnificationTable},
};
use crate::HirAnalysisDb;

#[salsa::tracked(return_ref)]
pub fn check_func_body(db: &dyn HirAnalysisDb, func: Func) -> TypedBody {
    let Ok(mut checker) = TyChecker::new_with_func(db, func) else {
        return TypedBody::empty();
    };

    checker.run();
    checker.finish()
}

pub struct TyChecker<'db> {
    db: &'db dyn HirAnalysisDb,
    env: TyCheckEnv<'db>,
    table: UnificationTable<'db>,
    expected: TyId,
}

impl<'db> TyChecker<'db> {
    fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func) -> Result<Self, ()> {
        let env = TyCheckEnv::new_with_func(db, func)?;
        let expected_ty = match func.ret_ty(db.as_hir_db()) {
            Some(hir_ty) => {
                let ty = lower_hir_ty(db, hir_ty, func.scope());
                if ty.is_star_kind(db) {
                    ty
                } else {
                    TyId::invalid(db, InvalidCause::Other)
                }
            }
            None => TyId::unit(db),
        };

        Ok(Self::new(db, env, expected_ty))
    }

    fn run(&mut self) {
        let root_expr = self.env.body().expr(self.db.as_hir_db());
        self.check_expr(root_expr, self.expected);
    }

    fn finish(mut self) -> TypedBody {
        // TODO: check for untyped expressions and patterns.
        self.env.finish(&mut self.table)
    }

    fn new(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
        }
    }

    fn body(&self) -> Body {
        self.env.body()
    }

    fn lit_ty(&mut self, lit: &LitKind) -> TyId {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarUniverse::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db.as_hir_db());
                self.table
                    .new_var(TyVarUniverse::String(len_bytes), &Kind::Star)
            }
        }
    }

    fn lower_ty(&self, hir_ty: HirTyId, span: DynLazySpan) -> TyId {
        let ty = lower_hir_ty(self.db, hir_ty, self.env.scope());
        if let Some(diag) = ty.emit_diag(self.db, span) {
            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        ty
    }

    /// Returns the fresh type variable for pattern and expr type checking. The
    /// kind of the type variable is `*`, and the universe is `General`.
    fn fresh_ty(&mut self) -> TyId {
        self.table.new_var(TyVarUniverse::General, &Kind::Star)
    }

    fn fresh_tys_n(&mut self, n: usize) -> Vec<TyId> {
        (0..n).map(|_| self.fresh_ty()).collect()
    }

    fn unify_ty<T>(&mut self, t: T, actual: TyId, expected: TyId) -> TyId
    where
        T: Into<Typeable>,
    {
        let t = t.into();

        let actual = match self.table.unify(expected, actual) {
            Ok(()) => actual,

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.apply_subst(self.db, &mut self.table);
                let expected = expected.apply_subst(self.db, &mut self.table);
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::type_mismatch(
                        self.db,
                        t.lazy_span(self.env.body()),
                        expected,
                        actual,
                    )
                    .into(),
                );
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Err(UnificationError::OccursCheckFailed) => {
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::InfiniteOccurrence(t.lazy_span(self.env.body())).into(),
                );

                TyId::invalid(self.db, InvalidCause::Other)
            }
        };

        match t {
            Typeable::Expr(expr) => self.env.type_expr(expr, actual),
            Typeable::Pat(pat) => self.env.type_pat(pat, actual),
        }

        actual
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBody {
    body: Option<Body>,
    pat_ty: FxHashMap<PatId, TyId>,
    expr_ty: FxHashMap<ExprId, TyId>,
}

impl TypedBody {
    pub fn expr_ty(&self, db: &dyn HirAnalysisDb, expr: ExprId) -> TyId {
        self.expr_ty
            .get(&expr)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }
    pub fn pat_ty(&self, db: &dyn HirAnalysisDb, pat: PatId) -> TyId {
        self.pat_ty
            .get(&pat)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    fn empty() -> Self {
        Self {
            body: None,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, derive_more::From)]
enum Typeable {
    Expr(ExprId),
    Pat(PatId),
}

impl Typeable {
    fn lazy_span(self, body: Body) -> DynLazySpan {
        match self {
            Self::Expr(expr) => expr.lazy_span(body).into(),
            Self::Pat(pat) => pat.lazy_span(body).into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ResolvedPathData {
    Adt {
        adt: AdtDef,
        args: FxHashMap<TyId, TyId>,
        path: PathId,
    },

    Variant {
        enum_: Enum,
        idx: usize,
        args: FxHashMap<TyId, TyId>,
        path: PathId,
    },
}

impl ResolvedPathData {
    fn new_adt(
        db: &dyn HirAnalysisDb,
        table: &mut UnificationTable,
        adt: AdtDef,
        path: PathId,
    ) -> Self {
        let args = adt
            .params(db)
            .iter()
            .map(|param| (*param, table.new_var_from_param(*param)))
            .collect();

        Self::Adt { adt, args, path }
    }

    fn new_variant(
        db: &dyn HirAnalysisDb,
        table: &mut UnificationTable,
        enum_: Enum,
        idx: usize,
        path: PathId,
    ) -> Self {
        let args = lower_adt(db, AdtRefId::from_enum(db, enum_))
            .params(db)
            .iter()
            .map(|param| (*param, table.new_var_from_param(*param)))
            .collect();

        Self::Variant {
            enum_,
            idx,
            args,
            path,
        }
    }

    pub(crate) fn adt_ref(&self, db: &dyn HirAnalysisDb) -> AdtRefId {
        match self {
            Self::Adt { adt, .. } => adt.adt_ref(db),
            Self::Variant { enum_, .. } => AdtRefId::from_enum(db, *enum_),
        }
    }

    pub(crate) fn adt_def(&self, db: &dyn HirAnalysisDb) -> AdtDef {
        match self {
            Self::Adt { adt, .. } => *adt,
            Self::Variant { enum_, .. } => lower_adt(db, AdtRefId::from_enum(db, *enum_)),
        }
    }

    pub(crate) fn data_kind(&self, db: &dyn HirAnalysisDb) -> &'static str {
        match self {
            Self::Adt { adt, .. } => adt.adt_ref(db).kind_name(db),
            Self::Variant { enum_, idx, .. } => {
                let hir_db = db.as_hir_db();
                match enum_.variants(hir_db).data(hir_db)[*idx].kind {
                    hir::hir_def::VariantKind::Unit => "unit variant",
                    HirVariantKind::Tuple(_) => "tuple variant",
                    HirVariantKind::Record(_) => "record variant",
                }
            }
        }
    }

    pub(crate) fn initializer_hint(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db.as_hir_db();

        let expected_sub_pat = match self {
            Self::Adt { .. } => {
                let AdtRef::Struct(s) = self.adt_ref(db).data(db) else {
                    return None;
                };

                s.format_initializer_args(hir_db)
            }

            Self::Variant { enum_, idx, .. } => {
                enum_.variants(hir_db).data(hir_db)[*idx].format_initializer_args(hir_db)
            }
        };

        let path = self.path().pretty_print(hir_db);
        Some(format!("{}{}", path, expected_sub_pat))
    }

    pub(super) fn def_span(&self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        match self {
            Self::Adt { .. } => self.adt_ref(db).name_span(db),
            Self::Variant { enum_, idx, .. } => {
                enum_.lazy_span().variants_moved().variant(*idx).into()
            }
        }
    }

    pub(super) fn def_name(&self, db: &dyn HirAnalysisDb) -> IdentId {
        match self {
            Self::Adt { adt, .. } => adt.adt_ref(db).name(db),
            Self::Variant { enum_, idx, .. } => {
                *enum_.variants(db.as_hir_db()).data(db.as_hir_db())[*idx]
                    .name
                    .unwrap()
            }
        }
    }

    fn path(&self) -> PathId {
        match self {
            Self::Adt { path, .. } => *path,
            Self::Variant { path, .. } => *path,
        }
    }

    fn is_record(&self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Adt { adt, .. } => matches!(
                adt.adt_ref(db).data(db),
                AdtRef::Struct(_) | AdtRef::Contract(_)
            ),

            Self::Variant { enum_, idx, .. } => matches!(
                enum_.variants(db.as_hir_db()).data(db.as_hir_db())[*idx].kind,
                HirVariantKind::Record(_)
            ),
        }
    }

    fn is_unit_variant(&self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Adt { .. } => false,
            Self::Variant { enum_, idx, .. } => {
                let hir_db = db.as_hir_db();
                matches!(
                    enum_.variants(hir_db).data(hir_db)[*idx].kind,
                    hir::hir_def::VariantKind::Unit
                )
            }
        }
    }

    fn is_tuple_variant(&self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Adt { .. } => false,
            Self::Variant { enum_, idx, .. } => {
                let hir_db = db.as_hir_db();
                matches!(
                    enum_.variants(hir_db).data(hir_db)[*idx].kind,
                    HirVariantKind::Tuple(_)
                )
            }
        }
    }

    fn ty(&self, db: &dyn HirAnalysisDb) -> TyId {
        let adt = match self {
            Self::Adt { adt, .. } => *adt,

            Self::Variant { enum_, .. } => lower_adt(db, AdtRefId::from_enum(db, *enum_)),
        };

        let adt_ty = TyId::adt(db, adt);
        self.args().fold(adt_ty, |ty, arg| TyId::app(db, ty, *arg))
    }

    fn record_field_ty(&mut self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<TyId> {
        let hir_db = db.as_hir_db();

        let (hir_field_list, field_list) = match self {
            Self::Adt { adt, .. } => match adt.adt_ref(db).data(db) {
                AdtRef::Struct(s) => (s.fields(hir_db), &adt.fields(db)[0]),
                AdtRef::Contract(c) => (c.fields(hir_db), &adt.fields(db)[0]),

                _ => return None,
            },

            Self::Variant { enum_, ref idx, .. } => {
                match enum_.variants(hir_db).data(hir_db)[*idx].kind {
                    hir::hir_def::VariantKind::Record(fields) => {
                        (fields, &self.adt_def(db).fields(db)[*idx])
                    }

                    _ => return None,
                }
            }
        };

        let field_idx = hir_field_list.field_idx(hir_db, name)?;
        let ty = field_list.ty(db, field_idx);

        Some(ty.apply_subst(db, self.subst()))
    }

    fn args(&self) -> impl Iterator<Item = &TyId> {
        match self {
            Self::Adt { args, .. } | Self::Variant { args, .. } => args.values(),
        }
    }

    fn subst(&mut self) -> &mut impl Subst {
        match self {
            Self::Adt { args, .. } | Self::Variant { args, .. } => args,
        }
    }

    fn field_tys(&mut self, db: &dyn HirAnalysisDb) -> Vec<TyId> {
        let (adt, idx) = match self {
            Self::Adt { adt, .. } => {
                if matches!(
                    adt.adt_ref(db).data(db),
                    AdtRef::Struct(_) | AdtRef::Contract(_)
                ) {
                    (*adt, 0)
                } else {
                    return vec![];
                }
            }
            Self::Variant { enum_, idx, .. } => {
                let adt = lower_adt(db, AdtRefId::from_enum(db, *enum_));
                (adt, *idx)
            }
        };

        adt.fields(db)[idx]
            .iter_types(db)
            .map(|ty| ty.apply_subst(db, self.subst()))
            .collect()
    }

    fn record_labels(&mut self, db: &dyn HirAnalysisDb) -> FxHashSet<IdentId> {
        let hir_db = db.as_hir_db();

        let fields = match self {
            Self::Adt { adt, .. } => match adt.adt_ref(db).data(db) {
                AdtRef::Struct(s) => s.fields(hir_db),
                AdtRef::Contract(c) => c.fields(hir_db),

                _ => return FxHashSet::default(),
            },

            Self::Variant { enum_, ref idx, .. } => {
                match enum_.variants(hir_db).data(hir_db)[*idx].kind {
                    hir::hir_def::VariantKind::Record(fields) => fields,

                    _ => return FxHashSet::default(),
                }
            }
        };

        fields
            .data(hir_db)
            .iter()
            .filter_map(|field| field.name.to_opt())
            .collect()
    }
}
