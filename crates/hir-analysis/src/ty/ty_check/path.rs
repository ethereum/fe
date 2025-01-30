use std::collections::hash_map::Entry;

use hir::{
    hir_def::{
        scope_graph::{FieldParent, ScopeId},
        FieldDefListId as HirFieldDefListId, IdentId, VariantKind as HirVariantKind,
    },
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;
use smallvec2::SmallVec;

use super::{env::LocalBinding, TyChecker};
use crate::{
    name_resolution::{diagnostics::NameResDiag, is_scope_visible_from, PathRes, ResolvedVariant},
    ty::{
        adt_def::{AdtDef, AdtField, AdtRef, AdtRefId},
        diagnostics::{BodyDiag, FuncBodyDiag},
        ty_def::{InvalidCause, TyData, TyId},
    },
    HirAnalysisDb,
};

impl<'db> TyId<'db> {
    pub(crate) fn adt_ref(&self, db: &'db dyn HirAnalysisDb) -> Option<AdtRefId<'db>> {
        self.adt_def(db).map(|def| def.adt_ref(db))
    }

    pub(crate) fn adt_def(&self, db: &'db dyn HirAnalysisDb) -> Option<AdtDef<'db>> {
        let base = self.decompose_ty_app(db).0;
        match base.data(db) {
            TyData::TyBase(base) => base.adt(),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInBody<'db> {
    Reso(PathRes<'db>),
    Binding(LocalBinding<'db>),
    NewBinding(IdentId<'db>),
    #[allow(dead_code)] // TODO: we might be failing to report some errors
    Diag(FuncBodyDiag<'db>),
    Invalid,
}

pub(super) struct RecordInitChecker<'tc, 'db, 'a> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    data: &'a mut RecordLike<'db>,
    already_given: FxHashMap<IdentId<'db>, DynLazySpan<'db>>,
    invalid_field_given: bool,
}

#[derive(Clone)]
pub(crate) enum RecordLike<'db> {
    Type(TyId<'db>),
    Variant(ResolvedVariant<'db>),
}

impl<'tc, 'db, 'a> RecordInitChecker<'tc, 'db, 'a> {
    pub(super) fn new(tc: &'tc mut TyChecker<'db>, data: &'a mut RecordLike<'db>) -> Self {
        assert!(data.is_record(tc.db));

        Self {
            tc,
            data,
            already_given: FxHashMap::default(),
            invalid_field_given: false,
        }
    }

    pub(super) fn feed_label(
        &mut self,
        label: Option<IdentId<'db>>,
        field_span: DynLazySpan<'db>,
    ) -> Result<TyId<'db>, FuncBodyDiag<'db>> {
        let label = match label {
            Some(label) => match self.already_given.entry(label) {
                Entry::Occupied(first_use) => {
                    let diag = BodyDiag::DuplicatedRecordFieldBind {
                        primary: field_span.clone(),
                        first_use: first_use.get().clone(),
                        name: label,
                    };

                    self.invalid_field_given = true;
                    return Err(diag.into());
                }

                Entry::Vacant(entry) => {
                    entry.insert(field_span.clone());
                    label
                }
            },

            None => {
                let diag = BodyDiag::ExplicitLabelExpectedInRecord {
                    primary: field_span,
                    hint: self.data.initializer_hint(self.tc.db),
                };

                self.invalid_field_given = true;
                return Err(diag.into());
            }
        };

        let Some(ty) = self.data.record_field_ty(self.tc.db, label) else {
            let diag = BodyDiag::RecordFieldNotFound {
                span: field_span,
                label,
            };

            self.invalid_field_given = true;
            return Err(diag.into());
        };

        let field_scope = self.data.record_field_scope(self.tc.db, label).unwrap();
        if is_scope_visible_from(self.tc.db, field_scope, self.tc.env.scope()) {
            Ok(ty)
        } else {
            let diag = NameResDiag::Invisible(
                field_span,
                label,
                field_scope.name_span(self.tc.db.as_hir_db()),
            );

            self.invalid_field_given = true;
            Err(diag.into())
        }
    }

    pub(super) fn finalize(
        self,
        initializer_span: DynLazySpan<'db>,
        allow_missing_field: bool,
    ) -> Result<(), FuncBodyDiag<'db>> {
        if !self.invalid_field_given && !allow_missing_field {
            let expected_labels = self.data.record_labels(self.tc.db);
            let missing_fields: SmallVec<_, 4> = expected_labels
                .iter()
                .filter(|f| !self.already_given.contains_key(f))
                .cloned()
                .collect();

            if !missing_fields.is_empty() {
                let diag = BodyDiag::MissingRecordFields {
                    primary: initializer_span,
                    missing_fields,
                    hint: self.data.initializer_hint(self.tc.db),
                };

                return Err(diag.into());
            }
        }

        Ok(())
    }
}

impl<'db> RecordLike<'db> {
    fn is_record(&self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            RecordLike::Type(ty) => {
                if let Some(adt_ref) = ty.adt_ref(db) {
                    matches!(adt_ref.data(db), AdtRef::Struct(..))
                } else {
                    false
                }
            }
            RecordLike::Variant(variant) => {
                matches!(variant.variant_kind(db), HirVariantKind::Record(..))
            }
        }
    }

    pub fn record_field_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        match self {
            RecordLike::Type(ty) => {
                let args = ty.generic_args(db);
                let hir_db = db.as_hir_db();
                let (hir_field_list, field_list) = self.record_field_list(db)?;
                let field_idx = hir_field_list.field_idx(hir_db, name)?;
                let field_ty = field_list.ty(db, field_idx).instantiate(db, args);
                if field_ty.is_star_kind(db) {
                    field_ty
                } else {
                    TyId::invalid(db, InvalidCause::Other)
                }
                .into()
            }
            RecordLike::Variant(variant) => {
                let args = variant.ty.generic_args(db);
                let hir_db = db.as_hir_db();
                let (hir_field_list, field_list) = self.record_field_list(db)?;
                let field_idx = hir_field_list.field_idx(hir_db, name)?;
                Some(field_list.ty(db, field_idx).instantiate(db, args))
            }
        }
    }

    fn record_field_list(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId<'db>, &'db AdtField<'db>)> {
        match self {
            RecordLike::Type(ty) => {
                let hir_db = db.as_hir_db();
                let adt_def = ty.adt_def(db)?;
                match adt_def.adt_ref(db).data(db) {
                    AdtRef::Struct(s) => (s.fields(hir_db), &adt_def.fields(db)[0]).into(),
                    AdtRef::Contract(c) => (c.fields(hir_db), &adt_def.fields(db)[0]).into(),
                    _ => None,
                }
            }
            RecordLike::Variant(variant) => match variant.variant_kind(db) {
                hir::hir_def::VariantKind::Record(fields) => (
                    fields,
                    &variant.ty.adt_def(db).unwrap().fields(db)[variant.idx],
                )
                    .into(),
                _ => None,
            },
        }
    }

    fn record_field_idx(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<usize> {
        let (hir_field_list, _) = self.record_field_list(db)?;
        hir_field_list.field_idx(db.as_hir_db(), name)
    }

    pub fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>> {
        match self {
            RecordLike::Type(ty) => {
                let field_idx = self.record_field_idx(db, name)?;
                let adt_ref = ty.adt_ref(db)?;
                let parent = FieldParent::Item(adt_ref.as_item(db));
                Some(ScopeId::Field(parent, field_idx))
            }
            RecordLike::Variant(variant) => {
                let field_idx = self.record_field_idx(db, name)?;
                let parent = FieldParent::Variant(variant.enum_(db).into(), variant.idx);
                Some(ScopeId::Field(parent, field_idx))
            }
        }
    }

    pub fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> Vec<IdentId<'db>> {
        let hir_db = db.as_hir_db();
        match self {
            RecordLike::Type(ty) => {
                let Some(adt_ref) = ty.adt_ref(db) else {
                    return Vec::default();
                };
                let fields = match adt_ref.data(db) {
                    AdtRef::Struct(s) => s.fields(hir_db),
                    AdtRef::Contract(c) => c.fields(hir_db),
                    _ => return Vec::default(),
                };
                fields
                    .data(hir_db)
                    .iter()
                    .filter_map(|field| field.name.to_opt())
                    .collect()
            }
            RecordLike::Variant(variant) => {
                let fields = match variant.variant_kind(db) {
                    hir::hir_def::VariantKind::Record(fields) => fields,
                    _ => return Vec::default(),
                };
                fields
                    .data(hir_db)
                    .iter()
                    .filter_map(|field| field.name.to_opt())
                    .collect()
            }
        }
    }

    pub fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String {
        match self {
            RecordLike::Type(ty) => {
                if let Some(adt_ref) = ty.adt_ref(db) {
                    adt_ref.kind_name(db).to_string()
                } else if ty.is_func(db) {
                    "fn".to_string()
                } else {
                    ty.pretty_print(db).to_string()
                }
            }
            RecordLike::Variant(variant) => {
                let hir_db = db.as_hir_db();
                match variant.enum_(db).variants(hir_db).data(hir_db)[variant.idx].kind {
                    HirVariantKind::Unit => "unit variant",
                    HirVariantKind::Tuple(_) => "tuple variant",
                    HirVariantKind::Record(_) => "record variant",
                }
                .to_string()
            }
        }
    }

    pub fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        match self {
            RecordLike::Type(ty) => {
                let hir_db = db.as_hir_db();
                if ty.adt_ref(db).is_some() {
                    let AdtRef::Struct(s) = ty.adt_ref(db)?.data(db) else {
                        return None;
                    };
                    let name = s.name(hir_db).unwrap().data(hir_db);
                    let init_args = s.format_initializer_args(db.as_hir_db());
                    Some(format!("{}{}", name, init_args))
                } else {
                    None
                }
            }
            RecordLike::Variant(variant) => {
                let hir_db = db.as_hir_db();
                let expected_sub_pat = variant.enum_(db).variants(hir_db).data(hir_db)[variant.idx]
                    .format_initializer_args(hir_db);
                let path = variant.path.pretty_print(hir_db);
                Some(format!("{}{}", path, expected_sub_pat))
            }
        }
    }
}

impl<'db> From<TyId<'db>> for RecordLike<'db> {
    fn from(value: TyId<'db>) -> Self {
        Self::Type(value)
    }
}

impl<'db> From<ResolvedVariant<'db>> for RecordLike<'db> {
    fn from(value: ResolvedVariant<'db>) -> Self {
        Self::Variant(value)
    }
}
