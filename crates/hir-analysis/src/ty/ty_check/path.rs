use std::collections::hash_map::Entry;

use hir::{
    hir_def::{
        scope_graph::ScopeId, FieldDefListId as HirFieldDefListId, FieldParent, IdentId,
        VariantKind as HirVariantKind,
    },
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use super::{env::LocalBinding, TyChecker};
use crate::{
    name_resolution::{diagnostics::NameResDiag, is_scope_visible_from, PathRes, ResolvedVariant},
    ty::{
        adt_def::{AdtDef, AdtField, AdtRef},
        diagnostics::{BodyDiag, FuncBodyDiag},
        ty_def::{InvalidCause, TyData, TyId},
    },
    HirAnalysisDb,
};

impl<'db> TyId<'db> {
    pub(crate) fn adt_ref(&self, db: &'db dyn HirAnalysisDb) -> Option<AdtRef<'db>> {
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

pub(super) struct RecordInitChecker<'tc, 'db, 'a, T> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    data: &'a T,
    already_given: FxHashMap<IdentId<'db>, DynLazySpan<'db>>,
    invalid_field_given: bool,
}

impl<'tc, 'db, 'a, T> RecordInitChecker<'tc, 'db, 'a, T>
where
    T: RecordLike<'db>,
{
    /// Create a new `RecordInitChecker` for the given record path.
    ///
    /// ## Panics
    /// Panics if the given `data` is not a record.
    pub(super) fn new(tc: &'tc mut TyChecker<'db>, data: &'a T) -> Self {
        assert!(data.is_record(tc.db));

        Self {
            tc,
            data,
            already_given: FxHashMap::default(),
            invalid_field_given: false,
        }
    }

    /// Feed a label to the checker.
    /// Returns the type of the field if the label is valid, otherwise returns
    /// an error.
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
            let diag = NameResDiag::Invisible(field_span, label, field_scope.name_span(self.tc.db));

            self.invalid_field_given = true;
            Err(diag.into())
        }
    }

    /// Finalize the checker and return an error if there are missing fields.
    pub(super) fn finalize(
        self,
        initializer_span: DynLazySpan<'db>,
        allow_missing_field: bool,
    ) -> Result<(), FuncBodyDiag<'db>> {
        if !self.invalid_field_given && !allow_missing_field {
            let expected_labels = self.data.record_labels(self.tc.db);
            let missing_fields: Vec<_> = expected_labels
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

pub(crate) trait RecordLike<'db> {
    fn is_record(&self, db: &'db dyn HirAnalysisDb) -> bool;

    fn record_field_ty(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<TyId<'db>>;

    fn record_field_list(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId<'db>, &'db AdtField<'db>)>;

    fn record_field_idx(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<usize> {
        let (hir_field_list, _) = self.record_field_list(db)?;
        hir_field_list.field_idx(db, name)
    }

    fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>>;

    fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> Vec<IdentId<'db>>;

    fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String>;

    fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String;
}

impl<'db> RecordLike<'db> for TyId<'db> {
    fn is_record(&self, db: &'db dyn HirAnalysisDb) -> bool {
        let Some(adt_ref) = self.adt_ref(db) else {
            return false;
        };

        matches!(adt_ref, AdtRef::Struct(..))
    }

    fn record_field_ty(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<TyId<'db>> {
        let args = self.generic_args(db);
        let (hir_field_list, field_list) = self.record_field_list(db)?;

        let field_idx = hir_field_list.field_idx(db, name)?;
        let field_ty = field_list.ty(db, field_idx).instantiate(db, args);

        if field_ty.is_star_kind(db) {
            field_ty
        } else {
            TyId::invalid(db, InvalidCause::Other)
        }
        .into()
    }

    fn record_field_list(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId<'db>, &'db AdtField<'db>)> {
        let adt_def = self.adt_def(db)?;
        match adt_def.adt_ref(db) {
            AdtRef::Struct(s) => (s.fields(db), &adt_def.fields(db)[0]).into(),
            AdtRef::Contract(c) => (c.fields(db), &adt_def.fields(db)[0]).into(),

            _ => None,
        }
    }

    fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>> {
        let field_idx = self.record_field_idx(db, name)?;
        let adt_ref = self.adt_ref(db)?;
        let parent = match adt_ref {
            AdtRef::Struct(s) => FieldParent::Struct(s),
            AdtRef::Contract(c) => FieldParent::Contract(c),
            _ => return None,
        };
        Some(ScopeId::Field(parent, field_idx))
    }

    fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> Vec<IdentId<'db>> {
        let Some(adt_ref) = self.adt_ref(db) else {
            return Vec::default();
        };
        let fields = match adt_ref {
            AdtRef::Struct(s) => s.fields(db),
            AdtRef::Contract(c) => c.fields(db),

            _ => return Vec::default(),
        };

        fields
            .data(db)
            .iter()
            .filter_map(|field| field.name.to_opt())
            .collect()
    }

    fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String {
        if let Some(adt_ref) = self.adt_ref(db) {
            adt_ref.kind_name().to_string()
        } else if self.is_func(db) {
            "fn".to_string()
        } else {
            self.pretty_print(db).to_string()
        }
    }

    fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        if self.adt_ref(db).is_some() {
            let AdtRef::Struct(s) = self.adt_ref(db)? else {
                return None;
            };

            let name = s.name(db).unwrap().data(db);
            let init_args = s.format_initializer_args(db);
            Some(format!("{}{}", name, init_args))
        } else {
            None
        }
    }
}

impl<'db> RecordLike<'db> for ResolvedVariant<'db> {
    fn is_record(&self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.variant_kind(db), HirVariantKind::Record(..))
    }

    fn record_field_ty(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<TyId<'db>> {
        let args = self.ty.generic_args(db);

        let (hir_field_list, field_list) = self.record_field_list(db)?;
        let field_idx = hir_field_list.field_idx(db, name)?;

        Some(field_list.ty(db, field_idx).instantiate(db, args))
    }

    fn record_field_list(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId<'db>, &'db AdtField<'db>)> {
        match self.variant_kind(db) {
            hir::hir_def::VariantKind::Record(fields) => {
                (fields, &self.ty.adt_def(db).unwrap().fields(db)[self.idx]).into()
            }

            _ => None,
        }
    }

    fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>> {
        let field_idx = self.record_field_idx(db, name)?;
        let parent = FieldParent::Variant(self.enum_(db), self.idx as u16);
        Some(ScopeId::Field(parent, field_idx))
    }

    fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> Vec<IdentId<'db>> {
        let fields = match self.variant_kind(db) {
            hir::hir_def::VariantKind::Record(fields) => fields,
            _ => return Vec::default(),
        };

        fields
            .data(db)
            .iter()
            .filter_map(|field| field.name.to_opt())
            .collect()
    }

    fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String {
        match self.enum_(db).variants(db).data(db)[self.idx].kind {
            HirVariantKind::Unit => "unit variant",
            HirVariantKind::Tuple(_) => "tuple variant",
            HirVariantKind::Record(_) => "record variant",
        }
        .to_string()
    }

    fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        let expected_sub_pat =
            self.enum_(db).variants(db).data(db)[self.idx].format_initializer_args(db);

        let path = self.path.pretty_print(db);
        Some(format!("{}{}", path, expected_sub_pat))
    }
}
