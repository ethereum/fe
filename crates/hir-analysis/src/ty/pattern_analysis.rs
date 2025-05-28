//! Pattern matching analysis for exhaustiveness and reachability checking
//! Based on "Warnings for pattern matching" by Luc Maranget

use crate::name_resolution::{resolve_path, PathRes, ResolvedVariant};
use crate::ty::ty_check::{RecordLike, TupleLike};
use crate::ty::ty_def::TyId;
use crate::ty::AdtRef;
use crate::HirAnalysisDb;
use common::indexmap::IndexSet;
use hir::hir_def::{
    scope_graph::ScopeId, Body as HirBody, LitKind, Partial, Pat as HirPat, PatId, PathId,
    RecordPatField, VariantKind,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternMatrix<'db> {
    pub rows: Vec<PatternRowVec<'db>>,
}

impl<'db> PatternMatrix<'db> {
    pub fn new(rows: Vec<PatternRowVec<'db>>) -> Self {
        Self { rows }
    }

    pub fn from_hir_patterns(
        db: &'db dyn HirAnalysisDb,
        patterns: &[HirPat<'db>],
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        ty: TyId<'db>,
    ) -> Self {
        let rows = patterns
            .iter()
            .enumerate()
            .map(|(i, pat)| {
                PatternRowVec::new(vec![SimplifiedPattern::from_hir_pat(
                    pat, db, body, scope, i, ty,
                )])
            })
            .collect();
        Self { rows }
    }

    pub fn find_missing_patterns(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<Vec<SimplifiedPattern<'db>>> {
        if self.nrows() == 0 {
            // Non Exhaustive!
            return Some(vec![]);
        }
        if self.ncols() == 0 {
            return None;
        }

        let ty = self.first_column_ty();
        let sigma_set = self.sigma_set();

        if sigma_set.is_complete(db, ty) {
            for ctor in sigma_set.into_iter() {
                match self
                    .phi_specialize(db, ctor.clone())
                    .find_missing_patterns(db)
                {
                    Some(vec) if vec.is_empty() => {
                        let pat_kind = SimplifiedPatternKind::Constructor {
                            kind: ctor.clone(),
                            fields: vec![],
                        };
                        let pat = SimplifiedPattern::new(pat_kind, ty);

                        return Some(vec![pat]);
                    }

                    Some(mut vec) => {
                        let field_num = ctor.arity(db);
                        // For infinite types or mismatched patterns, generate wildcards
                        if vec.len() < field_num {
                            let field_types = ctor.field_types(db);
                            let mut fields = Vec::with_capacity(field_num);
                            for &field_ty in &field_types {
                                fields.push(SimplifiedPattern::wildcard(None, field_ty));
                            }
                            let pat_kind = SimplifiedPatternKind::Constructor {
                                kind: ctor.clone(),
                                fields,
                            };
                            // Use the constructor's type instead of the potentially wrong ty parameter
                            let constructor_ty = match ctor {
                                ConstructorKind::TupleLike(TupleLike::Type(tuple_ty)) => tuple_ty,
                                ConstructorKind::TupleLike(TupleLike::Variant(variant)) => {
                                    variant.ty
                                }
                                ConstructorKind::RecordLike(RecordLike::Type(record_ty)) => {
                                    record_ty
                                }
                                ConstructorKind::RecordLike(RecordLike::Variant(variant)) => {
                                    variant.ty
                                }
                                _ => ty, // For literal constructors, use the original ty
                            };
                            let pat = SimplifiedPattern::new(pat_kind, constructor_ty);
                            return Some(vec![pat]);
                        }
                        debug_assert!(vec.len() >= field_num);
                        let rem = vec.split_off(field_num);
                        let pat_kind = SimplifiedPatternKind::Constructor {
                            kind: ctor.clone(),
                            fields: vec,
                        };
                        let pat = SimplifiedPattern::new(pat_kind, ty);

                        let mut result = vec![pat];
                        result.extend_from_slice(&rem);
                        return Some(result);
                    }

                    None => {}
                }
            }

            None
        } else {
            self.d_specialize(db).find_missing_patterns(db).map(|vec| {
                let sigma_set = self.sigma_set();
                let kind = if sigma_set.is_empty() {
                    SimplifiedPatternKind::WildCard(None)
                } else {
                    let complete_sigma = SigmaSet::complete_sigma(db, ty);
                    if complete_sigma.is_empty() {
                        // Infinite type - can't enumerate all constructors, so use wildcard
                        SimplifiedPatternKind::WildCard(None)
                    } else {
                        let difference = complete_sigma.difference(&sigma_set);
                        SimplifiedPatternKind::Or(
                            difference
                                .into_iter()
                                .map(|ctor| {
                                    let kind =
                                        SimplifiedPatternKind::ctor_with_wild_card_fields(db, ctor);
                                    SimplifiedPattern::new(kind, ty)
                                })
                                .collect(),
                        )
                    }
                };

                let mut result = vec![SimplifiedPattern::new(kind, ty)];
                result.extend_from_slice(&vec);

                result
            })
        }
    }

    pub fn is_row_useful(&self, db: &'db dyn HirAnalysisDb, row: usize) -> bool {
        if row == 0 {
            return true;
        }

        let previous = PatternMatrix {
            rows: self.rows[0..row].to_vec(),
        };
        previous.is_pattern_useful(db, &self.rows[row])
    }

    fn is_pattern_useful(&self, db: &'db dyn HirAnalysisDb, pat_vec: &PatternRowVec<'db>) -> bool {
        if self.nrows() == 0 {
            return true;
        }
        if self.ncols() == 0 {
            return false;
        }

        let Some(head_pattern) = pat_vec.head() else {
            return false; // Empty pattern vector is not useful
        };

        match &head_pattern.kind {
            SimplifiedPatternKind::WildCard(_) => self
                .d_specialize(db)
                .is_pattern_useful(db, &pat_vec.d_specialize(db)[0]),

            SimplifiedPatternKind::Constructor { kind, .. } => self
                .phi_specialize(db, kind.clone())
                .is_pattern_useful(db, &pat_vec.phi_specialize(db, kind.clone())[0]),

            SimplifiedPatternKind::Or(pats) => pats
                .iter()
                .any(|pat| self.is_pattern_useful(db, &PatternRowVec::new(vec![pat.clone()]))),
        }
    }

    pub fn phi_specialize(&self, db: &'db dyn HirAnalysisDb, ctor: ConstructorKind<'db>) -> Self {
        let rows = self
            .rows
            .iter()
            .flat_map(|row| row.phi_specialize(db, ctor.clone()))
            .collect();
        PatternMatrix { rows }
    }

    pub fn d_specialize(&self, db: &'db dyn HirAnalysisDb) -> Self {
        let rows = self
            .rows
            .iter()
            .flat_map(|row| row.d_specialize(db))
            .collect();
        PatternMatrix { rows }
    }

    pub fn sigma_set(&self) -> SigmaSet<'db> {
        SigmaSet::from_rows(self.rows.iter(), 0)
    }

    pub fn first_column_ty(&self) -> TyId<'db> {
        self.rows[0].first_column_ty()
    }

    pub fn nrows(&self) -> usize {
        self.rows.len()
    }

    pub fn ncols(&self) -> usize {
        if self.rows.is_empty() {
            0
        } else {
            self.rows[0].len()
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternRowVec<'db> {
    pub inner: Vec<SimplifiedPattern<'db>>,
}

impl<'db> PatternRowVec<'db> {
    pub fn new(inner: Vec<SimplifiedPattern<'db>>) -> Self {
        Self { inner }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn head(&self) -> Option<&SimplifiedPattern<'db>> {
        self.inner.first()
    }

    pub fn phi_specialize(
        &self,
        db: &'db dyn HirAnalysisDb,
        ctor: ConstructorKind<'db>,
    ) -> Vec<Self> {
        if self.inner.is_empty() {
            return vec![];
        }

        let first_pat = &self.inner[0];
        let ctor_fields = ctor.field_types(db);

        match &first_pat.kind {
            SimplifiedPatternKind::WildCard(bind) => {
                let mut inner = Vec::with_capacity(self.inner.len() + ctor_fields.len() - 1);
                for field_ty in ctor_fields {
                    inner.push(SimplifiedPattern::wildcard(bind.clone(), field_ty));
                }
                inner.extend_from_slice(&self.inner[1..]);
                vec![Self::new(inner)]
            }

            SimplifiedPatternKind::Constructor { kind, fields } => {
                if *kind == ctor {
                    let mut inner = Vec::with_capacity(self.inner.len() + ctor_fields.len() - 1);
                    inner.extend_from_slice(fields);
                    inner.extend_from_slice(&self.inner[1..]);
                    vec![Self::new(inner)]
                } else {
                    vec![]
                }
            }

            SimplifiedPatternKind::Or(pats) => {
                let mut result = vec![];
                for pat in pats {
                    let mut tmp_inner = Vec::with_capacity(self.inner.len());
                    tmp_inner.push(pat.clone());
                    tmp_inner.extend_from_slice(&self.inner[1..]);
                    let tmp = PatternRowVec::new(tmp_inner);
                    result.extend(tmp.phi_specialize(db, ctor.clone()));
                }
                result
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn d_specialize(&self, db: &'db dyn HirAnalysisDb) -> Vec<Self> {
        if self.inner.is_empty() {
            return vec![];
        }

        let first_pat = &self.inner[0];
        match &first_pat.kind {
            SimplifiedPatternKind::WildCard(_) => {
                let inner = self.inner[1..].to_vec();
                vec![Self::new(inner)]
            }

            SimplifiedPatternKind::Constructor { .. } => vec![],

            SimplifiedPatternKind::Or(pats) => {
                let mut result = vec![];
                for pat in pats {
                    let mut tmp_inner = Vec::with_capacity(self.inner.len());
                    tmp_inner.push(pat.clone());
                    tmp_inner.extend_from_slice(&self.inner[1..]);
                    let tmp = PatternRowVec::new(tmp_inner);
                    result.extend(tmp.d_specialize(db));
                }
                result
            }
        }
    }

    fn first_column_ty(&self) -> TyId<'db> {
        self.inner[0].ty
    }

    fn collect_column_ctors(&self, column: usize) -> Vec<ConstructorKind<'db>> {
        if column >= self.inner.len() {
            return vec![];
        }
        self.inner[column].kind.collect_ctors()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimplifiedPattern<'db> {
    pub kind: SimplifiedPatternKind<'db>,
    pub ty: TyId<'db>,
}

impl<'db> SimplifiedPattern<'db> {
    pub fn new(kind: SimplifiedPatternKind<'db>, ty: TyId<'db>) -> Self {
        Self { kind, ty }
    }

    pub fn wildcard(bind: Option<(String, usize)>, ty: TyId<'db>) -> Self {
        Self::new(SimplifiedPatternKind::WildCard(bind), ty)
    }

    pub fn constructor(
        ctor: ConstructorKind<'db>,
        fields: Vec<SimplifiedPattern<'db>>,
        ty: TyId<'db>,
    ) -> Self {
        Self::new(
            SimplifiedPatternKind::Constructor { kind: ctor, fields },
            ty,
        )
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, SimplifiedPatternKind::WildCard(_))
    }

    pub fn from_hir_pat(
        pat: &HirPat<'db>,
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        match pat {
            HirPat::WildCard | HirPat::Rest => SimplifiedPattern::wildcard(None, expected_ty),

            HirPat::Lit(lit_partial) => Self::from_literal_pat(lit_partial, expected_ty),

            HirPat::Path(path_partial, _) => {
                Self::from_path_pat(path_partial, db, scope, arm_idx, expected_ty)
            }

            HirPat::Tuple(elements) => {
                Self::from_tuple_pat(elements, db, body, scope, arm_idx, expected_ty)
            }

            HirPat::PathTuple(path_partial, elements) => Self::from_path_tuple_pat(
                path_partial,
                elements,
                db,
                body,
                scope,
                arm_idx,
                expected_ty,
            ),

            HirPat::Record(path_partial, fields) => {
                Self::from_record_pat(path_partial, fields, db, body, scope, arm_idx, expected_ty)
            }

            HirPat::Or(left, right) => {
                Self::from_or_pat(left, right, db, body, scope, arm_idx, expected_ty)
            }
        }
    }

    /// Unified constructor resolution from path for enum variants
    fn resolve_path_constructor(
        path_partial: &Partial<PathId<'db>>,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        expected_ty: TyId<'db>,
    ) -> Option<(ConstructorKind<'db>, TyId<'db>)> {
        let Partial::Present(path_id) = path_partial else {
            return None;
        };

        match resolve_path(db, *path_id, scope, true) {
            Ok(PathRes::EnumVariant(variant)) => {
                let ctor = Self::variant_to_constructor(variant, db);
                Some((ctor, variant.ty))
            }
            Ok(PathRes::Ty(ty_id)) => {
                // Check if this is an imported enum variant
                if let Some(variant) =
                    Self::try_resolve_enum_variant_from_ty(path_id, ty_id, db, expected_ty)
                {
                    let ctor = Self::variant_to_constructor(variant, db);
                    Some((ctor, expected_ty))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Constructor resolution specifically for record patterns (handles both enum variants and struct types)
    fn resolve_record_constructor(
        path_partial: &Partial<PathId<'db>>,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<(ConstructorKind<'db>, TyId<'db>)> {
        let Partial::Present(path_id) = path_partial else {
            return None;
        };

        match resolve_path(db, *path_id, scope, true) {
            Ok(PathRes::EnumVariant(variant)) => {
                let ctor = ConstructorKind::RecordLike(RecordLike::Variant(variant));
                Some((ctor, variant.ty))
            }
            Ok(PathRes::Ty(struct_ty)) => {
                // Handle struct types in record patterns
                let ctor = ConstructorKind::RecordLike(RecordLike::Type(struct_ty));
                Some((ctor, struct_ty))
            }
            _ => None,
        }
    }

    /// Unified subpattern collection from pattern list
    fn collect_subpatterns_from_elements(
        elements: &[PatId],
        field_types: &[TyId<'db>],
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
    ) -> Vec<SimplifiedPattern<'db>> {
        elements
            .iter()
            .zip(field_types.iter())
            .map(|(pat_id, &field_ty)| match pat_id.data(db, body) {
                Partial::Present(pat_data) => {
                    SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx, field_ty)
                }
                Partial::Absent => SimplifiedPattern::wildcard(None, field_ty),
            })
            .collect()
    }

    fn from_literal_pat(lit_partial: &Partial<LitKind<'db>>, expected_ty: TyId<'db>) -> Self {
        if let Partial::Present(lit_kind) = lit_partial {
            let ctor = ConstructorKind::Literal(*lit_kind, expected_ty);
            SimplifiedPattern::constructor(ctor, vec![], expected_ty)
        } else {
            SimplifiedPattern::wildcard(None, expected_ty)
        }
    }

    fn from_path_pat(
        path_partial: &Partial<PathId<'db>>,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        if let Some((ctor, ctor_ty)) =
            Self::resolve_path_constructor(path_partial, db, scope, expected_ty)
        {
            SimplifiedPattern::constructor(ctor, vec![], ctor_ty)
        } else if let Partial::Present(path_id) = path_partial {
            let binding_name = path_id
                .ident(db)
                .to_opt()
                .map(|ident| (ident.data(db).to_string(), arm_idx));
            SimplifiedPattern::wildcard(binding_name, expected_ty)
        } else {
            SimplifiedPattern::wildcard(None, expected_ty)
        }
    }

    fn from_tuple_pat(
        elements: &[PatId],
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        let field_types = if expected_ty.is_tuple(db) {
            let (_, elems) = expected_ty.decompose_ty_app(db);
            elems.to_vec()
        } else {
            vec![expected_ty; elements.len()] // Fallback for non-tuple types
        };

        let subpatterns = Self::collect_subpatterns_from_elements(
            elements,
            &field_types,
            db,
            body,
            scope,
            arm_idx,
        );

        let ctor = ConstructorKind::TupleLike(TupleLike::Type(expected_ty));
        SimplifiedPattern::constructor(ctor, subpatterns, expected_ty)
    }

    fn from_path_tuple_pat(
        path_partial: &Partial<PathId<'db>>,
        elements: &[PatId],
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        if let Some((ctor, ctor_ty)) =
            Self::resolve_path_constructor(path_partial, db, scope, expected_ty)
        {
            let field_types = ctor.field_types(db);
            let subpatterns = Self::collect_subpatterns_from_elements(
                elements,
                &field_types,
                db,
                body,
                scope,
                arm_idx,
            );
            SimplifiedPattern::constructor(ctor, subpatterns, ctor_ty)
        } else {
            SimplifiedPattern::wildcard(None, expected_ty)
        }
    }

    fn from_record_pat(
        path_partial: &Partial<PathId<'db>>,
        fields: &[RecordPatField<'db>],
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        if let Some((ctor, ctor_ty)) = Self::resolve_record_constructor(path_partial, db, scope) {
            let subpatterns =
                Self::collect_record_subpatterns(&ctor, fields, db, body, scope, arm_idx);
            SimplifiedPattern::constructor(ctor, subpatterns, ctor_ty)
        } else {
            SimplifiedPattern::wildcard(None, expected_ty)
        }
    }

    fn from_or_pat(
        left: &PatId,
        right: &PatId,
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        let left_pat = match left.data(db, body) {
            Partial::Present(pat_data) => {
                SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx, expected_ty)
            }
            Partial::Absent => SimplifiedPattern::wildcard(None, expected_ty),
        };
        let right_pat = match right.data(db, body) {
            Partial::Present(pat_data) => {
                SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx, expected_ty)
            }
            Partial::Absent => SimplifiedPattern::wildcard(None, expected_ty),
        };

        let mut patterns = Vec::new();
        match left_pat.kind {
            SimplifiedPatternKind::Or(pats) => patterns.extend(pats),
            _ => patterns.push(left_pat),
        }
        match right_pat.kind {
            SimplifiedPatternKind::Or(pats) => patterns.extend(pats),
            _ => patterns.push(right_pat),
        }

        let ty = patterns
            .first()
            .map(|p| p.ty)
            .unwrap_or_else(|| panic!("Cannot create OR pattern with no alternatives"));
        SimplifiedPattern::new(SimplifiedPatternKind::Or(patterns), ty)
    }

    fn variant_to_constructor(
        variant: ResolvedVariant<'db>,
        db: &'db dyn HirAnalysisDb,
    ) -> ConstructorKind<'db> {
        match variant.variant.kind(db) {
            VariantKind::Unit | VariantKind::Tuple(_) => {
                ConstructorKind::TupleLike(TupleLike::Variant(variant))
            }
            VariantKind::Record(_) => ConstructorKind::RecordLike(RecordLike::Variant(variant)),
        }
    }

    fn try_resolve_enum_variant_from_ty(
        path_id: &PathId<'db>,
        ty_id: TyId<'db>,
        db: &'db dyn HirAnalysisDb,
        expected_ty: TyId<'db>,
    ) -> Option<ResolvedVariant<'db>> {
        if !path_id.is_bare_ident(db) {
            return None;
        }

        let adt_def = ty_id.adt_def(db)?;
        let AdtRef::Enum(enum_def) = adt_def.adt_ref(db) else {
            return None;
        };

        let ident_value = path_id.ident(db).to_opt()?;

        // Look for variant with this name
        for (idx, variant_def) in enum_def.variants(db).data(db).iter().enumerate() {
            if variant_def.name.to_opt() == Some(ident_value) {
                return Some(ResolvedVariant {
                    ty: expected_ty,
                    variant: hir::hir_def::EnumVariant {
                        enum_: enum_def,
                        idx: idx as u16,
                    },
                    path: *path_id,
                });
            }
        }

        None
    }

    fn collect_record_subpatterns(
        ctor: &ConstructorKind<'db>,
        fields: &[RecordPatField<'db>],
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
    ) -> Vec<SimplifiedPattern<'db>> {
        let field_types = ctor.field_types(db);
        fields
            .iter()
            .zip(field_types.iter())
            .map(
                |(field_pat, &field_ty)| match field_pat.pat.data(db, body) {
                    Partial::Present(pat_data) => SimplifiedPattern::from_hir_pat(
                        pat_data, db, body, scope, arm_idx, field_ty,
                    ),
                    Partial::Absent => SimplifiedPattern::wildcard(None, field_ty),
                },
            )
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimplifiedPatternKind<'db> {
    WildCard(Option<(String, usize)>),
    Constructor {
        kind: ConstructorKind<'db>,
        fields: Vec<SimplifiedPattern<'db>>,
    },
    Or(Vec<SimplifiedPattern<'db>>),
}

impl<'db> SimplifiedPatternKind<'db> {
    fn collect_ctors(&self) -> Vec<ConstructorKind<'db>> {
        match self {
            Self::WildCard(_) => vec![],
            Self::Constructor { kind, .. } => vec![kind.clone()],
            Self::Or(pats) => {
                let mut ctors = vec![];
                for pat in pats {
                    ctors.extend_from_slice(&pat.kind.collect_ctors());
                }
                ctors
            }
        }
    }

    pub fn ctor_with_wild_card_fields(
        db: &'db dyn HirAnalysisDb,
        kind: ConstructorKind<'db>,
    ) -> Self {
        let fields = kind
            .field_types(db)
            .into_iter()
            .map(|ty| SimplifiedPattern::wildcard(None, ty))
            .collect();
        Self::Constructor { kind, fields }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConstructorKind<'db> {
    TupleLike(TupleLike<'db>),
    RecordLike(RecordLike<'db>),
    Literal(LitKind<'db>, TyId<'db>),
}

impl<'db> ConstructorKind<'db> {
    pub fn field_types(&self, db: &'db dyn HirAnalysisDb) -> Vec<TyId<'db>> {
        match self {
            Self::TupleLike(tuple_like) => tuple_like.tuple_field_types(db),
            Self::RecordLike(record_like) => {
                // For record-like, get field types in order
                let labels = record_like.record_labels(db);
                labels
                    .into_iter()
                    .filter_map(|label| record_like.record_field_ty(db, label))
                    .collect()
            }
            Self::Literal(_, _) => vec![],
        }
    }

    pub fn arity(&self, db: &'db dyn HirAnalysisDb) -> usize {
        match self {
            Self::TupleLike(tuple_like) => tuple_like.tuple_arity(db),
            Self::RecordLike(record_like) => record_like.record_labels(db).len(),
            Self::Literal(_, _) => 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SigmaSet<'db>(IndexSet<ConstructorKind<'db>>);

impl<'db> SigmaSet<'db> {
    pub fn from_rows<'a>(rows: impl Iterator<Item = &'a PatternRowVec<'db>>, column: usize) -> Self
    where
        'db: 'a,
    {
        let mut ctor_set = IndexSet::new();
        for row in rows {
            for ctor in row.collect_column_ctors(column) {
                ctor_set.insert(ctor);
            }
        }
        Self(ctor_set)
    }

    pub fn complete_sigma(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        let mut ctors = IndexSet::new();

        if ty.is_bool(db) {
            ctors.insert(ConstructorKind::Literal(LitKind::Bool(true), ty));
            ctors.insert(ConstructorKind::Literal(LitKind::Bool(false), ty));
        } else if ty.is_tuple(db) {
            ctors.insert(ConstructorKind::TupleLike(TupleLike::Type(ty)));
        } else if let Some(adt_def) = ty.adt_def(db) {
            if let AdtRef::Enum(enum_def) = adt_def.adt_ref(db) {
                let variants_list = enum_def.variants(db);
                for (idx, _) in variants_list.data(db).iter().enumerate() {
                    let variant = ResolvedVariant {
                        ty,
                        variant: hir::hir_def::EnumVariant::new(enum_def, idx),
                        path: hir::hir_def::PathId::new(
                            db,
                            Partial::Present(hir::hir_def::IdentId::new(
                                db,
                                "_placeholder_".to_string(),
                            )),
                            hir::hir_def::GenericArgListId::none(db),
                            None,
                        ),
                    };
                    let ctor = match variant.variant.kind(db) {
                        VariantKind::Unit => {
                            ConstructorKind::TupleLike(TupleLike::Variant(variant))
                        }
                        VariantKind::Tuple(_) => {
                            ConstructorKind::TupleLike(TupleLike::Variant(variant))
                        }
                        VariantKind::Record(_) => {
                            ConstructorKind::RecordLike(RecordLike::Variant(variant))
                        }
                    };
                    ctors.insert(ctor);
                }
            } else if let AdtRef::Struct(_struct_def) = adt_def.adt_ref(db) {
                ctors.insert(ConstructorKind::TupleLike(TupleLike::Type(ty)));
            }
        }

        Self(ctors)
    }

    pub fn is_complete(&self, db: &'db dyn HirAnalysisDb, _ty: TyId<'db>) -> bool {
        match self.0.first() {
            Some(ctor) => {
                let expected = ctor_variant_num(db, ctor.clone());
                debug_assert!(self.0.len() <= expected);
                self.0.len() == expected
            }
            None => false,
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn difference(&self, other: &Self) -> Vec<ConstructorKind<'db>> {
        self.0.difference(&other.0).cloned().collect()
    }
}

impl<'db> IntoIterator for SigmaSet<'db> {
    type Item = ConstructorKind<'db>;
    type IntoIter = <IndexSet<ConstructorKind<'db>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

// Public API for pattern analysis
pub fn check_exhaustiveness<'db>(
    db: &'db dyn HirAnalysisDb,
    patterns: &[HirPat<'db>],
    body: HirBody<'db>,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Result<(), Vec<String>> {
    let matrix = PatternMatrix::from_hir_patterns(db, patterns, body, scope, ty);
    match matrix.find_missing_patterns(db) {
        Some(missing) => {
            let missing_strings = missing
                .iter()
                .map(|pat| display_missing_pattern(pat))
                .collect();
            Err(missing_strings)
        }
        None => Ok(()),
    }
}

pub fn check_reachability<'db>(
    db: &'db dyn HirAnalysisDb,
    patterns: &[HirPat<'db>],
    body: HirBody<'db>,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Vec<bool> {
    let matrix = PatternMatrix::from_hir_patterns(db, patterns, body, scope, ty);
    (0..patterns.len())
        .map(|i| matrix.is_row_useful(db, i))
        .collect()
}

fn ctor_variant_num(db: &dyn HirAnalysisDb, ctor: ConstructorKind<'_>) -> usize {
    match ctor {
        ConstructorKind::TupleLike(TupleLike::Variant(variant)) => {
            variant.variant.enum_.variants(db).data(db).len()
        }
        ConstructorKind::RecordLike(RecordLike::Variant(variant)) => {
            variant.variant.enum_.variants(db).data(db).len()
        }
        ConstructorKind::TupleLike(TupleLike::Type(_)) => 1,
        ConstructorKind::RecordLike(RecordLike::Type(_)) => 1,
        ConstructorKind::Literal(LitKind::Bool(_), _) => 2,
        ConstructorKind::Literal(LitKind::Int(_), _) => usize::MAX, // Infinite possibilities
        ConstructorKind::Literal(LitKind::String(_), _) => usize::MAX, // Infinite possibilities
    }
}

fn display_missing_pattern(pattern: &SimplifiedPattern<'_>) -> String {
    match &pattern.kind {
        SimplifiedPatternKind::WildCard(_) => "_".to_string(),

        SimplifiedPatternKind::Constructor { kind, fields, .. } => {
            match kind {
                ConstructorKind::TupleLike(TupleLike::Variant(_variant)) => {
                    // Try to get a reasonable name for the variant
                    if fields.is_empty() {
                        "variant".to_string()
                    } else {
                        "variant(..)".to_string()
                    }
                }
                ConstructorKind::RecordLike(RecordLike::Variant(_variant)) => {
                    // Try to get a reasonable name for the record variant
                    if fields.is_empty() {
                        "variant".to_string()
                    } else {
                        "variant { .. }".to_string()
                    }
                }
                ConstructorKind::TupleLike(TupleLike::Type(_)) => {
                    if fields.is_empty() {
                        "()".to_string()
                    } else {
                        let parts: Vec<String> =
                            fields.iter().map(display_missing_pattern).collect();
                        format!("({})", parts.join(", "))
                    }
                }
                ConstructorKind::RecordLike(RecordLike::Type(_)) => {
                    if fields.is_empty() {
                        "{}".to_string()
                    } else {
                        "{ .. }".to_string()
                    }
                }
                ConstructorKind::Literal(lit, _) => match lit {
                    LitKind::Bool(b) => b.to_string(),
                    LitKind::Int(_) => "int_literal".to_string(),
                    LitKind::String(_) => "string_literal".to_string(),
                },
            }
        }

        SimplifiedPatternKind::Or(patterns) => {
            if patterns.is_empty() {
                "_".to_string()
            } else {
                let parts: Vec<String> = patterns.iter().map(display_missing_pattern).collect();
                parts.join(" | ")
            }
        }
    }
}
