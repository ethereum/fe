//! Pattern matching analysis for exhaustiveness and reachability checking
//! Based on "Warnings for pattern matching" by Luc Maranget

use super::simplified_pattern::{
    ctor_variant_num, ConstructorKind, SimplifiedPattern, SimplifiedPatternKind,
};
use crate::ty::ty_def::TyId;
use crate::ty::AdtRef;
use crate::HirAnalysisDb;
use common::indexmap::IndexSet;
use hir::hir_def::{scope_graph::ScopeId, Body as HirBody, LitKind, Pat as HirPat};

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

    /// Find missing patterns that would make the matrix exhaustive
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
                            for &field_ty in field_types.iter() {
                                fields.push(SimplifiedPattern::wildcard(None, field_ty));
                            }
                            let pat_kind = SimplifiedPatternKind::Constructor {
                                kind: ctor.clone(),
                                fields,
                            };
                            // Use the constructor's type instead of the potentially wrong ty parameter
                            let constructor_ty = match ctor {
                                ConstructorKind::Type(ty) => ty,
                                ConstructorKind::Variant(_variant, ty) => ty,
                                ConstructorKind::Literal(_, ty) => ty,
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
            ctors.insert(ConstructorKind::Type(ty));
        } else if let Some(adt_def) = ty.adt_def(db) {
            if let AdtRef::Enum(enum_def) = adt_def.adt_ref(db) {
                let variants_list = enum_def.variants(db);
                for (idx, _) in variants_list.data(db).iter().enumerate() {
                    let variant = hir::hir_def::EnumVariant::new(enum_def, idx);
                    let ctor = ConstructorKind::Variant(variant, ty);
                    ctors.insert(ctor);
                }
            } else if let AdtRef::Struct(_struct_def) = adt_def.adt_ref(db) {
                ctors.insert(ConstructorKind::Type(ty));
            }
        }

        Self(ctors)
    }

    pub fn is_complete(&self, db: &'db dyn HirAnalysisDb, _ty: TyId<'db>) -> bool {
        match self.0.first() {
            Some(ctor) => {
                let expected = ctor_variant_num(db, ctor);
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
            let condensed_patterns = condense_missing_patterns(db, &missing, ty);
            Err(condensed_patterns)
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

/// Intelligently condense missing patterns for better diagnostics
fn condense_missing_patterns<'db>(
    db: &'db dyn HirAnalysisDb,
    missing: &[SimplifiedPattern<'db>],
    _ty: TyId<'db>,
) -> Vec<String> {
    if missing.is_empty() {
        return vec![];
    }

    let mut result = Vec::new();

    // Display missing patterns using type-based notation
    for pattern in missing.iter().take(3) {
        use crate::ty::simplified_pattern::display_missing_pattern;
        result.push(display_missing_pattern(db, pattern));
    }

    // If we had more patterns than we showed, indicate that
    if missing.len() > 3 {
        result.push(format!("... and {} more patterns", missing.len() - 3));
    }

    result
}
