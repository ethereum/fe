//! Pattern matching analysis for exhaustiveness and reachability checking
//! Based on "Warnings for pattern matching" by Luc Maranget

use crate::name_resolution::{resolve_path, PathRes, ResolvedVariant};
use crate::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
use crate::ty::AdtRef;
use crate::HirAnalysisDb;
use hir::hir_def::{scope_graph::ScopeId, Body as HirBody, LitKind, Partial, Pat as HirPat};
use common::indexmap::IndexSet;

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
    ) -> Self {
        let rows = patterns
            .iter()
            .enumerate()
            .map(|(i, pat)| {
                PatternRowVec::new(vec![SimplifiedPattern::from_hir_pat(
                    pat, db, body, scope, i,
                )])
            })
            .collect();
        Self { rows }
    }

    pub fn find_missing_patterns(&self, db: &'db dyn HirAnalysisDb) -> Option<Vec<SimplifiedPattern<'db>>> {
        if self.nrows() == 0 {
            let ty = TyId::never(db);
            return Some(vec![SimplifiedPattern::wildcard(None, ty)]);
        }
        if self.ncols() == 0 {
            return None;
        }

        let ty = self.first_column_ty();
        let sigma_set = self.sigma_set();

        // Check if we have any patterns that cover everything
        for row in &self.rows {
            if let Some(pattern) = row.head() {
                match &pattern.kind {
                    SimplifiedPatternKind::WildCard(_) => {
                        return None; // Wildcard covers everything
                    }
                    SimplifiedPatternKind::Constructor { kind: ConstructorKind::Tuple(_), fields } => {
                        // Tuple with all wildcards covers everything for tuples
                        if ty.is_tuple(db) && fields.iter().all(|f| matches!(f.kind, SimplifiedPatternKind::WildCard(_))) {
                            return None;
                        }
                    }
                    _ => {}
                }
            }
        }

        if sigma_set.is_complete(db, ty) || sigma_set.0.is_empty() {
            for ctor in sigma_set.into_iter() {
                match self.phi_specialize(db, ctor).find_missing_patterns(db) {
                    Some(vec) if vec.is_empty() => {
                        let pat = SimplifiedPattern::constructor_with_wildcards(db, ctor, ty);
                        return Some(vec![pat]);
                    }
                    Some(mut vec) => {
                        let field_num = ctor.arity(db);
                        if vec.len() >= field_num {
                            let rem = vec.split_off(field_num);
                            let pat = SimplifiedPattern::constructor(ctor, vec, ty);
                            let mut result = vec![pat];
                            result.extend_from_slice(&rem);
                            return Some(result);
                        }
                    }
                    None => {}
                }
            }
            None
        } else {
            // Check default case specialization
            let default_specialized = self.d_specialize(db);
            if default_specialized.rows.is_empty() {
                // No wildcard patterns - missing constructor coverage
                let complete_sigma = SigmaSet::complete_sigma(db, ty);
                let missing_ctors = complete_sigma.difference(&sigma_set);
                if missing_ctors.is_empty() {
                    None // All constructors covered
                } else {
                    let missing_patterns: Vec<_> = missing_ctors
                        .into_iter()
                        .map(|ctor| SimplifiedPattern::constructor_with_wildcards(db, ctor, ty))
                        .collect();
                    Some(missing_patterns)
                }
            } else {
                // We have wildcard patterns - check if they're exhaustive
                match default_specialized.find_missing_patterns(db) {
                    None => None, // Wildcards make it exhaustive
                    Some(missing) => {
                        if missing.is_empty() {
                            None // Complete
                        } else {
                            Some(missing) // Still missing some patterns
                        }
                    }
                }
            }
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
                .phi_specialize(db, *kind)
                .is_pattern_useful(db, &pat_vec.phi_specialize(db, *kind)[0]),

            SimplifiedPatternKind::Or(pats) => pats.iter().any(|pat| {
                self.is_pattern_useful(db, &PatternRowVec::new(vec![pat.clone()]))
            }),
        }
    }

    pub fn phi_specialize(&self, db: &'db dyn HirAnalysisDb, ctor: ConstructorKind<'db>) -> Self {
        let rows = self
            .rows
            .iter()
            .flat_map(|row| row.phi_specialize(db, ctor))
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

    pub fn head(&self) -> Option<&SimplifiedPattern<'db>> {
        self.inner.first()
    }

    pub fn phi_specialize(&self, db: &'db dyn HirAnalysisDb, ctor: ConstructorKind<'db>) -> Vec<Self> {
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
                if *kind == ctor || self.constructors_compatible(*kind, ctor, db) {
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
                    result.extend(tmp.phi_specialize(db, ctor));
                }
                result
            }
        }
    }

    fn constructors_compatible(&self, a: ConstructorKind<'db>, b: ConstructorKind<'db>, db: &'db dyn HirAnalysisDb) -> bool {
        match (a, b) {
            // Tuples are compatible if they have the same arity
            (ConstructorKind::Tuple(ty_a), ConstructorKind::Tuple(ty_b)) => {
                if ty_a.is_tuple(db) && ty_b.is_tuple(db) {
                    let (_, elems_a) = ty_a.decompose_ty_app(db);
                    let (_, elems_b) = ty_b.decompose_ty_app(db);
                    elems_a.len() == elems_b.len()
                } else {
                    false
                }
            }
            // Enum variants are compatible if they're the same variant
            (ConstructorKind::EnumVariant(var_a), ConstructorKind::EnumVariant(var_b)) => {
                var_a.variant.enum_ == var_b.variant.enum_ && var_a.variant.idx == var_b.variant.idx
            }
            // Literals are compatible if they're exactly the same
            (ConstructorKind::Literal(lit_a, _), ConstructorKind::Literal(lit_b, _)) => {
                lit_a == lit_b
            }
            _ => false,
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

    pub fn constructor(ctor: ConstructorKind<'db>, fields: Vec<SimplifiedPattern<'db>>, ty: TyId<'db>) -> Self {
        Self::new(SimplifiedPatternKind::Constructor { kind: ctor, fields }, ty)
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, SimplifiedPatternKind::WildCard(_))
    }

    pub fn constructor_with_wildcards(
        db: &'db dyn HirAnalysisDb,
        ctor: ConstructorKind<'db>,
        ty: TyId<'db>,
    ) -> Self {
        let fields = ctor
            .field_types(db)
            .into_iter()
            .map(|field_ty| SimplifiedPattern::wildcard(None, field_ty))
            .collect();
        Self::constructor(ctor, fields, ty)
    }

    pub fn or(patterns: Vec<SimplifiedPattern<'db>>) -> Self {
        let ty = patterns.first().map(|p| p.ty).unwrap_or_else(|| {
            panic!("Cannot create OR pattern with no alternatives")
        });
        Self::new(SimplifiedPatternKind::Or(patterns), ty)
    }

    pub fn from_hir_pat(
        pat: &HirPat<'db>,
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
    ) -> Self {
        match pat {
            HirPat::WildCard | HirPat::Rest => {
                let ty = TyId::never(db);
                SimplifiedPattern::wildcard(None, ty)
            }

            HirPat::Lit(lit_partial) => {
                if let Partial::Present(lit_kind) = lit_partial {
                    let ty = match lit_kind {
                        LitKind::Bool(_) => TyId::bool(db),
                        LitKind::Int(_) => TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256))),
                        LitKind::String(_) => {
                            TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::String)))
                        }
                    };
                    let ctor = ConstructorKind::Literal(lit_kind.clone(), ty);
                    SimplifiedPattern::constructor(ctor, vec![], ty)
                } else {
                    let ty = TyId::never(db);
                    SimplifiedPattern::wildcard(None, ty)
                }
            }

            HirPat::Path(path_partial, _) => {
                if let Partial::Present(path_id) = path_partial {
                    match resolve_path(db, *path_id, scope, true) {
                        Ok(PathRes::EnumVariant(variant)) => {
                            let ctor = ConstructorKind::EnumVariant(variant.clone());
                            SimplifiedPattern::constructor(ctor, vec![], variant.ty)
                        }
                        Ok(PathRes::Ty(ty_id)) => {
                            // Check if this is an imported enum variant
                            if path_id.is_bare_ident(db) {
                                if let Some(adt_def) = ty_id.adt_def(db) {
                                    if let AdtRef::Enum(enum_def) = adt_def.adt_ref(db) {
                                        if let Some(ident_value) = path_id.ident(db).to_opt() {
                                            // Look for variant with this name
                                            for (idx, variant_def) in enum_def.variants(db).data(db).iter().enumerate() {
                                                if variant_def.name.to_opt() == Some(ident_value) {
                                                    let resolved_variant = ResolvedVariant {
                                                        ty: ty_id,
                                                        variant: hir::hir_def::EnumVariant::new(enum_def, idx),
                                                        path: *path_id,
                                                    };
                                                    let ctor = ConstructorKind::EnumVariant(resolved_variant);
                                                    return SimplifiedPattern::constructor(ctor, vec![], ty_id);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            // Not an enum variant, treat as binding
                            let binding_name = path_id
                                .ident(db)
                                .to_opt()
                                .map(|id| (id.data(db).to_string(), arm_idx));
                            let ty = TyId::never(db);
                            SimplifiedPattern::wildcard(binding_name, ty)
                        }
                        _ => {
                            let binding_name = path_id
                                .ident(db)
                                .to_opt()
                                .map(|id| (id.data(db).to_string(), arm_idx));
                            let ty = TyId::never(db);
                            SimplifiedPattern::wildcard(binding_name, ty)
                        }
                    }
                } else {
                    let ty = TyId::never(db);
                    SimplifiedPattern::wildcard(None, ty)
                }
            }

            HirPat::Tuple(elements) => {
                let subpatterns: Vec<_> = elements
                    .iter()
                    .map(|pat_id| {
                        match pat_id.data(db, body) {
                            Partial::Present(pat_data) => {
                                SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx)
                            }
                            Partial::Absent => {
                                let ty = TyId::never(db);
                                SimplifiedPattern::wildcard(None, ty)
                            }
                        }
                    })
                    .collect();

                // For tuple patterns, we need to handle the case where the expected type 
                // might already be known, but for now use the subpattern types
                let element_types: Vec<_> = subpatterns.iter().map(|p| p.ty).collect();
                let tuple_ty = if element_types.iter().any(|ty| *ty == TyId::never(db)) {
                    // If we have any never types, create a generic tuple
                    TyId::tuple(db, elements.len())
                } else {
                    TyId::tuple_with_elems(db, &element_types)
                };
                let ctor = ConstructorKind::Tuple(tuple_ty);
                SimplifiedPattern::constructor(ctor, subpatterns, tuple_ty)
            }

            HirPat::PathTuple(path_partial, elements) => {
                if let Partial::Present(path_id) = path_partial {
                    match resolve_path(db, *path_id, scope, true) {
                        Ok(PathRes::EnumVariant(variant)) => {
                            let subpatterns: Vec<_> = elements
                                .iter()
                                .map(|pat_id| {
                                    match pat_id.data(db, body) {
                                        Partial::Present(pat_data) => {
                                            SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx)
                                        }
                                        Partial::Absent => {
                                            let ty = TyId::never(db);
                                            SimplifiedPattern::wildcard(None, ty)
                                        }
                                    }
                                })
                                .collect();
                            
                            let ctor = ConstructorKind::EnumVariant(variant.clone());
                            SimplifiedPattern::constructor(ctor, subpatterns, variant.ty)
                        }
                        _ => {
                            let ty = TyId::never(db);
                            SimplifiedPattern::wildcard(None, ty)
                        }
                    }
                } else {
                    let ty = TyId::never(db);
                    SimplifiedPattern::wildcard(None, ty)
                }
            }

            HirPat::Record(path_partial, fields) => {
                if let Partial::Present(path_id) = path_partial {
                    match resolve_path(db, *path_id, scope, true) {
                        Ok(PathRes::EnumVariant(variant)) => {
                            // This is a record enum variant pattern like E::Var { x, .. }
                            let subpatterns: Vec<_> = fields
                                .iter()
                                .map(|field_pat| {
                                    match field_pat.pat.data(db, body) {
                                        Partial::Present(pat_data) => {
                                            SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx)
                                        }
                                        Partial::Absent => {
                                            let ty = TyId::never(db);
                                            SimplifiedPattern::wildcard(None, ty)
                                        }
                                    }
                                })
                                .collect();
                            
                            let ctor = ConstructorKind::EnumVariant(variant.clone());
                            SimplifiedPattern::constructor(ctor, subpatterns, variant.ty)
                        }
                        Ok(PathRes::Ty(struct_ty)) => {
                            // This is a struct pattern
                            let subpatterns: Vec<_> = fields
                                .iter()
                                .map(|field_pat| {
                                    match field_pat.pat.data(db, body) {
                                        Partial::Present(pat_data) => {
                                            SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx)
                                        }
                                        Partial::Absent => {
                                            let ty = TyId::never(db);
                                            SimplifiedPattern::wildcard(None, ty)
                                        }
                                    }
                                })
                                .collect();
                            
                            // For structs, we'll use a tuple constructor for now (simplified)
                            let ctor = ConstructorKind::Tuple(struct_ty);
                            SimplifiedPattern::constructor(ctor, subpatterns, struct_ty)
                        }
                        _ => {
                            let ty = TyId::never(db);
                            SimplifiedPattern::wildcard(None, ty)
                        }
                    }
                } else {
                    let ty = TyId::never(db);
                    SimplifiedPattern::wildcard(None, ty)
                }
            }

            HirPat::Or(left, right) => {
                let left_pat = match left.data(db, body) {
                    Partial::Present(pat_data) => {
                        SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx)
                    }
                    Partial::Absent => {
                        let ty = TyId::never(db);
                        SimplifiedPattern::wildcard(None, ty)
                    }
                };
                let right_pat = match right.data(db, body) {
                    Partial::Present(pat_data) => {
                        SimplifiedPattern::from_hir_pat(pat_data, db, body, scope, arm_idx)
                    }
                    Partial::Absent => {
                        let ty = TyId::never(db);
                        SimplifiedPattern::wildcard(None, ty)
                    }
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

                SimplifiedPattern::or(patterns)
            }


        }
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
            Self::Constructor { kind, .. } => vec![*kind],
            Self::Or(pats) => {
                let mut ctors = vec![];
                for pat in pats {
                    ctors.extend_from_slice(&pat.kind.collect_ctors());
                }
                ctors
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum ConstructorKind<'db> {
    EnumVariant(ResolvedVariant<'db>),
    Tuple(TyId<'db>),
    Literal(LitKind<'db>, TyId<'db>),
}

impl<'db> ConstructorKind<'db> {
    pub fn field_types(&self, db: &'db dyn HirAnalysisDb) -> Vec<TyId<'db>> {
        match self {
            Self::EnumVariant(variant) => {
                match variant.variant.kind(db) {
                    hir::hir_def::VariantKind::Unit => vec![],
                    hir::hir_def::VariantKind::Tuple(fields) => {
                        vec![TyId::never(db); fields.data(db).len()]
                    }
                    hir::hir_def::VariantKind::Record(fields) => {
                        vec![TyId::never(db); fields.data(db).len()]
                    }
                }
            }
            Self::Tuple(ty) => {
                if ty.is_tuple(db) {
                    let (_, elems) = ty.decompose_ty_app(db);
                    elems.to_vec()
                } else {
                    vec![]
                }
            }
            Self::Literal(_, _) => vec![],
        }
    }

    pub fn arity(&self, db: &'db dyn HirAnalysisDb) -> usize {
        self.field_types(db).len()
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
            ctors.insert(ConstructorKind::Tuple(ty));
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
                    ctors.insert(ConstructorKind::EnumVariant(variant));
                }
            }
        }

        Self(ctors)
    }

    pub fn is_complete(&self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
        let complete_set = Self::complete_sigma(db, ty);
        self.0.len() == complete_set.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn difference(&self, other: &Self) -> Vec<ConstructorKind<'db>> {
        self.0.difference(&other.0).copied().collect()
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
) -> Result<(), Vec<String>> {
    let matrix = PatternMatrix::from_hir_patterns(db, patterns, body, scope);
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
) -> Vec<bool> {
    let matrix = PatternMatrix::from_hir_patterns(db, patterns, body, scope);
    (0..patterns.len())
        .map(|i| matrix.is_row_useful(db, i))
        .collect()
}

fn display_missing_pattern<'db>(pattern: &SimplifiedPattern<'db>) -> String {
    match &pattern.kind {
        SimplifiedPatternKind::WildCard(_) => "_".to_string(),
        
        SimplifiedPatternKind::Constructor { kind, fields, .. } => {
            match kind {
                ConstructorKind::EnumVariant(_variant) => {
                    // Try to get a reasonable name for the variant
                    if fields.is_empty() {
                        "variant".to_string()
                    } else {
                        "variant(..)".to_string()
                    }
                }
                ConstructorKind::Tuple(_) => {
                    if fields.is_empty() {
                        "()".to_string()
                    } else {
                        let parts: Vec<String> = fields
                            .iter()
                            .map(display_missing_pattern)
                            .collect();
                        format!("({})", parts.join(", "))
                    }
                }
                ConstructorKind::Literal(lit, _) => {
                    match lit {
                        LitKind::Bool(b) => b.to_string(),
                        LitKind::Int(_) => "int_literal".to_string(),
                        LitKind::String(_) => "string_literal".to_string(),
                    }
                }
            }
        }
        
        SimplifiedPatternKind::Or(patterns) => {
            let parts: Vec<String> = patterns
                .iter()
                .map(display_missing_pattern)
                .collect();
            parts.join(" | ")
        }
    }
}