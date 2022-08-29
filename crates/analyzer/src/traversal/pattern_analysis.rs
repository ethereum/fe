//! This module includes utility structs and its functions for pattern matching analysis.
//! The algorithm here is based on [Warnings for pattern matching](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/warnings-for-pattern-matching/3165B75113781E2431E3856972940347)

use std::fmt::{self};

use fe_parser::{
    ast::{MatchArm, Pattern},
    node::Node,
};
use indexmap::IndexSet;

use crate::{
    context::{AnalyzerContext, NamedThing},
    display::{DisplayWithDb, Displayable},
    namespace::{
        items::{EnumVariantId, EnumVariantKind},
        scopes::BlockScope,
        types::{Type, TypeId},
    },
    AnalyzerDb,
};

#[derive(Clone)]
pub struct PatternMatrix<'db> {
    rows: Vec<PatternRowVec<'db>>,
    db: &'db dyn AnalyzerDb,
}

impl<'db> PatternMatrix<'db> {
    pub fn from_arms(
        scope: &'db BlockScope<'db, 'db>,
        arms: &[Node<MatchArm>],
        ty: TypeId,
    ) -> Self {
        let mut rows = Vec::with_capacity(arms.len());
        for arm in arms {
            rows.push(PatternRowVec::new(
                vec![simplify_pattern(scope, &arm.kind.pat.kind, ty)],
                scope.db(),
            ));
        }

        Self {
            rows,
            db: scope.db(),
        }
    }

    pub fn find_non_exhaustiveness(&self) -> Option<Vec<SimplifiedPattern>> {
        if self.nrows() == 0 {
            // Non Exhaustive!
            return Some(vec![]);
        }
        if self.ncols() == 0 {
            return None;
        }

        let ty = self.first_column_ty();
        if self.is_complete() {
            for ctor in self.sigma_set() {
                match self.phi_specialize(ctor).find_non_exhaustiveness() {
                    Some(vec) if vec.is_empty() => {
                        let pat_kind = SimplifiedPatternKind::Constructor {
                            kind: ctor,
                            fields: vec![],
                        };
                        let pat = SimplifiedPattern::new(pat_kind, ty);

                        return Some(vec![pat]);
                    }

                    Some(mut vec) => {
                        let field_num = ctor.field_len(self.db);
                        debug_assert!(vec.len() >= field_num);
                        let rem = vec.split_off(field_num);
                        let pat_kind = SimplifiedPatternKind::Constructor {
                            kind: ctor,
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
            self.d_specialize().find_non_exhaustiveness().map(|vec| {
                let sigma_set = self.sigma_set();
                let kind = if sigma_set.is_empty() {
                    SimplifiedPatternKind::WildCard
                } else {
                    let complete_sigma = all_ctors(self.db, ty);
                    SimplifiedPatternKind::Or(
                        complete_sigma
                            .difference(&sigma_set)
                            .into_iter()
                            .map(|ctor| {
                                let kind = SimplifiedPatternKind::ctor_with_wild_card_fields(
                                    self.db, *ctor,
                                );
                                SimplifiedPattern::new(kind, ty)
                            })
                            .collect(),
                    )
                };

                let mut result = vec![SimplifiedPattern::new(kind, ty)];
                result.extend_from_slice(&vec);

                result
            })
        }
    }

    pub fn is_row_useful(&self, row: usize) -> bool {
        debug_assert!(self.nrows() > row);

        Self {
            rows: self.rows[0..row].to_vec(),
            db: self.db,
        }
        .is_pattern_useful(&self.rows[row])
    }

    pub fn nrows(&self) -> usize {
        self.rows.len()
    }

    pub fn ncols(&self) -> usize {
        debug_assert_ne!(self.nrows(), 0);
        let ncols = self.rows[0].size();
        debug_assert!(self.rows.iter().all(|row| row.size() == ncols));
        ncols
    }

    pub fn is_complete(&self) -> bool {
        let sigma_set = self.sigma_set();

        match sigma_set.first().map(|ctor| ctor.ty(self.db)) {
            Some(ty) => {
                let expected = ctor_variant_num(self.db, ty);
                debug_assert!(sigma_set.len() <= expected);
                sigma_set.len() == expected
            }
            None => false,
        }
    }

    pub fn sigma_set(&self) -> IndexSet<ConstructorKind> {
        let mut ctor_set = IndexSet::new();
        for col in &self.rows {
            for ctor in col.collect_first_elem_ctors() {
                ctor_set.insert(ctor);
            }
        }
        ctor_set
    }

    pub fn phi_specialize(&self, ctor: ConstructorKind) -> Self {
        let mut new_cols = Vec::new();
        for col in &self.rows {
            new_cols.extend_from_slice(&col.phi_specialize(ctor));
        }
        Self {
            rows: new_cols,
            db: self.db,
        }
    }

    pub fn d_specialize(&self) -> Self {
        let mut new_cols = Vec::new();
        for col in &self.rows {
            new_cols.extend_from_slice(&col.d_specialize());
        }
        Self {
            rows: new_cols,
            db: self.db,
        }
    }

    fn first_column_ty(&self) -> TypeId {
        debug_assert_ne!(self.ncols(), 0);
        self.rows[0].first_column_ty()
    }

    fn is_pattern_useful(&self, pat_vec: &PatternRowVec) -> bool {
        if self.nrows() == 0 {
            return true;
        }

        if self.ncols() == 0 {
            return false;
        }

        match &pat_vec.first_pat().unwrap().kind {
            SimplifiedPatternKind::WildCard => self
                .d_specialize()
                .is_pattern_useful(&pat_vec.d_specialize()[0]),

            SimplifiedPatternKind::Constructor { kind, .. } => self
                .phi_specialize(*kind)
                .is_pattern_useful(&pat_vec.phi_specialize(*kind)[0]),

            SimplifiedPatternKind::Or(pats) => {
                for pat in pats {
                    if self.is_pattern_useful(&PatternRowVec::new(vec![pat.clone()], self.db)) {
                        return true;
                    }
                }
                false
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct SimplifiedPattern {
    kind: SimplifiedPatternKind,
    ty: TypeId,
}

impl SimplifiedPattern {
    pub fn new(kind: SimplifiedPatternKind, ty: TypeId) -> Self {
        Self { kind, ty }
    }

    pub fn wild_card(ty: TypeId) -> Self {
        Self::new(SimplifiedPatternKind::WildCard, ty)
    }
}

impl DisplayWithDb for SimplifiedPattern {
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            SimplifiedPatternKind::WildCard => write!(f, "_"),
            SimplifiedPatternKind::Constructor {
                kind: ConstructorKind::Variant(id),
                fields,
            } => {
                let ctor_name = id.name_with_parent(db);
                write!(f, "{ctor_name}")?;
                if !id.kind(db).unwrap().is_unit() {
                    write!(f, "(")?;
                    let mut delim = "";
                    for field in fields {
                        let displayable = field.display(db);
                        write!(f, "{delim}{displayable}")?;
                        delim = ", ";
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            }
            SimplifiedPatternKind::Or(pats) => {
                let mut delim = "";
                for pat in pats {
                    let pat = pat.display(db);
                    write!(f, "{delim}{pat}")?;
                    delim = "| ";
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum SimplifiedPatternKind {
    WildCard,
    Constructor {
        kind: ConstructorKind,
        fields: Vec<SimplifiedPattern>,
    },
    Or(Vec<SimplifiedPattern>),
}

impl SimplifiedPatternKind {
    pub fn collect_ctors(&self) -> Vec<ConstructorKind> {
        match self {
            Self::WildCard => vec![],
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

    pub fn ctor_with_wild_card_fields(db: &dyn AnalyzerDb, kind: ConstructorKind) -> Self {
        let fields = kind
            .field_types(db)
            .into_iter()
            .map(SimplifiedPattern::wild_card)
            .collect();
        Self::Constructor { kind, fields }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ConstructorKind {
    Variant(EnumVariantId),
}

impl ConstructorKind {
    pub fn field_types(&self, db: &dyn AnalyzerDb) -> Vec<TypeId> {
        match self {
            Self::Variant(id) => match id.kind(db).unwrap() {
                EnumVariantKind::Unit => vec![],
                EnumVariantKind::Tuple(types) => types.to_vec(),
            },
        }
    }

    pub fn field_len(&self, db: &dyn AnalyzerDb) -> usize {
        match self {
            Self::Variant(id) => match id.kind(db).unwrap() {
                EnumVariantKind::Unit => 0,
                EnumVariantKind::Tuple(types) => types.len(),
            },
        }
    }

    pub fn ty(&self, db: &dyn AnalyzerDb) -> TypeId {
        match self {
            Self::Variant(id) => id.parent(db).as_type(db),
        }
    }
}

#[derive(Clone)]
struct PatternRowVec<'db> {
    inner: Vec<SimplifiedPattern>,
    db: &'db dyn AnalyzerDb,
}

impl<'db> PatternRowVec<'db> {
    fn new(inner: Vec<SimplifiedPattern>, db: &'db dyn AnalyzerDb) -> Self {
        Self { inner, db }
    }

    fn size(&self) -> usize {
        self.inner.len()
    }

    fn first_pat(&self) -> Option<&SimplifiedPattern> {
        self.inner.first()
    }

    fn phi_specialize(&self, ctor: ConstructorKind) -> Vec<Self> {
        debug_assert!(!self.inner.is_empty());

        let first_pat = &self.inner[0];
        let ctor_fields = ctor.field_types(self.db);
        match &first_pat.kind {
            SimplifiedPatternKind::WildCard => {
                let mut inner = Vec::with_capacity(self.inner.len() + ctor_fields.len() - 1);
                for field_ty in ctor_fields {
                    inner.push(SimplifiedPattern::wild_card(field_ty));
                }
                inner.extend_from_slice(&self.inner[1..]);
                vec![Self::new(inner, self.db)]
            }

            SimplifiedPatternKind::Constructor { kind, fields } => {
                if *kind == ctor {
                    let mut inner = Vec::with_capacity(self.inner.len() + ctor_fields.len() - 1);
                    inner.extend_from_slice(fields);
                    inner.extend_from_slice(&self.inner[1..]);
                    vec![Self::new(inner, self.db)]
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
                    let tmp = PatternRowVec::new(tmp_inner, self.db);
                    for v in tmp.phi_specialize(ctor) {
                        result.push(v);
                    }
                }
                result
            }
        }
    }

    fn d_specialize(&self) -> Vec<Self> {
        debug_assert!(!self.inner.is_empty());

        let first_pat = &self.inner[0];
        match &first_pat.kind {
            SimplifiedPatternKind::WildCard => {
                let inner = self.inner[1..].to_vec();
                vec![Self::new(inner, self.db)]
            }

            SimplifiedPatternKind::Constructor { .. } => {
                vec![]
            }

            SimplifiedPatternKind::Or(pats) => {
                let mut result = vec![];
                for pat in pats {
                    let mut tmp_inner = Vec::with_capacity(self.inner.len());
                    tmp_inner.push(pat.clone());
                    tmp_inner.extend_from_slice(&self.inner[1..]);
                    let tmp = PatternRowVec::new(tmp_inner, self.db);
                    for v in tmp.d_specialize() {
                        result.push(v);
                    }
                }
                result
            }
        }
    }

    fn collect_first_elem_ctors(&self) -> Vec<ConstructorKind> {
        debug_assert!(!self.inner.is_empty());

        let first_pat = &self.inner[0];
        first_pat.kind.collect_ctors()
    }

    fn first_column_ty(&self) -> TypeId {
        debug_assert!(!self.inner.is_empty());

        self.inner[0].ty
    }
}

fn ctor_variant_num(db: &dyn AnalyzerDb, ty: TypeId) -> usize {
    match ty.typ(db) {
        Type::Enum(id) => id.variants(db).len(),
        _ => {
            unimplemented!()
        }
    }
}

fn all_ctors(db: &dyn AnalyzerDb, ty: TypeId) -> IndexSet<ConstructorKind> {
    match ty.typ(db) {
        Type::Enum(id) => id
            .variants(db)
            .values()
            .map(|id| ConstructorKind::Variant(*id))
            .collect(),
        _ => {
            unimplemented!()
        }
    }
}

fn simplify_pattern(scope: &BlockScope, pat: &Pattern, ty: TypeId) -> SimplifiedPattern {
    let kind = match pat {
        Pattern::WildCard => SimplifiedPatternKind::WildCard,

        Pattern::Path(path) => match scope.maybe_resolve_path(&path.kind) {
            Some(NamedThing::EnumVariant(variant)) => SimplifiedPatternKind::Constructor {
                kind: ConstructorKind::Variant(variant),
                fields: vec![],
            },
            _ => SimplifiedPatternKind::WildCard,
        },

        Pattern::PathTuple(path, elts) => {
            let variant = match scope.maybe_resolve_path(&path.kind).unwrap() {
                NamedThing::EnumVariant(variant) => variant,
                _ => unreachable!(),
            };
            let ctor_kind = ConstructorKind::Variant(variant);
            let fields = ctor_kind.field_types(scope.db());
            debug_assert_eq!(fields.len(), elts.len());

            SimplifiedPatternKind::Constructor {
                kind: ctor_kind,
                fields: elts
                    .iter()
                    .zip(fields.into_iter())
                    .map(|(pat, ty)| simplify_pattern(scope, &pat.kind, ty))
                    .collect(),
            }
        }

        Pattern::Or(pats) => SimplifiedPatternKind::Or(
            pats.iter()
                .map(|pat| simplify_pattern(scope, &pat.kind, ty))
                .collect(),
        ),
    };

    SimplifiedPattern::new(kind, ty)
}
