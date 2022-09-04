//! This module includes utility structs and its functions for pattern matching
//! analysis. The algorithm here is based on [Warnings for pattern matching](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/warnings-for-pattern-matching/3165B75113781E2431E3856972940347)

use std::fmt::{self};

use fe_parser::{
    ast::{LiteralPattern, MatchArm, Pattern},
    node::Node,
};
use indexmap::IndexSet;
use smol_str::SmolStr;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternMatrix {
    rows: Vec<PatternRowVec>,
}

impl PatternMatrix {
    pub fn new(rows: Vec<PatternRowVec>) -> Self {
        Self { rows }
    }

    pub fn from_arms<'db>(
        scope: &'db BlockScope<'db, 'db>,
        arms: &[Node<MatchArm>],
        ty: TypeId,
    ) -> Self {
        let mut rows = Vec::with_capacity(arms.len());
        for (i, arm) in arms.iter().enumerate() {
            rows.push(PatternRowVec::new(vec![simplify_pattern(
                scope,
                &arm.kind.pat.kind,
                i,
                ty,
            )]));
        }

        Self { rows }
    }

    pub fn rows(&self) -> &[PatternRowVec] {
        &self.rows
    }

    pub fn into_rows(self) -> Vec<PatternRowVec> {
        self.rows
    }

    pub fn find_non_exhaustiveness(&self, db: &dyn AnalyzerDb) -> Option<Vec<SimplifiedPattern>> {
        if self.nrows() == 0 {
            // Non Exhaustive!
            return Some(vec![]);
        }
        if self.ncols() == 0 {
            return None;
        }

        let ty = self.first_column_ty();
        let sigma_set = self.sigma_set();
        if sigma_set.is_complete(db) {
            for ctor in sigma_set.into_iter() {
                match self.phi_specialize(db, ctor).find_non_exhaustiveness(db) {
                    Some(vec) if vec.is_empty() => {
                        let pat_kind = SimplifiedPatternKind::Constructor {
                            kind: ctor,
                            fields: vec![],
                        };
                        let pat = SimplifiedPattern::new(pat_kind, ty);

                        return Some(vec![pat]);
                    }

                    Some(mut vec) => {
                        let field_num = ctor.arity(db);
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
            self.d_specialize(db)
                .find_non_exhaustiveness(db)
                .map(|vec| {
                    let sigma_set = self.sigma_set();
                    let kind = if sigma_set.is_empty() {
                        SimplifiedPatternKind::WildCard(None)
                    } else {
                        let complete_sigma = SigmaSet::complete_sigma(db, ty);
                        SimplifiedPatternKind::Or(
                            complete_sigma
                                .difference(&sigma_set)
                                .into_iter()
                                .map(|ctor| {
                                    let kind =
                                        SimplifiedPatternKind::ctor_with_wild_card_fields(db, ctor);
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

    pub fn is_row_useful(&self, db: &dyn AnalyzerDb, row: usize) -> bool {
        debug_assert!(self.nrows() > row);

        Self {
            rows: self.rows[0..row].to_vec(),
        }
        .is_pattern_useful(db, &self.rows[row])
    }

    pub fn nrows(&self) -> usize {
        self.rows.len()
    }

    pub fn ncols(&self) -> usize {
        debug_assert_ne!(self.nrows(), 0);
        let ncols = self.rows[0].len();
        debug_assert!(self.rows.iter().all(|row| row.len() == ncols));
        ncols
    }

    pub fn swap_col(&mut self, col1: usize, col2: usize) {
        for row in &mut self.rows {
            row.swap(col1, col2);
        }
    }

    pub fn sigma_set(&self) -> SigmaSet {
        SigmaSet::from_rows(self.rows.iter(), 0)
    }

    pub fn phi_specialize(&self, db: &dyn AnalyzerDb, ctor: ConstructorKind) -> Self {
        let mut new_cols = Vec::new();
        for col in &self.rows {
            new_cols.extend_from_slice(&col.phi_specialize(db, ctor));
        }
        Self { rows: new_cols }
    }

    pub fn d_specialize(&self, db: &dyn AnalyzerDb) -> Self {
        let mut new_cols = Vec::new();
        for col in &self.rows {
            new_cols.extend_from_slice(&col.d_specialize(db));
        }
        Self { rows: new_cols }
    }

    fn first_column_ty(&self) -> TypeId {
        debug_assert_ne!(self.ncols(), 0);
        self.rows[0].first_column_ty()
    }

    fn is_pattern_useful(&self, db: &dyn AnalyzerDb, pat_vec: &PatternRowVec) -> bool {
        if self.nrows() == 0 {
            return true;
        }

        if self.ncols() == 0 {
            return false;
        }

        match &pat_vec.head().unwrap().kind {
            SimplifiedPatternKind::WildCard(_) => self
                .d_specialize(db)
                .is_pattern_useful(db, &pat_vec.d_specialize(db)[0]),

            SimplifiedPatternKind::Constructor { kind, .. } => self
                .phi_specialize(db, *kind)
                .is_pattern_useful(db, &pat_vec.phi_specialize(db, *kind)[0]),

            SimplifiedPatternKind::Or(pats) => {
                for pat in pats {
                    if self.is_pattern_useful(db, &PatternRowVec::new(vec![pat.clone()])) {
                        return true;
                    }
                }
                false
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimplifiedPattern {
    pub kind: SimplifiedPatternKind,
    pub ty: TypeId,
}

impl SimplifiedPattern {
    pub fn new(kind: SimplifiedPatternKind, ty: TypeId) -> Self {
        Self { kind, ty }
    }

    pub fn wildcard(bind: Option<(SmolStr, usize)>, ty: TypeId) -> Self {
        Self::new(SimplifiedPatternKind::WildCard(bind), ty)
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, SimplifiedPatternKind::WildCard(_))
    }
}

impl DisplayWithDb for SimplifiedPattern {
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            SimplifiedPatternKind::WildCard(None) => write!(f, "_"),
            SimplifiedPatternKind::WildCard(Some((name, _))) => write!(f, "{name}"),

            SimplifiedPatternKind::Constructor {
                kind: ConstructorKind::Enum(id),
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

            SimplifiedPatternKind::Constructor {
                kind: ConstructorKind::Tuple(_),
                fields,
            } => {
                write!(f, "(")?;
                let mut delim = "";
                for field in fields {
                    let displayable = field.display(db);
                    write!(f, "{delim}{displayable}")?;
                    delim = ", ";
                }
                write!(f, ")")
            }

            SimplifiedPatternKind::Constructor {
                kind: ConstructorKind::Literal((lit, _)),
                ..
            } => {
                write!(f, "{lit}")
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimplifiedPatternKind {
    WildCard(Option<(SmolStr, usize)>),
    Constructor {
        kind: ConstructorKind,
        fields: Vec<SimplifiedPattern>,
    },
    Or(Vec<SimplifiedPattern>),
}

impl SimplifiedPatternKind {
    pub fn collect_ctors(&self) -> Vec<ConstructorKind> {
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

    pub fn ctor_with_wild_card_fields(db: &dyn AnalyzerDb, kind: ConstructorKind) -> Self {
        let fields = kind
            .field_types(db)
            .into_iter()
            .map(|ty| SimplifiedPattern::wildcard(None, ty))
            .collect();
        Self::Constructor { kind, fields }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ConstructorKind {
    Enum(EnumVariantId),
    Tuple(TypeId),
    Literal((LiteralPattern, TypeId)),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LiteralConstructor {
    Bool(bool),
}

impl ConstructorKind {
    pub fn field_types(&self, db: &dyn AnalyzerDb) -> Vec<TypeId> {
        match self {
            Self::Enum(id) => match id.kind(db).unwrap() {
                EnumVariantKind::Unit => vec![],
                EnumVariantKind::Tuple(types) => types.to_vec(),
            },
            Self::Tuple(ty) => ty.tuple_elts(db),
            Self::Literal(_) => vec![],
        }
    }

    pub fn arity(&self, db: &dyn AnalyzerDb) -> usize {
        match self {
            Self::Enum(id) => match id.kind(db).unwrap() {
                EnumVariantKind::Unit => 0,
                EnumVariantKind::Tuple(types) => types.len(),
            },
            Self::Tuple(ty) => ty.tuple_elts(db).len(),
            Self::Literal(_) => 0,
        }
    }

    pub fn ty(&self, db: &dyn AnalyzerDb) -> TypeId {
        match self {
            Self::Enum(id) => id.parent(db).as_type(db),
            Self::Tuple(ty) => *ty,
            Self::Literal((_, ty)) => *ty,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SigmaSet(IndexSet<ConstructorKind>);

impl SigmaSet {
    pub fn from_rows<'a>(rows: impl Iterator<Item = &'a PatternRowVec>, column: usize) -> Self {
        let mut ctor_set = IndexSet::new();
        for row in rows {
            for ctor in row.collect_column_ctors(column) {
                ctor_set.insert(ctor);
            }
        }
        Self(ctor_set)
    }

    pub fn complete_sigma(db: &dyn AnalyzerDb, ty: TypeId) -> Self {
        match ty.typ(db) {
            Type::Enum(id) => Self(
                id.variants(db)
                    .values()
                    .map(|id| ConstructorKind::Enum(*id))
                    .collect(),
            ),
            _ => {
                unimplemented!()
            }
        }
    }

    pub fn is_complete(&self, db: &dyn AnalyzerDb) -> bool {
        match self.0.first() {
            Some(ctor) => {
                let expected = ctor_variant_num(db, *ctor);
                debug_assert!(self.len() <= expected);
                self.len() == expected
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

    pub fn iter(&self) -> impl Iterator<Item = &ConstructorKind> {
        self.0.iter()
    }

    pub fn difference(&self, other: &Self) -> Self {
        Self(self.0.difference(&other.0).cloned().collect())
    }
}

impl IntoIterator for SigmaSet {
    type Item = ConstructorKind;
    type IntoIter = <IndexSet<ConstructorKind> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternRowVec {
    pub inner: Vec<SimplifiedPattern>,
}

impl PatternRowVec {
    pub fn new(inner: Vec<SimplifiedPattern>) -> Self {
        Self { inner }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn pats(&self) -> &[SimplifiedPattern] {
        &self.inner
    }

    pub fn head(&self) -> Option<&SimplifiedPattern> {
        self.inner.first()
    }

    pub fn phi_specialize(&self, db: &dyn AnalyzerDb, ctor: ConstructorKind) -> Vec<Self> {
        debug_assert!(!self.inner.is_empty());

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
                    for v in tmp.phi_specialize(db, ctor) {
                        result.push(v);
                    }
                }
                result
            }
        }
    }

    pub fn swap(&mut self, a: usize, b: usize) {
        self.inner.swap(a, b);
    }

    pub fn d_specialize(&self, db: &dyn AnalyzerDb) -> Vec<Self> {
        debug_assert!(!self.inner.is_empty());

        let first_pat = &self.inner[0];
        match &first_pat.kind {
            SimplifiedPatternKind::WildCard(_) => {
                let inner = self.inner[1..].to_vec();
                vec![Self::new(inner)]
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
                    let tmp = PatternRowVec::new(tmp_inner);
                    for v in tmp.d_specialize(db) {
                        result.push(v);
                    }
                }
                result
            }
        }
    }

    pub fn collect_column_ctors(&self, column: usize) -> Vec<ConstructorKind> {
        debug_assert!(!self.inner.is_empty());

        let first_pat = &self.inner[column];
        first_pat.kind.collect_ctors()
    }

    fn first_column_ty(&self) -> TypeId {
        debug_assert!(!self.inner.is_empty());

        self.inner[0].ty
    }
}

fn ctor_variant_num(db: &dyn AnalyzerDb, ctor: ConstructorKind) -> usize {
    match ctor {
        ConstructorKind::Enum(variant) => {
            let enum_id = variant.parent(db);
            enum_id.variants(db).len()
        }
        ConstructorKind::Tuple(_) => 1,
        ConstructorKind::Literal((LiteralPattern::Bool(_), _)) => 2,
    }
}

fn simplify_pattern(
    scope: &BlockScope,
    pat: &Pattern,
    arm_idx: usize,
    ty: TypeId,
) -> SimplifiedPattern {
    let kind = match pat {
        Pattern::WildCard => SimplifiedPatternKind::WildCard(None),

        Pattern::Literal(lit) => {
            let ctor_kind = ConstructorKind::Literal((lit.kind, ty));
            SimplifiedPatternKind::Constructor {
                kind: ctor_kind,
                fields: vec![],
            }
        }

        Pattern::Tuple(elts) => {
            let ctor_kind = ConstructorKind::Tuple(ty);
            let elts_tys = ty.tuple_elts(scope.db());

            SimplifiedPatternKind::Constructor {
                kind: ctor_kind,
                fields: elts
                    .iter()
                    .zip(elts_tys.into_iter())
                    .map(|(pat, ty)| simplify_pattern(scope, &pat.kind, arm_idx, ty))
                    .collect(),
            }
        }

        Pattern::Path(path) => match scope.maybe_resolve_path(&path.kind) {
            Some(NamedThing::EnumVariant(variant)) => SimplifiedPatternKind::Constructor {
                kind: ConstructorKind::Enum(variant),
                fields: vec![],
            },
            _ => {
                debug_assert!(path.kind.segments.len() == 1);
                SimplifiedPatternKind::WildCard(Some((path.kind.segments[0].kind.clone(), arm_idx)))
            }
        },

        Pattern::PathTuple(path, elts) => {
            let variant = match scope.maybe_resolve_path(&path.kind).unwrap() {
                NamedThing::EnumVariant(variant) => variant,
                _ => unreachable!(),
            };
            let ctor_kind = ConstructorKind::Enum(variant);
            let fields = ctor_kind.field_types(scope.db());
            debug_assert_eq!(fields.len(), elts.len());

            SimplifiedPatternKind::Constructor {
                kind: ctor_kind,
                fields: elts
                    .iter()
                    .zip(fields.into_iter())
                    .map(|(pat, ty)| simplify_pattern(scope, &pat.kind, arm_idx, ty))
                    .collect(),
            }
        }

        Pattern::Or(pats) => SimplifiedPatternKind::Or(
            pats.iter()
                .map(|pat| simplify_pattern(scope, &pat.kind, arm_idx, ty))
                .collect(),
        ),
    };

    SimplifiedPattern::new(kind, ty)
}

impl TypeId {
    fn tuple_elts(self, db: &dyn AnalyzerDb) -> Vec<TypeId> {
        match self.typ(db) {
            Type::Tuple(tup) => tup.items.to_vec(),
            _ => unreachable!(),
        }
    }
}
