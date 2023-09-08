use hir::hir_def::{Contract, Enum, Struct};

use crate::HirAnalysisDb;

use super::trait_::Predicate;

#[salsa::interned]
pub struct TyId {
    data: TyData,
}

impl TyId {
    pub fn kind<'db>(self, db: &'db dyn HirAnalysisDb) -> &'db Kind {
        ty_kind(db, self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyData {
    /// Type variable.
    TyVar(TyVar),

    /// Type Parameter.
    TyParam(TyParam),

    /// Dependent type, e.g., [T; N: usize]
    DependentTy(TyVar, TyId),

    // Type application, e.g., `Option<i32>` is represented as `TApp(TyConst(Option),
    // TyConst(i32)`.
    TApp(Box<TyId>, Box<TyId>),

    TyConst(TyConst),

    Invalid,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// Represents monotypes, `*`.
    Star,

    /// Represents higher order types.
    /// e.g.,
    /// `* -> *` or `(* -> *) -> *`
    Abs(Box<Kind>, Box<Kind>),
}

impl Kind {
    fn abs(lhs: Kind, rhs: Kind) -> Self {
        Kind::Abs(Box::new(lhs), Box::new(rhs))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar {
    id: u32,
    kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParam {
    name: usize,
    kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyConst {
    Primitive(PrimTy),
    Abs,
    Adt(AdtDef),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimTy {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    String,
    Array,
    Tuple(usize),
    Ptr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum AdtId {
    Enum(Enum),
    Struct(Struct),
    Contract(Contract),
}

pub(super) trait HasKind {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind;
}

impl HasKind for TyData {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyData::TyVar(ty_var) => ty_var.kind(db),
            TyData::TyParam(ty_param) => ty_param.kind.clone(),
            TyData::DependentTy(_, _) => Kind::Star,
            TyData::TApp(lhs, _) => match lhs.kind(db) {
                Kind::Abs(_, rhs) => *rhs.clone(),
                _ => unreachable!(),
            },
            TyData::TyConst(ty_const) => ty_const.kind(db),
            TyData::Invalid => Kind::Star,
        }
    }
}

impl HasKind for TyVar {
    fn kind(&self, _db: &dyn HirAnalysisDb) -> Kind {
        self.kind.clone()
    }
}

impl HasKind for TyConst {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyConst::Primitive(prim) => prim.kind(db),
            TyConst::Abs => Kind::abs(Kind::Star, Kind::abs(Kind::Star, Kind::Star)),
            TyConst::Adt(adt) => adt.kind(db),
        }
    }
}

impl HasKind for PrimTy {
    fn kind(&self, _: &dyn HirAnalysisDb) -> Kind {
        match self {
            Self::Array => (0..2)
                .into_iter()
                .fold(Kind::Star, |acc, _| Kind::abs(Kind::Star, acc)),
            Self::Tuple(n) => (0..*n)
                .into_iter()
                .fold(Kind::Star, |acc, _| Kind::abs(Kind::Star, acc)),
            Self::Ptr => Kind::abs(Kind::Star, Kind::Star),
            _ => Kind::Star,
        }
    }
}

impl HasKind for AdtDef {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::abs(ty_kind(db, *param).clone(), kind);
        }

        kind
    }
}

#[salsa::tracked(return_ref)]
pub fn ty_kind(db: &dyn HirAnalysisDb, ty: TyId) -> Kind {
    ty.data(db).kind(db)
}

#[salsa::tracked]
pub struct AdtDef {
    pub adt: AdtId,
    #[return_ref]
    pub params: Vec<TyId>,
    #[return_ref]
    predicates: Vec<Predicate>,
}
