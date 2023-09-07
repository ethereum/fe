use hir::hir_def::{Contract, Enum, Struct};

use crate::HirAnalysisDb;

#[salsa::interned]
pub struct Ty {
    data: TyData,
}

impl Ty {
    pub fn kind<'db>(self, db: &'db dyn HirAnalysisDb) -> &'db Kind {
        ty_kind(db, self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyData {
    /// Type variables.
    TyVar(TyVar),

    // Type application, e.g., `Option<i32>` is represented as `TApp(TyConst(Option),
    // TyConst(i32)`.
    TApp(Box<TyData>, Box<TyData>),

    TyConst(TyConst),
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
pub enum TyConst {
    Primitive(PrimTy),
    Abs,
    Adt(AdtTy),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtTy {
    id: AdtId,
    kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            TyData::TApp(lhs, _) => match lhs.kind(db) {
                Kind::Abs(_, rhs) => *rhs,
                _ => unreachable!(),
            },
            TyData::TyConst(ty_const) => ty_const.kind(db),
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

impl HasKind for AdtTy {
    fn kind(&self, _: &dyn HirAnalysisDb) -> Kind {
        self.kind.clone()
    }
}

#[salsa::tracked(return_ref)]
pub fn ty_kind(db: &dyn HirAnalysisDb, ty: Ty) -> Kind {
    ty.data(db).kind(db)
}
