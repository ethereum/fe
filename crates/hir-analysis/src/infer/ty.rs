use hir::hir_def::{Contract, Enum, IdentId, Struct};

use crate::HirAnalysisDb;

#[salsa::interned]
pub struct TyId {
    data: TyData,
}

impl TyId {
    pub fn kind<'db>(self, db: &'db dyn HirAnalysisDb) -> &'db Kind {
        ty_kind(db, self)
    }
}

#[salsa::tracked]
pub struct AdtDef {
    pub adt: AdtId,
    #[return_ref]
    pub params: Vec<TyId>,
    pub variants: Vec<AdtVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtVariant {
    ty: TyId,
    /// Fields of the variant.
    /// If the parent is an struct, the length of the vector is always 1.
    fields: Vec<TyId>,
}

#[salsa::tracked(return_ref)]
pub fn ty_kind(db: &dyn HirAnalysisDb, ty: TyId) -> Kind {
    ty.data(db).kind(db)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyData {
    /// Type variable.
    TyVar(TyVar),

    /// Type Parameter.
    TyParam(TyParam),

    TyAll(TyAll),

    // Type application,
    // e.g.,`TApp(TyConst(Option), TyConst(i32))`.
    TApp(TyId, TyId),

    TyConst(TyConst),

    // TODO: DependentTy,
    // TermTy(TermTy)
    // DependentTyAll(TyAll, TyConst),
    // DependentTyParam(TyVar, TyConst),

    // Invalid type which means the type is not defined.
    // This type can be unified with any other types.
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

    /// `Any` kind is set to the type iff the type is `Invalid`.
    Any,
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
    name: IdentId,
    kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyAll {
    index: usize,
    kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TAll {
    idx: usize,
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
            TyData::TyAll(ty_all) => ty_all.kind.clone(),
            TyData::TApp(lhs, rhs) => match lhs.kind(db) {
                Kind::Abs(k_arg, k_ret) => {
                    debug_assert!(rhs.kind(db) == k_arg.as_ref());
                    k_ret.as_ref().clone()
                }
                _ => unreachable!(),
            },
            TyData::TyConst(ty_const) => ty_const.kind(db),
            TyData::Invalid => Kind::Any,
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
