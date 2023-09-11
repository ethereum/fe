use std::fmt;

use hir::{
    hir_def::{
        prim_ty::{IntTy as HirIntTy, PrimTy as HirPrimTy, UintTy as HirUintTy},
        scope_graph::ScopeId,
        Contract, Enum, IdentId, ItemKind, Partial, Struct, TypeId as HirTyId,
    },
    span::DynLazySpan,
};

use crate::HirAnalysisDb;

#[salsa::interned]
pub struct TyId {
    pub data: TyData,
}

impl TyId {
    pub fn kind<'db>(self, db: &'db dyn HirAnalysisDb) -> &'db Kind {
        ty_kind(db, self)
    }

    pub fn is_invalid(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::Invalid(_) => true,
            _ => false,
        }
    }

    pub fn invalid_cause(self, db: &dyn HirAnalysisDb) -> Option<InvalidCause> {
        match self.data(db) {
            TyData::Invalid(cause) => Some(cause),
            _ => None,
        }
    }

    pub(super) fn ptr(db: &dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::Ptr)))
    }

    pub(super) fn tuple(db: &dyn HirAnalysisDb, n: usize) -> Self {
        Self::new(db, TyData::TyCon(TyConcrete::tuple(n)))
    }

    pub(super) fn adt(db: &dyn HirAnalysisDb, adt: AdtDef) -> Self {
        Self::new(db, TyData::TyCon(TyConcrete::Adt(adt)))
    }

    /// Perform type level application.
    /// If the kind is mismatched, return `TyData::Invalid`.
    pub(super) fn apply(db: &dyn HirAnalysisDb, abs: Self, arg: Self) -> TyId {
        if abs.is_invalid(db) || arg.is_invalid(db) {
            return TyId::invalid(db, InvalidCause::Other);
        }

        let k_ty = abs.kind(db);
        let k_arg = arg.kind(db);

        if k_ty.is_applicable(&k_arg) {
            Self::new(db, TyData::TyApp(abs, arg))
        } else {
            Self::invalid(db, InvalidCause::KindMismatch { abs, arg })
        }
    }

    pub(super) fn invalid(db: &dyn HirAnalysisDb, cause: InvalidCause) -> Self {
        Self::new(db, TyData::Invalid(cause))
    }

    pub(super) fn from_hir_prim_ty(db: &dyn HirAnalysisDb, hir_prim: HirPrimTy) -> Self {
        match hir_prim {
            HirPrimTy::Bool => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::Bool))),

            HirPrimTy::Int(int_ty) => match int_ty {
                HirIntTy::I8 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::I8))),
                HirIntTy::I16 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::I16))),
                HirIntTy::I32 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::I32))),
                HirIntTy::I64 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::I64))),
                HirIntTy::I128 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::I128))),
                HirIntTy::I256 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::I256))),
            },

            HirPrimTy::Uint(uint_ty) => match uint_ty {
                HirUintTy::U8 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::U8))),
                HirUintTy::U16 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::U16))),
                HirUintTy::U32 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::U32))),
                HirUintTy::U64 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::U64))),
                HirUintTy::U128 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::U128))),
                HirUintTy::U256 => Self::new(db, TyData::TyCon(TyConcrete::Prim(PrimTy::U256))),
            },
        }
    }

    /// Returns true if the type is declared as a monotype or fully applied
    /// type.
    pub(super) fn is_mono_type(self, db: &dyn HirAnalysisDb) -> bool {
        match self.kind(db) {
            Kind::Abs(_, _) => false,
            _ => true,
        }
    }
}

#[salsa::tracked]
pub struct AdtDef {
    pub adt: AdtRefId,
    #[return_ref]
    pub params: Vec<TyId>,
    pub variants: Vec<AdtVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtVariant {
    pub name: Partial<IdentId>,
    /// Fields of the variant.
    /// If the adt is an struct or contract, the length of the vector is always
    /// 1.
    pub tys: Vec<Partial<HirTyId>>,
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

    // Type application,
    // e.g.,`TApp(TyConst(Option), TyConst(i32))`.
    TyApp(TyId, TyId),

    TyCon(TyConcrete),

    // TODO: DependentTy,
    // TermTy(TermTy)
    // DependentTyParam(TyParam, TyConst),
    // DependentTyVar(TyVar, TyConst),

    // Invalid type which means the type is not defined.
    // This type can be unified with any other types.
    Invalid(InvalidCause),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidCause {
    /// Type is not fully applied where it is required.
    NotFullyApplied,

    /// Kind mismatch in type level application.
    KindMismatch { abs: TyId, arg: TyId },

    /// Associated Type is not allowed at the moment.
    AssocTy,

    /// Type is not defined even though the name resolution is succeeded.
    ReferenceToNonType,

    /// `Other` indicates the cause is already reported in other analysis
    /// passes, e.g., parser or name resolution.
    Other,
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

    fn is_applicable(&self, rhs: &Self) -> bool {
        match self {
            Self::Abs(k_arg, _) => k_arg.as_ref() == rhs,
            _ => false,
        }
    }

    fn is_any(&self) -> bool {
        match self {
            Self::Any => true,
            _ => false,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Abs(lhs, rhs) => write!(f, "({} -> {})", lhs, rhs),
            Self::Any => write!(f, "Any"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar {
    pub id: u32,
    pub kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParam {
    pub name: IdentId,
    pub idx: usize,
    pub kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyConcrete {
    Prim(PrimTy),
    Abs,
    Adt(AdtDef),
}

impl TyConcrete {
    pub(super) fn tuple(n: usize) -> Self {
        Self::Prim(PrimTy::Tuple(n))
    }
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

#[salsa::interned]
pub struct AdtRefId {
    pub data: AdtRef,
}

impl AdtRefId {
    pub fn scope(self, db: &dyn HirAnalysisDb) -> ScopeId {
        self.data(db).scope()
    }

    pub fn as_item(self, db: &dyn HirAnalysisDb) -> ItemKind {
        match self.data(db) {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
            AdtRef::Contract(c) => c.into(),
        }
    }

    pub fn name(self, db: &dyn HirAnalysisDb) -> IdentId {
        let hir_db = db.as_hir_db();
        match self.data(db) {
            AdtRef::Enum(e) => e.name(hir_db),
            AdtRef::Struct(s) => s.name(hir_db),
            AdtRef::Contract(c) => c.name(hir_db),
        }
        .to_opt()
        .unwrap_or_else(|| IdentId::new(hir_db, "<unknown>".to_string()))
    }

    pub fn name_span(self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        self.scope(db)
            .name_span(db.as_hir_db())
            .unwrap_or_else(|| DynLazySpan::invalid())
    }

    pub fn from_enum(db: &dyn HirAnalysisDb, enum_: Enum) -> Self {
        Self::new(db, AdtRef::Enum(enum_))
    }

    pub fn from_struct(db: &dyn HirAnalysisDb, struct_: Struct) -> Self {
        Self::new(db, AdtRef::Struct(struct_))
    }

    pub fn from_contract(db: &dyn HirAnalysisDb, contract: Contract) -> Self {
        Self::new(db, AdtRef::Contract(contract))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AdtRef {
    Enum(Enum),
    Struct(Struct),
    Contract(Contract),
}

impl AdtRef {
    pub fn scope(self) -> ScopeId {
        match self {
            Self::Enum(e) => e.scope(),
            Self::Struct(s) => s.scope(),
            Self::Contract(c) => c.scope(),
        }
    }
}

pub(super) trait HasKind {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind;
}

impl HasKind for TyData {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyData::TyVar(ty_var) => ty_var.kind(db),
            TyData::TyParam(ty_param) => ty_param.kind.clone(),
            TyData::TyCon(ty_const) => ty_const.kind(db),
            TyData::TyApp(abs, arg) => match abs.kind(db) {
                Kind::Abs(k_arg, k_ret) => {
                    debug_assert!(k_arg.as_ref() == arg.kind(db));
                    k_ret.as_ref().clone()
                }
                _ => unreachable!(),
            },
            TyData::Invalid(_) => Kind::Any,
        }
    }
}

impl HasKind for TyVar {
    fn kind(&self, _db: &dyn HirAnalysisDb) -> Kind {
        self.kind.clone()
    }
}

impl HasKind for TyConcrete {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyConcrete::Prim(prim) => prim.kind(db),
            TyConcrete::Abs => Kind::abs(Kind::Star, Kind::abs(Kind::Star, Kind::Star)),
            TyConcrete::Adt(adt) => adt.kind(db),
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
