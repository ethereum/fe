use crate::{hir_def::TypeId, HirDb};

use super::{kw, Body, IdentId, Partial, PathId};

#[salsa::interned]
pub struct GenericArgListId {
    #[return_ref]
    pub data: Vec<GenericArg>,
}

impl GenericArgListId {
    pub fn len(self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }

    pub fn is_empty(self, db: &dyn HirDb) -> bool {
        self.data(db).is_empty()
    }
}

#[salsa::interned]
pub struct GenericParamListId {
    #[return_ref]
    pub data: Vec<GenericParam>,
}

impl GenericParamListId {
    pub fn len(&self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }
}

#[salsa::interned]
pub struct FuncParamListId {
    #[return_ref]
    pub data: Vec<FuncParam>,
}

#[salsa::interned]
pub struct WhereClauseId {
    #[return_ref]
    pub data: Vec<WherePredicate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum GenericParam {
    Type(TypeGenericParam),
    Const(ConstGenericParam),
}

impl GenericParam {
    pub fn name(&self) -> Partial<IdentId> {
        match self {
            Self::Type(ty) => ty.name,
            Self::Const(c) => c.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericParam {
    pub name: Partial<IdentId>,
    pub bounds: Vec<TypeBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericParam {
    pub name: Partial<IdentId>,
    pub ty: Partial<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum GenericArg {
    Type(TypeGenericArg),
    Const(ConstGenericArg),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericArg {
    pub ty: Partial<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericArg {
    pub body: Partial<Body>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncParam {
    pub is_mut: bool,
    pub label: Option<FuncParamName>,
    pub name: Partial<FuncParamName>,
    pub ty: Partial<TypeId>,

    /// `true` if this parameter is `self` and the type is not specified.
    /// `ty` should have `Self` type without any type arguments.
    pub self_ty_fallback: bool,
}

impl FuncParam {
    pub fn name(&self) -> Option<IdentId> {
        match self.name.to_opt()? {
            FuncParamName::Ident(name) => Some(name),
            _ => None,
        }
    }

    pub fn is_self_param(&self) -> bool {
        self.name.to_opt().map_or(false, |name| name.is_self())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate {
    pub ty: Partial<TypeId>,
    pub bounds: Vec<TypeBound>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FuncParamName {
    Ident(IdentId),
    Underscore,
}

impl FuncParamName {
    pub fn ident(&self) -> Option<IdentId> {
        match self {
            FuncParamName::Ident(name) => Some(*name),
            _ => None,
        }
    }

    pub fn is_self(&self) -> bool {
        self.ident() == Some(kw::SELF)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeBound {
    Trait(TraitRefId),
    Kind(Partial<KindBound>),
}

#[salsa::interned]
pub struct TraitRefId {
    /// The path to the trait.
    pub path: Partial<PathId>,
    /// The type arguments of the trait.
    pub generic_args: Option<GenericArgListId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KindBound {
    /// `*`
    Mono,
    /// `* -> *`
    Abs(Partial<Box<KindBound>>, Partial<Box<KindBound>>),
}
