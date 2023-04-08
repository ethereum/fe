use crate::context::AnalyzerContext;
use crate::display::DisplayWithDb;
use crate::display::Displayable;
use crate::errors::TypeError;
use crate::namespace::items::{
    ContractId, EnumId, FunctionId, FunctionSigId, ImplId, Item, StructId, TraitId,
};
use crate::AnalyzerDb;

use fe_common::impl_intern_key;
use fe_common::Span;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use smol_str::SmolStr;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;
use strum::{AsRefStr, EnumIter, EnumString};

pub fn u256_min() -> BigInt {
    BigInt::from(0)
}

pub fn u256_max() -> BigInt {
    BigInt::from(2).pow(256) - 1
}

pub fn i256_max() -> BigInt {
    BigInt::from(2).pow(255) - 1
}

pub fn i256_min() -> BigInt {
    BigInt::from(-2).pow(255)
}

pub fn address_max() -> BigInt {
    BigInt::from(2).pow(160) - 1
}

/// Names that can be used to build identifiers without collision.
pub trait SafeNames {
    /// Name in the lower snake format (e.g. lower_snake_case).
    fn lower_snake(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Base(Base),
    Array(Array),
    Map(Map),
    Tuple(Tuple),
    String(FeString),
    /// An "external" contract. Effectively just a `newtype`d address.
    Contract(ContractId),
    /// The type of a contract while it's being executed. Ie. the type
    /// of `self` within a contract function.
    SelfContract(ContractId),
    // The type when `Self` is used within a trait or struct
    SelfType(TraitOrType),
    Struct(StructId),
    Enum(EnumId),
    Generic(Generic),
    SPtr(TypeId),
    Mut(TypeId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TraitOrType {
    TraitId(TraitId),
    TypeId(TypeId),
}

type TraitFunctionLookup = (Vec<(FunctionId, ImplId)>, Vec<(FunctionId, ImplId)>);

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct TypeId(pub(crate) u32);
impl_intern_key!(TypeId);
impl TypeId {
    pub fn unit(db: &dyn AnalyzerDb) -> Self {
        db.intern_type(Type::unit())
    }
    pub fn bool(db: &dyn AnalyzerDb) -> Self {
        db.intern_type(Type::bool())
    }
    pub fn int(db: &dyn AnalyzerDb, int: Integer) -> Self {
        db.intern_type(Type::int(int))
    }
    pub fn address(db: &dyn AnalyzerDb) -> Self {
        db.intern_type(Type::Base(Base::Address))
    }
    pub fn base(db: &dyn AnalyzerDb, t: Base) -> Self {
        db.intern_type(Type::Base(t))
    }
    pub fn tuple(db: &dyn AnalyzerDb, items: &[TypeId]) -> Self {
        db.intern_type(Type::Tuple(Tuple {
            items: items.into(),
        }))
    }

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Type {
        db.lookup_intern_type(*self)
    }
    pub fn deref_typ(&self, db: &dyn AnalyzerDb) -> Type {
        self.deref(db).typ(db)
    }
    pub fn deref(self, db: &dyn AnalyzerDb) -> TypeId {
        match self.typ(db) {
            Type::SPtr(inner) => inner,
            Type::Mut(inner) => inner.deref(db),
            Type::SelfType(TraitOrType::TypeId(inner)) => inner,
            _ => self,
        }
    }
    pub fn make_sptr(self, db: &dyn AnalyzerDb) -> TypeId {
        Type::SPtr(self).id(db)
    }

    pub fn has_fixed_size(&self, db: &dyn AnalyzerDb) -> bool {
        self.typ(db).has_fixed_size(db)
    }

    /// `true` if Type::Base or Type::Contract (which is just an Address)
    pub fn is_primitive(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Base(_) | Type::Contract(_))
    }
    pub fn is_bool(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Base(Base::Bool))
    }
    pub fn is_contract(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Contract(_) | Type::SelfContract(_))
    }
    pub fn is_integer(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Base(Base::Numeric(_)))
    }
    pub fn is_map(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Map(_))
    }
    pub fn is_string(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::String(_))
    }
    pub fn is_self_ty(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::SelfType(_))
    }
    pub fn as_struct(&self, db: &dyn AnalyzerDb) -> Option<StructId> {
        if let Type::Struct(id) = self.typ(db) {
            Some(id)
        } else {
            None
        }
    }
    pub fn as_trait_or_type(&self) -> TraitOrType {
        TraitOrType::TypeId(*self)
    }
    pub fn is_struct(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Struct(_))
    }
    pub fn is_sptr(&self, db: &dyn AnalyzerDb) -> bool {
        match self.typ(db) {
            Type::SPtr(_) => true,
            Type::Mut(inner) => inner.is_sptr(db),
            _ => false,
        }
    }
    pub fn is_generic(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.deref(db).typ(db), Type::Generic(_))
    }

    pub fn is_mut(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Mut(_))
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.typ(db).name(db)
    }

    pub fn kind_display_name(&self, db: &dyn AnalyzerDb) -> &str {
        match self.typ(db) {
            Type::Contract(_) | Type::SelfContract(_) => "contract",
            Type::Struct(_) => "struct",
            Type::Array(_) => "array",
            Type::Tuple(_) => "tuple",
            _ => "type",
        }
    }

    /// Return the `impl` for the given trait. There can only ever be a single
    /// implementation per concrete type and trait.
    pub fn get_impl_for(&self, db: &dyn AnalyzerDb, trait_: TraitId) -> Option<ImplId> {
        db.impl_for(*self, trait_)
    }

    /// Looks up all possible candidates of the given function name that are implemented via traits.
    /// Groups results in two lists, the first contains all theoretical possible candidates and
    /// the second contains only those that are actually callable because the trait is in scope.
    pub fn trait_function_candidates(
        &self,
        context: &mut dyn AnalyzerContext,
        fn_name: &str,
    ) -> TraitFunctionLookup {
        let candidates = context
            .db()
            .all_impls(*self)
            .iter()
            .cloned()
            .filter_map(|_impl| {
                _impl
                    .function(context.db(), fn_name)
                    .map(|fun| (fun, _impl))
            })
            .collect::<Vec<_>>();

        let in_scope_candidates = candidates
            .iter()
            .cloned()
            .filter(|(_, _impl)| {
                context
                    .module()
                    .is_in_scope(context.db(), Item::Trait(_impl.trait_id(context.db())))
            })
            .collect::<Vec<_>>();

        (candidates, in_scope_candidates)
    }

    /// Signature for the function with the given name defined directly on the type.
    /// Does not consider trait impls.
    pub fn function_sig(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionSigId> {
        match self.typ(db) {
            Type::SPtr(inner) => inner.function_sig(db, name),
            Type::Contract(id) => id.function(db, name).map(|fun| fun.sig(db)),
            Type::SelfContract(id) => id.function(db, name).map(|fun| fun.sig(db)),
            Type::Struct(id) => id.function(db, name).map(|fun| fun.sig(db)),
            Type::Enum(id) => id.function(db, name).map(|fun| fun.sig(db)),
            // TODO: This won't hold when we support multiple bounds
            Type::Generic(inner) => inner
                .bounds
                .first()
                .and_then(|bound| bound.function(db, name)),
            _ => None,
        }
    }

    /// Like `function_sig` but returns a `Vec<FunctionSigId>` which not only
    /// considers functions natively implemented on the type but also those
    /// that are provided by implemented traits on the type.
    pub fn function_sigs(&self, db: &dyn AnalyzerDb, name: &str) -> Rc<[FunctionSigId]> {
        db.function_sigs(*self, name.into())
    }

    pub fn self_function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionSigId> {
        let fun = self.function_sig(db, name)?;
        fun.takes_self(db).then_some(fun)
    }

    /// Returns `true` if the type qualifies to implement the `Emittable` trait
    /// TODO: This function should be removed when we add `Encode / Decode`
    /// trait
    pub fn is_emittable(self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Type::Struct(_)) && self.is_encodable(db).unwrap_or(false)
    }

    /// Returns `true` if the type is encodable in Solidity ABI.
    /// TODO: This function must be removed when we add `Encode`/`Decode` trait.
    pub fn is_encodable(self, db: &dyn AnalyzerDb) -> Result<bool, TypeError> {
        match self.typ(db) {
            Type::Base(_) | Type::String(_) | Type::Contract(_) => Ok(true),
            Type::Array(arr) => arr.inner.is_encodable(db),
            Type::Struct(sid) => {
                // Returns `false` if diagnostics is not empty.
                // The diagnostics is properly emitted in struct definition site, so there is no
                // need to emit the diagnostics here.
                if !db.struct_dependency_graph(sid).diagnostics.is_empty() {
                    return Ok(false);
                };
                let mut res = true;
                // We have to continue the iteration even if an item is NOT encodable so that we
                // could propagate an error which returned from `item.is_encodable()` for
                // keeping consistency.
                for (_, &fid) in sid.fields(db).iter() {
                    res &= fid.typ(db)?.is_encodable(db)?;
                }
                Ok(res)
            }
            Type::Tuple(tup) => {
                let mut res = true;
                // We have to continue the iteration even if an item is NOT encodable so that we
                // could propagate an error which returned from `item.is_encodable()` for
                // keeping consistency.
                for item in tup.items.iter() {
                    res &= item.is_encodable(db)?;
                }
                Ok(res)
            }
            Type::Mut(inner) => inner.is_encodable(db),
            Type::SelfType(id) => match id {
                TraitOrType::TraitId(_) => Ok(false),
                TraitOrType::TypeId(id) => id.is_encodable(db),
            },
            Type::Map(_)
            | Type::SelfContract(_)
            | Type::Generic(_)
            | Type::Enum(_)
            | Type::SPtr(_) => Ok(false),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Base {
    Numeric(Integer),
    Bool,
    Address,
    Unit,
}

impl Base {
    pub fn name(&self) -> SmolStr {
        match self {
            Base::Numeric(num) => num.as_ref().into(),
            Base::Bool => "bool".into(),
            Base::Address => "address".into(),
            Base::Unit => "()".into(),
        }
    }
    pub fn u256() -> Base {
        Base::Numeric(Integer::U256)
    }
}

#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, AsRefStr, EnumString, EnumIter,
)]
#[strum(serialize_all = "snake_case")]
pub enum Integer {
    U256,
    U128,
    U64,
    U32,
    U16,
    U8,
    I256,
    I128,
    I64,
    I32,
    I16,
    I8,
}

pub const U256: Base = Base::Numeric(Integer::U256);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Array {
    pub size: usize,
    pub inner: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Map {
    pub key: TypeId,
    pub value: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Generic {
    pub name: SmolStr,
    pub bounds: Rc<[TraitId]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub items: Rc<[TypeId]>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct FeString {
    pub max_size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub self_decl: Option<SelfDecl>,
    pub ctx_decl: Option<CtxDecl>,
    pub params: Vec<FunctionParam>,
    pub return_type: Result<TypeId, TypeError>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SelfDecl {
    pub span: Span,
    pub mut_: Option<Span>,
}

impl SelfDecl {
    pub fn is_mut(&self) -> bool {
        self.mut_.is_some()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct CtxDecl {
    pub span: Span,
    pub mut_: Option<Span>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionParam {
    label: Option<SmolStr>,
    pub name: SmolStr,
    pub typ: Result<TypeId, TypeError>,
}
impl FunctionParam {
    pub fn new(label: Option<&str>, name: &str, typ: Result<TypeId, TypeError>) -> Self {
        Self {
            label: label.map(SmolStr::new),
            name: name.into(),
            typ,
        }
    }
    pub fn label(&self) -> Option<&str> {
        match &self.label {
            Some(label) if label == "_" => None,
            Some(label) => Some(label),
            None => Some(&self.name),
        }
    }
}

#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, AsRefStr, EnumIter,
)]
pub enum GenericType {
    Array,
    String,
    Map,
}

impl GenericType {
    pub fn name(&self) -> SmolStr {
        self.as_ref().into()
    }
    pub fn params(&self) -> Vec<GenericParam> {
        match self {
            GenericType::String => vec![GenericParam {
                name: "max size".into(),
                kind: GenericParamKind::Int,
            }],
            GenericType::Map => vec![
                GenericParam {
                    name: "key".into(),
                    kind: GenericParamKind::PrimitiveType,
                },
                GenericParam {
                    name: "value".into(),
                    kind: GenericParamKind::AnyType,
                },
            ],
            GenericType::Array => vec![
                GenericParam {
                    name: "element type".into(),
                    kind: GenericParamKind::AnyType,
                },
                GenericParam {
                    name: "size".into(),
                    kind: GenericParamKind::Int,
                },
            ],
        }
    }

    // see traversal::types::apply_generic_type_args for error checking
    pub fn apply(&self, db: &dyn AnalyzerDb, args: &[GenericArg]) -> Option<TypeId> {
        let typ = match self {
            GenericType::String => match args {
                [GenericArg::Int(max_size)] => Some(Type::String(FeString {
                    max_size: *max_size,
                })),
                _ => None,
            },
            GenericType::Map => match args {
                [GenericArg::Type(key), GenericArg::Type(value)] => Some(Type::Map(Map {
                    key: *key,
                    value: *value,
                })),
                _ => None,
            },
            GenericType::Array => match args {
                [GenericArg::Type(element), GenericArg::Int(size)] => Some(Type::Array(Array {
                    size: *size,
                    inner: *element,
                })),
                _ => None,
            },
        }?;
        Some(db.intern_type(typ))
    }
}

pub struct GenericParam {
    pub name: SmolStr,
    pub kind: GenericParamKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericParamKind {
    Int,

    // Ideally these would be represented as trait constraints.
    PrimitiveType,
    // FixedSizeType, // not needed yet
    AnyType,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericArg {
    Int(usize),
    Type(TypeId),
}

impl Integer {
    /// Returns `true` if the integer is signed, otherwise `false`
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Integer::I256
                | Integer::I128
                | Integer::I64
                | Integer::I32
                | Integer::I16
                | Integer::I8
        )
    }

    pub fn size(&self) -> usize {
        match self {
            Integer::U256 | Integer::I256 => 32,
            Integer::U128 | Integer::I128 => 16,
            Integer::U64 | Integer::I64 => 8,
            Integer::U32 | Integer::I32 => 4,
            Integer::U16 | Integer::I16 => 2,
            Integer::U8 | Integer::I8 => 1,
        }
    }

    /// Returns size of integer type in bits.
    pub fn bits(&self) -> usize {
        self.size() * 8
    }

    /// Returns `true` if the integer is at least the same size (or larger) than
    /// `other`
    pub fn can_hold(&self, other: Integer) -> bool {
        self.size() >= other.size()
    }

    /// Returns `true` if `num` represents a number that fits the type
    pub fn fits(&self, num: BigInt) -> bool {
        match self {
            Integer::U8 => num.to_u8().is_some(),
            Integer::U16 => num.to_u16().is_some(),
            Integer::U32 => num.to_u32().is_some(),
            Integer::U64 => num.to_u64().is_some(),
            Integer::U128 => num.to_u128().is_some(),
            Integer::I8 => num.to_i8().is_some(),
            Integer::I16 => num.to_i16().is_some(),
            Integer::I32 => num.to_i32().is_some(),
            Integer::I64 => num.to_i64().is_some(),
            Integer::I128 => num.to_i128().is_some(),
            Integer::U256 => num >= u256_min() && num <= u256_max(),
            Integer::I256 => num >= i256_min() && num <= i256_max(),
        }
    }

    /// Returns max value of the integer type.
    pub fn max_value(&self) -> BigInt {
        match self {
            Integer::U256 => u256_max(),
            Integer::U128 => u128::MAX.into(),
            Integer::U64 => u64::MAX.into(),
            Integer::U32 => u32::MAX.into(),
            Integer::U16 => u16::MAX.into(),
            Integer::U8 => u8::MAX.into(),
            Integer::I256 => i256_max(),
            Integer::I128 => i128::MAX.into(),
            Integer::I64 => i64::MAX.into(),
            Integer::I32 => i32::MAX.into(),
            Integer::I16 => i16::MAX.into(),
            Integer::I8 => i8::MAX.into(),
        }
    }

    /// Returns min value of the integer type.
    pub fn min_value(&self) -> BigInt {
        match self {
            Integer::U256 => u256_min(),
            Integer::U128 => u128::MIN.into(),
            Integer::U64 => u64::MIN.into(),
            Integer::U32 => u32::MIN.into(),
            Integer::U16 => u16::MIN.into(),
            Integer::U8 => u8::MIN.into(),
            Integer::I256 => i256_min(),
            Integer::I128 => i128::MIN.into(),
            Integer::I64 => i64::MIN.into(),
            Integer::I32 => i32::MIN.into(),
            Integer::I16 => i16::MIN.into(),
            Integer::I8 => i8::MIN.into(),
        }
    }
}

impl Type {
    pub fn id(&self, db: &dyn AnalyzerDb) -> TypeId {
        db.intern_type(self.clone())
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self {
            Type::Base(inner) => inner.name(),
            _ => self.display(db).to_string().into(),
        }
    }

    pub fn def_span(&self, context: &dyn AnalyzerContext) -> Option<Span> {
        match self {
            Self::Struct(id) => Some(id.span(context.db())),
            Self::Contract(id) | Self::SelfContract(id) => Some(id.span(context.db())),
            _ => None,
        }
    }

    /// Creates an instance of bool.
    pub fn bool() -> Self {
        Type::Base(Base::Bool)
    }

    /// Creates an instance of address.
    pub fn address() -> Self {
        Type::Base(Base::Address)
    }

    /// Creates an instance of u256.
    pub fn u256() -> Self {
        Type::Base(Base::Numeric(Integer::U256))
    }

    /// Creates an instance of u8.
    pub fn u8() -> Self {
        Type::Base(Base::Numeric(Integer::U8))
    }

    /// Creates an instance of `()`.
    pub fn unit() -> Self {
        Type::Base(Base::Unit)
    }

    pub fn is_unit(&self) -> bool {
        *self == Type::Base(Base::Unit)
    }

    pub fn int(int_type: Integer) -> Self {
        Type::Base(Base::Numeric(int_type))
    }

    pub fn has_fixed_size(&self, db: &dyn AnalyzerDb) -> bool {
        match self {
            Type::Base(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::String(_)
            | Type::Struct(_)
            | Type::Enum(_)
            | Type::Generic(_)
            | Type::Contract(_) => true,
            Type::Map(_) | Type::SelfContract(_) => false,
            Type::SelfType(inner) => match inner {
                TraitOrType::TraitId(_) => true,
                TraitOrType::TypeId(id) => id.has_fixed_size(db),
            },
            Type::SPtr(inner) | Type::Mut(inner) => inner.has_fixed_size(db),
        }
    }
}

pub trait TypeDowncast {
    fn as_array(&self, db: &dyn AnalyzerDb) -> Option<Array>;
    fn as_tuple(&self, db: &dyn AnalyzerDb) -> Option<Tuple>;
    fn as_string(&self, db: &dyn AnalyzerDb) -> Option<FeString>;
    fn as_map(&self, db: &dyn AnalyzerDb) -> Option<Map>;
    fn as_int(&self, db: &dyn AnalyzerDb) -> Option<Integer>;
}

impl TypeDowncast for TypeId {
    fn as_array(&self, db: &dyn AnalyzerDb) -> Option<Array> {
        match self.typ(db) {
            Type::Array(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_tuple(&self, db: &dyn AnalyzerDb) -> Option<Tuple> {
        match self.typ(db) {
            Type::Tuple(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_string(&self, db: &dyn AnalyzerDb) -> Option<FeString> {
        match self.typ(db) {
            Type::String(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_map(&self, db: &dyn AnalyzerDb) -> Option<Map> {
        match self.typ(db) {
            Type::Map(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_int(&self, db: &dyn AnalyzerDb) -> Option<Integer> {
        match self.typ(db) {
            Type::Base(Base::Numeric(int)) => Some(int),
            _ => None,
        }
    }
}

impl From<Base> for Type {
    fn from(value: Base) -> Self {
        Type::Base(value)
    }
}

impl DisplayWithDb for Type {
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Display;
        match self {
            Type::Base(inner) => inner.fmt(f),
            Type::String(inner) => inner.fmt(f),
            Type::Array(arr) => {
                write!(f, "Array<{}, {}>", arr.inner.display(db), arr.size)
            }
            Type::Map(map) => {
                let Map { key, value } = map;
                write!(f, "Map<{}, {}>", key.display(db), value.display(db),)
            }
            Type::Tuple(id) => {
                write!(f, "(")?;
                let mut delim = "";
                for item in id.items.iter() {
                    write!(f, "{}{}", delim, item.display(db))?;
                    delim = ", ";
                }
                write!(f, ")")
            }
            Type::Contract(id) | Type::SelfContract(id) => write!(f, "{}", id.name(db)),
            Type::Struct(id) => write!(f, "{}", id.name(db)),
            Type::Enum(id) => write!(f, "{}", id.name(db)),
            Type::Generic(inner) => inner.fmt(f),
            Type::SPtr(inner) => write!(f, "SPtr<{}>", inner.display(db)),
            Type::Mut(inner) => write!(f, "mut {}", inner.display(db)),
            Type::SelfType(_) => write!(f, "Self"),
        }
    }
}
impl DisplayWithDb for TypeId {
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.typ(db).format(db, f)
    }
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Base::Numeric(int) => return int.fmt(f),
            Base::Bool => "bool",
            Base::Address => "address",
            Base::Unit => "()",
        };
        write!(f, "{name}")
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Integer::U256 => "u256",
            Integer::U128 => "u128",
            Integer::U64 => "u64",
            Integer::U32 => "u32",
            Integer::U16 => "u16",
            Integer::U8 => "u8",
            Integer::I256 => "i256",
            Integer::I128 => "i128",
            Integer::I64 => "i64",
            Integer::I32 => "i32",
            Integer::I16 => "i16",
            Integer::I8 => "i8",
        };
        write!(f, "{name}")
    }
}

impl fmt::Display for FeString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "String<{}>", self.max_size)
    }
}

impl DisplayWithDb for FunctionSignature {
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let FunctionSignature {
            self_decl,
            ctx_decl: _,
            params,
            return_type,
        } = self;

        write!(f, "params: [")?;
        let mut delim = "";
        if let Some(s) = self_decl {
            write!(f, "{}self", if s.mut_.is_some() { "mut " } else { "" },)?;
            delim = ", ";
        }

        for p in params {
            write!(
                f,
                "{}{{ label: {:?}, name: {}, typ: {} }}",
                delim,
                p.label,
                p.name,
                p.typ.as_ref().unwrap().display(db),
            )?;
            delim = ", ";
        }
        write!(f, "] -> {}", return_type.as_ref().unwrap().display(db))
    }
}

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl FromStr for Base {
    type Err = strum::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(Base::Bool),
            "address" => Ok(Base::Address),
            "()" => Ok(Base::Unit),
            _ => Ok(Base::Numeric(Integer::from_str(s)?)),
        }
    }
}
