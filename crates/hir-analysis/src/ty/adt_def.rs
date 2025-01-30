use hir::{
    hir_def::{
        scope_graph::ScopeId, Contract, Enum, FieldDefListId, IdentId, IngotId, ItemKind, Partial,
        Struct, TypeId as HirTyId, VariantDefListId, VariantKind,
    },
    span::DynLazySpan,
};

use super::{
    binder::Binder,
    ty_def::{InvalidCause, TyId},
    ty_lower::{collect_generic_params, lower_hir_ty, GenericParamOwnerId, GenericParamTypeSet},
};
use crate::HirAnalysisDb;

/// Lower HIR ADT definition(`struct/enum/contract`) to [`AdtDef`].
#[salsa::tracked]
pub fn lower_adt<'db>(db: &'db dyn HirAnalysisDb, adt: AdtRefId<'db>) -> AdtDef<'db> {
    AdtTyBuilder::new(db, adt).build()
}

/// Represents a ADT type definition.
#[salsa::tracked]
pub struct AdtDef<'db> {
    pub adt_ref: AdtRefId<'db>,

    /// Type parameters of the ADT.
    #[return_ref]
    pub param_set: GenericParamTypeSet<'db>,

    /// Fields of the ADT, if the ADT is an enum, this represents variants.
    /// Otherwise, `fields[0]` represents all fields of the struct.
    #[return_ref]
    pub fields: Vec<AdtField<'db>>,
}

impl<'db> AdtDef<'db> {
    pub(crate) fn name(self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        self.adt_ref(db).name(db)
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.adt_ref(db).name_span(db)
    }

    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).params(db)
    }

    pub(crate) fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).explicit_params(db)
    }

    pub fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.adt_ref(db).scope(db)
    }

    pub(crate) fn variant_ty_span(
        self,
        db: &'db dyn HirAnalysisDb,
        field_idx: usize,
        ty_idx: usize,
    ) -> DynLazySpan<'db> {
        match self.adt_ref(db).data(db) {
            AdtRef::Enum(e) => {
                let span = e.lazy_span().variants_moved().variant_moved(field_idx);
                match e.variants(db.as_hir_db()).data(db.as_hir_db())[field_idx].kind {
                    VariantKind::Tuple(_) => span.tuple_type_moved().elem_ty_moved(ty_idx).into(),
                    VariantKind::Record(_) => {
                        span.fields_moved().field_moved(ty_idx).ty_moved().into()
                    }
                    VariantKind::Unit => unreachable!(),
                }
            }

            AdtRef::Struct(s) => s
                .lazy_span()
                .fields_moved()
                .field_moved(field_idx)
                .ty_moved()
                .into(),

            AdtRef::Contract(c) => c
                .lazy_span()
                .fields_moved()
                .field_moved(field_idx)
                .ty_moved()
                .into(),
        }
    }

    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> IngotId<'db> {
        let hir_db = db.as_hir_db();
        match self.adt_ref(db).data(db) {
            AdtRef::Enum(e) => e.top_mod(hir_db).ingot(hir_db),
            AdtRef::Struct(s) => s.top_mod(hir_db).ingot(hir_db),
            AdtRef::Contract(c) => c.top_mod(hir_db).ingot(hir_db),
        }
    }

    pub(crate) fn as_generic_param_owner(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<GenericParamOwnerId<'db>> {
        self.adt_ref(db).generic_owner_id(db)
    }
}

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtField<'db> {
    /// Fields of the variant.
    /// If the adt is an struct or contract,
    /// the length of the vector is always 1.
    ///
    /// To allow recursive types, the type of the field is represented as a HIR
    /// type and.
    tys: Vec<Partial<HirTyId<'db>>>,

    scope: ScopeId<'db>,
}
impl<'db> AdtField<'db> {
    pub fn ty(&self, db: &'db dyn HirAnalysisDb, i: usize) -> Binder<TyId<'db>> {
        let ty = if let Some(ty) = self.tys[i].to_opt() {
            lower_hir_ty(db, ty, self.scope)
        } else {
            TyId::invalid(db, InvalidCause::Other)
        };

        Binder::bind(ty)
    }

    /// Iterates all fields types of the `field`.
    pub fn iter_types<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId<'db>>> + 'a {
        (0..self.num_types()).map(|i| self.ty(db, i))
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(super) fn new(tys: Vec<Partial<HirTyId<'db>>>, scope: ScopeId<'db>) -> Self {
        Self { tys, scope }
    }
}

#[salsa::interned]
pub struct AdtRefId<'db> {
    pub data: AdtRef<'db>,
}

impl<'db> AdtRefId<'db> {
    pub fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.data(db).scope()
    }

    pub fn as_item(self, db: &'db dyn HirAnalysisDb) -> ItemKind<'db> {
        match self.data(db) {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
            AdtRef::Contract(c) => c.into(),
        }
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        let hir_db = db.as_hir_db();
        match self.data(db) {
            AdtRef::Enum(e) => e.name(hir_db),
            AdtRef::Struct(s) => s.name(hir_db),
            AdtRef::Contract(c) => c.name(hir_db),
        }
        .to_opt()
        .unwrap_or_else(|| IdentId::new(hir_db, "<unknown>".to_string()))
    }

    pub fn kind_name(self, db: &dyn HirAnalysisDb) -> &'static str {
        self.as_item(db).kind_name()
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.scope(db)
            .name_span(db.as_hir_db())
            .unwrap_or_else(DynLazySpan::invalid)
    }

    pub fn from_enum(db: &'db dyn HirAnalysisDb, enum_: Enum<'db>) -> Self {
        Self::new(db, AdtRef::Enum(enum_))
    }

    pub fn from_struct(db: &'db dyn HirAnalysisDb, struct_: Struct<'db>) -> Self {
        Self::new(db, AdtRef::Struct(struct_))
    }

    pub fn from_contract(db: &'db dyn HirAnalysisDb, contract: Contract<'db>) -> Self {
        Self::new(db, AdtRef::Contract(contract))
    }

    pub fn try_from_item(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> Option<Self> {
        match item {
            ItemKind::Enum(e) => Some(Self::from_enum(db, e)),
            ItemKind::Struct(s) => Some(Self::from_struct(db, s)),
            ItemKind::Contract(c) => Some(Self::from_contract(db, c)),
            _ => None,
        }
    }

    pub(crate) fn generic_owner_id(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<GenericParamOwnerId<'db>> {
        match self.data(db) {
            AdtRef::Enum(e) => Some(GenericParamOwnerId::new(db, e.into())),
            AdtRef::Struct(s) => Some(GenericParamOwnerId::new(db, s.into())),
            AdtRef::Contract(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum AdtRef<'db> {
    Enum(Enum<'db>),
    Struct(Struct<'db>),
    Contract(Contract<'db>),
}

impl<'db> AdtRef<'db> {
    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Enum(e) => e.scope(),
            Self::Struct(s) => s.scope(),
            Self::Contract(c) => c.scope(),
        }
    }
}

struct AdtTyBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    adt: AdtRefId<'db>,
    params: GenericParamTypeSet<'db>,
    variants: Vec<AdtField<'db>>,
}

impl<'db> AdtTyBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, adt: AdtRefId<'db>) -> Self {
        Self {
            db,
            adt,
            params: GenericParamTypeSet::empty(db, adt.scope(db)),
            variants: Vec::new(),
        }
    }

    fn build(mut self) -> AdtDef<'db> {
        self.collect_generic_params();
        self.collect_variants();
        AdtDef::new(self.db, self.adt, self.params, self.variants)
    }

    fn collect_generic_params(&mut self) {
        let owner = match self.adt.data(self.db) {
            AdtRef::Contract(_) => return,
            AdtRef::Enum(enum_) => enum_.into(),
            AdtRef::Struct(struct_) => struct_.into(),
        };
        let owner_id = GenericParamOwnerId::new(self.db, owner);

        self.params = collect_generic_params(self.db, owner_id);
    }

    fn collect_variants(&mut self) {
        match self.adt.data(self.db) {
            AdtRef::Struct(struct_) => {
                self.collect_field_types(struct_.fields(self.db.as_hir_db()));
            }

            AdtRef::Contract(contract) => {
                self.collect_field_types(contract.fields(self.db.as_hir_db()))
            }

            AdtRef::Enum(enum_) => {
                self.collect_enum_variant_types(enum_.variants(self.db.as_hir_db()))
            }
        };
    }

    fn collect_field_types(&mut self, fields: FieldDefListId<'db>) {
        let scope = self.adt.scope(self.db);

        let fields = fields
            .data(self.db.as_hir_db())
            .iter()
            .map(|field| field.ty)
            .collect();

        self.variants.push(AdtField::new(fields, scope));
    }

    fn collect_enum_variant_types(&mut self, variants: VariantDefListId<'db>) {
        let scope = self.adt.scope(self.db);

        variants
            .data(self.db.as_hir_db())
            .iter()
            .for_each(|variant| {
                // TODO: FIX here when record variant is introduced.
                let tys = match variant.kind {
                    VariantKind::Tuple(tuple_id) => tuple_id.data(self.db.as_hir_db()).clone(),

                    VariantKind::Record(fields) => fields
                        .data(self.db.as_hir_db())
                        .iter()
                        .map(|field| field.ty)
                        .collect(),

                    VariantKind::Unit => vec![],
                };

                let variant = AdtField::new(tys, scope);
                self.variants.push(variant)
            })
    }
}
