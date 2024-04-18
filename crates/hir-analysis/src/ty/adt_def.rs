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
pub fn lower_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) -> AdtDef {
    AdtTyBuilder::new(db, adt).build()
}

/// Represents a ADT type definition.
#[salsa::tracked]
pub struct AdtDef {
    pub adt_ref: AdtRefId,

    /// Type parameters of the ADT.
    #[return_ref]
    pub param_set: GenericParamTypeSet,

    /// Fields of the ADT, if the ADT is an enum, this represents variants.
    /// Otherwise, `fields[0]` represents all fields of the struct.
    #[return_ref]
    pub fields: Vec<AdtField>,
}

impl AdtDef {
    pub(crate) fn name(self, db: &dyn HirAnalysisDb) -> IdentId {
        self.adt_ref(db).name(db)
    }

    pub(crate) fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.param_set(db).params(db)
    }

    pub(crate) fn original_params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.param_set(db).explicit_params(db)
    }

    pub(crate) fn is_struct(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.adt_ref(db).data(db), AdtRef::Struct(_))
    }

    pub(crate) fn variant_ty_span(
        self,
        db: &dyn HirAnalysisDb,
        field_idx: usize,
        ty_idx: usize,
    ) -> DynLazySpan {
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

    pub(crate) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        let hir_db = db.as_hir_db();
        match self.adt_ref(db).data(db) {
            AdtRef::Enum(e) => e.top_mod(hir_db).ingot(hir_db),
            AdtRef::Struct(s) => s.top_mod(hir_db).ingot(hir_db),
            AdtRef::Contract(c) => c.top_mod(hir_db).ingot(hir_db),
        }
    }

    pub(crate) fn as_generic_param_owner(
        self,
        db: &dyn HirAnalysisDb,
    ) -> Option<GenericParamOwnerId> {
        self.adt_ref(db).generic_owner_id(db)
    }
}

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtField {
    /// Fields of the variant.
    /// If the adt is an struct or contract,
    /// the length of the vector is always 1.
    ///
    /// To allow recursive types, the type of the field is represented as a HIR
    /// type and.
    tys: Vec<Partial<HirTyId>>,

    scope: ScopeId,
}
impl AdtField {
    pub fn ty(&self, db: &dyn HirAnalysisDb, i: usize) -> Binder<TyId> {
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
        db: &'a dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId>> + 'a {
        (0..self.num_types()).map(|i| self.ty(db, i))
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(super) fn new(tys: Vec<Partial<HirTyId>>, scope: ScopeId) -> Self {
        Self { tys, scope }
    }
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

    pub fn kind_name(self, db: &dyn HirAnalysisDb) -> &'static str {
        self.as_item(db).kind_name()
    }

    pub fn name_span(self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        self.scope(db)
            .name_span(db.as_hir_db())
            .unwrap_or_else(DynLazySpan::invalid)
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

    pub(crate) fn generic_owner_id(self, db: &dyn HirAnalysisDb) -> Option<GenericParamOwnerId> {
        match self.data(db) {
            AdtRef::Enum(e) => Some(GenericParamOwnerId::new(db, e.into())),
            AdtRef::Struct(s) => Some(GenericParamOwnerId::new(db, s.into())),
            AdtRef::Contract(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
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

struct AdtTyBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    adt: AdtRefId,
    params: GenericParamTypeSet,
    variants: Vec<AdtField>,
}

impl<'db> AdtTyBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, adt: AdtRefId) -> Self {
        Self {
            db,
            adt,
            params: GenericParamTypeSet::empty(db, adt.scope(db)),
            variants: Vec::new(),
        }
    }

    fn build(mut self) -> AdtDef {
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

    fn collect_field_types(&mut self, fields: FieldDefListId) {
        let scope = self.adt.scope(self.db);

        let fields = fields
            .data(self.db.as_hir_db())
            .iter()
            .map(|field| field.ty)
            .collect();

        self.variants.push(AdtField::new(fields, scope));
    }

    fn collect_enum_variant_types(&mut self, variants: VariantDefListId) {
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
