use common::ingot::{Ingot, IngotKind};
use hir::analysis::ty::evidence::{Evidence, EvidenceId, EvidenceList};
use hir::{
    analysis::{
        HirAnalysisDb,
        semantic::{EffectProviderSubst, GenericSubst, ImplEnv, SemanticInstance},
        ty::{
            RootProviderScope,
            trait_def::{TraitInstId, TraitRefId},
            ty_check::{BodyOwner, EffectParamSite},
            ty_def::{BorrowKind, ClosureTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{
        Body, CallableDef, ItemKind, TopLevelMod, params::FuncParamMode, scope_graph::ScopeId,
    },
    semantic::{ProviderBinding, ProviderSource},
};

/// Selects how much detail an identity rendering includes.
///
/// `Sort` is the complete stable identity. `Symbol` deliberately omits detail
/// already represented by generic suffixes or disambiguation hashes and
/// filters incidental internal effect-provider constraints.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IdentityMode {
    Sort,
    Symbol,
}

pub fn semantic_instance_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
) -> String {
    semantic_instance_identity_in_mode(db, semantic, IdentityMode::Sort)
}

pub fn semantic_instance_symbol_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
) -> String {
    semantic_instance_identity_in_mode(db, semantic, IdentityMode::Symbol)
}

pub fn semantic_instance_identity_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
    mode: IdentityMode,
) -> String {
    let key = semantic.key(db);
    format!(
        "{}$subst${}$effects${}$impl${}",
        body_owner_identity(db, key.owner(db)),
        generic_subst_identity(db, key.subst(db)),
        effect_provider_subst_identity(db, key.effect_providers(db), mode),
        impl_env_identity(db, key.impl_env(db), key.evidence(db), mode)
    )
}

fn body_owner_identity<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> String {
    match owner {
        BodyOwner::Func(func, mode) => {
            let owner_context = semantic_owner_context_identity(db, owner).unwrap_or_default();
            // A `mut?` function's bodies, one per mode, are distinct.
            let mode = match mode {
                None => "",
                Some(BorrowKind::Ref) => "$ref",
                Some(BorrowKind::Mut) => "$mut",
            };
            format!(
                "func${}${owner_context}{mode}",
                item_identity(db, func.into())
            )
        }
        BodyOwner::Const(const_) => format!("const${}", item_identity(db, const_.into())),
        BodyOwner::AnonConstBody { body, expected } => format!(
            "anon_const${}${}",
            module_path_components_for_scope(db, body.scope()).join("$"),
            type_identity(db, expected)
        ),
        BodyOwner::ContractInit { contract } => {
            format!("contract_init${}", item_identity(db, contract.into()))
        }
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "contract_recv${}${recv_idx}${arm_idx}",
            item_identity(db, contract.into())
        ),
        BodyOwner::Closure { ty, receiver, .. } => format!(
            "closure_body${}${receiver:?}",
            type_identity(db, TyId::closure(db, ty))
        ),
    }
}

pub fn semantic_owner_context_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<String> {
    let BodyOwner::Func(func, _) = owner else {
        return None;
    };
    if let Some(trait_) = func.containing_trait(db) {
        return Some(format!("trait${}", item_name_component(db, trait_.into())));
    }
    if let Some(impl_trait) = func.containing_impl_trait(db) {
        let trait_identity = impl_trait
            .trait_inst(db)
            .map(|trait_inst| trait_identity(db, trait_inst))
            .unwrap_or_else(|| {
                impl_trait
                    .hir_trait_ref(db)
                    .to_opt()
                    .map(|trait_ref| trait_ref.pretty_print(db))
                    .unwrap_or_else(|| "trait".to_string())
            });
        let impl_identity = format!("{trait_identity}${}", type_identity(db, impl_trait.ty(db)));
        return Some(format!(
            "impl_trait${}",
            stable_identity_fingerprint(&impl_identity)
        ));
    }
    func.containing_impl(db).map(|impl_| {
        format!(
            "impl${}",
            stable_identity_fingerprint(&type_identity(db, impl_.ty(db)))
        )
    })
}

/// Trace-facing variant of [`semantic_owner_context_identity`]: spells the
/// impl context out textually instead of compressing it to a 64-bit
/// fingerprint. HIR body identity on the trace wire must not rest on a hash
/// whose collision would merge two different bodies' facts.
pub fn semantic_owner_context_trace_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<String> {
    let BodyOwner::Func(func, _) = owner else {
        return None;
    };
    if let Some(trait_) = func.containing_trait(db) {
        return Some(format!("trait${}", item_name_component(db, trait_.into())));
    }
    if let Some(impl_trait) = func.containing_impl_trait(db) {
        let trait_identity = impl_trait
            .trait_inst(db)
            .map(|trait_inst| trait_identity(db, trait_inst))
            .unwrap_or_else(|| {
                impl_trait
                    .hir_trait_ref(db)
                    .to_opt()
                    .map(|trait_ref| trait_ref.pretty_print(db))
                    .unwrap_or_else(|| "trait".to_string())
            });
        return Some(format!(
            "impl_trait${trait_identity}${}",
            type_identity(db, impl_trait.ty(db))
        ));
    }
    func.containing_impl(db)
        .map(|impl_| format!("impl${}", type_identity(db, impl_.ty(db))))
}

/// Generics-free identity of the HIR body itself: no substitution, effect, or
/// impl-environment components, and textual (fingerprint-free) impl context.
/// Every instantiation of the same body maps to the same identity.
pub fn body_owner_trace_identity<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> String {
    if let BodyOwner::Func(func, _) = owner {
        let owner_context = semantic_owner_context_trace_identity(db, owner).unwrap_or_default();
        format!("func${}${owner_context}", item_identity(db, func.into()))
    } else {
        body_owner_identity(db, owner)
    }
}

fn generic_subst_identity<'db>(db: &'db dyn HirAnalysisDb, subst: GenericSubst<'db>) -> String {
    generic_args_identity(db, subst.generic_args(db))
}

pub fn generic_args_identity<'db>(db: &'db dyn HirAnalysisDb, args: &[TyId<'db>]) -> String {
    args.iter()
        .map(|ty| type_identity(db, *ty))
        .collect::<Vec<_>>()
        .join("$")
}

fn effect_provider_subst_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    subst: EffectProviderSubst<'db>,
    mode: IdentityMode,
) -> String {
    subst
        .providers(db)
        .iter()
        .map(|provider| provider_binding_identity(db, provider, mode))
        .collect::<Vec<_>>()
        .join("$")
}

fn provider_binding_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    binding: &ProviderBinding<'db>,
    mode: IdentityMode,
) -> String {
    let ty = match mode {
        IdentityMode::Sort => format!("$ty${}", type_identity(db, binding.provider_ty)),
        IdentityMode::Symbol => String::new(),
    };
    format!(
        "idx${}{ty}$mut${}$source${}$semantics${}",
        binding.provider_idx,
        binding.is_mut,
        provider_source_identity(db, &binding.source, mode),
        provider_semantics_identity(db, &binding.semantics, mode)
    )
}

fn provider_source_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &ProviderSource<'db>,
    mode: IdentityMode,
) -> String {
    match source {
        ProviderSource::UsesParam {
            site,
            requirement_idx,
        } => format!(
            "uses_param${}${requirement_idx}",
            effect_param_site_identity(db, *site)
        ),
        ProviderSource::ContractField { field } => format!(
            "contract_field${}${}",
            item_identity(db, field.contract.into()),
            field.index
        ),
        ProviderSource::RootProvider {
            scope,
            registration,
        } => {
            let ty = match mode {
                IdentityMode::Sort => {
                    format!("$ty${}", type_identity(db, registration.provider_ty))
                }
                IdentityMode::Symbol => String::new(),
            };
            format!(
                "root_provider${}$idx${}$kind${:?}{ty}",
                root_provider_scope_identity(db, *scope),
                registration.idx,
                registration.site_kind
            )
        }
    }
}

fn provider_semantics_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    semantics: &hir::analysis::ty::ProviderSemantics<'db>,
    mode: IdentityMode,
) -> String {
    let target = semantics
        .target_ty
        .map(|ty| type_identity(db, ty))
        .unwrap_or_default();
    let ty = match mode {
        IdentityMode::Sort => format!("ty${}$", type_identity(db, semantics.provider_ty)),
        IdentityMode::Symbol => String::new(),
    };
    format!(
        "{ty}kind${:?}$space${:?}$target${target}$transport${:?}",
        semantics.kind, semantics.address_space, semantics.transport
    )
}

fn impl_env_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    env: ImplEnv<'db>,
    evidence: EvidenceList<'db>,
    mode: IdentityMode,
) -> String {
    let assumptions = trait_list_identity(db, env.assumptions(db).list(db).iter().copied(), mode);
    let witnesses = trait_list_identity(db, env.witnesses(db).iter().copied(), mode);
    // Callees normalize at an ingot root, and every ingot's root module has
    // the same name, so the scope must name its ingot.
    let scope = env.normalization_scope(db);
    let identity = format!(
        "scope${}${}$assumptions${assumptions}$witnesses${witnesses}",
        ingot_identity_for_top_mod(db, scope.top_mod(db)),
        module_path_components_for_scope(db, scope).join("$")
    );
    if evidence.is_ordinary(db) {
        return identity;
    }
    // Instances that differ only in non-ordinary evidence or evidence parameters
    // run different code.
    let mut parts = Vec::new();
    for (idx, entry) in evidence.entries(db).iter().enumerate() {
        if let Some(ev) = entry
            && !ev.is_ordinary(db)
        {
            parts.push(format!("bound{idx}={}", evidence_id_identity(db, *ev)));
        }
    }
    for &(param, ev) in evidence.params(db) {
        parts.push(format!("param{param}={}", evidence_id_identity(db, ev)));
    }
    format!("{identity}$evidence${}", parts.join("$"))
}

fn evidence_id_identity<'db>(db: &'db dyn HirAnalysisDb, ev: EvidenceId<'db>) -> String {
    use hir::analysis::ty::trait_def::ImplementorOrigin;
    match ev.data(db) {
        Evidence::Assumption(idx) => format!("assumption{idx}"),
        Evidence::Param(idx) => format!("param{idx}"),
        Evidence::Unsupplied => "unsupplied".to_string(),
        Evidence::Impl {
            selected,
            instantiated,
            conds,
        } => {
            let origin = match selected.origin(db) {
                ImplementorOrigin::Hir(impl_trait) => {
                    let name = impl_trait
                        .selection_name(db)
                        .map_or_else(|| "ordinary".to_string(), |name| name.data(db).to_string());
                    format!(
                        "impl${}${name}",
                        named_impl_evidence_identity(db, impl_trait)
                    )
                }
                ImplementorOrigin::Assumption => "assumption".to_string(),
                ImplementorOrigin::VirtualContract(_) => "virtual".to_string(),
                ImplementorOrigin::Callable => "callable".to_string(),
                ImplementorOrigin::Structural => "structural".to_string(),
            };
            let base = format!(
                "{origin}${}",
                trait_identity(db, instantiated.trait_inst(db))
            );
            if conds.is_empty() {
                base
            } else {
                let conds = conds
                    .iter()
                    .map(|c| evidence_id_identity(db, *c))
                    .collect::<Vec<_>>()
                    .join("$");
                format!("{base}$conds$[{conds}]")
            }
        }
    }
}

/// Local choices with equal names still denote different declarations. Keep
/// module-level identities unchanged, and name lexical parents without Salsa IDs.
fn named_impl_evidence_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: hir::hir_def::ImplTrait<'db>,
) -> String {
    let identity = item_identity(db, ItemKind::ImplTrait(impl_trait));
    if impl_trait.selection_name(db).is_none() {
        return identity;
    }
    let mut parents = Vec::new();
    let mut current = impl_trait.scope().parent(db);
    while let Some(scope) = current {
        match scope {
            ScopeId::Item(ItemKind::TopMod(_)) => break,
            ScopeId::Item(ItemKind::Mod(_)) => {}
            ScopeId::Item(ItemKind::Body(_)) => {
                let parent = scope.parent(db).expect("body has a lexical parent");
                let ordinal = scope
                    .scope_graph(db)
                    .children(parent)
                    .filter(|child| matches!(child, ScopeId::Item(ItemKind::Body(_))))
                    .position(|child| child == scope)
                    .expect("body is a child of its lexical parent");
                parents.push(format!("body{ordinal}"));
            }
            ScopeId::Item(ItemKind::Func(func)) => parents.push(format!(
                "{}${}",
                item_identity(db, func.into()),
                semantic_owner_context_identity(db, BodyOwner::Func(func, None))
                    .unwrap_or_default()
            )),
            ScopeId::Item(item) => parents.push(format!(
                "{}${}",
                item.kind_name(),
                item_name_component(db, item)
            )),
            ScopeId::Block(body, expression) => {
                parents.push(format!("block{}", body.iter_block(db)[&expression]));
            }
            _ => {}
        }
        current = scope.parent(db);
    }
    if parents.is_empty() {
        identity
    } else {
        parents.reverse();
        format!("{identity}$lexical${}", parents.join("$"))
    }
}

fn trait_list_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    insts: impl Iterator<Item = TraitInstId<'db>>,
    mode: IdentityMode,
) -> String {
    match mode {
        IdentityMode::Sort => insts
            .map(|inst| trait_identity(db, inst))
            .collect::<Vec<_>>()
            .join("$"),
        IdentityMode::Symbol => {
            let mut identities = insts
                .filter(|inst| !trait_inst_is_internal_effect_provider_constraint(db, *inst))
                .map(|inst| trait_identity(db, inst))
                .collect::<Vec<_>>();
            identities.sort();
            identities.join("$")
        }
    }
}

fn trait_inst_is_internal_effect_provider_constraint<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> bool {
    trait_inst_mentions_effect_provider_param(db, trait_inst)
        || trait_inst_is_effect_ref_marker(db, trait_inst)
}

fn trait_inst_is_effect_ref_marker<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> bool {
    let trait_def = trait_inst.def(db);
    let Some(name) = trait_def.name(db).to_opt() else {
        return false;
    };
    trait_def.top_mod(db).ingot(db).kind(db) == IngotKind::Core
        && matches!(name.data(db).as_str(), "EffectRef" | "EffectRefMut")
        && module_path_components_for_scope(db, trait_def.scope())
            .iter()
            .any(|component| component == "effect_ref")
}

fn trait_inst_mentions_effect_provider_param<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> bool {
    trait_ref_mentions_effect_provider_param(db, trait_inst.trait_ref(db))
        || trait_inst
            .assoc_type_bindings(db)
            .iter()
            .any(|(_, ty)| ty_mentions_effect_provider_param(db, *ty))
}

fn trait_ref_mentions_effect_provider_param<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_ref: TraitRefId<'db>,
) -> bool {
    trait_ref
        .args(db)
        .iter()
        .any(|ty| ty_mentions_effect_provider_param(db, *ty))
}

fn ty_mentions_effect_provider_param<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    if matches!(ty.data(db), TyData::TyApp(..)) {
        return ty_mentions_effect_provider_param(db, ty.base_ty(db))
            || ty
                .generic_args(db)
                .iter()
                .any(|arg| ty_mentions_effect_provider_param(db, *arg));
    }

    match ty.data(db) {
        TyData::TyParam(param) => param.is_effect_provider(),
        TyData::AssocTy(assoc) => trait_ref_mentions_effect_provider_param(db, assoc.trait_),
        TyData::QualifiedTy(trait_inst) | TyData::ConstraintTerm(trait_inst) => {
            trait_inst_mentions_effect_provider_param(db, *trait_inst)
        }
        TyData::TyVar(_)
        | TyData::TyBase(_)
        | TyData::ConstTy(_)
        | TyData::Never
        | TyData::Invalid(_) => false,
        TyData::TyApp(..) => unreachable!("TyApp handled before data match"),
    }
}

fn root_provider_scope_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: RootProviderScope<'db>,
) -> String {
    match scope {
        RootProviderScope::Func(func) => format!("func${}", item_identity(db, func.into())),
        RootProviderScope::Contract(contract) => {
            format!("contract${}", item_identity(db, contract.into()))
        }
    }
}

fn effect_param_site_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> String {
    match site {
        EffectParamSite::Func(func) => format!("func${}", item_identity(db, func.into())),
        EffectParamSite::Contract(contract) => {
            format!("contract${}", item_identity(db, contract.into()))
        }
        EffectParamSite::ContractInit { contract } => {
            format!("contract_init${}", item_identity(db, contract.into()))
        }
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "contract_recv${}${recv_idx}${arm_idx}",
            item_identity(db, contract.into())
        ),
        EffectParamSite::Closure(def) => format!(
            "closure${}${}",
            closure_owner_identity(db, def.body),
            def.expr.as_u32()
        ),
    }
}

/// The owner of the body a closure is in: closures are nameless.
fn closure_owner_identity<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> String {
    BodyOwner::from_body(db, body).map_or_else(
        || module_path_components_for_scope(db, body.scope()).join("$"),
        |owner| body_owner_identity(db, owner),
    )
}

pub fn type_identity<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> String {
    if matches!(ty.data(db), TyData::TyApp(..)) {
        let base = type_identity(db, ty.base_ty(db));
        let args = ty
            .generic_args(db)
            .iter()
            .map(|arg| type_identity(db, *arg))
            .collect::<Vec<_>>()
            .join("$");
        return format!("app${base}${args}");
    }

    match ty.data(db) {
        TyData::TyBase(base) => match base {
            TyBase::Prim(prim) => format!("prim${prim:?}"),
            TyBase::Adt(adt) => format!("adt${}", item_identity(db, adt.adt_ref(db).as_item())),
            TyBase::Contract(contract) => {
                format!("contract${}", item_identity(db, (*contract).into()))
            }
            TyBase::Func(callable) => match callable {
                CallableDef::Func(func) => format!("func${}", item_identity(db, (*func).into())),
                CallableDef::VariantCtor(variant) => format!(
                    "variant_ctor${}${}",
                    item_identity(db, variant.enum_.into()),
                    variant.name(db).unwrap_or("variant")
                ),
                CallableDef::StructCtor(struct_) => {
                    format!("struct_ctor${}", item_identity(db, (*struct_).into()))
                }
            },
            TyBase::Closure(closure) => {
                // Closures are nameless: the owner of the body defining one
                // and its position there identify it.
                let def = closure.def(db);
                let owner = closure_owner_identity(db, def.body);
                let tys = closure
                    .parent_args(db)
                    .iter()
                    .chain(closure.captures(db))
                    .chain(&closure.param_tys(db))
                    .chain([closure.ret_ty(db)].iter())
                    .map(|ty| type_identity(db, *ty))
                    .collect::<Vec<_>>()
                    .join("$");
                format!(
                    "closure${owner}${}${:?}${tys}",
                    def.expr.as_u32(),
                    closure.modes(db)
                )
            }
        },
        TyData::TyParam(param) => {
            format!(
                "param${}${}",
                item_identity(db, param.owner.item()),
                param.idx
            )
        }
        TyData::AssocTy(assoc) => format!(
            "assoc${}${}",
            trait_ref_identity(db, assoc.trait_),
            assoc.name.data(db)
        ),
        TyData::QualifiedTy(trait_inst) => format!("qualified${}", trait_identity(db, *trait_inst)),
        TyData::ConstraintTerm(goal) => format!("constraint${}", trait_identity(db, *goal)),
        TyData::ConstTy(const_ty) => {
            format!(
                "const_ty${}",
                stable_identity_fingerprint(&const_ty.pretty_print_concrete(db))
            )
        }
        TyData::Never => "never".to_string(),
        TyData::TyVar(_) => format!("var${}", stable_identity_fingerprint(ty.pretty_print(db))),
        TyData::Invalid(cause) => {
            format!(
                "invalid${}",
                stable_identity_fingerprint(&cause.pretty_print(db))
            )
        }
        TyData::TyApp(..) => unreachable!("TyApp handled before data match"),
    }
}

fn trait_identity<'db>(db: &'db dyn HirAnalysisDb, trait_inst: TraitInstId<'db>) -> String {
    let trait_ref = trait_ref_identity(db, trait_inst.trait_ref(db));
    let assoc_types = trait_inst
        .assoc_type_bindings(db)
        .iter()
        .map(|(name, ty)| format!("{}${}", name.data(db), type_identity(db, *ty)))
        .collect::<Vec<_>>()
        .join("$");
    format!("{trait_ref}$assoc${assoc_types}")
}

fn trait_ref_identity<'db>(db: &'db dyn HirAnalysisDb, trait_ref: TraitRefId<'db>) -> String {
    let args = trait_ref
        .args(db)
        .iter()
        .map(|arg| type_identity(db, *arg))
        .collect::<Vec<_>>()
        .join("$");
    format!(
        "{}$args${args}",
        item_identity(db, trait_ref.def(db).into())
    )
}

pub fn item_identity<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> String {
    let top_mod = item.top_mod(db);
    format!(
        "{}${}${}${}",
        ingot_identity_for_top_mod(db, top_mod),
        module_path_components_for_scope(db, item.scope()).join("$"),
        item.kind_name(),
        item_name_component(db, item)
    )
}

fn item_name_component<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> String {
    item.name(db)
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| item.kind_name().replace(' ', "_"))
}

pub fn module_path_components_for_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> Vec<String> {
    let mut modules = Vec::new();
    let mut current = scope.parent_module(db);
    while let Some(module_scope) = current {
        match module_scope.item() {
            ItemKind::TopMod(top_mod) => modules.push(top_mod.name(db).data(db).to_string()),
            ItemKind::Mod(module) => modules.push(
                module
                    .name(db)
                    .to_opt()
                    .map(|name| name.data(db).to_string())
                    .unwrap_or_else(|| "mod".to_string()),
            ),
            _ => {}
        }
        current = module_scope.parent_module(db);
    }
    if modules.is_empty() {
        modules.push(scope.top_mod(db).name(db).data(db).to_string());
    }
    modules.reverse();
    modules
}

pub fn ingot_component_for_scope<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> String {
    let top_mod = scope.top_mod(db);
    ingot_logical_name(db, top_mod.ingot(db), top_mod)
}

fn ingot_identity_for_top_mod<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> String {
    let ingot = top_mod.ingot(db);
    format!(
        "{:?}${}",
        ingot.kind(db),
        ingot_logical_name(db, ingot, top_mod)
    )
}

fn ingot_logical_name<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    top_mod: TopLevelMod<'db>,
) -> String {
    ingot
        .config(db)
        .and_then(|config| config.metadata.name.map(|name| name.to_string()))
        .unwrap_or_else(|| match ingot.kind(db) {
            IngotKind::Core => "core".to_string(),
            IngotKind::Std => "std".to_string(),
            IngotKind::StandAlone => format!("standalone${}", top_mod.name(db).data(db)),
            IngotKind::Local => format!("local${}", top_mod.name(db).data(db)),
            IngotKind::External => format!("external${}", top_mod.name(db).data(db)),
        })
}

/// The symbol name of a closure's body called with `receiver`.
pub fn closure_symbol_component<'db>(
    db: &'db dyn HirAnalysisDb,
    closure: ClosureTy<'db>,
    receiver: FuncParamMode,
) -> String {
    let kind = match receiver {
        FuncParamMode::Mut => "closure_mut",
        FuncParamMode::View | FuncParamMode::Own => "closure",
    };
    format!(
        "__{kind}_{}",
        stable_identity_fingerprint(&type_identity(db, TyId::closure(db, closure)))
    )
}

pub fn stable_identity_fingerprint(value: &str) -> String {
    let mut fingerprint = 0xcbf29ce484222325u64;
    for byte in value.bytes() {
        fingerprint ^= u64::from(byte);
        fingerprint = fingerprint.wrapping_mul(0x100000001b3);
    }
    format!("{fingerprint:016x}")
}

#[cfg(test)]
mod tests {
    use super::semantic_instance_identity;
    use hir::{
        analysis::{
            semantic::{get_or_build_semantic_instance, root_semantic_instance_key},
            ty::ty_check::BodyOwner,
        },
        hir_def::ItemKind,
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn stable_key_distinguishes_named_evidence_and_shares_ordinary_instances() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "stable_key_evidence_identity.fe".into(),
            r#"
trait Inner {
    const fn inner() -> u256
}
impl Inner for u256 {
    const fn inner() -> u256 { 10 }
}
impl Inner for u256 as AltInner20 {
    const fn inner() -> u256 { 20 }
}
impl Inner for u256 as AltInner30 {
    const fn inner() -> u256 { 30 }
}

struct Boxed<T> {
    pub x: T,
}
trait Outer {
    const fn outer(self) -> u256
}
impl<T: Inner> Outer for Boxed<T> {
    const fn outer(self) -> u256 { T::inner() }
}

const fn helper<T: Inner>(_ m: Boxed<T>) -> u256 {
    m.outer()
}

const fn call_before() -> u256 {
    helper(Boxed { x: 1 as u256 })
}
const fn call_alt20() -> u256 {
    with (AltInner20) {
        helper(Boxed { x: 1 as u256 })
    }
}
const fn call_alt30() -> u256 {
    with (AltInner30) {
        helper(Boxed { x: 1 as u256 })
    }
}
const fn call_after() -> u256 {
    helper(Boxed { x: 1 as u256 })
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let func = |name: &str| {
            top_mod
                .children_non_nested(&db)
                .find_map(|item| match item {
                    ItemKind::Func(f)
                        if f.name(&db).to_opt().is_some_and(|n| n.data(&db) == name) =>
                    {
                        Some(f)
                    }
                    _ => None,
                })
                .unwrap()
        };
        let helper_fn = func("helper");
        let helper_key_in = |caller_name: &str| {
            let caller = func(caller_name);
            let inst = get_or_build_semantic_instance(
                &db,
                root_semantic_instance_key(&db, BodyOwner::Func(caller, None)).unwrap(),
            );
            let key = inst
                .callees(&db)
                .iter()
                .find(|c| c.key.owner(&db) == BodyOwner::Func(helper_fn, None))
                .unwrap()
                .key;
            get_or_build_semantic_instance(&db, key)
        };

        let id_before = semantic_instance_identity(&db, helper_key_in("call_before"));
        let id_alt20 = semantic_instance_identity(&db, helper_key_in("call_alt20"));
        let id_alt30 = semantic_instance_identity(&db, helper_key_in("call_alt30"));
        let id_after = semantic_instance_identity(&db, helper_key_in("call_after"));

        assert_eq!(id_before, id_after);
        assert_ne!(id_before, id_alt20);
        assert_ne!(id_before, id_alt30);
        assert_ne!(id_alt20, id_alt30);
    }

    #[test]
    fn stable_keys_distinguish_same_named_choices_in_lexical_scopes() {
        use super::semantic_instance_symbol_identity;
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "local_choice_identity.fe".into(),
            r#"
trait Pick { fn pick(self) -> u256 }
impl Pick for u256 { fn pick(self) -> u256 { 1 } }
fn helper<T: Pick>(x: T) -> u256 { x.pick() }
fn first() -> u256 {
    impl Pick for u256 as Local { fn pick(self) -> u256 { 20 } }
    with (Local) { helper(0 as u256) }
}
fn second() -> u256 {
    impl Pick for u256 as Local { fn pick(self) -> u256 { 30 } }
    with (Local) { helper(0 as u256) }
}
fn sibling_blocks() -> u256 {
    let a = {
        impl Pick for u256 as Local { fn pick(self) -> u256 { 40 } }
        with (Local) { helper(0 as u256) }
    }
    let b = {
        impl Pick for u256 as Local { fn pick(self) -> u256 { 50 } }
        with (Local) { helper(0 as u256) }
    }
    a + b
}
fn ordinary() -> u256 { helper(0 as u256) + helper(1 as u256) }
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let function = |name: &str| {
            top_mod
                .children_non_nested(&db)
                .find_map(|item| match item {
                    ItemKind::Func(func)
                        if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == name) =>
                    {
                        Some(func)
                    }
                    _ => None,
                })
                .unwrap()
        };
        let helper = function("helper");
        let instances = |name: &str| {
            let root = get_or_build_semantic_instance(
                &db,
                root_semantic_instance_key(&db, BodyOwner::Func(function(name), None)).unwrap(),
            );
            root.callees(&db)
                .iter()
                .filter(|callee| callee.key.owner(&db) == BodyOwner::Func(helper, None))
                .map(|callee| get_or_build_semantic_instance(&db, callee.key))
                .collect::<Vec<_>>()
        };
        let mut choices = instances("first");
        assert_eq!(choices.len(), 1);
        let second = instances("second");
        assert_eq!(second.len(), 1);
        choices.extend(second);
        let siblings = instances("sibling_blocks");
        assert_eq!(siblings.len(), 2);
        choices.extend(siblings);
        for (index, choice) in choices.iter().enumerate() {
            for other in &choices[index + 1..] {
                assert_ne!(
                    semantic_instance_identity(&db, *choice),
                    semantic_instance_identity(&db, *other)
                );
                assert_ne!(
                    semantic_instance_symbol_identity(&db, *choice),
                    semantic_instance_symbol_identity(&db, *other)
                );
            }
        }
        let ordinary = instances("ordinary");
        assert!(!ordinary.is_empty());
        for instance in &ordinary {
            assert_eq!(
                semantic_instance_identity(&db, ordinary[0]),
                semantic_instance_identity(&db, *instance)
            );
            assert_eq!(
                semantic_instance_symbol_identity(&db, ordinary[0]),
                semantic_instance_symbol_identity(&db, *instance)
            );
        }
    }

    #[test]
    fn named_choice_identity_distinguishes_contract_receive_bodies() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "recv_choice_identity.fe".into(),
            r#"
use std::abi::sol
trait Pick { fn pick(self) -> u256 }
msg Reads {
    #[selector = sol("first()")]
    First -> u256,
    #[selector = sol("second()")]
    Second -> u256,
}
pub contract Reader {
    recv Reads {
        First -> u256 {
            impl Pick for u256 as Local { fn pick(self) -> u256 { 20 } }
            with (Local) { (0 as u256).pick() }
        }
        Second -> u256 {
            impl Pick for u256 as Local { fn pick(self) -> u256 { 30 } }
            with (Local) { (0 as u256).pick() }
        }
    }
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let choices = top_mod
            .children_nested(&db)
            .filter_map(|item| match item {
                ItemKind::ImplTrait(impl_) if impl_.selection_name(&db).is_some() => Some(impl_),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(choices.len(), 2);
        assert_ne!(
            super::named_impl_evidence_identity(&db, choices[0]),
            super::named_impl_evidence_identity(&db, choices[1])
        );
    }
}
