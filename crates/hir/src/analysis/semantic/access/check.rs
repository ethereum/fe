//! The access analysis of one normalized body.
//!
//! An access is open from the statement that creates it to the `end` that
//! liveness elaboration placed after its last use: a borrow or view of a
//! place, or a projection call's session. A session reserves the places its
//! arguments name and the domains its `uses` clause names, and grants one
//! access per yielded component. Every operation is checked against the
//! accesses open around it: overlapping accesses conflict unless both read,
//! except that an access through a carrier is authorized by the accesses the
//! carrier was derived from.
use std::collections::BTreeSet;

use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use super::{
    domain::Domains,
    place::{AbsPlace, Base, Path, Step, path_of, paths_overlap},
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SemOrigin, SemanticInstance,
            access::{control::semantic_may_return, projection_result_spaces},
            capability::semantics::{CapabilityClass, capability_semantics, field_handle_spaces},
            definite_assignment::literal_index,
            diagnostics::{
                SemanticDiagnostic, SemanticDiagnosticKind, SemanticDiagnosticSpan, operand_origin,
            },
            get_or_build_semantic_instance,
            normalized::{
                HandleOrigin, NBlockId, NDataPath, NEffectArg, NEffectArgValue, NExpr, NOperand,
                NPlace, NPlaceBase, NRootKind, NStatement, NStatementKind, NStructuralPath,
                NTerminatorKind, NValueDefinition, NValueId, NormalizedBody, ReadMode,
                access::AccessTarget,
            },
        },
        ty::{
            corelib::{MemoryAccessKind, effect_key_state_access, external_call_state_access},
            provider::{ProviderAddressSpace, provider_semantics},
            shape::Shape,
            trait_resolution::PredicateListId,
            ty_check::{BodyOwner, LocalBinding},
            ty_def::{BorrowKind, CapabilityKind, TyId},
            ty_is_snapshot,
        },
    },
    hir_def::{CallableDef, FuncParamMode, scope_graph::ScopeId},
    semantic::{EffectRequirementKey, ProviderSource},
};

type Diag<'db> = SemanticDiagnostic<'db>;
pub(super) type TokenId = u32;
type TokenSet = SmallVec<TokenId, 4>;

/// The tokens a value holds, each at the path of the value where its carrier
/// sits: a projection's tuple or sum shape holds one grant per component.
pub(super) type Held = BTreeSet<(TokenId, Path)>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TokenKind {
    /// A data parameter, open for the whole body.
    Input,
    /// A borrow or view this body opened.
    Access,
    /// A session's reservation of an argument or effect domain.
    Reservation,
    /// A component a session grants.
    Grant,
    /// A provider handle: names a domain, confers no exclusivity.
    Handle,
}

#[derive(Clone, Debug)]
pub(super) struct Token<'db> {
    kind: TokenKind,
    mode: BorrowKind,
    pub regions: Vec<AbsPlace>,
    /// The tokens this one was derived through, which authorize it.
    parents: TokenSet,
    origin: SemOrigin<'db>,
    /// An access of a zero-sized place protects no data.
    zero_sized: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum MoveKey {
    Value(NValueId),
    Root(u32),
    Param(u32),
    /// The referent of a `mut` access, which may hold a hole while the
    /// access is open.
    Access(TokenId),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct State {
    /// Accesses, reservations and grants opened and not yet ended.
    open: BTreeSet<TokenId>,
    /// Possibly moved (or uninitialized) values, roots and parameter paths.
    moved: BTreeSet<(MoveKey, Path)>,
}

impl State {
    fn join(&mut self, other: &Self) -> bool {
        let before = (self.open.len(), self.moved.len());
        self.open.extend(other.open.iter().copied());
        self.moved.extend(other.moved.iter().cloned());
        before != (self.open.len(), self.moved.len())
    }
}

/// The places an effect of a call reaches, with the mode it takes and the
/// tokens of the carrier its place is reached through.
struct Footprint {
    regions: Vec<AbsPlace>,
    mode: BorrowKind,
    parents: TokenSet,
}

/// Where an access through a place goes, and which tokens authorize it.
pub(super) struct Resolved {
    pub regions: Vec<AbsPlace>,
    /// The tokens of the carrier the place is reached through.
    direct: TokenSet,
    authority: TokenSet,
}

/// The kinds of a statement's token creation sites, for stable identities
/// across fixed-point iterations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Site {
    Open,
    Handle,
    Effect(usize),
    Arg(usize),
    Grant(usize),
}

pub(super) struct Analysis<'a, 'db> {
    pub db: &'db dyn HirAnalysisDb,
    pub instance: SemanticInstance<'db>,
    pub body: &'a NormalizedBody<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    param_count: u32,
    pub domains: Domains<'db>,
    /// The domain of each provider root.
    root_domain: Vec<Option<u32>>,
    pub tokens: Vec<Token<'db>>,
    pub values: Vec<Held>,
    /// Tokens each root's contents may hold.
    contents: Vec<Held>,
    sites: FxHashMap<(NValueId, Site), TokenId>,
    /// The tokens the statement defining a value opens, which its `end`
    /// closes.
    opened: FxHashMap<NValueId, TokenSet>,
    /// The values whose accesses an `end` closes. Normalization opens
    /// others for a single call, which closes them.
    ended: FxHashSet<NValueId>,
    /// The projection each session calls.
    sessions: FxHashMap<NValueId, SemanticInstance<'db>>,
    entry: Vec<Option<State>>,
    moved_at: FxHashMap<(MoveKey, Path), SemOrigin<'db>>,
    /// The statement at which each block diverges into a call that never returns.
    divergence: Vec<Option<usize>>,
    /// Whether transfer functions report violations.
    checking: bool,
}

impl<'a, 'db> Analysis<'a, 'db> {
    /// `divergence` cuts each block at a call that never returns. Provisional
    /// analyses run while callee instances are still being built, so they
    /// keep every path.
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedBody<'db>,
        divergence: bool,
    ) -> Self {
        let typed_body = instance.key(db).typed_body(db);
        let param_count = (0..)
            .take_while(|index| typed_body.param_binding(*index).is_some())
            .count() as u32;
        let mut analysis = Self {
            db,
            instance,
            body,
            scope: instance.key(db).impl_env(db).normalization_scope(db),
            assumptions: instance.assumptions(db),
            param_count,
            domains: Domains::default(),
            root_domain: vec![None; body.roots.len()],
            tokens: Vec::new(),
            values: vec![Held::new(); body.values.len()],
            contents: vec![Held::new(); body.roots.len()],
            sites: FxHashMap::default(),
            opened: FxHashMap::default(),
            ended: body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .filter_map(|statement| match statement.kind {
                    NStatementKind::End { access } => Some(access),
                    _ => None,
                })
                .collect(),
            sessions: FxHashMap::default(),
            entry: vec![None; body.blocks.len()],
            moved_at: FxHashMap::default(),
            divergence: vec![None; body.blocks.len()],
            checking: false,
        };
        if divergence {
            for (block, data) in body.blocks.iter().enumerate() {
                analysis.divergence[block] =
                    data.statements
                        .iter()
                        .position(|statement| match &statement.kind {
                            NStatementKind::Define {
                                expr: NExpr::Call { callee, .. },
                                ..
                            } => !semantic_may_return(
                                db,
                                get_or_build_semantic_instance(db, callee.key),
                            ),
                            _ => false,
                        });
            }
        }
        for (index, root) in body.roots.iter().enumerate() {
            if let NRootKind::Provider { binding } = &root.kind {
                analysis.root_domain[index] = Some(analysis.domains.provider(binding));
            }
        }
        analysis.seed_entry();
        analysis
    }

    fn token(&mut self, value: NValueId, site: Site, token: Token<'db>) -> TokenId {
        if let Some(&id) = self.sites.get(&(value, site)) {
            return id;
        }
        self.tokens.push(token);
        let id = (self.tokens.len() - 1) as TokenId;
        self.sites.insert((value, site), id);
        if matches!(
            self.tokens[id as usize].kind,
            TokenKind::Access | TokenKind::Reservation | TokenKind::Grant
        ) {
            self.opened.entry(value).or_default().push(id);
        }
        id
    }

    fn new_token(&self, kind: TokenKind, mode: BorrowKind, origin: SemOrigin<'db>) -> Token<'db> {
        Token {
            kind,
            mode,
            regions: Vec::new(),
            parents: TokenSet::new(),
            origin,
            zero_sized: false,
        }
    }

    /// The address space of data parameter `param`'s place in this
    /// instantiation.
    fn param_space(&self, param: u32) -> ProviderAddressSpace {
        self.instance.param_space(self.db, param)
    }

    /// The domain a handle of `ty` names when its provider is unknown.
    fn dynamic_handle(&mut self, ty: TyId<'db>) -> Option<u32> {
        matches!(
            capability_semantics(self.db, self.scope, self.assumptions, ty),
            Ok(Some(semantics)) if semantics.class == CapabilityClass::Handle
        )
        .then(|| {
            let space = provider_semantics(self.db, self.scope, self.assumptions, ty).address_space;
            self.domains.dynamic(space)
        })
    }

    fn seed_entry(&mut self) {
        let body = self.body;
        for (index, value) in body.values.iter().enumerate() {
            let NValueDefinition::EntryParam { param } = value.definition else {
                continue;
            };
            let value_id = NValueId::new(index);
            let origin = SemOrigin::Body(body.template_owner);
            let held = if let Some((kind, target)) = value.ty.as_capability(self.db)
                && param < self.param_count
            {
                let mode = match kind {
                    CapabilityKind::Mut => BorrowKind::Mut,
                    CapabilityKind::Ref | CapabilityKind::View => BorrowKind::Ref,
                };
                let mut input = self.new_token(TokenKind::Input, mode, origin);
                input.regions.push(AbsPlace::new(Base::Param(param)));
                input.zero_sized = target.is_zero_sized(self.db);
                self.token(value_id, Site::Open, input)
            } else {
                // An effect handle names its provider's domain.
                let domain = match value.source {
                    Some(LocalBinding::EffectParam { site, idx, .. }) => Some(
                        self.domains.provider_source(
                            ProviderSource::UsesParam {
                                site,
                                requirement_idx: idx as u32,
                            },
                            provider_semantics(self.db, self.scope, self.assumptions, value.ty)
                                .address_space,
                        ),
                    ),
                    _ => self.dynamic_handle(value.ty),
                };
                let Some(domain) = domain else {
                    continue;
                };
                let mut handle = self.new_token(TokenKind::Handle, BorrowKind::Mut, origin);
                handle.regions.push(AbsPlace::new(Base::Domain(domain)));
                self.token(value_id, Site::Handle, handle)
            };
            self.values[index] = Held::from([(held, Path::new())]);
        }
        let mut state = State::default();
        // Entry bindings, such as recv arm fields, initialize their slots. The
        // provisional body has the same entry locals and does not depend on
        // the call-site refinements this analysis may be computing.
        let raw = self.instance.provisional_body(self.db);
        let entry_bindings: Vec<_> = raw
            .entry_locals
            .iter()
            .filter_map(|local| raw.locals[local.index()].source)
            .collect();
        for (index, root) in body.roots.iter().enumerate() {
            if let NRootKind::LocalSlot { binding } = &root.kind
                && !binding.is_some_and(|binding| entry_bindings.contains(&binding))
            {
                state
                    .moved
                    .insert((MoveKey::Root(index as u32), Path::new()));
            }
        }
        self.entry[body.entry.index()] = Some(state);
    }

    /// The steps of a normalized path, resolving literal index values.
    fn path(&self, path: &NDataPath) -> Path {
        path_of(path, |value| literal_index(self.db, self.body, value))
    }

    /// `tokens` and every token they were derived from.
    fn ancestors(&self, tokens: &[TokenId]) -> TokenSet {
        let mut closure: TokenSet = tokens.iter().copied().collect();
        let mut pending = closure.to_vec();
        while let Some(token) = pending.pop() {
            for &parent in &self.tokens[token as usize].parents {
                if !closure.contains(&parent) {
                    closure.push(parent);
                    pending.push(parent);
                }
            }
        }
        closure.sort_unstable();
        closure
    }

    /// The tokens a value's carrier holds at its own position.
    fn direct(&self, value: NValueId) -> TokenSet {
        self.values[value.index()]
            .iter()
            .filter(|(_, rest)| rest.is_empty())
            .map(|(token, _)| *token)
            .collect()
    }

    /// The places a carrier argument names.
    pub fn passed_regions(&self, value: NValueId) -> Vec<AbsPlace> {
        self.passed(value)
            .iter()
            .flat_map(|token| self.tokens[*token as usize].regions.clone())
            .collect()
    }

    /// The accesses a carrier argument for a parameter of `mode` passes. A
    /// handle opens none, and a snapshot view argument is passed by copy.
    fn passed_to(&self, value: NValueId, mode: FuncParamMode) -> TokenSet {
        let ty = self.body.values[value.index()].ty;
        let ty = ty.as_capability(self.db).map_or(ty, |(_, target)| target);
        if mode == FuncParamMode::View && ty_is_snapshot(self.db, self.scope, ty, self.assumptions)
        {
            return TokenSet::new();
        }
        self.passed(value)
    }

    /// The accesses a carrier argument passes: passing a handle opens none.
    fn passed(&self, value: NValueId) -> TokenSet {
        self.direct(value)
            .into_iter()
            .filter(|token| self.tokens[*token as usize].kind != TokenKind::Handle)
            .collect()
    }

    pub fn resolve(&mut self, place: &NPlace<'db>) -> Resolved {
        let path = self.path(&place.path);
        let (regions, direct) = match place.base {
            NPlaceBase::Root(root) => {
                let base = self.root_domain[root.index()].map_or(Base::Root(root), Base::Domain);
                (vec![AbsPlace { base, path }], TokenSet::new())
            }
            NPlaceBase::CapabilityTarget { carrier } => {
                let carrier_ty = self.body.values[carrier.index()].ty;
                let direct = self.direct(carrier);
                let mut regions: Vec<AbsPlace> = direct
                    .iter()
                    .flat_map(|token| &self.tokens[*token as usize].regions)
                    .map(|region| region.extended(&path))
                    .collect();
                if regions.is_empty() || carrier_ty.as_ptr(self.db).is_some() {
                    let base = self
                        .dynamic_handle(carrier_ty)
                        .filter(|_| carrier_ty.as_ptr(self.db).is_none())
                        .map_or(Base::Raw, Base::Domain);
                    regions = vec![AbsPlace { base, path }];
                }
                regions.sort();
                regions.dedup();
                (regions, direct)
            }
        };
        Resolved {
            regions,
            authority: self.ancestors(&direct),
            direct,
        }
    }

    pub fn space(&self, base: Base) -> Option<ProviderAddressSpace> {
        match base {
            Base::Root(root) => Some(self.body.roots[root.index()].address_space),
            Base::Param(param) => Some(self.param_space(param)),
            Base::Domain(domain) => self.domains.space(domain),
            Base::State(space) => Some(space),
            // A grant lies in the space its projection's contract exports.
            Base::Grant { session, component } => {
                projection_result_spaces(self.db, *self.sessions.get(&session)?)
                    .get(component as usize)
                    .copied()
                    .flatten()
            }
            Base::Raw => Some(ProviderAddressSpace::Memory),
        }
    }

    /// Whether two abstract places may share storage. Raw memory is
    /// unchecked; a grant is covered by its session's reservations.
    fn overlaps(&self, lhs: &AbsPlace, rhs: &AbsPlace) -> bool {
        match (lhs.base, rhs.base) {
            (Base::Raw, _) | (_, Base::Raw) => false,
            (Base::State(space), other) | (other, Base::State(space)) => match other {
                Base::Grant { .. } => false,
                Base::Domain(domain) => self
                    .domains
                    .space(domain)
                    .is_none_or(|known| known == space),
                other => self.space(other) == Some(space),
            },
            (Base::Domain(lhs_domain), Base::Domain(rhs_domain)) if lhs_domain != rhs_domain => {
                self.domains.may_alias(lhs_domain, rhs_domain)
            }
            (lhs_base, rhs_base) => lhs_base == rhs_base && paths_overlap(&lhs.path, &rhs.path),
        }
    }

    fn any_overlap(&self, lhs: &[AbsPlace], rhs: &[AbsPlace]) -> bool {
        lhs.iter()
            .any(|lhs| rhs.iter().any(|rhs| self.overlaps(lhs, rhs)))
    }

    // --- value flow -------------------------------------------------------

    fn union(&mut self, value: NValueId, held: Held, changed: &mut bool) {
        let target = &mut self.values[value.index()];
        let before = target.len();
        target.extend(held);
        *changed |= target.len() != before;
    }

    /// What a value holds below `prefix`.
    fn prefixed(held: &Held, prefix: Step) -> Held {
        held.iter()
            .map(|(token, rest)| {
                let path = [prefix].into_iter().chain(rest.iter().copied()).collect();
                (*token, path)
            })
            .collect()
    }

    /// What the part of a value at `path` holds.
    fn projected(held: &Held, path: &[Step]) -> Held {
        held.iter()
            .filter(|(_, rest)| paths_overlap(rest, path))
            .map(|(token, rest)| {
                let rest = rest.get(path.len()..).unwrap_or_default();
                (*token, rest.iter().copied().collect())
            })
            .collect()
    }

    fn root_contents(&self, root: usize) -> Held {
        let mut held = self.contents[root].clone();
        if let NRootKind::CapabilityRepresentation { carrier: value }
        | NRootKind::Temporary { value } = self.body.roots[root].kind
        {
            held.extend(self.values[value.index()].iter().cloned());
        }
        held
    }

    /// Run the flow-insensitive fixed point over the tokens values and root
    /// contents hold, then the forward fixed point over open accesses and
    /// moves.
    pub fn solve(&mut self) {
        loop {
            let before: Vec<_> = self
                .tokens
                .iter()
                .map(|token| token.regions.clone())
                .collect();
            let mut changed = false;
            for block in 0..self.body.blocks.len() {
                let block = NBlockId::new(block);
                let statements: Vec<_> = self.statements(block).collect();
                for (_, statement) in statements {
                    self.flow(statement, &mut changed);
                }
                self.flow_terminator(block, &mut changed);
            }
            let regions_changed = self.tokens.len() != before.len()
                || self
                    .tokens
                    .iter()
                    .zip(&before)
                    .any(|(token, before)| token.regions != *before);
            if !changed && !regions_changed {
                break;
            }
        }
        let order = self.reverse_postorder();
        loop {
            let mut changed = false;
            for &block in &order {
                if let Some(state) = self.run_block(block) {
                    changed |= self.propagate(block, &state);
                }
            }
            if !changed {
                break;
            }
        }
    }

    fn flow(&mut self, statement: &NStatement<'db>, changed: &mut bool) {
        match &statement.kind {
            NStatementKind::End { .. } => {}
            NStatementKind::Store { destination, value } => {
                let held = self.values[value.value.index()].clone();
                if held.is_empty() {
                    return;
                }
                let path = self.path(&destination.path);
                for region in self.resolve(destination).regions {
                    if let Base::Root(root) = region.base {
                        let contents = &mut self.contents[root.index()];
                        let before = contents.len();
                        contents.extend(held.iter().map(|(token, rest)| {
                            let mut full = path.clone();
                            full.extend_from_slice(rest);
                            (*token, full)
                        }));
                        *changed |= contents.len() != before;
                    }
                }
            }
            NStatementKind::Define { result, expr } => {
                let held = self.define(*result, statement, expr);
                self.union(*result, held, changed);
            }
        }
    }

    fn flow_terminator(&mut self, block: NBlockId, changed: &mut bool) {
        if self.diverges(block) {
            return;
        }
        for successor in self.body.blocks[block.index()].terminator.kind.successors() {
            let target = &self.body.blocks[successor.block.index()];
            for (param, arg) in target.params.iter().zip(&successor.args) {
                let held = self.values[arg.value.index()].clone();
                self.union(*param, held, changed);
            }
        }
    }

    /// The tokens a defining statement's result holds, creating the tokens
    /// it opens.
    fn define(&mut self, result: NValueId, statement: &NStatement<'db>, expr: &NExpr<'db>) -> Held {
        let origin = statement.origin;
        let of = |this: &Self, operand: &NOperand| this.values[operand.value.index()].clone();
        match expr {
            NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => of(self, src),
            NExpr::ProjectValue { value, path } => {
                Self::projected(&of(self, value), &self.path(&path.0))
            }
            NExpr::ArrayRepeat { value, .. } => Self::prefixed(&of(self, value), Step::Index(None)),
            NExpr::AggregateMake { fields, .. } => fields
                .iter()
                .enumerate()
                .flat_map(|(field, operand)| {
                    Self::prefixed(&of(self, operand), Step::Field(field as u16))
                })
                .collect(),
            NExpr::EnumMake {
                variant, fields, ..
            } => fields
                .iter()
                .enumerate()
                .flat_map(|(field, operand)| {
                    Self::prefixed(
                        &of(self, operand),
                        Step::Variant {
                            variant: variant.0,
                            field: field as u16,
                        },
                    )
                })
                .collect(),
            NExpr::Load { place, .. } => {
                let mut loaded = Held::new();
                for region in self.resolve(place).regions {
                    if let Base::Root(root) = region.base {
                        loaded.extend(Self::projected(
                            &self.root_contents(root.index()),
                            &region.path,
                        ));
                    }
                }
                if loaded.is_empty() {
                    self.unknown_handle(result, origin)
                } else {
                    loaded
                }
            }
            // A borrow no `end` closes is not an access of its own: through
            // a carrier it names part of the carrier's access, and otherwise
            // it is normalization's view of a call argument, open for that
            // call. A snapshot argument is passed by copy: its read is the
            // whole access.
            NExpr::Borrow { place, .. } | NExpr::MakeView { place, .. }
                if !self.ended.contains(&result) =>
            {
                match place.base {
                    NPlaceBase::CapabilityTarget { carrier } => self
                        .direct(carrier)
                        .into_iter()
                        .map(|token| (token, Path::new()))
                        .collect(),
                    NPlaceBase::Root(_)
                        if ty_is_snapshot(self.db, self.scope, place.ty, self.assumptions) =>
                    {
                        Held::new()
                    }
                    NPlaceBase::Root(_) => {
                        let mode = match expr {
                            NExpr::Borrow { kind, .. } => *kind,
                            _ => BorrowKind::Ref,
                        };
                        self.open_access(result, origin, place, mode)
                    }
                }
            }
            NExpr::Borrow { place, kind, .. } => self.open_access(result, origin, place, *kind),
            NExpr::MakeView { place, .. } => {
                self.open_access(result, origin, place, BorrowKind::Ref)
            }
            NExpr::MakeHandle {
                fields,
                origin: handle_origin,
                ty,
                ..
            } => {
                let domain = match handle_origin {
                    HandleOrigin::Provider(binding) => Some(self.domains.provider(binding)),
                    HandleOrigin::Opaque(_) => self.dynamic_handle(*ty),
                };
                let mut held: Held = fields
                    .iter()
                    .enumerate()
                    .flat_map(|(field, operand)| {
                        Self::prefixed(&of(self, operand), Step::Field(field as u16))
                    })
                    .collect();
                if let Some(domain) = domain {
                    let mut handle = self.new_token(TokenKind::Handle, BorrowKind::Mut, origin);
                    handle.regions.push(AbsPlace::new(Base::Domain(domain)));
                    held.insert((self.token(result, Site::Handle, handle), Path::new()));
                }
                held
            }
            NExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => {
                if let BodyOwner::Func(func) = callee.key.owner(self.db)
                    && let Some(shape) = func.return_shape(self.db)
                {
                    self.sessions
                        .insert(result, get_or_build_semantic_instance(self.db, callee.key));
                    return self.open_session(result, origin, func, shape, args, effect_args);
                }
                // A handle result names the domains of the handles the call
                // was given, or an unknown one.
                let handles: Held = args
                    .iter()
                    .flat_map(|arg| self.values[arg.value.index()].iter())
                    .filter(|(token, rest)| {
                        rest.is_empty() && self.tokens[*token as usize].kind == TokenKind::Handle
                    })
                    .cloned()
                    .collect();
                if self
                    .dynamic_handle(self.body.values[result.index()].ty)
                    .is_none()
                {
                    Held::new()
                } else if handles.is_empty() {
                    self.unknown_handle(result, origin)
                } else {
                    handles
                }
            }
            NExpr::CodeRegionRef { .. }
            | NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::PointerCast { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => Held::new(),
        }
    }

    /// A handle whose provider the body cannot see names a dynamic domain.
    fn unknown_handle(&mut self, result: NValueId, origin: SemOrigin<'db>) -> Held {
        let Some(domain) = self.dynamic_handle(self.body.values[result.index()].ty) else {
            return Held::new();
        };
        let mut handle = self.new_token(TokenKind::Handle, BorrowKind::Mut, origin);
        handle.regions.push(AbsPlace::new(Base::Domain(domain)));
        Held::from([(self.token(result, Site::Handle, handle), Path::new())])
    }

    fn open_access(
        &mut self,
        result: NValueId,
        origin: SemOrigin<'db>,
        place: &NPlace<'db>,
        mode: BorrowKind,
    ) -> Held {
        let resolved = self.resolve(place);
        let mut access = self.new_token(TokenKind::Access, mode, origin);
        access.zero_sized = place.ty.is_zero_sized(self.db);
        let id = self.token(result, Site::Open, access);
        let token = &mut self.tokens[id as usize];
        token.regions = resolved.regions;
        token.parents = resolved.direct;
        Held::from([(id, Path::new())])
    }

    /// Opens a projection call's session: a reservation per argument
    /// carrier and per effect footprint, and a grant per access component,
    /// derived from all reservations.
    fn open_session(
        &mut self,
        result: NValueId,
        origin: SemOrigin<'db>,
        func: crate::hir_def::Func<'db>,
        shape: &Shape<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Held {
        let mut reservations = TokenSet::new();
        for (position, arg) in args.iter().enumerate() {
            let param_mode = CallableDef::Func(func).param_mode(self.db, position);
            let direct = self.passed_to(arg.value, param_mode);
            if direct.is_empty() {
                continue;
            }
            let mode = match param_mode {
                FuncParamMode::Mut => BorrowKind::Mut,
                FuncParamMode::View | FuncParamMode::Own => BorrowKind::Ref,
            };
            let regions: Vec<AbsPlace> = direct
                .iter()
                .flat_map(|token| self.tokens[*token as usize].regions.clone())
                .collect();
            let zero_sized = direct
                .iter()
                .all(|token| self.tokens[*token as usize].zero_sized);
            let id = self.token(
                result,
                Site::Arg(position),
                self.new_token(TokenKind::Reservation, mode, origin),
            );
            let token = &mut self.tokens[id as usize];
            token.regions = regions;
            token.parents = direct;
            token.zero_sized = zero_sized;
            reservations.push(id);
        }
        for (position, effect) in effect_args.iter().enumerate() {
            let Some(footprint) = self.effect_footprint(func, effect, args, effect_args) else {
                continue;
            };
            let id = self.token(
                result,
                Site::Effect(position),
                self.new_token(TokenKind::Reservation, footprint.mode, origin),
            );
            let token = &mut self.tokens[id as usize];
            token.regions = footprint.regions;
            token.parents = footprint.parents;
            reservations.push(id);
        }
        let mut held = Held::new();
        for (component, (path, mode)) in access_components(shape).into_iter().enumerate() {
            let mut grant = self.new_token(TokenKind::Grant, mode, origin);
            grant.regions.push(AbsPlace::new(Base::Grant {
                session: result,
                component: component as u16,
            }));
            grant.parents = reservations.clone();
            held.insert((self.token(result, Site::Grant(component), grant), path));
        }
        held
    }

    /// The domains an effect argument confers authority over, with its mode:
    /// every persistent and transient slot for a reentrant or raw storage
    /// capability, and the supplied place or handle's domain otherwise.
    fn effect_footprint(
        &mut self,
        func: crate::hir_def::Func<'db>,
        effect: &NEffectArg<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Option<Footprint> {
        let mode = if effect.required_mut {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        };
        let arg_ty = match &effect.arg {
            NEffectArgValue::Place(place) => place.ty,
            NEffectArgValue::Value(value) => self.body.values[value.value.index()].ty,
        };
        let requirement = func
            .effect_requirements(self.db)
            .iter()
            .find(|requirement| requirement.binding_idx == effect.binding_idx);
        let key = requirement.map(|requirement| match &requirement.key {
            EffectRequirementKey::Type(_) => EffectRequirementKey::Type(arg_ty),
            key => key.clone(),
        });
        // A zero-sized place holds nothing, but a field handle names its
        // field: the handle a `Field(T)` key takes, and those a provider holds.
        let field_key = requirement
            .is_some_and(|requirement| requirement.binding_ty.field_key_handle(self.db).is_some());
        let names_field = field_key
            || !field_handle_spaces(self.db, self.scope, self.assumptions, arg_ty).is_empty();
        if let Some(key) = key
            && let Some(access) = effect_key_state_access(
                self.db,
                self.scope,
                self.assumptions,
                key,
                effect.required_mut,
            )
        {
            return Some(Footprint {
                regions: state_regions(),
                mode: access.borrow_kind(),
                parents: TokenSet::new(),
            });
        }
        let (regions, parents) = match &effect.arg {
            NEffectArgValue::Place(place) if place.ty.is_zero_sized(self.db) && !names_field => {
                return None;
            }
            NEffectArgValue::Place(place) => {
                let resolved = self.resolve(place);
                let mut regions = self.named_regions(resolved.regions, arg_ty, !field_key);
                let mut parents = resolved.direct;
                // The callee may exercise a `Field(T)` authority over the
                // handles its arguments and other effects hold, of whatever
                // field: those of another type than the one supplied name
                // other fields.
                if field_key {
                    let holds_handles = |this: &Self, ty: TyId<'db>| {
                        ty != arg_ty
                            && !field_handle_spaces(this.db, this.scope, this.assumptions, ty)
                                .is_empty()
                    };
                    for arg in args {
                        let ty = self.body.values[arg.value.index()].ty;
                        let ty = ty.as_capability(self.db).map_or(ty, |(_, target)| target);
                        if holds_handles(self, ty) {
                            let (source, _) = self.source_place(arg.value);
                            regions.extend(self.named_regions(source, ty, false));
                            parents.extend(self.direct(arg.value));
                        }
                    }
                    for other in effect_args {
                        if let NEffectArgValue::Place(other_place) = &other.arg
                            && other.binding_idx != effect.binding_idx
                            && holds_handles(self, other_place.ty)
                        {
                            let resolved = self.resolve(other_place);
                            regions.extend(self.named_regions(
                                resolved.regions,
                                other_place.ty,
                                false,
                            ));
                            parents.extend(resolved.direct);
                        }
                    }
                }
                (regions, parents)
            }
            NEffectArgValue::Value(value) => (
                self.values[value.value.index()]
                    .iter()
                    .filter(|(token, _)| self.tokens[*token as usize].kind == TokenKind::Handle)
                    .flat_map(|(token, _)| self.tokens[*token as usize].regions.clone())
                    .collect(),
                TokenSet::new(),
            ),
        };
        (!regions.is_empty()).then_some(Footprint {
            regions,
            mode,
            parents,
        })
    }

    /// The regions an access of places holding a value of `ty` reaches:
    /// the places themselves if `data`, and the key spaces of the static slot
    /// handles the value holds, apart from them. A handle copied into memory
    /// names its key space in a dynamic domain of its space. A handle whose
    /// slot is a runtime value names its place.
    fn named_regions(
        &mut self,
        regions: Vec<AbsPlace>,
        ty: TyId<'db>,
        data: bool,
    ) -> Vec<AbsPlace> {
        let spaces = field_handle_spaces(self.db, self.scope, self.assumptions, ty);
        if spaces.is_empty() {
            return regions;
        }
        let space = match spaces.as_slice() {
            [space] => Some(*space),
            _ => None,
        };
        let mut named = if data { regions.clone() } else { Vec::new() };
        for region in regions {
            let base = if self.space(region.base) == Some(ProviderAddressSpace::Memory) {
                AbsPlace::new(Base::Domain(self.domains.dynamic(space)))
            } else {
                region
            };
            named.push(base.extended(&[Step::KeySpace]));
        }
        named.sort();
        named.dedup();
        named
    }

    /// The regions of the place a value was read from or borrowed, through
    /// forwards and temporaries, or else the places its own carriers name,
    /// with the tokens that authorize it.
    fn source_place(&mut self, value: NValueId) -> (Vec<AbsPlace>, TokenSet) {
        let body = self.body;
        let mut source = value;
        let (regions, parents) = loop {
            match body.defining_expr(source) {
                Some((_, NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. })) => {
                    source = src.value
                }
                Some((
                    _,
                    NExpr::Borrow { place, .. }
                    | NExpr::MakeView { place, .. }
                    | NExpr::Load { place, .. },
                )) => {
                    // A temporary holds the value it was initialized from.
                    if let NPlaceBase::Root(root) = place.base
                        && let NRootKind::Temporary { value } = body.roots[root.index()].kind
                    {
                        source = value;
                        continue;
                    }
                    let resolved = self.resolve(place);
                    break (resolved.regions, resolved.direct);
                }
                _ => {
                    let direct = self.direct(source);
                    let regions: Vec<AbsPlace> = direct
                        .iter()
                        .flat_map(|token| self.tokens[*token as usize].regions.clone())
                        .collect();
                    break (regions, direct);
                }
            }
        };
        if regions.is_empty() {
            (vec![AbsPlace::new(Base::Raw)], parents)
        } else {
            (regions, parents)
        }
    }

    // --- control flow -----------------------------------------------------

    /// The state on entry to `block`, after its parameters are (re)defined.
    fn block_entry(&self, block: NBlockId) -> Option<State> {
        let mut state = self.entry[block.index()].clone()?;
        for param in &self.body.blocks[block.index()].params {
            state
                .moved
                .retain(|(key, _)| *key != MoveKey::Value(*param));
        }
        Some(state)
    }

    /// The statements of `block` that can execute.
    fn statements(
        &self,
        block: NBlockId,
    ) -> impl Iterator<Item = (usize, &'a NStatement<'db>)> + use<'a, 'db> {
        let statements = &self.body.blocks[block.index()].statements;
        let end = self.divergence[block.index()].map_or(statements.len(), |end| end + 1);
        statements[..end].iter().enumerate()
    }

    fn diverges(&self, block: NBlockId) -> bool {
        self.divergence[block.index()].is_some()
    }

    fn successors(&self, block: NBlockId) -> Vec<NBlockId> {
        if self.diverges(block) {
            return Vec::new();
        }
        self.body.blocks[block.index()]
            .terminator
            .kind
            .successors()
            .into_iter()
            .map(|successor| successor.block)
            .collect()
    }

    fn reverse_postorder(&self) -> Vec<NBlockId> {
        let mut visited = vec![false; self.body.blocks.len()];
        let mut postorder = Vec::new();
        let mut stack = vec![(self.body.entry, 0)];
        visited[self.body.entry.index()] = true;
        while let Some((block, next)) = stack.last_mut() {
            let successors = self.successors(*block);
            if let Some(&successor) = successors.get(*next) {
                *next += 1;
                if !visited[successor.index()] {
                    visited[successor.index()] = true;
                    stack.push((successor, 0));
                }
            } else {
                postorder.push(*block);
                stack.pop();
            }
        }
        postorder.reverse();
        postorder
    }

    /// Runs a block's statements; returns the state at its terminator.
    fn run_block(&mut self, block: NBlockId) -> Option<State> {
        let mut state = self.block_entry(block)?;
        let statements: Vec<_> = self.statements(block).collect();
        for (_, statement) in statements {
            if self.transfer(statement, &mut state).is_err() {
                break;
            }
        }
        Some(state)
    }

    fn propagate(&mut self, block: NBlockId, state: &State) -> bool {
        if self.diverges(block) {
            return false;
        }
        let mut changed = false;
        let kind = &self.body.blocks[block.index()].terminator.kind;
        let mut moved = state.clone();
        if let Some(access) = kind.access(self.db, self.body)
            && let AccessTarget::Value { operand, .. } = access.target
            && access.kind == MemoryAccessKind::Move
        {
            moved
                .moved
                .insert((MoveKey::Value(operand.value), Path::new()));
        }
        for successor in kind.successors() {
            let mut edge = moved.clone();
            for arg in &successor.args {
                if arg.mode == ReadMode::Move {
                    edge.moved.insert((MoveKey::Value(arg.value), Path::new()));
                }
            }
            match &mut self.entry[successor.block.index()] {
                Some(existing) => changed |= existing.join(&edge),
                slot @ None => {
                    *slot = Some(edge);
                    changed = true;
                }
            }
        }
        changed
    }

    // --- checks -----------------------------------------------------------

    /// Checks the body; returns its result-space contract.
    pub fn check(&mut self) -> Result<Vec<Option<ProviderAddressSpace>>, Diag<'db>> {
        self.checking = true;
        for block in self.reverse_postorder() {
            let Some(mut state) = self.block_entry(block) else {
                continue;
            };
            let statements: Vec<_> = self.statements(block).collect();
            for (_, statement) in statements {
                self.transfer(statement, &mut state)?;
            }
            if !self.diverges(block) {
                self.check_terminator(block, &state)?;
            }
        }
        self.check_yield_paths()?;
        self.check_projection_recursion()?;
        self.check_yields()
    }

    /// Projections are inlined into their callers, so a projection may not
    /// reach itself through projection calls.
    fn check_projection_recursion(&self) -> Result<(), Diag<'db>> {
        let owner = self.instance.key(self.db).owner(self.db);
        if !self.instance.is_projection(self.db) {
            return Ok(());
        }
        let mut explored = FxHashSet::default();
        for statement in self.body.blocks.iter().flat_map(|block| &block.statements) {
            if let NStatementKind::Define {
                expr: NExpr::Call { callee, .. },
                ..
            } = &statement.kind
                && reaches_owner(
                    self.db,
                    get_or_build_semantic_instance(self.db, callee.key),
                    owner,
                    &mut Vec::new(),
                    &mut explored,
                )
            {
                return Err(self.diag(
                    SemanticDiagnosticKind::ProjectionRecursion,
                    statement.origin,
                    "this call reaches the projection again through projection calls, which \
                     cannot be inlined"
                        .into(),
                ));
            }
        }
        Ok(())
    }

    /// A projection yields exactly once on each path that completes.
    fn check_yield_paths(&self) -> Result<(), Diag<'db>> {
        const NOT_YET: u8 = 1;
        const YIELDED: u8 = 2;
        let BodyOwner::Func(func) = self.instance.key(self.db).owner(self.db) else {
            return Ok(());
        };
        if func.body(self.db).is_none() || func.return_shape(self.db).is_none() {
            return Ok(());
        }
        let mut entry = vec![0; self.body.blocks.len()];
        entry[self.body.entry.index()] = NOT_YET;
        let mut work = vec![self.body.entry];
        while let Some(block) = work.pop() {
            if self.diverges(block) {
                continue;
            }
            let state = entry[block.index()];
            let terminator = &self.body.blocks[block.index()].terminator;
            let after = match terminator.kind {
                NTerminatorKind::Yield { value, .. } if state & YIELDED != 0 => {
                    // A sum shape's empty variant grants nothing.
                    let message = if self.values[value.value.index()].is_empty() {
                        "a projection returns its empty variant only before it yields"
                    } else {
                        "this path has already yielded"
                    };
                    return Err(self.diag(
                        SemanticDiagnosticKind::YieldViolation,
                        operand_origin(value, terminator.origin),
                        message.into(),
                    ));
                }
                NTerminatorKind::Yield { .. } => YIELDED,
                NTerminatorKind::Return(_) if state & NOT_YET != 0 => {
                    return Err(self.diag(
                        SemanticDiagnosticKind::YieldViolation,
                        terminator.origin,
                        "a projection must yield on every path that completes".into(),
                    ));
                }
                _ => state,
            };
            for successor in self.successors(block) {
                let merged = entry[successor.index()] | after;
                if merged != entry[successor.index()] {
                    entry[successor.index()] = merged;
                    work.push(successor);
                }
            }
        }
        Ok(())
    }

    /// `mut` yields of session-owned places with no slide after them: writes
    /// through them are discarded when the session finishes.
    fn discarded_writes(&self) -> Vec<Diag<'db>> {
        let BodyOwner::Func(func) = self.instance.key(self.db).owner(self.db) else {
            return Vec::new();
        };
        let Some(shape) = func.return_shape(self.db) else {
            return Vec::new();
        };
        let components = access_components(shape);
        self.body
            .blocks
            .iter()
            .filter_map(|block| match block.terminator.kind {
                NTerminatorKind::Yield { value, ref resume } if !self.has_slide(resume.block) => {
                    Some((value, block.terminator.origin))
                }
                _ => None,
            })
            .filter(|(value, _)| {
                let held = &self.values[value.value.index()];
                components.iter().any(|(path, kind)| {
                    let mut tokens = held.iter().filter(|(_, rest)| rest == path).peekable();
                    *kind == BorrowKind::Mut
                        && tokens.peek().is_some()
                        && tokens.all(|(token, _)| {
                            self.tokens[*token as usize]
                                .regions
                                .iter()
                                .all(|region| matches!(region.base, Base::Root(_)))
                        })
                })
            })
            .map(|(value, origin)| {
                self.diag(
                    SemanticDiagnosticKind::DiscardedWrites,
                    operand_origin(value, origin),
                    "this yields a place of the projection's own frame, and no code after the \
                     yield writes it back"
                        .into(),
                )
            })
            .collect()
    }

    /// Whether code that can write runs between resuming at `block` and
    /// returning.
    fn has_slide(&self, mut block: NBlockId) -> bool {
        loop {
            let data = &self.body.blocks[block.index()];
            if data.statements.iter().any(|statement| {
                matches!(
                    statement.kind,
                    NStatementKind::Store { .. }
                        | NStatementKind::Define {
                            expr: NExpr::Call { .. },
                            ..
                        }
                )
            }) {
                return true;
            }
            match &data.terminator.kind {
                NTerminatorKind::Goto(next) => block = next.block,
                NTerminatorKind::Return(_) => return false,
                _ => return true,
            }
        }
    }

    fn diag(
        &self,
        kind: SemanticDiagnosticKind,
        origin: SemOrigin<'db>,
        message: String,
    ) -> Diag<'db> {
        SemanticDiagnostic::new(self.instance, kind, message, self.span(origin))
    }

    fn span(&self, origin: SemOrigin<'db>) -> SemanticDiagnosticSpan<'db> {
        SemanticDiagnosticSpan::OriginWithTemplateFallback {
            owner: self.instance.key(self.db).owner(self.db),
            template_owner: self.body.template_owner,
            origin,
        }
    }

    /// Reports `result` only while checking; the fixed point keeps going.
    fn report(&self, result: Result<(), Diag<'db>>) -> Result<(), Diag<'db>> {
        if self.checking { result } else { Ok(()) }
    }

    fn transfer(
        &mut self,
        statement: &NStatement<'db>,
        state: &mut State,
    ) -> Result<(), Diag<'db>> {
        let origin = statement.origin;
        if let NStatementKind::End { access } = &statement.kind {
            let ended = self.opened.get(access).cloned().unwrap_or_default();
            for token in &ended {
                state.open.remove(token);
            }
            // An access ends with its referent restored.
            let hole = ended.iter().find_map(|token| {
                Some((*token, self.hole(state, MoveKey::Access(*token))?.clone()))
            });
            state
                .moved
                .retain(|(key, _)| !matches!(key, MoveKey::Access(token) if ended.contains(token)));
            return match hole {
                Some((token, hole)) => self.report(Err(self.moved_diag(
                    "this access ends while its referent is moved out",
                    self.tokens[token as usize].origin,
                    &hole,
                ))),
                None => Ok(()),
            };
        }
        // A statement that opens accesses again opens new instances of them.
        if let NStatementKind::Define { result, .. } = &statement.kind
            && let Some(opened) = self.opened.get(result)
        {
            for token in opened {
                state.open.remove(token);
            }
        }
        // Operands are checked in order, so one operation cannot consume the
        // same value twice.
        for access in statement.kind.accesses(self.db, self.body) {
            match access.target {
                AccessTarget::Value { operand, path } => {
                    let consume = access.kind == MemoryAccessKind::Move;
                    let result = self.use_value(operand, path, state, consume, origin);
                    self.report(result)?;
                }
                AccessTarget::Place(place) => {
                    let origin = specific_origin(place.origin, origin);
                    let result = self.access_place(place, access.kind, state, origin);
                    self.report(result)?;
                }
            }
        }
        match &statement.kind {
            NStatementKind::Store { destination, .. } => {
                let path = self.path(&destination.path);
                let key = match destination.base {
                    NPlaceBase::Root(root) => Some(MoveKey::Root(root.index() as u32)),
                    NPlaceBase::CapabilityTarget { carrier } => self.carrier_key(carrier),
                };
                if let Some(key) = key
                    && !path.contains(&Step::Index(None))
                {
                    state.moved.retain(|(moved, moved_path)| {
                        *moved != key || !moved_path.starts_with(&path)
                    });
                }
            }
            NStatementKind::Define { result, expr } => {
                state
                    .moved
                    .retain(|(key, _)| *key != MoveKey::Value(*result));
                if let NExpr::Call {
                    callee,
                    args,
                    effect_args,
                    ..
                } = expr
                {
                    let result = self.check_call(
                        callee.key.owner(self.db),
                        args,
                        effect_args,
                        state,
                        origin,
                    );
                    self.report(result)?;
                }
                let opened = self.opened.get(result).cloned().unwrap_or_default();
                if self.checking {
                    for &token in &opened {
                        self.check_opened(token, state)?;
                    }
                }
                // Accesses no `end` closes last for the operation using them.
                let mut used = Vec::new();
                expr.for_each_value_operand(|operand| used.push(operand.value));
                for value in used {
                    for (opener, tokens) in &self.opened {
                        if !self.ended.contains(opener)
                            && self.values[value.index()]
                                .iter()
                                .any(|(token, _)| tokens.contains(token))
                        {
                            for token in tokens {
                                state.open.remove(token);
                            }
                        }
                    }
                }
                state.open.extend(opened);
            }
            NStatementKind::End { .. } => {}
        }
        Ok(())
    }

    /// What tracks the initialization of a carrier's referent: the data
    /// parameter it is the input of, or the one `mut` access it holds.
    fn carrier_key(&self, carrier: NValueId) -> Option<MoveKey> {
        let [token] = self.direct(carrier)[..] else {
            return None;
        };
        let data = &self.tokens[token as usize];
        match (data.kind, data.regions.as_slice()) {
            (
                TokenKind::Input,
                [
                    AbsPlace {
                        base: Base::Param(param),
                        ..
                    },
                ],
            ) => Some(MoveKey::Param(*param)),
            (TokenKind::Access | TokenKind::Grant, _) if data.mode == BorrowKind::Mut => {
                Some(MoveKey::Access(token))
            }
            _ => None,
        }
    }

    /// The first hole in a referent an operation needs initialized.
    fn hole<'s>(&'s self, state: &'s State, key: MoveKey) -> Option<&'s (MoveKey, Path)> {
        state.moved.iter().find(|(moved, _)| *moved == key)
    }

    /// Reject a use of a possibly moved value, then record its move.
    fn use_value(
        &mut self,
        operand: NOperand,
        path: Option<&NStructuralPath>,
        state: &mut State,
        consume: bool,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let path = path.map(|path| self.path(&path.0)).unwrap_or_default();
        let key = MoveKey::Value(operand.value);
        if let Some(found) = state
            .moved
            .iter()
            .find(|(moved, moved_path)| *moved == key && paths_overlap(moved_path, &path))
        {
            return Err(self.moved_diag(
                "cannot use a value after it was moved",
                operand_origin(operand, origin),
                found,
            ));
        }
        if consume
            && self.body.values[operand.value.index()]
                .ty
                .as_capability(self.db)
                .is_none()
        {
            self.moved_at
                .entry((key, path.clone()))
                .or_insert(operand_origin(operand, origin));
            state.moved.insert((key, path));
        }
        Ok(())
    }

    fn moved_diag(
        &self,
        message: &str,
        origin: SemOrigin<'db>,
        moved: &(MoveKey, Path),
    ) -> Diag<'db> {
        let mut diag = self.diag(SemanticDiagnosticKind::MoveConflict, origin, message.into());
        if let Some(moved_at) = self.moved_at.get(moved) {
            diag.push_secondary("value is moved here".into(), self.span(*moved_at));
        }
        diag
    }

    fn access_place(
        &mut self,
        place: &NPlace<'db>,
        kind: MemoryAccessKind,
        state: &mut State,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let resolved = self.resolve(place);
        let path = self.path(&place.path);
        let key = match place.base {
            NPlaceBase::Root(root) if self.root_domain[root.index()].is_none() => {
                Some(MoveKey::Root(root.index() as u32))
            }
            NPlaceBase::Root(_) => None,
            NPlaceBase::CapabilityTarget { carrier } => self.carrier_key(carrier),
        };
        if let Some(key) = key {
            let found = state.moved.iter().find(|(moved, moved_path)| {
                *moved == key
                    && if kind == MemoryAccessKind::Write {
                        moved_path.len() < path.len() && path.starts_with(moved_path)
                    } else {
                        paths_overlap(moved_path, &path)
                    }
            });
            if let Some(found) = found {
                let message = if kind == MemoryAccessKind::Write {
                    "cannot assign to part of a moved value"
                } else if self.moved_at.contains_key(found) {
                    "cannot use a value after it was moved"
                } else {
                    "cannot use a value before it is initialized"
                };
                return Err(self.moved_diag(message, origin, found));
            }
            if kind == MemoryAccessKind::Move {
                // `[*]` names no definite element to restore.
                if path.contains(&Step::Index(None)) {
                    return Err(self.diag(
                        SemanticDiagnosticKind::MoveConflict,
                        origin,
                        "cannot move out of an element at a dynamic index".into(),
                    ));
                }
                if let Some(
                    space @ (ProviderAddressSpace::Storage | ProviderAddressSpace::Transient),
                ) = resolved
                    .regions
                    .iter()
                    .find_map(|region| self.space(region.base))
                {
                    return Err(self.diag(
                        SemanticDiagnosticKind::MoveConflict,
                        origin,
                        format!(
                            "cannot move out of {}, which never holds a hole",
                            space.pretty()
                        ),
                    ));
                }
                self.moved_at.entry((key, path.clone())).or_insert(origin);
                state.moved.insert((key, path));
            }
        }
        let writes = kind != MemoryAccessKind::Read;
        if writes
            && resolved
                .direct
                .iter()
                .any(|token| self.tokens[*token as usize].mode == BorrowKind::Ref)
        {
            let diag_kind = if kind == MemoryAccessKind::Move {
                SemanticDiagnosticKind::MoveConflict
            } else {
                SemanticDiagnosticKind::AccessConflict
            };
            return Err(self.diag(
                diag_kind,
                origin,
                format!(
                    "cannot {} a place reached through a `ref` access",
                    verb(kind)
                ),
            ));
        }
        if kind == MemoryAccessKind::Move
            && key.is_none()
            && resolved
                .regions
                .iter()
                .any(|region| region.base != Base::Raw)
        {
            return Err(self.diag(
                SemanticDiagnosticKind::MoveConflict,
                origin,
                "cannot move out of a place reached through an access".into(),
            ));
        }
        if writes {
            self.check_writable(&resolved.regions, origin)?;
        }
        if place.ty.is_zero_sized(self.db) {
            return Ok(());
        }
        let mode = if writes {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        };
        self.check_conflicts(
            state,
            &resolved.regions,
            mode,
            &resolved.authority,
            &[],
            kind,
            origin,
        )
    }

    fn check_writable(
        &self,
        regions: &[AbsPlace],
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        for region in regions {
            if let Some(space @ (ProviderAddressSpace::Calldata | ProviderAddressSpace::Code)) =
                self.space(region.base)
            {
                return Err(self.diag(
                    SemanticDiagnosticKind::StorageViolation,
                    origin,
                    format!("cannot write to {}", space.pretty()),
                ));
            }
        }
        Ok(())
    }

    /// The tokens open at a point: this body's open accesses and its data
    /// parameters.
    fn active<'s>(&'s self, state: &'s State) -> impl Iterator<Item = TokenId> + 's {
        state.open.iter().copied().chain(
            self.tokens
                .iter()
                .enumerate()
                .filter(|(_, token)| token.kind == TokenKind::Input)
                .map(|(id, _)| id as TokenId),
        )
    }

    /// Reject an access that overlaps an open access it is not derived from.
    #[allow(clippy::too_many_arguments)]
    fn check_conflicts(
        &self,
        state: &State,
        regions: &[AbsPlace],
        mode: BorrowKind,
        authority: &[TokenId],
        own: &[TokenId],
        kind: MemoryAccessKind,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        for token in self.active(state) {
            let data = &self.tokens[token as usize];
            if own.contains(&token)
                || authority.contains(&token)
                || data.zero_sized
                || mode == BorrowKind::Ref && data.mode == BorrowKind::Ref
                || !self.any_overlap(regions, &data.regions)
            {
                continue;
            }
            let held = match data.mode {
                BorrowKind::Mut => "a `mut`",
                BorrowKind::Ref => "a `ref`",
            };
            let what = match data.kind {
                TokenKind::Input => "parameter",
                TokenKind::Reservation | TokenKind::Grant => "projection",
                TokenKind::Access | TokenKind::Handle => "access",
            };
            let action = match regions.iter().find_map(|region| match region.base {
                Base::State(space) => Some(space),
                _ => None,
            }) {
                Some(space) => format!(
                    "this call may {} {}",
                    if mode == BorrowKind::Mut {
                        "write"
                    } else {
                        "read"
                    },
                    space.pretty()
                ),
                None => format!("cannot {} this place", verb(kind)),
            };
            let mut diag = self.diag(
                SemanticDiagnosticKind::AccessConflict,
                origin,
                format!("{action} while {held} {what} of it is open"),
            );
            diag.push_secondary(format!("{what} opened here"), self.span(data.origin));
            return Err(diag);
        }
        Ok(())
    }

    /// Check a token a statement opens against the accesses open around it.
    fn check_opened(&self, token: TokenId, state: &State) -> Result<(), Diag<'db>> {
        let data = &self.tokens[token as usize];
        if data.zero_sized || data.kind != TokenKind::Reservation {
            return Ok(());
        }
        let authority = self.ancestors(&data.parents);
        let kind = match data.mode {
            BorrowKind::Mut => MemoryAccessKind::MutAccess,
            BorrowKind::Ref => MemoryAccessKind::Read,
        };
        self.check_conflicts(
            state,
            &data.regions,
            data.mode,
            &authority,
            &[token],
            kind,
            data.origin,
        )
    }

    /// A call's interference footprint: its carrier arguments for the call's
    /// duration, its effects, and the state its external executions reach.
    fn check_call(
        &mut self,
        owner: BodyOwner<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        state: &State,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let BodyOwner::Func(func) = owner else {
            return Ok(());
        };
        for (position, arg) in args.iter().enumerate() {
            let param_mode = CallableDef::Func(func).param_mode(self.db, position);
            let arg_origin = operand_origin(*arg, origin);
            let direct = self.passed_to(arg.value, param_mode);
            if direct.is_empty() {
                // An aggregate passed by value is still viewed in place: the
                // call reads the place it came from.
                let ty = self.body.values[arg.value.index()].ty;
                if param_mode == FuncParamMode::View
                    && !ty_is_snapshot(self.db, self.scope, ty, self.assumptions)
                {
                    let (regions, parents) = self.source_place(arg.value);
                    let authority = self.ancestors(&parents);
                    self.check_conflicts(
                        state,
                        &regions,
                        BorrowKind::Ref,
                        &authority,
                        &[],
                        MemoryAccessKind::Read,
                        arg_origin,
                    )?;
                }
                continue;
            }
            let (mode, kind) = match param_mode {
                FuncParamMode::Mut => (BorrowKind::Mut, MemoryAccessKind::MutAccess),
                FuncParamMode::View | FuncParamMode::Own => {
                    (BorrowKind::Ref, MemoryAccessKind::Read)
                }
            };
            let regions: Vec<AbsPlace> = direct
                .iter()
                .filter(|token| !self.tokens[**token as usize].zero_sized)
                .flat_map(|token| self.tokens[*token as usize].regions.clone())
                .collect();
            let authority = self.ancestors(&direct);
            // The callee may read its argument at once.
            if let Some(key) = self.carrier_key(arg.value)
                && let Some(hole) = self.hole(state, key)
            {
                return Err(self.moved_diag(
                    "this argument's referent is moved out",
                    arg_origin,
                    hole,
                ));
            }
            self.check_conflicts(state, &regions, mode, &authority, &[], kind, arg_origin)?;
        }
        let mut footprints: Vec<_> = effect_args
            .iter()
            .filter_map(|effect| self.effect_footprint(func, effect, args, effect_args))
            .collect();
        if let Some(access) = external_call_state_access(self.db, func) {
            footprints.push(Footprint {
                regions: state_regions(),
                mode: access.borrow_kind(),
                parents: TokenSet::new(),
            });
        }
        for footprint in footprints {
            let kind = match footprint.mode {
                BorrowKind::Mut => MemoryAccessKind::Write,
                BorrowKind::Ref => MemoryAccessKind::Read,
            };
            let authority = self.ancestors(&footprint.parents);
            self.check_conflicts(
                state,
                &footprint.regions,
                footprint.mode,
                &authority,
                &[],
                kind,
                origin,
            )?;
        }
        Ok(())
    }

    fn check_terminator(&mut self, block: NBlockId, state: &State) -> Result<(), Diag<'db>> {
        let terminator = &self.body.blocks[block.index()].terminator;
        let mut state = state.clone();
        if let Some(access) = terminator.kind.access(self.db, self.body)
            && let AccessTarget::Value { operand, path } = access.target
        {
            let consume = access.kind == MemoryAccessKind::Move;
            self.use_value(operand, path, &mut state, consume, terminator.origin)?;
        }
        for successor in terminator.kind.successors() {
            let mut edge = state.clone();
            for arg in &successor.args {
                let consume = arg.mode == ReadMode::Move;
                self.use_value(*arg, None, &mut edge, consume, terminator.origin)?;
            }
        }
        if let NTerminatorKind::Yield { value, .. } = terminator.kind {
            self.check_grant_creation(value, &state, terminator.origin)?;
        }
        if matches!(
            terminator.kind,
            NTerminatorKind::Return(_) | NTerminatorKind::Yield { .. }
        ) && let Some(moved) = state
            .moved
            .iter()
            .find(|(key, _)| matches!(key, MoveKey::Param(_)))
        {
            return Err(self.diag(
                SemanticDiagnosticKind::MoveConflict,
                self.moved_at.get(moved).copied().unwrap_or(terminator.origin),
                "a `mut` parameter moved out of here must be reinitialized before the function returns".into(),
            ));
        }
        Ok(())
    }

    /// Creating a grant is an access of its component's mode on the yielded
    /// place, checked against every access the suspended frame retains: only
    /// the grant's own ancestors are exempt.
    fn check_grant_creation(
        &self,
        value: NOperand,
        state: &State,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let BodyOwner::Func(func) = self.instance.key(self.db).owner(self.db) else {
            return Ok(());
        };
        let Some(shape) = func.return_shape(self.db) else {
            return Ok(());
        };
        let held = &self.values[value.value.index()];
        for (path, mode) in access_components(shape) {
            let own: TokenSet = held
                .iter()
                .filter(|(_, rest)| *rest == path)
                .map(|(token, _)| *token)
                .collect();
            let regions: Vec<AbsPlace> = own
                .iter()
                .flat_map(|token| self.tokens[*token as usize].regions.clone())
                .collect();
            let kind = match mode {
                BorrowKind::Mut => MemoryAccessKind::MutAccess,
                BorrowKind::Ref => MemoryAccessKind::Read,
            };
            let authority = self.ancestors(&own);
            let origin = operand_origin(value, origin);
            self.check_conflicts(state, &regions, mode, &authority, &own, kind, origin)?;
        }
        Ok(())
    }

    /// A projection's yields: each component of every yield site names a
    /// place in one address space, its result-space contract, and the access
    /// components of a split are disjoint.
    fn check_yields(&self) -> Result<Vec<Option<ProviderAddressSpace>>, Diag<'db>> {
        let BodyOwner::Func(func) = self.instance.key(self.db).owner(self.db) else {
            return Ok(Vec::new());
        };
        let Some(shape) = func.return_shape(self.db) else {
            return Ok(Vec::new());
        };
        let components = access_components(shape);
        let mut spaces: Vec<Option<(ProviderAddressSpace, SemOrigin<'db>)>> =
            vec![None; components.len()];
        for block in &self.body.blocks {
            let NTerminatorKind::Yield { value, .. } = &block.terminator.kind else {
                continue;
            };
            let origin = operand_origin(*value, block.terminator.origin);
            let held = &self.values[value.value.index()];
            let regions: Vec<Vec<AbsPlace>> = components
                .iter()
                .map(|(path, _)| {
                    held.iter()
                        .filter(|(_, rest)| rest == path)
                        .flat_map(|(token, _)| self.tokens[*token as usize].regions.clone())
                        .collect()
                })
                .collect();
            for (index, component) in regions.iter().enumerate() {
                for region in component {
                    let Some(space) = self.space(region.base) else {
                        continue;
                    };
                    match spaces[index] {
                        Some((known, first)) if known != space => {
                            let mut diag = self.diag(
                                SemanticDiagnosticKind::TransportViolation,
                                origin,
                                format!(
                                    "this yield names {}, but another yield names {}",
                                    space.pretty(),
                                    known.pretty()
                                ),
                            );
                            diag.push_secondary(
                                format!("yields {}", known.pretty()),
                                self.span(first),
                            );
                            return Err(diag);
                        }
                        Some(_) => {}
                        None => spaces[index] = Some((space, origin)),
                    }
                }
                for (other, other_regions) in regions.iter().enumerate().skip(index + 1) {
                    if (components[index].1 == BorrowKind::Mut
                        || components[other].1 == BorrowKind::Mut)
                        && self.any_overlap(component, other_regions)
                    {
                        return Err(self.diag(
                            SemanticDiagnosticKind::AccessConflict,
                            origin,
                            "the components of a split must be disjoint when one of them is `mut`"
                                .into(),
                        ));
                    }
                }
            }
        }
        // A declared contract holds every yield, and is the contract of a
        // component no yield shows.
        let declared = self.instance.declared_result_spaces(self.db);
        spaces
            .into_iter()
            .enumerate()
            .map(|(index, inferred)| {
                let declared = declared
                    .get(index)
                    .copied()
                    .flatten()
                    .and_then(|contract| self.instance.contract_space(self.db, contract));
                match (inferred, declared) {
                    (Some((space, origin)), Some(declared)) if space != declared => Err(self.diag(
                        SemanticDiagnosticKind::TransportViolation,
                        origin,
                        format!(
                            "this yield names {}, but the signature declares {}",
                            space.pretty(),
                            declared.pretty()
                        ),
                    )),
                    (inferred, declared) => Ok(inferred.map(|(space, _)| space).or(declared)),
                }
            })
            .collect()
    }
}

/// Whether `instance`, a callee, is a projection that reaches `owner` through
/// projection calls. A function that recurs on the path is not explored
/// further: its own check reports it, which also bounds polymorphic recursion.
fn reaches_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    owner: BodyOwner<'db>,
    path: &mut Vec<BodyOwner<'db>>,
    explored: &mut FxHashSet<SemanticInstance<'db>>,
) -> bool {
    let instance_owner = instance.key(db).owner(db);
    if !instance.is_projection(db) || path.contains(&instance_owner) || !explored.insert(instance) {
        return false;
    }
    if instance_owner == owner {
        return true;
    }
    path.push(instance_owner);
    let reaches = instance.callees(db).iter().any(|callee| {
        reaches_owner(
            db,
            get_or_build_semantic_instance(db, callee.key),
            owner,
            path,
            explored,
        )
    });
    path.pop();
    reaches
}

/// The access components of a shape, by their path in its carrier.
fn access_components(shape: &Shape<'_>) -> Vec<(Path, BorrowKind)> {
    fn walk(shape: &Shape<'_>, path: &mut Path, out: &mut Vec<(Path, BorrowKind)>) {
        match shape {
            Shape::Owned(_) => {}
            Shape::Access(kind, _) => out.push((path.clone(), *kind)),
            Shape::Tuple(elems) => {
                for (field, elem) in elems.iter().enumerate() {
                    path.push(Step::Field(field as u16));
                    walk(elem, path, out);
                    path.pop();
                }
            }
            Shape::Sum {
                variant, payload, ..
            } => {
                path.push(Step::Variant {
                    variant: *variant,
                    field: 0,
                });
                walk(payload, path, out);
                path.pop();
            }
        }
    }
    let mut out = Vec::new();
    walk(shape, &mut Path::new(), &mut out);
    out
}

/// Every persistent and transient slot.
fn state_regions() -> Vec<AbsPlace> {
    [
        ProviderAddressSpace::Storage,
        ProviderAddressSpace::Transient,
    ]
    .map(|space| AbsPlace::new(Base::State(space)))
    .to_vec()
}

fn verb(kind: MemoryAccessKind) -> &'static str {
    match kind {
        MemoryAccessKind::Read => "read",
        MemoryAccessKind::MutAccess => "mutably access",
        MemoryAccessKind::Write => "write to",
        MemoryAccessKind::Move => "move out of",
    }
}

/// The more precise of two origins for a diagnostic: an expression, then a
/// statement, then the body.
fn specific_origin<'db>(first: SemOrigin<'db>, second: SemOrigin<'db>) -> SemOrigin<'db> {
    match (first, second) {
        (SemOrigin::Expr(_), _)
        | (SemOrigin::Stmt(_), SemOrigin::Body(_) | SemOrigin::Synthetic) => first,
        _ => second,
    }
}

/// Checks a body, returning its warnings and its result-space contract.
pub(super) fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
) -> Result<(Vec<Diag<'db>>, Vec<Option<ProviderAddressSpace>>), Diag<'db>> {
    let mut analysis = Analysis::new(db, instance, body, true);
    analysis.solve();
    let spaces = analysis.check()?;
    Ok((analysis.discarded_writes(), spaces))
}
