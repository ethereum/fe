use cranelift_entity::{entity_impl, PrimaryMap};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    hir_def::{
        scope_graph::{EdgeKind, Scope, ScopeEdge, ScopeGraph, ScopeId},
        EnumVariantListId, FnParamListId, FnParamName, GenericParamListId, ItemKind,
        RecordFieldListId, TopLevelMod, Use, Visibility,
    },
    HirDb,
};

pub struct ScopeGraphBuilder<'db> {
    pub(super) db: &'db dyn HirDb,
    pub(super) top_mod: TopLevelMod,
    graph: IntermediateScopeGraph,
    scope_stack: Vec<NodeId>,
    module_stack: Vec<NodeId>,
}

impl<'db> ScopeGraphBuilder<'db> {
    pub(crate) fn enter_top_mod(db: &'db dyn HirDb, top_mod: TopLevelMod) -> Self {
        let mut builder = Self {
            db,
            top_mod,
            graph: IntermediateScopeGraph::default(),
            scope_stack: Default::default(),
            module_stack: Default::default(),
        };

        builder.enter_scope(true);
        builder
    }

    pub fn build(self) -> ScopeGraph {
        self.graph.build(self.top_mod)
    }

    pub fn enter_scope(&mut self, is_mod: bool) {
        // Create dummy scope, the scope kind is initialized in `leave_scope`.
        let (dummy_scope_id, dummy_scope) = self.dummy_scope();
        let id = self.graph.push(dummy_scope_id, dummy_scope);
        self.scope_stack.push(id);
        if is_mod {
            self.module_stack.push(id);
        }
    }

    pub fn leave_scope(&mut self, item: ItemKind) {
        use ItemKind::*;

        let item_node = self.scope_stack.pop().unwrap();
        self.initialize_item_scope(item_node, item);

        if let ItemKind::TopMod(top_mod) = item {
            debug_assert!(self.scope_stack.is_empty());
            self.graph.add_edge(item_node, item_node, EdgeKind::self_());

            self.graph.add_external_edge(
                item_node,
                ScopeId::Item(top_mod.ingot(self.db).root_mod(self.db).into()),
                EdgeKind::ingot(),
            );
            for child in top_mod.child_top_mods(self.db) {
                let child_name = child.name(self.db);
                let edge = EdgeKind::mod_(child_name);
                self.graph
                    .add_external_edge(item_node, ScopeId::Item(child.into()), edge)
            }

            if let Some(parent) = top_mod.parent(self.db) {
                let edge = EdgeKind::super_();
                self.graph
                    .add_external_edge(item_node, ScopeId::Item(parent.into()), edge);
            }
            self.module_stack.pop().unwrap();

            return;
        }

        let parent_node = *self.scope_stack.last().unwrap();
        let parent_to_child_edge = match item {
            Mod(inner) => {
                self.module_stack.pop().unwrap();

                self.graph.add_edge(
                    item_node,
                    *self.module_stack.last().unwrap(),
                    EdgeKind::super_(),
                );
                self.graph.add_external_edge(
                    item_node,
                    ScopeId::Item(self.top_mod.ingot(self.db).root_mod(self.db).into()),
                    EdgeKind::ingot(),
                );
                self.graph.add_edge(item_node, item_node, EdgeKind::self_());

                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::mod_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Func(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                if let Some(params) = inner.params(self.db).to_opt() {
                    self.add_func_param_scope(item_node, inner.into(), params);
                }
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::value)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Struct(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_field_scope(item_node, inner.into(), inner.fields(self.db));
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Contract(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_field_scope(item_node, inner.into(), inner.fields(self.db));
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Enum(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_variant_scope(item_node, inner.into(), inner.variants(self.db));
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            TypeAlias(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Impl(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                EdgeKind::anon()
            }

            Trait(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::trait_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            ImplTrait(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                EdgeKind::anon()
            }

            Const(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::value)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Use(use_) => {
                self.graph.unresolved_uses.insert(use_);

                self.graph.add_lex_edge(item_node, parent_node);
                EdgeKind::anon()
            }

            Body(_) => {
                self.graph.add_lex_edge(item_node, parent_node);
                EdgeKind::anon()
            }

            _ => unreachable!(),
        };

        self.graph
            .add_edge(parent_node, item_node, parent_to_child_edge);
    }

    fn initialize_item_scope(&mut self, node: NodeId, item: ItemKind) {
        self.graph.initialize_item_scope(self.db, node, item)
    }

    fn add_field_scope(
        &mut self,
        parent_node: NodeId,
        parent_item: ItemKind,
        fields: RecordFieldListId,
    ) {
        let parent_scope = ScopeId::Item(parent_item);

        for (i, field) in fields.data(self.db).iter().enumerate() {
            let scope_id = ScopeId::Field(parent_item, i);
            let scope_data = Scope::new(scope_id, Some(parent_scope), field.vis);

            let field_node = self.graph.push(scope_id, scope_data);
            self.graph.add_lex_edge(field_node, parent_node);
            let kind = field
                .name
                .to_opt()
                .map(EdgeKind::field)
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, field_node, kind)
        }
    }

    fn add_variant_scope(
        &mut self,
        parent_node: NodeId,
        parent_item: ItemKind,
        variants: EnumVariantListId,
    ) {
        let parent_scope = ScopeId::Item(parent_item);
        let parent_vis = parent_item.vis(self.db);

        for (i, field) in variants.data(self.db).iter().enumerate() {
            let scope_id = ScopeId::Variant(parent_item, i);
            let scope_data = Scope::new(scope_id, Some(parent_scope), parent_vis);

            let variant_node = self.graph.push(scope_id, scope_data);
            self.graph.add_lex_edge(variant_node, parent_node);
            let kind = field
                .name
                .to_opt()
                .map(EdgeKind::variant)
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, variant_node, kind)
        }
    }

    fn add_func_param_scope(
        &mut self,
        parent_node: NodeId,
        parent_item: ItemKind,
        params: FnParamListId,
    ) {
        let parent_scope = ScopeId::Item(parent_item);

        for (i, param) in params.data(self.db).iter().enumerate() {
            let scope_id = ScopeId::FnParam(parent_item, i);
            let scope = Scope::new(scope_id, Some(parent_scope), Visibility::Private);
            let func_param_node = self.graph.push(scope_id, scope);

            self.graph.add_lex_edge(func_param_node, parent_node);
            let kind = param
                .name
                .to_opt()
                .map(|name| match name {
                    FnParamName::Ident(ident) => EdgeKind::value(ident),
                    FnParamName::Underscore => EdgeKind::anon(),
                })
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, func_param_node, kind)
        }
    }

    fn add_generic_param_scope(
        &mut self,
        parent_node: NodeId,
        parent_item: ItemKind,
        params: GenericParamListId,
    ) {
        let parent_scope = ScopeId::Item(parent_item);

        for (i, param) in params.data(self.db).iter().enumerate() {
            let scope_id = ScopeId::GenericParam(parent_item, i);
            let scope = Scope::new(scope_id, Some(parent_scope), Visibility::Private);

            let generic_param_node = self.graph.push(scope_id, scope);
            self.graph.add_lex_edge(generic_param_node, parent_node);
            let kind = param
                .name()
                .to_opt()
                .map(EdgeKind::generic_param)
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, generic_param_node, kind)
        }
    }

    fn dummy_scope(&self) -> (ScopeId, Scope) {
        let scope_id = ScopeId::Item(self.top_mod.into());
        (scope_id, Scope::new(scope_id, None, Visibility::Public))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeId(u32);
entity_impl!(NodeId);

#[derive(Default)]
struct IntermediateScopeGraph {
    nodes: PrimaryMap<NodeId, (ScopeId, Scope)>,
    edges: FxHashMap<NodeId, Vec<(NodeId, EdgeKind)>>,
    unresolved_uses: FxHashSet<Use>,
}

impl IntermediateScopeGraph {
    fn build(mut self, top_mod: TopLevelMod) -> ScopeGraph {
        for (from_node, edges) in self.edges {
            for (dest_node, kind) in edges {
                let dest = self.nodes[dest_node].0;
                let edge = ScopeEdge { dest, kind };
                self.nodes[from_node].1.edges.insert(edge);
            }
        }

        let scopes = self
            .nodes
            .into_iter()
            .map(|(_, (id, data))| (id, data))
            .collect();

        ScopeGraph {
            top_mod,
            scopes,
            unresolved_uses: self.unresolved_uses,
        }
    }

    fn push(&mut self, scope_id: ScopeId, scope_data: Scope) -> NodeId {
        self.nodes.push((scope_id, scope_data))
    }

    fn initialize_item_scope(&mut self, db: &dyn HirDb, node: NodeId, item: ItemKind) {
        let scope_id = ScopeId::Item(item);

        let scope_data = &mut self.nodes[node];
        scope_data.0 = scope_id;
        scope_data.1.id = scope_id;
        scope_data.1.vis = item.vis(db);
    }

    fn add_lex_edge(&mut self, child: NodeId, parent: NodeId) {
        self.edges
            .entry(child)
            .or_default()
            .push((parent, EdgeKind::lex()));
    }

    fn add_edge(&mut self, from: NodeId, dest: NodeId, kind: EdgeKind) {
        self.edges.entry(from).or_default().push((dest, kind));
    }

    /// Add an edge to the graph that is not part of the current file.
    fn add_external_edge(&mut self, from: NodeId, dest: ScopeId, kind: EdgeKind) {
        self.nodes[from].1.edges.insert(ScopeEdge { dest, kind });
    }
}
