//! Tooling API for IDE features
//!
//! This module provides public APIs for IDE and tooling integration.
//! It acts as a stable interface over the internal compiler machinery.
//!
//! ## Features
//! - Position collection and resolution for goto definition, hover, references
//! - Symbol resolution for identifiers, methods, fields
//! - Function analysis for local variable tracking
//!
//! ## Design Principles
//! - Provide stable public APIs that abstract internal compiler details
//! - Support incremental/partial analysis for performance
//! - Handle both local (within function) and global (cross-module) analysis
//!
//! ## Type Checker Integration
//! This API relies heavily on the type checker for accurate symbol resolution.
//! Previous versions included complex fallback logic to handle cases where the
//! type checker would fail on certain expressions (e.g., binary operations with
//! method calls). With improvements to the type checker's handling of temporary
//! values, this fallback logic is no longer needed, resulting in a cleaner and
//! more maintainable API.

use hir::{
    hir_def::{
        scope_graph::ScopeId, Body, Const, Enum, Expr, ExprId, FieldDef, FieldIndex, Func,
        FuncParam, FuncParamName, IdentId, Impl, ItemKind, Partial, Pat, PatId, PathId, Struct,
        TopLevelMod, Trait, VariantDef,
    },
    span::{
        item::{LazyConstSpan, LazyEnumSpan, LazyImplSpan, LazyStructSpan, LazyTraitSpan},
        DynLazySpan, LazySpan, LazySpanAtom,
    },
    visitor::{
        prelude::{
            LazyExprSpan, LazyFieldDefSpan, LazyFuncParamSpan, LazyFuncSpan, LazyPatSpan,
            LazyPathSpan, LazyVariantDefSpan,
        },
        walk_const, walk_enum, walk_expr, walk_field_def, walk_func, walk_func_param, walk_impl,
        walk_pat, walk_struct, walk_trait, walk_variant_def, Visitor, VisitorCtxt,
    },
    HirDb, SpannedHirDb,
};
use rustc_hash::FxHashMap;

use crate::{
    name_resolution::resolve_ident_to_bucket,
    ty::{
        func_def::FuncDef,
        method_table::probe_method_for_language_server,
        ty_check::{check_func_body, LocalBinding},
        ty_def::TyId,
    },
    HirAnalysisDb,
};

/// Text cursor position (compatible with language server protocols)
pub type Cursor = parser::TextSize;

// ============================================================================
// POSITION COLLECTION AND RESOLUTION
// ============================================================================

/// Represents a definition target that can be either a scope or a location
#[derive(Debug, Clone)]
pub enum Definition<'db> {
    /// A global item with a scope (functions, structs, enums, etc.)
    Scope(ScopeId<'db>),
    /// A local definition with just a location (local variables, parameters)
    Location(DynLazySpan<'db>),
}

/// Different types of positions that can be resolved to definitions
#[derive(Debug, Clone)]
pub enum ResolvablePosition<'db> {
    /// Definition of any top-level item (struct, fn, enum, const, trait)
    ItemDefinition(ItemKind<'db>, LazySpanAtom<'db>),

    /// A qualified path (e.g., `Color::Red`, `std::math::sqrt`)
    Path(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>),
    /// A field access expression (e.g., `container.value`)
    FieldAccess(ExprId, IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// A method call expression (e.g., `container.get()`)
    MethodCall(ExprId, IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// A reference to a local variable or function parameter
    LocalBinding(IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// A field within a pattern (e.g., `{ intensity }` in `Color::Green { intensity }`)
    PatternField(IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// The definition of a struct or contract field
    FieldDefinition(FieldDef<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// The definition of an enum variant
    VariantDefinition(VariantDef<'db>, ScopeId<'db>, LazySpanAtom<'db>),
}

/// Collector for gathering resolvable positions in code
struct PositionCollector<'db> {
    positions: Vec<ResolvablePosition<'db>>,
}

impl<'db> PositionCollector<'db> {
    fn new(_db: &'db dyn HirDb) -> Self {
        Self {
            positions: Vec::new(),
        }
    }
}

impl<'db> Visitor<'db> for PositionCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        let Some(span) = ctxt.span() else {
            return;
        };
        let scope = ctxt.scope();

        if path.parent(ctxt.db()).is_none() {
            // This is a single-segment path (e.g., `container`, `Color`). This is the
            // ONLY place we create a `LocalBinding` candidate from a path.
            // The resolver will correctly fall back to global scope if it's not a local.
            if let Some(ident) = path.ident(ctxt.db()).to_opt() {
                let atom_span = span.clone().segment(0).ident();

                // Debug: Print if this is 'self'
                if ident.is_self(ctxt.db()) {
                    eprintln!("DEBUG: Found 'self' path at scope {:?}", scope);
                }

                self.positions
                    .push(ResolvablePosition::LocalBinding(ident, scope, atom_span));
            }
        } else {
            // This is a multi-segment path (e.g., `Color::Red`). This can't be a local variable.
            // We only create `Path` candidates for each segment.
            for segment_idx in 0..=path.segment_index(ctxt.db()) {
                if let Some(segment_path) = path.segment(ctxt.db(), segment_idx) {
                    self.positions.push(ResolvablePosition::Path(
                        segment_path,
                        scope,
                        span.clone(),
                    ));
                }
            }
        }
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        match expr_data {
            // Handle field access: self.value, container.field
            Expr::Field(receiver_expr, field_name) => {
                if let Partial::Present(FieldIndex::Ident(field_ident)) = field_name {
                    if let Some(field_span) =
                        ctxt.span().map(|span| span.into_field_expr().accessor())
                    {
                        let scope = ctxt.scope();
                        self.positions.push(ResolvablePosition::FieldAccess(
                            *receiver_expr,
                            *field_ident,
                            scope,
                            field_span,
                        ));
                    }
                }
            }
            // Handle method calls: container.get(), container.display()
            Expr::MethodCall(receiver_expr, method_name, _generic_args, _call_args) => {
                if let Partial::Present(method_ident) = method_name {
                    // Try to get the method name span
                    let method_span = ctxt.span().map(|span| {
                        // Try to get method call expression span
                        let method_call_span = span.into_method_call_expr();
                        method_call_span.method_name()
                    });

                    if let Some(method_span) = method_span {
                        let scope = ctxt.scope();
                        self.positions.push(ResolvablePosition::MethodCall(
                            *receiver_expr,
                            *method_ident,
                            scope,
                            method_span,
                        ));
                    }
                }
            }
            _ => {}
        }

        // Continue with default traversal for other expressions
        walk_expr(self, ctxt, expr);
    }

    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        pat_data: &Pat<'db>,
    ) {
        match pat_data {
            // Handle record pattern fields: Green { intensity }
            Pat::Record(_path, fields) => {
                for field in fields {
                    if let Partial::Present(_field_ident) = field.label {
                        // TODO: Get proper span for field identifier
                        if let Some(_span) = ctxt.span() {
                            let _scope = ctxt.scope();
                            // For now, use the pattern span - we'd need to get the field span specifically
                            // self.positions.push(ResolvablePosition::PatternField(field_ident, scope, span));
                        }
                    }
                }
            }
            _ => {}
        }

        // Continue with default traversal
        walk_pat(self, ctxt, pat);
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
        field: &FieldDef<'db>,
    ) {
        if let Partial::Present(_) = field.name {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                self.positions.push(ResolvablePosition::FieldDefinition(
                    field.clone(),
                    ctxt.scope(),
                    span,
                ));
            }
        }
        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
        variant: &VariantDef<'db>,
    ) {
        if let Partial::Present(_) = variant.name {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                self.positions.push(ResolvablePosition::VariantDefinition(
                    variant.clone(),
                    ctxt.scope(),
                    span,
                ));
            }
        }
        walk_variant_def(self, ctxt, variant);
    }

    fn visit_func(&mut self, ctxt: &mut VisitorCtxt<'db, LazyFuncSpan<'db>>, func: Func<'db>) {
        // Collect the function definition itself
        if let Some(_name) = func.name(ctxt.db()).to_opt() {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                self.positions.push(ResolvablePosition::ItemDefinition(
                    ItemKind::Func(func),
                    span,
                ));
            }
        }

        // Continue with default traversal
        walk_func(self, ctxt, func);
    }

    fn visit_func_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncParamSpan<'db>>,
        param: &FuncParam<'db>,
    ) {
        // Collect parameter definitions
        if let Some(FuncParamName::Ident(ident)) = param.name.to_opt() {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                // Debug: Print if this is 'self' parameter
                if ident.is_self(ctxt.db()) {
                    eprintln!(
                        "DEBUG: Found 'self' parameter definition at scope {:?}",
                        ctxt.scope()
                    );
                }

                // Parameters are like local bindings - they exist within a function scope
                self.positions
                    .push(ResolvablePosition::LocalBinding(ident, ctxt.scope(), span));
            }
        }

        // Continue with default traversal
        walk_func_param(self, ctxt, param);
    }

    fn visit_struct(&mut self, ctxt: &mut VisitorCtxt<'db, LazyStructSpan<'db>>, s: Struct<'db>) {
        // Collect the struct definition itself
        if let Some(_name) = s.name(ctxt.db()).to_opt() {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                self.positions.push(ResolvablePosition::ItemDefinition(
                    ItemKind::Struct(s),
                    span,
                ));
            }
        }

        // Continue with default traversal
        walk_struct(self, ctxt, s);
    }

    fn visit_enum(&mut self, ctxt: &mut VisitorCtxt<'db, LazyEnumSpan<'db>>, e: Enum<'db>) {
        // Collect the enum definition itself
        if let Some(_name) = e.name(ctxt.db()).to_opt() {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                self.positions
                    .push(ResolvablePosition::ItemDefinition(ItemKind::Enum(e), span));
            }
        }

        // Continue with default traversal
        walk_enum(self, ctxt, e);
    }

    fn visit_const(&mut self, ctxt: &mut VisitorCtxt<'db, LazyConstSpan<'db>>, c: Const<'db>) {
        // Collect the const definition itself
        if let Some(_name) = c.name(ctxt.db()).to_opt() {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                self.positions
                    .push(ResolvablePosition::ItemDefinition(ItemKind::Const(c), span));
            }
        }

        // Continue with default traversal
        walk_const(self, ctxt, c);
    }

    fn visit_trait(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTraitSpan<'db>>, t: Trait<'db>) {
        // Collect the trait definition itself
        if let Some(_name) = t.name(ctxt.db()).to_opt() {
            if let Some(span) = ctxt.span().map(|s| s.name()) {
                self.positions
                    .push(ResolvablePosition::ItemDefinition(ItemKind::Trait(t), span));
            }
        }

        // Continue with default traversal
        walk_trait(self, ctxt, t);
    }

    fn visit_impl(&mut self, ctxt: &mut VisitorCtxt<'db, LazyImplSpan<'db>>, impl_: Impl<'db>) {
        // For impl blocks, we don't collect the impl itself as a position,
        // but we need to traverse into it to collect positions from its contents
        walk_impl(self, ctxt, impl_);
    }
}

/// Collect all resolvable positions in a top-level module
pub fn collect_resolvable_positions<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<ResolvablePosition<'db>> {
    let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
    let mut collector = PositionCollector::new(db);
    collector.visit_top_mod(&mut visitor_ctxt, top_mod);

    collector.positions
}

/// Find the best position at the given cursor location
pub fn find_position_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    cursor: Cursor,
    positions: Vec<ResolvablePosition<'db>>,
) -> Option<ResolvablePosition<'db>> {
    // Helper function to get position priority (higher number = higher priority)
    fn get_position_priority(pos: &ResolvablePosition) -> u32 {
        match pos {
            ResolvablePosition::ItemDefinition(_, _) => 8,
            ResolvablePosition::FieldAccess(_, _, _, _) => 7,
            ResolvablePosition::MethodCall(_, _, _, _) => 6,
            ResolvablePosition::FieldDefinition(_, _, _) => 5,
            ResolvablePosition::VariantDefinition(_, _, _) => 4,
            ResolvablePosition::PatternField(_, _, _) => 3,
            ResolvablePosition::LocalBinding(_, _, _) => 2,
            ResolvablePosition::Path(_, _, _) => 1,
        }
    }

    // Collect all candidates first, then apply priority logic
    let mut candidates = Vec::new();

    for position in positions {
        match &position {
            ResolvablePosition::Path(path, scope, lazy_span) => {
                // Check all segments of the path to see if cursor matches any of them
                for idx in 0..=path.segment_index(db) {
                    if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                        if seg_span.range.contains(cursor) {
                            if let Some(segment_path) = path.segment(db, idx) {
                                let seg_position = ResolvablePosition::Path(
                                    segment_path,
                                    *scope,
                                    lazy_span.clone(),
                                );
                                candidates.push((
                                    seg_position,
                                    seg_span.range.end() - seg_span.range.start(),
                                ));
                            }
                        }
                    }
                }
            }
            ResolvablePosition::FieldAccess(_expr, _ident, _scope, lazy_span) => {
                if let Some(span) = lazy_span.resolve(db) {
                    if span.range.contains(cursor) {
                        candidates.push((position.clone(), span.range.end() - span.range.start()));
                    }
                }
            }
            ResolvablePosition::MethodCall(_expr, _ident, _scope, lazy_span) => {
                if let Some(span) = lazy_span.resolve(db) {
                    if span.range.contains(cursor) {
                        candidates.push((position.clone(), span.range.end() - span.range.start()));
                    }
                }
            }
            ResolvablePosition::LocalBinding(ident, _scope, lazy_span) => {
                if let Some(span) = lazy_span.resolve(db) {
                    if span.range.contains(cursor) {
                        eprintln!(
                            "DEBUG: LocalBinding {} at range {:?} contains cursor {:?}",
                            ident.data(db),
                            span.range,
                            cursor
                        );
                        candidates.push((position.clone(), span.range.end() - span.range.start()));
                    }
                }
            }
            ResolvablePosition::PatternField(_ident, _scope, lazy_span) => {
                if let Some(span) = lazy_span.resolve(db) {
                    if span.range.contains(cursor) {
                        candidates.push((position.clone(), span.range.end() - span.range.start()));
                    }
                }
            }
            ResolvablePosition::FieldDefinition(_field_def, _scope, lazy_span) => {
                if let Some(span) = lazy_span.resolve(db) {
                    if span.range.contains(cursor) {
                        candidates.push((position.clone(), span.range.end() - span.range.start()));
                    }
                }
            }
            ResolvablePosition::VariantDefinition(_variant_def, _scope, lazy_span) => {
                if let Some(span) = lazy_span.resolve(db) {
                    if span.range.contains(cursor) {
                        candidates.push((position.clone(), span.range.end() - span.range.start()));
                    }
                }
            }
            ResolvablePosition::ItemDefinition(_item, lazy_span) => {
                if let Some(span) = lazy_span.resolve(db) {
                    if span.range.contains(cursor) {
                        candidates.push((position.clone(), span.range.end() - span.range.start()));
                    }
                }
            }
        }
    }

    // Now select the best candidate based on priority and range size
    let mut best_position = None;
    let mut best_range_size = None;
    let mut best_priority = None;

    for (position, range_size) in candidates {
        let priority = get_position_priority(&position);

        let is_better = best_range_size.is_none()
            || priority > best_priority.unwrap()
            || (priority == best_priority.unwrap() && range_size < best_range_size.unwrap());

        if is_better {
            best_position = Some(position);
            best_range_size = Some(range_size);
            best_priority = Some(priority);
        }
    }

    best_position
}

/// Find the smallest enclosing item that contains the cursor
pub fn find_enclosing_item<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<ItemKind<'db>> {
    let items = top_mod.scope_graph(db).items_dfs(db);

    let mut smallest_enclosing_item = None;
    let mut smallest_range_size = None;

    for item in items {
        let lazy_item_span = DynLazySpan::from(item.span());
        let item_span = lazy_item_span.resolve(db).unwrap();

        if item_span.range.contains(cursor) {
            let range_size = item_span.range.end() - item_span.range.start();
            if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                smallest_enclosing_item = Some(item);
                smallest_range_size = Some(range_size);
            }
        }
    }

    smallest_enclosing_item
}

/// Helper function to get the definition span for a LocalBinding
pub fn get_local_binding_definition_span<'db>(
    binding: &LocalBinding<'db>,
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Option<DynLazySpan<'db>> {
    match binding {
        LocalBinding::Local { pat, .. } => Some(pat.span(body).into()),
        LocalBinding::Param { idx, .. } => {
            // For parameters, we need to get the function that contains this body
            // and then get the parameter span
            let body_item = body.into();
            if let Some(func) = match body_item {
                ItemKind::Body(_body_def) => {
                    let body_scope = ScopeId::from_item(body_item);
                    if let Some(parent_scope) = body_scope.parent(db) {
                        if let ScopeId::Item(ItemKind::Func(parent_func)) = parent_scope {
                            Some(parent_func)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            } {
                // Get the parameter span using the parameter index
                if let Some(params) = func.params(db).to_opt() {
                    if let Some(_param) = params.data(db).get(*idx) {
                        // Get the parameter name span via the function span
                        let func_span = LazyFuncSpan::new(func);
                        let params_span = func_span.params();
                        let param_span = params_span.param(*idx);
                        Some(param_span.name().into())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

/// Enhanced expression information that includes binding references
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicExprInfo<'db> {
    /// The type of the expression
    pub ty: TyId<'db>,
    /// Whether the expression is mutable
    pub is_mut: bool,
    /// If this expression references a local binding, this contains the binding info
    pub local_binding: Option<LocalBinding<'db>>,
}

impl<'db> PublicExprInfo<'db> {
    /// Create a new expression info without local binding
    pub fn new(ty: TyId<'db>, is_mut: bool) -> Self {
        Self {
            ty,
            is_mut,
            local_binding: None,
        }
    }

    /// Create a new expression info with local binding reference
    pub fn with_binding(ty: TyId<'db>, is_mut: bool, binding: LocalBinding<'db>) -> Self {
        Self {
            ty,
            is_mut,
            local_binding: Some(binding),
        }
    }
}

/// Enhanced function analysis result containing all information needed for language server features
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionAnalysis<'db> {
    /// The function body being analyzed
    pub body: Option<Body<'db>>,
    /// Type information for all patterns in the function
    pub pattern_types: FxHashMap<PatId, TyId<'db>>,
    /// Enhanced expression information including local binding references
    pub expression_info: FxHashMap<ExprId, PublicExprInfo<'db>>,
    /// Map from identifier names to their local bindings in different scopes
    pub local_bindings: FxHashMap<IdentId<'db>, Vec<LocalBinding<'db>>>,
    /// Map from expressions to their local bindings (for variables that reference local bindings)
    pub expr_to_binding: FxHashMap<ExprId, LocalBinding<'db>>,
}

impl<'db> FunctionAnalysis<'db> {
    /// Get the type of an expression
    pub fn expr_type(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.expression_info
            .get(&expr)
            .map(|info| info.ty)
            .unwrap_or_else(|| TyId::invalid(db, crate::ty::ty_def::InvalidCause::Other))
    }

    /// Get the type of a pattern
    pub fn pattern_type(&self, db: &'db dyn HirAnalysisDb, pat: PatId) -> TyId<'db> {
        self.pattern_types
            .get(&pat)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, crate::ty::ty_def::InvalidCause::Other))
    }

    /// Check if an expression references a local binding
    pub fn expr_local_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        self.expr_to_binding.get(&expr).copied()
    }

    /// Find all local bindings for a given identifier name
    pub fn find_local_bindings(&self, ident: IdentId<'db>) -> &[LocalBinding<'db>] {
        self.local_bindings
            .get(&ident)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Get all expressions that reference local bindings
    pub fn all_local_references(&self) -> impl Iterator<Item = (ExprId, LocalBinding<'db>)> + '_ {
        self.expr_to_binding
            .iter()
            .map(|(&expr, &binding)| (expr, binding))
    }
}

/// Analyze a function to extract comprehensive information for language server features
pub fn analyze_function_for_language_server<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> FunctionAnalysis<'db> {
    let (_diags, typed_body) = check_func_body(db, func);

    // Get the function body if available
    let body = func.body(db);

    // Convert internal TypedBody to our public representation
    let mut analysis = FunctionAnalysis {
        body,
        pattern_types: FxHashMap::default(),
        expression_info: FxHashMap::default(),
        local_bindings: FxHashMap::default(),
        expr_to_binding: FxHashMap::default(),
    };

    // Parameter bindings are now handled differently since typed_body doesn't expose them
    // They will be populated when we analyze the body

    // Since we can't access private fields directly, we need to analyze the body structure
    // and extract information through the public interface
    if let Some(body) = body {
        extract_comprehensive_analysis_from_body(db, body, &typed_body, &mut analysis);
    }

    analysis
}

/// Extract comprehensive analysis from a function body using public interfaces
fn extract_comprehensive_analysis_from_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &crate::ty::ty_check::TypedBody<'db>,
    analysis: &mut FunctionAnalysis<'db>,
) {
    use hir::{
        hir_def::{Partial, Pat},
        visitor::{walk_expr, walk_pat, Visitor, VisitorCtxt},
    };

    struct ComprehensiveExtractor<'db, 'a> {
        db: &'db dyn HirAnalysisDb,
        _body: Body<'db>,
        typed_body: &'a crate::ty::ty_check::TypedBody<'db>,
        analysis: &'a mut FunctionAnalysis<'db>,
    }

    impl<'db, 'a> Visitor<'db> for ComprehensiveExtractor<'db, 'a> {
        fn visit_pat(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, hir::span::pat::LazyPatSpan<'db>>,
            pat: PatId,
            pat_data: &Pat<'db>,
        ) {
            // Get pattern type from typed body
            let pat_ty = self.typed_body.pat_ty(self.db, pat);
            self.analysis.pattern_types.insert(pat, pat_ty);

            // Extract identifier bindings from patterns
            if let Pat::Path(Partial::Present(path), ..) = pat_data {
                if let Partial::Present(ident) = path.ident(self.db) {
                    let binding = LocalBinding::Local {
                        pat,
                        is_mut: false, // TODO: Extract mutability from pattern
                    };

                    self.analysis
                        .local_bindings
                        .entry(ident)
                        .or_insert_with(Vec::new)
                        .push(binding);
                }
            }

            walk_pat(self, ctxt, pat);
        }

        fn visit_expr(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, hir::span::expr::LazyExprSpan<'db>>,
            expr: ExprId,
            _expr_data: &Expr<'db>,
        ) {
            // Get expression properties from typed body
            let expr_prop = self.typed_body.expr_prop(self.db, expr);

            // Now we can access the real LocalBinding directly
            let expr_info = if let Some(binding) = expr_prop.binding {
                self.analysis.expr_to_binding.insert(expr, binding);
                PublicExprInfo::with_binding(expr_prop.ty, expr_prop.is_mut, binding)
            } else {
                PublicExprInfo::new(expr_prop.ty, expr_prop.is_mut)
            };

            self.analysis.expression_info.insert(expr, expr_info);

            walk_expr(self, ctxt, expr);
        }
    }

    let mut extractor = ComprehensiveExtractor {
        db,
        _body: body,
        typed_body,
        analysis,
    };

    let mut visitor_ctxt = VisitorCtxt::with_body(db, body);
    extractor.visit_body(&mut visitor_ctxt, body);
}

/// Resolve an identifier to scopes, considering both local bindings and global scope resolution
pub fn resolve_identifier_comprehensive<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    func: Option<Func<'db>>,
) -> IdentifierResolution<'db> {
    // Only handle single-segment paths (identifiers)
    if path.parent(db).is_some() {
        return IdentifierResolution {
            global_scopes: Vec::new(),
            local_bindings: Vec::new(),
        };
    }

    // Get global scope resolution
    let bucket = resolve_ident_to_bucket(db, path, scope);
    let global_scopes = bucket.iter_ok().flat_map(|r| r.scope()).collect();

    // Get local binding resolution if we have a function
    let local_bindings = if let Some(func) = func {
        let analysis = analyze_function_for_language_server(db, func);
        if let Partial::Present(ident) = path.ident(db) {
            analysis.find_local_bindings(ident).to_vec()
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };

    IdentifierResolution {
        global_scopes,
        local_bindings,
    }
}

/// Result of comprehensive identifier resolution
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdentifierResolution<'db> {
    /// Scopes found through global name resolution
    pub global_scopes: Vec<ScopeId<'db>>,
    /// Local bindings found in the current function
    pub local_bindings: Vec<LocalBinding<'db>>,
}

impl<'db> IdentifierResolution<'db> {
    /// Check if any resolution was found
    pub fn is_empty(&self) -> bool {
        self.global_scopes.is_empty() && self.local_bindings.is_empty()
    }

    /// Get all resolved scopes (both global and local binding definition scopes)
    pub fn all_scopes(
        &self,
        _db: &'db dyn HirAnalysisDb,
        body: Option<Body<'db>>,
    ) -> Vec<ScopeId<'db>> {
        let scopes = self.global_scopes.clone();

        // For local bindings, we'd need to compute their definition scopes
        // This is more complex and would require access to the function definition
        if let Some(_body) = body {
            // TODO: Convert local bindings to their definition scopes
            // For now, we only return global scopes
        }

        scopes
    }
}

/// Find all references to an identifier in a function body
pub fn find_all_references_in_function<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    target_ident: IdentId<'db>,
) -> Vec<ExprId> {
    let analysis = analyze_function_for_language_server(db, func);

    analysis
        .all_local_references()
        .filter_map(|(expr_id, binding)| {
            // Check if this binding matches our target identifier
            // This is a simplified check - in practice we'd need to compare
            // the actual identifier names or binding definitions
            let _ = (binding, target_ident);
            Some(expr_id) // TODO: Implement proper matching logic
        })
        .collect()
}

/// Find methods available for a given type by name
/// This includes both inherent methods (from impl blocks) and trait methods
pub fn find_methods_for_type<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
    ty: TyId<'db>,
    method_name: IdentId<'db>,
) -> Vec<FuncDef<'db>> {
    probe_method_for_language_server(db, ingot, ty, method_name).to_vec()
}

/// Resolve a position to its definition(s)
pub fn resolve_position_to_definition<'db>(
    db: &'db dyn HirAnalysisDb,
    position: ResolvablePosition<'db>,
    enclosing_item: ItemKind<'db>,
) -> Option<Vec<Definition<'db>>> {
    match position {
        ResolvablePosition::ItemDefinition(item, _span) => {
            // An item definition simply resolves to itself
            Some(vec![Definition::Scope(item.scope())])
        }
        ResolvablePosition::Path(path, scope, _span) => {
            // Use the original path resolution logic
            use crate::name_resolution::{resolve_ident_to_bucket, resolve_path};
            if path.parent(db).is_none() {
                // Single segment path - use bucket API to get all domains
                if let Some(ident) = path.ident(db).to_opt() {
                    let single_ident_path = PathId::from_ident(db, ident);
                    let bucket = resolve_ident_to_bucket(db, single_ident_path, scope);
                    Some(
                        bucket
                            .iter_ok()
                            .flat_map(|r| r.scope())
                            .map(Definition::Scope)
                            .collect(),
                    )
                } else {
                    None
                }
            } else {
                // Multi-segment path - for constants and functions, we want to use the bucket API
                // to find their definition scope
                if let Some(parent_path) = path.parent(db) {
                    if let Ok(parent_res) = resolve_path(db, parent_path, scope, false) {
                        if let Some(parent_scope) = parent_res.as_scope(db) {
                            // Now resolve the final segment in the parent scope
                            if let Some(ident) = path.ident(db).to_opt() {
                                let bucket = resolve_ident_to_bucket(
                                    db,
                                    PathId::from_ident(db, ident),
                                    parent_scope,
                                );
                                let scopes: Vec<_> = bucket
                                    .iter_ok()
                                    .flat_map(|r| r.scope())
                                    .map(Definition::Scope)
                                    .collect();
                                if !scopes.is_empty() {
                                    return Some(scopes);
                                }
                            }
                        }
                    }
                }

                // Fall back to the original logic for other cases
                let value_resolved = resolve_path(db, path, scope, true);
                match value_resolved {
                    Ok(r) => {
                        // For modules and types, use their scope directly
                        r.as_scope(db).map(|s| vec![Definition::Scope(s)])
                    }
                    Err(_) => {
                        // Fall back to TYPE domain
                        let type_resolved = resolve_path(db, path, scope, false);
                        match type_resolved {
                            Ok(r) => {
                                // For types, try to get their own scope first, otherwise their definition scope
                                if let Some(scope) = r.as_scope(db) {
                                    Some(vec![Definition::Scope(scope)])
                                } else {
                                    // Similar logic for types that are constants
                                    if let Some(parent_path) = path.parent(db) {
                                        if let Ok(parent_res) =
                                            resolve_path(db, parent_path, scope, false)
                                        {
                                            parent_res
                                                .as_scope(db)
                                                .map(|s| vec![Definition::Scope(s)])
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                }
                            }
                            Err(err) => match err.kind {
                                crate::name_resolution::PathResErrorKind::NotFound(bucket) => Some(
                                    bucket
                                        .iter_ok()
                                        .flat_map(|r| r.scope())
                                        .map(Definition::Scope)
                                        .collect(),
                                ),
                                crate::name_resolution::PathResErrorKind::Ambiguous(vec) => Some(
                                    vec.into_iter()
                                        .flat_map(|r| r.scope())
                                        .map(Definition::Scope)
                                        .collect(),
                                ),
                                _ => None,
                            },
                        }
                    }
                }
            }
        }
        ResolvablePosition::MethodCall(receiver_expr, method_ident, scope, _span) => {
            // Get the function context to type the receiver expression
            let func = match enclosing_item {
                ItemKind::Func(func) => func,
                ItemKind::Body(body) => {
                    let body_scope = ScopeId::from_item(ItemKind::Body(body));
                    if let Some(parent_scope) = body_scope.parent(db) {
                        if let ScopeId::Item(ItemKind::Func(parent_func)) = parent_scope {
                            parent_func
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };

            // Get the receiver type
            let analysis = analyze_function_for_language_server(db, func);
            let receiver_ty = analysis.expr_type(db, receiver_expr);

            // The type checker now handles complex expressions correctly, no fallback needed
            // receiver_ty already has the correct type from the type checker

            // Look up both inherent and trait methods using probe_method
            let ingot = scope.ingot(db);
            let method_results = crate::ty::method_table::probe_method_for_language_server(
                db,
                ingot,
                receiver_ty,
                method_ident,
            );

            // Debug output

            if !method_results.is_empty() {
                let scopes: Vec<_> = method_results
                    .iter()
                    .map(|method_def| Definition::Scope(method_def.scope(db)))
                    .collect();
                Some(scopes)
            } else {
                None
            }
        }
        ResolvablePosition::FieldAccess(receiver_expr, field_ident, _scope, _span) => {
            // Get the function context to type the receiver expression
            let func = match enclosing_item {
                ItemKind::Func(func) => func,
                ItemKind::Body(body) => {
                    let body_scope = ScopeId::from_item(ItemKind::Body(body));
                    if let Some(parent_scope) = body_scope.parent(db) {
                        if let ScopeId::Item(ItemKind::Func(parent_func)) = parent_scope {
                            parent_func
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };

            // Get the receiver type and resolve field using RecordLike
            use crate::ty::ty_check::{check_func_body, RecordLike};
            let (_diags, typed_body) = check_func_body(db, func);
            let receiver_ty = typed_body.expr_ty(db, receiver_expr);

            // The type checker now handles complex expressions correctly, no fallback needed
            // receiver_ty already has the correct type from the type checker

            let record_like = RecordLike::from_ty(receiver_ty);
            if let Some(field_scope) = record_like.record_field_scope(db, field_ident) {
                Some(vec![Definition::Scope(field_scope)])
            } else {
                None
            }
        }
        ResolvablePosition::LocalBinding(ident, scope, span) => {
            // Debug: Print if this is 'self'
            if ident.is_self(db) {
                eprintln!("DEBUG: Resolving 'self' LocalBinding at scope {:?}", scope);
            }

            // First check if this position is a parameter definition itself
            // If the scope is a FuncParam scope, then this is the parameter definition
            if let ScopeId::FuncParam(_, _) = scope {
                // This is a parameter definition - return its own location
                return Some(vec![Definition::Location(span.into())]);
            }

            // For local variables, we need the enclosing function to check local bindings
            let func = match enclosing_item {
                ItemKind::Func(func) => Some(func),
                ItemKind::Body(body) => {
                    let body_scope = ScopeId::from_item(ItemKind::Body(body));
                    if let Some(parent_scope) = body_scope.parent(db) {
                        if let ScopeId::Item(ItemKind::Func(parent_func)) = parent_scope {
                            Some(parent_func)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            };

            if let Some(func) = func {
                let path = PathId::from_ident(db, ident);
                let resolution = resolve_identifier_comprehensive(db, path, scope, Some(func));

                // Debug: Print if this is 'self'
                if ident.is_self(db) {
                    eprintln!("DEBUG: resolve_identifier_comprehensive for 'self' returned {} local bindings", resolution.local_bindings.len());
                    eprintln!("DEBUG: scope is {:?}", scope);
                }

                // If we found local bindings, return their precise definition locations
                if !resolution.local_bindings.is_empty() {
                    let body = func.body(db)?;
                    let locations: Vec<_> = resolution
                        .local_bindings
                        .iter()
                        .filter_map(|b| {
                            let span = get_local_binding_definition_span(b, db, body);
                            if ident.is_self(db) {
                                eprintln!(
                                    "DEBUG: Got span for 'self' parameter: {:?}",
                                    span.is_some()
                                );
                            }
                            span
                        })
                        .map(Definition::Location)
                        .collect();
                    if !locations.is_empty() {
                        return Some(locations);
                    }
                }
            } else {
                if ident.is_self(db) {
                    eprintln!("DEBUG: No func found for 'self' resolution");
                }
            }

            // Fall back to global scope resolution only if no local bindings were found
            use crate::name_resolution::resolve_ident_to_bucket;
            let path = PathId::from_ident(db, ident);
            let bucket = resolve_ident_to_bucket(db, path, scope);
            let scopes: Vec<_> = bucket
                .iter_ok()
                .flat_map(|r| r.scope())
                .map(Definition::Scope)
                .collect();
            if scopes.is_empty() {
                None
            } else {
                Some(scopes)
            }
        }
        ResolvablePosition::PatternField(_ident, _scope, _span) => {
            // TODO: Implement pattern field resolution
            None
        }
        ResolvablePosition::FieldDefinition(_field_def, scope, _span) => {
            // A field definition resolves to its containing struct/contract
            Some(vec![Definition::Scope(scope)])
        }
        ResolvablePosition::VariantDefinition(_variant_def, scope, _span) => {
            // A variant definition resolves to its containing enum
            Some(vec![Definition::Scope(scope)])
        }
    }
}

/// High-level function for goto definition - combines position collection, cursor matching, and resolution
pub fn get_goto_definitions<'db, DB>(
    db: &'db DB,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<Definition<'db>>>
where
    DB: SpannedHirDb + HirAnalysisDb,
{
    // Collect all positions
    let positions = collect_resolvable_positions(db, top_mod);
    eprintln!("Collected {} resolvable positions", positions.len());

    // Find the position at cursor
    let position = find_position_at_cursor(db, cursor, positions)?;

    // Debug logging
    eprintln!(
        "Found position at cursor {:?}: {:?}",
        cursor,
        match &position {
            ResolvablePosition::Path(path, _, _) => format!("Path({})", path.pretty_print(db)),
            ResolvablePosition::FieldAccess(_ident, _, _, _) => format!("FieldAccess"),
            ResolvablePosition::MethodCall(_ident, _, _, _) => format!("MethodCall"),
            ResolvablePosition::PatternField(ident, _, _) =>
                format!("PatternField({})", ident.data(db)),
            ResolvablePosition::LocalBinding(ident, _, _) =>
                format!("LocalBinding({})", ident.data(db)),
            ResolvablePosition::FieldDefinition(_ident, _, _) => format!("FieldDefinition"),
            ResolvablePosition::VariantDefinition(_ident, _, _) => format!("VariantDefinition"),
            ResolvablePosition::ItemDefinition(item, _) => format!("ItemDefinition({:?})", item),
        }
    );

    // Find enclosing item for context
    let enclosing_item =
        find_enclosing_item(db, top_mod, cursor).unwrap_or_else(|| ItemKind::TopMod(top_mod));

    // Resolve position to definitions
    let result = resolve_position_to_definition(db, position, enclosing_item);
    eprintln!(
        "Resolution result: {:?}",
        result.as_ref().map(|defs| defs.len())
    );
    result
}

/// Legacy function that returns only scopes (for backwards compatibility)
pub fn get_goto_definition_scopes<'db, DB>(
    db: &'db DB,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>>
where
    DB: SpannedHirDb + HirAnalysisDb,
{
    // Get definitions and filter to only scopes
    let definitions = get_goto_definitions(db, top_mod, cursor)?;
    let scopes: Vec<_> = definitions
        .into_iter()
        .filter_map(|def| match def {
            Definition::Scope(scope) => Some(scope),
            Definition::Location(_) => None,
        })
        .collect();

    if scopes.is_empty() {
        None
    } else {
        Some(scopes)
    }
}

/// Find all references to a symbol throughout the module
/// This is groundwork for "Find All References" LSP feature
pub fn find_all_references<'db, DB>(
    db: &'db DB,
    top_mod: TopLevelMod<'db>,
    target_scope: ScopeId<'db>,
) -> Vec<ResolvablePosition<'db>>
where
    DB: SpannedHirDb + HirAnalysisDb,
{
    let positions = collect_resolvable_positions(db, top_mod);
    positions
        .into_iter()
        .filter(|position| {
            let enclosing_item = find_enclosing_item_for_position(db, top_mod, position)
                .unwrap_or_else(|| ItemKind::TopMod(top_mod));

            if let Some(definitions) =
                resolve_position_to_definition(db, position.clone(), enclosing_item)
            {
                definitions.iter().any(|def| match def {
                    Definition::Scope(scope) => *scope == target_scope,
                    Definition::Location(_) => false, // Local variables don't match scope targets
                })
            } else {
                false
            }
        })
        .collect()
}

/// Get hover information for a position - combines goto resolution with additional context
/// This enhances the existing hover functionality
pub fn get_hover_info<'db, DB>(
    db: &'db DB,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<(Vec<ScopeId<'db>>, ResolvablePosition<'db>)>
where
    DB: SpannedHirDb + HirAnalysisDb,
{
    let positions = collect_resolvable_positions(db, top_mod);
    let position = find_position_at_cursor(db, cursor, positions)?;

    let enclosing_item =
        find_enclosing_item(db, top_mod, cursor).unwrap_or_else(|| ItemKind::TopMod(top_mod));

    if let Some(definitions) = resolve_position_to_definition(db, position.clone(), enclosing_item)
    {
        // Extract scopes from definitions (ignore locations)
        let scopes: Vec<_> = definitions
            .into_iter()
            .filter_map(|def| match def {
                Definition::Scope(scope) => Some(scope),
                Definition::Location(_) => None,
            })
            .collect();

        if !scopes.is_empty() {
            Some((scopes, position))
        } else {
            None
        }
    } else {
        None
    }
}

/// Helper function to find enclosing item for a position
fn find_enclosing_item_for_position<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    position: &ResolvablePosition<'db>,
) -> Option<ItemKind<'db>> {
    // Extract cursor from position and find enclosing item
    let cursor = match position {
        ResolvablePosition::Path(_, _, span) => {
            if let Some(resolved_span) = span.clone().resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
        ResolvablePosition::FieldAccess(_, _, _, span) => {
            if let Some(resolved_span) = span.resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
        ResolvablePosition::MethodCall(_, _, _, span) => {
            if let Some(resolved_span) = span.resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
        ResolvablePosition::LocalBinding(_, _, span) => {
            if let Some(resolved_span) = span.resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
        ResolvablePosition::PatternField(_, _, span) => {
            if let Some(resolved_span) = span.resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
        ResolvablePosition::FieldDefinition(_, _, span) => {
            if let Some(resolved_span) = span.resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
        ResolvablePosition::VariantDefinition(_, _, span) => {
            if let Some(resolved_span) = span.resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
        ResolvablePosition::ItemDefinition(_, span) => {
            if let Some(resolved_span) = span.resolve(db) {
                resolved_span.range.start()
            } else {
                return None;
            }
        }
    };

    find_enclosing_item(db, top_mod, cursor)
}

// ============================================================================
// TYPE INFORMATION API (for hover support)
// ============================================================================

/// Returns the pretty-printed type of the expression at the given cursor position.
pub fn type_of_expression_at_cursor<'db, DB>(
    db: &'db DB,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<String>
where
    DB: SpannedHirDb + HirAnalysisDb,
{
    let enclosing_item = find_enclosing_item(db, top_mod, cursor)?;
    let enclosing_func = match enclosing_item {
        ItemKind::Func(func) => func,
        ItemKind::Body(body) => {
            // If we're in a body, get the containing function
            let body_scope = ScopeId::from_item(ItemKind::Body(body));
            if let Some(parent_scope) = body_scope.parent(db) {
                if let ScopeId::Item(ItemKind::Func(parent_func)) = parent_scope {
                    parent_func
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        _ => return None,
    };

    // We need to find the `ExprId` under the cursor.
    let body = enclosing_func.body(db)?;
    let (_diags, typed_body) = check_func_body(db, enclosing_func);

    // Find the smallest expression that contains the cursor.
    let mut best_expr: Option<ExprId> = None;
    let mut smallest_span_size = usize::MAX;

    for expr_id in body.exprs(db).iter().map(|(id, _)| id) {
        let expr_span = LazyExprSpan::new(body, expr_id);
        if let Some(span) = expr_span.resolve(db) {
            if span.range.contains(cursor) {
                let size = usize::from(span.range.end()) - usize::from(span.range.start());
                if size < smallest_span_size {
                    smallest_span_size = size;
                    best_expr = Some(expr_id);
                }
            }
        }
    }

    let target_expr = best_expr?;
    let ty = typed_body.expr_ty(db, target_expr);

    if ty.has_invalid(db) {
        None
    } else {
        Some(ty.pretty_print(db).to_string())
    }
}
