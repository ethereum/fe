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

use hir::{
    hir_def::{scope_graph::ScopeId, Body, Expr, ExprId, FieldIndex, Func, IdentId, ItemKind, Partial, Pat, PatId, PathId, TopLevelMod},
    span::{DynLazySpan, LazySpan, LazySpanAtom},
    visitor::{
        prelude::{LazyExprSpan, LazyPatSpan, LazyPathSpan},
        Visitor, VisitorCtxt, walk_expr, walk_pat,
    },
    SpannedHirDb,
};
use rustc_hash::FxHashMap;

use crate::{
    name_resolution::resolve_ident_to_bucket,
    ty::{
        func_def::FuncDef,
        method_table::probe_method_for_language_server,
        ty_check::check_func_body,
        ty_def::TyId,
    },
    HirAnalysisDb,
};

/// Text cursor position (compatible with language server protocols)  
pub type Cursor = parser::TextSize;

// ============================================================================
// POSITION COLLECTION AND RESOLUTION
// ============================================================================

/// Different types of positions that can be resolved to definitions
#[derive(Debug, Clone)]
pub enum ResolvablePosition<'db> {
    /// Path references like `Color::Red`, `std::collections::Map`
    Path(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>),
    /// Field access like `green.intensity`, `self.value`
    FieldAccess(ExprId, IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// Method calls like `container.get()`, `container.display()`
    MethodCall(ExprId, IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// Local variable references like `local_var`, function parameters
    LocalVariable(IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
    /// Pattern field references like `intensity` in `Green { intensity }`
    PatternField(IdentId<'db>, ScopeId<'db>, LazySpanAtom<'db>),
}

/// Collector for gathering resolvable positions in code
#[derive(Default)]
struct PositionCollector<'db> {
    positions: Vec<ResolvablePosition<'db>>,
}

impl<'db, 'ast: 'db> Visitor<'ast> for PositionCollector<'db> {

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>, path: PathId<'db>) {
        let Some(span) = ctxt.span() else {
            return;
        };
        let scope = ctxt.scope();
        
        // Create position for the full path
        self.positions.push(ResolvablePosition::Path(path, scope, span.clone()));
        
        // For multi-segment paths, create positions for each individual segment
        // Each segment gets its own path and its own span within the full path span
        let segment_count = path.segment_index(ctxt.db()) + 1;
        for segment_idx in 0..segment_count {
            if let Some(segment_path) = path.segment(ctxt.db(), segment_idx) {
                // Create a position for this specific segment
                // Note: We still use the full path span here because the segment span
                // resolution happens in find_position_at_cursor during cursor matching
                self.positions.push(ResolvablePosition::Path(segment_path, scope, span.clone()));
            }
        }
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'ast, LazyExprSpan<'ast>>,
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
                    let method_span = ctxt
                        .span()
                        .map(|span| {
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

        // Continue with default traversal
        walk_expr(self, ctxt, expr);
    }

    fn visit_ident(
        &mut self,
        ctxt: &mut VisitorCtxt<'ast, LazySpanAtom<'ast>>,
        ident: IdentId<'db>,
    ) {
        let Some(span) = ctxt.span() else {
            return;
        };
        let scope = ctxt.scope();
        // Collect as potential local variable reference
        self.positions
            .push(ResolvablePosition::LocalVariable(ident, scope, span));
    }

    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'ast, LazyPatSpan<'ast>>,
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
}

/// Collect all resolvable positions in a top-level module
pub fn collect_resolvable_positions<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<ResolvablePosition<'db>> {
    let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
    let mut collector = PositionCollector::default();
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
            ResolvablePosition::FieldAccess(_, _, _, _) => 5,
            ResolvablePosition::MethodCall(_, _, _, _) => 4,
            ResolvablePosition::PatternField(_, _, _) => 3,
            ResolvablePosition::LocalVariable(_, _, _) => 2,
            ResolvablePosition::Path(_, _, _) => 1,
        }
    }
    
    // Collect all candidates first, then apply priority logic
    let mut candidates = Vec::new();
    
    for position in positions {
        match &position {
            ResolvablePosition::Path(path, scope, lazy_span) => {
                // First check if cursor is in a specific segment (higher priority)
                for idx in 0..=path.segment_index(db) {
                    if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                        if seg_span.range.contains(cursor) {
                            if let Some(segment_path) = path.segment(db, idx) {
                                let seg_position = ResolvablePosition::Path(
                                    segment_path,
                                    *scope,
                                    lazy_span.clone(),
                                );
                                candidates.push((seg_position, seg_span.range.end() - seg_span.range.start()));
                            }
                        }
                    }
                }
                
                // Also add the full path position if it contains cursor
                let span = lazy_span.resolve(db)?;
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                }
            }
            ResolvablePosition::FieldAccess(_expr, _ident, _scope, lazy_span) => {
                let span = lazy_span.resolve(db)?;
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                }
            }
            ResolvablePosition::MethodCall(_expr, _ident, _scope, lazy_span) => {
                let span = lazy_span.resolve(db)?;
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                }
            }
            ResolvablePosition::LocalVariable(_ident, _scope, lazy_span) => {
                let span = lazy_span.resolve(db)?;
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                }
            }
            ResolvablePosition::PatternField(_ident, _scope, lazy_span) => {
                let span = lazy_span.resolve(db)?;
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
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

/// Public representation of a local binding (variable, parameter, pattern)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PublicLocalBinding<'db> {
    /// Local variable binding from a pattern
    Local {
        /// The pattern ID where this variable is defined
        pat: PatId,
        /// Whether the binding is mutable
        is_mut: bool,
    },
    /// Function parameter binding
    Param {
        /// Parameter index in the function signature
        idx: usize,
        /// The type of the parameter
        ty: TyId<'db>,
        /// Whether the parameter is mutable
        is_mut: bool,
    },
}

impl<'db> PublicLocalBinding<'db> {
    /// Check if this binding is mutable
    pub fn is_mut(&self) -> bool {
        match self {
            Self::Local { is_mut, .. } | Self::Param { is_mut, .. } => *is_mut,
        }
    }

    /// Get the definition span for this binding
    pub fn definition_span(&self, db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Option<DynLazySpan<'db>> {
        match self {
            Self::Local { pat, .. } => Some(pat.span(body).into()),
            Self::Param { idx, .. } => {
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
                    Some(func.span().params().param(*idx).name().into())
                } else {
                    None
                }
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
    pub local_binding: Option<PublicLocalBinding<'db>>,
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
    pub fn with_binding(ty: TyId<'db>, is_mut: bool, binding: PublicLocalBinding<'db>) -> Self {
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
    pub local_bindings: FxHashMap<IdentId<'db>, Vec<PublicLocalBinding<'db>>>,
    /// Map from expressions to their local bindings (for variables that reference local bindings)
    pub expr_to_binding: FxHashMap<ExprId, PublicLocalBinding<'db>>,
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
    pub fn expr_local_binding(&self, expr: ExprId) -> Option<PublicLocalBinding<'db>> {
        self.expr_to_binding.get(&expr).copied()
    }

    /// Find all local bindings for a given identifier name
    pub fn find_local_bindings(&self, ident: IdentId<'db>) -> &[PublicLocalBinding<'db>] {
        self.local_bindings.get(&ident).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Get all expressions that reference local bindings
    pub fn all_local_references(&self) -> impl Iterator<Item = (ExprId, PublicLocalBinding<'db>)> + '_ {
        self.expr_to_binding.iter().map(|(&expr, &binding)| (expr, binding))
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
                    let binding = PublicLocalBinding::Local {
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
            
            // Convert local binding if present
            let public_binding = expr_prop.binding.map(|_binding| {
                // Since we can't access the private LocalBinding enum directly,
                // we'll need a different approach. For now, we'll create a placeholder
                // that indicates we found a binding but can't extract its details
                // This is a limitation of the current private API structure
                
                // TODO: This needs to be fixed once we have access to LocalBinding details
                // For now, create a dummy binding
                PublicLocalBinding::Local {
                    pat: PatId::from_u32(0), // Placeholder
                    is_mut: expr_prop.is_mut,
                }
            });

            let expr_info = if let Some(binding) = public_binding {
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
    pub local_bindings: Vec<PublicLocalBinding<'db>>,
}

impl<'db> IdentifierResolution<'db> {
    /// Check if any resolution was found
    pub fn is_empty(&self) -> bool {
        self.global_scopes.is_empty() && self.local_bindings.is_empty()
    }

    /// Get all resolved scopes (both global and local binding definition scopes)
    pub fn all_scopes(&self, _db: &'db dyn HirAnalysisDb, body: Option<Body<'db>>) -> Vec<ScopeId<'db>> {
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

/// Resolve a position to its definition scopes
pub fn resolve_position_to_scopes<'db>(
    db: &'db dyn HirAnalysisDb,
    position: ResolvablePosition<'db>,
    enclosing_item: ItemKind<'db>,
) -> Option<Vec<ScopeId<'db>>> {
    match position {
        ResolvablePosition::Path(path, scope, _span) => {
            // Use the original path resolution logic
            use crate::name_resolution::{resolve_ident_to_bucket, resolve_path};
            if path.parent(db).is_none() {
                // Single segment path - use bucket API to get all domains
                let bucket = resolve_ident_to_bucket(db, path, scope);
                Some(bucket.iter_ok().flat_map(|r| r.scope()).collect())
            } else {
                // Multi-segment path - try both VALUE and TYPE domains
                let value_resolved = resolve_path(db, path, scope, true);
                match value_resolved {
                    Ok(r) => Some(r.as_scope(db).into_iter().collect::<Vec<_>>()),
                    Err(_) => {
                        // Fall back to TYPE domain
                        let type_resolved = resolve_path(db, path, scope, false);
                        match type_resolved {
                            Ok(r) => Some(r.as_scope(db).into_iter().collect::<Vec<_>>()),
                            Err(err) => match err.kind {
                                crate::name_resolution::PathResErrorKind::NotFound(bucket) => {
                                    Some(bucket.iter_ok().flat_map(|r| r.scope()).collect())
                                }
                                crate::name_resolution::PathResErrorKind::Ambiguous(vec) => {
                                    Some(vec.into_iter().flat_map(|r| r.scope()).collect())
                                }
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
            
            // Look up both inherent and trait methods using probe_method
            let ingot = scope.ingot(db);
            let method_results = crate::ty::method_table::probe_method_for_language_server(
                db, 
                ingot, 
                receiver_ty, 
                method_ident
            );
            
            if !method_results.is_empty() {
                let scopes: Vec<_> = method_results
                    .iter()
                    .map(|method_def| method_def.scope(db))
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
            let record_like = RecordLike::from_ty(receiver_ty);

            if let Some(field_scope) = record_like.record_field_scope(db, field_ident) {
                Some(vec![field_scope])
            } else {
                None
            }
        }
        ResolvablePosition::LocalVariable(ident, scope, _span) => {
            // Resolve local variable through scope using the original approach
            use crate::name_resolution::resolve_ident_to_bucket;
            let path = PathId::from_ident(db, ident);
            let bucket = resolve_ident_to_bucket(db, path, scope);
            Some(bucket.iter_ok().flat_map(|r| r.scope()).collect())
        }
        ResolvablePosition::PatternField(_ident, _scope, _span) => {
            // TODO: Implement pattern field resolution
            None
        }
    }
}

/// High-level function for goto definition - combines position collection, cursor matching, and resolution
pub fn get_goto_definition_scopes<'db, DB>(
    db: &'db DB,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>>
where
    DB: SpannedHirDb + HirAnalysisDb,
{
    // Collect all positions
    let positions = collect_resolvable_positions(db, top_mod);
    
    // Find the position at cursor
    let position = find_position_at_cursor(db, cursor, positions)?;
    
    // Find enclosing item for context
    let enclosing_item = find_enclosing_item(db, top_mod, cursor)
        .unwrap_or_else(|| ItemKind::TopMod(top_mod));
    
    // Resolve position to scopes
    resolve_position_to_scopes(db, position, enclosing_item)
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
            
            if let Some(scopes) = resolve_position_to_scopes(db, position.clone(), enclosing_item) {
                scopes.contains(&target_scope)
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
    
    let enclosing_item = find_enclosing_item(db, top_mod, cursor)
        .unwrap_or_else(|| ItemKind::TopMod(top_mod));
    
    if let Some(scopes) = resolve_position_to_scopes(db, position.clone(), enclosing_item) {
        Some((scopes, position))
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
        ResolvablePosition::LocalVariable(_, _, span) => {
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
    };
    
    find_enclosing_item(db, top_mod, cursor)
}