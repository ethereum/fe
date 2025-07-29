use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    hir_def::{
        scope_graph::ScopeId, Expr, ExprId, FieldIndex, IdentId, ItemKind, Partial, Pat, PatId,
        PathId, TopLevelMod,
    },
    lower::map_file_to_mod,
    span::{DynLazySpan, LazySpan, LazySpanAtom},
    visitor::{
        prelude::{LazyExprSpan, LazyPatSpan, LazyPathSpan},
        Visitor, VisitorCtxt,
    },
    SpannedHirDb,
};
use hir_analysis::name_resolution::{available_traits_in_scope, resolve_ident_to_bucket, resolve_path, PathResErrorKind};
use tracing::error;

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_scope, to_lsp_location_from_lazy_span, to_offset_from_position},
};
use driver::DriverDataBase;
pub type Cursor = parser::TextSize;


#[derive(Debug, Clone)]
enum ResolvablePosition<'db> {
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

#[derive(Default)]
struct ComprehensiveCollector<'db> {
    positions: Vec<ResolvablePosition<'db>>,
}

impl<'db, 'ast: 'db> Visitor<'ast> for ComprehensiveCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>, path: PathId<'db>) {
        use hir::hir_def::Partial;
        let Some(span) = ctxt.span() else {
            return;
        };
        let scope = ctxt.scope();
        
        // Always create the Path position for the full path
        self.positions.push(ResolvablePosition::Path(path, scope, span.clone()));
        
        // For single-segment paths, also create a LocalVariable position for local variable resolution
        if path.parent(ctxt.db()).is_none() {
            if let Partial::Present(ident) = path.ident(ctxt.db()) {
                let ident_span = span.segment(0).ident();
                self.positions.push(ResolvablePosition::LocalVariable(
                    ident,
                    scope,
                    ident_span,
                ));
            }
        }
    }

    // TODO: Add item definition collection back once we understand the span API better
    // For now, focus on getting expressions and method calls working correctly

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'ast, LazyExprSpan<'ast>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        // Only log method calls for now
        if let Expr::MethodCall(..) = expr_data {
            eprintln!("DEBUG: Found MethodCall expr");
        }
        
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
                    if let Some(method_span) = ctxt
                        .span()
                        .map(|span| span.into_method_call_expr().method_name())
                    {
                        let scope = ctxt.scope();
                        // Get the actual span to debug cursor matching (need SpannedHirDb)
                        // We'll debug this in the cursor matching function instead
                        self.positions.push(ResolvablePosition::MethodCall(
                            *receiver_expr,
                            *method_ident,
                            scope,
                            method_span,
                        ));
                    }
                }
            }
            // Note: Don't intercept Expr::Path here - let the normal visit_path handle it
            // The visit_path method will be called automatically for path expressions
            // and we can decide there whether to treat them as paths or local variables
            _ => {}
        }

        // Continue with default traversal
        hir::visitor::walk_expr(self, ctxt, expr);
    }

    // Note: Local variables are handled as paths, not standalone identifiers
    // This method is mainly for identifiers in other contexts like struct field names
    fn visit_ident(
        &mut self,
        _ctxt: &mut VisitorCtxt<'ast, LazySpanAtom<'ast>>,
        _ident: IdentId<'db>,
    ) {
        // Don't collect standalone identifiers as they're usually part of definitions
        // Local variable references are handled as Expr::Path in visit_path
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
        hir::visitor::walk_pat(self, ctxt, pat);
    }
}

fn find_position_at_cursor<'db>(
    db: &'db DriverDataBase,
    cursor: Cursor,
    positions: Vec<ResolvablePosition<'db>>,
) -> Option<ResolvablePosition<'db>> {
    // Find the most specific position that contains the cursor
    // Priority: LocalVariable > FieldAccess > MethodCall > PatternField > Path
    let mut best_position = None;
    let mut best_range_size = None;
    let mut best_priority = None;

    let mut matching_positions = 0;
    
    // Helper function to get position priority (higher number = higher priority)
    fn get_position_priority(pos: &ResolvablePosition) -> u32 {
        match pos {
            ResolvablePosition::LocalVariable(_, _, _) => 5,
            ResolvablePosition::FieldAccess(_, _, _, _) => 4,
            ResolvablePosition::MethodCall(_, _, _, _) => 3,
            ResolvablePosition::PatternField(_, _, _) => 2,
            ResolvablePosition::Path(_, _, _) => 1,
        }
    }
    
    // Collect all candidates first, then apply priority logic
    let mut candidates = Vec::new();
    
    for position in positions {
        match &position {
            ResolvablePosition::Path(path, scope, lazy_span) => {
                let span = match lazy_span.resolve(db) {
                    Some(s) => s,
                    None => continue,
                };
                if span.range.contains(cursor) {
                    // Add the full path position
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                    
                    // For multi-segment paths, also add specific segment positions
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
                }
            }
            ResolvablePosition::FieldAccess(_expr, _ident, _scope, lazy_span) => {
                let span = match lazy_span.resolve(db) {
                    Some(s) => s,
                    None => continue,
                };
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                }
            }
            ResolvablePosition::MethodCall(_expr, _ident, _scope, lazy_span) => {
                let span = match lazy_span.resolve(db) {
                    Some(s) => s,
                    None => {
                        eprintln!("DEBUG: MethodCall span failed to resolve");
                        continue;
                    }
                };
                // Debug all method call matching for now
                let cursor_byte: usize = cursor.into();
                if cursor_byte >= 490 && cursor_byte <= 495 {  // Focus on get() method specifically
                    eprintln!("DEBUG: Testing MethodCall at range {:?} against cursor {:?} (byte {})", span.range, cursor, cursor_byte);
                }
                if span.range.contains(cursor) {
                    eprintln!("DEBUG: MethodCall MATCHES cursor {} at range {:?}!", cursor_byte, span.range);
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                } else if cursor_byte >= 490 && cursor_byte <= 495 {
                    eprintln!("DEBUG: MethodCall does NOT match cursor {} (range {:?})", cursor_byte, span.range);
                }
            }
            ResolvablePosition::LocalVariable(_ident, _scope, lazy_span) => {
                let span = match lazy_span.resolve(db) {
                    Some(s) => s,
                    None => continue,
                };
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                }
            }
            ResolvablePosition::PatternField(_ident, _scope, lazy_span) => {
                let span = match lazy_span.resolve(db) {
                    Some(s) => s,
                    None => continue,
                };
                if span.range.contains(cursor) {
                    candidates.push((position.clone(), span.range.end() - span.range.start()));
                }
            }
        }
    }

    // Now select the best candidate based on priority and range size
    for (position, range_size) in candidates {
        matching_positions += 1;
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

fn find_pattern_definition_scope<'db>(
    _db: &'db DriverDataBase,
    func: hir::hir_def::Func<'db>,
    _pat_id: hir::hir_def::PatId,
) -> Option<hir::hir_def::scope_graph::ScopeId<'db>> {
    // For now, fallback to function scope
    // TODO: In the future, we could create more specific scopes for patterns
    Some(func.scope())
}

fn find_param_definition_scope<'db>(
    _db: &'db DriverDataBase,
    func: hir::hir_def::Func<'db>,
    param_idx: usize,
) -> Option<hir::hir_def::scope_graph::ScopeId<'db>> {
    // For function parameters, we can create a more specific scope
    // that points to the parameter definition
    
    // Create a FuncParam scope for the specific parameter
    Some(hir::hir_def::scope_graph::ScopeId::FuncParam(
        hir::hir_def::ItemKind::Func(func),
        param_idx as u16,
    ))
}

fn resolve_inherent_methods<'db>(
    db: &'db DriverDataBase,
    receiver_ty: hir_analysis::ty::ty_def::TyId<'db>,
    method_ident: IdentId<'db>,
    scope: ScopeId<'db>,
) -> Option<Vec<ScopeId<'db>>> {
    // Get the ingot from the scope
    let ingot = scope.ingot(db);
    
    let methods = hir_analysis::language_server_support::find_methods_for_type(
        db, ingot, receiver_ty, method_ident
    );
    
    if !methods.is_empty() {
        let scopes: Vec<_> = methods
            .into_iter()
            .map(|method_info| method_info.definition_scope)
            .collect();
        Some(scopes)
    } else {
        None
    }
}

fn resolve_method_call<'db>(
    db: &'db DriverDataBase,
    receiver_expr: ExprId,
    method_ident: IdentId<'db>,
    scope: ScopeId<'db>,
    enclosing_item: ItemKind<'db>,
) -> Option<Vec<ScopeId<'db>>> {
    use hir_analysis::ty::ty_check::{check_func_body, RecordLike};

    // Get the function containing this method call
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

    // Get the typed body and resolve the receiver type
    let (_diags, typed_body) = check_func_body(db, func);
    let receiver_ty = typed_body.expr_ty(db, receiver_expr);
    // First, try to find inherent methods using type-directed resolution
    // We need to look for impl blocks that implement methods for the receiver type
    if let Some(inherent_methods) = resolve_inherent_methods(db, receiver_ty, method_ident, scope) {
        if !inherent_methods.is_empty() {
            return Some(inherent_methods);
        }
    }
    
    // Try to resolve the method using the receiver type for fields (fallback)
    let record_like = RecordLike::from_ty(receiver_ty);
    if let Some(method_scope) = record_like.record_field_scope(db, method_ident) {
        return Some(vec![method_scope]);
    }

    // Check traits in scope for method resolution
    let available_traits = available_traits_in_scope(db, scope);
    let mut trait_method_scopes = Vec::new();
    
    for &trait_hir in available_traits {
        // Look through the trait's methods to find one with the matching name
        for method_func in trait_hir.methods(db) {
            if let Partial::Present(func_name) = method_func.name(db) {
                if func_name == method_ident {
                    // Found the method in this trait, get its scope
                    let method_scope = method_func.scope();
                    trait_method_scopes.push(method_scope);
                }
            }
        }
    }
    
    if !trait_method_scopes.is_empty() {
        return Some(trait_method_scopes);
    }

    // Fallback to identifier resolution for inherent methods
    let method_path = PathId::from_ident(db, method_ident);
    let bucket = resolve_ident_to_bucket(db, method_path, scope);
    
    // Try to get methods from the VALUE domain specifically
    if let Ok(res) = bucket.pick(hir_analysis::name_resolution::NameDomain::VALUE) {
        if let Some(scope) = res.scope() {
            Some(vec![scope])
        } else {
            None
        }
    } else {
        // If VALUE domain fails, try all available resolutions
        let scopes: Vec<_> = bucket.iter_ok().filter_map(|r| r.scope()).collect();
        if scopes.is_empty() {
            None
        } else {
            Some(scopes)
        }
    }
}


fn resolve_position<'db>(
    db: &'db DriverDataBase,
    position: ResolvablePosition<'db>,
    enclosing_item: ItemKind<'db>,
) -> Option<Vec<ScopeId<'db>>> {
    use hir_analysis::ty::ty_check::{check_func_body, RecordLike};

    match position {
        ResolvablePosition::Path(path, scope, _span) => {
            // For single-segment paths, use comprehensive resolution (covers both global items and local variables)
            if path.parent(db).is_none() {
                // Single-segment: use comprehensive API
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
                
                let resolution = hir_analysis::language_server_support::resolve_identifier_comprehensive(
                    db, path, scope, func
                );
                
                let mut all_scopes = Vec::new();
                
                // Add global scopes
                all_scopes.extend(resolution.global_scopes);
                
                // Add local bindings (prioritize these for single-segment identifiers)
                if !resolution.local_bindings.is_empty() && func.is_some() {
                    for binding in &resolution.local_bindings {
                        match binding {
                            hir_analysis::language_server_support::PublicLocalBinding::Local { pat, .. } => {
                                // Try to get the actual pattern scope for local variables
                                // PatId doesn't directly convert to a scope, so we need to find
                                // the containing scope of the pattern
                                if let Some(pat_scope) = find_pattern_definition_scope(db, func.unwrap(), *pat) {
                                    all_scopes.push(pat_scope);
                                } else {
                                    // Fallback to function scope if we can't find the pattern scope
                                    all_scopes.push(func.unwrap().scope());
                                }
                            }
                            hir_analysis::language_server_support::PublicLocalBinding::Param { idx, .. } => {
                                // For parameters, try to get the parameter scope
                                if let Some(param_scope) = find_param_definition_scope(db, func.unwrap(), *idx) {
                                    all_scopes.push(param_scope);
                                } else {
                                    // Fallback to function scope
                                    all_scopes.push(func.unwrap().scope());
                                }
                            }
                        }
                    }
                }
                
                Some(all_scopes)
            } else {
                // Multi-segment: use traditional path resolution
                let value_resolved = resolve_path(db, path, scope, true);
                match value_resolved {
                    Ok(r) => Some(r.as_scope(db).into_iter().collect::<Vec<_>>()),
                    Err(_) => {
                        // Fall back to TYPE domain
                        let type_resolved = resolve_path(db, path, scope, false);
                        match type_resolved {
                            Ok(r) => Some(r.as_scope(db).into_iter().collect::<Vec<_>>()),
                            Err(err) => match err.kind {
                                PathResErrorKind::NotFound(bucket) => {
                                    Some(bucket.iter_ok().flat_map(|r| r.scope()).collect())
                                }
                                PathResErrorKind::Ambiguous(vec) => {
                                    Some(vec.into_iter().flat_map(|r| r.scope()).collect())
                                }
                                _ => None,
                            },
                        }
                    }
                }
            }
        }
        ResolvablePosition::FieldAccess(receiver_expr, field_ident, _scope, _span) => {
            // Get the function containing this field access
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

            // Get the typed body and resolve field
            let (_diags, typed_body) = check_func_body(db, func);
            let receiver_ty = typed_body.expr_ty(db, receiver_expr);
            let record_like = RecordLike::from_ty(receiver_ty);

            if let Some(field_scope) = record_like.record_field_scope(db, field_ident) {
                Some(vec![field_scope])
            } else {
                None
            }
        }
        ResolvablePosition::MethodCall(receiver_expr, method_ident, scope, _span) => {
            eprintln!("DEBUG: Resolving MethodCall for method_ident");
            let result = resolve_method_call(db, receiver_expr, method_ident, scope, enclosing_item);
            if let Some(ref scopes) = result {
                eprintln!("DEBUG: MethodCall resolution SUCCESS - found {} scopes", scopes.len());
            } else {
                eprintln!("DEBUG: MethodCall resolution FAILED - no scopes found");
            }
            result
        }
        ResolvablePosition::LocalVariable(ident, scope, _span) => {
            // Use comprehensive resolution for local variables
            let path = PathId::from_ident(db, ident);
            
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
            
            let resolution = hir_analysis::language_server_support::resolve_identifier_comprehensive(
                db, path, scope, func
            );
            
            let mut all_scopes = Vec::new();
            
            // Add local bindings first (prioritize for local variables)
            if !resolution.local_bindings.is_empty() && func.is_some() {
                for _binding in &resolution.local_bindings {
                    // For local variables, return the function scope as a simplification
                    all_scopes.push(func.unwrap().scope());
                }
            }
            
            // Add global scopes as fallback
            all_scopes.extend(resolution.global_scopes);
            
            Some(all_scopes)
        }
        ResolvablePosition::PatternField(ident, scope, _span) => {
            // Resolve pattern field - similar to local variable for now
            let path = PathId::from_ident(db, ident);
            let bucket = resolve_ident_to_bucket(db, path, scope);
            Some(bucket.iter_ok().flat_map(|r| r.scope()).collect())
        }
    }
}

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

pub fn get_goto_target_scopes_for_cursor_with_positions<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
    positions: Vec<ResolvablePosition<'db>>,
) -> Option<Vec<ScopeId<'db>>> {
    // Use the provided positions instead of collecting fresh ones
    if let Some(position) = find_position_at_cursor(db, cursor, positions) {
        // Still need to find the enclosing item for context in resolution  
        let item = find_enclosing_item(db, top_mod, cursor).unwrap_or_else(|| {
            // Fallback to the top-level module item if no specific enclosing item found
            ItemKind::TopMod(top_mod)
        });
        
        return resolve_position(db, position, item);
    }

    None
}

pub fn get_goto_target_scopes_for_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>> {
    // Collect positions from the entire top-level module to ensure we capture
    // all possible resolvable positions (item names, fields, variants, etc.)
    let cursor_byte: usize = cursor.into();
    if cursor_byte >= 490 && cursor_byte <= 495 {
        eprintln!("DEBUG: get_goto_target_scopes_for_cursor called with cursor {:?} (byte {})", cursor, cursor_byte);
    }
    let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
    let mut collector = ComprehensiveCollector::default();
    collector.visit_top_mod(&mut visitor_ctxt, top_mod);

    // Use the helper function with the collected positions
    get_goto_target_scopes_for_cursor_with_positions(db, top_mod, cursor, collector.positions)
}

/// Get precise local variable definition locations using direct span information
fn get_precise_local_variable_locations(
    db: &DriverDataBase,
    top_mod: TopLevelMod,
    cursor: Cursor,
) -> Option<Vec<Result<async_lsp::lsp_types::Location, Box<dyn std::error::Error>>>> {
    // Find the enclosing function
    let enclosing_item = find_enclosing_item(db, top_mod, cursor)?;
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

    // Find the resolvable position at cursor
    let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
    let mut collector = ComprehensiveCollector::default();
    collector.visit_top_mod(&mut visitor_ctxt, top_mod);
    
    let position = find_position_at_cursor(db, cursor, collector.positions)?;
    
    match position {
        ResolvablePosition::LocalVariable(ident, scope, _span) => {
            // Use the comprehensive resolution to get local bindings
            let path = PathId::from_ident(db, ident);
            let resolution = hir_analysis::language_server_support::resolve_identifier_comprehensive(
                db, path, scope, Some(func)
            );
            
            let mut precise_locations = Vec::new();
            let body = func.body(db)?;
            
            // For each local binding, get its definition span directly
            for binding in &resolution.local_bindings {
                if let Some(def_span) = binding.definition_span(db, body) {
                    let location = to_lsp_location_from_lazy_span(db, def_span);
                    precise_locations.push(location);
                }
            }
            
            if !precise_locations.is_empty() {
                Some(precise_locations)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    // Get the file from the database first
    let params = params.text_document_position_params;
    let file_path_str = params.text_document.uri.path();
    let url = url::Url::from_file_path(file_path_str).map_err(|()| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            format!("Invalid file path: {file_path_str}"),
        )
    })?;
    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found in index: {url} (original path: {file_path_str})"),
            )
        })?;

    // Use the database content (which includes any unsaved changes) for cursor position
    let file_text = file.text(&backend.db);
    let cursor: Cursor = to_offset_from_position(params.position, file_text.as_str());
    let top_mod = map_file_to_mod(&backend.db, file);

    // Check if we can get precise locations for local variables using spans
    let locations = if let Some(precise_locations) = get_precise_local_variable_locations(&backend.db, top_mod, cursor) {
        // For local variables, use the precise span-based locations
        precise_locations
    } else {
        // For everything else, use the scope-based approach
        let scopes = get_goto_target_scopes_for_cursor(&backend.db, top_mod, cursor).unwrap_or_default();
        scopes
            .iter()
            .map(|scope| to_lsp_location_from_scope(&backend.db, *scope))
            .collect::<Vec<_>>()
    };

    let result: Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ()> =
        Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
            locations
                .into_iter()
                .filter_map(std::result::Result::ok)
                .collect(),
        )));
    let response = match result {
        Ok(response) => response,
        Err(e) => {
            error!("Error handling goto definition: {:?}", e);
            None
        }
    };
    Ok(response)
}
// }
#[cfg(test)]
mod tests {
    use common::ingot::IngotKind;
    use dir_test::{dir_test, Fixture};
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use driver::DriverDataBase;

    // given a cursor position and a string, convert to cursor line and column
    fn line_col_from_cursor(cursor: Cursor, s: &str) -> (usize, usize) {
        let mut line = 0;
        let mut col = 0;
        for (i, c) in s.chars().enumerate() {
            if i == Into::<usize>::into(cursor) {
                return (line, col);
            }
            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        (line, col)
    }

    fn extract_multiple_cursor_positions_from_spans(
        db: &DriverDataBase,
        top_mod: TopLevelMod,
    ) -> Vec<parser::TextSize> {
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
        let mut collector = ComprehensiveCollector::default();
        collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        extract_multiple_cursor_positions_from_spans_with_positions(db, &collector.positions)
    }

    fn extract_multiple_cursor_positions_from_spans_with_positions(
        db: &DriverDataBase,
        positions: &[ResolvablePosition],
    ) -> Vec<parser::TextSize> {
        let mut cursors = Vec::new();
        
        // Count different position types
        let method_call_count = positions.iter()
            .filter(|pos| matches!(pos, ResolvablePosition::MethodCall(..)))
            .count();
        eprintln!("DEBUG: Cursor extraction found {} total positions, {} MethodCall positions", 
                  positions.len(), method_call_count);

        // Collect cursors from all resolvable positions
        for position in positions {
            match position {
                ResolvablePosition::Path(path, _, lazy_span) => {
                    for idx in 0..=path.segment_index(db) {
                        if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                            cursors.push(seg_span.range.start());
                        }
                    }
                }
                ResolvablePosition::FieldAccess(_, _, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                    }
                }
                ResolvablePosition::MethodCall(_, _, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        let cursor_byte = span.range.start().into();
                        eprintln!("DEBUG: Extracting MethodCall cursor at byte {} (range {:?})", cursor_byte, span.range);
                        cursors.push(span.range.start());
                    }
                }
                ResolvablePosition::LocalVariable(_, _, lazy_span)
                | ResolvablePosition::PatternField(_, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                    }
                }
            }
        }

        cursors.sort();
        cursors.dedup();
        cursors
    }

    fn make_goto_cursors_snapshot(
        db: &DriverDataBase,
        fixture: &Fixture<&str>,
        top_mod: TopLevelMod,
    ) -> String {
        // Collect positions once and use them for both extraction and resolution
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
        let mut collector = ComprehensiveCollector::default();
        collector.visit_top_mod(&mut visitor_ctxt, top_mod);
        
        let cursors = extract_multiple_cursor_positions_from_spans_with_positions(db, &collector.positions);
        let mut cursor_path_map: BTreeMap<Cursor, String> = BTreeMap::default();

        for cursor in &cursors {
            // Use the same logic as the actual goto definition handler
            // Try precise local variable resolution first, then fall back to scope-based resolution
            let paths = if let Some(precise_locations) = get_precise_local_variable_locations(db, top_mod, *cursor) {
                // For local variables, convert locations to paths
                precise_locations
                    .into_iter()
                    .filter_map(|result| result.ok())
                    .map(|location| {
                        // Convert LSP location back to a descriptive path
                        // This is not ideal but matches what we need for the test
                        format!("local_var_def_at_line_{}", location.range.start.line + 1)
                    })
                    .collect::<Vec<_>>()
            } else {
                // For everything else, use the scope-based approach
                let scopes = get_goto_target_scopes_for_cursor_with_positions(db, top_mod, *cursor, collector.positions.clone()).unwrap_or_default();
                scopes
                    .iter()
                    .flat_map(|x| x.pretty_path(db))
                    .collect::<Vec<_>>()
            };

            if !paths.is_empty() {
                cursor_path_map.insert(*cursor, paths.join("\n"));
            }
        }


        let cursor_lines = cursor_path_map
            .iter()
            .map(|(cursor, path)| {
                let (cursor_line, cursor_col) = line_col_from_cursor(*cursor, fixture.content());
                format!("cursor position ({cursor_line:?}, {cursor_col:?}), path: {path}")
            })
            .collect::<Vec<_>>();

        format!(
            "{}\n---\n{}",
            fixture
                .content()
                .lines()
                .enumerate()
                .map(|(i, line)| format!("{i:?}: {line}"))
                .collect::<Vec<_>>()
                .join("\n"),
            cursor_lines.join("\n")
        )
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/single_ingot",
        glob: "**/lib.fe",
    )]
    fn test_goto_multiple_files(fixture: Fixture<&str>) {
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir =
            std::path::Path::new(&cargo_manifest_dir).join("test_files/single_ingot");

        let mut db = DriverDataBase::default();

        // Load all files from the ingot directory
        load_ingot_from_directory(&mut db, &ingot_base_dir);

        // Get our specific test file
        let fe_source_path = fixture.path();
        let file_url = Url::from_file_path(fe_source_path).unwrap();

        // Get the containing ingot - should be Local now
        let ingot = db.workspace().containing_ingot(&db, file_url).unwrap();
        assert_eq!(ingot.kind(&db), IngotKind::Local);

        // Introduce a new scope to limit the lifetime of `top_mod`
        {
            // Get the file directly from the file index
            let file_url = Url::from_file_path(fe_source_path).unwrap();
            let file = db.workspace().get(&db, &file_url).unwrap();
            let top_mod = map_file_to_mod(&db, file);

            let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod);
            snap_test!(snapshot, fixture.path());
        }

        // Get the containing ingot for the file path
        let file_url = Url::from_file_path(fixture.path()).unwrap();
        let ingot = db.workspace().containing_ingot(&db, file_url);
        assert_eq!(ingot.unwrap().kind(&db), IngotKind::Local);
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto*.fe"
    )]
    fn test_goto_cursor_target(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default(); // Changed to mut
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod);
        snap_test!(snapshot, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "smallest_enclosing*.fe"
    )]
    fn test_find_path_surrounding_cursor(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default(); // Changed to mut

        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let cursors = extract_multiple_cursor_positions_from_spans(&db, top_mod);

        let mut cursor_paths: Vec<(Cursor, String)> = vec![];

        for cursor in &cursors {
            let mut visitor_ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
            let mut path_collector = ComprehensiveCollector::default();
            path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

            let _full_paths: Vec<_> = path_collector
                .positions
                .into_iter()
                .filter_map(|pos| {
                    if let ResolvablePosition::Path(path, scope, span) = pos {
                        Some((path, scope, span))
                    } else {
                        None
                    }
                })
                .collect();

            // Use the comprehensive goto functionality
            if let Some(scopes) = get_goto_target_scopes_for_cursor(&db, top_mod, *cursor) {
                let path_results: Vec<_> = scopes
                    .iter()
                    .flat_map(|scope| scope.pretty_path(&db))
                    .collect();
                if !path_results.is_empty() {
                    cursor_paths.push((*cursor, path_results.join("\n")));
                }
            }
        }

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_paths
                .iter()
                .map(|(cursor, path)| { format!("cursor position: {cursor:?}, path: {path}") })
                .collect::<Vec<_>>()
                .join("\n")
        );
        snap_test!(result, fixture.path());
    }
}
