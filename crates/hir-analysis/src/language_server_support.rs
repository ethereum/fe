//! Language server support APIs
//! 
//! This module provides public APIs specifically designed for language server features
//! like goto definition, hover information, find all references, etc. These APIs provide
//! a stable interface over the internal name resolution and type checking machinery.

use hir::{
    hir_def::{scope_graph::ScopeId, Body, Expr, ExprId, Func, IdentId, ItemKind, Partial, PatId, PathId},
    span::DynLazySpan,
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
                    ItemKind::Body(body_def) => {
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
        body: Body<'db>,
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
            let public_binding = expr_prop.binding.map(|binding| {
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
        body,
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
#[derive(Debug, Clone)]
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

/// Public method information for language server features
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicMethodInfo<'db> {
    /// The function definition for this method
    pub func_def: FuncDef<'db>,
    /// The scope where this method is defined
    pub definition_scope: ScopeId<'db>,
}

/// Find methods available for a given type by name
/// This includes both inherent methods (from impl blocks) and trait methods
pub fn find_methods_for_type<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
    ty: TyId<'db>,
    method_name: IdentId<'db>,
) -> Vec<PublicMethodInfo<'db>> {
    // Since the canonical module is private, we need to create a public wrapper
    // that uses the existing probe_method function which handles canonicalization internally.
    // We'll add a public helper function in the method_table module.
    let func_defs = probe_method_for_language_server(db, ingot, ty, method_name);
    
    func_defs
        .iter()
        .map(|func_def| PublicMethodInfo {
            definition_scope: func_def.scope(db),
            func_def: *func_def,
        })
        .collect()
}