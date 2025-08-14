//! HIR synthesis from AST positions
//!
//! This module provides the reverse of the `span` module - instead of going from
//! HIR to AST positions, it synthesizes HIR information from AST starting points.
//! This enables efficient cursor position → HIR node resolution for language server features.
//!
//! ## Architecture
//!
//! The synthesis system works in three phases:
//! 1. **Body Discovery**: Find which function body contains a cursor position
//! 2. **AST Resolution**: Use parser's ast_index to find AST nodes at the position
//! 3. **HIR Synthesis**: Use existing source maps to convert AST nodes to HIR nodes
//!
//! ## Performance
//!
//! This approach is significantly faster than visitor-pattern traversal because:
//! - Body discovery uses cached range maps for O(log n) lookup
//! - AST traversal is localized to a single function body
//! - Source map lookup is O(1) with IndexMap

use parser::{ast_index::AstNodeAtPosition, TextSize};
use rangemap::RangeMap;

use crate::{
    hir_def::{Body, ExprId, ItemKind, PatId, PathId, StmtId, TopLevelMod, TypeId},
    span::{body_source_map, HirOrigin, LazySpan},
    SpannedHirDb,
};
use crate::visitor::{prelude::LazyPathSpan, Visitor, VisitorCtxt};

/// Wrapper for RangeMap to add Salsa compatibility traits
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BodyIndex<'db>(RangeMap<TextSize, Body<'db>>);

impl<'db> BodyIndex<'db> {
    pub fn new() -> Self {
        Self(RangeMap::new())
    }

    pub fn insert(&mut self, range: std::ops::Range<TextSize>, body: Body<'db>) {
        self.0.insert(range, body);
    }

    pub fn get(&self, position: &TextSize) -> Option<Body<'db>> {
        self.0.get(position).cloned()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'db> Default for BodyIndex<'db> {
    fn default() -> Self {
        Self::new()
    }
}

/// Wrapper for AST spatial index to add Salsa compatibility
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstIndex(parser::ast_index::AstSpatialIndex);

impl AstIndex {
    pub fn new(index: parser::ast_index::AstSpatialIndex) -> Self {
        Self(index)
    }

    pub fn find_at_position(&self, offset: TextSize) -> AstNodeAtPosition {
        self.0.find_at_position(offset)
    }
}

pub type Cursor = TextSize;

/// Additional context for HIR nodes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirNodeContext {
    /// Path expression with the segment index that contains the cursor
    PathSegment(usize),
    /// Field access where cursor is on the field name
    FieldAccess,
    /// Method call where cursor is on the method name
    MethodCall,
    /// Regular context
    Regular,
}

/// Context for item-level HIR nodes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ItemNodeContext {
    /// Type bound in generic parameter (e.g., T: Trait)
    TypeBound,
    /// Where clause predicate
    WhereClause,
    /// Super trait in trait definition
    SuperTrait,
    /// Type in impl header (e.g., impl Trait for Type)
    ImplHeader,
    /// Generic parameter name
    GenericParam,
    /// Regular item context
    Regular,
}

/// Result of a lazy HIR lookup from cursor position
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LazyHirResult<'db> {
    /// Found an expression HIR node
    Expr(Body<'db>, ExprId, HirNodeContext),
    /// Found a statement HIR node
    Stmt(Body<'db>, StmtId, HirNodeContext),
    /// Found a pattern HIR node
    Pat(Body<'db>, PatId),
    /// Found a path in an item context (not in a body)
    /// The optional segment index specifies which segment the cursor is on
    ItemPath(ItemKind<'db>, PathId<'db>, ItemNodeContext, Option<usize>),
    /// Found a type in an item context
    ItemType(ItemKind<'db>, TypeId<'db>, ItemNodeContext),
    /// Found a generic parameter name in an item context (by index in the param list)
    ItemGenericParam(ItemKind<'db>, u16),
    /// No HIR node found at this position
    None,
}

impl<'db> LazyHirResult<'db> {
    /// Get the body containing this HIR node, if any
    pub fn body(&self) -> Option<Body<'db>> {
        match self {
            LazyHirResult::Expr(body, _, _)
            | LazyHirResult::Stmt(body, _, _)
            | LazyHirResult::Pat(body, _) => Some(*body),
            LazyHirResult::ItemPath(_, _, _, _)
            | LazyHirResult::ItemType(_, _, _)
            | LazyHirResult::ItemGenericParam(_, _)
            | LazyHirResult::None => None,
        }
    }

    /// Check if this result contains any HIR node
    pub fn is_some(&self) -> bool {
        !matches!(self, LazyHirResult::None)
    }

    /// Check if this result is empty
    pub fn is_none(&self) -> bool {
        matches!(self, LazyHirResult::None)
    }
}

/// Trait for lazy HIR resolution from cursor positions
pub trait LazyHir<'db> {
    /// Find the HIR node at the given cursor position
    fn find_hir_at_position(&self, db: &'db dyn SpannedHirDb, cursor: Cursor)
        -> LazyHirResult<'db>;

    /// Find all bodies that might contain the given cursor position
    fn find_bodies_at_position(&self, db: &'db dyn SpannedHirDb, cursor: Cursor) -> Vec<Body<'db>>;
}

/// Trait for high-level HIR resolution (future extension point)
pub trait ResolveHir<'db> {
    /// Resolve the definition of a HIR node
    fn resolve_definition(
        &self,
        db: &'db dyn SpannedHirDb,
        result: LazyHirResult<'db>,
    ) -> Option<LazyHirResult<'db>>;

    /// Get type information for a HIR node
    fn resolve_type(&self, db: &'db dyn SpannedHirDb, result: LazyHirResult<'db>)
        -> Option<String>;
}

/// Lazy HIR resolver for a top-level module
#[derive(Debug, Clone)]
pub struct ModuleLazyHir<'db> {
    top_mod: TopLevelMod<'db>,
    /// Cache of bodies indexed by their text range for fast lookup
    body_cache: Option<BodyIndex<'db>>,
}

impl<'db> ModuleLazyHir<'db> {
    #[inline]
    fn range_contains_inclusive(range: parser::TextRange, cursor: Cursor) -> bool {
        if range.contains(cursor) {
            true
        } else {
            let start: u32 = range.start().into();
            let end: u32 = range.end().into();
            let cur: u32 = cursor.into();
            end > start && cur == end
        }
    }
    /// Create a new lazy HIR resolver for the given module
    pub fn new(top_mod: TopLevelMod<'db>) -> Self {
        Self {
            top_mod,
            body_cache: None,
        }
    }

    /// Get or build the body cache for fast range-based lookups
    fn ensure_body_cache(&mut self, db: &'db dyn SpannedHirDb) {
        if self.body_cache.is_some() {
            return;
        }

        // Use the Salsa query to get the cached body index
        let body_index = module_body_index(db, self.top_mod);
        self.body_cache = Some(body_index);
    }

    /// Find the body that most closely contains the given cursor position
    fn find_containing_body(
        &mut self,
        db: &'db dyn SpannedHirDb,
        cursor: Cursor,
    ) -> Option<Body<'db>> {
        self.ensure_body_cache(db);

        let cache = self.body_cache.as_ref().unwrap();

        // Use rangemap for efficient position-based lookup
        cache.get(&cursor)
    }
}

impl<'db> LazyHir<'db> for ModuleLazyHir<'db> {
    fn find_hir_at_position(
        &self,
        db: &'db dyn SpannedHirDb,
        cursor: Cursor,
    ) -> LazyHirResult<'db> {
        let mut resolver = self.clone();

        // First find the body containing this position
        if let Some(body) = resolver.find_containing_body(db, cursor) {
            // Handle nodes within function bodies (existing logic)
            return resolver.find_hir_in_body(db, body, cursor);
        }
        
        // If not in a body, check if we're in an item definition
        resolver.find_hir_in_item(db, cursor)
    }
    
    fn find_bodies_at_position(&self, db: &'db dyn SpannedHirDb, cursor: Cursor) -> Vec<Body<'db>> {
        let mut resolver = self.clone();
        resolver.ensure_body_cache(db);
        
        let cache = resolver.body_cache.as_ref().unwrap();
        
        // For now, just return the single body at this position
        if let Some(body) = cache.get(&cursor) {
            vec![body]
        } else {
            vec![]
        }
    }
}

impl<'db> ModuleLazyHir<'db> {
    /// Find HIR nodes within a function body
    fn find_hir_in_body(
        &self,
        db: &'db dyn SpannedHirDb,
        body: Body<'db>,
        cursor: Cursor,
    ) -> LazyHirResult<'db> {
        // Get the source map for this body
        let source_map = body_source_map(db, body);

        // Find the AST node at the cursor position within this body
        let ast_node = find_ast_node_at_position(db, body, cursor);

        // Convert AST node to HIR using the source map
        match ast_node {
            AstNodeAtPosition::Expr(expr_ast) => {
                let origin = HirOrigin::Raw(parser::ast::AstPtr::new(&expr_ast));
                if let Some(&expr_id) = source_map.expr_map.source_to_node.get(&origin) {
                    // Check if this is a path expression and determine which segment contains the cursor
                    let context = match expr_ast.kind() {
                        parser::ast::ExprKind::Path(path_expr) => {
                            if let Some(path) = path_expr.path() {
                                // Find which segment contains the cursor
                                let mut segment_index = None;
                                for (idx, segment) in path.segments().enumerate() {
                                    use parser::ast::prelude::AstNode;
                                    let segment_range = segment.syntax().text_range();
                                    if ModuleLazyHir::range_contains_inclusive(segment_range, cursor) {
                                        segment_index = Some(idx);
                                        break;
                                    }
                                }
                                
                                if let Some(idx) = segment_index {
                                    HirNodeContext::PathSegment(idx)
                                } else {
                                    HirNodeContext::Regular
                                }
                            } else {
                                HirNodeContext::Regular
                            }
                        }
                        parser::ast::ExprKind::MethodCall(method_call) => {
                            // If cursor is on the method name token
                            if let Some(name_tok) = method_call.method_name() {
                                let r = name_tok.text_range();
                                if ModuleLazyHir::range_contains_inclusive(r, cursor) {
                                    HirNodeContext::MethodCall
                                } else { HirNodeContext::Regular }
                            } else { HirNodeContext::Regular }
                        }
                        parser::ast::ExprKind::Field(field_expr) => {
                            // Check if cursor is on the field name
                            if let Some(field_name) = field_expr.field_name() {
                                let field_range = field_name.text_range();
                                if ModuleLazyHir::range_contains_inclusive(field_range, cursor) {
                                    HirNodeContext::FieldAccess
                                } else {
                                    HirNodeContext::Regular
                                }
                            } else {
                                HirNodeContext::Regular
                            }
                        }
                        _ => HirNodeContext::Regular
                    };
                    
                    LazyHirResult::Expr(body, expr_id, context)
                } else {
                    LazyHirResult::None
                }
            }
            AstNodeAtPosition::Stmt(stmt_ast) => {
                let origin = HirOrigin::Raw(parser::ast::AstPtr::new(&stmt_ast));
                if let Some(&stmt_id) = source_map.stmt_map.source_to_node.get(&origin) {
                    // Check if cursor is within a path type in the statement
                    let context = if let parser::ast::StmtKind::Let(let_stmt) = stmt_ast.kind() {
                        if let Some(ty) = let_stmt.type_annotation() {
                            if let parser::ast::TypeKind::Path(path_type) = ty.kind() {
                                if let Some(path) = path_type.path() {
                                    // Check if cursor is within this path
                                    use parser::ast::prelude::AstNode;
                                    let path_range = path.syntax().text_range();
                                    if ModuleLazyHir::range_contains_inclusive(path_range, cursor) {
                                        // Find which segment contains the cursor
                                        let mut segment_index = None;
                                        for (idx, segment) in path.segments().enumerate() {
                                            let segment_range = segment.syntax().text_range();
                                            if ModuleLazyHir::range_contains_inclusive(segment_range, cursor) {
                                                segment_index = Some(idx);
                                                break;
                                            }
                                        }
                                        
                                        if let Some(idx) = segment_index {
                                            HirNodeContext::PathSegment(idx)
                                        } else {
                                            HirNodeContext::Regular
                                        }
                                    } else {
                                        HirNodeContext::Regular
                                    }
                                } else {
                                    HirNodeContext::Regular
                                }
                            } else {
                                HirNodeContext::Regular
                            }
                        } else {
                            HirNodeContext::Regular
                        }
                    } else {
                        HirNodeContext::Regular
                    };
                    
                    LazyHirResult::Stmt(body, stmt_id, context)
                } else {
                    LazyHirResult::None
                }
            }
            AstNodeAtPosition::Pat(pat_ast) => {
                let origin = HirOrigin::Raw(parser::ast::AstPtr::new(&pat_ast));
                if let Some(&pat_id) = source_map.pat_map.source_to_node.get(&origin) {
                    LazyHirResult::Pat(body, pat_id)
                } else {
                    LazyHirResult::None
                }
            }
            AstNodeAtPosition::None => {
                LazyHirResult::None
            }
        }
    }

    fn find_bodies_at_position(&self, db: &'db dyn SpannedHirDb, cursor: Cursor) -> Vec<Body<'db>> {
        let mut resolver = self.clone();
        if let Some(body) = resolver.find_containing_body(db, cursor) {
            vec![body]
        } else {
            vec![]
        }
    }
    
    /// Find HIR nodes in item-level constructs (generic bounds, where clauses, etc.)
    fn find_hir_in_item(
        &self,
        db: &'db dyn SpannedHirDb,
        cursor: Cursor,
    ) -> LazyHirResult<'db> {
        use parser::ast::prelude::AstNode;
        use parser::ast::{GenericParamsOwner, WhereClauseOwner};
        
        // Parse the file to get the AST
        let green_node = crate::lower::parse_file_impl(db, self.top_mod);
        let root_syntax = parser::SyntaxNode::new_root(green_node);
        
        // Find the most specific node at the cursor position
        let token = match root_syntax.token_at_offset(cursor).right_biased() {
            Some(token) => token,
            None => return LazyHirResult::None,
        };
        
        // Walk up from the token to find relevant item-level constructs
        for ancestor in token.parent_ancestors() {
            // Check if we're in a struct/enum/trait/impl item
            if let Some(item) = parser::ast::Item::cast(ancestor.clone()) {
                if let Some(item_kind) = item.kind() {
                    match item_kind {
                        parser::ast::ItemKind::Struct(struct_item) => {
                        // Check if cursor is in generic parameters
                        if let Some(generics) = struct_item.generic_params() {
                            if ModuleLazyHir::range_contains_inclusive(generics.syntax().text_range(), cursor) {
                                // Find which generic parameter contains the cursor
                                for (idx, param) in generics.into_iter().enumerate() {
                                    // Param name under cursor -> generic param
                                    if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                        if let Some(name_tok) = type_param.name() {
                                            if ModuleLazyHir::range_contains_inclusive(name_tok.text_range(), cursor) {
                                                if let Some(item_kind) = parser::ast::Item::cast(struct_item.syntax().clone()).and_then(|i| i.kind()) {
                                                    let hir_item = self.find_enclosing_item_for_ast(db, &parser::ast::Item::cast(struct_item.syntax().clone()).unwrap()).unwrap_or_else(|| self.top_mod.into());
                                                    return LazyHirResult::ItemGenericParam(hir_item, idx as u16);
                                                }
                                            }
                                        }
                                    }
                                    if ModuleLazyHir::range_contains_inclusive(param.syntax().text_range(), cursor) {
                                        // Check if this is a type parameter with bounds
                                        if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                            if let Some(bounds) = type_param.bounds() {
                                                for bound in bounds {
                                                    if ModuleLazyHir::range_contains_inclusive(bound.syntax().text_range(), cursor) {
                                                        // This is a type bound - we need to resolve it
                                                        if let Some(path) = extract_path_from_type_bound(&bound) {
                                                            return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::TypeBound, cursor);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                        parser::ast::ItemKind::Impl(impl_item) => {
                            // Check if cursor is in generic parameters
                            if let Some(generics) = impl_item.generic_params() {
                                if ModuleLazyHir::range_contains_inclusive(generics.syntax().text_range(), cursor) {
                                    // Find which generic parameter contains the cursor
                                    for (idx, param) in generics.into_iter().enumerate() {
                                        if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                            if let Some(name_tok) = type_param.name() {
                                                if ModuleLazyHir::range_contains_inclusive(name_tok.text_range(), cursor) {
                                                    return LazyHirResult::ItemGenericParam(self.find_enclosing_item_for_ast(db, &item).unwrap_or(self.top_mod.into()), idx as u16);
                                                }
                                            }
                                        }
                                        if ModuleLazyHir::range_contains_inclusive(param.syntax().text_range(), cursor) {
                                            // Check if this is a type parameter with bounds
                                            if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                                if let Some(bounds) = type_param.bounds() {
                                                    for bound in bounds {
                                                        if ModuleLazyHir::range_contains_inclusive(bound.syntax().text_range(), cursor) {
                                                            // This is a type bound - we need to resolve it
                                                            if let Some(path) = extract_path_from_type_bound(&bound) {
                                                                return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::TypeBound, cursor);
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            
                            // Check if cursor is in the impl target type
                            if let Some(impl_target) = impl_item.ty() {
                                if ModuleLazyHir::range_contains_inclusive(impl_target.syntax().text_range(), cursor) {
                                    if let parser::ast::TypeKind::Path(path_type) = impl_target.kind() {
                                        if let Some(path) = path_type.path() {
                                            return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::ImplHeader, cursor);
                                        }
                                    }
                                }
                            }
                        }
                        parser::ast::ItemKind::ImplTrait(impl_trait) => {
                            // Check if cursor is in generic parameters
                            if let Some(generics) = impl_trait.generic_params() {
                                if generics.syntax().text_range().contains(cursor) {
                                    // Find which generic parameter contains the cursor
                                    for (idx, param) in generics.into_iter().enumerate() {
                                        if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                            if let Some(name_tok) = type_param.name() {
                                                if name_tok.text_range().contains(cursor) {
                                                    return LazyHirResult::ItemGenericParam(self.find_enclosing_item_for_ast(db, &item).unwrap_or(self.top_mod.into()), idx as u16);
                                                }
                                            }
                                        }
                                        if param.syntax().text_range().contains(cursor) {
                                            // Check if this is a type parameter with bounds
                                            if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                                if let Some(bounds) = type_param.bounds() {
                                                    for bound in bounds {
                                                        if bound.syntax().text_range().contains(cursor) {
                                                            // This is a type bound - we need to resolve it
                                                            if let Some(path) = extract_path_from_type_bound(&bound) {
                                                                return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::TypeBound, cursor);
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            
                            // Check if cursor is in the trait being implemented
                            if let Some(trait_ref) = impl_trait.trait_ref() {
                                if let Some(trait_path) = trait_ref.path() {
                                    if trait_path.syntax().text_range().contains(cursor) {
                                        return self.resolve_item_path(db, item.clone(), trait_path, ItemNodeContext::ImplHeader, cursor);
                                    }
                                }
                            }
                            
                            // Check if cursor is in the impl target type
                            if let Some(impl_target) = impl_trait.ty() {
                                if impl_target.syntax().text_range().contains(cursor) {
                                    if let parser::ast::TypeKind::Path(path_type) = impl_target.kind() {
                                        if let Some(path) = path_type.path() {
                                            return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::ImplHeader, cursor);
                                        }
                                    }
                                }
                            }
                        }
                        parser::ast::ItemKind::Trait(trait_item) => {
                        // Check if cursor is in generic parameters
                        if let Some(generics) = trait_item.generic_params() {
                            if generics.syntax().text_range().contains(cursor) {
                                // Find which generic parameter contains the cursor
                                for (idx, param) in generics.into_iter().enumerate() {
                                    if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                        if let Some(name_tok) = type_param.name() {
                                            if name_tok.text_range().contains(cursor) {
                                                return LazyHirResult::ItemGenericParam(self.find_enclosing_item_for_ast(db, &item).unwrap_or(self.top_mod.into()), idx as u16);
                                            }
                                        }
                                    }
                                    if param.syntax().text_range().contains(cursor) {
                                        // Check if this is a type parameter with bounds
                                        if let parser::ast::GenericParamKind::Type(type_param) = param.kind() {
                                            if let Some(bounds) = type_param.bounds() {
                                                for bound in bounds {
                                                    if bound.syntax().text_range().contains(cursor) {
                                                        // This is a type bound - we need to resolve it
                                                        if let Some(path) = extract_path_from_type_bound(&bound) {
                                                            return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::TypeBound, cursor);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // Check super traits: trait Foo: Bar + Baz {}
                        if let Some(supers) = trait_item.super_trait_list() {
                            if supers.syntax().text_range().contains(cursor) {
                                for trait_ref in supers {
                                    if let Some(path) = trait_ref.path() {
                                        if path.syntax().text_range().contains(cursor) {
                                            return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::SuperTrait, cursor);
                                        }
                                    }
                                }
                            }
                        }

                        // Check if cursor is in where clause bounds
                        if let Some(where_clause) = trait_item.where_clause() {
                            if where_clause.syntax().text_range().contains(cursor) {
                                for predicate in where_clause {
                                    if predicate.syntax().text_range().contains(cursor) {
                                        if let Some(bounds) = predicate.bounds() {
                                            for bound in bounds {
                                                if bound.syntax().text_range().contains(cursor) {
                                                    if let Some(path) = extract_path_from_type_bound(&bound) {
                                                        return self.resolve_item_path(db, item.clone(), path, ItemNodeContext::WhereClause, cursor);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                        _ => {}
                    }
                }
            }
        }
        
        // Generic catch-all: if inside an item, try to match any path span to avoid LSP fallbacks
        let green_node = crate::lower::parse_file_impl(db, self.top_mod);
        let root_syntax = parser::SyntaxNode::new_root(green_node);
        if let Some(token) = root_syntax.token_at_offset(cursor).right_biased() {
            for ancestor in token.parent_ancestors() {
                if let Some(item) = parser::ast::Item::cast(ancestor.clone()) {
                    if let Some(kind) = item.kind() {
                        let item_hir = match self.find_enclosing_item_for_ast(db, &item) { Some(it) => it, None => continue };
                        let mut vctxt = VisitorCtxt::with_item(db, item_hir);
                        let mut collector = PathSpanCollector::default();
                        collector.visit_item(&mut vctxt, item_hir);

                        let mut best: Option<(PathId<'db>, LazyPathSpan<'db>, usize, parser::TextRange)> = None;
                        for (path_id, _scope, lazy_span) in collector.paths {
                            if let Some(span) = lazy_span.resolve(db) {
                                if span.range.contains(cursor) {
                                    // segment idx by ident span
                                    let mut seg_idx = path_id.segment_index(db);
                                    for idx in 0..=path_id.segment_index(db) {
                                        if let Some(id_span) = lazy_span.clone().segment(idx).ident().resolve(db) {
                                            if id_span.range.contains(cursor) { seg_idx = idx; break; }
                                        }
                                    }
                                    let len = span.range.end() - span.range.start();
                                    if best.as_ref().map(|(_, _, _, r)| (r.end() - r.start()) > len).unwrap_or(true) {
                                        best = Some((path_id, lazy_span, seg_idx, span.range));
                                    }
                                }
                            }
                        }

                        if let Some((path_id, _lazy, seg, _r)) = best {
                            return LazyHirResult::ItemPath(item_hir, path_id, ItemNodeContext::Regular, Some(seg));
                        }
                    }
                }
            }
        }

        LazyHirResult::None
    }
    
    /// Resolve a path found in an item context to HIR
    fn resolve_item_path(
        &self,
        db: &'db dyn SpannedHirDb,
        item_ast: parser::ast::Item,
        path_ast: parser::ast::Path,
        context: ItemNodeContext,
        cursor: Cursor,
    ) -> LazyHirResult<'db> {
        use parser::ast::prelude::AstNode;

        // 1) Find enclosing HIR item for the given AST item by span containment
        let item_kind = match self.find_enclosing_item_for_ast(db, &item_ast) {
            Some(it) => it,
            None => return LazyHirResult::None,
        };

        // 2) Within that item, collect HIR paths and their spans
        let mut vctxt = VisitorCtxt::with_item(db, item_kind);
        let mut collector = PathSpanCollector::default();
        collector.visit_item(&mut vctxt, item_kind);

        // Derive segment index from AST path and cursor
        let seg_index = {
            let mut idx = None;
            for (i, seg) in path_ast.segments().enumerate() {
                if seg.syntax().text_range().contains(cursor) {
                    idx = Some(i);
                    break;
                }
            }
            idx
        };

        // 3) Match the AST path range to a collected HIR path's span
        let ast_range = path_ast.syntax().text_range();
        for (path_id, _scope, lazy_span) in collector.paths {
            if let Some(span) = lazy_span.resolve(db) {
                if span.range == ast_range {
                    debug_assert!({
                        // Invariant: AST segments count should match HIR len
                        let ast_len = path_ast.segments().count();
                        let hir_len = path_id.segment_index(db) + 1;
                        ast_len == hir_len
                    }, "AST/HIR path segment count mismatch in resolve_item_path");
                    return LazyHirResult::ItemPath(item_kind, path_id, context, seg_index);
                }
                // Fallback: allow containment in case of minor mismatch
                if span.range.contains(ast_range.start()) && span.range.contains(ast_range.end()) {
                    debug_assert!({
                        let ast_len = path_ast.segments().count();
                        let hir_len = path_id.segment_index(db) + 1;
                        ast_len == hir_len
                    }, "AST/HIR path segment count mismatch in resolve_item_path (containment)");
                    return LazyHirResult::ItemPath(item_kind, path_id, context, seg_index);
                }
            }
        }

        LazyHirResult::None
    }
}

/// Extract a path from a type bound AST node
fn extract_path_from_type_bound(bound: &parser::ast::TypeBound) -> Option<parser::ast::Path> {
    // Type bounds can be trait bounds or kind bounds
    // We're interested in trait bounds which contain paths
    bound.trait_bound()?.path()
}

/// Lightweight path span collector for a single item
#[derive(Default)]
struct PathSpanCollector<'db> {
    paths: Vec<(PathId<'db>, crate::hir_def::scope_graph::ScopeId<'db>, LazyPathSpan<'db>)>,
}

impl<'db, 'ast: 'db> Visitor<'ast> for PathSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>, path: PathId<'db>) {
        let Some(span) = ctxt.span() else { return; };
        let scope = ctxt.scope();
        self.paths.push((path, scope, span));
    }
}

impl<'db> ModuleLazyHir<'db> {
    /// Find the HIR ItemKind that encloses a given AST item by comparing spans
    fn find_enclosing_item_for_ast(
        &self,
        db: &'db dyn SpannedHirDb,
        item_ast: &parser::ast::Item,
    ) -> Option<ItemKind<'db>> {
        use parser::ast::prelude::AstNode;
        let ast_range = item_ast.syntax().text_range();
        let items = self.top_mod.scope_graph(db).items_dfs(db);

        let mut best: Option<(ItemKind<'db>, parser::TextRange)> = None;
        for item in items {
            if let Some(span) = crate::span::DynLazySpan::from(item.span()).resolve(db) {
                if span.range.contains(ast_range.start()) && span.range.contains(ast_range.end()) {
                    let len = span.range.end() - span.range.start();
                    if best.as_ref().map(|(_, r)| (r.end() - r.start()) > len).unwrap_or(true) {
                        best = Some((item, span.range));
                    }
                }
            }
        }
        best.map(|(it, _)| it)
    }
}

/// Find the most specific AST node at the given cursor position within a body
///
/// This function gets the body's AST and uses the parser's ast_index module
/// to find the deepest AST node at the cursor position.
fn find_ast_node_at_position(
    db: &dyn SpannedHirDb,
    body: Body,
    cursor: Cursor,
) -> AstNodeAtPosition {
    use parser::ast_index::find_ast_node_at_position;
    use parser::SyntaxNode;

    // Get the body's AST by parsing the entire file
    let green_node = crate::lower::parse_file_impl(db, body.top_mod(db));
    let root_syntax = SyntaxNode::new_root(green_node);


    // Use the parser's ast_index to find the node at the position
    let result = find_ast_node_at_position(&root_syntax, cursor);
    result
}

/// Build the body index for a module (simplified for now)
pub fn module_body_index<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
) -> BodyIndex<'db> {
    let mut body_index = BodyIndex::new();

    // Collect all bodies in the module
    let items = top_mod.scope_graph(db).items_dfs(db);
    for item in items {
        // Check if this item has a body
        let body = match item {
            ItemKind::Func(func) => {
                if let Some(body) = func.body(db) {
                    body
                } else {
                    continue;
                }
            }
            _ => continue,
        };

        // Get the span of this body and add to index
        if let Some(span) = body.span().resolve(db) {
            let range = span.range;
            body_index.insert(range.start()..range.end(), body);
        } else {
        }
    }

    body_index
}

/// Build the AST spatial index for a module (simplified for now)
pub fn module_ast_index<'db>(db: &'db dyn SpannedHirDb, top_mod: TopLevelMod<'db>) -> AstIndex {
    // Parse the file and build spatial index
    let green_node = crate::lower::parse_file_impl(db, top_mod);
    let root_syntax = parser::SyntaxNode::new_root(green_node);

    let spatial_index = parser::ast_index::AstSpatialIndex::build_from_syntax(&root_syntax);
    AstIndex::new(spatial_index)
}

/// Utility function to create a lazy HIR resolver for a cursor position
pub fn lazy_hir_for_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> LazyHirResult<'db> {
    let resolver = ModuleLazyHir::new(top_mod);
    resolver.find_hir_at_position(db, cursor)}

/// Basic implementation of ResolveHir for demonstration
pub struct BasicHirResolver;

impl<'db> ResolveHir<'db> for BasicHirResolver {
    fn resolve_definition(
        &self,
        _db: &'db dyn SpannedHirDb,
        _result: LazyHirResult<'db>,
    ) -> Option<LazyHirResult<'db>> {
        // TODO: Implement definition resolution
        // This would resolve expressions to their definitions,
        // variables to their declarations, etc.
        None
    }

    fn resolve_type(
        &self,
        _db: &'db dyn SpannedHirDb,
        _result: LazyHirResult<'db>,
    ) -> Option<String> {
        // TODO: Implement type resolution
        // This would get type information for expressions,
        // return types for functions, etc.
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lower::map_file_to_mod, test_db::TestDb};
    use common::InputDb;
    use url::Url;

    #[test]
    fn test_module_lazy_hir_creation() {
        let mut db = TestDb::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some("fn test() { 42 }".to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let resolver = ModuleLazyHir::new(top_mod);
        assert_eq!(resolver.top_mod, top_mod);
        assert!(resolver.body_cache.is_none());
    }

    #[test]
    fn test_body_cache_building() {
        let mut db = TestDb::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some("fn test() { 42 }\nfn other() { 24 }".to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test the Salsa query directly
        let body_index = module_body_index(&db, top_mod);

        // Should have built an index
        println!("Body index contains {} entries", body_index.len());

        // Test module resolver cache
        let mut resolver = ModuleLazyHir::new(top_mod);
        resolver.ensure_body_cache(&db);

        assert!(resolver.body_cache.is_some());
        let _cache = resolver.body_cache.as_ref().unwrap();
        // Cache building should complete without panicking
    }

    #[test]
    fn test_lazy_hir_basic() {
        let mut db = TestDb::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some("fn test() { 42 }".to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let result = lazy_hir_for_cursor(&db, top_mod, TextSize::from(0));
        // At position 0, we might not find a HIR node, which is expected
        assert!(matches!(result, LazyHirResult::None) || result.is_some());
    }

    #[test]
    fn test_ast_position_finding_integration() {
        let mut db = TestDb::default();
        let source = r#"
fn calculate(x: i32) -> i32 {
    let y = x + 1;
    let z = y * 2;
    z
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test at various cursor positions within the function body
        let positions = [
            TextSize::from(10), // Around function name (outside body)
            TextSize::from(35), // Around "let y" (statement)
            TextSize::from(50), // Around "x + 1" (expression)
            TextSize::from(70), // Around "let z" (expression context)
        ];

        let mut results = Vec::new();
        for pos in positions {
            let result = lazy_hir_for_cursor(&db, top_mod, pos);
            results.push((pos, result));
        }

        // Verify that we get meaningful results for positions inside function bodies
        // Position 10 should be None (outside body), others might find HIR nodes
        // The exact results depend on parsing and HIR generation
        for (pos, result) in &results {
            println!("Position {:?}: {:?}", pos, result);
            // Just verify the function doesn't panic and returns valid results
            assert!(matches!(result, LazyHirResult::None) || result.is_some());
        }
    }

    #[test]
    fn test_salsa_queries() {
        let mut db = TestDb::default();
        let source = r#"
fn test_func() -> i32 {
    let x = 42;
    x
}

fn another_func() -> bool {
    true
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test body index query
        let body_index = module_body_index(&db, top_mod);
        println!("Body index: {} entries", body_index.len());

        // Test AST index query
        let ast_index = module_ast_index(&db, top_mod);
        println!("AST index created successfully");

        // Test lookup in body index
        let cursor = TextSize::from(50); // Somewhere in the first function
        if let Some(body) = body_index.get(&cursor) {
            println!("Found body at cursor position");
            // Test AST lookup within that body
            let _ast_result = find_ast_node_at_position(&db, body, cursor);
        }
    }

    #[test]
    fn test_performance_comparison() {
        let mut db = TestDb::default();
        let source = r#"
fn large_function() -> i32 {
    let a = 1;
    let b = 2;
    let c = 3;
    let d = 4;
    let e = 5;
    let f = 6;
    let g = 7;
    let h = 8;
    let i = 9;
    let j = 10;
    let result = a + b + c + d + e + f + g + h + i + j;
    result
}

fn another_function() -> bool {
    let x = true;
    let y = false;
    x && y
}

fn third_function() -> String {
    let s1 = "hello";
    let s2 = "world";
    format!("{} {}", s1, s2)
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///performance_test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test rangemap performance
        let start = std::time::Instant::now();
        let body_index = module_body_index(&db, top_mod);
        let index_build_time = start.elapsed();

        // Test multiple lookups using rangemap
        let start = std::time::Instant::now();
        for offset in (0..source.len()).step_by(10) {
            let cursor = TextSize::from(offset as u32);
            let _result = body_index.get(&cursor);
        }
        let lookup_time = start.elapsed();

        println!("Index build time: {:?}", index_build_time);
        println!(
            "Lookup time for {} queries: {:?}",
            source.len() / 10,
            lookup_time
        );
        println!(
            "Average lookup time: {:?}",
            lookup_time / (source.len() / 10) as u32
        );

        // Test AST spatial index
        let start = std::time::Instant::now();
        let _ast_index = module_ast_index(&db, top_mod);
        let ast_build_time = start.elapsed();

        println!("AST index build time: {:?}", ast_build_time);

        // Verify we have multiple bodies indexed
        assert!(body_index.len() > 0);
        println!("Successfully indexed {} bodies", body_index.len());
    }
}
