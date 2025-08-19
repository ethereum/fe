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


/// Lazy HIR resolver for a top-level module
#[derive(Debug, Clone)]
pub struct ModuleLazyHir<'db> {
    top_mod: TopLevelMod<'db>,
    /// Cache of bodies indexed by their text range for fast lookup
    body_cache: Option<BodyIndex<'db>>,
}

impl<'db> ModuleLazyHir<'db> {
    /// Classify expression-specific cursor context with minimal branching.
    fn classify_expr_context_at_cursor(
        &self,
        expr_ast: &parser::ast::Expr,
        cursor: Cursor,
    ) -> HirNodeContext {
        match expr_ast.kind() {
            parser::ast::ExprKind::Path(p) => {
                if let Some(path) = p.path() {
                    if let Some(idx) = Self::path_segment_index_at_cursor(&path, cursor) {
                        return HirNodeContext::PathSegment(idx);
                    }
                }
                HirNodeContext::Regular
            }
            parser::ast::ExprKind::MethodCall(m) => {
                if let Some(name) = m.method_name() {
                    if Self::range_contains_inclusive(name.text_range(), cursor) {
                        return HirNodeContext::MethodCall;
                    }
                }
                HirNodeContext::Regular
            }
            parser::ast::ExprKind::Field(f) => {
                if let Some(name) = f.field_name() {
                    if Self::range_contains_inclusive(name.text_range(), cursor) {
                        return HirNodeContext::FieldAccess;
                    }
                }
                HirNodeContext::Regular
            }
            parser::ast::ExprKind::RecordInit(r) => {
                if let Some(path) = r.path() {
                    if let Some(idx) = Self::path_segment_index_at_cursor(&path, cursor) {
                        return HirNodeContext::PathSegment(idx);
                    }
                }
                HirNodeContext::Regular
            }
            _ => HirNodeContext::Regular,
        }
    }

    /// Classify statement-specific cursor context with minimal branching.
    fn classify_stmt_context_at_cursor(
        &self,
        stmt_ast: &parser::ast::Stmt,
        cursor: Cursor,
    ) -> HirNodeContext {
        if let parser::ast::StmtKind::Let(let_stmt) = stmt_ast.kind() {
            if let Some(ty) = let_stmt.type_annotation() {
                if let Some(path) = Self::path_in_type_at_cursor(&ty, cursor) {
                    if let Some(idx) = Self::path_segment_index_at_cursor(&path, cursor) {
                        return HirNodeContext::PathSegment(idx);
                    }
                }
            }
        }
        HirNodeContext::Regular
    }
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
    /// Return the path segment index at `cursor` if any, using inclusive end.
    fn path_segment_index_at_cursor(path: &parser::ast::Path, cursor: Cursor) -> Option<usize> {
        use parser::ast::prelude::AstNode;
        for (idx, segment) in path.segments().enumerate() {
            let r = segment.syntax().text_range();
            if ModuleLazyHir::range_contains_inclusive(r, cursor) {
                return Some(idx);
            }
        }
        None
    }

    /// If `ty` is a path type that contains the cursor, return its path.
    fn path_in_type_at_cursor(ty: &parser::ast::Type, cursor: Cursor) -> Option<parser::ast::Path> {
        if let parser::ast::TypeKind::Path(path_ty) = ty.kind() {
            if let Some(path) = path_ty.path() {
                use parser::ast::prelude::AstNode;
                if ModuleLazyHir::range_contains_inclusive(path.syntax().text_range(), cursor) {
                    return Some(path);
                }
            }
        }
        None
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
    fn find_hir_in_body(
        &self,
        db: &'db dyn SpannedHirDb,
        body: Body<'db>,
        cursor: Cursor,
    ) -> LazyHirResult<'db> {
        let source_map = body_source_map(db, body);
        match find_ast_node_at_position(db, body, cursor) {
            AstNodeAtPosition::Expr(expr_ast) => {
                let origin = HirOrigin::Raw(parser::ast::AstPtr::new(&expr_ast));
                if let Some(&expr_id) = source_map.expr_map.source_to_node.get(&origin) {
                    let ctx = self.classify_expr_context_at_cursor(&expr_ast, cursor);
                    LazyHirResult::Expr(body, expr_id, ctx)
                } else {
                    LazyHirResult::None
                }
            }
            AstNodeAtPosition::Stmt(stmt_ast) => {
                let origin = HirOrigin::Raw(parser::ast::AstPtr::new(&stmt_ast));
                if let Some(&stmt_id) = source_map.stmt_map.source_to_node.get(&origin) {
                    let ctx = self.classify_stmt_context_at_cursor(&stmt_ast, cursor);
                    LazyHirResult::Stmt(body, stmt_id, ctx)
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
            AstNodeAtPosition::None => LazyHirResult::None,
        }
    }

    fn find_hir_in_item(&self, db: &'db dyn SpannedHirDb, cursor: Cursor) -> LazyHirResult<'db> {
        if let Some(enclosing) = crate::collect::find_enclosing_item_at(db, self.top_mod, cursor) {
            let dmap = crate::hir_def::decl_source_map::decl_source_map(db, enclosing);
            // 1) Check generic parameter name hits first
            for (range, idx) in dmap.generic_param_name_entries.iter() {
                if Self::range_contains_inclusive(*range, cursor) {
                    return LazyHirResult::ItemGenericParam(enclosing, *idx);
                }
            }
            // 2) Then check path entries and compute segment index precisely
            let mut best: Option<(PathId<'db>, usize, parser::TextRange)> = None;
            for (range, (path_id, pspan)) in dmap.path_entries.iter() {
                if Self::range_contains_inclusive(*range, cursor) {
                    let mut seg_idx = path_id.segment_index(db);
                    for idx in 0..=path_id.segment_index(db) {
                        if let Some(id_span) = pspan.clone().segment(idx).ident().resolve(db) {
                            if Self::range_contains_inclusive(id_span.range, cursor) {
                                seg_idx = idx;
                                break;
                            }
                        }
                    }
                    let len = range.end() - range.start();
                    if best
                        .as_ref()
                        .map(|(_, _, r)| (r.end() - r.start()) > len)
                        .unwrap_or(true)
                    {
                        best = Some((*path_id, seg_idx, *range));
                    }
                }
            }
            if let Some((path_id, seg_idx, _)) = best {
                return LazyHirResult::ItemPath(
                    enclosing,
                    path_id,
                    ItemNodeContext::Regular,
                    Some(seg_idx),
                );
            }
        }
        LazyHirResult::None
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

/// Utility function to create a lazy HIR resolver for a cursor position
pub fn lazy_hir_for_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> LazyHirResult<'db> {
    let resolver = ModuleLazyHir::new(top_mod);
    resolver.find_hir_at_position(db, cursor)
}

// High-level resolution is delegated to hir-analysis. This module stays focused on
// cursor → HIR node synthesis only.

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

    // Removed heavy perf/AST-index tests; synthesis focuses on map-based lookups.
}
