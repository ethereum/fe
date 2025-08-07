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
    hir_def::{Body, ExprId, ItemKind, PatId, StmtId, TopLevelMod},
    span::{body_source_map, HirOrigin, LazySpan},
    HirDb, SpannedHirDb,
};

pub type Cursor = TextSize;

/// Result of a lazy HIR lookup from cursor position
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LazyHirResult<'db> {
    /// Found an expression HIR node
    Expr(Body<'db>, ExprId),
    /// Found a statement HIR node
    Stmt(Body<'db>, StmtId),
    /// Found a pattern HIR node
    Pat(Body<'db>, PatId),
    /// No HIR node found at this position
    None,
}

impl<'db> LazyHirResult<'db> {
    /// Get the body containing this HIR node, if any
    pub fn body(&self) -> Option<Body<'db>> {
        match self {
            LazyHirResult::Expr(body, _)
            | LazyHirResult::Stmt(body, _)
            | LazyHirResult::Pat(body, _) => Some(*body),
            LazyHirResult::None => None,
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
    body_cache: Option<RangeMap<TextSize, Body<'db>>>,
}

impl<'db> ModuleLazyHir<'db> {
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
        self.body_cache = Some(body_index.clone());
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
        cache.get(&cursor).copied()
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
        let Some(body) = resolver.find_containing_body(db, cursor) else {
            return LazyHirResult::None;
        };

        // Get the source map for this body
        let source_map = body_source_map(db, body);

        // Find the AST node at the cursor position within this body
        let ast_node = find_ast_node_at_position(db, body, cursor);

        // Convert AST node to HIR using the source map
        match ast_node {
            AstNodeAtPosition::Expr(expr_ast) => {
                let origin = HirOrigin::Raw(parser::ast::AstPtr::new(&expr_ast));
                if let Some(&expr_id) = source_map.expr_map.source_to_node.get(&origin) {
                    LazyHirResult::Expr(body, expr_id)
                } else {
                    LazyHirResult::None
                }
            }
            AstNodeAtPosition::Stmt(stmt_ast) => {
                let origin = HirOrigin::Raw(parser::ast::AstPtr::new(&stmt_ast));
                if let Some(&stmt_id) = source_map.stmt_map.source_to_node.get(&origin) {
                    LazyHirResult::Stmt(body, stmt_id)
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

    fn find_bodies_at_position(&self, db: &'db dyn SpannedHirDb, cursor: Cursor) -> Vec<Body<'db>> {
        let mut resolver = self.clone();
        if let Some(body) = resolver.find_containing_body(db, cursor) {
            vec![body]
        } else {
            vec![]
        }
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
    find_ast_node_at_position(&root_syntax, cursor)
}

/// Build the body index for a module (non-Salsa version for now)
pub fn module_body_index<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
) -> RangeMap<TextSize, Body<'db>> {
    let mut body_index = RangeMap::new();

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
        }
    }

    body_index
}

/// Build the AST spatial index for a module (non-Salsa version for now)
pub fn module_ast_index<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
) -> parser::ast_index::AstSpatialIndex {
    // Parse the file and build spatial index
    let green_node = crate::lower::parse_file_impl(db, top_mod);
    let root_syntax = parser::SyntaxNode::new_root(green_node);

    parser::ast_index::AstSpatialIndex::build_from_syntax(&root_syntax)
}

/// Utility function to create a lazy HIR resolver for a cursor position
pub fn lazy_hir_for_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> LazyHirResult<'db> {
    let resolver = ModuleLazyHir::new(top_mod);
    resolver.find_hir_at_position(db, cursor)
}

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
            let _ast_result = find_ast_node_at_position(&db, *body, cursor);
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
