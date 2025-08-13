# HIR Synthesis Extension: Current State and Knowledge

## Executive Summary

We are extending HIR synthesis to handle item-level constructs (generic type bounds, where clauses, impl headers) to avoid relying on the visitor pattern fallback. This is part of completing an unfinished refactoring where HIR synthesis should fully replace visitor-based traversal.

## Problem Context

### Original Issue
Generic type bounds don't resolve in goto definition:
```rust
pub struct Wrapper<S: Inner> {  // "Inner" doesn't trigger goto definition
    pub inner: S,
}
```

### Root Cause
HIR synthesis was designed only for function bodies, not item-level constructs. When the cursor is on a type bound like `Inner` above, it's not inside any function body, so HIR synthesis returns `None`, falling back to the slower visitor pattern.

## Architecture Overview

### HIR Synthesis System
Located in `/crates/hir/src/synthesis.rs`, the synthesis system provides fast cursor → HIR node resolution:

1. **Body Discovery**: Find which function body contains a cursor position (O(log n) using RangeMap)
2. **AST Resolution**: Use parser's ast_index to find AST nodes at the position
3. **HIR Synthesis**: Convert AST nodes to HIR nodes using source maps

### Key Components

#### `LazyHirResult` Enum
Represents HIR nodes found at cursor positions:
- `Expr(Body, ExprId, HirNodeContext)` - Expression in a function body
- `Stmt(Body, StmtId, HirNodeContext)` - Statement in a function body  
- `Pat(Body, PatId)` - Pattern in a function body
- **NEW**: `ItemPath(ItemKind, PathId, ItemNodeContext)` - Path in item context
- **NEW**: `ItemType(ItemKind, TypeId, ItemNodeContext)` - Type in item context
- `None` - No HIR node found

#### `ItemNodeContext` Enum (NEW)
Context for item-level HIR nodes:
- `TypeBound` - Type bound in generic parameter (e.g., `T: Trait`)
- `WhereClause` - Where clause predicate
- `SuperTrait` - Super trait in trait definition
- `ImplHeader` - Type in impl header
- `GenericParam` - Generic parameter name
- `Regular` - Regular item context

## Implementation Progress

### Completed
✅ Added `ItemPath` and `ItemType` variants to `LazyHirResult`
✅ Added `ItemNodeContext` enum for item-level contexts
✅ Refactored `find_hir_at_position` to separate body vs item handling
✅ Extracted body handling into `find_hir_in_body` method
✅ Implemented `find_hir_in_item` method structure
✅ Added AST traversal for:
   - Struct generic parameters and bounds
   - Impl generic parameters and bounds  
   - ImplTrait generic parameters and bounds
   - Trait generic parameters and bounds
✅ Implemented `extract_path_from_type_bound` helper
✅ Code compiles successfully

### In Progress
🔄 `resolve_item_path` method - Currently returns `None`, needs to:
   1. Find the HIR ItemKind corresponding to the AST item
   2. Resolve the path within that item's scope
   3. Return appropriate `LazyHirResult::ItemPath` or `LazyHirResult::ItemType`

### Not Started
❌ Where clause handling (placeholder returns `None`)
❌ Super trait handling in trait definitions
❌ Associated type bounds
❌ Connecting AST items to HIR items for actual resolution
❌ Testing the new synthesis paths
❌ Removing visitor pattern fallback

## Technical Details

### AST Structure Discovery
The Fe parser AST has these key types for generic bounds:
- `GenericParamList` - List of generic parameters (implements `IntoIterator`)
- `GenericParam` - Individual parameter with `kind()` method
- `TypeGenericParam` - Type parameter with `bounds()` returning `TypeBoundList`
- `TypeBound` - Individual bound with `trait_bound()` method
- `TraitRef` - Trait reference with `path()` method

### Item Types
- `Struct` - Struct definitions
- `Impl` - Self implementations (no trait)
- `ImplTrait` - Trait implementations
- `Trait` - Trait definitions
- `Enum`, `TypeAlias`, `Const`, etc. - Other item types

### Key Traits Used
- `GenericParamsOwner` - Provides `generic_params()` method
- `WhereClauseOwner` - Provides `where_clause()` method
- `AstNode` - Core trait for AST nodes

## Current Limitations

1. **No HIR Resolution**: The `resolve_item_path` stub means we find the AST nodes but can't resolve them to HIR definitions yet.

2. **Missing Item Mapping**: Need to connect AST items to their HIR counterparts. The existing lowering infrastructure should have this mapping.

3. **Incomplete Coverage**: Only handling generic bounds so far, not where clauses, super traits, or associated types.

4. **No Tests**: The new code paths aren't tested yet. Need to verify with the existing test case in `goto_generic_bounds_test.rs`.

## Next Steps

### Immediate (Priority 1)
1. Implement `resolve_item_path` by:
   - Finding how AST items map to HIR items in the lowering module
   - Using the item's scope to resolve paths
   - Returning proper `LazyHirResult` variants

2. Test with existing test case to verify generic bounds resolve

### Short Term (Priority 2)  
1. Handle where clauses
2. Handle super traits in trait definitions
3. Add more test coverage

### Long Term (Priority 3)
1. Remove visitor pattern fallback entirely
2. Performance benchmarking vs visitor pattern
3. Handle all remaining item-level constructs

## Code Locations

- **Main implementation**: `/crates/hir/src/synthesis.rs`
- **Goto definition**: `/crates/language-server/src/functionality/goto.rs`
- **Test file**: `/crates/language-server/src/functionality/goto_generic_bounds_test.rs`
- **Parser AST types**: `/crates/parser/src/ast/`

## Design Philosophy

The HIR synthesis approach is fundamentally more efficient than visitor pattern because:
- **O(log n)** body lookup with RangeMap vs O(n) tree traversal
- **Localized** AST traversal within items vs whole-file traversal
- **Direct** source map lookups vs recursive searching
- **Cacheable** via Salsa queries

This extension maintains these performance characteristics while expanding coverage to item-level constructs that were previously only accessible via the slower visitor pattern.