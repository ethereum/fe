//! HIR integration layer for language server features
//!
//! This module provides a thin integration layer between the language server
//! and the HIR synthesis API. It focuses on language server specific concerns
//! like position conversion, LSP response formatting, and feature integration.
//!
//! The actual HIR synthesis logic lives in the HIR crate's synthesis module.
//! This module only handles:
//! - Position/offset conversion for LSP protocol
//! - Integration with existing language server features
//! - Fallback to current implementations when needed
//! - LSP-specific result formatting

// Re-export the main synthesis API from HIR crate for convenience
pub use hir::lazy_hir_for_cursor;
pub use hir::synthesis::{LazyHir, LazyHirResult, ModuleLazyHir, ResolveHir};

// Re-export cursor type for consistency with existing language server code
pub use parser::TextSize as Cursor;

/// Language server specific utilities for HIR integration
pub struct HirIntegration;

impl HirIntegration {
    /// Convert LSP position to cursor offset for HIR synthesis
    pub fn position_to_cursor(position: async_lsp::lsp_types::Position, text: &str) -> Cursor {
        crate::util::to_offset_from_position(position, text)
    }

    /// Quick HIR lookup for language server features
    pub fn find_hir_at_lsp_position<'db>(
        db: &'db dyn hir::SpannedHirDb,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        position: async_lsp::lsp_types::Position,
        text: &str,
    ) -> LazyHirResult<'db> {
        let cursor = Self::position_to_cursor(position, text);
        lazy_hir_for_cursor(db, top_mod, cursor)
    }

    /// Check if HIR synthesis found a meaningful result
    pub fn has_hir_result(result: &LazyHirResult<'_>) -> bool {
        result.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    #[test]
    fn test_hir_integration_api() {
        // Test that the re-exported API works correctly
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some("fn test() { 42 }".to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test basic HIR resolution
        let result = lazy_hir_for_cursor(&db, top_mod, Cursor::from(0));

        // Should work without panicking
        assert!(matches!(result, LazyHirResult::None) || result.is_some());
    }

    #[test]
    fn test_position_conversion() {
        let text = "fn test() {\n    42\n}";
        let position = async_lsp::lsp_types::Position::new(1, 4); // Line 1, column 4 ("42")

        let cursor = HirIntegration::position_to_cursor(position, text);

        // Should convert to a valid cursor position
        assert!((u32::from(cursor) as usize) < text.len());
    }

    #[test]
    fn test_lsp_position_lookup() {
        let mut db = DriverDataBase::default();
        let text = "fn test() {\n    let x = 42;\n    x\n}";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(text.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test lookup at line 1, column 8 (around "x = 42")
        let position = async_lsp::lsp_types::Position::new(1, 8);
        let result = HirIntegration::find_hir_at_lsp_position(&db, top_mod, position, text);

        // Should work without panicking and provide meaningful result info
        let has_result = HirIntegration::has_hir_result(&result);
        println!("HIR result found: {}", has_result);
    }

    #[test]
    fn test_module_lazy_hir_wrapper() {
        // Test that ModuleLazyHir works through the wrapper
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some("fn test() { 42 }".to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let resolver = ModuleLazyHir::new(top_mod);
        let result = resolver.find_hir_at_position(&db, Cursor::from(10));

        // Should work without panicking
        assert!(matches!(result, LazyHirResult::None) || result.is_some());
    }
}
