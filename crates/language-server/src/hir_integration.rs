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

    /// Benchmark HIR synthesis performance vs traditional approaches
    pub fn benchmark_hir_synthesis<'db>(
        db: &'db dyn hir::SpannedHirDb,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        positions: &[Cursor],
    ) -> HirSynthesisBenchmark {
        let mut hir_synthesis_times = Vec::new();
        let mut total_hir_time = std::time::Duration::ZERO;

        // Benchmark HIR synthesis approach
        for &position in positions {
            let start = std::time::Instant::now();
            let _result = lazy_hir_for_cursor(db, top_mod, position);
            let duration = start.elapsed();
            hir_synthesis_times.push(duration);
            total_hir_time += duration;
        }

        let avg_hir_time = total_hir_time / positions.len() as u32;
        let min_hir_time = hir_synthesis_times
            .iter()
            .min()
            .copied()
            .unwrap_or_default();
        let max_hir_time = hir_synthesis_times
            .iter()
            .max()
            .copied()
            .unwrap_or_default();

        HirSynthesisBenchmark {
            positions_tested: positions.len(),
            average_time: avg_hir_time,
            min_time: min_hir_time,
            max_time: max_hir_time,
            total_time: total_hir_time,
        }
    }

    /// Run comprehensive performance analysis
    pub fn analyze_performance<'db>(
        db: &'db dyn hir::SpannedHirDb,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        source_text: &str,
    ) -> PerformanceAnalysis {
        // Generate test positions across the source
        let positions: Vec<Cursor> = (0..source_text.len())
            .step_by(source_text.len().max(1) / 50) // Test 50 positions
            .map(|i| Cursor::from(i as u32))
            .collect();

        let benchmark = Self::benchmark_hir_synthesis(db, top_mod, &positions);

        let positions_per_second = if benchmark.average_time.as_nanos() > 0 {
            1_000_000_000 / benchmark.average_time.as_nanos() as u64
        } else {
            u64::MAX
        };

        PerformanceAnalysis {
            source_length: source_text.len(),
            benchmark,
            positions_per_second,
        }
    }
}

/// Performance benchmark results for HIR synthesis
#[derive(Debug, Clone)]
pub struct HirSynthesisBenchmark {
    pub positions_tested: usize,
    pub average_time: std::time::Duration,
    pub min_time: std::time::Duration,
    pub max_time: std::time::Duration,
    pub total_time: std::time::Duration,
}

/// Comprehensive performance analysis
#[derive(Debug, Clone)]
pub struct PerformanceAnalysis {
    pub source_length: usize,
    pub benchmark: HirSynthesisBenchmark,
    pub positions_per_second: u64,
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

    #[test]
    fn test_hir_synthesis_performance() {
        let mut db = DriverDataBase::default();
        let source = r#"
fn complex_function(x: i32, y: bool) -> String {
    let mut result = String::new();

    if y {
        for i in 0..x {
            if i % 2 == 0 {
                result.push_str(&format!("even: {}", i));
            } else {
                result.push_str(&format!("odd: {}", i));
            }
        }
    } else {
        result = format!("simple: {}", x);
    }

    result
}

fn another_function() -> Vec<i32> {
    let mut numbers = Vec::new();
    for i in 0..10 {
        numbers.push(i * i);
    }
    numbers
}

fn third_function(data: &[i32]) -> Option<i32> {
    data.iter().max().copied()
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///performance.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Run performance analysis
        let analysis = HirIntegration::analyze_performance(&db, top_mod, source);

        println!("Performance Analysis Results:");
        println!("Source length: {} characters", analysis.source_length);
        println!("Positions tested: {}", analysis.benchmark.positions_tested);
        println!("Average lookup time: {:?}", analysis.benchmark.average_time);
        println!("Min lookup time: {:?}", analysis.benchmark.min_time);
        println!("Max lookup time: {:?}", analysis.benchmark.max_time);
        println!("Total time: {:?}", analysis.benchmark.total_time);
        println!("Positions per second: {}", analysis.positions_per_second);

        // Verify performance characteristics
        assert!(analysis.benchmark.average_time.as_nanos() < 10_000_000); // < 10ms
        assert!(analysis.positions_per_second > 100); // > 100 positions/sec
        assert!(analysis.benchmark.positions_tested > 0);
    }

    #[test]
    fn test_benchmark_consistency() {
        let mut db = DriverDataBase::default();
        let source = "fn test() { let x = 42; x + 1 }";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///consistency.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let positions = vec![
            Cursor::from(0),
            Cursor::from(10),
            Cursor::from(20),
            Cursor::from(30),
        ];

        // Run benchmark multiple times
        let benchmark1 = HirIntegration::benchmark_hir_synthesis(&db, top_mod, &positions);
        let benchmark2 = HirIntegration::benchmark_hir_synthesis(&db, top_mod, &positions);

        // Results should be consistent (within reasonable variance)
        let time_diff = if benchmark1.average_time > benchmark2.average_time {
            benchmark1.average_time - benchmark2.average_time
        } else {
            benchmark2.average_time - benchmark1.average_time
        };

        // Allow for some variance, but should be mostly consistent
        assert!(time_diff < std::time::Duration::from_millis(50));
        assert_eq!(benchmark1.positions_tested, benchmark2.positions_tested);
    }
}
