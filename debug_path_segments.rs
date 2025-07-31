fn main() {
    // The snapshot shows:
    // cursor position (6, 13), path: goto_debug_nested::nested
    // But there's no entry for cursor position (6, 21) which would be NESTED_CONST
    
    // This means the test framework is not extracting a cursor position at column 21
    // The test uses extract_multiple_cursor_positions_from_spans which gets ALL spans
    
    // The problem might be:
    // 1. We're not creating a span for the NESTED_CONST segment
    // 2. Or the span resolution is not working correctly
    
    println!("Current behavior in goto_debug_nested.snap:");
    println!("  - cursor (6, 13) -> goto_debug_nested::nested");
    println!("  - cursor (6, 21) -> NOT PRESENT IN SNAPSHOT");
    println!("\nThis suggests the NESTED_CONST segment span is not being created/extracted properly");
}