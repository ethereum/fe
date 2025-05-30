# Expected vs Actual Decision Tree Structure for Tuple Patterns

This document illustrates the bug in Fe's decision tree generation for tuple patterns.

## The Problem

Currently, Fe treats all tuple patterns as single opaque constructors instead of decomposing them into tests on individual elements.

## Example 1: Simple Boolean Tuple

### Pattern:
```fe
fn test_boolean_tuple(t: (bool, bool)) -> u8 {
    match t {
        (true, true) => 3
        (true, false) => 2
        (false, true) => 1
        (false, false) => 0
    }
}
```

### Current (Incorrect) Decision Tree:
```
Switch on expr
└─ tuple() =>
   └─ Execute arm #0
```

### Expected (Correct) Decision Tree:
```
Switch on expr.0
├─ true =>
│  └─ Switch on expr.1
│     ├─ true => Execute arm #0   // (true, true)
│     └─ false => Execute arm #1  // (true, false)
└─ false =>
   └─ Switch on expr.1
      ├─ true => Execute arm #2   // (false, true)
      └─ false => Execute arm #3  // (false, false)
```

## Example 2: Three-Element Tuple

### Pattern:
```fe
fn test_three_tuple(t: (bool, bool, bool)) -> u8 {
    match t {
        (true, true, true) => 7
        (true, true, false) => 6
        (true, false, true) => 5
        (true, false, false) => 4
        (false, true, true) => 3
        (false, true, false) => 2
        (false, false, true) => 1
        (false, false, false) => 0
    }
}
```

### Current (Incorrect) Decision Tree:
```
Switch on expr
└─ tuple() =>
   └─ Execute arm #0
```

### Expected (Correct) Decision Tree:
```
Switch on expr.0
├─ true =>
│  └─ Switch on expr.1
│     ├─ true =>
│     │  └─ Switch on expr.2
│     │     ├─ true => Execute arm #0   // (true, true, true)
│     │     └─ false => Execute arm #1  // (true, true, false)
│     └─ false =>
│        └─ Switch on expr.2
│           ├─ true => Execute arm #2   // (true, false, true)
│           └─ false => Execute arm #3  // (true, false, false)
└─ false =>
   └─ Switch on expr.1
      ├─ true =>
      │  └─ Switch on expr.2
      │     ├─ true => Execute arm #4   // (false, true, true)
      │     └─ false => Execute arm #5  // (false, true, false)
      └─ false =>
         └─ Switch on expr.2
            ├─ true => Execute arm #6   // (false, false, true)
            └─ false => Execute arm #7  // (false, false, false)
```

## Example 3: Mixed Type Tuple

### Pattern:
```fe
fn test_mixed_tuple(t: (bool, u8)) -> u8 {
    match t {
        (true, 0) => 10
        (true, 1) => 11
        (true, _) => 12
        (false, 0) => 20
        (false, _) => 21
    }
}
```

### Current (Incorrect) Decision Tree:
```
Switch on expr
└─ tuple() =>
   └─ Execute arm #0
```

### Expected (Correct) Decision Tree:
```
Switch on expr.0
├─ true =>
│  └─ Switch on expr.1
│     ├─ 0 => Execute arm #0      // (true, 0)
│     ├─ 1 => Execute arm #1      // (true, 1)
│     └─ _ => Execute arm #2      // (true, _)
└─ false =>
   └─ Switch on expr.1
      ├─ 0 => Execute arm #3      // (false, 0)
      └─ _ => Execute arm #4      // (false, _)
```

## Example 4: Nested Tuples

### Pattern:
```fe
fn test_nested_tuple(t: ((bool, bool), bool)) -> u8 {
    match t {
        ((true, true), true) => 4
        ((true, true), false) => 3
        ((true, false), true) => 2
        ((true, false), false) => 1
        ((false, _), _) => 0
    }
}
```

### Current (Incorrect) Decision Tree:
```
Switch on expr
└─ tuple() =>
   └─ Execute arm #0
```

### Expected (Correct) Decision Tree:
```
Switch on expr.0.0  // First element of inner tuple
├─ true =>
│  └─ Switch on expr.0.1  // Second element of inner tuple
│     ├─ true =>
│     │  └─ Switch on expr.1  // Outer tuple second element
│     │     ├─ true => Execute arm #0   // ((true, true), true)
│     │     └─ false => Execute arm #1  // ((true, true), false)
│     └─ false =>
│        └─ Switch on expr.1
│           ├─ true => Execute arm #2   // ((true, false), true)
│           └─ false => Execute arm #3  // ((true, false), false)
└─ false =>
   └─ Execute arm #4  // ((false, _), _)
```

## Impact of the Bug

1. **Incorrect Analysis**: Exhaustiveness and unreachability analysis is completely wrong for tuple patterns
2. **Performance**: All tuple patterns result in linear search instead of efficient decision trees
3. **Debugging**: Pattern matching behavior cannot be properly analyzed or optimized
4. **Correctness**: While runtime might work due to separate codegen, static analysis is fundamentally broken

## What Needs to be Fixed

The decision tree builder needs to:

1. **Recognize tuple patterns** as composite patterns that need decomposition
2. **Generate proper occurrences** for tuple elements (expr.0, expr.1, etc.)
3. **Create nested switch nodes** for each tuple element
4. **Handle wildcards and or-patterns** correctly within tuple context
5. **Support nested tuples** with proper occurrence paths

## Test Status

All current decision tree tests pass because they only test that:
- Decision trees can be generated without crashing
- The output can be formatted/displayed

They do NOT test that the decision trees are correct or optimal.