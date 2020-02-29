use yultsur::*;

/// Returns all functions that should be available during runtime.
pub fn all() -> Vec<yul::Statement> {
    vec![alloc(), free(), avail(), callval(), callptr()]
}

/// Returns the highest available pointer.
pub fn avail() -> yul::Statement {
    function_definition! {
        function avail() -> ptr {
            (ptr := mload(0x00))
        }
    }
}

/// Allocate a given number of bytes.
pub fn alloc() -> yul::Statement {
    function_definition! {
        function alloc(size) -> ptr {
            (ptr := mload(0x00))
            (mstore(0x00, (add(ptr, size))))
        }
    }
}

/// Set the highest available pointer.
pub fn free() -> yul::Statement {
    function_definition! {
        function free(ptr) {
            (mstore(0x00, ptr))
        }
    }
}

/// Returns calldata of a given size(bytes) at `pos` as a u256.
pub fn callval() -> yul::Statement {
    function_definition! {
        function callval(pos, size) -> return_val {
            (return_val := (shr(
                (sub(256, (mul(8, size)))),
                (calldataload(pos))
            )))
        }
    }
}

/// Returns a pointer to a newly allocated segment of memory of a given size(bytes) with the
/// calldata at `pos` copied into it.
pub fn callptr() -> yul::Statement {
    function_definition! {
        function callptr(pos, size) -> return_ptr {
            (return_ptr := alloc(size))
            (calldatacopy(return_ptr, pos, size))
        }
    }
}

#[test]
fn test_avail() {
    assert_eq!(
        avail().to_string(),
        "function avail() -> ptr { ptr := mload(0x00) }"
    )
}

#[test]
fn test_alloc() {
    assert_eq!(
        alloc().to_string(),
        "function alloc(size) -> ptr { ptr := mload(0x00) mstore(0x00, add(ptr, size)) }"
    )
}
#[test]
fn test_free() {
    assert_eq!(
        free().to_string(),
        "function free(ptr) { mstore(0x00, ptr) }"
    )
}
