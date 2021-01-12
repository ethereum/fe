use yultsur::*;

/// Returns the highest available pointer.
pub fn avail() -> yul::Statement {
    function_definition! {
        function avail() -> ptr {
            (ptr := mload(0x00))
            (if (eq(ptr, 0x00)) { (ptr := 0x20) })
        }
    }
}

/// Allocate a given number of bytes.
pub fn alloc() -> yul::Statement {
    function_definition! {
        function alloc(size) -> ptr {
            (ptr := mload(0x00))
            (if (eq(ptr, 0x00)) { (ptr := 0x20) })
            (mstore(0x00, (add(ptr, size))))
        }
    }
}

/// Stores a value in a newly allocated memory segment.
pub fn alloc_mstoren() -> yul::Statement {
    function_definition! {
        function alloc_mstoren(val, size) -> ptr {
            (ptr := alloc(size))
            (mstoren(ptr, val, size))
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

/// Copy calldata to memory a newly allocated segment of memory.
pub fn ccopym() -> yul::Statement {
    function_definition! {
        function ccopym(cptr, size) -> mptr {
            (mptr := alloc(size))
            (calldatacopy(mptr, cptr, size))
        }
    }
}

/// Load a static string from data into a newly allocated segment of memory.
pub fn load_data_string() -> yul::Statement {
    function_definition! {
        function load_data_string(code_ptr, size) -> mptr {
            (mptr := alloc(32))
            (mstore(mptr, size))
            (let content_ptr := alloc(size))
            (datacopy(content_ptr, code_ptr, size))
        }
    }
}

/// Copy memory to a given segment of storage.
pub fn mcopys() -> yul::Statement {
    function_definition! {
        function mcopys(mptr, sptr, size) {
            (let offset := 0)
            (for { } (lt((add(offset, 32)), size)) { }
            {
                (let _mptr := add(mptr, offset))
                (let _sptr := add(sptr, offset))
                (sstore(_sptr, (mload(_mptr))))
                (offset := add(offset, 32))
            })

            (let rem := sub(size, offset))
            (if (gt(rem, 0)) {
                (let _mptr := add(mptr, offset))
                (let _sptr := add(sptr, offset))
                (sstoren(_sptr, (mloadn(_mptr, rem)), rem))
            })
        }
    }
}

/// Copy storage to a newly allocated segment of memory.
pub fn scopym() -> yul::Statement {
    function_definition! {
        function scopym(sptr, size) -> mptr {
            (mptr := alloc(size))
            (let offset := 0)
            (for { } (lt((add(offset, 32)), size)) { }
            {
                (let _mptr := add(mptr, offset))
                (let _sptr := add(sptr, offset))
                (mstore(_mptr, (sload(_sptr))))
                (offset := add(offset, 32))
            })

            (let rem := sub(size, offset))
            (if (gt(rem, 0)) {
                (let _mptr := add(mptr, offset))
                (let _sptr := add(sptr, offset))
                (mstoren(_mptr, (sloadn(_sptr, rem)), rem))
            })
        }
    }
}

/// Copies a segment of storage to another segment of storage.
pub fn scopys() -> yul::Statement {
    function_definition! {
        function scopys(ptr1, ptr2, size) {
            (let offset := 0)
            (for { } (lt((add(offset, 32)), size)) { }
            {
                (let _ptr1 := add(ptr1, offset))
                (let _ptr2 := add(ptr2, offset))
                (sstore(_ptr2, (sload(_ptr1))))
                (offset := add(offset, 32))
            })

            (let rem := sub(size, offset))
            (if (gt(rem, 0)) {
                (let _ptr1 := add(ptr1, offset))
                (let _ptr2 := add(ptr2, offset))
                (sstoren(_ptr2, (sloadn(_ptr1, rem)), rem))
            })
        }
    }
}

/// Copies a segment of memory to another segment of memory.
pub fn mcopym() -> yul::Statement {
    function_definition! {
        function mcopym(ptr1, size) -> ptr2 {
            (ptr2 := alloc(size))
            (let offset := 0)
            (for { } (lt((add(offset, 32)), size)) { }
            {
                (let _ptr1 := add(ptr1, offset))
                (let _ptr2 := add(ptr2, offset))
                (mstore(_ptr2, (mload(_ptr1))))
                (offset := add(offset, 32))
            })

            (let rem := sub(size, offset))
            (if (gt(rem, 0)) {
                (let _ptr1 := add(ptr1, offset))
                (let _ptr2 := add(ptr2, offset))
                (mstoren(_ptr2, (mloadn(_ptr1, rem)), rem))
            })
        }
    }
}

/// Read a 256 bit value from memory and right-shift according to size.
pub fn mloadn() -> yul::Statement {
    function_definition! {
        function mloadn(ptr, size) -> val {
            (val := shr((sub(256, (mul(8, size)))), (mload(ptr))))
        }
    }
}

/// Read a 256 bit value from storage and right-shift according to size.
pub fn sloadn() -> yul::Statement {
    function_definition! {
        function sloadn(ptr, size) -> val {
            (val := shr((sub(256, (mul(8, size)))), (sload(ptr))))
        }
    }
}

/// Read a 256 bit value from calldata and right-shift according to size.
pub fn cloadn() -> yul::Statement {
    function_definition! {
        function cloadn(ptr, size) -> val {
            (val := (shr((sub(256, (mul(8, size)))), (calldataload(ptr)))))
        }
    }
}

/// Stores a value in memory, only modifying the given size (0 < size <= 32).
pub fn mstoren() -> yul::Statement {
    function_definition! {
        function mstoren(ptr, val, size) {
            (let size_bits := mul(8, size))
            (let left := shl((sub(256, size_bits)), val))
            (let right := shr(size_bits, (mload((add(ptr, size))))))
            (mstore(ptr, (or(left, right))))
        }
    }
}

/// Stores a value in storage, only modifying the given size (0 < size <= 32).
pub fn sstoren() -> yul::Statement {
    function_definition! {
        function sstoren(ptr, val, size) {
            (let size_bits := mul(8, size))
            (let left := shl((sub(256, size_bits)), val))
            (let right := shr(size_bits, (sload((add(ptr, size))))))
            (sstore(ptr, (or(left, right))))
        }
    }
}

/// Takes two 256 bit values and returns the keccak256 value of both.
pub fn dualkeccak256() -> yul::Statement {
    function_definition! {
        function dualkeccak256(a, b) -> return_val {
            (let ptr := avail())
            (mstore(ptr, a))
            (mstore((add(ptr, 32)), b))
            (return_val := keccak256(ptr, 64))
        }
    }
}

/// Rounds a 256 bit value up to the nearest multiple of 32.
pub fn ceil32() -> yul::Statement {
    function_definition! {
        function ceil32(n) -> return_val {
            (return_val := mul((div((add(n, 31)), 32)), 32))
        }
    }
}

/// Evaluates the ternary expression and returns the result.
pub fn ternary() -> yul::Statement {
    function_definition! {
        function ternary(test, if_expr, else_expr) -> result {
            ([switch! {
                switch test
                (case 1 {(result := if_expr)})
                (case 0 {(result := else_expr)})
            }])
        }
    }
}
