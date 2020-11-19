use yultsur::*;

/// Returns all functions that should be available during runtime.
pub fn std() -> Vec<yul::Statement> {
    vec![
        avail(),
        alloc(),
        alloc_mstoren(),
        free(),
        ccopy(),
        mcopy(),
        scopy(),
        mloadn(),
        sloadn(),
        cloadn(),
        mstoren(),
        sstoren(),
        dualkeccak256(),
        ceil32(),
    ]
}

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
pub fn ccopy() -> yul::Statement {
    function_definition! {
        function ccopy(cptr, size) -> mptr {
            (mptr := alloc(size))
            (calldatacopy(mptr, cptr, size))
        }
    }
}

/// Copy memory to a given segment of storage.
pub fn mcopy() -> yul::Statement {
    function_definition! {
        function mcopy(mptr, sptr, size) {
            (for {(let i := 0)} (lt(i, size)) {(i := add(i, 1))}
            {
                (let _mptr := add(mptr, i))
                (let _sptr := add(sptr, i))
                (sstoren(_sptr, (mloadn(_mptr, 1)), 1))
            })
        }
    }
}

/// Copy storage to a newly allocated segment of memory.
pub fn scopy() -> yul::Statement {
    function_definition! {
        function scopy(sptr, size) -> mptr {
            (mptr := alloc(size))
            (for {(let i := 0)} (lt(i, size)) {(i := add(i, 1))}
            {
                (let _mptr := add(mptr, i))
                (let _sptr := add(sptr, i))
                (mstoren(_mptr, (sloadn(_sptr, 1)), 1))
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

/// Rounds a 256 bit value up to the naerest multiple of 32.
pub fn ceil32() -> yul::Statement {
    function_definition! {
        function ceil32(n) -> return_val {
            (return_val := mul((div((add(n, 31)), 32)), 32))
        }
    }
}
