use crate::constants::PANIC_OUT_OF_BOUNDS;
use crate::operations::revert as revert_operations;

use yultsur::*;

/// Return all data runtime functions
pub fn all() -> Vec<yul::Statement> {
    vec![
        alloc_mstoren(),
        alloc(),
        avail(),
        bytes_mcopys(),
        bytes_scopym(),
        bytes_scopys(),
        bytes_sloadn(),
        bytes_sstoren(),
        ccopym(),
        ceil32(),
        cloadn(),
        free(),
        get_array_item(),
        load_data_string(),
        map_value_ptr(),
        mcopym(),
        mcopys(),
        mloadn(),
        mstoren(),
        scopym(),
        scopys(),
        set_zero(),
        sloadn(),
        sstoren(),
        ternary(),
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

/// Set the highest available pointer.
pub fn free() -> yul::Statement {
    function_definition! {
        function free(ptr) {
            (mstore(0x00, ptr))
        }
    }
}

/// Set the given segment of the value (defined in bits) to zero.
pub fn set_zero() -> yul::Statement {
    function_definition! {
        function set_zero(start_bit, end_bit, val) -> result {
            (let left_shift_dist := sub(256, start_bit))
            (let right_shift_dist := end_bit)
            // shift left then right to zero out the desired bits
            (let left := shl(left_shift_dist, (shr(left_shift_dist, val))))
            // shift right then left to zero out the desired bits
            (let right := shr(right_shift_dist, (shl(right_shift_dist, val))))
            // combine the left and right side
            // the segment defined by `start_bit` and `end_bit` will be zero
            (result := or(left, right))
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

/// Copy calldata to a newly allocated segment of memory.
pub fn ccopym() -> yul::Statement {
    function_definition! {
        function ccopym(cptr, size) -> mptr {
            (mptr := alloc(size))
            (calldatacopy(mptr, cptr, size))
        }
    }
}

/// Copy memory to a given segment of storage.
///
/// The storage pointer addresses a word.
pub fn mcopys() -> yul::Statement {
    function_definition! {
        function mcopys(mptr, sptr, size) {
            (let mptr_offset := 0)
            (let sptr_offset := 0)
            (for { } (lt((add(mptr_offset, 32)), size)) { }
            {
                (let _mptr := add(mptr, mptr_offset))
                (let _sptr := add(sptr, sptr_offset))
                (sstore(_sptr, (mload(_mptr))))
                (mptr_offset := add(mptr_offset, 32))
                (sptr_offset := add(sptr_offset, 1))
            })

            (let rem := sub(size, mptr_offset))
            (if (gt(rem, 0)) {
                (let _mptr := add(mptr, mptr_offset))
                (let _sptr := add(sptr, sptr_offset))
                (let zeroed_val := set_zero((mul(rem, 8)), 256, (mload(_mptr))))
                (sstore(_sptr, zeroed_val))
            })
        }
    }
}

/// Copy storage to a newly allocated segment of memory.
///
/// The storage pointer addresses a word.
pub fn scopym() -> yul::Statement {
    function_definition! {
        function scopym(sptr, size) -> mptr {
            (mptr := alloc(size))
            (let mptr_offset := 0)
            (let sptr_offset := 0)
            (for { } (lt((add(mptr_offset, 32)), size)) { }
            {
                (let _mptr := add(mptr, mptr_offset))
                (let _sptr := add(sptr, sptr_offset))
                (mstore(_mptr, (sload(_sptr))))
                (mptr_offset := add(mptr_offset, 32))
                (sptr_offset := add(sptr_offset, 1))
            })

            (let rem := sub(size, mptr_offset))
            (if (gt(rem, 0)) {
                (let _mptr := add(mptr, mptr_offset))
                (let _sptr := add(sptr, sptr_offset))
                (mstoren(_mptr, rem, (sloadn(_sptr, 0, rem))))
            })
        }
    }
}

/// Copies a segment of storage to another segment of storage.
///
/// The storage pointers address words.
pub fn scopys() -> yul::Statement {
    function_definition! {
        function scopys(ptr1, ptr2, size) {
            (let word_size := div((add(size, 31)), 32))
            (let offset := 0)
            (for { } (lt((add(offset, 1)), size)) { }
            {
                (let _ptr1 := add(ptr1, offset))
                (let _ptr2 := add(ptr2, offset))
                (sstore(_ptr2, (sload(_ptr1))))
                (offset := add(offset, 1))
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
                (mstoren(_ptr2, rem, (mloadn(_ptr1, rem))))
            })
        }
    }
}

/// Read a value of n bytes from memory at the given address.
pub fn mloadn() -> yul::Statement {
    function_definition! {
        function mloadn(ptr, size) -> val {
            (val := shr((sub(256, (mul(8, size)))), (mload(ptr))))
        }
    }
}

/// Read a value of n bytes at the given word address and bytes offset.
///
/// (offset + size) must be less that 32, otherwise it enters undefined
/// behavior.
pub fn sloadn() -> yul::Statement {
    function_definition! {
        function sloadn(word_ptr, bytes_offset, bytes_size) -> val {
            (let bits_offset := mul(bytes_offset, 8))
            (let bits_size := mul(bytes_size, 8))
            (let bits_padding := sub(256, bits_size))

            (let word := sload(word_ptr))
            (let word_shl := shl(bits_offset, word))
            (val := shr(bits_padding, word_shl))
        }
    }
}

/// Read a value of n bytes from calldata at the given address.
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
        function mstoren(ptr, size, val) {
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
        function sstoren(word_ptr, bytes_offset, bytes_size, val) {
            (let bits_offset := mul(bytes_offset, 8))
            (let bits_size := mul(bytes_size, 8))

            (let old_word := sload(word_ptr))
            // zero out the section to be modified
            (let zeroed_word := set_zero(
                bits_offset,
                (add(bits_offset, bits_size)),
                old_word
            ))
            // get the number of bits we need to shift to get the value to the correct offset
            (let left_shift_dist := sub((sub(256, bits_size)), bits_offset))
            (let offset_val := shl(left_shift_dist, val))
            // use or to place the new value in the zeroed out section
            (let new_word := or(zeroed_word, offset_val))
            (sstore(word_ptr, new_word))
        }
    }
}

/// Copy memory to a given segment of storage.
///
/// The storage pointer addresses a byte.
pub fn bytes_mcopys() -> yul::Statement {
    function_definition! {
        function bytes_mcopys(mptr, sptr, size) {
            (let word_ptr := div(sptr, 32))
            (mcopys(mptr, word_ptr, size))
        }
    }
}

/// Copy storage to a newly allocated segment of memory.
///
/// The storage pointer addresses a byte.
pub fn bytes_scopym() -> yul::Statement {
    function_definition! {
        function bytes_scopym(sptr, size) -> mptr {
            (let word_ptr := div(sptr, 32))
            (mptr := scopym(word_ptr, size))
        }
    }
}

/// Copies a segment of storage to another segment of storage.
///
/// The storage pointers address bytes.
pub fn bytes_scopys() -> yul::Statement {
    function_definition! {
        function bytes_scopys(ptr1, ptr2, size) {
            (let word_ptr1 := div(ptr1, 32))
            (let word_ptr2 := div(ptr2, 32))
            (scopys(word_ptr1, word_ptr2, size))
        }
    }
}

/// Read a value of n bytes at the given byte address.
///
/// The value must not span multiple words.
pub fn bytes_sloadn() -> yul::Statement {
    function_definition! {
        function bytes_sloadn(sptr, size) -> val {
            (let word_ptr := div(sptr, 32))
            (let bytes_offset := mod(sptr, 32))
            (val := sloadn(word_ptr, bytes_offset, size))
        }
    }
}

/// Stores a value in storage at the given address, only modifying a segment of
/// the given size.
///
/// The modified segment must not span multiple words.
pub fn bytes_sstoren() -> yul::Statement {
    function_definition! {
        function bytes_sstoren(sptr, size, val) {
            (let word_ptr := div(sptr, 32))
            (let bytes_offset := mod(sptr, 32))
            (sstoren(word_ptr, bytes_offset, size, val))
        }
    }
}

/// Stores a value in a newly allocated memory segment.
pub fn alloc_mstoren() -> yul::Statement {
    function_definition! {
        function alloc_mstoren(val, size) -> ptr {
            (ptr := alloc(size))
            (mstoren(ptr, size, val))
        }
    }
}

/// Derives the byte address of a value corresponding to a map key.
///
/// The address is always divisible by 32, so it points to a word.
pub fn map_value_ptr() -> yul::Statement {
    function_definition! {
        function map_value_ptr(a, b) -> return_val {
            (let ptr := avail())
            (mstore(ptr, a))
            (mstore((add(ptr, 32)), b))
            (let hash := keccak256(ptr, 64))
            (return_val := set_zero(248, 256, hash))
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

/// Returns a pointer to the array item at the requested index.
/// Reverts with a panic if the index is out of bounds.
pub fn get_array_item() -> yul::Statement {
    function_definition! {
        function get_array_item(array_ptr, array_length, index, inner_size) -> ptr {
            (if (iszero((lt(index, array_length)))) {
                [revert_operations::panic_revert(PANIC_OUT_OF_BOUNDS)]
            } )
            (ptr := add(array_ptr, (mul(index, inner_size))))
        }
    }
}
