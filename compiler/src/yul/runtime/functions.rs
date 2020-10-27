use fe_semantics::namespace::types::{
    AbiEncoding,
    AbiType,
    FeSized,
};
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

/// Dynamically creates an encoding function for any set of type parameters.
pub fn abi_encode<T: AbiEncoding>(types: Vec<T>) -> yul::Statement {
    let func_name = abi_encode_name(types.iter().map(|typ| typ.abi_name()).collect());
    let mut params = vec![];
    let mut stmts = vec![];

    // iterate over all parameters of our encoding function and create a statement
    // that encodes them
    for (i, _) in types.iter().enumerate() {
        let param_ident = identifier! { (format!("val_{}", i)) };
        params.push(param_ident.clone());
        let param_expr = identifier_expression! { [param_ident] };

        let stmt = match types[i].abi_type() {
            AbiType::UniformRecursive { child, count } => {
                // we copy each value in memory to a new segment with the correct padding
                // all values use a left padding
                let element_count = literal_expression! { (count) };
                let element_size = literal_expression! { (child.size()) };
                let element_abi_size = literal_expression! { (child.abi_size()) };

                statement! {
                    (for {(let i := 0)} (lt(i, [element_count])) {(i := add(i, 1))}
                    {
                        (let val_ptr := add([param_expr], (mul(i, [element_size.clone()]))))
                        (let val := mloadn(val_ptr, [element_size]))
                        (pop((alloc_mstoren(val, [element_abi_size]))))
                    })
                }
            }
            AbiType::Terminal => {
                // we store each terminal value in a new slot
                // these should always be base types with an `abi_size` of 32 bytes
                let abi_size = literal_expression! { (types[i].abi_size()) };
                statement! { pop((alloc_mstoren([param_expr], [abi_size]))) }
            }
        };
        stmts.push(stmt)
    }

    // our encoding function take a dynamic set of params and generates an encoding
    // statement for each param
    function_definition! {
        function [func_name]([params...]) -> ptr {
            (ptr := avail())
            [stmts...]
        }
    }
}

/// Generates an ABI encoding function name for a given set of types.
pub fn abi_encode_name(names: Vec<String>) -> yul::Identifier {
    let mut full_name = "abi_encode".to_string();

    for name in names {
        let safe_name = name.replace("[", "").replace("]", "");
        full_name.push('_');
        full_name.push_str(&safe_name);
    }

    identifier! { (full_name) }
}

#[cfg(test)]
mod tests {
    use crate::yul::runtime::functions::{
        abi_encode,
        abi_encode_name,
    };
    use fe_semantics::namespace::types::Base;

    #[test]
    fn test_abi_encode_name() {
        assert_eq!(
            abi_encode_name(vec!["u256".to_string(), "bytes[100]".to_string()]).to_string(),
            "abi_encode_u256_bytes100"
        )
    }

    #[test]
    fn test_abi_encode() {
        assert_eq!(
            abi_encode(vec![Base::U256, Base::Address]).to_string(),
            "function abi_encode_uint256_address(val_0, val_1) -> ptr { ptr := avail() pop(alloc_mstoren(val_0, 32)) pop(alloc_mstoren(val_1, 32)) }"
        )
    }
}
