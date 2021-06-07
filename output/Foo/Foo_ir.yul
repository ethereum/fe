object \"Foo\" {
    code {
        let size := datasize(\"runtime\")
        datacopy(0, dataoffset(\"runtime\"), size)
        return(0, size)
    }
    object \"runtime\" {
        code {
            function $$bar() -> return_val { revert(0, 0) }
            function $$revert_custom_error() -> return_val {
                revert_with_0x3253e3c7_u256_bool(struct_Error_new(1, true), add(64, 0))
                {
                    return_val := 0x0
                    leave
                }
            }
            function $$revert_other_error() -> return_val {
                revert_with_0x9bc8662f_u256_bool(struct_OtherError_new(1, true), add(64, 0))
                {
                    return_val := 0x0
                    leave
                }
            }
            function contract_create2(data_ptr, data_size, value, salt) -> return_address {
                let mptr := alloc(data_size)
                datacopy(mptr, data_ptr, data_size)
                return_address := create2(value, mptr, data_size, salt)
            }
            function contract_create(data_ptr, data_size, value) -> return_address {
                let mptr := alloc(data_size)
                datacopy(mptr, data_ptr, data_size)
                return_address := create(value, mptr, data_size)
            }
            function abi_unpack(mptr, array_size, inner_data_size) { for { let i := 0 } lt(i, array_size) { i := add(i, 1) } {
                let val_ptr := add(mptr, mul(i, inner_data_size))
                let val := mloadn(val_ptr, inner_data_size)
                pop(alloc_mstoren(val, 32))
            } }
            function abi_pack_calldata(mptr, array_size, inner_data_size) -> packed_ptr {
                packed_ptr := avail()
                for { let i := 0 } lt(i, array_size) { i := add(i, 1) } {
                    let val_ptr := add(mptr, mul(i, 32))
                    let val := calldataload(val_ptr)
                    pop(alloc_mstoren(val, inner_data_size))
                }
            }
            function abi_pack_mem(mptr, array_size, inner_data_size) -> packed_ptr {
                packed_ptr := avail()
                for { let i := 0 } lt(i, array_size) { i := add(i, 1) } {
                    let val_ptr := add(mptr, mul(i, 32))
                    let val := mload(val_ptr)
                    pop(alloc_mstoren(val, inner_data_size))
                }
            }
            function alloc_mstoren(val, size) -> ptr {
                ptr := alloc(size)
                mstoren(ptr, size, val)
            }
            function alloc(size) -> ptr {
                ptr := mload(0x00)
                if eq(ptr, 0x00) { ptr := 0x20 }
                mstore(0x00, add(ptr, size))
            }
            function avail() -> ptr {
                ptr := mload(0x00)
                if eq(ptr, 0x00) { ptr := 0x20 }
            }
            function bytes_mcopys(mptr, sptr, size) {
                let word_ptr := div(sptr, 32)
                mcopys(mptr, word_ptr, size)
            }
            function bytes_scopym(sptr, size) -> mptr {
                let word_ptr := div(sptr, 32)
                mptr := scopym(word_ptr, size)
            }
            function bytes_scopys(ptr1, ptr2, size) {
                let word_ptr1 := div(ptr1, 32)
                let word_ptr2 := div(ptr2, 32)
                scopys(word_ptr1, word_ptr2, size)
            }
            function bytes_sloadn(sptr, size) -> val {
                let word_ptr := div(sptr, 32)
                let bytes_offset := mod(sptr, 32)
                val := sloadn(word_ptr, bytes_offset, size)
            }
            function bytes_sstoren(sptr, size, val) {
                let word_ptr := div(sptr, 32)
                let bytes_offset := mod(sptr, 32)
                sstoren(word_ptr, bytes_offset, size, val)
            }
            function ccopym(cptr, size) -> mptr {
                mptr := alloc(size)
                calldatacopy(mptr, cptr, size)
            }
            function ceil32(n) -> return_val { return_val := mul(div(add(n, 31), 32), 32) }
            function cloadn(ptr, size) -> val { val := shr(sub(256, mul(8, size)), calldataload(ptr)) }
            function free(ptr) { mstore(0x00, ptr) }
            function load_data_string(code_ptr, size) -> mptr {
                mptr := alloc(32)
                mstore(mptr, size)
                let content_ptr := alloc(size)
                datacopy(content_ptr, code_ptr, size)
            }
            function map_value_ptr(a, b) -> return_val {
                let ptr := avail()
                mstore(ptr, a)
                mstore(add(ptr, 32), b)
                let hash := keccak256(ptr, 64)
                return_val := set_zero(248, 256, hash)
            }
            function mcopym(ptr1, size) -> ptr2 {
                ptr2 := alloc(size)
                let offset := 0
                for { } lt(add(offset, 32), size) { } {
                    let _ptr1 := add(ptr1, offset)
                    let _ptr2 := add(ptr2, offset)
                    mstore(_ptr2, mload(_ptr1))
                    offset := add(offset, 32)
                }
                let rem := sub(size, offset)
                if gt(rem, 0) {
                    let _ptr1 := add(ptr1, offset)
                    let _ptr2 := add(ptr2, offset)
                    mstoren(_ptr2, rem, mloadn(_ptr1, rem))
                }
            }
            function mcopys(mptr, sptr, size) {
                let mptr_offset := 0
                let sptr_offset := 0
                for { } lt(add(mptr_offset, 32), size) { } {
                    let _mptr := add(mptr, mptr_offset)
                    let _sptr := add(sptr, sptr_offset)
                    sstore(_sptr, mload(_mptr))
                    mptr_offset := add(mptr_offset, 32)
                    sptr_offset := add(sptr_offset, 1)
                }
                let rem := sub(size, mptr_offset)
                if gt(rem, 0) {
                    let _mptr := add(mptr, mptr_offset)
                    let _sptr := add(sptr, sptr_offset)
                    let zeroed_val := set_zero(mul(rem, 8), 256, mload(_mptr))
                    sstore(_sptr, zeroed_val)
                }
            }
            function mloadn(ptr, size) -> val { val := shr(sub(256, mul(8, size)), mload(ptr)) }
            function mstoren(ptr, size, val) {
                let size_bits := mul(8, size)
                let left := shl(sub(256, size_bits), val)
                let right := shr(size_bits, mload(add(ptr, size)))
                mstore(ptr, or(left, right))
            }
            function scopym(sptr, size) -> mptr {
                mptr := alloc(size)
                let mptr_offset := 0
                let sptr_offset := 0
                for { } lt(add(mptr_offset, 32), size) { } {
                    let _mptr := add(mptr, mptr_offset)
                    let _sptr := add(sptr, sptr_offset)
                    mstore(_mptr, sload(_sptr))
                    mptr_offset := add(mptr_offset, 32)
                    sptr_offset := add(sptr_offset, 1)
                }
                let rem := sub(size, mptr_offset)
                if gt(rem, 0) {
                    let _mptr := add(mptr, mptr_offset)
                    let _sptr := add(sptr, sptr_offset)
                    mstoren(_mptr, rem, sloadn(_sptr, 0, rem))
                }
            }
            function scopys(ptr1, ptr2, size) {
                let word_size := div(add(size, 31), 32)
                let offset := 0
                for { } lt(add(offset, 1), size) { } {
                    let _ptr1 := add(ptr1, offset)
                    let _ptr2 := add(ptr2, offset)
                    sstore(_ptr2, sload(_ptr1))
                    offset := add(offset, 1)
                }
            }
            function set_zero(start_bit, end_bit, val) -> result {
                let left_shift_dist := sub(256, start_bit)
                let right_shift_dist := end_bit
                let left := shl(left_shift_dist, shr(left_shift_dist, val))
                let right := shr(right_shift_dist, shl(right_shift_dist, val))
                result := or(left, right)
            }
            function sloadn(word_ptr, bytes_offset, bytes_size) -> val {
                let bits_offset := mul(bytes_offset, 8)
                let bits_size := mul(bytes_size, 8)
                let bits_padding := sub(256, bits_size)
                let word := sload(word_ptr)
                let word_shl := shl(bits_offset, word)
                val := shr(bits_padding, word_shl)
            }
            function sstoren(word_ptr, bytes_offset, bytes_size, val) {
                let bits_offset := mul(bytes_offset, 8)
                let bits_size := mul(bytes_size, 8)
                let old_word := sload(word_ptr)
                let zeroed_word := set_zero(bits_offset, add(bits_offset, bits_size), old_word)
                let left_shift_dist := sub(sub(256, bits_size), bits_offset)
                let offset_val := shl(left_shift_dist, val)
                let new_word := or(zeroed_word, offset_val)
                sstore(word_ptr, new_word)
            }
            function ternary(test, if_expr, else_expr) -> result { switch test
            case 1 { result := if_expr }
            case 0 { result := else_expr } }
            function checked_add_u256(val1, val2) -> sum {
                if gt(val1, sub(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, val2)) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_u128(val1, val2) -> sum {
                if gt(val1, sub(0xffffffffffffffffffffffffffffffff, val2)) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_u64(val1, val2) -> sum {
                if gt(val1, sub(0xffffffffffffffff, val2)) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_u32(val1, val2) -> sum {
                if gt(val1, sub(0xffffffff, val2)) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_u16(val1, val2) -> sum {
                if gt(val1, sub(0xffff, val2)) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_u8(val1, val2) -> sum {
                if gt(val1, sub(0xff, val2)) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_i256(val1, val2) -> sum {
                if and(iszero(slt(val1, 0)), sgt(val2, sub(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, val1))) { revert(0, 0) }
                if and(slt(val1, 0), slt(val2, sub(0x8000000000000000000000000000000000000000000000000000000000000000, val1))) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_i128(val1, val2) -> sum {
                if and(iszero(slt(val1, 0)), sgt(val2, sub(0x7fffffffffffffffffffffffffffffff, val1))) { revert(0, 0) }
                if and(slt(val1, 0), slt(val2, sub(0xffffffffffffffffffffffffffffffff80000000000000000000000000000000, val1))) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_i64(val1, val2) -> sum {
                if and(iszero(slt(val1, 0)), sgt(val2, sub(0x7fffffffffffffff, val1))) { revert(0, 0) }
                if and(slt(val1, 0), slt(val2, sub(0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000, val1))) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_i32(val1, val2) -> sum {
                if and(iszero(slt(val1, 0)), sgt(val2, sub(0x7fffffff, val1))) { revert(0, 0) }
                if and(slt(val1, 0), slt(val2, sub(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000, val1))) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_i16(val1, val2) -> sum {
                if and(iszero(slt(val1, 0)), sgt(val2, sub(0x7fff, val1))) { revert(0, 0) }
                if and(slt(val1, 0), slt(val2, sub(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000, val1))) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_add_i8(val1, val2) -> sum {
                if and(iszero(slt(val1, 0)), sgt(val2, sub(0x7f, val1))) { revert(0, 0) }
                if and(slt(val1, 0), slt(val2, sub(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80, val1))) { revert(0, 0) }
                sum := add(val1, val2)
            }
            function checked_div_unsigned(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                result := div(val1, val2)
            }
            function checked_div_i256(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                if and(eq(val1, 0x8000000000000000000000000000000000000000000000000000000000000000), eq(val2, sub(0, 1))) { revert(0, 0) }
                result := sdiv(val1, val2)
            }
            function checked_div_i128(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                if and(eq(val1, 0xffffffffffffffffffffffffffffffff80000000000000000000000000000000), eq(val2, sub(0, 1))) { revert(0, 0) }
                result := sdiv(val1, val2)
            }
            function checked_div_i64(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                if and(eq(val1, 0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000), eq(val2, sub(0, 1))) { revert(0, 0) }
                result := sdiv(val1, val2)
            }
            function checked_div_i32(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                if and(eq(val1, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000), eq(val2, sub(0, 1))) { revert(0, 0) }
                result := sdiv(val1, val2)
            }
            function checked_div_i16(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                if and(eq(val1, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000), eq(val2, sub(0, 1))) { revert(0, 0) }
                result := sdiv(val1, val2)
            }
            function checked_div_i8(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                if and(eq(val1, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80), eq(val2, sub(0, 1))) { revert(0, 0) }
                result := sdiv(val1, val2)
            }
            function checked_exp_unsigned(base, exponent, max) -> power {
                if iszero(exponent) {
                    power := 1
                    leave
                }
                if iszero(base) {
                    power := 0
                    leave
                }
                switch base
                case 1 {
                    power := 1
                    leave
                }
                case 2 {
                    if gt(exponent, 255) { revert(0, 0) }
                    power := exp(2, exponent)
                    if gt(power, max) { revert(0, 0) }
                    leave
                }
                if and(sgt(power, 0), gt(power, div(max, base))) { revert(0, 0) }
                if or(and(lt(base, 11), lt(exponent, 78)), and(lt(base, 307), lt(exponent, 32))) {
                    power := exp(base, exponent)
                    if gt(power, max) { revert(0, 0) }
                    leave
                }
                power, base := checked_exp_helper(1, base, exponent, max)
                if gt(power, div(max, base)) { revert(0, 0) }
                power := mul(power, base)
            }
            function checked_exp_signed(base, exponent, min, max) -> power {
                switch exponent
                case 0 {
                    power := 1
                    leave
                }
                case 1 {
                    power := base
                    leave
                }
                if iszero(base) {
                    power := 0
                    leave
                }
                power := 1
                switch sgt(base, 0)
                case 1 { if gt(base, div(max, base)) { revert(0, 0) } }
                case 0 { if slt(base, sdiv(max, base)) { revert(0, 0) } }
                if and(exponent, 1) { power := base }
                base := mul(base, base)
                exponent := shr(1, exponent)
                power, base := checked_exp_helper(power, base, exponent, max)
                if and(sgt(power, 0), gt(power, div(max, base))) { revert(0, 0) }
                if and(slt(power, 0), slt(power, sdiv(min, base))) { revert(0, 0) }
                power := mul(power, base)
            }
            function checked_exp_helper(_power, _base, exponent, max) -> power, base {
                power := _power
                base := _base
                for { } gt(exponent, 1) { } {
                    if gt(base, div(max, base)) { revert(0, 0) }
                    if and(exponent, 1) { power := mul(power, base) }
                    base := mul(base, base)
                    exponent := shr(1, exponent)
                }
            }
            function checked_exp_u256(base, exponent) -> power { power := checked_exp_unsigned(base, exponent, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff) }
            function checked_exp_u128(base, exponent) -> power { power := checked_exp_unsigned(base, exponent, 0xffffffffffffffffffffffffffffffff) }
            function checked_exp_u64(base, exponent) -> power { power := checked_exp_unsigned(base, exponent, 0xffffffffffffffff) }
            function checked_exp_u32(base, exponent) -> power { power := checked_exp_unsigned(base, exponent, 0xffffffff) }
            function checked_exp_u16(base, exponent) -> power { power := checked_exp_unsigned(base, exponent, 0xffff) }
            function checked_exp_u8(base, exponent) -> power { power := checked_exp_unsigned(base, exponent, 0xff) }
            function checked_exp_i256(base, exponent) -> power { power := checked_exp_signed(base, exponent, 0x8000000000000000000000000000000000000000000000000000000000000000, 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff) }
            function checked_exp_i128(base, exponent) -> power { power := checked_exp_signed(base, exponent, 0xffffffffffffffffffffffffffffffff80000000000000000000000000000000, 0x7fffffffffffffffffffffffffffffff) }
            function checked_exp_i64(base, exponent) -> power { power := checked_exp_signed(base, exponent, 0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000, 0x7fffffffffffffff) }
            function checked_exp_i32(base, exponent) -> power { power := checked_exp_signed(base, exponent, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000, 0x7fffffff) }
            function checked_exp_i16(base, exponent) -> power { power := checked_exp_signed(base, exponent, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000, 0x7fff) }
            function checked_exp_i8(base, exponent) -> power { power := checked_exp_signed(base, exponent, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80, 0x7f) }
            function checked_mod_unsigned(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                result := mod(val1, val2)
            }
            function checked_mod_signed(val1, val2) -> result {
                if iszero(val2) { revert(0, 0) }
                result := smod(val1, val2)
            }
            function checked_mul_u256(val1, val2) -> product {
                if and(iszero(iszero(val1)), gt(val2, div(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, val1))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_u128(val1, val2) -> product {
                if and(iszero(iszero(val1)), gt(val2, div(0xffffffffffffffffffffffffffffffff, val1))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_u64(val1, val2) -> product {
                if and(iszero(iszero(val1)), gt(val2, div(0xffffffffffffffff, val1))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_u32(val1, val2) -> product {
                if and(iszero(iszero(val1)), gt(val2, div(0xffffffff, val1))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_u16(val1, val2) -> product {
                if and(iszero(iszero(val1)), gt(val2, div(0xffff, val1))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_u8(val1, val2) -> product {
                if and(iszero(iszero(val1)), gt(val2, div(0xff, val1))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_i256(val1, val2) -> product {
                if and(and(sgt(val1, 0), sgt(val2, 0)), gt(val1, div(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, val2))) { revert(0, 0) }
                if and(and(sgt(val1, 0), slt(val2, 0)), slt(val2, sdiv(0x8000000000000000000000000000000000000000000000000000000000000000, val1))) { revert(0, 0) }
                if and(and(slt(val1, 0), sgt(val2, 0)), slt(val1, sdiv(0x8000000000000000000000000000000000000000000000000000000000000000, val2))) { revert(0, 0) }
                if and(and(slt(val1, 0), slt(val2, 0)), slt(val1, sdiv(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, val2))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_i128(val1, val2) -> product {
                if and(and(sgt(val1, 0), sgt(val2, 0)), gt(val1, div(0x7fffffffffffffffffffffffffffffff, val2))) { revert(0, 0) }
                if and(and(sgt(val1, 0), slt(val2, 0)), slt(val2, sdiv(0xffffffffffffffffffffffffffffffff80000000000000000000000000000000, val1))) { revert(0, 0) }
                if and(and(slt(val1, 0), sgt(val2, 0)), slt(val1, sdiv(0xffffffffffffffffffffffffffffffff80000000000000000000000000000000, val2))) { revert(0, 0) }
                if and(and(slt(val1, 0), slt(val2, 0)), slt(val1, sdiv(0x7fffffffffffffffffffffffffffffff, val2))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_i64(val1, val2) -> product {
                if and(and(sgt(val1, 0), sgt(val2, 0)), gt(val1, div(0x7fffffffffffffff, val2))) { revert(0, 0) }
                if and(and(sgt(val1, 0), slt(val2, 0)), slt(val2, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000, val1))) { revert(0, 0) }
                if and(and(slt(val1, 0), sgt(val2, 0)), slt(val1, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000, val2))) { revert(0, 0) }
                if and(and(slt(val1, 0), slt(val2, 0)), slt(val1, sdiv(0x7fffffffffffffff, val2))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_i32(val1, val2) -> product {
                if and(and(sgt(val1, 0), sgt(val2, 0)), gt(val1, div(0x7fffffff, val2))) { revert(0, 0) }
                if and(and(sgt(val1, 0), slt(val2, 0)), slt(val2, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000, val1))) { revert(0, 0) }
                if and(and(slt(val1, 0), sgt(val2, 0)), slt(val1, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000, val2))) { revert(0, 0) }
                if and(and(slt(val1, 0), slt(val2, 0)), slt(val1, sdiv(0x7fffffff, val2))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_i16(val1, val2) -> product {
                if and(and(sgt(val1, 0), sgt(val2, 0)), gt(val1, div(0x7fff, val2))) { revert(0, 0) }
                if and(and(sgt(val1, 0), slt(val2, 0)), slt(val2, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000, val1))) { revert(0, 0) }
                if and(and(slt(val1, 0), sgt(val2, 0)), slt(val1, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000, val2))) { revert(0, 0) }
                if and(and(slt(val1, 0), slt(val2, 0)), slt(val1, sdiv(0x7fff, val2))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_mul_i8(val1, val2) -> product {
                if and(and(sgt(val1, 0), sgt(val2, 0)), gt(val1, div(0x7f, val2))) { revert(0, 0) }
                if and(and(sgt(val1, 0), slt(val2, 0)), slt(val2, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80, val1))) { revert(0, 0) }
                if and(and(slt(val1, 0), sgt(val2, 0)), slt(val1, sdiv(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80, val2))) { revert(0, 0) }
                if and(and(slt(val1, 0), slt(val2, 0)), slt(val1, sdiv(0x7f, val2))) { revert(0, 0) }
                product := mul(val1, val2)
            }
            function checked_sub_unsigned(val1, val2) -> diff {
                if lt(val1, val2) { revert(0, 0) }
                diff := sub(val1, val2)
            }
            function checked_sub_i256(val1, val2) -> diff {
                if and(iszero(slt(val2, 0)), slt(val1, add(0x8000000000000000000000000000000000000000000000000000000000000000, val2))) { revert(0, 0) }
                if and(slt(val2, 0), sgt(val1, add(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, val2))) { revert(0, 0) }
                diff := sub(val1, val2)
            }
            function checked_sub_i128(val1, val2) -> diff {
                if and(iszero(slt(val2, 0)), slt(val1, add(0xffffffffffffffffffffffffffffffff80000000000000000000000000000000, val2))) { revert(0, 0) }
                if and(slt(val2, 0), sgt(val1, add(0x7fffffffffffffffffffffffffffffff, val2))) { revert(0, 0) }
                diff := sub(val1, val2)
            }
            function checked_sub_i64(val1, val2) -> diff {
                if and(iszero(slt(val2, 0)), slt(val1, add(0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000, val2))) { revert(0, 0) }
                if and(slt(val2, 0), sgt(val1, add(0x7fffffffffffffff, val2))) { revert(0, 0) }
                diff := sub(val1, val2)
            }
            function checked_sub_i32(val1, val2) -> diff {
                if and(iszero(slt(val2, 0)), slt(val1, add(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000, val2))) { revert(0, 0) }
                if and(slt(val2, 0), sgt(val1, add(0x7fffffff, val2))) { revert(0, 0) }
                diff := sub(val1, val2)
            }
            function checked_sub_i16(val1, val2) -> diff {
                if and(iszero(slt(val2, 0)), slt(val1, add(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000, val2))) { revert(0, 0) }
                if and(slt(val2, 0), sgt(val1, add(0x7fff, val2))) { revert(0, 0) }
                diff := sub(val1, val2)
            }
            function checked_sub_i8(val1, val2) -> diff {
                if and(iszero(slt(val2, 0)), slt(val1, add(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80, val2))) { revert(0, 0) }
                if and(slt(val2, 0), sgt(val1, add(0x7f, val2))) { revert(0, 0) }
                diff := sub(val1, val2)
            }
            function abi_encode_uint256(encode_val_0) -> return_ptr {
                return_ptr := avail()
                let data_offset := 32
                {
                    let ptr := alloc(32)
                    mstore(ptr, encode_val_0)
                }
            }
            function abi_encode_tuple_uint256_bool(encode_val_0) -> return_ptr {
                return_ptr := avail()
                let data_offset := 64
                pop(mcopym(encode_val_0, 64))
            }
            function abi_encode_tuple_uint256_bool(encode_val_0) -> return_ptr {
                return_ptr := avail()
                let data_offset := 64
                pop(mcopym(encode_val_0, 64))
            }
            function abi_decode_data__calldata(head_start, data_end) { }
            function revert_with_0x3253e3c7_u256_bool(data_ptr, size) {
                let ptr := alloc_mstoren(0x3253e3c7, 4)
                pop(abi_encode_tuple_uint256_bool(data_ptr))
                revert(ptr, add(4, size))
            }
            function revert_with_0x9bc8662f_u256_bool(data_ptr, size) {
                let ptr := alloc_mstoren(0x9bc8662f, 4)
                pop(abi_encode_tuple_uint256_bool(data_ptr))
                revert(ptr, add(4, size))
            }
            function struct_Error_new(msg, val) -> return_val {
                return_val := alloc(32)
                mstore(return_val, msg)
                let val_ptr := alloc(32)
                mstore(val_ptr, val)
            }
            function struct_Error_get_msg_ptr(ptr) -> return_val { return_val := add(ptr, 0) }
            function struct_Error_get_val_ptr(ptr) -> return_val { return_val := add(ptr, 63) }
            function struct_OtherError_new(msg, val) -> return_val {
                return_val := alloc(32)
                mstore(return_val, msg)
                let val_ptr := alloc(32)
                mstore(val_ptr, val)
            }
            function struct_OtherError_get_msg_ptr(ptr) -> return_val { return_val := add(ptr, 0) }
            function struct_OtherError_get_val_ptr(ptr) -> return_val { return_val := add(ptr, 63) }
            switch cloadn(0, 4)
            case 0xfebb0f7e {
                let return_val := $$bar()
                let encode_start := abi_encode_uint256(return_val)
                let encode_size := add(32, 0)
                return(encode_start, encode_size)
            }
            case 0x39c3e2d7 {
                pop($$revert_custom_error())
                return(0, 0)
            }
            case 0x285bc32f {
                pop($$revert_other_error())
                return(0, 0)
            }
            default { return(0, 0) }
        }
    }
}