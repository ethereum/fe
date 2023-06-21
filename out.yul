object \"test\" {
    code {
        function buf$rw_u16($a, $b) {
            let $$tmp_2 := $$alloc(64)
            let $$tmp_4 := $$alloc(64)
            $$tmp_4 := buf$MemoryBuffer$new(4)
            $mcopym($$tmp_4, $$tmp_2, 64)
            let $$tmp_5 := $$alloc(128)
            let $$tmp_6 := $$alloc(128)
            $$tmp_6 := buf$MemoryBuffer$reader($$tmp_2)
            $mcopym($$tmp_6, $$tmp_5, 128)
            let $$tmp_7 := $$alloc(128)
            let $$tmp_8 := $$alloc(128)
            $$tmp_8 := buf$MemoryBuffer$writer($$tmp_2)
            $mcopym($$tmp_8, $$tmp_7, 128)
            buf$MemoryBufferWriter$write_u16($$tmp_7, $a)
            buf$MemoryBufferWriter$write_u16($$tmp_7, $b)
            let $$tmp_11
            $$tmp_11 := buf$MemoryBufferReader$read_u16($$tmp_5)
            let $$tmp_12
            $$tmp_12 := $$tmp_11
            let $$tmp_13
            $$tmp_13 := $a
            spec$assert_eq($$tmp_12, $$tmp_13)
            let $$tmp_15
            $$tmp_15 := buf$MemoryBufferReader$read_u16($$tmp_5)
            let $$tmp_16
            $$tmp_16 := $$tmp_15
            let $$tmp_17
            $$tmp_17 := $b
            spec$assert_eq($$tmp_16, $$tmp_17)
            leave
        }
        function buf$MemoryBuffer$new($len) -> $$ret {
            let $$tmp_2
            $$tmp_2 := add($len, 31)
            let $$tmp_3
            $$tmp_3 := buf$alloc($$tmp_2)
            let $$tmp_4 := $$alloc(64)
            $aggregate_init_10($$tmp_4, $$tmp_3, $len)
            $$ret := $$tmp_4
            leave
        }
        function buf$MemoryBuffer$reader($self) -> $$ret {
            let $$tmp_1 := $$alloc(128)
            $$tmp_1 := buf$MemoryBufferReader$new($self)
            let $$tmp_2 := $$alloc(128)
            $mcopym($$tmp_1, $$tmp_2, 128)
            $$ret := $$tmp_2
            leave
        }
        function buf$MemoryBuffer$writer($self) -> $$ret {
            let $$tmp_1 := $$alloc(128)
            $$tmp_1 := buf$MemoryBufferWriter$new($self)
            let $$tmp_2 := $$alloc(128)
            $mcopym($$tmp_1, $$tmp_2, 128)
            $$ret := $$tmp_2
            leave
        }
        function buf$MemoryBufferWriter$write_u16($self, $value) {
            buf$MemoryBufferWrite$u16$write_buf($value, $self)
            leave
        }
        function buf$MemoryBufferReader$read_u16($self) -> $$ret {
            let $$tmp_2
            $$tmp_2 := buf$MemoryBufferReader$read_n($self, 2)
            let $$tmp_3
            $$tmp_3 := $$tmp_2
            $$ret := $$tmp_3
            leave
        }
        function spec$assert_eq($a, $b) {
            let $$tmp_2
            $$tmp_2 := evm$eq($a, $b)
            let $$tmp_4
            $$tmp_4 := eq($$tmp_2, 0)
            switch $$tmp_4
            case 1 { spec$invalid() }
            case 0 { }
            leave
        }
        function buf$alloc($len) -> $$ret {
            let $$tmp_1
            $$tmp_1 := buf$avail()
            let $$tmp_3
            $$tmp_3 := add($$tmp_1, $len)
            evm$mstore(64, $$tmp_3)
            $$ret := $$tmp_1
            leave
        }
        function buf$MemoryBufferReader$new($buf) -> $$ret {
            let $$tmp_1 := $$alloc(64)
            $mcopym($buf, $$tmp_1, 64)
            let $$tmp_2
            $$tmp_2 := buf$MemoryBuffer$len($buf)
            let $$tmp_3 := $$alloc(64)
            $$tmp_3 := buf$Cursor$new($$tmp_2)
            let $$tmp_4 := $$alloc(64)
            $mcopym($$tmp_3, $$tmp_4, 64)
            let $$tmp_5 := $$alloc(128)
            $aggregate_init_11($$tmp_5, $$tmp_1, $$tmp_4)
            $$ret := $$tmp_5
            leave
        }
        function buf$MemoryBufferWriter$new($buf) -> $$ret {
            let $$tmp_1 := $$alloc(64)
            $mcopym($buf, $$tmp_1, 64)
            let $$tmp_2
            $$tmp_2 := buf$MemoryBuffer$len($buf)
            let $$tmp_3 := $$alloc(64)
            $$tmp_3 := buf$Cursor$new($$tmp_2)
            let $$tmp_4 := $$alloc(64)
            $mcopym($$tmp_3, $$tmp_4, 64)
            let $$tmp_5 := $$alloc(128)
            $aggregate_init_12($$tmp_5, $$tmp_1, $$tmp_4)
            $$ret := $$tmp_5
            leave
        }
        function buf$MemoryBufferWrite$u16$write_buf($self, $writer) {
            let $$tmp_2
            $$tmp_2 := $self
            buf$MemoryBufferWriter$write_n($writer, $$tmp_2, 2)
            leave
        }
        function buf$MemoryBufferReader$read_n($self, $len) -> $$ret {
            let $$tmp_2
            $$tmp_2 := buf$MemoryBufferReader$read_offset($self, $len)
            let $$tmp_3
            $$tmp_3 := evm$mload($$tmp_2)
            let $$tmp_6
            $$tmp_6 := mul($len, 8)
            let $$tmp_7
            $$tmp_7 := sub(256, $$tmp_6)
            let $$tmp_8
            $$tmp_8 := evm$shr($$tmp_7, $$tmp_3)
            $$ret := $$tmp_8
            leave
        }
        function evm$eq($x, $y) -> $$ret {
            let $$tmp_2
            $$tmp_2 := eq($x, $y)
            $$ret := $$tmp_2
            leave
        }
        function spec$invalid() {
            evm$revert_mem(0, 0)
            leave
        }
        function buf$avail() -> $$ret {
            let $$tmp_0
            $$tmp_0 := evm$mload(64)
            let $$tmp_3
            $$tmp_3 := eq($$tmp_0, 0)
            switch $$tmp_3
            case 1 {
                $$ret := 96
                leave
            }
            case 0 {
                $$ret := $$tmp_0
                leave
            }
        }
        function evm$mstore($p, $v) {
            mstore($p, $v)
            leave
        }
        function buf$MemoryBuffer$len($self) -> $$ret {
            let $$tmp_2
            $$tmp_2 := $$mptr_load(add($self, 32), 0)
            $$ret := $$tmp_2
            leave
        }
        function buf$Cursor$new($len) -> $$ret {
            let $$tmp_2 := $$alloc(64)
            $aggregate_init_15($$tmp_2, 0, $len)
            $$ret := $$tmp_2
            leave
        }
        function buf$MemoryBufferWriter$write_n($self, $value, $len) {
            let $$tmp_3
            $$tmp_3 := buf$MemoryBufferWriter$write_offset($self, $len)
            let $$tmp_4
            let $$tmp_7
            $$tmp_7 := mul($len, 8)
            let $$tmp_8
            $$tmp_8 := sub(256, $$tmp_7)
            $$tmp_4 := evm$shl($$tmp_8, $value)
            evm$mstore($$tmp_3, $$tmp_4)
            leave
        }
        function buf$MemoryBufferReader$read_offset($self, $len) -> $$ret {
            let $$tmp_3 := $$alloc(64)
            $$tmp_3 := add($self, 0)
            let $$tmp_4
            $$tmp_4 := buf$MemoryBuffer$offset($$tmp_3)
            let $$tmp_6 := $$alloc(64)
            $$tmp_6 := add($self, 64)
            let $$tmp_7
            $$tmp_7 := buf$Cursor$advance($$tmp_6, $len)
            let $$tmp_8
            $$tmp_8 := add($$tmp_4, $$tmp_7)
            $$ret := $$tmp_8
            leave
        }
        function evm$mload($p) -> $$ret {
            let $$tmp_1
            $$tmp_1 := mload($p)
            $$ret := $$tmp_1
            leave
        }
        function evm$shr($bits, $value) -> $$ret {
            let $$tmp_2
            $$tmp_2 := shr($bits, $value)
            $$ret := $$tmp_2
            leave
        }
        function evm$revert_mem($offset, $len) { revert($offset, $len) }
        function buf$MemoryBufferWriter$write_offset($self, $len) -> $$ret {
            let $$tmp_3 := $$alloc(64)
            $$tmp_3 := add($self, 0)
            let $$tmp_4
            $$tmp_4 := buf$MemoryBuffer$offset($$tmp_3)
            let $$tmp_6 := $$alloc(64)
            $$tmp_6 := add($self, 64)
            let $$tmp_7
            $$tmp_7 := buf$Cursor$advance($$tmp_6, $len)
            let $$tmp_8
            $$tmp_8 := add($$tmp_4, $$tmp_7)
            $$ret := $$tmp_8
            leave
        }
        function evm$shl($bits, $value) -> $$ret {
            let $$tmp_2
            $$tmp_2 := shl($bits, $value)
            $$ret := $$tmp_2
            leave
        }
        function buf$MemoryBuffer$offset($self) -> $$ret {
            let $$tmp_2
            $$tmp_2 := $$mptr_load(add($self, 0), 0)
            $$ret := $$tmp_2
            leave
        }
        function buf$Cursor$advance($self, $len) -> $$ret {
            let $$tmp_2
            $$tmp_2 := $$mptr_load(add($self, 0), 0)
            let $$tmp_4
            $$tmp_4 := $$mptr_load(add($self, 0), 0)
            $$mptr_store(add($self, 0), add($$tmp_4, $len), 0, 0x0)
            $$ret := $$tmp_2
            leave
        }
        function $$alloc(size) -> ptr {
            ptr := mload(64)
            if eq(ptr, 0x00) { ptr := 96 }
            mstore(64, add(ptr, size))
        }
        function $mcopym($src, $dst, $size) {
            let iter_count := div($size, 32)
            let original_src := $src
            for { let i := 0 } lt(i, iter_count) { i := add(i, 1) } {
                mstore($dst, mload($src))
                $src := add($src, 32)
                $dst := add($dst, 32)
            }
            let rem := sub($size, sub($src, original_src))
            if gt(rem, 0) {
                let rem_bits := mul(rem, 8)
                let dst_mask := sub(shl(sub(256, rem_bits), 1), 1)
                let src_mask := not(dst_mask)
                let src_value := and(mload($src), src_mask)
                let dst_value := and(mload($dst), dst_mask)
                mstore($dst, or(src_value, dst_value))
            }
        }
        function $$mptr_store(ptr, value, shift_num, mask) {
            value := shl(shift_num, value)
            let ptr_value := and(mload(ptr), mask)
            value := or(value, ptr_value)
            mstore(ptr, value)
        }
        function $aggregate_init_10($ptr, $arg0, $arg1) {
            $$mptr_store(add($ptr, 0), $arg0, 0, 0x0)
            $$mptr_store(add($ptr, 32), $arg1, 0, 0x0)
        }
        function $aggregate_init_11($ptr, $arg0, $arg1) {
            $mcopym($arg0, add($ptr, 0), 64)
            $mcopym($arg1, add($ptr, 64), 64)
        }
        function $aggregate_init_12($ptr, $arg0, $arg1) {
            $mcopym($arg0, add($ptr, 0), 64)
            $mcopym($arg1, add($ptr, 64), 64)
        }
        function $$mptr_load(ptr, shift_num) -> ret { ret := shr(shift_num, mload(ptr)) }
        function $aggregate_init_15($ptr, $arg0, $arg1) {
            $$mptr_store(add($ptr, 0), $arg0, 0, 0x0)
            $$mptr_store(add($ptr, 32), $arg1, 0, 0x0)
        }
        buf$rw_u16(calldataload(0), calldataload(32))
        stop()
    }
}
