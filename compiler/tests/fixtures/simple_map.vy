contract Foo:
    pub bar: map<u256, u256>

    pub def read_bar(key: u256) -> u256:
        return bar[key]

    pub def write_bar(key: u256, value: u256) -> u256:
        bar[key] = value
---
{
    object "Foo" {
        bar := 0:u256

        function write_bar(key:u256, value:u256) {
            _map_load_u256_u256(bar, key, value)
        }

        function read_bar(key:u256) -> return_val {
            return_val := _map_load_u256_u256(bar, key)
        }

        function _map_sload_u256_u256(m:u256, k:u256) -> v:u256 {
            let p:u256 := msize()
            mstore(msize(), m)
            mstore(msize(), k)
            v := sload(keccak(p, 2:u256))
        }

        function _map_sstore_u256_u256(m:u256, k:u256, v:u256) {
            let p:u256 := msize()
            mstore(msize(), m)
            mstore(msize(), k)
            sstore(keccak(p, 2:u256), v)
        }
    }
}
