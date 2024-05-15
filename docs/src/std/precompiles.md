# Precompiles

Precompiles are EVM functions that are prebuilt and optimized as part of the Fe standard library. There are currently nine precompiles available in Fe. The first four precompiles were defined in the original [Ethereum Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf) (`ec_recover`, `SHA2_256`, `ripemd_160`, `identity)`. Four more were added during the [Byzantium fork](https://ethereum.org/en/history/#byzantium) (`mod_exp`, `ec_add`, `ec_mul` and `ec_pairing`). A final precompile, `blake2f` was added in [EIP-152](https://github.com/ethereum/EIPs/issues/152) during the [Istanbul fork](https://ethereum.org/en/history/#istanbul).

The nine precompiles available in the Fe standard library are:

- [`ec_recover`](#ecrecover)
- [`SHA2 256`](#sha2256)
- [`ripemd160`](#Ripemd160)
- [`identity`](#identity)
- [`mod_exp`](#modexp)
- [`ec_add`](#ecadd)
- [`ec_mul`](#ecmul)
- [`ec_pairing`](#ecpairing)
- [`blake2f`](#blake2f)

These precompiles are imported as follows:

```fe,ignore,ignore
use std::precompiles
```

## `ec_recover`

`ec_recover` is a cryptographic function that retrieves a signer's address from a signed message. It is the fundamental operation used for verifying signatures in Ethereum. Ethereum uses the  Elliptic Curve Digital Signature Algorithm (ECDSA) for verifying signatures. This algorithm uses two parameters, `r` and `s`. Ethereum's implementation also uses an additional 'recovery identifier' parameter, `v`, which is used to identify the correct elliptic curve point from those that can be calculated from `r` and `s` alone.


### Parameters

- `hash`: the hash of the signed message, `u256`
- `v`: the recovery identifier, a number in the range 27-30, `u256`
- `r`: elliptic curve parameter, `u256`
- `s`: elliptic curve parameter, `u256`

### Returns

`ec_recover` returns an address.

### Function signature

```fe,ignore,ignore
pub fn ec_recover(hash: u256, v: u256, r: u256, s: u256) -> address
```

### Example

```fe,ignore,ignore
let result: address = precompiles::ec_recover(
    hash: 0x456e9aea5e197a1f1af7a3e85a3212fa4049a3ba34c2289b4c860fc0b0c64ef3,
    v: 28,
    r: 0x9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac8038825608,
    s: 0x4f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada
)
```

## `SHA2_256`

`SHA2_256` is a hash function. a hash function generates a unique string of characters of fixed length from arbitrary input data.


### Parameters

- `buf`: a sequence of bytes to hash, `MemoryBuffer`

### Returns

`SHA2_256` returns a hash as a `u256`

### Function signature

```fe,ignore,ignore
pub fn sha2_256(buf input_buf: MemoryBuffer) -> u256
```

### Example

```fe,ignore
let buf: MemoryBuffer = MemoryBuffer::from_u8(value: 0xff)
let result: u256 = precompiles::sha2_256(buf)
```

## `ripemd_160`

`ripemd_160` is a hash function that is rarely used in Ethereum, but is included in many crypto libraries as it is used in Bitcoin core.

### Parameters

- `input_buf`: a sequence of bytes to hash, `MemoryBuffer`

### Returns

`ripemd_160` returns a hash as a `u256`

### Function signature

```fe,ignore
pub fn ripemd_160(buf input_buf: MemoryBuffer) -> u256

```
### Example
```fe,ignore
let buf: MemoryBuffer = MemoryBuffer::from_u8(value: 0xff)
let result: u256 = precompiles::ripemd_160(buf)
```


## `identity`

`identity` is a function that simply echoes the input of the function as its output. This can be used for efficient data copying.

### Parameters

- `input_buf`: a sequence of bytes to hash, `MemoryBuffer`

### Returns

`identity` returns a sequence of bytes, `MemoryBuffer`

### Function signature

```fe,ignore
pub fn identity(buf input_buf: MemoryBuffer) -> MemoryBuffer

```
### Example
```fe,ignore
let buf: MemoryBuffer = MemoryBuffer::from_u8(value: 0x42)
let mut result: MemoryBufferReader = precompiles::identity(buf).reader()
```


## `mod_exp`

`mod_exp` is a modular exponentiation function required for elliptic curve operations.

### Parameters

- b: `MemoryBuffer`: the base (i.e. the number being raised to a power), `MemoryBuffer`
- e: `MemoryBuffer`: the exponent (i.e. the power `b` is raised to), `MemoryBuffer`
- m: `MemoryBuffer`: the modulus, `MemoryBuffer`
- b_size: `u256`: the length of `b` in bytes, `u256`
- e_size: `u256`: the length of `e` in bytes, `u256`
- m_size: `u256`: then length of `m` in bytes, `u256`

### Returns

`mod_exp` returns a sequence of bytes, `MemoryBuffer`

### Function signature

```fe,ignore
pub fn mod_exp(
    b_size: u256,
    e_size: u256,
    m_size: u256,
    b: MemoryBuffer,
    e: MemoryBuffer,
    m: MemoryBuffer,
) -> MemoryBuffer

```
### Example
```fe,ignore
let mut result: MemoryBufferReader = precompiles::mod_exp(
    b_size: 1,
    e_size: 1,
    m_size: 1,
    b: MemoryBuffer::from_u8(value: 8),
    e: MemoryBuffer::from_u8(value: 9),
    m: MemoryBuffer::from_u8(value: 10),
).reader()
```

## `ec_add`

`ec_add` does point addition on elliptic curves.

### Parameters

- `x1`: x-coordinate 1, `u256`
- `y1`: y coordinate 1, `u256`
- `x2`: x coordinate 2, `u256`
- `y2`: y coordinate 2, `u256`


### Function signature

```fe,ignore
pub fn ec_add(x1: u256, y1: u256, x2: u256, y2: u256)-> (u256,u256)
```

### Returns

`ec_add` returns a tuple of `u256`, `(u256, u256)`.

### Example

```fe,ignore
let (x, y): (u256, u256) = precompiles::ec_add(x1: 1, y1: 2, x2: 1, y2: 2)
```


## `ec_mul`

`ec_mul` is for multiplying elliptic curve points.

### Parameters

- `x`: x-coordinate, `u256`
- `y`: y coordinate, `u256`
- `s`: multiplier, `u256`

### Function signature

```fe,ignore
pub fn ec_mul(x: u256, y: u256, s: u256)-> (u256,u256)
```

### Returns

`ec_mul` returns a tuple of `u256`, `(u256, u256)`.

### Example

```fe,ignore
let (x, y): (u256, u256) = precompiles::ec_mul(
    x: 1,
    y: 2,
    s: 2
)
```

## `ec_pairing`

`ec_pairing` does elliptic curve pairing - a form of encrypted multiplication.

### Parameters

- `input_buf`: sequence of bytes representing the result of the elliptic curve operation `(G1 * G2) ^ k`, as `MemoryBuffer`

### Returns

`ec_pairing` returns a `bool` indicating whether the pairing is satisfied (`true`) or not (`false`).

### Example

```fe,ignore
    let mut input_buf: MemoryBuffer = MemoryBuffer::new(len: 384)
    let mut writer: MemoryBufferWriter = buf.writer()

    writer.write(value: 0x2cf44499d5d27bb186308b7af7af02ac5bc9eeb6a3d147c186b21fb1b76e18da)
    writer.write(value: 0x2c0f001f52110ccfe69108924926e45f0b0c868df0e7bde1fe16d3242dc715f6)
    writer.write(value: 0x1fb19bb476f6b9e44e2a32234da8212f61cd63919354bc06aef31e3cfaff3ebc)
    writer.write(value: 0x22606845ff186793914e03e21df544c34ffe2f2f3504de8a79d9159eca2d98d9)
    writer.write(value: 0x2bd368e28381e8eccb5fa81fc26cf3f048eea9abfdd85d7ed3ab3698d63e4f90)
    writer.write(value: 0x2fe02e47887507adf0ff1743cbac6ba291e66f59be6bd763950bb16041a0a85e)
    writer.write(value: 0x0000000000000000000000000000000000000000000000000000000000000001)
    writer.write(value: 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd45)
    writer.write(value: 0x1971ff0471b09fa93caaf13cbf443c1aede09cc4328f5a62aad45f40ec133eb4)
    writer.write(value: 0x091058a3141822985733cbdddfed0fd8d6c104e9e9eff40bf5abfef9ab163bc7)
    writer.write(value: 0x2a23af9a5ce2ba2796c1f4e453a370eb0af8c212d9dc9acd8fc02c2e907baea2)
    writer.write(value: 0x23a8eb0b0996252cb548a4487da97b02422ebc0e834613f954de6c7e0afdc1fc)

    assert precompiles::ec_pairing(buf)
}
```


## `blake_2f`

`blake_2f` is a compression algorithm for the cryptographic hash function `BLAKE2b`. It takes as an argument the state vector `h`, message block vector `m`, offset counter `t`, final block indicator flag `f`, and number of rounds `rounds`. The state vector provided as the first parameter is modified by the function.

### Parameters

- `h`: the state vector, `Array<u64, 8>`
- `m`: message block vector, `Array<u64, 16>`
- `t`: offset counter, `Array<u64, 2>`
- `f`: bool
- `rounds`: number of rounds of mixing, `u32`

### Returns

`blake_2f` returns a modified state vector, `Array<u64, 8>`

### Function signature

```fe,ignore
pub fn blake_2f(rounds: u32, h: Array<u64, 8>, m: Array<u64, 16>, t: Array<u64, 2>, f: bool) ->  Array<u64, 8>
```

### Example
```fe,ignore
let result: Array<u64, 8> = precompiles::blake_2f(
    rounds: 12,
    h: [
        0x48c9bdf267e6096a,
        0x3ba7ca8485ae67bb,
        0x2bf894fe72f36e3c,
        0xf1361d5f3af54fa5,
        0xd182e6ad7f520e51,
        0x1f6c3e2b8c68059b,
        0x6bbd41fbabd9831f,
        0x79217e1319cde05b,
    ],
    m: [
        0x6162630000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
        0x0000000000000000,
    ],
    t: [
        0x0300000000000000,
        0x0000000000000000,
    ],
    f: true
)
```