contract Foo:
    pub bar: map<address, bytes[10]>

    pub def read_bar(key: address) -> bytes[10]:
        return bar[key]

    pub def write_bar(key: address, value: bytes[10]):
        bar[key] = value