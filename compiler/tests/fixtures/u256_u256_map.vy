contract Foo:
    pub bar: map<uint256, uint256>

    pub def read_bar(key: uint256) -> uint256:
        return self.bar[key]

    pub def write_bar(key: uint256, value: uint256):
        self.bar[key] = value