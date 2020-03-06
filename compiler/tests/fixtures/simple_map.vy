contract Foo:
    pub bar: map<u256, u256>

    pub def read_bar(key: u256) -> u256:
        return bar[key]

    pub def write_bar(key: u256, value: u256) -> u256:
        bar[key] = value