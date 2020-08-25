contract Foo:
    pub def bar(x: uint256) -> uint256[5]:
        my_array: uint256[5]
        my_array[3] = x
        return my_array
