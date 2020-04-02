contract Foo:
    pub def bar(x: uint256, y: uint256, z: uint256) -> uint256[3]:
        my_array: uint256[3]
        my_array[0] = x
        my_array[1] = y
        my_array[2] = z
        return my_array
