error StringError(string reason);
error U256Error(uint256 reason);
error I256Error(int256 reason);
error U8Error(uint8 reason);
error TwoU256Error(uint256 reason, uint256 reason2);

struct Bag {
  uint256 val1;
  int256 val2;
  bool val3;
}

error StructError(Bag data);

contract Foo {

  function revert_bare() public pure {
    revert();
  }

  function revert_me() public pure {
    revert("Not enough Ether provided.");
  }

  function revert_with_long_string() public pure {
    revert("A muuuuuch longer reason string that consumes multiple words");
  }

  function revert_with_empty_string() public pure {
    revert("");
  }

  function revert_with_string_error() public pure {
    revert StringError("Not enough Ether provided.");
  }

  function revert_with_u256_error() public pure {
    revert U256Error(100);
  }

  function revert_with_i256_error() public pure {
    revert I256Error(-100);
  }

  function revert_with_u8_error() public pure {
    revert U8Error(100);
  }

  function revert_with_two_u256_error() public pure {
    revert TwoU256Error(100, 100);
  }

  function revert_with_struct_error() public pure {
    revert StructError(Bag ({ val1: 100, val2: -100, val3: true }));
  }

  function panic_divide_by_zero(uint256 val1, uint256 val2) public pure {
    val1 / val2;
  }

  function panic_assert() public pure {
    assert(false);
  }

}