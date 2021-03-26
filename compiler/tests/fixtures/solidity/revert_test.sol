contract Foo {
  function revert_me() public pure returns(uint){
    revert("Not enough Ether provided.");
  }

  function revert_with_long_string() public pure returns(uint){
    revert("A muuuuuch longer reason string that consumes multiple words");
  }

  function revert_with_empty_string() public pure returns(uint){
    revert("");
  }
}