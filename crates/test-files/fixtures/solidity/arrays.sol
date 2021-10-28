
contract Foo {

  int8[1] data;


  function bar(int8 value) public returns(int8[1] memory) {
    data = [value];
    return data;
  }
}