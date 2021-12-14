struct MyStruct {
    uint256 my_num;
    uint8 my_num2;
    bool my_bool;
    address my_addr;
    int8 my_num3;
}

contract Foo {

  MyStruct data;
  int8[1] items;
  uint256[100] more_items;
  string some_string;

  function set_data(MyStruct memory val) public {
    data = val;
  }

  function get_data() public view returns (MyStruct memory) {
    return data;
  }

  function set_string(string memory val) public {
    some_string = val;
  }

  function get_string() public view returns (string memory) {
    return some_string;
  }

  function set_item(uint256 index, int8 value) public {
    items[index] = value;
  }

  function get_items() public view returns (int8[1] memory) {
    return items;
  }

  function set_range(uint256 from, uint256 to) public {
    uint256 index = from;
    uint256 counter = 0;
    while (index < to) {
      more_items[index] = more_items[index] + block.number;
      index += 1;
      counter += 1;

      if (counter > 20) {
        break;
      }
    }
  }

  function get_range() public view returns (uint256[100] memory) {
    return more_items;
  }
}