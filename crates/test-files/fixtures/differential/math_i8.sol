contract Foo {

  function add(int8 val1, int8 val2) public pure returns (int8){
    return val1 + val2;
  }

  function subtract(int8 val1, int8 val2) public pure returns (int8){
    return val1 - val2;
  }

  function multiply(int8 val1, int8 val2) public pure returns (int8){
    return val1 * val2;
  }

  function divide(int8 val1, int8 val2) public pure returns (int8){
    return val1 / val2;
  }

  function modulo(int8 val1, int8 val2) public pure returns (int8){
    return val1 % val2;
  }

  function leftshift(int8 val1, uint8 val2) public pure returns (int8){
    return val1 << val2;
  }

  function rightshift(int8 val1, uint8 val2) public pure returns (int8){
    return val1 >> val2;
  }

  function invert(int8 val1) public pure returns (int8){
    return ~val1;
  }

  function cast1(int8 val1) public pure returns (uint8){
    return uint8(val1);
  }

  function cast2(int8 val1) public pure returns (uint16){
    return uint16(uint8(val1));
  }

  function cast3(int8 val1) public pure returns (uint16){
    return uint16(int16(val1));
  }

  function order_of_operation(int8 val1, int8 val2, uint8 val3) public pure returns (int8) {
    return val1 - val2 * (val1 + val2 / 4 * val1 - val2**val3) + val1;
  }

  function negate(int8 val1) public pure returns (int8){
    return -val1;
  }
}