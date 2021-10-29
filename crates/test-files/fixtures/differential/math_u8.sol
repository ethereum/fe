contract Foo {

  function add(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 + val2;
  }

  function subtract(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 - val2;
  }

  function multiply(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 * val2;
  }

  function divide(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 / val2;
  }

  function pow(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 ** val2;
  }

  function modulo(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 % val2;
  }

  function leftshift(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 << val2;
  }

  function rightshift(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 >> val2;
  }

  function invert(uint8 val1) public pure returns (uint8){
    return ~val1;
  }

  function bit_and(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 & val2;
  }

  function bit_or(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 | val2;
  }

  function bit_xor(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 ^ val2;
  }

  function cast1(uint8 val1) public pure returns (int8){
    return int8(val1);
  }

  function cast2(uint8 val1) public pure returns (int16){
    return int16(int8(val1));
  }

  function cast3(uint8 val1) public pure returns (int16){
    return int16(uint16(val1));
  }

  function order_of_operation(uint8 val1, uint8 val2) public pure returns (uint8){
    return val1 - val2 * (val1 + val2 / 4 * val1 - val2**val1) + val1;
  }

  function sqrt(uint val) public pure returns (uint z) {
      if (val > 3) {
          z = val;
          uint x = val / 2 + 1;
          while (x < z) {
              z = x;
              x = (val / x + x) / 2;
          }
      } else if (val != 0) {
          z = 1;
      }
  }
}