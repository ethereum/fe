# Stack

The following can be stored on the stack:

- base type values
- pointers to reference type values

The size of each value stored on the stack must not exceed 256 bits. Since all base types are less
than or equal to 256 bits in size, we store them on the stack. Pointers to values stored in memory or storage may also be stored on the stack.

Example:

```python
# function scope
foo: u256 = 42 # foo is stored on the stack
bar: Array<u256, 100> # bar is a memory pointer stored on the stack
```
