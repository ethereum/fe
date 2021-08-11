# Constant size values in storage

Storage pointers for constant size values are determined at compile time.

Example:

```python
# contract scope
foo: u256 # foo is assigned a static pointer by the compiler
```

The value of a base type in storage is found by simply loading the value from storage at the
given pointer.

To find an element inside of a sequence type, the relative location of the element is added to the
given pointer.