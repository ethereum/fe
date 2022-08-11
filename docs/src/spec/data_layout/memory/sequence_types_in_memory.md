# Sequence types in memory

Sequence type values may exceed the 256-bit stack slot size, so we store them in memory and
reference them using pointers kept on the stack.

Example:

```fe
fn f() {
    let foo: Array<u256, 100> = [0; 100] // foo is a pointer that references 100 * 256 bits in memory.
}
```

To find an element inside of a sequence type, the relative location of the element is added to the
given pointer.
