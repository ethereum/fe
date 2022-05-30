# The `to_mem` function

Reference type values can be copied from storage and into memory using the `to_mem` function.

Example:

```fe,ignore
let my_array_var: Array<u256, 10> = self.my_array_field.to_mem()
```
