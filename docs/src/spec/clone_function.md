# The `clone` function

Reference type values in memory can be cloned using the `clone` function.

Example:

```python
# with clone
foo: Array<u256, 10> = bar.clone() # `foo` points to a new segment of memory
assert foo[1] == bar[1] 
foo[1] = 42
assert foo[1] != bar[1] # modifying `foo` does not modify bar

# without clone
foo: Array<u256, 10> = bar # `foo` and `bar` point to the same segment of memory
assert foo[1] == bar[1]
foo[1] = 42
assert foo[1] == bar[1] # modifying `foo` also modifies `bar`
```
