# `pass` statement


> **<sup>Syntax</sup>**\
> _PassStatement_ :\
> &nbsp;&nbsp; `pass`

The `pass` statement is a *null* operation, it has no effect on code execution. It is useful as a placeholder when a statement is required syntactically, but no code needs to be executed.

Example of `pass` being used to define an empty contract body.

```python
contract Foo:
    pass
```

Example of `pass` being used to define an empty function body.

```python
contract Foo:

    pub fn bar():
        pass
```

Example of `pass` being used to define an code block after an `if` statement.

```python
contract Foo:

    pub fn bar(condition: bool):
        if condition:
            pass
```
