# Attribute expressions

> **<sup>Syntax</sup>**\
> _AttributeExpression_ :\
> &nbsp;&nbsp; [_Expression_] `.` [IDENTIFIER]

An attribute expression evaluates to the location of an attribute of a [struct], [tuple] or [contract].

The syntax for an attribute expression is an expression, then a `.` and finally an identifier.


Examples:

```python
struct Point:
    x: u256
    y: u256

contract Foo:
    some_point: Point
    some_tuple: (bool, u256)

    fn get_point() -> Point:
        return Point(x=100, y=500)

    pub fn baz(some_point: Point, some_tuple: (bool, u256)):
        # Different examples of attribute expressions
        let bool_1: bool = some_tuple.item0
        let x1: u256 = some_point.x
        let point1: u256 = get_point().x
        let point2: u256 = some_point.x

```

[_Expression_]: expressions.md
[expression]: expressions.md
[IDENTIFIER]: identifiers.md
[tuple]: tuple_types.md
[struct]: structs.md
[contract]: contracts.md
[attribute expression]: expr_attribute.md