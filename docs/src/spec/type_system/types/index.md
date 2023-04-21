# Types

Every variable, item, and value in a Fe program has a type. The _ of a
*value* defines the interpretation of the memory holding it and the operations
that may be performed on the value.

Built-in types are tightly integrated into the language, in nontrivial ways
that are not possible to emulate in user-defined types. User-defined types have
limited capabilities.

The list of types is:

* Data types
    * Base types:
        * [Boolean] — `true` or `false`
        * [Address] - Ethereum address
        * [Numeric] — integer
    * Reference types:
        * Sequence types
            * [Tuple]
            * [Array]
            * [String]
            * [Struct]
            * [Enum]
        * [Map]
* Other types:
    * [Unit]
    * [Contract]
    * [Function]


[Array]: array.md
[Boolean]:boolean.md
[Address]:address.md
[Numeric]:numeric.md
[Tuple]: tuple.md
[String]: string.md
[Struct]: struct.md
[Enum]: enum.md
[Map]: map.md
[Unit]: unit.md
[Contract]: contract.md
[Function]: function.md