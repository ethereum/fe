### Types

Every variable, item, and value in a Fe program has a type. The _type_ of a
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
            * [Bytes]
            * [String]
            * [Struct]
            * [Enum]
        * [HashMap]
* Other types:
    * [Event]
    * [Contract]
    * [Function]