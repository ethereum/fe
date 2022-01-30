# Data Layout

There are three places where data can be stored on the EVM:

- **stack**: 256-bit values placed on the stack that are loaded using `DUP` operations.
- **storage**: 256-bit address space where 256-bit values can be stored. Accessing higher
storage slots does not increase gas cost.
- **memory**: 256-bit address space where 256-bit values can be stored. Accessing higher
memory slots increases gas cost.

Each [data type] can be stored in these locations. How data is stored is
described in this section.

[data type]: ../type_system/types/index.md