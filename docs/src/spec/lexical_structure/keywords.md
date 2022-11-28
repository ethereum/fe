# Keywords

Fe divides keywords into two categories:

* [strict](#strict-keywords)
* [reserved](#reserved-keywords)

## Strict keywords

These keywords can only be used in their correct contexts. They cannot
be used as the [identifiers](./identifiers.md).

> **Lexer:**\
> KW_AS             : `as`\
> KW_BREAK          : `break`\
> KW_CONST          : `const`\
> KW_CONTINUE       : `continue`\
> KW_CONST          : `contract`\
> KW_FN             : `fn`\
> KW_ELSE           : `else`\
> KW_ENUM           : `enum`\
> KW_EVENT          : `event`\
> KW_FALSE          : `false`\
> KW_FOR            : `for`\
> KW_IDX             : `idx`\
> KW_IF             : `if`\
> KW_IN             : `in`\
> KW_LET            : `let`\
> KW_MATCH          : `match`\
> KW_MUT            : `mut`\
> KW_NONPAYABLE     : `nonpayable`\
> KW_PAYABLE        : `payable`\
> KW_PUB            : `pub`\
> KW_RETURN         : `return`\
> KW_REVERT         : `revert`\
> KW_SELFVALUE      : `self`\
> KW_STRUCT         : `struct`\
> KW_TRUE           : `true`\
> KW_USE            : `use`\
> KW_WHILE          : `while` \
> KW_ADDRESS        : `address`


## Reserved keywords

These keywords aren't used yet, but they are reserved for future use. They have
the same restrictions as strict keywords. The reasoning behind this is to make
current programs forward compatible with future versions of Fe by forbidding
them to use these keywords.

> **Lexer:**\
> KW_ABSTRACT       : `abstract`\
> KW_ASYNC          : `async`\
> KW_AWAIT          : `await`\
> KW_DO             : `do`\
> KW_EXTERNAL       : `external`\
> KW_FINAL          : `final`\
> KW_IMPL           : `impl`\
> KW_MACRO          : `macro`\
> KW_OVERRIDE       : `override`\
> KW_PURE           : `pure`\
> KW_SELFTYPE       : `Self`\
> KW_STATIC         : `static`\
> KW_SUPER          : `super`\
> KW_TRAIT          : `trait`\
> KW_TYPE           : `type`\
> KW_TYPEOF         : `typeof`\
> KW_VIEW           : `view`\
> KW_VIRTUAL        : `virtual`\
> KW_WHERE          : `where`\
> KW_YIELD          : `yield`
