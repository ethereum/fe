# Identifiers

> **<sup>Lexer:<sup>**\
> IDENTIFIER_OR_KEYWORD :\
> &nbsp;&nbsp; &nbsp;&nbsp; \[`a`-`z` `A`-`Z`\]&nbsp;\[`a`-`z` `A`-`Z` `0`-`9` `_`\]<sup>\*</sup>\
> &nbsp;&nbsp; | `_` \[`a`-`z` `A`-`Z` `0`-`9` `_`\]<sup>+</sup>
><sub>*Except a [strict] or [reserved] keyword*</sub>
>

An identifier is any nonempty ASCII string of the following form:

Either

* The first character is a letter.
* The remaining characters are alphanumeric or `_`.

Or

* The first character is `_`.
* The identifier is more than one character. `_` alone is not an identifier.
* The remaining characters are alphanumeric or `_`.

[strict]: keywords.md#strict-keywords
[reserved]: keywords.md#reserved-keywords