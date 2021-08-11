# Tokens

## Significant whitespace

In Fe whitespace is not just used to guide the human eye but it also carries meaning. The rules are essentially [borrowed from Python](https://docs.python.org/2.0/ref/indentation.html).

### `NEWLINE`

A token that represents a new line.

### `INDENT`

A token that represents adding one level of indentation.

### `DEDENT`

A token that represents removing one level of indentation

## Literals

A literal is an expression consisting of a single token, rather than a sequence
of tokens, that immediately and directly denotes the value it evaluates to,
rather than referring to it by name or some other evaluation rule. A literal is
a form of constant expression, so is evaluated (primarily) at compile time.

### Examples

#### Strings

|                                              | Example         | Characters  | Escapes             |
|----------------------------------------------|-----------------|-------------|---------------------|
| [String](#string-literals)                   | `"hello"`       | ASCII subset | [Quote](#quote-escapes) & [ASCII](#ascii-escapes) |


#### ASCII escapes

|   | Name |
|---|------|
| `\n` | Newline |
| `\r` | Carriage return |
| `\t` | Tab |
| `\\` | Backslash |

#### Quote escapes

|   | Name |
|---|------|
| `\"` | Double quote |

#### Numbers

TODO

### String literals

> **<sup>Lexer</sup>**\
> STRING_LITERAL :\
> &nbsp;&nbsp; `"` (\
> &nbsp;&nbsp; &nbsp;&nbsp; PRINTABE_ASCII_CHAR\
> &nbsp;&nbsp; &nbsp;&nbsp; | QUOTE_ESCAPE\
> &nbsp;&nbsp; &nbsp;&nbsp; | ASCII_ESCAPE\
> &nbsp;&nbsp; )<sup>\*</sup> `"`
>
> PRINTABLE_ASCII_CHAR :\
> &nbsp;&nbsp; Any ASCII character between `0x1F` and `0x7E`
>
> QUOTE_ESCAPE :\
> &nbsp;&nbsp; `\"`
>
> ASCII_ESCAPE :\
> &nbsp;&nbsp; | `\n` | `\r` | `\t` | `\\`
>


A _string literal_ is a sequence of any characters that are in the set of
printable ASCII characters as well as a set of defined escape sequences.

Line breaks are allowed in string literals.
```