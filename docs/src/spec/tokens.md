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

| [Number literals](#number-literals)`*` | Example |
|----------------------------------------|---------|
| Decimal integer | `98_222` |
| Hex integer | `0xff` |
| Octal integer | `0o77` |
| Binary integer | `0b1111_0000` |

`*` All number literals allow `_` as a visual separator: `1_234`

### String literals

> **<sup>Lexer</sup>**\
> STRING_LITERAL :\
> &nbsp;&nbsp; `"` (\
> &nbsp;&nbsp; &nbsp;&nbsp; PRINTABLE_ASCII_CHAR\
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


### Integer literals

> **<sup>Lexer</sup>**\
> INTEGER_LITERAL :\
> &nbsp;&nbsp; ( DEC_LITERAL | BIN_LITERAL | OCT_LITERAL | HEX_LITERAL )
>
> DEC_LITERAL :\
> &nbsp;&nbsp; DEC_DIGIT (DEC_DIGIT|`_`)<sup>\*</sup>
>
> BIN_LITERAL :\
> &nbsp;&nbsp; `0b` (BIN_DIGIT|`_`)<sup>\*</sup> BIN_DIGIT (BIN_DIGIT|`_`)<sup>\*</sup>
>
> OCT_LITERAL :\
> &nbsp;&nbsp; `0o` (OCT_DIGIT|`_`)<sup>\*</sup> OCT_DIGIT (OCT_DIGIT|`_`)<sup>\*</sup>
>
> HEX_LITERAL :\
> &nbsp;&nbsp; `0x` (HEX_DIGIT|`_`)<sup>\*</sup> HEX_DIGIT (HEX_DIGIT|`_`)<sup>\*</sup>
>
> BIN_DIGIT : \[`0`-`1`]
>
> OCT_DIGIT : \[`0`-`7`]
>
> DEC_DIGIT : \[`0`-`9`]
>
> HEX_DIGIT : \[`0`-`9` `a`-`f` `A`-`F`]
>

An _integer literal_ has one of four forms:

* A _decimal literal_ starts with a *decimal digit* and continues with any
  mixture of *decimal digits* and _underscores_.
* A _hex literal_ starts with the character sequence `U+0030` `U+0078`
  (`0x`) and continues as any mixture (with at least one digit) of hex digits
  and underscores.
* An _octal literal_ starts with the character sequence `U+0030` `U+006F`
  (`0o`) and continues as any mixture (with at least one digit) of octal digits
  and underscores.
* A _binary literal_ starts with the character sequence `U+0030` `U+0062`
  (`0b`) and continues as any mixture (with at least one digit) of binary digits
  and underscores.



Examples of integer literals of various forms:

```python
123;                               // type u256
0xff;                              // type u256
0o70;                              // type u256
0b1111_1111_1001_0000;             // type u256
0b1111_1111_1001_0000i64;          // type u256
```

Note that the Fe syntax considers `-1` as an application of the [unary minus
operator] to an integer literal `1`, rather than a single integer literal.

[unary minus operator]: expr_unary_operators.md
