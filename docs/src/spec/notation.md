# Notation

## Grammar

The following notations are used by the *Lexer* and *Syntax* grammar snippets:

| Notation          | Examples                      | Meaning                                   |
|-------------------|-------------------------------|-------------------------------------------|
| CAPITAL           | KW_IF                         | A token produced by the lexer             |
| _ItalicCamelCase_ | _Item_                        | A syntactical production                  |
| `string`          | `x`, `while`, `*`             | The exact character(s)                    |
| \\x               | \\n, \\r, \\t, \\0            | The character represented by this escape  |
| x<sup>?</sup>     | `pub`<sup>?</sup>             | An optional item                          |
| x<sup>\*</sup>    | _OuterAttribute_<sup>\*</sup> | 0 or more of x                            |
| x<sup>+</sup>     |  _MacroMatch_<sup>+</sup>     | 1 or more of x                            |
| x<sup>a..b</sup>  | HEX_DIGIT<sup>1..6</sup>      | a to b repetitions of x                   |
| \|                | `u8` \| `u16`, Block \| Item  | Either one or another                     |
| [ ]               | \[`b` `B`\]                     | Any of the characters listed              |
| \[ - \]             | \[`a`-`z`\]                     | Any of the characters in the range        |
| ~[ ]              | ~\[`b` `B`\]                    | Any characters, except those listed       |
| ~`string`         | ~`\n`, ~`*/`                  | Any characters, except this sequence      |
| ( )               | (`,` _Parameter_)             | Groups items                              |

