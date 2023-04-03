# Functions

> **<sup>Syntax</sup>**\
> _Function_ :\
> &nbsp;&nbsp; _FunctionQualifiers_ `fn` [IDENTIFIER]\
> &nbsp;&nbsp; &nbsp;&nbsp; `(` _FunctionParameters_<sup>?</sup> `)`\
> &nbsp;&nbsp; &nbsp;&nbsp; _FunctionReturnType_<sup>?</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; `{`\
> &nbsp;&nbsp; &nbsp;&nbsp; _FunctionStatements_<sup>*</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; `}`
>
> _FunctionQualifiers_ :\
> &nbsp;&nbsp; `pub`<sup>?</sup>
>
> _FunctionStatements_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;  [_ReturnStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_VariableDeclarationStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_AssignStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_AugmentedAssignStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_ForStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_WhileStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_IfStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_AssertStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_BreakStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_ContinueStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_RevertStatement_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Expression_]\
>
> _FunctionParameters_ :\
> &nbsp;&nbsp;  `self`<sup>?</sup> | `self,`<sup>?</sup>   _FunctionParam_ (`,` _FunctionParam_)<sup>\*</sup> `,`<sup>?</sup>
>
> _FunctionParam_ :\
> &nbsp;&nbsp; _FunctionParamLabel_<sup>?</sup> [IDENTIFIER] `:` [_Types_]
>
> _FunctionParamLabel_ :\
> &nbsp;&nbsp; _ | [IDENTIFIER]
>
> _FunctionReturnType_ :\
> &nbsp;&nbsp; `->` [_Types_]


A _function_ definition consists of name and code block along with an optional
list of parameters and return value. Functions are declared with the
keyword `fn`. Functions may declare a set of *input* parameters,
through which the caller passes arguments into the function, and
the *output* [*type*][_Types_] of the value the function will return to its caller
on completion.

When referred to, a _function_ yields a first-class *value* of the
corresponding zero-sized [*function type*][_FunctionTypes_], which
when called evaluates to a direct call to the function.

A function header ends with a colon (`:`) after which the function body begins.

For example, this is a simple function:

```fe
fn add(x: u256, y: u256) -> u256 {
    return x + y
}
```

Functions can be defined inside of a contract, inside of a struct, or at the
"top level" of a module (that is, not nested within another item).

Example:

```fe
fn add(_ x: u256, _ y: u256) -> u256 {
    return x + y
}

contract CoolCoin {
    balance: Map<address, u256>

    fn transfer(mut self, from sender: address, to recipient: address, value: u256) -> bool {
        if self.balance[sender] < value {
            return false
        }
        self.balance[sender] -= value
        self.balance[recipient] += value
        return true
    }
    pub fn demo(mut self) {
        let ann: address = 0xaa
        let bob: address = 0xbb
        self.balance[ann] = 100

        let bonus: u256 = 2
        let value: u256 = add(10, bonus)
        let ok: bool = self.transfer(from: ann, to: bob, value)
    }
}
```

Function parameters have optional labels. When a function is called, the
arguments must be labeled and provided in the order specified in the
function definition.

The label of a parameter defaults to the parameter name; a different label
can be specified by adding an explicit label prior to the parameter name.
For example:
```fe
fn encrypt(msg cleartext: u256, key: u256) -> u256 {
    return cleartext ^ key
}

fn demo() {
    let out: u256 = encrypt(msg: 0xdecafbad, key: 0xfefefefe)
}
```

Here, the first parameter of the `encrypt` function has the label `msg`,
which is used when calling the function, while the parameter name is
`cleartext`, which is used inside the function body. The parameter name
is an implementation detail of the function, and can be changed without
modifying any function calls, as long as the label remains the same.

When calling a function, a label can be omitted when the argument is
a variable with a name that matches the parameter label. Example:

```fe,ignore
let msg: u256 = 0xdecafbad
let cyf: u256 = encrypt(msg, key: 0x1234)
```

A parameter can also be specified to have no label, by using `_` in place of a
label in the function definition. In this case, when calling the function, the
corresponding argument must not be labeled. Example:
```fe
fn add(_ x: u256, _ y: u256) -> u256 {
    return x + y
}
fn demo() {
    let sum: u256 = add(16, 32)
}
```

Functions defined inside of a contract or struct may take `self` as a
parameter. This gives the function the ability to read and write contract
storage or struct fields, respectively. If a function takes `self`
as a parameter, the function must be called via `self`. For example:

```fe,ignore
let ok: bool = self.transfer(from, to, value)
```

[NEWLINE]: ../lexical_structure/tokens.md#newline
[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Types_]: ../type_system/types/index.md
[_FunctionTypes_]: ../type_system/types/function.md

[_ReturnStatement_]: ../statements/return.md
[_VariableDeclarationStatement_]: ../statements/let.md
[_AssignStatement_]: ../statements/assign.md
[_AugmentedAssignStatement_]: ../statements/augassign.md
[_ForStatement_]: ../statements/for.md
[_WhileStatement_]: ../statements/for.md
[_IfStatement_]: ../statements/if.md
[_AssertStatement_]: ../statements/assert.md
[_BreakStatement_]: ../statements/break.md
[_ContinueStatement_]: ../statements/continue.md
[_RevertStatement_]: ../statements/revert.md
[_Expression_]: ../expressions/index.md
