# Traits

> **<sup>Syntax</sup>**\
> _Trait_ :\
> &nbsp;&nbsp; `trait` [IDENTIFIER] `{`\
> &nbsp;&nbsp; &nbsp;&nbsp; _TraitMethod_<sup>\*</sup>\
> &nbsp;&nbsp; `}`
>
>
> _TraitMethod_ :\
> &nbsp;&nbsp; `fn` [IDENTIFIER]\
> &nbsp;&nbsp; &nbsp;&nbsp; `(` _FunctionParameters_<sup>?</sup> `)`\
> &nbsp;&nbsp; &nbsp;&nbsp; _FunctionReturnType_<sup>?</sup> `;`\

A _trait_ is a collection of function signatures that a type can implement. Traits are implemented for specific types through separate implementations. A type can implement a trait by providing a function body for each of the trait's functions. Traits can be used as type bounds for generic functions to restrict the types that can be used with the function.


All traits define an implicit type parameter `Self` that refers to "the type that is implementing this interface". 


Example of the `Min` trait from Fe's standard library:
```fe
pub trait Min {
  fn min() -> Self;
}
```

Example of the `i8` type implementing the `Min` trait:
```fe
impl Min for i8 {
  fn min() -> Self {
    return -128
  }
}
```

Example of a function restricting a generic parameter to types implementing the `Compute` trait:
```fe
pub trait Compute {
  fn compute(self) -> u256;
}

struct Example {
  fn do_something<T: Compute>(val: T) -> u256 {
    return val.compute()
  }
}
```


[NEWLINE]: ../lexical_structure/tokens.md#newline
[IDENTIFIER]: ../lexical_structure/identifiers.md
[_FunctionParameters_]: ./functions.md#function_parameters
[_FunctionReturnType_]: ./functions.md#function_return_type
