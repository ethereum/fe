# Visibility and Privacy

These two terms are often used interchangeably, and what they are attempting to convey is the answer to the question "Can this item be used at this location?"

Fe knows two different types of visibility for functions and state variables: `public` and `private`. Visibility of `private` is the default and is used if no other visibility is specified.

**Public:** External functions are part of the contract interface, which means they can be called from other contracts and via transactions.

**Private:** Those functions and state variables can only be accessed internally from within the same contract. This is the default visibility.

For example, this is a function that can be called externally from a transaction:

```python
pub fn answer_to_life_the_universe_and_everything() -> u256:
    return 42
```

Top-level definitions in a Fe source file can also be specified as `pub` if the file exists within the context of an Ingot. Declaring a definition as `pub` enables other modules within an Ingot to `use` the definition.

For example, given an Ingot with the following structure:

```
example_ingot
└── src
    ├── ding
    │   └── dong.fe
    └── main.fe
```

With `ding/dong.fe` having the following contents:

```python
pub struct Dyng:
  my_address: address
  my_u256: u256
  my_i8: i8
```


Then `main.fe` can use the `Dyng` struct since it is `pub`-qualified:

```python
use ding::{dong::Dyng}

contract Foo:
 pub fn get_my_dyng() -> Dyng:
        return Dyng(
            my_address=address(8),
            my_u256=42,
            my_i8=-1
        )
```
