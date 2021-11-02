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