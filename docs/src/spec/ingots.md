# Ingots

An Ingot is a tree of modules that produces a library or a set of contracts.

Below is an example of an Ingot file tree along with the source of its modules.

```
`-- basic_ingot
    `-- src
        |-- bar
        |   `-- baz.fe
        |-- bing.fe
        `-- main.fe
```

We define a struct inside of `src/bar/baz.fe`:

```
pub struct Baz:
    pub my_bool: bool
    pub my_u256: u256
```

We defined a contract inside of `src/bing.fe`:

```
pub contract BingContract:

   pub fn add(x: u256, y: u256) -> u256:
       return x + y
```

We define another contract inside of `src/main.fe` that uses the items defined above:

```
use bar::baz::Baz
use bing::BingContract

contract Foo:
    pub fn get_my_baz() -> Baz:
        return Baz(my_bool=true, my_u256=26)

    pub fn create_bing_contract() -> u256:
        let bing_contract: BingContract = BingContract.create(0)
        return bing_contract.add(40, 50)
```

The contract `Foo` is compiled into bytecode and written to the output directory by running `fe path/to/ingot`.
