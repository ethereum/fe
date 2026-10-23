A glob import that brings in a module named `core` or `std` no longer hides the prelude. With `use foo::*` and a module `foo::std`, `Address` and the other prelude names weren't found.
