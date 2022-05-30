# `pragma` statement


> **<sup>Syntax</sup>**\
> _PragmaStatement_ :\
> &nbsp;&nbsp; `pragma` _VersionRequirement_
>
> _VersionRequirement_ :<sub>Following the semver implementation by [cargo] </sub>\


The pragma statement is denoted with the keyword `pragma`. Evaluating a `pragma`
statement will cause the compiler to reject compilation if the version of the compiler does not conform to the given version requirement.

An example of a `pragma` statement:

```fe,ignore
pragma ^0.1.0
```

The version requirement syntax is identical to the one that is used by cargo ([more info]).

[more info]:https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
[cargo]:https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
