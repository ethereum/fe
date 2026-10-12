Creating a contract now counts as using its bodies for dependency trust: `evm.create2<dep::C>(...)` reports `16-0015` when `C`'s `init` or a recv arm runs unsafe code of an untrusted dependency.
