# Vendored Dependencies

## tree-sitter.js + tree-sitter.wasm

The web-tree-sitter runtime (Emscripten build of the tree-sitter C library).

- **Source:** https://github.com/tree-sitter/tree-sitter
- **License:** MIT (see LICENSE-tree-sitter)
- **Version:** 0.24.5

These files are the Emscripten/WASM build of tree-sitter's C core, used for
client-side syntax highlighting in the browser. They are not built from this
repository.

## tree-sitter-fe.wasm

The Fe language grammar compiled to WASM. Built from `crates/tree-sitter-fe/`.
To rebuild after grammar changes:

```sh
cd crates/tree-sitter-fe
npm ci --ignore-scripts
npm rebuild tree-sitter-cli
npx tree-sitter generate --abi=14
cd ../..
cargo check --locked -p fe-web
```

Requires Node and `emcc` (Emscripten). The `fe-web` build script runs the
pinned tree-sitter CLI to rebuild the WASM and updates its `.inputs` stamp.
Commit both `tree-sitter-fe.wasm` and `tree-sitter-fe.wasm.inputs`; changing
only the stamp leaves the browser using a parser for a different grammar.
