# `fe-language-server`
## An LSP server for the Fe programming language
The Fe Language Server project aims to develop a robust, efficient, and feature-rich language server that enhances the development experience for Fe developers. This includes implementing a wide range of Language Server Protocol (LSP) features, improving the backend infrastructure, integrating with the VSCode extension, and thorough testing and documentation.

## Current status: 🚧 experimental 🚧

The language server is being developed against an ongoing Fe compiler rewrite, so it doesn't currently support the version of Fe in the main branch.

## Editor support
An LSP enabled VSCode Extension designed to work with this language server can be found in [the `editors/vscode` directory](./editors/vscode).

## Development
To build the language server binary, run:
```bash
cargo build
```

One straightforward way to interact with the language server during development is to follow [the instructions in the Fe VSCode extension readme](./editors/vscode/README.md).

## Progress and Roadmap
### May 2023
- [x] LSP server crate scaffolding
- [x] LSP-based VSCode extension
- [x] Hover prototype
  - printing debug information
- [x] Basic development setup and documentation
- [x] Orientation, research and study of rust-analyzer and Fe compiler
### June 2023
- [x] Diagnostics publishing prototype
  - using current deprecated diagnostics functionality
### July 2023
- [x] Syntax highlighting in the VSCode extension via Textmate grammar file
- Initial work on transitioning to the new compiler architecture
  - [x] Salsa2022 database setup for language server
  - [x] Overhaul diagnostics types to support types in new compiler crates
  - [x] Working prototype of new diagnostics functionality based on @Y-Nak's `name-resolution` branch
### August 2023
- [x] Improvements to LSP server + VSCode extension development setup
- [x] Improved `document/didChange` handling
- [x] Flexible LSP server global logging support using the `log` crate
- [x] Code style cleanup and linting
- Further work on transitioning to the new compiler overhaul
  - [x] Go-to definition prototype working for standalone .fe files; based on @Y-Nak's compiler rewrite
    - [x] Name resolution from cursor position
    - [x] Snap tests and unit tests for go-to definition functionality
  - [x] Error handling improvements
    - At the time, the rewritten name resolution only supported type resolution in the early resolution stage, but the data types and LSP server implementation should seamlessly support progress on the new compiler's name resolution functionality without many changes
### September 2023
- [x] Research and study of rust-analyzer's file synchronization functionality
- Go-to definition cleanup
- Initial "workspace" functionality
  - a data structure and API for generating, caching and synchronizing necessary salsa inputs to support language server functionality
  - should support file synchronization via filesystem watching and LSP file change events alike
  - [x] Sketch of initial workspace cache data structures
    - Featuring a prefix tree based lookup for salsa inputs representing files, ingots and top-modules
    - Supporting local, external and standalone ingot caches.
  - [x] Upgraded go-to definition functionality to work with "local" ingots containing multiple modules
  - [x] Integration of workspace data structures with existing language server data structures; slight refactor of existing architecture
  - [x] Initial workspace sync prototype and tests
- [x] Initial support for subdiagnostics
- [x] Workspace sync API brainstorming and study
### October-December 2023
- [x] Improve the internal synchronization API for the "workspace" data structure to support an explicit update step
- [x] Integrated the "workspace" data structure with LSP events and a rudimentary filesystem watcher
### January-March 2024
- Initial prototype supporting concurrency/task management with `tower-lsp`
### July-November 2024
- Big architecture overhaul, switched from `tower-lsp` to `async-lsp`
    - much better control over concurrency and event processing order, working around issues described [here](https://github.com/ebkalderon/tower-lsp/issues/284)
    - custom actor implementation supporting lock-free language server state
    - handlers with thread-local futures
- Optional stream-based event handling support
- Multithreaded tracing and logging improvements
- Debounced, multithreaded diagnostics
- TCP support; support for multiple clients
- Organize and prepare server architecture for a variety of LSP features
- Implement a task queue system for long-running tasks
- Enhance hover feature to return useful information and documentation
- Initial work on VSCode extension release pipeline

### To-do and tentative roadmap

#### Server improvements
- [ ] Implement progress feedback notifications
- [ ] Implement configurable options for the language server
  - Diagnostics configuration
  - Inlay configuration

#### LSP Features
- [ ] Expand go-to feature to support variables, functions, implementations and other references
- [ ] Support go-to definitions for Fe standard library
- [ ] Improve diagnostics implementation and expand tests
- [ ] Autofill and completions
- [ ] Import completion
- [ ] Refactoring symbol names and file names
- [ ] Inlay hints and annotations
- [ ] File system navigation
- [ ] Show syntax tree, HIR, etc.
- [ ] Syntactic and semantic highlighting

#### Integration with VSCode and Zed
- [x] Configurable LSP binary
- [ ] VSCode extension release pipeline
  - [x] build setup
  - [ ] release pipeline github actions
- [ ] Implement configuration and options shortcuts in the VSCode extension
- [ ] Investigate support for running tests/proofs from the VSCode extension
- [ ] Zed integration

#### Testing and Documentation
- [ ] Better test coverage
- [ ] Document the code, architecture, rationale for decisions, risks, and roadmap
- [ ] Create a catalog of examples that activate various LSP features

#### Research and Miscellaneous
- [ ] Look into possibility of supporting proof and Fe test functionality in the language server
- [ ] Investigate possible use cases for extending the salsa architecture into the language server more directly, e.g. performance improvements
- [ ] View bytecode for selected code
- [ ] Code actions for smart contract development
- [ ] Would any LSP features be useful for plugging into LLM-based tools?  E.g. analytics or descriptions of the codebase?
- [ ] Security review and documentation
