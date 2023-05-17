# Fe LSP client VSCode extension
This needs a better name.

## Development/Debugging
Before running the VSCode extension, ensure the language server is built by following the instructions in [the `language-server` crate's README.md](../../README.md).

Once you've built the language server binary, run:
```bash
npm install
npm run build
```

Then open this directory in VSCode and press `F5` to run the extension and start the debugger.

A new VSCode window will open with the Fe test fixtures directory and this extension loaded.

## Building releases
### TODO