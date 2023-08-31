## Installation

At this point Fe is only available for **Linux** and **MacOS**.

> Note: If you happen to be a Windows developer, consider getting involved
> and help us to support the Windows platform. [Here would be a good place to start.](https://github.com/ethereum/fe/issues/62)

### Download the compiler

At this point Fe is only distributed via a single executable file linked from the [home page](https://fe-lang.org). In the future we will make sure it can be installed through popular package managers such as `apt` or `homebrew`.

Depending on your operating system, the file that you download is either named `fe_amd64` or `fe_mac`.

> Note: We will rename the file to `fe` and assume that name for the rest of the guide. In the future when Fe can be installed via other mechanisms we can assume `fe` to become the canonical command name.

### Add permission to execute

In order to be able to execute the Fe compiler we will have to make the file *executable*. This can be done by navigating to the directory where the file is located and executing `chmod + x <filename>` (e.g. `chmod +x fe`).

After we have set the proper permissions we should be able to run `./fe` and an output that should be roughly comparable to:

```
fe 0.21.0-alpha
The Fe Developers <snakecharmers@ethereum.org>
An implementation of the Fe smart contract language

USAGE:
    fe_amd64_latest <SUBCOMMAND>

OPTIONS:
    -h, --help       Print help information
    -V, --version    Print version information

SUBCOMMANDS:
    build    Build the current project
    check    Analyze the current project and report errors, but don't build artifacts
    help     Print this message or the help of the given subcommand(s)
    new      Create new fe project
```

### Editor support & Syntax highlighting

Fe is a new language and editor support is still in its early days. However, basic syntax highlighting is available for Visual Studio Code via this [VS Code extension](https://marketplace.visualstudio.com/items?itemName=fe-lang.code-ve).

In Visual Studio Code open the extension sidebar (Ctrl-Shift-P / Cmd-Shift-P, then "Install Extension") and search for `fe-lang.code-ve`. Click on the extension and then click on the `Install` button.

We are currently working on a Language Server Protocol (LSP), which in the future will enable more advanced editor features such as code completion, go-to definition and refactoring.