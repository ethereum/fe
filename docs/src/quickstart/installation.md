## Installation

At this point Fe is only available for **Linux** and **MacOS**.

> Note: If you happen to be a Windows developer, consider getting involved
> and help us to support the Windows platform. [Here would be a good place to start.](https://github.com/ethereum/fe/issues/62)

### Download the compiler

At this point Fe is only distributed via a single executable file linked from the [home page](../../). In the future we will make sure it can be installed through popular package managers such as `apt` or `homebrew`.

Depending on your operating system, the file that you download is either named `fe_amd64` or `fe_mac`.

> Note: We will rename the file to `fe` and assume that name for the rest of the guide. In the future when Fe can be installed via other mechanisms we can assume `fe` to become the canonical command name.

### Add permission to execute

In order to be able to execute the Fe compiler we will have to make the file *executable*. This can be done by navigating to the directory where the file is located and executing `chmod + x <filename>` (e.g. `chmod +x fe`).

After we have set the proper permissions we should be able to run `./fe_amd64 --help` and an output that should be roughly compareable to:

```
Fe 0.4.0-alpha
Compiler for the Fe language

USAGE:
    fe_amd64 [FLAGS] [OPTIONS] <input>

FLAGS:
    -h, --help         Prints help information
        --overwrite    Overwrite contents of output directory`
    -V, --version      Prints version information

OPTIONS:
    -e, --emit <emit>                Comma separated compile targets e.g. -e=bytecode,yul [default: abi,bytecode]
                                     [possible values: abi, bytecode, ast, tokens, yul, loweredAst]
        --optimize <optimize>        Whether the Yul optimizer should be used or not e.g. --optimize=false [default: true]
    -o, --output-dir <output-dir>    The directory to store the compiler output e.g /tmp/output [default: output]

ARGS:
    <input>    The input source file to use e.g erc20.fe
```