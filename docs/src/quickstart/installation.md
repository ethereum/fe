## Installation

At this point Fe is only available for **Linux** and **MacOS**.

> Note: If you happen to be a Windows developer, consider getting involved
> and help us to support the Windows platform. [Here would be a good place to start.](https://github.com/ethereum/fe/issues/62)


First run the command below to get fe compiler, and add fe compier to your PATH:

```bash
curl https://raw.githubusercontent.com/ethereum/fe/create-install-script/install.sh | sh
```

Check install successfuly 
```

> fe


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

Happy coding!!! 