# Installation

At this point Fe is available for **Linux** and **MacOS** natively but can also be installed on **Windows** via [WSL](https://learn.microsoft.com/en-us/windows/wsl/about).

> Note: If you happen to be a Windows developer, consider getting involved
> and help us to support the Windows platform natively. [Here would be a good place to start.](https://github.com/ethereum/fe/issues/62)

On a computer with MacOS and an ARM chip, you need to install Rosetta, Apple's x86-to-ARM translator, to be able to run the executable. 
```
/usr/sbin/softwareupdate --install-rosetta --agree-to-license
```

## Package managers

Fe can be installed from [Homebrew](https://brew.sh/). Homebrew is available for Mac, Linux and Windows (via [WSL](https://learn.microsoft.com/en-us/windows/wsl/about)). The following command installs Fe and exposes it as `fe` without any further configuration necessary:

```sh
brew install fe-lang/tap/fe
```

## Download the compiler

Fe is distributed via a single executable file linked from the [home page](https://fe-lang.org). In the future we will make sure it can be installed through a variety of popular package managers such as `apt`.

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


## Building from source

You can also build Fe from the source code provided in our Github [repository](https://github.com/ethereum/fe). To do this you will need to have [Rust](https://www.rust-lang.org/tools/install) installed. Then, clone the github repository using:

```sh
git clone https://github.com/ethereum/fe.git
```

Depending on your environment you may need to install some additional packages before building the Fe binary, specifically `libboost-all-dev`, `libclang` and `cmake`. For example, on a Linux system:

```sh
sudo apt-get update &&\
 apt-get install libboost-all-dev &&\
 apt-get install libclang-dev &&\
 apt-get install cmake
```

Navigate to the folder containing the Fe source code.

```sh
cd fe
```

Now, use Rust to build the Fe binary. To run Fe, you need to build using `solc-backend`.

```sh
cargo build -r --feature solc-backend
```

You will now find your Fe binary in `/target/release`. Check the build with:

```sh
./target/release/fe --version
```

If everything worked, you should see the Fe version printed to the terminal:

```sh
fe 0.24.0
```

You can run the built-in tests using:

```sh
cargo test --workspace --features solc-backend
```


## Editor support & Syntax highlighting

Fe is a new language and editor support is still in its early days. However, basic syntax highlighting is available for Visual Studio Code via this [VS Code extension](https://marketplace.visualstudio.com/items?itemName=fe-lang.code-ve).

In Visual Studio Code open the extension sidebar (Ctrl-Shift-P / Cmd-Shift-P, then "Install Extension") and search for `fe-lang.code-ve`. Click on the extension and then click on the `Install` button.

We are currently working on a Language Server Protocol (LSP), which in the future will enable more advanced editor features such as code completion, go-to definition and refactoring.
