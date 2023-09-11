# Fe projects

A project is a collection of files containing Fe code and configuration data. Often, smart contract development can become too complex to contain all the necessary code inside a single file. In these cases, it is useful to organize your work into multiple files and directories. This allows you to group thematically linked code and seletively import the code you need when you need it.

## Creating a project

You can start a project using the `new` subcommand:

`$ fe new <my_project>`

This will generate a template project containing the following:

- A `src` directory containing two .fe files.
- A `fe.toml` manifest with basic project info and some local project imports.

## Manifest

The `fe.toml` file is known as a manifest. The manifest is written in [TOML](https://toml.io/) format. The purpose of this file is to provide all the metadata that is required for the project to compile. The file begins with definitions for the project name and version, then the project dependencies are listed under a heading `[dependencies]`. Dependencies are files in the local filesystem that are required for your project to run.

For example:

```toml
name="my-project"
version = "1.0"

[dependencies]
dependency_1 = "../lib"
```

You can also specify which version of a particular dependency you want to use, using curly braces:

```toml
name="my-project"
version = "1.0"

[dependencies]
dependency_1 = {path = "../lib", version = "1.0"}
```

## Project modes

There are two project modes: `main` and `lib`. 

Main projects can import libraries and have code output. 

Libraries on the other hand cannot import main projects and do not have code outputs. Their purpose is to be imported into other projects.

The mode of a project is determined automatically by the presence of either `src/main.fe` or  `src/lib.fe`.

## Importing

You can import code from external files with the following syntax:

```fe
use utils::get_42
```

This will import the `get_42` function from the file `utils.fe`.

You can also import using a custom name/alias:

```fe
use utils::get_42 as get_42
```

## Tests

The templates created using `fe new` include a simple test demonstrating the test syntax.

To write a unit test, create a function with a name beginning with `test_`. The function should instantiate your contract and call the contract function you want to test. You can use `assert` to check that the returned value matches an expected value.

For example, to test the `say_hello` function on `Contract` whioch is expected to return the string `"hello"`:

```fe
fn test_contract(mut ctx: Context) {
    let contract: Contract = Contract.create(ctx, 0)
    assert main.say_hello() == "hello"
}
```

You can run all the tests in a project by running the following command:

```sh
fe test <project-root>
```

You will receive test results directly to the console.

## Running your project


Once you have created a project, you can run the usual Fe CLI subcommands against the project path.