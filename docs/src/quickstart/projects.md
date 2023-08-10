## Creating a project

If you would like to write Fe code across multiple files and import libraries, you should create a Fe project.

A template project can be created using the `new` subcommand.

`$ fe new <my_project>`

This will generate a template project that demonstrates some key features. It contains the following:

- A `src` directory containing two .fe files. These files show how one can import other modules and how to write tests.
- A `fe.toml` manifest with basic project info and some local project imports.

There are two project modes, they are `main` and `lib`. Main projects can import libraries and have code output. Libraries on the other hand cannot import main projects and do not have code outputs.

The mode of a project is determined by the presence of either `src/main.fe` or  `src/lib.fe`.

Once you have created a project, you can run the usual Fe CLI subcommands against the project path.