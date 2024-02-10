# Contributing

You *can* contribute to Fe! 


## Ways to contribute:

### 1. Reporting or fixing issues.

If you find problems with Fe you can report them to the development team on the project [Github](https://github.com/ethereum/fe).
You are also welcome to work on existing issues, especially any tagged `good first issue` on the project issue board.

### 2. Improving the docs.

We always appreciate improvements to the project documentation. This could be fixing any bugs you find, adding some detail to a description or explanation or developing a new user guide.

To add to the docs you can fork the Fe Github repository and make updates in the `/docs` directory. You can build and serve locally using `mdbook build && mdbook serve`. Then, when you are happy with your changes you can raise a pull request to the main repository.

### 3. Developing Fe

You are also welcome to work on Fe itself. There are many opportunities to help build the language, for example working on the compiler or the language specification, adding tests or developing tooling. 

It is a good idea to connect with the existing Fe community to find out what are the priority areas that need attention and to discuss your idea in context before putting time into it. You can find Fe developers on the [Discord](https://discord.gg/yCT6NYBb) or you can use the [Github issue board](https://github.com/ethereum/fe/issues).

> **Please note** that there has been a substantial amount of work done on the `fe-v2` branch of the repository that includes breaking changes. When merged `fe-v2` will revert new contributions based on `master`. 
> 
> To make your work v2 ready you can build off the `fe-v2` branch. It is recommended to seek out issues tagged `v2` in the Github Issue board.

### 4. Community engagement

We appreciate help answering questions on the Discord and other platforms such as Stack Exchange or Twitter.

> Please note that this project has a [Code of Conduct](code_of_conduct.md). By participating in this project — in the issues, pull requests, or Discord channel — you agree to abide by its terms.

## Processes

### Reporting issues

To report an issue, please use the [Github issue board](https://github.com/ethereum/fe/issues). When reporting issues, please mention the following details:

- Fe version.
- your operating system.
- the steps required to reproduce the issue
- actual vs expected behaviour
- any error messages or relevant logs
- the specific source code where the issue originates

The appropriate place for technical discussions about the language itself is the Fe [Discord](https://discord.gg/ywpkAXFjZH).

### Rasing Pull Requests

Please [fork the Fe repository](https://docs.github.com/en/get-started/quickstart/fork-a-repo) and raise [pull requests](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request) against the `master` branch.

Your commit messages should be concise and explain the changes made. Your pull request description should explain why the changes were made and list the specific changes. If you have to pull in changes from `master` to your fork (e.g. to resolve merge conflicts), please use `git rebase` rather than `git merge`.

New features should be accompanied by appropriate tests.

Finally, please make sure you respect the coding style for this project.


Thank you for contributing to Fe!
