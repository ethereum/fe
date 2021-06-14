## Release

### Versioning

Make sure that version follows [semver](https://semver.org/) rules e.g (`0.2.0-alpha`).

**For the time being, ALWAYS specify the `-alpha` suffix.**

### Generate Release Notes

**Prerequisite**: Release notes are generated with [towncrier](https://pypi.org/project/towncrier/).Ensure to have `towncrier` installed and the command is available.

Run `make notes version=<version>` where `<version>` is the version we are generating the release notes for e.g. `0.2.0-alpha`.

Example:

```
make notes version=0.2.0-alpha
```

Examine the generated release notes and if needed perform and commit any manual changes.

### Generate the release

Run `make release version=<version>`.

Example:

```
make release version=0.2.0-alpha
```

This will also run the tests again as the last step because some of them may need to be adjusted because of the changed version number.

### Tag and push the release

**Prerequisite**: Make sure the central repository is configured as `upstream`, **not** `origin`.

After the tests were adjusted run `make push-tag` to create the tag and push it to Github.


### Manually edit the release on GitHub

Running the previous command will push a new tag to Github and cause CI to create a [release](https://github.com/ethereum/fe/releases) with the Fe binaries attached. We may want to edit the release afterwards to put in some verbiage about the release.