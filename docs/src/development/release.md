## Release

### Versioning

Make sure that version follows [semver](https://semver.org/) rules e.g (`0.23.0`).

### Generate Release Notes

**Prerequisite**: Release notes are generated with [towncrier](https://pypi.org/project/towncrier/).Ensure to have `towncrier` installed and the command is available.

Run `make notes version=<version>` where `<version>` is the version we are generating the release notes for e.g. `0.23.0`.

Example:

```
make notes version=0.23.0
```

Examine the generated release notes and if needed perform and commit any manual changes.

### Generate the release

Run `make release version=<version>`.

Example:

```
make release version=0.23.0
```

This will also run the tests again as the last step because some of them may need to be adjusted because of the changed version number.

### Tag and push the release

**Prerequisite**: Make sure the central repository is configured as `upstream`, **not** `origin`.

After the tests are adjusted run `make push-tag` to create the tag and push it to Github.


### Manually edit the release on GitHub

Running the previous command will push a new tag to Github and cause the CI to create a [release](https://github.com/ethereum/fe/releases) with the Fe binaries attached. We may want to edit the release afterwards to put in some verbiage about the release.

## Updating Docs & Website

A release of a new Fe compiler should usually go hand in hand with updating the website and documentation. For one, the front page of [fe-lang.org](https://fe-lang.org) links to the download of the compiler but won't automatically pick up the latest release without a fresh deployment. Furthermore, if code examples and other docs needed to be updated along with compiler changes, these updates are also only reflected online when the site gets redeployed. This is especially problematic since our docs do currently not have a version switcher to view documentation for different compiler versions ([See GitHub issue #543](https://github.com/ethereum/fe/issues/543)).

### Preview the sites locally

Run `make serve-website` and visit http://0.0.0.0:8000 to preview it locally. Ensure the front page displays the correct compiler version for download and that the docs render correctly.

### Deploy website & docs

**Prerequisite**: Make sure the central repository is configured as `upstream`, **not** `origin`.

Run `make deploy-website` and validate that [fe-lang.org](https://fe-lang.org) renders the updated sites (Can take up to a few minutes).