# Release prerequisites

A PyPI publish cannot be undone: a released version number cannot be uploaded
again. The team decides a release. Log the dispatch on fleet-ops #577 before
running it, with the baseline acceptance criteria and the release version it
publishes.

The `pypi` GitHub environment is configured with a required reviewer. This
protection lives in the repository's GitHub environment settings; workflow
comments do not configure or verify it. Administrator bypass of environment
protections must be disabled.

The checked-in publishing workflow permits only manual dispatch from `main`.
With this workflow file, a dispatch from another ref is skipped by the publish
job. A branch can modify its own workflow file, so the `pypi` environment must
also restrict deployment branches to `main`, and the PyPI trusted publisher
must bind this repository, `publish.yml`, and the `pypi` environment. These
external settings are prerequisites; their live configuration is not verified
by repository tests. See [PyPI trusted publisher configuration](https://docs.pypi.org/trusted-publishers/adding-a-publisher/).

The current job grants OIDC permission to its dependency-installation and test
steps as well as the publish step. Separate build/test from the
credential-bearing publish job before publishing.

Merge of a branch or PR does not dispatch the workflow or publish a release.

The package and CLI read the installed distribution version. An uninstalled
source checkout reads the version from `pyproject.toml`. The project version
is `2.1.0`.
After a version change, an installed or editable distribution must be
reinstalled to refresh its metadata before source-checkout CLI checks.
