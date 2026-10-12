# Release authorization

Owner board card O16 (2026-10-09) permits merge, with no publication until the
baseline and version decision card requirements are met.
The baseline acceptance criteria and release version are not established by
O16; the later owner card must identify them before publication is authorized.

The `pypi` GitHub environment must have the owner as a **REQUIRED REVIEWER**
before any publish. This protection must be configured in the repository's
GitHub environment settings; workflow comments do not configure or verify it.
Administrator bypass of environment protections must be disabled.

**No PyPI publish may occur without a later owner decision card.** The later
card must authorize publication after the baseline and version requirements
are met. Required-reviewer approval and publication authorization are separate
requirements.

The checked-in publishing workflow permits only manual dispatch from `main`.
With this workflow file, a dispatch from another ref is skipped by the publish
job. A branch can modify its own workflow file, so the `pypi` environment must
also restrict deployment branches to `main`, and the PyPI trusted publisher
must bind this repository, `publish.yml`, and the `pypi` environment. These
external settings are prerequisites; their live configuration is not verified
by repository tests. See [PyPI trusted publisher configuration](https://docs.pypi.org/trusted-publishers/adding-a-publisher/).

The current job grants OIDC permission to its dependency-installation and test
steps as well as the publish step. This existing exposure remains unresolved
by O16. The later release decision must address separating build/test from
the credential-bearing publish job before authorizing publication.

Merge of the branch or PR does
not authorize a workflow dispatch or a PyPI release.

The package and CLI read the installed distribution version. An uninstalled
source checkout reads the version from `pyproject.toml`. The project version
remains `2.1.0` under O16.
After a version change, an installed or editable distribution must be
reinstalled to refresh its metadata before source-checkout CLI checks.
