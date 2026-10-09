# PyPI release control review

Review manual publishing against both the checked-in workflow and external
settings. A branch can edit its own workflow guard. Before an authorized
release, verify required reviewers, deployment-branch restrictions, and the
PyPI trusted publisher's environment binding. Repository tests do not establish
these settings. The release prerequisites are defined in
`docs/api/contributing/releases.md`; O16 does not authorize publication.
