# Deployment parity

A deployment stamp records an observation at deployment time. It does not
establish that a deployed copy still matches the repository. The acceptance
criterion is no pending rsync changes against the exported repository revision
for the files included in deployment.

`bash scripts/cfd/deploy_lane.sh <destination> <campaign>` exports `HEAD` into a
scratch directory under the required `TMPDIR`. Uncommitted edits and untracked
files are not deployed. Commit intended changes before deployment. The scripts
and package source are copied from that same frozen export, using checksum
comparison so equal size and modification time cannot conceal changed content.

After the existing remote syntax check, separate checksum dry runs compare the
package and scripts to the export. A nonempty change list or an unsuccessful
comparison prevents a new `DEPLOYED.txt` entry. A successful comparison appends
the full revision and `parity=verified`, without recording machine identity.
An older stamp remains historical evidence if a later deployment fails.

The existing exclusions remain in force: package bytecode and script backups,
cache directories and the script README are outside the acceptance criterion.
Destination-only script files, including the stamp, are preserved and are not
checked. Destination-only package files retain the existing deletion policy.
Case data, registries and campaign entry points remain outside this deployment.
No solver is invoked by the parity check.

Use `--dry-run` before deployment to inspect the proposed transfer. A dry run
neither writes a stamp nor certifies parity. If verification fails, inspect the
reported differences and transport error, resolve the cause, then redeploy the
intended committed revision. Do not repair a deployed script without committing
and redeploying the correction. Checksum parity is a point-in-time observation;
it does not prevent concurrent changes or subsequent drift, and deployment is
not atomic. Continuing drift monitoring remains separate work.

References:

- [Deployment-stamp defect](https://github.com/vamseeachanta/digitalmodel/issues/2059).
- [rsync manual](https://download.samba.org/pub/rsync/rsync.1),
  `--checksum`, `--dry-run`, `--itemize-changes`, and `--delete`.
- [Git archive manual](https://git-scm.com/docs/git-archive),
  DESCRIPTION and `<tree-ish>`.
