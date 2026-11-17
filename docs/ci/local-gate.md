# Local pull-request gate

`scripts/ci/local_gate.sh` runs the pull-request gates on any Linux host with
`git`, `gh`, `uv` and Python 3.11 or later, then posts a commit status named
`local-gate` on the tested head. It exists because a reviewed pull request can
wait for a self-hosted runner longer than its jobs take to run. Unverified
trial notes dated 16 Nov 2026 report queue waits of 15 to 54 minutes and job
runtimes of 0.1 to 8 minutes. The underlying measurement evidence is unavailable;
these figures do not establish current performance.

## What one run does

```bash
scripts/ci/local_gate.sh --pr 1234 --sha <40-character head> --repo <owner/name> --work-root <directory>
```

1. Refuses to start when the volume that holds `--work-root` has less free
   space than `--min-free-gb` (default 20). The gate deletes nothing to make room.
2. Confirms through the GitHub API that the pull request is open, comes from
   this repository and still has `--sha` as its head.
3. Fetches the base branch and the head into its own bare mirror under the work
   root, checks out the base tip in a temporary worktree and merges the head
   into it. This is the same tree the hosted workflows test.
4. Runs `scripts/ci/detect_touched_domains.py --mode touched`, the detector the
   `Quality Gates by Domain` workflow uses, and plans one module per touched
   domain. The comparison is the base tip against the merged tree, so the
   selection reflects the pull request's own change. The hosted job compares
   the base tip with the pull-request head; for a head that is behind the base
   that also counts everything the base gained since the fork point, and one
   source merge on the base then selects every shard for every open pull
   request. In a trial on 17 Nov 2026 a four-module plan became a 24-module
   plan after one unrelated merge to the base. The shard commands come from `.claude/quality-gates.yaml` and the
   harness command from the workflow file, both read from the tree under test.
5. Runs the modules in parallel (`--jobs`, default 4), each in a separate
   checkout with its own log and time limit (`--module-timeout`, default 1500 s)
   inside one overall budget (`--budget-seconds`, default 3600 s). API, clone,
   fetch, merge, detection, plan-reading and checkout subprocesses receive the
   remaining overall budget. Timeout cleanup sends TERM, waits at most 1 s for
   the leader, then sends KILL to its group even if the leader already exited.
   An unverified trial note dated 16 Nov 2026 reports a longest shard of 1122 s;
   the underlying measurement evidence is unavailable.
6. Posts the `local-gate` status on `--sha`, writes `result.json` and the logs
   under `<work-root>/runs/`, and removes the temporary checkouts.

Table 1 lists the modules.

| Module | Content | Hosted equivalent |
| --- | --- | --- |
| `lint` | Text-encoding check on changed Python lines and its dedicated tests, report-provenance ratchet, repository-structure validation and its dedicated tests, engineering-register check on changed documents | `Text Encoding`, `Repository Structure`, the two static steps of `Quality Gates` |
| `ci-harness` | The harness and compliance test list of the domain workflow | `CI harness and compliance tests` |
| one per touched domain | The `tests-<domain>` command, with the same additions the workflow applies to `contracts`, `orcaflex`, `specialized` and `subsea` | `tests-<domain>` |

Table 1: modules of one local gate run.

## Result rules

The result is fail-closed. Table 2 gives every outcome.

| Condition | Status posted | Exit code |
| --- | --- | --- |
| Every planned module ran and returned 0 | `success` | 0 |
| A module failed, timed out or was not started inside the budget | `failure` | 1 |
| The head does not merge into the current base tip | `failure` | 1 |
| Low disk, fetch failure, detector failure, a touched domain without a command, a pull request from a fork | `error` | 2 |
| The head moved or the pull request is closed | none | 3 |

Table 2: local gate outcomes.

The status description is capped at 140 characters as a local compatibility
policy. The [commit-status API documentation](https://docs.github.com/en/rest/commits/statuses#create-a-commit-status)
requests a short description but gives no numeric bound; 140 is not claimed as
a verified protocol limit. The status description starts with
`base <full commit id>:`, the base tip the head was merged into, followed by the module count, the elapsed time and every
module that did not pass. A result is evidence for one head on one base: a new
push needs a new run, and a merge step that requires an up-to-date result
compares that commit id with the current tip of the base branch and runs the
gate again when the base has moved.

## Workflows that become advisory

A merge step may take `local-gate` `success` at the exact head in place of
waiting for the runner queue. Under that rule the pull-request runs of the
following workflows are advisory, which means a queued or running job no longer
delays a merge:

- `Quality Gates by Domain` (the local gate runs the same shards)
- `Quality Gates`
- `Text Encoding`
- `Repository Structure`
- `Build API Docs`, `Content store tests`, `Gmsh and OpenFOAM Bridge Tests`,
  `Parametric atlas drift`, `Workflow Automation Tests`, `Capability card links`

Advisory does not mean ignored. A hosted job that has finished with a failure
at the head still blocks the merge, with one exception: the jobs the local gate
runs itself on the same merged tree (the three `Quality Gates by Domain` job
groups, `Text Encoding` and `Repository Structure`) are superseded by the
`local-gate` result. A runner that refused or dropped one of those jobs
therefore does not hold a pull request the gate has passed. A job that has not
finished is disregarded.

These runs are not advisory:

- `Full Matrix Sweep` on every push to `main` and nightly. It is the only run
  that executes every shard, so it is the check on shards the touched-domain
  selection did not run and on drift between a gate run and a later base tip.
- Every push run on `main`, `Deploy Docs to Pages` and `Publish to PyPI`.

## Host requirements

- A native Linux file system for `--work-root`. A trial on an NTFS volume
  mounted through FUSE reportedly failed during package installation with
  `No space left on device` while reporting 120 GB free. This trial observation
  is unverified because its underlying measurement evidence is unavailable.
- The interpreter the hosted jobs use, passed as `--python <path>`. A
  standalone CPython 3.11 build was reported to lack `fcntl.F_SEAL_WRITE`, with
  22 failures in `tests/workflows/test_hosted_execution.py`. This trial result
  is unverified because its underlying evidence is unavailable.
- Repository identity from `--repo <owner/name>` or `GITHUB_REPOSITORY`. The
  requested value is supplied to every module, including dependency URL expansion.
- An existing Git identity for the temporary merge, read from normal runtime
  configuration or environment. The gate supplies no identity override.
- `npm` for the `contracts` module and `libglu1-mesa` for `solver-smoke` and
  `specialized`. A missing prerequisite fails that module; nothing is installed
  with elevated rights.

## Limits

- Resource defaults (4 modules, 1500 s per module, 3600 s overall and a 20 GB
  free-space floor) are policy choices, not measured capacity limits. Process
  cleanup has a 1 s TERM grace ceiling and a 1 s reap timeout beyond the execution
  budget. Cleanup and status-post helpers outside that budget each have a 60 s
  policy timeout.
- The local gate does not run the repository-wide test and coverage job of
  `Quality Gates`, the Python 3.12 matrix leg, or the baseline comparison of the
  domain aggregate. Those results arrive from the hosted runs and from the push
  to `main`.
- Disk use. Every module has its own checkout of the merged commit, as every
  hosted job has, so a file one shard writes cannot change the result of
  another. Unverified trial notes report about 25 GB under the work root with
  four modules: a complete mirror (3.4 GB), the package cache (19 GB) and the
  checkouts. The underlying measurement evidence is unavailable. Each checkout
  is removed as soon as its module ends, so the checkouts in use are bounded by
  `--jobs`; the scratch directory is removed at the end of a run. The mirror
  and the cache stay for the next run. A stop signal ends the running modules,
  starts no further one and leaves the result `failure`.
- The gate executes code from the pull request with the credentials of the
  account that runs it. It accepts same-repository pull requests only, the same
  boundary the self-hosted workflow jobs apply.
- Branch protection is unchanged by this script. Making `local-gate` a required
  status is a repository setting and a separate decision.

## Tests

`tests/scripts/test_local_gate.py` covers module planning, the budget and
timeout rules, repository environment and identity, the status text and every
outcome in Table 2. Setup commands and clocks use fakes. Real local subprocesses
prove that timeout cleanup kills a TERM-ignoring descendant after its leader
exits, closes inherited capture pipes, and expands dependency URLs from the
requested repository without reaching the network.
