# CI rerun rules

These rules state when a test run starts for a pull request and when an
existing result stands. The aim is one run per code change, on the modules
that change touches, and no run for an event that leaves the tree unchanged.

Each rule names where it is enforced. "Workflows" means the files under
`.github/workflows/`. "Local gate" means `scripts/ci/local_gate.sh`, proposed
separately; rules that depend on it are marked as not yet in force.

## Rules

| Rule | Statement | Enforced by | State |
| --- | --- | --- | --- |
| R1 | Tests run on a code push to the pull request, for the modules that push touches. | `Quality Gates by Domain`, `detect_touched_domains.py --diff-from merge-base` | In force with the merge-base detection change |
| R2 | A title, description or label edit, a review and a comment run no job. The last result at that head stands. A change of base branch runs the gates. | Metadata-edit condition and second job name in the four gate workflows; `tests/workflows/test_ci_triggers.py` | In force with this change |
| R3 | A newer push cancels the older run of the same pull request. | Per-pull-request `concurrency` group with `cancel-in-progress` | Already in force |
| R4 | A draft pull request gets no gate run until it is marked ready, except the one job named in the note below. | Draft condition on the first job of each gate workflow | In force for the jobs listed in the note |
| R5 | A merge to `main` starts no run on open pull requests. | No workflow listens for it | Already in force |
| R6 | A module result is reused when its inputs are unchanged. | Local gate | Proposed; see below |
| R7 | An infrastructure failure gets one automatic rerun. A second one goes to the runner maintainer with no further rerun. | Not enforced in the workflows | Proposed; see below |
| R8 | A test failure gets no rerun until a new push. A test recovered by one retry is recorded, not ignored. | `scripts/ci/run_shard.py` and the flaky register, `orcaflex` shard only | Partly in force |
| R9 | The full matrix runs nightly on `main`, on manual dispatch, and for a change to shared source, `pyproject.toml` or `uv.lock`. | `Full Matrix Sweep` (schedule, dispatch); `FULL_MATRIX_PATHS` and `FULL_MATRIX_PREFIXES` in the detector | In force with this change |
| R10 | A pull request that changes only files no test or build reads runs no test shard. Text encoding and repository structure still run. | Detector returns an empty matrix; `domain-tests` is skipped | Partly in force; see below |

Table 1: rerun rules and where each is enforced.

## Notes on single rules

**R2, edits.** GitHub sends one event, `edited`, for a title edit, a
description edit and a change of base branch. The four gate workflows listen
for it. Every job that starts a workflow, and every aggregate job, carries the
condition `!(github.event.action == 'edited' && !github.event.changes.base)`;
the jobs that need those are skipped with them:

- A base change changes the tree under test. Every job runs under its usual
  name, in the pull request's usual concurrency group, exactly as for a push.
- A title or description edit runs no job. A skipped job still posts a check,
  and under the usual name that check would become the newest result at the
  head and stand in for an earlier failure. Every job therefore takes the name
  `Not run for a metadata edit - <job>` for this event, and the run takes a
  concurrency group of its own so that it cancels nothing. The earlier checks
  remain the newest under their names.

The cost of a metadata edit is four workflow runs with no job started.

**R2, reopen.** `reopened` remains a trigger; `tests/contracts/test_vba_credit.py`
also requires it on the domain workflow.

**R4, drafts.** The draft condition is on `Detect touched test domains` and
`Domain test aggregate` (the jobs between them need the first), on
`Quality Gates (Python 3.11)` and `Run Quality Gates`, on `Atlas staleness check`
and on `focused-tests`. `Parametric package tests` carries no draft condition
and runs for a draft that touches its paths; it runs on a hosted runner.

**R5 and R6, `main` moving.** Hosted results are not invalidated when `main`
moves. The proposed local gate tests the head merged into the current base and
names that base in its status, so a merge step can require a result for the
base it merges into. R6 keeps that affordable: the gate records, per module,
the merged commit of its last pass, and marks a module passed without running
when the detector reports that the difference between that commit and the
present merged tree does not touch the module, with the interpreter and the
module command unchanged. The record is one JSON file and one Git reference
per module under the gate work root. The detector is the single definition of
"inputs changed", so no second mapping is maintained. This is not built here.

**R7, infrastructure failures.** A job that fails before any test runs
(runner set-up, checkout, dependency install, time limit with no test failure)
is an infrastructure failure. GitHub Actions reruns a job only through the
interface or an API call from a second workflow with `actions: write`; no such
workflow exists in this repository, and adding one is a separate decision.
Until then the rule is applied by the person or tool that watches the run:
one rerun, then a report to the runner maintainer.

**R8, flaky tests.** `run_shard.py` retries a bounded set of failed test IDs
once in report mode and writes the evidence; `docs/ci/flaky-register.md`
describes the register. Only the `orcaflex` shard uses it. Extending it to
every shard, and opening an issue when a retry recovers a test, is proposed.

**R10, changes no test reads.** The detector already returns an empty matrix
for paths that no domain owns and that are not full-matrix triggers (see
`NO_DOMAIN_PATHS` and the domain table in `tests/DOMAINS.md`), and the
`domain-tests` job is then skipped. Two jobs still run for such a pull
request: `CI harness and compliance tests` and the `Quality Gates` matrix.
Skipping those two for an empty matrix is proposed, not done here: the
`Quality Gates` matrix runs repository-wide checks whose inputs have not been
enumerated, and the rule requires that list to come from what the checks read.

## Fast and slow tests

`pytest.ini` registers a `slow` marker, but only 22 tests in 16 files carry
it, so the marker cannot split a fast tier from a nightly tier today. The
proposed convention: a test that takes more than 10 s on a hosted runner is
marked `slow`; pull-request shard commands add `-m "not slow"`; the nightly
sweep runs everything. The marks are to come from measured durations
(`--durations=50` in the nightly sweep), not from a bulk edit.

## Measurements

| Figure | Value | Source | State |
| --- | --- | --- | --- |
| Runner time of one full sweep | 133.8 min over 24 jobs, longest job 19.6 min | Run 43824707679 (`schedule`, 16 Nov 2026): sum of job durations from `gh api repos/<owner>/<repo>/actions/runs/43824707679/jobs` | Reproducible from the run |
| Same, three `push` sweeps on 16 Nov 2026 | 141.5, 127.9 and 129.6 min | Runs 43848236852, 43867164887, 43870493821, same method | Reproducible from the runs |
| Sweeps started on 16 Nov 2026 | 11 by `push`, 1 by `schedule` | `gh run list --workflow full-matrix-sweep.yml` filtered to that date | Reproducible while the runs are retained |
| Pull-request runs of `Quality Gates by Domain` on 16 Nov 2026 | 108 runs for 92 heads, 58 cancelled | `gh run list --workflow quality-gates-by-domain.yml --event pull_request`, counted once on 16 Nov 2026 | Unverified: the count is not retained as a file |

Table 2: measured figures quoted for this change and where each comes from.
