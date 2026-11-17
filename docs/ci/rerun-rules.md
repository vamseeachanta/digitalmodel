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
| R2 | A title, description or label edit, a review and a comment start no run. The last result at that head stands. | Trigger types `opened`, `synchronize`, `reopened`, `ready_for_review` only; `tests/workflows/test_ci_triggers.py` | In force with this change |
| R3 | A newer push cancels the older run of the same pull request. | Per-pull-request `concurrency` group with `cancel-in-progress` | Already in force |
| R4 | A draft pull request gets no hosted run until it is marked ready. | Draft condition on the first job of each gate workflow | Already in force |
| R5 | A merge to `main` starts no run on open pull requests. | No workflow listens for it | Already in force |
| R6 | A module result is reused when its inputs are unchanged. | Local gate | Proposed; see below |
| R7 | An infrastructure failure gets one automatic rerun. A second one goes to the runner maintainer with no further rerun. | Not enforced in the workflows | Proposed; see below |
| R8 | A test failure gets no rerun until a new push. A test recovered by one retry is recorded, not ignored. | `scripts/ci/run_shard.py` and the flaky register, `orcaflex` shard only | Partly in force |
| R9 | The full matrix runs nightly on `main`, on manual dispatch, and for a change to shared source or dependencies. | `Full Matrix Sweep` (schedule, dispatch); `FULL_MATRIX_PATHS` and `FULL_MATRIX_PREFIXES` in the detector | In force with this change |
| R10 | A pull request that changes only files no test or build reads runs no test shard. Text encoding and repository structure still run. | Detector returns an empty matrix; `domain-tests` is skipped | Partly in force; see below |

Table 1: rerun rules and where each is enforced.

## Notes on single rules

**R2, reopen.** `reopened` remains a trigger. A reopen is rare, and it is the
way to run the gates again after the base branch of a pull request is changed,
because `edited` no longer starts a run. `tests/contracts/test_vba_credit.py`
also requires `reopened` on the domain workflow.

**R2, base change.** After the base branch of a pull request is changed, the
gates do not start by themselves. Close and reopen the pull request, or push,
to test against the new base.

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
