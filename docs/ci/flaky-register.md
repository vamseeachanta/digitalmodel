# Flaky register

The planned daily main-health loop will propose entries from retry artifacts
and evidenced same-tree mismatches through a pull request. That automation is
not part of this change. Entries close with the merged fix recorded in `fix_issue`.

The register never deselects or skips a test. In enforce mode, a retry-recovered
test ID can clear only when its register entry has `status: open` and a nonempty
`evidence` list. Missing, fixed, empty or unreadable entries keep the shard red.
Each entry records a unique ID, kind, first and last observation dates, run
evidence, cause (including uncertainty), fix reference or `pending`, and status.

`scripts/ci/run_shard.py --mode report --shard NAME --junit reports/junit-NAME.xml --max-retry-ids 20 -- COMMAND`
runs the original command unchanged. Report mode is the default: it always
returns the first attempt's exit code, even if a diagnostic retry passes with
register evidence. Only the `orcaflex` shard uses the wrapper in the domain gate
and full sweep. Other shards run their direct commands; contracts never retry.
Widening the pilot and selecting enforce mode are separate changes after a week
of report-mode observations.

After exit 1, a fresh parsed JUnit report with collected tests and 1 to 20 failed
or errored IDs permits one retry with the original non-path flags. Collection
errors, timeouts, incomplete evidence and larger sets are never retried. All
non-retried exits retain their original codes, including 2, 3, 4, 5 and signals.
Enforce mode requires a zero retry exit, every requested ID passing (no skips),
and register evidence for every recovered ID before returning zero.

The retry record keeps each first-attempt failed ID and its first message (up
to 500 characters per ID), recovered IDs, original exit, retry outcome, register
decisions, commit, run ID and CPU model. Warnings and the job summary include
each first failure even when the retry passes. Separate JUnit files preserve both
attempts. Extraction carries `first_attempt_failed` and `recovered` into shard
results. Comparison considers first failures from successful and failed shards,
classifying each as pre-existing on fresh main evidence for the same shard and
ID, registered with evidence after recovery, or NEW. The committed baseline mode
remains `report`. Recovered IDs are excluded from open main-baseline failures.

Atlas runs its direct command and is outside the retry pilot. Signal exits
propagate without retry in both modes. `--decide-only --from-log LOG` inspects
cached hosted logs without executing their content. An isolated pass does not
establish a failure's cause; shared state and leftover files can produce a retry
pass. Failures whose test IDs cannot be established retain their raw classname
and name in the retry record and job summary.
Register evidence remains a human decision.

On pull requests, register selection fetches the base branch tip at run time
and reads its register with `git show` into a temporary file. The wrapper and
comparator receive that file through `--register`. An absent base register
means no registered IDs; fetch or object-read errors fail closed. Other events
use the checked-out register. A pull request cannot certify its own failures
by adding entries to its checkout.
