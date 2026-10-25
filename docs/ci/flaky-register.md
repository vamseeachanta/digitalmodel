# Flaky register

The planned daily main-health loop will propose entries from retry artifacts
and evidenced same-tree mismatches through a pull request. That automation is
not part of this change. Entries close only when recorded evidence establishes
resolution and `fix_issue` identifies the merged fix.
Repeated failures without a passing run on the same tree use
`kind: repeated-failure`, rather than claiming a same-tree mismatch.
The repeated numerical failure remains open. Pull request 2576 made an existing
absolute tolerance explicit without changing comparator acceptance; that merged
change and a successful shard do not establish resolution. The reference in
`fix_issue` records that related change, rather than evidence of closure. Its
recorded failures on different trees do not establish a same-tree mismatch.

The register never deselects or skips a test. In enforce mode, a retry-recovered
test ID can clear only when its register entry has `status: open` and a nonempty
`evidence` list. Every URL must resolve to a completed failing job for the same
shard in this repository, from a main push or scheduled run. PR evidence, other
shards and unverifiable metadata cannot authorize a recovery. Missing, fixed,
empty or unreadable entries keep the shard red.
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

The 20-ID ceiling is an operational choice to bound the number of diagnostic
test invocations after a failed shard; it is not derived from observed runtimes
or a statistical criterion. A smaller `--max-retry-ids` can reduce that budget.
One costly ID can still take longer than many inexpensive IDs, so the ceiling
does not establish a duration bound or replace the workflow timeout.

The retry record keeps each first-attempt failed ID and its first message (up
to 500 characters per ID), recovered IDs, original exit, retry outcome, register
decisions, commit, run ID and CPU model. Warnings and the job summary include
each first failure even when the retry passes. Separate JUnit files preserve both
attempts. Extraction carries `first_attempt_failed` and `recovered` into shard
results. Comparison considers first failures from successful and failed shards,
classifying each as pre-existing on fresh main evidence for the same shard and
ID, registered with evidence after recovery, or NEW. The committed baseline mode
remains `report`. Recovered IDs are excluded from open main-baseline failures.

The 500-character message limit is an operational summary-size choice per ID,
not an assertion that every diagnostic fits within it. IDs and failure counts
are not truncated, and the first-attempt JUnit file retains the full messages.
The record uses `kind: retry-pass` only when at least one requested ID has fresh,
identified passing retry evidence. Failed, skipped, missing, stale, malformed or
unidentified retry results with no recovered IDs retain `kind: test-ids`, even
if the retry process exits zero. Partial recovery remains visible and cannot
clear enforce mode unless every requested ID passes with register evidence.

The Atlas drift gate runs its direct command. The diagnostic `--signal-retry`
option retries the whole command once after a signal exit (128 or above),
excluding timeout and termination
codes. Ordinary exits 1 to 127 propagate without retry. Both modes return the
first signal exit, and the record and summary retain both attempt exits.
`--decide-only --from-log LOG` inspects cached hosted
logs without executing their content. Neither an isolated pass nor a signal
retry establishes a failure's cause; shared state and leftover files can also
produce a retry pass. Failures whose test IDs cannot be established retain their
raw classname and name in the retry record and job summary.
Register evidence remains a human decision.

On pull requests, register selection fetches the base branch tip at run time
and reads its register with `git show` into a temporary file. The wrapper and
comparator receive that file through `--register` and independently reselect
the base register on PRs before approving any entry. An absent base register
means no registered IDs; fetch or object-read errors fail closed. Other events
use the checked-out register. A pull request cannot certify its own failures
by adding entries to its checkout.

Mutable dependency references may resolve to different revisions between
attempts; dependency revision pinning is not implemented by the retry wrapper.
