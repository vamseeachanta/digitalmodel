# Flaky register

The daily main-health loop adds entries from `flaky-*.json` artifacts and evidenced same-tree mismatches through a pull request; entries close with the merged fix pull request recorded in `fix_issue`.

The register is informational: it never selects tests or changes gate conclusions.
Each entry records a unique test or command ID, kind, first and last observation
dates, nonempty run evidence, cause (including uncertainty), fix pull request
number or `pending`, and `open` or `fixed` status.

`scripts/ci/run_shard.py --shard NAME --junit reports/junit-NAME.xml --max-retry-ids 20 -- COMMAND`
runs the original command unchanged. After exit 1, a fresh parsed JUnit report
with collected tests and 1 to 20 failed or errored IDs permits one retry with
the original non-path flags. Collection errors, timeouts, incomplete evidence
and larger failure sets remain red. Exit 5 is propagated unchanged. A retry
must report every requested ID passing and exit zero to clear the shard.
Recovered IDs leave warnings and a JSON record containing the first failure
message (at most 500 characters), commit, run ID and observed CPU model.
Both attempts retain their full console output and separate JUnit reports.

`--signal-retry` retries a non-pytest command once after a signal exit (128 or
above), excluding timeout and termination codes. Ordinary exits 1 to 127 are
propagated without retry. `--decide-only --from-log LOG` reads cached hosted
logs without executing their content; it requires a terminal exit code and
collection evidence for test-ID decisions.

The SCR case may remain red on a second attempt on the same runner; runner
dependence is inferred from the observed floating-point differences. Retry
eligibility is not evidence that a failure is transient. Code failures remain
red after their retry. Signal retry does not establish the abort's cause.
