# Flaky register

Entries are added by pull request from `flaky-*.json` artifacts or documented
run evidence. A `fixed` entry requires a nonempty `passing_evidence` list of
GitHub Actions run API URLs with numeric repository and run IDs, alongside a
positive integer `fix_pr`. Closure requires checking that those runs pass the
affected test or command on the corrected tree and that the fix pull request is
merged. The schema checks the field structure and URL format; it does not fetch
run outcomes or merge status. A merged pull request alone does not establish
that a failure is resolved. Open entries retain the existing fields without
`passing_evidence`.
Repeated failures without a passing run on the same tree use
`kind: repeated-failure`, rather than claiming a same-tree mismatch.

The register is informational: it never selects tests or changes gate conclusions.
Each entry records a unique test or command ID, kind, first and last observation
dates, nonempty run evidence, cause (including uncertainty), fix pull request
number or `pending`, and `open` or `fixed` status.

`scripts/ci/run_shard.py --shard NAME --junit reports/junit-NAME.xml --max-retry-ids 20 -- COMMAND`
runs the original command unchanged. After exit 1, a fresh parsed JUnit report
with collected tests and 1 to 20 failed or errored IDs permits one retry with
the original non-path flags. Collection errors, timeouts, incomplete evidence
and larger failure sets are not retried. Every original exit code is preserved,
including exits 2, 3, 4, 5 and signal exits. A passing retry provides evidence
only: the original failing gate remains red.
Recovered IDs leave warnings and a JSON record containing the first failure
message (at most 500 characters), commit, run ID and observed CPU model.
Both attempts retain their full console output and separate JUnit reports.
No recovery record is written when no requested ID passes on retry.
The baseline extractor reads the first attempt: recovered IDs remain first-attempt
failures under a failed shard, rather than failures under a successful shard.

The 20-ID ceiling is an operational budget choice: it bounds one retry's selected
test count and excludes larger failure sets from retry.
It does not bound execution time, and no runtime measurements establish 20 as an
optimal threshold. `--max-retry-ids` may lower this ceiling but cannot raise it.
The 500-character message limit keeps the JSON record a compact diagnostic
preview; full messages remain in the console and JUnit reports. It is neither a
redaction control nor a bound on total artifact size. These are software policy
choices, not standards-derived engineering coefficients.

`--signal-retry` retries a non-pytest command once after a signal exit (128 or
above), excluding timeout and termination codes. Ordinary exits 1 to 127 are
propagated without retry. A passing signal retry also preserves the original
failing exit code. `--decide-only --from-log LOG` reads cached hosted
logs without executing their content; it requires a terminal exit code and
collection evidence for test-ID decisions.

The repeated numerical failure entry remains open. The referenced pull request
made an existing absolute tolerance explicit; that change alone does not
establish resolution. Its recorded runs show failures on different trees;
they establish neither a same-tree mismatch nor passing evidence for closure.
Retry eligibility is not evidence that a failure is transient. All initial
failures remain red after retry. Signal retry does not establish the abort's
cause. Launchers that resolve a mutable dependency reference may use a different
dependency revision on retry; dependency revision pinning is not implemented.
