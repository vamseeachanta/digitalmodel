# Parallel speedup benchmark diagnostics

The parallel benchmark retains its existing performance gate and records the
observations needed to investigate intermittent failures. Timing sensitivity is
established by the failed attempt described in
[issue #2103](https://github.com/vamseeachanta/digitalmodel/issues/2103) and the
successful [unchanged retry](https://github.com/vamseeachanta/digitalmodel/issues/2103#issuecomment-5641302552).
The precise cause remains unestablished.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Add timing and CPU observations and independent result checks to support investigation. |
| 1 | 2026-10-25 | Link the issue and unchanged retry so the timing evidence and retained limit can be audited. |

## Test contract

`tests/infrastructure/performance/test_benchmarks.py` continues to require the
sequential mean divided by the parallel mean to exceed the existing limit.
[Issue #2103](https://github.com/vamseeachanta/digitalmodel/issues/2103) (2026)
records that limit; this change supplies no new acceptance
criterion. The existing workload, pool size, warm-up and repetition counts are
retained. Means use the sum of samples divided by their count, as defined in the
[Python statistics documentation, edition 3.11](https://docs.python.org/3.11/library/statistics.html#statistics.mean)
(retrieved 2026-10-19).

Each observation contains both raw elapsed-time sample lists, both means, their
ratio, worker count, logical CPU count, affinity CPU count and load averages.
The JSON observation appears on captured standard output and in a failing
assertion, so a failed test retains its timing evidence without an output file.
These are raw measurements, with precision retained for further analysis.

The CPU fields follow the [Python OS documentation, edition 3.11](https://docs.python.org/3.11/library/os.html)
(retrieved 2026-10-19). Logical count describes the system; affinity describes
where the calling process may execute. Neither establishes available CPU time.
Missing platform APIs or OS errors leave affinity and load unknown. CPU quota
is explicitly `NOT_EVALUATED`. Context is sampled after timing and is an
observation, not a time series or an attribution of the failure's cause.

An additional untimed test checks sequential and process-pool outputs against
independently expanded sums in its docstring. Distinct inputs also check result
ordering. Unused engineering imports are removed so unavailable optional modules
cannot skip these infrastructure tests.

## Deterministic coverage

The helper tests use synthetic elapsed times with hand-calculated means and
ratios. They also cover empty, nonpositive and nonfinite timings, invalid worker
counts, unavailable platform information and preservation of recorded samples.
The benchmark contract tests supply controlled timings while executing the
workload, checking success, failure and the original strict boundary. These
timings are test inputs, not measured performance or proposed limits.

## Later slices

Controlled CPU affinity, quota and contention experiments remain outstanding.
The new observations do not reproduce the hosted failure or prove a root cause.
A proposed stable CI performance contract requires those experiments and review;
the current timing gate remains active. No CI check is removed or skipped.
