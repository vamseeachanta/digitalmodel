# Sustained-load test policy

`TestStressTestScenarios.test_sustained_load` exercises a synthetic NumPy
workload. Its gate retains these repository criteria:

| Quantity | Criterion | Basis |
| --- | --- | --- |
| Failed requests / all requests | Less than 1.0 % | [Issue 1889](https://github.com/vamseeachanta/digitalmodel/issues/1889), 2026 |
| Successful requests / duration (s) | Greater than 8.0 requests/s | Issue 1889 |
| p95 response time (s) | Less than 0.05 s | [PR 1938](https://github.com/vamseeachanta/digitalmodel/pull/1938), 2026, resolving [issue 1936](https://github.com/vamseeachanta/digitalmodel/issues/1936) |

Table: Existing synthetic-test acceptance criteria; equality fails each gate.

The latency ceiling is an existing CI policy, not a standard-derived limit or a
product service-level objective. No new latency limit is introduced here. Mean
and maximum latency remain diagnostic values and do not set the p95 budget.
An absolute budget avoids making the gate stricter as the mean decreases, but a
sufficiently long scheduler pause can still affect it. A failure requires
investigation of the workload and runner scheduling; product degradation is not
established by this synthetic workload alone.

## Worked synthetic example

Inputs: a ten-second interval with one hundred successes, no errors, ninety-four
response times of 0.1 ms, and six response times of 1.0 ms. These are deliberately
constructed regression inputs, not measured observations or engineering data.

The existing analyzer sorts response times and selects zero-based index
`floor(0.95 * N)` for p95. With `N = 100`, index 95 falls in the six slower
samples, giving p95 = 1.0 ms. This documents the analyzer's current order-statistic
convention; it does not introduce an interpolated percentile estimator.

The independent hand calculation gives:

- Mean = `(94 * 0.1 + 6 * 1.0) / 100` = 0.154 ms.
- Historical relative budget = `5 * 0.154` = 0.770 ms, so 1.0 ms would fail.
- Existing absolute budget = 50.0 ms, so 1.0 ms passes.
- Throughput = `100 / 10` = 10.0 requests/s, above 8.0 requests/s.
- Error rate = `0 / (100 + 0)` = 0.0 %, below 1.0 %.

The ratio policy and its failure mechanism are recorded in issue 1889 and
[issue 1928](https://github.com/vamseeachanta/digitalmodel/issues/1928). PR 1938
establishes the absolute policy used for the answer above. No engineering
standard or licensed reference supplies these CI criteria.

## Regression coverage

`test_sustained_load_policy.py` feeds synthetic samples through the real analyzer
and calls the real sustained-load assertions. Only the timed collection step is
replaced, so the regressions do not sleep or depend on local scheduling.

The cases cover uniform fast responses, an isolated maximum outlier, jitter that
moves p95 above the old relative budget, latency below/at/above the absolute
ceiling, errors, and throughput below/at its required minimum. Expected values
are calculated in the test docstrings. The original timed load tests remain in
place and continue to exercise concurrent request collection.

Run both files with the repository test environment:

```sh
uv run python -m pytest tests/infrastructure/performance/test_sustained_load_policy.py tests/infrastructure/performance/test_load_testing.py
```

For an isolated check without resolving the project environment, include the
dependencies imported by both modules and the performance fixtures. A
pytest-only environment cannot collect these tests because the shared
`conftest.py` imports `psutil` and `memory_profiler`:

```sh
uv run --no-project --with pytest --with numpy --with pandas --with psutil \
  --with memory-profiler --with pytest-asyncio --no-build \
  python -m pytest tests/infrastructure/performance/test_sustained_load_policy.py \
  tests/infrastructure/performance/test_load_testing.py -q -rA -p no:cacheprovider
```

This runs nine deterministic policy cases and seven timed scenarios. The
`--no-build` option requires compatible wheels; an unavailable wheel requires a
supported test environment rather than a source build.

Long-term latency trending and calibration against product requirements remain
separate benchmark work; these regressions do not establish either.
