# Plan: digitalmodel #529 — convert_batch parallel path doesn't aggregate success counts

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/529
**Status:** plan-review
**Tier:** T2

## Context

`OrcaFlexConverterEnhanced.convert_batch(parallel=True)` shows per-file success ticks on the progress bar but never rolls worker-future results into `self.stats`. After a 5-file parallel run, `stats['successful']` is `0`. The serial branch increments `self.stats['successful'] += 1` at `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py:212`; the parallel branch is missing the equivalent aggregation.

Surfaced by `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_batch_parallel_conversion`. Spillover from #510 (plan scope explicitly excluded `src/` edits — adversarial review D1). Sibling fixture-scoping bug tracked at #530; remaining 9 pre-existing failures at #531.

## Plan

1. **Reproduce locally before edits.** Run `uv run pytest tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_batch_parallel_conversion -v` to confirm the `assert 0 > 0` failure on current main. Capture serial-path stats from `test_batch_serial_conversion` (or equivalent) as the reference shape.

2. **Audit serial-path counters.** Read `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py:212` and surrounding success/failure/validation_failed/files_by_output_type/errors increments to enumerate every key that the serial path mutates per worker iteration. This is the contract the parallel path must match.

3. **Aggregate worker results in the parallel branch.** Each worker future already returns success/failure metadata to drive the progress-bar tick — extend the `as_completed` consumer to mirror those increments into `self.stats`. Keep the progress-bar integration intact; do not change the worker function signature unless required for the keys it currently omits.

4. **Add parity regression test.** New test under `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py` (or a sibling) that runs the same input set through both `parallel=False` and `parallel=True` and asserts `stats_serial == stats_parallel` for the five tracked keys (`successful`, `failed`, `validation_failed`, `files_by_output_type`, `errors`). Use the existing 5-file fixture set; no new test data.

5. **Smoke check.** `uv run pytest tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py -k "batch" -v` then full file run to confirm no regression.

## Acceptance Criteria

- [ ] `test_batch_parallel_conversion` passes
- [ ] New parity test asserts `stats_serial == stats_parallel` and passes
- [ ] No change to public API of `convert_batch` (signature + return value preserved)
- [ ] `git diff` touches only `orcaflex_converter_enhanced.py` and `test_orcaflex_converter_enhanced.py`
- [ ] Full file `pytest tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py` shows no new failures vs. baseline
