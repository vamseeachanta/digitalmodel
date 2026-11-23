# Pint Performance Benchmark Results

**Issue:** vamseeachanta/workspace-hub#1486
**Date:** 2026-03-31
**Pint:** 0.25.3 | **NumPy:** 1.26.4 | **Python:** 3.11

## Benchmark Design

Two representative offshore engineering calculations benchmarked with three approaches each:

1. **Hydrostatic pressure:** `P = rho * g * depth` (2 multiplications)
2. **Morison drag force:** `F = 0.5 * Cd * rho * A * v^2` (4 multiplications + 1 power)

Approaches:
- **Raw NumPy** -- baseline, no unit tracking
- **Pint Quantity** -- full unit propagation on every operand
- **@ureg.wraps()** -- Pint validates/strips units at function boundary, raw NumPy inside

Timing: `timeit` with 50 repeats x 10 calls, reporting median (ms).

## Results

### Hydrostatic Pressure (P = rho * g * depth)

| Array Size | Raw NumPy | Pint Quantity | @ureg.wraps | Pint/Raw | Wraps/Raw |
|------------|-----------|---------------|-------------|----------|-----------|
| 100        | 0.001 ms  | 0.038 ms      | 0.086 ms    | 27x      | 61x       |
| 10,000     | 0.006 ms  | 0.046 ms      | 0.102 ms    | 7x       | 16x       |
| 1,000,000  | 0.727 ms  | 0.795 ms      | 5.624 ms    | 1.1x     | 7.7x      |

### Morison Drag Force (F = 0.5 * Cd * rho * A * v^2)

| Array Size | Raw NumPy | Pint Quantity | @ureg.wraps | Pint/Raw | Wraps/Raw |
|------------|-----------|---------------|-------------|----------|-----------|
| 100        | 0.002 ms  | 0.084 ms      | 0.079 ms    | 40x      | 37x       |
| 10,000     | 0.011 ms  | 0.099 ms      | 0.099 ms    | 9x       | 9x        |
| 1,000,000  | 1.113 ms  | 4.993 ms      | 5.568 ms    | 4.5x     | 5.0x      |

## Key Findings

1. **Pint Quantity overhead is dominated by constant per-operation cost (~30-80 us), not array size.** At 100 elements the ratio is 27-40x; at 1M elements it drops to 1-5x because NumPy vectorized math dominates.

2. **@ureg.wraps() is not faster than full Pint Quantity for simple expressions.** The unit-strip/reattach overhead at each call boundary is comparable to or worse than Pint's internal bookkeeping. It is useful only for wrapping legacy code that cannot accept Quantity objects.

3. **More complex expressions (Morison vs hydrostatic) show higher overhead** because more Pint operations accumulate. The 4.5x ratio at 1M elements for Morison vs 1.1x for hydrostatic reflects additional unit-check multiplications.

4. **Absolute times are small.** Even the worst case (Morison, 1M elements, Pint Quantity) is 5 ms -- acceptable for batch engineering calculations.

## Guidance Thresholds

| Scenario | Recommendation |
|----------|---------------|
| Array size > 10K | Use full Pint Quantity freely. Overhead is 1-5x but absolute time is milliseconds. Unit safety is worth it. |
| Array size 1K-10K | Full Pint Quantity is the default. Overhead is 5-10x but absolute cost is sub-millisecond. |
| Array size < 1K, called once | Full Pint Quantity. The 27-40x ratio is on microsecond base times -- irrelevant. |
| Scalar/tiny array in tight inner loop (millions of calls) | Raw NumPy with unit documentation in comments/docstrings. |
| Wrapping legacy non-Pint functions | @ureg.wraps() at the boundary. Not for new code. |
| New calculation modules | Always use Pint Quantity from `src/digitalmodel/units.py`. |

## Pint 0.25.x Notes

- `@ureg.wraps` with `"dimensionless"` in the input spec triggers an `AssertionError` (known Pint bug). Use `None` for dimensionless parameters and pass the raw float instead.
- Import `ureg` and `Q_` from `src/digitalmodel/units.py` to ensure a single shared registry.

## Reproduction

```bash
cd digitalmodel/
uv run python -m pytest tests/test_pint_benchmark.py -v   # correctness
uv run python scripts/integrations/pint_benchmark.py       # timing
```

## See Also

- [Pint evaluation](pint-evaluation.md) -- library overview and API patterns
- [UnitRegistry module](../../src/digitalmodel/units.py) -- shared `ureg` and `Q_`
- [Benchmark script](../../scripts/integrations/pint_benchmark.py)
- [Benchmark tests](../../tests/test_pint_benchmark.py)
