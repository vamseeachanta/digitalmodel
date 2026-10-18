# Pipeline pressure-loss benchmark analysis

## Summary

The current rounded calculator is compared with two retained worksheet cases.
Friction-head differences are within 0.4 % for the 4.5-inch case and within
1.1 % for the 4.0-inch case, relative to cached worksheet values. These bounds
characterize existing rounding; they are not an engineering design criterion.
The calculation no longer writes an unrelated equation image to a fixed path.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-14 | Add real-calculator regression comparisons to expose the retained rounding difference and remove incidental plotting. |

## Basis and method

Inputs are **assumed, based on previous experience**. Both cases use a length
of 10,000 ft, acceleration of 9.81 m/s², absolute roughness of 0.045 mm, fluid
density of 15 ppg, viscosity of 60 cP and flow of 10 bbl/min. The bore diameters
are 4.5 in and 4.0 in. The reference worksheet's cached numerical values and
formula cells were read; the workbook was not recalculated by a spreadsheet
application. No source identity or original document is distributed here.

The existing implementation converts inputs to SI, rounds diameter, area,
velocity and friction factor, and returns Darcy friction head and total head.
The reference worksheet retains more precision. Its turbulent friction-factor
formula is the same Haaland form used by the existing calculator, followed by
Darcy head `h = f * (L / D) * U² / (2 * g)`. Both implementations choose the
turbulent expression above Reynolds number 3000. That transition choice is
retained, not independently qualified by this comparison.

## Results

| Bore (in) | Cached friction head (m) | Calculated friction head (m) | Difference (%) | Regression bound (%) |
| --- | --- | --- | --- | --- |
| 4.5 | 293.6 | 294.8 | +0.4 | 0.4 |
| 4.0 | 513.9 | 508.5 | -1.1 | 1.1 |

Table 1. Calculated head compared with cached worksheet head.

The signed difference is `100 * (calculated / cached - 1)`. Each bound is
`ceil(abs(difference) * 10) / 10`, evaluated using unrounded cached values before
presentation. Reynolds-number differences are bounded in the same manner by
0.5 % and 0.8 %, respectively. Potential head is 3048.0 m in both cases.

## Reproduction and limits

`tests/subsea/pipeline/test_pressure_loss_benchmark.py` calls `Pressureloss.run`
directly, checks head and Reynolds number against the retained values, and
requires that the calculation write no files. No mocked engine supplies the
expected result. Full-precision cached numbers remain in the test assertions;
the table above is rounded for presentation.

The tests run with the repository's project environment:

```sh
PYTHONPATH=src uv run --no-sources python -m pytest tests/subsea/pipeline/test_pressure_loss_benchmark.py -q -p no:cacheprovider
```

The test temporary directory is supplied outside the checkout by the caller.
The comparison covers two turbulent single-phase cases only. It does not
establish transitional, laminar, non-Newtonian or multiphase accuracy. The
calculator's intermediate rounding and existing output keys are unchanged;
pressure loss in MPa is still not returned. A future numerical revision should
establish its acceptance criteria and independent reference basis before
changing the preserved arithmetic.
