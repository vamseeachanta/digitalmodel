# Local metal-loss spatial-profile audit

The legacy local metal-loss calculation retains spatial adjacency and integrates
contiguous intervals of the row-minimum thickness profile. Each saved pressure
estimate now includes its governing interval and the matching RSF and Folias
factor. This geometric correction does not qualify the pressure calculation as
an edition-specific API 579 Level 2 assessment: qualification remains
`NOT_EVALUATED`. No equipment disposition follows from these results.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-22 | Retained interval evidence, documented the caller audit and added a worked example so saved estimates can be traced to measurements. |

## Method and source

`API579_components.LMLLevel2Intervals(grid)` accepts strictly increasing numeric
longitudinal row coordinates and nonnegative finite measured walls. Columns
represent circumferential planes; the critical wall at each row is the minimum
across columns. The minimum can switch between planes. Coordinates and walls
must use the same length unit; the function performs no unit conversion.

For every pair of measured endpoints `i < j`, in start-major then end-major
order, the interval includes all intervening readings:

```text
L[i,j] = x[j] - x[i]
A[i,j] = sum((x[k+1] - x[k]) * (t[k] + t[k+1]) / 2, k=i..j-1)
t_mean[i,j] = A[i,j] / L[i,j]
```

The integration source is [NIST Digital Library of Mathematical Functions
(2010), §3.5(i), Eq. 3.5.1](https://dlmf.nist.gov/3.5.E1), applied to each
segment. The divisor 2 averages the two segment endpoints. Integration is exact
for the assumed piecewise-linear profile, including unequal segment lengths;
interpolation between sparse readings remains an assumption. No thickness sort,
extrapolated pitch, periodic circumferential interval or singleton interval is
introduced. Missing readings, duplicate coordinates and reversed coordinates are
rejected rather than silently joined or reordered.

Each record contains zero-based positions within the supplied grid, original
endpoint coordinates, length, integrated wall area and measured mean wall.
Positions within an extracted local thin area are relative to that extraction;
coordinates retain their original labels. Area uses squared length units.
`LMLLevel2Evaluation(grid, data)` remains a compatibility wrapper returning the
same mean-wall and length lists. Both methods use the same interval calculation.

## Worked example

The following dimensions are synthetic inputs in one consistent length unit,
not standard coefficients or measured equipment data. The two columns switch
which plane supplies the minimum:

| Coordinate | Plane A wall | Plane B wall | Critical wall |
| --- | --- | --- | --- |
| 5 | 2 | 8 | 2 |
| 7 | 9 | 6 | 6 |
| 12 | 10 | 12 | 10 |

The first segment has area `(7-5)*(2+6)/2 = 8`; the second has area
`(12-7)*(6+10)/2 = 40`. The complete profile therefore has area `48`, length
`7` and mean wall `48/7`, displayed as `6.9`. The complete interval enumeration
is:

| Start position | End position | Start coordinate | End coordinate | Length | Area | Mean wall |
| --- | --- | --- | --- | --- | --- | --- |
| 0 | 1 | 5 | 7 | 2 | 8 | 4.0 |
| 0 | 2 | 5 | 12 | 7 | 48 | 6.9 |
| 1 | 2 | 7 | 12 | 5 | 40 | 8.0 |

```python
import pandas as pd
from digitalmodel.asset_integrity.common.API579_components import API579_components

grid = pd.DataFrame([[2, 8], [9, 6], [10, 12]], index=[5, 7, 12])
intervals = API579_components({}).LMLLevel2Intervals(grid)
for interval in intervals:
    print(interval)
```

The independently written hand calculations are asserted in
`tests/asset_integrity/test_lml_spatial_profiles.py`. That suite also distinguishes
adjacent `[2,2,10]` from separated `[2,10,2]` readings at unit pitch: complete
means are `4.0` and `6.0`, respectively. A thickness sort would erase that
difference. Linear-segment refinement, endpoint inclusion and coordinate
translation are checked separately.

## Governing evidence and future corrosion allowance

`LMLAcceptability` evaluates the legacy pressure routine for each measured
interval. `Level 2 governing interval` selects the minimum calculated pressure;
equal pressures retain the first interval in enumeration order. Its `MAWPr`,
`RSF` and `Mt` belong to that same interval. The record includes `FCA` and
`adjusted_mean_wall = mean_wall - FCA`; measured `area` and `mean_wall` remain
unchanged. This subtraction is bookkeeping for the existing caller, not
qualification of its corrosion model.

For compatibility, `MAWPr, L2`, `RSF, L2` and `Mt, L2` retain their existing
independent minima. They can refer to different intervals. The new record is
required when associating pressure with a geometry or factor. It survives
built-in type conversion and summary rounding without rounding its evidence.
Workflow tests use dummy pressure outputs with deliberately different minima
and tied pressures; those outputs are selection tests, not engineering
reference values.

## Caller and saved-result audit

The production chain is `API579.API579` → `lml` → `API579LML` →
`LMLAcceptability` → `LMLLevel2Intervals`. The legacy compatibility wrapper has
no other production callers in the package. The newer
`assessment.level2_engine.Level2Engine` is separate and does not call this
routine. The standalone `examples/asset_integrity/example_api579_lml.py`
reimplements illustrative formulas; it is not an independent qualification
reference for the interval calculation.

Affected outputs are `Result.LML`, summary tables and exports containing
`MAWPr, L2`, `RSF, L2` and `Mt, L2`. Durable workflows
`api579-pipe-ffs-b314` and `api579-pipe-ffs-b318` exercise the legacy chain.
Historical outputs from the sorted-profile implementation require reassessment
from raw coordinate/wall grids, with the generating revision established first.
The tracked YAML/JSON/CSV search found no saved legacy L2 pressure/RSF records;
this does not establish an inventory of external results. Existing GML fixtures
are not LML evidence. No external saved result is reassessed here.

## Qualification and way forward

API 579-1/ASME FFS-1, Fitness-For-Service, 2016, Part 5 (local metal loss),
Required Data/Measurements for a FFS Assessment, is the existing legacy
procedure citation. The applicable licensed edition and independent
edition-matched worked examples have not been reviewed here. Geometry hand
calculations establish integration behavior only. The legacy pressure
coefficients, acceptance thresholds, Folias table coverage, background wall,
Level 1 extent convention and applicability checks remain outside this change.

Before numerical acceptance, the maintainers need the applicable procedure and
permitted source evidence, an independent pressure/RSF benchmark and a review
of the affected historical outputs. Until then the saved qualification remains
`NOT_EVALUATED`, regardless of raw pressure or RSF values.
