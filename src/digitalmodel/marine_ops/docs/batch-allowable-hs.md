# Batch allowable significant wave height

`allowable_hs_table` returns one pandas row per operation, peak period and
requested heading, and optionally writes the same table to CSV. It reuses
the [envelope calculation basis](envelope-basis.md) and the installation
module's complex crane-tip transfer. All supplied criteria apply in every cell.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-27 | Add a batch table for planning several operations over periods and headings. |

## Inputs and example

Supply one `RAOSet`, a nonempty list of `BatchOperation` objects, positive
`tp_range_s`, and a heading grid in degrees. Each operation has a unique `key`,
nonempty `criteria`, and `alpha` in `(0, 1]` (default 1). Each `ResponseCriterion`
has a unique label within its operation, a DOF, positive `max_significant`,
an explicit unit, and a response type:

| Response | Translation unit | Rotation unit | Response moment |
| --- | --- | --- | --- |
| `motion` | `m` | `deg` | m0 |
| `velocity` | `m/s` | `deg/s` | m2 |
| `acceleration` | `m/s^2` | `deg/s^2` | m4 |

Without `point_m`, the criterion uses the RAO reference origin. An optional
`point_m=(x, y, z)` offset in metres uses the existing installation crane-tip
convention, including rotational phase and conversion from degrees to radians.
Point criteria support translations only and require all six DOFs with finite
phases and identical frequency and heading grids. RAO units are m/m or deg/m;
frequency is rad/s. The existing envelope's non-square array contract applies.

```python
from digitalmodel.marine_ops.batch_envelope import (
    BatchOperation, ResponseCriterion, allowable_hs_table,
)

operations = [BatchOperation("lift", (
    ResponseCriterion("tip_heave", "heave", 1.0, "m", point_m=(10, 0, 20)),
    ResponseCriterion("tip_speed", "heave", 0.5, "m/s",
                      response="velocity", point_m=(10, 0, 20)),
), alpha=0.9)]
table = allowable_hs_table(
    rao_set, operations, tp_range_s=[8, 12], headings=[0, 90],
    spectrum_type="jonswap", gamma=3.3, csv_path="allowable-hs.csv",
)
```

`spectrum_type` accepts `jonswap` and `pierson_moskowitz`; the latter uses
the same generator with gamma=1. JONSWAP accepts finite gamma >= 1. Hs is evaluated at a 1 m reference
height and scaled linearly. For significant response `s_ref`, each criterion
gives `Hs_limit = alpha * C / s_ref`. A constant motion gain G gives
`s_ref = G/2`, so `Hs_limit = 2 * alpha * C / G`. The minimum governs each cell.
Ties retain criterion order. The cap defaults to 20 m and is configurable.

Run the complete synthetic example from the repository root:

```sh
PYTHONPATH=src python examples/marine_ops/batch_allowable_hs.py allowable-hs.csv
```

It uses the envelope tests' constant-gain synthetic RAO grid for lift,
splash-zone lowering, set-down and transit. Its criteria are illustrative.

## Output and limits

Rows retain operation, period and heading input order. `hs_limit_m` is the
alpha-reduced forecast limit. `governing_criterion` identifies the criterion
for that cell. `heading_deg` retains the requested heading;
`governing_heading_deg` records the selected RAO heading. Circular nearest
heading selection uses the existing bounded mismatch rule; it does not
interpolate headings or infer symmetry.

`status` is `evaluated`, `capped`, or `insufficient_coverage`. Capped rows carry
the label `cap` and a null governing heading; the Hs value is a bound rather
than a physical maximum. A required response with more than 1% of its retained
moment outside the RAO band produces a null Hs, label `coverage` and null
governing heading. Other cells continue. The check includes criteria that do
not govern. CSV represents null values as empty fields.

The table also records alpha, spectrum type, effective gamma, the largest
extrapolated response-moment fraction, captured wave variance fraction,
`criterion_type="forecast_limit"`, and `qualification_status="NOT_EVALUATED"`.
Numerical evaluation does not qualify operating limits or alpha selection.

Velocity and acceleration reuse the spectral moment weighting on the existing
finite integration band (0.01 to 8 times peak frequency). Captured wave variance
does not establish convergence of higher response moments. In particular,
acceleration is sensitive to high-frequency RAOs and the spectral tail;
bandwidth and convergence need assessment for the intended use. Held-edge
continuation assumes no unseen resonance. The wrapper covers linear vessel
or point responses; it does not add slamming, load, clearance, relative-wave,
or nonlinear installation criteria.
