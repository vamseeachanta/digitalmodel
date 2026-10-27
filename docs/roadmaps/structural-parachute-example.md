# Structural capability roadmap: parachute drag example

This entry covers only `structural.parachute.parachute_drag`: a circular-area
drag calculation with a scalar opening factor. The executable example closes a
runnable-example gap; existing tests already cover conversions, area, drag,
speed scaling and selected invalid inputs. It is a synthetic arithmetic example,
not a parachute selection or a prediction of deployment peak loads.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-26 | Add an executable synthetic example and an independent smoke test to make the existing API reproducible. |

## Current API and synthetic inputs

[`calculate_drag_force(speed_mph, chute_diameter_ft, cd, cx, rho)`](../../src/digitalmodel/structural/parachute/parachute_drag.py)
returns `DragResult`. Its `force_lbs` field is **pound-force (lbf)**, not mass;
`force_n` is newtons (N). Density is in slug/ft³, speed is supplied in mph,
diameter in ft, and `cd` and `cx` are dimensionless. `mph_to_fps` and
`chute_area` expose the conversion and circular-area helpers separately.

All example inputs are deliberately synthetic definitions: density
`rho = 0.002 slug/ft³`, drag coefficient `cd = 1.0`, opening factor `cx = 1.5`,
diameter `10.0 ft`, and speeds `30.0` and `60.0 mph`. They are not measured data
or recommended design values. The area, speeds and forces below are derived
from these definitions in the independent hand calculation.

From the repository root, use Python with `PYTHONPATH=src` and PyYAML installed.
The drag calculation uses the standard library; importing its package currently
also imports the assessment module, which requires PyYAML. No CAD or solver
software is required. Execute the following Python block unchanged:

```python
from digitalmodel.structural.parachute.parachute_drag import calculate_drag_force

# Synthetic definitions from Current API and synthetic inputs above.
inputs = dict(chute_diameter_ft=10.0, cd=1.0, cx=1.5, rho=0.002)
baseline = calculate_drag_force(speed_mph=30.0, **inputs)
faster = calculate_drag_force(speed_mph=60.0, **inputs)

for result in (baseline, faster):
    print(
        f"{result.speed_mph:.1f} mph = {result.speed_fps:.1f} ft/s; "
        f"area = {result.chute_area_ft2:.1f} ft^2; "
        f"drag = {result.force_lbs:.1f} lbf ({result.force_n:.1f} N)"
    )
print(f"force ratio = {faster.force_lbs / baseline.force_lbs:.1f}")
```

Expected output (derived below, rounded to one decimal place):

```text
30.0 mph = 44.0 ft/s; area = 78.5 ft^2; drag = 228.1 lbf (1014.5 N)
60.0 mph = 88.0 ft/s; area = 78.5 ft^2; drag = 912.3 lbf (4058.2 N)
force ratio = 4.0
```

## Independent hand calculation

The [NASA drag equation](https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/)
gives `D = rho × V² × Cd × A / 2`; this API additionally multiplies by `cx`.
For a circle, `A = pi × (diameter / 2)²`, hence `A = 25 pi ft² = 78.5 ft²`.
The factor of one half comes from the drag equation; dividing the diameter by
two gives the radius in the circular-area identity.

[NIST SP 811, Appendix B.8](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8)
lists `1 mph = 0.44704 m/s` and `1 ft = 0.3048 m`. Their quotient is
`22/15 ft/s per mph`, giving exactly `44 ft/s` and `88 ft/s` for the synthetic
speeds. Therefore, without using the production conversion or area helpers:

- `Fslow = (1/2) × 0.002 × 44² × 1.0 × (25 pi) × 1.5 = 72.6 pi lbf = 228.1 lbf`.
- `Ffast = (1/2) × 0.002 × 88² × 1.0 × (25 pi) × 1.5 = 290.4 pi lbf = 912.3 lbf`.
- `Ffast / Fslow = (60/30)² = 4.0`: doubling speed quadruples force.

The dimensions reduce to `slug/ft³ × ft²/s² × ft² = slug ft/s² = lbf`.
NIST's pound-force entry gives `4.448222 N/lbf`. The current API uses the rounded
constant `4.44822 N/lbf`; both yield the displayed newton values. The smoke test
uses the NIST value independently and allows a relative difference of `1e-6`
(one part per million) solely to accommodate that rounding. It checks the
unrounded forces as well as the displayed values and unit labels.

## Capability, tests and remaining issues

The existing suite is
[`tests/structural/parachute/test_parachute_drag.py`](../../tests/structural/parachute/test_parachute_drag.py).
Before this entry, it collected **22 tests: 22 passed, 0 failed, 0 skipped**.
With the example smoke test, it collects **23 tests: 23 passed, 0 failed,
0 skipped**. Existing assertions and tolerances are unchanged. The assessment
suite that also calls this module passes **26 tests**. These counts describe
the focused runs, not coverage of the entire structural package.

To reproduce the drag run with temporary output outside the checkout, set
`TMPDIR` to an external writable directory, then run:

```sh
PYTHONPATH=src uv run --no-project --with pytest --with pyyaml --with pytest-asyncio python -m pytest tests/structural/parachute/test_parachute_drag.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/parachute-example"
```

| Implemented capability | Existing test mapping | Gap addressed or remaining |
| --- | --- | --- |
| mph to ft/s; circular projected area | `TestUnitConversions`, `TestChuteArea` | The new example checks conversion and area against independent constants. |
| Drag, scalar opening factor, lbf and N results | `TestDragForce`, `TestCdSensitivity`, `TestChatGPTCrossValidation` | Existing hand-calculation helpers reuse production helpers; the new smoke test independently derives expected values without changing existing assertions. The legacy comparison tests are retained, not treated as experimental evidence. |
| Executable API example and squared speed dependence | `test_documented_synthetic_drag_example` (added); existing `TestDragForce::test_force_scales_with_v_squared` | The smoke test executes this document's Python block and checks forces, converted speeds, area and printed units. |

Existing open issues were checked on 2026-10-26:

- [#1697](../../../../issues/1697) remains open: the full structural inventory
  and the OrcaWave/OrcaFlex, hydrodynamics and pipeline roadmaps are still due.
  This entry covers only the parachute drag module.
- [#214](../../../../issues/214) and [#1666](../../../../issues/1666) track the
  wider frame-load work. This drag example does not establish member forces,
  attachment reactions, connection capacity or a verified deployment load.
- [#1677](../../../../issues/1677) tracks a deployment dynamics template and
  time histories. A constant `cx` in this module does not resolve inflation,
  snap loads or a time-dependent response.

Next steps are the remaining roadmap inventory and the engineering evidence
tracked by those issues. No solver integration, empirical coefficient validation
or wider input-validation policy is added by this example.
