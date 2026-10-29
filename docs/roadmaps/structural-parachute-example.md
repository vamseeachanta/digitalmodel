# Structural capability roadmap: parachute drag example

This entry covers only `structural.parachute.parachute_drag`: a circular-area
drag calculation with a scalar opening factor. The executable example closes a
runnable-example gap; existing tests already cover conversions, area, drag,
speed scaling and selected invalid inputs. The example evaluates cited teaching
inputs for steady drag; it does not establish a parachute selection or deployment
peak loads.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-26 | Add an executable synthetic example and an independent smoke test to make the existing API reproducible. |
| 1 | 2026-10-27 | Replace synthetic inputs with cited teaching inputs and update the independent calculation so the input basis is traceable. |

## Current API and cited inputs

[`calculate_drag_force(speed_mph, chute_diameter_ft, cd, cx, rho)`](../../src/digitalmodel/structural/parachute/parachute_drag.py)
returns `DragResult`. Its `force_lbs` field is **pound-force (lbf)**, not mass;
`force_n` is newtons (N). Density is in slug/ft³, speed is supplied in mph,
diameter in ft, and `cd` and `cx` are dimensionless. `mph_to_fps` and
`chute_area` expose the conversion and circular-area helpers separately.

The input basis is NASA's [*Adventures in Rocket Science*, 2008 edition,
activity 23, part 2, "Model Rocket Parachute Size Calculation", printed page 124](https://files.eric.ed.gov/fulltext/ED511735.pdf#page=134).
That worked-example section lists density `1.22 kg/m³`, parasheet drag coefficient
`0.75`, desired landing speed `3 m/s` and a standard canopy diameter of `18 in`.
The calculation below evaluates that stated standard canopy at the stated speed;
it does not reproduce the section's separately printed sizing result. These are
teaching inputs, not measured data or design recommendations.

The source equation describes steady drag, so `cx = 1.0` introduces no opening
amplification. It is an equation mapping, not an empirical opening factor. The
second speed is derived by doubling the cited speed solely to demonstrate
squared speed dependence; it is not another sourced operating condition.
The source's SI density and speed are used without mixing its rounded customary
equivalents. Diameter is converted from `18 in` exactly, rather than from the
source's rounded metre equivalent.

From the repository root, use Python with `PYTHONPATH=src` and PyYAML installed.
The drag calculation uses the standard library; importing its package currently
also imports the assessment module, which requires PyYAML. No CAD or solver
software is required. Execute the following Python block unchanged:

```python
from digitalmodel.structural.parachute.parachute_drag import calculate_drag_force

# NASA, Adventures in Rocket Science (2008), activity 23, part 2, p. 124.
# NIST SP 811 (2008), Appendix B.8: inch, foot, mph and slug/ft^3 entries.
inputs = dict(
    chute_diameter_ft=18.0 * 0.0254 / 0.3048,
    cd=0.75,
    cx=1.0,  # Unity maps the source's steady-drag equation to this API.
    rho=1.22 / 515.3788,
)
speed_mph = 3.0 / 0.44704
baseline = calculate_drag_force(speed_mph=speed_mph, **inputs)
faster = calculate_drag_force(speed_mph=2.0 * speed_mph, **inputs)

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
6.7 mph = 9.8 ft/s; area = 1.8 ft^2; drag = 0.2 lbf (0.7 N)
13.4 mph = 19.7 ft/s; area = 1.8 ft^2; drag = 0.6 lbf (2.7 N)
force ratio = 4.0
```

## Independent hand calculation

The [NASA drag equation](https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/)
gives `D = rho × V² × Cd × A / 2`; the cited guide's page 124 sizing equation
rearranges this steady balance for circular diameter. This API additionally
multiplies by `cx`, held at unity here. For a circle,
`A = pi × (diameter / 2)²`, giving `A = (9/16) pi ft² = 1.8 ft²` for `18 in`.
The factor of one half comes from the drag equation; dividing diameter by two
gives the radius in the circular-area identity.

[NIST SP 811, 2008 edition, Appendix B.8, "Factors for Units Listed Alphabetically"](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8)
lists `1 in = 0.0254 m`, `1 ft = 0.3048 m`, `1 mph = 0.44704 m/s` and
`1 slug/ft³ = 515.3788 kg/m³`. The snippet retains these conversion factors at
their listed precision. The API inputs are therefore `18 × 0.0254 / 0.3048 ft`,
`3 / 0.44704 mph` and `1.22 / 515.3788 slug/ft³`. In the independent calculation:

- `Vslow = 3 / 0.3048 ft/s = 9.8 ft/s`; the doubled speed is `19.7 ft/s`.
- `ASI = pi × (18 × 0.0254 / 2)² m²`.
- `Fslow = (1/2) × 1.22 × 3² × 0.75 × ASI = 0.7 N = 0.2 lbf`.
- `Ffast = 4 × Fslow = 2.7 N = 0.6 lbf`.
- `Ffast / Fslow = (6/3)² = 4.0`: doubling speed quadruples force.

All displayed results are rounded after the calculations. The SI dimensions
reduce to `kg/m³ × m²/s² × m² = kg m/s² = N`. The customary dimensions reduce to
`slug/ft³ × ft²/s² × ft² = slug ft/s² = lbf`. NIST's pound-force entry gives
`4.448222 N/lbf`; the current API uses `4.44822 N/lbf`. The smoke test derives
forces directly in SI, then converts independently to lbf. A relative tolerance
of `1e-6` (one part per million) accommodates the rounded density and API force
conversion factors. Unrounded speed, area and force checks precede checks of the
displayed values and unit labels; rounded display values are not calculation
inputs.

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
| Executable API example and squared speed dependence | `test_documented_cited_drag_example` (added); existing `TestDragForce::test_force_scales_with_v_squared` | The smoke test executes this document's Python block and checks the cited inputs, forces, converted speeds, area and printed units. |

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
