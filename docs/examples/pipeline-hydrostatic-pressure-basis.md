# Pipeline hydrostatic pressure basis

This executable note maps a static fluid-column pressure head to the existing
[fluid-column helpers](../../src/digitalmodel/hydrostatics/fluid_column.py) and
[their tests](../../tests/hydrostatics/test_fluid_column.py). It establishes
arithmetic and reference-pressure conventions for a synthetic pipeline pressure
input. Pipeline-code acceptance is not evaluated.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-26 | Add a synthetic hand calculation and executable example to make the pressure basis and its limits explicit. |
| 1 | 2026-10-27 | State the bounded scope, independent oracle and executable-example check together for review. |

## Plan-lite

Establish only a synthetic static pressure-input basis. Verify it by an
independent unit-area force balance and execution of the documented example.
Leave pipeline-code acceptance and edition qualification outside this slice.

## Synthetic basis

All inputs below are synthetic choices for exact hand arithmetic, not site data,
material properties, standard gravity, or standard-derived test requirements.
This table is the input source for the example and its regression tests.

| Quantity | Value | Basis |
| --- | --- | --- |
| Density | 1000.0 kg/m³ | Synthetic constant density |
| Gravity | 10.0 m/s² | Synthetic acceleration, explicitly supplied to override the helper default |
| Top elevation | 20.0 m | Synthetic upward-positive coordinate |
| Bottom elevation | -55.0 m | Synthetic upward-positive coordinate on the same datum |
| Column height | 75.0 m | Top minus bottom: 20.0 - (-55.0) |
| Surface pressure | 120000.0 Pa absolute | Synthetic boundary pressure, supplied explicitly |
| Hand-calculation area | 1.0 m² | Synthetic unit area; cancels from pressure |

Table: Synthetic inputs and their derivation.

For a static, constant-density column under constant gravity, the helper relation
is `delta_p = density * gravity * height`. An independent force-over-area
calculation gives volume `1.0 * 75.0 = 75.0 m³`, mass
`1000.0 * 75.0 = 75000.0 kg`, and weight
`75000.0 * 10.0 = 750000.0 N`. Dividing by the unit area gives
**750000.0 Pa** pressure rise. Adding the supplied absolute surface pressure gives
`120000.0 + 750000.0 = 870000.0 Pa` absolute at the bottom.

Here, gauge pressure means pressure relative to the supplied surface pressure.
It is not a claim that the supplied surface pressure equals local atmospheric
pressure. `hydrostatic_pressure` returns the column pressure rise;
`seabed_pressure` adds the supplied surface pressure. Its result is absolute only
when that surface input is absolute. A zero surface input returns the column
head without implicitly adding atmospheric pressure.

## Datum convention

The height is a vertical separation, not pipeline length along its route or an
absolute elevation. With upward-positive elevations,
`height = top_elevation - bottom_elevation`. The tests use synthetic common datum
shifts of `0.0 m`, `250.0 m`, and `-400.0 m`. Adding any of these to both elevations
preserves the `75.0 m` separation and therefore both pressure results. Density,
gravity and surface pressure remain fixed during this comparison.

## Executable example

```python
from digitalmodel.hydrostatics.fluid_column import (
    hydrostatic_pressure,
    seabed_pressure,
)

# Source: Synthetic basis table in this note; all inputs are artificial.
density_kg_m3 = 1000.0
gravity_m_s2 = 10.0
top_elevation_m = 20.0
bottom_elevation_m = -55.0
height_m = top_elevation_m - bottom_elevation_m
surface_absolute_pa = 120000.0

gauge_pa = hydrostatic_pressure(
    density_kg_m3, height_m, gravity=gravity_m_s2
)
absolute_pa = seabed_pressure(
    density_kg_m3,
    height_m,
    surface_pressure=surface_absolute_pa,
    gravity=gravity_m_s2,
)

# Independent unit-area force balance above, not helper-generated expectations.
assert gauge_pa == 750000.0
assert absolute_pa == 870000.0
print(f"Column gauge pressure: {gauge_pa:.1f} Pa")
print(f"Bottom absolute pressure: {absolute_pa:.1f} Pa")
```

`TestPipelinePressureBasis::test_documented_example_executes` reads and executes
this exact fenced example. The other added cases compare both helpers against
the hand calculation and check common datum shifts. Existing unit-conversion,
worksheet and vector tests remain in place. From the repository root, with a
writable temporary directory supplied through `TMPDIR`, run:

```bash
PYTHONPATH=src uv run --no-project --with pytest --with numpy python -m pytest tests/hydrostatics/test_fluid_column.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/bt-fluid-column"
```

## Repository standards context

The [codes register](../domains/codes-register.md), under pipe wall thickness and
pressure containment, records DNV-ST-F101, API RP 1111, ASME B31.4 and ASME B31.8
and points to their separate implementations and tests. The
[pipeline overview](../domains/pipelines/_subsea_pipelines.md), under Structural
Design, also lists references for free span, on-bottom stability, buckling and
cathodic protection. These repository entries are context only: no publisher
source or licensed standard was consulted for this note. Current editions,
amendments, the DNV-OS-F101 versus DNV-ST-F101 designation, and applicability of
those references remain **unverified here**. No edition is promoted to verified
or latest status by this calculation.

The generic pressure-head relation supplies a possible load input; it does not
select an allowable pressure, test factor, wall thickness, duration, resistance
model, or pipeline-code acceptance limit. Internal and external pressures need
separate physical bases and consistent references before any differential load
can be assessed. Variable density or gravity, flow losses, transients, temperature
effects and structural response are outside this constant-column example.

## Remaining scope and way forward

Parent issue #1813 remains open. The complete standards inventory still needs
publisher, current revision, title, scope summary, key methods and applicability
boundaries for pipeline design, installation, on-bottom stability, free span,
buckling, corrosion and cathodic protection, and pre-commissioning. The applicable
editions and design basis need qualification before using code-specific criteria.
This note supplies only an executable hydrostatic pressure input basis and makes
no inventory-wide compliance claim.
