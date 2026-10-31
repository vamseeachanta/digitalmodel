# Dimensional anti-roll roll-reduction analysis

The `digitalmodel.hydrodynamics.vessel_roll_reduction` module calculates
single-frequency roll reduction versus conduit area from explicit vessel data
and externally calibrated tank impedance. It supplements the normalized
`utube_tank` model. It does not establish a real-vessel result for
[issue 1562](https://github.com/vamseeachanta/digitalmodel/issues/1562): the
confirmed loading condition and tank response are not supplied in the public
repository. That calculation remains **NOT_EVALUATED**.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-19 | Added dimensional coupling and a declared arithmetic example so unavailable vessel data cannot become an illustrative vessel result. |
| B | 2026-10-24 | Required a change reason for each report issue and rejected boolean and nonnumeric dimensional inputs to preserve provenance and input validity. |

## Inputs and method

`VesselRollBasis` requires displacement **mass in kg**, transverse GM in m,
**undamped** natural period in s (including added inertia), hull damping as a
fraction of critical, gravity in m/s², and source/condition provenance. No
engineering constants or damping ratios are defaulted.

The loading condition must match across all inputs. The baseline must include
frozen tank mass/inertia when the supplied tank response is incremental to
frozen contents. Tank free-surface and static contributions must not be counted
twice. These conventions are caller responsibilities, not automatic corrections.

The following public equations were read on 2026-10-19:

- `C = m g GM`: *Marine Craft Model*, Kinetics, surface-craft restoring matrix,
  [online edition](https://www.fossen.biz/html/marineCraftModel.html).
- `I = C / omega_n²`, `omega_n = 2 pi / T`: inversion of the single-mode
  period equation in *Modal analysis: Theory*, Outline theory,
  [online edition](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Modalanalysis,Theory.htm).
- `B = 2 zeta sqrt(I C)`: inversion of the critical-damping ratio in
  *Results: Roll damping*, Percentage of critical,
  [online edition](https://www.orcina.com/webhelp/OrcaWave/Content/html/Results,Rolldamping.htm).

The online references do not state an edition year; the access date identifies
the retrieved basis. No values from licensed publications are reproduced.

For `exp(+i omega t)`, the scalar harmonic equation is
`(C - I omega² + i omega B) phi = M_wave + H phi`.
Thus `Z0 = C - I omega² + i omega B`, `Z = Z0 - H`, and
`reduction (%) = 100 (1 - abs(Z0 / Z))` for identical wave moments.
These are direct algebraic reductions of the equation of motion in the first
source and the percentage definition. Negative values indicate amplification.

Each `TankImpedance` requires conduit area in m², forcing period in s,
complex tank-on-vessel moment per roll in N m/rad, linearization roll amplitude
in rad, and provenance. A damping moment has **negative imaginary part** in this
convention. The source must identify tank mass, reservoir separation, plan area,
fill, conduit type and effective length, calibration and tuning condition.
Geometry alone does not supply a tank impedance. All sweep points must have the
same frequency and amplitude and distinct areas; output is sorted by area.

## Worked arithmetic example

The following values are declared solely for an independent arithmetic check;
they are not a vessel dataset or an assumed real design. No empirical values
are used. Take `m=10 kg`, `g=10 m/s²`, `GM=1 m`, `T=2 pi s`, `zeta=0.1`.
Then `omega_n=1 rad/s`, `C=100 N m/rad`, `I=100 kg m²`, and
`B=20 N m s/rad`. At forcing frequency `1 rad/s`, `Z0=20i N m/rad`.
With tank moment/roll `H=-20i N m/rad`, `Z=40i N m/rad`. Therefore
`abs(phi_tank/phi_bare)=20/40=0.5`, giving **50.0 %** reduction.
A second supplied point `H=-60i` gives `20/80=0.25`, or **75.0 %** reduction.
These arbitrary impedance/area pairs demonstrate the interface, not an area law.

```python
import math
from digitalmodel.hydrodynamics.vessel_roll_reduction import (
    TankImpedance, VesselRollBasis, reduction_sweep, render_roll_reduction_html,
)

vessel = VesselRollBasis(
    displacement_mass_kg=10, gm_m=1, natural_period_s=2 * math.pi,
    hull_damping_ratio=0.1, gravity_m_s2=10,
    source="Declared arithmetic inputs; documentation worked example",
)
points = [
    TankImpedance(1, 2 * math.pi, -20j, 0.01, "Declared first arithmetic point"),
    TankImpedance(2, 2 * math.pi, -60j, 0.01, "Declared second arithmetic point"),
]
result = reduction_sweep(vessel, points)
assert [row.reduction_percent for row in result.rows] == [50.0, 75.0]
html = render_roll_reduction_html(
    result, revision="A", date="2026-10-19",
    change="Issued dimensional area sweep using declared arithmetic inputs",
)
# The caller may save this section in the analysis report that contains it.
missing = reduction_sweep(None, [])
assert missing.status == "NOT_EVALUATED"
```

Each report issue requires a `change` description stating what changed and why.
For a reissue, pass earlier `(revision, date, change)` rows through
`previous_revisions`; the renderer appends the current issue with its supplied
change description without changing those rows.

The HTML contains the input basis, sources, revision history, area/reduction
curve, table, missing evidence and limitations. Provenance strings are escaped;
callers must redact private identifiers before public publication. Missing data
produces no numeric rows; zero tank moment produces a true zero reduction.

## Limits and remaining work

This interface covers the dimensional vessel basis, impedance-based area sweep,
and report section. It does not reconstruct the private coupled model, generate
CFD data, tune geometry, iterate quadratic damping with response amplitude,
interpolate between areas, calculate a frequency-band peak or evaluate a sea
state. A prescribed-amplitude linearized result requires consistency with the
calibration amplitude before it can represent an operating point.

The real-vessel curve and replacement of report section 4.3 remain pending
confirmed inputs and calibrated impedances. The earlier transfer coefficient
is not used as a vessel-coupling law. No licensed solver is imported or queried.

Run the independent arithmetic, sign, missing-data, validation and report tests:

```bash
PYTHONPATH=src uv run python -m pytest tests/unit/hydrodynamics/test_vessel_roll_reduction.py
```
