# Cathodic protection analysis: distribution and deposition sensitivity

The [worked YAML example](screening_and_calcareous.yml) adds a geometric
current-distribution screen and a calcareous demand comparison to the existing
single-family offshore route. Both are optional. Existing results, installed
counts and adequacy verdicts keep their reference basis. The comparison does
not adopt a reduction in the design.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-26 | Added synthetic geometry and demand sensitivity to demonstrate the optional calculations and their limits. |
| 1 | 2026-10-26 | Required a supplied reach and engineering basis for a geometric PASS so illustrative limits remain flagged. |

## Current distribution

Set `inputs.current_distribution` to enable the screen. Its `zones` mapping uses
the existing structure zone identifiers. Every zone drawing positive CP current
is screened; zones drawing no CP current are excluded. Missing anode positions
or governing points produce `FLAG` and an unknown governing margin. Unknown
zone identifiers and malformed coordinates are rejected.

Each zone supplies `anode_positions_m` and `governing_points_m`: lists of `[x, y, z]`
coordinates in metres in the same Cartesian frame. Supply every surface point
or an engineer-selected set of governing points, including remote extremities.
The screen does not generate a mesh or infer that a supplied point set covers
the complete surface. Anodes are local to their assigned zone; a neighbouring
zone's anode is not credited automatically.

`reach_m` is a positive finite input, globally or per zone. A geometric `PASS`
requires an explicit reach and a nonblank text `reach_basis` describing its
engineering criterion and source. The basis is recorded as supplied; the screen
does not independently validate its engineering applicability. Zones inherit
both global values unless overridden. A zone reach override requires its own
`reach_basis`; a basis for a different global reach is not reused.

The documented default is **4.0 m**, half the **8.0 m** example
spacing in the repository's [ship workflow fixture](../../tests/fixtures/cathodic_protection/workflow_inputs/ships.yml).
This is an illustrative geometric scale from existing repository data, with
no validated offshore electrical reach implied. It is not a code-derived
criterion: **user to confirm against the governing code**. The worked example
explicitly supplies **5.0 m**, assumed, based on previous experience. Omitted
reach or basis produces `FLAG` pending engineering confirmation, even where
the illustrative margins meet the numeric limits. The worked example supplies
no engineering basis and therefore remains flagged.

For every supplied governing point, the screen records the nearest local anode,
its straight-line distance and `reach − distance`. Adjacent anode spacing is
the longest edge of a Euclidean minimum spanning tree. This detects separated
clusters even when each anode has a close neighbour, while allowing a long
array of adequately spaced anodes. The spacing limit is `2 × reach`, following
the geometric overlap convention already used by the pipeline route in
`src/digitalmodel/cathodic_protection/engine_adapter.py`; no pipeline electrical
reach is transferred to the offshore calculation. The spacing margin is
`2 × reach − longest edge`. For a single anode, spacing is zero and reach
still governs coverage. The zone margin is the smaller of the reach and
spacing margins. A zero margin meets the geometric limit; any negative margin
flags. A `PASS` also requires the explicit reach and basis described above.
Verdicts use unrounded values. Report distances and margins use three significant
figures; reach inputs and coordinates retain their supplied precision.

`results.current_distribution` contains the per-zone `PASS` or `FLAG`, signed
margins, effective reach basis, reasons and individual point checks. A geometric PASS does not resolve
shielding, obstructions, electrolyte paths, potential distribution or continuity.
Review the governing points, reposition or add anodes where flagged, and confirm
protection by engineering assessment and field measurements.

The generated report includes an engineer-completed electrical-continuity
checklist for bonding of appurtenances, anode connections and isolation joints.
It assigns no computed continuity verdict.

## Calcareous demand phases

Set `inputs.calcareous` to enable the comparison. Supply either a scalar `factor`
for all phases or `phase_factors` containing exactly `initial`, `mean` and `final`.
Every factor must be finite and in `(0, 1]`. The default is **1.0** for every
phase, taking no additional credit. Unknown inputs are rejected. The worked
example's factors **1.0, 0.5, 0.8** are assumed, based on previous experience;
they illustrate sensitivity and require engineering confirmation.

For submerged and tidal zones, each factor multiplies the corresponding existing
bare-steel current density before applying area and that phase's coating
breakdown. Buried and concrete-embedded zones retain their reference densities.
Connected-current allowances also retain their reference values. The mean
factor represents effective lifetime-average demand reduction; it is supplied
by the engineer and is not interpolated from the endpoints. No deposition
kinetics or time-dependent curve is inferred.

`results.calcareous` reports both cases' phase densities and demands by zone,
total initial, mean and final demand, net anode mass and mass-only anode count.
Mass uses the existing kernel:

`mean current × design life × 8760 / (capacity × utilisation)`.

The report labels the cases **Without calcareous** and **With calcareous**.
Without calcareous means no *additional* reduction on the existing density
basis. Existing phase densities may already reflect calcareous deposits; this
comparison cannot establish a deposit-free physical baseline. Avoid double
counting and confirm any adopted factor against evidence and the governing code.
Mass-only counts do not replace the initial/final current-output checks.

## Worked calculation and way forward

The example uses **500.0 m²**, **25.0 years**, the existing zone-density and
coating routes, **200.0 kg** individual net anode mass and **0.85** utilisation.
No external source or standard document was consulted for these supplements.

| Case | Initial demand (A) | Mean demand (A) | Final demand (A) | Net anode mass (kg) | Mass-only count |
| --- | --- | --- | --- | --- | --- |
| Without additional reduction | 2.0 | 8.5 | 20.8 | 1095.0 | 6 |
| With illustrative factors | 2.0 | 4.2 | 16.6 | 547.5 | 3 |

The three illustrative anode positions have adjacent spacing **10.0 m** and
maximum governing-point distance **5.0 m**. Both margins are **0.0 m**, meeting
the illustrative geometric limits. The screen returns `FLAG` because an
engineering reach basis is missing. These positions describe the screen only; the reference
mass-only selection of **6** anodes fails the final current-output check; the
reference route recommends **18** anodes. A reduction in mean demand does not
establish that three installed anodes meet the final output demand. Develop and
screen the physical layout for the adopted count, then repeat the output checks.

The maintainer and engineer should confirm reach, surface coverage and deposition
assumptions, complete the continuity checklist, and recheck installed anode output
before adopting a design change. Multi-component, mixed-family, phased and other
calculation routes reject these inputs explicitly rather than ignoring them.

## Run through the existing workflow

Load the YAML and use the existing adapter and report builder:

```python
from pathlib import Path
import yaml

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.reporting import render_html

source = Path("examples/cathodic_protection/screening_and_calcareous.yml")
cfg = run_cathodic_protection(yaml.safe_load(source.read_text(encoding="utf-8")))
html = render_html(anode_design_report(cfg))
```

The engine uses the same `basename: cathodic_protection` routing. The example
test executes YAML loading, both calculations, report assembly and HTML rendering.
