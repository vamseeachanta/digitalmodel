# Damaged jacket member diagnostic example

This synthetic example runs the existing bounded dent/bow prediction,
uniform-wall geometry and stress diagnostics, and severed-member handoff.
All shared fitness decisions are **ESCALATE** and
`engineering_acceptance_established` is `false`. Numerical predictions and
registry discovery do not establish engineering acceptance.

## Run

From the repository root, with the package and its dependencies installed:

```sh
uv run python -m digitalmodel.structural.jacket_topside.damaged_member_screen \
  examples/workflows/jacket-damaged-member-screen/input.yml \
  --output examples/workflows/jacket-damaged-member-screen/results/screen.json
```

The command writes strict JSON to `results/screen.json` beside this guide and
the sibling analysis to `results/screen.html`. These are runtime outputs,
not committed reference results. The HTML is also retained as `report_html`
inside the JSON. To keep outputs elsewhere, supply a different `.json` path
to `--output`; the HTML follows that path with a `.html` suffix.

The [workflow registry](../../../docs/registry/workflows.yaml) records
`jacket-damaged-member-screen` version 1 as `experimental`, `latest: false`,
and `runtime: standalone`. Use its explicit module invocation. General engine
dispatch and catalogue promotion are outside this registration.

## Input basis and units

The three cases in [input.yml](input.yml) are synthetic, not inspection
measurements or an experimental test series. They are assessed independently;
their results do not establish platform reserve strength.

Dent/bow inputs are dimensionless ratios with explicit support, loading and
bow-reference conditions. The ratio of diameter to thickness uses midwall
diameter; bow is referenced to the plastic neutral axis. Uniform-wall inputs
use inches for geometry and effective length, kips for force, kip-inches for
moment, and ksi for yield strength and elastic modulus. Remaining wall is an
explicit representative thickness with unchanged outside diameter; a minimum
reading is not substituted for an average. There is no automatic unit conversion.

## Qualification gaps

Published experimental golden validation and qualified member/joint strength
checks remain missing. Dent reductions for applied bending and pressure,
localized corrosion, cracks, fatigue, and combined corrosion/dent strength
remain outside the bounded calculation. Corrosion outputs are geometry and
stress diagnostics, not qualified member capacity checks.

Flooded/severed-member escalation is workflow policy, not a standards-derived
assessment-level mapping or computed pushover/reserve-strength result.
`solver_executed` remains `false`; no solver is executed. API RP 2SIM and
ISO 19902 engineering acceptance remains **NOT_EVALUATED**.

See the [method and limitations](../../../docs/domains/asset-integrity/jacket-damaged-member-screen.md)
for the existing source basis, study envelope and qualification inventory.
