# Pipeline corroded-defect screen

Run offline from the repository root:

```bash
uv run python -m digitalmodel examples/workflows/pipeline-corroded-defect-screen/input.yml
uv run python -m digitalmodel examples/workflows/pipeline-corroded-defect-screen/synthetic.yml
```

The HTML comparison is written to `results/input-pipeline-defect-screen.html` or
`results/synthetic-pipeline-defect-screen.html`;
the engine also saves the indexed assessment in the output YAML. Both examples
use synthetic thickness grids, with no client or measured data. `input.yml` uses
the existing B31G reference geometry; `synthetic.yml` uses the regenerated
RSTRENG-2D validation-record grid. The 1183 / 1219 / 1334 psi reference pressures
belong to B31G / Modified B31G / DNV-RP-F101, respectively.

Lengths are inches; stresses and pressures are psi. Grid rows correspond to
strictly increasing axial positions, columns to circumferential UT readings.
Loss is nominal wall minus measured remaining thickness.
Above-nominal readings are rejected; mill-tolerance correction remains outside
this screen and this limitation is disclosed in the report.
The caller supplies
one single-defect assessment window and its total affected circumferential arc
width. The whole axial window, including any intact points, defines the
maximum-depth defect bound. Colony interaction is not inferred.

The four B31G-family pressure allowables divide failure pressure by the explicit
`safety_factor`; DNV uses the single-defect allowable-stress calculation with
caller `usage_factor` interpreted as operational F2.
DNV-RP-F101 Part B safe working pressure applies F = F1 x F2 with F1 = 0.9;
the examples use F2 = 0.72,
so F = 0.648. The circumferential row is a separate axial membrane
stress screen using SMYS as flow stress multiplied by the explicit caller
`axial_design_factor` in (0, 1]. It is not a burst-pressure calculation.
Loss at either axial boundary requires a strictly boolean `length_confirmed: true`
to establish the supplied full defect length; otherwise every method is flagged
and the verdict is ESCALATE. The synthetic reference example explicitly confirms
its defined 8 in length; no measured inspection-length evidence is implied.
The minimum allowable-to-demand ratio governs current-demand screening. A ratio
at or above 1 yields ACCEPT; a lower nonnegative ratio yields DERATE (reduce
every exceeded pressure/stress demand to its reported limit). No RSF severity
bands or replacement verdict are inferred from a demand ratio; any
raised applicability flag forces ESCALATE and makes every number reference-only.
RSTRENG and RSTRENG-2D(MAX) are expected to agree because the latter delegates to
the former after projection; this is not an independent 2D interaction solution.

Positive internal pressure and positive tensile membrane demand are required.
Zero demand, compression, bending, external pressure, crack-like defects and
combined-loading qualification are outside this workflow. Remaining life is not
evaluated from a single grid. Edition-matched API 579 Part 5 qualification is not
established; the report links the existing, bounded validation records.
