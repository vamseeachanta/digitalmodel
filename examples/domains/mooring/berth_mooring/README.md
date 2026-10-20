# Generic berth mooring worked example

## Summary

This capability prepares a generic berth mooring review package: explicit input
bases, wind/current resultants, passing-force histories, twelve native review
models, a case table and a worked analysis in the standard HTML report form.
It also reduces exported line tensile and fender compression magnitudes against
supplied allowables. Coupled responses are `NOT_EVALUATED` in the example.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 00 | 2026-10-20 | Initial solver-free preparation and worked example to expose the method and pending response. |

## Inputs and method

Use [the generic input](inputs.yml).
Every physical value is **assumed, based on previous experience**. Its `basis`
mapping applies to all numerical leaves in each input group. No physical value
comes from the provenance-unknown mooring/fender group. The existing vendor
fender model was inspected for object conventions only; its data are not
redistributed. The existing passing sample informed the workflow structure,
without contributing numerical parameters.

- Steady wind/current screen: `F = rho Cd A |v| v / 2`,
  `Mz = x Fy - y Fx`. The [NASA Drag Equation](https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/)
  (public web edition, retrieved 2026-10-20) supplies the dynamic-pressure
  relation. Area, coefficient, density and application point are required inputs.
  A single coefficient applies to the resultant; this is an isotropic screen.
- Passage kinematics: `stagger = initial_stagger + speed * time`, with explicit
  sampling times and speed through still water. The force calculation reuses
  `src/digitalmodel/hydrodynamics/wang_passing_ship.py`, whose equations and
  finite-depth image method are documented in
  [the 1975-method note](../../../../docs/domains/hydrodynamics/wang-passing-ship-forces.md).
  The documented paper is *Dynamic Effects of Ship Passage on Moored Vessels*
  (1975); the repository note also identifies the 2003 paper's equations 2.4–2.9.
  The original papers were not newly retrieved for this example. No new force
  coefficient or acceptance limit is attributed to them.
- Tension-only lines: `T = EA (l-l0)/l0` when `l > l0`, otherwise zero.
  Compression-only fenders: interpolate supplied length/tension points;
  negative tension denotes compression. Both follow
  [Orcina Links: Data](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Links,Data.htm)
  (public web edition, retrieved 2026-10-20). The line stiffness is **EA in kN**,
  not EA/l0. These links omit line mass, sag and bending; they are not resolved
  catenary mooring lines. Fender tables finish with zero force and zero slope
  so extension beyond engagement does not create tensile reaction.
- Stored results: `utilisation = sampled peak / supplied allowable`.
  PASS means `peak <= allowable`, by definition of an allowable. Each criterion
  requires its own source; no safety factor or licensed-standard limit is assumed.

Axes are x along the berth, +y toward the passing track and z upward. Positive
yaw is right handed. The quay and all link endpoints are at y <= 0. Wind/current
velocities point **toward** their force direction, rather than meteorological
“from” directions. Loads are superposed in the initial fixed body frame.

## Worked answer and use

The example assumes air density 1.2 kg/m³, wind area 1000.0 m², coefficient 1.0
and speed 10.0 m/s: `0.5 × 1.2 × 1000 × 1 × 10² = 60000 N = 60.0 kN`.
Current density 1000.0 kg/m³, area 200.0 m², coefficient 1.0 and speed 1.0 m/s
give `0.5 × 1000 × 200 × 1 × 1² = 100000 N = 100.0 kN`.
Combined sway is **160.0 kN**, with **1600.0 kN m** yaw at x = 10.0 m.
These are applied loads, not line tensions or fender reactions.

From the repository root, select an output directory outside the checkout:

```sh
PYTHONPATH=src uv run --no-project --with numpy --with scipy --with pandas \
  --with pyyaml --with pydantic --with plotly --with jinja2 \
  python -m digitalmodel.hydrodynamics.berth_mooring_report \
  examples/domains/mooring/berth_mooring/inputs.yml "$OUTPUT_DIR"
```

Outputs are `report.html`, `case-table.csv`, twelve native review `.yml` files,
twelve SI force histories and `manifest.json` with input/artifact digests. The
case table crosses three explicitly declared passages with four environmental
combinations: calm, current, wind and combined. Passage speeds are 1.0 and
2.0 m/s; centreline separations are 100.0 and 150.0 m. All are assumed example
values. Native files use tonnes and kN; histories use N and N m. The conversion
uses kilo = 10³ (BIPM SI Brochure, ninth edition, 2019, SI prefixes).

Public source retrieval records are in [sources.json](sources.json).
Artifact paths in the manifest are relative to the reproducible build output.

The committed [worked report](report.html) records
the sampled applied-load peaks. Rebuilding creates the full review model set;
these models are structurally validated, not solver-load-attested. Native wind
and current are disabled: CSV loads require an applied-load adapter before use.

Stored-result reduction is available as `evaluate_results(spec, stored)` in
`digitalmodel.hydrodynamics.berth_mooring`. `stored` contains a nonempty
`solver_version` string, `input_sha256` copied from the matching manifest, and `cases`, keyed by the exact generated case names.
Each case maps every line/fender name to a nonempty list of nonnegative load
magnitudes in kN. Convert signed native fender compression to its positive
magnitude during export. Missing cases/objects, nonfinite data and negative
magnitudes are rejected. Missing criteria leave the result `NOT_EVALUATED`. The optional `--results` JSON
argument includes the reduced loads in the report; it never invokes a solver.
The digest binds the full input, including criteria; changing criteria requires
an explicitly updated export record. Digest agreement checks consistency, not
authenticity or physical qualification.

## Qualification boundary and way forward

No licensed solver is started, imported or queried. Native review files still
need applied-load wiring, hydrodynamic mass/damping qualification, initial
static equilibrium and licensed load attestation before response analysis.
Zero damping and waves are explicit exclusions; inactive unit wave period and
coordinate zeros are model setup, not design values. The initial line extensions
are illustrative and do not imply a balanced initial berth equilibrium.

Passing forces assume parabolic hull sections and a straight parallel track.
Bank/channel effects, relative current effects on passage, hull-motion feedback,
friction, nonlinear line material behaviour and fatigue are excluded. Sampling,
quadrature/image-sum convergence and continuous-time peaks are not established
for this example. The report presents sampled peaks only.

Supply sourced allowable tensions, fender reactions, compression and displacement
limits, then qualify the model and retain results on one recorded solver version.
An approved twelve-case solve and convergence study can then complete the
coupled-response acceptance. No design acceptance is implied by preparation.
