# KCS actuator-disk self-propulsion analysis

The builder prepares the first body-force rung: a full-domain, fixed-hull,
two-phase `interFoam` case with a uniform axial propeller source and bounded
PI control of RPM. The coarse smoke advances six physical time steps. It does
not establish a self-propulsion point or agreement with experimental scatter.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-23 | Add a reproducible case, controller, reference readback and startup evidence before validation. |

## Reproduce

Run from a repository checkout. Set `CASE` to a new external output directory,
set `RESULT` to a JSON output beside it, and source the environment file of
the newest installed OpenFOAM version. The smoke was exercised with v2312.
No Python package beyond the standard library is needed by these commands.
The builder refuses an existing destination and prints its measured wall time.

```bash
python scripts/cfd/kcs_self_propulsion.py build "$CASE"
python scripts/cfd/kcs_self_propulsion.py smoke "$CASE" --output "$RESULT"
python scripts/cfd/kcs_self_propulsion.py check "$CASE/controller.csv"
```

`smoke` invokes `blockMesh`, `snappyHexMesh -overwrite`, `topoSet`, `setFields`,
`checkMesh`, and three serial `interFoam` segments at nice level 10. Each
segment advances two steps of 0.0005 s, then averages that segment's measured
hull resistance before updating RPM. A successful command exit indicates
startup or readback success; it is not a validation acceptance flag.
Raw tool logs and solver fields remain in the case directory. The builder
records generated-input digests in `manifest.json`. Completed preparation
stages and feedback segments have digest-bound checkpoints; a completed stage
is not rerun. A log without a completed checkpoint requires inspection and a
new case directory. Existing cases and logs are never removed automatically.

The layout follows the repository's [Wigley case study](../wigleHull_LTS/wigleHull_LTS.md)
and existing hull-resistance builders: `0`, `constant`, `system`, a generated
STL and a provenance manifest. The generator is
[the actuator-disk module](../../../../../src/digitalmodel/solvers/openfoam/kcs_actuator_disk.py);
[the command script](../../../../../scripts/cfd/kcs_self_propulsion.py) exposes
build, smoke and reference checks.

## Geometry and force model

The existing public Tokyo 2005 structured KCS surface grids are tessellated
using the repository's geometry generator. No parametric stand-in or private
geometry is used. The older grid differs from the later Tokyo 2015 re-fairing;
that mismatch must be resolved before validation. Coordinates are centered at
midship, positive x downstream and positive z upward. The hull remains fixed
at even keel, without rudder. Both sides are meshed; forces need no symmetry
multiplier. The waterline is z = 0.

The disk center follows the public KCS particulars: x/L from the forward
perpendicular is 0.9825 and depth/L is 0.02913. A cylindrical cell zone of
radius 0.125 m and thickness 0.1 m receives the total downstream fluid force.
`vectorSemiImplicitSource`, with `volumeMode absolute`, distributes that force
by selected cell volume. Because `interFoam` solves density-weighted momentum,
the source is specified in newtons, without dividing by water density. The
reaction on the propeller acts forward. No swirl or torque is represented.
The coarse disk contains fluid cells outside the cut hull; its exact loaded
area is not a blade representation.

The demonstrator uses T = rho (RPM/60)² D⁴ KT, with D = 0.25 m and constant
KT = 0.17 taken from the experiment. This freezes a measured coefficient:
RPM agreement would not be an independent KP505 prediction. A measured
open-water curve and advance-ratio coupling remain necessary.

For each completed segment, error e = R − SFC − T. The incremental PI update
is RPM_next = clamp(RPM + Kp e + Ki integral(e dt), 0, 900), with conditional
integration to prevent windup at the bounds. Gains are Kp = 1 RPM/N and
Ki = 0.1 RPM/(N s); these are demonstration settings, assumed, based on previous
experience. Negative measured resistance during a transient remains signed.
Setting the towing correction to zero in the controller gives T = R; the
workshop ship point instead requires T = R − 30.3 N. The propeller source is
held fixed within each solver segment and updated between restarts.

## Public reference and diagnostic comparison

The [reference JSON](../../../../../src/digitalmodel/solvers/openfoam/data/kcs_tokyo2015_self_propulsion.json)
records the source address, source digest, worksheet cells, units and
normalizations. The public [Case 2.5 instructions](https://t2015.nmri.go.jp/Instructions_KCS/Case_2.5/Case_2-5.html)
state 9.5 rps and a 30.3 N towing correction. The linked V&V workbook,
Sheet1 column C, gives CT = 0.003966, KT = 0.17, KQ = 0.0288,
and R − T = 30.25 N. The dimensional comparison derives T and R from those
coefficients using the stated water properties, speed and bare-hull area.
The rounded published quantities do not close identically. They are retained
as published, without adjusting any value to force agreement.

| Quantity | Reference | Basis |
| --- | ---: | --- |
| RPM | 570.0 | Published 9.5 rps × 60 |
| Thrust (N) | 59.9 | Derived from KT, density, diameter and rps |
| Resistance (N) | 90.2 | Derived from CT, density, speed and area |

The workbook's experimental-uncertainty column is blank. The checker reports
signed percentage differences and force imbalance, and always marks validation
`not_established`. It never substitutes an arbitrary tolerance for published
scatter. Diagnostic readback of a supplied window does not certify convergence,
matched conditions or provenance of that input history.

## Smoke evidence

The [portable evidence](smoke_evidence.json) records the mesh, actual advanced
steps, RPM sequence and reference comparison. Its digests identify the raw
logs retained with the external case; raw logs and generated fields are not
committed. The evidence is regenerated with the `smoke` command above.

The mesh contains 41,482 cells and the source selects 106 cells. `checkMesh`
reported `Mesh OK`. Six steps reached 0.003 s using one process. The measured
build took 0.7 s and the smoke took 4.8 s; these timings describe this coarse
setup only. RPM reached its 900.0 cap after the first segment because startup
forces were still large. These samples are impulsive transients, not an
operating point. The controller mean and experimental differences in the JSON
serve only to exercise readback.

The smoke deliberately uses laminar transport, first-order momentum advection,
no boundary layers, a coarse free surface, a short domain, and an initially
uniform flow with unrelaxed pressure. It is unsuitable for resistance or
self-propulsion prediction at this Reynolds number.

## Left

- Resolve the geometry vintage and prepare the matched public hull surface.
- Start from a converged resistance field with the required attitude; implement
  free heave and pitch or justify matched fixed-attitude conditions.
- Replace the constant measured KT with a sourced open-water performance curve
  and wake-dependent loading; tune controller gains on settled force histories.
- Add turbulence, wall layers, free-surface and disk refinement, domain and
  time-step sensitivity, convergence windows and grid uncertainty.
- Obtain published experimental scatter and qualify a converged comparison.
- Run the full actuator-disk validation and demonstrate thrust balance at the
  ship point. No such result is claimed here.
- Resolved rotating blades with AMI, torque validation, hull pressure probes,
  pressure-pulse harmonics and a pressure-fluctuation reference dataset.
- Full capability-page validation tables remain dependent on those results.
