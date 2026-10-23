# Diffraction report stability analysis

Reported transverse and longitudinal metacentric heights use the density and
gravity associated with the hydrostatic restoring matrix. The native extraction
and numeric bundle report paths retain a consistent unit basis. A missing bundle
environment leaves GM unevaluated with a `NOT_EVALUATED` explanation.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Document environment propagation and an independent calculation to detect fixed-default GM errors. |

## Inputs and method

For small rotations about an equilibrium with consistent matrix reference axes,

```text
GM_T = C44 / (rho * g * V)
GM_L = C55 / (rho * g * V)
```

The basis is the displacement-body hydrostatic stiffness matrix in the public
[OrcaWave help, Results: Hydrostatics, Hydrostatic stiffness matrix](https://www.orcina.com/webhelp/OrcaWave/Content/html/Results,Hydrostatics.htm#HydrostaticStiffnessMatrix)
(online edition accessed 2026-10-22). These are dimensional stiffness entries per
radian; rho is water density, g is gravitational acceleration and V is displaced
volume. This interpretation requires the equilibrium and axes appropriate to GM.

The [model units documentation](https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Model.htm#Units)
(online edition accessed 2026-10-22) defines native SI units as tonnes, metres,
kN and seconds. Native report extraction therefore passes `WaterDensity` in
tonnes per cubic metre and `g` in metres per second squared directly to
`compute_stability`. Its restoring and inertia matrices retain their native basis.

`DiffractionResults` bundles use kg and N, following [the internal units contract](../../../UNITS.md).
They carry optional top-level `water_density` (kg/m³) and `gravity` (m/s²) fields.
The result producer retains these from `DiffractionSpec.environment` and converts
native hydrostatic mass and stiffness to kg and N before serialization. Other
bundle producers must supply these fields from their actual input environment.
The report adapter passes both explicitly; it never substitutes the computation
helper's historical defaults for a missing bundle value.

## Worked example

All values below are synthetic inputs chosen for independent hand arithmetic;
they are not recommended environmental constants or acceptance criteria.

| Input | Value | Unit |
| --- | --- | --- |
| Water density | 1000.0 | kg/m³ |
| Gravity | 10.0 | m/s² |
| Displaced volume | 10.0 | m³ |
| Roll stiffness C44 | 200000.0 | N m/rad |
| Pitch stiffness C55 | 600000.0 | N m/rad |

The denominator is `1000.0 × 10.0 × 10.0 = 100000.0 N`, so
`GM_T = 200000.0 / 100000.0 = 2.0 m` and
`GM_L = 600000.0 / 100000.0 = 6.0 m`.
The equivalent native inputs are density `1.0 t/m³` and stiffnesses
`200.0` and `600.0 kN m/rad`; they give the same heights.

Run the example with the repository's Python dependencies available:

```sh
PYTHONPATH=src uv run python examples/demos/diffraction/report_environment.py \
  --density 1000 --gravity 10 --volume 10 --c44 200000 --c55 600000
```

Expected output:

```json
{"gm_transverse": 2.0, "gm_longitudinal": 6.0}
```

Holding stiffness and volume fixed, changing only density to `1250.0 kg/m³`
gives denominator `125000.0 N`, hence heights `1.6 m` and `4.8 m`.
Changing only gravity to `8.0 m/s²` gives denominator `80000.0 N`, hence
heights `2.5 m` and `7.5 m`. Regression tests exercise both changes through
mocked native extraction and serialized bundle-to-HTML generation. Stiffness
is deliberately held fixed to expose the denominator used by the report; this
is an arithmetic regression, not a prediction of the same body's response
after its physical environment changes.

## Limits and way forward

Legacy bundles without either environment field retain coefficient reporting,
but cannot establish GM. Bundles declaring a unit system other than `SI` also
leave GM unevaluated to avoid mixing density and stiffness bases.
Waterplane moments, centre of floatation and inertia
that are absent from numeric bundles remain `NOT_EVALUATED`; no BM or natural
period is inferred from missing data. The direct `compute_stability` helper
retains its previous defaults for compatibility; report assembly supplies
explicit values, including explicit unknowns.

No licensed solver execution or native file round trip is established by these
tests. Native extraction retains its existing metric-unit contract; arbitrary
non-metric unit support is outside this change. Other result producers must
record the input environment before their bundles can report GM. Existing
stability status thresholds are unchanged and do not establish design acceptance.
