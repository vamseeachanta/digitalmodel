# Parametric RAO pilot: ranges and assumptions

The retained open-source pilot for [issue #1655](https://github.com/vamseeachanta/digitalmodel/issues/1655)
covers box and analytic ship forms. Its [lookup](../hydrodynamics/parametric-rao/lookup.html)
is a discrete response reference; data readiness is partial and design use remains unverified.

## Parametric ranges

The values below are copied from [basis.json](../../../examples/hydrodynamics/parametric_rao/basis.json).
Each dimension has two discrete values; their Cartesian product gives eight cases per form
and sixteen cases in total. Intermediate dimensions are not qualified.

| Source key in basis.json | Value | Meaning and units |
|---|---|---|
| `dimensions.length_m` | `[100.0, 110.00000000000001]` | Length (m) |
| `dimensions.beam_m` | `[20.0, 22.0]` | Beam (m) |
| `dimensions.draft_m` | `[8.0, 8.8]` | Draught (m) |
| `omega_rad_s` | `[0.2, 0.34, 0.55, 0.83, 1.04, 1.32]` | Angular frequency (rad/s), before masking |
| `heading_deg` | `[0, 90]` | Wave propagation heading (deg) |
| `water_depth_m` | `200.0` | Water depth (m) |
| `rho_kg_m3` | `1025.0` | Water density (kg/m³) |
| `gravity_m_s2` | `9.80665` | Gravitational acceleration (m/s²) |

Table 1. Discrete pilot dimensions and environmental inputs, with exact JSON values.

The stored upper length retains the floating-point representation of a ten percent
increase. Coordinates use x along the hull, y across the beam and z upward, with
waterline z=0. Headings are measured counterclockwise from +x; zero degrees is the
matched longitudinal (head-sea) case. All forms are fore-aft symmetric.

## Form parameters

The [parametric form reference](parametric-form.md) defines the generator's wider parameter ranges
and construction. Those ranges do not extend this RAO dataset.
The pilot ship uses the synthetic Wigley comparator; no measured vessel is represented.

## Analysis assumptions

The remaining numerical inputs are also retained in basis.json.

| Source key in basis.json | Value | Meaning and units |
|---|---|---|
| `reference_dimensions_m` | `[100, 20, 8]` | Reference length, beam and draught (m) |
| `box_gyradii_m` | `[7, 29, 29]` | Reference roll, pitch and yaw radii of gyration (m) |
| `cog_draft_fraction` | `-0.25` | Initial vertical centre of gravity / draught (dimensionless) |
| `box_resolution` | `[50, 10, 8]` | Box panel subdivisions along each axis (counts) |
| `ship_target_panels` | `365` | Requested ship mesh size (panel count target) |
| `convergence_resolutions` | `[[25, 5, 4], [50, 10, 8], [75, 15, 12]]` | Box sensitivity subdivisions (counts) |
| `irregular_frequency_lower_fraction` | `-0.01` | Lower offset from the estimated interior mode (dimensionless) |
| `irregular_frequency_upper_fraction` | `0.03` | Upper offset from the estimated interior mode (dimensionless) |

Table 2. Loading, mesh and screening assumptions from the pinned basis.

Box loading follows the merged reference: mass equals displaced volume times water density,
and the centre of gravity is at z = cog_draft_fraction × draught.
The roll gyradius scales with beam / reference beam; pitch and yaw gyradii scale with length /
reference length. Rotational inertia is mass times squared gyradius.
The ship instead uses homogeneous solid inertia closed at the waterplane: signed
tetrahedral integrals about the waterplane origin are transferred by the parallel-axis
theorem to the uniform-density volume centroid. That centroid replaces the initial
centre of gravity; the wetted radiation mesh is unchanged. No measured loading is asserted.

The formulation is linear, inviscid, zero-speed potential flow for rigid bodies, without viscous
or mooring damping. No lid is used, retaining the reference condition.
For boxes the interior-mode estimate is omega² = g k coth(k T), where k = pi sqrt(1/L² + 1/B²).
The inclusive screening band extends from one percent below to three percent above that estimate.
This is an assumed band, not measured edges: affected responses reach at least +2.2 percent,
while retained box_00 and
box_04 samples bound the lower side (`irregular_frequency_band_reason`).

Frequencies in that band, negative heave added mass, or any negative diagonal
radiation damping require masking. Flagged coefficients and diagnostic responses
remain in [results.json](../hydrodynamics/parametric-rao/results.json); the affected
frequency is removed for all headings and DOFs from the Parquet arrays and graphs.
Unmasked sign defects or box interior-mode points fail the build. Translation
amplitudes use m/m, rotations use deg/m, and phase uses degrees with `exp(-i omega t)`.

## Acceptance criteria used

The values in [criteria.json](../../../examples/hydrodynamics/parametric_rao/criteria.json)
are beforehand pilot screening inputs, not standard-derived allowances or experimental uncertainty.

| Source key in criteria.json | Value | Meaning and units |
|---|---|---|
| `heave_absolute_tolerance_m_per_m` | `0.002` | Benchmark absolute tolerance (m/m) |
| `heave_relative_tolerance` | `0.02` | Benchmark relative tolerance (dimensionless) |
| `convergence_absolute_tolerance_m_per_m` | `0.02` | Finest-pair heave change bound (m/m) |

Table 3. Frozen benchmark and panel sensitivity criteria.

Each reference box head-sea heave sample must satisfy absolute difference <=
absolute tolerance + relative tolerance × abs(reference). The three recorded
reference frequencies use matched geometry, mesh, loading, gravity, density and depth.
The finest-pair absolute heave change must satisfy the convergence bound; this
check covers head-sea heave at those reference samples only, not every DOF or heading.

## Limits of use

The [manifest](../hydrodynamics/parametric-rao/manifest.json) states the qualification verbatim:

Open-source reproduction and head-sea heave panel sensitivity only

The lookup's **Basis and limits** section also records the unresolved limitations.
Box sway-roll reciprocity remains unresolved; box beam-sea roll is not mesh-qualified.
Retained box values at 1.32 rad/s repeat to about 1.3 %; two records with identical
mesh digests differ by 0.46 % there. The cause is not established. The convergence
bound does not discriminate at 1.32 rad/s: the recorded finest-pair change is
8.9e-5 m/m, or 1.3 % of the amplitude. Hydrostatic stiffness uses face-centre
quadrature whereas ship inertia uses exact polyhedral integrals.

Only the two listed headings and discrete frequencies are covered. Graph segments
are linear connections between solved points; off-grid interpolation is unqualified.
The pilot has no forward-speed or viscous-damping input. Roll RAOs near resonance
are potential-flow responses and are not usable for operations. Independent
experimental validation, measured loading and licensed diffraction remain outside the pilot.

## How to reproduce

The [runner](../../../examples/hydrodynamics/parametric_rao/run.py) requires the
open-source Capytaine version recorded in the manifest. From the repository root,
use separate scratch and output directories to preserve the pinned evidence:

```sh
python examples/hydrodynamics/parametric_rao/run.py --scratch .pilot-scratch --output .pilot-output --dry-run
python examples/hydrodynamics/parametric_rao/run.py --scratch .pilot-scratch --output .pilot-output
```

The dry run validates all geometries before solving. Criteria are digest-bound
before the first computation; resume rejects changed inputs or tampered records.
`--build-only` requires the original per-case and convergence scratch records and
receipt; the published outputs alone do not supply that execution scratch.
The manifest binds basis, criteria and [revisions.json](../../../examples/hydrodynamics/parametric_rao/revisions.json)
by SHA-256 and lists byte counts and digests for every retained output. Verify
those digests before use. The retained-output tests compare stored solver-version
metadata with stored records; they do not require an installed solver or run a new solve.
