# Equilibrium evidence in diffraction validation

The shared validation helpers accept explicit equilibrium evidence for the
existing CoG/CoB, mass/displacement and signed force checks. This is a bounded
part of issue #51.
It allows a caller to obtain an equilibrium verdict and JSON report without
constructing `OutputValidator` directly. It does not execute a solver.

## Usage

```python
from digitalmodel.hydrodynamics.diffraction.validation_runner import run_validation

outcome = run_validation(
    results,
    output_dir,
    report_stem,
    equilibrium_inputs={
        "mass_displacement": {
            "mass_kg": supplied_mass_kg,
            "displacement_m3": supplied_displaced_volume_m3,
            "water_density_kg_m3": supplied_water_density_kg_m3,
            "relative_tolerance": supplied_mass_budget_tolerance,
        },
    },
)
```

`validate_results(results, output_file, equilibrium_inputs=evidence)` accepts
the same mapping; `output_file` remains an optional positional argument.
The new evidence argument is keyword-only in both helpers. Existing calls
without it retain their behavior.

## Evidence contract

Supply only evidence for the same structure, load case, attitude, coordinate
frame and sign convention. There is no implicit unit conversion or extraction
from `results`. Every tolerance is an explicit caller input; no universal
engineering acceptance limit is provided.

| Rule | Required keyword arguments | Units and assumptions |
| --- | --- | --- |
| `cog_cob` | `centre_of_gravity`, `centre_of_buoyancy`, `tolerance_m` | Cartesian position vectors in metres; horizontal axes perpendicular to gravity; free flotation without external moments |
| `mass_displacement` | `mass_kg`, `displacement_m3`, `water_density_kg_m3`, `relative_tolerance` | kg, cubic metres, kg per cubic metre, dimensionless tolerance; free flotation |
| `force_balance` | `input_forces_n`, `output_forces_n`, `relative_tolerance`, `absolute_tolerance_n` | Signed Cartesian force vectors in N; dimensionless relative tolerance and absolute tolerance in N |

For reaction forces, the caller must reverse the reaction sign before
comparison. Missing tolerance arguments and invalid numerical evidence fail
the supplied rule. Omitted rules remain `NOT_EVALUATED`.

## Equations and sources

No numerical rule is changed by this integration. The existing arithmetic
contracts are in
[equilibrium_checks.py](../../../src/digitalmodel/hydrodynamics/diffraction/equilibrium_checks.py).

- Horizontal alignment: `d_xy = hypot(x_G - x_B, y_G - y_B)` and acceptance
  `d_xy <= tolerance_m`. Weight and buoyancy have a common vertical line of
  action for free flotation without external moments. The physical basis is
  force and torque equilibrium in OpenStax, *University Physics Volume 1*
  (2016), [section 12.1, Conditions for Static Equilibrium](https://openstax.org/books/university-physics-volume-1/pages/12-1-conditions-for-static-equilibrium),
  together with the buoyancy force described in section 14.4 below. The
  horizontal distance is the Cartesian moment-arm separation; vertical
  separation alone is not a failure and does not establish stability.
- Displaced mass: `m_d = density * volume`, with acceptance
  `abs(mass - m_d) <= relative_tolerance * m_d`. OpenStax, *University Physics
  Volume 1* (2016), [section 14.4, Archimedes' Principle and Buoyancy](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy),
  states that a floating object's mass equals the mass of displaced fluid.
  The residual is normalized by displaced mass, as required by the existing
  arithmetic contract.
- Force comparison: for each signed component,
  `abs(output - input) <= absolute_tolerance_n + relative_tolerance * abs(input)`.
  The force-equilibrium basis is OpenStax (2016), section 12.1 above. The
  additive numerical tolerance policy comes from the existing
  `check_force_balance` contract, not a published class acceptance criterion.
  It compares reported forces; it is not a general dynamics balance including
  inertia, nor a moment or complex RAO check.

The sources establish the physical relationships, not the numerical tolerances.
Select tolerances from documented coordinate precision, mass budgets and force
convergence requirements. No density, gravity coefficient or acceptance limit
is inferred by the helpers.

## Verdict and report behavior

The returned report retains the existing fields:

- `equilibrium_checks`: failure lists per rule.
- `equilibrium_not_evaluated`: reasons for rules without evidence.
- `equilibrium_status`: `PASS` when all supplied and required rules pass,
  `NOT_EVALUATED` when any rule lacks evidence, or `FAIL` when any rule fails.
  Failure takes precedence over missing evidence.

An equilibrium failure produces an overall `FAIL` and appears in the shared
helper's flattened issue list. Missing evidence alone does not change the
overall verdict; an overall `PASS` can coexist with equilibrium
`NOT_EVALUATED`. Inspect both fields when deciding whether equilibrium has
been assessed. Disabled validation or absent diffraction results remain
`SKIPPED` and do not consume evidence. JSON export preserves these fields.

## Test basis and remaining work

[The helper regression tests](../../../tests/hydrodynamics/diffraction/test_validation_equilibrium_inputs.py)
write out independent hand calculations for horizontal separation, displaced
mass and signed force residuals at inclusive acceptance boundaries and beyond
them. All example inputs and limits are synthetic arithmetic cases, not
recommended physical data. The suite also covers partial and absent evidence,
missing tolerances, unchanged input mappings and skipped validation.

Later slices can route evidence through analysis configuration, extract it from
documented result formats, attach tolerance provenance, and add category-specific
CSV tables and charts. Static restrained-body equilibrium, dynamic inertial
balance, ball-joint rotations and stability acceptance require their own
evidence and criteria. This slice makes no claim to complete those checks.
