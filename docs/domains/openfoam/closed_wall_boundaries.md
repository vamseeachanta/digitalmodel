# Closed-wall boundary conditions

A generated no-slip closure now receives wall mesh topology and turbulence wall
conditions together. The coupled sloshing outlet remains named `outlet` for
compatibility with existing probes and manifests. Open-flow cases retain an open
outlet. This addresses #1979; it does not establish wall-force accuracy.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Align closed-wall topology and field conditions to prevent incompatible wall-function patches. |
| Update | 2026-10-22 | Reject explicit wall functions on open generated patches before writing case files; document the override checks. |

## Method and sources

OpenFOAM v2312 (2023) defines [noSlip][no-slip] as a zero wall velocity:
`U_wall = 0`. Consequently the stationary face flux is
`Q_face = U_wall dot S_face = 0`, independently of the face area vector.
These identities describe the prescribed boundary; a solved volume balance
requires separate execution evidence.

The mesh and field entries must agree:

| Field or dictionary | Closed outlet | Bottom | Open outlet default |
| --- | --- | --- | --- |
| `blockMeshDict` mesh type | `wall` | `wall` | `patch` |
| `U` | `noSlip` | `noSlip` | `zeroGradient` |
| `p_rgh` | `fixedFluxPressure` | `fixedFluxPressure` | `fixedValue` |
| `alpha.water` | `zeroGradient` | `zeroGradient` | `zeroGradient` |
| `k` | `kqRWallFunction` | `kqRWallFunction` | `zeroGradient` |
| `omega` (SST) | `omegaWallFunction` | `omegaWallFunction` | `zeroGradient` |
| `epsilon` (k-epsilon) | `epsilonWallFunction` | `epsilonWallFunction` | `zeroGradient` |
| `nut` | `nutkWallFunction` | `nutkWallFunction` | `calculated` |

The [nutkWallFunction][nut], [kqRWallFunction][k], and
[epsilonWallFunction][epsilon] Usage sections require mesh type `wall`.
The [omegaWallFunction API documentation][omega] specifies the SST wall condition.
`kqRWallFunction` wraps a zero-gradient condition for the wall-function treatment.
No wall-law constants are copied or tuned by the generator.

The [fixedFluxPressure Description][pressure] specifies that pressure gradients
match the imposed velocity flux while compensating body forces, including
gravity. It replaces the multiphase wall pressure default and the coupled
outlet's previous explicit `zeroGradient`. Single-phase kinematic `p` keeps its
existing wall default. Explicit pressure overrides remain authoritative.

`boundary_patch_types` determines topology from the effective `U` overrides:
any generated patch with `noSlip` becomes `wall`; the original bottom stays a
wall. Other patch types, names, faces, cell counts and vertices are retained.
Repeated overrides use the last field definition, including `extra` entries,
just as the field writer does. Unknown patch names do not create mesh faces.
The same topology selects turbulence wall defaults. Explicit turbulence field
conditions take precedence, so callers remain responsible for the compatibility
of their custom conditions.

The builder now checks effective overrides before creating or overwriting any
case files. An explicit `kqRWallFunction`, `omegaWallFunction`,
`epsilonWallFunction` or `nutkWallFunction` on a generated non-wall patch raises
`ValueError` naming the patch, field, boundary condition and effective mesh type.
The standalone turbulence writer performs the same check before writing any
turbulence field. This enforces the documented mesh requirement without silently
turning a requested open boundary into a wall. The [omegaWallFunction API][omega]
describes wall treatment for specific dissipation rate and turbulent kinetic
energy production. The preflight applies the same wall-only contract to this
supported wall function.

Validation follows serialization: the last override for a patch and field wins,
including the effective `type` in `extra`. Only fields emitted by the selected
turbulence model are checked. Laminar emits none; SST emits `k`, `omega` and
`nut`; k-epsilon emits `k`, `epsilon` and `nut`; the existing Smagorinsky writer
emits `k` and `nut`. Conditions on other patch names and custom wall-function
types remain outside this check. It does not validate custom field dimensions,
wall-law coefficients or mesh resolution.

## Worked example: close a sloshing outlet

Inputs are the existing sloshing case defaults and an explicit `noSlip` outlet.
The pressure, phase and turbulence boundary conditions use generated defaults.
No new physical coefficient or acceptance tolerance is needed for this example.

```python
from pathlib import Path

from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.models import (
    BoundaryCondition, BoundaryType, CaseType, OpenFOAMCase,
)

case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="closed_wall_example")
case.boundary_conditions = [
    BoundaryCondition("outlet", BoundaryType.NO_SLIP, "U"),
]
# Supply a writable output directory when executing this example.
def build_example(output_directory: Path) -> Path:
    return OpenFOAMCaseBuilder(case).build(output_directory)
```

Method: the velocity override marks `outlet` as `wall` in `blockMeshDict`;
the turbulence writer assigns the wall types in the table; the pressure writer
assigns `fixedFluxPressure`. The answer is the closed-outlet column above.
For the hand check, `U_wall = (0, 0, 0)` makes the stationary face flux zero.
The `test_no_slip_overrides_drive_mesh_and_turbulence_together` regression
executes this construction. The coupled-case regression also exercises the
actual exchange mapper, preserving its inlet and atmospheric top vent.

### Worked example: explicit wall-function override

Starting with the same inputs, explicitly request the documented turbulent
viscosity wall condition. Its initial value uses the existing internal field;
no new physical value is supplied:

```python
case.boundary_conditions.append(
    BoundaryCondition(
        "outlet", BoundaryType.ZERO_GRADIENT, "nut",
        value="$internalField", extra={"type": "nutkWallFunction"},
    )
)
```

Method: resolve the effective `U` condition first, then resolve the effective
`nut` override, including `extra`. The answer is mesh type `wall` and a `0/nut`
outlet entry containing `type nutkWallFunction; value $internalField;`.
The initial `bc_type` is replaced by `extra["type"]` during serialization.

Now replace the closure with an open velocity condition:

```python
case.boundary_conditions.append(
    BoundaryCondition("outlet", BoundaryType.ZERO_GRADIENT, "U")
)
```

The resulting mesh type is `patch`, so building this case raises `ValueError`
before writing any files. Restore the no-slip closure to retain a wall, or
replace the explicit turbulence override with a condition suitable for the
intended open boundary. These are caller choices; the preflight does not make
that physical decision. Regression tests execute both outcomes, including an
invalid rebuild over an existing case whose files must remain unchanged.

## Verification and limits

The regression suite covers SST, k-epsilon, the existing LES field set,
laminar omission of turbulence fields, each generated no-slip face, open outlets
for every case type, repeated velocity overrides, explicit field overrides and
unchanged mesh geometry. Assertions use the documented boundary types rather
than values calculated by the implementation.
The override regressions also cover rejection on open and symmetry patches,
compatible wall overrides, repeated field and velocity overrides, and inactive
turbulence fields. These tests establish generated-dictionary consistency only.

Pre-existing turbulence initialization guesses are retained unchanged and are
**assumed, based on previous experience**. They are solver seeds, not validated
inflow data or calibrated wall-law coefficients. Physical turbulence boundary
values must be supplied for an engineering analysis. Field dimensions retain
the existing OpenFOAM dimensional representation; see [Basic input/output file
format, section 2.2.6, Dimensional units][dimensions] (online edition).

Solver runtime, wall shear, force resultants, near-wall mesh resolution and
convergence are **NOT_EVALUATED** here. No solver was run for this change.
Before using structural loads, run a mesh and time-step study with the intended
operating conditions and retain the force histories. No acceptance limit is
invented for those checks. Vent wetting under roll (#1980) and exchange inventory
limits (#1978) remain separate work.

[no-slip]: https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/wall/noSlip/
[nut]: https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/wall/nutkWallFunction/
[k]: https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/wall/kqRWallFunction/
[epsilon]: https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/wall/epsilonWallFunction/
[omega]: https://api.openfoam.com/2312/classFoam_1_1omegaWallFunctionFvPatchScalarField.html
[pressure]: https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/inlet/fixedFluxPressure/
[dimensions]: https://www.openfoam.com/documentation/user-guide/2-openfoam-cases/2-2-basic-inputoutput-file-format
