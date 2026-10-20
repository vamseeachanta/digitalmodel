# Mesh-derived calm-water friction records

The implementation connects existing mesh hydrostatics and SI friction to a
traceable result. It provides a friction estimate and an independent synthetic
arithmetic check.
Physical hull-resistance accuracy, wave resistance and total effective power
remain **NOT_EVALUATED**. Refs #2239.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-20 | Added friction records, benchmark gates and analytical evidence checks to extend the existing geometry and friction kernels. |
| 2 | 2026-10-20 | Retained mesh friction only, deferred the benchmark and analytical gates and transfer inventory, and corrected the equation source locators. |

## Implementation and ownership

`src/digitalmodel/naval_architecture/mesh_resistance.py` consumes
`mesh_hydrostatics.compute_hydrostatics` without changing its output schema.
Geometry/input hashes, SI conditions, source revision, package version,
mathematical class, component forces, citations and omissions accompany the
result. `friction_scaling.py` remains the source of SI dimensionless kernels.

Benchmark comparisons, analytical evidence gates and the cross-domain inventory
are deferred to separately authorized changes under #2239. They are not
dependencies of the friction record or worked example.

These are algorithms and ephemeral result records, not a new data catalog.
Reusable retained results remain with the existing authoritative data repository. Original
synthetic examples are covered by the repository license; external source
documents and numerical datasets are not redistributed here. The consuming
workflow must check source rights and hashes against retained bytes before
using external data.

## Geometry and operating contract

`TriMesh` requires declared units and axes, manifold closed triangles, outward
normals and a baseline datum. The canonical frame is right handed, x forward,
y port, z up. Draft and trim specify the waterplane; trim is forward minus aft
draft across the mesh length. Artificial clipping caps contribute to volume,
never wetted area. This is a hull surface, not a CFD volume mesh.

The existing geometry kernel supplies volume, wetted area, waterline length,
beam and section coefficients. Its topology tests do not certify absence of
triangle self-intersections. Independent tessellations and section grids must
be checked for convergence before physical application. No new convergence
qualification is inferred from the synthetic box calculation.

Every call declares speed, density, kinematic viscosity, gravity, draft, trim,
free-surface state, water depth, roughness and appendage state. The current
friction workflow accepts calm water, a smooth surface and absent appendages.
`water_depth_m=None` explicitly declares unbounded depth; finite depth is
retained but its hydrodynamic effect is not modeled. No fluid property or form
factor is silently inferred. A missing form factor leaves that component
unevaluated. This applies equally to fine and full-bodied forms: neither gets
a qualified total resistance from this correlation.

`geometry_quality.py` was inspected: its diffraction panel limits and aspect
ratio checks are a different consumer policy. The new adapter reuses `TriMesh`
rather than importing those defaults. The sectional-profile
`visualization/design_tools/hull_hydrostatics.py` is another representation;
its trapezoidal integration is not used as an independent mesh oracle.

## Equations and sources

| Quantity | Equation | Open source inspected |
|---|---|---|
| Reynolds number | Re = UL/nu | ITTC CFD guideline, 2024 revision 02, section 2.1 |
| Froude number | Fn = U/sqrt(gL) | Same source, section 2.1 |
| Friction coefficient | CF = 0.075/(log10(Re) - 2)^2 | Same source, unnumbered tutorial, printed page 8 |
| Friction force | RF = rho U^2 S CF/2 | ITTC performance prediction procedure, 2021 revision 05, section 2.4.1 |
| Declared form term | Rform = k RF | Same procedure, section 2.4.1, form-factor decomposition |
| Component power | PF = RF U | Mechanical work definition; CFD guideline section 1 distinguishes effective power |

The coefficient values in the correlation are sourced by those equations.
All other physical numbers in the example are synthetic declared inputs.
The inherited correlation refuses its algebraic singularity; this is not a
transition-Reynolds or validated applicability criterion. Acceptance limits
and uncertainty budgets require their own declared source.

Sources opened on 2026-10-20:

- [ITTC Practical Guidelines for Ship Resistance CFD, 2024 revision 02](https://ittc.info/media/11960/75-03-02-04.pdf), HTTP 200.
- [1978 ITTC Performance Prediction Method, 2021 revision 05](https://ittc.info/media/11796/75-02-03-014.pdf), HTTP 200.

The cited source locations were checked on 2026-10-20. Availability establishes
no redistribution right. The announcement mentioned in the issue is not used
as an executable engineering method or as authority for a numerical value.

The friction wrapper reuses the existing `Citation` sidecar contract.
Configured missing/stale references still fail closed. Explicit `cite=False`
supports the standalone synthetic example but records UNRESOLVED citations;
it does not imply that a standards sidecar was resolved.

## Worked example

Run `PYTHONPATH=src python examples/naval_architecture/mesh_resistance_example.py`
in an environment with NumPy, SciPy and PyYAML. The example writes JSON to stdout.

The synthetic box is length 10 m, beam 2 m and depth 2 m, submerged to 1 m with
zero trim. Speed is 1 m/s, density 1000 kg/m^3, viscosity 1e-5 m^2/s and
gravity 10 m/s^2. These are declared exercise values, not fluid measurements.

The independent hand calculation is:

- Volume = LBT = 20 cubic metres.
- Physical wetted area = LB + 2LT + 2BT = 44 square metres; the waterplane cap is excluded.
- Re = 1,000,000 and Fn = 0.1.
- CF = 0.075/16, from the cited correlation (displayed as 4.7e-3).
- RF = 1000 x 1^2 x 44 x CF/2 = 103.1 N.
- Declared k = 0.2 supplies k RF = 20.6 N.
- Friction power = RF U = 103.1 W. Total effective and shaft power are unknown.

Displayed results are rounded; calculations and test expectations retain the
unrounded arithmetic values.

The example exposes the independent hand-calculated force and an
`arithmetic_matches` boolean with an absolute roundoff budget of 1e-10 N and
zero relative tolerance. That budget checks arithmetic only; it is not a
towing-tank accuracy criterion. No benchmark or analytical evidence gate is run.

## Method selection and later slices

| Method | Mathematical status | Geometry and scope | Evidence, cost and limitation |
|---|---|---|---|
| Mesh plus friction correlation | Empirical correlation on integrated geometry | Closed surface, wetted S and L; smooth bare hull | Box arithmetic verified; geometry cost follows mesh faces; no total-force accuracy claim |
| Existing form-factor fit and scale transfer | Empirical fit and similarity transfer | Matched conditions and independently determined form factor | `prohaska.py`, `friction_scaling.py`; low-cost scalar/least-squares kernels; inherited assumptions still apply |
| Existing empirical hull method | Empirical correlation | Hull coefficients | `holtrop_mennen.py`; #2020 remains open; not used as a trusted validation oracle |
| Thin-ship method | Linear-theory integral evaluated numerically | Converged sections, slender-hull assumptions | NOT_EVALUATED pending readable source and independent linear-theory benchmark; no full-bodied use or closed-form label |
| Potential-flow panels | Numerical integral-equation method | Surface panels and boundary conditions | Follow-up; excludes turbulent separation and needs its own mesh convergence evidence |
| RANS resistance | Numerical PDE solution | Volume/domain mesh and full conditions | Owned by #1173/#2023; no solver is invoked here and existing accuracy evidence is not promoted |

A public analytic box is the current pilot; it checks geometry and arithmetic,
not realistic bow/stern flow. Independently generated analytic parabolic hull
tessellations already exist in `hull_fixtures.py` and its tests. Physical
benchmarks require a pinned redistribution-rights record, matched force
definitions, fluid and scale, experimental uncertainty, and predeclared
criteria before any physical-validation status is assigned. No external hull
dataset is copied into this change.

Future benchmark work must refuse circular comparisons that share a source
digest, even when read-only access is permitted. Future analytical evidence
must distinguish an explicitly justified NOT_APPLICABLE check from a zero
residual or missing evidence. Neither deferred gate is implemented here.

## Verification and known limits

New tests use synthetic arithmetic, explicit rejection cases and an independent
box derivation. Existing mesh, friction, citation and empirical-kernel tests
were also run. No numerical output from a proprietary solver is used as truth.
No existing fixture is recalibrated. This is numerical and workflow
verification; no physical accuracy or certification is claimed.
