# Plan: digitalmodel #2190 — canonical BRep curvature signature via bundled OpenCascade

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2190
**Parent:** #2170 (decision D5; decision page workspace-hub PR #3889)
**Date:** 2026-09-26 · **Status:** plan drafted by the orchestrator; dispatched to the Codex lane on the owner's instruction of 2026-09-26 ("plan #2190 and dispatch to codex"). The `status:plan-approved` label is the owner's to apply.
**Tier:** T2 · **Lane:** lane:codex · **Client:** N/A

## Context

The mesh backend of HullProd is representation-sensitive: on the Wigley control the signature `I_D`
reads 3.69, 4.01, 4.16, 4.24 over four refinements (first-order convergence, extrapolated about
4.31). Decision D5 deferred the canonical BRep route because it assumed FreeCAD was needed for STEP
export. That assumption is wrong: the `[curvature]` extra installs `cadquery-ocp`, so the
OpenCascade Python bindings (`OCP`) are available wherever HullProd is.

Spike evidence (orchestrator, 2026-09-26, HullProd 1.0.1, Python 3.12): a Wigley half hull
(L 100, B 10, T 6.25) sampled 41 x 11, fitted with `GeomAPI_PointsToBSplineSurface` (degree 3 to 8,
C2, tol 1e-6), written with `STEPControl_Writer` (unit M) and assessed with `hullprod.assess(step,
lref=100)` returned backend `brep_native`, `I_D = 4.3094`, `I_D_plus = 2.1545`, `I_D_minus = 2.1549`,
`a_elliptic = 0.6263`, `a_saddle = 0.3737`, status `valid`, in 1.0 s. The mirrored, sewn full shell
gave identical values in 1.7 s. This is the mesh-independent reference the catalog needs.

## Scope

1. **`src/digitalmodel/hydrodynamics/hull_library/hull_surface_brep.py`** (new, under 400 lines,
   OCP imported lazily inside functions):
   - `profile_point_grid(profile, n_x, n_z) -> ndarray (n_x, n_z, 3)`: starboard surface points
     from a `HullProfile`, resampled with the same shape-preserving interpolation as
     `mesh_generator._shape_preserving_interp` (import it; do not duplicate), marine z (0 at
     waterline, -draft at keel), default `n_x = 41`, `n_z = 11` or denser when stations are denser.
   - `bspline_face_from_grid(points, *, deg_min=3, deg_max=8, tol=1e-6)`: `TColgp_Array2OfPnt` →
     `GeomAPI_PointsToBSplineSurface(..., GeomAbs_C2, tol).Surface()` → `BRepBuilderAPI_MakeFace`.
   - `mirror_and_sew(face, plane="y")`: `gp_Trsf.SetMirror` about the centre plane,
     `BRepBuilderAPI_Sewing(1e-4)`, returns the shell.
   - `flat_bottom_face(points)` (should, not must): planar face between the keel edge of the
     surface and the centreline so the BRep surface set matches the mesh generator, which always
     adds flat bottom panels. If it proves fragile, omit and document that `a_flat` is not
     comparable between representations while `I_D`, `I_D_plus`, `I_D_minus` are.
   - `export_step(shape, path, unit="M") -> Path`: sets `write.step.unit`, `STEPControl_AsIs`,
     returns the path; raise on a non-`IFSelect_RetDone` status.
   - `profile_to_step(profile, path, *, mirror=True, bottom=True, n_x=None, n_z=None) -> Path`.
2. **`curvature_screen.py`** additions (keep existing behaviour and tests intact):
   - `CurvatureSignature.representation: str = "mesh"` (new field, default keeps YAML round trips
     backwards compatible) and `CurvatureSignature.source: str | None = None` (file name).
   - `screen_step(path, *, lref, hull_type=None, workdir=None) -> CurvatureScreenResult`: runs
     `hullprod.assess` on an IGES/STEP path, `representation="brep"`, `reliability="not_applicable"`,
     `status` from `metric_validity.developability_deviation.status`, fields `None`.
   - `screen_profile(profile, config=None, *, representation="mesh" | "brep" | "both", ...)`:
     for `brep`, build the STEP in a temp dir via `profile_to_step` and call `screen_step`; for
     `both`, return the mesh result with `provenance["brep_signature"]` and
     `provenance["representation_delta"]` (see 3).
   - `representation_delta(brep, mesh) -> dict`: absolute and relative deltas for the seven values.
3. **Catalog:** `HullCatalogEntry.curvature_signature_brep: CurvatureSignature | None` and
   `HullCatalog.screen_hull(hull_id, config=None, representation="mesh")` accepting `"brep"` and
   `"both"`; `PanelCatalogEntry` unchanged (panel files have no BRep).
4. **Tests** `tests/hydrodynamics/hull_library/test_hull_surface_brep.py` (skip without hullprod):
   - closed-form: Wigley BRep `I_D` within 0.5 % of a numerical integral of the analytic Gaussian
     curvature of `y = B/2 (1-(2x/L)^2)(1-(z/T)^2)` computed in the test with numpy on a fine grid
     (comparator class `closed-form`, per the reproducibility rule); class fractions 0.626 / 0.374
     within 0.01.
   - `BRepPrimAPI_MakeSphere` radius 1 → STEP → `I_D = 4.000` (lref 2); `BRepPrimAPI_MakeCylinder`
     side face → `a_single = 1.000`, `I_D = 0`.
   - fixture `ship_profile`: BRep status `valid`, `a_elliptic > a_saddle`; mesh-vs-BRep delta on
     `I_D` reported in the assertion message and required below 15 % at 7 225 panels (record the
     measured value in the PR body; if it exceeds 15 %, keep the test as a documented expectation
     with the measured number and explain the cause in the PR body rather than loosening silently).
   - STEP round trip: `export_step` output re-read by HullProd has unit M and the same bounds.
   - catalog `screen_hull(..., representation="both")` stores both signatures; YAML round trip of a
     `CurvatureSignature` without `representation` still loads as `"mesh"`.
5. **Docs:** add a "BRep route" section to `docs/domains/hull_library/curvature-screening.md`
   (when to use, the spike numbers, what is and is not comparable) and commit this plan as
   `docs/plans/2026-09-26-issue-2190-brep-curvature-signature.md`.

## Out of scope

FreeCAD (`freecad_hull.py` stays as the FreeCAD-hosted route, untouched); IGES writing; trimming
to a waterline; screening GDF panel inventories as BRep (no surface exists); form parametrization
(#2191); any change to `mesh_generator.py`, `quality_gates.py` or client data.

## Acceptance

- `hullprod.assess` on `profile_to_step(ship_profile)` returns backend `brep_native`, status `valid`.
- Wigley closed-form test passes; sphere and cylinder exact.
- Full `tests/hydrodynamics/hull_library` + `diffraction/test_quality_gates.py` green with the
  existing curvature tests at 0 skips; new modules black/ruff clean; no client, host or
  private-path identifiers.
- PR body reports the fixture-ship mesh-vs-BRep delta table (I_D, I_D_plus, I_D_minus, four
  fractions) at 1 600 and 7 225 panels.

## Risks

- `GeomAPI_PointsToBSplineSurface` may oscillate on a coarse 5-station profile; mitigate by
  resampling with the shape-preserving interpolation first and keeping `deg_max` at 8.
- Sewing tolerance versus STEP unit: write in metres and keep tolerances in metres.
- Windows OCP wheels print STEP statistics to stdout; suppress in the library (`contextlib.redirect_stdout`).
