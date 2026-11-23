# Plan: digitalmodel #277 — WRK-1251 FreeCAD deep parametric engineering

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/277
**Status:** plan-review
**Tier:** T3 (multi-stage CAD + FEM + study pipeline; new module + skill extension)

## Context

Issue #277 (WRK-1251) builds production-grade FreeCAD automation for parametric hull generation, FEM (CalculiX) preprocessing, and design-table-driven parameter sweeps. Existing surface in the repo (verified): `.claude/skills/freecad-automation/` has the API-surface skill, `.claude/skills/engineering/freecad-agents/` exists, `docs/domains/freecad/` already contains `freecad.md`, `freecad_assembly4.md`, `freecad_design_capabilities.md`, `freecad_design_dxf.md`, `freecad_fem.md`, plus `design.md`, `simple/`, `src/`, and a sample `0145-01-PRE-0001-01 Drawing Automation (Draft1).pptx`. No `src/digitalmodel/` Python module wraps FreeCAD yet — the skill is the entry point but engineering code does not exist.

Acceptance criteria call for: hull generator with hydrostatics within 2% of analytical, FEM chain (gmsh → CalculiX) producing stress-concentration factor within 5% of Kt≈3 reference, design-table batch run producing a results YAML, FreeCAD→STEP→gmsh→STL round-trip with manifold check, all headless via `freecadcmd`, integration tests on simply-supported beam + pressure vessel, and an updated `freecad-automation` SKILL.md with deep workflow sections.

**Stale-flag:** Not stale. Status is `working` (not `done` like several siblings) and the acceptance list is fully unchecked. Issue is targeted at `ace-linux-2` which has FreeCAD installed; the plan must run there.

## Plan

### Task 1 — Module scaffold
Create `src/digitalmodel/cad/freecad/` with submodules: `hull.py` (parametric hull generator), `hydrostatics.py` (displacement, KM, BM, waterplane), `fem.py` (gmsh + CalculiX driver), `study.py` (design-table batch). Mirror the test layout under `tests/cad/freecad/`. Verify on `ace-linux-2` that `freecadcmd` runs headlessly; capture the version into `docs/domains/freecad/freecad-version-pin.md`.

### Task 2 — Parametric hull + hydrostatics
Implement `hull.generate(L, B, T, Cb)` producing a NURBS hull surface via the FreeCAD Part workbench API and exporting to STEP/STL. Implement `hydrostatics.compute(hull, draft)` returning displacement, KM, BM, waterplane area, LCB. Acceptance fixture: `(L=100, B=20, T=8, Cb=0.7)` → analytical displacement 11,200 t (= L·B·T·Cb·ρ) — the implementation must agree within 2%.

### Task 3 — FEM chain (gmsh → CalculiX)
Implement `fem.run(geometry_step_path, mesh_size, materials, boundary_conditions)` that meshes the STEP through gmsh, writes a CalculiX `.inp` deck, invokes CalculiX, and extracts stress + displacement fields. Validate against the plate-with-hole stress-concentration benchmark (Kt ≈ 3.0 for finite plate, hole radius/half-width = 0.1) — implementation must land within 5%.

### Task 4 — Design-table study driver
Implement `study.run_matrix(spec_yaml)` that consumes a YAML parameter matrix (≥3 axes, e.g., plate thickness × stiffener spacing × material grade), invokes hull + FEM per cell, and emits a results comparison YAML keyed by case-id with peak stress, mass, and any user-named output expressions.

### Task 5 — Round-trip pipeline
Add `tests/cad/freecad/test_roundtrip.py`: FreeCAD hull → STEP → gmsh tetra mesh → STL → manifold check (closed-surface test via `mesh.is_watertight` or a count-of-boundary-edges == 0 assertion). Failure modes (non-manifold edges, self-intersection) must produce actionable error messages, not silent surface-defect output.

### Task 6 — Skill update
Extend `.claude/skills/freecad-automation/SKILL.md` with three new sections: "Parametric hull generation", "FEM chain (gmsh + CalculiX)", "Design-table studies". Each section links to the new src paths and shows a 5–10 line invocation example.

## Acceptance Criteria

- [ ] `src/digitalmodel/cad/freecad/{hull,hydrostatics,fem,study}.py` present; all operate via `freecadcmd` (headless).
- [ ] Hull benchmark `(L=100, B=20, T=8, Cb=0.7)` displacement within 2% of analytical 11,200 t.
- [ ] FEM benchmark plate-with-hole Kt within 5% of 3.0.
- [ ] Design-table run with ≥3 parameter variations produces a results YAML readable by downstream tooling.
- [ ] Round-trip test asserts STL manifoldness (zero open boundary edges).
- [ ] `simply_supported_beam` and `pressure_vessel` integration tests pass against analytical references.
- [ ] `freecad-automation` SKILL.md updated with the three new workflow sections.

## Open questions

- CalculiX installation path on `ace-linux-2`: confirm the binary location and version before Task 3; if ≤2.20, evaluate whether the `*FREQUENCY` and `*BUCKLE` cards needed for design-table sweeps are stable.
- Should the parametric hull use FreeCAD's Surface workbench (NURBS) or a simpler Loft from station polylines? NURBS is closer to industrial practice but loft-from-stations is more deterministic for hydrostatics validation — flag for owner before Task 2.
