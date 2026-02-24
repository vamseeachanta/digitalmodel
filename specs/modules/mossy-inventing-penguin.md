---
title: "Enrich OrcaWave Benchmark Reports with Full Solver Input Metadata"
description: "Extract comprehensive OrcaWave parameters from SaveData() YAML exports and display in benchmark HTML reports"
version: "1.0"
module: "diffraction"
session:
  id: "mossy-inventing-penguin"
  agent: "claude-opus-4-6"
review: "complete"
work_item: "WRK-134-enhancement"
---

## Context

The validation benchmark HTML reports (WRK-134) currently show sparse solver input sections. The root cause: `build_solver_metadata()` in `solver_metadata.py` expects `spec["vessel"]` but DiffractionSpec uses `spec["bodies"]`, so most fields resolve to empty.

The user wants ALL relevant OrcaWave parameters displayed, sourced directly from the OrcaWave YAML export (`diff.SaveData()` output) — not the spec.yml abstraction.

## Plan

### Step 1: Save .owd input as .yml (`validate_owd_vs_spec.py`)

In `solve_owd()` — call `diff.SaveData()` BEFORE `diff.Calculate()` to export the input configuration:

```python
yml_path = out_dir / f"{owd_path.stem}_input.yml"
diff.SaveData(str(yml_path.resolve()))
```

Return `(DiffractionResults, yml_path)` tuple instead of just `DiffractionResults`.

Same for `_solve_spec_via_orcfxapi()` — after `diff.LoadData()` but before `diff.Calculate()`:

```python
spec_input_yml = out_dir / "spec_input.yml"
diff.SaveData(str(spec_input_yml.resolve()))
```

Update `run_case()` to capture both yml_paths and pass them to `run_comparison()`.

### Step 2: Create `build_orcawave_metadata_from_yml()` (`solver_metadata.py`)

New function that reads an OrcaWave YAML and extracts ALL relevant parameters:

**General settings:** UnitsSystem, SolveType, LoadRAOCalculationMethod, LinearSolverMethod, DivideNonPlanarPanels, LengthTolerance, WavesReferredToBy

**Environment:** WaterDepth, WaterDensity

**Frequencies/Headings:** PeriodOrFrequency (count, min, max), WaveHeading (count, min, max)

**Body (from Bodies[body_index]):** BodyName, BodyMeshFileName, BodyMeshFormat, BodyMeshSymmetry, BodyInertiaSpecifiedBy, BodyCentreOfMassZRelativeToFreeSurface, BodyRadiiOfGyration (diagonal), BodyAddInteriorSurfacePanels + method, BodyConnectionParent, BodyFixedDOFs, BodyIncreaseRollDampingToTarget, BodyExternalDampingMatrix (non-zero summary), BodyExternalStiffnessMatrix (non-zero summary)

**Damping lid (conditional):** HasResonanceDampingLid, DampingLidMeshFileName, DampingFactorEpsilon

**QTF (conditional):** QTFCalculationMethod, QTFFrequencyTypes, QTFMin/MaxCrossingAngle, IncludeMeanDriftFullQTFs

**Free surface zone (conditional):** FreeSurfacePanelledZoneType, mesh, inner_radius

Returns `dict[str, str]` with `ow_` prefixed keys for OrcaWave-specific params plus existing keys (mesh_file, mass, etc.) for backward compat.

Encoding: try `utf-8` then `latin-1` (OrcaWave degree symbol issue).

### Step 3: Expand `_PARAM_ROWS` (`benchmark_plotter.py`)

Add 3 new sections with ~19 rows to the existing 7 sections:

```
-- OrcaWave Solver Settings --
Units system, Solve type, Load RAO method, Linear solver,
Divide non-planar panels, Length tolerance, Waves referred to by

-- OrcaWave Body Details --
Body count, Body name, Inertia specification, Interior surface panels,
Connection parent, Fixed DOFs, Roll damping target,
External damping, External stiffness

-- OrcaWave Advanced (conditional) --
Damping lid mesh, Damping factor (epsilon),
QTF crossing angles, QTF frequency types, QTF calc method,
Include mean drift QTFs,
Free surface zone type, Free surface zone mesh, Free surface inner radius
```

Missing keys render as "-" (existing behavior) — AQWA reports unaffected.

Filter: skip rendering section headers when ALL rows in that section are "-".

### Step 4: Wire metadata in `run_comparison()` (`validate_owd_vs_spec.py`)

```python
metadata = {
    "OrcaWave (.owd)": build_orcawave_metadata_from_yml(owd_yml, body_index=...),
    "OrcaWave (spec.yml)": build_orcawave_metadata_from_yml(spec_yml, body_index=...),
}
```

Set `input_file` key to the .yml path for the file viewer (scrollable YAML preview in report).

### Step 5: Fix `_extract_from_spec()` bodies fallback (`solver_metadata.py`)

One-line fix so the existing function works with DiffractionSpec `bodies` format:

```python
if not vessel and "bodies" in spec:
    bodies = spec["bodies"]
    if isinstance(bodies, list) and len(bodies) > 0:
        vessel = bodies[0].get("vessel", {})
```

## Critical Files

| File | Action |
|------|--------|
| `scripts/benchmark/solver_metadata.py` | EDIT — add `build_orcawave_metadata_from_yml()`, fix `_extract_from_spec()` |
| `src/.../diffraction/benchmark_plotter.py` | EDIT — expand `_PARAM_ROWS` with 3 OrcaWave sections |
| `scripts/benchmark/validate_owd_vs_spec.py` | EDIT — save .yml exports, wire new metadata |

## Verification

1. Run `uv run python scripts/benchmark/validate_owd_vs_spec.py --case 2.1` — report shows full OrcaWave input section
2. Run `--case 2.9` — damping lid section populated (epsilon=0.016)
3. Run `--case 3.1` — QTF + free surface zone sections populated
4. Run `--all` — all 11 cases show enriched input sections, no regressions
5. Existing AQWA benchmark reports unaffected (OrcaWave sections show "-")
