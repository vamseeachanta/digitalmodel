---
title: "OrcaWave vs WAMIT Validation Benchmark Suite"
description: "Create spec.yml, run OrcaWave, and benchmark against WAMIT reference data for 12 Orcina validation cases"
version: "1.0"
module: "diffraction"
session:
  id: "mossy-inventing-penguin"
  agent: "claude-opus-4-6"
review: "complete"
work_item: "WRK-134"
completion:
  phase_0: "complete — .owd feasibility probe, no WAMIT ref data in .owd"
  phase_1: "complete — 4 library-matched cases (2.7, 2.8, 3.2, 3.3), all pass"
  phase_2: "complete — 6 new hull library cases (2.1-2.3, 2.5c/f, 2.6), all 10 pass"
  phase_3: "complete — moonpool damping lid (2.9) + Full QTF (3.1), 2.4 blocked (mesh missing)"
  phase_4: "complete — master HTML summary, --summary-only flag, all 12 cases in table"
---

## Context

The folder `docs/modules/orcawave/L00_validation_wamit/` contains 12 validation cases (2.1-2.9, 3.1-3.3) from Orcina's official OrcaWave validation report. Each case has raw WAMIT/OrcaWave input files (.gdf meshes, .cfg configs, .owd projects) and a master PDF validation report — but NO spec.yml, NO automated benchmark scripts, and NO machine-readable WAMIT output.

Goal: Create automated, reproducible benchmark reports for each case by:
1. Creating `spec.yml` per case (DiffractionSpec format)
2. Linking all geometry to the hull library (single source of truth)
3. Adding 6 new hull shapes to the library
4. Re-running OrcaWave via existing runner infrastructure
5. Extracting WAMIT reference data (from .owd or PDF digitization)
6. Generating HTML benchmark reports (OrcaWave vs WAMIT)

## Validation Cases

| Case | Geometry | Panels | Physics | Hull Library |
|------|----------|--------|---------|-------------|
| 2.1 | Cylinder R=1, T=0.5 | 256 | Basic diffraction | **NEW** |
| 2.2 | Same + mesh variants | 256 | Mesh sensitivity | NEW (same mesh) |
| 2.3 | Same cylinder | 256 | Truncated variant | NEW (same mesh) |
| 2.4 | Hemisphere | minimal | Reference only | PARTIAL (sphere_r5 exists) |
| 2.5 | ISSC TLP | 128/fine | Mesh convergence | **NEW** (2 meshes) |
| 2.6 | Cylinder R=1 T=2 + spheroid | 112/64 | Multi-variant | **NEW** (2 shapes) |
| 2.7 | Pyramid | 408 | Convergence study | EXISTS (pyramid_zc08) |
| 2.8 | Ellipsoid + wall-sided | 96 | Mesh refinement | EXISTS (ellipsoid_96p) |
| 2.9 | Moonpool cylinder + lid | 368+32 | Damping lid | **NEW** |
| 3.1 | Bottom-mounted cylinder | 100 | Fixed structure | **NEW** |
| 3.2 | Sphere with lid + zone | 240 | Interior zone | EXISTS (sphere_r5) |
| 3.3 | Cylinder + Ellipsoid + zone | 420+220 | Multi-body | EXISTS (both) |

## Plan

### Phase 0: .owd Feasibility Probe

**Goal**: Determine if `.owd` files contain extractable WAMIT reference data.

Create `scripts/benchmark/probe_owd_content.py`:
- Load `2.7/OrcaWave v11.0 files/Pyramid.owd` via `OrcFxAPI.Diffraction`
- Inspect available attributes: RAO data, added mass, damping, comparison curves
- Check if WAMIT reference values are embedded alongside OrcaWave results
- **Decision gate**: If .owd has WAMIT data → primary extraction path. If not → PDF digitization.

### Phase 1: Library-Matched Cases (2.7, 2.8, 3.2, 3.3)

These 4 cases use geometry already in `data/hull_library/panels/primitives/`.

**Step 1.1: Create `WamitReferenceLoader`**

New file: `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py`

```python
class WamitReferenceLoader:
    @classmethod
    def from_owd(cls, owd_path: Path, vessel_name: str, water_depth: float) -> DiffractionResults:
        """Extract WAMIT comparison data from .owd file."""

    @classmethod
    def from_yaml(cls, path: Path) -> DiffractionResults:
        """Load digitized WAMIT reference data from YAML."""
```

Handles WAMIT non-dimensionalization: added mass `A_ij/(rho*L^3)`, damping `B_ij/(rho*omega*L^3)`, etc. Re-dimensionalizes to SI units matching OrcaWave output.

**Step 1.2: Create spec.yml per case**

Files: `docs/modules/orcawave/L00_validation_wamit/<case>/spec.yml`

Each spec references hull library geometry via relative path to `data/hull_library/panels/primitives/`:
- 2.7 → `pyramid_zc08.gdf`
- 2.8 → `ellipsoid_96p.gdf`
- 3.2 → `sphere_r5.gdf`
- 3.3 → `cylinder_r10_d50.gdf` + `ellipsoid_96p.gdf` (multi-body)

Parameters extracted from WAMIT `.frc` files (mass, inertia, IMODE) and `.cfg` files (water depth, frequencies).

**Step 1.3: Extract reference data**

Based on Phase 0 outcome:
- **If .owd extraction works**: Use `WamitReferenceLoader.from_owd()` directly
- **If not**: Create `reference_data.yaml` per case with digitized values from PDF

**Step 1.4: Run OrcaWave + generate benchmark reports**

Use existing pipeline:
```
spec.yml → DiffractionSpec → OrcaWaveRunner → .owr → extract → DiffractionResults
```

Then benchmark:
```python
solver_results = {"OrcaWave": orcawave_results, "WAMIT": wamit_results}
runner = BenchmarkRunner(config)
result = runner.run_from_results(solver_results, solver_metadata=metadata)
```

Output: `benchmark_output/validation/<case>/benchmark_report.html`

**Step 1.5: Extend `solver_metadata.py` for WAMIT**

Add `_build_wamit_meta()` to `scripts/benchmark/solver_metadata.py` so WAMIT solver input section is populated in HTML reports.

### Phase 2: New Hull Library Entries (Cases 2.1-2.3, 2.5, 2.6)

**Step 2.1: Add new geometries to hull library**

| hull_id | Source | Target | Panels |
|---------|--------|--------|--------|
| `val_cylinder_r1_t05` | L00/2.1/test01.gdf | `panels/primitives/` | 256 |
| `val_issc_tlp_coarse` | L00/2.5/test06.gdf | `panels/semi_subs/` | ~128 |
| `val_issc_tlp_fine` | L00/2.5/test07.gdf | `panels/semi_subs/` | fine |
| `val_cylinder_r1_t2` | L00/2.6/test05c.gdf | `panels/primitives/` | 112 |
| `val_spheroid` | L00/2.6/test05s.gdf | `panels/primitives/` | 64 |

**Step 2.2: Update `hull_panel_catalog.yaml`** with 5 new entries.

**Step 2.3: Create spec.yml + reference data + benchmark reports** for cases 2.1-2.3, 2.5, 2.6.

### Phase 3: Advanced Cases (2.4, 2.9, 3.1)

- **2.4 (Hemisphere)**: Check if sphere_r5 half-mesh works; may need parametric generation
- **2.9 (Moonpool + lid)**: Multi-body spec with `ControlSurfaceSpec` for damping lid; add moonpool mesh to library
- **3.1 (Bottom-mounted)**: Fixed structure, no RAOs — compare excitation forces instead; add BMC mesh to library

### Phase 4: Batch Runner + Master Report

**Step 4.1: Create validation config**

File: `docs/modules/orcawave/L00_validation_wamit/validation_config.yaml` — registry of all 12 cases with paths, phase, status.

**Step 4.2: Batch benchmark script**

File: `scripts/benchmark/run_validation_benchmark.py`
```bash
uv run python scripts/benchmark/run_validation_benchmark.py --case 2.7
uv run python scripts/benchmark/run_validation_benchmark.py --phase 1
uv run python scripts/benchmark/run_validation_benchmark.py --all
```

**Step 4.3: Aggregate HTML report** — master summary page linking all 12 case reports.

## Critical Files

| File | Action |
|------|--------|
| `scripts/benchmark/probe_owd_content.py` | CREATE — Phase 0 feasibility probe |
| `src/.../diffraction/wamit_reference_loader.py` | CREATE — WAMIT reference data loader |
| `scripts/benchmark/run_validation_benchmark.py` | CREATE — batch validation runner |
| `scripts/benchmark/solver_metadata.py` | EDIT — add `_build_wamit_meta()` |
| `data/hull_library/panels/primitives/*.gdf` | CREATE — 5+ new hull meshes |
| `data/hull_library/catalog/hull_panel_catalog.yaml` | EDIT — add new entries |
| `docs/modules/orcawave/L00_validation_wamit/*/spec.yml` | CREATE — 12 spec files |
| `docs/modules/orcawave/L00_validation_wamit/validation_config.yaml` | CREATE — case registry |
| `tests/hydrodynamics/diffraction/test_wamit_reference_loader.py` | CREATE — unit tests |

## Reuse (existing infrastructure)

| Component | Path |
|-----------|------|
| DiffractionSpec | `src/.../diffraction/input_schemas.py` |
| OrcaWave backend | `src/.../diffraction/orcawave_backend.py` |
| BenchmarkRunner | `src/.../diffraction/benchmark_runner.py` |
| BenchmarkPlotter | `src/.../diffraction/benchmark_plotter.py` |
| build_solver_metadata | `scripts/benchmark/solver_metadata.py` |
| run_3way_benchmark | `scripts/benchmark/run_3way_benchmark.py` (pattern reference) |
| Hull catalog | `data/hull_library/catalog/hull_panel_catalog.yaml` |

## Verification

1. **Phase 0**: `.owd` probe reports available data fields (or confirms fallback needed)
2. **Phase 1**: 4 HTML benchmark reports generated; RAO heave r > 0.99
3. **Phase 2**: 5 new hull library entries; 5 more benchmark reports
4. **Phase 3**: 3 advanced case reports (moonpool, fixed-body, hemisphere)
5. **Phase 4**: `--all` flag produces 12 reports + master summary page
6. **Hull library**: All geometry sourced from `data/hull_library/` (no local GDF copies in L00 specs)
7. **Tests**: `pytest tests/hydrodynamics/diffraction/test_wamit_reference_loader.py` passes

## Work Item

Create `WRK-134` at `D:\workspace-hub\.claude\work-queue\pending\WRK-134.md`:
- Title: OrcaWave vs WAMIT Validation Benchmark Suite
- Module: diffraction
- Related: WRK-031, WRK-132
- Blocked by: none
- Blocks: none
- Update `state.yaml`: last_id 133 → 134

## Execution Order

```
Phase 0 (probe .owd) → decision gate
  ↓
Phase 1 (4 library-matched cases: 2.7, 2.8, 3.2, 3.3)
  ↓
Phase 2 (add hull library entries + cases 2.1-2.3, 2.5, 2.6)
  ↓
Phase 3 (advanced: 2.4, 2.9, 3.1)
  ↓
Phase 4 (batch runner + master report)
```

Pause between phases for user review.
