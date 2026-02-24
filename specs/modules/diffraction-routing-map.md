---
title: "Diffraction Analysis Routing Map"
description: "Architecture, data flow, and work item coordination for the diffraction/hydrodynamics pipeline"
version: "1.0"
module: "diffraction"
session:
  id: "mossy-inventing-penguin"
  agent: "claude-opus-4-6"
review: "pending"
---

## Overview

This document maps the complete diffraction analysis pipeline: architecture, data flows, solver integration, hull infrastructure, and related work items. It serves as the coordination reference for all diffraction-related development.

## Architecture: Spec-Driven Solver Pipeline

```
spec.yml (DiffractionSpec)
    |
    v
[Input Validation]  ──────────────── input_schemas.py (659 LOC)
    |
    +────────────────────────────+
    |                            |
    v                            v
AQWA Backend                OrcaWave Backend
(aqwa_backend.py, 807)      (orcawave_backend.py, 587)
    |                            |
    v                            v
.dat (FORTRAN deck)         .yml (OrcaWave model)
    |                            |
    v                            v
aqwa_runner.py (616)        orcawave_runner.py (600)
    |                            |
    v                            v
.LIS + .HYD (binary)       .owr + .sim (binary)
    |                            |
    v                            v
aqwa_lis_parser.py (551)    orcawave_converter.py (382)
aqwa_result_extractor (104) orcawave_data_extraction (448)
    |                            |
    +────────────────────────────+
    |
    v
DiffractionResults  ─────────────── output_schemas.py (435 LOC)
    |
    v
[Output Validation]  ──────────────  output_validator.py (438)
    |
    +────────────────+───────────────+
    |                |               |
    v                v               v
comparison_      benchmark_      orcaflex_
framework (516)  plotter (1586)  exporter (460)
    |                |               |
    v                v               v
Statistics       HTML/JSON       OrcaFlex YAML
(deviations)     reports         (RAO import)
```

## Key Schema Types

### Input: DiffractionSpec

| Section | Key Fields | Notes |
|---------|-----------|-------|
| `vessel` | name, type, geometry, inertia, external_damping | Single-body mode |
| `bodies` | list[BodySpec] | Multi-body mode (future) |
| `environment` | water_depth, water_density | SI units (m, kg/m3) |
| `frequencies` | input_type (period/frequency), values | rad/s or seconds |
| `wave_headings` | values (degrees), symmetry | 0-360 convention |
| `solver_options` | remove_irreg_freq, qtf_calculation, precision | Solver control |
| `outputs` | formats, components, directory | Result configuration |

### Output: DiffractionResults

| Component | Shape | Description |
|-----------|-------|-------------|
| RAOs | (n_freq, n_heading) per DOF | Complex amplitude + phase |
| Added Mass | (6, 6, n_freq) | Frequency-dependent mass matrix |
| Damping | (6, 6, n_freq) | Radiation damping matrix |
| Excitation | (6, n_freq, n_heading) | Wave excitation forces |
| Mean Drift | (6, n_heading) | 2nd-order mean drift forces |
| QTF | (6, 6, n_freq, n_freq, n_heading) | Quadratic transfer functions |

## Hull Library

```
data/hull_library/
├── catalog/hull_panel_catalog.yaml      23 entries, 7 categories
├── panels/
│   ├── barges/      (2: unit_box, 100x20x10)
│   ├── ships/       (1: l01_vessel_385p)
│   ├── semi_subs/   (3: OC4, L03 centre/outer)
│   ├── lngc/        (2: 125k, 180k)
│   ├── fpso/        (1: FST 204m)
│   ├── spars/       (1: r=10m)
│   └── primitives/  (4: cylinder, sphere, ellipsoid, pyramid)
├── profiles/        YAML hull section definitions
└── schematics/      SVG visualization (body plan, profile, plan views)
```

## Benchmark Library

| Hull | Revision | Spec | Status | Key Metric |
|------|----------|------|--------|-----------|
| Barge | r5 | `L02_barge_benchmark/spec.yml` | PASS | heave r=1.000, roll r=0.997 |
| Ship | r2 | `L03_ship_benchmark/spec.yml` | SPLIT | roll r=0.263 (solver-inherent) |
| Spar | r2 | `L04_spar_benchmark/spec.yml` | PASS | heave r=1.000, surge r=0.998 |

**Benchmark output**: `benchmark_output/wrk132_rerun/` (ship_r2, barge_r5, spar_r2)

## Known Bugs (Open)

| # | Location | Issue | Impact |
|---|----------|-------|--------|
| 1 | `orcawave_backend.py:333` | Hardcodes SolveType, ignores `qtf_calculation` | OrcaWave always runs Potential+source (level 2/6) |
| 2 | `aqwa_backend.py:412` | Hardcodes `RESTART 1 5`, should be `1 8` for QTF | AQWA never runs QTF even when requested |
| 3 | `SolverOptions.qtf_calculation` | Boolean can't represent 6 OrcaWave SolveType levels | Enum needed for full solver control |

## External Damping Asymmetry (Resolved)

- **AQWA FIDP**: Only affects time-domain response (stages 6+), NOT frequency-domain RAOs
- **OrcaWave BodyExternalDamping**: Included in frequency-domain RAOs
- **Recommendation**: Remove external_damping from spec.yml for RAO-only benchmarks; use `analysis_type: diffraction` until Bug 2 fixed

## Work Item Routing

### Dependency Graph

```
COMPLETED                    ACTIVE                         NEXT
─────────                    ──────                         ────
WRK-132 (benchmarks) ──────► WRK-031 (75%, 3-way fw) ────► WRK-126 (all-model bench)
                                                                    |
WRK-114 (hull catalog) ────► WRK-110 (expand hulls) ──┐           |
                             WRK-115 (RAO linking)  ◄──┤           |
                             WRK-116 (mesh scaling)    │           v
                             WRK-117 (mesh refine) ◄───┘    WRK-130 (diffr. reports)
                                                                    |
                                                                    v
WRK-128 (prop routing) ────► WRK-039 (SPM benchmark) ────► WRK-131 (passing ship)
```

### Items by Tier

**Tier 1 — Foundation (largely complete)**

| ID | Title | Status | Priority |
|----|-------|--------|----------|
| WRK-132 | Benchmark refinement (barge/ship/spar) | DONE | — |
| WRK-031 | 3-way solver framework | 75% | Fix unit box or substitute |
| WRK-126 | Full model library benchmark + seed equivalence | Pending | High — regression baseline |

**Tier 2 — Infrastructure & Reporting**

| ID | Title | Status | Blocked By |
|----|-------|--------|-----------|
| WRK-128 | Property routing (3 routes) | Pending | — |
| WRK-129 | OrcaFlex structure reporting | Pending | — |
| WRK-130 | Diffraction reporting per hull type | Pending | — |

**Tier 3 — Hull Infrastructure**

| ID | Title | Status | Blocked By |
|----|-------|--------|-----------|
| WRK-110 | Expand hull library (FST, LNGC, benchmark) | Pending | Plan review |
| WRK-115 | Link RAOs to hull catalog entries | Pending | WRK-110 |
| WRK-116 | Scale hull meshes to target dimensions | Pending | WRK-110 |
| WRK-117 | Mesh refinement/coarsening families | Pending | WRK-116 |
| WRK-101 | Mesh decimation (QEM algorithm) | Pending | — |

**Tier 4 — Applications**

| ID | Title | Status | Blocked By |
|----|-------|--------|-----------|
| WRK-039 | SPM benchmarking AQWA vs OrcaFlex | Pending | AQWA input generation |
| WRK-131 | Passing ship analysis | Pending | WRK-130 |
| WRK-043 | Parametric hull form + RAO generation | Pending | WRK-116, WRK-117 |

### Recommended Execution Order

1. **Bug fixes** (Bugs 1-3 above) — unblock QTF analysis for ship/future models
2. **WRK-130** (diffraction reporting) — standardize output across all hull types
3. **WRK-126** (full benchmark) — establish regression baseline with all 51+ models
4. **WRK-110 → WRK-115** (hull expansion + RAO linking) — grow the geometry library
5. **WRK-116 → WRK-117** (mesh scaling + refinement) — enable convergence studies
6. **WRK-039** (SPM benchmark) — cross-domain validation
7. **WRK-131** (passing ship) — first applied engineering analysis

## Critical File Index

### Schemas (define the data contracts)
- `src/.../diffraction/input_schemas.py` — DiffractionSpec (659 LOC)
- `src/.../diffraction/output_schemas.py` — DiffractionResults (435 LOC)

### Backends (spec → solver input conversion)
- `src/.../diffraction/aqwa_backend.py` — AQWA .dat generation (807 LOC)
- `src/.../diffraction/orcawave_backend.py` — OrcaWave .yml generation (587 LOC)

### Runners (solver execution)
- `src/.../diffraction/aqwa_runner.py` — AQWA execution (616 LOC)
- `src/.../diffraction/orcawave_runner.py` — OrcaWave execution (600 LOC)

### Parsers (solver output → DiffractionResults)
- `src/.../diffraction/aqwa_lis_parser.py` — .LIS binary parsing (551 LOC)
- `src/.../diffraction/orcawave_converter.py` — .owr extraction (382 LOC)
- `src/.../diffraction/reverse_parsers.py` — Multi-format parsing (746 LOC)

### Comparison & Reporting
- `src/.../diffraction/comparison_framework.py` — Statistical analysis (516 LOC)
- `src/.../diffraction/benchmark_plotter.py` — HTML plot generation (1586 LOC)
- `src/.../diffraction/benchmark_runner.py` — Orchestration (731 LOC)
- `src/.../diffraction/multi_solver_comparator.py` — 3-way comparison (584 LOC)

### Mesh Pipeline
- `src/.../diffraction/mesh_pipeline.py` — GDF/STL/DAT loading (323 LOC)
- `src/.../diffraction/geometry_quality.py` — Mesh validation (487 LOC)
- `src/.../bemrosetta/mesh/` — Format handlers (dat, gdf, stl)

### Benchmark Scripts
- `scripts/benchmark/run_3way_benchmark.py` — Master 3-way benchmark (1529 LOC)
- `scripts/benchmark/rerun_benchmarks_with_fidp.py` — FIDP investigation (1052 LOC)
- `scripts/benchmark/audit_solver_inputs.py` — Input validation (794 LOC)
- `scripts/benchmark/compare_hydro_properties.py` — Coefficient comparison (1311 LOC)

### Specs
- `docs/modules/orcawave/L02_barge_benchmark/spec.yml`
- `docs/modules/orcawave/L03_ship_benchmark/spec.yml`
- `docs/modules/orcawave/L04_spar_benchmark/spec.yml`

## Module Statistics

| Area | Files | LOC (approx) |
|------|-------|-------------|
| Diffraction pipeline | 33 | 15,800 |
| AQWA legacy module | 20 | 5,200 |
| BEMRosetta mesh tools | 21 | 4,500 |
| Hull library | 10 | 1,500 |
| Core models/utilities | 6 | 1,400 |
| Benchmark scripts | 12 | 7,000 |
| Tests (diffraction) | 20+ | ~4,000 |
| **Total** | **~120** | **~39,400** |
