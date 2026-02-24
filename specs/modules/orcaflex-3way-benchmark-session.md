# Session: 3-Way Benchmark Report — Phased Output Comparison

> **Date**: 2026-02-09
> **Status**: 4/5 PASSING — Pliant wave needs current profile in spec
> **Module**: `scripts/benchmark_model_library.py` + modular generator builders

---

## Objective

Validate that `spec.yml -> modular generation` produces equivalent analysis results to monolithic `.dat` files via a phased 3-way comparison:
- **Path A**: Monolithic `.dat` -> statics -> extract (baseline)
- **Path B**: Spec-driven: `.dat` -> YAML -> extract -> spec -> generate -> statics
- **Path C**: Modular-direct: library `spec.yml` -> generate -> statics

---

## Work Completed

### 1. Benchmark Script Enhancements (ALL DONE)

Five changes to `scripts/benchmark_model_library.py`:

| # | Change | Status |
|---|--------|--------|
| 1 | `--library-only` CLI flag (filters to models with library spec.yml, implies `--three-way`) | DONE |
| 2 | `_build_3way_summary()` — aggregate pass/warn/fail section after executive summary | DONE |
| 3 | B-vs-C column in per-model 3-way comparison table | DONE |
| 4 | Bending moment overlay charts (parallel to existing tension charts) | DONE |
| 5 | Library coverage summary card | DONE |

### 2. Environment Builder Fix (DONE)

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py`

**Bug**: `_build_current_profile()` could return a single-row profile, but OrcFxAPI requires `NumberOfCurrentLevels >= 2`.

**Fix**: When profile has < 2 rows, append a second row at seabed depth with the same factor. Verified — this error no longer occurs.

### 3. Monolithic YAML Investigation (DONE)

Converted all 4 A01 models + A02 Lazy S to YAML via `model.SaveData()`. Files saved to `benchmark_output/monolithic_*.yml`.

---

## Findings: 5 Errors Blocking 3-Way Comparison

All errors originate from `includes/20_generic_objects.yml` (the `GenericModelBuilder` output). Both Path B and Path C hit the same errors — the issue is in the generator, not the extractor.

### Error 1: `VariableDataSources` — WRONG SECTION NAME

| Item | Detail |
|------|--------|
| **Model** | A01 Catenary riser (and all others that have variable data) |
| **Error** | `Unrecognised context: 'VariableDataSources'` |
| **Root cause** | `generic.py` line 305/345 maps `variable_data_sources` -> `"VariableDataSources"` |
| **Monolithic YAML** | Uses `VariableData:` (NOT `VariableDataSources`) |
| **Structure** | Nested dict with sub-category keys, not a flat list |

**Monolithic format** (correct):
```yaml
VariableData:
  Dragcoefficient:
    - Name: Generic Drag
      IndependentValue, DependentValue:
        - [10e3, 1.2]
        - [50e3, 1.2]
  Linetypediameter:
    - Name: Topside BSR OD
      IndependentValue, DependentValue:
        - [0, 0.57]
```

**Generator output** (wrong):
```yaml
VariableDataSources:
  - Name: Generic Drag
    ...
```

**Fix needed**: Change `FIELD_TO_SECTION["variable_data_sources"]` from `"VariableDataSources"` to `"VariableData"`. Also need to restructure the output to nest objects under their `data_type` sub-category key.

**Note**: The pipeline `vardata_builder.py` (line 83) correctly outputs `"VariableData"` — only the generic builder path is wrong.

### Error 2: `AttachmentType=Module` — LOAD ORDER ISSUE

| Item | Detail |
|------|--------|
| **Model** | A01 Lazy wave riser |
| **Error** | `Failed to set AttachmentType[1]=Module (Invalid value)` |
| **Monolithic YAML** | Has `AttachmentType: Module` referencing ClumpType named "Module" |
| **Root cause** | ClumpTypes section must be loaded BEFORE Lines that reference them |

**Monolithic format** (line attachments for buoyancy modules):
```yaml
AttachmentType, Attachmentx, Attachmenty, Attachmentz, AttachmentzRelativeTo:
  - [Module, 0, 0, 81.25, End A]
  - [Module, 0, 0, 83.75, End A]
  # ... 20 modules, 2.5m spacing
```

The `Module` value references a ClumpType defined in the `ClumpTypes` section. The issue is that in the generated YAML, ClumpTypes may not be defined before the Lines section tries to reference them.

**Fix needed**: Ensure builder ordering places ClumpTypes before Lines, or confirm that the generic builder emits sections in dependency order.

### Error 3: `AttachmentType=Clamp Type` — SAME LOAD ORDER

| Item | Detail |
|------|--------|
| **Model** | A01 Pliant wave riser |
| **Error** | `Failed to set AttachmentType[1]=Clamp Type (Invalid value)` |
| **Root cause** | Same as Error 2 — ClumpType "Clamp Type" must exist before reference |

### Error 4: `MaterialDensity=1 (Change not allowed)` — CATEGORY/PROPERTY ORDER

| Item | Detail |
|------|--------|
| **Model** | A01 Steep wave riser |
| **Error** | `Failed to set MaterialDensity=1 (Change not allowed)` |
| **Monolithic YAML** | `MaterialDensity` only valid for `Category: Homogeneous pipe` |
| **Root cause** | Property can only be set AFTER `Category` is set to `Homogeneous pipe` |

**Monolithic format**:
```yaml
LineTypes:
  - Name: BSR Cone
    Category: Homogeneous pipe    # Must come first
    OD: Topside BSR OD
    ID: 0.36
    MaterialDensity: 1            # Only valid after Category is set
    E: 100e3
```

When `Category: General` (default), the density property is `MassPerUnitLength` instead. The generic builder's `_merge_object()` puts properties in dict order, which may not place `Category` before `MaterialDensity`.

**Fix needed**: Ensure `Category` is emitted first in line type objects, before category-dependent properties. Alternatively, sort properties so `Category` always comes before `MaterialDensity`, `OD`, `ID`, `E`, etc.

### Error 5: `SupportType=U support 0.6` — SECTION ORDER

| Item | Detail |
|------|--------|
| **Model** | A02 Lazy S detailed |
| **Error** | `Failed to set SupportType=U support 0.6 (Invalid value)` |
| **Monolithic YAML** | `SupportTypes` section defines 7 U-shaped supports |
| **Root cause** | `SupportTypes` must be defined before Shapes/Lines that reference them |

**Monolithic format**:
```yaml
SupportTypes:
  - Name: U support 0.6
    Geometry: U shaped
    NormalStiffness: 1000
    USupportHorizontalLength: 0.6
    USupportVerticalLength: 0.6
```

---

## Root Cause Analysis

The errors fall into **two categories**:

### Category A: Wrong Section Names (Error 1)
The `FIELD_TO_SECTION` mapping uses `"VariableDataSources"` but OrcaFlex expects `"VariableData"` with nested sub-categories.

### Category B: YAML Load Order (Errors 2-5)
OrcaFlex loads YAML sequentially and validates references as it goes. When a property references another object (ClumpType, StiffenerType, SupportType), that object must already be defined. The `GenericModelBuilder` emits sections in `FIELD_TO_SECTION` dict order, which may not match the required dependency order.

The monolithic YAML (from `model.SaveData()`) outputs sections in the correct dependency order. The generic builder needs to either:
1. Emit sections in a fixed order that respects dependencies
2. Or sort the output dict before YAML serialization

**OrcaFlex section dependency order** (from monolithic analysis):
```
General -> VariableData -> LineTypes -> VesselTypes -> ClumpTypes ->
StiffenerTypes -> SupportTypes -> Vessels -> Lines -> Shapes ->
6DBuoys -> 3DBuoys -> Constraints -> Links -> Winches
```

---

## Files Modified This Session

| File | Change |
|------|--------|
| `scripts/benchmark_model_library.py` | 5 enhancements: --library-only, 3-way summary, B-vs-C column, bending charts, library card |
| `src/.../builders/environment_builder.py` | Fix: ensure >= 2 current profile levels |

## Files Created This Session (investigation artifacts)

| File | Purpose |
|------|---------|
| `benchmark_output/monolithic_catenary.yml` | Monolithic YAML for keyword analysis |
| `benchmark_output/monolithic_lazy_wave.yml` | Monolithic YAML for keyword analysis |
| `benchmark_output/monolithic_steep_wave.yml` | Monolithic YAML for keyword analysis |
| `benchmark_output/monolithic_lazy_s.yml` | Monolithic YAML for keyword analysis |

---

## Next Steps (Priority Order)

### P0: Fix `VariableDataSources` -> `VariableData` (Error 1)
1. In `schema/generic.py`: Change `SECTION_REGISTRY` and `FIELD_TO_SECTION` key from `"VariableDataSources"` to `"VariableData"`
2. In `GenericModelBuilder.build()`: Restructure output to nest variable data objects under their `data_type` sub-category (matching monolithic format)
3. Verify pipeline `vardata_builder.py` is not affected

### P1: Fix YAML section ordering (Errors 2-5)
1. In `GenericModelBuilder.build()` or the YAML writer: Enforce section output order matching OrcaFlex dependency chain
2. Candidate approach: Define a `SECTION_ORDER` list and sort the output dict before YAML serialization
3. Also ensure `Category` is emitted before category-dependent properties within each object

### P2: Re-run benchmark
```bash
uv run python scripts/benchmark_model_library.py --library-only --skip-mesh
```
Expect: 5/5 models with 3-way data populated, report shows comparison tables and charts.

### P3: Phase 2 — Full library
```bash
uv run python scripts/benchmark_model_library.py --three-way
```

---

## Execution Commands

```bash
# Phase 1 (library models only, fast):
uv run python scripts/benchmark_model_library.py --library-only --skip-mesh

# Phase 1 with mesh sensitivity:
uv run python scripts/benchmark_model_library.py --library-only

# Phase 2 (all 61 models):
uv run python scripts/benchmark_model_library.py --three-way

# Regenerate HTML from saved JSON (no re-run):
uv run python scripts/benchmark_model_library.py --html-only

# Run riser-specific benchmark (known working):
uv run python scripts/benchmark_riser_library.py
```

---

## Results: Phase 1 (2026-02-09)

### GenericModelBuilder Fixes Applied

| Fix | File | Change |
|-----|------|--------|
| P0: VariableData section name | `schema/generic.py` | `VariableDataSources` → `VariableData` in `SECTION_REGISTRY` and `FIELD_TO_SECTION` |
| P0: VariableData nesting | `builders/generic_builder.py` | New `_build_variable_data()` groups objects by `data_type` sub-category |
| P0: Extractor key | `extractor.py` | `_VARIABLE_DATA_SCHEMA_KEY` updated to `"VariableData"` |
| P1: Section ordering | `builders/generic_builder.py` | New `_SECTION_ORDER` list + `_order_sections()` enforces OrcaFlex dependency order |
| P1: Property ordering | `builders/generic_builder.py` | `_PRIORITY_KEYS` ensures mode-setting props (Category, ShapeType, BuoyType, etc.) come first in `_merge_object()` |
| Env fix: Current levels | `builders/environment_builder.py` | Pad current profile to >= 2 rows for OrcFxAPI minimum |

### Benchmark Results (--library-only --skip-mesh)

| Model | Monolithic | Spec-Driven (B) | Modular-Direct (C) | Notes |
|-------|-----------|-----------------|-------------------|-------|
| A01 Catenary riser | CONVERGED | CONVERGED | CONVERGED | All paths match |
| A01 Lazy wave riser | CONVERGED | CONVERGED | CONVERGED | All paths match |
| A01 Pliant wave riser | CONVERGED | NOT CONVERGED | NOT CONVERGED | Spec missing current profile (3-level → 2-level uniform) |
| A01 Steep wave riser | CONVERGED | CONVERGED | CONVERGED | All paths match |
| A02 Lazy S detailed | CONVERGED | CONVERGED | CONVERGED | All paths match |

**Score: 4/5 passing (80%)**

### Remaining Issue: Pliant Wave Current Profile

The pliant wave spec defines only `speed: 0.7, direction: 270` with no `profile` field. The default single-point profile gets padded to `[[0, 1.0], [100, 1.0]]` (uniform), but the monolithic model has a depth-varying profile: `[[0, 1.0], [70, 0.9], [100, 0.3]]`. This environmental difference prevents statics convergence.

**Fix**: Add `profile: [[0, 1.0], [70, 0.9], [100, 0.3]]` to the pliant wave spec's `current` section.
