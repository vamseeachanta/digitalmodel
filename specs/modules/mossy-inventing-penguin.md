---
title: "Fix & Re-run All 3 Diffraction Benchmarks (Barge/Ship/Spar)"
description: "Fix bugs in rerun script (spar .owr path, ship water_depth), re-run all 3 hull benchmarks, commit spec.yml fix"
version: "3.1"
module: "diffraction"
session:
  id: "mossy-inventing-penguin"
  agent: "claude-opus-4-6"
review: "done"
parent_work_item: "WRK-132"
completed: "2026-02-12"
results:
  ship: "6/6 DOFs, consensus SPLIT (roll solver-inherent)"
  barge: "6/6 DOFs, all correlations unchanged"
  spar: "6/6 DOFs, was SKIPPED now complete"
  commit: "3cb38c2b"
  routing_map: "specs/modules/diffraction-routing-map.md"
---

## Context

WRK-132 (diffraction benchmark refinement) is complete but the final rerun script has 2 bugs that prevented the spar benchmark from running and used incorrect ship water depth. The user wants all 3 benchmarks (barge, ship, spar) to produce complete results with both AQWA and OrcaWave values, and an investigation into input value differences.

**Current state**:
- Ship: ran successfully with FIDP but used `SHIP_WATER_DEPTH = 30.0` instead of 500.0
- Barge: ran successfully, no changes needed
- Spar: SKIPPED — wrong .owr path in script

**Root causes**:
1. `rerun_benchmarks_with_fidp.py:80` — `SHIP_WATER_DEPTH = 30.0` should be `500.0` (confirmed from LIS and spec.yml)
2. `rerun_benchmarks_with_fidp.py:95` — `SPAR_OWR` points to non-existent path `L04_spar_benchmark/benchmark_results/spar_benchmark.owr`; actual file is at `benchmark_output/spar_benchmark/r1_spar_benchmark/spar_benchmark.owr`

## Plan

### Step 1: Fix rerun script bugs (2 edits)

**File**: `scripts/benchmark/rerun_benchmarks_with_fidp.py`

| Line | Current | Fix |
|------|---------|-----|
| 80 | `SHIP_WATER_DEPTH = 30.0` | `SHIP_WATER_DEPTH = 500.0  # confirmed from LIS + spec.yml` |
| 95 | `SPAR_OWR = SPAR_BENCHMARK_DIR / "benchmark_results" / "spar_benchmark.owr"` | `SPAR_OWR = Path("benchmark_output/spar_benchmark/r1_spar_benchmark/spar_benchmark.owr")` |

### Step 2: Re-run all 3 benchmarks

```bash
uv run python scripts/benchmark/rerun_benchmarks_with_fidp.py
```

**Expected**: Ship + Barge + Spar all complete. Spar should no longer show `[ERROR] Spar .owr not found`.

Ship water_depth fix may slightly change ship correlations (benchmark extraction uses water_depth for normalization context). Barge should be unchanged.

### Step 3: Run input parameter audit

```bash
uv run python scripts/benchmark/audit_solver_inputs.py
```

This script already exists and compares all input parameters (mass, CoG, radii of gyration, external damping, frequencies, headings) across barge/ship/spar for both solvers. Review output for mismatches.

### Step 4: Commit ship spec.yml water_depth fix

The ship spec.yml (`docs/modules/orcawave/L03_ship_benchmark/spec.yml`) has an uncommitted change: `water_depth: 30.0` → `500.0`. Commit this along with the rerun script fixes.

## Critical Files

| File | Action |
|------|--------|
| `scripts/benchmark/rerun_benchmarks_with_fidp.py` | EDIT lines 80, 95 |
| `docs/modules/orcawave/L03_ship_benchmark/spec.yml` | COMMIT (already modified, uncommitted) |
| `scripts/benchmark/audit_solver_inputs.py` | RUN (no changes) |

## Verification

1. All 3 hulls produce HTML + JSON reports in `benchmark_output/wrk132_rerun/`
2. Spar benchmark completes (was SKIPPED before)
3. Ship correlations stable or improved with correct water_depth
4. Barge correlations unchanged
5. Input audit shows no unexpected parameter mismatches
6. `benchmark_output/wrk132_rerun/fidp_benchmark_summary.json` includes all 3 hulls

---
---

# ARCHIVED: WRK-121 Plan (s7 Model Extraction)

> Below is the original WRK-121 plan preserved for reference.

## Context (WRK-121)

The `rock-oil-field/s7/` directory contains ~235 unique OrcaFlex `.dat` models spanning jumper installation, umbilical laying, manifold deployment, mooring, mudmat, and training scenarios. These models should be:
1. **Sanitized** — strip all client-identifiable references
2. **Converted** — `.dat` (binary) → `.yml` (YAML)
3. **Cataloged** — organized in `docs/modules/orcaflex/<category>/`
4. **spec.yml generated** — each model gets a `spec.yml` so it can be regenerated via the existing modular builder
5. **Round-trip validated** — spec → modular generation → compare with monolithic original

The existing `MonolithicExtractor` + `GenericModel` schema already supports extracting spec.yml from arbitrary OrcaFlex models (proven on 47/49 models in the 3-way benchmark). Jumpers, installation models, and mooring will use the `generic` field in `ProjectInputSpec`.

## Existing Infrastructure (reuse, don't rebuild)

| Component | Path | Purpose |
|-----------|------|---------|
| MonolithicExtractor | `src/.../modular_generator/extractor.py` | .yml → spec.yml extraction |
| GenericModel schema | `src/.../modular_generator/schema/generic.py` | Arbitrary OrcaFlex object support |
| GenericModelBuilder | `src/.../modular_generator/builders/generic_builder.py` | spec.yml → modular YAML |
| ProjectInputSpec | `src/.../modular_generator/schema/root.py` | Root schema (pipeline/riser/generic) |
| convert_cli.py | `src/.../orcaflex/convert_cli.py` | .dat ↔ .yml batch conversion |
| benchmark_model_library.py | `scripts/benchmark_model_library.py` | 3-way benchmark (A→B→C paths) |
| semantic_validate.py | `scripts/semantic_validate.py` | Section-by-section monolithic vs modular |
| legal-sanity-scan.sh | `workspace-hub/scripts/legal/legal-sanity-scan.sh` | Client reference scanning |
| 4 jumper YAMLs | `docs/modules/orcaflex/mooring/rigid_jumper/` | Already-converted jumper models |
| Jumper doc | `docs/modules/orcaflex/jumper/jumper.md` | Existing jumper overview |

---

## Step 1: Create Sanitization Script & Deny List

### 1a. Sanitization mapping

| Original | Sanitized | Category |
|----------|-----------|----------|
| Ballymore | deepwater_field_a | Project |
| Candies / WyatteCandies / WyattCandies | installation_vessel_01 / iv01_type | Vessel |
| Seven Arctic / 7arctic | installation_vessel_02 / region_a | Vessel / Region |
| 7seas / Seven Seas | region_b | Region |
| Shell Perdido / Perdido South | deepwater_field_b | Project |
| Talos Venice / talos_venice | deepwater_field_c | Project |
| BP_MD2_FJR | regional_design_01 | Project |
| ONGC | operator_a | Client |
| Limerock / limerock | pipeline_route_a | Location |
| SS7A2365, USNGL23LR3, ss7a2365 | _(remove)_ | User/Machine |
| `User:` / `Machine:` / `File:` YAML headers | _(remove)_ | Metadata |

### 1b. Files to create

- **`scripts/sanitize_s7_models.py`** — Main script:
  1. Walk `s7/` directory, find all `.dat` and standalone `.yml` files
  2. Dedup (hash-based: skip files with identical content)
  3. For `.dat`: Load via `OrcFxAPI.Model()` → rename matching objects → `SaveData()` as `.yml`
  4. For `.yml`: Text-based regex replacement using mapping table
  5. Strip YAML header metadata (User, Machine, File path lines)
  6. Output sanitized `.yml` to staging directory with category-based folder structure
  7. Log all transformations to `sanitization_audit.json`

- **`digitalmodel/.legal-deny-list.yaml`** — Deny list with block-severity patterns for all original names

### 1c. Exclusions

- `shell_perdido_south/mudmat_tool/rev1/` — two 84MB files (rev2/rev3 are 17-110KB)
- Duplicate copies (e.g., `Seven Arctic_v11.dat` × 4, `__01a_7Seas*.dat` × 4)
- Parametric `.yml` variants (talos_venice/infield bulk, sut_mm/01-SZ-Xdeg seeds)

**Estimated unique models after dedup**: ~180 `.dat` + ~40 standalone `.yml`

## Step 2: Convert, Sanitize & Organize Models

### 2a. Target directory structure

```
docs/modules/orcaflex/
├── jumper/                              # User-specified target
│   ├── manifold_to_plet/
│   │   ├── monolithic/                  # Sanitized full YAML
│   │   │   ├── DZ_AHCoff.yml
│   │   │   └── SZ.yml
│   │   └── spec.yml                     # Extracted spec for builder
│   ├── plet_to_plem/
│   │   ├── monolithic/
│   │   │   ├── DZ_AHCoff.yml
│   │   │   └── SZ.yml
│   │   └── spec.yml
│   └── sut_mm/
│       ├── monolithic/
│       │   ├── SZ_base.yml
│       │   ├── DZ_no_AHC.yml
│       │   ├── DZ_AHC.yml
│       │   └── resonance/
│       └── spec.yml
│
├── installation/                        # NEW
│   ├── umbilical/
│   │   ├── first_end/
│   │   │   ├── monolithic/ (21 step .yml files)
│   │   │   └── spec.yml    (one spec per phase)
│   │   └── second_end/
│   │       ├── monolithic/ (21 step .yml files)
│   │       └── spec.yml
│   ├── manifold/
│   │   ├── region_a/
│   │   │   ├── monolithic/ (SZ/DZ variants)
│   │   │   └── spec.yml
│   │   └── region_b/
│   │       ├── monolithic/
│   │       └── spec.yml
│   ├── mudmat/
│   │   ├── monolithic/ (rev2 + rev3 files)
│   │   └── spec.yml
│   └── pipeline/
│       ├── route_a/
│       │   ├── monolithic/ (init, NL, ld, 2nd_deck steps)
│       │   └── spec.yml
│       └── route_b/
│           ├── monolithic/ (limerock steps)
│           └── spec.yml
│
├── mooring/                             # EXISTING - extend
│   ├── reference/
│   │   ├── monolithic/
│   │   │   ├── baseline_no_interference.yml
│   │   │   ├── single_interference.yml
│   │   │   ├── wd98_105_pretension.yml
│   │   │   └── wd98_65_pretension.yml
│   │   └── spec.yml
│   └── feeding_mooring_line/
│       ├── monolithic/
│       │   └── feeding_mooring_line.yml
│       └── spec.yml
│
├── training/                            # NEW
│   ├── crane_master/
│   │   ├── monolithic/ (CM2, CM3)
│   │   └── spec.yml
│   ├── hulls/
│   │   ├── monolithic/
│   │   └── spec.yml
│   └── node_feeding/
│       ├── monolithic/
│       └── spec.yml
│
└── regional/                            # NEW
    ├── monolithic/
    └── spec.yml
```

**Pattern**: Every model group gets `monolithic/` (sanitized original) + `spec.yml` (extracted spec).

### 2b. Move existing jumper YAMLs

Move `docs/modules/orcaflex/mooring/rigid_jumper/*.yml` → `docs/modules/orcaflex/jumper/*/monolithic/`

## Step 3: Extract spec.yml for All Models

### 3a. Extraction pipeline (per model group)

```python
from digitalmodel.solvers.orcaflex.modular_generator.extractor import MonolithicExtractor
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

# For each sanitized monolithic .yml:
extractor = MonolithicExtractor(Path("monolithic/model.yml"))
spec_dict = extractor.extract()

# Override metadata for correct classification
spec_dict["metadata"]["structure"] = "jumper"  # or "mooring", "installation", etc.
spec_dict["metadata"]["operation"] = "in-place"  # or "installation"
spec_dict["metadata"]["project"] = "Reference Library"

# Validate schema
spec = ProjectInputSpec(**spec_dict)

# Save
yaml.dump(spec_dict, open("spec.yml", "w"))
```

### 3b. Batch extraction script

**File**: `scripts/extract_s7_specs.py`

- Walks all `monolithic/` directories under `docs/modules/orcaflex/`
- For each `.yml` file: extract → validate → save `spec.yml`
- For multi-step models (installation phases): extract from the FIRST step as base spec, note that subsequent steps share the same line types/vessel types but differ in positions and winch payouts
- Log extraction results: success/failure, any schema validation warnings

### 3c. Multi-step installation models — spec strategy

Installation models (umbilical, pipeline) have 10-25 sequential steps. Strategy:

| Approach | Description | Effort |
|----------|-------------|--------|
| **One spec per step** | Each step gets its own spec.yml | Low effort, high duplication |
| **Base spec + overrides** | Extract base from Step01, delta for each subsequent step | Medium effort, clean |
| **Single spec + campaign** | One spec with campaign-style variation parameters | Aligns with existing campaign generator |

**Recommended**: **Base spec + overrides** — extract spec from Step01, document what changes between steps (primarily: winch payout, line position, statics initial conditions). This matches the existing `campaign_generator` pattern.

## Step 4: Validate Round-Trip (spec → modular → compare)

### 4a. Run 3-way benchmark on new models

Extend `scripts/benchmark_model_library.py` to include the new model groups:

```bash
# Path A: Load monolithic .yml directly in OrcaFlex → run statics
# Path B: spec.yml → modular generation → load in OrcaFlex → run statics
# Path C: spec.yml → modular generation → direct model build → run statics
# Compare: tension & bending moment at key arc lengths
```

### 4b. Expected issues and fixes

Based on the 51-model benchmark experience:

| Issue | Likely models | Fix |
|-------|--------------|-----|
| External DLL references | Jumper DZ models (ExternalFunction64.dll for AHC) | Add to skip list or mock |
| Dormant wind properties | Any model with wind | Already fixed (K02 pattern) |
| RayleighDamping as list | Installation models | Already fixed (K03 pattern) |
| Vessel RAO import | Models with detailed vessel types | Verify RAO data round-trips |
| 3D seabed data | Mooring (feeding_mooring_line) | Verify seabed point arrays |
| Large section counts | Jumper (25 sections), pipeline (50+) | Verify segment counts match |

### 4c. Acceptance criteria

- **Tension**: < 1% relative difference OR < 0.1 kN absolute
- **Bending moment**: < 5% relative difference OR < 0.01 kN.m absolute
- **Statics convergence**: Both paths must converge
- **Semantic validation**: 0 significant diffs (cosmetic diffs OK)

## Step 5: Run Legal Sanity Scan

1. Create `digitalmodel/.legal-deny-list.yaml`:
   ```yaml
   version: "1.0"
   patterns:
     client_references:
       - pattern: "Ballymore"
         severity: block
       - pattern: "Candies"
         severity: block
       - pattern: "WyatteCandies"
         severity: block
       - pattern: "Seven Arctic"
         severity: block
       - pattern: "Shell Perdido"
         severity: block
       - pattern: "Perdido South"
         severity: block
       - pattern: "Talos Venice"
         severity: block
       - pattern: "ONGC"
         severity: block
       - pattern: "Limerock"
         severity: block
       - pattern: "BP_MD2"
         severity: block
       - pattern: "SS7A2365"
         severity: block
       - pattern: "USNGL23LR3"
         severity: block
     exclusions:
       - "*.md"             # Documentation may reference generically
       - ".legal-deny-list.yaml"
   ```
2. Run: `bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel`
3. Fix any remaining violations
4. Verify exit code 0

## Step 6: Create Jumper Modelling Skill

**File**: `.claude/skills/eng/orcaflex-jumper-analysis.md`

### Concepts to encode (from Manifold-to-PLET and PLET-to-PLEM extraction)

1. **Multi-section pipe** — 25 sections with alternating line types (bare coated, buoyancy, strake)
2. **M-shape buoyancy** — Mid-span modules (10.16m blocks), density 0.694 te/m3
3. **Rigid end connectors** — OCS 200-V (1.8m OD), separate lines, EndBxBendingStiffness=1e+307
4. **Installation rigging chain** — Vessel → Crane Pedestal (6DBuoy) → CraneBoomBase (Constraint) → Crane wire (Winch) → Sling → Masterlink (3DBuoy) → Slings → Spreader Bar (6DBuoy) → Lift Slings + Turnbuckles → Clamps
5. **Three-point lift** — 120ft spreader bar, asymmetric pickup at 3 arc lengths
6. **AHC system** — Winch + ExternalFunction64.dll; DZ/SZ on/off variants
7. **Dual-zone analysis** — SZ (shallow, JONSWAP, Hs=1.5m) vs DZ (near-seabed, Dean stream, Hs=2m)
8. **Coatings** — Insulation (0.979 te/m3, 76.2mm) + Buoyancy (0.694 te/m3, 343mm) or Strakes (1.128 te/m3, 5mm)
9. **Clamp attachments** — 5× 10inchJumperClamp (0.26 te) at sling points
10. **Two-step statics** — Step1: User specified; Step2: Full statics
11. **Parametric studies** — Seeds (20+), headings (0/30/165°), Hs (0.75-2.5m)
12. **KIT weight check** — 4 KITs, ~46 te total, COG per KIT

### Skill should also reference

- spec.yml location: `docs/modules/orcaflex/jumper/*/spec.yml`
- Generation command: `uv run python -m digitalmodel.solvers.orcaflex.modular_generator --spec <spec.yml>`
- Validation: `uv run python scripts/semantic_validate.py --mono <monolithic.yml> --modular <generated.yml>`

## Step 7: Hull Meshes & RAOs — Data Governance

Hull meshes belong to the **diffraction analysis** domain. The repository already has an established hull library:

**Existing hull mesh library**: `data/hull_library/panels/` (14 GDF meshes across 7 categories: barges, FPSO, LNGC, primitives, semi_subs, ships, spars)
**Catalog**: `data/hull_library/catalog/hull_panel_catalog.yaml` (23 entries with dimensions, panel counts, symmetry)
**Mesh resolution**: `DiffractionSpec.vessel.geometry.mesh_file` — relative to spec.yml dir, auto-resolved by `aqwa_backend.py`

Any hull shapes and RAOs found in s7 models will be:

1. **Sanitized** — vessel names, project codes, client identifiers replaced per Step 1 mapping
2. **Hull meshes** → Added to existing `data/hull_library/panels/<category>/` and registered in `hull_panel_catalog.yaml`
3. **RAO datasets** → Stored with sanitized generic vessel class names in `docs/modules/orcaflex/vessel_raos/`
4. **Cross-referenced** — linked from model spec.yml files via relative paths (DiffractionSpec convention)

**Known RAO/Hull sources in s7:**
- `ballymore/WyattCandies-Case1-2-seastateRAOs/` → 2 RAO cases (sanitize vessel name)
- `shell_perdido_south/anl/manifold/*/seastateRAOs/` → manifold RAO filters
- `analysis_general/train/hulls/NeptuneBatchTestCase.dat` → hull test case
- Vessel type definitions embedded in .dat files (RAO tables) → extract during conversion

**External data dependencies** registered in `config/data_sources.yaml` per project convention.

## Step 8: Future Enhancement — Dedicated Jumper Schema (not in this plan)

A richer jumper-specific schema could replace the generic approach. Documented for future work:

- `schema/jumper.py` — JumperSpec, JumperLine, JumperEnd, JumperRigging Pydantic models
- `schema/root.py` — Add `jumper: JumperSpec | None` to ProjectInputSpec
- `builders/jumper_*.py` — Dedicated builders with `should_generate()` → `spec.is_jumper()`
- Engineering-intent specs (pipe dimensions, buoyancy layout, rigging chain) vs raw OrcaFlex properties

---

## Verification Checklist

1. **Sanitization**: `grep -ri "ballymore\|candies\|perdido\|talos\|ongc\|limerock\|SS7A2365" docs/modules/orcaflex/` → 0 matches
2. **Legal scan**: `bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel` → exit code 0
3. **File count**: ~220 sanitized `.yml` models across target directories
4. **spec.yml count**: One `spec.yml` per model group (est. 15-20 spec files)
5. **Schema validation**: All spec.yml files pass `ProjectInputSpec(**spec_dict)` without errors
6. **Round-trip**: Run benchmark on at least jumper + mooring models → tension diff < 1%
7. **Skill**: `.claude/skills/eng/orcaflex-jumper-analysis.md` exists and loads via `/orcaflex-jumper-analysis`
8. **Model integrity**: Spot-check 5 converted `.yml` files: `uv run python -c "import OrcFxAPI; m = OrcFxAPI.Model('path.yml'); print(m.general.WaterDepth)"`

## Critical Files

| File | Action |
|------|--------|
| `scripts/sanitize_s7_models.py` | CREATE — sanitization + conversion + organization |
| `scripts/extract_s7_specs.py` | CREATE — batch spec.yml extraction from monolithic models |
| `docs/modules/orcaflex/jumper/*/monolithic/` | CREATE — sanitized jumper models |
| `docs/modules/orcaflex/jumper/*/spec.yml` | CREATE — jumper model specs |
| `docs/modules/orcaflex/installation/**/monolithic/` | CREATE — sanitized installation models |
| `docs/modules/orcaflex/installation/**/spec.yml` | CREATE — installation specs |
| `docs/modules/orcaflex/mooring/reference/spec.yml` | CREATE — mooring model specs |
| `docs/modules/orcaflex/training/*/spec.yml` | CREATE — training model specs |
| `.claude/skills/eng/orcaflex-jumper-analysis.md` | CREATE — jumper modelling skill |
| `digitalmodel/.legal-deny-list.yaml` | CREATE — deny list for legal scan |
| `src/.../modular_generator/extractor.py` | REUSE — existing MonolithicExtractor |
| `src/.../modular_generator/builders/generic_builder.py` | REUSE — existing GenericModelBuilder |
| `scripts/benchmark_model_library.py` | EXTEND — add new model groups to benchmark |

## Execution Order

```
Step 1 → Step 2 → Step 5 (legal scan) → Step 3 → Step 4 → Step 6 → Step 7
         ↑ sanitize    ↑ gate             ↑ extract  ↑ validate  ↑ skill  ↑ RAOs/hulls
```

Steps 1-2 (sanitize) must pass Step 5 (legal scan) before Step 3 (spec extraction) proceeds. This ensures no client data leaks into spec.yml files.

## Work Queue Coordination

### WRK-121 (this item) — CREATED
- `D:\workspace-hub\.claude\work-queue\pending\WRK-121.md`
- Links to this plan via `spec_ref: specs/modules/mossy-inventing-penguin.md`

### Status Corrections Required

| Item | Current Status | Actual Status | Evidence |
|------|---------------|---------------|----------|
| WRK-100 | pending | **DONE** — archive | Barge benchmark r4 complete: all 6 DOFs positive phase, heave ~1.000. Report at `benchmark_output/barge_benchmark/r4/` |
| WRK-099 | pending | **BLOCKED** — add note | Unit Box benchmark attempted but AQWA mesh input error prevented completion. Not a blocker for WRK-100 (barge done independently) |
| WRK-031 | pending (Phase 3) | **PARTIALLY DONE** — update progress | Phase 3 progress: Barge r4 DONE, Ship r1 DONE, Spar r1 DONE, Unit Box SKIPPED (AQWA error). Update with results summary |
| WRK-033 | archived (in pending/) | **MOVE** to `archive/2026-02/` | Status says archived, but file is still in `pending/` directory |

### Consolidation Recommendations

**No merges recommended** — all pending items have distinct scopes. However:

1. **WRK-100 → archive**: Barge benchmark is complete. Remove `blocked_by: [WRK-099]` since it was completed independently.
2. **WRK-099**: Update notes with AQWA mesh error finding. May need different mesh format or AQWA version fix before retry.
3. **WRK-031**: Update Phase 3 progress — 3 of 4 hulls benchmarked (barge, ship, spar). Only remaining: fix unit box or substitute another simple geometry.
4. **WRK-033**: Move from `pending/` to `archive/2026-02/` (already marked archived/superseded).
5. **Hull cluster (WRK-110, 115, 116, 117)**: Keep separate — they're sequential phases. But add shared parent ref to WRK-114 (archived but foundational).

### Dependency Updates Required

| Item | Field | Update |
|------|-------|--------|
| WRK-119 | `related` | Add `WRK-121` |
| WRK-119 | `blocked_by` | Add `WRK-121` — raw spec.yml from WRK-121 are input to template curation |
| WRK-045 | `related` | Add `WRK-121` — jumper skill + reference models accelerate jumper analysis |
| WRK-118 | `related` | Add `WRK-121` — s7 models expand benchmark set |
| WRK-120 | `related` | Add `WRK-121` — concrete property routing examples from jumper/installation/mooring |
| WRK-032 | `related` | Add `WRK-121` — s7 installation models validate pipeline installation framework |
| WRK-103 | `related` | Add `WRK-121` — sanitized vessel data (Candies→iv01, Seven Arctic→iv02) seeds fleet |
| WRK-036 | `related` | Add `WRK-121` — Phase 0 (review s7 examples) directly enabled by WRK-121 extraction |

### state.yaml Update
- Bump `last_id: 120` → `last_id: 121`
- Increment `total_captured: 106` → `total_captured: 107`

### INDEX.md
- Regenerate to include WRK-121

---

## OrcaFlex Work Routing Plan

### Execution Tiers (all 27 OrcaFlex-related pending items)

**Tier 1: Foundation Hardening** — complete core infrastructure
1. **WRK-032** — Modular pipeline installation Phase 5 (stinger rollers, campaign tests, CLI)
2. **WRK-064** — Format converter license validation (completes .dat↔.yml toolchain)
3. **WRK-051** — Test coverage 2.7%→80% (safety net for all subsequent work)

**Tier 2: Model Library Expansion** — extract and catalog real-world models
4. **WRK-121** — Extract s7 models, sanitize, generate spec.yml, create jumper skill
5. **WRK-119** — Curate ideal spec.yml templates (blocked by WRK-121)

**Tier 3: Property Routing & Analysis Setup** — build analysis workflows
6. **WRK-120** — Map property routing: spec → OrcaFlex objects
7. **WRK-045** — Rigid jumper stress + VIV analysis (uses WRK-121 models)
8. **WRK-046** — Drilling/completion riser parametric analysis
9. **WRK-095** — Engineering unit tracking (cross-cutting, can parallel with Tier 2-3)

**Tier 4: Benchmarking & Validation** — validate across domains
10. **WRK-118** — All models time/frequency domain benchmark with seed equivalence
11. **WRK-031** — OrcaWave vs AQWA 3-way benchmark (Phase 3: fix unit box or substitute)
12. **WRK-099** — Unit Box benchmark (needs AQWA mesh fix)
13. ~~WRK-100~~ — Barge benchmark **DONE → archive**

**Tier 5: Hull Library & Mesh** — expand geometry capabilities
14. **WRK-106** — Hull panel generator from line definitions
15. **WRK-110** — Expand hull library (FST, LNGC, benchmark shapes)
16. **WRK-115** — Link RAO data to hull catalog entries
17. **WRK-116** — Scale hull meshes to target dimensions
18. **WRK-117** — Mesh refinement/coarsening for convergence studies
19. **WRK-101** — Mesh decimation utility (QEM algorithm)
20. **WRK-102** — Generic hull definitions for rigs (worldenergydata)

**Tier 6: Advanced Analysis & Cross-Validation** — longer-term
21. **WRK-044** — Pipeline wall thickness (DNV-ST-F101)
22. **WRK-036** — Structure deployment analysis (Phase 0 enabled by WRK-121)
23. **WRK-039** — SPM benchmarking AQWA vs OrcaFlex
24. **WRK-043** — Parametric hull form analysis with RAO generation

**Data & External** — largely independent, can run anytime
25. **WRK-103** — Construction vessel data (worldenergydata)
26. **WRK-105** — Drilling riser component data (worldenergydata)
27. **WRK-037** — OrcaFlex licensing agreement (admin/procurement)

**Parked** — blocked or low priority
- **WRK-075** — OFFPIPE integration (BLOCKED: waiting on user docs)
- **WRK-047** — OpenFOAM CFD (not installed)
- **WRK-048** — Blender configs (not installed)
- **WRK-050** — Hardware consolidation Phase 4
- **WRK-041** — Hobbies repo (non-engineering)

### Critical Dependency Chain

```
WRK-032 (hardening) ─┐
WRK-064 (converter) ──┤
                       ├── WRK-121 (s7 extract) ──┬── WRK-119 (spec templates)
WRK-051 (tests) ──────┘                           ├── WRK-120 (property routing)
                                                   ├── WRK-045 (jumper analysis)
                                                   ├── WRK-118 (full benchmark)
                                                   └── WRK-036 (deployment Phase 0)
```

### Completed Foundation (18 archived items providing infrastructure)

| Layer | Items | Reusable Infrastructure |
|-------|-------|------------------------|
| Spec Converter | WRK-026 family (057-063) | `DiffractionSpec`, AQWA/OrcaWave backends, mesh converter, reverse parsers, CLI |
| Solver Runners | WRK-025, 027, 028, 030 | `AQWARunner`, `AQWABatchRunner`, `OrcaWaveConverter`, `RAOPlotter` |
| Modular Generator | WRK-065 | S-lay builders, dual-mode architecture, 99/99 tests |
| Hull Catalog | WRK-114 | `data/hull_library/panels/` (14 meshes), `hull_panel_catalog.yaml` (23 entries) |
| Diffraction Benchmarks | (sessions) | Barge r4, Ship r1, Spar r1 — reports and data in `benchmark_output/` |

### Post-Approval Actions

1. Update 8 work items (WRK-119, 045, 118, 120, 032, 103, 036 + status corrections)
2. Archive WRK-100 (barge benchmark done)
3. Move WRK-033 from `pending/` to `archive/2026-02/`
4. Update WRK-031, WRK-099 progress notes
5. Update `state.yaml` (last_id: 121)
6. Create routing document at `specs/modules/orcaflex-work-routing.md`
7. Regenerate INDEX.md
