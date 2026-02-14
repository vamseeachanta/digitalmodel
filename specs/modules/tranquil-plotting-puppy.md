---
title: "WRK-126 Phase 0: Expand Template Library — One Per Structure Category"
description: "Create curated spec.yml templates for CALM buoy, jumper, wind turbine, drilling riser + complete riser set, with statics benchmarking and 3-agent cross-review at every phase"
version: "2.0"
module: orcaflex/templates
session:
  id: "20260213_wrk126_templates"
  agent: "claude-opus-4-6"
review: pending
---

# WRK-126 Phase 0: Expand Template Library

## Context

WRK-127 established 6 curated templates (all 100/100 quality). WRK-126 needs end-to-end benchmarking across structure types, but most non-riser categories have only massive raw spec.yml files (12k-50k lines) that are unsuitable as templates. Before benchmarking, we need clean, validated templates for each structure category.

**Current templates** (6 in `docs/modules/orcaflex/library/templates/`):

| Template | Lines | Quality | Benchmarked |
|----------|-------|---------|-------------|
| riser_catenary | 108 | 100/100 | Yes |
| riser_lazy_wave | 188 | 100/100 | Yes |
| pipeline_installation | 101 | 95/100 | Yes |
| mooring_buoy | 763 | 85/100 | No |
| installation_subsea | 196 | 90/100 | No |
| installation_pull_in | 645 | 80/100 | No |

**Gaps** — no templates for:
- CALM buoy (C06 raw spec is 12,167 lines)
- Jumper (S7 spec is 23 lines — too minimal)
- Wind turbine (K02 raw spec is 50,276 lines)
- Drilling riser (B01 raw spec is 12,278 lines)
- Riser steep wave / pliant wave (tier2_fast specs are 95/100, just need curation)

---

## Standard Phase Workflow (applies to ALL phases)

Every phase follows this mandatory sequence:

```
1. CREATE    → Write/curate spec.yml template
2. AUDIT     → Run quality audit (target 90+/100)
3. BENCHMARK → Run statics convergence via benchmark_model_library.py
4. COMMIT    → git commit with descriptive message
5. REVIEW    → Cross-review: Claude + Codex + Gemini (3 agents minimum)
6. FIX       → Address any P1/P2 issues from review
7. PROCEED   → Only after review passes, move to next phase
```

### Benchmark Commands (reused every phase)
```bash
# Quality audit
uv run python scripts/audit_spec_library.py --path docs/modules/orcaflex/library/templates/<template_name>/

# Statics benchmark (generates modular model, loads in OrcFxAPI, runs CalculateStatics)
uv run python scripts/benchmark_model_library.py --models <template_name> --verbose

# Cross-review (mandatory after each commit)
scripts/review/cross-review.sh <commit-sha> all
```

### Benchmark Pass Criteria
- Quality score: 90+/100 (95+ for riser templates)
- Statics: model loads without error, CalculateStatics() converges
- For templates with monolithic reference: compare effective tension within 5% tolerance
- Cross-review: no P1 issues; P2 issues addressed before proceeding

---

## Gate 0: Plan Cross-Review (Before Phase 1)

**Trigger**: This plan document committed.

```bash
scripts/review/cross-review.sh <plan-commit-sha> all
```

| Reviewer | Method | Pass Criteria |
|----------|--------|---------------|
| Claude | Inline review by orchestrating agent | No P1 issues in plan |
| Codex | `codex review --commit <sha>` | No structural concerns |
| Gemini | `gemini --prompt` review | No scope/feasibility issues |

**Proceed to Phase 1 only after Gate 0 passes.**

---

## Phase 1: Complete Riser Set (~30 min)

### Goal
Curate steep wave and pliant wave riser templates from existing tier2_fast specs.

### Source → Template

| Source | Lines | Quality | Action |
|--------|-------|---------|--------|
| `tier2_fast/a01_steep_wave_riser/spec.yml` | 151 | 95/100 | Copy, add template header, verify comments |
| `tier2_fast/a01_pliant_wave_riser/spec.yml` | 182 | 95/100 | Copy, add template header, verify comments |

### Steps
1. Copy spec.yml to `templates/riser_steep_wave/spec.yml` and `templates/riser_pliant_wave/spec.yml`
2. Add template header comment block (matching existing templates)
3. Ensure metadata has `structure: riser`, `operation: production`
4. Verify schema validation passes (`ProjectInputSpec`)
5. Update `templates/catalog.yaml` with new entries

### Benchmark
```bash
# Quality audit — target 95+/100
uv run python scripts/audit_spec_library.py --path docs/modules/orcaflex/library/templates/riser_steep_wave/
uv run python scripts/audit_spec_library.py --path docs/modules/orcaflex/library/templates/riser_pliant_wave/

# Statics convergence (already proven: steep=0.14s, pliant=0.24s)
uv run python scripts/benchmark_model_library.py --models riser_steep_wave --verbose
uv run python scripts/benchmark_model_library.py --models riser_pliant_wave --verbose
```

**Pass criteria**: Quality 95+/100, statics converge for both models.

### Commit & Cross-Review
```bash
git commit -m "feat(orcaflex): add riser steep wave and pliant wave templates"
scripts/review/cross-review.sh <sha> all
```

**Gate 1**: No P1 issues. Quality 95+/100. Statics converge. Catalog updated.

---

## Phase 2: CALM Buoy Template (~2-3 hrs)

### Goal
Hand-craft a clean CALM buoy template from the C06 discretised model. First seed-sensitive template (JONSWAP spectrum).

### Source Analysis
- Monolithic: `C06/C06 Discretised CALM buoy.dat` (302 KB)
- Raw spec: 12,167 lines — far too large
- Key components: CALM buoy (6D buoy), 6 mooring chains, 2 hoses, hawser, vessel

### Template Design (~200-300 lines target)
- Environment: JONSWAP wave, power-law current, constant wind
- Generic section: vessel_types, vessels, line_types, lines, buoys_6d
- Trim to essential properties only (skip cosmetic/dormant per OrcaFlex gotchas)

### Steps
1. Load monolithic C06 model with OrcFxAPI
2. Identify essential objects (buoy, mooring lines, vessel, hoses)
3. Extract only critical properties (skip cosmetic/dormant)
4. Write clean spec.yml with proper metadata and inline comments
5. Validate schema (`ProjectInputSpec`)

### Benchmark
```bash
# Quality audit — target 90+/100
uv run python scripts/audit_spec_library.py --path docs/modules/orcaflex/library/templates/calm_buoy_moored/

# Statics benchmark (generate modular → load → CalculateStatics)
uv run python scripts/benchmark_model_library.py --models calm_buoy_moored --verbose

# Compare against monolithic reference (effective tension within 5%)
```

**Pass criteria**: Quality 90+/100. Statics converge. Tension within 5% of monolithic.

### Commit & Cross-Review
```bash
git commit -m "feat(orcaflex): add CALM buoy moored template from C06"
scripts/review/cross-review.sh <sha> all
```

**Gate 2**: No P1 issues. Quality 90+/100. Statics converge. Tension benchmark passes.

---

## Phase 3: Jumper Template (~2-3 hrs)

### Goal
Create a subsea jumper template from S7 SUT_MM models.

### Source Analysis
- S7 monolithic: `docs/modules/orcaflex/jumper/sut_mm/monolithic/` (49 models)
- Raw spec: 23 lines — too minimal
- Need to load one representative monolithic model, extract jumper-specific properties

### Template Design (~150-250 lines target)
Key components: rigid/flexible jumper line, end connections (PLET/manifold), buoyancy modules, steel pipe line types, seabed interaction.

### Steps
1. Load first SUT_MM monolithic model with OrcFxAPI
2. Inventory all objects (1-3 lines, 2 shapes/constraints, 1-2 line types)
3. Extract essential properties, skip dormant/cosmetic
4. Write clean spec.yml with jumper-specific metadata
5. Validate schema (`ProjectInputSpec`)

### Benchmark
```bash
# Quality audit — target 90+/100
uv run python scripts/audit_spec_library.py --path docs/modules/orcaflex/library/templates/jumper_rigid_subsea/

# Statics benchmark
uv run python scripts/benchmark_model_library.py --models jumper_rigid_subsea --verbose

# Compare against monolithic reference (effective tension within 5%)
```

**Pass criteria**: Quality 90+/100. Statics converge. Tension within 5% of monolithic.

### Commit & Cross-Review
```bash
git commit -m "feat(orcaflex): add rigid subsea jumper template from S7"
scripts/review/cross-review.sh <sha> all
```

**Gate 3**: No P1 issues. Quality 90+/100. Statics converge.

---

## Phase 4: Drilling Riser Template (~2-3 hrs)

### Goal
Create a drilling riser template from B01 model.

### Source Analysis
- Monolithic: `docs/modules/orcaflex/examples/raw/B01/B01 Drilling riser.dat`
- Raw spec: 12,278 lines — too large

### Template Design (~200-300 lines target)
Key components: riser string (drill pipe, heavy weight, BHA), drillship with heave compensation, tensioner system, BOP/wellhead at seabed.

### Steps
1. Load monolithic B01 model with OrcFxAPI
2. Identify essential objects (riser string, vessel, tensioner, BOP)
3. Extract critical properties, skip dormant/cosmetic
4. Write clean spec.yml with drilling-specific metadata
5. Validate schema (`ProjectInputSpec`)

### Benchmark
```bash
# Quality audit — target 90+/100
uv run python scripts/audit_spec_library.py --path docs/modules/orcaflex/library/templates/drilling_riser/

# Statics benchmark
uv run python scripts/benchmark_model_library.py --models drilling_riser --verbose

# Compare against monolithic reference (effective tension within 5%)
```

**Pass criteria**: Quality 90+/100. Statics converge. Tension within 5% of monolithic.

### Commit & Cross-Review
```bash
git commit -m "feat(orcaflex): add drilling riser template from B01"
scripts/review/cross-review.sh <sha> all
```

**Gate 4**: No P1 issues. Quality 90+/100. Statics converge.

---

## Phase 5: Wind Turbine Template (~2-3 hrs)

### Goal
Create a fixed-bottom wind turbine template from K02 model.

### Source Analysis
- K02: `docs/modules/orcaflex/examples/raw/K02/` — 10MW fixed-bottom OWT (50,276 lines raw spec)
- K02 has external .bts wind file dependency

### Template Design (~200-400 lines target)
Key components: turbine object, support structure (monopile), wind loading, blade/RNA properties. Use steady wind to avoid external file dependencies, with comments showing how to switch to turbulent field.

### Steps
1. Load monolithic K02 model with OrcFxAPI
2. Identify essential objects (turbine, monopile, foundation)
3. Replace turbulent wind with steady wind (avoid .bts dependency)
4. Extract critical properties, skip dormant/cosmetic
5. Write clean spec.yml with wind-turbine metadata
6. Validate schema (`ProjectInputSpec`)

### Risk
K02 depends on external .bts wind files. Template uses steady wind to avoid external dependencies. Comments document how to switch to turbulent field.

### Benchmark
```bash
# Quality audit — target 90+/100
uv run python scripts/audit_spec_library.py --path docs/modules/orcaflex/library/templates/wind_turbine_fixed/

# Statics benchmark
uv run python scripts/benchmark_model_library.py --models wind_turbine_fixed --verbose
```

**Pass criteria**: Quality 90+/100. Statics converge (with steady wind, no external deps).

### Commit & Cross-Review
```bash
git commit -m "feat(orcaflex): add fixed-bottom wind turbine template from K02"
scripts/review/cross-review.sh <sha> all
```

**Gate 5**: No P1 issues. Quality 90+/100. Statics converge.

---

## Execution Order

```
Gate 0: Plan Cross-Review (Claude + Codex + Gemini)
    │
    v
Phase 1: Riser steep + pliant ──── Create → Audit → Benchmark → Commit → Cross-Review
    │
    v  Gate 1 pass?
Phase 2: CALM buoy ──── Create → Audit → Benchmark → Commit → Cross-Review
    │
    v  Gate 2 pass?
Phase 3: Jumper ──── Create → Audit → Benchmark → Commit → Cross-Review
    │
    v  Gate 3 pass?
Phase 4: Drilling riser ──── Create → Audit → Benchmark → Commit → Cross-Review
    │
    v  Gate 4 pass?
Phase 5: Wind turbine ──── Create → Audit → Benchmark → Commit → Cross-Review
    │
    v  Gate 5 pass?
Template library: 12 templates across 6+ categories
Then → WRK-126 (end-to-end dynamics benchmarking)
```

---

## Scope Summary

| Phase | Template | Target Lines | Quality Target | Benchmark |
|-------|----------|-------------|----------------|-----------|
| 1: Riser steep + pliant | 2 templates | 151 + 182 | 95+/100 | Statics converge |
| 2: CALM buoy | 1 template | ~200-300 | 90+/100 | Statics + monolithic comparison |
| 3: Jumper | 1 template | ~150-250 | 90+/100 | Statics + monolithic comparison |
| 4: Drilling riser | 1 template | ~200-300 | 90+/100 | Statics + monolithic comparison |
| 5: Wind turbine | 1 template | ~200-400 | 90+/100 | Statics (steady wind) |
| **Total** | **6 new templates** | **~1100-1400** | | **5 benchmark gates** |

Post-expansion: **12 templates** across 6+ categories (riser x4, pipeline x1, mooring x2, installation x2, jumper x1, drilling x1, wind x1).

---

## Cross-Review Summary (6 Mandatory Gates)

| Gate | Trigger | Reviewers | Pass Criteria |
|------|---------|-----------|---------------|
| **Gate 0** | Plan committed | Claude + Codex + Gemini | No P1 issues in plan |
| **Gate 1** | Phase 1 committed | Claude + Codex + Gemini | Quality 95+, statics converge, catalog updated |
| **Gate 2** | Phase 2 committed | Claude + Codex + Gemini | Quality 90+, statics converge, tension <5% diff |
| **Gate 3** | Phase 3 committed | Claude + Codex + Gemini | Quality 90+, statics converge |
| **Gate 4** | Phase 4 committed | Claude + Codex + Gemini | Quality 90+, statics converge |
| **Gate 5** | Phase 5 committed | Claude + Codex + Gemini | Quality 90+, statics converge |

```bash
# Run at each gate:
scripts/review/cross-review.sh <commit-sha> all
```

---

## Critical Files

### Create
- `docs/modules/orcaflex/library/templates/riser_steep_wave/spec.yml`
- `docs/modules/orcaflex/library/templates/riser_pliant_wave/spec.yml`
- `docs/modules/orcaflex/library/templates/calm_buoy_moored/spec.yml`
- `docs/modules/orcaflex/library/templates/jumper_rigid_subsea/spec.yml`
- `docs/modules/orcaflex/library/templates/drilling_riser/spec.yml`
- `docs/modules/orcaflex/library/templates/wind_turbine_fixed/spec.yml`

### Modify
- `docs/modules/orcaflex/library/templates/catalog.yaml` — add 6 new entries

### Reuse (existing scripts, no new code)
- `scripts/audit_spec_library.py` — quality scoring
- `scripts/benchmark_model_library.py` — statics validation + monolithic comparison
- `scripts/review/cross-review.sh` — 3-agent cross-review
- Existing template structure in `templates/riser_catenary/spec.yml` as formatting reference

### Source Models (read-only)
- `docs/modules/orcaflex/library/tier2_fast/a01_steep_wave_riser/spec.yml` (Phase 1)
- `docs/modules/orcaflex/library/tier2_fast/a01_pliant_wave_riser/spec.yml` (Phase 1)
- `docs/modules/orcaflex/examples/raw/C06/C06 Discretised CALM buoy.dat` (Phase 2)
- `docs/modules/orcaflex/jumper/sut_mm/monolithic/` (Phase 3)
- `docs/modules/orcaflex/examples/raw/B01/B01 Drilling riser.dat` (Phase 4)
- `docs/modules/orcaflex/examples/raw/K02/` (Phase 5)

---

## Quality Criteria (from WRK-127)

| Criterion | Points | Required |
|-----------|--------|----------|
| Schema valid (ProjectInputSpec) | 30 | Yes |
| Has comments (header + inline) | 20 | Yes |
| Meaningful name | 15 | Yes |
| Meaningful description | 10 | Yes |
| No raw_properties | 15 | Yes |
| < 1000 lines | 10 | Yes |
| **Target minimum** | **90/100** | |

---

## Risks

| Risk | Mitigation |
|------|-----------|
| CALM buoy extraction loses critical properties | Compare statics tension against monolithic reference |
| S7 jumper has external BaseFile refs | Resolve or inline before template creation |
| K02 wind turbine needs .bts file | Use steady wind in template, document turbulent option |
| B01 drilling riser has complex BOP | Simplify to essential components, note omissions |
| Templates too large (>1000 lines) | Aggressive trimming of cosmetic/dormant properties |
| Cross-review CLI produces no output | Note as NO_OUTPUT, proceed with remaining verdicts (min 2/3) |
