# OrcaFlex Work Routing Analysis

> **Generated**: 2026-02-11 | **Updated**: 2026-02-11
> **Agent**: Strategic Planner (Claude Opus 4.6)
> **Scope**: 23 OrcaFlex-specific pending + 5 archived items
> **Roadmap WRK**: WRK-125 (this document is the living artifact)

---

## Section 1: Executive Summary

| Metric | Count |
|--------|-------|
| Total OrcaFlex-specific items (pending) | 23 |
| Archived/completed items reviewed | 5 (WRK-033, WRK-097, WRK-100, WRK-114, WRK-122) |
| Items with approved plans | 14 |
| Items with inline plans (unapproved) | 4 (WRK-110, WRK-115, WRK-116, WRK-117) |
| Items needing new WRK numbers | 3 (benchmark models, spec templates, property routing — displaced from WRK-118/119/120) |
| Items explicitly blocked | 0 (no formal `blocked_by` entries remaining) |
| Items implicitly blocked | ~6 (depend on prerequisites not yet built) |
| Items in-progress | 1 (WRK-121) |
| High priority items | 3 (WRK-051, WRK-121, WRK-126) |
| Items targeting non-digitalmodel repos | 5 (WRK-037, WRK-095, WRK-102, WRK-103, WRK-105) |

### Displaced Items (user replaced WRK-118/119/120 with non-OrcaFlex items)

The following OrcaFlex items were originally captured as WRK-118/119/120 but those IDs were reassigned to different work. Re-captured as:

| New WRK | Original Title | Priority | Status |
|---------|---------------|----------|--------|
| WRK-126 | Benchmark all models time/freq domain + seed equivalence | high | pending |
| WRK-127 | Ideal spec.yml templates by structure type | high | pending (blocked by WRK-121) |
| WRK-128 | Property routing: spec → OrcaFlex objects | high | pending |

### Module Maturity Assessment

The OrcaFlex modular generator is the most mature subsystem:
- **22 builders** registered (pipeline + riser + generic)
- **53 model library entries** extracted from OrcaFlex examples
- **4 riser models** converging at benchmark (catenary, lazy wave, pliant wave, steep wave)
- **3-way benchmark pipeline** validated on 51 models (47/49 pass Path B+C)
- **Format converter** complete with 102 tests (YAML-only; license validation pending)
- **Hull panel library** established with 14 GDF meshes, 23 catalog entries, 7 categories
- **Diffraction benchmarks** done for barge (r4), ship (r1), spar (r1)

The pipeline installation campaign system (WRK-032) has Phases 0-4 complete with 209 tests, but Phase 5 hardening remains.

Areas with zero implementation: deployment analysis (WRK-036), rigid jumper (WRK-045), drilling riser (WRK-046), VIV module, AQWA solver module, parametric hull analysis pipeline.

---

## Section 2: Dependency Graph

```
Legend: A --> B means A blocks B (B cannot start until A completes)
        [DONE] = completed/archived
        [IP]   = in progress
        (**)   = not in OrcaFlex scope but impacts OrcaFlex items

FOUNDATION LAYER (no blockers)
================================
WRK-051  Test Coverage Improvement [HIGH]
WRK-119  Test Suite Optimization [HIGH]
WRK-037  OrcaFlex Framework Agreement [MEDIUM, admin]

EXTRACTION & CATALOGING
========================
WRK-121  Extract S7 Models [HIGH, IP]
   |
   +--> WRK-036  Deployment Analysis (needs s7 reference examples)
   +--> WRK-045  Rigid Jumper Analysis (needs s7 jumper models)
   +--> WRK-032  Pipeline Installation Phase 5 (needs s7 installation examples)

HULL & MESH INFRASTRUCTURE
============================
WRK-114  Hull Panel Collection [DONE]
   |
   +--> WRK-110  Expand Hull Library (FST, LNGC, benchmark shapes)
   +--> WRK-115  Link RAO Data to Hull Shapes
   +--> WRK-116  Scale Hull Panel Meshes
   +--> WRK-117  Mesh Refinement & Convergence
   |
WRK-101  Mesh Decimation/QEM [LOW]
   +--> WRK-117  (benefits from improved coarsening)
   |
WRK-106  Hull Panel Generator from Lines
   +--> WRK-110  (can use line generator for FST/LNGC meshes)
   +--> WRK-043  Parametric Hull Analysis (needs hull generation)

DIFFRACTION & BENCHMARKING
============================
WRK-031  3-Way Benchmark (Phase 3 mostly done: barge/ship/spar)
   |
   +--> WRK-099  Unit Box Benchmark (BLOCKED: AQWA mesh error)
   |
WRK-043  Parametric Hull Analysis [LOW]
   needs: WRK-106, WRK-110, WRK-116

SPM & MOORING
==============
WRK-039  SPM Benchmarking (AQWA vs OrcaFlex)
   needs: solvers/aqwa/ module (does not exist)

PIPELINE & INSTALLATION
=========================
WRK-032  Pipeline Installation Phase 5 (hardening)
WRK-044  Pipeline Wall Thickness (standalone, no blockers)

RISER ANALYSIS
===============
WRK-046  Drilling Riser (needs WRK-105 data, OrcFxAPI)
WRK-105  Drilling Riser Component Data (**worldenergydata**)

CONVERTER & TOOLING
====================
WRK-064  Format Converter License Validation (needs OrcFxAPI)

DATA DEPENDENCIES (worldenergydata)
=====================================
WRK-102  Hull Data for Rigs (**worldenergydata**)
WRK-103  Construction Vessel Data (**worldenergydata**)
WRK-105  Drilling Riser Data (**worldenergydata**)

RE-CAPTURED ORCAFLEX ITEMS (from displaced WRK-118/119/120)
=============================================================
WRK-126  Benchmark All Models Time/Freq + Seed Equiv [HIGH]
WRK-127  Ideal spec.yml Templates by Structure Type [HIGH]
   needs: WRK-121 (s7 extraction)
WRK-128  Property Routing: Spec → OrcaFlex Objects [HIGH]

CROSS-CUTTING / INFRASTRUCTURE (non-OrcaFlex)
================================================
WRK-095  Engineering Unit Tracking (**rock-oil-field**)
WRK-118  AI Agent Strategy (**workspace-hub**, not OrcaFlex)
WRK-119  Test Suite Optimization (**workspace-hub**, not OrcaFlex)
WRK-120  Local Data Strategy (**worldenergydata**, not OrcaFlex)
```

### Critical Path (longest dependency chain)

```
WRK-114 [DONE] --> WRK-106 --> WRK-043
                                  ^
                   WRK-110 -------+
                   WRK-116 -------+
```

The parametric hull analysis pipeline (WRK-043) is the furthest downstream item, requiring hull panel generation (WRK-106), expanded hull library (WRK-110), and mesh scaling (WRK-116) as prerequisites. This is a 4-item chain.

### Parallel Opportunities

The following groups are fully independent and can run concurrently:

- **Group A**: WRK-032 (pipeline hardening), WRK-044 (wall thickness), WRK-064 (converter validation)
- **Group B**: WRK-101 (mesh decimation), WRK-106 (hull generator), WRK-110 (hull library expansion)
- **Group C**: WRK-045 (rigid jumper), WRK-046 (drilling riser) -- both need OrcFxAPI but are independent of each other
- **Group D**: WRK-102, WRK-103, WRK-105 -- all worldenergydata items, fully independent of digitalmodel code

---

## Section 3: Current State Assessment

| WRK | Title | Declared Status | Actual State | Assessment |
|-----|-------|----------------|--------------|------------|
| WRK-031 | 3-Way Benchmark | pending | Phase 3 mostly done (barge r4, ship r1, spar r1). Unit box blocked by AQWA error. BEMRosetta not yet integrated. | STALE -- should be ~75% complete |
| WRK-032 | Pipeline Installation Modular | pending | Phases 0-4 complete (209 tests). Phase 5 hardening identified with 4 sub-phases. | STALE -- should be ~85% complete |
| WRK-036 | Structure Deployment | pending | Zero implementation. No deployment schema, no builders. Phase 0 (s7 review) not started. | ACCURATE |
| WRK-037 | OrcaFlex Agreement | pending | Admin/procurement task. No code deliverables. | ACCURATE |
| WRK-039 | SPM Benchmarking | pending | `solvers/aqwa/` module does not exist. Requires significant prerequisite work. | ACCURATE |
| WRK-043 | Parametric Hull Analysis | pending | Hull library exists (14 meshes, catalog). No parametric pipeline code yet. | ACCURATE |
| WRK-044 | Pipeline Wall Thickness | pending | No `structural/pipeline/wall_thickness/` directory exists. Zero implementation. | ACCURATE |
| WRK-045 | Rigid Jumper Analysis | pending | No `subsea/rigid_jumper/` directory. No VIV module. Zero implementation. | ACCURATE |
| WRK-046 | Drilling Riser | pending | Riser builders exist (riser_lines, riser_linetype, riser_vessel, riser_clumptype, riser_links) but for production risers, not drilling risers. Zero drilling-specific implementation. | ACCURATE |
| WRK-051 | Test Coverage | pending | Coverage at 2.7%. Fundamental infrastructure issue. | ACCURATE |
| WRK-064 | Format Converter Validation | pending | 102 tests pass (YAML-only). License-dependent validation pending. All converter code exists. | STALE -- should be ~80% complete |
| WRK-095 | Unit Tracking System | pending | `assetutilities/units/` has 687 lines, 11 files. Rock-oil-field integration layer not started. | ACCURATE (target repo is rock-oil-field) |
| WRK-099 | Unit Box Benchmark | pending | Attempted but AQWA mesh error blocked completion. Multiple spec attempts in benchmark_output/. | BLOCKED but status says pending |
| WRK-100 | Barge Benchmark | archived (in pending/) | Complete. Barge r4 done, all 6 DOFs positive phase. | STALE -- should be moved to archive |
| WRK-101 | Mesh Decimation | pending | `coarsen_mesh.py` (249 lines) exists with vertex clustering. No QEM implementation. | ACCURATE |
| WRK-102 | Hull Data for Rigs | pending | Target: worldenergydata. No hull data files created yet. | ACCURATE |
| WRK-103 | Construction Vessel Data | pending | Target: worldenergydata. No vessel data files created yet. | ACCURATE |
| WRK-105 | Drilling Riser Data | pending | Target: worldenergydata. No riser component data files created yet. | ACCURATE |
| WRK-106 | Hull Panel Generator | pending | No `line_generator/` directory exists. Zero implementation. | ACCURATE |
| WRK-110 | Hull Library Expansion | pending | Hull library has 14 GDF files in 7 categories + 23 catalog entries. FST/LNGC not yet added. | ACCURATE |
| WRK-115 | RAO-Hull Linking | pending | No RAO registry or linking infrastructure. Benchmark RAOs exist in benchmark_output/. | ACCURATE |
| WRK-116 | Hull Mesh Scaling | pending | No `mesh_scaler.py` exists. Zero implementation. | ACCURATE |
| WRK-117 | Mesh Refinement | pending | No `mesh_refiner.py` exists. Zero implementation. | ACCURATE |
| WRK-118 | AI Agent Strategy | pending | Cross-review process exists but no formalized strategy document. Target: workspace-hub. | ACCURATE |
| WRK-119 | Test Suite Optimization | pending | No tiered test profiles exist. Current test invocation uses ad-hoc PYTHONPATH. | ACCURATE |
| WRK-120 | Local Data Strategy | pending | Target: worldenergydata. BSEE .bin files issue from WRK-098. | ACCURATE |
| WRK-121 | Extract S7 Models | in_progress | Plan approved. Scripts and skills creation in progress. | ACCURATE |

---

## Section 4: Plan Quality Review

| WRK | Plan State | Phases | Has AC | Has Tests | Dependencies Correct | Rating |
|-----|-----------|--------|--------|-----------|---------------------|--------|
| WRK-031 | Inline, approved | 4 steps (Phase 3) | Yes (7 items) | Implicit | Yes | READY -- Phase 3 remaining work is clear |
| WRK-032 | spec_ref + inline (Phase 5) | 5A-5D sub-phases | Yes (25 items) | Yes (~26 new) | Yes | READY -- Phase 5 hardening is well-scoped |
| WRK-036 | Inline, approved | 5 phases + Phase 0 | Yes (8 items) | Yes (~90 est.) | Missing implicit dep on WRK-121 Phase 0 | NEEDS_REVIEW -- Phase 0 depends on s7 extraction |
| WRK-037 | Inline, approved | 4 steps | Yes (2 items) | N/A (admin) | Yes | READY -- simple procurement task |
| WRK-039 | Inline, approved | 6 phases | Yes (6 items) | Phase 5 | Missing: `solvers/aqwa/` prerequisite is huge | NEEDS_REVIEW -- Phase 0 prerequisite is underestimated |
| WRK-043 | Inline, approved | 6 phases | Yes (8 items) | Yes (~71 est.) | Missing dep on WRK-106, WRK-110, WRK-116 | NEEDS_REVIEW -- upstream items not declared |
| WRK-044 | Inline, approved | 4 phases | Yes (9 items) | Yes | Yes | READY -- standalone, no blockers |
| WRK-045 | Inline, approved | 6 phases | Yes (9 items) | Phase 6 | Missing dep: VIV module prerequisite is real work | NEEDS_REVIEW -- VIV prerequisite not tracked as WRK item |
| WRK-046 | Inline, approved | 5 phases | Yes (10 items) | Phase 5 | Yes (relates WRK-105) | READY -- but Phase 2 may need deferral |
| WRK-051 | Inline, approved | 5 phases | Yes (7 items) | N/A | Yes | READY -- well-scoped with realistic timeline caveat |
| WRK-064 | Inline, approved | 6 steps | Yes (5 items) | Yes (existing 102) | Yes | READY -- needs licensed machine |
| WRK-095 | Inline, approved | 5 phases | Yes (9 items) | Phase 5 | Yes | READY -- but not digitalmodel scope |
| WRK-099 | Inline, approved | 6 steps | Yes (6 items) | Implicit | Yes | BLOCKED -- AQWA mesh input error unresolved |
| WRK-101 | Inline, approved | 6 phases | Yes (6 items) | Yes (7+ tests) | Yes | READY -- QEM is self-contained |
| WRK-102 | Inline, approved | 4 phases | Yes (6 items) | Phase 4 | Yes | READY -- worldenergydata scope |
| WRK-103 | Inline, approved | 4 phases | Yes (5 items) | Phase 4 | Yes | READY -- worldenergydata scope |
| WRK-105 | Inline, approved | 4 phases | Yes (8 items) | Phase 4 | Yes | READY -- worldenergydata scope |
| WRK-106 | Inline, approved | 5 phases | Yes (9 items) | Phase 5 | Yes | READY -- no external deps |
| WRK-110 | Inline, NOT reviewed | 4 phases | Yes (7 items) | Phase 4 implicit | Missing dep on WRK-106 for mesh generation | NEEDS_REVIEW -- plan not cross-reviewed |
| WRK-115 | Inline, NOT reviewed | 4 phases | Yes (6 items) | Phase 4 implicit | Yes (references WRK-114 done) | NEEDS_REVIEW -- plan not cross-reviewed or approved |
| WRK-116 | Inline, NOT reviewed | 4 phases | Yes (8 items) | Phase 4 | References non-existent `diffraction_spec/` path | NEEDS_REVIEW -- target path incorrect |
| WRK-117 | Inline, NOT reviewed | 5 phases | Yes (8 items) | Phase 5 | References WRK-116 for patterns | NEEDS_REVIEW -- plan not cross-reviewed or approved |
| WRK-126 | Inline, NOT reviewed | 6 phases | Yes | Phase 5 | Yes | NEEDS_REVIEW -- plan exists, not cross-reviewed |
| WRK-127 | Inline, NOT reviewed | 5 phases | Yes | Phase 5 | blocked_by WRK-121 | NEEDS_REVIEW -- plan exists, blocked |
| WRK-128 | Inline, NOT reviewed | 5 phases | Yes | Phase 5 | Yes | NEEDS_REVIEW -- plan exists, not cross-reviewed |
| WRK-121 | spec_ref | Phases in spec | Yes (9 items) | Implicit | Yes | READY -- in progress |

### Key Plan Gaps

1. **WRK-039 (SPM Benchmarking)**: The Phase 0 prerequisite to create `solvers/aqwa/` is a significant undertaking that is not tracked as a separate WRK item. This should be a standalone item or the scope of WRK-039 should be explicitly constrained to OrcaFlex-only analysis initially.

2. **WRK-043 (Parametric Hull)**: Does not declare dependencies on WRK-106 (hull generator), WRK-110 (library expansion), or WRK-116 (mesh scaling), all of which are practical prerequisites.

3. **WRK-045 (Rigid Jumper)**: The VIV prerequisite (DNV-RP-F105 screening) is significant engineering work. Should be tracked as a separate WRK item.

4. **WRK-116 (Mesh Scaling)**: References `src/digitalmodel/solvers/diffraction_spec/mesh_scaler.py` as target path but `solvers/diffraction_spec/` does not exist. Should target `hydrodynamics/diffraction/` or `hydrodynamics/hull_library/`.

5. **WRK-110, WRK-115, WRK-116, WRK-117**: None of these have been cross-reviewed or user-approved. Their plans exist but have not been validated.

---

## Section 5: Recommended Execution Order

### Phase 0: Housekeeping (can run immediately, parallel)

| # | WRK | Action | Effort |
|---|-----|--------|--------|
| 0a | WRK-100 | Move from pending/ to archive/ -- work is complete | 5 min |
| 0b | WRK-031 | Update status to reflect ~75% complete, update percent_complete | 10 min |
| 0c | WRK-032 | Update status to reflect ~85% complete | 10 min |
| 0d | WRK-064 | Update status to reflect ~80% complete | 10 min |
| 0e | WRK-110/115/116/117 | Queue for cross-review (plans exist but not validated) | 2-3 hrs |

### Phase 1: Foundation & Active Work (1-2 weeks)

All items below can run in parallel.

| # | WRK | Title | Priority | Rationale |
|---|-----|-------|----------|-----------|
| 1a | **WRK-121** | Extract S7 Models | HIGH | Already in-progress. Unblocks WRK-036, WRK-045, WRK-032. |
| 1b | **WRK-051** | Test Coverage (Phase 1 only) | HIGH | Fix test infrastructure first. Foundation for all future work. |
| 1c | **WRK-119** | Test Suite Optimization | HIGH | Create plan, then implement Tier 1 (commit tests). Needs plan creation first. |
| 1d | **WRK-064** | Format Converter Validation | MEDIUM | Quick win on licensed machine. 102 tests already pass. |
| 1e | **WRK-037** | OrcaFlex Agreement | MEDIUM | Admin task, no code dependency. Can proceed independently. |

### Phase 2: Infrastructure & Hull Pipeline (2-3 weeks)

Start after Phase 1 items are at least underway.

| # | WRK | Title | Priority | Rationale |
|---|-----|-------|----------|-----------|
| 2a | **WRK-032** | Pipeline Installation Phase 5 | MEDIUM | Well-scoped hardening. 4 sub-phases, ~26 new tests. |
| 2b | **WRK-044** | Pipeline Wall Thickness | MEDIUM | Standalone. Pure Python, no license needed. Unlocked for immediate start. |
| 2c | **WRK-101** | Mesh Decimation (QEM) | LOW | Pure numpy. Enables WRK-117 (mesh convergence). |
| 2d | **WRK-106** | Hull Panel Generator | MEDIUM | Enables WRK-043 and WRK-110. Core infrastructure. |
| 2e | **WRK-116** | Hull Mesh Scaling | MEDIUM | After plan review/fix. Enables parametric pipeline. |

### Phase 3: Hull Library Expansion & Linking (2-3 weeks)

Depends on Phase 2d (WRK-106) and Phase 2e (WRK-116) for mesh generation/scaling.

| # | WRK | Title | Priority | Rationale |
|---|-----|-------|----------|-----------|
| 3a | **WRK-110** | Expand Hull Library (FST, LNGC) | MEDIUM | Uses hull generator from WRK-106 for new meshes. |
| 3b | **WRK-115** | Link RAO Data to Hull Shapes | MEDIUM | After plan review. Links benchmark results to catalog. |
| 3c | **WRK-117** | Mesh Refinement & Convergence | MEDIUM | After plan review. Uses WRK-101 QEM for coarsening. |

### Phase 4: Analysis Capabilities (3-4 weeks)

Depends on WRK-121 (S7 extraction) for reference examples. Can run items in parallel.

| # | WRK | Title | Priority | Rationale |
|---|-----|-------|----------|-----------|
| 4a | **WRK-045** | Rigid Jumper Analysis | MEDIUM | Needs OrcFxAPI. S7 jumper models provide reference. |
| 4b | **WRK-046** | Drilling Riser Analysis | MEDIUM | Needs OrcFxAPI. Start Phase 1 (drilling) only. |
| 4c | **WRK-036** | Structure Deployment | LOW | Needs OrcFxAPI + S7 reference examples. |

### Phase 5: Advanced Pipelines (4-6 weeks)

Depends on hull infrastructure (Phase 2-3) and analysis capabilities (Phase 4).

| # | WRK | Title | Priority | Rationale |
|---|-----|-------|----------|-----------|
| 5a | **WRK-043** | Parametric Hull Analysis | LOW | Needs WRK-106, WRK-110, WRK-116 complete. |
| 5b | **WRK-039** | SPM Benchmarking | MEDIUM | Needs `solvers/aqwa/` module. Significant prerequisite. |
| 5c | **WRK-099** | Unit Box Benchmark | MEDIUM | Blocked by AQWA mesh error. Investigate as side effort. |

### Phase 6: Data Collection (parallel, any time)

These are worldenergydata items. Can run independently at any phase.

| # | WRK | Title | Priority | Notes |
|---|-----|-------|----------|-------|
| 6a | **WRK-102** | Hull Data for Rigs | MEDIUM | Manual research, 60-70% data completeness |
| 6b | **WRK-103** | Construction Vessel Data | MEDIUM | Manual research, 30-40 hrs for 50+ vessels |
| 6c | **WRK-105** | Drilling Riser Data | MEDIUM | Feeds WRK-046 |

### Phase 7: Benchmarking & Validation (3-4 weeks)

| # | WRK | Title | Priority | Notes |
|---|-----|-------|----------|-------|
| 7a | **WRK-126** | Benchmark all models time/freq + seed equiv | HIGH | Needs OrcFxAPI. Extends existing 51-model benchmark. |
| 7b | **WRK-127** | Ideal spec.yml templates | HIGH | Blocked by WRK-121. Curate from all extracted specs. |
| 7c | **WRK-128** | Property routing: spec → OrcaFlex | HIGH | Core architecture. Enables engineering-intent specs. |

### Phase 8: Cross-Cutting (parallel, any time — non-OrcaFlex)

| # | WRK | Title | Priority | Notes |
|---|-----|-------|----------|-------|
| 8a | **WRK-095** | Unit Tracking System | HIGH | rock-oil-field scope |

---

## Section 6: Immediate Next Actions

### Action 1: Complete WRK-121 (Extract S7 Models) -- HIGHEST PRIORITY
- **Why**: Already in-progress. Blocks 3+ downstream items (WRK-036, WRK-045, WRK-032 Phase 0).
- **What to do**: Continue sanitization script, legal deny list, and model categorization. Run legal scan.
- **Agent**: Current session or dedicated subagent.

### Action 2: Archive WRK-100 (Barge Benchmark) -- QUICK WIN
- **Why**: Work is complete (barge r4 done). File is in pending/ but status says archived. Move to `archive/2026-02/WRK-100.md`.
- **What to do**: `mv .claude/work-queue/pending/WRK-100.md .claude/work-queue/archive/2026-02/`
- **Agent**: Any agent, 1-minute task.

### Action 3: Cross-review WRK-110, WRK-115, WRK-116, WRK-117 plans
- **Why**: These 4 items have inline plans but have NOT been cross-reviewed or user-approved. They form the hull infrastructure pipeline.
- **What to do**: Run `scripts/review/cross-review.sh` on each. Fix WRK-116 target path (should be `hydrodynamics/hull_library/mesh_scaler.py` or `hydrodynamics/diffraction/mesh_scaler.py`, not non-existent `solvers/diffraction_spec/`).
- **Agent**: Planner agent with Codex/Gemini cross-review.

### Action 4: Review plans for WRK-126, WRK-127, WRK-128 (re-captured OrcaFlex items)
- **Why**: These 3 HIGH priority OrcaFlex items were displaced from WRK-118/119/120 and re-captured with plans.
- **What to do**: Cross-review the inline plans. WRK-126 (benchmark all models) and WRK-128 (property routing) are high-impact.
- **Agent**: Planner agent.

### Action 5: Start WRK-044 (Pipeline Wall Thickness) -- QUICK WIN
- **Why**: Standalone, no external dependencies, pure Python, approved plan. Can be completed in 1-2 sessions.
- **What to do**: TDD implementation of DNV-ST-F101 calculations. No license required.
- **Agent**: Coder agent with TDD discipline.

---

## Section 7: Items Needing Plan Review

### Must Be Cross-Reviewed Before Execution

| WRK | Title | What Needs Review |
|-----|-------|-------------------|
| WRK-110 | Hull Library Expansion | Plan exists but `plan_reviewed: false`. Verify mesh generation approach (WRK-106 dependency), validate FST/LNGC dimension sources. |
| WRK-115 | RAO-Hull Linking | Plan exists but `plan_reviewed: false`, `plan_approved: false`. Verify schema extension approach, RAO data format, auto-linking hook design. |
| WRK-116 | Hull Mesh Scaling | Plan exists but `plan_reviewed: false`. **Critical**: Target path `solvers/diffraction_spec/mesh_scaler.py` does not exist. Must be corrected to valid location. Verify scaling algorithm handles mixed quad/tri meshes. |
| WRK-117 | Mesh Refinement | Plan exists but `plan_reviewed: false`. Verify quad subdivision algorithm, relationship with WRK-101 coarsening, convergence study script design. |

### Re-captured Items (plans carried over from original WRK-118/119/120)

| WRK | Title | Plan Status |
|-----|-------|-------------|
| WRK-126 | Benchmark all models time/freq + seed equiv | Inline plan with 6 phases. Needs cross-review. |
| WRK-127 | Ideal spec.yml templates by structure type | Inline plan with 5 phases. Blocked by WRK-121. |
| WRK-128 | Property routing: spec → OrcaFlex objects | Inline plan with 5 phases. Needs cross-review. |

### Plan Quality Issues (already approved but need attention)

| WRK | Issue |
|-----|-------|
| WRK-039 | Phase 0 prerequisite (`solvers/aqwa/` creation) is not tracked as a separate WRK item. The effort to create an AQWA solver module is comparable to the SPM benchmark itself. Recommend splitting into WRK-039A (AQWA module) and WRK-039B (SPM benchmark). |
| WRK-043 | Does not declare blocked_by on WRK-106, WRK-110, WRK-116. These are practical prerequisites without which Phase 1-2 cannot execute meaningfully. |
| WRK-045 | VIV prerequisite (DNV-RP-F105 screening module in `structural/viv/`) is called out in the plan but not tracked as a WRK item. This is 1-2 weeks of work on its own. |

---

## Section 8: Risk Register

### External Dependencies

| Risk | Affected Items | Impact | Mitigation |
|------|---------------|--------|------------|
| **OrcaFlex license availability** | WRK-064, WRK-045, WRK-046, WRK-036 | Cannot run end-to-end tests or generate analysis results | WRK-122 (completed) documented license workflow. Use `pytest.importorskip('OrcFxAPI')` for graceful test skipping. Schedule license-dependent work for specific machine access windows. |
| **ANSYS/AQWA license availability** | WRK-031 (Phase 3), WRK-039, WRK-099 | Cannot run AQWA solver. Blocks 3-way benchmark completion and SPM benchmarking. | WRK-122 identified ANSYS license is on ACMA-ANSYS05. AQWA v252 can process v18.1 inputs. Fallback: proceed with OrcaWave-only results. |
| **AQWA mesh input error** | WRK-099 (Unit Box) | Blocks unit box benchmark. Root cause: mesh format incompatibility. | Investigate `_MESH_FORMAT_MAP` in diffraction code. Try alternative mesh formats (DAT vs GDF). May need manual AQWA mesh creation. |
| **Missing external files** | WRK-031 (B01, K02 models) | 2 of 49 models fail: B01 needs PyModel plugin, K02 needs wind.bts. | These are known acceptable failures. Document and exclude from pass rate. |
| **`solvers/aqwa/` module does not exist** | WRK-039 | SPM benchmarking cannot start until AQWA input generation is built. | Create as separate WRK item (WRK-039A). Minimum viable: AQWA batch input (.dat) writer for coupled analysis. |
| **No VIV module** | WRK-045 (Phase 4) | Rigid jumper VIV assessment blocked. DNV-RP-F105 screening must be built from scratch. | Create separate WRK item for `structural/viv/` module. Time-box to screening-only (not full response model). |
| **Hull profile YAML files** | WRK-043, WRK-106 | No verified hull profile YAML files in `data/hull_profiles/`. WRK-043 plan assumes they exist. | Verify during Phase 1 of WRK-106. Create sample profiles from known hull forms (box barge, KCS ship). |
| **PyVista/VTK/GMSH not installed** | WRK-101 (Phase 2-3), WRK-117 | Optional backends for mesh operations unavailable. | Primary QEM implementation (Phase 1) uses pure numpy. VTK/GMSH backends are optional enhancements with skip-marked tests. |
| **worldenergydata data collection effort** | WRK-102, WRK-103, WRK-105 | Manual research (30-70+ hours total). Data completeness 60-70%. | Start with minimal sample datasets (3-5 entries) as proof of concept. Expand iteratively. |
| **rock-oil-field access for WRK-121** | WRK-121 | S7 models contain ~235 .dat files that must be accessed, sanitized, and categorized. | Plan already accounts for legal deny list and sanitization. Ensure repo access is available. |

### Technical Debt Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| **2.7% test coverage** (WRK-051) | All new features built on untested foundation. Refactoring is risky. | Prioritize Phase 1 (fix test infrastructure) before large feature work. |
| **Pre-existing test failures** | 31+ tests fail from pipeline builder changes (test_backward_compat, test_campaign, etc.) | Track in WRK-051. Fix incrementally, do not block new work. |
| **WRK-100 in wrong directory** | pending/ contains completed item, creating confusion. | Immediate action: move to archive/. |
| **4 unapproved plans** | WRK-110/115/116/117 could have design issues that cause rework. | Cross-review before starting implementation. |

---

## Appendix A: Item Classification by Target Repository

### digitalmodel (primary) — 20 items
WRK-031, WRK-032, WRK-036, WRK-043, WRK-044, WRK-045, WRK-046, WRK-051, WRK-064, WRK-099, WRK-101, WRK-106, WRK-110, WRK-115, WRK-116, WRK-117, WRK-121, WRK-125 (roadmap), WRK-126, WRK-127, WRK-128

### worldenergydata — 3 items
WRK-102, WRK-103, WRK-105

### rock-oil-field — 1 item
WRK-095

### aceengineer-admin — 1 item
WRK-037

### Non-OrcaFlex (excluded from this routing)
WRK-118 (AI agent strategy, workspace-hub), WRK-119 (test suite optimization, workspace-hub), WRK-120 (BSEE data strategy, worldenergydata)

## Appendix B: Complexity vs Priority Matrix

```
               LOW COMPLEXITY    MEDIUM COMPLEXITY    COMPLEX
HIGH PRIO    |                 | WRK-119, WRK-120  | WRK-051, WRK-121
             |                 |                    |
MEDIUM PRIO  | WRK-037         | WRK-064, WRK-101  | WRK-031, WRK-032
             |                 | WRK-102, WRK-103   | WRK-039, WRK-043
             |                 | WRK-105, WRK-110   | WRK-044, WRK-045
             |                 | WRK-115, WRK-116   | WRK-046, WRK-095
             |                 | WRK-117            | WRK-106, WRK-118
             |                 | WRK-099            |
LOW PRIO     |                 |                    | WRK-036
             |                 |                    | WRK-043
```

## Appendix C: Items with Significant Completed Work

These items have substantial completed work but their `status` field does not reflect progress:

| WRK | % Complete (est.) | Evidence |
|-----|-------------------|----------|
| WRK-031 | 75% | Barge r4, Ship r1, Spar r1 complete. Unit Box blocked. BEMRosetta pending. |
| WRK-032 | 85% | Phases 0-4 done (209 tests). Phase 5 hardening remains. |
| WRK-064 | 80% | 102 tests pass. License validation + backward-compat wrapper pending. |
| WRK-100 | 100% | Barge r4 complete. File should be in archive. |
| WRK-114 | 100% | Archived with commit 34d4ee469. 14 GDF files, 23 catalog entries. |

---

## Appendix D: Change Log

| Date | Change |
|------|--------|
| 2026-02-11 | Initial generation by planner agent |
| 2026-02-11 | WRK-118/119/120 replaced by user with non-OrcaFlex items. Original OrcaFlex content re-captured as WRK-126/127/128. |
| 2026-02-11 | Created WRK-125 as evolving roadmap tracking item |

---

*This is a living document tracked by WRK-125. Update when items change status, new dependencies are discovered, or work is completed.*
