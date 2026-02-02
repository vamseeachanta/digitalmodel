---
title: "Flatten and simplify digitalmodel module structure"
description: "Complete repo-wide restructuring: src/ flattening (done), then tests/, examples/, scripts/, and output directory cleanup."
version: "2.0"
module: "digitalmodel"

session:
  id: "velvety-wandering-bird"
  agent: "claude-opus-4.5"

review:
  required_iterations: 3
  current_iteration: 0
  status: "pending"
  reviewers:
    openai_codex:
      status: "pending"
      iteration: 0
      feedback: ""
    google_gemini:
      status: "pending"
      iteration: 0
      feedback: ""
  ready_for_next_step: false

status: "in-progress"
progress: 60
priority: "high"
complexity: "complex"
tags: [architecture, refactoring, discoverability, module-structure]

created: "2026-02-01"
updated: "2026-02-01"
target_completion: ""

links:
  spec: "specs/modules/velvety-wandering-bird.md"
  work_item: "WRK-066"
  branch: "refactor/wrk-066-flatten-repo-structure"
---

# Flatten and Simplify digitalmodel Module Structure

> **Module**: digitalmodel | **Status**: in-progress | **Work Item**: WRK-066

## Summary

Phases 0-6 (src/ flattening) are **complete** — merged to main via PR #129. All 47 modules moved from `digitalmodel.X` to `digitalmodel.X` with MetaPathFinder backward compat.

Phases 7-11 address the **rest of the repo**: tests, examples, scripts, and output directories still reference the old `modules/` layout.

---

## Remaining Problems

| Problem | Location | Count |
|---------|----------|-------|
| Tests mirror old `modules/` structure | `tests/modules/` | 47 subdirs, 337 files |
| Duplicate test directories | `tests/X/` AND `tests/modules/X/` | 13 sets |
| Unorganized root test files | `tests/*.py` | 46 files |
| Legacy `digitalmodel.*` imports in tests | 8 test `.py` files | 12 references |
| Legacy `digitalmodel.*` imports in scripts | 15 script files | 29+ references |
| Examples mirror old `modules/` structure | `examples/modules/` | 24 subdirs |
| Empty output directories | `results/` | 0 files |
| Massive coverage output | `htmlcov/` | 149 MB, 1735 files |

---

## Completed Phases (0-6) — src/ Flattening

| Phase | Status | Commit |
|-------|--------|--------|
| Phase 0: Safety Net | Done | `dbb1ff2a5` |
| Phase 1: Fix Naming | Done | `7b80d968d` |
| Phase 2: Remove Dead Code | Done | `da47916d3` |
| Phase 3: Consolidate Duplicates | Done | `8b51d1c9c` |
| Phase 4: Flatten modules/ (6 batches) | Done | `4734d5efb`..`924393cf6` |
| Phase 5: Absorb Top-Level Packages | Done | `52648a6cd` |
| Phase 6: Cleanup | Done | `c23008bb9` |

All squash-merged to main as commit `72187cd11`.

---

## Phase 7: Flatten tests/modules/ Directory

**Goal**: Move all test subdirs from `tests/modules/X/` to `tests/X/`, matching the flattened src layout.

### 7A — Resolve 13 Duplicate Test Directories

These directories exist at BOTH `tests/X/` and `tests/modules/X/`:

| Directory | Resolution |
|-----------|------------|
| `blender_automation` | Merge into `tests/blender_automation/` |
| `cli` | Merge into `tests/cli/` |
| `contracts` | Merge into `tests/contracts/` |
| `core` | Merge into `tests/core/` |
| `custom` | Merge into `tests/custom/` |
| `data` | Merge into `tests/data/` |
| `factories` | Merge into `tests/factories/` |
| `fatigue` | Merge into `tests/fatigue/` |
| `property` | Merge into `tests/property/` |
| `integration` | Merge into `tests/integration/` |
| `marine_engineering` | Merge into `tests/marine_engineering/` |
| `security` | Merge into `tests/security/` |
| `stress` | Merge into `tests/stress/` |

**Per duplicate**:
- [ ] Compare contents — identify unique vs overlapping test files
- [ ] Move unique files from `tests/modules/X/` to `tests/X/`
- [ ] For overlapping files, keep the more complete version
- [ ] Delete `tests/modules/X/` after merge
- [ ] Run tests to verify nothing broke

### 7B — Move Remaining 34 Non-Duplicate Directories

Move from `tests/modules/X/` to `tests/X/`:

```
analysis, aqwa, artificial_lift, automation, bemrosetta,
catenary_riser, cathodic_protection, code_dnvrph103, diffraction,
digitalmarketing, fea, fatigue_analysis, gis, gmsh_meshing,
hydrodynamics, installation, marine_analysis, mooring,
mooring_analysis, orcaflex, performance, pipe_cross_section,
pipeline, rao_analysis, security, ship_design, signal_analysis,
structural_analysis, time_series, umbilical_analysis, unit, utils,
validation, viv_analysis, workflow_automation
```

**Per batch** (commit after each batch):
- [ ] `git mv tests/modules/X/ tests/X/` (check for collisions first)
- [ ] Run `uv run pytest` to verify
- [ ] Commit

### 7C — Organize 46 Root Test Files

Move loose test files in `tests/` root into appropriate subdirectories:

| File Pattern | Move To |
|-------------|---------|
| `test_engine*.py`, `simple_engine_test.py`, `additional_engine_tests.py` | `tests/engine/` |
| `test_catenary*.py`, `test_lazy_wave*.py`, `solve_catenary*.py`, `check_catenary_geometry.py` | `tests/catenary/` |
| `test_fatigue*.py`, `test_complete_fatigue_pipeline.py`, `test_rainflow*.py` | `tests/fatigue/` |
| `test_wall_thickness*.py`, `test_plate_capacity.py` | `tests/structural_analysis/` |
| `test_orcaflex_agent.py` | `tests/orcaflex/` |
| `test_aqwa.py` | `tests/aqwa/` |
| `test_reporting.py` | `tests/reporting/` |
| `test_config*.py` | `tests/core/` |
| `test_workflow*.py` | `tests/workflow_automation/` |
| `test_restructure_compat.py` | `tests/compat/` (keep — tests backward compat) |
| `test_module_reorganization.py` | `tests/compat/` |
| `conftest.py`, `__init__.py` | Stay in `tests/` root |
| `coverage_analysis.py`, `establish_baseline.py`, `final_coverage_report.py` | `scripts/coverage/` (not tests) |
| `test_performance_baselines.py`, `test_solver_benchmarks.py` | `tests/benchmarks/` |
| `test_quality_metrics.py` | `tests/validation/` |
| `test_skill_resolver.py`, `test_agent_dashboard.py`, `test_memory_lifecycle.py` | `tests/core/` |
| `test_validation_utils.py` | `tests/validation/` |

### 7D — Delete tests/modules/

- [ ] After all moves, verify `tests/modules/` is empty
- [ ] `rm -rf tests/modules/`
- [ ] Update `pyproject.toml` testpaths if needed (currently `["tests"]` — should still work)
- [ ] Commit

---

## Phase 8: Fix Legacy Import References

**Goal**: Update all remaining `digitalmodel.*` references outside of the compat layer.

### 8A — Test Files (12 references in 8 files)

| File | Action |
|------|--------|
| `tests/test_restructure_compat.py` | **Keep** — this tests backward compat deliberately |
| `tests/test_engine_isolated.py` | Update imports to `digitalmodel.X` |
| `tests/test_engine.py` | Update imports to `digitalmodel.X` |
| `tests/simple_engine_test.py` | Update imports to `digitalmodel.X` |
| `tests/modules/integration/test_multi_module_interaction.py` | Update imports (moves in Phase 7) |
| `tests/coverage_analysis.py` | Update imports |
| `tests/additional_engine_tests.py` | Update imports |
| `tests/modules/analysis/*.md` | Update documentation references |

### 8B — Script Files (15 files with references)

| File(s) | Action |
|---------|--------|
| `scripts/import_map.json` | Regenerate with `scripts/build_import_map.py` |
| `scripts/build_import_map.py` | **Keep** — scans for old imports by design |
| `scripts/initialize_skill_metadata.py` | Update imports |
| `scripts/demo_skill_resolver.py` | Update imports |
| `scripts/agent_dashboard_cli.py` | Update imports |
| `scripts/python/digitalmodel/tools/process_fatigue_*.py` (8 files) | Update imports |
| `scripts/python/digitalmodel/tools/migrate_to_signal_analysis.py` | Update imports |

### 8C — Verification

- [ ] `grep -r "digitalmodel\.modules\." tests/ scripts/ examples/ --include="*.py"` returns only compat test + import map script
- [ ] Commit

---

## Phase 9: Flatten examples/modules/ Directory

**Goal**: Move all example subdirs from `examples/modules/X/` to `examples/X/`.

### 9A — Move 24 Example Directories

```
api_standards, artificial_lift, calm_buoy, core, diffraction,
fatigue, fea, fpso, hydrodynamics, input_files, metocean, mooring,
ocimf, orcaflex, reservoir, _shared, stress, structural_analysis,
tutorials, utilities, validation, vessel, workflows
```

- [ ] `git mv examples/modules/X/ examples/X/` for each
- [ ] Update any internal path references in example scripts
- [ ] Delete `examples/modules/` directory
- [ ] Update `examples/README.md` if it references `modules/` paths
- [ ] Commit

---

## Phase 10: Root-Level Cleanup

**Goal**: Clean up repo root directories and stale artifacts.

### 10A — Delete Empty/Stale Directories

| Directory | Action | Reason |
|-----------|--------|--------|
| `results/` | Delete | Empty — 0 files, only placeholder subdirs |
| `htmlcov/` | Ensure in .gitignore, delete local | 149 MB generated coverage output |
| `.benchmarks/` | Delete if empty | Stale benchmark artifacts |
| `coordination/` | Delete if empty | Unused coordination dir |
| `projects/` | Delete if empty | Unused projects dir |

### 10B — Consolidate Output Directories

Current state: `reports/` (171 files), `outputs/` (27 files), `results/` (empty).

- [ ] Keep `reports/` as canonical output directory (already in .gitignore)
- [ ] Move `outputs/` contents to `reports/outputs/` if needed, or keep separate
- [ ] Ensure all output dirs are in `.gitignore`
- [ ] Delete `results/`

### 10C — Move Non-Test Scripts Out of tests/

These files in `tests/` root are not tests:

| File | Move To |
|------|---------|
| `coverage_analysis.py` | `scripts/coverage/` |
| `establish_baseline.py` | `scripts/coverage/` |
| `final_coverage_report.py` | `scripts/coverage/` |

- [ ] `git mv` each file
- [ ] Commit

---

## Phase 11: Final Verification and Documentation

### 11A — Structural Verification

- [ ] `ls tests/` shows flat module directories (no `modules/` subdir)
- [ ] `ls examples/` shows flat module directories (no `modules/` subdir)
- [ ] `grep -r "digitalmodel\.modules\." . --include="*.py"` returns only:
  - `src/digitalmodel/modules/__init__.py` (compat shim)
  - `src/digitalmodel/_compat.py` (compat registry)
  - `tests/test_restructure_compat.py` (compat tests)
  - `scripts/build_import_map.py` (import scanner)
- [ ] `uv run pytest` — full test suite passes
- [ ] All 16 CLI entry points resolve

### 11B — Update Documentation

- [ ] Update `examples/README.md` with new paths
- [ ] Update any `docs/` references to `modules/` paths
- [ ] Update `tests/modules/analysis/*.md` docs (moved in Phase 7)

---

## Critical Files

| File | Why |
|------|-----|
| `src/digitalmodel/modules/__init__.py` | MetaPathFinder compat shim — do not modify |
| `src/digitalmodel/_compat.py` | Moved modules registry — do not modify |
| `tests/conftest.py` | Shared test fixtures — may need path updates |
| `pyproject.toml` | testpaths, CLI entry points |
| `.gitignore` | Output directory patterns |

---

## Verification

After each phase:
1. `uv run pytest` — test suite passes
2. `grep -r "digitalmodel\.modules\." --include="*.py"` — only compat layer references remain
3. No broken imports in tests or scripts

Final state:
- `tests/modules/` does not exist
- `examples/modules/` does not exist
- All test files organized by module in `tests/X/`
- Zero `digitalmodel.*` imports outside compat layer
- Clean repo root (no empty dirs, no stale outputs)

---

## Progress

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 0-6: src/ Flattening | Done | Merged to main (PR #129) |
| Phase 7: Flatten tests/modules/ | Pending | 47 subdirs, 13 duplicates |
| Phase 8: Fix Legacy Imports | Pending | 12 test + 15 script references |
| Phase 9: Flatten examples/modules/ | Pending | 24 subdirs |
| Phase 10: Root-Level Cleanup | Pending | Empty dirs, output consolidation |
| Phase 11: Final Verification | Pending | Docs, structural checks |

---

## Cross-Review Process (MANDATORY)

> **REQUIREMENT**: Minimum **3 review iterations** with OpenAI Codex and Google Gemini before implementation.

| Iter | Date | Reviewer | Status | Feedback Summary |
|------|------|----------|--------|------------------|
| 1 | | Codex | Pending | |
| 1 | | Gemini | Pending | |
| 2 | | Codex | Pending | |
| 2 | | Gemini | Pending | |
| 3 | | Codex | Pending | |
| 3 | | Gemini | Pending | |

---

## Session Log

| Date | Session ID | Agent | Notes |
|------|------------|-------|-------|
| 2026-02-01 | velvety-wandering-bird | claude-opus-4.5 | Plan created, Phases 0-6 implemented and merged |
| 2026-02-01 | velvety-wandering-bird | claude-opus-4.5 | Phases 7-11 planned for repo-wide restructuring |
