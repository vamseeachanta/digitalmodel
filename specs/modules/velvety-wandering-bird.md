---
title: "Flatten and simplify digitalmodel module structure"
description: "Restructure the repository so all modules live directly under src/digitalmodel/ instead of nested in modules/. Fix naming violations, consolidate duplicates, absorb top-level packages."
version: "1.0"
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

status: "draft"
progress: 0
priority: "high"
complexity: "complex"
tags: [architecture, refactoring, discoverability, module-structure]

created: "2026-02-01"
updated: "2026-02-01"
target_completion: ""

links:
  spec: "specs/modules/velvety-wandering-bird.md"
  work_item: "WRK-066"
  branch: "refactor/wrk-066-flatten-module-structure"
---

# Flatten and Simplify digitalmodel Module Structure

> **Module**: digitalmodel | **Status**: draft | **Work Item**: WRK-066

## Summary

The current layout has 3 layers of confusion:
1. **25 framework packages** directly under `src/digitalmodel/` (core, fatigue, mooring, stress, etc.)
2. **50 domain modules** nested under `src/digitalmodel/modules/` (orcaflex, fatigue_analysis, etc.)
3. **4 separate packages** at `src/` top-level outside digitalmodel (blender_automation, data_procurement, marine_engineering, reports)

Plus: 3 kebab-case dirs that can't be imported, duplicate modules in multiple locations, and dead/stub modules.

**Target state**: Everything flat under `src/digitalmodel/`. Import is `digitalmodel.orcaflex`, not `digitalmodel.orcaflex`.

---

## Current Problems

| Problem | Examples | Count |
|---------|----------|-------|
| Modules buried in `modules/` subdir | `digitalmodel.orcaflex` instead of `digitalmodel.orcaflex` | 50 dirs |
| Duplicate modules | fatigue/ + modules/fatigue_analysis/, mooring/ + modules/mooring/, reporting in 2 places, data_procurement in 3 places, marine_engineering in 3 places | 6 sets |
| Kebab-case dirs (not importable) | `design-tools/`, `mcp-server/`, `orcaflex-browser/` | 3 dirs |
| Empty/dead modules | `structuralstrength/` (empty), `static/` (CSS/JS only) | 2 dirs |
| Packages outside digitalmodel | `src/blender_automation/`, `src/data_procurement/`, `src/marine_engineering/`, `src/reports/` | 4 pkgs |
| Duplicate config dirs | `config/` (real code) + `configs/` (build artifacts) | 2 dirs |
| Duplicate validators | `validation/` + `validators/` | 2 dirs |

**Import impact**: ~1,218 import statements across ~373 files reference `digitalmodel.modules.*`

---

## Phases

### Phase 0: Safety Net

- [ ] Tag current state: `git tag pre-restructure-v1`
- [ ] Create migration compat module `src/digitalmodel/_compat.py` with `sys.modules` aliasing
- [ ] Build import map script (crawl all `from digitalmodel.modules.` imports, output JSON)
- [ ] Add `tests/test_restructure_compat.py` — asserts both old and new import paths work

### Phase 1: Fix Naming Violations

Rename 3 kebab-case directories to valid Python names:

| From | To |
|------|----|
| `modules/design-tools/` | `modules/design_tools/` |
| `modules/mcp-server/` | `modules/mcp_server/` |
| `modules/orcaflex-browser/` | `modules/orcaflex_browser/` |

- [ ] `git mv` each directory
- [ ] Update `pyproject.toml` uv workspace member path (`src/modules/mcp-server/orcawave-mcp` -> correct path)
- [ ] Add `__init__.py` where missing
- [ ] Update any filesystem-path references in scripts/docs

**Risk**: Low — these dirs were never importable via Python.

### Phase 2: Remove Dead Code and Clean Artifacts

- [ ] Delete `modules/structuralstrength/` (empty dir, zero references)
- [ ] Move `modules/static/` CSS/JS to `assets/static/` (not a Python module)
- [ ] Relocate `configs/` contents (coverage.json, pytest.ini, etc.) to repo root, delete `configs/` dir
- [ ] Merge `validators/data_validator.py` into `validation/`, delete `validators/`

### Phase 3: Consolidate Duplicates

Resolve each duplicate so every concept has exactly one canonical location.

**3A — mooring**
- [ ] Move `digitalmodel/mooring/calm_buoy/` into `modules/mooring_analysis/calm_buoy/`
- [ ] Delete `digitalmodel/mooring/` (framework-level dir)
- [ ] Keep `modules/mooring/` stub for engine.py (flattens in Phase 4)

**3B — fatigue**
- [ ] Rename `digitalmodel/fatigue/` to `digitalmodel/fatigue_core/` (SN curve library, frequency domain)
- [ ] Keep `modules/fatigue_analysis/` as-is (application-level rainflow, CLI)
- [ ] Add backward-compat re-export in `digitalmodel/fatigue/__init__.py`

**3C — reporting**
- [ ] Move `modules/reporting/report_generator.py` + `path_utils.py` into `digitalmodel/reporting/`
- [ ] Update `digitalmodel/reporting/__init__.py` to export PlotlyReportGenerator
- [ ] Replace `modules/reporting/` with re-export shim

**3D — data_procurement**
- [ ] Delete `src/data_procurement/` (top-level skeleton, subset of modules version)
- [ ] Keep `digitalmodel/data_procurement/` (API clients: metocean, vessel, mooring, riser)
- [ ] Rename `modules/data_procurement/` to `modules/data_scraping/` (web scrapers for PDFs)

**3E — marine_engineering**
- [ ] Delete `src/marine_engineering/` (top-level partial copy)
- [ ] Keep `modules/marine_engineering/` as canonical (catenary, hydro, OCIMF, wave spectra)

**3F — blender_automation**
- [ ] Delete `src/blender_automation/` (lacks `__init__.py`, incomplete)
- [ ] Keep `modules/blender_automation/` as canonical

### Phase 4: Flatten modules/ Directory

Move all ~50 subdirectories from `modules/X/` to `digitalmodel/X/`.

**Backward compatibility**: Add `__getattr__` to `modules/__init__.py`:

```python
def __getattr__(name):
    if name in _MOVED_MODULES:
        warnings.warn(f"digitalmodel.modules.{name} is deprecated. Use digitalmodel.{name}", DeprecationWarning)
        mod = importlib.import_module(f"digitalmodel.{name}")
        sys.modules[f"digitalmodel.modules.{name}"] = mod
        return mod
    raise AttributeError(...)
```

**Move in batches** (commit per batch, test between batches):

| Batch | Modules | Rationale |
|-------|---------|-----------|
| 1 | transformation, vertical_riser, rigging, ct_hydraulics, pipe_cross_section, fea_model, pyintegrity, finance, project_management, digitalmarketing, services, skills, visualization | Leaf modules, no cross-deps |
| 2 | rao_analysis, time_series, pipe_capacity, pipeline, hydrodynamics, structural, structural_analysis, viv_analysis | Core analysis |
| 3 | signal_analysis, fatigue_analysis, mooring_analysis, catenary, catenary_riser, mooring | Signal + fatigue + mooring chain |
| 4 | orcaflex, aqwa, diffraction, bemrosetta, gis, orcawave | Large/complex modules |
| 5 | automation, workflow_automation, marine_analysis, marine_engineering, orcaflex_post_process, artificial_lift, ai_workflows, api_analysis | Remaining |
| 6 | design_tools, mcp_server, orcaflex_browser, data_scraping, blender_automation, reporting | Previously renamed/merged |

**Per batch**:
- [ ] `git mv modules/X/ digitalmodel/X/` (check for name collisions first)
- [ ] Update internal imports within moved modules
- [ ] Run test suite
- [ ] Commit

**After all batches**:
- [ ] Update 16 CLI entry points in `pyproject.toml` (remove `.modules.` from paths)
- [ ] Update `engine.py` imports
- [ ] Update `__init__.py` re-exports

### Phase 5: Absorb Remaining Top-Level Packages

- [ ] Delete `src/blender_automation/` (already handled in 3F)
- [ ] Delete `src/marine_engineering/` (already handled in 3E)
- [ ] Delete `src/data_procurement/` (already handled in 3D)
- [ ] Move `src/reports/cp/` into `digitalmodel/reporting/cp_assets/`, delete `src/reports/`
- [ ] Verify `pyproject.toml` `include = ["digitalmodel*"]` covers everything

### Phase 6: Cleanup

- [ ] Remove `modules/` directory entirely (move compat shim to `_compat.py`)
- [ ] Add deprecation timeline to CHANGELOG (6 months warning, 12 months removal)
- [ ] Update README import examples
- [ ] Update `docs/` references

---

## Name Collision Resolution

These `modules/` dirs would collide with existing framework-level dirs when flattened:

| Module | Existing dir | Resolution |
|--------|-------------|------------|
| `modules/mooring/` | `digitalmodel/mooring/` | Phase 3A deletes framework-level mooring/ first |
| `modules/reporting/` | `digitalmodel/reporting/` | Phase 3C merges into framework-level reporting/ |
| `modules/data_procurement/` | `digitalmodel/data_procurement/` | Phase 3D renames to data_scraping/ |
| `modules/marine_engineering/` | (none after 3E) | Clean move |
| `modules/blender_automation/` | (none after 3F) | Clean move |
| `modules/stress/` | `digitalmodel/stress/` | Need to audit — merge or rename |

---

## Critical Files

| File | Why |
|------|-----|
| `src/digitalmodel/__init__.py` | Re-exports 12 classes from modules — update every phase |
| `src/digitalmodel/modules/__init__.py` | Becomes `__getattr__` compat shim in Phase 4 |
| `pyproject.toml` | 16 CLI entry points + package discovery + uv workspace |
| `src/digitalmodel/engine.py` | 20 imports from `digitalmodel.modules.*` |

---

## Verification

After each phase:
1. `uv run pytest` — full test suite must pass
2. `uv run python -c "from digitalmodel import OrcaFlex, Aqwa, Mooring"` — top-level re-exports work
3. `uv run python -c "from digitalmodel.orcaflex import OrcaFlex"` — backward compat works (Phases 4+)
4. `uv run digital_model --help` — CLI entry point works
5. Verify all 16 CLI commands resolve: `run-to-sim --help`, `aqwa --help`, etc.

Final verification:
- `ls src/` shows only `digitalmodel/` (no other packages)
- `ls src/digitalmodel/modules/` is gone or empty compat shim
- No `from digitalmodel.modules.` in source (only in compat layer)
- Deprecation warnings emit when using old paths

---

## Progress

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 0: Safety Net | Pending | |
| Phase 1: Fix Naming | Pending | |
| Phase 2: Remove Dead Code | Pending | |
| Phase 3: Consolidate Duplicates | Pending | |
| Phase 4: Flatten modules/ | Pending | Largest phase (~50 dirs, ~1218 imports) |
| Phase 5: Absorb Top-Level | Pending | |
| Phase 6: Cleanup | Pending | |

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
| 2026-02-01 | velvety-wandering-bird | claude-opus-4.5 | Plan created, linked to WRK-066 |
