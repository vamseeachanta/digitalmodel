---
title: "WRK-066 Iteration 2: Clean ghost directories, flatten nested names, delete legacy"
description: "Post-grouping cleanup: remove 33 ghost dirs, flatten visualization/visualization/ and hydrodynamics/hydrodynamics/, delete obsolete legacy/, clean modules/ stubs, handle untracked files."
version: "1.0"
module: "digitalmodel"

session:
  id: "memoized-wishing-gray"
  agent: "claude-opus-4.5"

review:
  required_iterations: 3
  current_iteration: 3
  status: "completed"
  findings:
    iteration_1:
      focus: "correctness and completeness"
      critical: "missing rao_analysis/__init__.py (FIXED in 71bbfb2a)"
      major: "diffraction/cli.py deprecated imports (PRE-EXISTING)"
      verdict: "1 regression found and fixed"
    iteration_2:
      focus: "security, edge cases, regression risk"
      critical: "import hook sanitization (LOW risk, Python prevents traversal)"
      major: "deprecated paths in source files (ALL PRE-EXISTING)"
      verdict: "no new regressions"
    iteration_3:
      focus: "completeness of fix, final assessment"
      tests: "31/31 passed (30 compat + 1 rao_analysis)"
      verdict: "PASS — commit is sound after __init__.py fix"
  reviewers:
    cross_review_agent:
      status: "completed"
      iterations: 3

status: "reviewed"
implemented: "2026-02-03"
commit: "a507315e"
fix_commit: "71bbfb2a"
priority: "high"
complexity: "complex"
tags: [architecture, cleanup, WRK-066, iteration-2]

created: "2026-02-02"
links:
  spec: "specs/modules/memoized-wishing-gray.md"
  prior_spec: "specs/modules/velvety-wandering-bird.md"
  work_item: "WRK-066"
---

# WRK-066 Iteration 2: Post-Grouping Cleanup

> **Continues**: velvety-wandering-bird (Phases 0-6 flatten + Phases 12-18 grouping)

## What Was Already Done

- **72187cd1**: Flattened `modules/` to `digitalmodel/` (Phases 0-6)
- **c187d3ed**: Reorganized into 11 domain groups with 3-layer compat (Phases 12-18 iter 1)

## What This Plan Covers

Six cleanup phases, ordered by risk (lowest first).

---

## Phase A: Delete 33 Ghost Directories

**Risk: NONE** — These contain only `__pycache__/` (no `.py` files).

Delete these directories at `src/digitalmodel/`:

```
agents/  analysis/  aqwa/  base_configs/  base_solvers/  calculations/
common/  config/  core/  ct_hydraulics/  custom/  data/  data_procurement/
digitalmarketing/  fatigue/  mooring_analysis/  orcaflex/  pipe_capacity/
pipeline/  rao_analysis/  reporting/  reservoir/  signal_analysis/  stress/
time_series/  transformation/  validation/  validators/  vertical_riser/
viv_analysis/
```

Also delete ghost subdirs under `modules/`:
```
modules/aqwa/  modules/artificial_lift/  modules/automation/  modules/bemrosetta/
modules/blender_automation/  modules/catenary/  modules/catenary_riser/
modules/ct_hydraulics/  modules/data_procurement/  modules/design-tools/
modules/diffraction/  modules/digitalmarketing/  modules/fatigue_analysis/
modules/gmsh_meshing/  modules/hydrodynamics/  modules/marine_analysis/
modules/marine_engineering/  modules/mcp-server/  modules/mooring/
modules/mooring_analysis/  modules/orcaflex/  modules/orcaflex_post_process/
modules/pipe_capacity/  modules/pipeline/  modules/pyintegrity/
modules/rao_analysis/  modules/reporting/  modules/rigging/
modules/signal_analysis/  modules/skills/  modules/structural_analysis/
modules/time_series/  modules/transformation/  modules/vertical_riser/
modules/viv_analysis/  modules/workflow_automation/
```

**Keep**: `modules/__init__.py` (Layer 1 compat shim — still needed)

**Verification**: `uv run pytest tests/test_restructure_compat.py` passes

---

## Phase B: Handle Untracked Files

**Risk: LOW** — Assess before acting.

Git status shows untracked:
- `src/digitalmodel/modules/orcawave/`
- `src/digitalmodel/modules/visualization/`

**Action**: Check if these contain real source files or are just `__pycache__` ghosts.
- If ghost → delete
- If real files → move to correct grouped location (`solvers/orcawave/`, `visualization/`)

---

## Phase C: Delete legacy/ Directory

**Risk: LOW** — 0 production imports, only 4 test mock references.

**What**: Delete `src/digitalmodel/legacy/` (148 files: apirp2rd, apistd2rd, nl_stress, sn_curves, stackup_schematic, vmstress)

**Import audit results**: Zero imports from `digitalmodel.legacy` in source code. Only 4 mock patch references in `tests/workflows/integration/test_legacy_compatibility.py`.

**Action**:
1. Delete `src/digitalmodel/legacy/`
2. Update or delete `tests/workflows/integration/test_legacy_compatibility.py` (remove legacy mock patches)

**Verification**: `uv run pytest` — full suite passes

---

## Phase D: Flatten Nested Name Duplication

### D1: Flatten `visualization/visualization/` → `visualization/`

**Risk: LOW** — 2 files need import updates, no name collisions.

**Files to move up**:
- `visualization/visualization/agent_dashboard.py` → `visualization/agent_dashboard.py`
- `visualization/visualization/orcaflex-dashboard/` → `visualization/orcaflex-dashboard/`
- Merge `visualization/visualization/__init__.py` into `visualization/__init__.py`
- Delete `visualization/visualization/`

**Files to update** (2):
- `tests/test_agent_dashboard.py` — update import path
- `examples/visualization/visualization/basic_usage.py` — update import path

**Add compat redirect**: Register `visualization.visualization` → `visualization` in `_compat.py` if needed, or accept the clean break (low usage).

### D2: Flatten `hydrodynamics/hydrodynamics/` → `hydrodynamics/`

**Risk: MODERATE** — 30+ import updates, CLI entry point change.

**Files to move up** (7):
- `hydrodynamics/hydrodynamics/cli.py` → `hydrodynamics/cli.py`
- `hydrodynamics/hydrodynamics/coefficient_database.py` → `hydrodynamics/coefficient_database.py`
- `hydrodynamics/hydrodynamics/interpolator.py` → `hydrodynamics/interpolator.py`
- `hydrodynamics/hydrodynamics/models.py` → `hydrodynamics/models.py`
- `hydrodynamics/hydrodynamics/ocimf_loading.py` → `hydrodynamics/ocimf_loading.py`
- `hydrodynamics/hydrodynamics/wave_spectra.py` → `hydrodynamics/wave_spectra.py`
- Merge `hydrodynamics/hydrodynamics/__init__.py` into `hydrodynamics/__init__.py`
- Delete `hydrodynamics/hydrodynamics/`

**Critical files to update**:
- `pyproject.toml` — CLI entry point: `hydrodynamics = "digitalmodel.hydrodynamics.cli:main"`
- `tests/specialized/cli/test_hydrodynamics_cli.py` (24 updates)
- `tests/hydrodynamics/hydrodynamics/test_hydrodynamics_unit.py` (5 imports) — also move test dir
- `tests/hydrodynamics/hydrodynamics/test_hydrodynamics_cli.py` (1 import) — also move test dir
- `tests/test_restructure_compat.py` (1 reference)
- `examples/hydrodynamics/hydrodynamics/basic_usage.py`

**Verification**:
- `uv run pytest tests/hydrodynamics/` passes
- `uv run python -m digitalmodel.hydrodynamics.cli --help` works

---

## Phase E: Update Documentation Import Paths

**Risk: LOW** — Documentation-only changes.

Batch find-replace across `docs/`, `specs/`, `.claude/skills/`, `examples/`:
- `digitalmodel.modules.X` → `digitalmodel.<group>.X`
- `digitalmodel.orcaflex` → `digitalmodel.solvers.orcaflex`
- `digitalmodel.aqwa` → `digitalmodel.hydrodynamics.aqwa`
- (apply full _FLAT_TO_GROUP mapping)

**Verification**: Grep confirms no remaining `digitalmodel.modules.` references in docs.

---

## Phase F: Final Cleanup

1. Remove any remaining empty `__pycache__` dirs: `find src/ -type d -name __pycache__ -empty -delete`
2. Verify `modules/` contains only `__init__.py` (compat shim)
3. Update WRK-066 work item status

**Verification**:
- `uv run pytest` — full suite green
- `uv run python -c "from digitalmodel.solvers.orcaflex.orcaflex import OrcaFlex"` — grouped path works
- `uv run python -c "from digitalmodel.orcaflex.orcaflex import OrcaFlex"` — compat path works with warning
- All 16 CLI entry points resolve

---

## Critical Files

| File | Role |
|------|------|
| `src/digitalmodel/__init__.py` | Top-level exports, installs compat |
| `src/digitalmodel/_compat.py` | Layer 2 flat→group redirect |
| `src/digitalmodel/modules/__init__.py` | Layer 1 modules→flat redirect |
| `pyproject.toml` | CLI entry points, package config |
| `tests/test_restructure_compat.py` | Compat test suite |

---

## Execution Order

```
Phase A (ghost dirs)  →  Phase B (untracked)  →  Phase C (legacy/)
     →  Phase D1 (vis flatten)  →  Phase D2 (hydro flatten)
     →  Phase E (docs)  →  Phase F (final)
```

Each phase: implement → test → commit.
