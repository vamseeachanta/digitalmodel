---
title: "WRK-066 Iteration 3: Migrate deprecated imports to canonical grouped paths"
description: "Update source code, examples, and config to use canonical grouped import paths instead of relying on the compat redirect layer."
version: "1.0"
module: "digitalmodel"

session:
  id: "racing-placid-gazelle"
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
priority: "medium"
complexity: "moderate"
tags: [architecture, imports, WRK-066, iteration-3]

created: "2026-02-03"
links:
  spec: "specs/modules/racing-placid-gazelle.md"
  prior_spec: "specs/modules/memoized-wishing-gray.md"
  work_item: "WRK-066"
---

# WRK-066 Iteration 3: Migrate Deprecated Imports to Canonical Paths

> **Module**: digitalmodel | **Status**: draft | **Created**: 2026-02-03

## Summary

Update source code, examples, and configuration to use canonical grouped import paths directly, reducing reliance on the 3-layer compat redirect system. This is the final cleanup iteration for WRK-066.

## What Was Already Done

- **72187cd1**: Flattened `modules/` to `digitalmodel/` (iter 0)
- **c187d3ed**: Reorganized into 11 domain groups with 3-layer compat (iter 1)
- **a507315e**: Deleted ghosts, flattened nesting, removed legacy (iter 2)
- **71bbfb2a**: Fixed missing rao_analysis/__init__.py (iter 2 review fix)

## What This Plan Covers

Four phases, ordered by risk (lowest first).

---

## Phase A: Fix Deprecated Imports in Source Code

**Risk: LOW** — 7 imports in 6 files, all have working compat fallback.

Update `from digitalmodel.diffraction import` to `from digitalmodel.hydrodynamics.diffraction import` in:

| File | Lines | Import |
|------|-------|--------|
| `src/digitalmodel/hydrodynamics/diffraction/cli.py` | 23, 31 | `digitalmodel.diffraction` |
| `src/digitalmodel/hydrodynamics/diffraction/batch_processor.py` | 200 | `digitalmodel.diffraction` |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py` | ~9 | `digitalmodel.diffraction` |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | ~16 | `digitalmodel.diffraction` |
| `src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py` | ~13 | `digitalmodel.diffraction` |
| `src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py` | ~12 | `digitalmodel.diffraction` |

**Verification**: `uv run pytest tests/hydrodynamics/ -x -v`

---

## Phase B: Fix pyproject.toml Entry Point

**Risk: LOW** — configuration fix only.

The `test-automation` entry point references `test_automation.__main__:main`, which lives in `tests/` (excluded from package builds).

**Action**: Either:
1. Remove the entry point (if test-automation CLI is not user-facing), OR
2. Move the module to `src/digitalmodel/test_automation/` if it should be distributed

**Verification**: `uv run python -c "import importlib; [importlib.import_module(ep.value.split(':')[0]) for ep in importlib.metadata.entry_points(group='console_scripts') if ep.name != 'test-automation']"` (all other entry points resolve)

---

## Phase C: Update Example Files

**Risk: NONE** — examples are not imported by production code.

~51 deprecated import references across `examples/` files. Two patterns:

1. `from digitalmodel.modules.X import` → `from digitalmodel.<group>.X import`
2. `from digitalmodel.<flat_name> import` → `from digitalmodel.<group>.<flat_name> import`

Apply `_FLAT_TO_GROUP` mapping to all example files.

**Verification**: `grep -r "digitalmodel\.modules\." examples/` returns zero results.

---

## Phase D: Update Remaining Source Docstrings and READMEs

**Risk: NONE** — documentation only.

~12 deprecated references in `__init__.py` docstrings and module READMEs within `src/`:

- `src/digitalmodel/solvers/orcaflex/MODULE_README.md` (6 refs)
- `src/digitalmodel/structural/fatigue/__init__.py` docstring (1 ref)
- `src/digitalmodel/structural/stress/README.md` (3 refs)
- `src/digitalmodel/marine_ops/reservoir/__init__.py` docstring (1 ref)
- Other scattered refs (~2)

**Verification**: `grep -r "from digitalmodel\.\(orcaflex\|fatigue\|stress\|reservoir\)" src/ --include="*.md" --include="*.py"` returns only canonical paths.

---

## Execution Order

```
Phase A (source imports)  →  Phase B (pyproject.toml)
     →  Phase C (examples)  →  Phase D (docstrings)
```

Each phase: implement → test → commit.

---

## Critical Files

| File | Role |
|------|------|
| `src/digitalmodel/_compat.py` | Compat redirect layer (DO NOT modify) |
| `src/digitalmodel/modules/__init__.py` | Layer 1 compat shim (DO NOT modify) |
| `pyproject.toml` | CLI entry points |

---

## Out of Scope

- Removing the compat layer itself (still needed for external consumers)
- Modifying test imports (tests may legitimately test compat paths)
- Changing `_ModulesRedirectFinder` insert position (separate concern)

---

## Cross-Review Process (MANDATORY)

> **REQUIREMENT**: Minimum **3 review iterations** with OpenAI Codex and Google Gemini before implementation.

### Review Status

| Gate | Status |
|------|--------|
| Iterations (>= 3) | 0/3 |
| OpenAI Codex | pending |
| Google Gemini | pending |
| **Ready** | false |

### Approval Checklist

- [ ] Iteration 1 complete (both reviewers)
- [ ] Iteration 2 complete (both reviewers)
- [ ] Iteration 3 complete (both reviewers)
- [ ] **APPROVED**: Ready for implementation

---

## Progress

| Phase | Status | Notes |
|-------|--------|-------|
| Review Iteration 1 | Pending | |
| Review Iteration 2 | Pending | |
| Review Iteration 3 | Pending | |
| Plan Approved | Pending | |
| Phase A | Pending | 7 imports in 6 source files |
| Phase B | Pending | 1 entry point fix |
| Phase C | Pending | ~51 example file updates |
| Phase D | Pending | ~12 docstring/README refs |

---

## Session Log

| Date | Session ID | Agent | Notes |
|------|------------|-------|-------|
| 2026-02-03 | racing-placid-gazelle | claude-opus-4.5 | Plan created |
