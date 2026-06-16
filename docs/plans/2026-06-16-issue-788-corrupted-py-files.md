# Plan for #788: fix/retire 12 non-Python `.py` files that break imports + mypy

> **Status:** plan-review (NOT self-approved)
> **Complexity:** T2 (12 files, well-cataloged; 6 need real fixes, 6 are orphans to rename/delete + a cross-repo baseline un-exempt)
> **Date:** 2026-06-16
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/788

---

## Resource Intelligence Summary

12 files under `src/` carry a `.py` extension but fail `ast.parse` (confirmed on the live tree 2026-06-16) — they break `import`, `py_compile`, packaging, and mypy (mypy aborts exit 2 on the first one reached). Surfaced while implementing the workspace-hub mypy ratchet (#3148), which currently **exempts** digitalmodel because of these.

### The corruption looks like a damaging event, not random rot
Across the 12: a stray space inside a class name, mangled `lask import` (truncated `from flask import`), an `auth=***` redaction literal, injected katakana inside a keyword (`asyncムasync`), leading/mid-file BOM (U+FEFF), a malformed comprehension, FastAPI signature bugs, and a filename with a space. The spread (encoding damage + redaction + truncation) suggests a bad merge / encoding-mangling / find-replace event worth a root-cause look, not piecemeal typos.

### Disposition catalog
**A. IMPORTED — break live chains → FIX (priority):**
| File | Defect | Imported by |
|---|---|---|
| `visualization/orcaflex_dashboard/backend/app/api/exports.py` | non-default param after defaulted (FastAPI) | `backend/app/main.py:22` |
| `visualization/orcaflex_dashboard/backend/app/api/upload.py` | same FastAPI sig bug | `backend/app/main.py:23` |
| `solvers/orcawave/diffraction/orchestrator.py` | leading BOM U+FEFF | `hydrodynamics/diffraction/diffraction_cli.py:238` (note: import path `digitalmodel.orcawave...` ≠ on-disk `solvers/orcawave...` — may be stale) |
| `web/digitaltwinfeed/AppMenuDropdown/AppMenuDropdown.py` | mangled `lask import` | blueprint registry `web/digitaltwinfeed/app.py:41-44` |
| `web/digitaltwinfeed/bseedata/bseedata.py` | mangled `lask import` | app.py:41-44 |
| `web/digitaltwinfeed/BuoyCALM/BuoyCALM.py` | `auth=***` redaction literal | app.py:41-44 |

**B. ORPHANS — no inbound imports → rename out of `*.py` (or delete if dead):**
`specialized/project_management/projectScheduleD01.py` + `projectSchedule_D01.py` (CLI/spec text), `visualization/design_tools/case_studies/01_offshore_platform.py` (stray space in class; filename starts with digit), `workflows/ai_workflows/case_studies/06_refinery_upgrade.py` (malformed comprehension; digit-prefixed), `workflows/mcp_server/orcawave/client/cli_client.py` (katakana injection), `structural/plate_capacity/PlateBuckling_Plots/plateBucklingCal_Long Plot.py` (space in filename + BOM).

### Reproduce (Step 1.5 — confirmed)
`find src -name '*.py' | ast.parse` → 12 failures. `uv run mypy src/ --config-file pyproject.toml` → exit 2 (crash). The 3 import chains above fail to load.

---

## Design

1. **Group A (imported) — fix the corruption** so the import chains load:
   - `orchestrator.py`: strip the BOM (trivial).
   - `exports.py`/`upload.py`: reorder params so non-default precedes default (or give the param a default) — verify against FastAPI router usage.
   - blueprints (`AppMenuDropdown`/`bseedata`/`BuoyCALM`): repair `from flask import ...`; replace the `auth=***` redaction with the intended value or an env lookup (confirm intent — do NOT hardcode a secret).
   - Each fix verified by `python -c "import ast; ast.parse"` + the importing module loading.
2. **Group B (orphans) — rename or delete.** Per file decide: if it's spec/CLI text, rename to `.txt`/`.md` (or move under a non-package dir); if it's a dead corrupted script, delete. **Owner decision needed per file** (surface at review) — default: rename to preserve content, since deletion is irreversible.
3. **Cross-repo: un-exempt digitalmodel in the mypy ratchet.** After all 12 are fixed, in workspace-hub `config/quality/mypy-baseline.yaml` replace digitalmodel's `exempt: true` with the real measured `error_count` (re-seed via `check_mypy_ratchet.py --init` or a measured run). Separate workspace-hub PR; reference #788.

---

## Files to Change
- 6 Group-A files (fix) + 6 Group-B files (rename/delete) in digitalmodel.
- (separate workspace-hub PR) `config/quality/mypy-baseline.yaml` — un-exempt digitalmodel.
- A guard test (below).

---

## TDD Test List
| Test | Verifies |
|---|---|
| no_non_python_py_files_in_src | `ast.parse` passes for EVERY `src/**/*.py` (regression guard — prevents recurrence) |
| dashboard_backend_imports | `backend/app/main.py` imports exports+upload routers without error |
| digitaltwinfeed_blueprints_import | the 3 blueprints import + register |
| diffraction_orchestrator_imports | `diffraction_cli` import resolves (or path corrected) |
| mypy_runs_not_crashes | `uv run mypy src/` exits 0/1, never 2 |

---

## Acceptance Criteria
- [ ] 0 files in `src/` fail `ast.parse`.
- [ ] The 3 broken import chains load.
- [ ] `uv run mypy src/` runs (exit 1, real count) — no exit-2 crash.
- [ ] workspace-hub mypy-baseline un-exempts digitalmodel with the measured count (separate PR).
- [ ] Add the `no_non_python_py_files_in_src` regression test so this can't recur.
- [ ] legal-sanity-scan passes (esp. the `auth=***` fix introduces no hardcoded secret).

---

## Risks / Open Questions
- **Open (per orphan): rename vs delete** — needs owner judgment; default rename to preserve.
- **Risk:** `BuoyCALM auth=***` — must not be "fixed" by hardcoding a secret; use env/config. Flag for review.
- **Risk:** `orchestrator.py` import path mismatch (`digitalmodel.orcawave` vs on-disk `solvers/orcawave`) — fixing the BOM may expose a separate stale-import bug; verify the importer.
- **Root cause:** investigate how 12 files got corrupted (bad merge/encoding) to prevent recurrence — possibly a pre-commit `ast.parse` guard repo-wide.

## Complexity: T2
12 cataloged files; 6 mechanical-ish fixes (BOM/import/sig), 6 rename/delete, 1 cross-repo baseline + a regression test. Imported-file fixes need intent verification.
