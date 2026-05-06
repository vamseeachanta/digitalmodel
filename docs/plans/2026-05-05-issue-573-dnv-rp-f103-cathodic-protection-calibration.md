# Plan: digitalmodel #573 — DNV-RP-F103 calibration drift in `test_cathodic_protection_dnv.py`

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/573
**Status:** plan-review
**Tier:** T2 (calibration: 16 tests across 6 classes; involves DNV-RP-F103 standard reconciliation + Citation emission)

## Root cause
- Test file: `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` — 16 failures across 6 `TestDNV*` classes (full list in issue body).
- Source: `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:67` `DNV_RP_F103_2010` method.
- Calibration drift between source and test fixture values for one or more DNV-RP-F103 (Oct 2010) constants/formulas:
  - Eq.11 (longitudinal metallic resistance)
  - Annex 1 Eq.2 / Eq.4 (coating breakdown)
  - Table 5-1 (current density values)
  - Sec. 5.6.10 (steel resistivity default)
- 6 classes failing simultaneously suggests a shared constant or helper drifted; some tests may need value updates while others may need source corrections.

## Pre-requisite (now satisfied)
Wiki citation surface for DNV-RP-F103 has landed via [vamseeachanta/workspace-hub#2636](https://github.com/vamseeachanta/workspace-hub/pull/2636). Per `.claude/rules/calc-citation-contract.md`, source method must emit `Citation` references on each standards-derived constant/formula it uses.

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py -xvs 2>&1 | tee /tmp/issue-573-repro.log
```
Capture verbatim top error from one representative failing test in each of the 6 classes.

### Task 2 — Reconcile against DNV-RP-F103 (Oct 2010)
Reference PDF on the Elements drive: `/mnt/ace/O&G-Standards/DNV/DNV_RP_F103_(2010)_Cathodic_Protection_of_Submarine_Pipelines_by_Galvanic_Anodes.pdf`. Wiki page: `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-f103.md` (workspace-hub).

For each failing test, classify:
- **Source drifted from standard** → fix source value in `cathodic_protection.py`.
- **Test fixture drifted from standard / source rounding precision** → update test expected value.
- **Both wrong** → fix both, document which authoritative value was used.

Build a small CSV mapping `test_name → drifted_constant → standard_value → source_value → fix_target`.

### Task 3 — Fix
- Edit `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:67` `DNV_RP_F103_2010` method:
  - Update any drifted constants to standard values.
  - Add `Citation` emission per `.claude/rules/calc-citation-contract.md` — each standards-derived constant gets a `Citation` instance pointing at `dnv-rp-f103.md`. Use `digitalmodel/src/digitalmodel/orcaflex/mooring_design.py` (DNV-OS-E301 pilot) as the emission pattern.
- Edit `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` for tests where the fixture (not source) was drifted.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py -xvs
```
All 16 tests pass.

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/ -x -k "cathodic or dnv"
```
Currently-passing companion `test_dnv_rp_b401_doc_verified.py` must remain green (confirms F103/B401 separation is intact).

## Acceptance Criteria
- All 16 listed tests pass under `pytest -x` and under `pytest-xdist`.
- `test_dnv_rp_b401_doc_verified.py` remains green (no F103/B401 cross-contamination).
- `cathodic_protection.py::DNV_RP_F103_2010` emits `Citation` references per `.claude/rules/calc-citation-contract.md`, pointing at `dnv-rp-f103.md` wiki page (failing closed if wiki page absent — verifies #2471 frontmatter resolves).
- PR body lists per-test root cause (source-drift vs test-drift) and which authoritative DNV-RP-F103 value was applied.

## Out of scope
- DNV-RP-B401 (separate test suite, currently passing).
- Wiki page maintenance for `dnv-rp-f103.md` (separate workspace-hub workflow).
- Other workspace-hub#2609 clusters (R1/R2/R4/R5/R6/R7/R8 — separate sub-issues).

## Open questions
- If the F103 wiki page lacks `code_id`/`publisher`/`revision` frontmatter (#2471), forward-adopt those fields per `calc-citation-contract.md` step 2 and call this out in the PR body.
