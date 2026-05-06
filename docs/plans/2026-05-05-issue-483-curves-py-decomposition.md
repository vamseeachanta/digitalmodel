# Plan: digitalmodel #483 — curves.py decomposition: break up 29,666-line monolith

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/483
**Status:** plan-review
**Tier:** T3 (multi-file refactor)
**Priority:** high
**Parent:** #1962 (Tier-1 Repo Ecosystem Refactoring) — Phase 2

## Context

The issue body identifies `src/digitalmodel/naval_architecture/curves.py` at 29,666 lines as the largest technical debt item. **Investigation finding (potential blocker):** that path is **not git-tracked** in the source tree. The 29,671-line file lives at `build/lib/digitalmodel/naval_architecture/curves.py` (a build artifact). The git-tracked source tree contains `src/digitalmodel/naval_architecture/curves_of_form.py` (1,929 bytes) plus 25+ other naval_architecture modules already split out (`hydrostatics.py`, `damage_stability.py`, `floating_platform_stability.py`, `hull_form.py`, `holtrop_mennen.py`, `propeller.py`, `resistance.py`, `seakeeping.py`, etc.).

Two interpretations: (a) the refactor already substantially happened and only `curves_of_form.py` remains as the residual facade — verify this and close as essentially complete; (b) the source-tree `curves.py` was deleted out of band and the issue body is now stale — restore from history, then split. Plan opens with verification and branches.

## Plan

1. **Verify the monolith's git history.** `git log --all --diff-filter=AD --follow -- src/digitalmodel/naval_architecture/curves.py` and `git log --all -- '**/naval_architecture/curves.py'`. Capture: when was a 29k-line `curves.py` last on `main`? Was it deleted, renamed, or split? Compare list of symbols in `build/lib/digitalmodel/naval_architecture/curves.py` against the union of symbols in current `src/digitalmodel/naval_architecture/*.py`. If the union covers the 29k file's public API, the refactor is already done — escalate to user with evidence and propose closing as done.

2. **If the file genuinely exists somewhere git-tracked, restore it on a refactor branch.** Use `git checkout <last-good-sha> -- src/digitalmodel/naval_architecture/curves.py`. Confirm tests on that SHA were green so we have a known-good baseline.

3. **Plan the seven-target split (TDD, per issue body).** For each target file, draft the symbol list before moving anything:
   - `curves/hull_geometry.py` (offsets, stations, hull-form definitions)
   - `curves/hydrostatics.py` (displacement, centers, buoyancy)
   - `curves/stability.py` (GM, KG, righting-arm curves)
   - `curves/weight_estimation.py` (lightweight, deadweight)
   - `curves/capabilities.py` (cross curves, KN values)
   - `curves/io.py` (GHS, MaxSurf, OrcaWave file parsers)
   - `curves/__init__.py` (backward-compat re-exports — every symbol in the original public API is re-exported here)

4. **Move in seven small commits, one target per commit, tests green between each.** For each target: write or extend its tests (in `tests/naval_architecture/test_curves_<target>.py`) **before** moving symbols. Move symbols, run `uv run pytest tests/naval_architecture/ -x`, commit. After all seven, the original `curves.py` becomes a <100-line facade that simply does `from .curves import *` for the seven submodules (preserving downstream `from digitalmodel.naval_architecture.curves import X`).

5. **Verification + cross-review.** Each new module must be <500 lines. Run `wc -l src/digitalmodel/naval_architecture/curves/*.py`. Run the full naval-architecture test suite plus an import-smoke: `uv run python -c "from digitalmodel.naval_architecture.curves import *"`. Then queue Codex + Gemini cross-review per repo policy (Phase 2 Tier-1 work requires it).

## Acceptance Criteria

- [ ] Verification step (Plan 1) is documented in the PR with explicit answer: monolith location, git history, and refactor strategy chosen
- [ ] If refactor proceeds: each new module under `src/digitalmodel/naval_architecture/curves/` is <500 lines
- [ ] If refactor proceeds: facade `curves.py` is <100 lines (re-exports only); no breaking import path changes
- [ ] If refactor proceeds: every existing test in `tests/naval_architecture/` passes after each of the seven commits
- [ ] Codex + Gemini cross-review completed per Phase 2 Tier-1 policy
- [ ] If verification shows the work is already done: issue is closed with the evidence rather than executing a no-op refactor

## Open questions

1. **Blocker until clarified:** Is the 29k `curves.py` actually expected to live in `src/`, or is the issue tracking against a now-stale build artifact? Plan-review approval should confirm before executor proceeds.
2. If split must happen, do downstream consumers expect `from .curves import X` (current name) or `from .curves.<module> import X` (new path)? Default: preserve top-level imports via the facade.
