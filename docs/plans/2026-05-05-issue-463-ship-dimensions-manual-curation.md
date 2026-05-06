# Plan: digitalmodel #463 — Populate ship-dimensions dataset from ship plans (manual curation follow-on)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/463
**Status:** plan-approved
**Tier:** T2 (data population + verification)
**Blocked by:** #461 (template artifact recovery)
**Predecessor:** #457 (code-side foundation — DONE)
**User directive (2026-05-06):** Evaluate both **Codex** and **Claude** models for the ship-plan extraction step. The data-extraction approach should A/B-test the two providers on a representative ship-plan PDF before committing to one. Capture comparison criteria (extraction accuracy, dimension-field coverage, hallucination rate, cost-per-doc) in a session note before scaling to the full dataset.

## Context

`#457` delivered the code path: `default_dimension_template_path()`, `load_dimension_template()`, `validate_vessel_entry()`, `merge_template_into_registry()`, and the registry-merge normalization in `src/digitalmodel/naval_architecture/ship_dimensions.py` (204 lines, 9 public functions). Regression tests cover default loading, alias handling, merge normalization.

`data/ship_dimensions_template.yaml` (111 lines) carries 10 hull entries (BB-42, BB-35, CV-60, plus 7 more) but **none have `entry_status: verified`** (audit: `grep -c "entry_status: verified" data/ship_dimensions_template.yaml` → 0). The original WRK-1380 mission targets 110 SNAME ship plans with prioritization BB/CV/CA → DD → auxiliaries, with at least 5 cross-referenced against Jane's.

The blocker call-out is correct: until #461 confirms the canonical template artifact source (`generate-ship-dimension-template.py` per WRK-1339 Child E), this issue can't safely commit data — the schema may shift if the upstream generator does.

## Plan

1. **Wait on #461 unblock.** Do not edit `data/ship_dimensions_template.yaml` until #461 lands a decision: either the canonical generator is recovered (and may regenerate the template, in which case existing 10 entries must be re-validated) or replaced explicitly. Track #461's status. If #461 closes with "the existing `data/ship_dimensions_template.yaml` IS the canonical artifact," skip the regeneration step and proceed.

2. **Confirm priority queue tooling.** Per the WRK-1380 Final Plan in #457's body, two scripts are slated:
   - `scripts/ship-dimensions/build-priority-queue.py` — orders extraction work BB/CV/CA → DD → auxiliaries
   - `scripts/ship-dimensions/validate-phase1.py` — deterministic Phase 1 thresholds (≥30 complete, all capital ships, ≥5 verified)
   These may not exist yet — `find scripts/ship-dimensions -type f`. If absent, this issue's PR includes them.

3. **Build extraction queue.** Run (or write then run) `scripts/ship-dimensions/build-priority-queue.py` against `data/ship_dimensions_template.yaml` to emit `data/ship_dimensions_extraction_queue.yaml` ordered by class priority. The queue is committed alongside data updates so reviewers see extraction order.

4. **Populate dimensions.** For each vessel in the queue, open the corresponding ship-plan PDF (location: `client_projects/sname-ship-plans/` or per #461's resolved path), read off LOA / beam / draft / depth / displacement / speed, write into the YAML under the existing schema. **Do not invent missing values** — leave blank (per #457's WRK-1380 Final Plan: "leaving unknown fields blank rather than inventing values"). Add `source: "USNA ship plan: <pdf-name>"` per entry.

5. **Cross-reference against Jane's.** For ≥ 5 vessels, look up the entry in Jane's Fighting Ships 2009-2010 (manifest path resolved via #461). When dimensions match within published tolerance, set `entry_status: verified` and add `verification_source: "Jane's Fighting Ships 2009-2010, p. <n>"`.

6. **Run Phase 1 validator.** `uv run python scripts/ship-dimensions/validate-phase1.py data/ship_dimensions_template.yaml` — must report ≥ 30 complete, all capital ships complete, ≥ 5 verified. If it fails, return to step 4.

7. **Tests.**
   - `tests/scripts/ship_dimensions/test_build_priority_queue.py::test_classes_ordered_correctly`
   - `tests/scripts/ship_dimensions/test_validate_phase1.py::test_passes_when_thresholds_met`
   - `tests/scripts/ship_dimensions/test_validate_phase1.py::test_fails_when_capital_ship_incomplete`
   These guard the validation path against regression even when the data file changes.

8. **Smoke check.** `uv run python -c "from digitalmodel.naval_architecture.ship_dimensions import load_dimension_template, validate_vessel_entry; entries = load_dimension_template(); errs = [(e['hull_id'], validate_vessel_entry(e)) for e in entries]; print({k: v for k, v in errs if v})"` — surface remaining validation issues; should be near-empty.

## Acceptance Criteria

- [ ] #461 closed (or its decision adopted) before any data edit lands
- [ ] `build-priority-queue.py` and `validate-phase1.py` exist and have unit tests
- [ ] `data/ship_dimensions_template.yaml` carries ≥ 30 entries with all six required dimensions populated
- [ ] All capital-ship hull-ids (BB, CV, CA, CB classes) are complete
- [ ] ≥ 5 entries have `entry_status: verified` with `verification_source` referencing Jane's
- [ ] No invented numeric values (`grep -nE "loa_ft: 0\.0|beam_ft: 0\.0" data/ship_dimensions_template.yaml` is empty)
- [ ] `validate-phase1.py` exits 0 on the populated dataset

## Open Questions

- Where do the SNAME ship-plan PDFs live? Likely `client_projects/sname-ship-plans/` but #461 must confirm. Default: pause data work until path is locked.
- "Speed" field: design speed vs trial speed vs sustained speed — Jane's typically reports max speed; ship plans typically report design speed. Document the convention chosen at the top of the YAML.
