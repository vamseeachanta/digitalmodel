# Plan: digitalmodel #457 — Ship dimensions template + loader (WRK-1380)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/457
**Status:** plan-review
**Tier:** T2 (largely landed; close-out + final-plan compliance)
**Stage in WRK-1380:** Cross-Review (in_progress per body)

## Context

#457 delivered the **code-side** of WRK-1380:
- `data/ship_dimensions_template.yaml` (111 lines, 10 hull entries — verified present)
- `src/digitalmodel/naval_architecture/ship_dimensions.py` (204 lines): `default_dimension_template_path`, `load_dimension_template`, `validate_vessel_entry`, `merge_template_into_registry`, `_resolve_hull_id`, `_normalize_registry_entry`, `_is_number`, `_is_positive_number` (verified)
- Naval-architecture package exports — extends `__init__.py`
- Regression tests for default loading, alias handling, merge normalization (executor confirms via `grep -l "test_ship_dimensions" tests/`)

The Final Plan in the issue body lists three open dependencies:
1. Confirm `generate-ship-dimension-template.py` location → handled in #461
2. Confirm canonical `ship-dimensions.yaml` path → handled in #461
3. Confirm execution location (workspace-hub vs digitalmodel) → answer is **digitalmodel** (loader + data live here per the landed code)

The plan also lists two scripts to create:
- `scripts/ship-dimensions/build-priority-queue.py` — Stage 6
- `scripts/ship-dimensions/validate-phase1.py` — Stage 6

These are **NOT yet present** (confirmed: `find scripts/ship-dimensions -type f 2>/dev/null` → empty). They're the WRK Stage-6 deliverable.

This issue closes when (a) the two scripts land with their tests, (b) cross-review (Stage 6) signs off, (c) the user reviews the plan-final delivery (Stage 7) per the WRK gate sequence in the body.

## Plan

1. **Confirm the existing landed surface still validates.** Run `uv run pytest tests/naval_architecture/test_ship_dimensions.py -v` (or equivalent path — executor confirms via `find tests -name "test_ship_dimensions*"`). All existing tests must remain green before Stage-6 scripts land. If any are red, fix them as part of this PR.

2. **Author `scripts/ship-dimensions/build-priority-queue.py`.** Per the Final Plan pseudocode:
   ```python
   def build_queue(template_path):
       entries = load_dimension_template(template_path)
       priority = {"BB": 1, "CV": 1, "CA": 1, "CB": 1, "DD": 2}
       def class_of(hull_id):
           m = re.match(r"([A-Z]+)-?\d+", hull_id)
           return m.group(1) if m else "?"
       def rank(e):
           return (priority.get(class_of(e["hull_id"]), 3),
                   "complete" if all(e.get(k) for k in REQUIRED) else "incomplete",
                   not e.get("entry_status") == "verified")
       return sorted(entries, key=rank)
   ```
   Output: `data/ship_dimensions_extraction_queue.yaml` (ordered list of hull-ids, completion state, verification flag). CLI: `uv run python scripts/ship-dimensions/build-priority-queue.py [--out queue.yaml]`.

3. **Author `scripts/ship-dimensions/validate-phase1.py`.** Per the Final Plan:
   - Load `ship-dimensions.yaml`
   - Count entries with all six required fields → must be ≥ 30
   - Check every capital-ship hull-id complete
   - Count `entry_status: verified` → must be ≥ 5
   - Exit 0 on pass, non-zero with reason on fail
   The script consumes the digitalmodel loader (`from digitalmodel.naval_architecture.ship_dimensions import load_dimension_template, validate_vessel_entry`).

4. **Tests.** Per Final Plan's test-table:
   - `tests/scripts/ship_dimensions/test_validate_phase1.py::test_validate_phase1_happy_path` — passes ≥ 30 complete + all capital ships + ≥ 5 verified
   - `test_validate_phase1_threshold_failure` — fails and reports unmet threshold
   - `tests/scripts/ship_dimensions/test_build_priority_queue.py::test_build_priority_queue_orders_classes` — BB/CV/CA first, DD second, auxiliaries last
   Test fixtures: synthetic 35-vessel YAML in tests/fixtures (5 capital, 10 destroyers, 20 auxiliaries, 6 marked verified).

5. **Cross-review (Stage 6).** Per WRK gate sequence in the issue body, this is the current open stage. After scripts land + tests green, dispatch cross-review per `.claude/rules/` (Codex + Gemini if available). Capture findings in a `docs/reviews/wrk-1380-cross-review.md`.

6. **Stage 7 user review.** This step is **user-gated**, not agent-self-approved. Plan stops here for executor: implementation done, cross-review filed, awaiting `status:plan-approved` from the user. **Do not self-label.**

7. **Smoke check.** `uv run python scripts/ship-dimensions/build-priority-queue.py --out /tmp/queue.yaml && uv run python scripts/ship-dimensions/validate-phase1.py data/ship_dimensions_template.yaml` — first command produces queue, second exits non-zero (current data has 0 verified, well under threshold). The non-zero exit is **expected** at this stage and is the bridge to #463.

## Acceptance Criteria

- [ ] Existing `ship_dimensions.py` tests still green
- [ ] `scripts/ship-dimensions/build-priority-queue.py` exists with CLI + module-level functions
- [ ] `scripts/ship-dimensions/validate-phase1.py` exists with CLI + exit codes
- [ ] All three test cases from the Final Plan land and pass
- [ ] Cross-review (Stage 6) artifact filed at `docs/reviews/wrk-1380-cross-review.md`
- [ ] No data edits to `data/ship_dimensions_template.yaml` — that's #463's scope
- [ ] No self-approval; awaiting user review per Stage 7

## Open Questions

- Are the WRK-stage labels (`status:plan-review`, etc.) the same as the WRK-1380 stage tracker in the issue body? Confirm with batch-agent: this plan adopts the GitHub-label gate (`status:plan-review` → user → `status:plan-approved`) as authoritative.
- Cross-review tooling: per memory, `scripts/peer-review/cross-review.sh` may not be invokable from a planning-only session. Default to a single-author review with transparent provenance per the `feedback_permission_gate_blocks_cross_review` precedent.
