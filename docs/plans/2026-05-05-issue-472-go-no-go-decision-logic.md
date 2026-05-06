# Plan: digitalmodel #472 — Implement Go/No-Go decision logic per DNV-RP-H103

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/472
**Status:** plan-review
**Tier:** T2 (extend existing module)
**Parent:** #471

## Context

`src/digitalmodel/marine_ops/installation/go_no_go.py` (360 lines) already exists with `DecisionState`, `CriterionState`, `CriterionResult`, `GoNoGoDecision`, `evaluate_go_no_go()`, and `print_decision()`. The signature accepts `crane_swl_uc_limit`, `sling_wll_margin_limit`, `daf_min`, `daf_max`, `min_bend_radius_factor`, `total_lift_capacity_margin` — covers 6 of the 6 criteria the issue lists. The pipeline (`marine_ops/installation/jumper_installation.py`) already wires `run_go_no_go: bool = True` and pulls `GoNoGoDecision` into `PipelineOutput`.

Gap analysis vs the issue's 6-criterion table:
1. Crane SWL UC < 1.0 — covered (`crane_swl_uc_limit`)
2. DAF 1.1-1.3 — covered (`daf_min`/`daf_max`)
3. Sling WLL ratio > 1.5 — covered (`sling_wll_margin_limit`)
4. **Splash zone Hs < 1.0 m — NOT in current criteria args**, must be added
5. Min bend radius ≥ 50×OD — covered (`min_bend_radius_factor`)
6. Total lift < vessel capacity — covered (`total_lift_capacity_margin`)

So the work is (a) add Hs splash-zone criterion, (b) ensure `evaluate_go_no_go` reads the right keys from `run_jumper_analysis()` output, (c) make MARGINAL/GO/NO_GO state aggregation correct (issue says "All criteria must pass for GO" — current `_check_criterion` allows WARNING which currently feeds MARGINAL; need to confirm the aggregator), (d) integration test against both jumper configs.

## Plan

1. **Audit current criteria-to-results mapping.** Read `evaluate_go_no_go` in full (`src/digitalmodel/marine_ops/installation/go_no_go.py`). Verify for each criterion:
   - Does it pull the right key from the `results: Dict` parameter?
   - Is the inequality direction (above_is_safe) correct?
   - What happens when the input key is missing? (Should be FAIL with a "missing data" reason, not silent skip.)
   Capture findings as inline comments on the `evaluate_go_no_go` body.

2. **Add splash-zone Hs criterion.** Extend `evaluate_go_no_go` signature with `splash_zone_hs_max_m: float = 1.0` keyword. Compute from `results["splash_zone_hs_m"]` (key the executor will define in `jumper_lift.run_jumper_analysis` step 3). Cite DNV-ST-N001 §X (executor confirms exact clause from the standards page).

3. **Surface splash-zone Hs from jumper analysis.** Edit `marine_ops/installation/jumper_lift.py` `run_jumper_analysis()` to consume `marine_ops/installation/splash_zone.py:splash_zone_assessment` and populate `results["splash_zone_hs_m"]`. The splash-zone module already produces `SplashZoneResult` with an Hs limit — wire it; do not re-derive.

4. **Fix aggregation: any FAIL → NO_GO; any WARNING → MARGINAL; all PASS → GO.** Re-read the aggregator block in `evaluate_go_no_go`. The issue states "All criteria must pass for GO" — confirm WARNING is not silently aggregated to GO. Add a unit test that locks this behavior.

5. **Pipeline wiring confirmation.** Edit `marine_ops/installation/jumper_installation.py` `_run_go_no_go` to forward `splash_zone_hs_max_m` from `PipelineConfig.go_no_go_criteria` if provided. No structural pipeline change otherwise.

6. **Tests.** Extend `tests/marine_ops/installation/test_go_no_go.py`:
   - `test_aggregator_all_pass_returns_go`
   - `test_aggregator_one_warning_returns_marginal`
   - `test_aggregator_one_fail_returns_no_go`
   - `test_splash_zone_hs_above_limit_fails`
   - `test_evaluate_go_no_go_against_mf_plet_real_results` (integration, fixture-loaded)
   - `test_evaluate_go_no_go_against_plet_plem_real_results`

7. **Smoke check.** `uv run pytest tests/marine_ops/installation/test_go_no_go.py -v` and `uv run python -c "from digitalmodel.marine_ops.installation.jumper_installation import run_pipeline; out = run_pipeline('docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml', output_dir='/tmp/g'); print(out.go_no_go.overall_state.value)"`. Should print `GO`, `MARGINAL`, or `NO_GO`.

## Acceptance Criteria

- [ ] All 6 criteria from the issue table evaluated, each with explicit standard reference (DNV-RP-H103 / DNV-ST-N001 / Marine warranty)
- [ ] Splash-zone Hs criterion added and pulled from `splash_zone_assessment`
- [ ] Aggregator: PASS-only → GO; any WARNING → MARGINAL; any FAIL → NO_GO; missing input data → FAIL with reason
- [ ] `tests/marine_ops/installation/test_go_no_go.py` ≥ 6 new tests, all passing
- [ ] Pipeline produces a non-null `GoNoGoDecision` for both Ballymore configs

## Open Questions

- The current `_check_criterion` "warning_factor" semantics (lines 56-78 of `go_no_go.py`) need a comment-level review — particularly the dual `margin = limit - value` reassignment in the `above_is_safe=False` branch. Step 1's audit will surface whether this is a defect or intentional.
