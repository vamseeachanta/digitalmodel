# Plan: digitalmodel #475 — Add pytest test suite for jumper_lift.py (81 tests)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/475
**Status:** plan-review
**Tier:** T2 (test migration)
**Parent:** #471

## Context

The Windows Claude-cowork conversion at `client_projects/engineering_workbooks/ballymore/jumper_manifold_to_plet/claude_excel_addin/test_jumper_lift.py` carries 81 unittest-style tests covering every intermediate value from the source workbook with cell-reference traceability. The existing `tests/marine_ops/installation/test_jumper_lift.py` already has the file (599 lines) with 81 `def test_` matches and pytest imports — preliminary inspection shows the migration may already be partially executed. The work in this issue is to **finalize** the migration: confirm the imported classes resolve under the digitalmodel namespace, ensure pytest fixtures and approx-tolerances replace unittest assertions, fill any classes still missing tests, and confirm both jumper variants are exercised.

Test classes called out in the issue: `TestBarePipe (9)`, `TestConnectorClamp (4)`, `TestPipeGeometry (11)`, `TestBuoyancyModule (7)`, `TestStrakeModule (7)`, `TestRigging (6)`, `TestCraneConfig (4)`, `TestWeightCheck (6)`, `TestPipeline` (end-to-end). Total: 54 explicit + 27 in `TestPipeline` ≈ 81.

## Plan

1. **Diff existing vs source.** Run `diff -u tests/marine_ops/installation/test_jumper_lift.py client_projects/engineering_workbooks/ballymore/jumper_manifold_to_plet/claude_excel_addin/test_jumper_lift.py` (when ws014 is reachable, or fetch the source from the parent excel-conversion issue's attached snapshot). Categorize gaps: missing tests, unittest leftovers, broken imports.

2. **Fix imports.** Confirm every `from digitalmodel.marine_ops.installation.jumper_lift import ...` symbol exists. Cross-check against the actual exports in `src/digitalmodel/marine_ops/installation/jumper_lift.py` — currently provides `INCH_TO_M`, `LB_TO_KG`, `BarePipeProperties`, `BuoyancyModuleProperties`, `ClampProperties`, `ConnectorProperties`, `CraneConfig`, `PipeSectionLengths`, `RiggingProperties` (and 24 functions). Any test-side import of a missing symbol → either add the symbol (small public surface) or rewrite the test to use the existing API.

3. **Convert unittest → pytest.** For any class still using `setUp` / `self.assertAlmostEqual`:
   - replace `setUp` with `setup_method` or `@pytest.fixture(autouse=True)`
   - replace `self.assertAlmostEqual(a, b, places=4)` with `assert a == pytest.approx(b, abs=1e-4)`
   - replace `self.assertTrue(x)` with `assert x`
   - keep class-based grouping; pytest collects classes named `Test*` natively

4. **Add both-variant coverage.** For every test asserting on a `JumperConfig`, add a parametrize over both `KNOWN_JUMPER_CONFIGS["ballymore_mf_plet"]` and `KNOWN_JUMPER_CONFIGS["ballymore_plet_plem"]` where the test is geometry-agnostic. Where the test asserts MF-PLET-specific numbers (e.g. total length 71.64 m), keep MF-PLET only and mark with `@pytest.mark.mf_plet_specific` (register marker in `tests/marine_ops/installation/conftest.py`).

5. **Add `generate_orcaflex_line_sections_yaml` test.** Issue calls out this function explicitly. Add `test_orcaflex_line_sections_yaml_27_sections`, `test_orcaflex_line_sections_yaml_for_plet_plem_distinct_lengths`, `test_orcaflex_yaml_round_trip_via_yaml_safe_load`.

6. **Add pipeline integration test.** `test_pipeline_mf_plet_end_to_end`:
   ```python
   def test_pipeline_runs_mf_plet_end_to_end(tmp_path):
       from digitalmodel.marine_ops.installation.jumper_installation import run_pipeline, PipelineConfig
       cfg = PipelineConfig(spec_path="docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml",
                            output_dir=str(tmp_path), run_orcaflex=False, generate_report=False)
       out = run_pipeline(cfg)
       assert out.jumper_analysis is not None
       assert out.go_no_go is not None
   ```

7. **Smoke check.** `uv run python -m pytest tests/marine_ops/installation/test_jumper_lift.py -v` — must report 81+ collected, 0 errors, 0 unintended failures (xfail/skip permitted only for ws014-only tests, marker `pytest.mark.windows_only`).

## Acceptance Criteria

- [ ] `tests/marine_ops/installation/test_jumper_lift.py` collects ≥ 81 tests with no import errors
- [ ] All tests use pytest idioms (no `setUp`, `assertEqual`, `assertAlmostEqual`)
- [ ] Both `ballymore_mf_plet` and `ballymore_plet_plem` are exercised (parametrize where applicable)
- [ ] `generate_orcaflex_line_sections_yaml()` has dedicated tests covering count + variant-divergence + YAML round-trip
- [ ] Pipeline-integration test passes against the MF-PLET spec
- [ ] `uv run pytest tests/marine_ops/installation/test_jumper_lift.py -v` is green

## Open Questions

- The current 599-line file may already be ahead of the issue. Step 1's diff dictates whether this becomes a 1-hour cleanup or a multi-hour fresh migration. If the file is already 81-passing, this issue is closeable on a delta confirmation (no new code).
