## Verdict
MAJOR

## Retrieval
- Read `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md:31-116,244-391,434-451`.
- Read `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py:121-230,240-325,434-527,553-606,633-737`.
- Read `src/digitalmodel/hydrodynamics/diffraction/comparison_framework.py:32-41`.
- Read `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py:28-417` and `tests/hydrodynamics/diffraction/conftest.py:343-418`; confirmed 22 tests with `git show origin/main:... | rg -c 'def test_'`.
- Read `scripts/benchmark/run_3way_benchmark.py:1100-1239,1305-1373`, `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py:128-200`, and `scripts/run_benchmark_ship_raos.py:42-157,176-223,227-353`.
- Read the real grids at `orcawave_001_ship_raos_rev2.yml:36-59` and AQWA oracle block at `001_SHIP_RAOS_REV2.LIS:33304-33320`.
- Reimplemented `scripts/run_benchmark_ship_raos.py:259-320` against the committed CSVs. It reproduced every reported statistic, including heave correlation `-0.8551433929687695` and RMS `0.698114117150058`.
- Interpolated the committed OrcaWave heave CSV correctly onto the AQWA grid. Result: correlation `0.867534752196191`, relative RMS `0.5152775016612859`.
- Calculated grid overlap: AQWA range is wholly inside the OrcaWave range; overlap/AQWA-span is `1.0`, despite only three exact rounded intersections.
- Counted `20,241` test definitions under `tests/`; `test_cli_integration.py` contains 12.
- Attempted `PYTHONPATH=src uv run python -m pytest tests/hydrodynamics/diffraction/test_multi_solver_comparator.py -q`; collection could not run because the configured sibling `../assetutilities` is absent. System Python also lacked SciPy.

## Findings
1. The D0 tests specify mutually exclusive outcomes for the same real grids. `test_insufficient_overlap_raises` requires the AQWA/OrcaWave grids to raise because only 3 of 20 values coincide, while `test_common_grid_is_ascending_and_within_both` and the L01 oracle require those grids to be aligned successfully (`plan:325-326,338`). The proposed overlap formula measures range overlap (`plan:261-263`), and the live grids have 100% overlap relative to AQWA’s span. Exact-point intersection is not overlap when interpolation is allowed. Both tests cannot pass under the pseudocode. The additional union grid (`plan:264`) also weights the verdict toward the denser solver and lacks a numerical interpolation oracle.

2. The proposed external-oracle expectation is contradicted by the committed data and cannot pass solely after the planned fix. Correct ascending interpolation of the committed CSV heave data produced `corr=0.8675`, `rel_rms=0.5153`, not `corr>0.95` and `rel_rms<0.10` (`plan:338`). The plan explicitly allows the disagreement to be real (`plan:436-439`), yet the test requires agreement. This tests a desired physical outcome, not correct comparison behavior, and makes a defensible `fail` verdict impossible to accept.

3. The claimed provenance uncertainty and abscissa attribution are already falsified by the tree. `scripts/run_benchmark_ship_raos.py:176-194` creates identical placeholder added-mass and damping matrices, explaining all 72 correlations of `1.0`; lines 259-320 reduce both solvers to the three approximately common, ascending frequencies. Replaying that code reproduced every committed RAO statistic exactly. Therefore the `-0.8551` artifact was not produced by raw 10-versus-20 opposite-order subtraction. Phase 0 says no fix will be chosen before attribution (`plan:346-350`), but D0–D5 and the oracle expectation have already been chosen around a different mechanism.

4. Several TDD rows are green, vacuous, or permit broken implementations:
   - `test_np_interp_descending_xp_is_wrong` asserts NumPy’s existing behavior and will pass before and after production code changes (`plan:324`).
   - `test_loader_sorts_freqs_and_raos_together` duplicates existing sorting at `run_3way_benchmark.py:434-436`; it is not necessarily red on `origin/main`.
   - `test_agreement_scale_is_symmetric` can pass when both orders return the same wrong verdict; current correlation and absolute RMS are already symmetric (`plan:329`, comparator `209-220`).
   - `test_two_solver_ladder_has_no_majority_rung` repeats the rejected disjunction pattern: a disagreeing input may incorrectly return `FULL` and still pass (`plan:330`).
   - `test_three_solver_ladder_unchanged` is a characterization test, and “3 solvers, 2 agreeing” does not specify whether two solvers or two pairwise edges agree (`plan:331`).
   - `test_common_grid_is_ascending_and_within_both` can pass if RAOs are permuted independently of their frequencies (`plan:326`).
   - `test_max_phase_diff_is_circular` accepts any value ≤180, including a broken constant-zero result (`plan:336`).
   - `test_aqwa_lis_parser_matches_hand_checked_block` is useful regression coverage but exercises an existing parser, so it is not necessarily red today (`plan:339`).
   Exact expected values, production entry points, and explicit red-state reasons are missing.

5. The empty-array test is not implementable from the supplied pseudocode. Guarding `_calculate_deviation_stats` does not prevent `np.max(avg_mag)`, `np.argmax(phase_diff)`, and later `np.max` calls in `multi_solver_comparator.py:273,301-316`. JSON serialization also executes `float(correlation)` at lines 677-684, which rejects the planned `None`. `test_empty_arrays_report_insufficient_data` (`plan:332`) needs an end-to-end report/export oracle and a defined representation for unavailable extrema, errors, and correlations.

6. Phase and unit tests do not cover the real failure modes. The plan wraps differences only after interpolation (`plan:288-290`), but phase interpolation across `179°/-179°` must unwrap or use complex interpolation first; the current raw linear interpolation is at `run_3way_benchmark.py:1182-1188`. Neither phase test detects branch-cut interpolation. The unit test covers only explicit pitch `deg/m` versus `rad/m` (`plan:337`), while the actual CSV loader emits `unit=""` (`scripts/run_benchmark_ship_raos.py:157`) and the AQWA extraction path also emits blank units (`run_3way_benchmark.py:642`). Unknown units, roll, and yaw can therefore bypass the proposed guard.

7. The quality/status data model and compatibility impact on the 22 existing tests are unspecified. `DeviationStatistics` has no `quality` field (`comparison_framework.py:32-41`), that file is absent from the Files to Change table, and `derive_status` refers to a singular per-DOF quality although reports contain pairwise DOF comparisons. Static inspection shows the existing fixtures all use the same increasing grid (`conftest.py:357-375`), so the abscissa contract should not require re-baselining them. Likely compatibility breaks instead include the hard-coded default tolerance assertion (`test_multi_solver_comparator.py:66-84`) if Phase 2 changes it and report-enum assertions at lines 350-369 if new statuses leak into `BenchmarkReport`. “Re-baseline the other 21” (`plan:312`) does not state which expectations change or prevent weakening tests to accommodate implementation.

8. Acceptance is neither regression-safe nor methodologically defensible. The threshold of ≥2,121 passes (`plan:381`) is vacuous against 20,241 discovered test functions and could tolerate losing most of the suite. No command is specified for obtaining those counts, while the preceding command runs only the diffraction directory (`plan:380`). Allowing ≤12 failures does not prove they are exactly the pre-existing CLI failures. Finally, setting `CORR_MIN` and tolerance from the same Phase 2 observations being judged (`plan:386`) is circular calibration; almost any observed result can be made to pass. Thresholds need independent engineering requirements or a separately frozen calibration/validation split.

## Blockers
- Findings 1 and 4: define one coherent overlap policy, an independent target-grid/weighting rule, and exact analytic interpolation tests. Replace every membership/disjunctive or inequality-only oracle with the one expected outcome and verify each behavioral test fails on `origin/main`.
- Findings 2 and 3: rewrite Phase 0 around the verified producer path and separate “comparison correctness” from “solver agreement.” The ship test must accept a correctly derived `fail` verdict when the committed solvers genuinely disagree.
- Finding 5: specify and test complete empty-data propagation through comparison, consensus, JSON serialization, and status derivation.
- Finding 6: require phase unwrapping or complex interpolation tests across a branch cut, plus fail-closed tests for blank/unknown units and all three rotational DOFs.
- Finding 7: define where quality lives, how multiple pair qualities aggregate into a DOF/report status, and enumerate exact expected changes to the 22 existing tests.
- Finding 8: replace fitted thresholds with independently justified criteria and use an explicit full-suite command with exact allowed failure node IDs or a verified baseline comparison.
