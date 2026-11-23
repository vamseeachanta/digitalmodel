## Verdict
MAJOR

## Retrieval
- Read `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md:1-460`.
- Read `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py:120-140,203-230,250-321,331-380,434-527,533-737`; ran `wc -l` and targeted `rg` searches for interpolation, sorting, frequency equality, and rotational-unit handling.
- Read `scripts/benchmark/run_3way_benchmark.py:420-445` and `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py:60-82,138-156,428-474`.
- Read `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py:1-417` and `test_multi_solver_units_guard.py:1-112`; `rg -c '^\\s*def test_'` returned 22.
- Read `orcawave_001_ship_raos_rev2.yml:1-90`, `001_SHIP_RAOS_REV2.LIS:32340-32772,33298-33365`, and inspected the workbook’s `Displacement RAOs` sheet.
- Ran `stat -c '%n %s bytes'` on the three ship inputs and co-located unit-box artifacts.
- Read `L00_validation_wamit/validation_config.yaml:1-150`; counted statuses and enumerated every `reference_data.yaml`.
- Read `benchmark_report.json` and `benchmark_summary.json:1-31`; counted the matrix correlations and checked report fields.
- Computed both frequency-domain ranges and their intersection from the stated grids; also attempted raw NumPy subtraction of `(20,9)` and `(10,9)` arrays.
- Checked every cited/new artifact path with `test -e`, and searched for the proposed symbols.
- Ran `git status --short --branch`, `git worktree list --porcelain`, and the final cleanup-state audit; the review worktree remained clean.

## Findings
1. The proposed insufficient-overlap test is mathematically false for the real grids. The plan equates “three coincident knots” with insufficient domain overlap and requires `AbscissaOverlapError` (`docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md:257-265,323-326`). Recalculation from the listed periods gives AQWA `0.285599–2.252038 rad/s` and OrcaWave `0.285599–3.141593 rad/s`: the intersection covers 100% of the smaller domain and 68.853% of the union span. Interpolation does not require coincident knots. This test would reject the exact valid resampling case the contract is meant to support.

2. The plan has not demonstrated that the comparator processed the raw 10/20-point grids. `_calculate_deviation_stats` subtracts arrays directly (`multi_solver_comparator.py:203-220`), and NumPy reports `ValueError: operands could not be broadcast together with shapes (20,9) (10,9)`. Therefore the committed report could only have arisen after an unrecorded transformation or from different inputs. The plan acknowledges unknown provenance at lines 83-96, but still states that correlations “are computed without an abscissa contract” and calls this the root cause at lines 21-24 and 419. That causal claim is not established before Phase 0.

3. The ordering contract contradicts itself. It says descending inputs must raise and “no code path silently sorts” (`docs/plans/...:323,382`), while its loader deliberately calls `argsort` and its dedicated test requires sorting (`docs/plans/...:267-270,327`). The implementation cannot satisfy both requirements. The plan must define whether ordering normalization belongs at ingestion or whether all descending source data is rejected.

4. The synthetic `−0.3961/+1.0000` reproduction is not independently reproducible from the plan or tree. The plan supplies four output samples but no function or complete synthetic series (`docs/plans/...:62-80`), then makes `corr ≈ −0.40` a regression-test oracle (`docs/plans/...:324`). The real grids alone do not determine those correlations. A checked-in fixture or complete formula and tolerance is required.

5. Two exact line citations are inaccurate. In `run_3way_benchmark.py`, frequencies are reordered at line 436 but `raw_raos` is reordered at line 437, outside the cited `433-436` range. Separately, the files table says circular phase handling occurs at “both 293 and 316” (`docs/plans/...:306`); line 293 merely passes frequency metadata. The independent linear phase subtraction is line 297, with its maximum consumed at line 316 (`multi_solver_comparator.py:290-316`).

6. The L00 blocked-case citation is wrong. The counts are correctly 11 pass and 1 blocked, and four `reference_data.yaml` files exist, but line 51 blocks case `2.4`, not case `2.2` (`L00_validation_wamit/validation_config.yaml:20-27,44-54`; plan lines 169-173). This is explicitly presented as “all verified.”

7. Several cited evidence/governance paths do not exist in this checkout: `scripts/review/results/2026-08-03-plan-1633-claude.md`, `.claude/rules/calc-citation-contract.md`, `.claude/rules/wiki-sibling-routing.md`, `.claude/rules/licensed-solver-dispatch.md`, `scripts/legal/legal-sanity-scan.sh`, `config/deckhand/policy.yml`, and `queue/heartbeat/ace-win-2.json`. The plan claims some were consulted (`docs/plans/...:121-140`) and requires the missing legal script to pass (`:390`). External-repository evidence needs an explicit repository, commit, and resolvable path; the legal acceptance command is currently unexecutable here.

8. The AQWA version description is incomplete. The file says “generated by Aqwa in Workbench 2022 R2” at `001_SHIP_RAOS_REV2.LIS:4`, but identifies `Aqwa-Line 2024 R2` at line 36. Calling it simply “Aqwa Workbench 2022 R2” (`docs/plans/...:114`) obscures that the generating Workbench and solver versions differ.

9. The claimed uncertainty about the workbook is contradicted by the artifact. The `Displacement RAOs` sheet has 189 rows covering headings 0° through 180°, all 20 periods, six amplitudes, and six phases; units appear in row 9. `rao_extractor.py:251-343` explicitly supports native OrcaWave displacement-RAO sheets and sorts the extracted frequency/RAO arrays together. The risk at `docs/plans/...:440-442` and Phase-0 reader uncertainty at `:355-359` should be resolved now rather than retained as a possible Phase-3 licence dependency.

10. Threshold phasing is internally inconsistent. The introduction says thresholds are set in Phase 3 (`docs/plans/...:21-25`), while Phase 2 and its acceptance criterion say measured values and thresholds are set in Phase 2 (`:355-359,386`). This changes whether threshold selection depends on a licensed rerun.

11. The human-facing report path does not exist and is omitted from “Files to Change.” It appears in the Artifact Map (`docs/plans/...:239`) but has no creation step at lines 301-315 and no acceptance criterion. The plan therefore does not execute or verify one of its declared artifacts.

12. The remaining assigned numeric claims were accurate: `multi_solver_comparator.py` is 737 lines; the test file has 22 test definitions; ship input sizes are 3,391,338, 2,340,355, and 4,127 bytes; the YAML has the stated 20-period grid; the LIS RAO table has the stated 10 displayed periods; line 33311 contains heave `0.9224` at 22 s; the report’s six DOF statistics and all 72 matrix correlations match; and comparator lines 209, 217-218, 264-268, 275-288, 297, 316, 469-471, and 483-490 otherwise support the plan’s descriptions.

## Blockers
- Finding 1: Replace coincident-point counting with a defined domain-overlap metric and correct the real-grid test to permit interpolation over the shared domain.
- Finding 2: Complete Phase 0 before naming the root cause; identify the producing script and exact pre-comparison array shapes/order, then revise attribution accordingly.
- Finding 3: Choose one ordering policy—loader normalization or fail-closed rejection—and make pseudocode, tests, and acceptance criteria consistent.
- Finding 4: Commit a complete deterministic synthetic fixture or specify the full generating formula and tolerances.
- Finding 7: Supply resolvable repository/commit/path citations for external evidence and replace the missing legal-scan command with an executable checkout-relative route.
- Finding 10: State unambiguously whether thresholds are selected in Phase 2 or Phase 3 and update every dependent gate.
- Finding 11: Add the HTML report to Files to Change with an acceptance/verification step, or remove it from the Artifact Map.
