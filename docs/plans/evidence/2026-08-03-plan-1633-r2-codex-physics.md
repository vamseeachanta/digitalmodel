## Verdict
MAJOR

## Retrieval
- Read `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md:31-81,244-265,323-359,379-388,419-442`.
- Read AQWA grid and RAO table in `docs/domains/orcawave/L01_aqwa_benchmark/001_SHIP_RAOS_REV2.LIS:32118-32132,32340-32772,33304-33315`.
- Read OrcaWave grid/model properties in `docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.yml:34-59,79-125`.
- Read matched-model properties in `docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2_matched.yml:36-59,106-132`.
- Read report values in `docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/benchmark_report.json:1-56`.
- Read report-producing workflow in `scripts/run_benchmark_ship_raos.py:42-158,227-353`.
- Read CSV extraction/unit conversions in `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py:25-59,62-110`.
- Read comparator arithmetic in `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py:203-230,240-321`.
- Read existing harmonization implementation in `scripts/benchmark/run_3way_benchmark.py:1100-1239`.
- Ran `git log --follow` and `git show --stat` on the report: commit `19e8eae3` introduced the report, both source CSVs, and `scripts/run_benchmark_ship_raos.py` together.
- Ran a Python reproduction over the committed CSVs. The three-frequency workflow reproduced all six report correlations and RMS errors exactly. Interpolation onto all ten AQWA frequencies produced heave `corr=0.867535, rel_rms=0.5153`, roll `corr=0.089085, rel_rms=0.9604`, and pitch `corr=0.958743, rel_rms=0.9772`.
- Ran `git status --short` and `git diff --check`; worktree was clean.

## Findings
1. The descending-`np.interp` defect did not produce the recorded report. `scripts/run_benchmark_ship_raos.py:278-320` selects three approximately coincident frequencies and performs nearest-frequency filtering, with no `np.interp`. Re-running that arithmetic reproduced the report exactly: heave `−0.855143392968770`, roll `0.489313031455848`, and pitch RMS `26.153448148151465`, matching `benchmark_report.json:29-48`. Git history also introduced the report, CSVs, and this script in one commit. The plan nevertheless calls the abscissa contract the “Root cause” at `docs/plans/...:419`, contradicting its own non-attribution admission at lines 78-81 and 436-439. The proposed lead diagnosis is false for the artifact under review.

2. Refusing these grids as “3-of-20 overlap” is technically wrong and contradicts the pseudocode. AQWA spans `0.28557–2.2521 rad/s` (`001_SHIP_RAOS_REV2.LIS:32125-32132` plus the final two rows reported at `32724,32772`); OrcaWave spans approximately `0.2856–3.1416 rad/s`, from periods `22–2 s` (`orcawave_001_ship_raos_rev2.yml:39-59`). Thus essentially the entire AQWA frequency interval lies inside OrcaWave’s interval. Three coincident nodes are not three-point domain overlap; they merely show different sampling. The range-overlap formula at plan lines 261-264 measures interval overlap, while `test_insufficient_overlap_raises` at line 325 demands rejection based on coincident-node count. These requirements cannot both describe the same contract. This case should interpolate within the shared interval, not raise.

3. `union_within(...)` plus unspecified `interp_on(...)` is not yet a defensible hydrodynamic comparison. Plan lines 257-265 do not specify whether interpolation operates on magnitude, wrapped phase, or the complex transfer function. Interpolating magnitude and phase independently can cross phase discontinuities and mishandle response zeros. Using the union also weights the comparison toward whichever solver supplied more grid points, creating many synthetic AQWA values because OrcaWave has twice as many nodes. Existing code demonstrates these hazards: `run_3way_benchmark.py:1171-1188` separately interpolates magnitude and phase and even permits extrapolation. The contract needs a declared evaluation grid and complex-valued interpolation with no extrapolation before it can claim physically meaningful statistics.

4. Phase 2 names physically mismatched solver models, so interpolation cannot yield a valid solver-verification verdict. The selected OrcaWave YAML has `WaterDepth: 30`, `BodyCentreOfMass: [2.53, 0, -1.974]`, and `BodyMass: 9017.95` (`orcawave_001_ship_raos_rev2.yml:34,103-104`). AQWA records `500 m` water depth and COG near `[108.882, 0.002, 8.5]` (`001_SHIP_RAOS_REV2.LIS:21207,21239`). A matched OrcaWave configuration already exists with `WaterDepth: 500`, COG `[108.88,0.002,8.0]`, and mass `44082.20` (`orcawave_001_ship_raos_rev2_matched.yml:36,106-108`). Yet plan lines 355-359 select the unmatched `.xlsx`/`.yml`. Grid alignment cannot repair different mass, inertia, COG, and depth.

5. The proposed fix leaves every challenged DOF materially unexplained. Full-range magnitude interpolation onto the ten-point AQWA grid changed heave from `−0.855` to `+0.868`, but left `rel_rms=0.515`, far outside the proposed `0.10`. Roll worsened from `0.489` to `0.089` with `rel_rms=0.960`; pitch retained high correlation but `rms=20.839` and `rel_rms=0.977`. The pitch RMS is not explained by descending interpolation or a CSV deg/rad mismatch: both AQWA and OrcaWave rotational amplitudes are converted with `np.degrees` at `create_interactive_rao_comparison.py:45-49,105-109`. These results point to the physical-input mismatch and genuine response differences, not the alleged interpolation root cause.

6. The oracle and threshold criteria are circular and currently unattainable. `test_l01_ship_heave_matches_aqwa_lis` requires `corr > 0.95` and `rel_rms < 0.10` at plan line 338, despite the aligned committed data giving `0.868` and `0.515`. The acceptance criterion at line 386 then proposes setting `CORR_MIN` and tolerance from the same Phase 2 observations being judged. That fits the verdict boundary to the result instead of validating against an independent physical tolerance. It creates pressure either to weaken thresholds or distort interpolation until the test passes.

## Blockers
- Finding 1: Reframe the diagnosis around the verified report-producing CSV workflow. Remove the claim that descending `np.interp` explains the recorded artifact; retain it only as a separate defect in `run_proper_comparison.py`.
- Finding 2: Define overlap using shared interval coverage, minimum supporting samples, and maximum grid gaps—not exact-node coincidence. The real 10/20 grids must be accepted for bounded interpolation.
- Finding 3: Specify and test a fixed evaluation-grid policy, complex-RAO interpolation, phase convention handling, and strict no-extrapolation behavior.
- Finding 4: Replace Phase 2’s unmatched `.xlsx`/YAML with results from the matched 500 m, 44,082.20 t model, or explicitly classify the current comparison as an input-mismatch demonstration rather than solver validation.
- Finding 5: Attribute heave, roll, and pitch separately using matched physical inputs before claiming a corrected hydrodynamic verdict.
- Finding 6: Set tolerances from independent engineering requirements or validation uncertainty before measuring the candidate result. Replace the predetermined passing heave assertion with an externally justified expected value or an explicit disagreement assertion.
