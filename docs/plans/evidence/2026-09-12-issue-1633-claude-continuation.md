# Claude code review — issue 1633 continuation

Reference revision: f5f1bb20. Review tools were read-only. Code-stage verdict: MAJOR. This is a historical finding record, not final clearance.

VERDICT: MAJOR

The three recorded corrections are partially substantiated. Correction 1 (tolerance callers) and Correction 3 (nonfinite scalars) hold as narrowly stated. Correction 2 (summary refusal preservation) does not support the claim made for it: the ALL PASS path retains an independent false-pass channel that the new regression cannot reach, and the same loop carries a latent exception on the ordinary non-refused shape. Plan text at `<canonical-checkout>/docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md` was not readable from this worktree (Read denied on that path; no copy under `docs/plans/` here), so the scope assessment below rests on the evidence document and the code, not on the approved plan wording.

================================================================
BOUNDED CORRECTION FINDINGS
================================================================

BC-1 MAJOR — Handwritten pass still produces ALL PASS via the missing-report fallback.
`scripts/benchmark/validate_owd_vs_spec.py:1997-2023`. When `benchmark_report.json` is absent, the loader parses `r=` out of the free-text `notes` field and, on no match, assigns `corr_val = 1.0` (line 2002) together with a fabricated `max_abs_diff = 1e-3` (line 2017) for all six DOFs. `report_refused` stays False because `_report_has_refusal` is only called inside the `report_path.is_file()` branch (line 1932). Status therefore resolves to `"completed"` (line 2054), which is in the passing set at line 2414; `max_diff` 1e-3 exceeds the 1e-6 skip; `corr` 1.0 is not below 0.999; `all_pass` survives and line 2431 emits ALL PASS. A case marked `status: pass` in `validation_config.yaml` with no benchmark artifact at all yields a green verdict backed by a correlation the code invented. The evidence document states summary loading "prevents a handwritten pass from producing ALL PASS"; that claim is true only for the artifact-present branch. `test_summary_refusal_overrides_handwritten_pass` always writes a report JSON (`write_summary_fixture`, test lines 58-63), so the fallback is untested.

BC-2 MAJOR — The ALL PASS loop raises on the ordinary non-refused shape.
`validate_owd_vs_spec.py:2421-2423` iterates `summary.items()` and indexes `s["correlation"]` for every key. The per-body summary built at lines 1985-1990 is `{**dof_stats, "hydro": pw_hydro, "am_correlations": pw_am, "damp_correlations": pw_damp}`, and the live-run producer at line 953 builds the same shape. `pw_am` is a `{"1,1": …}` correlation map with no `"correlation"` key (KeyError); `pw_hydro` is either the serialized hydrostatic dict, whose keys are `displacement_volume_diff … stiffness_matrix_correlation` (KeyError), or `None` (TypeError). The call site at line 2548 has no exception guard. The regression test does not surface this because its case resolves to `"refused"`, which fails the membership test at line 2414 and `continue`s before the inner loop. Condition for the failure: any case with `status: pass`, a present `benchmark_report.json`, and no nested REFUSED. This was not executed — it is read off the control flow; verification requires running `--summary-only` against a non-refused artifact set.

BC-3 MINOR — `comparison_failed` and `owd_only` count as passing.
`validate_owd_vs_spec.py:2414` and `:2144-2147`. A case whose comparison failed does not clear `all_pass` and is tallied under the "Passed" headline stat. The criterion for the Passed counter is membership in `("completed", "owd_only", "comparison_failed")`, which does not match the label it renders.

BC-4 MINOR — The caller regression's second assertion is tautological.
`tests/hydrodynamics/diffraction/test_review_1633_blockers.py:37` asserts `report.comparison_status == "REFUSED"` after asserting `policy is None` at line 35. `multi_solver_comparator.py:1080-1081` adds `UNCONFIGURED_POLICY` to the refusal set unconditionally whenever `self.policy is None`, so REFUSED is entailed by the preceding line for any input. The load-bearing assertion is `policy is None`; a reintroduced `tolerance=` literal is caught by the `ValueError` from `_build_comparison_policy` (`benchmark_runner.py:247-251`) raising through line 34, not by either assert.

BC-5 MINOR — Two diffraction `BenchmarkConfig` sites sit outside the guard.
`CALLERS` (test lines 14-18) names six scripts covering eight construction sites. Not covered: `scripts/run_benchmark_ship_raos.py:347` and `scripts/benchmark/validate_owd_vs_spec.py:929`. Both were inspected and pass no `tolerance`, so no present defect; the omission is that the script which generates the ALL PASS summary is the one not pinned against tolerance reintroduction.

BC-6 MINOR — The `"dof"` parametrization exercises a key the serializer never emits.
`_serialize_pairwise` (`multi_solver_comparator.py:1239-1258`) writes `refusal_reason` into each `rao_comparisons` entry and never `comparison_status`. The test's `dof` scope injects `comparison_status` into a hand-built DOF dict and passes through the generic recursion in `_report_has_refusal` (line 1868). Real per-DOF refusals are detected only because `generate_report` aggregates them into the parent pairwise `comparison_status` (lines 1099-1101). `_report_has_refusal` ignores `refusal_reason` and `refusal_reasons` entirely, so the detection rests on one aggregation path with no direct coverage of the field the serializer actually writes.

BC-7 MINOR — Ambiguous tolerance survives in the sibling comparison path.
`comparison_framework.py:118,130` accepts `tolerance: float = 0.05`, stores it, and never reads it — `self.tolerance` has no consumer in the file. `_assess_agreement` (lines 474-516) classifies EXCELLENT/GOOD/FAIR/POOR on hardcoded 0.99/0.995/0.95/0.98/0.90/0.95 thresholds with no declared uncertainty budget and no justification, drops `None` correlations silently (lines 482-500), and has no refusal concept — its `_calculate_deviation_stats` (lines 188-198) assigns `NOT_APPLICABLE` with no `ABSENT_DIAGONAL` promotion. This path is user-reachable via `cli.py:318-320,369` (`--tolerance`, default 0.05) and `compare_diffraction_results` (`comparison_framework.py:634,651`), and `tests/hydrodynamics/diffraction/test_comparison_framework.py:82` still constructs `DiffractionComparator(aqwa, ow, tolerance=0.10)`. The evidence document's scope is the eight `BenchmarkConfig` sites and is accurate as written; the defect class is not eliminated from the module.

BC-8 MINOR — Nonfinite coverage stops at the two scalars.
`ComparisonPolicy.__post_init__` (`multi_solver_comparator.py:64-72`) guards the two named scalars explicitly; `minimum_explained_variance` is covered incidentally because NaN and ±inf both fail `0.0 < v < 1.0`. Uncovered: `compare_hydrostatics` (lines 830-836) computes `displacement_volume_diff`, `mass_diff`, `cog_diff`, `cob_diff`, `waterplane_area_diff` with no finiteness check and no refusal quality — only the correlation is guarded (lines 844-850). A NaN hydrostatic value reaches `export_report_json` with `allow_nan=False` (line 1216) and raises at write time instead of refusing. Secondary: `ComparisonPolicy(justification=None)` raises AttributeError at line 71 rather than ValueError, and a string `minimum_explained_variance` raises TypeError at line 69; both are reachable only on direct construction, since `_build_comparison_policy` pre-checks None.

BC-9 MINOR — Dead duplicated block. `validate_owd_vs_spec.py:1943-1963` computes `dof_stats` and lines 1964-1984 recompute it identically, discarding the first. No behavioural effect; it doubles the surface any future edit must keep in step.

BC-10 MINOR — Divergent refusal set, and the fail-closed verdict module has no production caller.
`benchmark_verdict.py:100` hardcodes `refused = {"INSUFFICIENT_DATA", "INSUFFICIENT_SAMPLING"}` rather than importing `REFUSAL_QUALITIES`, so `ABSENT_DIAGONAL`, `UNTRUSTED_SOURCE` and `INVALID_ABSCISSA` do not produce a named refusal. No false pass results: those qualities carry `correlation=None`, caught at line 106, and `_can_pass` (line 89) allow-lists only `COMPARED` and `NULL_RESPONSE`. The consequence is a mislabelled reason. Separately, `derive_status` has no caller in `src/` or `scripts/` — the module documented as the fail-closed status derivation is not wired into the verdict that ships, which is the ad-hoc loop at `validate_owd_vs_spec.py:2411-2431` carrying BC-1, BC-2 and BC-3.

ZD-0 MAJOR (in the function named by objective 2, independent of the zero-diagonal question) — Matrix comparison has no abscissa alignment.
`_compare_matrix_set` (`multi_solver_comparator.py:770-783`) pairs `set_a.matrices[k]` with `set_b.matrices[k]` by list position, takes `freqs = set_a.frequencies.values` from the first solver only, and nowhere compares `set_a.frequencies` to `set_b.frequencies` or the two `matrices` lengths. `_validate_inputs` (lines 275-286) checks solver count, vessel name and matrix units, not the frequency grid. RAO comparison routes through `align_responses` with `AbscissaGapError` / `AbscissaOverlapError` / `InsufficientSampling` handling (lines 577-629); matrices receive none of it. Three outcomes: equal length with different frequency values yields a silently index-mismatched comparison reported as `COMPARED` with a correlation; unequal lengths raise an uncaught `ValueError` from `values2 - values1` at line 355; length 1 against length N broadcasts silently. `_validate_matrix_units` exists because cross-solver disagreement is expected in this class, which makes the absent grid check the same defect class one field over.

================================================================
ZERO-DIAGONAL DESIGN RECOMMENDATION
================================================================

The premise is false as written. `REFUSAL_QUALITIES` (lines 47-50), `_compare_matrix_set` (lines 789-794) and the `DeviationStatistics` docstring (`comparison_framework.py:60-64`) all rest on "every real body has non-zero added mass and damping in all six DOFs" / "every DOF resists acceleration". Counterexamples, in order of force:

- Yaw added inertia about the vertical axis of a body of revolution (spar, vertical cylinder, sphere) is identically zero in potential flow; yaw radiation damping likewise. `scripts/benchmark/run_spar_benchmark.py` exists in this repository, so the geometry class is live, not hypothetical.
- Radiation damping tends to zero in the low- and high-frequency limits in every DOF, and for a fully submerged body at depth it is exponentially small; the matrix type is `added_mass` or `damping` (`output_schemas.py:227`) and the rule currently applies identically to both, though the physical claim is weakest for damping.
- Sphere rotation: all three rotational added-inertia diagonals vanish in unbounded fluid.

Scope of the resulting defect. This is a false-REFUSE, not a false-PASS: an axisymmetric case is pinned at `comparison_status == "REFUSED"` and can never be DECIDED. Under the fail-closed posture that is the safe direction, and the operational cost is that spar/sphere benchmarks cannot clear. Detection is also narrower than the premise suggests: `NOT_APPLICABLE` requires `not np.any(flat)` on both legs (line 368), i.e. exact zeros. A BEM solve on an axisymmetric mesh generally returns a small numerical residual rather than exact 0.0, in which case the cell reaches `np.ptp(...) == 0.0` only if constant, otherwise `COMPARED`. Whether AQWA or OrcaWave emit literal zeros for symmetry-implied terms was not verified and cannot be verified from source here.

What existing metadata can establish:
- `HydrodynamicMatrix.matrix_type` separates added mass from damping, permitting a type-specific rule with no new plumbing.
- `HydrodynamicMatrix.source` already separates `placeholder`/`unknown` from `solver` and drives `UNTRUSTED_SOURCE` (lines 771-788).
- The full 6×6 at every frequency is in hand, so an all-zero matrix (unambiguously missing data) is distinguishable from a single zero diagonal in an otherwise populated matrix (ambiguous).
- Both legs are visible, so "both solvers independently produced the zero" is observable.

What existing metadata cannot establish:
- Hull geometry or axisymmetry. `DiffractionResults` (`output_schemas.py:373-391`) carries `vessel_name`, `analysis_tool`, `water_depth`, `raos`, matrices, optional `hydrostatics`, `phase_convention`, `unit_system`. No mesh, no hull form, no symmetry declaration.
- Mesh symmetry does exist on the input side (`input_schemas.SymmetryType`, consumed at `orcawave_backend.py:255,265`), but it is not on `DiffractionResults`, is never passed to `MultiSolverComparator`, and would not settle the question if it were: plane symmetry is not axisymmetry. A box has XZ and YZ symmetry and a nonzero yaw added moment of inertia.
- Submergence relative to the free surface, and whether the frequency grid approaches the limits where damping legitimately vanishes.

Recommended remedy, minimal and fail-closed:

1. Correct the justification without changing behaviour. Replace the universal-physics claim at `multi_solver_comparator.py:47-50`, `:789-794` and `comparison_framework.py:60-64` with the accurate one: a zero diagonal is not self-explaining, and the result set carries no geometry or submergence metadata by which a physical zero could be distinguished from a failed extraction, so it refuses pending an explicit declaration. No verdict changes; no test changes beyond comment text. This is the part that should not wait, because the refusal rule is currently defended by a statement that is wrong, and a future reader is as likely to delete the rule as to keep it.

2. Do not add an inference from the numbers. Nothing in the data distinguishes a physical zero from a missing one; any rule keyed on zero patterns, matrix fill fraction, or "only the yaw diagonal is zero" would be exactly the inference the objective forbids.

3. Carry the exemption as a declared input, not a derived one. Add an explicit allow-list to `ComparisonPolicy` — a frozenset of `(matrix_type, dof_index)` pairs with a mandatory per-entry justification — and downgrade `ABSENT_DIAGONAL` to `NOT_APPLICABLE` only for declared cells, serialising the declaration into `comparison_policy` in the report (`to_dict`, lines 113-130) so the exemption is visible in the artifact. Default behaviour with no declaration is unchanged refusal. No acceptance tolerance moves; no solver run is implied.

Item 3 is not a bugfix. It introduces a human-declared exemption to a refusal gate, which requires a domain decision on who may declare, what evidence backs a declaration, and whether damping and added mass take the same treatment. That belongs in an addendum to the #1633 plan or a new issue, sequenced alongside [#2111](https://github.com/vamseeachanta/digitalmodel/issues/2111); it should not be absorbed into the current correction commit.

================================================================
SCOPE AND GATE ASSESSMENT
================================================================

- BC-1, BC-2, BC-3 fall inside the approved comparison-correctness scope and inside the file the recorded correction already touched. BC-1 in particular contradicts a claim made in the evidence document, so it must be fixed or the claim narrowed before that document stands as review evidence.
- ZD-0 is a comparison-correctness defect in `_compare_matrix_set` and fits the approved scope directly. It needs no domain decision — the remedy is the alignment check the RAO path already performs.
- Zero-diagonal item 1 (comment correction) fits the approved scope; item 3 (declared exemption) does not and needs a new domain decision or a plan addendum.
- BC-7 sits in `comparison_framework.py` / `cli.py`, outside the eight sites the correction claimed. Whether the same tolerance discipline extends to `DiffractionComparator` is a scope call for the issue owner, not a defect in what was claimed.
- BC-10's second half — `derive_status` having no production caller — is a gate question rather than a code defect: the shipped ALL PASS verdict does not run through the module written to make it fail-closed.
- No gate is cleared by this review. This is one provider's finding set. PR [#2106](https://github.com/vamseeachanta/digitalmodel/pull/2106) should stay unmerged.
- Not assessed, per instruction: nested `AbscissaConfig` validation (deferred to [#2111](https://github.com/vamseeachanta/digitalmodel/issues/2111)).

================================================================
EVIDENCE CHECKED
================================================================

Read in full or in the cited ranges, worktree `<diffraction-worktree>`, branch `bugfix/1633-review-blockers`, clean at `f5f1bb20`:

- `docs/plans/evidence/2026-09-12-issue-1633-review-corrections.md` (whole file)
- `tests/hydrodynamics/diffraction/test_review_1633_blockers.py` (whole file)
- `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` (whole file)
- `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py:90-309`
- `src/digitalmodel/hydrodynamics/diffraction/benchmark_verdict.py` (whole file)
- `src/digitalmodel/hydrodynamics/diffraction/comparison_framework.py:30-139, 359-424, 474-621`
- `src/digitalmodel/hydrodynamics/diffraction/benchmark_helpers.py:20-100`
- `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py:223-420`
- `scripts/benchmark/validate_owd_vs_spec.py:915-965, 1840-2068, 2090-2148, 2360-2480, 2535-2574`
- `scripts/run_benchmark_ship_raos.py:335-374`
- `tests/hydrodynamics/diffraction/conftest.py:397-451`
- `tests/hydrodynamics/diffraction/test_review_round3_regressions.py:157-215`, `test_benchmark_helpers.py:454-560`

Repository-wide greps: `MultiSolverComparator(` / `ComparisonPolicy` / `from_uncertainties` / `tolerance` across `scripts/benchmark/**` and `src/digitalmodel/hydrodynamics/**`; `BenchmarkConfig(` across `src`, `scripts`, `tests`; `derive_status` and `BenchmarkVerdict` across `src` and `scripts`; `ABSENT_DIAGONAL|NOT_APPLICABLE|NULL_RESPONSE|REFUSAL_QUALITIES` across `src`, `scripts`, `tests`; `symmetr|axisymmetric` across `src/digitalmodel/hydrodynamics/diffraction/*.py`; `tolerance=` across `tests/hydrodynamics/diffraction/*.py`.

Not done: no test execution, no solver run, no file writes (read-only constraint). BC-2 and BC-1 are control-flow readings and should be confirmed by running `validate_owd_vs_spec.py --summary-only` against a non-refused artifact set and against a config-only case. The approved plan document was not readable from this worktree.

