# Issue 2082 — CI shard collection discovery

Collection-only local attempt; no test bodies executed, no environment sync or dependency installation. Existing Python with checkout `src` and adjacent assetutilities `src` on PYTHONPATH; bytecode writes disabled.

## Configured gate

Config SHA256: `2004cb1d15de79e6e797db8122c6af2db700636b394ca93cdf931e38eaf6a8f0`

```text
uv run --no-sources --with 'assetutilities @ git+https://github.com/vamseeachanta/assetutilities.git@main' --with-editable '.[test]' --extra cfd python -m pytest tests/solver/__init__.py tests/solver/conftest.py tests/solver/smoke_test.py tests/solver/test_parametric_spec_generator.py tests/solvers/__init__.py tests/solvers/blender_automation/ tests/solvers/calculix/ tests/solvers/fea/ tests/solvers/gmsh_meshing/ tests/solvers/openfoam/ tests/solvers/smoke/ tests/solvers/test_base_solvers.py tests/solvers/test_config_migration.py tests/solvers/test_integration_solvers.py tests/solvers/test_solver_concurrency.py tests/solvers/test_solver_performance.py tests/solvers/test_solver_stress.py tests/test_meshio_integration.py tests/test_parametric_coordinator.py tests/test_sectionproperties_integration.py tests/test_solver_benchmarks.py -rfE -p no:randomly -p no:sugar --no-header -q --tb=line
```

The local invocation used the exact pytest argument suffix above, appending only `--collect-only`; it substituted the existing interpreter for the CI uv dependency provisioning prefix. This is not evidence of CI environment equivalence or shard execution success.

Exit code: 2

## New smoke node IDs observed

```text
(none emitted)
```

## Collection diagnostics

```text
=================================== ERRORS ====================================
_______ ERROR collecting tests/solvers/openfoam/test_artifact_index.py ________
E   AttributeError: module 'os' has no attribute 'O_DIRECTORY'
=========================== short test summary info ===========================
ERROR tests/solvers/openfoam/test_artifact_index.py - AttributeError: module ...
!!!!!!!!!!!!!!!!!!! Interrupted: 1 error during collection !!!!!!!!!!!!!!!!!!!!
=================== 1423 tests collected, 1 error in 8.35s ====================

```

## Collection hook confirmation

A second collection used identical pytest arguments and a read-only `pytest_collection_finish` recorder to capture fully qualified new node IDs (the default tree display did not emit node IDs). Exit 2. Observed 61 new smoke nodes; the unrelated Windows OpenFOAM collection error remains.

```text
tests/solvers/smoke/test_ci_registration.py::test_smoke_regressions_are_in_metadata_and_executed_arguments
tests/solvers/smoke/test_ci_registration.py::test_touched_source_and_new_test_paths_select_smoke
tests/solvers/smoke/test_native_harness.py::test_success_retains_artifacts_and_sanitizes_proof
tests/solvers/smoke/test_native_harness.py::test_failure_retains_diagnostics[failure]
tests/solvers/smoke/test_native_harness.py::test_failure_retains_diagnostics[badproof]
tests/solvers/smoke/test_native_harness.py::test_failure_retains_diagnostics[nonfinite]
tests/solvers/smoke/test_native_harness.py::test_timeout_kills_owned_descendant_only
tests/solvers/smoke/test_native_harness.py::test_existing_output_is_rejected
tests/solvers/smoke/test_native_harness.py::test_missing_proof_input_refuses_before_child_launch[probes.py]
tests/solvers/smoke/test_native_harness.py::test_missing_proof_input_refuses_before_child_launch[workflow.py]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_success_requires_independent_readback_and_proof_fields
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_nonfinite_static_refuses_without_nonstandard_json[nan]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_nonfinite_static_refuses_without_nonstandard_json[inf]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_nonfinite_static_refuses_without_nonstandard_json[-inf]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_dynamic_history_refuses[history0]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_dynamic_history_refuses[history1]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_dynamic_history_refuses[history2]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_dynamic_history_refuses[history3]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_dynamic_history_refuses[history4]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_incomplete_or_wrong_solve_state_refuses[SimulationStopped-False]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_incomplete_or_wrong_solve_state_refuses[RunningSimulation-True]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_incomplete_or_wrong_solve_state_refuses[SimulationPaused-True]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_saved_simulation_refuses[missing]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_saved_simulation_refuses[empty]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_invalid_saved_simulation_refuses[corrupt]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_is_validated_independently[history0]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_is_validated_independently[history1]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_is_validated_independently[history2]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_is_validated_independently[history3]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_is_validated_independently[history4]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_is_validated_independently[history5]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_state_and_completion_refuse[SimulationStopped-False]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_state_and_completion_refuse[RunningSimulation-True]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_state_and_completion_refuse[SimulationPaused-True]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_readback_requires_expected_line
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_observed_thread_count_after_solve_or_load_refuses[solve]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_observed_thread_count_after_solve_or_load_refuses[data_reader]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_observed_thread_count_after_solve_or_load_refuses[simulation_reader]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_every_constructor_attempt_has_explicit_one_thread[None]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_every_constructor_attempt_has_explicit_one_thread[1]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_every_constructor_attempt_has_explicit_one_thread[2]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_every_constructor_attempt_has_explicit_one_thread[3]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_api_errors_remain_structured_failures[statics]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_api_errors_remain_structured_failures[dynamics]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_api_errors_remain_structured_failures[save_data]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_api_errors_remain_structured_failures[load_data]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_api_errors_remain_structured_failures[save_sim]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_api_errors_remain_structured_failures[load_sim]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_missing_api_is_structured_and_has_no_fabricated_observations
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_native_handles_not_retained_after_return[None]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_native_handles_not_retained_after_return[dynamics]
tests/solvers/smoke/test_orcaflex_probe_contract.py::test_native_handles_not_retained_after_return[load_sim]
tests/solvers/smoke/test_workflow_contract.py::test_failure_report_precedes_raise_and_preserves_scratch_policy[True-True]
tests/solvers/smoke/test_workflow_contract.py::test_failure_report_precedes_raise_and_preserves_scratch_policy[True-False]
tests/solvers/smoke/test_workflow_contract.py::test_failure_report_precedes_raise_and_preserves_scratch_policy[False-True]
tests/solvers/smoke/test_workflow_contract.py::test_failure_report_precedes_raise_and_preserves_scratch_policy[False-False]
tests/solvers/smoke/test_workflow_contract.py::test_workflow_cleans_scratch_when_injected_probe_raises
tests/solvers/smoke/test_workflow_contract.py::test_mixed_dispatch_and_hostname_remain_explicit
tests/solvers/smoke/test_workflow_contract.py::test_cli_json_and_exit_code[True]
tests/solvers/smoke/test_workflow_contract.py::test_cli_json_and_exit_code[False]
tests/solvers/smoke/test_workflow_contract.py::test_engine_dispatch_uses_real_smoke_configuration
```

## Routing and execution risks

- `.github/workflows/quality-gates-by-domain.yml` reads this exact YAML command and executes it as its domain-gate step. It installs the Gmsh runtime library for solver-smoke. The detector selects `ubuntu-latest` for this domain.
- Direct calls to the existing detector confirmed the new test path selects solver-smoke only; `src/digitalmodel/solvers/smoke/probes.py` selects all 23 domains, including solver-smoke. Therefore this source change does not imply a smoke-only PR matrix.
- The local full collection failed because `src/digitalmodel/solvers/openfoam/artifact_index.py:27` evaluates `os.O_DIRECTORY` at import on Windows. No missing Python dependency was reported. No fallback flags or source patches were applied; Ubuntu PR CI remains the authoritative full-shard check.
- Collection itself runs the read-only `blender --version` availability probe in `tests/solvers/blender_automation/conftest.py:53`. Collection is not universally subprocess-free.
- Executing this complete shard on a licensed host is unsafe to describe as offline-only: explicitly included `tests/solver/smoke_test.py` imports native OrcFxAPI inside test bodies and can calculate diffraction when installed. Its `solver` marker does not deselect tests in the configured command. The marker hook also matches the broader `solvers` path substring, so blanket marker exclusion would not faithfully reproduce this shard.
- `tests/solvers/calculix/test_fem_chain.py` contains availability-gated actual `ccx` solves; Gmsh integration also depends on installed native tooling. No full-shard test bodies were executed here.
- The second collection observed 1,425 tests and 61 new smoke nodes, compared with 1,423 tests on the first attempt while the implementation worktree was changing concurrently. Both attempts failed at the same unrelated OpenFOAM import. These are timestamp-local observations, not a stable final-commit test count.

## Disposition

New smoke test discovery under the configured full pytest argument list is demonstrated. Successful full-shard collection/execution in the actual CI environment remains pending. Only this sanitized evidence file was written by this discovery task; no dependency, configuration or source changes were made.

## Actual Ubuntu CI result — 2026-09-10

The earlier local Windows limitation is superseded for this specific Ubuntu shard by completed CI evidence, not by an environment change:

- [Run 34493395354, solver-smoke job 102926238191](https://github.com/vamseeachanta/digitalmodel/actions/runs/34493395354/job/102926238191)
- Exact reviewed CI head: `681635702d7b0155bbe969c92846d97ce180b5e9`.
- Job conclusion: **success**, completed `2026-09-10T15:13:51Z`; domain-gate step completed successfully at `15:13:47Z`.
- Actual full configured pytest invocation collected **1,446 items** and reported **1,396 passed, 50 skipped in 144.80 seconds**.
- New smoke files: `test_ci_registration.py` **2 passed**; `test_orcaflex_probe_contract.py` **42 passed**; `test_workflow_contract.py` **9 passed**; `test_native_harness.py` **8 skipped** on Ubuntu. New-file total: **53 passed, 8 skipped**. No failures in the new smoke files were reported.

Exact relevant log excerpt (timestamps retained):

```text
2026-09-10T15:11:34.8491131Z collected 1446 items
2026-09-10T15:11:45.7280822Z tests/solvers/smoke/test_ci_registration.py ..                           [ 75%]
2026-09-10T15:11:45.7325256Z tests/solvers/smoke/test_native_harness.py ssssssss                      [ 76%]
2026-09-10T15:11:45.7839219Z tests/solvers/smoke/test_orcaflex_probe_contract.py .................... [ 77%]
2026-09-10T15:11:46.5547854Z ......................                                                   [ 78%]
2026-09-10T15:11:46.5951215Z tests/solvers/smoke/test_workflow_contract.py .........                  [ 79%]
2026-09-10T15:13:46.7201670Z ================= 1396 passed, 50 skipped in 144.80s (0:02:24) =================
```

The job metadata and completed log were fetched read-only through `gh api`. This evidence is limited to the exact head above: it does not cover newer commit `f61dedfd`, its two additional Windows cleanup tests, a native Windows solve, the entire multi-domain workflow, or deployment approval. No jobs were cancelled and no environment was changed.
