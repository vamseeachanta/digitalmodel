# Tests without automatic workflow selectors on 18 November 2026

**Measured.** At base `c90ef11aa6bc77047215e753f5c3290c73e57635`, 240 discoverable files containing 2,675 test definitions have no automatic workflow selector. Pytest collected 5,559 cases from 230 of those files. With the requested test extra and released dependency (`assetutilities==0.1.0`), the per-file run recorded **5,499 passes, 22 failures, 0 execution errors and 38 skips** in **15.78 minutes**.

| Selection at the base | Measured count |
| --- | ---: |
| Eligible under pytest discovery or named explicitly among its filename matches | 2,512 |
| Targeted by at least one automatic workflow | 2,272 |
| Targeted only by the manual publish workflow | 240 |
| Outside every workflow's file selectors, including manual publish | 0 |
| Recursively ignored and never explicitly selected | 12 |
| Unresolved workflows or selectors | 0 |

The publish command targets `tests/` on a manual dispatch to main and applies `-m "not solver and not slow"` and `-x`. A target path establishes eligibility when a workflow runs. Actual case execution depends on triggers, markers, collection and prior failures. The cost scenarios below concern the 240 files outside automatic CI. **Measured.** A total of 269 files fall outside the nightly matrix alone. The other 29 already have automatic selectors (12 in the parametric workflow, seven in meshing, six in the PR subsea gate and four in store-data). Their execution cost is outside both projections.

## Method and outcomes

The inventory applies `pytest.ini` test roots, filename globs and directory exclusions, and accounts for literal `collect_ignore` entries. It compares discoverable names to commands in 13 workflows. The base conftest defines `_tests_dir` as its own parent directory. The two matrix workflows use the domain reader for `tests/DOMAINS.md`, take commands from `.claude/quality-gates.yaml`, then apply workflow-specific extensions. Ownership rows alone do not select a file. The PR harness, non-shard commands, shell path globs and `--ignore` operands count. The aggregate quality gate delegates its domain tests under CI. Unresolved commands make the inventory exit nonzero.

The target was a separate, initially clean checkout of the base. Measurement used Linux, CPython 3.11.18, pytest 8.4.2 and the requested editable `[test]` environment. Source counts include `def test_*` and `async def test_*` methods/functions. One collect-only invocation retained exact node IDs, continued on collection errors and respected the repository's solver deselection. Each nonempty file then ran once in a separate process with those IDs, JUnit XML, `--durations=0`, a 300-second file limit and the balance of a shared 90-minute budget. The processes shared the same base checkout and generated caches. The script verifies completed node IDs against collection and checks JUnit case counts. Unknown or missing results remain visible.

| Observation | Measured |
| --- | ---: |
| Successfully collected cases | 5,559 |
| Deselected solver cases | 6 |
| Files with collection errors | 9 |
| Successfully collected files with zero eligible cases | 1 |
| Passed / failed / errored at execution / skipped | 5,499 / 22 / 0 / 38 |
| Timed-out files / files left by the campaign cap | 0 / 0 |
| Collected cases without a verified JUnit outcome | 0 |
| Wall time for collection (s) | 31.162 |
| Sum of per-file execution wall times (s) | 946.908 |
| Elapsed from collection start through run exit, including driver overhead (s) | 992.492 |
| Sum of JUnit case times including setup and teardown (s) | 553.509 |

All nine collection errors were in `tests/workflow_api/`. Their first error was `ModuleNotFoundError: No module named 'assetutilities.workflow_api'`. The requested command installed `assetutilities==0.1.0`. Many existing shards overlay that dependency from its development branch. These nine files have no measurement in that alternative environment. The empty file was `tests/solver/test_licensed_e2e_arbitrary_mesh.py`, whose six cases carry the solver marker. The same released dependency triggered seven embed-mode failures in the root engine tests. The licensed solver API was absent. Eight structural-solver cases skipped for a missing executable and one skipped for the optional `pyNastran` module. Other skip reasons identify the missing data or platform. The 22 failures occurred in ten files; their paths and native tracebacks are retained with the JUnit evidence.

**Measured failure counts.** The engine embed-mode tests contributed seven. The parametric atlas bridge, serving and pilot tests contributed eight (seven unavailable response-column errors and one assertion). Pipelay contributed two datetime serialization failures, the vessel database contributed two citation assertions, the legal checks contributed two assertion failures, and the structural-solver benchmark contributed one manifest mismatch. All 38 JUnit skips carried type `pytest.skip`; none carried `pytest.xfail`.

### Skip reasons

| Reason from JUnit (wording shortened where needed) | Cases (Measured) |
| --- | ---: |
| Solver executable required on PATH | 6 |
| Companion energy-data checkout absent | 5 |
| Companion data package lacks the required YAML | 5 |
| Standards registry absent in standalone checkout | 4 |
| Provenance reference checkout absent | 4 |
| Raw offshore platform test files absent | 2 |
| Stack-up registry absent in standalone checkout | 2 |
| Solver executable unavailable via environment override or PATH | 2 |
| Raw reference turbine files absent | 1 |
| Private deny list absent | 1 |
| Private mapping file absent | 1 |
| Vessel response library path unset | 1 |
| Design acceptance criteria page absent | 1 |
| Workspace deny list absent in standalone checkout | 1 |
| Windows-only sentinel test | 1 |
| Optional pyNastran module unavailable | 1 |

### Per-folder results

All entries are **Measured**. Folder names are relative to `tests/`. The root row covers files directly under `tests/`. Seconds are summed per-file execution wall times, including process startup, imports, collection inside that process, and any failed or timed-out attempt. The separate inventory collection is timed above.

| Folder | Files | Functions | Cases | Collect errors | Pass | Fail | Error | Skip | Timeouts | Wall time (s) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| floating_wind | 22 | 297 | 421 | 0 | 418 | 0 | 0 | 3 | 0 | 306.803 |
| hydrodynamics | 27 | 410 | 1387 | 0 | 1387 | 0 | 0 | 0 | 0 | 194.976 |
| (root) | 21 | 182 | 344 | 0 | 337 | 7 | 0 | 0 | 0 | 80.924 |
| legal | 16 | 235 | 329 | 0 | 325 | 2 | 0 | 2 | 0 | 66.037 |
| marine_ops | 21 | 191 | 437 | 0 | 429 | 2 | 0 | 6 | 0 | 41.419 |
| riser_database | 12 | 94 | 171 | 0 | 159 | 0 | 0 | 12 | 0 | 33.257 |
| solvers | 20 | 260 | 501 | 0 | 490 | 1 | 0 | 10 | 0 | 33.035 |
| pipelay | 7 | 56 | 118 | 0 | 116 | 2 | 0 | 0 | 0 | 21.097 |
| code_checks | 8 | 112 | 271 | 0 | 271 | 0 | 0 | 0 | 0 | 18.006 |
| validation | 4 | 54 | 72 | 0 | 72 | 0 | 0 | 0 | 0 | 13.425 |
| report_evals | 1 | 24 | 45 | 0 | 45 | 0 | 0 | 0 | 0 | 12.988 |
| parametrics | 5 | 25 | 65 | 0 | 57 | 8 | 0 | 0 | 0 | 12.520 |
| seismic | 6 | 64 | 176 | 0 | 176 | 0 | 0 | 0 | 0 | 11.020 |
| uncertainty | 5 | 23 | 40 | 0 | 40 | 0 | 0 | 0 | 0 | 9.672 |
| cad | 5 | 48 | 91 | 0 | 91 | 0 | 0 | 0 | 0 | 8.542 |
| provider_data | 5 | 26 | 63 | 0 | 58 | 0 | 0 | 5 | 0 | 7.652 |
| security | 2 | 13 | 15 | 0 | 15 | 0 | 0 | 0 | 0 | 7.066 |
| lifting_lug | 5 | 25 | 70 | 0 | 70 | 0 | 0 | 0 | 0 | 6.851 |
| technical_safety | 4 | 31 | 64 | 0 | 64 | 0 | 0 | 0 | 0 | 6.216 |
| process_diagrams | 3 | 47 | 67 | 0 | 67 | 0 | 0 | 0 | 0 | 6.203 |
| fire_safety | 2 | 33 | 33 | 0 | 33 | 0 | 0 | 0 | 0 | 6.151 |
| tug | 5 | 48 | 116 | 0 | 116 | 0 | 0 | 0 | 0 | 5.808 |
| compare_tool | 1 | 1 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 4.599 |
| maintenance | 3 | 63 | 69 | 0 | 69 | 0 | 0 | 0 | 0 | 4.122 |
| common | 3 | 20 | 44 | 0 | 44 | 0 | 0 | 0 | 0 | 3.995 |
| infrastructure | 3 | 70 | 229 | 0 | 229 | 0 | 0 | 0 | 0 | 3.542 |
| hygiene | 1 | 3 | 9 | 0 | 9 | 0 | 0 | 0 | 0 | 2.775 |
| well_access | 2 | 27 | 27 | 0 | 27 | 0 | 0 | 0 | 0 | 2.574 |
| riser_results | 2 | 30 | 87 | 0 | 87 | 0 | 0 | 0 | 0 | 2.360 |
| cfd | 1 | 21 | 89 | 0 | 89 | 0 | 0 | 0 | 0 | 1.971 |
| riser_study | 1 | 10 | 39 | 0 | 39 | 0 | 0 | 0 | 0 | 1.929 |
| unit | 1 | 11 | 11 | 0 | 11 | 0 | 0 | 0 | 0 | 1.831 |
| production_chemistry | 1 | 18 | 18 | 0 | 18 | 0 | 0 | 0 | 0 | 1.485 |
| metocean | 1 | 3 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 1.359 |
| governance | 1 | 3 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 1.260 |
| residual | 1 | 10 | 10 | 0 | 10 | 0 | 0 | 0 | 0 | 1.223 |
| verification | 1 | 9 | 9 | 0 | 9 | 0 | 0 | 0 | 0 | 1.118 |
| hydrostatics | 1 | 13 | 15 | 0 | 15 | 0 | 0 | 0 | 0 | 1.097 |
| solver | 1 | 6 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0.000 |
| workflow_api | 9 | 59 | 0 | 9 | 0 | 0 | 0 | 0 | 0 | 0.000 |

## Comparison with the 16 November walk

**Estimate from the original walk:** 240 files, 2,669 functions, including 106 riser-fatigue functions. **Measured at this base:** 240 automatic-gap files, 2,675 functions, six more than the estimate. The PR subsea command explicitly adds `tests/riser_fatigue/`; its six files contain 106 definitions and separately collect 201 cases. The nightly subsea extension adds only `tests/riser_bom/`. None of the riser-fatigue files belongs to this automatic-gap cohort.

A Git-history replay found 274 files and 3,113 definitions outside the base shard commands before workflow extensions and non-shard selectors. PR extensions account for 11 files/147 definitions, including one file also in the harness. Store-data, meshing and parametric workflows account for another 4/54, 7/59 and 12/178 respectively. That leaves 240/2,675. On the same 240 paths, early 16 November contained 2,668 definitions. Commit `3fb7767e8` added four and `d441c0783` added three. These replay figures are **Measured**. The original walk supplied neither paths nor a SHA, so its exact membership and six-definition difference cannot be reconstructed.

## Runner cost

**Estimate at the measured local runner speed.** Running the measured workload once nightly with one process per file adds **15.78 runner-minutes/day** (946.908 s ÷ 60). Running it on each of the 99 domain-gate PR runs reported for 16 November adds **1,562.40 runner-minutes/day** (99 × the unrounded single-run time). The multiplier is the supplied historical workload, not a measurement of PR frequency made here. Both projections include the observed failures and skipped cases. They exclude environment provisioning, runner billing granularity and the separate collect-only audit.

**Measured.** JUnit accounts for 553.509 s of setup, test and teardown across reported cases. A new shard that groups files would pay shared imports and session setup fewer times. Its runtime, the cost of fixing the collection errors, and runs with extra dependencies require a measurement in that configuration. The previous 6–1,450 runner-minute/day range was an **Estimate**. The nightly projection falls inside it. The 99-run projection exceeds its upper end by 112.40 runner-minutes/day at the measured file-process rate.

## Reproduction

Use Linux with GNU `timeout`, the measurement tool from this change, a clean checkout of the stated SHA, Python 3.11, and a fresh output directory. The original job kept `measurement/` and the environment manifest outside Git. `uv --no-sources` re-resolved the project lock for this run; the environment manifest records the resolved digest and installed versions.

```sh
# TOOL points to the absolute path of scripts/ci/orphan_tests.py in this change.
# BASE points to the clean target checkout. JOB points to the external output folder.
cd "$BASE"
export JOB UV_PYTHON=3.11 PYTHONPATH=src
python3 "$TOOL" list --root "$BASE" --output-dir "$JOB/measurement"
uv run --no-sources --with-editable '.[test]' python "$TOOL" collect --output-dir "$JOB/measurement"
uv run --no-sources --with-editable '.[test]' python "$TOOL" run --output-dir "$JOB/measurement"
uv run --no-sources --with-editable '.[test]' python -m pytest --collect-only -q tests/riser_fatigue -p no:sugar -p no:randomly > "$JOB/riser-collect.log"
```

Here both measurement phases report a nonzero status. Run both to retain the successful files alongside the collection errors. To reproduce the published totals from the retained outputs:

```sh
python3 "$JOB/summarize.py" "$JOB"
```

For a direct totals check using just the three JSON outputs:

```python
import json
from pathlib import Path
import os

out = Path(os.environ["JOB"]) / "measurement"
inventory, collection, run = [json.loads((out / name).read_text())
                              for name in ("list.json", "collect.json", "run.json")]
print(len(inventory["automatic_orphans"]), sum(inventory["functions"].values()))
print(sum(len(row["nodes"]) for row in collection["files"].values()))
print(run["outcomes"], run["skip_reasons"], run["seconds"])
```

The measured selection manifest SHA-256 is `eb4b485698548b81ee2f229a696522b515c90e0609d3dbcebf42bc2cd0404bc7`. The resolved dependency lock SHA-256 is `510a33cb474ea1004dccaf78233c22343bc56ca0f9c4586a89ab33cf667fb7fa`. The reducer checks SHA/digest bindings, reads every retained XML, compares each completed file's finished-node events with collection, and emits folder totals, skip reasons and wall times. The job folder also holds per-file logs, the collection log, raw JSON and the historical replay. No existing test was removed, moved or added to CI selection. The sole harness change registers the measurement regression file.
