# Plan for #1631: make the licensed lane capable of reporting failure

> **Status:** plan-review (awaiting owner approval — never self-approved)
> **Complexity:** T3
> **Date:** 2026-08-05
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1631
> **Client:** N/A
> **Lane:** lane:claude
> **Branch:** `plan/1631-licensed-lane-fail-closed` (worktree off `origin/main` @ `0fdeea67`)
> **Review artifacts:** r1 Claude — inline, main session (see Adversarial Review Summary)

> **Provenance.** Premises were re-measured on 2026-08-05 against digitalmodel
> `origin/main` @ `0fdeea67` and against `vamseeachanta/deckhand-licensed-runs-queue`
> `origin/main` @ `ece52e32`. The queue repository is a **separate repository and
> was read only** — nothing in this plan modifies it. Two premises carried
> forward from the issue are refuted below, one of them load-bearing: the three
> mock-mode mechanisms the issue cites are real defects, but they are **not** the
> mechanism that produced the twelve misleading results.

---

## Premise verification (2026-08-05)

| # | Claim as filed / handed over | Verdict | Evidence |
|---|---|---|---|
| 1 | 12 requests, 12 results, zero pending | **CONFIRMED** | `git ls-tree -r origin/main queue/requests` → 12 `.json` + `.gitkeep`; `queue/results` → 12 top-level `lr_acma_*.json`. No request lacks a result. |
| 2 | Every result reports `state: finished`, `returncode: 0` | **CONFIRMED** | All 12 result JSONs carry `"state": "finished"`, `"returncode": 0`, `"reason": "finished"`. No other state occurs anywhere in the repository's history of results. |
| 3 | The validator files inside those results say 2 `FAIL`, 5 `WARNING`, 0 `PASS` across 7 `*_validation.json` | **CONFIRMED, exactly** | 7 files; `overall_status` = `FAIL` (`lr_acma_2a4a51f24387`, `lr_acma_36fb1ffbc5e0`), `WARNING` (`lr_acma_3bd7c32b74db`, `lr_acma_59bf094e4182`, `lr_acma_ac735070ce14`, `lr_acma_bb24945c4b8b`, `lr_acma_c3b4e7d55c83`). Zero `PASS`. |
| 4 | Every request is dated 2026-07-13 | **REFUTED** | `created_at` spans **2026-06-20 → 2026-07-18**. Only 3 of 12 fall on 2026-07-13. The most recent approved request is `lr_acma_ac735070ce14` at `2026-07-18T20:35:10Z`. The clean sweep is a five-week record, not a one-day batch. |
| 5 | The poller has run ~21,700 polls since and processed nothing new | **CONFIRMED, with a correction** | `queue/heartbeat/ace-win-1.json` → `polls: 21558`, `last_poll_at: 2026-08-05T04:44:36Z`. It has processed nothing new because **no request has been submitted since 2026-07-18** — the poller is not stuck, it is idle. Both statements are true; only the second is a defect-free explanation. |
| 6 | (Issue §Note on lane health) "The lane is currently down"; ace-win-1 last polled `2026-07-23T10:01:34Z` | **REFUTED as of today** | All four active hosts polled within the last 15 minutes: ace-win-1 `04:44:36Z`, ace-linux-1 `04:54:44Z`, ace-linux-2 `04:47:28Z`, gpu-claw `04:55:40Z` (2026-08-05). ace-win-2 remains stopped since 2026-07-13. The common-cause stop the issue describes has since recovered; **no part of this plan should assume the lane is down.** |
| 7 | `run_to_sim.py` sleeps 0.1 s and sets `result['success'] = True` when `self.mock_mode or not ORCAFLEX_AVAILABLE` | **CONFIRMED** | `src/digitalmodel/solvers/orcaflex/run_to_sim.py:74-79` — branch, `time.sleep(0.1)  # Simulate processing time`, `result['success'] = True`. |
| 8 | `universal_runner.py` silently flips `self.mock_mode = True` on `ImportError`, then writes a `.sim` containing the literal string `"Mock simulation for <name>"` | **CONFIRMED** | `universal/universal_runner.py:111-121` (`except ImportError:` → `logger.warning(...)` → `self.mock_mode = True`); `:347-361` writes `sim_file.write_text(f"Mock simulation for {model_file.name} (analysis: {analysis_type})")` then sets `result['success'] = True`. |
| 9 | `model_interface.py:163` makes the typed `LicenseError` at `:182-188` unreachable by default | **CONFIRMED, with a correction** | `self.use_mock = use_mock or not ORCFXAPI_AVAILABLE` at `:163`; `_check_license()` is called only `if not self.use_mock` (`:175-176`). The `NO_MODULE` `LicenseError` at `:182-188` is therefore **provably dead code** — it can only be reached when `ORCFXAPI_AVAILABLE` is false, which is exactly when `use_mock` is forced true. The correction: the *`NO_LICENSE`* branch below it (`:190+`) **is** reachable, but only on a host where `OrcFxAPI` imports and the seat check then fails. Also uncited in the issue: `:156-163` already honours `ORCAFLEX_FORCE_MOCK` / `ORCAFLEX_SKIP_REAL`, so an explicit-opt-in mechanism partly exists here already. |
| 10 | **These three mechanisms are what produced the twelve misleading results** | **REFUTED — load-bearing** | None of the 12 runs went through any OrcaFlex mock path. Their workflows are `orcawave-diffraction-solve` (6), `aqwa-diffraction-solve` (4), `openfoam-run-batch` (1), `orcaflex-strength-post` (1). The two `FAIL` runs were **real licensed OrcaWave solves** on a Windows host (`D:\ws\...`, `NobleValiant` GDF, 10,489 vertices) that produced physically invalid output. No `.sim` file, and no `"Mock simulation for ..."` artifact, appears anywhere in the queue. The mock mechanisms are genuine defects; they are **not** this evidence's cause, and a fix confined to them would leave every one of the 12 results exactly as misleading as it is today. |
| 10a | (New) **Even the one OrcaFlex workflow in the queue does not route through the three named modules** | **CONFIRMED — the issue's fix list would miss it** | `orcaflex-strength-post` resolves via `usecase_registry/registry.yaml:128-136` → `basename: orcaflex_post_process` → `engine.py:238-240` → `solvers/orcaflex/orcaflex.py:19-36` `OrcaFlex.router()`, which calls `orcaflex_preprocess`, `orcaflex_analysis`, `opp.post_process_router`, `all_vars`. It imports **none** of `run_to_sim`, `universal_runner`, or `core/model_interface`. Its fail-open surface is instead `orcaflex_utilities.py:1-4` (bare `try: import OrcFxAPI / except Exception: print(...)`, module imports anyway) and `:55-68` (`is_orcaflex_available()` returns `False` rather than raising), plus soft imports in `opp_range_graph.py:6`, `opp_time_series.py:16`, `opp_visualization.py:10`, `orcaflex_objects.py:7`, `all_vars.py:3`. **Fixing exactly the three files the issue names would leave `lr_acma_ff132001b7ad`'s workflow as fail-open as it is today.** |
| 10b | (New) There are **five** availability-inferred mock surfaces, not three | **CONFIRMED** | Beyond the three named: `run_to_sim_cli.py:109-112` (`if not args.mock and not ORCAFLEX_AVAILABLE: args.mock = True`, then exits `0` on mock success at `:132-135`) and `universal/batch_processor.py:270-276` (writes `sim_file.write_text(f"Mock simulation for {model_path.name}")`, sets `success = True`). A further ~14 modules infer mock or print-and-continue; full inventory in Resource Intelligence. |
| 10c | (New) Two mock behaviours are **more severe than the issue describes** | **CONFIRMED** | `core/model_interface.py:570-573` — `extract_results` in mock mode does `import numpy as np; return np.random.randn(100)`, i.e. **returns random numbers as engineering results**. `:509-511` — `save_model` does `output_path.touch()`, creating an **empty** `.sim`/`.dat`. Separately, `run_to_sim.py:74-80` sets `success = True` and reports `sim_output = str(sim_file)` while **writing no file at all**, so the reported artifact path points at nothing; and `:128` logs `Mode: REAL` on an auto-fallback run because `self.mock_mode` is never mutated there — **the log actively misreports the mode**. |
| 11 | The validator verdict is discarded and exit status is decided solely by the subprocess not raising | **CONFIRMED, and root-caused more precisely than filed** | See rows 12–13. The discard happens in two places, and the second is the more fundamental. |
| 12 | (New) The consuming side is a thin subprocess wrapper with no independent judgement | **CONFIRMED** | `deckhand/src/deckhand/licensed_run_agent_runtime.py:37-50`: `argv = ["uv", "run", "python", "-m", "digitalmodel", str(input_path)]` … `return {"returncode": proc.returncode, ...}`. Deckhand adds nothing. **The exit status of `python -m digitalmodel` is the entire success signal**, which places the fix squarely in this repository. |
| 13 | (New) `python -m digitalmodel <input.yml>` has **no failure exit path at all** for the engine contract | **CONFIRMED — empirically** | `src/digitalmodel/__main__.py:main()` ends with a bare `engine()` call whose return value is discarded; there is no `sys.exit(...)` on that path. `engine()` returns `cfg_base` (a dict, `engine.py:847`), never a status code. Probe (stubbing `digitalmodel.engine` to return a config carrying `validation_verdict: FAIL`): `main()` returned `None`, no `SystemExit`, **process exit status 0**. The only way this command exits non-zero today is an uncaught exception. This is the mechanism behind `returncode: 0`, and it is host-independent and license-independent. |
| 14 | (New) The diffraction lane **already refuses** a silent unlicensed fallback | **CONFIRMED — changes the scope** | `hydrodynamics/diffraction/workflow.py:118-127` already raises `RuntimeError` on `status == "failed"` and on `status == "dry_run" and not requested_dry_run` ("solver unavailable: run fell back to dry-run … Run on a licensed host"). Landed `19e8eae3`, 2026-07-04, i.e. **before** 8 of the 12 runs. Requirements "a run that cannot solve refuses" and "an unlicensed host is distinguishable" are therefore **already satisfied on the path that produced this evidence**. What is missing is narrower and more specific than the issue states. |
| 15 | (New) The verdict is already computed, already typed, and already attached to the result object | **CONFIRMED — the gap is a few lines, not a subsystem** | `diffraction/validation_runner.py` defines a closed 5-value contract (`PASS`/`WARNING`/`FAIL`/`ERROR`/`SKIPPED`); `orcawave_runner.py:239` declares `validation_verdict: str = "SKIPPED"` on `RunResult` and `:594` populates it. `workflow.py:102-133` reads `result.status` and **never reads `result.validation_verdict`**. That single omission is the whole propagation gap. |
| 16 | (New) A fail-closed gate that raises is already established house practice here | **CONFIRMED — precedent to mirror, not invent** | Three precedents. (a) `structural/parametric_coordinator.py:296-302` raises `OrcaFlexUnavailableError("… run the OrcaFlex campaign on a licensed workstation")` when `ORCAFLEX_AVAILABLE` is false — **and `tests/test_parametric_coordinator.py:365,375-394` already asserts it with `pytest.raises`**. This is the exact behavioural contract to generalise. (b) `solvers/smoke/probes.py:40-49` `check_orcaflex()` reports `{"ok": False, "stage": "import", ...}` honestly. (c) `diffraction/quality_gates.py:155-176` `enforce_quality_gates` raises on blocking `FAIL` — the *pre*-solve mesh gate; all 5 mesh reports in the queue are `WARNING`, so it correctly did not block. The missing symmetry is a *post*-solve equivalent. |
| 19 | (New) `OrcFxAPI` is not importable on this host, and cannot be | **CONFIRMED** | `pyproject.toml:187` puts `OrcFxAPI` in an optional `solvers` extra (`uv.lock:1258`, `marker = "extra == 'solvers'"`). Even when installed, the wheel is Windows-only at import: `OrcFxAPI.py:38` calls `OrcaFlexAPIConfig.lib()` and `:99-118` touch `ctypes.windll.gdi32` / `kernel32` / `winmm` / `OleAut32`. **Every `except ImportError` fallback listed above therefore fires unconditionally on every Linux CI runner.** |
| 20 | (New) `orcaflex-strength-post` is already excluded from CI by registry metadata, not by a mock | **CONFIRMED** | `docs/registry/workflows.yaml:434` → `runtime: requires-license`; `tests/workflows/test_durable_workflows.py:53-54` skips any workflow whose `runtime != "offline"`. So CI never exercises it, and no CI job would have caught the crash. `tests/workflows/` is a **live lane** ([#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968)) and is not touched by this plan. |
| 21 | (New) No CI workflow sets the mock env vars or deselects the solver marker | **CONFIRMED** | `ORCAFLEX_FORCE_MOCK` / `ORCAFLEX_SKIP_REAL` appear in exactly four places repo-wide: read at `model_interface.py:156-157`, set at `tests/solvers/orcaflex/run_tests.py:63,80` (an **uncollected** helper script that also references a nonexistent path and a nonexistent `ORCAFLEX_SKIP_MOCK`). No `.github/workflows/*.yml` sets either, and none passes `-m "not solver"`. `tests/solver/conftest.py:9-13` auto-marks by the substring `"solver"` in the path, which also catches everything under `tests/solvers/`. |
| 17 | `lr_acma_ff132001b7ad` (`orcaflex-strength-post`) crashed and returned no artifacts, yet `returncode: 0` | **CONFIRMED** | Its result JSON has **no `returned_files` key at all** and `returncode: 0`. A run that produced nothing is indistinguishable, at the result level, from one that produced everything. |
| 18 | `scripts/legal/legal-sanity-scan.sh` does not exist in this repository | **CONFIRMED** | `scripts/legal/` does not exist. No acceptance criterion below cites it. See the note under Acceptance Criteria — the omission is deliberate and instructive. |

**What the refutations change.** Premise 10 is the one that matters. The issue's
"Requested resolution direction" leads with mock-mode removal (items 3 and 4 of
5); the measured evidence says mock mode is a *separate, unexercised* liability
and that fixing it would not have turned a single one of the 12 results red.
This plan therefore inverts the issue's ordering: the exit-status and verdict
propagation defect is the primary work, and the mock-mode defects are fixed as a
second, independent lane so that the same class cannot bite a path that is not
yet carrying licensed traffic. Premise 14 further removes work the issue implies
is outstanding. Premises 13, 15 and 16 shrink the primary fix from "build a
fail-closed pipeline" to "connect three things that already exist".

Premise 10a is the second-order trap and is worth stating on its own: **the
issue's three-file fix list does not intersect the one OrcaFlex workflow that
actually appears in the queue.** An implementer who worked the issue as written
would edit `run_to_sim.py`, `universal_runner.py` and `model_interface.py`,
observe green tests, and leave `orcaflex-strength-post` — the workflow behind
`lr_acma_ff132001b7ad`, the run that returned no artifacts at all — exactly as
fail-open as before. That is the same shape of mistake as the lane itself: a
change that reports success without having checked the thing it claims to fix.

---

## Deliverable

`python -m digitalmodel <input.yml>` will exit non-zero when the run it just
performed did not earn a success, and the reason will be recorded as structured
data rather than recoverable only by regex over `stdout_tail`. Specifically:

- A **run-contract module** (new, outside any live lane) that owns the single
  mapping from a run's verdict to a process exit status, and that emits a
  machine-readable run-verdict sidecar next to the run's outputs.
- **An exit path at the entrypoint**, so a failing verdict can leave the process
  at all — which today it cannot, for any workflow, on any host.
- **Verdict propagation** from the diffraction runner's existing
  `validation_verdict` field through the router into that contract.
- **Solver provenance in the result**, so an unlicensed host is distinguishable
  from a licensed one without reading logs.
- **Mock mode made explicit** on all five OrcaFlex paths *and* on the
  `orcaflex-strength-post` surface the issue's fix list omits, with mock
  artifacts made unmistakable, and with unlicensed development explicitly
  preserved.
- **A recorded disposition** for the 12 committed results.

Out of the box this changes the observable behaviour of exactly one command,
in one direction: runs that would have reported `returncode: 0` while carrying a
`FAIL` verdict will report non-zero. Runs carrying `PASS` or `WARNING` will
continue to report `0`.

---

## Resource Intelligence Summary

### Existing repo code

| Component | Path | State today |
|---|---|---|
| Entrypoint | `src/digitalmodel/__main__.py:60-80` | `main()` calls `engine()` bare; return value discarded; no `sys.exit` on the engine-contract path. |
| Engine | `src/digitalmodel/engine.py:89-847` | Returns `cfg_base`; routes `basename == "diffraction"` at `:608-612`; raises `ValueError`/`NotImplementedError` for routing errors only. |
| Diffraction router | `src/digitalmodel/hydrodynamics/diffraction/workflow.py:29-133` | Already raises on `failed` and on unrequested `dry_run` (`:118-127`). Records `settings["run_status"]`. Does **not** record or act on `result.validation_verdict`. |
| Verdict contract | `src/digitalmodel/hydrodynamics/diffraction/validation_runner.py:1-60` | Closed 5-value vocabulary, documented, shared by the OrcaWave and AQWA runners. Already correct. |
| Verdict production | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py:239,594` (and `aqwa_runner.py:46`) | `RunResult.validation_verdict` declared and populated. Already correct. |
| Verdict definition | `src/digitalmodel/hydrodynamics/diffraction/output_validator.py:350-377` | `FAIL` iff any issue string matches the substrings `negative` or `missing`; else `WARNING` if any issue; else `PASS`. Predates this data (#611/#625). |
| Pre-solve gate precedent | `src/digitalmodel/hydrodynamics/diffraction/quality_gates.py:155-176` | Raises on blocking `FAIL`. The pattern to mirror. |
| Mock path A | `src/digitalmodel/solvers/orcaflex/run_to_sim.py:74-80` | Availability-inferred mock; sets `success = True`; **writes no file** yet reports `sim_output`; `:128` logs `Mode: REAL` anyway. **Zero test coverage** of `OrcaFlexModelRunner` or `run_models` anywhere in `tests/`. |
| Mock path B | `src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:111-124,347-363` | `ImportError` → mock; writes a `.sim` at the real artifact path; propagates the inferred flag into `BatchProcessor` at `:147-150`. |
| Mock path C | `src/digitalmodel/solvers/orcaflex/core/model_interface.py:156-188` | Availability-inferred mock; already honours `ORCAFLEX_FORCE_MOCK`/`ORCAFLEX_SKIP_REAL`; `NO_MODULE` `LicenseError` dead. Downstream mock branches fabricate: `:509-511` `save_model` → `touch()` (empty artifact); `:570-573` `extract_results` → `np.random.randn(100)`; `:313` mock skips validation entirely. |
| Mock path D | `src/digitalmodel/solvers/orcaflex/run_to_sim_cli.py:109-112` | CLI-layer auto-mock (`if not args.mock and not ORCAFLEX_AVAILABLE: args.mock = True`), then exit `0` at `:132-135`. |
| Mock path E | `src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:270-276` | Same `.sim` forgery as path B, in the batch path. |
| **`orcaflex-strength-post` surface** | `orcaflex_utilities.py:1-4,35-38,55-68` and `opp_*` / `all_vars` / `orcaflex_objects` soft imports | The actual fail-open surface for the one OrcaFlex workflow in the queue (premise 10a). `is_orcaflex_available()` returns `False` rather than raising. |
| Other inferred fallbacks | `orcaflex_converter_enhanced.py:132-141`, `examples_integration/converter.py:27-35`, `examples_integration/batch_converter.py:86-91`, `mooring_tension_iteration/orcaflex_interface.py:388-399` | Same shape: `except ImportError` → `use_mock = True` behind a caller who passed `False`. |
| Fail-closed precedent | `src/digitalmodel/structural/parametric_coordinator.py:296-302` | Raises `OrcaFlexUnavailableError`; **already asserted** by `tests/test_parametric_coordinator.py:365,375-394` via `pytest.raises`. The contract to generalise. |
| Package import hazard | `src/digitalmodel/solvers/orcaflex/__init__.py:20-25,36-41,43-54,84-92` | Wraps every subimport in `try/except ImportError` → `*_AVAILABLE = False`, `run_models = None`. **A fail-closed raise at import time would be swallowed into a `None` export.** Raises must therefore happen at call time, not import time. |
| Consumer | `deckhand/src/deckhand/licensed_run_agent_runtime.py:37-50` | Separate repository. Thin subprocess wrapper; no judgement of its own. |
| Licensed e2e suite | `tests/solver/test_licensed_e2e_arbitrary_mesh.py:85` | `pytestmark = pytest.mark.skipif` on `OrcFxAPI` import; already contains `test_auto_validation_verdict`. |
| Router tests | `tests/hydrodynamics/diffraction/test_workflow_router.py` | 15 tests, all passing, including `test_run_orcawave_silent_dry_run_fallback_raises` — the established pattern for proving refusal **without a license**, by patching `run_solve`. |

### Gaps identified

1. **No exit path exists.** The entrypoint cannot report failure for any
   workflow. Everything else is downstream of this.
2. **No verdict seam.** The router reads `result.status` and drops
   `result.validation_verdict` on the floor.
3. **No structured verdict in the result.** The queue's own README constrains
   results to "audit + summary only", and the verdict is currently present only
   as prose inside `stdout_tail`.
4. **No solver provenance.** Nothing in a result distinguishes "solved on a
   licensed seat" from "did not".
5. **No positive-artifact requirement.** `lr_acma_ff132001b7ad` returned nothing
   and passed.
6. **Availability-inferred mock on five OrcaFlex paths** (A–E above), plus the
   `orcaflex_utilities` / `opp_*` surface that the one queued OrcaFlex workflow
   actually uses, plus ~14 further modules that print and continue.
7. **The 12 committed results carry an unearned success flag** and no caveat.

### Blast radius of making mock explicit

Measured, not assumed. `OrcFxAPI` is unimportable here (premise 19), so every
fallback currently fires — which means the blast radius is the set of things
that *rely* on it firing.

- **Zero collected tests rely on the implicit fallback for the three named
  modules.** `tests/solvers/orcaflex/universal/test_universal_runner.py:51-55,
  99-136, 158-180, 225-230` all pass `mock_mode=True` **explicitly** and are
  unaffected. Note `:225-230` asserts `sim_file.exists()` for each fake `.sim` —
  it codifies forged artifacts as correct behaviour and must be updated by D6.
- **At risk (skip-guarded):** `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:58-64,
  80-125, 145-156, 245-253, 255-290, 298-305` construct with `use_mock=False`
  and then read back the *mutated* flag (`if not converter_real.use_mock:`);
  `:154` is written as `assert stats['failed'] == 0 or converter_real.use_mock  # Allow failures in mock mode`.
  These would raise at construction under fail-closed. **Mitigation:** they
  `pytest.skip` when the sample corpus is absent; the implementer must confirm
  whether that corpus resolves in CI before treating them as breakage.
- **Uncollected but real:** six `tests/solvers/orcaflex/*/scripts/run_*.py`
  helpers call `UniversalOrcaFlexRunner()` with no arguments, as does
  `scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py:78`. Not
  collected by pytest, so CI stays green, but an operator running them would
  hit the new raise. They need the explicit flag.
- **Not collected at all:** `tests/solvers/orcaflex/test_orcaflex_unit.py` is in
  `tests/conftest.py:17` `collect_ignore`; `tests/solvers/orcaflex/run_tests.py`
  is already rotted (references a nonexistent path and a nonexistent
  `ORCAFLEX_SKIP_MOCK`).

### Evidence

- Queue measurement commands and outputs are reproducible read-only against
  `deckhand-licensed-runs-queue` `origin/main` @ `ece52e32`.
- The exit-status probe is reproducible on any host, licensed or not, and is
  reduced to a regression test in the TDD list below (`test_main_exits_non_zero_on_fail_verdict`).
- `uv run --no-sync python -m pytest tests/hydrodynamics/diffraction/test_workflow_router.py -q`
  → **15 passed in 0.38s** on this unlicensed Linux host, 2026-08-05.
- `uv run --no-sync python -m pytest tests/solver/test_licensed_e2e_arbitrary_mesh.py -q -rs`
  → **6 skipped**, `OrcFxAPI not available (licensed Windows host only; see #610)`.
  This is the exact shape of the verification hole that D7 addresses.

---

## Design decisions

**D1 — The defect is "a completed run's verdict never reaches the exit status", not "mock mode fabricated these results".** Premise 10 refutes the
issue's framing. The 12 results were produced by real solves and by a
post-processing crash, on the engine contract, none of which touches an OrcaFlex
mock path. Fixing mock mode first would leave the demonstrated defect intact.
Mock mode is still fixed here (D5/D6) — as an independent lane, because it is a
real fail-open liability on paths that will carry traffic later, not because it
explains this evidence.

**D2 — The exit status is the product's failure channel, and it is owned in one place.** Deckhand is a thin `subprocess.run` wrapper (premise 12), so
`returncode` is the whole contract. A new `src/digitalmodel/run_contract.py`
will own the verdict→exit mapping and the run-verdict sidecar. Alternatives:

- Teach Deckhand to parse validator files → rejected. It is a separate
  repository, it would re-derive a judgement digitalmodel already holds, and
  every other consumer of `python -m digitalmodel` would remain fail-open.
- Have each workflow call `sys.exit` itself → rejected. The mapping would drift
  per workflow, which is how the pre-solve and post-solve gates diverged in the
  first place (premise 16).

**D3 — `FAIL` and `ERROR` refuse; `WARNING` and `PASS` do not. This is the inverse-defect guard, and it introduces no fitted constant.** A gate that
also refused `WARNING` would refuse **5 of the 7** validated runs in the queue
and would read as rigorous while making the lane unusable. The boundary is
**not** chosen from that distribution: it is the pre-existing vocabulary of
`output_validator._determine_overall_status` (`:350-377`, "`FAIL` iff an issue
mentions `negative` or `missing`"), specified under #611/#625 long before these
runs, and the pre-existing `SKIPPED`/`ERROR` extension in `validation_runner`.
This plan **reuses that boundary and does not retune it**. See the Risks section
— the boundary is a crude substring heuristic and that weakness is inherited
knowingly, not silently.

**D4 — Unlicensed development stays possible, and the plan says exactly how.** Three preserved routes, none of which are weakened by this work:

- `dry_run: true` in the input spec remains a first-class, *explicitly requested*
  mode. The router's existing guard (`workflow.py:123-127`) only refuses a
  dry-run that was **not** requested. That distinction is already correct and is
  left untouched.
- `postprocess_diffraction_run` (`diffraction/postprocess.py:76`) already exists
  precisely to "postprocess one diffraction run bundle on Linux (no license)".
  Unaffected.
- The OrcaFlex mock CLIs keep working, behind an explicit flag (D5).

A run on an unlicensed host that *asks* for a real solve will refuse — that is
the point. A run that asks for a dry run, or post-processes an existing bundle,
will not.

**D5 — Mock mode becomes explicit-opt-in on all five OrcaFlex paths, and the raise happens at call time, not import time.** Removing mock outright would
break the `run-to-sim` / `orcaflex-universal` / `orcaflex-sim` console scripts
and every runner constructed on a Linux host. The change is to delete the
*inference* (`or not ORCAFLEX_AVAILABLE`, `except ImportError: self.mock_mode = True`,
`if not args.mock and not ORCAFLEX_AVAILABLE: args.mock = True`) while keeping
the *explicit* switches (constructor argument, CLI flag, and the
`ORCAFLEX_FORCE_MOCK` / `ORCAFLEX_SKIP_REAL` env vars that
`model_interface.py:156-163` already honours). Absence of a licence with no
explicit opt-in becomes a raised `LicenseError` — which resurrects the dead
`NO_MODULE` branch at `:182-188` instead of deleting it, and matches the
`OrcaFlexUnavailableError` contract already asserted at
`tests/test_parametric_coordinator.py:365`.

**The raise must be at call time.** `solvers/orcaflex/__init__.py:20-25,36-41`
wraps every subimport in `try/except ImportError` and degrades the export to
`None`. An import-time raise would be swallowed there and would convert a loud
refusal into a `NoneType` error at some later, unrelated call site — a strictly
worse failure mode than today's.

**D5a — `orcaflex-strength-post` is fixed at its own surface, or it is not fixed.** Per premise 10a this workflow reaches none of paths A–E. Making it
fail closed means `orcaflex_utilities.py:1-4` (stop printing and continuing) and
`:55-68` (`is_orcaflex_available()` must feed a call-time raise in
`opp.post_process_router`, not silently return `False`). This is listed
separately because it is the *only* item in this plan that touches the workflow
the queue actually exercised, and it is the item the issue's own fix list omits.

**D6 — Mock artifacts are written to a distinct location AND carry an in-band marker. Presented as a ranked decision, per the issue's own framing.**

1. **Recommended — distinct location plus marker.** Mock output goes to a
   `*_mock/` sibling of the real output directory, and every mock artifact name
   carries a `.mock` infix (`model.mock.sim`). The existing literal body
   (`"Mock simulation for ..."`) is retained as a third, in-band signal. Applies
   to all three writers: `universal_runner.py:358`, `batch_processor.py:272`,
   and `model_interface.py:509` (which today `touch()`es an *empty* file, giving
   a downstream consumer nothing to inspect at all). Cost: any caller that
   hardcodes the real path stops finding mock output — which is the desired
   failure, and is the reason to prefer this. One test codifies the current
   behaviour and must move with it:
   `tests/solvers/orcaflex/universal/test_universal_runner.py:225-230` asserts
   `sim_file.exists()` for each forged `.sim`.
2. Marker-only, real path. Cheaper, and preserves every existing caller, but a
   consumer that globs `*.sim` still cannot tell them apart without opening each
   file. Rejected as the primary because the issue's stated harm is exactly a
   downstream consumer seeing "a `.sim` on disk".
3. Remove mock artifact writing entirely. Cleanest, but it converts the mock
   runners into no-ops and breaks their CLIs; not justified by the evidence.

**D7 — Refusal and the real path must both be exercised, and a licence-gated test is never the proof.** The trap this plan must not fall into is a suite
that asserts "the mock path is refused" while every test that would exercise a
real solve is `skipif`-ed away — which is the *current* state of
`tests/solver/test_licensed_e2e_arbitrary_mesh.py` (6 skipped here). Therefore:

- The primary tests inject a **fake completed solve** by patching `run_solve`,
  exactly as `test_workflow_router.py` already does. These construct a
  `RunResult` with `status == "completed"` and `validation_verdict == "FAIL"` —
  i.e. they exercise the **success path** of the solver and the **refusal path**
  of the verdict gate. They run on every host, including one where `OrcFxAPI`
  imports successfully.
- A `PASS`-verdict counterpart asserts exit `0`, so the gate is proven capable
  of *not* firing. A gate never observed green is as unproven as one never
  observed red.
- The licence-gated e2e assertion is **supplementary**, and the acceptance
  criteria state explicitly that its skip does not satisfy anything.

**D8 — The 12 committed results are annotated in place by a follow-on issue in the queue repository, and quarantined by provenance here. Ranked.**

1. **Recommended — record a caveat here, file the annotation there.** This plan
   adds `docs/reports/2026-08-05-issue-1631-licensed-lane-result-audit.html`
   naming all 12 run IDs, their workflows, their validator verdicts, and the
   statement that none of them was gated. It then files a follow-on issue
   against `deckhand-licensed-runs-queue` to annotate or quarantine the result
   files themselves. Rationale: **this plan must not modify that repository**,
   and an audit record that lives in the repository that caused the defect is
   the durable artifact.
2. Re-run all 12 once fail-closed. Correct in principle, but it consumes the
   single OrcaWave/AQWA seat, requires ace-win-1 availability, and — for the two
   `FAIL` runs — is *expected to refuse*, which produces no new information that
   the audit does not already record. Recommend re-running only after the
   owner decides what to do about the `FAIL` physics (see Risks).
3. Delete them. Rejected: they are the only evidence of the defect.
4. Leave them untouched with no record. Rejected — this is the status quo the
   issue exists to end.

**D9 — Solver provenance is recorded positively, not inferred from absence.** The run-verdict sidecar records `solver_available: true|false`,
`solver_identity` (module name and version where the API exposes one), and
`host_kind: licensed|unlicensed`, determined at the moment of the solve attempt.
A consumer reads one field. This is what makes premise 6's requirement
("distinguishable without reading logs") true at the *result* level rather than
the log level.

**D10 — A completed run that produced no artifacts does not pass.** `lr_acma_ff132001b7ad` returned `returncode: 0` with no `returned_files` key at
all. The run contract will treat "verdict says completed, artifact list is
empty" as `ERROR`. This is a positive-evidence requirement of the same family as
[#1524](https://github.com/vamseeachanta/digitalmodel/issues/1524) and
[#1517](https://github.com/vamseeachanta/digitalmodel/issues/1517).

**D11 — No fitted constants.** Every value this plan introduces is a member of a
pre-existing closed vocabulary (`PASS`/`WARNING`/`FAIL`/`ERROR`/`SKIPPED`, from
`validation_runner`) or a process exit code chosen by convention (`0` success,
`1` refusal). No threshold is derived from the 12 results, the 7 validator
files, or any measurement taken in the course of writing this plan.

---

## Pseudocode

```text
# --- src/digitalmodel/run_contract.py (new; D2, D9, D10, D11) ---

REFUSING_VERDICTS = {"FAIL", "ERROR"}        # D3: reuses validation_runner's vocabulary
PASSING_VERDICTS  = {"PASS", "WARNING", "SKIPPED"}

RunVerdict:
    verdict            # one of the five; no other value may be constructed
    workflow
    solver_available   # D9: recorded, never inferred from absence
    solver_identity
    host_kind
    artifacts          # list of paths the run claims to have produced
    issues

    def is_refusal():
        if verdict in REFUSING_VERDICTS: return True
        # D10: a completed run that produced nothing is an ERROR, not a success
        if verdict in PASSING_VERDICTS and verdict != "SKIPPED" and not artifacts:
            return True
        return False

def write_run_verdict(verdict, output_dir):
    # structured sidecar, so no consumer ever regexes stdout_tail again
    write output_dir / "run_verdict.json"

def exit_status_for(verdict):
    return 1 if verdict.is_refusal() else 0

# --- src/digitalmodel/__main__.py (D2; premise 13) ---

def main():
    ... existing --help and CLI-routing branches, unchanged ...
    cfg = engine()
    verdict = run_contract.from_cfg(cfg)     # None when a workflow declares none
    if verdict is None:
        return 0                             # unchanged behaviour for un-migrated workflows
    write_run_verdict(verdict, verdict.output_dir)
    sys.exit(exit_status_for(verdict))

# --- diffraction/workflow.py::_run_solver (D1; premise 15) ---
# existing failed / unrequested-dry-run guards at :118-127 stay exactly as they are
    if status == "completed":
        exported = self._export_results_json(result, settings)
        ...
    settings["validation_verdict"] = result.validation_verdict   # <- the missing line
    settings["solver_available"]   = result.solver_available
    return cfg
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/run_contract.py` | D2/D9/D10/D11: the single verdict→exit mapping and the `run_verdict.json` sidecar. Deliberately outside `hydrodynamics/` and `solvers/` so it belongs to no live lane. |
| Modify | `src/digitalmodel/__main__.py:60-80` | D2 / premise 13: give the engine contract an exit path. Today `engine()`'s return value is discarded and the process always exits 0. |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/workflow.py:129-133` | D1 / premise 15: record `validation_verdict` and `solver_available` into `settings`. **Two added lines.** Coordination required — see Risks. |
| Modify | `src/digitalmodel/solvers/orcaflex/core/model_interface.py:156-188` | D5, **highest leverage, zero collected-test breakage**: drop `or not ORCFXAPI_AVAILABLE` from `:163`; keep the env-var opt-ins; the `NO_MODULE` `LicenseError` at `:182-188` becomes reachable. Also `:509-511` (`touch()`) and `:570-573` (`np.random.randn`) per D6. |
| Modify | `src/digitalmodel/solvers/orcaflex/run_to_sim.py:74-80,128` | D5: delete the `or not ORCAFLEX_AVAILABLE` inference; raise when neither a licence nor an explicit opt-in is present; stop reporting `sim_output` for a file never written; stop logging `Mode: REAL` for a mock run. |
| Modify | `src/digitalmodel/solvers/orcaflex/run_to_sim_cli.py:109-112,132-135` | D5 (path D): replace the auto-`args.mock = True` with a non-zero-exit error. |
| Modify | `src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:111-124,347-363` | D5/D6: delete the `except ImportError → mock_mode = True` flip; write mock artifacts to `*_mock/` with a `.mock` infix. |
| Modify | `src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:270-276` | D5/D6 (path E): the same forgery in the batch path. |
| Modify | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:1-4,55-68` | **D5a — the only change that touches the workflow the queue actually ran.** Stop printing-and-continuing; make `is_orcaflex_available()` feed a call-time raise. |
| Modify | `src/digitalmodel/solvers/orcaflex/opp.py` (`post_process_router`) | D5a: refuse at call time when the API is unavailable and no opt-in was given. |
| Create | `tests/test_run_contract.py` | Unit coverage for the mapping, the sidecar, and the closed vocabulary. |
| Create | `tests/test_main_exit_status.py` | The regression for premise 13, host-independent. |
| Create | `tests/solvers/orcaflex/test_mock_is_explicit.py` | D5/D5a/D6 coverage. `run_to_sim` and `run_models` have **zero** tests today. |
| Modify | `tests/hydrodynamics/diffraction/test_workflow_router.py` | D7: verdict-propagation and refusal tests, using the file's existing `run_solve`-patching pattern. |
| Modify | `tests/solvers/orcaflex/universal/test_universal_runner.py:225-230` | D6: `test_real_dat_files` currently asserts `sim_file.exists()` for forged `.sim` files, codifying the defect. Retarget to the `*_mock/` path. |
| Modify | `tests/solvers/orcaflex/*/scripts/run_*.py` (6 files), `scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py:78` | D5: pass the explicit mock flag; they construct `UniversalOrcaFlexRunner()` with no arguments. |
| Modify | `tests/solver/test_licensed_e2e_arbitrary_mesh.py` | D7: supplementary licensed assertion only. |
| Create | `docs/reports/2026-08-05-issue-1631-licensed-lane-result-audit.html` | D8: the durable record of all 12 results and their verdicts. |
| Update | `docs/plans/README.md` | index row |

**Explicitly untouched**, to stay clear of live lanes:
`src/digitalmodel/solvers/openfoam/` and `tests/workflows/`
([#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) is live in
both); every file under `src/digitalmodel/hydrodynamics/diffraction/` **except**
the two-line addition to `workflow.py`; `src/digitalmodel/engine.py` (the
verdict is read from `cfg` by the entrypoint, so the engine needs no change);
and the entire `deckhand-licensed-runs-queue` repository.

---

## TDD Test List

Every row states the expected value and why it is red on `origin/main` @ `0fdeea67`.

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_main_exits_non_zero_on_fail_verdict` | stubbed `engine()` returning a cfg with `validation_verdict: "FAIL"` | `SystemExit(1)` | measured 2026-08-05: `main()` returns `None`, no `SystemExit`, exit status **0** |
| `test_main_exits_zero_on_warning_verdict` | same, `"WARNING"` | exit `0` | D3 anti-inverse-defect guard; proves the gate can be green. Passes today for the wrong reason, so it is paired with the row above and both must move together |
| `test_main_exits_zero_when_workflow_declares_no_verdict` | cfg with no verdict | exit `0`, no `SystemExit(1)` | back-compat for un-migrated workflows; red once the exit path exists if the None case is mishandled |
| `test_router_records_fail_verdict_in_settings` | patched `run_solve` → `RunResult(status="completed", validation_verdict="FAIL")` | `cfg["diffraction"]["validation_verdict"] == "FAIL"` | the key is never written (`workflow.py:129-133`) |
| `test_router_records_pass_verdict_in_settings` | same, `"PASS"` | key present and `"PASS"` | same |
| `test_completed_solve_with_fail_verdict_refuses_end_to_end` | patched `run_solve`, full `main()` | exit `1` | **exercises the solver success path and the verdict refusal path together** (D7); no licence required |
| `test_completed_solve_with_pass_verdict_succeeds_end_to_end` | same, `"PASS"` | exit `0` | anti-vacuity partner of the row above |
| `test_completed_run_with_no_artifacts_is_error` | verdict `"PASS"`, empty artifact list | `is_refusal()` true, exit `1` | D10; reproduces `lr_acma_ff132001b7ad`, which is `returncode: 0` with no `returned_files` |
| `test_run_verdict_sidecar_is_written_and_parseable` | any completed run | `run_verdict.json` exists, parses, `verdict` ∈ the 5 values | no sidecar exists; the verdict is only in `stdout_tail` prose |
| `test_run_verdict_rejects_unknown_verdict_string` | `"OK"` | raises | no closed-vocabulary enforcement exists outside `validation_runner` |
| `test_sidecar_records_solver_availability_and_host_kind` | unlicensed host | `solver_available is False`, `host_kind == "unlicensed"` | D9; nothing records provenance today |
| `test_run_to_sim_raises_without_license_and_without_opt_in` | no `OrcFxAPI`, no flag | `LicenseError` | `run_to_sim.py:74-79` returns `success = True` |
| `test_run_to_sim_mock_still_works_when_explicitly_requested` | `mock_mode=True` | succeeds, marked mock | D4/D5 — proves the strict path did not remove unlicensed development |
| `test_universal_runner_does_not_infer_mock_from_importerror` | no `OrcFxAPI` | raises; `self.mock_mode` stays `False` | `universal_runner.py:111-121` flips it silently |
| `test_universal_runner_mock_artifact_path_is_distinct` | explicit mock | artifact under `*_mock/`, name contains `.mock` | `:347-361` writes to the real output dir |
| `test_model_interface_no_module_license_error_is_reachable` | no `OrcFxAPI`, no opt-in | `LicenseError(error_code="NO_MODULE")` | `:163` forces `use_mock`, so `:182-188` is dead code |
| `test_model_interface_env_opt_in_still_yields_mock` | `ORCAFLEX_FORCE_MOCK=1` | mock, no raise | D4 — the CI escape hatch survives |
| `test_mock_extract_results_does_not_return_random_numbers` | mock wrapper, `extract_results` | raises, or returns a clearly-marked mock object — never a bare array | `model_interface.py:570-573` returns `np.random.randn(100)` today |
| `test_mock_save_model_does_not_create_bare_artifact` | mock wrapper, `save_model` | no empty file at the real path | `:509-511` does `output_path.touch()` |
| `test_run_to_sim_cli_does_not_auto_enable_mock` | no `OrcFxAPI`, no `--mock` | non-zero exit | `run_to_sim_cli.py:109-112` flips `args.mock` and exits `0` |
| `test_batch_processor_mock_artifact_path_is_distinct` | explicit mock | artifact under `*_mock/` with `.mock` infix | `batch_processor.py:270-276` writes to the real path |
| `test_strength_post_refuses_without_license` | `orcaflex_post_process` basename, no `OrcFxAPI`, no opt-in | raises / non-zero exit | **D5a** — `orcaflex_utilities.py:1-4` prints and continues; this is the path behind `lr_acma_ff132001b7ad` and the three files the issue names would not have covered it |
| `test_import_of_orcaflex_package_still_succeeds_without_license` | import `digitalmodel.solvers.orcaflex` | imports cleanly, no raise | D5 hazard guard: `__init__.py:20-25,36-41` swallows import-time raises into `None` exports, so the refusal must be at call time and importing must stay safe |
| `test_licensed_solve_emits_verdict_and_provenance` *(skipif OrcFxAPI)* | real seat | verdict ∈ 5 values, `solver_available is True` | **supplementary only** — skips on every unlicensed host (measured: 6 skipped here) |

**Not included, deliberately:** no test asserting a `WARNING` run fails (D3 — it
must not); no test that retunes `_determine_overall_status` (D3/D11 — that would
fit a constant to the data being judged); no test asserting the queue repository
contains anything (out of scope, and it is another repository); no test that
excludes a file from both a baseline and an after-run, which proves nothing about
that file.

---

## Execution environment

- All non-`skipif` tests above run on this unlicensed Linux host. Verified
  toolchain, 2026-08-05: `uv run --no-sync python -m pytest <path> -q` — the
  repo `.venv` has **no `pytest`**, so a criterion citing `.venv/bin/pytest`
  would not execute. The `uv run` form is what Deckhand itself uses
  (`licensed_run_agent_runtime.py:37`).
- The licensed assertion requires ace-win-1, which is **currently up** (premise
  6, polled `2026-08-05T04:44:36Z`) but holds a single OrcaWave/AQWA seat. It is
  not a prerequisite for merging; see Acceptance Criteria.

---

## Acceptance Criteria

- [ ] **Every test above fails on `origin/main` @ `0fdeea67` and passes after**, with the RED list captured against a clean `origin/main` worktree and recorded in the PR body.
- [ ] **`python -m digitalmodel <input.yml>` exits non-zero for a completed run carrying a `FAIL` verdict.** Baseline measured 2026-08-05: exit status **0**, `main()` returns `None`, no `SystemExit`. The criterion is a non-zero status, demonstrated by a test that runs **without a licence**.
- [ ] **The same command still exits `0` for a `WARNING` verdict.** This is the inverse-defect guard and it is not optional: 5 of the 7 validated runs in the queue are `WARNING`, and a gate that refused them would be as useless as today's, while looking rigorous.
- [ ] **The gate is proven capable of both firing and not firing**, by the `FAIL`/`PASS` end-to-end pair. A gate observed only green, or only red, is not evidence.
- [ ] **`run_verdict.json` is written for every completed run, parses as JSON, and its `verdict` is one of exactly five strings.** A criterion satisfiable by an absent or empty sidecar is not a criterion, so the test asserts the file exists, is non-empty, and that an unknown verdict string raises.
- [ ] **An unlicensed host is distinguishable from a licensed one by reading one field of `run_verdict.json`** — `solver_available` — with no log parsing. Asserted on this host, where it must be `false`.
- [ ] **A completed run reporting zero artifacts refuses.** Reproduces `lr_acma_ff132001b7ad` (`returncode: 0`, no `returned_files` key).
- [ ] **Unlicensed development still works**, demonstrated by three passing tests, not by assertion: an explicitly requested `dry_run` still succeeds; an explicit `mock_mode=True` still succeeds; `ORCAFLEX_FORCE_MOCK=1` still yields mock. If any of these refuses, the plan has produced the inverse defect and must not merge.
- [ ] **No OrcaFlex path infers mock mode from import failure.** `grep -rn "or not ORCAFLEX_AVAILABLE\|or not ORCFXAPI_AVAILABLE" src/` returns nothing; the `except ImportError` at `universal_runner.py:118-121` no longer assigns `mock_mode`; and `run_to_sim_cli.py:109-112` no longer assigns `args.mock`. All five paths A–E, not the three named in the issue.
- [ ] **`orcaflex-strength-post` refuses on an unlicensed host.** This is called out separately because it is the workflow behind `lr_acma_ff132001b7ad` and it routes through **none** of the three modules the issue names (premise 10a). A green run of the other criteria with this one unmet means the demonstrated OrcaFlex defect is untouched.
- [ ] **Importing `digitalmodel.solvers.orcaflex` on an unlicensed host still succeeds.** `solvers/orcaflex/__init__.py:20-41` swallows import-time `ImportError` into `None` exports, so an import-time raise would degrade a loud refusal into a later `NoneType` error. The refusal must be at call time, and this criterion proves it.
- [ ] **A mock artifact cannot be mistaken for a real one**: distinct directory, `.mock` infix in the filename, and the in-band body marker — three independent signals, asserted separately so that removing one does not silently pass. Asserted for `universal_runner.py:358`, `batch_processor.py:272`, and `model_interface.py:509`.
- [ ] **Mock `extract_results` no longer returns `np.random.randn(100)` as engineering results**, and mock `save_model` no longer `touch()`es a bare artifact at the real path.
- [ ] `uv run --no-sync python -m pytest tests/hydrodynamics/diffraction/test_workflow_router.py tests/test_run_contract.py tests/test_main_exit_status.py -q` passes. Baseline for the first file today: **15 passed in 0.38s**.
- [ ] **The full suite is compared node-ID by node-ID** against a baseline captured in the same worktree at the branch point. No new failure node IDs, and **no test excluded from both sides** — symmetric exclusion proves nothing about the excluded file and hid two real regressions on [#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575).
- [ ] **The licensed e2e assertion is supplementary and its skip satisfies nothing.** On an unlicensed host it reports `skipped` (measured: 6 skipped, 2026-08-05) and **no criterion above may be met by that skip**. Merging does not require a licensed run; the owner may separately request one.
- [ ] **The audit report names all 12 run IDs, their workflows, and their validator verdicts**, and states that none was gated. A report covering fewer than 12 does not satisfy this.
- [ ] **A follow-on issue is filed against `deckhand-licensed-runs-queue`** for annotating or quarantining the committed results. **No file in that repository is modified by this work.**
- [ ] The implementing agent will post a summary comment on [#1631](https://github.com/vamseeachanta/digitalmodel/issues/1631), and will not push to `main`, merge, or close the issue unless separately authorized.
- [ ] r1 review artifact recorded.
- [ ] **The legal-scan criterion is deliberately absent.** `scripts/legal/legal-sanity-scan.sh` does not exist in this repository (verified 2026-08-05), and the workspace-hub `--repo=` form is fail-open under workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804). The irony is worth stating plainly, because it is the same defect class this plan exists to fix: **a check that returns success without checking**. Citing it as evidence here would reproduce, inside the remedy, the exact failure being remedied.

---

## Out of scope

- **Retuning `_determine_overall_status`.** The `FAIL`/`WARNING` boundary is a substring heuristic (`negative`, `missing`). Changing it while judging it against the data it produced would be fitting a constant to its own evidence (D11). Filed as an open question instead.
- **The physics of the two `FAIL` runs** — added-mass and damping matrices asymmetric at all 33 frequencies, negative added-mass diagonals, roll RAO 358.26 deg/m. Those are real solver or input defects and belong to [#1640](https://github.com/vamseeachanta/digitalmodel/issues/1640)'s remediation lanes. This plan makes them *visible*; it does not diagnose them.
- **The `orcaflex-strength-post` crash itself** — [#885](https://github.com/vamseeachanta/digitalmodel/issues/885) owns it. This plan only ensures it can no longer report success.
- **The public release and Hugging Face publication path** — [#1604](https://github.com/vamseeachanta/digitalmodel/issues/1604) owns that.
- **Re-running the 12 cases.** D8 option 2; recommended only after the owner rules on the `FAIL` physics.
- **Any modification to `deckhand-licensed-runs-queue` or to Deckhand.** Read-only throughout.
- **The remaining ~14 print-and-continue modules** (`OrcaFlexAnalysis.py:9`, `mooring.py:12`, `orcaflex_parallel_analysis.py`, `orcaflex_optimized_parallel*.py`, `comprehensive_benchmark.py`, `pipeline_schematic.py`, `post_results/postProcess.py`, `preprocess/load_vessel.py:53`, and the `examples_integration/` converters). Inventoried in Resource Intelligence and worth a follow-on. `orcaflex_utilities.py` and `opp.py` are **not** in this exclusion — they are in scope via D5a, because they are on the queued workflow's path.
- **`tests/workflows/` and `docs/registry/workflows.yaml`.** The registry already excludes `orcaflex-strength-post` from CI via `runtime: requires-license` (premise 20). `tests/workflows/` is a live lane ([#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968)), so this plan neither relies on changing that skip nor touches it.
- **Deselecting the `solver` marker in CI, or adding `-m "not solver"` to any workflow.** Premise 21 notes no workflow does this today; changing CI selection is a separate decision with its own blast radius.

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude — inline, main session | **MAJOR** — 7 findings, all folded in |

1. **The plan originally accepted the issue's causal story.** Had the mock-mode
   mechanisms been fixed first, all 12 results would still have reported
   success, and the issue would have been closed on evidence it never addressed.
   → Premise 10 re-measured and refuted; D1 inverts the ordering.
2. **The first draft's gate refused `WARNING`.** That would refuse 5 of the 7
   validated runs — rigorous-looking and useless. → D3, plus a dedicated
   `WARNING`-exits-0 criterion and three unlicensed-development criteria.
3. **The `FAIL`/`WARNING` boundary looked like a fitted constant.** It is chosen
   from a distribution this plan measured. → Traced to
   `_determine_overall_status` (#611/#625), which predates the data; D11 states
   the rule and the Risks section owns the inherited weakness rather than hiding
   it.
4. **A criterion cited `.venv/bin/pytest`, which does not exist here.** The repo
   venv has no `pytest`; that criterion could not have executed. → Replaced with
   the verified `uv run --no-sync python -m pytest` form, matching Deckhand's own
   invocation.
5. **The verification rested on a `skipif`-gated licensed test.** On this host it
   reports 6 skipped, so the whole gate would have been "proven" by nothing. →
   D7: the primary tests patch `run_solve` to inject a *completed* solve, so they
   exercise the real success path and the refusal path together, on any host —
   including one where `OrcFxAPI` imports fine, which is the assertion that would
   otherwise have passed vacuously.
6. **Two criteria were satisfiable by an empty result** — "a sidecar is written"
   and "no new test failures". → The sidecar criterion now asserts non-empty,
   parseable, and closed-vocabulary; the suite criterion now forbids symmetric
   exclusion and requires node-ID comparison.
7. **The plan did not say what happens to the 12 committed results.** → D8, with
   ranked options, a named audit artifact, and a follow-on issue in the correct
   repository.
8. **The plan inherited the issue's three-file fix list without tracing the call
   graph.** `orcaflex-strength-post` — the only OrcaFlex workflow in the queue,
   and the one that returned no artifacts — routes through
   `orcaflex.py` → `opp` / `orcaflex_utilities`, touching none of the three.
   Working the issue as written would have produced a green PR that left the
   demonstrated OrcaFlex defect in place. → Premise 10a, D5a, a dedicated test
   row, and a dedicated acceptance criterion. Two further inferred-mock surfaces
   (`run_to_sim_cli.py:109-112`, `batch_processor.py:270-276`) were found by the
   same trace.
9. **A fail-closed raise placed at import time would have been swallowed.**
   `solvers/orcaflex/__init__.py:20-41` degrades failed subimports to `None`, so
   an import-time raise converts a loud refusal into a `NoneType` error far from
   the cause. → D5 states call-time explicitly, with a test and a criterion that
   importing the package on an unlicensed host still succeeds.

**Verdict: ready for owner review.** No blockers outstanding.

---

## Risks and Open Questions

- **Open question for the owner — is `FAIL` the right refusal boundary, given how it is computed?** `_determine_overall_status` returns `FAIL` only when an
  issue string contains `negative` or `missing`. Under that rule, "added-mass
  matrix not symmetric at all 33 frequencies" scores as `WARNING`, not `FAIL` —
  systematic non-physicality that this gate would let through. This plan
  deliberately does not change it (D11). Tightening it is a defensible stricter
  choice and is the owner's call; it should be its own issue with its own
  evidence.
- **Risk — the two-line change to `hydrodynamics/diffraction/workflow.py` is in a directory this session was told to stay clear of.** Nothing else in this plan
  touches diffraction. Before implementing, the agent must run
  `git worktree list`, check in-flight branches for that file, and confirm no
  live lane owns it; if one does, the two lines wait rather than race. The
  alternative seam — having the entrypoint discover `*_validation.json` by
  globbing the output directory — avoids the file entirely but is path-guessy
  and would re-derive a verdict the runner already holds typed. It is the
  fallback, not the design.
- **Risk — `test_orcaflex_converter_enhanced.py` is the one place with real test fallout.** Seven tests construct with `use_mock=False` and then branch on the
  *mutated* flag; `:154` is literally
  `assert stats['failed'] == 0 or converter_real.use_mock  # Allow failures in mock mode`.
  Under fail-closed they raise at construction. They are `pytest.skip`-guarded on
  a sample corpus, so they may already be skipping in CI — **the implementer must
  measure whether that corpus resolves in CI before assuming no breakage.**
  Assuming it skips is exactly the "absence of signal reads as success" trap this
  plan exists to close.
- **Risk — behaviour change for existing consumers.** Any caller of
  `python -m digitalmodel` that today treats exit 0 as "ran" will begin seeing
  exit 1 for `FAIL` runs. That is the intent. Un-migrated workflows are
  unaffected because a workflow that declares no verdict still exits 0.
- **Unverified — the `run_verdict.json` sidecar's fit with the queue's result schema.** The queue README constrains results to "audit + summary only" and
  says heavy artifacts stay host-local. The sidecar is small and metadata-only,
  so it should qualify, but whether Deckhand should *surface* it into
  `queue/results/<run_id>.json` is a Deckhand-side decision this plan does not
  make.
- **Unverified — AQWA parity.** `aqwa_runner.py` imports the same
  `validation_runner` (`:46`) and the router treats both solvers through one
  code path, so verdict propagation should cover AQWA identically. This was
  confirmed by import and by the shared router, **not** by executing an AQWA
  solve, which needs the licensed host.
- **Cannot be verified without a licensed host:** that a *real* OrcaWave or AQWA
  solve populates `validation_verdict` end-to-end on ace-win-1; that
  `solver_available` reports `true` anywhere; and that the two `FAIL` cases
  actually refuse when re-run. Everything else — the exit path, the mapping, the
  sidecar, verdict propagation through the router, the empty-artifact rule, and
  all three mock-mode fixes — is verifiable on an unlicensed Linux host, and the
  TDD list is constructed so that it is.
- **BLOCKER — approval:** [#1631](https://github.com/vamseeachanta/digitalmodel/issues/1631)
  has no approved-plan evidence until the owner explicitly approves this plan.
  No agent may create that state.

---

## Complexity: T3

One new module, nine modified source modules across two subsystems, three new
test files, four modified test files, seven uncollected helper scripts, and a
cross-repository follow-on. The change alters the exit-status contract of the
package's primary entrypoint, which every consumer of `python -m digitalmodel`
observes. Not T2: the blast radius is the entrypoint contract itself, the fix
spans the solver lane and the hydrodynamics lane, and coordination is required
both with a live lane and with a second repository. The implementation is
naturally two independently reviewable increments — the verdict/exit lane (D1–D3,
D9, D10) and the mock lane (D5, D5a, D6) — and the owner may approve them
separately; the verdict lane alone is what turns the 12 drained runs red.
