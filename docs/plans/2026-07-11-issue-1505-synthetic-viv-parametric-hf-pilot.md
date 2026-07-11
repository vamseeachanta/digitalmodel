# Plan: digitalmodel #1505 — Public synthetic VIV parametric algorithm-run pilot to Hugging Face

> **Status:** adversarial-reviewed (r1 BLOCK remediated; ready for user review)
> **Complexity:** T3
> **Date:** 2026-07-11
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1505
> **Parent:** https://github.com/vamseeachanta/workspace-hub/issues/3427 (repository-linked algorithm-run datasets)
> **Blocked-by:** https://github.com/vamseeachanta/workspace-hub/issues/3433
> **Client:** N/A
> **Project:** Repository-linked algorithm-run datasets
> **Lane:** lane:claude
> **Execution mode:** single-lane; PR branch is the documented exception to main-only execution
> **HF target:** `aceengineer/digitalmodel-runs` (dedicated digitalmodel dataset — NEVER a combined domain-run store)
> **Review artifacts:** `scripts/review/results/issue-1505-round-1/2026-07-11-plan-1505-{claude,codex,gemini,disagreement}.md` (current round; outputs never inputs — promote only after fanout exits)
> **Artifact timing:** current-round provider files are outputs; zero/missing files observed by a provider during its own run are not evidence

---

## Resource Intelligence Summary

Issue #1505 is OPEN at `status:needs-plan`, gated AFK-capable-only after its own
adversarially reviewed plan **and** explicit user approval. It is the single
end-to-end public synthetic `digitalmodel` pilot that must prove the whole
run-ledger contract established by parent workspace-hub#3427. The owner comment
(2026-07-10) re-states the hard constraints: dedicated `digitalmodel` HF dataset
only, source repo remains authority for code/descriptors/schemas/tests/report, the
pilot must **crosswalk** the landed `digitalmodel.workflow_api` runner/provenance/
golden harness (evidence, not re-create/alias), and the parent plan does NOT
authorize this implementation.

### Sources consulted (concrete)

1. **Issue #1505 body + owner comment** (`gh issue view 1505`, captured
   2026-07-11) — supplies the ten acceptance criteria copied verbatim below and the
   parent-contract dependency. `status:needs-plan`; blocked-by workspace-hub#3433.

2. **`src/digitalmodel/orcaflex/viv_screening.py`** (checkout SHA
   `9a458b51696c43dba8004c3053f2076a168d40b3`) — the **selected pilot algorithm**.
   Confirmed public-safe:
   - Module docstring: *"Does NOT require OrcFxAPI — all calculations are
     analytical."* Cites only public standards: DNV-RP-C205 (2019) §9, DNV-RP-F105
     (2017) §4/§5, Blevins (1990). No client/licensed/pointer-only source.
   - Pure functions with typed pydantic I/O: `strouhal_number(reynolds)`;
     `VIVScreeningInput` (props `reynolds_number`, `st`, `vortex_shedding_frequency`,
     `stability_parameter` Ks = 2·m*·ζ, `check_reduced_velocity(fn)` → Vr = V/(fn·D)
     + cross-flow lock-in 4≤Vr≤8 / in-line 1≤Vr≤3.5 flags); `BeamProperties`
     (`natural_frequency(mode)`, `natural_frequencies(n)` per DNV-RP-F105 §4.3);
     `viv_screening(viv_input, beam, n_modes)` → `VIVScreeningResult`
     (`screening_pass`, `critical_mode`, `critical_vr`, per-mode `details`);
     `estimate_response_amplitude(Vr, D, Ks)` → **A/D amplitude ratio** (DNV-RP-C205
     Fig 9-3, returns 0.0 outside 4≤Vr≤8, exp(-0.15·Ks) peak decay).
   - **Determinism**: no time, no RNG, no filesystem, no network, no OrcFxAPI. Inputs
     are floats; outputs are rounded floats/bools. This is exactly the pure,
     standards-based, redistribution-free calc Gate A public admission needs.
   - Gap: `viv_screening.py` is a **standalone module — it is NOT wired into the
     engine router** (see source 4). The pilot must add a thin router arm.

3. **`docs/registry/workflows.yaml`** (`schema_version: 2`, deckhand versioned
   routing) — a row carries `id`/`basename`/`title`/`input`/`outputs`/`test`/
   `runtime`, optionally `version`/`status`/`latest` (absence = v1/stable/latest) and
   a `result:` block (e.g. `kind: files`). Reference rows read verbatim:
   - `buckling-parametric` (line 1245): `basename: buckling_parametric`, three
     `results/` outputs (`input.yml` save_cfg dump, byte-stable `results.json` with
     `meta.generated_at` omitted, rounded `cases.csv`), `result.kind: files`,
     `runtime: offline`. This is the closest determinism-golden precedent.
   - `viv-parametric` (line 292): `basename: parametric_run`, sweeps the **heavier
     legacy `viv_analysis`** base — NOT `viv_screening.py`. So the pilot cannot reuse
     it; it needs its own clean base + `viv-parametric-screening` id.

4. **`src/digitalmodel/engine.py`** — the basename→router dispatch (`elif basename
   == "cathodic_protection": ...`). Confirmed **no `viv_screening` arm exists**; the
   only VIV path is `basename == "viv_analysis"` → `subsea.viv_analysis.VIVAnalysis`
   (legacy, heavier). `src/digitalmodel/workflows/parametric_run.py::router` is the
   sweep harness: reads `parametric_run.base_input`, factorial `variants`, writes
   `case_{index}.yml` + a `cases.csv` manifest.

5. **`src/digitalmodel/workflow_api/{runner,provenance,golden}.py` +
   `__init__.py`** — the landed harness the pilot **crosswalks**:
   - `runner.run_workflow(workflow_id, params, cfg, verify_reproducible)` → typed
     `assetutilities.workflow_api.ResultEnvelope`; resolves a BARE in-repo id against
     `docs/registry/workflows.yaml`, drives `digitalmodel.engine.engine(embed=True,
     root_folder=<tmp>, log_to_file=False)` side-effect-free, stamps
     `code_version("digitalmodel")`, lifts DNV/API `citations` sidecar →
     `provenance.standard_revisions`. Fail-closed: unknown id → `status="error"`
     envelope, never raises. `determinism.result_hash` is the #3282 content hash.
   - `provenance.stamp_provenance(...)` delegates to `make_provenance` (#3282 shape;
     never re-hashes).
   - `golden.golden_workflow_test(id, golden_path, verify_reproducible)` — verdict is
     **string equality of `determinism.result_hash`** vs a committed golden;
     `GOLDEN_VOLATILE_KEYS` prunes git_sha/package_version/data_as_of/reproducible by
     NAME; `REGEN_GOLDENS=1` rewrites + SKIPs (owner re-sanction gate).
   - Goldens live in `tests/workflow_api/goldens/*.json` ({result, result_hash,
     standard_revisions}); tests in `tests/workflow_api/test_run_workflow_*.py` use
     `golden_helpers.load_golden` and assert `env.determinism["result_hash"] ==
     golden["result_hash"]` + per-file sha256.

6. **assetutilities `workflow_api` modules the pilot CONSUMES** (landed on the
   assetutilities side per parent #3427; must be on `main` before execution —
   **cited by PR, not yet content-verified here because assetutilities is not in this
   checkout**, see Risks):
   - **#111 `identity`** — `algorithm_version_id` (clean SHA-bound Algorithm Version)
     + deterministic `run_id`, pinned canonicalization.
   - **#112 `artifact`** — content-addressed artifact store.
   - **#113 `inputs`** — public-admission Gate A (complete/canonical/hashed/
     schema-valid/publicly-replayable).
   - **#114 `output_contract` + `report`** — curated native outputs, rolling HTML
     report, `output_equality_digest`.
   - **#115 `metrics`** — algorithm-scoped metrics (definitions/units/derivations/
     quality).
   - **#116 `publication`** — `RunProjection`, promotion state machine
     (emitted→…→accepted), real `HfPort`, source-repo `publications.jsonl` ledger.
   - Parent run-ledger contract workspace-hub#3452 is already merged (per task
     brief); this pilot is the first consumer.

Engineering calculation standard IS applicable and public: DNV-RP-C205 §9 /
DNV-RP-F105 §4-5 / Blevins (1990). All are cited, none redistributed — only the
citation strings and synthetic numeric inputs enter the dataset.

---

## Deliverable

One published, immutable Hugging Face dataset revision under
`aceengineer/digitalmodel-runs` projecting **≥3 meaningful VIV-screening parameter
variations + 1 exact replay** of the synthetic `viv-parametric-screening` workflow,
with the source repo retaining a single rolling HTML report (mandatory Inputs +
Outputs sections, links to the exact dataset revision), a committed determinism
golden, complete/canonical/hashed replayable inputs, curated native outputs,
algorithm-scoped metrics, and a passing clean-room replay — all with legal + secret
scans clean (no client identifiers, no local absolute paths).

---

## Design

### D1 — Wire the pure algorithm into a single dedicated, embed-aware router

`viv_screening.py` is not routed. Add ONE thin, side-effect-clean adapter
`src/digitalmodel/orcaflex/viv_screening_workflow.py` — the sole router for this
pilot — modeled **exactly** on `structural/buckling_workflow.py::BucklingParametricWorkflow`
(the byte-stable determinism-golden precedent), plus one `elif basename ==
"viv_parametric_screening":` arm in `engine.py`. The router:
- reads synthetic `VIVScreeningInput` + `BeamProperties` fields (D, V, span, etc.)
  from `cfg`,
- calls `viv_screening(...)` and `estimate_response_amplitude(Vr, D, Ks)`
  (**note**: `estimate_response_amplitude`'s `outer_diameter` argument is unused in
  the function body — A/D depends only on Vr and Ks — so do not rely on D affecting
  the A/D output directly; D moves the result only through `VIVScreeningInput`'s
  `check_reduced_velocity`/Re/St),
- attaches a DNV-RP-C205 / DNV-RP-F105 `citations` sidecar (so
  `runner._standard_revisions_from_payload` lifts them into provenance),
- resolves the output directory **exactly like `BucklingParametricWorkflow._result_dir(cfg)`**:
  `Analysis.result_folder` if present else `cfg["_config_dir_path"]/"results"`, which
  `runner.configure_embed` rebases onto the injected throwaway `root_folder`. This is
  the load-bearing fix (see Adversarial Review Summary / M1): the router must be
  embed/`root_folder`-aware so `run_workflow._run_once` writes only into the sandbox it
  rmtrees, keeping the run side-effect-free and letting `extract_result(..., root=tmp)`
  find the files for the golden. **It must NOT use `workflows/parametric_run.py`**,
  whose `_resolve_output_path` hardcodes `REPO_ROOT/results/parametric_run/…` and is
  NOT embed-aware.
- writes native `results.json` via a `write_outputs(..., timestamp=None)`-style call
  (byte-stable: `meta.generated_at` omitted, floats rounded exactly as the pydantic
  models already round) + a `cases.csv`.

Native engineering schema (Vr, f_n, Re, St, Ks, A/D, lock-in flags, screening_pass,
critical_mode) is **retained in full** in `results.json`; only curated artifacts
publish (D4).

### D2 — Single registered workflow `viv-parametric-screening`; variations are EXTERNAL runs

ONE new workflow dir `examples/workflows/viv-parametric-screening/input.yml`
(synthetic single-case base), registered in `workflows.yaml` (`schema_version 2`,
`version: 1`, `status: stable`, `latest: true`, `runtime: offline`,
`result.kind: files`) whose `basename: viv_parametric_screening` dispatches to the
D1 dedicated router. **There is no second workflow and no `parametric_run` row** —
the `parametric_run` harness is dropped entirely (it is not embed-aware, see M1).

The "parametric" sweep is produced by **EXTERNAL** calls, exactly as the pseudocode
shows: `run_workflow("viv-parametric-screening", params=variant)` once per variant,
each supplying distinct synthetic `params` (D, V, span → distinct canonical inputs →
distinct `run_id`). This is the single, unambiguous mechanism (no internal-factorial
alternative). Each call runs one synthetic tubular member; a case's `params` vary:
- **outer diameter** D (e.g. 0.2032, 0.273, 0.3556 m),
- **current velocity** V (e.g. 0.6, 0.8, 1.0 m/s),
- **modal / natural frequency** driver (via `BeamProperties.length`/`effective_tension`
  so f_n moves, e.g. spans 40/60/80 m).

**Pinned meaningful cases (≥3, chosen so equality is demonstrable, not accidental).**
The full published grid is meaningful (15/27 cases enter cross-flow lock-in). The
plan pins at least these three, spanning both the lock-in and the suppressed regime:
- **C-lockin:** D=0.2032 m, V=1.0 m/s, span=60 m → Vr in [4,8], **A/D ≈ 0.97**
  (non-zero, cross-flow lock-in; `lock_in=True`).
- **C-mid:** D=0.273 m, V=0.8 m/s, span=60 m → intermediate Vr, non-zero A/D.
- **C-suppressed:** D=0.3556 m, V=0.6 m/s, span=40 m → Vr outside [4,8],
  **A/D = 0.0** (`estimate_response_amplitude` suppressed; `lock_in=False`).
The exact-replay case (D3) re-runs **C-lockin** so a NON-ZERO-A/D result is what the
equality digest compares — a degenerate all-zero comparison cannot pass by accident.

Each case emits A/D amplitude ratio + cross-flow lock-in flag + a **fatigue-proxy
metric** (e.g. cycle-rate·(A/D)^m surrogate, defined with units/derivation in the
metrics module — an illustrative screening proxy, NOT a certified fatigue life). The
external-runs model keeps each `results.json`/`cases.csv` compact + byte-stable for
the golden (mirroring buckling-parametric's `timestamp=None` recipe).

### D3 — Identity, ≥3 variations + exactly 1 exact replay

- Build `algorithm_version_id` from the SHA-bound Algorithm Version (assetutilities
  `identity` #111) over `viv_screening.py` + descriptor.
- Emit ≥3 distinct-parameter runs (distinct canonical inputs → distinct `run_id`s).
- Emit **exactly one exact replay**: re-run one prior case (the **C-lockin**,
  non-zero-A/D case from D2) from its published canonical inputs; assert it
  **resolves to the SAME `run_id`** and passes `output_equality` (via
  `output_contract.output_equality_digest`). **Any mismatch BLOCKS publication** —
  the promotion state machine must not reach `accepted`.
- **run_id dependency (hard):** today's `runner.run_workflow` has NO `run_id`
  concept — it stamps `workflow_id`/provenance/`determinism.result_hash` only.
  Same-`run_id` (AC3) is provided entirely by wiring in assetutilities `identity`
  #111 (`derive_run_identity`/`run_id`). That signature must be confirmed on `main`
  before the AC3 tests can be written (see Sequencing pre-req and TDD RED gate).
- **Equality artifact set excludes volatile provenance (m2):** `runner.run_workflow`
  auto-stamps `provenance.data_as_of = utc_now_iso()` — a wall-clock value that
  changes every run. The determinism golden already prunes it by name via
  `GOLDEN_VOLATILE_KEYS`, but the `output_equality` / curated artifact set is
  **explicitly limited to the timestamp-free `results.json` + `cases.csv`**;
  `provenance` (and `data_as_of` in particular) is NOT part of the equality digest.
  Exact-replay equality is asserted over that timestamp-free set only.

### D4 — Inputs / outputs / metrics / publication

- **Inputs** (assetutilities `inputs` #113): each case's inputs are complete,
  canonical, hashed, schema-valid, publicly replayable. Gate A public admission runs
  here — synthetic numbers + public DNV citations pass; any dirty/licensed/
  pointer-only field fails admission and BLOCKS.
- **Outputs** (`output_contract` #114): native `results.json` retains full
  engineering schema; only curated artifacts (curated CSV/JSON + metrics) project to
  the dataset. `output_equality_digest` is the exact-replay verdict.
- **Metrics** (`metrics` #115): algorithm-scoped — A/D ratio (dimensionless,
  derivation DNV-RP-C205 Fig 9-3), lock-in flag (bool, Vr∈[4,8]), fatigue-proxy
  (units + derivation + quality note "screening surrogate").
- **Publication** (`publication` #116): `RunProjection` → promotion state machine
  emitted→…→accepted; **real `HfPort`** publishes to `aceengineer/digitalmodel-runs`
  at a verified immutable revision (commit SHA pinned); a `publications.jsonl` ledger
  row is appended in the source repo.

### D5 — Rolling report + clean-room replay

- ONE rolling HTML report in `digitalmodel` (e.g.
  `docs/reports/viv-parametric-screening/index.html` via `report` #114) with
  MANDATORY **Inputs** and **Outputs** sections, the applicable metrics/comparisons,
  and links to the **exact** dataset revision (pinned HF SHA).
- **Clean-room replay**: a fresh checkout, from the PUBLISHED inputs only,
  reproduces the accepted outputs (result_hash + per-artifact sha256 match). This is
  a CI-runnable test, not a manual step.

---

## Pseudocode

```text
# D1 router — SINGLE dedicated router, modeled on BucklingParametricWorkflow.
# NOT parametric_run (that hardcodes REPO_ROOT/results/... and is not embed-aware).
# basename == "viv_parametric_screening"; engine embed path owns tmp root + rmtree.
def viv_parametric_screening_router(cfg):
    vin  = VIVScreeningInput(**cfg["viv_parametric_screening"]["input"])
    beam = BeamProperties(**cfg["viv_parametric_screening"]["beam"])
    res  = viv_screening(vin, beam, n_modes=cfg["viv_parametric_screening"].get("n_modes", 10))
    crit = res.details[ (res.critical_mode or 1) - 1 ]
    a_d  = estimate_response_amplitude(crit["reduced_velocity"],
                                       vin.outer_diameter, vin.stability_parameter)  # D unused in body
    fatigue_proxy = crit["vortex_shedding_freq_Hz"] * (a_d ** M_EXP)   # screening surrogate
    payload = {"a_d_ratio": a_d, "lock_in": not res.screening_pass,
        "critical_mode": res.critical_mode, "critical_vr": res.critical_vr,
        "fatigue_proxy": round(fatigue_proxy, 6), "modes": res.details,
        "citations":[{"code_id":"DNV-RP-C205","publisher":"DNV","revision":"2019","section":"9"},
                     {"code_id":"DNV-RP-F105","publisher":"DNV","revision":"2017","section":"4-5"}]}
    # embed-aware output dir, exactly like BucklingParametricWorkflow._result_dir(cfg):
    out_dir = Path(cfg.get("Analysis",{}).get("result_folder")
                   or (Path(cfg["_config_dir_path"]) / "results"))   # rebased to tmp root_folder
    write out_dir/"results.json" (meta.generated_at omitted; timestamp=None) + out_dir/"cases.csv"
    cfg["viv_parametric_screening"] = {"result_folder": str(out_dir), ...}
    return cfg

# D3 variations (EXTERNAL single-case runs — the ONLY sweep mechanism) + exact replay
runs = [ run_workflow("viv-parametric-screening", params=variant) for variant in VARIANTS ]  # >= 3
replay = run_workflow("viv-parametric-screening", cfg=canonical_inputs_of(runs[0]))  # runs[0] = C-lockin
assert identity.run_id(replay) == identity.run_id(runs[0])            # same run_id (needs #111 identity)
# equality over timestamp-free results.json + cases.csv ONLY (provenance/data_as_of excluded):
assert output_contract.output_equality(replay, runs[0])              # else BLOCK publication

# D4 publish (execution-time; owner-gated; WRITE-scope token)
proj = publication.RunProjection.from_runs(runs, algorithm_version_id)
state = publication.promote(proj)          # emitted -> ... -> accepted (blocks on any equality/admission fail)
rev  = publication.HfPort.publish("aceengineer/digitalmodel-runs", proj)   # immutable revision
append publications.jsonl {algorithm_version_id, run_ids, hf_repo, hf_revision, accepted_at}

# D5 clean-room replay (CI)
inputs = hf_download(rev)/inputs           # published inputs only
assert run_workflow(cfg=inputs).determinism["result_hash"] == accepted_result_hash
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/orcaflex/viv_screening_workflow.py` | SINGLE dedicated, embed-aware router wrapping the pure `viv_screening.py`, modeled exactly on `structural/buckling_workflow.py` (output dir from `Analysis.result_folder`/`_config_dir_path`; `timestamp=None`; native schema + DNV citations sidecar). Does NOT use `workflows/parametric_run.py` |
| Modify | `src/digitalmodel/engine.py` | add `elif basename == "viv_parametric_screening":` dispatch arm (the sole VIV-screening route) |
| Create | `examples/workflows/viv-parametric-screening/input.yml` | synthetic single-case base input (`basename: viv_parametric_screening`; all numbers synthetic; DNV citations only). Variations are external `run_workflow(..., params=variant)` calls — no factorial/parametric_run block |
| Create | `examples/workflows/viv-parametric-screening/README.md` | run instructions (match cathodic-protection README shape) |
| Modify | `docs/registry/workflows.yaml` | register ONLY `viv-parametric-screening` (schema_version 2 row; `basename: viv_parametric_screening`; `result.kind: files`). No `parametric_run`-backed row |
| Create | `config/publication/viv-parametric-screening.yml` (`publication.yml`) | dataset target `aceengineer/digitalmodel-runs`, curated-artifact allowlist, metric defs, report path — all config externalized to YAML |
| Create | `tests/workflow_api/goldens/viv_parametric_screening.json` | committed determinism golden (result_hash + per-file sha256) |
| Create | `tests/workflow_api/test_run_workflow_viv_screening.py` | envelope + reference-golden + exact-replay-equality tests |
| Create | `tests/workflow_api/test_viv_screening_publication.py` | admission-fail, metrics-definition, clean-room-replay, promotion-block tests |
| Create | `docs/reports/viv-parametric-screening/index.html` | ONE rolling HTML report (mandatory Inputs+Outputs, links to pinned HF revision) |
| Create | `scripts/review/results/issue-1505-round-1/2026-07-11-plan-1505-{claude,codex,gemini,disagreement}.md` | revision-isolated plan reviews |
| Update | `../workspace-hub/docs/plans/README.md` | mandatory central plan index row (separate commit; do NOT mix into the dm PR) |

No client data, no licensed OrcaFlex path, no local absolute path enters any file.

---

## TDD Test List

Every acceptance criterion maps to a failing-first test.

| Test | Current RED | Final GREEN | AC |
|---|---|---|---|
| `test_algorithm_is_public_synthetic` | no admission assertion | inputs carry only synthetic numbers + DNV citations; `inputs` admission passes | AC1 |
| `test_dirty_input_fails_admission` | none | an input with a licensed/pointer-only/absolute-path field is REJECTED by admission (BLOCKS) | AC1, AC10 |
| `test_at_least_three_variations_plus_one_replay` | workflow absent | ≥3 distinct-input runs (the pinned C-lockin **A/D≈0.97**, C-mid non-zero, C-suppressed **A/D=0.0**) + exactly 1 exact replay of C-lockin emitted; assert C-lockin `lock_in=True`/non-zero A/D and C-suppressed `lock_in=False`/A/D==0.0 so runs are meaningfully comparable, not accidentally equal | AC2 |
| `test_exact_replay_same_run_id_and_equality` | none | replay of the non-zero-A/D C-lockin case resolves to SAME `run_id` (via `identity` #111 `derive_run_identity`) and passes `output_equality` over the timestamp-free set | AC3 |
| `test_provenance_data_as_of_excluded_from_equality_digest` | none | `provenance`/`data_as_of` (volatile `utc_now_iso()`) is NOT in the `output_equality`/curated digest; two runs of identical inputs at different wall-clock times still pass exact-replay equality (digest limited to timestamp-free `results.json` + `cases.csv`) | AC3, AC5 |
| `test_equality_mismatch_blocks_publication` | none | a perturbed replay fails equality → promotion never reaches `accepted` | AC3 |
| `test_inputs_complete_canonical_hashed_schema_valid_replayable` | none | inputs module validates all five properties | AC4 |
| `test_outputs_retain_native_schema_only_curated_publish` | none | `results.json` full schema retained; only curated allowlist projects | AC5 |
| `test_metrics_have_definitions_units_derivations_quality` | none | A/D, lock-in, fatigue-proxy each carry the four metric fields | AC6 |
| `test_viv_parametric_screening_reference_golden` | golden absent | `run_workflow(..., verify_reproducible=True)` result_hash == committed golden + per-file sha256 | AC2, AC5 |
| `test_dataset_projection_publishes_immutable_revision` (mocked HfPort in CI; real port execution-only) | none | `RunProjection` publishes at a verified immutable revision to `aceengineer/digitalmodel-runs` | AC7 |
| `test_rolling_report_has_inputs_outputs_and_revision_links` | report absent | HTML has Inputs+Outputs sections, metrics/comparisons, links to exact dataset revision | AC8 |
| `test_clean_room_replay_reproduces_accepted_outputs` | none | replay from published inputs only == accepted result_hash + sha256 | AC9 |
| `test_no_client_identifiers_or_absolute_paths` | none | legal + secret scan over all new files finds none | AC10 |

Tests are written first; each RED row is captured failing against the current
checkout before the workflow/router/publication code lands.

**HARD RED-phase pre-requisite (was a residual risk; now blocking).** Before ANY
test that references the assetutilities surface is written, confirm the following
public signatures on assetutilities `main` (not merely "cited by PR"): `identity`
#111 `run_id` / `derive_run_identity`; `output_contract` #114
`output_equality_digest`; `publication` #116 `build_projection` and `HfPort`. In
particular, today's `digitalmodel.workflow_api.runner.run_workflow` has **no
`run_id` concept** — exact-replay-same-`run_id` (AC3) depends entirely on wiring in
`identity` #111, so `test_exact_replay_same_run_id_and_equality` cannot be authored
until that signature is verified. If any signature differs, adjust D3/D4 and the
tests before RED capture. Notes on partial coverage: AC1's "reject candidates with
unresolved redistribution rights" half is satisfied at the resource-intelligence
(document) layer, not by a runtime test (see `test_dirty_input_fails_admission`,
which covers only the input-admission half); and AC7's real immutable-revision check
runs at owner-gated **execution** with a live `HfPort`, not in CI (CI mocks the
port), so `test_dataset_projection_publishes_immutable_revision` proves only the
projection/promotion path, not the actual HF revision.

---

## Acceptance Criteria

Verbatim from issue #1505:

- [ ] Resource intel selects and documents one wholly public or synthetic algorithm and rejects candidates with unresolved redistribution rights.
- [ ] At least three meaningful parameter variations and one exact replay are emitted.
- [ ] The exact replay resolves to the same run identifier and passes output equality; any mismatch blocks publication.
- [ ] Inputs are complete, canonical, hashed, schema-valid, and publicly replayable.
- [ ] Outputs retain their native engineering schema; only curated artifacts publish.
- [ ] Algorithm-scoped metrics include definitions, units, derivations, and quality.
- [ ] The dedicated `digitalmodel` dataset projection is published at a verified immutable Hugging Face revision.
- [ ] One rolling HTML report in `digitalmodel` contains mandatory Inputs and Outputs sections, applicable metrics/comparisons, and links to exact dataset revisions.
- [ ] A clean-room replay from the published inputs reproduces the accepted outputs.
- [ ] Legal and secret scans pass with no client identifiers or local absolute paths.

Plan-specific gates:

- [ ] Every acceptance criterion above has a failing-first (RED) test captured before implementation.
- [ ] `viv_screening.py` is consumed unchanged as the pilot algorithm (crosswalks the landed `workflow_api` runner/provenance/golden — never re-creates or aliases them as strict public identity).
- [ ] Legal + secret scan output attached to the PR; no client identifier, no local absolute path in any committed artifact.
- [ ] The `digitalmodel` source repo remains authority for code/descriptors/schemas/tests/report; the HF dataset is a projection only.

---

## Sequencing & Gate

1. **Upstream dependency (blocking):** assetutilities PRs **#111–#116**
   (`identity`, `artifact`, `inputs`, `output_contract`+`report`, `metrics`,
   `publication`) MUST be merged to assetutilities `main` and importable as
   `assetutilities.workflow_api.*` before pilot execution. Parent run-ledger
   contract workspace-hub#3452 is already merged. Blocked-by workspace-hub#3433
   must also be resolved.
2. **Plan gate:** this plan needs its OWN adversarial review (round-1 artifacts
   above) with ≥2 usable no-MAJOR provider reviews, then explicit user approval;
   move #1505 to `status:plan-approved` and record `.planning/plan-approved/1505.md`.
   The implementing agent must not self-approve.
3. **TDD:** **HARD pre-requisite before writing any test** — confirm the
   assetutilities #111–#116 public signatures (`identity.run_id`/`derive_run_identity`,
   `output_contract.output_equality_digest`, `publication.build_projection`, `HfPort`)
   on assetutilities `main`; `run_workflow` has no `run_id` today, so AC3 depends on
   `identity` #111 being wired. Only then capture all RED tests; then D1→D5.
4. **Execution PUBLISHES a public dataset to `aceengineer/*` on Hugging Face
   (outward-facing).** This is a distinct, higher bar than an internal merge:
   - It requires **explicit owner go-ahead at run time** (separate from plan
     approval) because it puts an aceengineer-branded artifact on the public web.
   - The **HF token must have WRITE scope**; the real `HfPort` publish step is
     execution-only and never runs in CI (CI uses a mocked port).
   - Publication is HITL with its OWN approval: promotion may only reach `accepted`
     after equality + admission gates pass AND the owner authorizes the outward push.
5. **Report/ledger:** commit the rolling report + `publications.jsonl` row in the dm
   PR; commit the central plan-index row separately from clean `workspace-hub/main`.

---

## Adversarial Review Summary

| Provider | Verdict | Notes |
|---|---|---|
| Claude (r1) | BLOCK — remediated | 1 MAJOR (M1) + 3 MINOR (m1–m3); all remediated in this revision — see row below |
| Codex | PENDING | invoke directly from repo with concise local-plan path prompt |
| Gemini | PENDING | availability is not approval |

**r1 (Claude) — BLOCK, now remediated:**
- **M1 (MAJOR):** the plan routed the sweep through `workflows/parametric_run.py`,
  whose `router` resolves outputs to a hardcoded `REPO_ROOT/results/parametric_run/…`
  (NOT embed/`root_folder`-aware). That writes into the real repo tree instead of the
  throwaway sandbox `run_workflow._run_once` creates + rmtrees — breaking the
  side-effect-free premise and leaving `extract_result(..., root=tmp)` with no files
  → empty/degenerate golden. The cited byte-stable precedent `buckling-parametric`
  does NOT use `parametric_run`; it uses its own `BucklingParametricWorkflow.router`
  whose `_result_dir(cfg)` = `Analysis.result_folder`/`cfg["_config_dir_path"]/"results"`,
  which `configure_embed` rebases onto the injected `root_folder`. The plan was also
  internally contradictory (D2 internal-factorial via parametric_run vs. an external
  loop of single-case `run_workflow` calls in the pseudocode). **Remediation:**
  committed to a SINGLE dedicated, embed-aware router modeled exactly on
  `BucklingParametricWorkflow` (`timestamp=None`, `result.kind: files`, byte-stable),
  dropped `parametric_run` and the second workflow, and made the ≥3 variations
  EXTERNAL `run_workflow("viv-parametric-screening", params=variant)` calls (D1, D2,
  Pseudocode, Files-to-Change).
- **m1 (MINOR):** pinned specific published cases spanning both regimes — C-lockin
  (D=0.2032 m, V=1.0 m/s, span=60 m → A/D≈0.97) and a suppressed A/D=0.0 case — and
  replay the non-zero case so equality is demonstrable, not accidental (D2, D3, TDD).
- **m2 (MINOR):** `run_workflow` auto-stamps `provenance.data_as_of = utc_now_iso()`;
  the `output_equality`/curated digest is now explicitly limited to the timestamp-free
  `results.json` + `cases.csv`, with a TDD assertion that provenance/`data_as_of` is
  excluded and exact-replay equality still holds (D3, TDD).
- **m3 (MINOR):** confirming assetutilities #111–#116 public signatures on `main` is
  now a HARD RED-phase pre-requisite (not a residual risk); noted that `run_workflow`
  has no `run_id` today so AC3 depends on wiring in `identity` #111 (TDD, Sequencing,
  Risks).
- Notes (n1–n3): AC1's rights-rejection half is doc-satisfied (no runtime test);
  AC7's real immutable-revision check is owner-gated execution-only (HfPort mocked in
  CI); `estimate_response_amplitude`'s `outer_diameter` is unused in the body.

**Overall:** BLOCK remediated; still requires ≥1 further usable no-MAJOR provider
review (Codex/Gemini) + explicit user approval before implementation.

---

## Risks and Open Questions

- **[public-admission risk] The pure algorithm is not the routed one.**
  `orcaflex/viv_screening.py` is public-safe and analytical, but the engine's only
  VIV arm routes `viv_analysis` → the heavier legacy `subsea.viv_analysis.VIVAnalysis`
  which may pull optional/plotting deps and touch example trees. The pilot MUST add a
  new clean `viv_parametric_screening` router arm and NOT reuse the existing
  `viv-parametric` (which sweeps the legacy base). If the new arm accidentally
  imports the legacy path, byte-stability and admission both break. The router also
  must NOT use `workflows/parametric_run.py` (see M1 / Adversarial Review Summary):
  its `_resolve_output_path` hardcodes `REPO_ROOT/results/parametric_run/…` and is
  not embed-aware, which would write into the real repo tree, break the sandbox
  premise, and leave `extract_result(..., root=tmp)` with no files → degenerate golden.
- **[admission risk — RESOLVED by pinned cases] `estimate_response_amplitude`
  returns 0.0 outside 4≤Vr≤8.** Addressed in D2 by pinning specific published cases
  that span both regimes: C-lockin (D=0.2032 m, V=1.0 m/s, span=60 m → A/D≈0.97,
  non-zero) and C-suppressed (A/D=0.0), with the exact replay run against the
  non-zero C-lockin case so equality is demonstrable, not accidental. Note
  `estimate_response_amplitude`'s `outer_diameter` argument is **unused in the
  function body** (A/D is a function of Vr and Ks only) — do not rely on D affecting
  A/D directly.
- **[dependency — now a HARD RED-phase pre-requisite, not a residual risk]**
  assetutilities #111–#116 public signatures (`identity.run_id`/`derive_run_identity`,
  `output_contract.output_equality_digest`, `publication.build_projection`, `HfPort`)
  MUST be confirmed on assetutilities `main` **before** any test referencing them is
  written — see the TDD "HARD RED-phase pre-requisite" block. `run_workflow` today has
  no `run_id` concept, so AC3 same-`run_id` depends on wiring in `identity` #111. If a
  signature differs, adjust D3/D4 and the tests before RED capture.
- **[byte-stability risk] Golden must be machine-stable.** Follow the
  buckling-parametric recipe exactly: omit `meta.generated_at`, keep the sweep SMALL,
  rely on the pydantic models' existing rounding. Any unrounded float or timestamp
  breaks the cross-machine golden and the clean-room replay.
- **[outward-facing risk] Public HF publish is irreversible-ish.** A pushed public
  dataset revision is world-visible; the WRITE-scope token and owner go-ahead gate
  this. CI must never hold the real token — mock `HfPort` in tests.
- **Fatigue-proxy definition (open):** the surrogate (cycle-rate·(A/D)^m) is a
  screening indicator, not certified fatigue life. Confirm the exponent m and label
  it clearly as "screening surrogate" in the metrics `quality` field to avoid
  over-claiming.
- **Report location (open):** `docs/reports/viv-parametric-screening/index.html` vs a
  Pages-published path — confirm which surface the parent contract expects for the
  "rolling report in the source repository."

---

## Complexity

**T3** — spans a new engine router arm + adapter, a new registered parametric
workflow with synthetic fixtures, six consumed assetutilities modules (identity/
artifact/inputs/output_contract+report/metrics/publication), a committed determinism
golden, a clean-room replay, an externalized publication config, a rolling HTML
report, and an outward-facing public HF publish with its own HITL gate. It couples
determinism, provenance, public admission, and an external side-effecting port —
each an independent failure surface — so it is above T2 despite the pilot algorithm
itself being a small pure calc.
