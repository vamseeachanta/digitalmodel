# Plan for #1602: Riser Data to OrcaFlex to Hugging Face Strength and Operability Program

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-07-16
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1602
> **Client:** N/A
> **Lane:** lane:codex
> **Design:** `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html`
> **Review artifacts:** `scripts/review/results/issue-1602-round-1/2026-07-16-plan-1602-claude.md` | `...-codex.md` | `...-gemini.md`

---

## Authorization Boundary

This parent plan will govern the cross-repository program contract, terminology,
dependency order, promotion gates, and closeout evidence. It will not authorize
implementation in a child repository. Each child issue will require its own
resource intelligence, future-tense plan, adversarial review, and explicit user
approval before code or data changes begin.

The solver-neutral normalized schema and source registry will be the single
source of truth. Existing and generated OrcaFlex files will be evidence or
derived execution artifacts. Raw drilling/completion DAT or YAML files will not
be published to Hugging Face.

## Resource Intelligence Summary

Planning used `parallel-readonly` execution: three independent read-only lanes
inspected digitalmodel, worldenergydata, and the licensed-run/HF publication
surfaces. The orchestrator verified the load-bearing claims against live GitHub
and local files before drafting this plan.

### Existing repo code

- `digitalmodel/src/digitalmodel/riser_database/loader.py` currently loads five
  fixed public tables and exposes `RiserDatabase.load()`, typed accessors, and
  `iter_public_rows()`. New public tables will have to enter both `_TABLE_SPECS`
  and the fail-closed public-row iterator.
- `digitalmodel/src/digitalmodel/riser_database/getters.py` already returns
  cited riser values through `get_riser_dff()`, `get_riser_scf()`,
  `get_flexjoint_angle_limit()`, and related getters. Result criteria will reuse
  these `Citation` sidecars rather than copying standards constants.
- `digitalmodel/src/digitalmodel/orcaflex/riser_input_schema.py` already defines
  solver-neutral SCR, SLWR, flexible, TTR, and drilling inputs. It has no
  completion/workover discriminator and no immutable analysis-case identity.
- `digitalmodel/src/digitalmodel/solvers/orcaflex/modular_generator/` already
  converts `ProjectInputSpec` to modular OrcaFlex files. `GenerationResult`
  currently contains generated paths, resolved variables, and warnings, but no
  case hash, bundle hash, per-file hash manifest, or repeat-byte attestation.
- `digitalmodel/scripts/run_riser_analysis.py` already orchestrates spec loading,
  generation, statics/dynamics, and limited tension/bend/curvature extraction.
  It imports `OrcFxAPI` at module import, has no structured solver attestation,
  and does not yet emit the required strength utilization or operability tables.
- `worldenergydata/.../curated/drilling_riser_components.csv` is the canonical
  36-row component table. Its loader currently delegates malformed rows to
  pandas, allowing silent column shifts.
- `deckhand/src/deckhand/licensed_run_agent.py`,
  `licensed_run_agent_runtime.py`, and `licensed_run_queue.py` provide approved,
  jailed, single-seat execution with an input hash and metadata queue. They do
  not yet bind a complete bundle/case/model chain or return typed result files.
- `workspace-hub/scripts/hf/save_results_to_hf.py` already stages Parquet,
  uploads through the Hugging Face API, and verifies remote file presence. It
  prints datasets-server URLs but does not poll and assert `/is-valid`,
  `/splits`, and `/rows` for every required config.
- `aceengineer-website/config/capabilities.yaml` is the canonical website data
  catalog; its validation/refresh scripts will be used only after the dataset is
  live and verified.

### Standards and citation boundary

| Standard family | Ledger state | Use in this program |
|---|---|---|
| API RP 17G / ISO 13628-7 | `done`; private wiki page is metadata-only for the encrypted official edition | Scope/provenance identifier only until an approved cited getter supplies a value |
| ISO 13624-1 | `done` | Drilling-riser scope and source crosswalk; no licensed table/prose copied |
| API RP 16Q / API 16R | `done` | Drilling component/operation crosswalk; no uncited numerical default |

`workspace-hub/data/document-index/standards-transfer-ledger.yaml` is the
status authority. A `done` ledger state means the standard is indexed; it does
not grant redistribution rights or authorize a numerical constant. Every
standards-derived calculation value will fail closed without the required
`Citation` sidecar.

### LLM Wiki pages consulted

- `llm-wiki/wikis/engineering-standards/wiki/standards/iso-13628-7.md`
  identifies the completion/workover scope and records the official edition as
  encrypted metadata-only. The draft is explicitly not a substitute for the
  official edition.

No wiki content will be modified by this parent plan. Child plans that add wiki
content will declare the appropriate sibling wiki and client-routing metadata.

### Documents and issues consulted

- `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html` records the
  owner-approved Option 1 public-safe architecture.
- [digitalmodel #1199](https://github.com/vamseeachanta/digitalmodel/issues/1199)
  and landed [digitalmodel #1245](https://github.com/vamseeachanta/digitalmodel/issues/1245)
  establish the public riser-database and leak-gate foundation.
- [digitalmodel #138](https://github.com/vamseeachanta/digitalmodel/issues/138)
  remains the analysis child; [digitalmodel #811](https://github.com/vamseeachanta/digitalmodel/issues/811)
  remains the drilling stackup/operability child.
- [worldenergydata #698](https://github.com/vamseeachanta/worldenergydata/issues/698)
  will grow the same component catalog only after the delimiter/integrity repair.
- `deckhand/docs/deckhand/licensed-run-go-live-ace-win-2.md` documents the
  licensed host lane, but historical host success is not current attestation.
- `workspace-hub/.claude/skills/data/hf-dataset-publishing/SKILL.md` defines the
  publication and verification contract for public HF datasets.
- `workspace-hub/config/workstations/registry.yaml` lists the Windows solver
  roles; the current Deckhand policy still defaults real execution off.

### Child issue tree and dependency boundaries

| Layer | Issue | Owned outcome | Hard dependency |
|---|---|---|---|
| D0 — upstream integrity | [worldenergydata #1046](https://github.com/vamseeachanta/worldenergydata/issues/1046) | 31-column repair and strict loader rejection | none; coordinate with WED #698 |
| D1 — source/normalized data | [digitalmodel #1603](https://github.com/vamseeachanta/digitalmodel/issues/1603) | source registry, normalized cases, synthetic completion references, deterministic adapter | D0 for WED component promotion |
| E1 — drilling workflow | [digitalmodel #811](https://github.com/vamseeachanta/digitalmodel/issues/811) | deterministic stackup/operability input workflow | D1 contract |
| E2 — analysis semantics | [digitalmodel #138](https://github.com/vamseeachanta/digitalmodel/issues/138) | strength and operability extraction/criteria | D1 and applicable E1 outputs |
| E3 — licensed execution | [deckhand #568](https://github.com/vamseeachanta/deckhand/issues/568) | attested Windows solve and hashed result return | D1 bundle plus E2 result contract |
| R1 — release/publication | [digitalmodel #1604](https://github.com/vamseeachanta/digitalmodel/issues/1604) | allowlisted release, HF upload, immutable and datasets-server verification | D1 + E2 + E3 verified outputs |
| R2 — website registration | [aceengineer-website #75](https://github.com/vamseeachanta/aceengineer-website/issues/75) | capability entry for the already-live verified dataset | R1 verified HF revision |

Parent approval will authorize only orchestration: advancing children through
their own gates, verifying dependency artifacts, and maintaining the program
status. It will not authorize any D0–R2 implementation.

### Gaps identified

- No immutable source registry or normalized drilling/completion analysis-case
  contract exists in digitalmodel.
- No explicitly synthetic completion/workover public reference set exists.
- No deterministic hash chain currently links source, normalized input, case,
  generated model, solver execution, result, and HF revision.
- No fail-closed adapter proves that every normalized field maps to OrcaFlex.
- No licensed-run workflow provides current solver/license/statics attestation
  and typed hashed result return.
- No public release builder rejects raw solver files and joins every result to a
  verified case and execution.
- No riser-specific HF repository exists or has datasets-server verification.

### Evidence (embedded verification)

**Repository heads** (verified 2026-07-16T21:37:36Z):

```text
digitalmodel GitHub main      87d56cac637f971ca3ed57d8ca98c16a845ff0f7
worldenergydata GitHub main   090228fb4a1193e4190fc4da90644d9f40a20b5a
deckhand GitHub main          2027c64218c69d319993c7d6e72ab7452b906447
workspace-hub GitHub main     d9db0d7665c66736ae185e462213c92da9a65d82
```

The shared worldenergydata and Deckhand remote-tracking refs were stale during
planning. Their child implementations will begin from fresh worktrees at the
then-current GitHub `main`, not from the shared checkouts.

**Issue statuses** (verified 2026-07-16T21:37:36Z with `gh issue view`):

- digitalmodel #1602, #1603, #1604 — OPEN, `status:needs-plan`, `lane:codex`.
- worldenergydata #1046 — OPEN, `status:needs-plan`, `lane:codex`.
- Deckhand #568 — OPEN, `status:needs-plan`, `lane:codex`.
- digitalmodel #138 and #811 — OPEN legacy issues; linkage comments explicitly
  preserve their separate future plan/user-approval gates.

**CSV reproduction proof** (2026-07-16T21:37:36Z):

```text
header_width=31
data_rows=36
data_widths={29: 3, 30: 29, 31: 4}
bad_lines=2:30,...,15:29,16:29,17:29,...,37:30
```

The exact reproduction used Python's standard `csv.reader`, counted every row,
and compared it with the header width. The observed failure matches #1046: 32
of 36 rows are malformed.

**Licensed execution reproduction proof** (2026-07-16T21:37:36Z):

```text
$ python -c "import OrcFxAPI; print(OrcFxAPI.__version__)"
ModuleNotFoundError: No module named 'OrcFxAPI'

$ rg -n "execution_enabled|orcaflex-strength-post" deckhand/config/deckhand/policy.yml
103:  execution_enabled: false
107:    - "orcaflex-strength-post"
```

The Linux planning host cannot execute the licensed solver, and Deckhand's
shared real-execution switch is off. Real results will therefore require #568's
current Windows-host attestation and approved workflow; no local mock output can
satisfy the solver gate.

**HF helper gap proof** (2026-07-16T21:37:36Z):

```text
save_results_to_hf.py:330 prints the datasets-server /is-valid URL
save_results_to_hf.py:333 prints per-config /rows URLs
```

No polling/assertion implementation for `/is-valid`, `/splits`, or `/rows` was
found. #1604 will add riser-specific fail-closed verification without weakening
the canonical helper's immutable-file verification.

Source count: more than 12 distinct code, registry, issue, plan, and wiki
sources across four repositories and workspace-hub.

---

## Cross-Layer Contract

### Canonical terms

| Term | Meaning | Owner |
|---|---|---|
| `source_id` | stable registry identity for one canonical evidence source or synthetic seed | D1 |
| `configuration_id` | stable solver-neutral assembly identity | D1 |
| `case_id` | stable identity for configuration + loading + environment + criteria + requested stages | D1 |
| `bundle_sha256` | canonical hash of the complete generated input manifest and its files | D1/E3 |
| `model_sha256` | hash of the exact primary OrcaFlex model bytes executed | D1/E3 |
| `execution_id` | immutable licensed-run identity bound to case, bundle, model, code, solver, and host role | E3 |
| `result_sha256` | canonical hash of typed result tables and execution attestation | E2/E3 |
| `release_id` | immutable allowlisted public release identity | R1 |
| `hf_commit_sha` | Hugging Face repository commit returned by upload and independently verified | R1 |

The identity chain will be:

```text
source_sha256 -> normalized_input_sha256 -> case_sha256
  -> bundle_sha256 + model_sha256
  -> execution_id -> result_sha256
  -> release_id -> hf_commit_sha
```

Hashes will use canonical UTF-8 JSON for logical records: sorted keys, explicit
schema version, no NaN/Infinity, stable scalar types, SI units, and no timestamp
or machine path. File manifests will use sorted relative POSIX paths plus size
and SHA-256. Timestamps may appear in attestations but will not determine stable
case, model, result, or release identity.

### Lifecycle state machine

```text
DISCOVERED
  -> CLASSIFIED
  -> NORMALIZED
  -> SOLVER_ELIGIBLE
  -> MODEL_VERIFIED
  -> EXECUTION_ATTESTED
  -> RESULT_VERIFIED
  -> RELEASE_APPROVED
  -> HF_VERIFIED

Any stage -> WITHHELD | REJECTED | SOLVER_FAILED | RESULT_QUARANTINED
```

No later state may be inferred from process exit code alone. Every transition
will have a typed decision record, reason code, producing code revision, input
hashes, and verifier evidence.

### Source and public-release policy

- Public repository data will not become public-release eligible solely because
  it is in Git; provenance, units, confidentiality, license, and plausibility
  gates must all pass.
- The malformed WED catalog will remain quarantined until #1046 lands and D1
  records the repaired canonical hash.
- The digitalmodel copy will be a hash-verified downstream fixture, never a
  second canonical source.
- Completion/workover public cases will be independently authored synthetic
  references. They will not be derived from withheld DAT/YAML values.
- Withheld entries will expose only a logical ID, source class, reason code,
  affected surface, issue URL, and review status. They will not expose private
  paths, names, numeric values, or source hashes that create a disclosure risk.
- Standards identifiers and clause locators may publish; licensed prose, tables,
  and uncited numerical constants may not.

### Analysis contract

Each solver-eligible configuration will receive:

1. a deterministic baseline statics case;
2. a configuration-driven operability matrix with declared coordinate frames,
   SI units, offset convention, environment coordinates, requested solver
   stages, and acceptance criteria;
3. typed failure records for every requested case that does not complete; and
4. result rows only for attested real OrcaFlex execution.

Strength outputs will include effective tension, bend moment, curvature,
relevant stress/utilization, criterion ID, location, units, and citation. A
utilization value will not be emitted when capacity, material basis, safety
factor, or citation is incomplete.

Operability outputs will include the requested wave/current/offset/top-tension
coordinates, flex-joint angle, tensioner force/stroke and margin where modeled,
limiting criterion/location, limiting sea state or offset, and operable flag.
An operability probability/fraction will be omitted unless an independently
publishable metocean scatter basis exists.

The exact sweep rows, coordinate frames, sign conventions, capacities,
tolerances, and independent spot-check oracles will be frozen in the reviewed
plans for #811 and #138. Those plans must distinguish engineering sweep rows
from any off-grid display/default values and must not hide requested coverage in
representative plots.

### Publication contract

The target is the public multi-config dataset
`aceengineer/digitalmodel-riser-analysis` with required configs:

- `source_registry`
- `riser_components`
- `riser_configurations`
- `analysis_cases`
- `strength_results`
- `operability_results`
- `withheld_sources`

The release builder will use an exact file/config allowlist. It will reject raw
`.dat`, `.yml`, `.yaml`, solver binaries, arbitrary recursive extras, absolute
paths, secret-like values, client/project identifiers, unapproved source
classes, mock results, missing hashes, and results without an attested join.

The publication gate will require both:

1. immutable file names, sizes, and SHA-256 verified against the returned HF
   commit SHA; and
2. latest datasets-server `/is-valid`, `/splits`, and `/rows` checks for every
   required config.

Because datasets-server exposes the current repository view rather than a
revision-pinned view, immutable byte verification and viewer verification will
remain separate evidence. Website registration will occur only after both pass.

---

## Artifact Map

| Artifact | Path or issue |
|---|---|
| Approved design | `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html` |
| This parent plan | `docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md` |
| Plan index | `docs/plans/README.md` |
| Parent review round | `scripts/review/results/issue-1602-round-1/` |
| Upstream CSV repair | worldenergydata #1046 |
| Normalized source/case contract | digitalmodel #1603 |
| Drilling workflow | digitalmodel #811 |
| Strength/operability analysis | digitalmodel #138 |
| Licensed execution/return | Deckhand #568 |
| HF release/publication | digitalmodel #1604 |
| Website registration | aceengineer-website #75 |
| Program completion report | `docs/reports/{completion_date}-1602-completeness.html` |

---

## Deliverable

The program will produce a provenance-safe public Hugging Face dataset whose
normalized drilling and explicitly synthetic completion/workover inputs are the
single source of truth and whose strength/operability results are traceable to
verified real OrcaFlex executions, while restricted sources remain visibly but
safely withheld.

---

## Pseudocode

```text
function register_source(candidate):
    classify canonical/duplicate/public/synthetic/private/licensed/derived
    hash canonical source bytes without exposing a restricted path
    require ownership, license, units, provenance, and release decision
    return source record or withheld record; never infer eligibility

function normalize_configuration(source_records):
    parse only allowlisted fields and reject malformed row widths
    convert declared units to SI; reject ambiguous/non-finite values
    validate relationships, physical bounds, and required citations
    canonicalize logical JSON and derive configuration/input hashes
    return immutable solver-neutral configuration

function compile_case(configuration, environment, loading, criteria, stages):
    require complete inputs and explicit frames/sign conventions
    canonicalize the complete request and derive case_id/case_sha256
    map every canonical field to ProjectInputSpec or fail with field locator
    generate twice; require identical bytes and per-file hash manifest
    return case plus deterministic bundle; raw OrcaFlex is not canonical

function execute_licensed(bundle, expected_identity):
    verify request approval, code commit, bundle, model, and adapter hashes
    attest OrcFxAPI/DLL version, license acquisition, and real statics smoke
    run requested stages under the single-seat lock
    apply semantic success checks independent of return code
    return typed result bundle or structured terminal failure

function verify_results(case, execution, result_bundle):
    verify all hashes and exact one-to-one joins
    require real-solver attestation and expected output schemas/row counts
    run physical bounds and independent engineering spot checks
    require Citation sidecars for standards-derived utilization/limits
    return verified results or quarantine record

function build_public_release(verified_records):
    select only RELEASE_APPROVED records and exact configs/columns
    reject forbidden extensions, paths, identities, source classes, and mocks
    validate schemas, joins, units, nulls, hashes, citations, and row counts
    write immutable staging directory plus dataset card and release manifest

function publish_and_verify(release):
    upload to aceengineer/digitalmodel-riser-analysis
    verify file bytes at returned hf_commit_sha
    poll datasets-server validity with bounded retry/backoff
    verify splits and representative rows for every required config
    register website capability only after both verification classes pass
```

---

## Anticipated Child Files to Change

These paths are planning anchors, not authorization. Each child plan will verify
current `main`, freeze exact APIs, and may revise paths before its own review.

| Child | Action | Path | Reason |
|---|---|---|---|
| #1046 | Modify | `packages/worldenergydata-vessel_fleet/src/worldenergydata/vessel_fleet/_data/curated/drilling_riser_components.csv` | delimiter-only alignment repair |
| #1046 | Modify | `packages/worldenergydata-vessel_fleet/src/worldenergydata/vessel_fleet/loaders/drilling_riser_loader.py` | strict pre-pandas width validation |
| #1046 | Modify | `packages/worldenergydata-vessel_fleet/tests/unit/vessel_fleet/loaders/test_drilling_riser_loader.py` | RED tests and semantic anchors |
| #1603 | Create | `src/digitalmodel/riser_database/source_registry.py` | source classification and release eligibility |
| #1603 | Create | `src/digitalmodel/riser_database/analysis_case.py` | normalized configuration/case models and canonical hashes |
| #1603 | Create | `src/digitalmodel/riser_database/orcaflex_adapter.py` | fail-closed `ProjectInputSpec` projection and bundle manifest |
| #1603 | Modify | `src/digitalmodel/riser_database/loader.py` | register new public tables and iterator coverage |
| #1603 | Modify | `data/riser_database/manifest.yaml`, `scripts/riser_database/build_tables.py`, `scripts/riser_database/sources.yml` | build/manifest/provenance contract |
| #1603 | Create/modify | `tests/riser_database/`, `tests/orcaflex/test_riser_input_schema.py`, `tests/solvers/orcaflex/modular_generator/` | D1 TDD and deterministic mapping |
| #811/#138 | Refactor | `scripts/run_riser_analysis.py` plus importable modules under `src/digitalmodel/solvers/orcaflex/risers/` | lazy solver import and typed analysis boundary |
| #811/#138 | Create/modify | dedicated unit/integration tests under `tests/solvers/orcaflex/` and `tests/scripts/` | fake-model unit tests plus licensed integration contract |
| #568 | Modify | `src/deckhand/licensed_run_agent.py`, `licensed_run_agent_runtime.py`, `licensed_run_queue.py` | bundle identity, semantic status, typed result references |
| #568 | Modify | `config/deckhand/policy.yml`, `scripts/deckhand/verify-ace-win-2.py`, Deckhand tests/runbook | workflow allowlist and current host attestation |
| #1604 | Create | `src/digitalmodel/riser_database/release.py` and `scripts/riser_database/publish_hf_release.py` | allowlisted release and fail-closed HF verification |
| #1604 | Create/modify | `tests/riser_database/test_release.py`, publisher tests, `docs/riser_database.md` | release TDD and dataset contract |
| #75 | Modify | `config/capabilities.yaml` and website validation fixtures/tests | register only the verified live dataset |

Each code file will remain within the repository's 400-line limit; functions
will remain within 50 lines. Child plans will split modules further when the
verified API requires it.

---

## TDD Test List

Tests will be written and observed failing in each approved child before the
corresponding implementation.

| Test | Owner | Verifies |
|---|---|---|
| exact 31-field rows with line diagnostics | #1046 | malformed CSV cannot reach pandas |
| semantic anchors for all five component types | #1046 | delimiter repair preserves column meaning |
| all 36 IDs/text/source/notes preserved | #1046 | repair is structural, not data invention |
| stable source/configuration/case hashes | #1603 | identical logical input has identical identity |
| duplicate canonical-source detection | #1603 | digitalmodel fixture cannot become a second source |
| completion synthetic-label and non-derivation fixtures | #1603 | withheld models cannot seed public values |
| ambiguous units/non-finite/unmapped field rejection | #1603 | normalization and adapter fail closed |
| public iterator/leak/provenance/citation coverage | #1603 | every public row reaches mandatory gates |
| two generations produce identical bytes/manifests | #1603 | deterministic OrcaFlex projection |
| explicit coordinate-frame/sign/sweep coverage | #811/#138 | requested operability matrix is complete and unambiguous |
| utilization absent when capacity/citation is incomplete | #138 | calc citation contract fails closed |
| fake OrcFxAPI cannot claim real solver result | #138/#568 | mock/real truth boundary |
| license unavailable or semantic failure despite rc=0 | #568 | structured failure overrides process return code |
| tampered case/bundle/model/output hash rejection | #568 | end-to-end identity is enforced |
| typed result return rehashes on Linux | #568 | host-local paths/bytes cannot be trusted implicitly |
| every result joins to case + attested execution | #1604 | orphan or mixed-revision rows cannot publish |
| exact config/column/extension allowlist | #1604 | raw or arbitrary files cannot enter release |
| private path/client/secret/mock/restricted-token fixtures | #1604 | public leak gate covers release artifacts and card |
| immutable HF byte verification at commit SHA | #1604 | uploaded files match the staged release |
| bounded `/is-valid`, `/splits`, `/rows` polling | #1604 | viewer readiness is asserted for every config |
| online capability validation against live configs | #75 | website cannot advertise a missing/stale dataset |

Licensed integration tests will be tagged and will require the attested Windows
host. Unit tests will use fakes only to verify orchestration/failure behavior;
fake outputs will never satisfy release tests.

---

## Execution Sequence and Gates

1. **Plan the children.** Each D0–R2 owner will verify fresh `main`, check
   parallel work, write its plan, run the required adversarial review tier, and
   stop for user approval.
2. **Land D0 and D1.** #1046 will repair source integrity. #1603 will establish
   the normalized schema, source registry, synthetic completion references,
   deterministic adapter, public gates, and identity manifests.
3. **Land E1 and E2.** #811 and #138 will consume the D1 contract without
   redefining it, freeze the engineering sweep/criteria, and provide typed
   strength/operability extraction with independent spot checks.
4. **Attest and run E3.** #568 will register the fixed workflow, prove current
   host/license/statics capability, execute each solver-eligible case, and return
   typed hashed results. Execution remains disabled until the canary passes.
5. **Build and publish R1.** #1604 will generate the allowlisted release, pass
   unit/integration/legal/leak/citation checks, upload once, verify immutable HF
   bytes, then verify datasets-server configs and rows.
6. **Register R2.** #75 will add website capability metadata only after the live
   dataset is verified.
7. **Close the parent.** #1602 will remain open until every child is closed or an
   explicit user-approved scope reduction is recorded. The parent will post a
   source/case/result matrix, completeness report, code/artifact review evidence,
   legal scan evidence, HF commit, datasets-server evidence, and cleanup audit.

Parallelism is allowed only where dependencies permit it. #1046 and the
synthetic-case portion of #1603 may proceed in separate worktrees after their
own approvals. #811/#138 test design may proceed after D1's contract is fixed;
licensed execution cannot begin before the exact bundle and result schemas land.
R1 may develop against synthetic fixtures but cannot publish until E3 results
pass all release gates.

---

## Acceptance Criteria

- [ ] Every child issue has a canonical plan, adversarial review evidence, and
  explicit user approval before implementation.
- [ ] The live source inventory classifies every candidate as canonical,
  duplicate, synthetic, public, private, licensed, derived, or withheld.
- [ ] WED CSV repair preserves all 36 IDs/text and rejects any future wrong-width
  row before pandas parsing.
- [ ] Solver-neutral normalized configurations and analysis cases are the SSOT;
  OrcaFlex files are deterministic derived artifacts.
- [ ] Public completion/workover cases are explicitly synthetic and demonstrably
  not reverse-derived from withheld models.
- [ ] Every eligible configuration has a baseline statics request and the
  reviewed configuration-driven operability matrix; every ineligible or failed
  case has a typed reason record.
- [ ] Real solver results attest case, bundle, model, code, solver/DLL, license,
  host role, result hashes, and semantic solver success.
- [ ] Strength utilization and standards-derived limits fail closed without
  capacity basis, units, revision, and `Citation` sidecars.
- [ ] Independent engineering spot checks and physical plausibility gates pass;
  implementation formulas are not reused as their own oracle.
- [ ] The public release contains only the seven required configs and allowlisted
  scalar fields; no raw DAT/YAML, private paths, client/project identifiers,
  secret-like values, licensed content, or mock results appear.
- [ ] Every public result joins to exactly one published case, normalized
  configuration, attested execution, and result hash.
- [ ] `aceengineer/digitalmodel-riser-analysis` files match the immutable returned
  HF commit SHA and datasets-server validity/splits/rows pass for every config.
- [ ] The dataset card reports schema versions, row counts, units, nulls,
  licenses, source hashes, solver limitations, synthetic labeling, and safe
  withheld-source reasons.
- [ ] Website registration occurs only after the verified dataset is live.
- [ ] Each repo's focused/full tests, security/leak checks, and
  `scripts/legal/legal-sanity-scan.sh` pass where applicable.
- [ ] T3 code/artifact cross-review has no unresolved MAJOR findings.
- [ ] The parent issue receives implementation summaries with inline issue URLs,
  a completeness score/report, named residual risks, and cleanup audit evidence.

### Parent plan verification commands

```bash
# Plan/design structure
xmllint --html --noout docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html
! rg -n 'TO''DO|TB''D|<re''po>|N''NN' \
  docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md

# Plan/index and review artifacts
rg -n 'issue-1602-riser-data-orcaflex-hf-plan' docs/plans/README.md
test -s scripts/review/results/issue-1602-round-1/2026-07-16-plan-1602-claude.md
test -s scripts/review/results/issue-1602-round-1/2026-07-16-plan-1602-codex.md
test -s scripts/review/results/issue-1602-round-1/2026-07-16-plan-1602-gemini.md

# Legal/diff gate before pushing the reviewed plan
cd ../../workspace-hub
bash scripts/legal/legal-sanity-scan.sh \
  --repo=../agent-worktrees/dm-1602-design --diff-only
```

---

## Adversarial Review Summary

Pending T3 review. Round 1 will use revision-stamped, non-empty artifacts under
`scripts/review/results/issue-1602-round-1/`. Any MAJOR finding will keep the
plan in `draft`, require a revision, and trigger a fresh round namespace.

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | — |
| Codex | PENDING | — |
| Gemini | PENDING | — |

**Overall result:** PENDING

---

## Risks and Open Questions

- **Current licensed-host readiness:** Deckhand real execution is disabled and
  historical host evidence is stale. #568 must prove a current real statics
  canary before any production case dispatch.
- **Source provenance:** Repairing CSV alignment will not manufacture missing
  field-level lineage. Records without sufficient provenance will remain
  withheld even when syntactically valid.
- **Standards rights:** Ledger/wiki availability does not grant republication.
  Public artifacts will carry identifiers/citations, not licensed content.
- **Completion coverage:** The first public release will use independently
  authored synthetic completion/workover references. Confidential legacy models
  will not count as public eligible datasets.
- **Engineering coverage:** The exact #811/#138 sweep depends on source-backed
  or synthetic boundary conditions. Missing values will reduce eligibility, not
  be guessed to inflate case count.
- **HF consistency:** datasets-server is eventually consistent and not
  revision-pinned. Verification will use bounded polling and will keep immutable
  byte proof separate from latest-view proof.
- **Legacy issue state:** #138 carries a stale `dispatch:ready` label without a
  current approved plan. Its child planning session must reconcile that state
  and may not treat the label as implementation authorization.
- **No open owner decision:** Option 1, the SSOT boundary, raw-model exclusion,
  synthetic completion policy, target HF repository, and child gate model are
  fixed by the approved design. Any proposed change to those boundaries will
  return to the user before implementation.

---

## Complexity: T3

T3 is required because this program spans four implementation repositories plus
workspace-hub operating infrastructure, licensed engineering software, public
data publication, standards-derived calculations, confidentiality boundaries,
and a multi-stage identity chain. Both plan-stage and code/artifact-stage
cross-provider adversarial review are mandatory.
