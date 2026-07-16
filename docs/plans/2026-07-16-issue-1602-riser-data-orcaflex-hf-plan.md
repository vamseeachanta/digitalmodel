# Plan for #1602: Riser Data to OrcaFlex to Hugging Face Strength and Operability Program

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-07-16
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1602
> **Client:** N/A
> **Lane:** lane:codex
> **Design:** `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html`
> **Normative contract:** `docs/plans/issue-1602-riser-analysis-contract-v1.yaml`
> **Review artifacts:** Rounds 1–4 under `scripts/review/results/issue-1602-round-{1,2,3,4}/`; approval review under `scripts/review/results/issue-1602-round-5/`

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

The machine-readable contract bundle named in the header is normative for every child:
it freezes coordinate semantics, orthogonal source governance, field lineage,
canonical hash preimages, field disposition, model verification, authenticated
execution, artifact transport requirements, required engineering outputs,
release schemas, HF mutation/verification, website evidence, and parent
closeout. A child may tighten that contract. Any incompatible or weakening
change will require a version bump, parent-plan revision, fresh adversarial
review, and user approval before dependent implementation continues.

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
| E0 — early host readiness | [deckhand #568](https://github.com/vamseeachanta/deckhand/issues/568) | non-production signed canary, transport-backend decision, and current host/license/DLL proof | its own approved child plan; no production bundle yet |
| E1 — drilling workflow | [digitalmodel #811](https://github.com/vamseeachanta/digitalmodel/issues/811) | deterministic stackup/operability input workflow | D1 plus legacy A1/#808 and A2/#809, unless a separately approved public-only slice is isolated |
| E2a — analysis contract/unit implementation | [digitalmodel #138](https://github.com/vamseeachanta/digitalmodel/issues/138) | internal strength/operability equations, required-output matrix, extraction, independent oracles | D1 and applicable E1 outputs |
| E3 — licensed execution | [deckhand #568](https://github.com/vamseeachanta/deckhand/issues/568) | authenticated Windows solve, safe artifact data plane, and signed result envelope | D1 bundle plus E2a result contract |
| E2b — licensed validation | [digitalmodel #138](https://github.com/vamseeachanta/digitalmodel/issues/138) | validate extraction/oracles against E3 real outputs before #138 closes | E3 signed outputs |
| R1 — release/publication | [digitalmodel #1604](https://github.com/vamseeachanta/digitalmodel/issues/1604) | versioned release schemas, decoded leak scan, exact-tree HF mutation, semantic verification, closeout verifier | D1 + E2b + E3 verified outputs |
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
- No protected signing-key trust root or selected access-controlled artifact
  transport backend currently proves that a result came from the licensed host.
- No versioned cross-repository conformance contract existed before this plan;
  `issue-1602-riser-analysis-contract-v1.yaml` will be the reviewed D1/E2/E3/R1
  authority.

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

`docs/plans/issue-1602-riser-analysis-contract-v1.yaml` is the bundle manifest.
It delegates exact normalized fields/dispositions, release schemas, and protocol
semantics to three referenced normative YAML files and validates them against a
JSON meta-schema. Reviewers and children will validate the bundle as data, not
rely on the summary below.

### Governance, lineage, and synthetic authorship

Source governance uses independent dimensions for canonicality, origin,
derivation, confidentiality, redistribution, and release state. A record may be
synthetic, canonical, public, and redistribution-approved simultaneously.
Every normalized field will have one or more ordered lineage rows containing
the exact source field/locator/hash, transformation/version, and input/output
units. A seed hash will bind lineage and clean-room claims without referring to
the final entity ID; the ordered lineage set will then participate in the final
normalized identity. This construction will be acyclic.

Synthetic completion/workover cases will follow the contract's clean-room
protocol: an authoring environment may access only a hashed public allowlist;
the author records citations, engineering rationale, environment evidence, and
a declaration; a different confidentiality reviewer performs a private
similarity/disclosure review. Public evidence will expose signed claim hashes,
key IDs, signed timestamps, an opaque private-evidence ID, and a blinded HMAC
commitment—but no private corpus path or corpus hash. Both claims will bind the
same configuration seed. Labels or unit fixtures alone cannot establish
non-derivation.

### Canonical identity and cross-repository compatibility

The contract uses NFC UTF-8 and RFC 8785 JCS, prohibits binary floats in logical
identity preimages, and represents engineering numbers as normalized decimal
strings. It specifies null/absence, set ordering, relative POSIX paths,
case-fold/Unicode collision rejection, and explicit self-field exclusions for
each hash view. Engineering configuration identity will be finalized before
time-bearing eligibility attestations; the attestations will sign both the
stable seed and final configuration ID without changing either identity.

```text
source_sha256 -> normalized_input_sha256 -> case_sha256
  -> bundle_sha256 -> model_sha256
  -> signed(attestation_sha256 + result_payload_sha256)
  -> result_envelope_sha256 -> release_id -> hf_commit_sha
```

D1, E2, E3, and R1 will run the same committed golden vectors and conformance
fixtures on Linux and Windows. Producers will declare the contract version;
consumers will accept only exact reviewed versions. Unknown fields, versions,
or enum values fail. Any compatibility change will be versioned and re-reviewed.

### Field disposition, coordinates, and model truth

Every contract field will appear exactly once in a reviewed disposition table:
direct solver map, transformed solver map, solver metadata, execution control,
result-only, publication-only, or rejected. “Map every field to
`ProjectInputSpec`” is not the rule. Missing disposition or a required field
filled by a Pydantic/generator default will fail. Tests will remove every
required engineering field and require rejection.

The global coordinate contract is ENU with +Z up, mean-water-level datum,
directions clockwise from true North and going-to, vessel heading by bow-to,
offset at the top-connection reference, and arc length from the top connection.
These definitions precede D1 hashing and receive rotation/end-role tests.

The SSOT and public projection will carry the physical inputs needed to rebuild
the case rather than generic pipe rows alone: hydrodynamic diameters and
coefficients; flex-joint stiffness/limits; tensioner stiffness, force, stroke,
and redundancy; rigid-package mass/inertia; connector capacities; explicit
connection topology; vessel motion source/mode; wave spectrum/phase; veering
current; pressure; criteria; and dynamics controls. The adapter will map a
veering current to OrcaFlex reference speed/direction plus depth-wise factor and
rotation, with a fail-closed all-zero rule when reference speed is zero.

`MODEL_VERIFIED` will require an empty owned stage, complete transitive input
closure, zero post-validator errors, real OrcFxAPI load, expected object/type /
connection counts, unit-system proof, and a solver-native save. The transfer
bundle hash and solver-native saved-model hash are separate.

### Authenticated licensed execution and transport

E0 will prove current host/API/DLL/license/statics capability early, without a
production bundle. #568's approved plan will select the access-controlled
content-addressed artifact backend before implementation. The metadata queue
will carry only logical references/hashes. Deterministic ZIP64 archives will be
atomically promoted, rehashed, safely extracted with traversal/link/collision /
size protections, and accepted idempotently by execution ID plus artifact hash.

The licensed host will sign the production result envelope with an Ed25519 key
held in the OS-protected secret store. A private verifier registry binds signer
key/machine identity to the public host role and handles rotation/revocation.
Nonce, request issuance/receipt/expiry, workflow/agent/code versions,
case/bundle/model hashes, API/DLL/bitness and runtime-dependency manifest,
license session, lock interval, canary identity/time/attestation, solver run
start/end, attestation issuance, semantic completion, and result payload hash
are signed. Self-asserted JSON and hashes are
insufficient.

### Required analysis and independent oracles

Both drilling and clean-room synthetic completion/workover families require a
baseline statics case. The normative contract freezes the v1 quantity/location
matrix. `NOT_MODELED` makes a requested case ineligible; it cannot silently omit
a safety-critical output. Dynamics extrema are required when dynamics are
requested. Fatigue is outside the v1 parent release; DFF and SCF are not strength
capacities.

#138's reviewed plan will freeze load combinations, pressure/corrosion basis,
strength equation, safety factors, mean/max criteria, tensioner redundancy,
stroke definition, critical-location rules, every sweep row, and tolerances.
It will use direct fail-closed cited getters and prohibit `riser_citations()` in
calculation paths. Independent oracles include equilibrium, geometry,
`M/EI`-curvature, separate stress reconstruction, zero-load symmetry,
rotational invariance, monotonic trends, and convergence/seed/duration checks
when dynamics are used. The contract fixes the sampling floor; missing requested
cases block release.

### Exact public release and website truth

The target will have thirteen exact Parquet configs: `source_registry`,
`transformations`, `citations`, `criteria`, `clean_room_reviews`, `field_lineage`,
`riser_components`, `riser_configurations`, `analysis_cases`,
`execution_attestations`, `strength_results`, `operability_results`, and
`withheld_sources`. The contract freezes every column, physical type,
nullability, key, enum, common release ID, and split.

R1 will decode and scan logical rows before serialization, staged Parquet, the
card/manifest, and downloaded remote Parquet. It will not rely on diff-only
legal scanning or binary grep. License decisions, deny-list/secret/path/client /
project checks, source allowlists, extensions, and exact remote tree will all be
fail-closed.

HF publication will use one atomic create-commit containing uploads and deletes,
with `parent_commit` equal to the observed head. The remote tree must exactly
equal the release manifest. Head must equal the returned `hf_commit_sha` both
before and after viewer polling. `/is-valid`, `/splits`, and `/rows` will be
checked for true validity, the exact config/split/feature set, manifest row
counts, representative primary keys, and the expected `release_id`.
Every result row will be joined fail-closed to the same execution case, case
hash, signed result-payload hash, `verified_success` semantic status, and public
criterion/citation pair before serialization.

Only aceengineer-website #75 owns website changes. Its online validator will
fail closed; refresh, tests, production build, deployment for the exact commit,
HTTP 200, and rendered expected release ID are required. Metadata registration
alone is not completion.

---

## Artifact Map

| Artifact | Path or issue |
|---|---|
| Approved design | `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html` |
| This parent plan | `docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md` |
| Normative cross-layer contract | `docs/plans/issue-1602-riser-analysis-contract-v1.yaml` |
| Normalized SSOT schema/dispositions | `docs/plans/issue-1602-riser-normalized-schema-v1.yaml` |
| Exact release schema | `docs/plans/issue-1602-riser-release-schema-v1.yaml` |
| Identity/solver/HF/closeout protocol | `docs/plans/issue-1602-riser-protocol-v1.yaml` |
| Contract structural meta-schema | `docs/plans/issue-1602-riser-contract-meta-schema-v1.json` |
| Plan index | `docs/plans/README.md` |
| Initial provider/fallback MAJOR review | `scripts/review/results/issue-1602-round-1/` |
| Round 3 blocking review | `scripts/review/results/issue-1602-round-3/` |
| Round 4 blocking review | `scripts/review/results/issue-1602-round-4/` |
| Approval review | `scripts/review/results/issue-1602-round-5/` |
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
    classify independent canonicality/origin/derivation/confidentiality/license/release dimensions
    hash canonical source bytes without exposing a restricted path
    require ownership, license, units, provenance, and release decision
    return source record or withheld record; never infer eligibility

function normalize_configuration(source_records):
    parse only allowlisted fields and reject malformed row widths
    convert declared units to SI; reject ambiguous/non-finite values
    require complete per-field lineage and reject implicit engineering defaults
    validate relationships, coordinate contract, physical bounds, and citations
    canonicalize logical JSON and derive configuration/input hashes
    return immutable solver-neutral configuration

function compile_case(configuration, environment, loading, criteria, stages):
    require complete inputs and explicit frames/sign conventions
    canonicalize the complete request and derive case_id/case_sha256
    require exactly one disposition for every field; map only solver fields
    generate in an empty stage; close every transitive dependency
    require repeat bytes, zero validator errors, real semantic load, and native save
    return case plus deterministic bundle; raw OrcaFlex is not canonical

function execute_licensed(bundle, expected_identity):
    verify request approval, code commit, bundle, model, and adapter hashes
    attest OrcFxAPI/DLL version, license acquisition, and real statics smoke
    run requested stages under the single-seat lock
    apply semantic success checks independent of return code
    sign nonce/freshness/code/license/hashes/status/result envelope with protected host key
    return signed typed result bundle or structured terminal failure

function verify_results(case, execution, result_bundle):
    verify all hashes, foreign keys, stable row IDs, and contract cardinalities
    verify signer trust/revocation/freshness/canary and expected output matrix/counts
    run contract-defined independent engineering oracles and sample floor
    require Citation sidecars for standards-derived utilization/limits
    return verified results or quarantine record

function build_public_release(verified_records):
    select only RELEASE_APPROVED records and thirteen exact versioned configs
    reject forbidden extensions, paths, identities, source classes, and mocks
    decode/scan logical, staged Parquet, card, manifest, and downloaded remote rows
    validate schemas, joins, units, nulls, hashes, citations, licenses, and counts
    write immutable staging directory plus dataset card and release manifest

function publish_and_verify(release):
    atomically upload/delete with observed head as parent_commit
    require exact remote-tree equality and file bytes at returned hf_commit_sha
    require head == hf_commit_sha before viewer checks
    poll datasets-server validity with bounded retry/backoff
    verify exact configs/splits/features/counts/primary keys/release_id
    require head == hf_commit_sha after viewer checks
    deploy website exact commit and verify HTTP route/release_id only through issue #75
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
| #1603 | Create | `src/digitalmodel/riser_database/source_registry.py` | orthogonal governance, field lineage, and release eligibility |
| #1603 | Create | `src/digitalmodel/riser_database/analysis_case.py` | normalized configuration/case models and canonical hashes |
| #1603 | Create | `src/digitalmodel/riser_database/orcaflex_adapter.py` | fail-closed `ProjectInputSpec` projection and bundle manifest |
| #1603 | Add | versioned schema/golden/conformance fixtures at paths frozen by the child plan | cross-repo contract enforcement on Linux and Windows |
| #1603 | Modify | `src/digitalmodel/riser_database/loader.py` | register new public tables and iterator coverage |
| #1603 | Modify | `data/riser_database/manifest.yaml`, `scripts/riser_database/build_tables.py`, `scripts/riser_database/sources.yml` | build/manifest/provenance contract |
| #1603 | Create/modify | `tests/riser_database/`, `tests/orcaflex/test_riser_input_schema.py`, `tests/solvers/orcaflex/modular_generator/` | D1 TDD and deterministic mapping |
| #811/#138 | Refactor | `scripts/run_riser_analysis.py` plus importable modules under `src/digitalmodel/solvers/orcaflex/risers/` | lazy solver import and typed analysis boundary |
| #811/#138 | Create/modify | dedicated unit/integration tests under `tests/solvers/orcaflex/` and `tests/scripts/` | fake-model unit tests plus licensed integration contract |
| #568 | Modify | `src/deckhand/licensed_run_agent.py`, `licensed_run_agent_runtime.py`, `licensed_run_queue.py` | signed envelope, artifact data-plane references, semantic status |
| #568 | Modify | `config/deckhand/policy.yml`, `scripts/deckhand/verify-ace-win-2.py`, Deckhand tests/runbook | workflow allowlist, trust registry, early/current host attestation |
| #1604 | Create | `src/digitalmodel/riser_database/release.py` and `scripts/riser_database/publish_hf_release.py` | decoded allowlisted release, atomic exact-tree HF mutation, semantic verification |
| #1604 | Create/modify | `tests/riser_database/test_release.py`, publisher tests, `docs/riser_database.md` | release TDD and dataset contract |
| #1604 | Create | `scripts/riser_database/verify_program_closeout.py` | deterministic child/HF/website/completeness closeout evidence |
| #75 | Modify | `config/capabilities.yaml`, `scripts/render-capabilities.js`, `assets/js/capabilities-refresh.js`, online/offline validators and DOM tests | remove withheld columns in both render paths and register/deploy only verified data |

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
| orthogonal governance combination matrix | #1603 | canonical/public/synthetic/license states are not conflated |
| complete multi-source field-lineage mutation tests | #1603 | every normalized field and transformation is traceable and hash-bound |
| clean-room environment/author/reviewer evidence | #1603 | synthetic promotion needs auditable non-derivation evidence, not a label |
| ambiguous units/non-finite/unmapped field rejection | #1603 | normalization and adapter fail closed |
| remove every required engineering field | #1603 | no Pydantic/generator default can silently fill SSOT input |
| exhaustive field-disposition coverage | #1603 | every contract field is mapped, metadata-only, execution/result/publication-only, or rejected |
| public iterator/leak/provenance/citation coverage | #1603 | every public row reaches mandatory gates |
| Linux/Windows golden vectors and hash mutation matrix | #1603/#568 | canonical identity is cross-platform and every declared input matters |
| complete transitive bundle and safe archive extraction | #1603/#568 | added/deleted/stale/path-collision/link/tampered files fail |
| two generations plus semantic load/native save | #1603 | deterministic and valid OrcaFlex model, not merely repeatable YAML |
| explicit coordinate-frame/sign/sweep coverage | #811/#138 | requested operability matrix is complete and unambiguous |
| utilization absent when capacity/citation is incomplete | #138 | calc citation contract fails closed |
| required-output matrix and missing-case cardinality | #138 | no requested quantity/location/case can silently disappear |
| equilibrium/geometry/M-EI/stress/rotation/trend oracles | #138 | independent checks do not reuse the extraction implementation |
| fake OrcFxAPI cannot claim real solver result | #138/#568 | mock/real truth boundary |
| license unavailable or semantic failure despite rc=0 | #568 | structured failure overrides process return code |
| tampered case/bundle/model/output hash rejection | #568 | end-to-end identity is enforced |
| signature/nonce/expiry/revocation/canary/lock failures | #568 | unauthenticated or stale host claims cannot pass |
| typed result safe retrieval and Linux rehash | #568 | host-local paths/bytes cannot be trusted implicitly |
| every result joins to case + attested execution | #1604 | orphan or mixed-revision rows cannot publish |
| exact thirteen-config schema/key/null/enum/extension manifest | #1604 | implementation cannot choose a self-consistent but wrong release |
| decoded staged/remote Parquet + card/manifest leak corpus | #1604 | binary files, stale files, paths, identities, secrets, mocks, and license conflicts fail |
| atomic delete/upload, parent race, and exact remote tree | #1604 | stale public files and concurrent commits cannot pass |
| semantic is-valid/splits/rows and head-before/after | #1604 | viewer proof is bound to exact release/configs/counts/features/keys |
| closeout verifier negative matrix | #1604 | missing approval/review/SHA/legal/completeness/HF/site evidence blocks parent close |
| fail-closed online validation/deploy/route checks | #75 | warnings or registry metadata cannot masquerade as a live capability |

Licensed integration tests will be tagged and will require the attested Windows
host. Unit tests will use fakes only to verify orchestration/failure behavior;
fake outputs will never satisfy release tests.

---

## Execution Sequence and Gates

1. **Plan the children.** Each D0–R2 owner will verify fresh `main`, check
   parallel work, write its plan, run the required adversarial review tier, and
   stop for user approval.
2. **Run E0 readiness early.** After #568's own plan approval, Deckhand will
   select the artifact backend and run a signed non-production
   API/DLL/license/statics canary. Failure will block production planning without
   waiting for D0–E2 implementation.
3. **Land D0 and D1.** #1046 will repair source integrity. #1603 will establish
   the normalized schema, source registry, synthetic completion references,
   field lineage, clean-room evidence, coordinate/identity contract,
   deterministic semantically loaded adapter, public gates, and conformance
   fixtures. #1603 will not absorb #809's CLI-dispatch scope.
4. **Land E1 and E2a.** #811 will remain governed by #808/#809 unless its own
   approved plan isolates a public-only slice. #138 will consume the D1 contract,
   freeze the full engineering matrix/criteria/oracles, and land unit-testable
   extraction without claiming licensed validation.
5. **Run E3 and E2b.** #568 will transfer the exact bundle, authenticate the
   production solve, and return signed typed results. #138 will then complete
   licensed extraction/oracle validation; neither issue closes before E2b.
6. **Build and publish R1.** #1604 will generate the exact thirteen-config release,
   decode and scan it, pass
   unit/integration/legal/leak/citation checks, upload once, verify immutable HF
   bytes and exact tree, then verify head-bound datasets-server semantics.
7. **Deploy R2.** #75 will fail-closed validate, refresh, test, build, deploy the
   exact website commit, and verify the public route/release ID.
8. **Close the parent.** #1604's closeout verifier will validate child approvals,
   non-empty reviews, landed SHAs, legal/code-review evidence, completeness
   threshold/owner label, exact HF state, and deployed website route. A scope
   reduction must be a signed user decision linked from the parent issue.

Parallelism is allowed only where dependencies permit it. E0 can run alongside
D0/D1 planning after #568 approval. #1046 and clean-room protocol implementation
may use separate worktrees after their approvals. E2a may begin after the D1
contract lands; production E3 waits for exact bundle/result schemas. R1 may
develop against signed conformance fixtures but cannot publish before E2b/E3.

---

## Acceptance Criteria

- [ ] Every child issue has a canonical plan, adversarial review evidence, and
  explicit user approval before implementation.
- [ ] The live source inventory classifies every candidate as canonical,
  duplicate, synthetic, public, private, licensed, derived, or withheld across
  the contract's independent governance dimensions.
- [ ] Every normalized engineering field has complete ordered lineage with
  source locator/hash, transformation, units, and a hash-bound lineage set.
- [ ] WED CSV repair preserves all 36 IDs/text and rejects any future wrong-width
  row before pandas parsing.
- [ ] Solver-neutral normalized configurations and analysis cases are the SSOT;
  OrcaFlex files are deterministic derived artifacts.
- [ ] Public completion/workover cases are explicitly synthetic and demonstrably
  satisfy clean-room author-environment, declaration, public citation/rationale,
  and independent private disclosure-review evidence.
- [ ] Linux and Windows produce the same contract golden hashes; every declared
  identity mutation changes the appropriate hash and implicit defaults fail.
- [ ] Every eligible configuration has a baseline statics request and the
  reviewed configuration-driven operability matrix; every ineligible or failed
  case has a typed reason record.
- [ ] Real solver results attest case, bundle, model, code, solver/DLL, license,
  host role, result hashes, semantic solver success, freshness/canary/lock, and a
  valid non-revoked protected-host signature.
- [ ] `MODEL_VERIFIED` proves transitive closure, zero validator errors, real
  OrcFxAPI load, object/connection/unit expectations, and solver-native save.
- [ ] Strength utilization and standards-derived limits fail closed without
  capacity basis, units, revision, and `Citation` sidecars.
- [ ] Independent engineering spot checks and physical plausibility gates pass;
  implementation formulas are not reused as their own oracle.
- [ ] The public release contains only the thirteen exact versioned configs and
  contract-defined columns/types/keys/nullability/enums;
  scalar fields; no raw DAT/YAML, private paths, client/project identifiers,
  secret-like values, licensed content, or mock results appear.
- [ ] Every public result joins to exactly one published case, normalized
  configuration, attested execution, and result hash.
- [ ] `aceengineer/digitalmodel-riser-analysis` files match the immutable returned
  HF commit SHA, the exact remote tree equals the manifest, head remains pinned
  before/after polling, and semantic validity/splits/rows checks pass.
- [ ] The dataset card reports schema versions, row counts, units, nulls,
  licenses, source hashes, solver limitations, synthetic labeling, and safe
  withheld-source reasons.
- [ ] Website issue #75 alone owns registration; fail-closed online validation,
  refresh, tests, build, exact-commit deployment, HTTP 200, and expected release
  ID rendering pass after the verified dataset is live.
- [ ] Each repo's focused/full tests, security/leak checks, and
  `scripts/legal/legal-sanity-scan.sh` pass where applicable.
- [ ] T3 code/artifact cross-review has no unresolved MAJOR findings.
- [ ] The parent issue receives implementation summaries with inline issue URLs,
  a completeness score/report, named residual risks, cleanup audit evidence, and
  a passing deterministic program-closeout verifier.

### Parent plan verification commands

```bash
# Plan/design structure
xmllint --html --noout docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html
uvx check-jsonschema \
  --schemafile docs/plans/issue-1602-riser-contract-meta-schema-v1.json \
  docs/plans/issue-1602-riser-normalized-schema-v1.yaml \
  docs/plans/issue-1602-riser-release-schema-v1.yaml \
  docs/plans/issue-1602-riser-protocol-v1.yaml
uv run --no-project --with pyyaml python - <<'PY'
from pathlib import Path
import yaml

root = Path("docs/plans")
normalized = yaml.safe_load((root / "issue-1602-riser-normalized-schema-v1.yaml").read_text())
release = yaml.safe_load((root / "issue-1602-riser-release-schema-v1.yaml").read_text())
protocol = yaml.safe_load((root / "issue-1602-riser-protocol-v1.yaml").read_text())
version = "1.0.0-draft.5"
assert {normalized["schema_version"], release["schema_version"],
        release["row_common"]["schema_version"]["const"],
        protocol["protocol_version"]} == {version}
expected = {"source_registry", "transformations", "citations", "criteria",
            "clean_room_reviews", "field_lineage", "riser_components",
            "riser_configurations", "analysis_cases", "execution_attestations",
            "strength_results", "operability_results", "withheld_sources"}
assert set(release["configs"]) == expected
assert release["enums"]["source_confidentiality"] == ["public"]
assert release["enums"]["redistribution"] == ["approved"]
assert "criteria" in release["configs"]
assert {"tree", "lower_package"} <= set(release["enums"]["location_kind"])
for entity in normalized["entities"].values():
    for field in entity["fields"].values():
        assert "disposition" in field
for config in release["configs"].values():
    for key in config["primary_key"]:
        field = config["columns"][key]
        assert field["nullable"] is False and field["type"] not in {"float32", "float64"}
refs = normalized["referential_integrity"]
for entity_name, entity in normalized["entities"].items():
    for field_name, field in entity["fields"].items():
        if "ref." in field["type"]:
            assert any(row.startswith(f"{entity_name}.{field_name} ->") for row in refs), (entity_name, field_name)
order = protocol["identity_construction_order"]
pos = lambda token: next(i for i, row in enumerate(order) if token in row)
for subject in ("criterion", "component", "connection", "boundary_attachment"):
    assert pos(f"{subject}_seed_sha256 =") < pos(f"{subject}_lineage_ids =")
assert pos("configuration_seed_sha256 =") < pos("configuration_lineage_ids =")
assert pos("normalized_input_sha256 =") < pos("configuration_id =") < pos("author_claim_sha256 =") < pos("review_claim_sha256 =")
assert pos("request_id =") < pos("bundle_sha256 =") < pos("model_sha256 =") < pos("execution_id =")
assert all("without_id_release_id_or_result_payload_hash" in row for row in order if "result_id =" in row)
clean = release["configs"]["clean_room_reviews"]["columns"]
assert {"configuration_seed_sha256", "private_evidence_id",
        "blinded_evidence_commitment", "author_issued_at",
        "reviewer_issued_at", "author_principal_id", "reviewer_principal_id",
        "trust_registry_version", "author_claim_sha256"} <= set(clean)
assert not {"private_corpus_sha256", "comparison_corpus_manifest_sha256"} & set(clean)
joins = set(release["cross_table_constraints"])
assert len(joins) >= 6 and any("semantic_status_verified_success" in x for x in joins)
assert any("noncriterion_strength_result" in x for x in joins)
assert any("quantity_and_unit" in x for x in joins)
component_entities = {"line_section_properties", "flex_joint_properties",
                      "telescopic_joint_properties", "tensioner_properties",
                      "rigid_package_properties", "connector_properties"}
assert component_entities <= set(normalized["entities"])
component_fields = normalized["entities"]["component"]["fields"]
assert "component_variant" in component_fields and "length_m" not in component_fields
variant_map = protocol["family_output_matrix"]["component_variant_map"]
assert set().union(*map(set, variant_map.values())) == set(normalized["enums"]["component_type"])
case_columns = release["configs"]["analysis_cases"]["columns"]
assert {"wave_model", "wave_gamma", "current_profile_jcs", "internal_pressure_pa",
        "external_pressure_model_jcs", "seawater_density_kg_m3", "gravity_m_s2",
        "seabed_contact_model_jcs", "wave_period_basis", "wave_time_origin_s",
        "criterion_ids_jcs", "dynamics_seed"} <= set(case_columns)
config_columns = release["configs"]["riser_configurations"]["columns"]
assert {"connection_graph_jcs", "boundary_attachments_jcs",
        "vessel_motion_mode", "vessel_motion_jcs"} <= set(config_columns)
att = release["configs"]["execution_attestations"]["columns"]
assert {"request_expires_at", "canary_issued_at", "canary_attestation_sha256",
        "canary_claims_jcs", "canary_signature_base64",
        "canary_verification_receipt_sha256", "solver_run_started_at",
        "solver_run_ended_at"} <= set(att)
assert protocol["coordinate_adapter_ENU_TRUE_NORTH_1_0_0"]["current_mapping"]["reference_speed"]
assert protocol["decoded_leak_scan"]["size_skip"] == "prohibited"
assert "ruleset_sha256" in protocol["decoded_leak_scan"]["evidence"]
assert protocol["model_and_bundle"]["model_dependency_resolver"]["method"].startswith("parse_")
assert protocol["transport_zip_store_v1"]["general_purpose_bit_11_UTF8"] == "set"
closeout = protocol["closeout_evidence_manifest"]
assert closeout["type"] == "object" and closeout["additionalProperties"] is False
assert set(closeout["required"]) == set(closeout["properties"])
assert protocol["solver_semantic_success"]["verification"]
assert protocol["clean_room_attestation"]["trust_registry"]["independence"]
print("issue-1602 planning semantic checks: PASS")
PY
for f in docs/plans/issue-1602-riser-*-v1.yaml \
  docs/plans/issue-1602-riser-contract-meta-schema-v1.json; do
  test "$(wc -l < "$f")" -le 400
done
! rg -n 'TO''DO|TB''D|<re''po>|N''NN' \
  docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md

# Plan/index and review artifacts
rg -n 'issue-1602-riser-data-orcaflex-hf-plan' docs/plans/README.md
test -s scripts/review/results/issue-1602-round-1/2026-07-16-plan-1602-fallback-data.md
test -s scripts/review/results/issue-1602-round-1/2026-07-16-plan-1602-fallback-solver.md
test -s scripts/review/results/issue-1602-round-1/2026-07-16-plan-1602-fallback-release.md
test -s scripts/review/results/issue-1602-round-2/2026-07-16-plan-1602-data.md
test -s scripts/review/results/issue-1602-round-2/2026-07-16-plan-1602-solver.md
test -s scripts/review/results/issue-1602-round-2/2026-07-16-plan-1602-release.md
test -s scripts/review/results/issue-1602-round-3/2026-07-16-plan-1602-data.md
test -s scripts/review/results/issue-1602-round-3/2026-07-16-plan-1602-solver.md
test -s scripts/review/results/issue-1602-round-3/2026-07-16-plan-1602-release.md
test -s scripts/review/results/issue-1602-round-4/2026-07-16-plan-1602-data.md
test -s scripts/review/results/issue-1602-round-4/2026-07-16-plan-1602-solver.md
test -s scripts/review/results/issue-1602-round-4/2026-07-16-plan-1602-release.md
test -s scripts/review/results/issue-1602-round-5/2026-07-16-plan-1602-data.md
test -s scripts/review/results/issue-1602-round-5/2026-07-16-plan-1602-solver.md
test -s scripts/review/results/issue-1602-round-5/2026-07-16-plan-1602-release.md

# Legal gates before and after committing the reviewed artifacts
cd ../../workspace-hub
bash scripts/legal/legal-sanity-scan.sh \
  --repo=../agent-worktrees/dm-1602-design --diff-only
bash scripts/legal/legal-sanity-scan.sh \
  --repo=../agent-worktrees/dm-1602-design
```

---

## Adversarial Review Summary

Round 1 provider CLIs were UNAVAILABLE: Claude timed out, Codex hit its known
stdin regression, and Gemini lacked non-interactive auth. Three independent
fallback reviewers then returned MAJOR on data/provenance, solver/engineering,
and release/security. Their findings produced the normative machine contract,
scope reconciliation, authenticated execution, exact release schemas, HF race /
stale-file defenses, and executable closeout gate in this revision.

Round 3 found remaining identity cycles, under-specified physical inputs,
unsafe public provenance fields, incomplete criteria/result joins, and
incomplete execution/transport semantics. Draft.4 closed those first-order
gaps, but Round 4 found physically invalid shared component fields, incomplete
boundary/environment inputs, unstable attestation-dependent engineering IDs,
missing subject-lineage bindings, incomplete canary/semantic-success proof,
over-broad result rules, and descriptive closeout evidence. Draft.5 will use
discriminated physical variants, explicit boundary and self-contained motion /
pressure/environment inputs, stable engineering IDs, every-subject lineage,
trusted author/reviewer roles, a provisional staging ID, typed canary and
semantic-success rules, conditional criterion joins, ruleset-bound leak proof,
and a machine-shaped closeout schema. Round 5 is pending. Any unresolved MAJOR
will keep the plan in `draft`.

| Provider | Verdict | Key findings |
|---|---|---|
| Initial provider fanout | UNAVAILABLE | Claude timeout; Codex stdin regression; Gemini auth unavailable |
| Fallback data/provenance | MAJOR | lineage, governance dimensions, hash spec, trust, clean room, scope/schema/defaults |
| Fallback solver/engineering | MAJOR | field disposition, semantic model load, coordinates, transport/signing, outputs/oracles, sequencing |
| Fallback release/security | MAJOR | exact schemas/tree/head, semantic viewer checks, decoded leak gate, website/closeout truth |
| Round 2 data | MAJOR | normalized fields/dispositions, decimal/hash construction, governance, clean room, enums/FKs/keys/citations/transforms |
| Round 2 solver | MAJOR | identity cycles, coordinate vectors, signed bytes, archive limits, result cardinality, oracle applicability/sampling/family matrix |
| Round 2 release | MAJOR | identity cycles, semantic schemas, website withholding, clean room, authoritative closeout, exact-revision validation |
| Round 3 data | MAJOR | identity cycles; incomplete component/topology/vessel/case inputs; public source and clean-room leakage; missing criteria and semantic validation |
| Round 3 solver | MAJOR | construction/hash ambiguity; veering-current mapping; incomplete locations/host claims; runtime dependency and ZIP semantics |
| Round 3 release | MAJOR | signed-time and clean-room cycles; missing result composite joins; incomplete decoded leak scan and typed closeout evidence |
| Round 4 data | MAJOR | criterion/subject lineage, unstable identity, reviewer principals, opaque motion/pressure, invalid component shape, compatibility, validator coverage |
| Round 4 solver | MAJOR | discriminated physics/boundaries/environment, staging cycle, canary/semantic success, output applicability, oracle predicates |
| Round 4 release | MAJOR | clean-room trust/method, chronology/canary, conditional criteria, lineage seeds, typed closeout, leak ruleset binding |
| Round 5 data | PENDING | — |
| Round 5 solver | PENDING | — |
| Round 5 release | PENDING | — |

**Overall result:** PENDING

---

## Risks and Open Questions

- **Current licensed-host readiness:** Deckhand real execution is disabled and
  historical host evidence is stale. #568 must select the artifact backend and
  prove a signed current real statics canary at E0 before production work.
- **Source provenance:** Repairing CSV alignment will not manufacture missing
  field-level lineage. Records without sufficient provenance will remain
  withheld even when syntactically valid.
- **Standards rights:** Ledger/wiki availability does not grant republication.
  Public artifacts will carry identifiers/citations, not licensed content. The
  child criteria matrix must cover API/ISO, API STD 2RD, DNV-OS-F201,
  DNV-RP-C203, and AMJIG applicability and prohibit fail-open citation wrappers.
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
- **Parent decisions fixed; child decisions remain gated:** Option 1, the SSOT
  boundary, raw-model exclusion, clean-room synthetic completion policy, target
  HF repository, exact cross-layer contract, and child gate model are fixed.
  #568's artifact backend, #138's numerical criteria/tolerances/sweep rows, and
  #1604's compatible public license will be frozen in their own reviewed plans
  and require explicit user approval before implementation.

---

## Complexity: T3

T3 is required because this program spans four implementation repositories plus
workspace-hub operating infrastructure, licensed engineering software, public
data publication, standards-derived calculations, confidentiality boundaries,
and a multi-stage identity chain. Both plan-stage and code/artifact-stage
cross-provider adversarial review are mandatory.
