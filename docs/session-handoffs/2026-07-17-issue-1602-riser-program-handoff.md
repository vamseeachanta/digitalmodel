# Handoff prompt — riser models, visuals, envelopes, and monitoring

You are the next engineering agent continuing the owner-approved solver-neutral
riser program in `vamseeachanta/digitalmodel`. Your immediate job is to advance
the next authorized lifecycle gate, not to bypass child issue approvals or run
licensed software opportunistically.

## Outcome

Build a traceable pipeline that will:

1. consume the solver-neutral riser configuration and analysis-case SSOT;
2. generate deterministic OrcaFlex v1 model bundles as derived target artifacts;
3. render engineering visuals in the digitalmodel repository;
4. execute reviewed cases through the licensed `ace-win-1` Deckhand lane;
5. extract verified strength and operability results and operating envelopes;
6. support a later real-time monitoring integration that tracks operating
   margin and data quality, measures decision-support performance, and evaluates
   potential risk reduction.

OrcaFlex is the first adapter target, not the canonical data model. A custom FEA
or another solver must consume the same immutable generic case and analysis
contracts through its own reviewed adapter.

## Authoritative starting point

- Parent issue: https://github.com/vamseeachanta/digitalmodel/issues/1602
- Parent status: `status:plan-approved`
- Approved parent plan:
  `docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md`
- Parent interface:
  `docs/plans/issue-1602-riser-analysis-contract-v1.yaml`
- Approved design:
  `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html`
- Approval evidence commit:
  `3e93aa8f3ab13914b4a4c182e1a8d69cbb61e945`
- Round 16 unanimous review:
  `scripts/review/results/issue-1602-round-16/`

Approved #1602 producer ownership and the new companion visualization issue are
divided as follows:

- normalized SSOT, provenance, generic parameters, deterministic target adapter
  and bundle: https://github.com/vamseeachanta/digitalmodel/issues/1603
- drilling request matrix: https://github.com/vamseeachanta/digitalmodel/issues/811
- model/result semantics, OrcaFlex v1 output/result extraction mappings,
  engineering oracles, and strength/operability results:
  https://github.com/vamseeachanta/digitalmodel/issues/138
- authenticated licensed execution and hashed result return:
  https://github.com/vamseeachanta/deckhand/issues/568
- public Hugging Face release and verification:
  https://github.com/vamseeachanta/digitalmodel/issues/1604
- visualization and reporting projection layer:
  https://github.com/vamseeachanta/digitalmodel/issues/1605

#1605 records the requested companion scope but is not implementation-authorized
and does not enter or block the approved #1602 closeout DAG unless the parent
plan is separately revised, reviewed, and owner-approved.

Closed/existing reusable capabilities are dependencies, not new #1602 child
implementations:

- #1279 owns generic operating-envelope physics, atlas, interpolation, and
  serving behavior: https://github.com/vamseeachanta/digitalmodel/issues/1279
- #1469 owns population of #1279's solver-backed envelope tier with licensed
  OrcaFlex cases: https://github.com/vamseeachanta/digitalmodel/issues/1469
- #1284 delivered the operating-envelope capabilities explorer:
  https://github.com/vamseeachanta/digitalmodel/issues/1284
- #1372–#1377 own the drilling-riser live twin, telemetry, correction,
  drift-off/time-to-limit, live operability, and replayable go/no-go flow:
  https://github.com/vamseeachanta/digitalmodel/issues/1372

The parent approval does **not** self-approve any child implementation. Each
child must complete Issue → Resource Intel → future-tense Plan → adversarial
review → `status:plan-review` → explicit owner approval →
`status:plan-approved` → TDD implementation → code/artifact review → closeout.

## First action

Check parallel work and live GitHub state. Perform discovery-first against the
current default branch and active worktrees. Then prepare the child plan for
digitalmodel #1603 using the canonical template at
`/mnt/local-analysis/workspace-hub/docs/plans/_template-issue-plan.md`. Its plan
must expose the parent-DAG checkpoints below rather than pretending #1603 can
produce bundles before #138/#811 contracts exist. Do not implement until the
user approves that child plan. If prior work already satisfies part of the
scope, prove it by commit, tests, and contract conformance before changing code.

## Non-negotiable architecture

The canonical SSOT contains generic engineering state only:

- topology and geometry;
- materials and section properties;
- contents and internal fluids;
- metocean, environment, and loads;
- boundary and initial conditions;
- analysis intent and stages;
- requested quantities and locations;
- acceptance criteria;
- units and coordinate frames.

It must not contain solver object names, solver file layouts, host or licence
settings, custom-FEA element names, or implicit solver defaults. Source models
are evidence. OrcaFlex files, native models, screenshots, execution payloads,
and results are derived artifacts. Adapters never back-write into the SSOT.
Unsupported or non-equivalent mappings produce an evidence-bound terminal
adapter failure and never reach execution.

Preserve the exact parent identity-chain order:

approved public source or safe provenance commitment → normalized configuration
→ analysis case → analysis request → solver target and adapter contract → solver
projection → deterministic bundle → execution request → native model → raw
solver payload → execution receipt → engineering-result payload → logical
release → HF commit → HF publication receipt → website evidence.

That order is not a claim that one displayed hash derives from the next. Enforce
the parent YAML's envelope equality bindings separately: the normalized case
binds configuration, case, generic-contract, provenance, and eligibility
identities; the selected family request preserves case and generic-contract
identities and adds request/model-contract identity; the bundle preserves those
and adds target, adapter, projection, target-gap, bundle, and manifest identity;
execution and result envelopes preserve every required upstream binding.

## Delivery sequence

### Phase A1 — normalized SSOT readiness

Plan and implement digitalmodel #1603 only after its owner approval, with an
explicit checkpoint after normalized-case readiness. At that checkpoint it will
produce deterministic normalized drilling and completion/workover cases, the
generic parameter contract, provenance/lineage and eligibility evidence,
coordinate contracts, exhaustive field disposition, and the target-adapter
interface. It will not yet produce executable bundles. Linux-side contract/unit
tests must not depend on `OrcFxAPI`.

### Phase A2 — analysis and family request readiness

After normalized-case readiness, take digitalmodel #138 and #811 independently
through planning, adversarial review, and owner approval. #138 will freeze the
pre-run model/result/criteria/oracle contracts and completion/workover request
matrix. #811 will freeze the drilling request matrix. #138 owns analysis-stage
semantics and requested-output mappings; it does not own generic-case to
OrcaFlex input object/discretization mapping.

### Phase A3 — OrcaFlex input projection and deterministic bundles

Return to the approved #1603 implementation only after the selected family
request and #138 model contract are available. #1603 will then own generic-case
to OrcaFlex input projection, object/discretization mapping, target-semantic
coverage/gap evidence, deterministic bundle bytes, file manifests, and
cross-platform conformance fixtures. Committed fixtures may contain public-safe
canonical inputs, mapping evidence, manifests, and expected hashes only. Native
bundle/model bytes, complete solver-object dumps, and reconstructable target
inputs remain ignored/ephemeral and host-local.

### Phase A4 — visualization contract and pre-run visuals

Plan digitalmodel #1605 at its own gate. #1605, not #1603, owns visual
projection/manifests, renderers, and reports. #1603 remains a producer of
normalized-case and target projection/bundle evidence; it must not acquire a
renderer dependency.

#1605's plan must freeze two completion checkpoints: pre-run canonical/target
views after Phase A3, and post-run response/envelope views only after verified
#138/#568 result evidence. The issue cannot close on pre-run renderers while its
post-run acceptance scope remains open.

The #1605 plan will produce derived, testable artifacts rather than SSOT fields:

1. **Canonical configuration view** — labelled vertical stack-up/profile,
   component roles, dimensions, boundaries, datum, coordinate frame, and
   critical locations from generic data.
2. **Target projection view** — the OrcaFlex object mapping and discretization,
   explicitly labelled as a derived OrcaFlex projection.
3. **Model preview** — a deterministic 2D profile and, where useful, interactive
   3D HTML scene with vessel/top connection, riser centreline, seabed/wellhead,
   current direction, offsets, and component legend.

Use an allowlisted visual-projection schema. Prohibit native model bytes, full
solver-object dumps, reconstructable target inputs, machine paths, secrets, and
client/project identifiers in committed fixtures, HTML, JSON, images, or image
metadata. Decode and scan HTML/JSON payloads and image metadata. Bind every
visual manifest to public eligibility, normalized-case envelope, target adapter
and projection identities, generator commit, plot-data hash, and artifact
hashes.

Prefer repository-native plotting/reporting code and self-contained HTML. Keep
plot-data small, deterministic, schema-versioned, and hash-bound. Test labels,
axes, units, coordinates, critical-point placement, deterministic serialization,
and public-data eligibility. Never publish a visual derived from a withheld or
confidential case. Inventory and reuse applicable patterns from
`scripts/capture_riser_views.py`, `scripts/generate_schematic.py`,
`scripts/visualize_load_cases.py`, and the existing OrcaFlex HTML validation or
model-library reports before adding a new visualization framework. Verify every
cited path against the live tree; do not assume historical example directories
are present.

Treat repository visuals as independent engineering artifacts labelled “not an
HF release artifact” until #1604 has published and verified the release. A visual
presented as a released/public #1602 result must first consume and bind
`logical_release_sha256`, `HF_commit_sha`, and
`HF_publication_receipt_sha256`; otherwise it must not claim release parity.

### Phase B — solver semantics and licensed execution

After Phases A1–A3, complete the reviewed #138 execution-authorizer boundary and
plan Deckhand #568 at its own gate. #138 owns generic requested quantities,
locations, stages/statistics, criteria, engineering oracles, and OrcaFlex
output/result extraction mappings. Deckhand owns request verification,
`ace-win-1` route policy, licence/canary proof, execution, signed receipts, and
content-addressed result return.

Treat `ace-win-1` as a logical Deckhand route, not a public physical hostname.
Do not infer or publish the underlying workstation mapping. Use an explicit
addressed route; do not change the committed default owner for unaddressed
requests. Keep `.sim`, `.dat`, model databases, and other heavy/native solver
artifacts host-local. Return only the bounded typed artifacts permitted by the
reviewed #568 data plane.

No current workflow satisfies #568. Do not dispatch the existing
`orcaflex-strength-post` post-processor as a substitute for bundle generation,
solve, and signed semantic receipt. Historical `ace-win-1` readiness is only
point-in-time evidence; each run needs fresh private host attestation, verifier,
heartbeat, seat lock, real canary, and explicitly addressed routing.

Do not run OrcaFlex merely because the host is reachable. Execution requires:

- approved child plans and implementations;
- a current real canary;
- an authorized immutable request;
- complete supported target-semantic evidence;
- exact bundle and native-model hashes;
- typed requested stages and outputs;
- an independently signed execution receipt;
- Linux-side hash verification before engineering acceptance.

Return-code zero is not semantic success. Mock or reduced-order output must
never be labelled OrcaFlex output.

### Phase C — verified results, existing envelope engine, and response visuals

For each eligible case, produce only results tied to the signed receipt and
engineering-result contract. At minimum cover the reviewed #138 quantities and
criteria for strength and operability, including critical-location values and
governing load case.

Before execution, #138 must freeze visualization-facing requested quantities,
locations, stages/statistics, coordinate frames, units, and bounded
sampling/downsampling semantics. #568 must return that typed numeric data only
through its reviewed, signed, hash-verified data plane. #1605 may render it only
after Linux-side hash and schema verification. Missing data remains explicitly
uncomputed; never reopen host-local native solver files or synthesize profiles.

#1279 remains the owner of generic operating-envelope physics, atlas,
interpolation, and serving behavior. Require discovery and conformance tests
against its landed `envelope.py`, `operability_atlas.py`, and fixtures before
adding logic. Before Phase C, inspect and disposition open #1469, which already
owns population of #1279's solver-backed tier, including dynamic envelope,
drift-off, recoil, and hang-off coverage. Route verified #138/#568 result
envelopes into #1469's reviewed population workflow. #1469 must pass its own
plan and approval gates before work resumes, unless the owner explicitly
approves supersession or closure. #1605 remains render-only and consumes the
populated #1279/#1469 output.

#1605 must also reuse or extend #1284's delivered operating-envelope explorer
surfaces rather than independently implementing another envelope viewer.

#1605 will render:

- static and dynamic riser centreline/deformed-shape views when available;
- tension, bending moment, stress/utilisation, curvature, and angle profiles by
  arc length using generic quantity/location identities;
- critical-location callouts and governing-criterion evidence;
- operating-envelope plots/tables over the approved sweep dimensions such as
  vessel offset, current, wave/seastate, top tension, internal fluid/mud weight,
  and water depth where applicable;
- explicit feasible, caution, failed, uncomputed, and unsupported regions;
- margin-to-limit, uncertainty/coverage, interpolation status, and governing
  criterion for every displayed envelope point.

Do not interpolate across failed, unsupported, or semantically non-equivalent
target cases. Preserve sparse truth where coverage is incomplete. Every plotted
point must resolve to case, request, model and result contracts, bundle, solver
target/adapter/projection and target-gap evidence, native-model/raw-payload
identity through the receipt, engineering-result envelope, criterion/oracle,
code commit, plot-data hash, and visual artifact hashes.

### Phase D — real-time monitoring and risk tracking

This is a non-blocking post-#1602 follow-on. It requires its own issue, plan,
review, and owner approval; it neither blocks nor may be claimed by the #1602
closeout verifier.

Do not create another generic telemetry, digital-twin, monitoring, atlas, or
dashboard issue. Reuse the existing capability owned by #1279 and #1372–#1377,
including telemetry ingestion, response correction, drift-off/time-to-limit,
live operability, and replayable go/no-go flow. First inspect their current code,
tests, and issue status.

Create a new issue only for the uncovered integration boundary, if live
discovery confirms it remains uncovered: **Bind #1602 solver-neutral release
evidence into the drilling-riser live twin and operational alerts.** That issue
must consume existing components rather than reimplement them.

Keep observed operational state separate from the immutable design SSOT:

- existing telemetry adapters map source-specific tags into generic observed
  quantities, units, frames, locations, timestamps, and quality flags;
- observations and state estimates never overwrite the canonical design case;
- the monitoring engine compares validated observations/state estimates with a
  versioned, verified operating envelope;
- output includes current margin, governing limit, trend, confidence,
  envelope/result release identity, data age, and data-quality state;
- missing, stale, out-of-range, contradictory, or frame/unit-invalid data fail
  visibly and cannot produce a green status;
- interpolation/surrogate use is explicit, bounded, validated, and distinguishable
  from a fresh OrcaFlex solve;
- alerts use reviewed hysteresis, persistence, acknowledgement, audit, and replay
  semantics;
- outputs remain engineering decision support until separately validated and
  authorized for any safety-critical claim.

Define risk reduction in measurable terms: earlier limit approach detection,
reduced time outside the verified envelope, fewer stale-data decisions,
traceable response to excursions, and quantified false-positive/false-negative
performance. Do not claim reduced risk without baseline and validation evidence.

The follow-on integration issue must bind each monitoring state/event to exact
`HF_repo_id`, `HF_commit_sha`, `logical_release_sha256`,
`HF_publication_receipt_sha256`, `configuration_id`, `case_id`,
`generic_parameter_contract_sha256`, solver target/adapter/projection identities,
engineering-result envelope and committed result-set identity, and operating-
envelope version/hash. It must reject mutable head references, unverified
commits, and release/receipt mismatches. It excludes new physics, new generic
telemetry ingestion, atlas generation, and dashboard reimplementation.

## Verification and governance

- Use TDD: failing contract/unit tests before implementation.
- Run focused tests, relevant integration tests, and the full required suite.
  From `/mnt/local-analysis/workspace-hub`, run the full applicable child scan
  with a workspace-hub-relative repository path before child closeout; diff-only
  scans are insufficient for implementation. For this worktree, the form is:

  ```bash
  cd /mnt/local-analysis/workspace-hub
  bash scripts/legal/legal-sanity-scan.sh \
    --repo=../agent-worktrees/dm-1602-design
  ```

  Replace only the final worktree name when a child uses a different worktree.
- Apply the calculation citation contract to standards-derived constants and
  criteria; fail closed when citation evidence is absent.
- Run adversarial plan and code/artifact reviews at the required tier. Prompts
  must hunt for defects and default to non-APPROVE.
- Use explicit pathspec commits; preserve unrelated worktree changes.
- Comment every implemented issue with files, tests, review verdicts, commit,
  limitations, and next dependency.
- Run the pre-completion cleanup audit before handing back.

## Stop conditions

Stop and report the exact blocker instead of inventing inputs or weakening a
gate when any of the following occurs:

- a child lacks explicit owner `status:plan-approved` authorization;
- a source is malformed, confidential, legally unclear, or non-reproducible;
- a generic field maps only through an implicit target default;
- target semantics are unsupported or non-equivalent;
- licensed host, canary, request signature, receipt, or returned hash fails;
- a result or plot cannot trace to the exact generic case and solver evidence;
- telemetry quality cannot support the displayed monitoring state.

At each handback, report current state, evidence, blocker/gap, and the single
recommended next action. Do not claim the full program complete until parent
#1602 invokes and passes its reviewed closeout verifier.
