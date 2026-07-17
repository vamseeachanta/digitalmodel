# Plan for #1602: Riser Data, OrcaFlex Analysis, and Hugging Face Program

> **Status:** draft — boundary-refactored; adversarial review pending
> **Complexity:** T3
> **Date:** 2026-07-16
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1602
> **Client:** N/A
> **Lane:** lane:codex
> **Design:** `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html`
> **Parent interface contract:** `docs/plans/issue-1602-riser-analysis-contract-v1.yaml`
> **Historical review evidence:** `scripts/review/results/issue-1602-round-{1,2,3,4,5}/`
> **Boundary-refactor review:** `scripts/review/results/issue-1602-round-6/`

---

## Authorization Boundary

This plan will govern the cross-repository architecture, ownership, dependency
order, interface bindings, promotion gates, and final program evidence. It will
not define repository-owned production schemas or authorize child
implementation. Every child issue will require its own resource intelligence,
future-tense plan, adversarial review, explicit owner approval, TDD
implementation, artifact review, and closeout.

The approved architecture will remain unchanged:

- The solver-neutral normalized configuration and analysis case will be the
  single source of truth.
- Source DAT/YAML and legacy OrcaFlex models will be evidence only.
- Generated OrcaFlex bundles, native models, and results will be derived
  artifacts.
- Raw completion/workover models will not enter public artifacts.
- Public completion/workover inputs will be independently authored clean-room
  synthetic cases.
- Only verified real OrcaFlex results will be published as solver results.
- The global frame will remain right-handed ENU, +Z up from mean water level,
  with directions clockwise from true North and going-to, bow-to vessel heading,
  offsets at the top connection, and arc length from the top connection.

Exact nested fields, hash preimages, fixtures, receipts, and executable
validators will be frozen by the child that produces them. The parent contract
will freeze only the cross-layer envelope bindings and fail-closed invariants.

## Resource Intelligence Summary

Planning uses a `parallel-readonly` evidence phase and one orchestrator-owned
plan artifact. The following live state anchors the plan:

- [worldenergydata #1046](https://github.com/vamseeachanta/worldenergydata/issues/1046)
  owns the malformed drilling-riser CSV repair or fail-closed rejection proof.
- [digitalmodel #1603](https://github.com/vamseeachanta/digitalmodel/issues/1603)
  owns the normalized SSOT, source registry, lineage, deterministic case IDs,
  adapter, and bundle manifest.
- [digitalmodel #811](https://github.com/vamseeachanta/digitalmodel/issues/811)
  owns the drilling stackup and operability workflow and remains dependent on
  [#808](https://github.com/vamseeachanta/digitalmodel/issues/808) and
  [#809](https://github.com/vamseeachanta/digitalmodel/issues/809) for its
  legacy private-corpus path. Private material will not seed public rows.
- [digitalmodel #138](https://github.com/vamseeachanta/digitalmodel/issues/138)
  owns OrcaFlex drilling/completion model semantics, result quantities,
  criteria, extraction, engineering oracles, and licensed post-run validation.
- [Deckhand #568](https://github.com/vamseeachanta/deckhand/issues/568)
  owns authenticated request/canary/execution receipts, safe artifact transport,
  host/license proof, and hashed result return.
- [digitalmodel #1604](https://github.com/vamseeachanta/digitalmodel/issues/1604)
  owns exact public release schemas, decoded leak checks, atomic Hugging Face
  publication, remote semantic verification, and the executable parent closeout
  verifier.
- [aceengineer-website #75](https://github.com/vamseeachanta/aceengineer-website/issues/75)
  owns capability registration, withheld-column removal, build/deployment, and
  live-route verification.

Current technical constraints:

- The canonical public drilling component CSV contains 36 rows and 31 declared
  columns; 32 rows have fewer fields than the header and remain unsafe pending
  #1046.
- This Linux host does not provide `OrcFxAPI`; real analyses will require the
  licensed Windows execution lane.
- Deckhand execution remains disabled until a reviewed workflow and current
  real canary pass.
- The target public dataset will be
  `aceengineer/digitalmodel-riser-analysis`; no raw solver model files will be
  included.
- Standards indexing or local holdings will not grant republication rights.
  Public rows will carry identifiers and citations, never licensed text.

Reproduction proof is N/A because this parent issue coordinates a new program;
the CSV defect and missing local solver capability are intake constraints owned
by their child issues, not a parent runtime regression.

## Parent/Child Ownership Matrix

| Layer | Owner | Exact child-owned contract | Parent-retained invariant |
|---|---|---|---|
| Source repair | WED #1046 | CSV grammar, corrected rows, semantic anchors | malformed or ambiguous rows will not promote |
| Normalized SSOT | DM #1603 | nested schemas, IDs, lineage, clean-room records, adapter, bundle manifest | solver-neutral SSOT; no silent drops/defaults; public/private boundary |
| Drilling workflow | DM #811 | stackup inputs, CLI/output, tolerances | only approved public/synthetic cases may enter public path |
| Engineering analysis | DM #138 | model objects, load cases, quantities, criteria, formulas, oracles, cardinality | outputs will be complete, cited, independently checked, and bound to a case |
| Licensed execution | Deckhand #568 | request/canary/receipt schemas, signatures, transport, host policy | real licensed solver; immutable case/bundle/model/result binding; fail closed |
| Public release | DM #1604 | Parquet configs, joins, leak rules, manifest, HF transaction, closeout schema | public-safe rows only; exact immutable release; no mock or unsigned results |
| Website | Website #75 | capability schema, renderer changes, tests, deployment proof | verified HF release first; withheld values never render |

No child will redefine another child's output. A consumer may tighten validation
but will reject, not reinterpret, an unknown producer version.

## Cross-Layer Interface Contract

`docs/plans/issue-1602-riser-analysis-contract-v1.yaml` will be the sole
normative parent interface. It will define six envelope boundaries:

1. `normalized_case` will bind schema/version, case ID/hash, source class,
   confidentiality, redistribution, units, provenance, and eligibility.
2. `deterministic_bundle` will bind the case, adapter/generator version, bundle
   hash, and file-manifest hash.
3. `execution_request_and_receipt` will bind case/bundle/model hashes, workflow
   and solver identity, request identity, semantic status, result hash, signer,
   and receipt hash.
4. `engineering_result` will bind case, execution receipt, result-contract
   version, quantity/unit and criterion/citation contracts, completeness, and
   payload hash.
5. `public_release` will bind release version/ID/manifest, HF repository and
   immutable commit, exact-tree evidence, and remote semantic evidence.
6. `website_evidence` will bind the website commit/deployment, public route,
   rendered release ID, and online verification evidence.

Each producer will freeze and test its complete owned schema before
implementation. Each consumer will verify the shared envelope and exact
approved producer version before processing. Unknown versions, mismatched IDs
or hashes, absent evidence, and silent coercion will fail.

The v1 public release will retain thirteen required query surfaces:
`source_registry`, `transformations`, `citations`, `criteria`,
`clean_room_reviews`, `field_lineage`, `riser_components`,
`riser_configurations`, `analysis_cases`, `execution_attestations`,
`strength_results`, `operability_results`, and `withheld_sources`. #1604 will
own their exact schemas and joins. Fatigue may remain later #138 scope but will
not be required in the #1602 v1 public release.

## Promotion Gates

The program will advance only through these ordered gates:

1. **Source → normalized:** source class, provenance, legal/license status,
   units, and malformed-row handling will pass.
2. **Normalized → bundle:** the approved schema, complete field mapping,
   no-default rule, deterministic generation, and bundle hash will pass.
3. **Bundle → execution:** an authorized request, bounded transport, licensed
   host preflight, current real canary, and bundle/model binding will pass.
4. **Execution → result:** requested solver stages, semantic success, signed
   receipt, payload hash, result completeness, and engineering checks will pass.
5. **Result → public:** public-source/clean-room eligibility, case/execution
   joins, units, criteria/citations, decoded leak scan, and legal scan will pass.
6. **Public → website:** immutable HF commit, exact configs/rows/release ID, and
   documented limitations/withholding will pass.
7. **Program closeout:** every child approval, no-MAJOR artifact review, landed
   remote ancestry, tests, legal evidence, completeness gate, HF evidence, and
   website evidence will pass.

Failure at a gate will retain a structured failure or withholding reason and
will stop downstream promotion. Missing data will not be guessed to inflate
coverage.

## Deliverable

The parent issue will deliver:

- one reviewed parent interface contract;
- one reconciled dependency/ownership tree;
- independently approved and completed child implementations;
- provenance-safe normalized drilling and synthetic completion/workover cases;
- real licensed OrcaFlex strength and operability results for every eligible
  case requested by the approved child analysis matrix;
- one immutable, verified Hugging Face release;
- one verified public website capability route; and
- one executable closeout evidence report proving the entire chain.

## Pseudocode

```text
require owner-approved parent plan

for child in dependency_order:
    require child issue, reviewed plan, and owner approval
    implement child-owned exact contract with tests first
    require child tests, legal scan, adversarial artifact review, and landed SHA

for each inventoried source:
    classify source and provenance
    repair or reject malformed public source rows
    normalize only eligible public or clean-room synthetic inputs
    emit versioned normalized_case envelope
    generate deterministic_bundle envelope

for each eligible requested case:
    submit authorized bundle to licensed execution owner
    require real canary, semantic solver success, and signed receipt
    extract child-defined strength and operability results
    require completeness, criteria/citations, and independent engineering checks

project only release-approved cases and verified results
require decoded leak/legal scans and exact release joins
atomically publish and verify the immutable HF commit
register and verify the exact website release
recompute parent closeout evidence from authoritative live state
```

## Planning and Implementation Artifacts

| Scope | Planned artifact |
|---|---|
| Parent | this plan, parent interface YAML, plan index, review artifacts |
| WED #1046 | child plan will freeze loader/test files and CSV disposition |
| DM #1603 | child plan will freeze normalized schema, registry, adapter, fixtures, and tests |
| DM #811 | child plan will freeze drilling workflow code, inputs, outputs, and tests |
| DM #138 | child plan will freeze model/result/criteria/oracle code and tests |
| Deckhand #568 | child plan will freeze queue/transport/receipt/policy code and tests |
| DM #1604 | child plan will freeze release schemas, publisher, verifier, and tests |
| Website #75 | child plan will freeze capability/render/validation/deployment files and tests |

Exact implementation paths will be declared and reviewed in each child plan.
The parent will not guess paths or duplicate those plans.

## TDD Contract by Child

| Required RED test class | Owner | Required outcome |
|---|---|---|
| malformed row widths and semantic anchors | WED #1046 | repair is explicit or row is rejected |
| normalized identity, schema rejection, lineage, privacy, no defaults | DM #1603 | deterministic SSOT and bundle |
| drilling stackup and operability tolerances | DM #811 | approved workflow behavior |
| quantity/criterion/location cardinality and independent oracles | DM #138 | traceable engineering results |
| tamper/replay/license/canary/semantic/transport failures | Deckhand #568 | only verified real execution succeeds |
| release joins, decoded leaks, concurrency, exact tree, remote semantics | DM #1604 | immutable public-safe HF release |
| withheld rendering, online failure, build/deploy/route checks | Website #75 | truthful live capability |
| missing approval/review/ancestry/test/legal/HF/website evidence | DM #1604 | parent closeout fails closed |

Licensed tests will be isolated and explicitly tagged. Unit tests will not mock
licensed success into a public result. The real licensed acceptance path will
remain mandatory before publication.

## Execution Sequence

1. The parent plan will pass fresh adversarial review and receive explicit owner
   approval.
2. #1046 and #1603 will define the usable source and normalized-case boundary.
3. #811 and #138 will define the approved drilling/completion analysis and
   engineering-result contracts.
4. #568 will define and prove authenticated licensed execution and return.
5. Eligible cases will run through the real solver and engineering checks.
6. #1604 will build, scan, publish, and remotely verify the release.
7. Website #75 will register and deploy the verified immutable release.
8. #1604's closeout verifier will retrieve all evidence; the parent will close
   only after the completeness gate and owner-only verification label pass.

Parallel work may occur only where the dependency graph permits read-only
research or isolated child planning. No write-capable downstream child will
start before its upstream interface and its own approval gate are complete.

## Acceptance Criteria

- [ ] The parent plan will pass a fresh adversarial review with no MAJOR verdict.
- [ ] The owner will separately approve the reviewed parent plan.
- [ ] Every child will have a reviewed, owner-approved plan before implementation.
- [ ] The source inventory will distinguish canonical, duplicate, synthetic,
  private, licensed, and derived records without exposing private identifiers.
- [ ] Malformed source rows will be repaired with semantic proof or rejected.
- [ ] The solver-neutral normalized case will remain the SSOT; OrcaFlex files
  will remain evidence or derived artifacts.
- [ ] Raw completion/workover models and restricted metadata will never enter
  public artifacts.
- [ ] Every public completion/workover input will carry approved clean-room
  synthetic evidence defined and tested by #1603/#1604.
- [ ] Every published result will bind one public case, deterministic bundle,
  native model, verified licensed execution, result payload, criteria/citations,
  units, and completeness status through approved envelope versions.
- [ ] Public provenance will use approved public source hashes or a reviewed
  confidentiality-safe commitment; private or withheld source hashes will not
  be published.
- [ ] Every eligible riser family will include a baseline statics case, and any
  missing requested result will block promotion.
- [ ] Mock or reduced-order output will never be labeled as OrcaFlex output.
- [ ] Strength and operability outputs will satisfy #138's approved result matrix,
  plausibility tests, and independent engineering checks.
- [ ] Publication will reject confidential values, host paths, client/project
  identifiers, restricted text, unsigned results, stale files, and unknown
  producer versions.
- [ ] The HF repository will match the approved exact release at the returned
  immutable commit and pass remote config/split/feature/row/release-ID checks.
- [ ] The website will deploy the exact reviewed commit, return HTTP 200, render
  the expected release ID, and omit withheld names and values.
- [ ] Every child will pass TDD, legal scan, artifact review, issue summary,
  landed-ancestry verification, and completeness requirements.
- [ ] Parent closeout will be computed from authoritative live evidence and will
  fail if any required evidence is missing or inconsistent.

## Parent Plan Verification

```bash
python - <<'PY'
from pathlib import Path
import yaml

p = Path("docs/plans/issue-1602-riser-analysis-contract-v1.yaml")
c = yaml.safe_load(p.read_text())
assert c["contract_id"] == "digitalmodel-riser-analysis-parent-interface"
assert c["contract_version"] == "2.0.0-draft.1"
owners = c["owners"]
required_owners = {"source_repair", "normalized_ssot", "drilling_workflow",
                   "analysis_semantics", "licensed_execution",
                   "public_release", "website"}
assert set(owners) == required_owners
envelopes = c["interface_envelopes"]
required_envelopes = {"normalized_case", "deterministic_bundle",
    "execution_request_and_receipt", "engineering_result", "public_release",
    "website_evidence"}
assert set(envelopes) == required_envelopes
assert c["global_coordinate_contract"]["axes"] == "ENU_right_handed"
assert len(c["public_dataset_surfaces"]["required_configs"]) == 13
assert set(c["child_acceptance_obligations"]) == {
    "normalized_ssot", "drilling_workflow", "analysis_semantics",
    "licensed_execution", "public_release", "website"}
for name, envelope in envelopes.items():
    assert envelope["producer"] in owners
    assert envelope["minimum_bindings"]
    for consumer in envelope["consumers"]:
        assert consumer in owners or consumer == "parent_closeout"
all_outputs = [item for owner in owners.values() for item in owner["outputs"]]
assert len(all_outputs) == len(set(all_outputs))
assert c["planning_validation"]["no_child_implementation_authorized_by_parent_alone"]
print("issue-1602 parent interface validation: PASS")
PY
xmllint --html --noout \
  docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html
! rg -n 'TO''DO|TB''D|<re''po>|N''NN' \
  docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md
for f in scripts/review/results/issue-1602-round-6/*.md; do test -s "$f"; done
cd ../../workspace-hub
bash scripts/legal/legal-sanity-scan.sh \
  --repo=../agent-worktrees/dm-1602-design --diff-only
bash scripts/legal/legal-sanity-scan.sh \
  --repo=../agent-worktrees/dm-1602-design
```

## Adversarial Review Summary

Rounds 1–5 are retained as historical defect evidence. They returned MAJOR
because the parent repeatedly attempted to freeze production-level normalized,
solver, receipt, release, and closeout schemas without the executable child
fixtures and tests needed to prove them. The owner approved the recommended
contract-depth refactor on 2026-07-16.

This revision will preserve every cross-layer safety outcome from those reviews
while moving exact schema/receipt/validator design to its accountable child.
Round 6 will review the new boundary itself: ownership gaps, lost acceptance
criteria, incompatible envelope bindings, dependency cycles, and fail-open
promotion paths. Any MAJOR will keep the plan in draft.

| Review wave | Verdict | Disposition |
|---|---|---|
| Initial provider fanout | UNAVAILABLE | documented provider outages; fallback review used |
| Round 1 fallback | MAJOR | cross-layer safety requirements identified |
| Rounds 2–3 | MAJOR | identity, provenance, solver, release, and closeout defects identified |
| Rounds 4–5 | MAJOR | parent over-specification and non-executable nested contracts identified |
| Owner boundary decision | APPROVED | parent will own interfaces/gates; children will own exact executable contracts |
| Round 6 boundary review | PENDING | — |

**Overall result:** PENDING ROUND 6

## Risks and Open Questions

- **Child-plan drift:** a child may omit a parent envelope binding. The parent
  closeout verifier will require exact approved versions and shared hashes.
- **Legacy issue breadth:** #138 and #811 contain broader historical scope. Their
  child plans will isolate #1602-required deliverables and will not use private
  corpus material to seed public rows.
- **Licensed-host readiness:** current execution is disabled. #568 will prove a
  current real canary before production cases.
- **Sparse public completion data:** the first public completion/workover cases
  will be clean-room synthetic. Withheld legacy models will not count as public
  coverage.
- **Standards rights:** criteria will cite reviewed identifiers and applications
  without publishing licensed text.
- **HF eventual consistency:** #1604 will freeze bounded polling and head-race
  behavior in its executable release plan.
- **Scope reduction:** any proposed removal of a parent acceptance criterion will
  require an owner-authored issue comment naming the exact removed item.

## Complexity: T3

The program crosses source integrity, offshore engineering, licensed Windows
execution, cryptographic evidence, public data release, and website deployment.
The parent will coordinate those boundaries; each child will own and prove its
implementation details.
