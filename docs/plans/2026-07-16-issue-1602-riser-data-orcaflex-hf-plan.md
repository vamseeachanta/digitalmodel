# Plan for #1602: Riser Data, OrcaFlex Analysis, and Hugging Face Program

> **Status:** plan-review — Round 14 unanimous APPROVE; owner approval pending
> **Complexity:** T3
> **Date:** 2026-07-16
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1602
> **Client:** N/A
> **Lane:** lane:codex
> **Design:** `docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html`
> **Parent interface contract:** `docs/plans/issue-1602-riser-analysis-contract-v1.yaml`
> **Historical review evidence:** `scripts/review/results/issue-1602-round-{1,2,3,4,5,6,7,8,9,10,11,12,13}/`
> **No-MAJOR boundary review:** `scripts/review/results/issue-1602-round-14/`

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
| Normalized SSOT | DM #1603 | nested schemas, IDs, lineage, clean-room records, global coordinate adapter, bundle builder | solver-neutral SSOT; no silent drops/defaults; public/private boundary |
| Drilling workflow | DM #811 | drilling stackup/request matrix and normalized role mapping | approved drilling requests; no duplicate coordinate rotation |
| Engineering analysis | DM #138 | pre-run model/result contracts, completion request matrix, request authorization, post-run results/oracles | acyclic pre-run readiness and post-run licensed validation |
| Licensed execution | Deckhand #568 | request verification, canary/receipt schemas, signatures, transport, host policy | executor will not self-authorize; real solver receipt binds raw payload |
| Public release | DM #1604 | Parquet configs, joins, leak rules, manifest, HF transaction, closeout verifier implementation | public-safe rows only; exact immutable release; no mock or unsigned results |
| Website | Website #75 | capability schema, renderer changes, tests, deployment proof | verified HF release first; withheld values never render |
| Parent closeout | DM #1602 | invoke landed verifier after every #1602-scoped child milestone and website complete | only parent will decide program completion |

No child will redefine another child's output. A consumer may tighten validation
but will reject, not reinterpret, an unknown producer version.

## Cross-Layer Interface Contract

`docs/plans/issue-1602-riser-analysis-contract-v1.yaml` will be the sole
normative parent interface. It will define twelve envelope boundaries:

1. `normalized_case` will bind public/safe provenance, configuration and case
   identities, units, confidentiality, redistribution, and eligibility.
2. `analysis_contract` will bind #138's pre-run model, result, criteria, and
   oracle contracts before any licensed request.
3. Separate drilling and completion/workover analysis-request envelopes will
   bind the approved case matrices to the model contract.
4. `deterministic_bundle` will bind the case and analysis request to the model
   contract, adapter/generator, bundle, and file manifest.
5. `execution_request` will be authorized and signed by #138 using #568's
   reviewed request schema; Deckhand will only verify and execute it.
6. `execution_receipt` will be independently signed by Deckhand and will bind
   the immutable signed request, requester authorization, requested stages,
   native model, solver identity, semantic status, and pre-existing raw solver
   payload.
7. `engineering_result` will be produced after receipt verification and will
   bind #138's complete derived result plus oracle and licensed-validation
   evidence without entering the raw-payload/receipt preimage.
8. `logical_release` will bind eligibility, clean-room, authenticated joins,
   result/criteria/oracle contracts, decoded leak evidence, and legal/license
   evidence before publication.
9. `HF_publication_receipt` will bind that logical release to the exact HF
   repository commit, tree verification, and remote semantic verification.
10. `website_evidence` will bind the logical release, HF publication receipt,
    and exact HF commit to the deployed website commit, route, and rendered
    release ID.
11. #1604 will implement the typed closeout verifier, but `program_closeout`
    will be invoked and produced by #1602 only after every #1602-scoped child
    milestone is complete.
12. The closeout envelope will bind the reviewed parent interface and verifier
    versions to fresh HF and website retrieval evidence at invocation time.

The parent identity order will be public source/commitment → normalized
configuration → case → analysis request → bundle → authorized execution request
→ native model → raw solver payload → signed execution receipt → engineering
result → logical release → HF commit → publication receipt → website evidence.
Each producer will freeze and
test its complete owned schema before implementation. Each consumer will verify
the shared envelope and exact approved producer version before processing.
Unknown versions, mismatched IDs or hashes, absent evidence, and silent coercion
will fail.

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

for each eligible requested case:
    require the approved analysis_contract envelope
    generate the family-owned analysis request
    generate deterministic_bundle envelope bound to that request
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
| unauthorized requester plus tamper/replay/license/canary/semantic/transport failures | #138 and Deckhand #568 | requester and executor are independent; only verified real execution succeeds |
| release joins, decoded leaks, concurrency, exact tree, remote semantics | DM #1604 | immutable public-safe HF release |
| withheld rendering, online failure, build/deploy/route checks | Website #75 | truthful live capability |
| missing approval/review/ancestry/test/legal/HF/website evidence | #1604 implementation; #1602 invocation | parent closeout fails closed |

Licensed tests will be isolated and explicitly tagged. Unit tests will not mock
licensed success into a public result. The real licensed acceptance path will
remain mandatory before publication.

## Execution Sequence

1. The parent plan will pass fresh adversarial review and receive explicit owner
   approval.
2. #1046 and #1603 will define the usable source and normalized-case boundary.
3. #138 will reach pre-run `analysis_contract_ready`; #811 and #138 will then
   produce the drilling and completion/workover request matrices.
4. #1603 will generate bundles bound to those requests and #138's model
   contract; #138 will independently authorize each execution request.
5. #568 will verify the request, run the real licensed solver, and return a
   signed receipt over the raw result payload.
6. #138 will perform post-run licensed validation, completeness checks, and
   independent oracles before emitting the engineering result.
7. #1604 will freeze and verify the logical release, publish it, then emit and
   remotely verify the HF publication receipt.
8. Website #75 will register and deploy the verified immutable release.
9. #1604's landed closeout verifier will retrieve all evidence when invoked by
   #1602; the parent will close
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
- [ ] The execution requester/authorizer and licensed executor will be distinct
  principals; the raw solver payload will exist before the receipt that signs
  it, and engineering validation will occur after receipt verification.
- [ ] Strength and operability outputs will satisfy #138's approved result matrix,
  plausibility tests, and independent engineering checks.
- [ ] Publication will reject confidential values, host paths, client/project
  identifiers, restricted text, unsigned results, stale files, and unknown
  producer versions.
- [ ] The HF repository will match the approved exact release at the returned
  immutable commit and pass remote config/split/feature/row/release-ID checks.
- [ ] The dataset card will document all configs, row counts, units, nulls,
  licenses, safe public source evidence, limitations, and issue-linked
  withholding reasons.
- [ ] The website will deploy the exact reviewed commit, return HTTP 200, render
  the expected release ID, bind the logical release, exact HF commit, and HF
  publication receipt, and omit withheld names and values.
- [ ] Every #1602-scoped child milestone will pass TDD, legal scan, artifact
  review, issue summary, landed-ancestry verification, and completeness
  requirements; a broader child issue may remain open for explicitly later
  scope such as fatigue.
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
assert c["contract_version"] == "2.0.0-draft.9"
owners = c["owners"]
required_owners = {"source_repair", "normalized_ssot", "drilling_workflow",
                   "analysis_semantics", "licensed_execution", "public_release",
                   "website", "parent_closeout"}
assert set(owners) == required_owners
assert set(c["planning_validation"]["required_owners"]) == required_owners
envelopes = c["interface_envelopes"]
required_envelopes = {"normalized_case", "analysis_contract",
    "drilling_analysis_request", "completion_workover_analysis_request",
    "deterministic_bundle", "execution_request", "execution_receipt",
    "engineering_result", "logical_release", "HF_publication_receipt",
    "website_evidence", "program_closeout"}
assert set(envelopes) == required_envelopes
assert set(c["planning_validation"]["required_envelopes"]) == required_envelopes
assert c["global_coordinate_contract"]["axes"] == "ENU_right_handed"
assert set(c["public_dataset_surfaces"]["required_configs"]) == {
    "source_registry", "transformations", "citations", "criteria",
    "clean_room_reviews", "field_lineage", "riser_components",
    "riser_configurations", "analysis_cases", "execution_attestations",
    "strength_results", "operability_results", "withheld_sources"}
assert set(c["source_of_truth"]["forbidden_public"]) == {
    "raw_DAT", "raw_YML", "raw_YAML", "confidential_models",
    "licensed_text", "machine_paths", "user_identifiers",
    "client_identifiers", "project_identifiers", "secrets",
    "private_source_hashes", "mock_solver_results"}
assert set(c["child_acceptance_obligations"]) == {
    "source_repair", "normalized_ssot", "drilling_workflow", "analysis_semantics",
    "licensed_execution", "public_release", "website"}
for name, envelope in envelopes.items():
    assert envelope["producer"] in owners
    assert envelope["minimum_bindings"]
    consumers = envelope.get("consumers", [envelope["consumer"]]
                         if "consumer" in envelope else [])
    for consumer in consumers:
        assert consumer in owners or consumer == "parent_closeout"
assert envelopes["execution_request"]["producer"] != envelopes["execution_receipt"]["producer"]
assert set(envelopes["execution_request"]["minimum_bindings"]) == {
    "request_schema_version", "execution_request_id",
    "execution_request_sha256", "requester_principal_id",
    "requester_signature_sha256", "authorization_policy_version",
    "authorization_evidence_sha256", "case_id", "case_sha256",
    "analysis_request_id", "bundle_sha256", "model_contract_sha256",
    "requested_solver_stages_sha256"}
assert set(envelopes["execution_receipt"]["minimum_bindings"]) == {
    "receipt_schema_version", "execution_request_id",
    "execution_request_sha256", "requester_signature_sha256",
    "authorization_evidence_sha256", "requested_solver_stages_sha256",
    "case_id", "case_sha256", "analysis_request_id", "bundle_sha256",
    "model_contract_sha256", "native_model_sha256",
    "solver_identity_sha256", "semantic_status",
    "raw_solver_payload_sha256", "host_signer_principal_id",
    "execution_receipt_sha256"}
normalized = set(envelopes["normalized_case"]["minimum_bindings"])
assert {"configuration_id", "configuration_sha256",
        "public_source_sha256_or_safe_provenance_commitment_sha256"} <= normalized
assert "public_release" in envelopes["normalized_case"]["consumers"]
assert "public_release" in envelopes["analysis_contract"]["consumers"]
release = set(envelopes["logical_release"]["minimum_bindings"])
assert {"public_eligibility_evidence_sha256", "clean_room_evidence_set_sha256",
        "authenticated_join_evidence_sha256", "decoded_leak_evidence_sha256",
        "legal_license_evidence_sha256"} <= release
website = set(envelopes["website_evidence"]["minimum_bindings"])
assert {"logical_release_sha256", "HF_commit_sha",
        "HF_publication_receipt_sha256", "website_evidence_sha256"} <= website
def expand_handoffs(rules):
    expanded = set()
    for rule in rules:
        sources = [rule["from"]]
        assert all(s in envelopes for s in sources)
        assert rule["to"] in envelopes
        pairs = [(field, field) for field in rule.get("fields", [])]
        pairs += list(rule.get("field_map", {}).items())
        for source in sources:
            for source_field, target_field in pairs:
                assert source_field in envelopes[source]["minimum_bindings"]
                assert target_field in envelopes[rule["to"]]["minimum_bindings"]
                expanded.add((source, source_field, rule["to"], target_field))
    return expanded

family_routing = c["required_family_routing"]
assert family_routing == {
    "discriminator_field": "riser_family",
    "allowed_routes": {
        "drilling": "drilling_analysis_request",
        "completion": "completion_workover_analysis_request",
        "workover": "completion_workover_analysis_request"},
    "selection": "exactly_one_route_per_normalized_case_and_bundle",
    "handoffs": [
        {"from": "normalized_case", "to": "selected_request",
         "fields": ["riser_family", "case_id", "case_sha256"]},
        {"from": "selected_request", "to": "deterministic_bundle",
         "fields": ["riser_family", "analysis_request_id", "case_id",
                    "case_sha256", "model_contract_sha256"]}]}
request_envelopes = set(family_routing["allowed_routes"].values())
assert all(family_routing["allowed_routes"][family] in request_envelopes
           for family in ("drilling", "completion", "workover"))
case_to_request_fields = set(family_routing["handoffs"][0]["fields"])
request_to_bundle_fields = set(family_routing["handoffs"][1]["fields"])
assert case_to_request_fields <= set(envelopes["normalized_case"]["minimum_bindings"])
assert request_to_bundle_fields <= set(envelopes["deterministic_bundle"]["minimum_bindings"])
for request_envelope in request_envelopes:
    request_fields = set(envelopes[request_envelope]["minimum_bindings"])
    assert case_to_request_fields <= request_fields
    assert request_to_bundle_fields <= request_fields

expected_handoffs = {
    ("analysis_contract", "model_contract_sha256", target,
     "model_contract_sha256")
    for target in ("drilling_analysis_request",
                   "completion_workover_analysis_request",
                   "deterministic_bundle")
} | {
    ("deterministic_bundle", f, "execution_request", f)
    for f in ("case_id", "case_sha256", "analysis_request_id",
              "bundle_sha256", "model_contract_sha256")
} | {
    ("execution_request", f, "execution_receipt", f)
    for f in ("execution_request_id", "execution_request_sha256",
              "requester_signature_sha256", "authorization_evidence_sha256",
              "requested_solver_stages_sha256", "case_id", "case_sha256",
              "analysis_request_id", "bundle_sha256", "model_contract_sha256")
} | {
    ("execution_receipt", f, "engineering_result", f)
    for f in ("case_id", "case_sha256", "execution_receipt_sha256",
              "raw_solver_payload_sha256")
} | {
    ("analysis_contract", f, "engineering_result", f)
    for f in ("result_contract_version", "result_contract_sha256",
              "criteria_contract_sha256", "oracle_contract_sha256")
} | {
    ("engineering_result", f, "logical_release", f)
    for f in ("result_contract_version", "result_contract_sha256",
              "criteria_contract_sha256", "oracle_contract_sha256")
} | {
    ("logical_release", "logical_release_sha256", target,
     "logical_release_sha256")
    for target in ("HF_publication_receipt", "website_evidence",
                   "program_closeout")
} | {
    ("logical_release", "release_id", "website_evidence",
     "rendered_release_id"),
    ("HF_publication_receipt", "HF_commit_sha", "website_evidence",
     "HF_commit_sha"),
    ("HF_publication_receipt", "HF_publication_receipt_sha256",
     "website_evidence", "HF_publication_receipt_sha256"),
    ("HF_publication_receipt", "HF_publication_receipt_sha256",
     "program_closeout", "HF_publication_receipt_sha256"),
    ("website_evidence", "website_evidence_sha256", "program_closeout",
     "website_evidence_sha256"),
    ("HF_publication_receipt", "HF_repo_id", "program_closeout",
     "fresh_HF_observed_repo_id"),
    ("HF_publication_receipt", "HF_commit_sha", "program_closeout",
     "fresh_HF_observed_commit_sha"),
    ("HF_publication_receipt", "HF_commit_sha", "program_closeout",
     "fresh_HF_observed_head_sha"),
    ("HF_publication_receipt", "logical_release_sha256", "program_closeout",
     "fresh_HF_observed_logical_release_sha256"),
    ("HF_publication_receipt", "HF_publication_receipt_sha256",
     "program_closeout", "fresh_HF_observed_publication_receipt_sha256"),
    ("website_evidence", "deployment_id", "program_closeout",
     "fresh_website_observed_deployment_id"),
    ("website_evidence", "website_commit_sha", "program_closeout",
     "fresh_website_observed_commit_sha"),
    ("website_evidence", "public_route", "program_closeout",
     "fresh_website_observed_route"),
    ("website_evidence", "rendered_release_id", "program_closeout",
     "fresh_website_observed_rendered_release_id"),
    ("website_evidence", "logical_release_sha256", "program_closeout",
     "fresh_website_observed_logical_release_sha256"),
    ("website_evidence", "HF_commit_sha", "program_closeout",
     "fresh_website_observed_HF_commit_sha"),
    ("website_evidence", "HF_publication_receipt_sha256", "program_closeout",
     "fresh_website_observed_HF_publication_receipt_sha256"),
    ("website_evidence", "website_evidence_sha256", "program_closeout",
     "fresh_website_observed_website_evidence_sha256"),
}
assert expand_handoffs(c["required_handoff_equalities"]) == expected_handoffs
assert c["envelope_commitment_rules"] == {
    "normalized_case": {
        "commitment_field": "normalized_case_envelope_sha256",
        "covers": "every_minimum_binding_except_commitment_field",
        "canonical_preimage_owner": "normalized_ssot"},
    "engineering_result": {
        "commitment_field": "engineering_result_envelope_sha256",
        "covers": "every_minimum_binding_except_commitment_field",
        "canonical_preimage_owner": "analysis_semantics"},
    "execution_receipt": {
        "commitment_field": "execution_receipt_sha256",
        "covers": "every_minimum_binding_except_commitment_field",
        "authentication": "host_signature_covers_commitment",
        "canonical_preimage_owner": "licensed_execution"},
    "execution_request": {
        "commitment_field": "execution_request_sha256",
        "covers": "every_minimum_binding_except_excluded_self_referential_fields",
        "excluded_self_referential_fields": ["execution_request_sha256",
                                              "requester_signature_sha256"],
        "authentication": "requester_signature_covers_commitment",
        "canonical_preimage_owner": "licensed_execution",
        "signature_producer": "analysis_semantics"}}
assert c["closeout_observation_rules"] == {
    "fresh_HF_required_checks": ["head_equals_published_commit",
        "exact_tree_recomputed", "datasets_server_valid",
        "configs_splits_features_rows_and_release_ID_reverified"],
    "fresh_website_required_checks": [
        "deployed_commit_equals_reviewed_commit", "HTTP_status_200",
        "fetched_content_or_DOM_verified",
        "rendered_logical_release_HF_commit_and_publication_receipt_match"],
    "evidence_commitments_cover_every_observed_field_and_check_result": True}
expected_derivations = {
    (("normalized_case.normalized_case_envelope_sha256[*]",),
     "logical_release.normalized_case_set_sha256",
     "child_defined_canonical_exact_set_commitment"),
    (("engineering_result.engineering_result_envelope_sha256[*]",),
     "logical_release.engineering_result_set_sha256",
     "child_defined_canonical_exact_set_commitment"),
    (("program_closeout.fresh_HF_retrieved_at",
      "program_closeout.fresh_HF_observed_repo_id",
      "program_closeout.fresh_HF_observed_commit_sha",
      "program_closeout.fresh_HF_observed_head_sha",
      "program_closeout.fresh_HF_observed_logical_release_sha256",
      "program_closeout.fresh_HF_observed_publication_receipt_sha256",
      "program_closeout.fresh_HF_exact_tree_evidence_sha256",
      "program_closeout.fresh_HF_semantic_remote_evidence_sha256"),
     "program_closeout.fresh_HF_retrieval_evidence_sha256",
     "child_defined_fresh_authenticated_retrieval_commitment"),
    (("program_closeout.fresh_website_retrieved_at",
      "program_closeout.fresh_website_observed_deployment_id",
      "program_closeout.fresh_website_observed_commit_sha",
      "program_closeout.fresh_website_HTTP_status",
      "program_closeout.fresh_website_observed_route",
      "program_closeout.fresh_website_observed_rendered_release_id",
      "program_closeout.fresh_website_observed_logical_release_sha256",
      "program_closeout.fresh_website_observed_HF_commit_sha",
      "program_closeout.fresh_website_observed_HF_publication_receipt_sha256",
      "program_closeout.fresh_website_observed_website_evidence_sha256",
      "program_closeout.fresh_website_content_evidence_sha256"),
     "program_closeout.fresh_website_retrieval_evidence_sha256",
     "child_defined_fresh_authenticated_retrieval_commitment"),
}
actual_derivations = {
    (tuple(d["inputs"] if isinstance(d["inputs"], list) else [d["inputs"]]),
     d["output"], d["mode"]) for d in c["required_handoff_derivations"]
}
assert actual_derivations == expected_derivations
for inputs, output, _ in actual_derivations:
    for reference in (*inputs, output):
        envelope_name, field = reference.replace("[*]", "").split(".", 1)
        assert field in envelopes[envelope_name]["minimum_bindings"]
order = c["identity_chain"]["order"]
assert order == ["approved_public_source_or_safe_provenance_commitment",
    "normalized_configuration", "analysis_case", "analysis_request",
    "deterministic_bundle", "execution_request", "native_model",
    "raw_solver_payload", "execution_receipt", "engineering_result_payload",
    "logical_release", "HF_commit", "HF_publication_receipt",
    "website_evidence"]
assert order.index("deterministic_bundle") < order.index("execution_request") < order.index("native_model")
assert order.index("raw_solver_payload") < order.index("execution_receipt") < order.index("engineering_result_payload")
assert order.index("logical_release") < order.index("HF_commit") < order.index("HF_publication_receipt") < order.index("website_evidence")
closeout = set(envelopes["program_closeout"]["minimum_bindings"])
assert {"parent_interface_version", "parent_interface_sha256",
        "closeout_verifier_version", "closeout_verifier_commit",
        "closeout_invoked_at", "fresh_HF_retrieval_evidence_sha256",
        "fresh_website_retrieval_evidence_sha256"} <= closeout
assert c["planning_validation"]["require_identity_chain_nodes_have_owner_or_envelope_binding"]
assert c["planning_validation"]["require_security_evidence_in_release_and_closeout"]
assert c["planning_validation"]["require_full_child_and_release_legal_scans"]
assert all("full_applicable_repository_legal_scan" in obligations
           for obligations in c["child_acceptance_obligations"].values())
assert "full_release_tree_legal_scan" in c["child_acceptance_obligations"]["public_release"]
edges = [tuple(part.strip() for part in edge.split("->"))
         for edge in c["milestone_DAG"]["edges"]]
nodes = set(c["milestone_DAG"]["milestone_owners"])
assert all(a in nodes and b in nodes for a, b in edges)
assert set(c["milestone_DAG"]["milestone_owners"].values()) <= set(owners)
incoming = {node: 0 for node in nodes}
outgoing = {node: [] for node in nodes}
for a, b in edges:
    outgoing[a].append(b); incoming[b] += 1
queue = [node for node, degree in incoming.items() if degree == 0]
visited = []
while queue:
    node = queue.pop(); visited.append(node)
    for child in outgoing[node]:
        incoming[child] -= 1
        if incoming[child] == 0: queue.append(child)
assert len(visited) == len(nodes)
all_outputs = [item for owner in owners.values() for item in owner["outputs"]]
assert len(all_outputs) == len(set(all_outputs))
assert c["planning_validation"]["no_child_implementation_authorized_by_parent_alone"]
print("issue-1602 parent interface validation: PASS")
PY
xmllint --html --noout \
  docs/plans/2026-07-16-issue-1602-riser-hf-analysis-design.html
! rg -n 'TO''DO|TB''D|<re''po>|N''NN' \
  docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md
for f in scripts/review/results/issue-1602-round-14/*.md; do test -s "$f"; done
gh issue view 811 -R vamseeachanta/digitalmodel --json body --jq .body |
  rg -F '#1603 exclusively owns the global coordinate transform.'
gh issue view 568 -R vamseeachanta/deckhand --json body --jq .body |
  rg -F 'emit only the execution-receipt bindings required by #1602.'
gh issue view 1604 -R vamseeachanta/digitalmodel --json body --jq .body |
  rg -F 'digitalmodel #1602 owns the parent cross-layer interface.'
gh issue view 75 -R vamseeachanta/aceengineer-website --json body --jq .body |
  rg -F '`HF_publication_receipt_sha256`'
cd ../../workspace-hub
bash scripts/legal/legal-sanity-scan.sh \
  --repo=../agent-worktrees/dm-1602-design --diff-only
```

The plan-stage scan will be diff-scoped because this branch changes only plan
and review artifacts. Each implementation child and the final release gate will
run the full applicable repository/release-tree legal scan; any historical
finding will require a reviewed forensic allowlist or remediation before that
child can promote.

## Adversarial Review Summary

Rounds 1–5 are retained as historical defect evidence. They returned MAJOR
because the parent repeatedly attempted to freeze production-level normalized,
solver, receipt, release, and closeout schemas without the executable child
fixtures and tests needed to prove them. The owner approved the recommended
contract-depth refactor on 2026-07-16.

This revision will preserve every cross-layer safety outcome from those reviews
while moving exact schema/receipt/validator design to its accountable child.
Round 6 reviewed the new boundary and returned unanimous MAJOR on independent
request authority, the #138/#568 lifecycle split, missing family request
envelopes, identity ordering, release-security evidence bindings, and validator
depth. Round 7 found the remaining interface joins: #1604 consumer edges,
signed request propagation, result/criteria/oracle propagation, an acyclic
logical-release/publication split, website/closeout receipt hashes, surface-
specific row evidence, and #1602-scoped milestone closeout. Round 8 found that
the initial propagation list did not require equality for every repeated case,
request, bundle, receipt, result, and rendered-release identity. Round 9 found
that the family-source rule incorrectly implied both request families, aggregate
commitments did not cover complete promotion evidence, fresh retrieval hashes
were not derived from the release/site identities, and the declared full legal
scan included unrelated historical residue. Draft 5 will add exact-one family
discrimination, complete-envelope set commitments, fresh retrieval derivations,
exact security baselines, and a passing diff-scoped plan scan while retaining
full applicable scans at implementation/release gates. Round 10 found that
exact-one family routing began too late, complete-envelope coverage was not an
explicit invariant, and closeout did not freshly prove HF semantics or fetched
website state. Draft 6 will add exhaustive case-to-request routing, complete-
envelope commitment rules, live HF/site observation rules, and validator-
enforced full legal-scan obligations. Round 11 found two remaining narrow gaps:
the hard validator did not expand selected-request fields over every concrete
route, and the authenticated execution receipt did not normatively commit every
receipt binding. Draft 7 will close both while leaving exact receipt preimages
and verification with Deckhand. Round 12 approved data and release but found the
symmetric request-authorization gap: requester authentication did not
normatively cover every request binding. Draft 8 will require the requester
signature to authenticate a commitment over every non-self-referential request
binding while leaving the exact preimage, signature format, and verifier with
Deckhand. Round 13 approved data and solver but found that the validator did not
pin the security-critical request/receipt minimum-binding sets before checking
their commitments. Draft 9 will assert those exact parent-owned minimum sets;
exact child schemas, preimages, formats, and fixtures will remain deferred.
Round 14 re-reviewed only this interface boundary and returned unanimous
APPROVE. The plan will remain at `plan-review` until explicit owner approval.

| Review wave | Verdict | Disposition |
|---|---|---|
| Initial provider fanout | UNAVAILABLE | documented provider outages; fallback review used |
| Round 1 fallback | MAJOR | cross-layer safety requirements identified |
| Rounds 2–3 | MAJOR | identity, provenance, solver, release, and closeout defects identified |
| Rounds 4–5 | MAJOR | parent over-specification and non-executable nested contracts identified |
| Owner boundary decision | APPROVED | parent will own interfaces/gates; children will own exact executable contracts |
| Round 6 boundary review | MAJOR | trust direction, #138/#568 cycle, missing request/model/oracle/security bindings, stale live source-hash criterion, shallow validator |
| Round 7 boundary review | MAJOR | missing consumer edges and binding propagation; release/HF cycle; stale live ownership text; closeout freshness gaps |
| Round 8 boundary review | MAJOR | incomplete lifecycle equality matrix, missing result-set commitment and rendered-release-ID binding, non-exhaustive validator |
| Round 9 boundary review | MAJOR | conditional family ambiguity, partial envelope commitments, unbound fresh retrieval, stale review check, failing unrelated full scan |
| Round 10 boundary review | MAJOR | family routing starts after normalization; envelope commitment coverage unstated; fresh HF/site semantics incomplete; full-scan gate prose-only |
| Round 11 boundary review | MAJOR | selected-request route fields under-validated; authenticated receipt coverage unstated |
| Round 12 boundary review | MAJOR | data/release APPROVE; requester-authenticated request commitment coverage unstated |
| Round 13 boundary review | MAJOR | data/solver APPROVE; request/receipt security minimum sets not pinned by validator |
| Round 14 boundary review | APPROVE | unanimous data, solver, and release/security approval; no parent-boundary blockers |

**Overall result:** APPROVE — AWAITING EXPLICIT OWNER PLAN APPROVAL

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
