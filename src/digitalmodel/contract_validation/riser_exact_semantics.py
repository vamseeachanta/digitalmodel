# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Frozen independent constants for the approved #1602 semantics."""

from __future__ import annotations

import hashlib
import json
from collections import Counter
from typing import Any

from .errors import ContractValidationError

DERIVATION_HASHES = [
    "7bf78ca22d8da927f65141c8a9bed41f610e6b7aa6c65c7896045e9ca93d4693",
    "17082c1a81c7c437175cc77e6c61fe8e67cab6e1b16db83d7ad862285b0c46bc",
    "bf39c560e48bb8750a9308f462f0f81a70b68bdbb20edb2899bd8cf7da13bfb0",
    "ce49207605a0bd84f551aa34d4fcd272ebbe0b285936da8a62b36e38f0780fdb",
    "7e960d7cfe97f7927302dd9c2a3a8a7849c38cc7b96f4154f24c6135f552bfe2",
    "5e6947f6573f4c8fb83c1fd32d2667e550a82cd95ccd8043413ee9928cfa2a3e",
    "97deae1eb0dc78f1c775939ddbc90402857118c13d884999587d3284e37a8034",
    "c910d841e345c303a485cb10971c2c339a93e072064951aeeeb4e71cc857257e",
]
RELEASE_RULES = [
    "every_result_reference_is_a_set_member",
    "every_set_member_is_referenced_by_a_released_result",
    "every_member_resolves_to_approved_producer_envelope",
    "long_form_rows_exactly_cover_hydrodynamic_response_set",
]
RELEASE_EXACT_SETS = [
    "floating_host_identity_set_sha256",
    "hydrodynamic_response_set_sha256",
    "vessel_motion_boundary_set_sha256",
]
RELEASE_MEMBERSHIP_HASH = (
    "2b3e4949ee7cb3386219f1a2abcb8f7f71f5cf689c043e6665054f2376c73f70"
)
HOST_REF_RECORD_HASHES = [
    "da7cc2404a104dca7e29cdb6a05bba0792b60015705f008999d91ee1f50f7717",
    "053adf2315c8d856e11d9087775f93c32273f80fb0f9fc799e059c6b07df0d3f",
    "a5e9ad02139097c01b06d5e5edea6ce13030db1aacb1cbc9b271f775038032ee",
    "8dad95848797338e3726333cb3e223ad48c5910c15f95bd75c5031262f3cdda8",
]
EXPECTED_REF_COUNTS = Counter(
    {
        ("required_ref", "route_union_handoff_expansion.required_branch_equalities"): 1,
        (
            "not_applicable_ref",
            "route_union_handoff_expansion.not_applicable_branch_equalities",
        ): 1,
        (
            "required_ref",
            "normalized_route_handoff_expansion.required_branch_equalities",
        ): 1,
        (
            "not_applicable_ref",
            "normalized_route_handoff_expansion.not_applicable_branch_equalities",
        ): 1,
        (
            "fields_ref",
            "route_union_handoff_expansion.selected_branch_by_discriminator",
        ): 4,
        (
            "fields_ref",
            "normalized_route_handoff_expansion.required_branch_equalities",
        ): 1,
        (
            "fields_ref",
            "normalized_route_handoff_expansion.selected_branch_by_discriminator",
        ): 1,
    }
)
BASELINE_EQUALITIES = [
    "logical_release_sha256",
    "HF_commit_sha",
    "HF_publication_receipt_sha256",
    "floating_host_identity_set_sha256",
    "hydrodynamic_response_set_sha256",
    "hydrodynamic_response_values_set_sha256",
    "vessel_motion_boundary_set_sha256",
    "operating_envelope_version",
    "operating_envelope_sha256",
]
FAMILY_CONSTRAINTS = {
    "drilling": "required",
    "completion": "required_or_not_applicable",
    "workover": "required_or_not_applicable",
}
GUARD_PREDICATES = {
    "completion_or_workover_not_applicable": "completion_workover_request_has_reviewed_disposition_sha256",
    "normalized_case_not_applicable": "normalized_case_has_reviewed_disposition_sha256",
}
CHILD_CROSSWALK = {
    "https://github.com/vamseeachanta/worldenergydata/issues/1046": "source_repair",
    "https://github.com/vamseeachanta/worldenergydata/issues/1050": "floating_host_source",
    "https://github.com/vamseeachanta/digitalmodel/issues/1618": "floating_host_identity",
    "https://github.com/vamseeachanta/digitalmodel/issues/1619": "hydrodynamic_response",
    "https://github.com/vamseeachanta/digitalmodel/issues/1603": "normalized_ssot",
    "https://github.com/vamseeachanta/digitalmodel/issues/811": "drilling_workflow",
    "https://github.com/vamseeachanta/digitalmodel/issues/138": "analysis_semantics",
    "https://github.com/vamseeachanta/digitalmodel/issues/1609": "normalized_ssot",
    "https://github.com/vamseeachanta/deckhand/issues/568": "licensed_execution",
    "https://github.com/vamseeachanta/digitalmodel/issues/1611": "analysis_semantics",
    "https://github.com/vamseeachanta/digitalmodel/issues/1604": "public_release",
    "https://github.com/vamseeachanta/aceengineer-website/issues/75": "website",
}
COVERS_BY_ENVELOPE = {
    "normalized_case": "every_minimum_binding_except_commitment_field",
    "engineering_result": "every_minimum_binding_except_commitment_field",
    "execution_receipt": "every_minimum_binding_except_commitment_field",
    "execution_request": "every_minimum_binding_except_excluded_self_referential_fields",
    "public_floater_source_record": "every_minimum_binding_except_commitment_field",
    "floating_host_identity": "every_minimum_binding_except_commitment_field",
    "hydrodynamic_response": "every_minimum_binding_except_commitment_field",
    "vessel_motion_boundary": "every_minimum_binding_except_commitment_field",
    "analysis_contract": "every_effective_minimum_binding_except_commitment_field",
    "drilling_analysis_request": "every_effective_minimum_binding_except_commitment_field",
    "completion_workover_analysis_request": "every_effective_minimum_binding_except_commitment_field",
    "deterministic_bundle": "every_effective_minimum_binding_except_commitment_field",
    "logical_release": "every_effective_minimum_binding_except_commitment_field_including_all_v3_set_commitments",
    "HF_publication_receipt": "every_effective_minimum_binding_except_commitment_field",
    "website_evidence": "every_effective_minimum_binding_except_commitment_field",
    "program_closeout": "every_effective_minimum_binding_except_commitment_field",
}
HOST_EXTENSION_HASH = "54dd8106b5d8dbd134889db80db5b84706f2ef08303268d6655d19ce96a82b14"
ASSURANCE_KEYS = {
    "analytical_validation_contract",
    "authority_issue",
    "child_acceptance_extensions",
    "child_issue_owner_crosswalk",
    "closeout_rules",
    "component_id",
    "contract_version",
    "diffraction_blockers",
    "hydrodynamic_claim_truth_table",
    "invariants",
    "non_blocking_consumer_graph",
    "non_blocking_follow_on",
    "normalized_route_handoff_expansion",
    "planning_validation",
    "promotion_gate_extensions",
    "public_dataset_surface_extensions",
    "release_membership_contract",
    "required_RED_fixtures",
    "required_handoff_derivations",
    "schema_version",
    "source_evidence_rules",
}
APPROVED_FINGERPRINTS = {
    "root": "afa39fb15eb81503692ae3b5b8b90a619f9eae94addbb7cd1cf929c1eea90178",
    "base": "cfb5c5d03b87c9de7a998323b8bd9a59dc4c3cc85b77189b3ab255f5c1ec83b4",
    "host": "ec91bff38f6f15f16e2e1646fe096e8269911dea7faaa158656e472edb5e4927",
    "assurance": "264ead67a60e820584ca882674d4296222357b9b021249e442ab1d1cb21bf176",
}
HOST_FIELDS = [
    "floating_host_id",
    "loading_condition_id",
    "hydrodynamic_response_id",
    "vessel_motion_boundary_id",
    "floating_host_identity_envelope_sha256",
    "hydrodynamic_response_envelope_sha256",
    "vessel_motion_boundary_envelope_sha256",
]
NORMALIZED_FIELDS = [
    "floating_host_id",
    "loading_condition_id",
    "floating_host_identity_envelope_sha256",
    "hydrodynamic_response_id",
    "hydrodynamic_response_envelope_sha256",
]


def _union_contract(required_fields: list[str]) -> dict[str, Any]:
    return {
        "discriminator": "host_motion_applicability",
        "allowed_values": ["required", "not_applicable"],
        "required_branch": {
            "required_fields": required_fields,
            "forbidden_fields": ["reviewed_disposition_sha256"],
        },
        "not_applicable_branch": {
            "required_fields": ["reviewed_disposition_sha256"],
            "forbidden_fields": required_fields,
        },
        "exactly_one_branch": True,
        "family_constraints": FAMILY_CONSTRAINTS,
    }


def _record_hash(record: dict[str, Any]) -> str:
    data = json.dumps(record, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(data).hexdigest()


def validate_approved_fingerprints(
    root: dict[str, Any],
    base: dict[str, Any],
    host: dict[str, Any],
    assurance: dict[str, Any],
) -> None:
    """Bind all approved semantic surfaces independently of manifest hashes."""

    observed = {
        "root": _record_hash(root),
        "base": _record_hash(base),
        "host": _record_hash(host),
        "assurance": _record_hash(assurance),
    }
    if observed != APPROVED_FINGERPRINTS:
        changed = sorted(
            key for key in observed if observed[key] != APPROVED_FINGERPRINTS[key]
        )
        raise ContractValidationError(
            f"approved semantic fingerprint mismatch: {changed}"
        )


def validate_assurance_surface(assurance: dict[str, Any]) -> None:
    """Reject undeclared assurance overwrite keys before composition checks."""

    if set(assurance) != ASSURANCE_KEYS:
        raise ContractValidationError("assurance surface keys are not exact")


def validate_host_extension_surface(host: dict[str, Any]) -> None:
    """Reject envelope-extension overwrite attempts after specific checks."""

    if _record_hash(host["existing_envelope_extensions"]) != HOST_EXTENSION_HASH:
        raise ContractValidationError("host envelope extensions are not exact")


def _resolve(document: dict[str, Any], reference: str) -> Any:
    value: Any = document
    for part in reference.split("."):
        if not isinstance(value, dict) or part not in value:
            raise ContractValidationError(
                f"nested reference does not resolve: {reference}"
            )
        value = value[part]
    return value


def _validate_derivations(composed: dict[str, Any]) -> None:
    observed = [_record_hash(item) for item in composed["required_handoff_derivations"]]
    if observed != DERIVATION_HASHES:
        raise ContractValidationError("required derivation record is not exact")
    membership = composed["release_membership_contract"]
    if _record_hash(membership) != RELEASE_MEMBERSHIP_HASH:
        raise ContractValidationError("release membership rules are not exact")


def _collect_refs(value: Any) -> Counter[tuple[str, str]]:
    found: Counter[tuple[str, str]] = Counter()
    if isinstance(value, dict):
        for key, item in value.items():
            if key.endswith("_ref") and isinstance(item, str):
                found[(key, item)] += 1
            found.update(_collect_refs(item))
    elif isinstance(value, list):
        for item in value:
            found.update(_collect_refs(item))
    return found


def _validate_nested_refs(composed: dict[str, Any]) -> None:
    host = composed["route_union_handoff_expansion"]
    expected_host = {
        "required_ref": "route_union_handoff_expansion.required_branch_equalities",
        "not_applicable_ref": "route_union_handoff_expansion.not_applicable_branch_equalities",
    }
    if host["selected_branch_by_discriminator"] != expected_host:
        raise ContractValidationError("host nested reference mapping is not exact")
    normalized = composed["normalized_route_handoff_expansion"]
    expected_normalized = {
        "required_ref": "normalized_route_handoff_expansion.required_branch_equalities",
        "not_applicable_ref": "normalized_route_handoff_expansion.not_applicable_branch_equalities",
    }
    if normalized["selected_branch_by_discriminator"] != expected_normalized:
        raise ContractValidationError(
            "normalized nested reference mapping is not exact"
        )
    for mapping in (expected_host, expected_normalized):
        for reference in mapping.values():
            if not isinstance(_resolve(composed, reference), list):
                raise ContractValidationError("nested reference is not a field list")
    expected_routes = {
        ("completion_workover_analysis_request", "deterministic_bundle"),
        ("deterministic_bundle", "execution_request"),
        ("execution_request", "execution_receipt"),
        ("execution_receipt", "engineering_result"),
    }
    records = [
        item for item in composed["required_handoff_equalities"] if "fields_ref" in item
    ]
    observed_routes = {(item["from"], item["to"]) for item in records}
    hashes = [_record_hash(item) for item in records]
    if observed_routes != expected_routes or hashes != HOST_REF_RECORD_HASHES:
        raise ContractValidationError("host nested fields_ref routes are not exact")
    if _collect_refs(composed) != EXPECTED_REF_COUNTS:
        raise ContractValidationError("reference-bearing surface is not exact")


def _validate_route_semantics(composed: dict[str, Any]) -> None:
    if composed["host_motion_route_union"] != _union_contract(HOST_FIELDS):
        raise ContractValidationError("host route union semantics are not exact")
    if composed["normalized_host_response_union"] != _union_contract(NORMALIZED_FIELDS):
        raise ContractValidationError("normalized route union semantics are not exact")
    drilling = composed["interface_envelopes"]["drilling_analysis_request"]
    if drilling.get("fixed_applicability") != "required":
        raise ContractValidationError("drilling route fixed semantics are not exact")
    for envelope in composed["interface_envelopes"].values():
        union = envelope.get("discriminated_union")
        if union and union not in (
            composed["host_motion_route_union"],
            composed["normalized_host_response_union"],
        ):
            raise ContractValidationError(
                "envelope route union semantics are not exact"
            )


def _validate_normalized_handoffs(composed: dict[str, Any]) -> None:
    normalized = composed["normalized_route_handoff_expansion"]
    expected_drilling = {
        "from": "normalized_case",
        "to": "drilling_analysis_request",
        "fixed_applicability": "required",
        "fields_ref": "normalized_route_handoff_expansion.required_branch_equalities",
    }
    expected_completion = {
        "from": "normalized_case",
        "to": "completion_workover_analysis_request",
        "fields_ref": "normalized_route_handoff_expansion.selected_branch_by_discriminator",
    }
    if normalized["drilling_handoff"] != expected_drilling:
        raise ContractValidationError("normalized drilling handoff is not exact")
    if normalized["completion_workover_handoff"] != expected_completion:
        raise ContractValidationError("normalized completion handoff is not exact")


def _validate_constants(composed: dict[str, Any]) -> None:
    if composed["milestone_DAG"]["guard_predicates"] != GUARD_PREDICATES:
        raise ContractValidationError("guard predicates are not exact")
    graph = composed["non_blocking_consumer_graph"]
    if graph["required_baseline_equalities"] != BASELINE_EQUALITIES:
        raise ContractValidationError("monitoring baseline equalities are not exact")
    if composed["child_issue_owner_crosswalk"] != CHILD_CROSSWALK:
        raise ContractValidationError("child owner crosswalk mapping is not exact")
    observed_covers = {
        name: rule["covers"]
        for name, rule in composed["envelope_commitment_rules"].items()
    }
    if observed_covers != COVERS_BY_ENVELOPE:
        raise ContractValidationError("commitment covers expression is not exact")


def validate_exact_semantics(composed: dict[str, Any]) -> None:
    """Reject plausible mutations of the approved semantic constants."""

    _validate_derivations(composed)
    _validate_nested_refs(composed)
    _validate_route_semantics(composed)
    _validate_normalized_handoffs(composed)
    _validate_constants(composed)
