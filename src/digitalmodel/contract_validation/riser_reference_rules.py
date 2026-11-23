# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Reference-resolution and independent effective-surface checks."""

from __future__ import annotations

from typing import Any

from .errors import ContractValidationError

EXPECTED_KEYS = {
    "contract_id",
    "contract_version",
    "status",
    "authority_issue",
    "approved_design",
    "authority",
    "source_of_truth",
    "solver_neutrality_contract",
    "owners",
    "global_coordinate_contract",
    "interface_envelopes",
    "identity_chain",
    "milestone_DAG",
    "compatibility",
    "public_dataset_surfaces",
    "promotion_gates",
    "global_invariants",
    "child_acceptance_obligations",
    "parent_acceptance_evidence",
    "planning_validation",
    "required_family_routing",
    "required_handoff_equalities",
    "envelope_commitment_rules",
    "closeout_observation_rules",
    "required_handoff_derivations",
    "downstream_chain_fields",
    "host_motion_route_union",
    "normalized_host_response_union",
    "analysis_lineage_fields",
    "host_motion_binding",
    "route_union_handoff_expansion",
    "invariants",
    "required_RED_fixtures",
    "analytical_validation_contract",
    "hydrodynamic_claim_truth_table",
    "diffraction_blockers",
    "child_issue_owner_crosswalk",
    "source_evidence_rules",
    "closeout_rules",
    "release_membership_contract",
    "normalized_route_handoff_expansion",
    "non_blocking_consumer_graph",
    "non_blocking_follow_on",
}
EXPECTED_CHILD_ISSUES = {
    "https://github.com/vamseeachanta/worldenergydata/issues/1046",
    "https://github.com/vamseeachanta/worldenergydata/issues/1050",
    "https://github.com/vamseeachanta/digitalmodel/issues/1618",
    "https://github.com/vamseeachanta/digitalmodel/issues/1619",
    "https://github.com/vamseeachanta/digitalmodel/issues/1603",
    "https://github.com/vamseeachanta/digitalmodel/issues/811",
    "https://github.com/vamseeachanta/digitalmodel/issues/138",
    "https://github.com/vamseeachanta/digitalmodel/issues/1609",
    "https://github.com/vamseeachanta/deckhand/issues/568",
    "https://github.com/vamseeachanta/digitalmodel/issues/1611",
    "https://github.com/vamseeachanta/digitalmodel/issues/1604",
    "https://github.com/vamseeachanta/aceengineer-website/issues/75",
}


def _edges(records: list[str]) -> list[tuple[str, str]]:
    result: list[tuple[str, str]] = []
    for record in records:
        source, target = record.split("->", maxsplit=1)
        result.append((source.strip(), target.strip()))
    return result


def _resolve(document: dict[str, Any], reference: str) -> Any:
    value: Any = document
    for part in reference.split("."):
        if not isinstance(value, dict) or part not in value:
            raise ContractValidationError(
                f"symbolic reference does not resolve: {reference}"
            )
        value = value[part]
    return value


def _validate_effective_declarations(
    root: dict[str, Any],
    base: dict[str, Any],
    host: dict[str, Any],
    composed: dict[str, Any],
) -> None:
    if set(composed) != EXPECTED_KEYS:
        raise ContractValidationError(
            "composed normative top-level surface is not exact"
        )
    if composed["authority"] != root["effective_authority"]:
        raise ContractValidationError("effective authority comparison failed")
    owner_decl = root["effective_owners"]
    expected_owners = set(base["owners"]) | set(owner_decl["add"])
    if set(composed["owners"]) != expected_owners:
        raise ContractValidationError("effective owner comparison failed")
    envelope_decl = root["effective_interface_envelopes"]
    expected_envelopes = set(base["interface_envelopes"]) | set(envelope_decl["add"])
    if set(composed["interface_envelopes"]) != expected_envelopes:
        raise ContractValidationError("effective envelope comparison failed")
    if composed["identity_chain"] != host["identity_chain"]:
        raise ContractValidationError("effective identity chain comparison failed")


def _validate_dag_guards(host: dict[str, Any], composed: dict[str, Any]) -> None:
    dag = composed["milestone_DAG"]
    nodes = {node for edge in _edges(dag["edges"]) for node in edge}
    predicates = set(dag["guard_predicates"].values())
    extension = host["milestone_DAG_extensions"]
    if not set(extension["edges"]) <= set(dag["edges"]):
        raise ContractValidationError("effective DAG extension comparison failed")
    guard_maps = (
        dag["deterministic_bundle_readiness_guards"],
        dag["normalized_case_readiness_guards"],
    )
    for guard_map in guard_maps:
        for references in guard_map.values():
            if not set(references) <= nodes:
                raise ContractValidationError("DAG guard milestone does not resolve")
    if len(predicates) != len(dag["guard_predicates"]):
        raise ContractValidationError("DAG guard predicate is ambiguous")


def _validate_symbolic_handoffs(host: dict[str, Any], composed: dict[str, Any]) -> None:
    for record in host["required_handoff_equalities"]:
        if record not in composed["required_handoff_equalities"]:
            raise ContractValidationError("host handoff constraint was lost")
        reference = record.get("fields_ref")
        if reference:
            selected = _resolve(composed, reference)
            for branch_ref in selected.values():
                fields = _resolve(composed, branch_ref)
                if not isinstance(fields, list):
                    raise ContractValidationError(
                        "fields_ref does not resolve to fields"
                    )
    normalized = composed["normalized_route_handoff_expansion"]
    for branch_ref in normalized["selected_branch_by_discriminator"].values():
        if not isinstance(_resolve(composed, branch_ref), list):
            raise ContractValidationError("normalized fields_ref is invalid")


def _validate_release_propagation(composed: dict[str, Any]) -> None:
    fields = [
        "logical_release_sha256",
        "floating_host_identity_set_sha256",
        "hydrodynamic_response_set_sha256",
        "hydrodynamic_response_values_set_sha256",
        "vessel_motion_boundary_set_sha256",
    ]
    routes = {
        ("logical_release", "HF_publication_receipt"),
        ("HF_publication_receipt", "website_evidence"),
        ("website_evidence", "program_closeout"),
        ("logical_release", "program_closeout"),
    }
    equalities = composed["required_handoff_equalities"]
    for source, target in routes:
        covered = set().union(
            *(
                set(item.get("fields", []))
                for item in equalities
                if item["from"] == source and item["to"] == target
            )
        )
        if not set(fields) <= covered:
            raise ContractValidationError(
                "HF/site/closeout commitment propagation lost"
            )


def _validate_monitoring_bindings(composed: dict[str, Any]) -> None:
    graph = composed["non_blocking_consumer_graph"]
    equalities = graph["required_baseline_equalities"]
    if len(equalities) != len(set(equalities)):
        raise ContractValidationError("monitoring baseline equalities are duplicated")
    hf_fields = set(
        composed["interface_envelopes"]["HF_publication_receipt"]["minimum_bindings"]
    )
    release_fields = [
        field for field in equalities if not field.startswith("operating_")
    ]
    if not set(release_fields) <= hf_fields:
        raise ContractValidationError("monitoring release baseline is not bound")
    sources = {
        source
        for source, target in _edges(graph["edges"])
        if target == "release_bound_monitoring_baseline"
    }
    if sources != {"immutable_HF_publication_receipt", "versioned_operating_envelope"}:
        raise ContractValidationError("monitoring baseline sources are not exact")


def validate_references(
    root: dict[str, Any],
    base: dict[str, Any],
    components: dict[str, dict[str, Any]],
    composed: dict[str, Any],
) -> None:
    """Validate independent declarations and all symbolic references."""

    host = components["host_motion_component"]
    assurance = components["assurance_component"]
    _validate_effective_declarations(root, base, host, composed)
    _validate_dag_guards(host, composed)
    _validate_symbolic_handoffs(host, composed)
    _validate_release_propagation(composed)
    _validate_monitoring_bindings(composed)
    if set(assurance["child_issue_owner_crosswalk"]) != EXPECTED_CHILD_ISSUES:
        raise ContractValidationError("child owner crosswalk issue set is not exact")
