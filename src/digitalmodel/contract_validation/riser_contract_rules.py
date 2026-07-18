# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Effective-surface rules for the #1602 riser parent contract."""

from __future__ import annotations

from typing import Any

from .errors import ContractValidationError
from .riser_exact_semantics import validate_exact_semantics
from .riser_reference_rules import validate_references

EXPECTED_CONFIGS = [
    "source_registry",
    "transformations",
    "citations",
    "criteria",
    "clean_room_reviews",
    "field_lineage",
    "riser_components",
    "riser_configurations",
    "analysis_cases",
    "execution_attestations",
    "strength_results",
    "operability_results",
    "withheld_sources",
    "floating_hosts",
    "hydrodynamic_responses",
    "hydrodynamic_response_values",
    "vessel_motion_boundaries",
]
EXPECTED_LOAD_ORDER = [
    "solver_neutral_base_v2",
    "host_motion_component",
    "assurance_component",
]
EXPECTED_DIRECTIVES = {
    "owners": "merge_by_owner_key_and_append_strengthening_amendment",
    "interface_envelopes": "union_new_keys_only",
    "existing_envelope_extensions": "extend_effective_minimum_bindings_or_discriminated_union",
    "identity_chain": "replace_base_order_with_declared_v3_order",
    "envelope_commitment_rules": "union_new_keys_and_lift_all_coverage_to_effective_bindings",
    "required_handoff_equalities": "append_full_constraints_deduplicate_exact_records_and_conjoin_same_route_requirements",
    "required_handoff_derivations": "append_unique_by_output",
    "milestone_DAG_extensions": "append_unique_edges_and_guards",
    "promotion_gate_extensions": "conjoin_same_named_gate_requirements_without_loss",
    "public_dataset_surface_extensions": "union_configs_and_apply_projection_contract",
    "planning_validation": "deep_union_lists_and_require_equal_scalars",
    "closeout_rules": "append_strengthening_rules",
    "child_acceptance_obligations": "normalize_role_and_issue_keys_through_crosswalk_then_replace_explicit_removal_and_append",
    "non_blocking_consumer_graph": "isolated_union_excluded_from_closeout",
}


def _union(left: list[Any], right: list[Any]) -> list[Any]:
    result = list(left)
    for item in right:
        if item not in result:
            result.append(item)
    return result


def _validate_required_surface(
    root: dict[str, Any], composed: dict[str, Any], assurance: dict[str, Any]
) -> None:
    validation = assurance["planning_validation"]
    owners = composed["owners"]
    envelopes = composed["interface_envelopes"]
    for owner in validation["required_owners"]:
        if owner not in owners:
            raise ContractValidationError(f"missing required owner: {owner}")
    required_envelopes = (
        validation["required_added_envelopes"]
        + validation["required_downstream_envelopes"]
    )
    for envelope in required_envelopes:
        if envelope not in envelopes:
            raise ContractValidationError(f"missing required envelope: {envelope}")
    configs = root["effective_public_dataset_surfaces"]["exact_required_configs"]
    if configs != EXPECTED_CONFIGS:
        raise ContractValidationError("contract must declare exactly 17 public configs")
    if configs != validation["required_public_configs"]:
        raise ContractValidationError(
            "17 public configs disagree with assurance contract"
        )
    if configs != composed["public_dataset_surfaces"]["required_configs"]:
        raise ContractValidationError("effective config declaration mismatch")


def _validate_manifest_constants(root: dict[str, Any]) -> None:
    composition = root["composition"]
    if composition["load_order"] != EXPECTED_LOAD_ORDER:
        raise ContractValidationError("declared load order is invalid")
    if composition["merge_directives"] != EXPECTED_DIRECTIVES:
        raise ContractValidationError("declared merge directives are invalid")


def _validate_envelopes(composed: dict[str, Any]) -> None:
    owners = composed["owners"]
    envelopes = composed["interface_envelopes"]
    rules = composed["envelope_commitment_rules"]
    if set(rules) != set(envelopes):
        raise ContractValidationError("commitment coverage does not match envelopes")
    for name, envelope in envelopes.items():
        producer = envelope.get("producer")
        if producer not in owners:
            raise ContractValidationError(f"unresolved producer for envelope: {name}")
        consumers = envelope.get("consumers", [])
        if "consumer" in envelope:
            consumers = [envelope["consumer"]]
        for consumer in consumers:
            if consumer not in owners:
                raise ContractValidationError(
                    f"unresolved consumer for envelope: {name}"
                )
        commitment = rules[name].get("commitment_field")
        if commitment not in envelope["minimum_bindings"]:
            raise ContractValidationError(f"commitment field missing: {name}")
        exclusions = rules[name].get("excluded_self_referential_fields", [commitment])
        allowed = [commitment]
        if name == "execution_request":
            allowed.append("requester_signature_sha256")
        if sorted(exclusions) != sorted(allowed):
            raise ContractValidationError(
                f"commitment preimage exclusions invalid: {name}"
            )
        covers = rules[name].get("covers", "")
        if not covers.startswith("every_"):
            raise ContractValidationError(f"commitment preimage is incomplete: {name}")
        rules[name]["effective_preimage_fields"] = [
            field for field in envelope["minimum_bindings"] if field not in exclusions
        ]


def _parse_edges(edges: list[object]) -> list[tuple[str, str]]:
    result: list[tuple[str, str]] = []
    for edge in edges:
        if not isinstance(edge, str) or edge.count("->") != 1:
            raise ContractValidationError(f"invalid DAG edge: {edge}")
        source, target = (part.strip() for part in edge.split("->"))
        if not source or not target:
            raise ContractValidationError(f"invalid DAG edge: {edge}")
        result.append((source, target))
    return result


def _validate_dag(composed: dict[str, Any], assurance: dict[str, Any]) -> None:
    dag = composed["milestone_DAG"]
    edges = _parse_edges(dag["edges"])
    nodes = {node for edge in edges for node in edge}
    missing = nodes - set(dag["milestone_owners"])
    if missing:
        raise ContractValidationError(f"DAG nodes have no owner: {sorted(missing)}")
    monitoring_edges = _parse_edges(assurance["non_blocking_consumer_graph"]["edges"])
    monitoring_nodes = {node for edge in monitoring_edges for node in edge}
    if nodes & monitoring_nodes:
        raise ContractValidationError("monitoring node appears in blocking DAG")
    outgoing: dict[str, list[str]] = {node: [] for node in nodes}
    indegree: dict[str, int] = {node: 0 for node in nodes}
    for source, target in edges:
        outgoing[source].append(target)
        indegree[target] += 1
    queue = sorted(node for node, count in indegree.items() if count == 0)
    visited = 0
    while queue:
        node = queue.pop(0)
        visited += 1
        for target in outgoing[node]:
            indegree[target] -= 1
            if indegree[target] == 0:
                queue.append(target)
    if visited != len(nodes):
        raise ContractValidationError("blocking DAG contains a cycle")
    reachable = {"parent_closeout_complete"}
    while True:
        additions = {source for source, target in edges if target in reachable}
        if additions <= reachable:
            break
        reachable.update(additions)
    if reachable != nodes:
        raise ContractValidationError("not every blocking DAG node reaches closeout")


def _validate_routes(
    base: dict[str, Any], host: dict[str, Any], assurance: dict[str, Any]
) -> None:
    expected_routes = {
        "drilling": "drilling_analysis_request",
        "completion": "completion_workover_analysis_request",
        "workover": "completion_workover_analysis_request",
    }
    if base["required_family_routing"]["allowed_routes"] != expected_routes:
        raise ContractValidationError("invalid family route declaration")
    for name, extension in host["existing_envelope_extensions"].items():
        union = (
            extension.get("discriminated_union")
            if isinstance(extension, dict)
            else None
        )
        if union is not None:
            _validate_route_union(name, union)
    chain = host["downstream_chain_fields"]
    expected_chain = [
        "floating_host_id",
        "loading_condition_id",
        "hydrodynamic_response_id",
        "vessel_motion_boundary_id",
        "floating_host_identity_envelope_sha256",
        "hydrodynamic_response_envelope_sha256",
        "vessel_motion_boundary_envelope_sha256",
    ]
    count = assurance["planning_validation"]["downstream_chain_field_count"]
    if chain != expected_chain or len(chain) != count:
        raise ContractValidationError("host-motion chain fields are incomplete")
    required = host["route_union_handoff_expansion"]["required_branch_equalities"]
    if required != ["host_motion_applicability", *expected_chain]:
        raise ContractValidationError("required route drops host-motion chain fields")


def _validate_route_union(name: str, union: dict[str, Any]) -> None:
    if union.get("allowed_values") != ["required", "not_applicable"]:
        raise ContractValidationError(f"route union values invalid: {name}")
    if union.get("exactly_one_branch") is not True:
        raise ContractValidationError(f"route union is not exactly-one: {name}")
    required = union["required_branch"]
    absent = union["not_applicable_branch"]
    if set(required["required_fields"]) != set(absent["forbidden_fields"]):
        raise ContractValidationError(f"route union forbidden fields invalid: {name}")
    if required["forbidden_fields"] != absent["required_fields"]:
        raise ContractValidationError(f"route union disposition invalid: {name}")


def _validate_handoffs(composed: dict[str, Any], host: dict[str, Any]) -> None:
    envelopes = composed["interface_envelopes"]
    route = host["route_union_handoff_expansion"]
    valid_ref = "route_union_handoff_expansion.selected_branch_by_discriminator"
    for handoff in composed["required_handoff_equalities"]:
        reference = handoff.get("fields_ref")
        if reference and reference != valid_ref:
            raise ContractValidationError(f"invalid handoff fields_ref: {reference}")
        fields = handoff.get("fields", [])
        if reference:
            fields = _union(
                route["required_branch_equalities"],
                route["not_applicable_branch_equalities"],
            )
        source, target = handoff["from"], handoff["to"]
        if source not in envelopes or target not in envelopes:
            raise ContractValidationError(
                f"unresolved handoff endpoint: {source} -> {target}"
            )
        for field in fields:
            if field not in envelopes[source]["minimum_bindings"]:
                raise ContractValidationError(f"handoff source drops field: {field}")
            if field not in envelopes[target]["minimum_bindings"]:
                raise ContractValidationError(f"handoff target drops field: {field}")
    selected = route["selected_branch_by_discriminator"]
    expected = {
        "route_union_handoff_expansion.required_branch_equalities",
        "route_union_handoff_expansion.not_applicable_branch_equalities",
    }
    if set(selected.values()) != expected:
        raise ContractValidationError("invalid route-union handoff expansion")


def _validate_composition(
    root: dict[str, Any], base: dict[str, Any], host: dict[str, Any]
) -> None:
    owners = root["effective_owners"]
    if set(host["owners"]) != set(owners["add"] + owners["amend"]):
        raise ContractValidationError("owner composition differs from declaration")
    envelopes = root["effective_interface_envelopes"]
    if set(host["interface_envelopes"]) != set(envelopes["add"]):
        raise ContractValidationError("envelope composition differs from declaration")
    if set(host["existing_envelope_extensions"]) != set(envelopes["extend"]):
        raise ContractValidationError("envelope extensions differ from declaration")
    allowed = "child_acceptance_obligations.public_release.exact_13_config_schemas"
    if root["composition"]["explicit_removals"] != [allowed]:
        raise ContractValidationError("undeclared removal in composition")
    obligations = base["child_acceptance_obligations"]["public_release"]
    if "exact_13_config_schemas" not in obligations:
        raise ContractValidationError("declared removal does not exist in base")


def _validate_config_derivation(
    root: dict[str, Any], base: dict[str, Any], assurance: dict[str, Any]
) -> None:
    base_configs = base["public_dataset_surfaces"]["required_configs"]
    additions = assurance["public_dataset_surface_extensions"]["add_configs"]
    derived = _union(base_configs, additions)
    effective = root["effective_public_dataset_surfaces"]
    if derived != effective["exact_required_configs"]:
        raise ContractValidationError("public config derivation mismatch")
    if len(derived) != effective["exact_config_count"]:
        raise ContractValidationError("public config count mismatch")


def _validate_promotion_gates(
    base: dict[str, Any], assurance: dict[str, Any], composed: dict[str, Any]
) -> None:
    gates = composed["promotion_gates"]
    for name, requirements in base["promotion_gates"].items():
        if not set(requirements) <= set(gates.get(name, [])):
            raise ContractValidationError(f"base promotion gate weakened: {name}")
    for name, requirements in assurance["promotion_gate_extensions"].items():
        if not set(requirements) <= set(gates.get(name, [])):
            raise ContractValidationError(f"promotion gate extension lost: {name}")


def _validate_children(assurance: dict[str, Any], composed: dict[str, Any]) -> None:
    crosswalk = assurance["child_issue_owner_crosswalk"]
    owners = composed["owners"]
    obligations = composed["child_acceptance_obligations"]
    if not set(crosswalk.values()) <= set(owners):
        raise ContractValidationError("child owner crosswalk does not resolve")
    for issue, additions in assurance["child_acceptance_extensions"].items():
        if issue not in crosswalk:
            raise ContractValidationError("child obligation has no owner crosswalk")
        if not set(additions) <= set(obligations[crosswalk[issue]]):
            raise ContractValidationError("child obligation was lost")


def _validate_derivations(
    base: dict[str, Any], assurance: dict[str, Any], composed: dict[str, Any]
) -> None:
    derivations = composed["required_handoff_derivations"]
    outputs = [item["output"] for item in derivations]
    expected = [item["output"] for item in base["required_handoff_derivations"]]
    expected += [item["output"] for item in assurance["required_handoff_derivations"]]
    if len(outputs) != len(set(outputs)) or outputs != expected:
        raise ContractValidationError("handoff derivation outputs are not exact")
    set_fields = assurance["release_membership_contract"]["exact_sets"]
    derived_fields = {item.split(".")[-1] for item in outputs}
    if not set(set_fields) <= derived_fields:
        raise ContractValidationError("release membership derivation is incomplete")


def _validate_monitoring(assurance: dict[str, Any], composed: dict[str, Any]) -> None:
    graph = assurance["non_blocking_consumer_graph"]
    edges = _parse_edges(graph["edges"])
    nodes = {node for edge in edges for node in edge}
    indegree: dict[str, int] = {node: 0 for node in nodes}
    outgoing: dict[str, list[str]] = {node: [] for node in nodes}
    for source, target in edges:
        outgoing[source].append(target)
        indegree[target] += 1
    queue = [node for node, count in indegree.items() if count == 0]
    seen = 0
    while queue:
        node = queue.pop()
        seen += 1
        for target in outgoing[node]:
            indegree[target] -= 1
            if indegree[target] == 0:
                queue.append(target)
    if seen != len(nodes):
        raise ContractValidationError("monitoring graph contains cycle")
    if not graph.get("consumer_owner") or not graph.get(
        "excluded_from_parent_closeout_reachability"
    ):
        raise ContractValidationError("monitoring graph is not owned and excluded")
    baseline = "release_bound_monitoring_baseline"
    if sum(target == baseline for _, target in edges) != 2:
        raise ContractValidationError("monitoring baseline prerequisites are not bound")
    if set(nodes) & {
        node
        for edge in _parse_edges(composed["milestone_DAG"]["edges"])
        for node in edge
    }:
        raise ContractValidationError("monitoring graph overlaps blocking DAG")


def validate_contract(
    root: dict[str, Any],
    base: dict[str, Any],
    components: dict[str, dict[str, Any]],
    composed: dict[str, Any],
) -> None:
    """Validate every effective Task 1 parent-contract surface."""

    host = components["host_motion_component"]
    assurance = components["assurance_component"]
    _validate_manifest_constants(root)
    _validate_required_surface(root, composed, assurance)
    _validate_composition(root, base, host)
    _validate_config_derivation(root, base, assurance)
    _validate_envelopes(composed)
    _validate_dag(composed, assurance)
    _validate_routes(base, host, assurance)
    _validate_handoffs(composed, host)
    _validate_promotion_gates(base, assurance, composed)
    _validate_children(assurance, composed)
    _validate_derivations(base, assurance, composed)
    _validate_monitoring(assurance, composed)
    validate_references(root, base, components, composed)
    validate_exact_semantics(composed)
