"""Multi-component riser extension of the cited B401 family route."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.b401_anode_families import (
    FamilyDemand,
    design_anode_family,
    overall_family_status,
)
from digitalmodel.cathodic_protection.b401_component_allocations import (
    allocation_check,
    parse_allocations,
    parse_families,
    reconcile_allocations,
)
from digitalmodel.cathodic_protection.b401_component_connectivity import (
    components as _components,
    continuity as _continuity,
    continuity_records as _continuity_records,
)
from digitalmodel.cathodic_protection.b401_component_zones import zone_calculations
from digitalmodel.cathodic_protection.b401_tables import (
    edition_provenance,
)

CASES = ("mass", "initial", "final")


def _design_family(
    name: str,
    family: Any,
    host: str,
    loads: Mapping[str, Any],
    fractions: Mapping[str, float],
    components: Mapping[str, Mapping[str, Any]],
    component_results: dict[str, Any],
    graph: Mapping[str, Mapping[str, float]],
    edition: Edition,
) -> dict[str, Any]:
    demand = {
        case: sum(load[case] for load in loads.values())
        for case in ("initial", "mean", "final")
    }
    current_years = sum(load["current_years"] for load in loads.values())
    result = design_anode_family(
        family,
        FamilyDemand.model_validate(demand),
        1.0,
        edition,
        mean_current_years_A_year=current_years,
    )
    paths, continuity = _continuity_records(host, loads, components, graph)
    result.update(
        installed_on_component=host,
        protected_components=sorted(loads),
        continuity_paths=paths,
        continuity_checks=continuity,
    )
    host_row = component_results[host]
    host_row["hosted_anode_families"].append(name)
    host_row["hosted_anode_count"] += result["anode_count"]
    for target, fraction in fractions.items():
        component_results[target]["allocation_checks"][name] = allocation_check(
            result, loads[target], fraction
        )
    return result


def _design_families(
    families: Mapping[str, Any],
    hosts: Mapping[str, str],
    loads: Mapping[str, Any],
    allocations: Mapping[str, Mapping[str, float]],
    components: Mapping[str, Mapping[str, Any]],
    component_results: dict[str, Any],
    graph: Mapping[str, Mapping[str, float]],
    edition: Edition,
) -> tuple[dict[str, dict[str, Any]], list[str]]:
    results = {
        name: _design_family(
            name,
            family,
            hosts[name],
            loads[name],
            allocations[name],
            components,
            component_results,
            graph,
            edition,
        )
        for name, family in families.items()
    }
    citations = [
        citation for result in results.values() for citation in result["citations"]
    ]
    return results, citations


def _component_candidate(
    name: str, row: dict[str, Any]
) -> tuple[float, str, str, str] | None:
    checks = row["allocation_checks"]
    row["required_mass_kg"] = sum(
        check["required_mass_kg"] for check in checks.values()
    )
    row["required_anode_count"] = sum(
        check["required_anode_count"] for check in checks.values()
    )
    if row["coverage"] == "NOT_EVALUATED":
        row["status"] = {
            "result": "FAIL",
            "governing_family": None,
            "governing_case": "coverage:not_evaluated",
            "governing_ratio": None,
        }
        return None
    if not checks:
        passive = (
            "EXCLUDED"
            if "excluded_self_protected" in row["zone_dispositions"]
            else "NO_CP_DEMAND"
        )
        row["status"] = {
            "result": passive,
            "governing_family": None,
            "governing_case": None,
            "governing_ratio": 0.0,
        }
        return None
    family, worst = max(
        sorted(checks.items()), key=lambda item: item[1]["governing_ratio"]
    )
    passed = all(all(check["checks"].values()) for check in checks.values())
    row["status"] = {
        "result": "PASS" if passed else "FAIL",
        "governing_family": family,
        "governing_case": worst["governing_case"],
        "governing_ratio": worst["governing_ratio"],
    }
    return worst["governing_ratio"], name, family, worst["governing_case"]


def _status_components(
    components: dict[str, Any], families: Mapping[str, Mapping[str, Any]]
) -> tuple[list[tuple[float, str, str, str]], bool]:
    candidates = []
    for name, row in components.items():
        candidate = _component_candidate(name, row)
        if candidate is not None:
            candidates.append(candidate)
        row["hosted_anode_families"].sort()
    candidates.extend(
        (row["governing_ratio"], "", name, row["governing_case"])
        for name, row in families.items()
    )
    return candidates, all(
        row["status"]["result"] != "FAIL" for row in components.values()
    )


def _override_reconciliation_status(
    status: dict[str, Any], reconciliation_pass: bool, coverage_failure: bool
) -> None:
    if not reconciliation_pass and not coverage_failure:
        status.update(
            governing_component=None,
            governing_family=None,
            governing_case="allocation_reconciliation",
            governing_ratio=None,
            reason="allocated capacity does not reconcile to physical family capacity",
        )


def _overall_status(
    components: Mapping[str, Mapping[str, Any]],
    families: Mapping[str, Mapping[str, Any]],
    candidates: list[tuple[float, str, str, str]],
    coverage_failure: bool,
    reconciliation_pass: bool,
) -> tuple[dict[str, Any], dict[str, Any]]:
    family_status = overall_family_status(families)
    worst = max(
        candidates,
        key=lambda item: (
            item[0],
            bool(item[1]),
            item[1],
            item[2],
            -CASES.index(item[3]),
        ),
    )
    allocations_pass = all(
        row["status"]["result"] != "FAIL" for row in components.values()
    )
    passed = (
        allocations_pass
        and family_status["result"] == "PASS"
        and not coverage_failure
        and reconciliation_pass
    )
    status: dict[str, Any] = {
        "result": "PASS" if passed else "FAIL",
        "governing_component": None if coverage_failure else (worst[1] or None),
        "governing_family": None if coverage_failure else worst[2],
        "governing_case": "coverage:not_evaluated" if coverage_failure else worst[3],
        "governing_ratio": None if coverage_failure else worst[0],
        "reason": "one or more component zones are not evaluated"
        if coverage_failure
        else f"{worst[1] or 'family pool'}:{worst[2]}:{worst[3]} governs at adequacy ratio {worst[0]:.3f}",
        "checks": {
            name: row["status"]["result"] != "FAIL" for name, row in components.items()
        },
        "use_status": "client-use-with-eor-check",
    }
    status["checks"]["allocation_reconciliation"] = reconciliation_pass
    _override_reconciliation_status(status, reconciliation_pass, coverage_failure)
    return status, family_status


def _results_payload(
    edition: Edition,
    components: dict[str, Any],
    families: dict[str, dict[str, Any]],
    citations: list[str],
    reconciliation: dict[str, Any],
    status: dict[str, Any],
    family_status: dict[str, Any],
) -> dict[str, Any]:
    installed_count = sum(int(row["anode_count"]) for row in families.values())
    installed_mass = sum(
        int(row["anode_count"]) * float(row["individual_mass_kg"])
        for row in families.values()
    )
    demand = {
        case: sum(float(row["current_demand_A"][case]) for row in components.values())
        for case in ("initial", "mean", "final")
    }
    return {
        "mode": "b401_components",
        "standard": standard_for_edition(edition),
        "edition": edition,
        "provenance": edition_provenance(edition),
        "components": components,
        "current_demand_A": demand,
        "anode_families": families,
        "anode_requirements": {
            "total_required_mass_kg": sum(
                float(row["required_mass_kg"]) for row in families.values()
            ),
            "required_anode_count": sum(
                int(row["recommended_anode_count"]) for row in families.values()
            ),
            "installed_anode_count": installed_count,
            "installed_anode_mass_kg": installed_mass,
        },
        "current_output_verification": {"families": families, **family_status},
        "reconciliation": reconciliation,
        "citations": sorted(set(citations)),
        "status": status,
    }


def _component_inputs(
    inputs: Mapping[str, Any],
) -> tuple[Edition, dict[str, dict[str, Any]]]:
    if (
        inputs.get("structure")
        or inputs.get("environment")
        or inputs.get("design_data", {}).get("design_life")
    ):
        raise ValueError("mixed flat and component B401 inputs are not allowed")
    edition = normalize_edition(
        str(inputs.get("design_data", {}).get("edition", DEFAULT_EDITION))
    )
    return edition, _components(inputs)


def _reconciliation_passes(reconciliation: Mapping[str, Any]) -> bool:
    return all(
        all(checks.values())
        for name, checks in reconciliation.items()
        if name.endswith("_checks")
    )


def run_b401_components(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run component-local B401 demand with hosted, allocated anode families."""
    inputs = cfg["inputs"]
    edition, components = _component_inputs(inputs)
    component_results, loads, citations, coverage_failure = zone_calculations(
        components, edition
    )
    allocations = parse_allocations(inputs.get("allocations"), loads)
    families, hosts = parse_families(inputs, components, loads)
    graph = _continuity(inputs.get("electrical_continuity"), components)
    family_results, family_citations = _design_families(
        families,
        hosts,
        loads,
        allocations,
        components,
        component_results,
        graph,
        edition,
    )
    citations.extend(family_citations)
    candidates, _ = _status_components(component_results, family_results)
    reconciliation = reconcile_allocations(
        allocations, component_results, family_results
    )
    reconciliation_pass = _reconciliation_passes(reconciliation)
    status, family_status = _overall_status(
        component_results,
        family_results,
        candidates,
        coverage_failure,
        reconciliation_pass,
    )
    cfg["results"] = _results_payload(
        edition,
        component_results,
        family_results,
        citations,
        reconciliation,
        status,
        family_status,
    )
    return cfg


__all__ = ["run_b401_components"]
