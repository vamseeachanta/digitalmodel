"""Retrofit remaining-mass and combined-output assessment helpers."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import Edition
from digitalmodel.cathodic_protection.b401_structures_phases_schema import (
    basis,
    mapping,
    nonempty,
    number,
    rule_citations,
)


def retrofit_result(
    raw: Any, inputs: Mapping[str, Any], families: Mapping[str, Any], edition: Edition
) -> dict[str, Any] | None:
    if raw is None:
        return None
    item = mapping(raw, "retrofit")
    name = nonempty(item.get("family"), "retrofit.family")
    family = mapping(families.get(name), f"retrofit family {name}")
    source = _family_input(inputs, name)
    if family.get("count_source") != "input" or source.get("count") is None:
        raise ValueError("retrofit existing family requires an installed count input")
    remaining_count = _remaining_anode_count(item, source)
    installed_mass = remaining_count * float(family["individual_mass_kg"])
    consumed, measured, physical, usable, future = _balances(
        item, family, installed_mass
    )
    proposed = mapping(item.get("proposed_anode"), "retrofit.proposed_anode")
    utilization, mass_provenance = _validate_inputs(item, proposed, edition, installed_mass, measured)
    shortfall = max(future - max(usable, 0.0), 0.0)
    gross = shortfall / utilization
    existing_output, output_basis = _existing_output(
        item, family, remaining_count, usable, edition
    )
    counts = _counts(item, proposed, gross, existing_output)
    recommended = max(counts.values())
    checks = _combined_checks(item, proposed, existing_output, recommended)
    return {
        "family": name,
        "basis": item["remaining_mass_basis"],
        "remaining_mass_basis": item["remaining_mass_basis"],
        "remaining_mass_reason": item["remaining_mass_reason"],
        "remaining_mass_provenance": mass_provenance,
        "consumed_mass_kg": consumed,
        "physical_remaining_mass_kg": max(physical, 0.0),
        "usable_remaining_mass_kg": max(usable, 0.0),
        "raw_physical_balance_kg": physical,
        "raw_usable_balance_kg": usable,
        "future_usable_mass_required_kg": future,
        "mass_shortfall_kg": shortfall,
        "additional_gross_mass_kg": gross,
        **counts,
        "recommended_additional_count": recommended,
        "remaining_anode_count": remaining_count,
        "existing_output_basis": output_basis,
        "combined_output_checks": checks,
        "limitations": _limitations(item.get("condition")),
        "method_basis": "B401 retrofit evidence"
        if edition == "2021"
        else "project-practice remaining-mass method",
    }


def _family_input(inputs: Mapping[str, Any], name: str) -> Mapping[str, Any]:
    for raw in inputs.get("anode_families", []):
        if isinstance(raw, Mapping) and raw.get("name") == name:
            return raw
    raise ValueError(f"unknown retrofit anode family {name!r}")


def _remaining_anode_count(
    item: Mapping[str, Any], source: Mapping[str, Any]
) -> int:
    installed = int(source["count"])
    missing = number(item.get("missing_anode_count", 0), "retrofit missing count")
    if not missing.is_integer() or missing > installed:
        raise ValueError("retrofit missing count must be an integer within installed count")
    return installed - int(missing)


def _balances(
    item: Mapping[str, Any], family: Mapping[str, Any], installed: float
) -> tuple[float, Any, float, float, float]:
    capacity = float(family["capacity_Ah_kg"])
    consumed = kernel.mass_consumed(
        number(item.get("historical_mean_current_A"), "retrofit historical demand"),
        number(item.get("age_years"), "retrofit age"),
        capacity,
    )
    measured = item.get("measured_remaining_mass_kg")
    physical = installed - consumed if measured is None else number(
        measured, "measured remaining mass"
    )
    usable = physical - installed * (1.0 - float(family["utilization_factor"]))
    future = kernel.mass_consumed(
        number(item.get("future_mean_current_A"), "retrofit future demand"),
        number(item.get("future_life_years"), "retrofit future life", positive=True),
        capacity,
    )
    return consumed, measured, physical, usable, future


def _validate_inputs(
    item: Mapping[str, Any],
    proposed: Mapping[str, Any],
    edition: Edition,
    installed: float,
    measured: Any,
) -> tuple[float, dict[str, str]]:
    mass_provenance = _mass_provenance(item, measured)
    basis(proposed.get("output_basis"), "retrofit.proposed_anode.output_basis")
    utilization = number(
        proposed.get("utilization_factor"), "retrofit utilization_factor", positive=True
    )
    if utilization > 1.0:
        raise ValueError("retrofit utilization_factor must not exceed 1.0")
    if measured is not None and float(measured) > installed:
        raise ValueError("measured remaining mass cannot exceed present installed mass")
    condition = mapping(item.get("condition"), "retrofit.condition")
    if edition == "2021":
        for key in _CONDITION_FIELDS:
            nonempty(condition.get(key), f"retrofit.condition.{key}")
    return utilization, mass_provenance


def _mass_provenance(item: Mapping[str, Any], measured: Any) -> dict[str, str]:
    mass_basis = nonempty(
        item.get("remaining_mass_basis"), "retrofit.remaining_mass_basis"
    )
    source = nonempty(
        item.get("remaining_mass_source"), "retrofit.remaining_mass_source"
    )
    reason = nonempty(
        item.get("remaining_mass_reason"), "retrofit.remaining_mass_reason"
    )
    expected = "measured_remaining_mass" if measured is not None else "inferred_from_age_and_demand"
    expected_source = "measurement" if measured is not None else "project_practice"
    if mass_basis != expected:
        suffix = "requires measured_remaining_mass_kg" if measured is None else "requires measured_remaining_mass basis"
        raise ValueError(f"retrofit {mass_basis} {suffix}")
    if source != expected_source:
        raise ValueError(f"retrofit remaining_mass_source must be {expected_source}")
    return {"basis": mass_basis, "source": source, "reason": reason}


_CONDITION_FIELDS = (
    "potential_history",
    "steel",
    "coating",
    "calcareous_deposit",
    "marine_growth",
    "remaining_life",
)


def _existing_output(
    item: Mapping[str, Any],
    family: Mapping[str, Any],
    count: int,
    usable_mass: float,
    edition: Edition,
) -> tuple[float, dict[str, str]]:
    default_basis = {
        "source": "calculated_b401_result",
        "reason": "Depleted-family output is used for remaining in-service anodes.",
        "reference": f"anode_families.{family['name']}.current_output_final_A",
        "citation": next(
            value for value in rule_citations(edition) if "output adequacy" in value
        ),
    }
    if usable_mass <= 0.0:
        default_basis["reason"] = "No output is credited after usable mass is exhausted."
        return 0.0, default_basis
    supplied = item.get("existing_output_per_anode_A")
    if supplied is not None:
        supplied_basis = basis(
            item.get("existing_output_basis"), "retrofit.existing_output_basis"
        )
        per_anode = number(supplied, "retrofit existing output per anode")
        return count * per_anode, supplied_basis
    return count * float(family["current_output_final_A"]), default_basis


def _counts(
    item: Mapping[str, Any],
    proposed: Mapping[str, Any],
    gross: float,
    existing_output: float,
) -> dict[str, int]:
    mass = number(proposed.get("individual_mass_kg"), "retrofit anode mass", positive=True)
    mass_count = kernel.anode_count(gross, mass) if gross else 0
    values = {"count_by_mass": mass_count}
    for case in ("initial", "final"):
        demand = number(item.get(f"future_{case}_current_A"), f"future {case} demand")
        per_anode = number(
            proposed.get(f"current_output_{case}_A"),
            f"proposed {case} output",
            positive=True,
        )
        values[f"count_by_{case}_output"] = kernel.anodes_for_current(
            max(demand - existing_output, 0.0), per_anode
        )
    return values


def _combined_checks(
    item: Mapping[str, Any],
    proposed: Mapping[str, Any],
    existing: float,
    count: int,
) -> dict[str, Any]:
    checks: dict[str, Any] = {}
    for case in ("initial", "final"):
        demand = number(item.get(f"future_{case}_current_A"), f"future {case} demand")
        added = count * float(proposed[f"current_output_{case}_A"])
        total = existing + added
        checks[case] = {
            "demand_A": demand,
            "existing_output_A": existing,
            "proposed_output_A": added,
            "total_output_A": total,
            "result": "PASS" if demand <= total else "FAIL",
        }
    return checks


def _limitations(raw: Any) -> list[str]:
    condition = mapping(raw, "retrofit.condition")
    prefixes = ("unavailable", "not_observed", "assumed")
    return [key for key, value in condition.items() if str(value).startswith(prefixes)]


__all__ = ["retrofit_result"]
