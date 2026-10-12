"""B401 riser-base compositions and phased anode-mass assessments."""

from __future__ import annotations

import copy
import math
from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import DEFAULT_EDITION, normalize_edition
from digitalmodel.cathodic_protection.b401_family_route import run_b401_families
from digitalmodel.cathodic_protection.b401_structures_phases_retrofit import (
    retrofit_result,
)
from digitalmodel.cathodic_protection.b401_structures_phases_schema import (
    PHASE_TYPES,
    basis,
    flatten_components,
    mapping,
    nonempty,
    number,
    project_basis,
    rule_citations,
)


def _component_results(
    summaries: dict[str, Any], results: Mapping[str, Any]
) -> dict[str, Any]:
    demand = mapping(results.get("current_demand_A"), "current_demand_A")
    areas = mapping(results.get("surface_areas_m2"), "surface_areas_m2")
    for name, summary in summaries.items():
        rows = [row for zone, row in demand.items() if zone.startswith(f"{name}::")]
        zone_names = [zone for zone in areas if zone.startswith(f"{name}::")]
        summary["zones"] = zone_names
        summary["area_m2"] = sum(float(areas[zone]) for zone in zone_names)
        summary["current_demand_A"] = {
            case: sum(float(row[f"I_{case}_A"]) for row in rows)
            for case in ("initial", "mean", "final")
        }
    return summaries


def _family_input(inputs: Mapping[str, Any], name: str) -> Mapping[str, Any]:
    for raw in inputs.get("anode_families", []):
        if isinstance(raw, Mapping) and raw.get("name") == name:
            return raw
    raise ValueError(f"unknown phase anode family {name!r}")


def _phase_family_row(
    raw: Mapping[str, Any], family: Mapping[str, Any], state: dict[str, float]
) -> dict[str, Any]:
    name = str(family["name"])
    mean = number(raw.get("mean_current_A"), f"phase family {name}.mean_current_A")
    life = state["life_years"]
    consumed = kernel.mass_consumed(mean, life, float(family["capacity_Ah_kg"]))
    start_physical = state["physical"]
    start_usable = state["usable"]
    raw_physical = start_physical - consumed
    raw_usable = start_usable - consumed
    available = max(start_usable, 0.0)
    shortfall = max(consumed - available, 0.0)
    individual = float(family["individual_mass_kg"])
    gross = shortfall / float(family["utilization_factor"])
    count = kernel.anode_count(gross, individual) if gross else 0
    state.update(physical=raw_physical, usable=raw_usable)
    return {
        "mean_current_A": mean,
        "consumed_mass_kg": consumed,
        "physical_mass_start_kg": max(start_physical, 0.0),
        "physical_mass_end_kg": max(raw_physical, 0.0),
        "usable_mass_start_kg": max(start_usable, 0.0),
        "usable_mass_end_kg": max(raw_usable, 0.0),
        "raw_physical_balance_kg": raw_physical,
        "raw_usable_balance_kg": raw_usable,
        "over_consumption_kg": max(-raw_physical, -raw_usable, 0.0),
        "mass_shortfall_kg": shortfall,
        "additional_gross_mass_kg": gross,
        "additional_anode_count": count,
    }


def _output_checks(raw: Mapping[str, Any], family: Mapping[str, Any]) -> dict[str, Any]:
    checks: dict[str, Any] = {}
    for case in ("initial", "final"):
        demand = number(raw.get(f"{case}_current_A", 0.0), f"{case}_current_A")
        output = float(family[f"total_output_{case}_A"])
        ratio = demand / output if output else math.inf
        checks[f"{case}_output"] = {
            "demand_A": demand,
            "output_A": output,
            "ratio": ratio,
            "result": "PASS" if demand <= output else "FAIL",
        }
    return checks


def _phase_results(
    assessment: Mapping[str, Any],
    inputs: Mapping[str, Any],
    families: Mapping[str, Any],
) -> list[dict[str, Any]]:
    states: dict[str, dict[str, float]] = {}
    rows: list[dict[str, Any]] = []
    for raw_phase in assessment.get("phases", []):
        row = _one_phase(raw_phase, inputs, families, states)
        if any(existing["name"] == row["name"] for existing in rows):
            raise ValueError(f"duplicate phase {row['name']!r}")
        rows.append(row)
    return rows


def _one_phase(
    raw: Any,
    inputs: Mapping[str, Any],
    families: Mapping[str, Any],
    states: dict[str, dict[str, float]],
) -> dict[str, Any]:
    phase = mapping(raw, "phase")
    name = nonempty(phase.get("name"), "phase.name")
    kind = nonempty(phase.get("type"), f"phase {name}.type")
    if kind not in PHASE_TYPES:
        raise ValueError(f"invalid phase {name!r} type {kind!r}")
    life = number(phase.get("life_years"), f"phase {name}.life_years", positive=True)
    raw_families = phase.get("families")
    if not isinstance(raw_families, list) or not raw_families:
        raise ValueError(f"phase {name!r} families must be non-empty")
    family_rows: dict[str, Any] = {}
    for raw_family in raw_families:
        item = mapping(raw_family, f"phase {name}.family")
        family_name = nonempty(item.get("family"), f"phase {name}.family")
        if family_name in family_rows:
            raise ValueError(f"duplicate family {family_name!r} in phase {name!r}")
        family = mapping(families.get(family_name), f"family {family_name}")
        source = _family_input(inputs, family_name)
        if family.get("count_source") != "input" or source.get("count") is None:
            raise ValueError(
                f"phase family {family_name!r} requires an installed count input"
            )
        installed = float(source["count"]) * float(family["individual_mass_kg"])
        state = states.setdefault(
            family_name,
            {
                "physical": installed,
                "usable": installed * float(family["utilization_factor"]),
            },
        )
        state["life_years"] = life
        row = _phase_family_row(item, family, state)
        row["basis"] = basis(item.get("basis"), f"phase {name}.{family_name}.basis")
        row["output_checks"] = _output_checks(item, family)
        family_rows[family_name] = row
    return {
        "name": name,
        "type": kind,
        "life_years": life,
        "basis": project_basis(phase.get("basis"), f"phase {name}.basis"),
        "families": family_rows,
    }


def _find_failure(
    phases: list[dict[str, Any]], record: Mapping[str, Any]
) -> Mapping[str, Any]:
    for phase in phases:
        if phase["name"] != record.get("phase"):
            continue
        family = phase["families"].get(record.get("family"), {})
        check = family.get("output_checks", {}).get(record.get("criterion"))
        if check and check["result"] == "FAIL":
            return mapping(check, "calculated output failure")
    raise ValueError(
        "accepted output shortfall must reference an actual calculated failure"
    )


def _accepted_shortfalls(
    raw: Any, phases: list[dict[str, Any]]
) -> list[dict[str, Any]]:
    records: list[dict[str, Any]] = []
    for raw_record in raw or []:
        item = mapping(raw_record, "accepted_output_shortfall")
        reason = nonempty(
            item.get("owner_reason"), "accepted_output_shortfall.owner_reason"
        )
        project_basis(item.get("basis"), "accepted_output_shortfall.basis")
        check = _find_failure(phases, item)
        for key in ("demand_A", "output_A", "ratio"):
            echoed = number(item.get(key), f"accepted output shortfall {key}")
            if not math.isclose(echoed, float(check[key]), rel_tol=1e-9, abs_tol=1e-9):
                raise ValueError(
                    f"accepted output shortfall {key} does not match calculated failure"
                )
        records.append(
            {
                **item,
                "owner_reason": reason,
                "disposition": "accepted_output_shortfall",
                "engineering_result": "FAIL",
            }
        )
    return records


def _set_assessment_status(
    status: dict[str, Any],
    phases: list[dict[str, Any]],
    retrofit: Mapping[str, Any] | None,
) -> None:
    output_failed = any(
        check["result"] == "FAIL"
        for phase in phases
        for family in phase["families"].values()
        for check in family["output_checks"].values()
    )
    mass_failed = any(
        family["mass_shortfall_kg"] > 0.0
        for phase in phases
        for family in phase["families"].values()
    )
    retrofit_failed = bool(retrofit and retrofit["recommended_additional_count"] > 0)
    if output_failed:
        status.update(
            result="FAIL",
            governing_case="phase_output",
            reason="one or more phase output checks fail",
        )
    elif mass_failed:
        status.update(
            result="FAIL",
            governing_case="phase_mass",
            reason="one or more phases have insufficient usable installed mass",
        )
    elif retrofit_failed:
        status.update(
            result="FAIL",
            governing_case="retrofit_additions",
            reason="the existing system requires additional retrofit anodes",
        )


def run_b401_structures_phases(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run component-local B401 families plus sequential phase/retrofit ledgers."""
    inputs = mapping(cfg.get("inputs"), "inputs")
    assessment = mapping(inputs.get("riser_base_assessment"), "riser_base_assessment")
    if inputs.get("structure") is not None:
        raise ValueError("riser_base_assessment cannot be combined with inputs.structure")
    flat_zones, summaries = flatten_components(assessment)
    flat_cfg = copy.deepcopy(cfg)
    flat_cfg["inputs"]["structure"] = {"zones": flat_zones}
    flat_cfg["inputs"].pop("riser_base_assessment", None)
    routed = run_b401_families(flat_cfg)
    results = routed["results"]
    edition = normalize_edition(
        str(inputs.get("design_data", {}).get("edition", DEFAULT_EDITION))
    )
    families = mapping(results.get("anode_families"), "anode_families")
    phases = _phase_results(assessment, inputs, families)
    accepted = _accepted_shortfalls(
        assessment.get("accepted_output_shortfalls"), phases
    )
    retrofit = retrofit_result(assessment.get("retrofit"), inputs, families, edition)
    results["riser_base_assessment"] = {
        "components": _component_results(summaries, results),
        "phases": phases,
        "retrofit": retrofit,
        "accepted_output_shortfalls": accepted,
        "rule_citations": rule_citations(edition),
        "project_practice_inputs": [
            "component composition and grouping",
            "phase segmentation, duration and demand allocation",
            "retrofit history, condition and remaining-mass inference",
            "accepted output shortfall disposition",
        ],
    }
    results["citations"] = sorted(set(results["citations"] + rule_citations(edition)))
    _set_assessment_status(results["status"], phases, retrofit)
    cfg["results"] = results
    return cfg


__all__ = ["run_b401_structures_phases"]
