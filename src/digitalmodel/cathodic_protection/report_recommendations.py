# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Report advice from the unchanged single-family B401 routed results."""

from __future__ import annotations

import math
from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection._edition import normalize_edition
from digitalmodel.cathodic_protection.b401_structures_phases_schema import (
    rule_citations,
)
from digitalmodel.cathodic_protection.b401_tables import protection_potential
from digitalmodel.reporting import TextBlock

_CHECKS = {"mass", "initial_current_output", "final_current_output"}


def _mapping(value: Any) -> Mapping[str, Any]:
    return value if isinstance(value, Mapping) else {}


def failed_keys(results: Mapping[str, Any]) -> list[str]:
    """Return the overall failure and each explicitly failed check."""
    status = _mapping(results.get("status"))
    keys = ["overall_status"] if status.get("result") == "FAIL" else []
    keys.extend(
        str(key)
        for key, value in sorted(_mapping(status.get("checks")).items())
        if value is False
    )
    return keys


def _number(value: Any, *, positive: bool = False) -> float:
    number = float(value)
    if not math.isfinite(number) or number < 0 or (positive and number <= 0):
        raise ValueError("recommendation requires finite non-negative quantities")
    return number


def _count(value: Any) -> int:
    number = _number(value)
    if not number.is_integer():
        raise ValueError("recommendation requires integer anode counts")
    return int(number)


def _quantities(results: Mapping[str, Any]) -> dict[str, float]:
    req = _mapping(results.get("anode_requirements"))
    ver = _mapping(results.get("current_output_verification"))
    values = {
        "checked": _count(ver["anode_count"]),
        "needed": _count(ver["recommended_anode_count"]),
        "mass": _number(req["individual_mass_kg"], positive=True),
        "required_mass": _number(req["total_mass_kg"]),
        "capacity": _number(req["electrochemical_capacity_Ah_kg"], positive=True),
        "utilisation": _number(req["utilization_factor"], positive=True),
        "hours": _number(req["design_life_hours"], positive=True),
        "mean": _number(ver["mean_current_demand_A"]),
        "mass_count": _count(ver["count_by_mass"]),
    }
    for phase in ("initial", "final"):
        values[f"{phase}_demand"] = _number(ver[f"{phase}_current_demand_A"])
        values[f"{phase}_output"] = _number(
            ver[f"anode_current_output_{phase}_A"], positive=True
        )
        values[f"{phase}_total"] = _number(ver[f"total_anode_current_output_{phase}_A"])
        values[f"{phase}_count"] = _count(ver[f"count_by_{phase}_current"])
    if values["utilisation"] > 1:
        raise ValueError("recommendation utilisation must not exceed one")
    counts = [values[f"{phase}_count"] for phase in ("mass", "initial", "final")]
    if values["needed"] != max(1, *counts):
        raise ValueError("recommended count does not match the routed sizing cases")
    values["additional"] = max(0, values["needed"] - values["checked"])
    return values


def _initial_design(results: Mapping[str, Any], key: str) -> str:
    values = _quantities(results)
    edition = normalize_edition(str(results.get("edition")))
    citations = rule_citations(edition)
    text = (
        f"The initial arrangement should use the route's recommended count of "
        f"{values['needed']:.0f} anodes; count checked: {values['checked']:.0f} anodes; "
        f"additional count: {values['additional']:.0f} anodes; additional net mass: "
        f"{values['additional'] * values['mass']:.3f} kg. "
        f"Mass demand = {values['mean']:.3f} A · {values['hours']:.1f} h / "
        f"({values['utilisation']:g} · {values['capacity']:g} Ah/kg) = "
        f"{values['required_mass']:.3f} kg; "
        f"ceil({values['required_mass']:.3f} kg / {values['mass']:.3f} kg per anode) "
        f"= {values['mass_count']:.0f} anodes (`{citations[4]}`). "
    )
    for phase in ("initial", "final"):
        text += (
            f"{phase.capitalize()} current sizing: "
            f"ceil({values[f'{phase}_demand']:.3f} A / "
            f"{values[f'{phase}_output']:.4f} A per anode) = "
            f"{values[f'{phase}_count']:.0f} anodes (`{citations[5]}`). "
        )
    if key == "overall_status":
        governing = _mapping(results.get("current_output_verification")).get(
            "governing_case"
        )
        key = {"mass": "mass", "initial": "initial_current_output"}.get(
            str(governing), "final_current_output"
        )
    text += _difference(values, key)
    return text + " See Appendix A for arrangement verification."


def _difference(values: Mapping[str, float], key: str) -> str:
    if key == "mass":
        demand = values["required_mass"]
        supplied = values["checked"] * values["mass"]
        quantity = "mass"
        units = "kg"
    else:
        phase = "initial" if key == "initial_current_output" else "final"
        demand = values[f"{phase}_demand"]
        supplied = values[f"{phase}_total"]
        quantity = f"{phase} current output"
        units = "A"
    if demand == 0:
        return f"The {quantity} demand is {demand:.3f} {units}."
    difference = 100 * (supplied - demand) / demand
    return (
        f"Checked {quantity} is {supplied:.3f} {units}, a signed difference of "
        f"{difference:.3f}% relative to the {demand:.3f} {units} demand."
    )


def _installed_structure(results: Mapping[str, Any]) -> str:
    edition = normalize_edition(str(results.get("edition")))
    criterion = protection_potential(edition).value
    ver = _mapping(results.get("current_output_verification"))
    labels = ver.get("citations") or []
    label = next(
        (str(item) for item in labels if "protective potential" in str(item)), ""
    )
    citation = f" (`{label}`)" if label else " (see Appendix A for citation evidence)"
    return (
        f"A structure-to-seawater potential survey is recommended against "
        f"{criterion:.3f} V versus Ag/AgCl/seawater{citation}; a measured potential "
        f"above {criterion:.3f} V should trigger investigation of protection "
        "performance. Anode remaining mass should be measured to assess depletion. "
        "See Appendix A for monitoring intervals, depletion criteria and condition data."
    )


def recommendation_block(results: Mapping[str, Any], key: str) -> TextBlock:
    """Describe three courses for one failure without modifying routed results."""
    title = f"Recommendation for {key.replace('_', ' ')}"
    result = (
        f"**{key.replace('_', ' ')}: FAIL.**\n\n" if key != "overall_status" else ""
    )
    supported = key in _CHECKS or key == "overall_status"
    try:
        initial = _initial_design(results, key) if supported else "See Appendix A."
        installed = _installed_structure(results) if supported else "See Appendix A."
    except (KeyError, TypeError, ValueError, OverflowError):
        initial = "See Appendix A for the missing sizing quantities."
        installed = "See Appendix A for the applicable monitoring criterion."
    return TextBlock(
        markdown=(
            result + f"### {title}\n\n"
            f"**Initial design:** {initial}\n\n"
            f"**Installed structure:** {installed}\n\n"
            "**Retrofit:** See Appendix A for quantity, timing and required condition data."
        )
    )


def conclusions_block(results: Mapping[str, Any]) -> TextBlock:
    """Summarise every failure and reference its detailed recommendation."""
    keys = failed_keys(results)
    lines = ["### Conclusions and recommendations", ""]
    if not keys:
        return TextBlock(markdown="\n".join([*lines, "No check fails."]))
    try:
        values = _quantities(results)
        quantity = (
            f"{values['additional']:.0f} additional anodes "
            f"({values['additional'] * values['mass']:.3f} kg additional net mass)"
        )
    except (KeyError, TypeError, ValueError, OverflowError):
        quantity = "the sizing quantities in Appendix A"
    for key in keys:
        title = f"Recommendation for {key.replace('_', ' ')}"
        if key not in _CHECKS and key != "overall_status":
            lines.append(
                f"- {title}: initial design, monitoring and Retrofit: Appendix A."
            )
            continue
        lines.append(
            f"- {title}: initial design should use {quantity}; an installed "
            "structure should receive a potential survey against the criterion "
            "and action trigger in that block. Retrofit quantity and timing: Appendix A."
        )
    return TextBlock(markdown="\n".join(lines))


def recommendation_caveats(results: Mapping[str, Any]) -> list[str]:
    """Collect limitations and missing condition evidence in the appendix."""
    if not failed_keys(results):
        return []
    caveats = [
        "For the route's recommended count, the recommended arrangement is not "
        "re-checked by this calculation; output and mass checks use the count checked.",
        "retrofit quantity not evaluated; retrofit timing not evaluated: confirmed "
        "installed count and original installed net mass, installation date or elapsed "
        "service, measured remaining mass or consumption, measured potentials, and "
        "historical and future current demand are required for an installed-system "
        "assessment. The constant-demand depletion profile does not establish the "
        "latest intervention year for a current-output failure.",
        "Monitoring interval and depletion action trigger are not evaluated: an "
        "applicable interval and action criterion are required; citation not held in "
        "the repository for these items in the selected route and edition.",
    ]
    try:
        _quantities(results)
    except (KeyError, TypeError, ValueError, OverflowError):
        caveats.append(
            "Initial-design quantities and monitoring applicability are not evaluated: "
            "complete routed demands, capacities, checked and recommended counts, "
            "and the applicable protection criterion are required."
        )
    unsupported = set(failed_keys(results)) - _CHECKS - {"overall_status"}
    if unsupported:
        caveats.append(
            "Recommendations for additional check types are not evaluated: "
            "the applicable check method, quantities and governing citation are required."
        )
    ver = _mapping(results.get("current_output_verification"))
    if not any(
        "protective potential" in str(item) for item in ver.get("citations") or []
    ):
        caveats.append(
            "Monitoring citation not held in the repository's routed citation list: "
            "the selected edition's protection-potential citation is required."
        )
    return caveats
