# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Report advice from the unchanged single-family B401 routed results."""

from __future__ import annotations

import math
from copy import deepcopy
from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection._edition import normalize_edition, standard_for_edition
from digitalmodel.cathodic_protection.b401_structures_phases_schema import (
    rule_citations,
)
from digitalmodel.cathodic_protection.b401_tables import (
    citation_label,
    protection_potential,
)
from digitalmodel.reporting.spec import Block, TableBlock, TextBlock
from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection


def _check_arrangement(inputs: Mapping[str, Any], count: int, length: float | None = None,
                       mass: float | None = None) -> dict[str, Any]:
    """Recalculate a proposed arrangement with the shared engine."""
    candidate = deepcopy(dict(inputs))
    anode = candidate["anode"]
    anode["count"] = count
    if length is not None:
        anode["length_m"] = length
        anode["individual_anode_mass_kg"] = mass
        anode.pop("radius_m", None)
    calculated = run_cathodic_protection({"inputs": candidate})
    return dict(_mapping(calculated["results"]))


def _geometry_option(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> dict[str, Any]:
    """Size an equivalent-cylinder option at fixed count and recheck it."""
    values = _quantities(results)
    count = int(values["checked"])
    mass = math.ceil(max(values["mass"], values["required_mass"] / count))
    lower = 0.0
    upper = float(_mapping(inputs.get("anode"))["length_m"])
    checked = _check_arrangement(inputs, count, upper, mass)
    for _ in range(32):
        if checked["status"]["result"] == "PASS":
            break
        lower, upper = upper, upper * 2
        checked = _check_arrangement(inputs, count, upper, mass)
    else:
        raise ValueError("a passing equivalent-cylinder option was not established")
    for _ in range(24):
        middle = (lower + upper) / 2
        trial = _check_arrangement(inputs, count, middle, mass)
        if trial["status"]["result"] == "PASS":
            upper = middle
        else:
            lower = middle
    length = math.ceil(upper * 1000) / 1000
    checked = _check_arrangement(inputs, count, length, mass)
    if checked["status"]["result"] != "PASS":
        raise ValueError("the dimensioned arrangement did not pass its recheck")
    return {"length": length, "mass": mass, "radius": kernel.equivalent_radius_from_mass(mass, length),
            "results": checked}


def _arrangement_rows(results: Mapping[str, Any]) -> list[list[Any]]:
    """Return the installed quantity, outputs and verdict from native results."""
    verification = _mapping(results["current_output_verification"])
    requirements = _mapping(results["anode_requirements"])
    return [["Anode count", verification["anode_count"]],
            ["Installed net mass (kg)", f"{verification['anode_count'] * requirements['individual_mass_kg']:.0f}"],
            ["Initial current output (A)", verification["total_anode_current_output_initial_A"]],
            ["Final current output (A)", verification["total_anode_current_output_final_A"]],
            ["Mass check", "PASS" if verification["mass_meets_requirement"] else "FAIL"],
            ["Initial current check", "PASS" if verification["initial_meets_demand"] else "FAIL"],
            ["Final current check", "PASS" if verification["final_meets_demand"] else "FAIL"],
            ["Overall", results["status"]["result"]]]


def arrangement_options(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> list[Block]:
    """Show independently rechecked design choices and condition-based actions."""
    if not failed_keys(results):
        return []
    values = _quantities(results)
    values["needed"] = max(values["needed"], values["checked"])
    values["additional"] = values["needed"] - values["checked"]
    increased = _check_arrangement(inputs, int(values["needed"]))
    if increased["status"]["result"] != "PASS":
        raise ValueError("increased-count arrangement did not pass its recheck")
    geometry = _geometry_option(inputs, results)
    original_rows, increased_rows = _arrangement_rows(results), _arrangement_rows(increased)
    rows = [[a[0], a[1], b[1]] for a, b in zip(original_rows, increased_rows)]
    criterion = protection_potential(normalize_edition(str(results.get("edition")))).value
    edition = normalize_edition(str(results["edition"]))
    retrofit_subject = "retrofit evidence" if edition == "2021" else "retrofit documentation"
    selected = {subject: _rule_source(edition, subject)
                for subject in ("anode mass", "output adequacy", retrofit_subject)}
    standard = f"{standard_for_edition(edition)}, Cathodic protection design"
    sources = (
        f"[b401-anode-mass] {standard}, {selected['anode mass']}; "
        f"[b401-output-adequacy] {standard}, {selected['output adequacy']}; "
        f"[b401-protection-criterion] {standard}, protective potential criterion; "
        f"[b401-retrofit] {standard}, {selected[retrofit_subject]}."
    )
    evidence = _mapping(_mapping(inputs.get("structure")).get("verification_evidence"))
    attachment = (
        "Clamped attachment is the published proposal for the measured lower-level deficiency "
        "[published-jacket-paper, p. 244, Section 5]. "
        if evidence.get("source_key") == "published-jacket-paper" else
        "Attachment and electrical continuity require condition and structural records [b401-retrofit]. "
    )
    retrofit_count = max(0, int(values["needed"] - values["checked"]))
    geometry_rows = [["Anode count", int(values["checked"]), int(values["checked"])],
                     ["Length (m)", _mapping(inputs["anode"])["length_m"], geometry["length"]],
                     ["Net mass per anode (kg)", values["mass"], geometry["mass"]],
                     ["Equivalent radius (m)", _mapping(results["current_output_verification"])["equivalent_radius_initial_m"], geometry["radius"]],
                     *[[a[0], a[1], b[1]] for a, b in zip(original_rows[2:], _arrangement_rows(geometry["results"])[2:])]]
    return [TextBlock(markdown=f"### Design increase and recheck\n\nAn increase to {values['needed']:.0f} anodes adds {values['additional']:.0f} units and {values['additional'] * values['mass']:.0f} kg net alloy. Count = max(count by mass, initial output, final output). Each option below is recalculated against the same demands [b401-anode-mass; b401-output-adequacy]."),
            TableBlock(title="Original and increased-count arrangement", columns=["Check or quantity", "Original", "Rechecked"], rows=rows),
            TextBlock(markdown=f"### Anode geometry alternative\n\nThe count can instead remain {values['checked']:.0f} with the equivalent-cylinder dimensions below. Radius = sqrt(net mass / (π × length × {kernel.ANODE_DENSITY_ALZNI:g} kg/m³)); alloy density is the existing aluminium calculation basis. The increased-count option retains the original unit dimensions and is preferred until attachment, clearance and fabrication constraints qualify the geometry alternative [b401-output-adequacy]."),
            TableBlock(title="Original and dimensioned equivalent-cylinder arrangement", columns=["Check or quantity", "Original", "Rechecked"], rows=geometry_rows),
            TextBlock(markdown=f"### Monitoring\n\nStructure potentials versus Ag/AgCl/seawater should be recorded at all monitoring levels and at remote or shielded members, buried interfaces and locations distant from anodes. A potential more positive than {criterion:.3f} V triggers investigation and intervention planning; protection requires a potential more negative than {criterion:.3f} V (illustrative input). Remaining anode mass and output should be recorded with elapsed service time. A survey before intervention and a survey after connection should confirm the response at the same locations. A measured depletion rate that cannot support the remaining life triggers additional sizing [b401-retrofit]."),
            TextBlock(markdown=f"### Retrofit\n\nAdditional electrically connected aluminium stand-off anodes provide a provisional aggregate allowance of {retrofit_count} units ({retrofit_count * values['mass']:.0f} kg), from the maximum mass and phase-output shortfall [b401-retrofit; b401-output-adequacy]. This aggregate quantity does not determine lower-level allocation. {attachment}Intervention planning is triggered at the time a potential exceeds the stated criterion or measured remaining capacity cannot support the remaining life. Latest intervention year and local retrofit quantity are not evaluated: elapsed service, remaining mass, future demand and a spatial survey are required."),
            TextBlock(markdown=sources)]

_CHECKS = {"mass", "initial_current_output", "final_current_output"}


def _rule_source(edition: Any, key: str) -> str:
    """Select a rule by its subject, independent of register ordering."""
    matches = [label for label in rule_citations(edition) if label.endswith(f"({key})")]
    if len(matches) != 1:
        raise ValueError(f"one citation is required for {key}")
    return matches[0]


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
    mass_source = _rule_source(edition, "anode mass")
    output_source = _rule_source(edition, "output adequacy")
    text = (
        f"The initial arrangement should use the route's recommended count of "
        f"{values['needed']:.0f} anodes; count checked: {values['checked']:.0f} anodes; "
        f"additional count: {values['additional']:.0f} anodes; additional net mass: "
        f"{values['additional'] * values['mass']:.3f} kg. "
        f"Mass demand = {values['mean']:.3f} A · {values['hours']:.1f} h / "
        f"({values['utilisation']:g} · {values['capacity']:g} Ah/kg) = "
        f"{values['required_mass']:.3f} kg; "
        f"ceil({values['required_mass']:.3f} kg / {values['mass']:.3f} kg per anode) "
        f"= {values['mass_count']:.0f} anodes (`{mass_source}`). "
    )
    for phase in ("initial", "final"):
        text += (
            f"{phase.capitalize()} current sizing: "
            f"ceil({values[f'{phase}_demand']:.3f} A / "
            f"{values[f'{phase}_output']:.4f} A per anode) = "
            f"{values[f'{phase}_count']:.0f} anodes (`{output_source}`). "
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
        f"{difference:.1f}% relative to the {demand:.3f} {units} demand."
    )


def _installed_structure(results: Mapping[str, Any]) -> str:
    edition = normalize_edition(str(results.get("edition")))
    # DNV-RP-B401: edition-specific protective-potential clause and value
    # are kept together by the cited table lookup; no sizing data are needed.
    cited = protection_potential(edition)
    criterion = cited.value
    citation = f" (`{citation_label(cited.citation)}`)"
    source_note = ""
    if edition == "2005":
        source_note = (
            " The selected edition 2005 with 2008 amendments uses the "
            "October 2010 protective-potential source (printed 2011) "
            "in the repository lookup."
        )
    elif edition == "2010":
        source_note = (
            " The citation identifies the October 2010 protective-potential "
            "source (printed 2011)."
        )
    return (
        f"A structure-to-seawater potential survey is recommended against "
        f"{criterion:.3f} V versus Ag/AgCl/seawater{citation}; a measured potential "
        f"above {criterion:.3f} V should trigger investigation of protection "
        f"performance.{source_note} "
        "Anode remaining mass should be measured to assess depletion. "
        "See Appendix A for monitoring intervals, depletion criteria and condition data."
    )


def recommendation_block(results: Mapping[str, Any], key: str) -> TextBlock:
    """Describe three courses for one failure without modifying routed results."""
    title = f"Recommendation for {key.replace('_', ' ')}"
    result = (
        f"**{key.replace('_', ' ')}: FAIL.**\n\n" if key != "overall_status" else ""
    )
    supported = key in _CHECKS or key == "overall_status"
    missing_method = (
        "NOT_EVALUATED: the applicable check method, quantities and governing "
        "citation are required. See Appendix A."
    )
    initial = retrofit = missing_method
    if supported:
        try:
            initial = _initial_design(results, key)
        except (KeyError, TypeError, ValueError, OverflowError):
            initial = (
                "NOT_EVALUATED: complete routed sizing quantities and the "
                "applicable edition are required. See Appendix A."
            )
        retrofit = (
            "Quantity and latest intervention year: NOT_EVALUATED. Confirmed "
            "installed count and net mass, elapsed service, measured remaining mass, "
            "measured potentials and historical and future current demand are "
            "required. See Appendix A for method limitations."
        )
    try:
        installed = _installed_structure(results)
    except (KeyError, TypeError, ValueError, OverflowError):
        installed = (
            "NOT_EVALUATED: the applicable edition and its cited monitoring "
            "criterion are required. See Appendix A."
        )
    return TextBlock(
        markdown=(
            result + f"### {title}\n\n"
            f"**Initial design:** {initial}\n\n"
            f"**Installed structure:** {installed}\n\n"
            f"**Retrofit:** {retrofit}"
        )
    )


def conclusions_block(results: Mapping[str, Any]) -> TextBlock:
    """Summarise every failure and reference its detailed recommendation."""
    keys = failed_keys(results)
    lines = ["### Conclusions and recommendations", ""]
    if not keys:
        return TextBlock(markdown="\n".join([*lines, "No check fails."]))
    try:
        normalize_edition(str(results.get("edition")))
        values = _quantities(results)
        initial = (
            f"initial design should use {values['additional']:.0f} additional anodes "
            f"({values['additional'] * values['mass']:.3f} kg additional net mass)"
        )
    except (KeyError, TypeError, ValueError, OverflowError):
        initial = "initial design quantity is NOT_EVALUATED (see Appendix A)"
    try:
        protection_potential(normalize_edition(str(results.get("edition"))))
        installed = (
            "an installed structure should receive a potential survey against "
            "the criterion and action trigger in that block"
        )
    except (KeyError, TypeError, ValueError, OverflowError):
        installed = "installed-structure monitoring is NOT_EVALUATED (edition required)"
    for key in keys:
        title = f"Recommendation for {key.replace('_', ' ')}"
        design = initial
        if key not in _CHECKS and key != "overall_status":
            design = (
                "initial design quantity is NOT_EVALUATED; the applicable "
                "method and citation are required (Appendix A)"
            )
        lines.append(
            f"- {title}: {design}; {installed}. Retrofit quantity and timing: "
            "NOT_EVALUATED (Appendix A)."
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
        normalize_edition(str(results.get("edition")))
        _quantities(results)
    except (KeyError, TypeError, ValueError, OverflowError):
        caveats.append(
            "Initial-design quantities are not evaluated: "
            "complete routed demands, capacities, checked and recommended counts, "
            "and the applicable edition are required."
        )
    unsupported = set(failed_keys(results)) - _CHECKS - {"overall_status"}
    if unsupported:
        caveats.append(
            "Initial-design and retrofit recommendations for additional check types "
            "are not evaluated: "
            "the applicable check method, quantities and governing citation are required."
        )
    try:
        protection_potential(normalize_edition(str(results.get("edition"))))
    except (KeyError, TypeError, ValueError, OverflowError):
        caveats.append(
            "Installed-structure monitoring is not evaluated: the applicable edition "
            "and its cited protection-potential criterion are required."
        )
    return caveats
