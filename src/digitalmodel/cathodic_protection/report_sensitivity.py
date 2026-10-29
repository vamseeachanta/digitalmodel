# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Sensitivity presentation using engine flags and exact integer criteria."""

from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection._edition import STANDARD_BY_EDITION
from digitalmodel.reporting.spec import Section, TableBlock, TextBlock


def _criterion_references(standard: str) -> tuple[str, str]:
    edition = next((key for key, label in STANDARD_BY_EDITION.items()
                    if label == standard), None)
    if edition is None:
        raise ValueError("sensitivity criterion edition is unsupported")
    if edition == "2021":
        mass, count = "[4.7.1]", "[4.8.4]"
    elif edition == "2017":
        mass, count = "[7.7.1]", "[7.8.4]"
    else:
        mass, count = "Sec. 7.7.1", "Sec. 7.8.4"
    return f"{standard}, {mass}; {count}", f"{standard}, {count}"


def _criterion(ver: Mapping[str, Any], flag: str, required: str, reference: str) -> str:
    provided, needed, passed = ver.get("anode_count"), ver.get(required), ver.get(flag)
    if (type(provided) is not int or type(needed) is not int
            or provided < 1 or needed < 0 or type(passed) is not bool):
        raise ValueError("sensitivity count criterion not evaluated: counts and flag required")
    if passed != (provided >= needed):
        raise ValueError("sensitivity count criterion disagrees with engine flag")
    status = "PASS" if passed else "FAIL"
    comparator = ">=" if passed else "<"
    return (f"{status}; N_verified {provided} {comparator} {needed} required; "
            f"margin {provided - needed:+d} anodes; {reference}")


def sensitivity_section(rows: Any) -> Section:
    """Render one row per case with criteria beside the engine's own statuses."""
    from digitalmodel.cathodic_protection.sensitivity import MODES, PARAMETER_UNITS, _number

    if not isinstance(rows, list) or not rows:
        raise ValueError("sensitivity rows must be a non-empty list")
    data = []
    sources = set()
    criterion_sources = set()
    for row in rows:
        if not isinstance(row, Mapping) or row.get("mode") not in MODES:
            raise ValueError("sensitivity row mode is required")
        parameter = row.get("parameter")
        if parameter not in PARAMETER_UNITS:
            raise ValueError("sensitivity parameter is unsupported")
        ver = row.get("adequacy")
        if not isinstance(ver, Mapping):
            raise ValueError("sensitivity adequacy is not evaluated")
        numbers = [_number(row.get(key), key, positive=True) for key in
                   ("initial_current_A", "mean_current_A", "final_current_A", "required_net_mass_kg")]
        value = _number(row.get("value"), "parameter value")
        count = row.get("recommended_anode_count")
        if type(count) is not int or count < 1:
            raise ValueError("recommended anode count is not evaluated")
        basis = row.get("basis")
        if not isinstance(basis, Mapping) or not basis.get("base_input") or not basis.get("standard"):
            raise ValueError("sensitivity source basis is required")
        mass_reference, count_reference = _criterion_references(basis["standard"])
        sources.add(f"{basis['base_input']}; {basis['standard']}")
        criterion_sources.add(mass_reference)
        data.append([
            row["mode"], f"{parameter} = {value:g} {PARAMETER_UNITS[parameter]}",
            *(f"{number:.3f}" for number in numbers[:3]), f"{numbers[3]:.2f}", count,
            _criterion(ver, "initial_meets_demand", "count_by_initial_current", count_reference),
            _criterion(ver, "mass_meets_requirement", "count_by_mass", mass_reference),
            _criterion(ver, "final_meets_demand", "count_by_final_current", count_reference),
        ])
    return Section(key="sensitivity", title="Sensitivity analysis", blocks=[
        TextBlock(markdown=(
            "One parameter changes at a time; other inputs retain their base values. "
            "Temperature has no effect in published mode because it retains effective "
            "phase densities; identical rows do not establish physical temperature "
            "insensitivity. In standard_tables mode temperature changes the table "
            "densities for the two bare seawater zones only; coated and buried zones "
            "retain their published densities. standard_tables is a what-if "
            "on a modified input, not a published result. The surrounding analysis uses "
            "the unchanged base input. Status checks use the base installed count, not "
            "the recommended count. Integer count criteria preserve shortfalls even "
            "when displayed current or mass values round to the same number."
        )),
        TableBlock(title="One-at-a-time sensitivity", columns=[
            "Mode", "Parameter and value [unit]", "Initial current [A]", "Mean current [A]",
            "Final current [A]", "Required net mass [kg]", "Recommended count [anodes]",
            "Initial status and criterion [anodes]", "Mass status and criterion [anodes]",
            "Final status and criterion [anodes]",
        ], rows=data, source=(
            "Source: read-only run of run_cathodic_protection using the sweep config "
            f"and base input ({'; '.join(sorted(sources))}); not yet reviewed. "
            "Criteria: engine fresh/depleted "
            f"current-output and mass counts, {'; '.join(sorted(criterion_sources))}; "
            "edition follows each row's base input."
        )),
    ])
