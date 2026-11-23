"""Report sections for B401 riser-base phased assessments."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any, cast

from digitalmodel.cathodic_protection.b401_family_report import (
    build_b401_family_sections,
)
from digitalmodel.reporting.spec import Section, TableBlock, TextBlock


def build_b401_structures_phases_sections(
    inputs: Mapping[str, Any], results: Mapping[str, Any]
) -> list[Section]:
    """Append component, phase, retrofit and disposition tables."""
    sections = cast(list[Section], build_b401_family_sections(inputs, results))
    sections.append(_assessment_section(results["riser_base_assessment"]))
    return sections


def _assessment_section(assessment: Mapping[str, Any]) -> Section:
    return Section(
        key="riser-base-phases",
        title="Riser-base composition and phased assessment",
        blocks=[
            TextBlock(
                markdown=(
                    "Phase scheduling, compositions, retrofit inference and owner "
                    "dispositions are explicit project-practice inputs."
                )
            ),
            _components_table(assessment),
            _phases_table(assessment),
            _retrofit_table(assessment),
            _accepted_table(assessment),
        ],
    )


def _components_table(assessment: Mapping[str, Any]) -> TableBlock:
    rows = [
        [
            name,
            row["type"],
            row["area_m2"],
            ", ".join(row["anode_families"]),
            row["composition_basis"],
            row["composition_reason"],
        ]
        for name, row in assessment["components"].items()
    ]
    return TableBlock(
        title="Riser-base components",
        columns=[
            "Component",
            "Type",
            "Area [m2]",
            "Anode families",
            "Composition basis",
            "Composition reason",
        ],
        rows=rows,
    )


def _phases_table(assessment: Mapping[str, Any]) -> TableBlock:
    rows = [
        [
            phase["name"],
            family,
            row["consumed_mass_kg"],
            row["usable_mass_end_kg"],
            row["mass_shortfall_kg"],
            row["additional_anode_count"],
            row["output_checks"]["initial_output"]["demand_A"],
            row["output_checks"]["initial_output"]["output_A"],
            row["output_checks"]["initial_output"]["ratio"],
            row["output_checks"]["initial_output"]["result"],
            row["output_checks"]["final_output"]["demand_A"],
            row["output_checks"]["final_output"]["output_A"],
            row["output_checks"]["final_output"]["ratio"],
            row["output_checks"]["final_output"]["result"],
        ]
        for phase in assessment["phases"]
        for family, row in phase["families"].items()
    ]
    return TableBlock(
        title="Phase mass ledger",
        columns=[
            "Phase",
            "Family",
            "Consumed [kg]",
            "Usable end [kg]",
            "Mass shortfall [kg]",
            "Additional anodes",
            "Initial demand [A]",
            "Initial output [A]",
            "Initial demand/output ratio",
            "Initial result",
            "Final demand [A]",
            "Final output [A]",
            "Final demand/output ratio",
            "Final result",
        ],
        rows=rows,
    )


def _retrofit_table(assessment: Mapping[str, Any]) -> TableBlock:
    retrofit = assessment.get("retrofit") or {}
    rows = [
        [key.replace("_", " "), value]
        for key, value in retrofit.items()
        if not isinstance(value, (dict, list))
    ]
    for case, check in retrofit.get("combined_output_checks", {}).items():
        rows.append(
            [
                f"combined {case} output",
                (
                    f"{check['total_output_A']:.6g} A vs "
                    f"{check['demand_A']:.6g} A: {check['result']}"
                ),
            ]
        )
    return TableBlock(
        title="Retrofit assessment", columns=["Result", "Value"], rows=rows
    )


def _accepted_table(assessment: Mapping[str, Any]) -> TableBlock:
    rows = [
        [
            row["family"],
            row["phase"],
            row["criterion"],
            row["engineering_result"],
            row["demand_A"],
            row["output_A"],
            row["ratio"],
            row["owner_reason"],
            row["basis"]["source"],
            row["basis"]["reason"],
        ]
        for row in assessment["accepted_output_shortfalls"]
    ]
    return TableBlock(
        title="Accepted output shortfalls (engineering result remains FAIL)",
        columns=[
            "Family",
            "Phase",
            "Criterion",
            "Engineering result",
            "Demand [A]",
            "Output [A]",
            "Demand/output ratio",
            "Owner reason",
            "Basis source",
            "Basis reason",
        ],
        rows=rows or [["none", "", "", "", "", "", "", "", "", ""]],
    )


__all__ = ["build_b401_structures_phases_sections"]
