"""Report sections for multi-component B401 riser designs."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

from digitalmodel.reporting.spec import Section, TableBlock

COMPONENT_COLUMNS = [
    "Component",
    "Type",
    "Life",
    "Environment",
    "Zone",
    "Disposition",
    "Area",
    "Family",
    "Coating basis",
    "Coating edition",
    "Coating system",
    "Operating temperature",
    "Maximum temperature",
    "Coating applicability",
    "Concrete weight coating",
    "Field-joint infill",
    "Compatible linepipe",
    "Initial",
    "Mean",
    "Final",
    "Rationale",
    "Component totals",
    "Coverage",
    "Component status",
    "Component governing family",
    "Component governing case",
    "Component governing ratio",
]
COMPONENT_UNITS = [
    "-",
    "-",
    "yr",
    "-",
    "-",
    "-",
    "m2",
    "-",
    "-",
    "-",
    "-",
    "degC",
    "degC",
    "-",
    "-",
    "-",
    "-",
    "A",
    "A",
    "-",
    "-",
    "-",
    "-",
    "-",
    "A",
    "-",
    "A",
]


def _mapping(value: Any) -> Mapping[str, Any]:
    return value if isinstance(value, Mapping) else {}


def _component_rows(results: Mapping[str, Any]) -> list[list[Any]]:
    rows: list[list[Any]] = []
    for name, raw in sorted(_mapping(results.get("components")).items()):
        component = _mapping(raw)
        for zone_name, zone_raw in sorted(_mapping(component.get("zones")).items()):
            zone = _mapping(zone_raw)
            coating = _mapping(zone.get("coating_breakdown"))
            demand = component.get("current_demand_A", {})
            status = _mapping(component.get("status"))
            rows.append(
                [
                    name,
                    component.get("type", ""),
                    component.get("design_life_years", ""),
                    component.get("environment", ""),
                    zone_name,
                    zone.get("disposition", ""),
                    zone.get("area_m2", ""),
                    zone.get("anode_family", ""),
                    coating.get("basis", ""),
                    coating.get("edition", ""),
                    coating.get("system", ""),
                    coating.get("operating_temperature_C", ""),
                    coating.get("max_temperature_C", ""),
                    coating.get("applicability", ""),
                    coating.get("concrete_weight_coating", ""),
                    coating.get("infill", ""),
                    coating.get("compatible_linepipe", ""),
                    zone.get("I_initial_A", 0.0),
                    zone.get("I_mean_A", 0.0),
                    zone.get("I_final_A", 0.0),
                    zone.get("rationale", ""),
                    demand,
                    component.get("coverage", ""),
                    status.get("result", ""),
                    status.get("governing_family", ""),
                    status.get("governing_case", ""),
                    status.get("governing_ratio", ""),
                ]
            )
    return rows


def _allocation_rows(results: Mapping[str, Any]) -> list[list[Any]]:
    rows: list[list[Any]] = []
    for component_name, raw in sorted(_mapping(results.get("components")).items()):
        for family_name, check_raw in sorted(
            _mapping(_mapping(raw).get("allocation_checks")).items()
        ):
            check = _mapping(check_raw)
            ratios = _mapping(check.get("adequacy_ratios"))
            rows.append(
                [
                    component_name,
                    family_name,
                    check.get("capacity_fraction", ""),
                    check.get("required_mass_kg", ""),
                    check.get("allocated_mass_kg", ""),
                    check.get("required_anode_count", ""),
                    check.get("allocated_initial_output_A", ""),
                    check.get("allocated_final_output_A", ""),
                    ratios.get("mass", ""),
                    ratios.get("initial", ""),
                    ratios.get("final", ""),
                    _mapping(check.get("checks")).get("mass", ""),
                    _mapping(check.get("checks")).get("initial", ""),
                    _mapping(check.get("checks")).get("final", ""),
                    check.get("governing_case", ""),
                    check.get("governing_ratio", ""),
                ]
            )
    return rows


def _host_rows(results: Mapping[str, Any]) -> list[list[Any]]:
    rows = []
    for name, raw in sorted(_mapping(results.get("anode_families")).items()):
        family = _mapping(raw)
        count = int(family.get("anode_count", 0))
        checks = _mapping(family.get("checks"))
        rows.append(
            [
                family.get("installed_on_component", ""),
                name,
                family.get("anode_type", ""),
                family.get("environment", ""),
                family.get("anode_count", ""),
                family.get("individual_mass_kg", ""),
                count * float(family.get("individual_mass_kg", 0.0)),
                family.get("required_mass_kg", ""),
                count * float(family.get("current_output_initial_A", 0.0)),
                count * float(family.get("current_output_final_A", 0.0)),
                checks.get("mass", ""),
                checks.get("initial", ""),
                checks.get("final", ""),
                family.get("governing_case", ""),
                family.get("governing_ratio", ""),
            ]
        )
    return rows


def _continuity_rows(results: Mapping[str, Any]) -> list[list[Any]]:
    rows = []
    for family_name, raw in sorted(_mapping(results.get("anode_families")).items()):
        family = _mapping(raw)
        for target, path in sorted(_mapping(family.get("continuity_paths")).items()):
            check = _mapping(_mapping(family.get("continuity_checks")).get(target))
            rows.append(
                [
                    family_name,
                    family.get("installed_on_component", ""),
                    target,
                    " -> ".join(path),
                    check.get("protected_life_years", ""),
                    check.get("minimum_edge_availability_years", ""),
                    check.get("host_life_check", ""),
                    check.get("life_check", ""),
                ]
            )
    return rows


def _references(results: Mapping[str, Any]) -> Section:
    usage = []
    components = _mapping(results.get("components"))
    for component_name, component_raw in sorted(components.items()):
        for zone_name, zone_raw in sorted(
            _mapping(_mapping(component_raw).get("zones")).items()
        ):
            zone = _mapping(zone_raw)
            coating = _mapping(zone.get("coating_breakdown"))
            density = _mapping(zone.get("current_densities_A_m2"))
            labels = sorted(
                set((coating.get("citations") or []) + (density.get("citations") or []))
            )
            usage.append([f"{component_name}/{zone_name}", ", ".join(labels) or "none"])
    for family_name, family_raw in sorted(
        _mapping(results.get("anode_families")).items()
    ):
        usage.append(
            [
                f"{family_name}: electrochemistry",
                ", ".join(_mapping(family_raw).get("citations") or []),
            ]
        )
    return Section(
        key="references",
        title="References",
        blocks=[
            TableBlock(
                title="Where each cited table was used",
                columns=["Item", "Citations"],
                rows=usage,
            )
        ],
    )


def _design_basis_section(results: Mapping[str, Any]) -> Section:
    return Section(
        key="design-basis",
        title="Design basis and component scope",
        blocks=[
            TableBlock(
                title="Component demand and disposition",
                columns=COMPONENT_COLUMNS,
                units=COMPONENT_UNITS,
                rows=_component_rows(results),
            )
        ],
    )


def _allocation_section(results: Mapping[str, Any]) -> Section:
    return Section(
        key="current-demand",
        title="Protected-component allocations",
        blocks=[
            TableBlock(
                title="Protected-component allocations",
                columns=[
                    "Component",
                    "Family",
                    "Fraction",
                    "Required mass",
                    "Allocated mass",
                    "Required count",
                    "Allocated initial output",
                    "Allocated final output",
                    "Mass ratio",
                    "Initial ratio",
                    "Final ratio",
                    "Mass PASS",
                    "Initial PASS",
                    "Final PASS",
                    "Governing",
                    "Ratio",
                ],
                rows=_allocation_rows(results),
            )
        ],
    )


def _anode_section(results: Mapping[str, Any]) -> Section:
    return Section(
        key="anode-requirements",
        title="Physically hosted anode systems",
        blocks=[
            TableBlock(
                title="Physically hosted anode families",
                columns=[
                    "Host",
                    "Family",
                    "Type",
                    "Environment",
                    "Count",
                    "Unit mass",
                    "Installed mass",
                    "Required mass",
                    "Installed initial output",
                    "Installed final output",
                    "Mass PASS",
                    "Initial PASS",
                    "Final PASS",
                    "Governing",
                    "Ratio",
                ],
                rows=_host_rows(results),
            ),
            TableBlock(
                title="Electrical continuity paths",
                columns=[
                    "Family",
                    "Host",
                    "Protected component",
                    "Resolved path",
                    "Protected life",
                    "Minimum edge availability",
                    "Host life PASS",
                    "Life PASS",
                ],
                rows=_continuity_rows(results),
            ),
        ],
    )


def _adequacy_rows(results: Mapping[str, Any]) -> list[list[Any]]:
    status = _mapping(results.get("status"))
    req = _mapping(results.get("anode_requirements"))
    rec = _mapping(results.get("reconciliation"))
    checks = {key: value for key, value in rec.items() if key.endswith("_checks")}
    return [
        ["Installed anode count", req.get("installed_anode_count", "")],
        ["Installed anode mass", req.get("installed_anode_mass_kg", "")],
        ["Hosted count reconciliation", rec.get("hosted_anode_count", "")],
        ["Allocated fractions", rec.get("allocated_fraction_by_family", "")],
        ["Allocated mass", rec.get("allocated_mass_kg_by_family", "")],
        [
            "Allocated initial output",
            rec.get("allocated_initial_output_A_by_family", ""),
        ],
        ["Allocated final output", rec.get("allocated_final_output_A_by_family", "")],
        ["Physical mass", rec.get("physical_mass_kg_by_family", "")],
        ["Physical initial output", rec.get("physical_initial_output_A_by_family", "")],
        ["Physical final output", rec.get("physical_final_output_A_by_family", "")],
        ["Allocation reconciliation checks", checks],
        ["Governing component", status.get("governing_component", "")],
        ["Governing family", status.get("governing_family", "")],
        ["Governing case", status.get("governing_case", "")],
        ["Governing ratio", status.get("governing_ratio", "")],
    ]


def _adequacy_section(results: Mapping[str, Any]) -> Section:
    from digitalmodel.cathodic_protection.report_adapters import (
        _status_block,
        _use_status_block,
    )

    status = _mapping(results.get("status"))
    return Section(
        key="adequacy",
        title="Adequacy",
        blocks=[
            _use_status_block(status),
            _status_block(status, "Multi-component riser anode design"),
            TableBlock(
                title="Overall reconciliation and governing case",
                columns=["Item", "Value"],
                rows=_adequacy_rows(results),
            ),
        ],
    )


def build_b401_component_sections(
    inputs: Mapping[str, Any], results: Mapping[str, Any]
) -> list[Section]:
    """Build the standard anode-design sections for component mode."""
    del inputs
    return [
        _design_basis_section(results),
        _allocation_section(results),
        _anode_section(results),
        _adequacy_section(results),
        _references(results),
    ]


__all__ = ["build_b401_component_sections"]
