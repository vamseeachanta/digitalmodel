"""Report sections for B401 designs containing independent anode families."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.reporting.figures import figure_from_columns
from digitalmodel.reporting.spec import FigureBlock, Section, TableBlock


def _mapping(value: Any) -> Mapping[str, Any]:
    return value if isinstance(value, Mapping) else {}


def _zone_names(results: Mapping[str, Any]) -> list[str]:
    return [
        name for name in _mapping(results.get("surface_areas_m2")) if name != "total_m2"
    ]


def _design_basis(results: Mapping[str, Any]) -> Section:
    areas = _mapping(results.get("surface_areas_m2"))
    demand = _mapping(results.get("current_demand_A"))
    rows = [
        [
            name,
            _mapping(demand.get(name)).get("area_basis", "").replace("_", " "),
            areas[name],
            _mapping(demand.get(name)).get("anode_family", ""),
        ]
        for name in _zone_names(results)
    ]
    return Section(
        key="design-basis",
        title="Design basis",
        blocks=[
            TableBlock(
                title="Zone area basis and family assignment",
                columns=["Zone", "Area basis", "Area", "Anode family"],
                units=["-", "-", "m2", "-"],
                rows=rows,
                source="cfg[results][surface_areas_m2], cfg[results][current_demand_A]",
            )
        ],
    )


def _demand_series(
    results: Mapping[str, Any], design_life: float
) -> dict[str, list[float]]:
    demand = _mapping(results.get("current_demand_A"))
    breakdown = _mapping(results.get("coating_breakdown"))
    densities = _mapping(results.get("current_densities_A_m2"))
    series: dict[str, list[float]] = {}
    for name in _zone_names(results):
        row = _mapping(demand.get(name))
        fc = _mapping(breakdown.get(name))
        density = float(_mapping(densities.get(name)).get("i_mean_A_m2", 0.0))
        area = float(row.get("area_m2", 0.0))
        if row.get("area_basis") == "reinforcement_steel":
            series[name] = [float(row.get("I_mean_A", 0.0))] * 2
        else:
            series[name] = [
                kernel.current_demand(
                    area,
                    density,
                    kernel.coating_breakdown_linear(
                        float(fc.get("a", 1.0)), float(fc.get("b_per_yr", 0.0)), year
                    ),
                )
                for year in (0.0, design_life)
            ]
    return series


def _demand_rows(results: Mapping[str, Any]) -> list[list[Any]]:
    demand = _mapping(results.get("current_demand_A"))
    rows: list[list[Any]] = []
    for name in _zone_names(results):
        row = _mapping(demand.get(name))
        rows.append(
            [
                name,
                str(row.get("area_basis", "")).replace("_", " "),
                row.get("anode_family", ""),
                row.get("area_m2", ""),
                row.get("I_initial_A", ""),
                row.get("I_mean_A", ""),
                row.get("I_final_A", ""),
            ]
        )
    rows.append(
        [
            "Total",
            "-",
            "-",
            _mapping(results.get("surface_areas_m2")).get("total_m2", ""),
            demand.get("total_initial_A", ""),
            demand.get("total_mean_A", ""),
            demand.get("total_final_A", ""),
        ]
    )
    return rows


def _current_demand(results: Mapping[str, Any]) -> Section:
    rows = _demand_rows(results)
    life = float(results.get("design_life_years", 0.0))
    return Section(
        key="current-demand",
        title="Current demand",
        blocks=[
            TableBlock(
                title="Current density, area basis and demand by zone",
                columns=[
                    "Zone",
                    "Area basis",
                    "Family",
                    "Area",
                    "Initial",
                    "Mean",
                    "Final",
                ],
                units=["-", "-", "-", "m2", "A", "A", "A"],
                rows=rows,
                source="cfg[results][current_demand_A]",
            ),
            FigureBlock(
                title="Maintenance current demand over the design life",
                caption="Concrete reinforcement demand is constant because Table x-3 is mean-only.",
                figure_id="fig-cp-demand-vs-time",
                plotly=figure_from_columns(
                    "line",
                    [0.0, life],
                    _demand_series(results, life),
                    title="Current demand vs time",
                    x_label="years",
                    y_label="current demand (A)",
                ),
            ),
        ],
    )


def _family_rows(results: Mapping[str, Any]) -> tuple[list[list[Any]], list[list[Any]]]:
    families = _mapping(results.get("anode_families"))
    mass_rows = []
    output_rows = []
    for name, raw in families.items():
        row = _mapping(raw)
        checks = _mapping(row.get("checks"))
        mass_rows.append(
            [
                name,
                row.get("material", ""),
                row.get("environment", ""),
                row.get("anode_surface_temperature_C", ""),
                row.get("capacity_Ah_kg", ""),
                row.get("closed_circuit_potential_V", ""),
                row.get("electrolyte_resistivity_ohm_m", ""),
                row.get("utilization_factor", ""),
                row.get("required_mass_kg", ""),
                row.get("individual_mass_kg", ""),
                row.get("count_by_mass", ""),
                row.get("recommended_anode_count", ""),
                row.get("anode_count", ""),
                row.get("count_source", ""),
                checks.get("mass", ""),
                row.get("governing_case", ""),
            ]
        )
        output_rows.append(
            [
                name,
                _mapping(row.get("current_demand_A")).get("initial", ""),
                row.get("resistance_initial_ohm", ""),
                row.get("current_output_initial_A", ""),
                row.get("total_output_initial_A", ""),
                row.get("count_by_initial_current", ""),
                checks.get("initial", ""),
                _mapping(row.get("current_demand_A")).get("final", ""),
                row.get("resistance_final_ohm", ""),
                row.get("current_output_final_A", ""),
                row.get("total_output_final_A", ""),
                row.get("count_by_final_current", ""),
                checks.get("final", ""),
            ]
        )
    return mass_rows, output_rows


def _mass_table(rows: list[list[Any]]) -> TableBlock:
    return TableBlock(
        title="Anode family mass and count checks",
        columns=[
            "Family",
            "Material",
            "Environment",
            "Temperature",
            "Capacity",
            "Potential",
            "Resistivity",
            "u",
            "Mass",
            "Unit mass",
            "N mass",
            "N recommended",
            "N checked",
            "Count source",
            "Mass OK",
            "Governing",
        ],
        units=[
            "-",
            "-",
            "-",
            "degC",
            "Ah/kg",
            "V",
            "ohm.m",
            "-",
            "kg",
            "kg",
            "-",
            "-",
            "-",
            "-",
            "-",
            "-",
        ],
        rows=rows,
        source="cfg[results][anode_families]",
    )


def _output_table(rows: list[list[Any]]) -> TableBlock:
    return TableBlock(
        title="Anode family current-output checks",
        columns=[
            "Family",
            "Initial demand",
            "R initial",
            "Ia initial",
            "Total initial output",
            "N initial",
            "Initial OK",
            "Final demand",
            "R final",
            "Ia final",
            "Total final output",
            "N final",
            "Final OK",
        ],
        units=[
            "-",
            "A",
            "ohm",
            "A",
            "A",
            "-",
            "-",
            "A",
            "ohm",
            "A",
            "A",
            "-",
            "-",
        ],
        rows=rows,
        source="cfg[results][anode_families]",
    )


def _family_requirements(results: Mapping[str, Any]) -> Section:
    mass_rows, output_rows = _family_rows(results)
    return Section(
        key="anode-requirements",
        title="Anode requirements by family",
        blocks=[_mass_table(mass_rows), _output_table(output_rows)],
    )


def _adequacy(results: Mapping[str, Any]) -> Section:
    from digitalmodel.cathodic_protection.report_adapters import (
        _status_block,
        _use_status_block,
    )

    status = _mapping(results.get("status"))
    families = _mapping(results.get("anode_families"))
    return Section(
        key="adequacy",
        title="Adequacy",
        blocks=[
            _use_status_block(status),
            _status_block(status, "Anode family design adequacy"),
            TableBlock(
                title="Family governing adequacy ratios",
                columns=["Family", "Case", "Ratio", "Result"],
                rows=[
                    [
                        name,
                        row.get("governing_case"),
                        row.get("governing_ratio"),
                        "PASS" if all(_mapping(row.get("checks")).values()) else "FAIL",
                    ]
                    for name, row in families.items()
                ],
                source="cfg[results][anode_families]",
            ),
        ],
    )


def _citation_usage(results: Mapping[str, Any]) -> list[tuple[str, list[Any]]]:
    families = _mapping(results.get("anode_families"))
    usage: list[tuple[str, list[Any]]] = [
        (f"{name}: electrochemistry and utilization", row.get("citations") or [])
        for name, row in families.items()
    ]
    densities = _mapping(results.get("current_densities_A_m2"))
    breakdown = _mapping(results.get("coating_breakdown"))
    for name in _zone_names(results):
        usage.append(
            (
                f"{name}: design current density",
                _mapping(densities.get(name)).get("citations") or [],
            )
        )
        usage.append(
            (
                f"{name}: coating breakdown",
                _mapping(breakdown.get(name)).get("citations") or [],
            )
        )
    return usage


def build_b401_family_sections(
    inputs: Mapping[str, Any], results: Mapping[str, Any]
) -> list[Section]:
    """Build the five standard anode-design sections for family mode."""
    del inputs
    from digitalmodel.cathodic_protection.report_adapters import _references_section

    return [
        _design_basis(results),
        _current_demand(results),
        _family_requirements(results),
        _adequacy(results),
        _references_section(results, _citation_usage(results)),
    ]


__all__ = ["build_b401_family_sections"]
