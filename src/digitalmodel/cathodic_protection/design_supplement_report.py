# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Report sections for opt-in CP distribution and deposition sensitivities."""

from collections.abc import Mapping
from typing import Any

from digitalmodel.reporting import Section, TableBlock, TextBlock

from digitalmodel.cathodic_protection.calcareous_demand import PHASES


def _number(value: float | None) -> str:
    return "Unknown" if value is None else f"{value:.1f}"


def _distribution_section(screen: Mapping[str, Any]) -> Section:
    rows = []
    point_rows = []
    for name, zone in screen["zones"].items():
        rows.append([
            name, zone["status"], _number(zone["reach_m"]),
            _number(zone["max_spacing_m"]), _number(zone["spacing_limit_m"]),
            _number(zone["spacing_margin_m"]), _number(zone["reach_margin_m"]),
            _number(zone["margin_m"]), "; ".join(zone["reasons"]) or "Limits met",
        ])
        for index, point in enumerate(zone["points"]):
            point_rows.append([
                name, index, ", ".join(_number(v) for v in point["coordinates_m"]),
                point["nearest_anode_index"], _number(point["distance_m"]),
                _number(point["margin_m"]),
            ])
    return Section(key="current-distribution", title="Current-distribution screen", blocks=[
        TextBlock(markdown=(
            f"{screen['reach_basis']} Zone reach inputs override the global reach. "
            f"Adjacent spacing: {screen['spacing_method']}; limit = twice the zone reach. "
            "Margins are limit minus measured distance; the zone margin is the smaller "
            "of spacing and reach margins. Verdicts use unrounded values. "
            "Missing positions or governing points produce FLAG and an unknown margin. "
            "Only supplied governing points and zone-local anodes are screened. "
            "A PASS does not establish protection: shielding, obstructions, "
            "electrolyte paths, potential and electrical continuity require separate "
            "engineering assessment and field measurements. For FLAG, review remote "
            "points, add or reposition anodes, confirm reach and repeat the screen."
        )),
        TableBlock(title="Zone reach and spacing", columns=[
            "Zone", "Screen", "Reach", "Adjacent spacing", "Spacing limit",
            "Spacing margin", "Reach margin", "Zone margin", "Reason",
        ], units=["-", "-", "m", "m", "m", "m", "m", "m", "-"], rows=rows),
        TableBlock(title="Governing points and nearest anodes", columns=[
            "Zone", "Point index", "Coordinates", "Nearest anode index", "Distance", "Reach margin",
        ], units=["-", "-", "m", "-", "m", "m"], rows=point_rows),
    ])


def _calcareous_section(result: Mapping[str, Any]) -> Section:
    rows = []
    for name, zone in result["zones"].items():
        for phase in PHASES:
            reference = zone["without_calcareous"]
            reduced = zone["with_calcareous"]
            rows.append([
                name, phase, f"{100 * zone['phase_factors'][phase]:.1f}",
                _number(1000 * reference["densities_A_m2"][phase]),
                _number(1000 * reduced["densities_A_m2"][phase]),
                _number(reference["demands_A"][phase]),
                _number(reduced["demands_A"][phase]),
            ])
    summary = []
    for key, label in (("without_calcareous", "Without calcareous"),
                       ("with_calcareous", "With calcareous")):
        case = result[key]
        summary.append([label, *[_number(case["demands_A"][p]) for p in PHASES],
                        _number(case["total_mass_kg"]), case["count_by_mass"]])
    return Section(key="calcareous-demand", title="Calcareous demand phases", blocks=[
        TextBlock(markdown=(
            "Sensitivity comparison beside the existing design results. Without calcareous "
            "means no additional reduction of the existing density basis; it is not "
            "an independently established deposit-free steel case. Existing phase densities "
            "may already include deposits: avoid double counting. The default factor is "
            "1.0 (no additional credit); user to confirm against the governing code. "
            "Factors multiply each phase density before coating breakdown and apply "
            "only to seawater-exposed submerged and tidal zones; buried and "
            "concrete-embedded zones retain their reference densities. The mean factor "
            "is the effective lifetime-average demand multiplier, supplied by the engineer; "
            "it is not interpolated from initial and final values. "
            f"Mass = mean demand × {_number(result['design_life_years'])} years × 8760 "
            "hours/year / (capacity × utilisation), using the existing design inputs. "
            f"Connected allowance of {_number(result['connected_current_A'])} A is unchanged "
            "in every phase. These are comparative net masses and mass-only counts. "
            "Existing design results and adequacy verdicts retain their reference basis. "
            "Before adopting a reduction, confirm deposition evidence and recheck initial "
            "and final anode output, installed count and spatial coverage."
        )),
        TableBlock(title="Phase density and current demand comparison", columns=[
            "Zone", "Phase", "Density retained", "Without calcareous density",
            "With calcareous density", "Without calcareous demand", "With calcareous demand",
        ], units=["-", "-", "%", "mA/m²", "mA/m²", "A", "A"], rows=rows),
        TableBlock(title="Lifetime demand and anode mass comparison", columns=[
            "Case", "Initial demand", "Mean demand", "Final demand", "Net anode mass", "Mass-only count",
        ], units=["-", "A", "A", "A", "kg", "-"], rows=summary),
    ])


def design_supplement_sections(results: Mapping[str, Any]) -> list[Section]:
    """Return optional sections, including a manual continuity checklist."""
    sections = []
    if "current_distribution" in results:
        sections.append(_distribution_section(results["current_distribution"]))
    if "calcareous" in results:
        sections.append(_calcareous_section(results["calcareous"]))
    if sections:
        sections.append(Section(key="electrical-continuity", title="Electrical continuity", blocks=[
            TextBlock(markdown=(
                "Engineer-completed checklist. No computed continuity verdict is assigned. "
                "Record evidence, acceptance criteria, date and responsible engineer for "
                "each item; resolve open items before relying on the CP design."
            )),
            TableBlock(title="Electrical-continuity checklist", columns=[
                "Item", "Evidence / criteria / date / engineer", "Disposition",
            ], rows=[
                ["Bonding of appurtenances", "________________", "Engineer to complete"],
                ["Anode connections", "________________", "Engineer to complete"],
                ["Isolation joints", "________________", "Engineer to complete"],
            ]),
        ]))
    return sections
