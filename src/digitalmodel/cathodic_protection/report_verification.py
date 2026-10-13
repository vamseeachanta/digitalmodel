# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Published-output aggregate checks independent of the resistance estimate."""

from __future__ import annotations

import math
from typing import Any, Mapping

from digitalmodel.cathodic_protection.report_elevation import monitoring_conclusion
from digitalmodel.reporting.spec import StatusBlock, TableBlock


def published_outputs(inputs: Mapping[str, Any]) -> dict[str, Mapping[str, Any]]:
    """Return positive published outputs only when their locators are present."""
    evidence = inputs.get("structure", {}).get("verification_evidence", {})
    targets = {t["quantity"]: t for t in evidence.get("targets", [])}
    needed = ("initial_output", "final_output")
    if not all(k in targets and targets[k].get("locator") for k in needed):
        return {}
    if not all(math.isfinite(float(targets[k]["printed"])) and
               float(targets[k]["printed"]) > 0 for k in needed):
        return {}
    return targets


def aggregate_table(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> TableBlock:
    """Compare installed capacity with demand under both output bases."""
    targets = published_outputs(inputs)
    ver = results["current_output_verification"]
    req = results["anode_requirements"]
    count = ver["anode_count"]
    rows = []
    for phase in ("initial", "final"):
        demand = float(ver[f"{phase}_current_demand_A"])
        target = targets[f"{phase}_demand"]
        capacity = count * float(targets[f"{phase}_output"]["printed"])
        model = float(ver[f"total_anode_current_output_{phase}_A"])
        rows.append([f"{phase.capitalize()} current", f"{demand:.3f}",
                     f"{100 * (demand / float(target['printed']) - 1):+.2f}%",
                     f"{capacity:.3f}", f"{100 * (capacity / demand - 1):+.2f}%",
                     "PASS" if capacity >= demand else "FAIL", f"{model:.3f}",
                     f"{100 * (model / demand - 1):+.2f}%",
                     f"{100 * (model / capacity - 1):+.2f}%",
                     "PASS" if model >= demand else "FAIL", "A",
                     f"[1], {target['locator']}; installed count × individual output"])
    required = float(req["total_mass_kg"])
    installed = count * float(req["individual_mass_kg"])
    margin = f"{100 * (installed / required - 1):+.2f}%"
    result = "PASS" if installed >= required else "FAIL"
    rows.append(["Mass", f"{required:.0f}", "not published", f"{installed:.0f}",
                 margin, result, f"{installed:.0f}", margin, "+0.00%", result, "kg",
                 "[1], p. 242, Section 2; life, capacity and utilisation are declared "
                 "assumptions, not printed in the paper"])
    return TableBlock(
        title="Installed arrangement: published outputs and independent estimate",
        columns=["Check", "Computed demand", "Difference from printed demand",
                 "Capacity using published outputs", "Published margin", "Published result",
                 "Independent estimate capacity", "Independent margin",
                 "Difference from published capacity", "Independent result", "Unit", "Source or note"],
        rows=rows,
    )


def measured_statement(inputs: Mapping[str, Any]) -> str:
    """Bind the latest measured deficiency to level, time and criterion."""
    raw = inputs.get("structure", {}).get("report_geometry", {})
    readings = raw.get("monitoring", [])
    if not readings:
        return monitoring_conclusion(inputs)
    criterion = float(raw["criterion_V"])
    latest: dict[float, Mapping[str, Any]] = {}
    for r in readings:
        level = float(r["elevation_m"])
        if level not in latest or r["elapsed_days"] > latest[level]["elapsed_days"]:
            latest[level] = r
    failing = [r for r in latest.values() if float(r["potential_V"]) >= criterion]
    if not failing:
        return monitoring_conclusion(inputs)
    levels = " and ".join(f"{r['elevation_m']:g} m (day {r['elapsed_days']:g})" for r in failing)
    return (f"Measured under-protection at {levels}: potentials are more positive than "
            f"{criterion:.3f} V versus Ag/AgCl/seawater; protection requires more negative than "
            f"{criterion:.3f} V. This spatial finding is not established by aggregate checks.")


def published_status(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> StatusBlock:
    """State aggregate adequacy separately from measured spatial protection."""
    table = aggregate_table(inputs, results)
    passed = all(row[5] == "PASS" for row in table.rows)
    verdict = "PASS" if passed else "FAIL"
    return StatusBlock(label="Aggregate checks and measured protection", status=verdict,
                       governing_case=None if passed else "aggregate demand",
                       detail=f"Aggregate checks {verdict} using published outputs. " +
                       measured_statement(inputs))
