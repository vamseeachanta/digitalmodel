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


def measured_failure(inputs: Mapping[str, Any]) -> bool:
    """Return whether any level fails at its latest reported survey."""
    raw = inputs.get("structure", {}).get("report_geometry", {})
    latest: dict[float, Mapping[str, Any]] = {}
    for r in raw.get("monitoring", []):
        level = float(r["elevation_m"])
        if level not in latest or r["elapsed_days"] > latest[level]["elapsed_days"]:
            latest[level] = r
    return any(float(r["potential_V"]) >= float(raw["criterion_V"]) for r in latest.values())


def _quantity_rows(demands: list[float], hours: float, results: Mapping[str, Any],
                   *, retrofit: bool) -> list[list[Any]]:
    """Size and recheck every phase and usable mass for both output bases."""
    req = results["anode_requirements"]
    ver = results["current_output_verification"]
    mass = float(req["individual_mass_kg"])
    required = demands[1] * hours / (float(req["electrochemical_capacity_Ah_kg"]) *
                                    float(req["utilization_factor"]))
    rows: list[list[Any]] = [["Required net mass", f"{required:.0f}", "kg",
                              "[2], anode mass; mean current × hours / (capacity × utilisation)"]]
    outputs = [("Published outputs", float(results["published_initial_output"]),
                float(results["published_final_output"])),
               ("Independent estimate", float(ver["anode_current_output_initial_A"]),
                float(ver["anode_current_output_final_A"]))]
    for label, initial, final in outputs:
        counts = [math.ceil(required / mass), math.ceil(demands[0] / initial), math.ceil(demands[2] / final)]
        count = max(counts)
        governing = ["mass", "initial current", "final current"][counts.index(count)]
        source = "[1], p. 242, Section 2" if label == "Published outputs" else "[2], anode resistance"
        if retrofit:
            first = counts[2]
            rows.extend([[label+" final-only count", first, "anodes", f"ceil(final addition / {final:.4f} A)"],
                         [label+" final-only final capacity", f"{first*final:.3f}", "A", source],
                         [label+" final-only net mass", f"{first*mass:.0f}", "kg", source],
                         [label+" final-only initial capacity", f"{first*initial:.3f}", "A", source],
                         [label+" final-only final check", "PASS" if first*final >= demands[2] else "FAIL", "-", source],
                         [label+" final-only mass check", "PASS" if first*mass >= required else "FAIL", "-", source],
                         [label+" final-only initial check", "PASS" if first*initial >= demands[0] else "FAIL", "-", source]])
        rows.extend([[label+(" all-check count" if retrofit else " count"), count, "anodes",
                      f"max(ceil({required:.0f} / {mass:.0f}), ceil({demands[0]:.3f} / {initial:.4f}), ceil({demands[2]:.3f} / {final:.4f})); unrounded values used"],
                     [label+" governing check", governing, "-", source],
                     [label+" initial capacity", f"{count*initial:.3f}", "A", source],
                     [label+" final capacity", f"{count*final:.3f}", "A", source],
                     [label+" net mass", f"{count*mass:.0f}", "kg", source],
                     [label+(" all-check recheck" if retrofit else " recheck"),
                      "PASS" if count*mass >= required and count*initial >= demands[0] and count*final >= demands[2] else "FAIL", "-", source]])
    return rows


def measured_options(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> list[Any]:
    """Size options from cited density increases without asserting local protection."""
    from digitalmodel.reporting.spec import TextBlock

    if not measured_failure(inputs):
        return []
    structure = inputs["structure"]
    basis = structure["verification_evidence"]["higher_density_basis"]
    targets = published_outputs(inputs)
    view = dict(results, published_initial_output=targets["initial_output"]["printed"],
                published_final_output=targets["final_output"]["printed"])
    demands = [float(results["connected_current_allowance_A"])] * 3
    addition = []
    for zone in structure["zones"]:
        densities = basis["zones"].get(zone["zone"], zone["effective_current_density_A_m2"])
        for i, phase in enumerate(("initial", "mean", "final")):
            demands[i] += float(zone["area_m2"]) * float(densities[phase])
        if zone["zone"] == basis["affected_zone"]:
            addition = [float(zone["area_m2"]) * (float(densities[p]) - float(zone["effective_current_density_A_m2"][p]))
                        for p in ("initial", "mean", "final")]
    if len(addition) != 3 or min(addition) <= 0:
        raise ValueError("positive zone additions require cited area and phase densities")
    life = float(results["design_life_years"])
    elapsed = max(float(r["elapsed_days"]) for r in structure["report_geometry"]["monitoring"])
    remaining = life - elapsed / 365
    if remaining <= 0:
        raise ValueError("remaining life must be established at the latest reading")
    locator = f"[1], {basis['locator']}; areas: p. 242, Table 1"
    design_rows = [[phase+" demand", f"{value:.3f}", "A", locator] for phase,value in zip(("Initial","Mean","Final"),demands)]
    design_rows += _quantity_rows(demands, life*8760, view, retrofit=False)
    zone_rows = [[phase+" addition", f"{value:.3f}", "A", locator+"; area × density difference"] for phase,value in zip(("Initial","Mean","Final"),addition)]
    zone_rows += [[f"Remaining life at day {elapsed:g}", f"{remaining:.4f}", "yr",
                   f"{life:g} − {elapsed:g}/365; remaining life at that reading"]]
    zone_rows += _quantity_rows(addition,remaining*8760,view,retrofit=True)
    return [TextBlock(markdown="### Increase at design\n\nThe higher seawater bare-steel densities identified by the paper are used to recheck initial design [1], p. 244, Table 3. Other density rows retain Table 2 values [1], p. 242. Count is the maximum of mass, initial-output and final-output requirements. The arithmetic establishes aggregate capacity, not potentials. Member geometry, distribution and electrical continuity are needed to verify local protection."),
            TableBlock(title="Increase at design: demand and recheck",columns=["Quantity","Value","Unit","Source or arithmetic"],rows=design_rows),
            TextBlock(markdown="### Retrofit\n\nThe 30 m to mudline bare zone includes both failing levels. Additional phase current equals its published area multiplied by the density increment [1], pp. 242 and 244, Tables 1–3. End-of-life output first sizes the addition, then initial output and remaining-life mass are rechecked. The final-only count fails the initial and mass rechecks; the all-check count below passes these additional-load checks. The paper's authors propose clamped anodes at -70.5 m and -84.1 m [1], p. 244, Section 5; installation is not established. Added anodes belong in this zone. The split is not evaluated: per-level anode distribution and member geometry are required. A potential more positive than -0.800 V triggers intervention planning; this trigger is met at both lowest levels at day 518. A date is not evaluated: current readings, measured depletion of the lower anodes, corrosion allowance and wall-thickness evidence are needed. The addition does not establish total zone adequacy or measured protection without the existing allocation and post-intervention readings."),
            TableBlock(title="Retrofit: lower-zone addition and recheck",columns=["Quantity","Value","Unit","Source or arithmetic"],rows=zone_rows),
            TextBlock(markdown="### Monitoring\n\nAll levels and remote or shielded locations require potential surveys versus Ag/AgCl/seawater; protection requires more negative than -0.800 V [2], protective potential criterion. Readings more positive than -0.800 V trigger investigation and intervention planning. Surveys before and after intervention should use the same locations. Remaining mass and depletion should be measured against remaining life. The recurring interval is not evaluated: current surveys, depletion history, corrosion allowance and affected-member condition are needed. Monitoring alone is not a demonstrated correction of the measured failure; retrofit assessment is recommended because the trigger is already met.")]


def verification_summary(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> str:
    """Open verification reports with result, finding, advice and limitations."""
    ver = results["current_output_verification"]
    if published_outputs(inputs):
        statement = published_status(inputs, results).detail
        targets = published_outputs(inputs)
        difference = max(abs(100 * (float(results["current_demand_A"][f"total_{phase}_A"]) / float(targets[f"{phase}_demand"]["printed"]) - 1)) for phase in ("initial", "mean", "final"))
        bound = math.ceil(difference * 1000) / 1000
        finding = ("Retrofit assessment of the affected lower zone is recommended because "
                   "the monitoring trigger is met; design, retrofit and monitoring options "
                   "and their rechecks are in Adequacy. " if measured_failure(inputs) else
                   "No measured failure is established by the latest readings. ")
        return (f"The published {ver['anode_count']}-anode arrangement is verified against "
                "the paper's printed outputs and the measured potentials. " + statement +
                " Computed initial, mean and final demands reproduce the printed totals "
                f"within {bound:.3f}%. " + finding +
                "Aggregate checks do not establish spatial protection; geometry, allocation, "
                "remaining condition and timing limits are stated in Appendix A. "
                "Published values and locators are in Published basis and comparison.")
    status = results["status"]
    governing = str(status["governing_case"]) + (" current" if status["governing_case"] != "mass" else "")
    return (f"The {ver['anode_count']}-anode arrangement is verified against computed mass "
            f"and initial and final current demands: {status['result']}; "
            f"{governing} governs. "
            + ("Rechecked design alternatives and monitoring requirements are in Adequacy. "
               if status["result"] == "FAIL" else "No aggregate check fails. ")
            + monitoring_conclusion(inputs) +
            " The elevation is illustrative; geometry and condition limitations are in Appendix A.")
