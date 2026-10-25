# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build source-bound CP step data and narrowly scoped public example reports.

No solver or standards document is opened. Case result bytes remain unchanged.
The reference jacket retains its saved whole-arrangement comparison; it is not
relabelled as a calculation limited to the exterior seawater surfaces shown.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import sys
from html import escape
from pathlib import Path
from typing import Any

import yaml

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT / "src") not in sys.path:
    sys.path.insert(0, str(ROOT / "src"))
from digitalmodel.cathodic_protection.report_schematics import render_schematic

BASE = "d34bda5cf18d11be48b17c7ba94e5c8127af2bf2"
REPORTS = "docs/domains/cathodic_protection/reports"
REFERENCE_INPUT = "tests/fixtures/cathodic_protection/workflow_inputs/reference_jacket_60m_members.yml"
REFERENCE_RESULTS = f"{REPORTS}/reference-jacket/cathodic-protection-analysis.citations.json"
BASIS_FILE = "scripts/cathodic_protection/published-jacket-basis.json"
DESTINATION = ROOT / "docs/api/cathodic-protection"
ASSUMED = "ASSUMED - to be confirmed"
COLUMNS = ["Quantity", "Symbol", "Value", "Unit", "Input class", "Basis", "Source"]
STEPS = [("inputs", "Inputs"), ("criteria", "Criteria"), ("anode-sizing", "Anode sizing"),
         ("distribution", "Distribution"), ("check", "Check"), ("monitoring-plan", "Monitoring plan")]
PURPOSES = ["State geometry, environment, coating and anode inputs.",
            "State the criteria and their recorded edition basis.",
            "Compare lifetime mass and current-output requirements.",
            "State the recorded allocation and positions requiring attention.",
            "Compare the provided arrangement with the saved demand.",
            "Identify survey positions and the inputs needed to set an intervention plan."]
MONITORING_RULE = (
    "When protection criteria are not met or their attainment is uncertain, monitoring is required. "
    "Where monitoring and retrofit are not planned, provide adequate anodes in the initial design."
)
MISSING_PLAN = ["reference electrode and applicable potential criterion", "measured potentials at deep and remote positions",
                "survey access and service schedule", "remaining anode mass and depletion rate",
                "electrical continuity and local current demand", "retrofit access and delivery lead time"]


class Sources:
    """Read each approved source once and retain byte digests and value locators."""

    def __init__(self, root: Path):
        self.root = root
        self.records: list[dict[str, Any]] = []
        self.documents: dict[str, Any] = {}

    def read(self, path: str) -> Any:
        if path not in self.documents:
            content = (self.root / path).read_text(encoding="utf-8")
            self.documents[path] = json.loads(content) if path.endswith(".json") else yaml.safe_load(content)
            self.records.append({"id": len(self.records) + 1, "path": path, "commit": BASE,
                                 "sha256": hashlib.sha256((self.root / path).read_bytes()).hexdigest(),
                                 "availability": "repository source; not a hosted report link"})
        return self.documents[path]

    def operand(self, path: str, *keys: Any) -> dict[str, Any]:
        self.read(path)
        return {"path": path, "keys": list(keys)}

    def value(self, operand: dict[str, Any]) -> Any:
        result = self.read(operand["path"])
        for key in operand["keys"]:
            result = result[key]
        return result

    def refs(self, operands: list[dict[str, Any]]) -> list[int]:
        return sorted({record["id"] for record in self.records
                       if any(record["path"] == operand["path"] for operand in operands)})


def _calculate(values: list[float], operation: str) -> float:
    if not values or any(isinstance(v, bool) or not isinstance(v, (float, int)) or not math.isfinite(v)
                         for v in values):
        raise ValueError("numeric row requires finite source values")
    if operation == "identity":
        return values[0]
    if operation == "product":
        return math.prod(values)
    if operation == "difference":
        return values[0] - values[1]
    if operation == "quotient":
        return values[0] / values[1]
    if operation == "difference_product":
        return (values[0] - values[1]) * values[2]
    if operation == "product_margin_percent":
        return 100 * (math.prod(values[:-1]) / values[-1] - 1)
    if operation == "margin_percent":
        return 100 * (values[0] / values[1] - 1)
    raise ValueError("unsupported source arithmetic")


def numeric(sources: Sources, key: str, quantity: str, symbol: str, unit: str,
            operands: list[dict[str, Any]], *, basis: str, input_class: str,
            operation: str = "identity", precision: int = 1, locator: str = "case data") -> dict[str, Any]:
    """Create a printed value from source operands, without typed result numbers."""
    value = _calculate([sources.value(operand) for operand in operands], operation)
    if not math.isfinite(value):
        raise ValueError("row arithmetic must be finite")
    return {"key": key, "quantity": quantity, "symbol": symbol, "value": f"{value:.{precision}f}".replace("-", "−"),
            "raw_value": value, "precision": precision, "unit": unit, "input_class": input_class,
            "basis": basis, "locator": locator, "source": sources.refs(operands),
            "evidence": {"operation": operation, "operands": operands}}


def text_row(key: str, quantity: str, value: str, *, source: list[int], basis: str,
             input_class: str, unit: str = "not applicable", symbol: str = "not applicable") -> dict[str, Any]:
    return {"key": key, "quantity": quantity, "symbol": symbol, "value": value, "unit": unit,
            "input_class": input_class, "basis": basis, "locator": "case data", "source": source}


def _monitoring(card: dict[str, Any], add_text: Any) -> None:
    positions = {
        "fixed-jacket": ["deepest seawater level", "remote faces and members distant from anodes"],
        "monopile": ["deepest exterior seawater position", "remote circumference away from anodes"],
        "subsea-structure": ["deepest lower frame positions", "remote outer faces and shielded connections"],
        "pipeline": ["remote ends of each spacing cell", "midpoints between bracelets", "deepest route positions"],
    }[card["id"]]
    card["monitoring"] = {"status": "NOT_EVALUATED", "rule": MONITORING_RULE, "positions": positions,
                          "required_inputs": MISSING_PLAN, "frequency": "NOT_EVALUATED", "trigger": "NOT_EVALUATED",
                          "retrofit_quantity": "NOT_EVALUATED", "intervention_date": "NOT_EVALUATED"}
    for key, quantity, value in [
        ("monitor-rule", "Maintainer monitoring and initial-design rule", MONITORING_RULE),
        ("watch", "Potential measurement positions", "; ".join(positions)),
        ("electrode", "Reference electrode", "NOT_EVALUATED: confirm the applicable electrode and potential basis"),
        ("frequency", "Survey frequency", "NOT_EVALUATED: service schedule and access are required"),
        ("trigger", "Trigger potential", "NOT_EVALUATED: qualified criterion and measured potentials are required"),
        ("action", "Action at trigger", "NOT_EVALUATED: investigate; size local intervention after condition and continuity checks"),
        ("date", "Retrofit quantity and required date", "NOT_EVALUATED: local demand, depletion, access and lead time are required"),
        ("up-front", "Initial-design option", "Provide adequate anode mass and output before installation if monitoring and retrofit are not planned"),
        ("missing", "Inputs still needed", "; ".join(MISSING_PLAN)),
    ]:
        add_text("monitoring-plan", key, quantity, value)


def _card(sources: Sources, case: str, identifier: str, title: str) -> dict[str, Any]:
    reference = case == "reference-jacket"
    pipeline = case == "S06"
    input_path = REFERENCE_INPUT if reference else f"{REPORTS}/cases/{case}/input.yml"
    result_path = REFERENCE_RESULTS if reference else f"{REPORTS}/cases/{case}/results.json"
    cfg = sources.read(input_path)["inputs"]
    result = sources.read(result_path)
    if reference and result["input_sha256"] != hashlib.sha256((sources.root / input_path).read_bytes()).hexdigest():
        raise ValueError("reference input digest does not match the saved case evidence")
    input_class = ASSUMED if reference else "synthetic"
    data_basis = ("Saved whole-arrangement comparison; assumed, based on previous experience. "
                  "Published geometry does not qualify the CP assumptions." if reference else "Synthetic case input and saved results")
    standard = "DNVGL-RP-F103 (September 2019, amended May 2021)" if pipeline else "DNV-RP-B401 (May 2021)"
    locators = ([item["section"] for item in result["citations"] if "buried" not in item["section"]]
                if reference else result["citations"])
    clause_titles = {
        "8-1": "Design current densities", "8-2": "Design current densities",
        "8-4": "Coating breakdown factors", "8-6": "Anode materials",
        "6-2": "Design current density", "A-1": "Coating breakdown factors",
        "6.4.2": "Anode utilisation factor", "6-3": "Anode material design values",
    }
    card: dict[str, Any] = {"id": identifier, "selector_anchor": "pipeline-riser" if pipeline else identifier,
            "case": case, "title": title, "status": "ready", "input_class": input_class,
            "standard": standard, "citation_locators": locators, "report": f"cases/{case}.html",
            "release_status": "clearance required before outward release", "steps": [],
            "limits": ("Saved whole-arrangement totals include surfaces outside the exterior-seawater card scope. "
                       "These totals are retained for traceability and are not scoped design acceptance. " if reference else "")
                      + "Aggregate sizing only. Local potentials, allocation, continuity and installed condition are not established."}
    steps: dict[str, dict[str, Any]] = {}
    for (step_id, heading), purpose in zip(STEPS, PURPOSES):
        step: dict[str, Any] = {"id": step_id, "title": heading, "purpose": purpose, "columns": COLUMNS,
                "rows": [], "report": card["report"] + "#" + step_id}
        card["steps"].append(step)
        steps[step_id] = step
    refs = sources.refs([sources.operand(input_path), sources.operand(result_path)])

    def add_text(step: str, key: str, quantity: str, value: str, **kwargs: Any) -> dict[str, Any]:
        row = text_row(key, quantity, value, source=refs, basis=data_basis, input_class=input_class, **kwargs)
        steps[step]["rows"].append(row)
        return row

    def op(*keys: Any, path: str = result_path) -> dict[str, Any]:
        return sources.operand(path, *keys)

    def inp(*keys: Any) -> dict[str, Any]:
        return op("inputs", *keys, path=input_path)

    def add(step: str, key: str, quantity: str, symbol: str, unit: str, *operands: dict[str, Any],
            locator: str | None = None, **kwargs: Any) -> dict[str, Any]:
        title = next((title for fragment, title in clause_titles.items() if locator and fragment in locator), "Case-pack criterion")
        basis = data_basis if locator is None else f"{standard}; {title}; {locator} (as cited by the case pack)"
        row = numeric(sources, key, quantity, symbol, unit, list(operands), basis=basis, input_class=input_class,
                      locator=locator or "case data", **kwargs)
        steps[step]["rows"].append(row)
        return row

    # The input class describes the case basis; standard-derived values retain
    # their exact case-pack locator and are never reselected from a publication.
    add_text("inputs", "structure", "Structure and zones", title + "; exterior seawater surfaces")
    if reference:
        depth = add("inputs", "water-depth", "Water depth", "h", "m",
                    op("dimensions", "water_depth_m", "value", path=input_path))
        depth["input_class"] = "published"
        depth["basis"] = "Published geometry, DOI 10.5194/wes-8-807-2023; fixture dimension locator"
        card["geometry_reference"] = {"doi": "10.5194/wes-8-807-2023", "licence": "CC BY 4.0"}
    add("inputs", "life", "Design life", "t", "yr", inp("design_data", "design_life"))
    if pipeline:
        for key, quantity, unit, precision in [
            ("length_m", "Pipeline length", "m", 1), ("outer_diameter_m", "Outer diameter", "m", 4),
            ("wall_thickness_m", "Wall thickness", "m", 4), ("outer_surface_area_m2", "Surface area", "m²", 1),
        ]:
            add("inputs", key, quantity, key, unit, op("pipeline_geometry_m", key), precision=precision)
        add("inputs", "fluid-temp", "Internal fluid temperature", "T", "°C", inp("pipeline", "internal_fluid_temperature_C"))
        add_text("inputs", "environment", "Seawater temperature and resistivity",
                 "NOT_EVALUATED: not recorded by this case; electrolyte properties require confirmation")
        add_text("inputs", "alloy", "Anode alloy and type", cfg["anode"]["material"] + "; bracelet")
        for key, quantity in [("mean_factor", "Mean coating breakdown"), ("final_factor", "Final coating breakdown")]:
            add("inputs", key, quantity, key, "dimensionless", op("coating_breakdown_factors", key), precision=3, locator=locators[1])
        add_text("inputs", "coating", "Coating category", cfg["pipeline"]["coating_type"])
    else:
        for index, zone in enumerate(cfg["structure"]["zones"]):
            if zone.get("base_zone", zone["zone"]) != "submerged":
                continue
            name = zone["zone"]
            row = add("inputs", "area-" + name, name + " surface area", "A", "m²",
                      inp("structure", "zones", index, "area_m2"))
            if reference:
                row["input_class"] = "published"
            add_text("inputs", "coating-" + name, name + " coating category", zone["coating_category"])
            if not reference:
                for phase in ("initial", "mean", "final"):
                    add("criteria", f"density-{name}-{phase}", f"{name} {phase} current density", f"i_{phase}", "A/m²",
                        op("current_densities_A_m2", name, f"i_{phase}_A_m2"), precision=3, locator=locators[0])
                for phase, field in [("initial", "f_ci"), ("mean", "f_cm"), ("final", "f_cf")]:
                    add("inputs", f"coating-{name}-{phase}", f"{name} {phase} coating breakdown", field, "dimensionless",
                        op("coating_breakdown", name, field), precision=3,
                        locator=next(label for label in locators if "8-4" in label))
        for key, quantity, symbol, unit, precision in [
            ("seawater_temperature_C", "Seawater temperature", "T", "°C", 1),
            ("seawater_resistivity_ohm_m", "Seawater resistivity", "rho", "Ω m", 3),
        ]:
            add("inputs", key, quantity, symbol, unit, inp("environment", key), precision=precision)
        add_text("inputs", "alloy", "Anode alloy and type", cfg["anode"]["material"] + "; " + cfg["anode"]["type"])
    for key, quantity, symbol, unit, precision in [
        ("individual_anode_mass_kg", "Net mass per anode", "m_a", "kg", 1),
        ("length_m", "Anode length", "L_a", "m", 3),
    ]:
        add("inputs", key, quantity, symbol, unit, inp("anode", key), precision=precision)
    dimension_key = "thickness_m" if pipeline else "radius_m"
    add("inputs", dimension_key, "Anode thickness" if pipeline else "Anode radius", "d_a", "m",
        inp("anode", dimension_key), precision=3)
    if reference:
        add_text("inputs", "capacity", "Electrochemical capacity", "NOT_EVALUATED: no value in the saved results sidecar")
        add_text("criteria", "utilisation", "Utilisation factor", "NOT_EVALUATED: no value in the saved results sidecar")
        add_text("criteria", "density", "Design current densities", "NOT_EVALUATED: values are not retained in the saved results sidecar")
        add_text("inputs", "coating-breakdown", "Coating breakdown factors", "NOT_EVALUATED: values are not retained in the saved results sidecar")
    else:
        capacity = "anode_capacity_Ah_kg" if pipeline else "electrochemical_capacity_Ah_kg"
        add("inputs", "capacity", "Electrochemical capacity", "epsilon", "Ah/kg", op("anode_requirements", capacity),
            precision=0, locator=next(label for label in locators if "6-3" in label or "8-6" in label))
        add("criteria", "utilisation", "Utilisation factor", "u", "dimensionless", op("anode_requirements", "utilization_factor"),
            precision=2, locator=next(label for label in locators if "6.4.2" in label) if pipeline else None)
    add_text("criteria", "potential", "Protective potential and reference electrode",
             "NOT_EVALUATED: qualify the applicable potential and reference electrode before a survey criterion is set")
    add_text("criteria", "checks", "Checks applied", "Mass; initial output and final output" if not pipeline else
             "Mass and final output; initial output is not recorded by this route")

    if reference:
        count = op("arrangements", "stated", "anode_count")
        required_count = op("arrangements", "rechecked", "anode_count")
        demands = {phase: op("arrangements", "stated", "checks", index, "required")
                   for phase, index in [("initial", 1), ("final", 2)]}
        mass_need = op("arrangements", "stated", "checks", 0, "required")
        mass_provided = op("arrangements", "stated", "checks", 0, "provided")
        count_ops = {"mass": op("sensitivity", 1, "count_by_mass"), "initial": op("sensitivity", 1, "count_by_initial_current"),
                     "final": op("sensitivity", 1, "count_by_final_current")}
        outputs = {phase: op("arrangements", "stated", "checks", index, "provided")
                   for phase, index in [("initial", 1), ("final", 2)]}
        governing = result["sensitivity"][1]["governing_case"]
        mass_operands, mass_operation = [mass_provided], "identity"
        demand_phases = ("initial", "final")
    elif pipeline:
        count = op("anode_requirements", "anode_count")
        required_count = count
        mass_need = op("anode_requirements", "total_anode_mass_kg")
        mass_operands, mass_operation = [count, inp("anode", "individual_anode_mass_kg")], "product"
        demands = {phase: op("current_demand_A", f"{phase}_current_demand_A") for phase in ("mean", "final")}
        count_ops = {phase: op("anode_requirements", f"anode_count_by_{phase}") for phase in ("mass", "final_current")}
        count_ops["final"] = count_ops.pop("final_current")
        outputs = {}
        governing = result["status"]["governing_case"]
        demand_phases = ("mean", "final")
    else:
        count = op("current_output_verification", "anode_count")
        required_count = op("current_output_verification", "recommended_anode_count")
        demands = {phase: op("current_demand_A", f"total_{phase}_A") for phase in ("initial", "mean", "final")}
        outputs = {phase: op("current_output_verification", f"total_anode_current_output_{phase}_A") for phase in ("initial", "final")}
        mass_need = op("anode_requirements", "total_mass_kg")
        mass_operands, mass_operation = [count, inp("anode", "individual_anode_mass_kg")], "product"
        count_ops = {"mass": op("current_output_verification", "count_by_mass"),
                     "initial": op("current_output_verification", "count_by_initial_current"),
                     "final": op("current_output_verification", "count_by_final_current")}
        governing = result["current_output_verification"]["governing_case"]
        demand_phases = ("initial", "mean", "final")
    for phase in demand_phases:
        add("anode-sizing", "demand-" + phase, phase.capitalize() + " current demand", "I_" + phase, "A", demands[phase])
    if reference:
        add_text("anode-sizing", "mean-demand", "Mean current demand", "NOT_EVALUATED: no total mean-current value in the saved sidecar")
    if pipeline:
        add_text("anode-sizing", "initial-demand", "Initial current demand", "NOT_EVALUATED: this route records mean and final demand")
        add_text("anode-sizing", "initial-output", "Initial output per anode", "NOT_EVALUATED: this route records final output only")
        add_text("anode-sizing", "initial-count", "Anodes needed for initial output", "NOT_EVALUATED: this route records mass and final-output counts")
    for phase in ("initial", "final"):
        if pipeline and phase == "initial":
            continue
        if reference:
            add("anode-sizing", "output-" + phase, phase.capitalize() + " output per anode", "I_a_" + phase, "A",
                outputs[phase], count, operation="quotient", precision=4)
            add_text("anode-sizing", "resistance-" + phase, phase.capitalize() + " anode resistance",
                     "NOT_EVALUATED: no resistance in the saved results sidecar")
        else:
            path = "anode_requirements" if pipeline else "current_output_verification"
            output_key = "anode_current_output_A" if pipeline else f"anode_current_output_{phase}_A"
            resistance_key = "anode_resistance_ohm" if pipeline else f"anode_resistance_{phase}_ohm"
            add("anode-sizing", "output-" + phase, phase.capitalize() + " output per anode", "I_a_" + phase, "A",
                op(path, output_key), precision=4)
            add("anode-sizing", "resistance-" + phase, phase.capitalize() + " anode resistance", "R_a_" + phase, "Ω",
                op(path, resistance_key), precision=6)
    for phase, operand in count_ops.items():
        add("anode-sizing", "count-" + phase, "Anodes needed for " + phase, "N_" + phase, "anodes", operand, precision=0)
    add_text("anode-sizing", "governing", "Governing check", governing)
    add("check", "provided-count", "Anodes provided", "N_p", "anodes", count, precision=0)
    add("check", "needed-count", "Anodes needed", "N_r", "anodes", required_count, precision=0)
    add("check", "mass-provided", "Mass provided", "M_p", "kg", *mass_operands, operation=mass_operation)
    add("check", "mass-needed", "Mass needed", "M_r", "kg", mass_need)
    # Every comparison is formed from unrounded saved values. Display margins
    # follow the reporting convention of one decimal place.
    comparisons = [("mass", mass_operands, mass_operation, mass_need, "kg")]
    for phase in ("initial", "final"):
        if pipeline and phase == "initial":
            continue
        if pipeline:
            output_operands = [count, op("anode_requirements", "anode_current_output_A")]
            operation = "product"
        else:
            output_operands, operation = [outputs[phase]], "identity"
        add("check", "provided-" + phase, phase.capitalize() + " output provided", "I_p_" + phase, "A",
            *output_operands, operation=operation)
        comparisons.append((phase, output_operands, operation, demands[phase], "A"))
    verdicts = []
    for phase, provided_ops, operation, required, unit in comparisons:
        provided = _calculate([sources.value(operand) for operand in provided_ops], operation)
        need = sources.value(required)
        verdicts.append(provided >= need)
        if operation == "product":
            # Keep the provided expression in the evidence, rather than insert
            # a computed constant into the source operands.
            add("check", "margin-" + phase, phase.capitalize() + " margin", "margin", "%",
                      *provided_ops, required, operation="product_margin_percent")
        else:
            add("check", "margin-" + phase, phase.capitalize() + " margin", "margin", "%",
                provided_ops[0], required, operation="margin_percent")
    card["verdict"] = "meets the criterion" if all(verdicts) else "does not meet the criterion"
    add_text("check", "verdict", "Aggregate result", card["verdict"])
    add("distribution", "arrangement-count", "Arrangement anode count", "N_p", "anodes", count, precision=0)
    if pipeline:
        add("distribution", "spacing", "Bracelet spacing", "s", "m", op("anode_spacing_m", "spacing_m"), precision=1)
        add("distribution", "protected-length", "Protected length per side of a bracelet", "L_p", "m",
            op("attenuation_analysis", "protected_length_m"), precision=1)
        add_text("distribution", "positions", "Deepest and remote positions",
                 "NOT_EVALUATED: route elevations and installation end offsets are not recorded; watch cell midpoints and ends")
    else:
        add_text("distribution", "allocation", "Anodes per zone",
                 "NOT_EVALUATED: only an aggregate count is recorded; no zone allocation is inferred")
        if reference:
            add("distribution", "deepest", "Deepest seawater level", "z", "m",
                op("inputs", "structure", "zones", 1, "axis_interval_m", 0, path=input_path))
        else:
            add_text("distribution", "deepest", "Deepest position",
                     "NOT_EVALUATED: elevations and member coordinates are not recorded")
    _monitoring(card, add_text)
    if not all(verdicts):
        add("check", "added-count", "Additional initial-design anodes", "delta_N", "anodes",
            required_count, count, operation="difference", precision=0)
        add("check", "added-mass", "Additional initial-design net mass", "delta_M", "kg",
            required_count, count, inp("anode", "individual_anode_mass_kg"), operation="difference_product")
        card["recommendation"] = {
            "design": "Use the additional initial-design anodes and net mass stated in the check table; recheck the final arrangement.",
            "monitoring": MONITORING_RULE,
            "retrofit": "NOT_EVALUATED: local quantity and required date need potentials, continuity, condition, demand and access evidence."
        }
    else:
        card["recommendation"] = {"design": "Retain the assessed aggregate arrangement subject to the stated limits.",
                                  "monitoring": MONITORING_RULE,
                                  "retrofit": "NOT_EVALUATED: no local quantity or intervention date is established."}
    rows = {row["key"]: row for step in card["steps"] for row in step["rows"]}
    keys = (["mass-provided", "mass-needed", "needed-count"] if reference else
            ["provided-count", "needed-count"] if not pipeline else
            ["provided-count", "spacing", "mass-provided", "mass-needed", "provided-final", "demand-final", "protected-length"])
    card["result_values"] = {key: rows[key]["value"] for key in keys}
    v = card["result_values"]
    if reference:
        card["result_line"] = (f"The trial arrangement provides {v['mass-provided']} kg against {v['mass-needed']} kg needed; "
                               f"{v['needed-count']} anodes are needed and {governing} governs on the saved aggregate basis.")
    elif pipeline:
        card["result_line"] = (f"The {v['provided-count']} bracelets at {v['spacing']} m provide {v['mass-provided']} kg against "
                               f"{v['mass-needed']} kg and {v['provided-final']} A against {v['demand-final']} A; the checks meet "
                               f"the criterion within the recorded {v['protected-length']} m protected length per side.")
    else:
        card["result_line"] = (f"The mass-sized arrangement of {v['provided-count']} anodes {card['verdict']}; "
                               f"{v['needed-count']} anodes are needed and {governing} current governs.")
    for step in card["steps"]:
        step["result_values"] = {row["key"]: row["value"] for row in step["rows"] if "raw_value" in row}
        step["result_line"] = card["result_line"] if step["id"] in {"anode-sizing", "check"} else {
            "inputs": "The stated input classes and missing data apply to this example.",
            "criteria": "Only the recorded aggregate criteria are assessed.",
            "distribution": "Allocation is limited to the quantities recorded in the case data.",
            "monitoring-plan": "NOT_EVALUATED: supply the listed inputs before setting survey criteria and intervention dates."
        }[step["id"]]
    if reference:
        card["schematic"] = {"status": "pending", "reason": "The existing jacket elevation remains in the source analysis; no new geometry is inferred."}
    else:
        geometry = ({"diameter_m": cfg["pipeline"]["outer_diameter_m"], "length_m": cfg["pipeline"]["length_m"],
                     "spacing_m": result["anode_spacing_m"]["spacing_m"], "bracelet_length_m": cfg["anode"]["length_m"],
                     "bracelet_thickness_m": cfg["anode"]["thickness_m"]} if pipeline else
                    {"anode_detail": {key: cfg["anode"][key] for key in ("length_m", "radius_m")}})
        card["schematic"] = {"status": "proportional" if pipeline else "partial", "path": f"schematics/{case}.svg",
                             "kind": "pipeline" if pipeline else "monopile" if case == "S02" else "manifold",
                             "geometry": geometry, "source": refs,
                             "reason": "Nominal spacing view; installation offsets are not recorded." if pipeline else
                                       "Overall dimensions are not recorded; proportional anode detail only."}
    return card


def build_data(root: Path = ROOT, *, include_riser: bool = False) -> dict[str, Any]:
    """Build four day-one cards; hull and riser contain no numbers by default."""
    sources = Sources(root)
    structures = [_card(sources, case, identifier, title) for case, identifier, title in [
        ("reference-jacket", "fixed-jacket", "Reference jacket"), ("S02", "monopile", "Monopile exterior"),
        ("S03", "subsea-structure", "Subsea manifold"), ("S06", "pipeline", "Bracelet pipeline")]]
    structures.append({"id": "hull", "selector_anchor": "hull", "title": "Ship or floating hull",
                       "status": "in preparation", "reason": "No public-eligible analysis is available."})
    riser: dict[str, Any] = {"id": "riser", "selector_anchor": "pipeline-riser", "title": "Riser",
             "status": "in preparation", "reason": "Framing clearance is pending."}
    if include_riser:
        # Opt-in is an unpublished data preview, never a change of clearance.
        # Only seawater family counts are exposed; no other component is copied.
        path = f"{REPORTS}/cases/S08/results.json"
        result = sources.read(path)
        families = result["current_output_verification"]["families"]
        riser = {**riser, "status": "preview", "release_status": "framing clearance required",
                 "family_counts": {name: item["anode_count"] for name, item in families.items()
                                   if item["environment"] == "seawater"},
                 "schematic": {"status": "pending", "reason": "Physical dimensions and coordinates are not recorded."}}
    structures.append(riser)
    published = json.loads((root / BASIS_FILE).read_text(encoding="utf-8"))
    data = {"schema_version": 1, "source_commit": BASE, "title": "Cathodic protection analysis data",
            "summary": "Published geometry and synthetic or assumed CP inputs; screening examples requiring engineering review.",
            "revision_history": [{"revision": "0", "date": "2026-10-25", "change": "Initial step data and public report copies from saved case evidence."}],
            "release_status": "clearance required before outward release", "structures": structures,
            "references": sources.records, "published_jacket_basis": published,
            "published_jacket_basis_note": "Separate published-jacket reproduction; it does not replace the reference-jacket card.",
            "public_report_builder": "Minimal source-bound copies; the broader public-example builder is not on the source commit."}
    return data


def _report(data: dict[str, Any], card: dict[str, Any]) -> str:
    """Render a public engineering copy without private input-document metadata."""
    e = escape
    parts = ['<!doctype html><html lang="en"><head><meta charset="utf-8">',
             '<meta name="viewport" content="width=device-width,initial-scale=1">',
             f'<title>{e(card["title"])} cathodic protection analysis</title>',
             '<style>body{font:1rem/1.6 system-ui,sans-serif;color:#123047;background:#f8fafc;max-width:74rem;margin:auto;padding:1rem}',
             'h1,h2{line-height:1.25}a{color:#087e8b}.table-wrap{overflow-x:auto}table{border-collapse:collapse;width:100%}',
             'th,td{padding:.6rem;text-align:left;border-bottom:1px solid #c5d9e4;vertical-align:top}',
             'th{background:#e8f3f7}section{margin:2rem 0}img{width:100%;height:auto}.notice{border-left:4px solid #087e8b;padding:1rem}',
             '</style></head><body>', f'<h1>{e(card["title"])} cathodic protection analysis</h1>',
             '<section id="summary"><h2>Summary</h2>', f'<p>{e(card["result_line"])}</p>',
             f'<p>{e(data["summary"])}</p><p class="notice">{e(card["release_status"])}</p></section>',
             '<section id="revision-history"><h2>Revision history</h2>',
             '<table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>']
    for revision in data["revision_history"]:
        parts.append('<tr>' + ''.join(f'<td>{e(revision[key])}</td>' for key in ("revision", "date", "change")) + '</tr>')
    parts.append('</table></section>')
    schematic = card["schematic"]
    if "path" in schematic:
        parts.append(f'<figure><img src="../{e(schematic["path"], quote=True)}" alt="{e(schematic["reason"], quote=True)}">'
                     f'<figcaption>{e(schematic["reason"])}</figcaption></figure>')
    for step in card["steps"]:
        parts.extend([f'<section id="{step["id"]}"><h2>{e(step["title"])}</h2>', f'<p>{e(step["purpose"])}</p>',
                      '<div class="table-wrap"><table><thead><tr>',
                      ''.join(f'<th>{e(column)}</th>' for column in COLUMNS), '</tr></thead><tbody>'])
        for row in step["rows"]:
            values = [row["quantity"], row["symbol"], row["value"], row["unit"], row["input_class"], row["basis"]]
            cells = ''.join(f'<td>{e(value)}</td>' for value in values)
            cells += '<td>' + ', '.join(f'<a href="#source-{ref}">[{ref}]</a>' for ref in row["source"]) + '</td>'
            parts.append('<tr>' + cells + '</tr>')
        parts.extend(['</tbody></table></div>', f'<p>{e(step["result_line"])}</p></section>'])
    parts.extend(['<section id="recommendations"><h2>Recommendations</h2>'])
    for phase, message in card["recommendation"].items():
        parts.append(f'<h3>{e(phase.capitalize())}</h3><p>{e(message)}</p>')
    parts.extend(['</section><section id="limits"><h2>Limits</h2>', f'<p>{e(card["limits"])}</p></section>',
                  '<section id="references"><h2>References</h2>', f'<p>{e(card["standard"])}; edition and locators retained from the case pack only.</p>'])
    used_refs = {ref for step in card["steps"] for row in step["rows"] for ref in row["source"]}
    for ref in data["references"]:
        if ref["id"] in used_refs:
            parts.append(f'<p id="source-{ref["id"]}">[{ref["id"]}] <code>{e(ref["path"])}</code>; '
                         f'commit <code>{ref["commit"]}</code>; SHA-256 <code>{ref["sha256"]}</code>. '
                         'Repository source, not a hosted link.</p>')
    parts.append('</section></body></html>\n')
    return '\n'.join(parts)


def write_outputs(data: dict[str, Any], destination: Path = DESTINATION) -> None:
    """Write the JSON, report copies and available case-derived schematics."""
    destination.mkdir(parents=True, exist_ok=True)
    (destination / "front-page.json").write_text(json.dumps(data, indent=2, ensure_ascii=False, allow_nan=False) + "\n",
                                                  encoding="utf-8", newline="\n")
    for card in data["structures"]:
        if card["status"] != "ready":
            continue
        report = destination / card["report"]
        report.parent.mkdir(parents=True, exist_ok=True)
        report.write_text(_report(data, card), encoding="utf-8", newline="\n")
        schematic = card["schematic"]
        if "path" in schematic:
            target = destination / schematic["path"]
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(render_schematic(schematic["kind"], schematic["geometry"]), encoding="utf-8", newline="\n")


def main() -> None:
    """Run a deterministic build, with riser preview disabled unless requested."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=DESTINATION)
    parser.add_argument("--include-riser", action="store_true", help="Include an unpublished seawater-family data preview")
    args = parser.parse_args()
    data = build_data(include_riser=args.include_riser)
    write_outputs(data, args.output)
    print("Built four case reports and source-bound step data; monitoring remains NOT_EVALUATED.")


if __name__ == "__main__":
    main()
