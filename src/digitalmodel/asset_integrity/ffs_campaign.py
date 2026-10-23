# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline inspection campaign composition; screening, never repair design.

Method and synthetic policy basis: campaign-screening-validation.html, revision 0.
Existing coordinator physics and sufficiency thresholds are reused unchanged.
No API RP 580 or ISO 24817 numerical limits are introduced by this adapter.
"""
from __future__ import annotations

from dataclasses import asdict, fields
from html import escape
import math
from pathlib import Path

import numpy as np

from digitalmodel.asset_integrity.assessment import FFSComponent, assess_component
from digitalmodel.asset_integrity.composite_repair import (
    CompositeRepairParameters,
    RepairContext,
    recommend_from_ffs_result,
)
from digitalmodel.asset_integrity.rbi_screening import (
    RBIParameters,
    RISK_BANDS,
    screen_ffs_result,
)

LIMITATIONS = (
    "Synthetic demonstration; policy values are required inputs, not code limits.",
    "API RP 580 published-case validation: NOT_EVALUATED; qualitative ranking only.",
    "ISO 24817 / ASME PCC-2 category qualification: NOT_EVALUATED; A-E labels "
    "are internal selector labels, not standard repair categories.",
    "No laminate sizing, installation qualification or return-to-service approval.",
    "Coordinator strength and sufficiency practice defaults are inherited unchanged; "
    "edition-matched qualification remains required for engineering use.",
)


def _numbers(value):
    if isinstance(value, dict):
        for item in value.values():
            _numbers(item)
    elif isinstance(value, (list, tuple)):
        for item in value:
            _numbers(item)
    elif isinstance(value, (int, float)) and not isinstance(value, bool):
        if not math.isfinite(value):
            raise ValueError("numeric inputs must be finite")


def _policy(block, key, cls):
    data = block.get(key)
    required = {field.name for field in fields(cls)}
    if not isinstance(data, dict) or set(data) != required:
        raise ValueError(f"{key} requires exactly {sorted(required)}")
    return cls(**data)


def _validate_policies(rbi, repair):
    for edges, count in ((rbi.rsf_margin_edges, 4), (rbi.life_ratio_edges, 4)):
        if len(edges) != count or any(v <= 0 or isinstance(v, bool) for v in edges):
            raise ValueError("RBI proxy edges must be positive")
        if any(a <= b for a, b in zip(edges, edges[1:])):
            raise ValueError("RBI proxy edges must strictly descend")
    if (len(rbi.risk_band_edges) != 3
            or any(type(v) is not int or v <= 0 for v in rbi.risk_band_edges)
            or list(rbi.risk_band_edges) != sorted(set(rbi.risk_band_edges))):
        raise ValueError("RBI risk edges must be increasing positive integers")
    if set(rbi.band_interval_cap_yr) != set(RISK_BANDS):
        raise ValueError("RBI interval caps require every risk band")
    scalars = [rbi.reference_interval_yr, rbi.high_corrosion_rate_mm_yr,
               rbi.code_max_interval_yr, *rbi.band_interval_cap_yr.values(),
               repair.default_temperature_f, repair.default_repair_life_yr,
               repair.reinspection_fraction_of_repair_life]
    if any(isinstance(v, bool) or v <= 0 for v in scalars):
        raise ValueError("policy scalars must be positive numbers")
    for limits in (repair.max_pressure_psi_by_class, repair.max_temperature_f_by_class):
        if any(isinstance(v, bool) or not isinstance(v, (int, float)) for v in limits.values()):
            raise ValueError("repair class limits must be numeric")
    if type(repair.allow_steam_service) is not bool:
        raise ValueError("allow_steam_service must be boolean")


def _component_input(record):
    for key in ("component", "grid", "consequence_class"):
        if key not in record:
            raise ValueError(f"component record requires {key}")
    data = record["component"]
    required = {"component_id", "design_code", "nominal_od_in", "nominal_wt_in",
                "design_pressure_psi", "smys_psi", "corrosion_rate_in_per_yr",
                "fca_in", "rsf_a"}
    if not required <= data.keys():
        raise ValueError(f"component requires {sorted(required)}")
    if not isinstance(data["component_id"], str) or not data["component_id"].strip():
        raise ValueError("component_id must be nonempty text")
    for key in required - {"component_id", "design_code"}:
        value = data[key]
        if isinstance(value, bool) or not isinstance(value, (int, float)) or value < 0:
            raise ValueError(f"invalid component {key}")
        if value == 0 and key not in {"corrosion_rate_in_per_yr", "fca_in"}:
            raise ValueError(f"component {key} must be positive")
    if data["rsf_a"] > 1 or data["nominal_od_in"] <= 2 * data["nominal_wt_in"]:
        raise ValueError("invalid strength threshold or pipe geometry")
    grid = record["grid"]
    if not isinstance(grid, list) or not grid or any(not isinstance(r, list) for r in grid):
        raise ValueError("grid must be a nonempty rectangular numeric array")
    if not grid[0] or any(len(r) != len(grid[0]) for r in grid):
        raise ValueError("grid must be rectangular")
    if any(isinstance(v, bool) or not isinstance(v, (int, float)) or v <= 0
           for row in grid for v in row):
        raise ValueError("grid thicknesses must be positive numbers in inches")
    if record.get("force_type") not in {"GML", "LML"}:
        raise ValueError("force_type must explicitly select GML or LML")
    consequence = record["consequence_class"]
    if not isinstance(consequence, str) or consequence not in "ABCDE" or len(consequence) != 1:
        raise ValueError("consequence_class must be A-E")
    return FFSComponent(**data), np.asarray(grid, dtype=float)


def _repair(result, record, params):
    if result.verdict != "REPAIR":
        return None
    context = record.get("repair_context")
    required = {field.name for field in fields(RepairContext)}
    if not isinstance(context, dict) or set(context) != required:
        return {"status": "NOT_EVALUATED", "candidate_classes": [],
                "reason": "complete repair_context required; no service defaults inferred"}
    flags = ("active_leak", "through_wall_now", "through_wall_within_repair_life", "axial_load")
    if any(type(context[key]) is not bool for key in flags):
        raise ValueError("repair context flags must be boolean")
    for key in ("pressure_psi", "temperature_f", "repair_life_yr"):
        if isinstance(context[key], bool) or not isinstance(context[key], (int, float)):
            raise ValueError(f"repair context {key} must be numeric")
    recommendation = recommend_from_ffs_result(result, context, params=params).to_dict()
    recommendation["standards_validation"] = "NOT_EVALUATED"
    recommendation["class_basis"] = "internal selector labels; not ISO/PCC-2 categories"
    return recommendation


def assess_campaign(block: dict) -> dict:
    """Chain sufficiency, qualitative risk, repair candidates and interval by ID."""
    _numbers(block)
    if not isinstance(block.get("policy_basis"), str) or not block["policy_basis"].strip():
        raise ValueError("policy_basis must identify the supplied policy")
    rbi = _policy(block, "rbi_policy", RBIParameters)
    repair = _policy(block, "repair_policy", CompositeRepairParameters)
    _validate_policies(rbi, repair)
    if not block.get("components"):
        raise ValueError("components must not be empty")
    rows, seen = [], set()
    for record in block["components"]:
        component, grid = _component_input(record)
        if component.component_id in seen:
            raise ValueError(f"duplicate component_id: {component.component_id}")
        seen.add(component.component_id)
        result = assess_component(component, grid, force_type=record["force_type"])
        risk = screen_ffs_result(result, record["consequence_class"],
                                 corrosion_rate_in_per_yr=component.corrosion_rate_in_per_yr,
                                 params=rbi).to_dict()
        interval = risk["recommended_interval_yr"]
        action = "PLANNED_INSPECTION"
        if result.verdict in {"REPAIR", "REPLACE"}:
            interval, action = 0.0, "ENGINEERING_ACTION_NOW"
        elif result.sufficiency_status == "TAKE_MORE":
            interval, action = None, "TAKE_MORE"
        elif result.verdict not in {"ACCEPT", "MONITOR"} or result.sufficiency_status == "ESCALATE":
            interval, action = None, "ENGINEERING_REVIEW"
        sufficiency = asdict(result.sufficiency)
        if result.verdict in {"REPAIR", "REPLACE"}:
            sufficiency["reasons"] = [
                f"{reason} This sufficiency message is superseded by the zero-life verdict "
                f"{result.verdict}; immediate engineering action governs continued use."
                for reason in sufficiency["reasons"]
            ]
        rows.append({"component_id": component.component_id,
                     "assessment": result.to_dict(), "sufficiency": sufficiency,
                     "rbi": risk, "repair": _repair(result, record, repair),
                     "next_inspection_interval_yr": interval, "action": action})
        # Discard the coordinator's implicit repair defaults: only explicit policy is reported.
        rows[-1]["assessment"].pop("repair_recommendation", None)
    # For equal risk, prioritize action severity, shorter life, then stable ID.
    severity = {"ENGINEERING_ACTION_NOW": 0, "ENGINEERING_REVIEW": 1,
                "TAKE_MORE": 2, "PLANNED_INSPECTION": 3}

    def rank_key(row):
        life = row["rbi"]["inspection_plan"]["remaining_life_years"]
        return (-row["rbi"]["risk_score"], severity[row["action"]],
                math.inf if life is None else life, row["component_id"])

    ranking = [{"component_id": row["component_id"],
                "risk_score": row["rbi"]["risk_score"],
                "risk_band": row["rbi"]["risk_band"]}
               for row in sorted(rows, key=rank_key)]
    payload = {"components": rows, "rank_register": ranking,
               "policy_basis": block["policy_basis"],
               "policy": {"rbi": asdict(rbi), "repair": asdict(repair)},
               "limitations": list(LIMITATIONS)}
    payload = _json_finite(payload)
    payload["ffs_report"] = _report(payload)
    return payload


def _json_finite(value):
    if isinstance(value, dict):
        return {key: _json_finite(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_finite(item) for item in value]
    if isinstance(value, float) and not math.isfinite(value):
        return None
    return value


def _report(payload):
    sections = ["<!doctype html><html lang='en'><meta charset='utf-8'>",
                "<title>FFS campaign analysis</title><body><h1>FFS campaign analysis</h1>",
                "<p>Summary: qualitative screening; engineering actions govern continued use.</p>",
                "<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th>"
                "<th>Change and reason</th></tr><tr><td>0</td><td>2026-10-20</td>"
                "<td>Campaign composition added to expose field and engineering actions.</td></tr>"
                "<tr><td>1</td><td>2026-10-22</td><td>Inclusive risk boundaries and action "
                "ranking corrected; named fields and verdict precedence clarify the report.</td></tr>"
                "</table><p>Table 1. Revision history.</p>",
                f"<p>Policy basis: {escape(payload['policy_basis'])}</p>"]
    for row in payload["components"]:
        sections.append(f"<h2>{escape(row['component_id'])}</h2>")
        values = {"Verdict": row["assessment"]["verdict"],
                  "Measurement sufficiency": row["sufficiency"]["status"],
                  "Risk score": row["rbi"]["risk_score"], "Risk band": row["rbi"]["risk_band"],
                  "Next inspection interval (yr)": row["next_inspection_interval_yr"],
                  "Action": row["action"]}
        sections.append("<dl>" + "".join(f"<dt>{escape(k)}</dt><dd>{escape(_display(v))}</dd>"
                                         for k, v in values.items()) + "</dl>")
        for reason in row["sufficiency"]["reasons"]:
            sections.append(f"<p>{escape(reason)}</p>")
        for action in row["sufficiency"]["actions"]:
            sections.append("<h3>Measurement action</h3>" + _named_fields(action))
        if row["repair"]:
            sections.append("<h3>Repair screening</h3>" + _named_fields(row["repair"]))
    sections.append("<h2>RBI rank register</h2><ol>" + "".join(
        f"<li>{escape(row['component_id'])}: {row['risk_score']} ({escape(row['risk_band'])})</li>"
        for row in payload["rank_register"]) + "</ol>")
    sections.append("<h2>Method and limits</h2><p>Existing FFS coordinator, measurement "
                    "sufficiency, RBI screening and inspection-planning engines are composed. "
                    "Remaining life = thickness allowance / corrosion rate; interval is the "
                    "minimum of half life, supplied code cap and supplied risk cap. "
                    "Repair review interval = repair life times supplied review fraction.</p>")
    sections.extend(f"<p>{escape(item)}</p>" for item in payload["limitations"])
    sections.append("</body></html>\n")
    return "\n".join(sections)


def _named_fields(values):
    labels = {"reinspection_interval_yr": "Reinspection interval (yr)"}
    entries = []
    for key, value in values.items():
        label = labels.get(key, key.replace("_", " ").capitalize())
        if isinstance(value, dict):
            rendered = _named_fields(value)
        elif isinstance(value, list):
            rendered = escape(", ".join(_display(item) for item in value) or "None")
        else:
            rendered = escape(_display(value))
        entries.append(f"<dt>{escape(label)}</dt><dd>{rendered}</dd>")
    return "<dl>" + "".join(entries) + "</dl>"


def _display(value):
    if value is None:
        return "NOT_EVALUATED"
    return f"{value:.1f}" if isinstance(value, float) else str(value)


def router(cfg: dict) -> dict:
    """Engine adapter; write a report only to an explicitly requested directory."""
    block = cfg["ffs_campaign"]
    result = assess_campaign(block)
    if block.get("output_dir"):
        output = Path(block["output_dir"])
        output.mkdir(parents=True, exist_ok=True)
        (output / "ffs-campaign.html").write_text(result["ffs_report"], encoding="utf-8")
    cfg["ffs_campaign_result"] = result
    cfg["ffs_campaign"] = {**block, "result": result}
    return cfg
