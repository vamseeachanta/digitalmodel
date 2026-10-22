# ABOUTME: Uniform FFS query router + indexed lookup table — routes pipe vs
# ABOUTME: plate queries to the right validated engine; O(1) point lookup.
"""Indexed FFS lookup + uniform query layer.

Two things:

1. A **uniform query router** ``evaluate_query(domain, **params)`` that sends a
   point query to the correct validated engine and returns one normalised
   record — so a caller never has to know which module
   computes a corroded pipe vs a corroded plate.
2. An **indexed lookup** (``build_lookup`` / ``write_lookup`` / ``query_index``)
   with the same shape as the buckling ``results.json``: ``meta`` + ``lookup``
   (full records) + ``index`` (O(1) signature -> verdict/metric), plus a
   ``cases.csv`` manifest.

Domains:
- ``pipe_corroded_strength`` — B31G / Modified B31G / RSTRENG / DNV-RP-F101
  remaining strength for a single defect (depth, length).
- ``plate_metal_loss`` — buckling FFS at a reduced plate thickness.

No physics here — every value comes from a golden-tested module. The
acceptance datum for pipe queries is a Barlow MAOP at the B31.8 class-1 design
factor (0.72), so methods are compared on the same basis.
"""

from __future__ import annotations

import json
import math
from numbers import Integral, Real
from pathlib import Path
from typing import Any, Optional

import pandas as pd

from .corroded_pipe import (
    SMYS_PSI,
    b31g_original,
    modified_b31g,
    rstreng_effective_area,
)
from .dnv_rp_f101 import SMTS_PSI, dnv_f101_single_defect
from ..structural.structural_analysis.models import MARINE_GRADES, PlateGeometry
from ..structural.structural_analysis.plate_metal_loss_ffs import (
    assess_plate_uniform_loss,
)

# Barlow MAOP design factor (ASME B31.8 location class 1, hoop-controlled).
_DESIGN_FACTOR = 0.72

# Fields (in order) that uniquely identify a scenario in each domain.
_KEY_FIELDS = {
    "pipe_corroded_strength": [
        "method", "grade", "D_in", "t_in", "d_in", "L_in", "maop_psi", "safety_factor",
    ],
    "plate_metal_loss": [
        "grade", "length_mm", "width_mm", "thickness_mm",
        "sigma_x_mpa", "metal_loss_mm", "fca_mm", "gamma_m",
    ],
}


def _num(v: Any) -> Any:
    if isinstance(v, Real) and not isinstance(v, bool):
        if not math.isfinite(v):
            raise ValueError("lookup inputs must be finite")
        # Python's shortest round-trip representation retains all input precision.
        if isinstance(v, Integral):
            return int(v)
        return int(v) if float(v).is_integer() else float(v)
    return v


def signature(domain: str, inputs: dict) -> str:
    """Deterministic O(1) lookup key for a scenario."""
    inputs = dict(inputs)
    if domain == "pipe_corroded_strength":
        m = inputs["method"].lower().replace("-", "_").replace(" ", "_")
        inputs["method"] = {
            "modified": "modified_b31g", "mod_b31g": "modified_b31g",
            "b31g_original": "b31g", "dnv": "dnv_f101", "dnv_rp_f101": "dnv_f101",
        }.get(m, m)
        if inputs.get("maop_psi") is None:
            inputs["maop_psi"] = barlow_maop_psi(inputs["D_in"], inputs["t_in"], SMYS_PSI[inputs["grade"]])
        inputs.setdefault("safety_factor", 1.39)
    elif domain == "plate_metal_loss":
        inputs.setdefault("fca_mm", 0.0)
        inputs.setdefault("gamma_m", 1.15)
    fields = _KEY_FIELDS[domain]
    return domain + "|" + json.dumps([_num(inputs[f]) for f in fields],
                                     ensure_ascii=True, allow_nan=False, separators=(",", ":"))


def _record(domain, inputs, metric, metric_name, acceptable, governing,
            extra=None, code_reference=""):
    inputs = {k: _num(v) for k, v in inputs.items()}
    rec = {
        "domain": domain,
        "inputs": inputs,
        "metric": round(float(metric), 4),
        "metric_name": metric_name,
        "acceptable": (None if acceptable is None else bool(acceptable)),
        "verdict": (
            "UNKNOWN" if acceptable is None
            else ("ACCEPTABLE" if acceptable else "UNACCEPTABLE")
        ),
        "governing": governing,
        "code_reference": code_reference,
        "key": signature(domain, inputs),
    }
    if extra:
        rec.update(extra)
    return rec


# ---------------------------------------------------------------------------
# Domain handlers
# ---------------------------------------------------------------------------
def barlow_maop_psi(D_in: float, t_in: float, smys_psi: float) -> float:
    """Design MAOP = design_factor * 2*SMYS*t/D (Barlow)."""
    return _DESIGN_FACTOR * 2.0 * smys_psi * t_in / D_in


def evaluate_pipe_corroded(
    *, method: str, grade: str, D_in: float, t_in: float, d_in: float,
    L_in: float, maop_psi: Optional[float] = None, safety_factor: float = 1.39,
) -> dict:
    """Corroded-pipe remaining strength via the selected method."""
    smys = SMYS_PSI[grade]
    if maop_psi is None:
        maop_psi = barlow_maop_psi(D_in, t_in, smys)
    m = method.lower().replace("-", "_").replace(" ", "_")
    if m in ("b31g", "b31g_original"):
        r = b31g_original(D_in, t_in, d_in, L_in, smys,
                          maop_psi=maop_psi, safety_factor=safety_factor)
        metric, mname, accept = r.safe_pressure_psi, "safe_pressure_psi", r.acceptable
    elif m in ("modified_b31g", "mod_b31g", "modified"):
        r = modified_b31g(D_in, t_in, d_in, L_in, smys,
                         maop_psi=maop_psi, safety_factor=safety_factor)
        metric, mname, accept = r.safe_pressure_psi, "safe_pressure_psi", r.acceptable
    elif m == "rstreng":
        r = rstreng_effective_area(D_in, t_in, [0.0, L_in], [d_in, d_in], smys,
                                   maop_psi=maop_psi, safety_factor=safety_factor)
        metric, mname, accept = r.safe_pressure_psi, "safe_pressure_psi", r.acceptable
    elif m in ("dnv_f101", "dnv_rp_f101", "dnv"):
        r = dnv_f101_single_defect(D_in, t_in, d_in, L_in, SMTS_PSI[grade],
                                   maop_psi=maop_psi)
        metric, mname, accept = r.allowable_pressure_psi, "allowable_pressure_psi", r.acceptable
    else:
        raise ValueError(
            f"unknown pipe method '{method}'. "
            "Use b31g / modified_b31g / rstreng / dnv_f101."
        )
    return _record(
        "pipe_corroded_strength",
        {"method": m, "grade": grade, "D_in": D_in, "t_in": t_in,
         "d_in": d_in, "L_in": L_in, "maop_psi": maop_psi,
         "safety_factor": safety_factor},
        metric, mname, accept, governing=m,
        extra={"maop_psi": float(maop_psi), "applicability": r.applicability.to_dict()},
        code_reference=r.code_reference,
    )


def evaluate_plate_metal_loss(
    *, grade: str, length_mm: float, width_mm: float, thickness_mm: float,
    sigma_x_mpa: float, metal_loss_mm: float, fca_mm: float = 0.0,
    gamma_m: float = 1.15,
) -> dict:
    """Plate buckling FFS at a reduced thickness."""
    material = MARINE_GRADES[grade]
    geometry = PlateGeometry(length_mm, width_mm, thickness_mm)
    r = assess_plate_uniform_loss(
        geometry, material, metal_loss_mm, fca_mm, sigma_x_mpa, gamma_m=gamma_m,
    )
    return _record(
        "plate_metal_loss",
        {"grade": grade, "length_mm": length_mm, "width_mm": width_mm,
         "thickness_mm": thickness_mm, "sigma_x_mpa": sigma_x_mpa,
         "metal_loss_mm": metal_loss_mm, "fca_mm": fca_mm, "gamma_m": gamma_m},
        r.utilization, "utilization", bool(r.passes), governing="plate_buckling",
        extra={"capacity_retained_frac": round(r.capacity_retained_frac, 4),
               "remaining_thickness_mm": round(r.remaining_thickness_mm, 3)},
        code_reference=r.code_reference,
    )


_DOMAINS = {
    "pipe_corroded_strength": evaluate_pipe_corroded,
    "plate_metal_loss": evaluate_plate_metal_loss,
}


def evaluate_query(domain: str, **params) -> dict:
    """Route a point query to the right engine and return a normalised record."""
    if domain not in _DOMAINS:
        raise ValueError(
            f"unknown domain '{domain}'. Use one of {sorted(_DOMAINS)}."
        )
    return _DOMAINS[domain](**params)


def record_from_assessment(result) -> dict:
    """Normalise an FFSAssessmentResult (coordinator) into a lookup record."""
    eligible = result.applicability.ok and result.sufficiency_status == "SUFFICIENT"
    return {
        "domain": "pipe_metal_loss_grid",
        "inputs": {"component_id": result.component_id,
                   "assessment_type": result.assessment_type},
        "metric": round(float(result.rsf), 4),
        "metric_name": "rsf",
        "acceptable": bool(result.passes) if eligible else None,
        "verdict": result.verdict if eligible else "UNKNOWN",
        "source_verdict": result.verdict,
        "governing": result.assessment_type,
        "code_reference": result.code_reference,
        "key": f"component|{result.component_id}",
        "sufficiency_status": result.sufficiency_status,
        "applicability": result.applicability.to_dict(),
        "remaining_life_yr": result.remaining_life_yr,
    }


# ---------------------------------------------------------------------------
# Lookup table
# ---------------------------------------------------------------------------
def build_lookup(records: list, *, meta_extra: Optional[dict] = None) -> dict:
    """Assemble records into a meta/lookup/index structure."""
    index = {}
    seen = {}
    for record in records:
        key = record["key"]
        if key in seen and seen[key] != record:
            raise ValueError("conflicting duplicate lookup key")
        seen[key] = record
        index[key] = {name: record[name] for name in
                      ("metric", "acceptable", "verdict", "applicability",
                       "sufficiency_status", "source_verdict") if name in record}
    domains = sorted({r["domain"] for r in records})
    meta = {
        "module": "digitalmodel.asset_integrity.ffs_lookup",
        "key_schema_version": 2,
        "standards": ["ASME B31G", "DNV-RP-F101", "DNV-RP-C201"],
        "n_records": len(records),
        "domains": domains,
        "design_factor": _DESIGN_FACTOR,
        "units": {"pipe": "in/psi", "plate": "mm/MPa"},
    }
    if meta_extra:
        meta.update(meta_extra)
    return {"meta": meta, "lookup": records, "index": index}


def query_index(lookup: dict, domain: str, **params) -> Optional[dict]:
    """O(1) cached lookup: build the signature and return the index entry."""
    key = signature(domain, params)
    return lookup["index"].get(key)


def write_lookup(lookup: dict, out_dir) -> dict:
    """Write ffs_results.json + ffs_cases.csv. Returns the output paths."""
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    json_path = out / "ffs_results.json"
    csv_path = out / "ffs_cases.csv"
    json_path.write_text(json.dumps(lookup), encoding="utf-8", newline="\n")

    rows = []
    for r in lookup["lookup"]:
        row = {"domain": r["domain"], **r.get("inputs", {}),
               r["metric_name"]: r["metric"], "verdict": r["verdict"],
               "acceptable": r["acceptable"], "governing": r["governing"]}
        if "applicability" in r:
            row["applicability"] = json.dumps(r["applicability"])
        for name in ("sufficiency_status", "source_verdict"):
            if name in r:
                row[name] = r[name]
        rows.append(row)
    pd.DataFrame(rows).to_csv(csv_path, index=False, encoding="utf-8", lineterminator="\n")
    return {"json": str(json_path), "csv": str(csv_path)}
