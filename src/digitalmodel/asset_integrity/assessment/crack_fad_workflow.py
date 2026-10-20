# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline plate FAD screening and Level 3 handoff, not service qualification.

Calculation authority remains in :mod:`crack_fad`: BS 7910:2013 Clause 7.3.2
Option 1, Annex P reference stress and the cited 1981 surface-plate K solution.
No standard coefficients or safety factors are introduced by this adapter.
The plate half-width, elastic modulus, measured toughness and numerical search
settings are explicit inputs. There is no inferred Charpy toughness or RSF.
"""

from __future__ import annotations

import json
from dataclasses import asdict
from pathlib import Path

from .crack_fad import assess_crack_like_flaw, critical_flaw_depth_mm
from .ffs_report import FFSReport
from .level3_escalation import escalate_to_level3
from .screening_report import finite_number, store_report

CURVE = "BS 7910:2013 Option 1"
GEOMETRY = ("a_mm", "c_mm", "t_mm", "width_mm")
LOADS = ("sigma_m_mpa", "sigma_b_mpa")
MATERIAL = ("sigma_y_mpa", "sigma_u_mpa", "e_mpa", "k_mat")
SEARCH = ("max_depth_fraction", "tol_mm")
FLAGS = ("interacting_defects", "complex_geometry")
LIMITATIONS = [
    "Published Annex worked-example validation: NOT_EVALUATED. "
    "Only independent arithmetic and workflow regression checks are established.",
    "BS 7910:2013 Option 1 is used; API 579 Part 9 safety factors and "
    "edition-specific service qualification: NOT_EVALUATED.",
    "Flat-plate semi-elliptical surface flaws under tensile membrane and "
    "nonnegative bending only. Width denotes plate half-width; c is half-length.",
    "Residual and secondary stress, weld mismatch, hydrogen, temperature-dependent "
    "toughness, fatigue growth and remaining life: NOT_EVALUATED.",
    "Only the surface and deepest crack-front points are checked; an intermediate "
    "front maximum is NOT_EVALUATED. Flaw interaction and geometry effects need specialist analysis.",
    "Critical depth is the lower end of a numerical bracket at fixed c, width "
    "and loads. Search assumes one crossing; global monotonicity is not established.",
    "SEARCH_BOUND_REACHED denotes a domain cap, not a critical failure depth; "
    "NO_ACCEPTABLE_DEPTH_FOUND means no passing depth at the supplied resolution.",
    "A point inside the FAD envelope does not establish fitness for service. "
    "Every crack case is escalated to the specialist under the existing screening policy.",
]


def _inputs(block):
    """Validate this adapter's explicit plate domain before any calculation."""
    allowed = set(GEOMETRY + LOADS + MATERIAL + SEARCH + FLAGS) | {
        "component_id",
        "geometry",
        "output_dir",
    }
    unknown = set(block) - allowed
    if unknown:
        raise ValueError(f"Unsupported crack inputs: {', '.join(sorted(unknown))}")
    if block.get("geometry") != "plate_surface":
        raise ValueError("geometry must be plate_surface")
    values = {
        key: finite_number(block, key, positive=key != "sigma_b_mpa")
        for key in GEOMETRY + LOADS + MATERIAL + SEARCH
    }
    # Domain from the existing 1981 plate solution in crack_fad.newman_raju_k.
    if not values["a_mm"] < values["t_mm"] or values["a_mm"] > values["c_mm"]:
        raise ValueError("require 0 < a < t and a <= c")
    if values["c_mm"] / values["width_mm"] >= 0.5:
        raise ValueError("plate solution requires c/width < 0.5")
    if values["sigma_u_mpa"] <= values["sigma_y_mpa"]:
        raise ValueError("require sigma_u_mpa > sigma_y_mpa for Option 1")
    if values["max_depth_fraction"] >= 1:
        raise ValueError("max_depth_fraction must be less than one")
    cap = min(values["max_depth_fraction"] * values["t_mm"], values["c_mm"])
    if values["tol_mm"] >= cap / 2:
        raise ValueError("tol_mm must be less than half the search depth cap")
    for flag in FLAGS:
        if flag in block and not isinstance(block[flag], bool):
            raise ValueError(f"{flag} must be a boolean")
    return values


def _depth(values):
    """Keep the same geometry, material and loading in every search trial."""
    args = {key: values[key] for key in LOADS + MATERIAL + SEARCH}
    # Total length = twice the input half-length (geometric definition).
    depth = critical_flaw_depth_mm(
        length_mm=2 * values["c_mm"],
        t_mm=values["t_mm"],
        width_mm=values["width_mm"],
        **args,
    )
    cap = min(values["max_depth_fraction"] * values["t_mm"], values["c_mm"])
    upper = cap - values["tol_mm"]
    status = "BRACKETED_BOUNDARY"
    if depth == upper:
        status = "SEARCH_BOUND_REACHED"
    elif depth == 0:
        status = "NO_ACCEPTABLE_DEPTH_FOUND"
    return {
        "depth_mm": depth,
        "status": status,
        "search_upper_mm": upper,
        "tolerance_mm": values["tol_mm"],
        "fixed_half_length_mm": values["c_mm"],
        "plate_half_width_mm": values["width_mm"],
    }


def _handoff(values, point, depth, block):
    """Preserve the calculated basis and identify unresolved specialist work."""
    handoff = escalate_to_level3(
        {key: values[key] for key in GEOMETRY},
        {key: values[key] for key in LOADS},
        {key: values[key] for key in MATERIAL},
        {"status": "NOT_EVALUATED", "reason": "Metal-loss Level 1 is not applicable."},
        dict(point, curve=CURVE),
        has_crack_like_flaw=True,
        **{flag: block.get(flag, False) for flag in FLAGS},
    )
    handoff.data_package["critical_depth"] = depth.copy()
    handoff.recommended_analysis[0] = (
        "Review the computed BS 7910:2013 Option 1 point, qualify the stress "
        "and toughness basis, and select any further fracture or numerical analysis."
    )
    handoff.notes = (
        "Level 3 specialist review is required by the crack screening policy. "
        "The BS 7910:2013 Option 1 point has been computed; service qualification, "
        "growth and advanced stress analysis have not been performed."
    )
    handoff.open_questions.extend(
        [
            "Confirm residual/secondary stresses, weld condition and local stress representation.",
            "Confirm toughness validity at service temperature and the governing code edition.",
            "Establish fatigue growth, inspection interval and the service disposition.",
            "Confirm a single boundary crossing and the depth search domain before using its bound.",
        ]
    )
    return asdict(handoff)


def router(cfg):
    """Assess one explicitly supplied plate flaw, render and optionally save."""
    block = cfg["crack_assessment"]
    values = _inputs(block)
    point = asdict(
        assess_crack_like_flaw(
            **{key: values[key] for key in GEOMETRY + LOADS + MATERIAL},
        )
    )
    depth = _depth(values)
    result = {
        "curve": CURVE,
        "assessment": point,
        "critical_depth": depth,
        "handoff": _handoff(values, point, depth, block),
        "qualification": "hand_calculation_only",
        "decision": {
            "verdict": "ESCALATE",
            "governing_criterion": "Crack screening requires specialist review; the point is "
            + ("inside" if point["acceptable"] else "outside")
            + " the BS 7910:2013 Option 1 envelope (Kr <= f(Lr), Lr <= Lr_max).",
        },
        "limitations": list(LIMITATIONS),
    }
    result["report_html"] = FFSReport.generate_crack_html(
        block.get("component_id", "PLATE"), result
    )
    store_report(result, block, "crack-fad-report.html", cfg.get("_config_dir_path"))
    if "report_path" in result:
        path = Path(result["report_path"]).with_name("level3-handoff.json")
        path.write_text(
            json.dumps(result["handoff"], indent=2, allow_nan=False) + "\n",
            encoding="utf-8",
        )
        result["handoff_path"] = str(path)
    cfg[cfg["basename"]] = result
    return cfg
