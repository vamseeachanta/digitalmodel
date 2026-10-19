# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline, condition-matched resistance sweeps and three-grid studies.

No solver is launched. Input evidence and acceptance limits are explicit;
this module checks declarations, not the authenticity of external records.
Equations and schema: docs/domains/openfoam/resistance-campaign.md.
Grid method: NASA, Examining Spatial (Grid) Convergence, updated 2021,
https://www.grc.nasa.gov/www/wind/valid/tutorial/spatconv.html.
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any

from .grid_convergence import grid_convergence

CONDITION_FIELDS = (
    "body_condition",
    "appendages",
    "wetted_surface_m2",
    "lpp_m",
    "density_kg_m3",
    "viscosity_m2_s",
)
# NASA (2021), Grid Considerations for a Grid Convergence Study.
MIN_REFINEMENT_RATIO = 1.1
# Numerical equality only; not an engineering acceptance tolerance. See Method.
RATIO_ROUNDOFF = 1e-12


def _positive(value: Any, name: str, *, zero_allowed: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or value < 0 or (value == 0 and not zero_allowed):
        raise ValueError(f"{name} must be finite and positive")
    return float(value)


def _source(record: dict) -> bool:
    return isinstance(record.get("source"), str) and bool(record["source"].strip())


def _condition(record: dict) -> tuple | None:
    condition = record.get("condition", {})
    if any(field not in condition for field in CONDITION_FIELDS):
        return None
    if any(
        not isinstance(condition[k], str) or not condition[k].strip()
        for k in CONDITION_FIELDS[:2]
    ):
        return None
    for key in CONDITION_FIELDS[2:]:
        _positive(condition[key], key)
    return tuple(condition[k] for k in CONDITION_FIELDS)


def _points(series: dict) -> list[dict]:
    points = list(series.get("points", []))
    for point in points:
        for key in ("froude", "ct", "reynolds"):
            _positive(point.get(key), key)
    points.sort(key=lambda point: point["froude"])
    if len({p["froude"] for p in points}) != len(points):
        raise ValueError("duplicate Froude numbers")
    return points


def _not_evaluated(reason: str, **details: Any) -> dict:
    return {"status": "NOT_EVALUATED", "reason": reason, **details}


def _limit(limits: dict, name: str) -> float | None:
    value = limits.get(name)
    if value is None or not _source(limits):
        return None
    return _positive(value, name, zero_allowed=True)


def evaluate_sweep(
    reference: dict, computed: dict, prerequisite: dict, limits: dict
) -> dict:
    """Require matching physical conditions and complete, identical speed sets.

    Signed point errors and signed adjacent-increment errors are tested against
    symmetric bands. Increment errors use max(adjacent reference Ct) as their
    scale, remaining defined at flat portions and extrema. This is the explicit
    campaign metric defined in the accompanying method, not a standard limit.
    """
    refs, runs = _points(reference), _points(computed)
    ref_condition, run_condition = _condition(reference), _condition(computed)
    if prerequisite.get("status") != "PASS" or not _source(prerequisite):
        return _not_evaluated("validated single-point prerequisite is missing")
    if not _source(reference) or not _source(computed):
        return _not_evaluated("reference or computed source is missing")
    if reference.get("qualified") is not True:
        return _not_evaluated("reference evidence has not been qualified")
    if ref_condition is None or ref_condition != run_condition:
        return _not_evaluated("physical condition tuple is incomplete or mismatched")
    # An increment requires two points; this is a dimensional requirement.
    if len(refs) < 2 or [p["froude"] for p in refs] != [p["froude"] for p in runs]:
        return _not_evaluated("need a complete matched multi-Froude series")
    if any(a["reynolds"] != b["reynolds"] for a, b in zip(refs, runs)):
        return _not_evaluated("Reynolds number differs at a matched speed")
    if any(p.get("settled") is not True for p in runs):
        return _not_evaluated("computed coefficients lack settled-history evidence")
    ct_limit = _limit(limits, "ct_fraction")
    increment_limit = _limit(limits, "increment_fraction")
    if ct_limit is None or increment_limit is None:
        return _not_evaluated("sourced two-sided Ct and increment limits are required")
    errors = [(run["ct"] - ref["ct"]) / ref["ct"] for ref, run in zip(refs, runs)]
    increments = []
    for index in range(len(refs) - 1):
        a, b = refs[index : index + 2]
        x, y = runs[index : index + 2]
        increments.append(
            ((y["ct"] - x["ct"]) - (b["ct"] - a["ct"])) / max(a["ct"], b["ct"])
        )
    ct_pass = all(-ct_limit <= error <= ct_limit for error in errors)
    increment_pass = all(
        -increment_limit <= error <= increment_limit for error in increments
    )
    return {
        "status": "PASS" if ct_pass and increment_pass else "FAIL",
        "froude": [p["froude"] for p in refs],
        "ct_errors_fraction": errors,
        "increment_errors_fraction": increments,
        "ct_pass": ct_pass,
        "increment_pass": increment_pass,
        "limits": {
            "ct_fraction": ct_limit,
            "increment_fraction": increment_limit,
            "source": limits["source"],
        },
    }


def evaluate_grids(grids: list[dict], limits: dict) -> dict:
    """GCI for one settled, systematically refined, constant-ratio triplet.

    Unsupported inputs are not extrapolated. The existing grid-convergence
    implementation supplies Richardson/GCI only after these stricter guards.
    Mesh-family identity is a caller declaration requiring external review.
    """
    # Three levels are needed to observe an order: NASA (2021), GCI section.
    if len(grids) != 3:
        return _not_evaluated("three completed mesh levels are required")
    for grid in grids:
        _positive(grid.get("h_m"), "h_m")
        _points({"points": [grid.get("point", {})]})
    ordered = sorted(grids, key=lambda grid: grid["h_m"])
    fine = ordered[0]
    basis = _condition(fine)
    if basis is None or any(_condition(grid) != basis for grid in ordered):
        return _not_evaluated("grid physical condition tuples differ or are incomplete")
    family = fine.get("mesh_family")
    if (
        not isinstance(family, str)
        or not family.strip()
        or any(grid.get("mesh_family") != family for grid in ordered)
    ):
        return _not_evaluated("a common systematic mesh family is required")
    for grid in ordered:
        if not _source(grid) or grid["point"].get("settled") is not True:
            return _not_evaluated("grid source or settled-history evidence is missing")
        if any(
            grid["point"][key] != fine["point"][key] for key in ("froude", "reynolds")
        ):
            return _not_evaluated("grid Froude or Reynolds numbers differ")
    h = [grid["h_m"] for grid in ordered]
    ratios = (h[1] / h[0], h[2] / h[1])
    if min(ratios) < MIN_REFINEMENT_RATIO:
        return _not_evaluated("refinement ratio is below the NASA recommended minimum")
    if not math.isclose(*ratios, rel_tol=RATIO_ROUNDOFF, abs_tol=0):
        return _not_evaluated(
            "unequal refinement ratios require a separate order method"
        )
    values = [grid["point"]["ct"] for grid in ordered]
    e21, e32 = values[1] - values[0], values[2] - values[1]
    if e21 == 0 or e32 == 0:
        classification = "degenerate"
    elif e21 / e32 < 0:
        classification = "oscillatory"
    elif e21 / e32 >= 1:
        classification = "divergent"
    else:
        classification = "monotonic"
    if classification != "monotonic":
        return _not_evaluated(
            "no positive-order monotonic Richardson estimate",
            classification=classification,
        )
    # NASA (2021), GCI: Fs=1.25 for three grids, provided by the existing API.
    try:
        gci = grid_convergence(h, values).to_dict()
    except (ArithmeticError, ValueError):
        return _not_evaluated(
            "grid arithmetic cannot form a finite observed-order estimate"
        )
    if any(
        not math.isfinite(value)
        for value in gci.values()
        if isinstance(value, (int, float))
    ):
        return _not_evaluated("grid diagnostics are outside finite numerical range")
    # Lists keep API and JSON/CLI reports identical.
    gci["h"], gci["f"] = list(gci["h"]), list(gci["f"])
    threshold = _limit(limits, "gci_medium_pct")
    details = {
        "classification": classification,
        "gci": gci,
        "froude": fine["point"]["froude"],
        "reynolds": fine["point"]["reynolds"],
    }
    if threshold is None:
        return _not_evaluated("sourced medium-grid GCI limit is missing", **details)
    return {
        "status": "PASS" if gci["gci_medium_pct"] <= threshold else "FAIL",
        "limit_pct": threshold,
        "limit_source": limits["source"],
        **details,
    }


def analyze_campaign(data: dict) -> dict:
    """Combine independent sweep and numerical gates; retain source declarations."""
    limits = data.get("limits", {})
    sweep = evaluate_sweep(
        data.get("reference", {}),
        data.get("computed", {}),
        data.get("prerequisite", {}),
        limits,
    )
    grid = evaluate_grids(data.get("grids", []), limits)
    if "gci" in grid:
        computed = data.get("computed", {})
        basis = _condition(computed)
        matched = any(
            point["froude"] == grid["froude"] and point["reynolds"] == grid["reynolds"]
            for point in computed.get("points", [])
        )
        if basis != _condition(data["grids"][0]) or not matched:
            grid = _not_evaluated("grid triplet does not belong to the computed sweep")
    statuses = {sweep["status"], grid["status"]}
    status = (
        "FAIL"
        if "FAIL" in statuses
        else "NOT_EVALUATED"
        if "NOT_EVALUATED" in statuses
        else "PASS"
    )
    return {
        "status": status,
        "sweep": sweep,
        "grid": grid,
        "evidence": {
            key: data.get(key)
            for key in ("reference", "computed", "prerequisite", "grids")
        },
        "scope": "offline declared-evidence analysis; no solver execution",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args(argv)
    try:
        report = analyze_campaign(json.loads(args.input.read_text()))
        args.output.write_text(json.dumps(report, indent=2, allow_nan=False) + "\n")
    except (ValueError, KeyError, TypeError, OSError) as exc:
        # Avoid echoing external file contents or local paths into reports.
        parser.exit(2, f"invalid campaign input or output ({type(exc).__name__})\n")
    # Shell protocol, not an engineering threshold: 0 pass, 1 fail, 2 unevaluated.
    return {"PASS": 0, "FAIL": 1, "NOT_EVALUATED": 2}[report["status"]]


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
