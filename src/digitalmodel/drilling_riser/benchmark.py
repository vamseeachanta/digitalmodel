# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solver-free benchmark screens and strict stored-summary comparisons.

Equation sources: MIT Structural Mechanics (2013), Lecture 6, Eqs (6.3)-(6.6),
and the in-repository derivations in riser_response.py, stackup.py and
operability.py; public drag equation linked in the report method. No numerical engineering acceptance is inferred.
"""

from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path

from digitalmodel.drilling_riser import generic_campaign
from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.riser_response import solve_static_response
from digitalmodel.drilling_riser.stackup import top_tension_required

REPORT = Path("docs/reports/modules/drilling-riser-benchmark")
PENDING = "reference to be selected: candidates must be public"
# These definitions are deliberately narrow: geometric extension is not stroke,
# and constant-tension slopes are not qualified assembly flex-joint rotations.
METRICS = {
    "lower_angle_deg": ("degrees", "absolute lower-end constant-tension column slope"),
    "upper_angle_deg": ("degrees", "absolute upper-end constant-tension column slope"),
    "top_tension_kn": ("kN", "constant-tension column top force"),
    "stroke_m": ("m", "mechanical tensioner stroke relative to initial position"),
}


def identity(value: dict) -> str:
    """Bind a result to canonical case inputs and their provenance."""
    return hashlib.sha256(json.dumps(value, sort_keys=True, allow_nan=False).encode()).hexdigest()


def proposed_table(root: Path) -> dict:
    """Retain the existing sweep as proposed required inputs, without promotion.

    The grid has no public range justification in hand. Its numeric proposals
    are transcribed from generic_campaign.case_table, not from memory.
    """
    original = generic_campaign.case_table(root)
    cases = []
    for row in original["cases"]:
        case = {"id": row["id"]}
        for name in ("offset_pct", "current_mps", "current_profile"):
            case[name] = dict(row[name], required_input=True, qualification=PENDING)
            case[name]["source"] = (
                "src/digitalmodel/drilling_riser/generic_campaign.py::case_table; "
                "proposed assumption only; " + PENDING
            )
        cases.append(case)
    return {"schema_version": 1, "template_sha256": original["template_sha256"],
            "status": "PROPOSED_INPUTS", "cases": cases}


def validate_table(root: Path, table: dict) -> None:
    """Require the unique, fully sourced fixed grid supported by the generator."""
    if identity(table) != identity(proposed_table(root)):
        raise ValueError("Unsupported or stale case table")
    ids = [row["id"] for row in table["cases"]]
    keys = [(row["offset_pct"]["value"], row["current_mps"]["value"])
            for row in table["cases"]]
    if len(set(ids)) != len(ids) or len(set(keys)) != len(keys):
        raise ValueError("Duplicate case table row")


def load_table(root: Path) -> dict:
    """Read and validate the committed benchmark input register."""
    table = json.loads((root / REPORT / "cases.json").read_text(encoding="utf-8"))
    validate_table(root, table)
    return table


def finite_number(value) -> bool:
    """JSON booleans, infinities and NaN are not engineering quantities."""
    try:
        return (not isinstance(value, bool) and isinstance(value, (int, float))
                and math.isfinite(value))
    except OverflowError:
        return False


def screen(*, length_m, offset_m, weight_kn, tension_factor, ei_nm2, drag_n_per_m) -> dict:
    """Apply the existing constant-tension beam and stack-up screens.

    EI*y'''' - T*y'' = q, pinned ends; riser_response.py module derivation.
    T=W*f; stackup.py::top_tension_required, with f supplied explicitly.
    Geometric extension=hypot(L,X)-L; Pythagoras, not equipment travel.
    """
    positive = (length_m, weight_kn, tension_factor, ei_nm2)
    if any(not finite_number(v) or v <= 0 for v in positive):
        raise ValueError("Positive finite screen inputs required")
    if any(not finite_number(v) for v in (offset_m, drag_n_per_m)):
        raise ValueError("Finite offset and drag required")
    tension = top_tension_required(weight_kn, tension_factor)
    response = solve_static_response(length_m=length_m, top_offset_m=offset_m,
                                     tension_n=tension * 1000, ei_nm2=ei_nm2,
                                     current_load_n_per_m=drag_n_per_m)
    # 1000 is the SI kN-to-N unit conversion, not an engineering coefficient.
    return {"lower_angle_deg": abs(response.angle_lower_deg),
            "upper_angle_deg": abs(response.angle_upper_deg),
            "top_tension_kn": tension,
            "geometric_extension_m": math.hypot(length_m, offset_m) - length_m,
            "stroke_m": None}


def screen_cases(root: Path, table: dict) -> list[dict]:
    """Compute diagnostic screens using the existing campaign's explicit basis."""
    validate_table(root, table)
    basis = generic_campaign.column_basis(root)
    spec = generic_campaign.read_spec(root)
    # Existing campaign assumption, derived here from its recorded force/weight;
    # this is not an API design factor or an assembly acceptance limit.
    factor = basis["min_tension_kn"] / basis["weight_kn"]
    fingerprints = generic_campaign.calculation_fingerprints()
    fingerprints["benchmark.py"] = generic_campaign.source_digest(Path(__file__))
    rows = []
    for case in table["cases"]:
        offset = watch_circle_radius_m(case["offset_pct"]["value"],
                                       spec["environment"]["water"]["depth"])
        speed = case["current_mps"]["value"]
        # Morison drag term q=rho*Cd*D*U²/2; riser_response.py module derivation.
        drag = basis["rho"] * basis["cd"] * basis["od"] * speed**2 / 2
        values = screen(length_m=basis["length"], offset_m=offset,
                        weight_kn=basis["weight_kn"], tension_factor=factor,
                        ei_nm2=basis["section"].bending_stiffness, drag_n_per_m=drag)
        rows.append({"case_id": case["id"], "case_digest": identity({"case": case,
                     "template_sha256": table["template_sha256"], "calculations": fingerprints,
                     "model_inputs": {path.relative_to(root / generic_campaign.DEST).as_posix():
                         generic_campaign.digest(path) for path in sorted(
                             (root / generic_campaign.DEST / "models" / case["id"]).rglob("*.yml"))}}),
                     "offset_pct": case["offset_pct"]["value"], "current_mps": speed,
                     "offset_m": offset, "screens": values})
    return rows


def not_evaluated(reason: str) -> dict:
    """Null is distinct from a physical zero and from a failed comparison."""
    return {"value": None, "difference_pct": None, "status": "NOT_EVALUATED",
            "reason": reason, "origin": None, "criterion_source": None, "solver_version": None}


def nonempty(value) -> bool:
    return isinstance(value, str) and bool(value.strip())


def read_result(path: Path, *, case_id: str, case_digest: str,
                screens: dict, criteria: dict) -> dict:
    """Read JSON only; no solver binding, native file or result extraction.

    Difference = 100*(stored-screen)/abs(screen). PASS means absolute difference
    is at most the explicitly supplied agreement criterion. It never means
    assembly acceptance. Zero reference has no defined percentage difference.
    Missing, malformed or stale evidence is NOT_EVALUATED, never zero or PASS.
    """
    empty = {key: not_evaluated("Stored result absent, malformed or stale") for key in screens}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return empty
    if (not isinstance(data, dict) or data.get("case_id") != case_id
            or data.get("case_digest") != case_digest
            or data.get("origin") not in ("synthetic", "stored")
            or not nonempty(data.get("solver_version"))
            or not isinstance(data.get("metrics"), dict)):
        return empty
    for key, reference in screens.items():
        if key not in METRICS:
            empty[key] = not_evaluated("Quantity is not a comparable solver response")
            continue
        item = data["metrics"].get(key)
        unit, definition = METRICS[key]
        if (not isinstance(item, dict) or not finite_number(item.get("value"))
                or item.get("unit") != unit or item.get("definition") != definition
                or not nonempty(item.get("source"))):
            continue
        result = dict(empty[key], value=item["value"], origin=data["origin"],
                      solver_version=data["solver_version"])
        empty[key] = result
        if not finite_number(reference) or reference == 0:
            result["reason"] = "Comparable nonzero screen unavailable; mechanical stroke is not geometric extension"
            continue
        difference = 100 * (item["value"] - reference) / abs(reference)
        if not math.isfinite(difference):
            result["reason"] = "Percentage difference is not finite"
            continue
        result["difference_pct"] = difference
        criterion = criteria.get(key)
        if (not isinstance(criterion, dict) or not finite_number(criterion.get("value"))
                or criterion["value"] < 0 or not nonempty(criterion.get("source"))):
            result["reason"] = "Required sourced agreement criterion absent"
            continue
        result.update(status="PASS" if abs(difference) <= criterion["value"] else "FAIL",
                      reason="Screen agreement only; no assembly acceptance",
                      criterion_source=criterion["source"])
    return empty


def evaluate(root: Path, results: Path | None = None, criteria: dict | None = None) -> list[dict]:
    """Build a complete ledger, retaining every missing result as a placeholder."""
    table = load_table(root)
    rows = screen_cases(root, table)
    for row in rows:
        if results is None:
            row["results"] = {key: not_evaluated("No committed result selected") for key in METRICS}
        else:
            row["results"] = read_result(results / (row["case_id"] + ".json"),
                case_id=row["case_id"], case_digest=row["case_digest"],
                screens={key: row["screens"][key] for key in METRICS}, criteria=criteria or {})
    return rows
