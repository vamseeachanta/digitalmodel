# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared structural and semantic checks for strength and fatigue exchange files."""

from __future__ import annotations

import json
import math
import re
from datetime import datetime
from pathlib import Path
from typing import Any

Document = dict[str, Any]


def number(value: Any, label: str, minimum: float | None = None) -> float:
    """Require a finite JSON number, excluding booleans."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{label}: expected a number")
    if not math.isfinite(value) or (minimum is not None and value < minimum):
        raise ValueError(f"{label}: invalid numeric value")
    return float(value)


def text(value: Any, label: str) -> str:
    """Require nonempty text."""
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{label}: expected nonempty text")
    return value


def vector(value: Any, label: str, size: int | None = None) -> list[float]:
    """Require a nonempty finite numeric array of the requested size."""
    if not isinstance(value, (list, tuple)) or not value:
        raise ValueError(f"{label}: expected a nonempty array")
    if size is not None and len(value) != size:
        raise ValueError(f"{label}: array length mismatch")
    return [number(item, label) for item in value]


def _keys(value: Any, required: tuple[str, ...], label: str) -> None:
    if not isinstance(value, dict) or any(key not in value for key in required):
        raise ValueError(f"{label}: missing required fields")


def _finite_tree(value: Any) -> None:
    if isinstance(value, dict):
        for item in value.values():
            _finite_tree(item)
    elif isinstance(value, (list, tuple)):
        for item in value:
            _finite_tree(item)
    elif isinstance(value, float) and not math.isfinite(value):
        raise ValueError("nonfinite JSON number")


def _validate_run(run: Document) -> None:
    _keys(run, ("input_sha256", "result_file", "date"), "run")
    if not re.fullmatch(r"[a-f0-9]{64}", text(run["input_sha256"], "input_sha256")):
        raise ValueError("input_sha256: expected a SHA-256 digest")
    filename = text(run["result_file"], "result_file")
    if "/" in filename or "\\" in filename or filename in {".", ".."}:
        raise ValueError("result_file: expected a basename")
    date = text(run["date"], "date")
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:\d{2})", date):
        raise ValueError("date: expected an RFC3339 timestamp")
    datetime.fromisoformat(date.replace("Z", "+00:00"))


def _strength(doc: Document) -> None:
    _keys(doc, ("software", "load_case", "arc_length_m", "effective_tension_kN",
                "bending_moment_kNm", "von_mises_stress_MPa", "envelopes",
                "von_mises_utilisation", "utilisation_method",
                "max_utilisation", "touchdown", "top_angle_deg", "hang_off_tension_kN"), "strength")
    _keys(doc["software"], ("name", "version"), "software")
    for key in ("name", "version"):
        text(doc["software"][key], f"software.{key}")
    _keys(doc["load_case"], ("id", "wave", "current", "offset"), "load_case")
    text(doc["load_case"]["id"], "load_case.id")
    number(doc["load_case"]["offset"], "offset")
    arcs = vector(doc["arc_length_m"], "arc_length_m")
    if arcs[0] != 0 or any(b <= a for a, b in zip(arcs, arcs[1:])):
        raise ValueError("arc_length_m: expected increasing stations from hang-off zero")
    for key in ("effective_tension_kN", "bending_moment_kNm", "von_mises_utilisation"):
        vector(doc[key], key, len(arcs))
    util = doc["von_mises_utilisation"]
    for value in util:
        number(value, "utilisation", 0)
    method = doc["utilisation_method"]
    _keys(method, ("name", "source", "allowable_MPa"), "utilisation_method")
    text(method["name"], "method.name")
    text(method["source"], "method.source")
    allowable = number(method["allowable_MPa"], "allowable_MPa", 0)
    if allowable == 0:
        raise ValueError("allowable_MPa: must be positive")
    stresses = vector(doc["von_mises_stress_MPa"], "von_mises_stress_MPa", len(arcs))
    for stress, utilisation in zip(stresses, util):
        number(stress, "equivalent stress", 0)
        if not math.isclose(utilisation, stress / allowable, rel_tol=1e-9, abs_tol=0):
            raise ValueError("utilisation: inconsistent equivalent stress / allowable")
    peak = doc["max_utilisation"]
    _keys(peak, ("value", "arc_length_m", "location"), "max_utilisation")
    arc = number(peak["arc_length_m"], "peak arc")
    text(peak["location"], "location")
    if arc not in arcs or not math.isclose(number(peak["value"], "peak value"), max(util)):
        raise ValueError("max_utilisation: inconsistent peak")
    if not math.isclose(peak["value"], util[arcs.index(arc)]):
        raise ValueError("max_utilisation: inconsistent station")
    touchdown = doc["touchdown"]
    _keys(touchdown, ("arc_length_m", "utilisation", "stress_range_MPa"), "touchdown")
    td_arc = number(touchdown["arc_length_m"], "touchdown arc")
    if td_arc not in arcs or not math.isclose(
            number(touchdown["utilisation"], "touchdown utilisation"), util[arcs.index(td_arc)]):
        raise ValueError("touchdown: inconsistent station or utilisation")
    static = doc.get("analysis_type") == "static"
    for field in ("effective_tension_kN", "bending_moment_kNm", "von_mises_stress_MPa"):
        envelope = doc["envelopes"][field]
        _keys(envelope, ("static", "min", "max"), "envelope")
        static_values = vector(envelope["static"], "static envelope", len(arcs))
        if static:
            if envelope["min"] is not None or envelope["max"] is not None:
                raise ValueError("static envelope: dynamic values must be null")
            governing = static_values
        else:
            low = vector(envelope["min"], "minimum envelope", len(arcs))
            high = vector(envelope["max"], "maximum envelope", len(arcs))
            if any(a > b for a, b in zip(low, high)):
                raise ValueError("envelope: inverted range")
            governing = [max(a, b) for a, b in zip(static_values, high)]
        if field == "von_mises_stress_MPa":
            for values in envelope.values():
                if values is not None:
                    for value in values:
                        number(value, "equivalent stress envelope", 0)
            if any(not math.isclose(a, b, rel_tol=1e-9, abs_tol=0)
                   for a, b in zip(stresses, governing)):
                raise ValueError("equivalent stress: inconsistent governing envelope")
    if touchdown["stress_range_MPa"] is None:
        if not static:
            raise ValueError("touchdown range: missing dynamic value")
    else:
        number(touchdown["stress_range_MPa"], "touchdown range", 0)
    angles = doc["top_angle_deg"]
    _keys(angles, ("static", "dynamic_min", "dynamic_max"), "top_angle_deg")
    number(angles["static"], "static angle", 0)
    tension = doc["hang_off_tension_kN"]
    _keys(tension, ("static", "dynamic_max"), "hang_off_tension_kN")
    number(tension["static"], "static tension")
    if static:
        if any(v is not None for v in (angles["dynamic_min"], angles["dynamic_max"], tension["dynamic_max"])):
            raise ValueError("static result: dynamic quantities must be null")
    else:
        low = number(angles["dynamic_min"], "minimum angle", 0)
        high = number(angles["dynamic_max"], "maximum angle", 0)
        if high < low:
            raise ValueError("angle: inverted dynamic range")
        number(tension["dynamic_max"], "maximum tension")


def _fatigue(doc: Document) -> None:
    _keys(doc, ("sea_state", "hot_spots", "sn_curve", "damage_per_year", "life_years",
                "duration_s", "annual_hours_basis"), "fatigue")
    sea = doc["sea_state"]
    _keys(sea, ("id", "Hs_m", "Tp_s", "direction_deg", "occurrence"), "sea_state")
    if not re.fullmatch(r"[A-Za-z0-9_-]+", text(sea["id"], "sea_state.id")):
        raise ValueError("sea_state.id: invalid identifier")
    number(sea["Hs_m"], "Hs_m", 0)
    if number(sea["Tp_s"], "Tp_s", 0) == 0:
        raise ValueError("Tp_s: must be positive")
    direction = number(sea["direction_deg"], "direction_deg", 0)
    occurrence = number(sea["occurrence"], "occurrence", 0)
    if direction >= 360 or occurrence > 1:
        raise ValueError("sea_state: direction or occurrence out of bounds")
    duration = number(doc["duration_s"], "duration_s", 0)
    annual_hours = number(doc["annual_hours_basis"], "annual_hours_basis", 0)
    if duration == 0 or annual_hours == 0:
        raise ValueError("duration_s and annual_hours_basis: must be positive")
    year_multiplier = number(annual_hours * 3600 / duration * occurrence, "annual multiplier", 0)
    curve = doc["sn_curve"]
    _keys(curve, ("name", "source", "m", "log_a", "thickness_correction"), "sn_curve")
    text(curve["name"], "sn_curve.name")
    text(curve["source"], "sn_curve.source")
    slope = number(curve["m"], "slope", 0)
    if slope == 0:
        raise ValueError("slope: must be positive")
    intercept = number(10 ** number(curve["log_a"], "log_a"), "S-N intercept", 0)
    if intercept == 0:
        raise ValueError("S-N intercept: must be positive")
    thickness = number(curve["thickness_correction"], "thickness correction", 0)
    if thickness == 0:
        raise ValueError("thickness correction: must be positive")
    hot_spots = doc["hot_spots"]
    if not isinstance(hot_spots, list) or not hot_spots:
        raise ValueError("hot_spots: expected nonempty array")
    names = set()
    identities = set()
    damages = []
    for hot in hot_spots:
        _keys(hot, ("name", "arc_length_m", "cycles_per_year", "scf",
                    "range_histogram", "damage_per_year"), "hot_spot")
        name = hot["name"]
        if name not in {"touchdown", "hang_off"}:
            raise ValueError("hot_spot: unexpected name")
        names.add(name)
        arc = number(hot["arc_length_m"], "hot_spot arc", 0)
        identity = (name, arc, number(hot.get("fibre_deg", 0), "fibre"))
        if identity in identities:
            raise ValueError("hot_spots: duplicate station/fibre")
        identities.add(identity)
        annual_cycles = number(hot["cycles_per_year"], "annual cycles", 0)
        scf = number(hot["scf"], "scf", 0)
        if scf == 0:
            raise ValueError("scf: must be positive")
        if not isinstance(hot["range_histogram"], list):
            raise ValueError("histogram: expected array")
        cycles = []
        contributions = []
        for row in hot["range_histogram"]:
            _keys(row, ("stress_range_MPa", "cycles"), "histogram")
            stress_range = number(row["stress_range_MPa"], "stress range", 0)
            count = number(row["cycles"], "cycle count", 0)
            cycles.append(count)
            contributions.append(count * (stress_range * scf * thickness) ** slope / intercept)
        expected_cycles = number(math.fsum(cycles) * year_multiplier, "derived annual cycles", 0)
        expected_damage = number(math.fsum(contributions) * year_multiplier, "derived station damage", 0)
        if not math.isclose(annual_cycles, expected_cycles, rel_tol=1e-9, abs_tol=0):
            raise ValueError("annual cycles: inconsistent histogram or annualisation")
        if "stress_range_MPa" in hot:
            number(hot["stress_range_MPa"], "stress range", 0)
        station_damage = number(hot["damage_per_year"], "station damage", 0)
        if not math.isclose(station_damage, expected_damage, rel_tol=1e-9, abs_tol=0):
            raise ValueError("station damage: inconsistent histogram or S-N curve")
        damages.append(expected_damage)
    if names != {"touchdown", "hang_off"}:
        raise ValueError("hot_spots: both regions are required")
    damage = number(doc["damage_per_year"], "damage_per_year", 0)
    if not math.isclose(damage, max(damages), rel_tol=1e-9, abs_tol=0):
        raise ValueError("damage_per_year: inconsistent governing station")
    life = doc["life_years"]
    if damage == 0:
        if life is not None:
            raise ValueError("life_years: zero damage requires null")
    elif life is None or not math.isclose(number(life, "life_years", 0) * damage, 1):
        raise ValueError("life_years: inconsistent reciprocal damage")


def validate_document(doc: Document, kind: str) -> None:
    """Validate a shared exchange document, raising ValueError on any defect."""
    try:
        _keys(doc, ("case_id", "run"), kind)
        text(doc["case_id"], "case_id")
        _finite_tree(doc)
        _validate_run(doc["run"])
        if kind == "strength":
            _strength(doc)
        elif kind == "fatigue":
            _fatigue(doc)
        else:
            raise ValueError("unknown document kind")
    except (TypeError, KeyError, IndexError, OverflowError) as error:
        raise ValueError(f"{kind}: invalid document structure") from error


def _unique_pairs(pairs: list[tuple[str, Any]]) -> Document:
    output: Document = {}
    for key, value in pairs:
        if key in output:
            raise ValueError("duplicate JSON key")
        output[key] = value
    return output


def _reject_constant(value: str) -> None:
    raise ValueError("nonfinite JSON constant")


def read_document(path: Path | str) -> Document:
    """Read JSON without accepting duplicate keys or nonfinite values."""
    doc = json.loads(Path(path).read_text(encoding="utf-8"),
                     object_pairs_hook=_unique_pairs, parse_constant=_reject_constant)
    if not isinstance(doc, dict):
        raise ValueError("expected a JSON object")
    _finite_tree(doc)
    return doc
