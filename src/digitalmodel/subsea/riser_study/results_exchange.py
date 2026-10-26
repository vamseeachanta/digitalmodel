# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared exported-result types and structural and semantic validation.

The normalised strength limit is unity by the definition stress / allowable;
no standard acceptance value is inferred. Method and allowable are export
inputs. Relative consistency tolerance only absorbs JSON round-off.
"""

from __future__ import annotations

import json
import math
from pathlib import Path, PurePosixPath
from typing import Any, Literal, NotRequired, TypedDict

from jsonschema import Draft202012Validator, FormatChecker


class Run(TypedDict):
    """Portable native export provenance."""

    input_sha256: str
    result_file: str
    date: str


class Software(TypedDict):
    """Recorded software identity, without an inferred version."""

    name: str
    version: str


class Offset(TypedDict):
    """Horizontal offset in metres."""

    x_m: float
    y_m: float


class LoadCase(TypedDict):
    """Exported environmental parameters; wave/current retain named units."""

    id: str
    wave: dict[str, Any]
    current: dict[str, Any]
    offset: Offset


class Allowable(TypedDict):
    """Stress denominator and its supplied basis."""

    value: float
    unit: Literal["MPa"]
    source: str


class Maximum(TypedDict):
    """Maximum over the exported strength samples."""

    value: float
    arc_length_m: float
    location: str


class Touchdown(TypedDict):
    """Touchdown summary, including the exported stress range."""

    arc_length_m: float
    utilisation: float
    stress_range_MPa: float


class TopAngle(TypedDict):
    """Acute inclination from vertical, in degrees."""

    static: float
    dynamic_min: float
    dynamic_max: float


class HangOffTension(TypedDict):
    """Static and dynamic maximum effective tension in kN."""

    static: float
    dynamic_max: float


class StrengthResult(TypedDict):
    """One case's strength export; arc is measured from hang-off."""

    case_id: str
    software: Software
    load_case: LoadCase
    arc_length_m: list[float]
    effective_tension_kN: list[float]
    bending_moment_kNm: list[float]
    von_mises_utilisation: list[float]
    method: str
    allowable: Allowable
    max_utilisation: Maximum
    touchdown: Touchdown
    top_angle_deg: TopAngle
    hang_off_tension_kN: HangOffTension
    run: Run


class RangeBin(TypedDict):
    """One stress-range histogram bin, with annual cycles."""

    stress_range_MPa: float
    cycles_per_year: float


class HotSpot(TypedDict):
    """One hot spot with either a single range or a histogram."""

    name: Literal["touchdown", "hang_off"]
    arc_length_m: float
    cycles_per_year: float
    scf: float
    stress_range_MPa: NotRequired[float]
    range_histogram: NotRequired[list[RangeBin]]


class SeaState(TypedDict):
    """Sea-state parameters with occurrence as a fraction."""

    id: str
    Hs_m: float
    Tp_s: float
    direction_deg: float
    occurrence: float


class SNCurve(TypedDict):
    """Supplied curve; no licensed publication value is supplied here."""

    name: str
    source: str
    m: float
    log_a: float
    thickness_correction: float


class FatigueResult(TypedDict):
    """One case and sea-state fatigue export."""

    case_id: str
    sea_state: SeaState
    hot_spots: list[HotSpot]
    sn_curve: SNCurve
    damage_per_year: float
    life_years: float | None
    run: Run


# Numerical round-off tolerance, not an engineering acceptance band.
ROUND_OFF = 1e-9


def _finite_tree(value: Any) -> None:
    if isinstance(value, float) and not math.isfinite(value):
        raise ValueError("non-finite numeric value")
    if isinstance(value, dict):
        for child in value.values():
            _finite_tree(child)
    if isinstance(value, list):
        for child in value:
            _finite_tree(child)


def portable_path(value: str) -> str:
    """Validate a relative POSIX provenance path without disclosing its value."""
    path = PurePosixPath(value)
    if (not value or path.is_absolute() or "\\" in value or ":" in value
            or any(part in {"", ".", ".."} for part in value.split("/"))
            or any(ord(char) < 32 for char in value)):
        raise ValueError("result_file must be a portable relative path")
    return value


def _strength_semantics(data: dict[str, Any]) -> None:
    arc = data["arc_length_m"]
    if arc[0] != 0 or any(a >= b for a, b in zip(arc, arc[1:])):
        raise ValueError("arc_length_m must increase strictly from hang-off zero")
    for key in ("effective_tension_kN", "bending_moment_kNm", "von_mises_utilisation"):
        if len(data[key]) != len(arc):
            raise ValueError(f"{key} length differs from arc_length_m")
    maximum = data["max_utilisation"]
    values = data["von_mises_utilisation"]
    if not math.isclose(maximum["value"], max(values), rel_tol=ROUND_OFF, abs_tol=ROUND_OFF):
        raise ValueError("max_utilisation differs from exported samples")
    if not any(math.isclose(s, maximum["arc_length_m"], abs_tol=ROUND_OFF, rel_tol=0)
               and math.isclose(u, maximum["value"], rel_tol=ROUND_OFF, abs_tol=ROUND_OFF)
               for s, u in zip(arc, values)):
        raise ValueError("maximum arc does not identify a maximum sample")
    touchdown = data["touchdown"]
    if not arc[0] <= touchdown["arc_length_m"] <= arc[-1]:
        raise ValueError("touchdown outside exported arc")
    for s, u in zip(arc, values):
        if s == touchdown["arc_length_m"] and not math.isclose(
                u, touchdown["utilisation"], rel_tol=ROUND_OFF, abs_tol=ROUND_OFF):
            raise ValueError("touchdown utilisation differs from coincident sample")
    angle = data["top_angle_deg"]
    if angle["dynamic_min"] > angle["dynamic_max"]:
        raise ValueError("top-angle dynamic range is reversed")


def _fatigue_semantics(data: dict[str, Any]) -> None:
    damage, life = data["damage_per_year"], data["life_years"]
    if damage == 0:
        if life is not None:
            raise ValueError("zero damage requires null life_years")
    elif life is None or not math.isclose(damage * life, 1, rel_tol=ROUND_OFF):
        raise ValueError("life_years must equal reciprocal annual damage")
    names = [spot["name"] for spot in data["hot_spots"]]
    if len(names) != len(set(names)):
        raise ValueError("duplicate hot-spot name")
    for spot in data["hot_spots"]:
        if "range_histogram" in spot and not math.isclose(
                sum(row["cycles_per_year"] for row in spot["range_histogram"]),
                spot["cycles_per_year"], rel_tol=ROUND_OFF, abs_tol=ROUND_OFF):
            raise ValueError("histogram annual cycles differ from hot-spot total")


def validate_result(data: Any, kind: Literal["strength", "fatigue"]) -> dict[str, Any]:
    """Validate schema, finite numbers, provenance and cross-field consistency.

    Returns:
        The original mapping after validation, without changing any result.

    Raises:
        ValueError: If structural or semantic validation fails.
    """
    if kind not in {"strength", "fatigue"}:
        raise ValueError("unknown result kind")
    schema = json.loads(Path(__file__).with_suffix(".schema.json").read_text())
    validator = Draft202012Validator({"$ref": f"#/$defs/{kind}", "$defs": schema["$defs"]},
                                    format_checker=FormatChecker())
    error = next(validator.iter_errors(data), None)
    if error is not None:
        # Values are deliberately omitted from diagnostics.
        field = ".".join(str(key) for key in error.absolute_path) or "record"
        raise ValueError(f"{field}: schema rule {error.validator}")
    _finite_tree(data)
    portable_path(data["run"]["result_file"])
    if kind == "strength":
        _strength_semantics(data)
    else:
        _fatigue_semantics(data)
    return data


def load_result(path: Path, kind: Literal["strength", "fatigue"]) -> dict[str, Any]:
    """Load JSON and validate it without importing a native solver interface."""
    return validate_result(json.loads(path.read_text(encoding="utf-8")), kind)
