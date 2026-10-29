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


class FibreStress(TypedDict):
    """Exported nonnegative equivalent stress on both pipe fibres, in MPa."""

    inner: list[float]
    outer: list[float]


class StressStates(TypedDict):
    """Static stresses and the maximum over each dynamic fibre history."""

    static: FibreStress
    dynamic_max: FibreStress | None


class U1Inputs(TypedDict):
    """Explicit stress combination and material basis for U1."""

    method: Literal["API RP 2RD"]
    smys_MPa: float
    stress_MPa: StressStates


class TensionEnvelope(TypedDict):
    """Dynamic extrema at the same arc stations as the static export."""

    dynamic_min: list[float] | None
    dynamic_max: list[float] | None


class AnalyticalStatic(TypedDict):
    """Independent analytical catenary outputs with explicit units."""

    top_tension_kN: float
    top_angle_deg: float
    touchdown_x_m: float


class StaticCheck(TypedDict):
    """Analytical comparison inputs; touchdown position is horizontal."""

    analytical: AnalyticalStatic
    software_touchdown_x_m: float
    basis: str


class CombinedCheck(TypedDict):
    """An exported combined-loading check over the recorded history."""

    arc_length_m: float
    utilisation: float
    check: str


class CombinedLoading(TypedDict):
    """Explicit applicability and exported API RP 1111 check values."""

    in_scope: bool
    method: str
    reason: str
    checks: list[CombinedCheck]


class Screening(TypedDict):
    """Additional inputs required by the per-size strength output contract."""

    condition: Literal["functional", "extreme"]
    dynamic: bool
    qualified: bool
    u1: U1Inputs
    tension_envelope_kN: TensionEnvelope
    hang_off_dynamic_min_kN: float | None
    excursion_reference_deg: float
    excursion_limit_deg: float
    static_check: StaticCheck | None
    api_rp_1111: CombinedLoading


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
    screening: NotRequired[Screening]


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
    if "screening" in data:
        _screening_semantics(data)


def _screening_semantics(data: dict[str, Any]) -> None:
    screen = data["screening"]
    count = len(data["arc_length_m"])
    dynamic = screen["dynamic"]
    if dynamic and screen["condition"] != "extreme":
        raise ValueError("dynamic screening requires the extreme condition")
    for state, fibres in screen["u1"]["stress_MPa"].items():
        if fibres is None:
            if state == "static" or dynamic:
                raise ValueError("missing U1 stress state")
        elif state == "dynamic_max" and not dynamic:
            raise ValueError("static record contains dynamic U1 stresses")
        elif any(len(values) != count for values in fibres.values()):
            raise ValueError("U1 fibre length differs from arc_length_m")
    envelope = screen["tension_envelope_kN"]
    for values in envelope.values():
        if dynamic and (values is None or len(values) != count):
            raise ValueError("missing or misaligned dynamic tension envelope")
        if not dynamic and values is not None:
            raise ValueError("static record contains a dynamic tension envelope")
    if dynamic:
        if any(a > b for a, b in zip(envelope["dynamic_min"], envelope["dynamic_max"])):
            raise ValueError("dynamic tension envelope is reversed")
        for actual, expected in [(screen["hang_off_dynamic_min_kN"], envelope["dynamic_min"][0]),
                                 (data["hang_off_tension_kN"]["dynamic_max"], envelope["dynamic_max"][0])]:
            if actual is None or not math.isclose(actual, expected, rel_tol=ROUND_OFF, abs_tol=ROUND_OFF):
                raise ValueError("hang-off tension differs from envelope at arc zero")
    elif screen["hang_off_dynamic_min_kN"] is not None:
        raise ValueError("static record contains a dynamic hang-off minimum")
    if not math.isclose(data["hang_off_tension_kN"]["static"], data["effective_tension_kN"][0],
                        rel_tol=ROUND_OFF, abs_tol=ROUND_OFF):
        raise ValueError("static hang-off tension differs from profile at arc zero")
    checks = screen["api_rp_1111"]
    if bool(checks["checks"]) != checks["in_scope"]:
        raise ValueError("combined-loading applicability differs from check rows")
    if any(not 0 <= check["arc_length_m"] <= data["arc_length_m"][-1] for check in checks["checks"]):
        raise ValueError("combined-loading check outside exported arc")


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
