# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Shared strength/fatigue export types and JSON Schema validation.

Field names and units follow the riser results-exchange contract. Fatigue
cycles_per_year means annual cycles at continuous exposure to that state;
occurrence is the exposure fraction applied by the consumer, exactly once.
Range histograms have annual bin counts summing to cycles_per_year. A scalar
range represents all those cycles. Stress is nominal before the recorded SCF.
The optional arc_distribution uses the same stress/count/SCF representation.

The strength array von_mises_utilisation retains its specified array shape;
utilisation_method and utilisation_allowable state its basis separately.
No default curve, material parameter, safety factor or exposure is supplied.
"""

from __future__ import annotations

import json
import math
from functools import lru_cache
from pathlib import Path
from typing import Literal, NotRequired, TypedDict

from jsonschema import Draft202012Validator, FormatChecker


class RunRecord(TypedDict):
    """Export provenance; result_file is the recorded source locator."""

    input_sha256: str
    result_file: str
    date: str
    data_basis: NotRequired[str]


class SeaState(TypedDict):
    """Sea state and annual exposure fraction, without normalization."""

    id: str
    Hs_m: float
    Tp_s: float
    direction_deg: float
    occurrence: float


class ThicknessCorrection(TypedDict):
    """Explicit power-law correction; no implicit reference or clipping."""

    thickness_mm: float
    reference_thickness_mm: float
    exponent: float
    source_label: str


class SNCurve(TypedDict):
    """Single-slope curve log10(N) = log_a - m*log10(range in MPa)."""

    name: str
    source_label: str
    m: float
    log_a: float
    thickness_correction: NotRequired[ThicknessCorrection | None]
    reference_cycles: NotRequired[float]
    reference_range_MPa: NotRequired[float]
    endurance_limit_MPa: NotRequired[float | None]


class RangeBin(TypedDict):
    """Nominal range and annual cycles at continuous sea-state exposure."""

    stress_range_MPa: float
    cycles_per_year: float


class ArcPoint(TypedDict):
    """Exactly one of stress_range_MPa and range_histogram is present."""

    arc_length_m: float
    cycles_per_year: float
    scf: float
    stress_range_MPa: NotRequired[float]
    range_histogram: NotRequired[list[RangeBin]]


class HotSpot(ArcPoint):
    """Named physical location, not an inferred arc coordinate."""

    name: Literal["touchdown", "hang_off"]


class FatigueResult(TypedDict):
    """Recorded benchmark damage and calculation inputs; legacy reciprocal optional."""

    case_id: str
    sea_state: SeaState
    hot_spots: list[HotSpot]
    sn_curve: SNCurve
    damage_per_year: float
    life_years: NotRequired[float | None]
    run: RunRecord
    arc_distribution: NotRequired[list[ArcPoint]]


class Software(TypedDict):
    """Exporting software identity."""

    name: str
    version: str


class LoadCase(TypedDict):
    """Recorded load case; wave, current and offset retain exporter fields."""

    id: str
    wave: dict
    current: dict
    offset: dict


class MaximumUtilisation(TypedDict):
    """Maximum along the exported arc."""

    value: float
    arc_length_m: float
    location: str


class Touchdown(TypedDict):
    """Strength and stress-range output at touchdown."""

    arc_length_m: float
    utilisation: float
    stress_range_MPa: float


class TopAngle(TypedDict):
    """Static angle and dynamic extrema in degrees."""

    static: float
    dynamic_min: float
    dynamic_max: float


class HangOffTension(TypedDict):
    """Static and maximum dynamic tension in kN."""

    static: float
    dynamic_max: float


class StrengthResult(TypedDict):
    """Aligned strength arrays with a recorded utilisation basis."""

    case_id: str
    software: Software
    load_case: LoadCase
    arc_length_m: list[float]
    effective_tension_kN: list[float]
    bending_moment_kNm: list[float]
    von_mises_utilisation: list[float]
    utilisation_method: str
    utilisation_allowable: float
    max_utilisation: MaximumUtilisation
    touchdown: Touchdown
    top_angle_deg: TopAngle
    hang_off_tension_kN: HangOffTension
    run: RunRecord


@lru_cache(maxsize=1)
def exchange_schema() -> dict:
    """Read the bundled JSON Schema without network resolution."""
    return json.loads(Path(__file__).with_suffix(".schema.json").read_text(encoding="utf-8"))


def _finite(value: object) -> None:
    if isinstance(value, float) and not math.isfinite(value):
        raise ValueError("result contains a nonfinite number")
    if isinstance(value, dict):
        for item in value.values():
            _finite(item)
    elif isinstance(value, list):
        for item in value:
            _finite(item)


def _increasing(values: list[float]) -> None:
    if any(a >= b for a, b in zip(values, values[1:])):
        raise ValueError("arc length must be strictly increasing")


def validate_result(data: dict, kind: Literal["fatigue", "strength", "g4_checks"]) -> None:
    """Validate field types, finiteness and cross-field export invariants.

    Raises:
        ValueError: If schema, arc alignment, names or cycle totals are invalid.
    """
    if kind not in ("fatigue", "strength", "g4_checks"):
        raise ValueError("unknown result kind")
    _finite(data)
    document = exchange_schema()
    selected = {"$ref": f"#/$defs/{kind}", "$defs": document["$defs"]}
    error = next(Draft202012Validator(selected, format_checker=FormatChecker()).iter_errors(data), None)
    if error is not None:
        # Report only field location and rule; never echo untrusted export values.
        location = ".".join(str(part) for part in error.absolute_path) or "result"
        raise ValueError(f"{kind}: {location}: {error.validator}")
    if kind == "g4_checks":
        return
    if kind == "strength":
        arc = data["arc_length_m"]
        _increasing(arc)
        for field in ("effective_tension_kN", "bending_moment_kNm", "von_mises_utilisation"):
            if len(data[field]) != len(arc):
                raise ValueError("strength array length differs from arc length")
        if data["top_angle_deg"]["dynamic_min"] > data["top_angle_deg"]["dynamic_max"]:
            raise ValueError("dynamic angle extrema are reversed")
        return
    spots = data["hot_spots"]
    if len({point["name"] for point in spots}) != len(spots):
        raise ValueError("duplicate hot spot name")
    points = [*spots, *data.get("arc_distribution", [])]
    for point in points:
        if "range_histogram" in point:
            total = math.fsum(item["cycles_per_year"] for item in point["range_histogram"])
            # Numerical tolerance for summing serialized annual cycle counts.
            if not math.isclose(total, point["cycles_per_year"], rel_tol=1e-9, abs_tol=0):
                raise ValueError("histogram cycles differ from cycles_per_year")
    if "arc_distribution" in data:
        _increasing([point["arc_length_m"] for point in data["arc_distribution"]])
    if "life_years" in data and (data["damage_per_year"] == 0) != (data["life_years"] is None):
        raise ValueError("life_years must be null exactly when damage is zero")
