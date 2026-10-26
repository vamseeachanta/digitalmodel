# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Typed exchange records and validation for exported strength and fatigue."""

from __future__ import annotations

import json
import math
import re
from datetime import datetime
from importlib.resources import files
from pathlib import Path
from typing import Any, NotRequired, TypedDict

from jsonschema import Draft202012Validator, FormatChecker


FORMAT_CHECKER = FormatChecker()


@FORMAT_CHECKER.checks("date-time", raises=ValueError)
def _date_time(value: Any) -> bool:
    # Check explicitly: jsonschema's optional date-time dependency may be
    # absent. Require RFC3339 seconds and a timezone instead of silently
    # accepting any string when that optional checker is unavailable.
    if not isinstance(value, str):
        return True
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}[Tt]\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:[Zz]|[+-]\d{2}:\d{2})", value):
        return False
    datetime.fromisoformat(value.upper().replace("Z", "+00:00"))
    return True


class CampaignRun(TypedDict):
    """Explicit attempt, completion and qualification status in the run ledger."""

    id: str
    phase: str
    status: str
    qualified: bool


class RunRecord(TypedDict):
    """Exporter provenance; the digest identifies the input bytes."""

    input_sha256: str
    result_file: str
    date: str
    id: NotRequired[str]
    phase: NotRequired[str]
    status: NotRequired[str]
    qualified: NotRequired[bool]


class Software(TypedDict):
    """Software identity reported by the exporting process."""

    name: str
    version: str


class LoadCase(TypedDict):
    """Load identifiers and unit-bearing exporter metadata."""

    id: str
    wave: dict[str, Any]
    current: dict[str, Any]
    offset: dict[str, Any]


class Allowable(TypedDict):
    """Recorded stress allowable and its basis, without inferred defaults."""

    value: float
    unit: str
    basis_label: str
    source: str


class MaximumUtilisation(TypedDict):
    """Sampled global maximum and its physical location."""

    value: float
    arc_length_m: float
    location: str


class Touchdown(TypedDict):
    """Touchdown response in the exported strength case."""

    arc_length_m: float
    utilisation: float
    stress_range_MPa: float


class TopAngle(TypedDict):
    """Static angle and dynamic extrema, in degrees."""

    static: float
    dynamic_min: float
    dynamic_max: float


class HangOffTension(TypedDict):
    """Static and maximum dynamic effective tension, in kilonewtons."""

    static: float
    dynamic_max: float


class StrengthResult(TypedDict):
    """One strength export per case; arrays share the arc-length grid."""

    case_id: str
    software: Software
    load_case: LoadCase
    arc_length_m: list[float]
    effective_tension_kN: list[float]
    bending_moment_kNm: list[float]
    von_mises_utilisation: list[float]
    utilisation_basis: NotRequired[str]
    method: str
    allowable: Allowable
    max_utilisation: MaximumUtilisation
    touchdown: Touchdown
    top_angle_deg: TopAngle
    hang_off_tension_kN: HangOffTension
    run: RunRecord


class SeaState(TypedDict):
    """One fatigue sensitivity sea state with fractional occurrence."""

    id: str
    Hs_m: float
    Tp_s: float
    direction_deg: float
    occurrence: float


class RangeBin(TypedDict):
    """Stress-range histogram bin, already annualised by the exporter."""

    stress_range_MPa: float
    cycles_per_year: float


class HotSpot(TypedDict):
    """Touchdown or hang-off response, with scalar range or histogram."""

    name: str
    arc_length_m: float
    cycles_per_year: float
    scf: float
    stress_range_MPa: NotRequired[float]
    range_histogram: NotRequired[list[RangeBin]]
    damage_per_year: NotRequired[float]


class SNCurve(TypedDict):
    """Exporter-supplied curve parameters; this package computes no damage."""

    name: str
    source: str
    m: float
    log_a: float
    thickness_correction: float


class FatigueResult(TypedDict):
    """Annual damage per case and sea state; legacy life is optional."""

    case_id: str
    sea_state: SeaState
    hot_spots: list[HotSpot]
    sn_curve: SNCurve
    damage_per_year: float
    life_years: NotRequired[float]
    run: RunRecord


def _unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON object key")
        result[key] = value
    return result


def _reject_constant(_value: str) -> None:
    raise ValueError("non-finite JSON number")


def load_json(path: Path) -> Any:
    """Read JSON, rejecting duplicate keys and non-finite literal numbers."""
    return json.loads(Path(path).read_text(encoding="utf-8"),
                      object_pairs_hook=_unique_object, parse_constant=_reject_constant)


def _finite_tree(value: Any) -> None:
    if isinstance(value, float) and not math.isfinite(value):
        raise ValueError("non-finite numeric value")
    if isinstance(value, dict):
        for item in value.values():
            _finite_tree(item)
    elif isinstance(value, list):
        for item in value:
            _finite_tree(item)


def validate_result(data: dict[str, Any], kind: str) -> None:
    """Validate structure, formats, finite values and sampled strength maxima.

    The JSON schema is the structural authority. Exact sample comparisons are
    intentional: these fields identify values from the same exported array,
    rather than independently calculated or rounded engineering quantities.
    """
    if kind not in ("strength", "fatigue"):
        raise ValueError("result kind must be strength or fatigue")
    _validate_schema(data, kind)
    if kind == "strength":
        arc = data["arc_length_m"]
        if any(b <= a for a, b in zip(arc, arc[1:])):
            raise ValueError("arc_length_m must be strictly increasing")
        for key in ("effective_tension_kN", "bending_moment_kNm", "von_mises_utilisation"):
            if len(data[key]) != len(arc):
                raise ValueError("strength arrays must share the arc-length grid")
        peak = data["max_utilisation"]
        util = data["von_mises_utilisation"]
        if peak["value"] != max(util) or not any(
            s == peak["arc_length_m"] and u == peak["value"] for s, u in zip(arc, util)
        ):
            raise ValueError("max_utilisation must identify a sampled global maximum")
        if not arc[0] <= data["touchdown"]["arc_length_m"] <= arc[-1]:
            raise ValueError("touchdown must lie within the exported arc-length range")
        angle = data["top_angle_deg"]
        if angle["dynamic_min"] > angle["dynamic_max"]:
            raise ValueError("dynamic angle extrema are reversed")


def _validate_schema(data: dict[str, Any], kind: str) -> None:
    """Apply the single shared schema to one exchange record."""
    _finite_tree(data)
    schema = json.loads(files(__package__).joinpath("results.schema.json").read_text())
    validator = Draft202012Validator(
        {"$ref": f"#/$defs/{kind}", "$defs": schema["$defs"]},
        format_checker=FORMAT_CHECKER,
    )
    errors = list(validator.iter_errors(data))
    if errors:
        # Do not echo raw exporter values, paths or arbitrary metadata.
        raise ValueError(f"{kind} record violates exchange schema")


def validate_campaign_run(data: dict[str, Any]) -> None:
    """Validate an explicit run status without inferring qualification."""
    _validate_schema(data, "campaign_run")
