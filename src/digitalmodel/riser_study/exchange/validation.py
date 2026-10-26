# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Schema and cross-field validation; errors omit untrusted payload values."""

from __future__ import annotations

from datetime import datetime
import json
import math
from pathlib import Path, PurePosixPath
import re
from typing import Any, Literal, overload

from jsonschema import Draft202012Validator, FormatChecker

from .records import FatigueResult, StrengthResult

VERSION = "ris-exchange/1"
ROUND_OFF = 1e-9  # Serialization tolerance, not an engineering acceptance band.
_FORMATS = FormatChecker()


@_FORMATS.checks("date-time", raises=ValueError)
def _date_time(value: Any) -> bool:
    """Check timezone components before datetime can normalize invalid offsets."""
    if not isinstance(value, str):
        return True
    if not re.fullmatch(
        r"\d{4}-\d{2}-\d{2}[Tt]\d{2}:\d{2}:\d{2}(?:\.\d+)?"
        r"(?:[Zz]|[+-](?:[01]\d|2[0-3]):[0-5]\d)", value
    ):
        return False
    datetime.fromisoformat(value.upper().replace("Z", "+00:00"))
    return True


def _unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON object key")
        result[key] = value
    return result


def _reject_constant(_value: str) -> None:
    raise ValueError("non-finite JSON number")


def _finite_tree(value: Any) -> None:
    if isinstance(value, float) and not math.isfinite(value):
        raise ValueError("non-finite numeric value")
    if isinstance(value, dict):
        for child in value.values():
            _finite_tree(child)
    elif isinstance(value, list):
        for child in value:
            _finite_tree(child)


def parse_json(payload: str | bytes) -> Any:
    """Decode a captured payload once, rejecting duplicates and nonfinite values."""
    try:
        text = payload.decode("utf-8") if isinstance(payload, bytes) else payload
        value = json.loads(text, object_pairs_hook=_unique_object,
                           parse_constant=_reject_constant)
    except (json.JSONDecodeError, UnicodeError):
        raise ValueError("invalid JSON encoding or syntax") from None
    _finite_tree(value)
    return value


def load_json(path: Path) -> Any:
    """Read strict UTF-8 JSON without echoing input content in parse errors."""
    return parse_json(Path(path).read_bytes())


def portable_path(value: str) -> str:
    """Require relative POSIX provenance; never resolve or open that locator."""
    if (not value or PurePosixPath(value).is_absolute() or "\\" in value or ":" in value
            or any(part in {"", ".", ".."} for part in value.split("/"))
            or any(ord(char) < 32 for char in value)):
        raise ValueError("result_file must be a portable relative path")
    return value


def exchange_schema(kind: str) -> dict[str, Any]:
    """Return a fresh standalone schema; no network references are followed."""
    if kind not in {"strength", "fatigue", "campaign_run", "g4_checks"}:
        raise ValueError("unknown result kind")
    source = "strength" if kind in {"strength", "campaign_run"} else "fatigue"
    schema = load_json(Path(__file__).with_name(f"{source}.schema.json"))
    schema["$ref"] = f"#/$defs/{kind}"
    return schema


def _validate_schema(data: Any, kind: str) -> None:
    _finite_tree(data)
    error = next(Draft202012Validator(exchange_schema(kind), format_checker=_FORMATS)
                 .iter_errors(data), None)
    if error is not None:
        # Schema paths contain trusted field names; instance keys/values are omitted.
        rule = error.validator
        raise ValueError(f"{kind}: schema rule {rule}")


def validate_campaign_run(data: Any) -> None:
    """Validate explicit ledger status, including completion before qualification."""
    _validate_schema(data, "campaign_run")


@overload
def validate_result(data: Any, kind: Literal["strength"]) -> StrengthResult: ...


@overload
def validate_result(data: Any, kind: Literal["fatigue"]) -> FatigueResult: ...


@overload
def validate_result(data: Any, kind: Literal["g4_checks"]) -> dict[str, Any]: ...


def validate_result(data: Any, kind: str) -> Any:
    """Validate and return the original record, preserving supported field forms."""
    if kind not in {"strength", "fatigue", "g4_checks"}:
        raise ValueError("unknown result kind")
    if isinstance(data, dict):
        if kind == "strength" and ({"method", "allowable"} & data.keys()) and (
                {"utilisation_method", "utilisation_allowable"} & data.keys()):
            raise ValueError("conflicting strength basis forms; use one complete pair")
        curve = data.get("sn_curve")
        if isinstance(curve, dict) and "source" in curve and "source_label" in curve:
            raise ValueError("conflicting curve source forms; use source or source_label")
    _validate_schema(data, kind)
    if kind == "g4_checks":
        return data
    portable_path(data["run"]["result_file"])
    if kind == "strength":
        _strength_semantics(data)
    else:
        _fatigue_semantics(data)
    return data


def load_result(path: Path, kind: Literal["strength", "fatigue"]) -> StrengthResult | FatigueResult:
    """Read one export without conversion, defaults, recalculation or solver imports."""
    return validate_result(load_json(path), kind)


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
    if touchdown["utilisation"] > maximum["value"] and not math.isclose(
            touchdown["utilisation"], maximum["value"], rel_tol=ROUND_OFF, abs_tol=ROUND_OFF):
        raise ValueError("touchdown utilisation exceeds maximum")
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
    damage = data["damage_per_year"]
    if "life_years" in data:
        life = data["life_years"]
        if damage == 0:
            if life is not None:
                raise ValueError("zero damage requires null life_years")
        elif life is None or not math.isclose(damage * life, 1, rel_tol=ROUND_OFF, abs_tol=0):
            raise ValueError("life_years must equal reciprocal annual damage")
    names = [spot["name"] for spot in data["hot_spots"]]
    if len(names) != len(set(names)):
        raise ValueError("duplicate hot-spot name")
    points = [*data["hot_spots"], *data.get("arc_distribution", [])]
    for point in points:
        if "range_histogram" in point:
            try:
                total = math.fsum(row["cycles_per_year"] for row in point["range_histogram"])
            except OverflowError:
                raise ValueError("histogram total exceeds numeric range") from None
            if not math.isclose(total, point["cycles_per_year"], rel_tol=ROUND_OFF, abs_tol=0):
                raise ValueError("histogram annual cycles differ from station total")
    if "arc_distribution" in data:
        arc = [point["arc_length_m"] for point in data["arc_distribution"]]
        if any(a >= b for a, b in zip(arc, arc[1:])):
            raise ValueError("arc_distribution must increase strictly")
