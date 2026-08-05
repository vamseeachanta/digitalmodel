"""Validation helpers for the authored OpenFOAM case schema."""

from __future__ import annotations

import math
import re
from typing import Any, Iterable, Mapping, Sequence


class ValidationError(ValueError):
    """Internal validation failure translated by the public parser."""


_SAFE_NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]{0,63}")
_WINDOWS_DEVICES = {"CON", "PRN", "AUX", "NUL"} | {
    f"{prefix}{number}"
    for prefix in ("COM", "LPT")
    for number in range(1, 10)
}


def require_mapping(value: Any, path: str) -> Mapping[str, Any]:
    """Require a mapping with string keys."""
    if not isinstance(value, Mapping):
        raise ValidationError(f"{path} must be a mapping")
    for key in value:
        if not isinstance(key, str):
            raise ValidationError(f"{path} contains non-string key {key!r}")
    return value


def check_keys(
    value: Mapping[str, Any],
    *,
    allowed: Iterable[str],
    required: Iterable[str] = (),
    path: str,
) -> None:
    """Reject unknown keys and report the first missing required key."""
    allowed_set = set(allowed)
    for key in value:
        if key not in allowed_set:
            raise ValidationError(f"unknown key {path}.{key}")
    for key in required:
        if key not in value:
            raise ValidationError(f"missing required key {path}.{key}")


def require_string(value: Any, path: str) -> str:
    """Require a string value."""
    if not isinstance(value, str):
        raise ValidationError(f"{path} must be a string")
    return value


def require_bool(value: Any, path: str) -> bool:
    """Require a boolean value."""
    if type(value) is not bool:
        raise ValidationError(f"{path} must be a boolean")
    return value


def require_int(value: Any, path: str, *, positive: bool = False) -> int:
    """Require an integer, explicitly excluding booleans."""
    if type(value) is bool or not isinstance(value, int):
        raise ValidationError(f"{path} must be an integer")
    if positive and value <= 0:
        raise ValidationError(f"{path} must be positive")
    return value


def require_number(value: Any, path: str, *, positive: bool = False) -> float:
    """Require a finite real number, explicitly excluding booleans."""
    if type(value) is bool or not isinstance(value, (int, float)):
        raise ValidationError(f"{path} must be numeric")
    result = float(value)
    if not math.isfinite(result):
        raise ValidationError(f"{path} must be finite")
    if positive and result <= 0.0:
        raise ValidationError(f"{path} must be positive")
    return result


def require_vector(
    value: Any,
    path: str,
    *,
    integers: bool = False,
    positive: bool = False,
) -> tuple[Any, Any, Any]:
    """Require a finite three-component numeric vector."""
    if isinstance(value, (str, bytes)) or not isinstance(value, Sequence):
        raise ValidationError(f"{path} must be a three-component sequence")
    if len(value) != 3:
        raise ValidationError(f"{path} must contain exactly three entries")
    validator = require_int if integers else require_number
    entries = [validator(item, path, positive=positive) for item in value]
    return entries[0], entries[1], entries[2]


def validate_case_name(value: Any, path: str) -> str:
    """Validate a confined, portable case directory name."""
    name = require_string(value, path)
    if _SAFE_NAME.fullmatch(name) is None or name.upper() in _WINDOWS_DEVICES:
        raise ValidationError(f"{path} is not a safe case name: {name!r}")
    return name


def require_string_sequence(value: Any, path: str) -> tuple[str, ...]:
    """Require a non-empty sequence of non-empty strings."""
    if isinstance(value, (str, bytes)) or not isinstance(value, Sequence):
        raise ValidationError(f"{path} must be a sequence of strings")
    result = tuple(require_string(item, path) for item in value)
    if not result or any(not item for item in result):
        raise ValidationError(f"{path} must contain non-empty strings")
    return result
