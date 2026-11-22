# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Enumerate explicit load cases without a solver or design standard.

Caller-defined enumeration is not standards compliance. No required headings,
environmental values, return periods or combinations are inferred here.
"""

from __future__ import annotations

import json
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from hashlib import sha256
from itertools import product
from math import isfinite
from numbers import Real
from types import MappingProxyType
from typing import TypeAlias, cast


EnvironmentalValue: TypeAlias = (
    None | bool | str | int | float
    | Mapping[str, "EnvironmentalValue"]
    | Sequence["EnvironmentalValue"]
)


@dataclass(frozen=True, slots=True)
class LoadCase:
    """Detached case with read-only mappings and tuple-valued sequences.

    Attributes:
        case_id: Content identifier including the basis and environmental data.
        basis_id: Caller-supplied basis, carried without interpreting its meaning.
        heading: Finite numeric heading, with no angular wrapping or unit change.
        return_period: Opaque, case-sensitive return-period label.
        environmental_case: Opaque, case-sensitive environmental case label.
        environment: Recursively immutable copy of the environmental mapping.
    """

    case_id: str
    basis_id: str
    heading: float
    return_period: str
    environmental_case: str
    environment: Mapping[str, EnvironmentalValue]


def _label(value: object, axis: str) -> str:
    """Require explicit labels without silent trimming or coercion."""
    if (
        not isinstance(value, str)
        or not value
        or value != value.strip()
        or not value.isprintable()
    ):
        raise ValueError(f"{axis}: labels must be nonempty printable strings "
                         "without surrounding whitespace")
    return value


def _axis(values: object, name: str) -> tuple[object, ...]:
    """Snapshot an ordered axis, rejecting strings and unordered containers."""
    if (
        not isinstance(values, Sequence)
        or isinstance(values, (str, bytes, bytearray))
        or not values
    ):
        raise ValueError(f"{name}: expected a nonempty ordered sequence")
    return tuple(values)


def _freeze(value: object, ancestors: frozenset[int] = frozenset()) -> EnvironmentalValue:
    """Copy supported values recursively, rejecting cycles and nonfinite data."""
    error = "environmental_cases: expected finite data with printable string keys"
    if value is None:
        return None
    if type(value) is bool or type(value) is str or type(value) is int:
        return value
    if type(value) is float:
        if not isfinite(value):
            raise ValueError(error)
        return value
    if id(value) in ancestors:
        raise ValueError("environmental_cases: cyclic data is not supported")
    ancestors = ancestors | {id(value)}
    if isinstance(value, Mapping):
        return MappingProxyType({
            _label(key, "environmental_cases"): _freeze(item, ancestors)
            for key, item in value.items()
        })
    if isinstance(value, (list, tuple)):
        return tuple(_freeze(item, ancestors) for item in value)
    raise ValueError(error)


def _json_value(value: EnvironmentalValue) -> object:
    """Represent frozen data in JSON without relying on object representations."""
    if isinstance(value, Mapping):
        return {key: _json_value(item) for key, item in value.items()}
    if isinstance(value, tuple):
        return [_json_value(item) for item in value]
    return value


def enumerate_load_cases(
    *,
    headings: Sequence[Real],
    return_periods: Sequence[str],
    environmental_cases: Mapping[str, Mapping[str, EnvironmentalValue]],
    basis_id: str,
) -> tuple[LoadCase, ...]:
    """Return the Cartesian product of caller-defined axes in input order.

    Headings vary slowest, then return periods, then environmental cases in
    mapping insertion order. Headings are converted to floats; equivalent
    numeric headings (including signed zero) are duplicates. Angles are not
    wrapped, so the caller is responsible for angular conventions and units.

    Labels are case-sensitive and must be nonempty printable strings without
    surrounding whitespace. Environmental values support string-keyed mappings,
    lists, tuples, strings, booleans, integers, finite floats and None. Lists and
    tuples become tuples; mappings become read-only copies. Unsupported values
    and cyclic containers are rejected rather than retained by reference.

    Identifiers use the full SHA-256 hexadecimal digest of a canonical JSON
    array containing basis, heading, period, environmental label and data, with
    sorted mapping keys and compact separators. Mapping order and the position
    in an axis do not affect identity. Changes to case content change identity.
    This identifier format is part of this enumeration API's contract.

    Args:
        headings: Nonempty ordered sequence of finite real numbers, not booleans.
        return_periods: Nonempty ordered sequence of unique explicit labels.
        environmental_cases: Nonempty ordered mapping from unique labels to
            nonempty environmental mappings with distinct contents.
        basis_id: Explicit identifier for the caller's basis, not a compliance
            certification or a request to apply a standard.

    Returns:
        Tuple of detached immutable records, one for each combination.

    Raises:
        ValueError: An axis, basis, label or environmental value is invalid, or
            duplicate axis values or environmental contents would repeat cases.
    """
    basis = _label(basis_id, "basis_id")
    directions = []
    for heading in _axis(headings, "headings"):
        if isinstance(heading, bool) or not isinstance(heading, Real):
            raise ValueError("headings: expected finite real numbers, not booleans")
        try:
            direction = float(heading)
        except (ValueError, OverflowError) as exc:
            raise ValueError("headings: must be representable as finite floats") from exc
        if not isfinite(direction):
            raise ValueError("headings: must be finite")
        # Signed zeros compare equal; use the same identity representation.
        directions.append(direction if direction else 0.0)
    if len(set(directions)) != len(directions):
        raise ValueError("headings: duplicate values")

    periods = tuple(
        _label(period, "return_periods")
        for period in _axis(return_periods, "return_periods")
    )
    if len(set(periods)) != len(periods):
        raise ValueError("return_periods: duplicate labels")

    if not isinstance(environmental_cases, Mapping) or not environmental_cases:
        raise ValueError("environmental_cases: expected a nonempty mapping")
    environments = {}
    for label, data in environmental_cases.items():
        label = _label(label, "environmental_cases")
        if not isinstance(data, Mapping) or not data:
            raise ValueError("environmental_cases: each case needs a nonempty mapping")
        frozen = cast(Mapping[str, EnvironmentalValue], _freeze(data))
        if label in environments or frozen in environments.values():
            raise ValueError("environmental_cases: duplicate labels or contents")
        environments[label] = frozen

    cases = []
    identifiers = set()
    for heading, period, (label, environment) in product(
        directions, periods, environments.items()
    ):
        payload = json.dumps(
            [basis, heading, period, label, _json_value(environment)],
            sort_keys=True, separators=(",", ":"), ensure_ascii=True, allow_nan=False,
        )
        case_id = "dlc-" + sha256(payload.encode("utf-8")).hexdigest()
        if case_id in identifiers:
            raise ValueError("environmental_cases: case identifier collision")
        identifiers.add(case_id)
        cases.append(LoadCase(case_id, basis, heading, period, label, environment))
    return tuple(cases)
