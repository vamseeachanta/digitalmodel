# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline solver-reference serialization preserving source evidence (issue 2226).

Rows are JSON-compatible mappings with condition_id, quantity and source_unit.
Optional value_state uses computed, absent, invalid, uncomputed,
cached_unqualified or unknown. Unknown vendor states remain in the source field
and export as unknown. Missing warning evidence stays missing; it is never
replaced with an empty list. Every other source field is retained unchanged.

The caller supplies supported units per quantity from its own sourced contract;
no conversions or inferred units are provided. This is an export utility, not a
source catalog or dataset registry. Existing identities and provenance pass
through without creating new identities or claiming to verify their evidence.

export_value_state describes available evidence; export_evaluation is always
NOT_EVALUATED. Even computed source values, an empty warning list or a successful
helper exit cannot establish applicability, freshness or engineering acceptance.
The raw value remains evidence, including zeros in uncomputed or cached rows.

CSV uses JSON in each present cell, including strings and warning lists. A blank
cell means the key was absent, whereas JSON null means an explicit null. Parse
cells with json.loads to reconstruct the same records as to_json(). Nonfinite
numbers must be supplied as explicit source text (RFC 8259, section 6, 2017);
that text is retained and classified invalid, without inventing a numeric zero.
"""

from __future__ import annotations

import csv
import io
import json
from collections.abc import Collection, Iterable, Mapping
from typing import Any

_VALUE_STATES = frozenset(
    {
        "computed",
        "absent",
        "invalid",
        "uncomputed",
        "cached_unqualified",
        "unknown",
    }
)
_RESERVED = frozenset({"export_value_state", "export_evaluation"})


def _snapshot(row: Mapping[str, Any]) -> dict[str, Any]:
    """Copy strict JSON evidence without silently coercing source types or keys."""
    if not isinstance(row, Mapping):
        raise ValueError("Each reference row must be a mapping")
    original = dict(row)
    try:
        copied = json.loads(json.dumps(original, allow_nan=False))
    except (ValueError, TypeError) as error:
        raise ValueError(
            "Source evidence must be JSON-compatible and finite"
        ) from error
    if copied != original:
        raise ValueError("Source evidence requires JSON string keys and array lists")
    return copied


def _prepare(row: Mapping[str, Any], units: Mapping[str, Collection[str]]) -> dict:
    result = _snapshot(row)
    if _RESERVED.intersection(result):
        raise ValueError("Source contains reserved export annotations")
    for field in ("condition_id", "quantity"):
        if not isinstance(result.get(field), str) or not result[field].strip():
            raise ValueError(f"Missing or invalid {field}")
    allowed = units.get(result["quantity"], ())
    unit = result.get("source_unit")
    if (
        isinstance(allowed, str)
        or not isinstance(unit, str)
        or not unit.strip()
        or unit not in allowed
    ):
        raise ValueError("Unsupported or undeclared source unit for quantity")
    warnings = result.get("warnings")
    if warnings is not None and (
        not isinstance(warnings, list)
        or any(not isinstance(warning, str) for warning in warnings)
    ):
        raise ValueError("warnings must be a list of strings or null")
    state = result.get("value_state", "unknown")
    if not isinstance(state, str):
        raise ValueError("value_state must be a string when supplied")
    value = result.get("value")
    if value is None:
        state = "absent"
    elif type(value) not in (int, float):
        state = "invalid"
    elif state not in _VALUE_STATES:
        state = "unknown"
    result["export_value_state"] = state
    result["export_evaluation"] = "NOT_EVALUATED"
    return result


class SolverReferenceExporter:
    """Snapshot condition records for JSON/CSV without running or qualifying a solver.

    Args:
        rows: Source records, retaining existing identifiers and evidence fields.
        supported_units: Explicit accepted source-unit spellings per quantity.
            Matching is exact; no aliases, dimension guesses or conversions.
    """

    def __init__(
        self,
        rows: Iterable[Mapping[str, Any]],
        *,
        supported_units: Mapping[str, Collection[str]],
    ) -> None:
        """Validate and copy every row before exposing an export."""
        if not isinstance(supported_units, Mapping):
            raise ValueError("supported_units must map quantities to unit collections")
        for allowed in supported_units.values():
            if (
                isinstance(allowed, str)
                or not isinstance(allowed, Collection)
                or any(not isinstance(unit, str) for unit in allowed)
            ):
                raise ValueError("supported_units requires collections of unit strings")
        self._rows = [_prepare(row, supported_units) for row in rows]

    def to_json(self) -> str:
        """Return strict JSON with all source fields and explicit export states."""
        return json.dumps(self._rows, allow_nan=False, indent=2) + "\n"

    def to_csv(self) -> str:
        """Return CSV with JSON cells preserving missing/null and warning lists."""
        if not self._rows:
            return ""
        fields = list(dict.fromkeys(key for row in self._rows for key in row))
        stream = io.StringIO(newline="")
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        for row in self._rows:
            writer.writerow(
                {key: json.dumps(value, allow_nan=False) for key, value in row.items()}
            )
        return stream.getvalue()
