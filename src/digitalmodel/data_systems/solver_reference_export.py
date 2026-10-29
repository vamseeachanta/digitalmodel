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

from_json() and from_csv() accept source rows or previously exported rows.
Readers reject duplicate keys/columns and inconsistent export annotations before
returning an exporter. Readback establishes structural fidelity only: source
claims, execution metadata and export annotations never authenticate a fresh run.
Floating-point tokens use Python float precision with ordinary rounding. A
nonzero token that rounds to zero causes the entire input to be rejected; supply
it as source text to retain it unchanged (classified invalid in the value field).
Already-decoded caller values have no source token to check for precision loss.
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


def _unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    """Reject duplicate JSON members before a parser can discard evidence."""
    # RFC 8259 (2017), section 4: duplicate member names are not interoperable.
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate JSON member in source evidence")
        result[key] = value
    return result


def _nonfinite_constant(token: str) -> None:
    """Reject nonfinite JSON extensions without echoing source evidence."""
    # RFC 8259 (2017), section 6 excludes NaN and Infinity numeric tokens.
    raise ValueError("Source evidence must be JSON-compatible and finite")


def _source_float(token: str) -> float:
    """Reject nonzero source tokens that cannot survive as a nonzero float."""
    value = float(token)
    significand = token.lower().split("e", 1)[0]
    if value == 0.0 and any(digit in "123456789" for digit in significand):
        raise ValueError("Source numeric evidence would underflow to zero")
    return value


def _read_json(text: str) -> Any:
    """Decode JSON without silently dropping members or accepting extensions."""
    return json.loads(
        text,
        object_pairs_hook=_unique_object,
        parse_constant=_nonfinite_constant,
        parse_float=_source_float,
    )


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

    @classmethod
    def _from_records(
        cls,
        records: list[Any],
        supported_units: Mapping[str, Collection[str]],
    ) -> SolverReferenceExporter:
        """Recompute annotations and reject incomplete or contradictory readback."""
        sources, annotations = [], []
        for record in records:
            source = _snapshot(record)
            supplied = {key: source.pop(key) for key in _RESERVED if key in source}
            if supplied and supplied.keys() != _RESERVED:
                raise ValueError("Incomplete export annotations")
            sources.append(source)
            annotations.append(supplied)
        exporter = cls(sources, supported_units=supported_units)
        for supplied, actual in zip(annotations, exporter._rows):
            if supplied and supplied != {key: actual[key] for key in _RESERVED}:
                raise ValueError("Inconsistent export annotations")
        return exporter

    @classmethod
    def from_json(
        cls,
        text: str,
        *,
        supported_units: Mapping[str, Collection[str]],
    ) -> SolverReferenceExporter:
        """Read a JSON array of source or exported rows using the same contract.

        Duplicate members, unsupported units and inconsistent annotations raise
        ValueError. Consistent annotations are recomputed, not trusted as proof
        of execution, applicability or qualification.
        """
        if not isinstance(text, str):
            raise ValueError("JSON input must be text")
        records = _read_json(text)
        if not isinstance(records, list):
            raise ValueError("JSON input must be an array of reference rows")
        return cls._from_records(records, supported_units)

    @classmethod
    def from_csv(
        cls,
        text: str,
        *,
        supported_units: Mapping[str, Collection[str]],
    ) -> SolverReferenceExporter:
        """Read the JSON-cell CSV contract without coercing missing evidence.

        Empty cells denote absent keys; JSON null denotes explicit null. Header
        names must be unique and every record must have the header's width.
        Plain numeric-table CSV requires an explicit adapter before this reader.
        """
        if not isinstance(text, str):
            raise ValueError("CSV input must be text")
        if not text:
            return cls([], supported_units=supported_units)
        reader = csv.reader(io.StringIO(text, newline=""), strict=True)
        records = []
        try:
            fields = next(reader)
            if len(fields) != len(set(fields)):
                raise ValueError("Duplicate CSV column in source evidence")
            if not {"condition_id", "quantity", "source_unit"}.issubset(fields):
                raise ValueError("CSV header is missing required reference fields")
            for cells in reader:
                if len(cells) != len(fields):
                    raise ValueError("CSV row width differs from header")
                records.append(
                    {
                        key: _read_json(cell)
                        for key, cell in zip(fields, cells)
                        if cell != ""
                    }
                )
        except csv.Error as error:
            raise ValueError("Malformed solver-reference CSV") from error
        return cls._from_records(records, supported_units)

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
