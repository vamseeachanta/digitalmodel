# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Configured external E305 tables, with fail-closed Citation sidecars.

No data location or licensed table value is included. Linear interpolation is
an explicit interface convention, whose suitability for a private dataset
requires the provider's verification. Extrapolation is never performed.
"""

from __future__ import annotations

import bisect
import hashlib
import itertools
import json
import math
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any, NoReturn

from digitalmodel.citations.schema import Citation, CitedValue, validate_citation


class TableError(ValueError):
    """Reject missing, malformed or inapplicable external table data."""


def _number(value: Any, *, positive: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise TableError('table numbers must be finite numeric values')
    if not math.isfinite(value) or (positive and value <= 0):
        raise TableError('table numbers violate finite-value or positivity requirements')
    return float(value)


def _object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result = {}
    for key, value in pairs:
        if key in result:
            raise TableError('duplicate JSON key')
        result[key] = value
    return result


def _reject_constant(value: str) -> NoReturn:
    raise TableError('nonfinite JSON number')


@dataclass(frozen=True)
class _Axis:
    name: str
    units: str
    values: tuple[float, ...]


def _axis(raw: Any) -> _Axis:
    if not isinstance(raw, dict) or set(raw) != {'name', 'units', 'values'}:
        raise TableError('axis requires name, units and values')
    if any(not isinstance(raw[key], str) or not raw[key].strip() for key in ('name', 'units')):
        raise TableError('axis name and units must be nonempty')
    if not isinstance(raw['values'], list) or not raw['values']:
        raise TableError('axis values must be a nonempty list')
    values = tuple(_number(value) for value in raw['values'])
    if any(right <= left for left, right in zip(values, values[1:])):
        raise TableError('axis values must be strictly increasing')
    return _Axis(raw['name'], raw['units'], values)


def _flatten(values: Any, shape: tuple[int, ...]) -> tuple[float, ...]:
    if not shape:
        return (_number(values, positive=True),)
    if not isinstance(values, list) or len(values) != shape[0]:
        raise TableError('table values must match their rectangular axes')
    return tuple(number for row in values for number in _flatten(row, shape[1:]))


@dataclass(frozen=True)
class _Table:
    name: str
    axes: tuple[_Axis, ...]
    values: tuple[float, ...]
    units: str
    citation: Citation


def _table(name: str, raw: Any, wiki_root: Path | None) -> _Table:
    if not isinstance(raw, dict) or set(raw) != {'axes', 'values', 'units', 'citation'}:
        raise TableError(f'{name}: axes, values, units and citation are required')
    if not isinstance(raw['axes'], list) or len(raw['axes']) not in (1, 2):
        raise TableError(f'{name}: one or two axes are required')
    axes = tuple(_axis(axis) for axis in raw['axes'])
    if len({axis.name for axis in axes}) != len(axes):
        raise TableError(f'{name}: duplicate axis name')
    if raw['units'] != ('m' if name == 'roughness' else '1'):
        raise TableError(f'{name}: invalid output units')
    if name == 'roughness' and [(a.name, a.units) for a in axes] != [('grain_size_m', 'm')]:
        raise TableError('roughness: a grain_size_m axis in m is required')
    if name == 'Fw' and {(a.name, a.units) for a in axes} != {('K', '1'), ('M', '1')}:
        raise TableError('Fw: dimensionless K and M axes are required')
    try:
        citation = Citation(**raw['citation'])
    except (TypeError, ValueError) as error:
        raise TableError(f'{name}: invalid citation metadata') from error
    if citation.code_id.casefold() != 'dnv-rp-e305' or citation.revision != '1988':
        raise TableError(f'{name}: an E305 1988 citation is required')
    validate_citation(citation, repo_root=wiki_root)
    values = _flatten(raw['values'], tuple(len(axis.values) for axis in axes))
    return _Table(name, axes, values, raw['units'], citation)


def _bracket(axis: _Axis, coordinate: float) -> tuple[tuple[int, float], ...]:
    value = _number(coordinate)
    if value < axis.values[0] or value > axis.values[-1]:
        raise TableError('lookup coordinate outside table domain')
    if len(axis.values) == 1:
        return ((0, 1.0),)
    left = min(max(bisect.bisect_right(axis.values, value) - 1, 0), len(axis.values) - 2)
    fraction = (value - axis.values[left]) / (axis.values[left + 1] - axis.values[left])
    return ((left, 1 - fraction), (left + 1, fraction))


@dataclass(frozen=True)
class TableSet:
    """Immutable table bytes and their resolved calculation-time provenance."""

    tables: tuple[_Table, ...]
    source_sha256: str
    wiki_root: Path | None

    def lookup(self, name: str, **coordinates: float) -> CitedValue:
        """Interpolate declared coordinates and resolve the citation again."""
        table = next((table for table in self.tables if table.name == name), None)
        if table is None:
            raise TableError('required table is absent')
        if set(coordinates) != {axis.name for axis in table.axes}:
            raise TableError('lookup coordinates must match the declared axes')
        validate_citation(table.citation, repo_root=self.wiki_root)
        corners = [_bracket(axis, coordinates[axis.name]) for axis in table.axes]
        value = 0.0
        for corner in itertools.product(*corners):
            offset = 0
            weight = 1.0
            for axis, (index, fraction) in zip(table.axes, corner):
                offset = offset * len(axis.values) + index
                weight *= fraction
            value += weight * table.values[offset]
        return CitedValue(value=value, citation=table.citation, units=table.units)


def load_tables(path: str | Path | None = None, *, wiki_root: Path | None = None) -> TableSet:
    """Load exactly the required tables from an explicit configured location."""
    configured = path if path is not None else os.environ.get('E305_TABLES_FILE')
    if not configured:
        raise TableError('table configuration is absent; set E305_TABLES_FILE or pass a file')
    try:
        with Path(configured).open('rb') as stream:
            raw = stream.read(5_000_001)  # Bounded input, unrelated to standard values.
    except (OSError, TypeError) as error:
        raise TableError('configured table file is absent or unreadable') from error
    if len(raw) > 5_000_000:
        raise TableError('table file exceeds the input-size bound')
    try:
        payload = json.loads(raw, object_pairs_hook=_object, parse_constant=_reject_constant)
    except (json.JSONDecodeError, UnicodeDecodeError) as error:
        raise TableError('malformed table JSON') from error
    if not isinstance(payload, dict) or type(payload.get('schema_version')) is not int or payload['schema_version'] != 1:
        raise TableError('unsupported table schema_version')
    required = {'roughness', 'Fw', 'R', 'clay_friction'}
    if not isinstance(payload.get('tables'), dict) or set(payload['tables']) != required:
        raise TableError('the required table set is absent or malformed')
    root = Path(wiki_root) if wiki_root is not None else None
    tables = tuple(_table(name, payload['tables'][name], root) for name in sorted(required))
    return TableSet(tables, hashlib.sha256(raw).hexdigest(), root)
