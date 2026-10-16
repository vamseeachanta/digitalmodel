# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""External cached-workbook fixture conversion and strict parity comparison."""

from __future__ import annotations

import hashlib
import io
import json
import math
import re
from pathlib import Path
from typing import Any, NoReturn

from digitalmodel.subsea.on_bottom_stability.e305_simplified import Case, Pipe


_EXPECTED_FIELDS = {'required_weight_n_m', 'available_weight_n_m', 'utilization'}
_ROLES = {'extreme_wave', 'extreme_current'}


def _object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError('duplicate JSON key')
        result[key] = value
    return result


def _reject_constant(value: str) -> NoReturn:
    raise ValueError('nonfinite JSON value')


def _read_json(raw: bytes) -> Any:
    return json.loads(raw, object_pairs_hook=_object, parse_constant=_reject_constant)


def _expected_case(value: Any) -> None:
    if not isinstance(value, dict) or set(value) != _EXPECTED_FIELDS:
        raise ValueError('all expected parity result fields are required')
    for key, number in value.items():
        if isinstance(number, bool) or not isinstance(number, (int, float)) or not math.isfinite(number):
            raise ValueError('expected results must be finite numbers')
        if number < 0 or (key == 'available_weight_n_m' and number == 0):
            raise ValueError('expected weight and utilization must be in their supported domain')


def validate_fixture(payload: dict[str, Any]) -> None:
    """Validate both independent cases, SI inputs and source digest metadata."""
    if not isinstance(payload, dict) or set(payload) != {'schema_version', 'g', 'segments', 'expected', 'source'}:
        raise ValueError('invalid private fixture layout')
    if type(payload['schema_version']) is not int or payload['schema_version'] != 1:
        raise ValueError('unsupported private fixture schema_version')
    if isinstance(payload['g'], bool) or payload['g'] != 9.81:
        raise ValueError('private parity regression requires g = 9.81')
    segments = payload['segments']
    if not isinstance(segments, list) or not segments:
        raise ValueError('private fixture requires a nonempty segment table')
    identifiers = []
    for segment in segments:
        if not isinstance(segment, dict) or set(segment) != {'id', 'pipe', 'cases'}:
            raise ValueError('invalid fixture segment')
        identifier = segment['id']
        if not isinstance(identifier, str) or not identifier.strip() or identifier in identifiers:
            raise ValueError('fixture segment identifiers must be nonempty and unique')
        identifiers.append(identifier)
        Pipe(**segment['pipe'])
        cases = segment['cases']
        if not isinstance(cases, list) or len(cases) != 2:
            raise ValueError('fixture requires both distinct load cases')
        parsed = [Case(**case) for case in cases]
        if {case.role for case in parsed} != _ROLES:
            raise ValueError('fixture requires both distinct load-case roles')
    expected = payload['expected']
    if not isinstance(expected, dict) or set(expected) != set(identifiers):
        raise ValueError('expected results must cover every fixture segment')
    for by_role in expected.values():
        if not isinstance(by_role, dict) or set(by_role) != _ROLES:
            raise ValueError('expected results must cover both cases')
        for value in by_role.values():
            _expected_case(value)
    source = payload['source']
    if not isinstance(source, dict) or set(source) != {'workbook_sha256', 'cell_map_sha256'}:
        raise ValueError('source digest metadata is required')
    if any(not isinstance(digest, str) or re.fullmatch('[0-9a-f]{64}', digest) is None
           for digest in source.values()):
        raise ValueError('source digests must be SHA-256 hexadecimal strings')


def load_fixture(path: str | Path) -> dict[str, Any]:
    """Read a present external fixture; missing or malformed data is an error."""
    payload = _read_json(Path(path).read_bytes())
    validate_fixture(payload)
    return dict(payload)


def _read_tree(node: Any, workbook: Any, *, expected: bool = False) -> Any:
    if isinstance(node, list):
        return [_read_tree(value, workbook, expected=expected) for value in node]
    if not isinstance(node, dict):
        raise ValueError('cell-map leaves require cached cells or explicit literals')
    if set(node) == {'sheet', 'cell'}:
        try:
            value = workbook[node['sheet']][node['cell']].value
        except (KeyError, ValueError, TypeError) as error:
            raise ValueError('invalid cached cell locator') from error
        if value is None:
            raise ValueError('required cached workbook cell is absent')
        return value
    if set(node) == {'literal'}:
        if expected:
            raise ValueError('expected results must come from cached workbook cells')
        return node['literal']
    if 'cell' in node or 'sheet' in node:
        raise ValueError('cached cell locators require exactly sheet and cell')
    return {key: _read_tree(value, workbook, expected=expected) for key, value in node.items()}


def _outside_repository(path: Path, root: Path) -> None:
    if path.resolve().is_relative_to(root.resolve()):
        raise ValueError('workbook, cell map and fixture must remain outside the public repository')


def convert_workbook(workbook_path: str | Path, cell_map_path: str | Path,
                     output_path: str | Path, *, repository_root: Path) -> dict[str, Any]:
    """Convert cached cells only; bind source digests to the bytes actually read."""
    from openpyxl import load_workbook

    workbook_path, cell_map_path, output_path = map(Path, (workbook_path, cell_map_path, output_path))
    for path in (workbook_path, cell_map_path, output_path):
        _outside_repository(path, repository_root)
    workbook_bytes = workbook_path.read_bytes()
    map_bytes = cell_map_path.read_bytes()
    mapping = _read_json(map_bytes)
    if not isinstance(mapping, dict) or set(mapping) != {'g', 'segments', 'expected'}:
        raise ValueError('cell map requires g, segments and expected')
    workbook = load_workbook(io.BytesIO(workbook_bytes), data_only=True, read_only=True, keep_links=False)
    try:
        payload = {'schema_version': 1,
                   'g': _read_tree(mapping['g'], workbook),
                   'segments': _read_tree(mapping['segments'], workbook),
                   'expected': _read_tree(mapping['expected'], workbook, expected=True),
                   'source': {'workbook_sha256': hashlib.sha256(workbook_bytes).hexdigest(),
                              'cell_map_sha256': hashlib.sha256(map_bytes).hexdigest()}}
    finally:
        workbook.close()
    validate_fixture(payload)
    data = (json.dumps(payload, indent=2, sort_keys=True, allow_nan=False) + '\n').encode('utf-8')
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_bytes(data)
    if output_path.read_bytes() != data:
        raise ValueError('fixture output readback differs from the converted bytes')
    return payload


def compare_fixture(fixture: dict[str, Any]) -> list[dict[str, Any]]:
    """Compare every independent expected result; zero targets require zero."""
    from digitalmodel.subsea.on_bottom_stability.e305_workflow import BASENAME, router

    validate_fixture(fixture)
    cfg = {BASENAME: {'mode': 'parity', 'g': fixture['g'], 'segments': fixture['segments']}}
    result = router(cfg)[BASENAME]['result']
    comparisons = []
    for segment in result['segments']:
        for case in segment['cases']:
            for field in sorted(_EXPECTED_FIELDS):
                expected = fixture['expected'][segment['id']][case['role']][field]
                actual = case[field]
                if actual is None or not math.isfinite(actual):
                    raise ValueError('private parity result is undefined or nonfinite')
                difference = abs(actual - expected)
                relative = difference / abs(expected) if expected != 0 else (0.0 if difference == 0 else math.inf)
                comparisons.append({'segment_id': segment['id'], 'role': case['role'],
                                    'field': field, 'actual': actual, 'expected': expected,
                                    'relative_error': relative})
    return comparisons
