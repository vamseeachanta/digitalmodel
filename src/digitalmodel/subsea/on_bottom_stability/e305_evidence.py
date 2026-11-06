# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Supplemental, offline input binding for typed parity-mode outputs.

Call ``canonical_input_basis`` on the independently retained configuration, then
``write_evidence`` on completed outputs with independently retained output hashes.
Later call ``verify_evidence`` with that expected basis and those original hashes;
never obtain the expected values from the archive being checked.

Identity uses UTF-8 JSON with sorted mapping keys, compact separators, unescaped
Unicode, no trailing newline and no non-finite numbers. Segments are sorted by
identifier and cases by role. All Pipe and Case fields, including their explicit
numerical controls and recorded-only factors, enter identity. Output destinations
and router result fields do not. Integer and floating-point spellings remain
distinct; no unit conversion or numerical equivalence tolerance is applied.

Schema version one is a local evidence protocol, not an engineering constant.
The weight convention is that of ``e305_simplified.weight_build_up``: SI layers,
contents and extra structural mass, explicit gravity and local-diameter buoyancy.
No engineering acceptance, signed execution provenance or solver reproduction is
established. Tables and sea-state conversion are outside this helper's scope.
The existing router, output manifest and qualification flags are not changed.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.subsea.on_bottom_stability.e305_simplified import (
    Case,
    Pipe,
    weight_build_up,
)
from digitalmodel.subsea.on_bottom_stability.e305_workflow import BASENAME

SCHEMA_VERSION = 1  # Supplemental evidence protocol revision.
EVIDENCE_NAME = 'e305.evidence.json'
OUTPUT_NAMES = ('e305.json', 'e305.csv', 'e305.manifest.json')
_WEIGHT_BASIS = {
    'added_mass': 'extra structural mass multiplied by gravity',
    'buoyancy': 'local displaced-water volume with length-weighted field joints',
    'method': 'e305_simplified.weight_build_up',
    'units': 'SI',
}


def _bytes(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(',', ':'),
                      ensure_ascii=False, allow_nan=False).encode('utf-8')


def _record(mode: Any, gravity: Any, segments: Any) -> dict[str, Any]:
    if mode != 'parity':
        raise ValueError('input basis requires explicit typed parity mode')
    if not isinstance(segments, list) or not segments:
        raise ValueError('input basis segments must be a nonempty list')
    records = []
    identifiers = set()
    for segment in segments:
        if not isinstance(segment, dict) or set(segment) != {'id', 'pipe', 'cases'}:
            raise ValueError('input basis segment requires id, pipe and cases only')
        identifier = segment['id']
        if not isinstance(identifier, str) or not identifier.strip() or identifier in identifiers:
            raise ValueError('input basis segment identifiers must be nonempty and unique')
        identifiers.add(identifier)
        raw_cases = segment['cases']
        if not isinstance(raw_cases, list) or not raw_cases:
            raise ValueError('input basis cases must be a nonempty list')
        try:
            pipe = Pipe(**segment['pipe'])
            weight_build_up(pipe, g=gravity)  # Validate explicit gravity and weight domain.
            cases = [asdict(Case(**case)) for case in raw_cases]
        except (TypeError, KeyError) as exc:
            raise ValueError('input basis requires complete typed Pipe and Case fields') from exc
        if (len(cases) != len({'extreme_wave', 'extreme_current'})
                or {case['role'] for case in cases} != {'extreme_wave', 'extreme_current'}):
            raise ValueError('input basis requires both distinct case roles')
        records.append({'id': identifier, 'pipe': asdict(pipe),
                        'cases': sorted(cases, key=lambda case: case['role'])})
    record = {'schema_version': SCHEMA_VERSION, 'mode': mode, 'g_m_s2': gravity,
              'weight_basis': dict(_WEIGHT_BASIS),
              'segments': sorted(records, key=lambda segment: segment['id'])}
    _bytes(record)  # Reject non-finite or unserializable values before publication.
    return record


def canonical_input_basis(cfg: dict[str, Any]) -> dict[str, Any]:
    """Build a destination-independent basis from an independent configuration.

    Only explicit parity Pipe and Case inputs are supported. Missing physical
    fields, duplicate identities, empty segments/cases and table inputs fail.
    Pipe and Case constructors supply only their existing numerical defaults.
    """
    try:
        params = cfg[BASENAME]
        return _record(params['mode'], params['g'], params['segments'])
    except (TypeError, KeyError) as exc:
        raise ValueError('input basis requires explicit mode, gravity and segments') from exc


def _validated_basis(basis: Any) -> dict[str, Any]:
    keys = {'schema_version', 'mode', 'g_m_s2', 'weight_basis', 'segments'}
    if not isinstance(basis, dict) or set(basis) != keys:
        raise ValueError('input basis is missing or has unexpected fields')
    if type(basis['schema_version']) is not int or basis['schema_version'] != SCHEMA_VERSION:
        raise ValueError('unsupported input basis schema version')
    if basis['weight_basis'] != _WEIGHT_BASIS:
        raise ValueError('input basis weight convention is missing or unsupported')
    return _record(basis['mode'], basis['g_m_s2'], basis['segments'])


def basis_digest(basis: dict[str, Any]) -> str:
    """Hash the validated canonical input record, excluding all output paths."""
    return hashlib.sha256(_bytes(_validated_basis(basis))).hexdigest()


def _pairs(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError('duplicate JSON object key in evidence')
        result[key] = value
    return result


def _nonfinite(value: str) -> None:
    raise ValueError('non-finite JSON number in evidence')


def _json(raw: bytes) -> Any:
    try:
        return json.loads(raw, object_pairs_hook=_pairs, parse_constant=_nonfinite)
    except (UnicodeError, json.JSONDecodeError) as exc:
        raise ValueError('invalid evidence JSON') from exc


def _output_identity(payload: Any, basis: dict[str, Any]) -> None:
    """Compare archived physical identities without evaluating stability demand."""
    try:
        if (payload['mode'] != basis['mode'] or _bytes(payload['g']) != _bytes(basis['g_m_s2'])
                or payload['table_source_sha256'] is not None):
            raise ValueError('output identity differs from expected input basis')
        segments = payload['segments']
        if not isinstance(segments, list) or len(segments) != len(basis['segments']):
            raise ValueError('output segment identity differs from expected input basis')
        actual = []
        for segment in segments:
            cases = segment['cases']
            if not isinstance(cases, list) or not cases:
                raise ValueError('output case identity is empty')
            for case in cases:
                if case['role'] != case['inputs']['role'] or case['table_lookups']:
                    raise ValueError('output case identity differs from expected input basis')
            actual.append({'id': segment['id'], 'pipe': segment['pipe'],
                           'cases': sorted([case['inputs'] for case in cases],
                                           key=lambda case: case['role'])})
        actual.sort(key=lambda segment: segment['id'])
        if _bytes(actual) != _bytes(basis['segments']):
            raise ValueError('output identity differs from expected input basis')
        for segment in segments:
            weight = asdict(weight_build_up(Pipe(**segment['pipe']), g=basis['g_m_s2']))
            if _bytes(segment['weight']) != _bytes(weight):
                raise ValueError('output weight differs from expected input basis')
    except (KeyError, TypeError, AttributeError) as exc:
        raise ValueError('output physical identity or weight basis is missing') from exc


def _checked_outputs(directory: Path, basis: dict[str, Any], pins: Any) -> dict[str, str]:
    if not isinstance(pins, dict) or set(pins) != set(OUTPUT_NAMES):
        raise ValueError('independent retained output digests are required')
    for digest in pins.values():
        if (not isinstance(digest, str) or len(digest) != hashlib.sha256().digest_size * 2
                or any(character not in '0123456789abcdef' for character in digest)):
            raise ValueError('retained output digest must be a lowercase SHA-256 value')
    outputs = {}
    for name in OUTPUT_NAMES:
        try:
            raw = (directory / name).read_bytes()
        except OSError as exc:
            raise ValueError(f'retained output digest cannot be checked: {name}') from exc
        if hashlib.sha256(raw).hexdigest() != pins[name]:
            raise ValueError(f'retained output digest mismatch: {name}')
        outputs[name] = raw
    manifest = _json(outputs['e305.manifest.json'])
    expected = {'schema_version': SCHEMA_VERSION,
                'outputs': {name: pins[name] for name in ('e305.json', 'e305.csv')}}
    if _bytes(manifest) != _bytes(expected):
        raise ValueError('output manifest digest bindings differ from retained digests')
    _output_identity(_json(outputs['e305.json']), basis)
    return dict(pins)


def _supplement(basis: dict[str, Any], pins: dict[str, str]) -> dict[str, Any]:
    return {'schema_version': SCHEMA_VERSION, 'input_basis': basis,
            'input_basis_sha256': basis_digest(basis),
            'output_manifest_sha256': pins['e305.manifest.json'],
            'output_sha256': pins}


def write_evidence(directory: str | Path, *, expected_basis: dict[str, Any],
                   retained_output_sha256: dict[str, str]) -> dict[str, Any]:
    """Validate identities and retained bytes before writing a supplemental file.

    The three existing output files are read only. A differing existing sidecar
    is rejected; identical evidence can be written repeatedly. Validation fails
    before any sidecar is created. Returns the supplemental record.
    """
    directory = Path(directory)
    basis = _validated_basis(expected_basis)
    pins = _checked_outputs(directory, basis, retained_output_sha256)
    record = _supplement(basis, pins)
    raw = _bytes(record) + b'\n'
    target = directory / EVIDENCE_NAME
    try:
        with target.open('xb') as stream:
            stream.write(raw)
    except FileExistsError:
        if target.read_bytes() != raw:
            raise ValueError('existing supplemental evidence differs') from None
    if target.read_bytes() != raw:
        raise ValueError('supplemental evidence readback differs')
    return record


def verify_evidence(directory: str | Path, *, expected_basis: dict[str, Any],
                    retained_output_sha256: dict[str, str]) -> dict[str, Any]:
    """Check an archive against independent input identity and retained hashes.

    Neither expected basis nor output hashes may be recovered from the sidecar
    under examination. Verification is offline and establishes byte integrity
    and physical input agreement only; it does not rerun the demand calculation.
    """
    directory = Path(directory)
    basis = _validated_basis(expected_basis)
    try:
        record = _json((directory / EVIDENCE_NAME).read_bytes())
    except OSError as exc:
        raise ValueError('supplemental input basis is missing') from exc
    if not isinstance(record, dict):
        raise ValueError('supplemental input basis record must be an object')
    observed_basis = _validated_basis(record.get('input_basis'))
    if (_bytes(observed_basis) != _bytes(basis)
            or record.get('input_basis_sha256') != basis_digest(basis)):
        raise ValueError('supplemental input basis digest differs from expected basis')
    pins = _checked_outputs(directory, basis, retained_output_sha256)
    expected = _supplement(basis, pins)
    # Normalize only explicitly unordered segment and case collections.
    observed = dict(record, input_basis=observed_basis)
    if _bytes(observed) != _bytes(expected):
        raise ValueError('supplemental output digest bindings or schema differ')
    return expected
