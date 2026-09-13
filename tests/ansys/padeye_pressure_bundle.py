"""Compose preparation checks against caller-attested capture digests.

The expected mapping must come from the frozen external capture receipt, not
be recomputed from the candidate artifacts. Hash matching does not authenticate
that receipt or independently establish its source-revision claim.
"""
import hashlib
import re

from tests.ansys.padeye_pressure_cdb import parse_pressure_cdb
from tests.ansys.padeye_pressure_domain import verify_frozen_pressure_domain
from tests.ansys.padeye_pressure_quality import assess_shape_output
from tests.ansys.padeye_pressure_study import verify_pressure_mesh

INPUT_MATRIX = {
    'a54f2e7a3bee45b3943dd98b23e1a9b340c33745f083a6ccf48ffa8af2fef18a': (16, 1110, 1032),
    'f0c84c4284a9e4524f6eb2f4b0cc15105f984f7874a39f8e4bfa85fb66754e5f': (32, 4284, 4128),
    '3e8f5675696dadac1f70450686bd4755f99c749ac856181fd1914e69a5929ae2': (64, 16824, 16512),
}


def _check_receipt(artifacts, expected):
    keys = ('input_sha256', 'cdb_sha256', 'log_sha256')
    if set(expected) != set(keys) | {'source_head', 'capture_id', 'log_artifact_class'}:
        raise ValueError('missing or unrecognized receipt fields')
    for key, data in zip(keys, artifacts):
        if not isinstance(data, bytes) or not re.fullmatch('[0-9a-f]{64}', expected[key]):
            raise ValueError('invalid artifact bytes or attested digest')
        if hashlib.sha256(data).hexdigest() != expected[key]:
            raise ValueError(f'artifact hash mismatch: {key}')
    if not re.fullmatch('[0-9a-f]{40}', expected['source_head']):
        raise ValueError('invalid recorded source revision')
    if not re.fullmatch(r'\d{8}T\d{6}Z', expected['capture_id']):
        raise ValueError('invalid capture identifier')
    if expected['log_artifact_class'] not in ('native_capture', 'format_reproduction'):
        raise ValueError('unsupported log artifact class')
    return {key: expected[key] for key in keys}


def verify_pressure_bundle(input_bytes, cdb_bytes, log_bytes, expected):
    """Check fixed input, capture hashes and all independent preparation gates."""
    try:
        hashes = _check_receipt((input_bytes, cdb_bytes, log_bytes), expected)
        if expected['input_sha256'] not in INPUT_MATRIX:
            raise ValueError('input differs from the frozen preparation matrix')
        edges, node_count, element_count = INPUT_MATRIX[expected['input_sha256']]
        mesh, pressures = parse_pressure_cdb(cdb_bytes.decode('utf-8'), edges)
        if len(mesh['nodes']) != node_count or len(mesh['elements']) != element_count:
            raise ValueError('native mesh counts differ from frozen matrix')
        domain = verify_frozen_pressure_domain(mesh)
        pressure = verify_pressure_mesh(mesh, pressures)
        shape = assess_shape_output(log_bytes.decode('utf-8'), element_count)
        kind = expected['log_artifact_class']
        status = 'native_preparation_checks_passed' if kind == 'native_capture' else 'format_fixture_checks_passed'
        return {'status': status, 'artifact_hashes_verified': 3,
                'log_artifact_class': kind, 'capture_id': expected['capture_id'],
                'verified_hashes': hashes,
                'recorded_source_head': expected['source_head'],
                'source_revision_independently_verified': False,
                'domain': domain, 'pressure': pressure, 'shape': shape,
                'native_qualification_complete': False, 'stress_solve_authorized': False}
    except (KeyError, TypeError, UnicodeError) as error:
        raise ValueError('malformed preparation bundle or external receipt') from error
