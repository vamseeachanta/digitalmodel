"""Observed N4 v261 audit fixture; numerical responses remain unqualified."""
import copy
import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_audit import verify_load_audit


FIXTURES = Path(__file__).with_name('fixtures')


def fixture():
    provenance_raw = (FIXTURES / 'v261_pressure_provenance.json').read_bytes()
    assert hashlib.sha256(provenance_raw).hexdigest() == '5a56f0bf808c192b003393ff3cf9c944754f2366cd8f96372e11aa263aabab26'
    provenance = json.loads(provenance_raw)
    for name, expected in provenance['files'].items():
        assert hashlib.sha256((FIXTURES / name).read_bytes()).hexdigest() == expected
    return ((FIXTURES / 'v261_pressure_loads.txt').read_bytes(),
            json.loads((FIXTURES / 'v261_pressure_case.json').read_bytes()))


def test_observed_pressure_loads_accept_numeric_decimal_equality():
    raw, case = fixture()
    assert case['pressure_mpa'] == '10' and b'10.000' in raw
    verify_load_audit(raw, case)


def test_observed_pressure_loads_accept_crlf_without_changing_values():
    raw, case = fixture()
    verify_load_audit(raw.replace(b'\n', b'\r\n'), case)


@pytest.mark.parametrize('damage', [
    'preamble', 'header', 'release', 'title', 'extra', 'truncated', 'missing_finish',
    'duplicate_finish', 'force', 'missing_flist', 'support_dof', 'support_real',
    'support_imaginary', 'missing_support', 'duplicate_support', 'selection',
    'missing_pair', 'duplicate_pair', 'missing_continuation', 'extra_continuation',
    'endpoint', 'reordered_endpoints', 'face', 'real', 'second_real', 'imaginary',
    'second_imaginary', 'unknown_column', 'nonfinite', 'exponent', 'mixed_zero',
])
def test_observed_pressure_audit_refuses_changed_native_evidence(damage):
    raw, case = fixture()
    rows = [b' '.join(row.split()) for row in raw.splitlines() if row.strip()]
    first = rows.index(b'1 4 1 10.000 0.0000')
    support = rows.index(b'1 UY 0.00000000 0.00000000')
    if damage == 'preamble': rows[0] += b' EXTRA'
    elif damage == 'header': rows[first - 1] = b'ELEMENT FACE KVAL P1 P2'
    elif damage == 'release': rows[1] = rows[1].replace(b'26.1', b'26.2')
    elif damage == 'title': rows[4] = b'Open cylinder verification P10N8'
    elif damage == 'extra': rows.append(b'UNKNOWN LOAD STATE')
    elif damage == 'missing_finish': rows.pop()
    elif damage == 'duplicate_finish': rows.append(rows[-1])
    elif damage == 'force': rows[rows.index(b'No nodal forces to list.')] = b'1 FX 10.0'
    elif damage == 'missing_flist': rows.remove(b'No nodal forces to list.')
    elif damage == 'support_dof': rows[support] = b'1 UX 0.00000000 0.00000000'
    elif damage == 'support_real': rows[support] = b'1 UY 0.00000001 0.00000000'
    elif damage == 'support_imaginary': rows[support] = b'1 UY 0.00000000 0.00000001'
    elif damage == 'missing_support': rows.pop(support)
    elif damage == 'duplicate_support': rows.insert(support, rows[support])
    elif damage == 'selection': rows = [row.replace(b'NODES 1 TO 345', b'NODES 1 TO 344') for row in rows]
    elif damage == 'missing_pair': del rows[first:first + 2]
    elif damage == 'duplicate_pair': rows[first:first] = rows[first:first + 2]
    elif damage == 'missing_continuation': rows.pop(first + 1)
    elif damage == 'extra_continuation': rows.insert(first + 1, rows[first + 1])
    elif damage == 'endpoint': rows[first + 1] = b'16 10.000 0.0000'
    elif damage == 'reordered_endpoints': rows[first:first + 2] = [b'1 4 15 10.000 0.0000', b'1 10.000 0.0000']
    elif damage == 'face': rows[first] = b'1 3 1 10.000 0.0000'
    elif damage == 'real': rows[first] = b'1 4 1 -10.000 0.0000'
    elif damage == 'second_real': rows[first + 1] = b'15 9.999 0.0000'
    elif damage == 'imaginary': rows[first] = b'1 4 1 10.000 0.0001'
    elif damage == 'second_imaginary': rows[first + 1] = b'15 10.000 0.0001'
    elif damage == 'unknown_column': rows[first] += b' 0'
    elif damage == 'nonfinite': rows[first] = b'1 4 1 NaN 0.0000'
    elif damage == 'exponent': rows[first] = b'1 4 1 1.000E+01 0.0000'
    elif damage == 'mixed_zero': rows.insert(0, b'No surface loads to list.')
    damaged = b'\n'.join(rows) + b'\n'
    if damage == 'truncated': damaged = damaged[:-1]
    assert damaged != raw
    with pytest.raises(EvidenceError):
        verify_load_audit(damaged, case)


@pytest.mark.parametrize('damage', ['subquantum_high', 'subquantum_low', 'zero',
    'face', 'connectivity', 'duplicate_element', 'duplicate_face', 'missing_face', 'title'])
def test_observed_pressure_audit_refuses_inconsistent_frozen_model(damage):
    raw, original = fixture()
    case = copy.deepcopy(original)
    if damage == 'subquantum_high': case['pressure_mpa'] = '10.0004'
    elif damage == 'subquantum_low': case['pressure_mpa'] = '9.9996'
    elif damage == 'zero': case['pressure_mpa'] = '0'
    elif damage == 'face': case['pressure_faces'][0]['face'] = 3
    elif damage == 'connectivity': case['elements'][0]['nodes'][3] = 19
    elif damage == 'duplicate_element': case['elements'].append(case['elements'][0])
    elif damage == 'duplicate_face': case['pressure_faces'].append(case['pressure_faces'][0])
    elif damage == 'missing_face': case['pressure_faces'].pop()
    elif damage == 'title': case['case_token'] = 'P10N8'
    if damage.startswith('subquantum'):
        for face in case['pressure_faces']:
            face['pressure_mpa'] = case['pressure_mpa']
    reason = 'Wrong real pressure' if damage.startswith('subquantum') else (
        'unmatched SFELIST endpoints' if damage == 'connectivity' else '')
    with pytest.raises(EvidenceError, match=reason):
        verify_load_audit(raw, case)


def test_unobserved_native_case_token_refuses_even_with_matching_banners():
    raw, case = fixture()
    case['case_token'] = 'P10N8'
    with pytest.raises(EvidenceError, match='observed N4'):
        verify_load_audit(raw.replace(b'P10N4', b'P10N8'), case)


@pytest.mark.parametrize('key', ['elements', 'pressure_faces', 'nodes', 'bottom_node_ids'])
def test_malformed_frozen_metadata_refuses_as_evidence_error(key):
    raw, case = fixture()
    del case[key]
    with pytest.raises(EvidenceError, match='Malformed frozen'):
        verify_load_audit(raw, case)
