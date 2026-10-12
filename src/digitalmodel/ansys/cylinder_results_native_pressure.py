"""Narrow observed v261 SFELIST grammar; no numerical qualification implied.

FACE NODES is one column heading. Each face has a five-token I endpoint row
and a three-token L continuation. KVAL and midside values are not listed.
"""
import re

from digitalmodel.ansys.cylinder_results import EvidenceError, decimal_value
from digitalmodel.ansys.cylinder_results_native_loads import verify_constraints
from digitalmodel.ansys.cylinder_results_native_status import lines, page_header


PREAMBLE = b'LIST ELEMENT SURFACE LOAD PRES FOR ALL SELECTED ELEMENTS'
HEADER = b'ELEMENT LKEY FACE NODES REAL IMAGINARY'
NOTE = rb'\*\*\* NOTE \*\*\* ELAPSED TIME = [0-9]+(?:\.[0-9]+)? TIME= [0-9]{2}:[0-9]{2}:[0-9]{2}'
REAL = rb'([+-]?[0-9]+\.[0-9]{3})'
IMAGINARY = rb'([+-]?[0-9]+\.[0-9]{4})'
FIRST = re.compile(rb'([1-9][0-9]*) ([1-9][0-9]*) ([1-9][0-9]*) ' + REAL + b' ' + IMAGINARY)
CONTINUATION = re.compile(rb'([1-9][0-9]*) ' + REAL + b' ' + IMAGINARY)
FACE_CORNERS = {4: (0, 3)}  # Frozen PLANE183 face 4, observed I then L listing.


def _expected_faces(case, pressure):
    elements = {}
    for element in case['elements']:
        key, nodes = element['element_id'], element['nodes']
        if (type(key) is not int or key <= 0 or key in elements or len(nodes) != 8
                or any(type(n) is not int or n <= 0 for n in nodes) or len(set(nodes)) != 8):
            raise EvidenceError('Invalid or duplicate frozen element connectivity')
        elements[key] = nodes
    expected = {}
    for face in case['pressure_faces']:
        key = face['element_id'], face['face']
        if (any(type(v) is not int for v in key) or key in expected
                or key[1] != 4 or key[0] not in elements
                or decimal_value(face['pressure_mpa']) != pressure):
            raise EvidenceError('Invalid or inconsistent frozen pressure face')
        expected[key] = tuple(elements[key[0]][i] for i in FACE_CORNERS[key[1]])
    if not expected:
        raise EvidenceError('Missing frozen pressure faces')
    return expected


def _pressure_pairs(rows, expected, pressure):
    found, index = set(), 6
    while index < len(rows) and not re.fullmatch(NOTE, rows[index]):
        first = FIRST.fullmatch(rows[index])
        second = CONTINUATION.fullmatch(rows[index + 1]) if index + 1 < len(rows) else None
        if not first or not second:
            raise EvidenceError('Unsupported or truncated SFELIST endpoint pair')
        element, face, node = (int(v) for v in first.groups()[:3])
        key = element, face
        if key in found or key not in expected or (node, int(second[1])) != expected[key]:
            raise EvidenceError('Duplicate, wrong face or unmatched SFELIST endpoints')
        values = (first[4], second[2])
        imaginary = (first[5], second[3])
        if (any(decimal_value(v.decode('ascii')) != pressure for v in values)
                or any(decimal_value(v.decode('ascii')) != 0 for v in imaginary)):
            raise EvidenceError('Wrong real pressure or nonzero imaginary surface load')
        found.add(key)
        index += 2
    if found != set(expected):
        raise EvidenceError('Native pressure face set differs from frozen input')
    return index


def _verify_pressure_loads(raw, case):
    rows = lines(raw)
    pressure = decimal_value(case['pressure_mpa'])
    token = case.get('case_token')
    if pressure <= 0 or token != 'P10N4':
        raise EvidenceError('Native pressure branch requires the observed N4 positive-pressure case')
    if len(rows) < 8 or rows[0] != PREAMBLE or rows[5] != HEADER:
        raise EvidenceError('Unsupported native pressure preamble or column header')
    page_header(rows[1:5])
    if rows[4] != ('Open cylinder verification ' + token).encode('ascii'):
        raise EvidenceError('Native pressure case title differs')
    index = _pressure_pairs(rows, _expected_faces(case, pressure), pressure)
    if (index + 1 >= len(rows) or not re.fullmatch(NOTE, rows[index])
            or rows[index + 1] != b'No nodal forces to list.'):
        raise EvidenceError('Missing native FLIST no-force record')
    verify_constraints(rows[index + 2:], case, case_token=token)


def verify_pressure_loads(raw, case):
    """Check printed loads exactly; malformed metadata remains evidence refusal."""
    try:
        _verify_pressure_loads(raw, case)
    except (KeyError, TypeError, AttributeError, IndexError) as exc:
        raise EvidenceError('Malformed frozen pressure metadata') from exc
