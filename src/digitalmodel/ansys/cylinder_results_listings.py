"""Narrow, synthetic-tested listing grammar; unmatched native text refuses."""
import re

from digitalmodel.ansys.cylinder_results import EvidenceError, parse_e24, validated_stations

HEADERS = {'stress': ('NODE', 'SX', 'SY', 'SZ', 'SXY', 'SYZ', 'SXZ'),
           'displacement': ('NODE', 'UX', 'UY', 'UZ')}
MAPS = {'SX': 'sigma_r', 'SY': 'sigma_z', 'SZ': 'sigma_theta', 'SXY': 'tau_rz',
        'UX': 'u_r', 'UY': 'u_z'}


def extract_block(raw, name):
    if not isinstance(raw, bytes) or not re.fullmatch(r'[A-Z_]+', name):
        raise EvidenceError('Missing bytes or invalid block identity')
    bounds = []
    for suffix in ('BEGIN', 'END'):
        pattern = rb'(?m)^ *OCV_' + name.encode() + b'_' + suffix.encode() + rb' *\r?\n'
        matches = list(re.finditer(pattern, raw))
        if len(matches) != 1:
            raise EvidenceError('Missing or duplicated native block delimiter')
        bounds.append(matches[0])
    if bounds[0].end() >= bounds[1].start():
        raise EvidenceError('Empty or reversed native block')
    return raw[bounds[0].end():bounds[1].start()]


def _table(raw, kind):
    if not isinstance(raw, bytes) or not raw.endswith(b'\n'):
        raise EvidenceError('Missing or truncated native table')
    lines = [line.rstrip(b'\r') for line in raw.split(b'\n')[:-1] if line.strip()]
    header = tuple(word.decode('ascii') for word in lines[0].split()) if lines else ()
    if header != HEADERS[kind]:
        raise EvidenceError('Unsupported native table header or component order')
    result = {}
    for line in lines[1:]:
        if len(line) != 8+24*(len(header)-1):
            raise EvidenceError('Truncated, wrapped or unsupported native row')
        if not re.fullmatch(rb' *[1-9]\d*', line[:8]):
            raise EvidenceError('Invalid table node identity')
        node = int(line[:8])
        if node in result:
            raise EvidenceError('Duplicate native table node')
        row = {}
        for index, label in enumerate(header[1:]):
            field = line[8+24*index:8+24*(index+1)]
            parse_e24(field, 'MPa' if kind == 'stress' else 'mm')
            row[label] = field
        result[node] = row
    if not result:
        raise EvidenceError('Empty native table')
    return result


def parse_nodal_listing(raw, stations, kind):
    """Parse nine nodes; units are fixed by the protocol, not inferred from magnitudes."""
    if kind not in HEADERS:
        raise EvidenceError('Unknown native listing kind')
    nodes = validated_stations(stations)
    if raw.lstrip().startswith(b'PRINT '):
        from digitalmodel.ansys.cylinder_results_native_listings import normalize_nodal
        raw = normalize_nodal(raw,kind)
    rows = _table(raw, kind)
    if set(rows) != set(nodes):
        raise EvidenceError('Native listing does not contain exactly nine stations')
    return {(nodes[node]['id'], MAPS[component]): value
            for node, row in rows.items() for component, value in row.items()
            if component in MAPS}


def parse_contribution_listing(raw, stations, *, element_ids):
    """Bind all element sections and every sampled element-node pair before means."""
    nodes = validated_stations(stations)
    if not isinstance(raw, bytes) or not element_ids:
        raise EvidenceError('Missing native contributions or expected element set')
    if raw.lstrip().startswith(b'PRINT '):
        from digitalmodel.ansys.cylinder_results_native_listings import normalize_presol
        raw = normalize_presol(raw)
    matches = list(re.finditer(rb'(?m)^ *ELEMENT *= *([1-9]\d*) *\r?\n', raw))
    if not matches or raw[:matches[0].start()].strip():
        raise EvidenceError('Unsupported element contribution preamble')
    result = {s['id']: {} for s in stations}
    seen = set()
    for index, match in enumerate(matches):
        element = int(match[1])
        if element in seen or element not in element_ids:
            raise EvidenceError('Duplicate or unexpected native element')
        seen.add(element)
        end = matches[index+1].start() if index+1 < len(matches) else len(raw)
        rows = _table(raw[match.end():end], 'stress')
        for node, station in nodes.items():
            expected = element in station['adjacent_element_ids']
            if (node in rows) != expected:
                raise EvidenceError('Native element-node adjacency differs')
            if expected:
                result[station['id']][element] = {MAPS[k]: v for k, v in rows[node].items()
                                                if k in MAPS}
    if seen != set(element_ids):
        raise EvidenceError('Incomplete selected-element contribution listing')
    return result
