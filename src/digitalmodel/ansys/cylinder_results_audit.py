"""Strict synthetic and observed v261 audit/status adapters.

Only explicit implemented layouts are supported; every native line is consumed.
Unsupported text remains INCOMPLETE, never inferred from requested settings.
"""
import re
from decimal import Decimal

from digitalmodel.ansys.cylinder_results import EvidenceError, decimal_value, parse_e24
from digitalmodel.ansys.cylinder_results_listings import extract_block


def _lines(raw):
    if not isinstance(raw, bytes) or not raw.endswith(b'\n'):
        raise EvidenceError('Missing or truncated native audit')
    return [line.rstrip(b'\r') for line in raw.split(b'\n')[:-1] if line.strip()]


def _surface(lines):
    if not lines:
        raise EvidenceError('Missing surface-load listing')
    if lines[0].strip() == b'NO SURFACE LOADS':
        return {}, 1
    if lines[0].split() != [b'ELEMENT',b'FACE',b'KVAL',b'P1',b'P2']:
        raise EvidenceError('Unsupported SFELIST header; native format unestablished')
    rows, index = {}, 1
    while index < len(lines) and lines[index].strip() != b'NO NODAL FORCES':
        line = lines[index]
        if len(line) != 72:
            raise EvidenceError('Unsupported/truncated pressure row')
        try:
            element, face, kval = [int(line[j:j+8]) for j in (0,8,16)]
        except ValueError as exc:
            raise EvidenceError('Invalid surface-load identity') from exc
        key = element, face
        if key in rows or kval != 1:
            raise EvidenceError('Duplicate or unsupported surface-load key')
        rows[key] = tuple(parse_e24(line[j:j+24], 'MPa')[0] for j in (24,48))
        index += 1
    return rows, index


def _supports(lines):
    if not lines or lines[0].split() != [b'NODE',b'DOF',b'VALUE']:
        raise EvidenceError('Missing/unsupported DLIST header')
    rows = {}
    for line in lines[1:]:
        if len(line) != 40 or not re.fullmatch(rb' *[1-9]\d*', line[:8]):
            raise EvidenceError('Unsupported support row or trailing audit text')
        node, dof = int(line[:8]), line[8:16].strip()
        value, _ = parse_e24(line[16:40], 'mm')
        if node in rows or dof != b'UY' or value != 0:
            raise EvidenceError('Duplicate, nonzero or wrong support DOF')
        rows[node] = value
    return rows


def verify_load_audit(raw, case):
    """Audit raw surface/nodal/support rows against frozen pressure and bottom set."""
    if b'No surface loads to list.' in raw:
        from digitalmodel.ansys.cylinder_results_native_loads import verify_zero_loads
        return verify_zero_loads(raw,case)
    lines = _lines(raw)
    if lines and b' '.join(lines[0].split()) == b'LIST ELEMENT SURFACE LOAD PRES FOR ALL SELECTED ELEMENTS':
        from digitalmodel.ansys.cylinder_results_native_pressure import verify_pressure_loads
        return verify_pressure_loads(raw, case)
    pressures, index = _surface(lines)
    if index >= len(lines) or lines[index].strip() != b'NO NODAL FORCES':
        raise EvidenceError('Missing FLIST or applied nodal force exists')
    supports = _supports(lines[index+1:])
    p = decimal_value(case['pressure_mpa'])
    expected = {(face['element_id'],4):(p,p) for face in case['pressure_faces']}
    if p == 0:
        expected = {}
    if pressures != expected:
        raise EvidenceError('Wrong pressure sign/magnitude/face or control has loads')
    if set(supports) != set(case['bottom_node_ids']):
        raise EvidenceError('Native support set differs from complete bottom row')


def _settings(raw, keys):
    lines = _lines(raw)
    result = {}
    for line in lines:
        match = re.fullmatch(rb' *([A-Z]+) *= *([A-Z0-9.+-]+) *', line)
        if not match:
            raise EvidenceError('Unsupported native status layout; profile unestablished')
        key, value = (v.decode('ascii') for v in match.groups())
        if key not in keys or key in result:
            raise EvidenceError('Unexpected or duplicate native status key')
        result[key] = value
    if result.keys() != set(keys):
        raise EvidenceError('Missing native status keys')
    return result


def parse_configuration(raw):
    """Read status bytes, with no fallback to requested configuration metadata."""
    if not isinstance(raw,bytes):
        raise EvidenceError('Missing native output')
    transition = split_load_audit(raw)['transition']
    _transition_configuration(transition)
    native_layout = b'CURRENT ANSYS CONFIGURATION' in transition
    matches = re.findall(rb'(?m)^ *RESUPREC *= *([+-]?\d+) *\r?$', raw)
    if ((native_layout and (matches or raw.count(b'USE DOUBLE PRECISION RESULTS FILE FORMAT') != 3
            or re.split(rb'(?m)^ *OCV_LOAD_AUDIT_BEGIN *\r?$',raw)[0].count(b'USE DOUBLE PRECISION RESULTS FILE FORMAT') != 1))
            or (not native_layout and matches != [b'0',b'0'])):
        raise EvidenceError('Unexpected RESUPREC outside bound pre/post sections')
    expected = dict(NDIGIT='8',FTYPE='E',NWIDTH='24',DSIGNF='16',LINE='100',CHAR='240')
    from digitalmodel.ansys.cylinder_results_native_status import format_status, outres_status, nerr_status
    if native_layout:
        format_status(extract_block(raw,'FORMAT'))
        outres_status(extract_block(raw,'OUTRES'))
        return {'nerr_nmerr':nerr_status(extract_block(raw,'NERR')),
                'resuprec':'0',
                'resuprec_basis':'inferred_from_native_double_precision_status',
                'resuprec_source_version':'v261'}
    if _settings(extract_block(raw,'FORMAT'),expected) != expected:
        raise EvidenceError('Native listing format differs')
    if _settings(extract_block(raw,'OUTRES'),('ALL','NAR')) != dict(ALL='ALL',NAR='NONE'):
        raise EvidenceError('Native OUTRES differs from explicit element solution')
    nerr = _settings(extract_block(raw,'NERR'),('NMERR',))['NMERR']
    if not re.fullmatch(r'\d+',nerr):
        raise EvidenceError('Negative or invalid native NMERR')
    return {'nerr_nmerr':int(nerr)}


def load_audit_block(raw):
    """Delimited window preserves the required no-mutation pre-solve sequence."""
    normalized = raw.replace(b'\r\n',b'\n')
    bounds = []
    for label in ('LOAD_AUDIT_BEGIN','SOLVED_STATE_BEGIN'):
        pattern = re.compile(rb'(?m)^ *OCV_' + label.encode() + rb' *\n')
        bounds.append(_one_match(pattern, normalized, label))
    if bounds[0].end() >= bounds[1].start():
        raise EvidenceError('Invalid load-audit window')
    return normalized[bounds[0].end():bounds[1].start()]



FINISH = re.compile(rb'(?m)^ *\*{5} ROUTINE COMPLETED \*{5} +ELAPSED TIME = +[0-9]+(?:\.[0-9]+)? *\n')
SOLUTION = re.compile(rb'(?m)^ *\*{5} +MAPDL SOLUTION ROUTINE +\*{5} *\n')
SOLVE = re.compile(rb'(?m)^ *\*{5} +MAPDL SOLVE +COMMAND +\*{5} *\n')
POST = re.compile(rb'(?m)^ *\*{5} +MAPDL RESULTS INTERPRETATION \(POST1\) +\*{5} *\n')


def split_load_audit(raw):
    """Retain load tables and complete transition; neither is silently discarded."""
    window = load_audit_block(raw)
    finishes = list(FINISH.finditer(window))
    if len(finishes) != 2:
        raise EvidenceError('Expected pre-solve and post-solve FINISH statuses')
    first = finishes[0]
    if not window[:first.start()].strip():
        raise EvidenceError('Missing load tables before FINISH')
    return {'loads': window[:first.start()], 'transition': window[first.start():]}


def _one_match(pattern, raw, name):
    matches = list(pattern.finditer(raw))
    if len(matches) != 1:
        raise EvidenceError('Missing or duplicate transition '+name)
    return matches[0]


def _transition_configuration(raw):
    """Bind statuses to processor boundaries; unknown CONFIG layouts refuse.

    RESUPREC-only sections retain the synthetic profile; complete observed v261
    CONFIG sections use a separate strict reader. Full solution bytes remain retained and diagnostic-scanned
    by the composed validator; no arbitrary CONFIG text is ignored.
    """
    finishes = list(FINISH.finditer(raw))
    solution = _one_match(SOLUTION, raw, 'solution entry')
    solve = _one_match(SOLVE, raw, 'SOLVE')
    post = _one_match(POST, raw, 'POST1 entry')
    finish_solution = _one_match(re.compile(rb'(?m)^ *FINISH SOLUTION PROCESSING *\n'),raw,'solution finish')
    if len(finishes)!=2 or not (finishes[0].end() <= solution.start()
            < solve.start() < finish_solution.start() < finishes[1].start() < post.start()):
        raise EvidenceError('Invalid pre/post-solve transition order')
    for section in (raw[finishes[0].end():solution.start()],
                    raw[finishes[1].end():post.start()]):
        if b'CURRENT ANSYS CONFIGURATION' in section:
            from digitalmodel.ansys.cylinder_results_native_status import config_section
            state = {'RESUPREC':config_section(section)}
        else:
            state = _settings(section, ('RESUPREC',))
        if state != {'RESUPREC':'0'}:
            raise EvidenceError('Pre/post native RESUPREC differs')
    return {'solution_bytes': raw[solution.start():finishes[1].end()],
            'post_bytes': raw[post.start():]}
