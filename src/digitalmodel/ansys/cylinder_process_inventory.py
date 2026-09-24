"""Pure, bounded process-family classification; no host observation or control.

Caller-supplied snapshots and CFD pins require independent source verification.
CLEAR covers only the declared selected process scope, never capacity or licensing.
"""
from copy import deepcopy
from decimal import (Context, Decimal, DivisionByZero, InvalidOperation,
                     Overflow, ROUND_HALF_EVEN, localcontext)
from pathlib import PureWindowsPath
import re

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes

ROW_FIELDS = {'pid', 'parent_pid', 'creation_time', 'name', 'executable_path',
              'executable_sha256', 'argv', 'script_sources'}
SELECTOR = 'ansys-mpi-lineage-v1'


def _text(value, limit=4096):
    if not isinstance(value, str) or not value.strip() or len(value) > limit or '\x00' in value:
        raise ValueError('Invalid bounded process text')
    return value


def _time(value):
    if not isinstance(value, str) or len(value) > 64 or not re.fullmatch(r'(?:0|[1-9][0-9]*)(?:\.[0-9]+)?', value):
        raise ValueError('Process times require finite nonnegative decimal strings')
    return Decimal(value)


def _pid(value, *, parent=False):
    if type(value) is not int or value < (0 if parent else 1) or value > 4294967295:
        raise ValueError('Invalid typed process identity')
    return value


def _path(value):
    _text(value)
    path = PureWindowsPath(value)
    if (not path.is_absolute() or not re.match(r'^[A-Za-z]:[\\/]', value)
            or any(p in ('.', '..') or p.endswith((' ', '.')) for p in value.replace('\\', '/').split('/')[1:])):
        raise ValueError('Process path requires an absolute local Windows identity')
    return str(path).casefold()


def _hash(value):
    if not isinstance(value, str) or not re.fullmatch('[0-9a-f]{64}', value):
        raise ValueError('Missing process/source digest')


def _row(row, observed):
    if not isinstance(row, dict) or set(row) != ROW_FIELDS:
        raise ValueError('Selected process row schema differs')
    _pid(row['pid'])
    _pid(row['parent_pid'], parent=True)
    if _time(row['creation_time']) > observed:
        raise ValueError('Process created after snapshot')
    name = _text(row['name'], 255)
    if name != PureWindowsPath(name).name or name.casefold() != PureWindowsPath(_path(row['executable_path'])).name:
        raise ValueError('Process basename and image path disagree')
    _hash(row['executable_sha256'])
    argv = row['argv']
    if not isinstance(argv, list) or not 1 <= len(argv) <= 128:
        raise ValueError('Missing bounded process argv')
    for arg in argv:
        _text(arg)
    sources = row['script_sources']
    if not isinstance(sources, list) or len(sources) > 16:
        raise ValueError('Invalid script source inventory')
    seen = set()
    for source in sources:
        if not isinstance(source, dict) or set(source) != {'path', 'sha256'}:
            raise ValueError('Invalid script source identity')
        path = _path(source['path'])
        _hash(source['sha256'])
        if path in seen:
            raise ValueError('Duplicate script source')
        seen.add(path)


def _rows(rows, observed):
    if not isinstance(rows, list) or len(rows) > 4096:
        raise ValueError('Selected process inventory exceeds declared bounds')
    by_pid = {}
    for row in rows:
        _row(row, observed)
        if row['pid'] in by_pid:
            raise ValueError('Duplicate process PID')
        by_pid[row['pid']] = row
    visited = set()
    for pid, row in by_pid.items():
        parent = by_pid.get(row['parent_pid'])
        if parent and _time(parent['creation_time']) > _time(row['creation_time']):
            raise ValueError('Parent created after child')
        trail = set()
        cursor = pid
        while cursor in by_pid and cursor not in visited:
            if cursor in trail:
                raise ValueError('Cyclic process ancestry')
            trail.add(cursor)
            cursor = by_pid[cursor]['parent_pid']
        visited.update(trail)
    return by_pid


def _snapshot(snapshot, expected_host, now, maximum_age_seconds):
    if not isinstance(snapshot, dict) or snapshot.get('schema') != 'process-snapshot-1':
        raise ValueError('Unsupported process snapshot')
    if snapshot.get('host') != _text(expected_host, 255) or snapshot.get('enumeration_complete') is not True:
        raise ValueError('Host differs or initial enumeration incomplete')
    observed, current, bound = _time(snapshot.get('observed_at')), _time(now), _time(maximum_age_seconds)
    with localcontext(Context(
            prec=130, rounding=ROUND_HALF_EVEN, Emin=-999999, Emax=999999,
            capitals=1, clamp=0, flags=[],
            traps=[InvalidOperation, DivisionByZero, Overflow])):
        # Exact subtraction of bounded inputs, independent of ambient context.
        age = current - observed
    if bound <= 0 or observed > current or age > bound:
        raise ValueError('Process snapshot is stale or future-dated')
    rows = _rows(snapshot.get('rows'), observed)
    coverage = snapshot.get('coverage')
    if not isinstance(coverage, dict) or coverage.get('selector') != SELECTOR:
        raise ValueError('Unreviewed process selector')
    for key in ('enumerated_count', 'selected_count', 'excluded_count'):
        if type(coverage.get(key)) is not int or not 0 <= coverage[key] <= 1000000:
            raise ValueError('Invalid enumeration coverage count')
    if (coverage.get('selected_details_complete') is not True
            or coverage['selected_count'] != len(rows)
            or coverage['enumerated_count'] != coverage['selected_count'] + coverage['excluded_count']):
        raise ValueError('Incomplete or contradictory selected scope')
    return rows, observed


def _roles(binding):
    roles = {key: [_pid(binding.get(key))] for key in ('controller', 'guard', 'mpi', 'helper')}
    for key in ('ranks', 'wrappers', 'console_helpers'):
        values = binding.get(key, [])
        if not isinstance(values, list) or len(values) > 4096:
            raise ValueError('Invalid CFD role list')
        roles[key] = [_pid(value) for value in values]
    flat = [pid for values in roles.values() for pid in values]
    if not roles['ranks'] or len(flat) != len(set(flat)):
        raise ValueError('CFD roles are empty or overlapping')
    return roles, set(flat)


def _script_pins(row):
    if not row['script_sources']:
        raise ValueError('Controller/guard/wrapper source pins are mandatory')
    arguments = {str(PureWindowsPath(a)).casefold() for a in row['argv']}
    sources = {_path(source['path']) for source in row['script_sources']}
    scripts = {argument for argument in arguments if argument.endswith('.py')}
    if not sources.issubset(arguments) or not scripts.issubset(sources):
        raise ValueError('Pinned script does not appear in bound argv')


def _topology(pins, roles):
    controller, guard, mpi, helper = (roles[k][0] for k in ('controller', 'guard', 'mpi', 'helper'))
    wrappers = set(roles['wrappers'])
    consumed = set()
    cursor = pins[guard]['parent_pid']
    while cursor != controller:
        if cursor not in wrappers or cursor in consumed:
            raise ValueError('Guard does not descend from bound controller/wrappers')
        consumed.add(cursor)
        cursor = pins[cursor]['parent_pid']
    cursor = pins[controller]['parent_pid']
    while cursor in wrappers:
        consumed.add(cursor)
        cursor = pins[cursor]['parent_pid']
    if consumed != wrappers:
        raise ValueError('Unaccounted wrapper topology')
    if pins[mpi]['parent_pid'] != guard or pins[helper]['parent_pid'] != mpi:
        raise ValueError('CFD MPI/helper ancestry differs')
    if any(pins[pid]['parent_pid'] != helper for pid in roles['ranks']):
        raise ValueError('CFD rank ancestry differs')
    if any(pins[pid]['parent_pid'] not in set(roles['ranks']) | {mpi, helper} for pid in roles['console_helpers']):
        raise ValueError('Console helper has unbound execution parent')


def _binding(binding, host, observed):
    if binding is None:
        return {}, set()
    if not isinstance(binding, dict) or binding.get('schema') != 'cfd-process-binding-1' or binding.get('host') != host:
        raise ValueError('CFD binding host/schema differs')
    roles, identities = _roles(binding)
    pins = _rows(binding.get('processes'), observed)
    if set(pins) != identities:
        raise ValueError('Every pinned process requires exactly one explicit role')
    for key in ('controller', 'guard', 'wrappers'):
        for pid in roles[key]:
            _script_pins(pins[pid])
    expected = {'mpi': 'mpiexec.exe', 'helper': 'smpd.exe', 'ranks': 'interfoam.exe', 'console_helpers': 'conhost.exe'}
    for key, name in expected.items():
        if any(pins[pid]['name'].casefold() != name for pid in roles[key]):
            raise ValueError('CFD role does not match the pinned executor family')
    _topology(pins, roles)
    argv = pins[roles['mpi'][0]]['argv']
    if argv.count('-n') != 1 or argv.index('-n') + 1 >= len(argv) or argv[argv.index('-n') + 1] != str(len(roles['ranks'])):
        raise ValueError('Pinned MPI rank count differs')
    return pins, identities


def _ansys(row):
    name = row['name'].casefold()
    path = _path(row['executable_path']).replace('\\', '/')
    return bool(re.fullmatch(r'(?:ansys[0-9]*|mapdl)\.exe', name) or '/ansys/bin/' in path)


def _family_matches(rows, pins, identities):
    if not pins or any(rows.get(pid) != pin for pid, pin in pins.items()):
        return False
    # A selected descendant of a bound member cannot disappear through filtering.
    return not any(row['parent_pid'] in identities and pid not in identities for pid, row in rows.items())


def _result(snapshot, rows, pins, identities):
    family_matches = _family_matches(rows, pins, identities)
    dispositions, conflicts, unknowns = [], [], []
    for pid, row in rows.items():
        if _ansys(row):
            category, reason = 'CONFLICT', 'recognized-ansys-solver-image'
        elif family_matches and pid in identities:
            category, reason = 'PRESERVED_CFD', 'exact-host-bound-cfd-family'
        else:
            category, reason = 'UNKNOWN', 'selected-process-not-covered-by-current-family-pins'
        item = {'pid': pid, 'creation_time': row['creation_time'], 'classification': category, 'reason': reason}
        dispositions.append(item)
        if category == 'CONFLICT':
            conflicts.append(item)
        elif category == 'UNKNOWN':
            unknowns.append(item)
    for pid in sorted(identities - set(rows)):
        unknowns.append({'pid': pid, 'creation_time': pins[pid]['creation_time'],
                         'classification': 'UNKNOWN', 'reason': 'bound-process-missing-from-snapshot'})
    return {'status': 'CONFLICT' if conflicts else 'UNKNOWN' if unknowns else 'CLEAR',
            'snapshot_sha256': digest_bytes(canonical_bytes(snapshot)),
            'raw_inventory': deepcopy(snapshot['rows']), 'dispositions': dispositions,
            'conflicts': conflicts, 'unknowns': unknowns,
            'process_inventory': deepcopy(conflicts + unknowns),
            'scope': SELECTOR, 'limitation': 'No host capacity, reservation, licence or launch-readiness determination.'}


def classify_process_inventory(snapshot, *, expected_host, cfd_binding, now, maximum_age_seconds):
    """Validate and classify selected rows without observing or changing the host.

    Decimal times preserve PID creation identity. Missing parents outside a bound
    controller root are permitted; missing members of its pinned family are not.
    """
    if isinstance(snapshot, dict) and snapshot.get("schema") == "process-snapshot-2":
        from digitalmodel.ansys.cylinder_process_inventory_v2 import classify
        return classify(snapshot, expected_host=expected_host, cfd_binding=cfd_binding,
                        now=now, maximum_age_seconds=maximum_age_seconds)
    try:
        rows, observed = _snapshot(snapshot, expected_host, now, maximum_age_seconds)
        pins, identities = _binding(cfd_binding, expected_host, observed)
        return _result(snapshot, rows, pins, identities)
    except (KeyError, TypeError, OverflowError) as error:
        raise ValueError('Malformed selected process evidence') from error
