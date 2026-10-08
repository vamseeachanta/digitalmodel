"""Pure v2 structural agreement with externally verified pins; no authority."""
from copy import deepcopy
from pathlib import PureWindowsPath
import re
from .cylinder_wrapper_consoles import console_pids, historical_projection


def _base():
    from digitalmodel.ansys import cylinder_process_inventory
    return cylinder_process_inventory


def _sources(row):
    v = _base()
    sources = row['script_sources']
    if not isinstance(sources, list) or len(sources) > 16:
        raise ValueError('Invalid v2 sources')
    indices = set()
    for source in sources:
        if not isinstance(source, dict) or set(source) != {'argv_index', 'path', 'sha256', 'resolution_basis'}:
            raise ValueError('Invalid indexed source fields')
        i = source['argv_index']
        if type(i) is not int or not 1 <= i < len(row['argv']) or i in indices:
            raise ValueError('Invalid or duplicate source index')
        indices.add(i)
        token = row['argv'][i]
        if not token.lower().endswith('.py'):
            raise ValueError('Source index is not a script')
        path = PureWindowsPath(token)
        if source['resolution_basis'] == 'absolute_argument':
            expected = v._path(token)
        elif source['resolution_basis'] == 'observed_interpreter_cwd':
            parts = token.replace('\\', '/').split('/')
            if path.anchor or any(not x or x in ('.', '..') or x.endswith((' ', '.')) or ':' in x for x in parts):
                raise ValueError('Unsafe relative script token')
            expected = v._path(str(PureWindowsPath(v._path(row['cwd'])) / path))
        else:
            raise ValueError('Unknown source resolution basis')
        if expected != v._path(source['path']):
            raise ValueError('Source path does not resolve from indexed argument')
        v._hash(source['sha256'])


def _rows(values, observed):
    v = _base()
    if not isinstance(values, list) or len(values) > 4096:
        raise ValueError('Invalid bounded v2 rows')
    projected = []
    for row in values:
        if not isinstance(row, dict) or set(row) != v.ROW_FIELDS | {'cwd', 'executable_resolved_path'}:
            raise ValueError('Invalid ten-field v2 row')
        item = {k: row[k] for k in v.ROW_FIELDS}
        item['script_sources'] = []
        v._row(item, observed)
        v._path(row['executable_resolved_path'])
        if row['cwd'] is not None:
            v._path(row['cwd'])
        _sources(row)
        projected.append(item)
    v._rows(projected, observed)
    return {row['pid']: row for row in values}


def _errors(snapshot, rows):
    v = _base()
    errors = snapshot.get('errors')
    if not isinstance(errors, list) or len(errors) > 4096:
        raise ValueError('Invalid bounded detail errors')
    seen = set(rows)
    allowed = {'missing_process', 'identity_changed', 'inaccessible_detail', 'unresolved_source', 'invalid_forwarder'}
    for error in errors:
        if not isinstance(error, dict) or set(error) != {'pid', 'reason_code'}:
            raise ValueError('Invalid detail error schema')
        pid = v._pid(error['pid'])
        if pid in seen or error['reason_code'] not in allowed:
            raise ValueError('Overlapping error PID or unknown reason')
        seen.add(pid)
    coverage = snapshot.get('coverage')
    if not isinstance(coverage, dict):
        raise ValueError('Missing coverage')
    if coverage.get('selected_count') != len(rows) + len(errors):
        raise ValueError('Contradictory selected error counts')
    if coverage.get('selected_details_complete') is not (not errors):
        raise ValueError('Contradictory selected completeness')
    return errors


def _snapshot(snapshot, host, now, age):
    v = _base()
    if not isinstance(snapshot, dict) or snapshot.get('schema') != 'process-snapshot-2':
        raise ValueError('Unsupported v2 snapshot')
    rows = _rows(snapshot.get('rows'), v._time(snapshot.get('observed_at')))
    errors = _errors(snapshot, rows)
    # Reuse unchanged v1 time/host/count checks on a complete projection.
    projected = dict(snapshot, schema='process-snapshot-1')
    projected['rows'] = [{**{k:r[k] for k in v.ROW_FIELDS}, 'script_sources': []} for r in rows.values()]
    projected['coverage'] = dict(snapshot['coverage'])
    for key in ('enumerated_count', 'selected_count', 'excluded_count'):
        value = projected['coverage'].get(key)
        if type(value) is not int or not 0 <= value <= 1000000:
            raise ValueError('Invalid typed coverage count')
    c = projected['coverage']
    if c['enumerated_count'] != c['selected_count'] + c['excluded_count']:
        raise ValueError('Contradictory enumeration count')
    c.update(selected_count=len(rows), excluded_count=c['excluded_count']+len(errors), selected_details_complete=True)
    _, observed = v._snapshot(projected, host, now, age)
    return rows, errors, observed


def _forwarders(records, rows, *, incomplete=False):
    v = _base()
    fields = {'parent_pid', 'parent_creation_time', 'child_pid', 'child_creation_time',
              'launcher_sha256', 'resource_kind_hex', 'embedded_target_literal',
              'target_alias_path', 'alias_identity', 'resolved_target_path', 'resolved_target_sha256'}
    if not isinstance(records, list) or len(records) > 16:
        raise ValueError('Invalid bounded forwarder observations')
    parents, children = set(), set()
    for f in records:
        if not isinstance(f, dict) or set(f) != fields:
            raise ValueError('Invalid forwarder fields')
        parent, child = v._pid(f['parent_pid']), v._pid(f['child_pid'])
        if parent == child or parent in parents or child in children:
            raise ValueError('Duplicate or self forwarder relation')
        parents.add(parent)
        children.add(child)
        v._time(f['parent_creation_time'])
        v._time(f['child_creation_time'])
        v._hash(f['launcher_sha256'])
        v._hash(f['resolved_target_sha256'])
        kind = f['resource_kind_hex']
        if not isinstance(kind, str) or not re.fullmatch('02(?:[0-9a-f]{2}){0,15}', kind):
            raise ValueError('Unsupported forwarder kind bytes')
        if v._path(f['embedded_target_literal']) != v._path(f['target_alias_path']):
            raise ValueError('Embedded target and normalized alias differ')
        v._path(f['resolved_target_path'])
        _alias(f['alias_identity'], f['target_alias_path'])
        if parent not in rows or child not in rows:
            if incomplete:
                continue
            raise ValueError('Forwarder endpoint missing')
        _relation(f, rows[parent], rows[child])
    return parents, children


def _alias(alias, target):
    v = _base()
    if not isinstance(alias, dict) or set(alias) != {'junction_path', 'device', 'inode', 'raw_link_target'}:
        raise ValueError('Invalid alias identity')
    junction = PureWindowsPath(v._path(alias['junction_path']))
    if junction != PureWindowsPath(v._path(target)).parent:
        raise ValueError('Alias junction is not the declared target parent')
    for key in ('device', 'inode'):
        if type(alias[key]) is not int or not 0 <= alias[key] < 2**128:
            raise ValueError('Invalid alias filesystem identity')
    v._text(alias['raw_link_target'])


def _relation(f, parent, child):
    v = _base()
    if (parent['creation_time'] != f['parent_creation_time']
            or child['creation_time'] != f['child_creation_time']
            or child['parent_pid'] != parent['pid']
            or parent['executable_sha256'] != f['launcher_sha256']
            or parent['script_sources'] or parent['argv'][1:] != child['argv'][1:]):
        raise ValueError('Forwarder identity, source suppression or argument tail differs')
    if (v._path(child['executable_path']) not in {v._path(f['target_alias_path']), v._path(f['resolved_target_path'])}
            or v._path(child['executable_resolved_path']) != v._path(f['resolved_target_path'])
            or child['executable_sha256'] != f['resolved_target_sha256']):
        raise ValueError('Forwarder child image differs')


def _binding(binding, host, observed):
    v = _base()
    if binding is None:
        return {}, set(), []
    if not isinstance(binding, dict) or binding.get('schema') != 'cfd-process-binding-2' or binding.get('host') != host:
        raise ValueError('Unsupported v2 binding host/schema')
    owner = binding.get('owner_reference')
    if not isinstance(owner, dict) or set(owner) != {'id', 'sha256'}:
        raise ValueError('Missing structural owner reference')
    v._text(owner['id'])
    v._hash(owner['sha256'])
    roles, ids = v._roles(binding)
    pins = _rows(binding.get('processes'), observed)
    if set(pins) != ids:
        raise ValueError('Every v2 pin requires exactly one role')
    parents, children = _forwarders(binding.get('forwarders'), pins)
    if not parents.issubset(roles['wrappers']) or not children.issubset({roles['controller'][0], roles['guard'][0]}):
        raise ValueError('Only wrapper roles may forward to controller/guard')
    for key in ('controller', 'guard', 'wrappers'):
        for pid in roles[key]:
            r = pins[pid]
            if pid not in parents:
                expected = {i for i,a in enumerate(r['argv']) if i and a.lower().endswith('.py')}
                if not expected or expected != {s['argv_index'] for s in r['script_sources']} or {'-m','-c'} & set(r['argv']):
                    raise ValueError('Missing complete interpreter script mapping')
    expected = {'mpi':'mpiexec.exe','helper':'smpd.exe','ranks':'interfoam.exe','console_helpers':'conhost.exe'}
    for role, name in expected.items():
        if any(pins[i]['name'].casefold() != name for i in roles[role]):
            raise ValueError('Role executor differs')
    supplemental = console_pids(binding, pins, binding['forwarders'])
    historical = historical_projection(binding, supplemental)
    historical_roles, _ = v._roles(historical)
    v._topology({pid: row for pid, row in pins.items() if pid not in supplemental}, historical_roles)
    argv = pins[roles['mpi'][0]]['argv']
    if argv.count('-n') != 1 or argv.index('-n')+1 >= len(argv) or argv[argv.index('-n')+1] != str(len(roles['ranks'])):
        raise ValueError('Pinned rank count differs')
    return pins, ids, binding['forwarders']


def classify(snapshot, *, expected_host, cfd_binding, now, maximum_age_seconds):
    v = _base()
    try:
        rows, errors, observed = _snapshot(snapshot, expected_host, now, maximum_age_seconds)
        _forwarders(snapshot.get('forwarders'), rows, incomplete=bool(errors))
        pins, ids, forwards = _binding(cfd_binding, expected_host, observed)
        matches = sorted(snapshot['forwarders'], key=lambda f:f['parent_pid']) == sorted(forwards, key=lambda f:f['parent_pid'])
        result = v._result(snapshot, rows, pins if matches and not errors else {}, ids if matches and not errors else set())
        if not matches or errors:
            missing = ids - set(rows) - {e['pid'] for e in errors}
            result['unknowns'].extend(
                {'pid':pid, 'creation_time':pins[pid]['creation_time'],
                 'classification':'UNKNOWN', 'reason':'bound-process-missing-from-snapshot'}
                for pid in sorted(missing))
            if result['unknowns'] and not result['conflicts']:
                result['status'] = 'UNKNOWN'
            result['process_inventory'] = deepcopy(result['conflicts'] + result['unknowns'])
        if errors:
            result['status'] = 'UNKNOWN'
            additions = [{'pid':e['pid'], 'classification':'UNKNOWN', 'reason':e['reason_code'], 'creation_time':None} for e in errors]
            result['unknowns'].extend(additions)
            result['process_inventory'] = deepcopy(result['conflicts'] + result['unknowns'])
        result['evidence_scope'] = 'conditional_structural_classification'
        result['incomplete'] = bool(errors)
        from .cylinder_wrapper_consoles import observed_console_pids
        result['wrapper_console_pids'] = sorted(observed_console_pids(
            rows, snapshot['forwarders']))
        return result
    except (KeyError, TypeError, OverflowError) as error:
        raise ValueError('Malformed v2 process evidence') from error
