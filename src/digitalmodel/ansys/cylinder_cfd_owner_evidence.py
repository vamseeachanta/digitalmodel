"""Fixed factual owner joins; no permission or historical load authentication."""
import base64
from decimal import Decimal
import json
from pathlib import PureWindowsPath

from digitalmodel.ansys.analysis_records import digest_bytes
from digitalmodel.ansys.cylinder_process_inventory import (
    ROW_FIELDS, _hash, _path, _pid, _roles, _text, _time, _topology,
)

MAX_BYTES = 4 * 1024 * 1024


def _pairs(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError('Duplicate owner JSON field')
        result[key] = value
    return result


def _constant(value):
    raise ValueError('Nonfinite owner JSON number')


def _bounded(value):
    stack, count = [(value, 0)], 0
    while stack:
        item, depth = stack.pop()
        count += 1
        if depth > 24 or count > 100000:
            raise ValueError('Owner JSON nesting/node budget exceeded')
        if isinstance(item, dict):
            if len(item) > 10000:
                raise ValueError('Owner object exceeds bounds')
            stack.extend((v, depth + 1) for v in item.values())
        elif isinstance(item, list):
            if len(item) > 10000:
                raise ValueError('Owner array exceeds bounds')
            stack.extend((v, depth + 1) for v in item)
        elif isinstance(item, str) and len(item) > 65536:
            raise ValueError('Owner text exceeds bounds')
        elif isinstance(item, Decimal) and not item.is_finite():
            raise ValueError('Nonfinite owner decimal')


def _json(raw):
    try:
        value = json.loads(raw.decode('utf-8'), object_pairs_hook=_pairs,
                           parse_float=Decimal, parse_constant=_constant)
        _bounded(value)
    except (UnicodeError, RecursionError, json.JSONDecodeError) as error:
        raise ValueError('Invalid bounded owner JSON') from error
    if not isinstance(value, dict):
        raise ValueError('Owner document must be object')
    return value


def _read(path, expected, callback, retained):
    _path(path); _hash(expected)
    raw = callback(path, expected)
    if not isinstance(raw, bytes) or len(raw) > MAX_BYTES or digest_bytes(raw) != expected:
        raise ValueError('Owner source bytes differ or exceed bounds')
    retained.append({'path':path, 'sha256':expected, 'bytes':len(raw)})
    return raw


def _same_path(left, right):
    if _path(left) != _path(right):
        raise ValueError('Owner source path relationship differs')


def _controller_argument(value, cwd):
    """Resolve only a basename or strict local absolute file identity."""
    _text(value)
    parts = value.replace('\\', '/').split('/')
    path = PureWindowsPath(value)
    absolute = path.is_absolute()
    tail = parts[1:] if absolute else parts
    if (any(not p or p in ('.', '..') or p.endswith((' ', '.'))
            or any(c in p for c in '<>:"|?*') for p in tail)
            or (not absolute and (path.anchor or len(parts) != 1))):
        raise ValueError('Unexpected controller argument identity')
    resolved = value if absolute else str(PureWindowsPath(cwd) / value)
    _path(resolved)
    return resolved


def _identities(receipt, binding):
    if binding.get('schema') != 'cfd-process-binding-2':
        raise ValueError('Owner resolution requires v2 binding')
    if receipt['snapshot']['host'] != binding['host']:
        raise ValueError('Owner host differs')
    roles, ids = _roles(binding)
    rows = binding['processes']
    if not isinstance(rows, list) or len(rows) != 126:
        raise ValueError('stale owner receipt: expected exact 126-member bound family')
    pins = {}
    for row in rows:
        pid = _pid(row['pid']); _pid(row['parent_pid'], parent=True)
        _time(row['creation_time'])
        if pid in pins:
            raise ValueError('Duplicate owner process pin')
        pins[pid] = row
    if set(pins) != ids:
        raise ValueError('Owner roles do not cover process pins')
    ancestors = receipt['current_cfd_owner_evidence']['ancestors']
    if not isinstance(ancestors, list) or len(ancestors) != 4:
        raise ValueError('Expected four historical ancestors')
    if roles['controller'] != [ancestors[2]['pid']] or roles['guard'] != [ancestors[0]['pid']]:
        raise ValueError('Controller/guard role differs')
    if set(roles['wrappers']) != {ancestors[3]['pid'], ancestors[1]['pid']}:
        raise ValueError('Forwarder wrapper roles differ')
    for ancestor in ancestors:
        pin = pins[_pid(ancestor['pid'])]
        for key in ('pid', 'parent_pid', 'creation_time', 'argv', 'cwd'):
            if type(ancestor[key]) is not type(pin[key]) or ancestor[key] != pin[key]:
                raise ValueError('Historical ancestor identity differs')
    _ancestry(pins)
    _family(receipt['snapshot']['rows'], pins, roles, ancestors)
    return pins, roles, ancestors



def _ancestry(pins):
    for pid, row in pins.items():
        parent = pins.get(row['parent_pid'])
        if parent and _time(parent['creation_time']) > _time(row['creation_time']):
            raise ValueError('Owner parent created after child')
        cursor, seen = pid, set()
        while cursor in pins:
            if cursor in seen:
                raise ValueError('Cyclic owner ancestry')
            seen.add(cursor)
            cursor = pins[cursor]['parent_pid']


def _family(rows, pins, roles, ancestors):
    if not isinstance(rows, list) or len(rows) != 122:
        raise ValueError('stale owner receipt: expected exact 122-row execution family')
    seen = set()
    for row in rows:
        pid = _pid(row['pid'])
        if set(row) != ROW_FIELDS or pid in seen or pid not in pins:
            raise ValueError('Historical execution row coverage differs')
        seen.add(pid)
        for key in ROW_FIELDS:
            if type(row[key]) is not type(pins[pid][key]) or row[key] != pins[pid][key]:
                raise ValueError('Historical execution identity differs')
    if seen != set(pins) - {a['pid'] for a in ancestors}:
        raise ValueError('Historical execution role set differs')
    if len(roles['ranks']) != 60 or len(roles['console_helpers']) != 60:
        raise ValueError('stale owner receipt: fixed rank/console count differs')
    for role, name in {'mpi':'mpiexec.exe','helper':'smpd.exe','ranks':'interfoam.exe','console_helpers':'conhost.exe'}.items():
        if any(pins[i]['name'].casefold() != name for i in roles[role]):
            raise ValueError('Owner role executor differs')
    _topology(pins, roles)


def _source_join(ancestor, pin, expected, callback, retained):
    sources = ancestor['sources']
    if not isinstance(sources, list) or len(sources) != len(expected):
        raise ValueError('Historical interpreter source count differs')
    actual = pin['script_sources']
    if not isinstance(actual, list) or len(actual) != len(expected):
        raise ValueError('Bound interpreter source count differs')
    for index, path, digest in expected:
        matches = [s for s in actual if type(s.get('argv_index')) is int and s['argv_index'] == index]
        historic = [s for s in sources if s.get('argv_literal') == pin['argv'][index]]
        if len(matches) != 1 or len(historic) != 1:
            raise ValueError('Interpreter source index join differs')
        source, old = matches[0], historic[0]
        basis = ('absolute_argument' if PureWindowsPath(pin['argv'][index]).is_absolute()
                 else 'observed_interpreter_cwd')
        if source.get('resolution_basis') != basis:
            raise ValueError('Interpreter source resolution basis differs')
        _same_path(source['path'], path); _same_path(old['cwd_resolved_path'], path)
        if source['sha256'] != digest or old['sha256'] != digest or old['exists'] is not True:
            raise ValueError('Interpreter source digest join differs')
        _read(path, digest, callback, retained)


def _config(receipt, pins, roles, callback, retained):
    records = receipt['current_cfd_owner_evidence']['owner_source_records']
    record = records[0]
    cfg = _json(_read(record['path'], record['sha256'], callback, retained))
    if type(cfg.get('schema')) is not int or cfg['schema'] != 1:
        raise ValueError('Unsupported fixed controller configuration schema')
    controller = pins[roles['controller'][0]]
    if len(controller['argv']) != 3:
        raise ValueError('Controller arguments differ')
    _same_path(cfg['root'], controller['cwd'])
    _same_path(str(PureWindowsPath(record['path']).parent), controller['cwd'])
    _same_path(record['path'], _controller_argument(controller['argv'][2], controller['cwd']))
    return cfg


def _branch(receipt, cfg, pins, roles, callback, retained):
    guard = pins[roles['guard'][0]]
    argv = pins[roles['mpi'][0]]['argv']
    if len(guard['argv']) != 4 or argv.count('-case') != 1 or argv.index('-case') + 1 >= len(argv):
        raise ValueError('Guard/MPI case arguments differ')
    case = argv[argv.index('-case') + 1]
    jobs = cfg['jobs']
    if not isinstance(jobs, list) or not 1 <= len(jobs) <= 256:
        raise ValueError('Invalid bounded configured jobs')
    matching = [j for j in jobs if _path(j['case']) == _path(case)]
    if len(matching) != 1:
        raise ValueError('Case must join exactly one configured job')
    job = matching[0]
    _same_path(guard['argv'][3], str(PureWindowsPath(case) / 'trial-config.json'))
    _json(_read(guard['argv'][3], job['config_sha256'], callback, retained))
    queue = receipt['launch_receipt_bounded_search']['child_launch_receipt']
    if (type(queue['wrapper_pid']) is not int or queue['wrapper_pid'] != guard['parent_pid']
            or queue['condition_id'] != job['condition_id']):
        raise ValueError('stale owner receipt: retained child queue branch differs')
    active = receipt['current_cfd_owner_evidence']['active_case']
    if PureWindowsPath(case).name != 'case' or PureWindowsPath(case).parent.name != active:
        raise ValueError('stale owner receipt: active case basename differs')
    return {'condition_id':_text(job['condition_id']), 'case':case, 'active_case':_text(active),
            'wrapper_pid':queue['wrapper_pid']}


def _fixed_sources(receipt, cfg, pins, roles, ancestors, callback, retained):
    controller, guard = pins[roles['controller'][0]], pins[roles['guard'][0]]
    _same_path(guard['argv'][1], cfg['adapter'])
    _same_path(guard['argv'][2], cfg['guard'])
    controller_path = str(PureWindowsPath(controller['cwd']) / 'rotation_queue.py')
    _same_path(_controller_argument(controller['argv'][1], controller['cwd']), controller_path)
    _source_join(ancestors[2], controller,
                 [(1, controller_path, cfg['evidence_sha256']['rotation_queue.py'])], callback, retained)
    _source_join(ancestors[0], guard,
                 [(1, cfg['adapter'], cfg['adapter_sha256']), (2, cfg['guard'], cfg['guard_sha256'])], callback, retained)


def _resolve_original_owner(operation, binding, read_callback):
    """Resolve fixed facts using the caller's redirect-refusing pinned reader.

    The two-argument reader must enforce MAX_BYTES before allocating file bytes.
    Every invocation rechecks fixed files. Volatile controller state is never read.
    Pure classification and these joins do not authenticate operational authority.
    """
    try:
        config, ref = operation['cfd_owner_evidence'], binding['owner_reference']
        if set(config) != {'id','path','sha256'} or set(ref) != {'id','sha256'}:
            raise ValueError('Owner reference schema differs')
        if config['id'] != ref['id'] or config['sha256'] != ref['sha256']:
            raise ValueError('Owner reference differs')
        _text(ref['id']); _hash(ref['sha256'])
        retained = []
        raw = _read(config['path'], config['sha256'], read_callback, retained)
        receipt = _json(raw)
        if receipt.get('schema') != 'pressure-current-resource-facts-1':
            raise ValueError('Unsupported existing owner receipt')
        pins, roles, ancestors = _identities(receipt, binding)
        cfg = _config(receipt, pins, roles, read_callback, retained)
        branch = _branch(receipt, cfg, pins, roles, read_callback, retained)
        _fixed_sources(receipt, cfg, pins, roles, ancestors, read_callback, retained)
        return {'evidence_scope':'conditional_fixed_source_relationships',
                'owner_reference':dict(ref), 'raw_receipt_base64':base64.b64encode(raw).decode('ascii'),
                'raw_receipt_sha256':digest_bytes(raw), 'fixed_file_pins':retained,
                'matched_joins':{'host':_text(binding['host']), 'controller':roles['controller'][0],
                 'guard':roles['guard'][0], 'execution_rows':122, 'branch':branch},
                'limitations':['No authenticated owner or historical loaded-byte claim.',
                 'Volatile state is historical corroboration only and is never read by this resolver.']}
    except (KeyError, TypeError, IndexError, AttributeError, OverflowError) as error:
        raise ValueError('Malformed owner evidence relationships') from error


def resolve_owner_evidence(operation, binding, read_callback):
    """Resolve the original family plus any independently pinned two-row supplement."""
    from .cylinder_wrapper_consoles import resolve_supplement

    historical, supplement = resolve_supplement(operation, binding, read_callback)
    result = _resolve_original_owner(operation, historical, read_callback)
    result['wrapper_console_pids'] = [] if supplement is None else supplement['pids']
    if supplement is not None:
        result['wrapper_console_evidence'] = supplement
    return result
