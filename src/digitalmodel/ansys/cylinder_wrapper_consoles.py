"""Pinned controller/guard branch consoles; conditional structural evidence."""
import base64
from copy import deepcopy

from .cylinder_process_inventory import _hash, _pid, _text


def _branches(forwarders):
    if not isinstance(forwarders, list) or len(forwarders) > 16:
        raise ValueError('Invalid bounded console branches')
    branches, endpoints = [], set()
    for forwarder in forwarders:
        if not isinstance(forwarder, dict):
            raise ValueError('Invalid console branch')
        parent = _pid(forwarder.get('parent_pid'))
        child = _pid(forwarder.get('child_pid'))
        branch = {parent, child}
        if parent == child or endpoints & branch:
            raise ValueError('Console branches must have disjoint endpoints')
        branches.append(branch)
        endpoints.update(branch)
    return branches


def observed_console_pids(rows, forwarders):
    endpoints = set().union(*_branches(forwarders)) if forwarders else set()
    return {pid for pid, row in rows.items()
            if row['parent_pid'] in endpoints and row['name'].casefold() == 'conhost.exe'
            and not row['script_sources']}


def console_pids(binding, pins, validated_forwarders):
    if 'wrapper_console_supplement' not in binding:
        return set()
    declaration = binding['wrapper_console_supplement']
    if not isinstance(declaration, dict) or set(declaration) != {'id', 'sha256', 'pids'}:
        raise ValueError('Malformed wrapper console declaration')
    _text(declaration['id'])
    _hash(declaration['sha256'])
    values = declaration['pids']
    if not isinstance(values, list) or len(values) != 2:
        raise ValueError('Exactly two supplemental console PIDs required')
    pids = {_pid(value) for value in values}
    if len(pids) != 2 or not pids <= set(binding['console_helpers']):
        raise ValueError('Duplicate or unassigned supplemental console PID')
    branches = _branches(validated_forwarders)
    if len(branches) != 2:
        raise ValueError('Exactly two validated console branches required')
    assignments = [0, 0]
    for pid in pids:
        if pid not in pins:
            raise ValueError('Supplemental console process is not pinned')
        row = pins[pid]
        if row['name'].casefold() != 'conhost.exe' or row['script_sources']:
            raise ValueError('Supplement must contain source-free console processes')
        matches = [index for index, branch in enumerate(branches)
                   if row['parent_pid'] in branch]
        if len(matches) != 1:
            raise ValueError('Supplemental console parent is not a bound branch endpoint')
        assignments[matches[0]] += 1
    if assignments != [1, 1]:
        raise ValueError('Exactly one supplemental console per branch required')
    observed = observed_console_pids(pins, validated_forwarders)
    if observed != pids:
        raise ValueError('Observed wrapper consoles differ from declaration')
    return observed


def historical_projection(binding, pids):
    projected = deepcopy(binding)
    projected['processes'] = [r for r in projected['processes'] if r['pid'] not in pids]
    projected['console_helpers'] = [pid for pid in projected['console_helpers'] if pid not in pids]
    projected.pop('wrapper_console_supplement', None)
    return projected


def _references(operation, binding):
    has_operation = 'cfd_wrapper_console_evidence' in operation
    has_binding = 'wrapper_console_supplement' in binding
    if not has_operation and not has_binding:
        return None
    if not has_operation or not has_binding:
        raise ValueError('Both wrapper console evidence references required')
    reference, declaration = operation['cfd_wrapper_console_evidence'], binding['wrapper_console_supplement']
    if (not isinstance(reference, dict) or set(reference) != {'id', 'path', 'sha256'}
            or not isinstance(declaration, dict) or set(declaration) != {'id', 'sha256', 'pids'}):
        raise ValueError('Malformed wrapper console references')
    if any(reference[key] != declaration[key] for key in ('id', 'sha256')):
        raise ValueError('Wrapper console evidence reference differs')
    return reference


def verify_agreement(operation, binding, owner, classification=None):
    """Bind conditional classification to the independently resolved source."""
    binding = {} if binding is None else binding
    reference = _references(operation, binding)
    expected = []
    if reference is not None:
        if binding.get('schema') != 'cfd-process-binding-2':
            raise ValueError('Wrapper console supplement requires v2 binding')
        values = binding['wrapper_console_supplement']['pids']
        if not isinstance(values, list) or len(values) != 2:
            raise ValueError('Two wrapper console PIDs required')
        expected = sorted({_pid(value) for value in values})
        if len(expected) != 2:
            raise ValueError('Duplicate wrapper console PIDs')
    observations = [owner.get('wrapper_console_pids', [])]
    if classification is not None:
        observations.append(classification.get('wrapper_console_pids', []))
    for values in observations:
        if (not isinstance(values, list) or any(type(pid) is not int for pid in values)
                or values != expected):
            raise ValueError('Wrapper console owner/classification agreement differs')


def resolve_supplement(operation, binding, read_callback):
    """Verify historical source bytes before projecting unchanged owner checks."""
    from .cylinder_cfd_owner_evidence import _json, _read
    from .cylinder_process_inventory_v2 import _binding, _snapshot

    try:
        reference = _references(operation, binding)
        if reference is None:
            return binding, None
        raw = _read(reference['path'], reference['sha256'], read_callback, [])
        source = _json(raw)
        rows, errors, observed = _snapshot(source, binding['host'], source['observed_at'], '1')
        pins, _, forwarders = _binding(binding, binding['host'], observed)
        if (errors or len(rows) != 128 or rows != pins
                or source['forwarders'] != forwarders or len(binding['console_helpers']) != 62):
            raise ValueError('Complete pinned 128-row supplement source differs')
        pids = console_pids(binding, rows, source['forwarders'])
        if pids != set(binding['wrapper_console_supplement']['pids']):
            raise ValueError('Historical observed consoles differ from declaration')
        projected = historical_projection(binding, pids)
        evidence = dict(source_reference={key: reference[key] for key in ('id', 'sha256')},
            raw_base64=base64.b64encode(raw).decode('ascii'), raw_sha256=reference['sha256'],
            source_observed_at=source['observed_at'], pids=sorted(pids),
            scope='historical source supplement; fresh admission observation still required')
        return projected, evidence
    except (KeyError, TypeError, IndexError, AttributeError, OverflowError) as error:
        raise ValueError('Malformed wrapper console source evidence') from error
