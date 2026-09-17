"""Explicit v2 factual collection; no ownership exemption or native execution."""
from fractions import Fraction
from copy import deepcopy
from pathlib import Path
import re
import socket
import time

import psutil

from .cylinder_diagnostic_snapshot import _seed
from .cylinder_parent_map import enumerate_windows_v2 as _enumerate, parent_of
from .cylinder_forwarder_resources import DeclaredReads, observe_forwarder
from .cylinder_absence_population import PopulationError, reconcile_observed_inventories
from .cylinder_active_names import ActiveNameResolution, parent_map_evidence


def _created(value):
    if not isinstance(value,str) or not re.fullmatch(r'\d{1,20}(?:\.\d{1,20})?',value):
        raise ValueError('bounded creation timestamp required')
    return Fraction(value)


def _pid(value):
    if type(value) is not int or not 0 < value < 2**32:
        raise ValueError('positive process ID required')
    return value


def _configuration(binding, discovery_seed):
    if (binding is None) == (discovery_seed is None):
        raise ValueError('exactly one v2 mode required')
    mode = binding if binding is not None else discovery_seed
    schema = 'cfd-process-binding-2' if binding is not None else 'process-discovery-seed-2'
    if not isinstance(mode,dict) or mode.get('schema') != schema:
        raise ValueError('explicit v2 schema required')
    if mode.get('host') != socket.gethostname():
        raise ValueError('collection host differs')
    selected = mode.get('processes' if binding is not None else 'selected_processes')
    candidates = mode.get('forwarders' if binding is not None else 'forwarder_candidates')
    if not isinstance(selected,list) or not isinstance(candidates,list):
        raise ValueError('explicit selected identities and forwarders required')
    count = len(mode.get('wrappers',[])) if binding is not None else len(candidates)
    if len(candidates) > min(16,count):
        raise ValueError('forwarder count exceeds profile')
    pinned = {}
    for row in selected:
        pid = _pid(row['pid']);_created(row['creation_time'])
        if pid in pinned:
            raise ValueError('duplicate selected process')
        pinned[pid] = row['creation_time']
    nominees = []
    for row in candidates:
        nominee = {key:row[key] for key in ('parent_pid','parent_creation_time','child_pid','child_creation_time')}
        nominee['declared_target_alias'] = row['target_alias_path' if binding is not None else 'declared_target_alias']
        for role in ('parent','child'):
            if pinned.get(_pid(row[role+'_pid'])) != row[role+'_creation_time']:
                raise ValueError('forwarder identity is not selected')
        if binding is not None and row['parent_pid'] not in mode['wrappers']:
            raise ValueError('forwarding parent must have wrapper role')
        nominees.append(nominee)
    if len({r['parent_pid'] for r in nominees}) != len(nominees):
        raise ValueError('duplicate forwarding parent')
    if len({r['child_pid'] for r in nominees}) != len(nominees):
        raise ValueError('duplicate forwarded child')
    return pinned,nominees


def _selection(initial,pinned):
    indexed = {row['pid']:row for row in initial}
    if len(indexed) != len(initial) or not set(pinned) <= set(indexed):
        raise ValueError('missing or duplicate initial nominated identity')
    family = {row['pid'] for row in initial if _seed(row)} | set(pinned)
    while True:
        expanded = family | {row['pid'] for row in initial if row['parent_pid'] in family}
        if expanded == family:
            break
        family = expanded
    return [row for row in initial if row['pid'] in family | set(pinned)]


def _state(process, parent_map):
    argv = process.cmdline()
    if (not isinstance(argv,list) or len(argv)>512
            or any(not isinstance(a,str) or len(a)>4096 or '\0' in a for a in argv)):
        raise ValueError('invalid command line')
    try:
        cwd = process.cwd()
    except (psutil.AccessDenied,OSError):
        cwd = None
    if cwd is not None and (not isinstance(cwd,str) or len(cwd)>4096):
        raise ValueError('invalid process cwd')
    return dict(pid=process.pid,parent_pid=parent_of(parent_map,process.pid),creation_time=str(process.create_time()),
                name=process.name(),executable_path=process.exe(),argv=argv,cwd=cwd)


def _sources(state,cache):
    if state['name'].lower().startswith('python') and any(a in ('-c','-m') for a in state['argv'][1:]):
        raise ValueError('inline/module interpreter invocation unsupported')
    result = []
    for index,arg in enumerate(state['argv']):
        if not arg.lower().endswith('.py'):
            continue
        path = Path(arg)
        if path.is_absolute():
            basis = 'absolute_argument'
        else:
            if not state['cwd'] or path.drive or '..' in path.parts or path.root:
                raise ValueError('relative script escapes or cwd unavailable')
            path = Path(state['cwd'])/path
            basis = 'observed_interpreter_cwd'
        _,identity = cache.read(str(path))
        result.append(dict(argv_index=index,path=str(path),sha256=identity['sha256'],resolution_basis=basis))
    return result


def _detail(initial,cache,forwarding,alias,started,pinned,parent_map):
    process = psutil.Process(initial['pid'])
    state = _state(process,parent_map)
    if (any(state[k] != initial[k] for k in initial)
            or _created(state['creation_time']) > _created(started)
            or state['pid'] in pinned and pinned[state['pid']] != state['creation_time']):
        raise ValueError('identity_changed')
    _,identity = cache.read(state['executable_path'],alias=alias,
                            maximum=16*1024**2 if forwarding else None,
                            allow_hardlinks=alias is None and not forwarding)
    try:
        sources = [] if forwarding else _sources(state,cache)
    except (ValueError,OSError) as exc:
        raise ValueError('unresolved_source') from exc
    return dict(state,executable_sha256=identity['sha256'],
                executable_resolved_path=identity['resolved_path'],script_sources=sources),state


def _error(error):
    if isinstance(error,psutil.NoSuchProcess):
        return 'missing_process'
    if isinstance(error,(psutil.Error,OSError)):
        return 'inaccessible_detail'
    return 'unresolved_source' if str(error)=='unresolved_source' else 'identity_changed'


def _details(selected,pinned,candidates,cache,started,parent_map):
    parents = {row['parent_pid'] for row in candidates}
    aliases = {row['child_pid']:row['declared_target_alias'] for row in candidates}
    rows,states,errors = {},{},{}
    for row in selected:
        pid = row['pid']
        try:
            rows[pid],states[pid] = _detail(row,cache,pid in parents,aliases.get(pid),started,pinned,parent_map)
        except (psutil.Error,OSError,ValueError) as error:
            errors[pid] = dict(pid=pid,reason_code=_error(error))
    return rows,states,errors


def _forwarders(candidates,rows,cache,errors):
    records = []
    for candidate in candidates:
        pid = candidate['parent_pid']
        if pid not in rows or candidate['child_pid'] not in rows:
            errors[pid] = dict(pid=pid,reason_code='invalid_forwarder')
            rows.pop(pid,None)
            continue
        try:
            records.append(observe_forwarder(candidate,rows,cache))
        except (OSError,ValueError,KeyError):
            errors[pid] = dict(pid=pid,reason_code='invalid_forwarder')
            rows.pop(pid,None)
    return records


def _stability(rows,states,errors,cache,parent_map):
    try:
        cache.verify()
    except (OSError,ValueError):
        for pid in list(rows):
            errors[pid] = dict(pid=pid,reason_code='identity_changed');rows.pop(pid)
    for pid in list(rows):
        try:
            process = psutil.Process(pid)
            if _state(process,parent_map) != states[pid] or not process.is_running():
                raise ValueError('identity_changed')
        except (psutil.Error,OSError,ValueError) as error:
            errors[pid] = dict(pid=pid,reason_code=_error(error));rows.pop(pid)


def _observed_stage(inventories, maps, names):
    raw = _enumerate()
    try:
        rows = names.observe(raw)
    except (ValueError, OSError) as exc:
        events = [dict(pid=row.get('pid'), disposition='REFUSED_INCOMPLETE',
                       change='blank_process_name') for row in raw
                  if isinstance(row, dict) and
                  (not isinstance(row.get('name'), str) or not row['name'].strip())]
        raise PopulationError(str(exc), dict(events=events, rejected_inventory=deepcopy(raw),
            refusal_reason=str(exc), event_scope='Raw blank-name candidates, not causal attribution',
            limitation='Process observation or name-resolution validation failed; collection refuses.')) from exc
    for row in rows:
        if not isinstance(row.get('name'), str) or not row['name'].strip():
            raise PopulationError('Incomplete observed process name', {
                'events': [dict(pid=row.get('pid'), disposition='REFUSED_INCOMPLETE',
                                change='blank_process_name')],
                'rejected_inventory': deepcopy(rows),
                'limitation': 'Normalized process-name invariant failed; collection refuses.'})
    inventories.append(deepcopy(rows))
    maps.append({row['pid']: row['parent_pid'] for row in rows})
    return rows


def _observed_collection(pinned, candidates, started):
    inventories, maps = [], []
    names = ActiveNameResolution()
    stage = 'A'
    try:
        initial = _observed_stage(inventories, maps, names)
        selected = _selection(initial, pinned)
        stage = 'B'
        _observed_stage(inventories, maps, names)
        stage = 'detail_reads'
        cache = DeclaredReads()
        rows, states, errors = _details(selected, pinned, candidates, cache, started, maps[1])
        stage = 'forwarders'
        forwarders = _forwarders(candidates, rows, cache, errors)
        stage = 'C'
        final = _observed_stage(inventories, maps, names)
        stage = 'reconciliation'
        population = reconcile_observed_inventories(
            inventories, maps, required_relevant_pids=sorted(row['pid'] for row in selected))
        stage = 'final_selection'
        if {r['pid'] for r in _selection(final, pinned)} != {r['pid'] for r in selected}:
            raise ValueError('Final selected population differs')
        stage = 'stability'
        _stability(rows, states, errors, cache, maps[2])
        stage = 'evidence_accounting'
        population['parent_maps'] = parent_map_evidence(maps)
        population['parent_map_basis'] = 'row-derived views of completed identity observations'
        population['enumeration_limitation'] = (
            'Pre-yield disappearance may omit identities; within-observation inconsistency refuses.')
        population['name_resolution'] = names.evidence()
        if population['name_resolution']['completed_stages'] != len(inventories):
            raise ValueError('Name-resolution completion count differs from inventories')
        return initial, selected, cache, rows, forwarders, errors, inventories, population
    except (ValueError, OSError, psutil.Error) as exc:
        evidence = deepcopy(getattr(exc, 'evidence', {}))
        evidence.update(failed_stage=stage, completed_inventories=inventories,
                        completed_parent_maps=parent_map_evidence(maps), name_resolution=names.evidence())
        evidence.setdefault('events', [])
        raise PopulationError(str(exc), evidence) from exc


def collect_v2(binding=None, *, discovery_seed=None):
    """Collect nominated roles without granting their ownership or exemption."""
    started = str(time.time())
    try:
        pinned,candidates = _configuration(binding,discovery_seed)
    except (KeyError,TypeError) as exc:
        raise ValueError('invalid v2 collector configuration') from exc
    initial,selected,cache,rows,forwarders,errors,inventories,population = _observed_collection(
        pinned,candidates,started)
    forwarders = [r for r in forwarders if r['parent_pid'] in rows and r['child_pid'] in rows]
    return dict(schema='process-snapshot-2',host=socket.gethostname(),observed_at=started,
        enumeration_complete=True,initial_inventory=initial,rows=list(rows.values()),
        observed_inventories=inventories,population_check=population,
        forwarders=forwarders,errors=list(errors.values()),declared_file_observations=cache.evidence(),
        coverage=dict(selector='ansys-mpi-lineage-v1',
        enumerated_count=len(initial),selected_count=len(selected),excluded_count=len(initial)-len(selected),
        selected_details_complete=not errors,
        limitation='Current files and process state only; historical loaded bytes unestablished.'))
