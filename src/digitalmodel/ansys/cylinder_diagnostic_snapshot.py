"""Read-only bounded host process collection, separate from classification."""
import hashlib
from pathlib import Path
import re
import socket
import time

import psutil


def _enumerate():
    rows = []
    for process in psutil.process_iter():
        try:
            name, parent = process.name(), process.ppid()
            created = process.create_time()
        except (psutil.Error, OSError) as error:
            raise ValueError('initial process name/parent enumeration incomplete') from error
        try:
            executable = process.exe()
        except (psutil.Error, OSError):
            executable = None
        rows.append(dict(pid=process.pid, parent_pid=parent, name=name,
                         executable_path=executable, creation_time=str(created)))
    if len({row['pid'] for row in rows}) != len(rows):
        raise ValueError('duplicate initial process identity')
    return rows


def _digest(path, cache):
    path = Path(path)
    key = str(path.resolve())
    if key not in cache:
        cache[key] = hashlib.sha256(path.read_bytes()).hexdigest()
    return cache[key]


def _details(initial, cache):
    try:
        process = psutil.Process(initial['pid'])
        created = process.create_time()
        name, parent, executable = process.name(), process.ppid(), process.exe()
        argv = process.cmdline()
        if (str(created) != initial['creation_time']
                or name != initial['name'] or parent != initial['parent_pid']
                or executable != initial['executable_path']):
            raise ValueError('selected process changed since enumeration')
        scripts = []
        for arg in argv:
            if arg.lower().endswith('.py'):
                path = Path(arg)
                if not path.is_absolute():
                    raise ValueError('selected script argument has no absolute source identity')
                scripts.append(dict(path=str(path), sha256=_digest(path, cache)))
        row = dict(pid=process.pid, parent_pid=parent, creation_time=str(created),
                   name=name, executable_path=executable,
                   executable_sha256=_digest(executable, cache), argv=argv,
                   script_sources=scripts)
        if process.create_time() != created or not process.is_running():
            raise ValueError('selected process identity changed during capture')
        return row
    except (psutil.Error, OSError) as error:
        raise ValueError('selected process details unavailable') from error


def _seed(row):
    name = row['name'].casefold()
    path = (row['executable_path'] or '').replace('\\', '/').casefold()
    return (bool(re.fullmatch(r'(?:ansys[0-9]*|mapdl)\.exe', name))
            or name in ('mpiexec.exe', 'smpd.exe')
            or '/ansys/bin/' in path)


def collect_process_snapshot(binding=None, *, discovery_seed=None):
    """Select solver/MPI descendants plus explicit CFD pins, never terminal peers.

    Seed-only binding is sufficient for discovery; the classifier separately
    requires full reviewed identities before any CFD exemption.
    """
    if discovery_seed is not None or (isinstance(binding, dict) and binding.get('schema') == 'cfd-process-binding-2'):
        from .cylinder_diagnostic_snapshot_v2 import collect_v2
        return collect_v2(binding, discovery_seed=discovery_seed)
    initial = _enumerate()
    pinned = {row['pid'] for row in (binding or {}).get('processes', [])}
    for key in ('controller', 'guard', 'mpi', 'helper'):
        if key in (binding or {}):
            pinned.add(binding[key])
    for key in ('ranks', 'wrappers', 'console_helpers'):
        pinned.update((binding or {}).get(key, []))
    family = {row['pid'] for row in initial if _seed(row)} | pinned
    while True:
        expanded = family | {r['pid'] for r in initial if r['parent_pid'] in family}
        if expanded == family:
            break
        family = expanded
    selected = [r for r in initial if r['pid'] in family | pinned]
    cache = {}
    rows = [_details(row, cache) for row in selected]
    return dict(schema='process-snapshot-1', host=socket.gethostname(),
                observed_at=str(time.time()), enumeration_complete=True, rows=rows,
                initial_inventory=initial, coverage=dict(selector='ansys-mpi-lineage-v1',
                    enumerated_count=len(initial), selected_count=len(rows),
                    excluded_count=len(initial)-len(rows), selected_details_complete=True,
                    limitation='Nonselected protected image paths may be unavailable.'))
