"""Pinned Windows psutil parent tables, independent across observation phases."""
import hashlib
import os
from pathlib import Path
from copy import deepcopy

import psutil
from psutil import _common, _compat
from .cylinder_parent_identities import capture_parent_identities

if os.name == 'nt':
    from psutil import _pswindows as backend
else:
    backend = None

PSUTIL_VERSION = '5.9.6'
SOURCE_DIGESTS = {
    '_common.py': '385e25e7b82289c3c8a16073e2e546be8fca3c344aab9d762c5644a0fa9a83f1',
    '_compat.py': '4f8286d99e2b2301a5edcded8c24cbea3673a94809a07601884ea92f192baf82',
    '_psutil_windows.pyd': '43e09408673687a787415912336ac13fcca9a7d7945b73d0c84ac4bb071e9106',
    '__init__.py': '04b07a82ef53a2308857f9d3e538561ed182121426f6f07eb24017f8a5f48007',
    '_pswindows.py': '267f2738387b5a85ebf290b49d07d70957a3167a1315f4da65e648f5217ea851',
}


class ParentMapChangeError(ValueError):
    """Rejected numeric observation; does not establish resource relevance."""

    def __init__(self, evidence):
        super().__init__('initial parent identity changed before selection')
        self.evidence = deepcopy(evidence)

    def __reduce__(self):
        return type(self), (self.evidence,)


def _parent_change_evidence(initial, final, rows):
    observed = {row['pid'] for row in rows}
    triggers = sorted((set(final) - set(initial)) |
                      {row['pid'] for row in rows if final.get(row['pid']) != row['parent_pid']})
    changes = []
    for pid in sorted(set(initial) | set(final)):
        before, after = initial.get(pid), final.get(pid)
        if before == after:
            continue
        kind = 'added' if pid not in initial else 'disappeared' if pid not in final else 'parent_changed'
        changes.append(dict(pid=pid, change=kind,
                            initial_parent_pid=before, final_parent_pid=after))
    observation = dict(schema='parent-map-refusal-1', status='REFUSED',
        resource_relevance='NOT_EVALUATED',
        initial_parent_table=[[pid, initial[pid]] for pid in sorted(initial)],
        final_parent_table=[[pid, final[pid]] for pid in sorted(final)],
        initial_observed_pids=sorted(observed),
        unobserved_initial_pids=sorted(set(initial) - observed),
        changes=changes, trigger_pids=triggers,
        limitation='Numeric parent observations only; creation identity and relevance are not established.')
    return dict(rejected_parent_observation=observation,
                events=[dict(change='parent_table_changed', disposition='REFUSED_PARENT_TABLE_CHANGED',
                             trigger_pids=triggers)])


def _backend_reader():
    if (os.name != 'nt' or not psutil.WINDOWS or backend is None
            or psutil.__version__ != PSUTIL_VERSION):
        raise ValueError('unsupported Windows psutil dependency')
    try:
        for module,name in ((psutil,'__init__.py'),(backend,'_pswindows.py'),
                            (_common,'_common.py'),(_compat,'_compat.py'),
                            (backend.cext,'_psutil_windows.pyd')):
            path = Path(module.__file__)
            if (path.name != name or path.is_symlink() or not path.is_file()
                    or hashlib.sha256(path.read_bytes()).hexdigest() != SOURCE_DIGESTS[name]):
                raise ValueError('installed psutil source identity differs')
        reader = backend.ppid_map
    except (OSError,AttributeError,TypeError) as exc:
        raise ValueError('installed psutil source or backend unavailable') from exc
    if not callable(reader):
        raise ValueError('Windows parent-table capability unavailable')
    return reader


def read_windows_parent_map():
    """Read one copied bounded table; no cross-phase cache or fallback."""
    reader = _backend_reader()
    try:
        result = reader()
    except Exception as exc:
        raise ValueError('Windows parent-table read failed') from exc
    if not isinstance(result,dict) or not 1 <= len(result) <= 65536:
        raise ValueError('bounded nonempty parent table required')
    if any(type(value) is not int or not 0 <= value < 2**32
           for pair in result.items() for value in pair):
        raise ValueError('parent table contains invalid process identity')
    return dict(result)


def parent_of(parent_map, pid):
    try:
        return parent_map[pid]
    except KeyError as exc:
        raise psutil.NoSuchProcess(pid) from exc


def _initial_identity(process, parent_map):
    try:
        name = process.name()
        created = str(process.create_time())
        parent = parent_of(parent_map,process.pid)
    except (psutil.Error,OSError) as exc:
        raise ValueError('initial process identity incomplete') from exc
    try:
        executable = process.exe()
    except (psutil.Error,OSError):
        executable = None
    return dict(pid=process.pid,parent_pid=parent,name=name,
                creation_time=created,executable_path=executable)


def enumerate_windows_v2():
    """Use one parent table as the initial population; no cached iterator objects."""
    parent_map = read_windows_parent_map()
    rows = []
    for pid in sorted(parent_map):
        try:
            process = psutil.Process(pid)
        except psutil.NoSuchProcess:
            continue  # Matches pre-yield disappearance; pinned absence refuses selection.
        except (psutil.Error,OSError) as exc:
            raise ValueError('initial process construction unavailable') from exc
        rows.append(_initial_identity(process,parent_map))
    final_map = read_windows_parent_map()
    if (set(final_map) - set(parent_map)
            or any(final_map.get(row['pid']) != row['parent_pid'] for row in rows)):
        evidence = _parent_change_evidence(parent_map, final_map, rows)
        try:
            evidence['rejected_parent_identity'] = capture_parent_identities(
                parent_map, final_map, rows,
                evidence['rejected_parent_observation']['trigger_pids'])
        except Exception as error:
            evidence['rejected_parent_identity'] = dict(schema='parent-change-identities-error-1',
                status='CAPTURE_FAILED', error_type=type(error).__name__[:64],
                resource_relevance='NOT_EVALUATED', table_identity_binding='NOT_ESTABLISHED')
        raise ParentMapChangeError(evidence)
    return rows
