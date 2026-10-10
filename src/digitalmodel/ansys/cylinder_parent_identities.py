"""Bounded supplemental identities after refusal; never process admission evidence."""
from copy import deepcopy
import re
import time
import psutil
from .analysis_records import canonical_bytes

MAX_IDENTITIES = 8
MAX_BYTES = 32768
SELECTION_RULE = 'sorted triggers, then sorted union of initial/final nonzero parents; first-occurrence dedup'


def _validated(row):
    fields = {'pid', 'creation_time', 'name', 'executable_path'}
    if not isinstance(row, dict) or set(row) not in (fields, fields | {'parent_pid'}):
        raise ValueError('Invalid identity fields')
    for key in ('pid', 'parent_pid'):
        if key in row and (type(row[key]) is not int or not 0 <= row[key] < 2**32):
            raise ValueError('Invalid identifier')
    value = row['creation_time']
    if not isinstance(value, str) or not re.fullmatch(r'[0-9]{1,20}(?:\.[0-9]{1,20})?', value):
        raise ValueError('Invalid creation time')
    for key in ('name', 'executable_path'):
        value = row[key]
        if value is not None and (not isinstance(value, str) or len(value) > 4096):
            raise ValueError('Invalid identity text')
    return deepcopy(row)


def _sample(pid):
    process = psutil.Process(pid)
    row = dict(pid=pid, creation_time=str(process.create_time()))
    for key, read in [('name', process.name), ('executable_path', process._proc.exe)]:
        try:
            row[key] = read()
        except psutil.NoSuchProcess:
            raise
        except (psutil.Error, OSError):
            row[key] = None
    return _validated(row)


def _post_table(pid):
    try:
        before, after = _sample(pid), _sample(pid)
        if before != after:
            return dict(status='IDENTITY_CHANGED', before=before, after=after)
        complete = all(after[key] and after[key].strip() for key in ('name', 'executable_path'))
        return dict(status='OBSERVED_AFTER_TABLE' if complete else 'INCOMPLETE_AFTER_TABLE', identity=after)
    except psutil.NoSuchProcess:
        return dict(status='DISAPPEARED')
    except (psutil.Error, OSError):
        return dict(status='UNAVAILABLE')
    except ValueError:
        return dict(status='INVALID_FIELDS')


def _original(pid, initial, rows):
    if pid not in initial:
        return dict(original_row_status='NOT_IN_INITIAL_TABLE')
    if pid not in rows:
        return dict(original_row_status='UNOBSERVED_INITIAL')
    try:
        return dict(original_row_status='RETAINED', initial_identity=_validated(rows[pid]),
                    initial_image_basis='original_collector_psutil_high_level_may_guess')
    except ValueError:
        return dict(original_row_status='INVALID_FIELDS')


def _candidates(initial, final, triggers):
    parents = {table.get(pid) for table in (initial, final) for pid in triggers} - {None, 0}
    return list(dict.fromkeys([*sorted(set(triggers)), *sorted(parents)]))


def capture_parent_identities(initial, final, rows, triggers):
    candidates = _candidates(initial, final, triggers)
    selected = candidates[:MAX_IDENTITIES]
    originals = {row['pid']: row for row in rows if row['pid'] in selected}
    record = dict(schema='parent-change-identities-1', status='SUPPLEMENTAL_ONLY',
        resource_relevance='NOT_EVALUATED', table_identity_binding='NOT_ESTABLISHED',
        selection_rule=SELECTION_RULE, candidate_count=len(candidates), selected_pids=selected,
        omitted_candidate_count=len(candidates)-len(selected), attempted_pids=[], records=[],
        canonical_byte_limit=MAX_BYTES, started_monotonic_ns=time.monotonic_ns(), ended_monotonic_ns=0,
        limitation='Initial rows and later two-object samples are distinct; later identity is not bound to earlier numeric tables.')
    for pid in selected:
        row = dict(pid=pid, **_original(pid, initial, originals))
        if pid in final:
            row['final_table_parent_pid'] = final[pid]
            record['attempted_pids'].append(pid)
            row['post_table'] = _post_table(pid)
        else:
            row['post_table'] = dict(status='NOT_IN_FINAL_TABLE')
        record['records'].append(row)
    record['ended_monotonic_ns'] = time.monotonic_ns()
    return _fit_details(record)


def _fit_details(record):
    details = record['records']
    stubs = [dict(pid=row['pid'], original_row_status='OMITTED_SIZE_LIMIT',
                  post_table=dict(status='OMITTED_SIZE_LIMIT')) for row in details]
    record['records'] = deepcopy(stubs)
    if len(canonical_bytes(record)) > MAX_BYTES:
        raise ValueError('Supplemental metadata exceeds canonical byte limit')
    for index, row in enumerate(details):
        record['records'][index] = row
        if len(canonical_bytes(record)) > MAX_BYTES:
            record['records'][index] = stubs[index]
    return record
