"""Cached pressure-launch checks and bounded retained-file accounting.

These checks establish neither licence reservation nor engineering acceptance.
The caller supplies the fully verified preflight and a mandatory current-byte
binding callback; no native processes or renewed observations are created here.
"""
import os
from copy import deepcopy
import re
import stat
from fractions import Fraction
from pathlib import Path

from .cylinder_diagnostic_preflight import _now
from .cylinder_diagnostic_resources import validate_capacity

ALLOWANCE_BYTES = 1024 ** 3
RESERVE_BYTES = 2 * 1024 ** 3
ORIGINAL_BYTES = 10174192


def _timestamp(value):
    if not isinstance(value, str) or not re.fullmatch(r'\d{1,20}(?:\.\d{1,20})?', value):
        raise ValueError('bounded decimal timestamp required')
    return Fraction(value)


def validate_observation_ages(evidence, ready_at, now, maximum_age):
    """Check four unchanged observation times with exact rational arithmetic."""
    if type(maximum_age) is not int or maximum_age not in (20, 30):
        raise ValueError('only the fixed 20/30 second age limits are supported')
    try:
        times = {
            'capacity': evidence['capacity_observation']['samples'][-1]['observed_at'],
            'process': evidence['process_snapshot']['observed_at'],
            'license': evidence['license_observation']['observed_at'],
            'ready': ready_at,
        }
    except (KeyError, IndexError, TypeError) as exc:
        raise ValueError('four retained observation timestamps required') from exc
    current = _timestamp(now)
    ages = {}
    for label, value in times.items():
        age = current - _timestamp(value)
        if not 0 <= age <= maximum_age:
            raise ValueError(f'{label} observation is future or stale')
        ages[label] = {'numerator': age.numerator, 'denominator': age.denominator}
    return {'status': 'PASS', 'age_seconds_rational': ages,
            'maximum_age_seconds': maximum_age}


def production_phase2(preflight, verify_current, approval, *, stage='phase2'):
    """Recheck existing evidence after claim without Git or licence queries."""
    if stage not in ('phase2', 'preclaim-probe'):
        raise ValueError('unsupported pressure resource-check stage')
    if not callable(verify_current) or verify_current(approval) is not True:
        raise ValueError('current source/config binding did not pass')
    preflight._bindings()
    preflight._reservation()
    if getattr(preflight, '_absence_mode', False):
        preflight.refresh_absence(stage)
    evidence = preflight.last_evidence
    if evidence.get('classification', {}).get('status') != 'CLEAR':
        raise ValueError('retained process classification is not clear')
    now = _now()
    ages = validate_observation_ages(evidence, preflight._ready_at, now, 30)
    capacity = validate_capacity(evidence['capacity_observation'], now=now)
    result = {'status': 'PASS', 'ages': ages, 'capacity': capacity}
    if getattr(preflight, '_absence_mode', False):
        result['process_absence_checks'] = deepcopy(evidence['process_absence_checks'])
    return result


def _checked_path(value, directory):
    path = Path(os.path.abspath(value))
    try:
        for part in [*reversed(path.parents), path]:
            info = part.lstat()
            if stat.S_ISLNK(info.st_mode) or getattr(info, 'st_file_attributes', 0) & 0x400:
                raise ValueError('redirected storage path')
        info = path.stat()
        expected = stat.S_ISDIR if directory else stat.S_ISREG
        if not expected(info.st_mode):
            raise ValueError('unexpected storage entry type')
    except (OSError, TypeError) as exc:
        raise ValueError('storage path unavailable') from exc
    return path


def _declarations(original_root, parent_claim, output_root, ledger_paths, reservation_path,
                  prior_capture_roots=(), supplemental_files=()):
    if not isinstance(ledger_paths, (list, tuple)):
        raise ValueError('explicit ledger path list required')
    if not isinstance(prior_capture_roots, (list, tuple)):
        raise ValueError('explicit prior capture root list required')
    if not isinstance(supplemental_files, (list, tuple)):
        raise ValueError('explicit supplemental capture file list required')
    rows = [('original', original_root, True), ('claim', parent_claim, False),
            ('output', output_root, True), ('reservation', reservation_path, False)]
    rows += [(f'prior-{index}', path, True)
             for index, path in enumerate(prior_capture_roots)]
    rows += [(f'ledger-{index}', path, False) for index, path in enumerate(ledger_paths)]
    rows += [(f'supplemental-{index}', path, False) for index, path in enumerate(supplemental_files)]
    checked = [(label, _checked_path(path, directory), directory)
               for label, path, directory in rows]
    for index, (_, path, directory) in enumerate(checked):
        identity = str(path).casefold()
        for _, other, other_directory in checked[index + 1:]:
            second = str(other).casefold()
            if (identity == second or directory and second.startswith(identity + os.sep)
                    or other_directory and identity.startswith(second + os.sep)):
                raise ValueError('overlapping storage declarations')
    return checked


def _tree_files(path):
    pending = [path]
    while pending:
        directory = pending.pop()
        try:
            entries = sorted(directory.iterdir())
            for child in entries:
                info = child.lstat()
                if stat.S_ISDIR(info.st_mode):
                    pending.append(_checked_path(child, True))
                else:
                    yield _checked_path(child, False)
        except OSError as exc:
            raise ValueError('storage enumeration unavailable') from exc


def _account(declarations):
    identities, files = set(), []
    for label, path, directory in declarations:
        for item in _tree_files(path) if directory else [path]:
            info = item.stat()
            identity = (info.st_dev, info.st_ino)
            if not info.st_ino or identity in identities:
                raise ValueError('duplicate or unestablished file identity')
            identities.add(identity)
            relative = item.relative_to(path).as_posix() if directory else item.name
            files.append({'path': f'{label}/{relative}', 'bytes': info.st_size})
    return files


def cumulative_storage(original_root, parent_claim, output_root, ledger_paths,
                       reservation_path, free_bytes, *, prior_capture_roots=(), supplemental_files=()):
    """Count explicitly owned files; metadata integrity is verified separately.

    FAIL retains all files and reports a budget violation. Missing declarations,
    redirects, duplicate identities and original-size drift refuse with ValueError.
    Prior roots include retained coarse mesh (N4) evidence for intermediate mesh
    (N8) execution. Callers bind their identities and supply all owned ledger files.
    This is a point-in-time accounting check, not a filesystem quota.
    """
    if type(free_bytes) is not int or free_bytes < 0:
        raise ValueError('nonnegative integer free bytes required')
    declarations = _declarations(original_root, parent_claim, output_root,
                                 ledger_paths, reservation_path, prior_capture_roots, supplemental_files)
    files = _account(declarations)
    original = sum(row['bytes'] for row in files if row['path'].startswith('original/'))
    if original != ORIGINAL_BYTES:
        raise ValueError('original capture byte count changed')
    total = sum(row['bytes'] for row in files)
    remaining = ALLOWANCE_BYTES - total
    required = max(remaining, 0) + RESERVE_BYTES
    return {'status': 'PASS' if remaining >= 0 and free_bytes >= required else 'FAIL',
            'total_bytes': total, 'original_bytes': original,
            'remaining_allowance_bytes': remaining, 'required_free_bytes': required,
            'reserve_bytes': RESERVE_BYTES, 'files': files}
