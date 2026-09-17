"""Cooperative revision publication and canonical matrix refresh; no solves."""
import copy
from datetime import datetime, timezone
import os
from pathlib import Path
import socket
import tempfile

from digitalmodel.ansys.analysis_evidence import (
    load_package, publish_package, response_csv, validate_package,
)
from digitalmodel.ansys.analysis_records import canonical_bytes, safe_id, verify_reference


def _evidence(package, resolver):
    references = [package['criteria_reference'], package['intake_reference']]
    references.extend(package.get('review_sources', []))
    if 'verification_reference' in package:
        references.append(package['verification_reference'])
    for case in package['cases']:
        references.extend(case['evidence'])
    for reference in references:
        verify_reference(reference, resolver)


def _relation(package, baseline, resolver=None, *, review_sha256=None):
    validate_package(baseline)
    validate_package(package)
    if package['dataset_id'] != baseline['dataset_id']:
        raise ValueError('dataset owner identity differs')
    if package['previous_package_hash'] != baseline['package_hash']:
        raise ValueError('previous package identity differs')
    if package['revision'] == baseline['revision']:
        raise ValueError('new explicit revision required')
    count = len(baseline['cases'])
    if len(package['cases']) != count or package['expected_cases'] != baseline['expected_cases']:
        raise ValueError('fixed matrix membership differs')
    if ('diagnostic_supplement' in package
            and package['diagnostic_supplement'] != baseline.get('diagnostic_supplement')):
        from digitalmodel.ansys.analysis_recovery_note import validate_recovery_note_transition
        validate_recovery_note_transition(package, baseline, resolver)
        return
    changed = [i for i in range(count) if package['cases'][i] != baseline['cases'][i]]
    if not changed:
        revision_fields = {'revision', 'previous_package_hash', 'package_hash'}
        before = {k: v for k, v in baseline.items() if k not in revision_fields}
        after = {k: v for k, v in package.items() if k not in revision_fields}
        if canonical_bytes(before) != canonical_bytes(after):
            raise ValueError('unchanged cases require identical study metadata')
        return
    if changed == [9] and package['cases'][9].get('capture_role') == 'diagnostic_capture':
        from digitalmodel.ansys.analysis_pressure_observed import validate_pressure_observed_transition
        validate_pressure_observed_transition(package, baseline, resolver)
        return
    if package['cases'][:count] != baseline['cases']:
        if any(c.get('capture_role') == 'diagnostic_replay' for c in package['cases']):
            from digitalmodel.ansys.analysis_replay import validate_replay_transition
            validate_replay_transition(package, baseline, resolver, review_sha256=review_sha256)
            return
        if not any(c.get('capture_role') == 'diagnostic_observation' for c in package['cases']):
            raise ValueError('historical case payload changed')
        from digitalmodel.ansys.analysis_observed import validate_observed_transition
        validate_observed_transition(package, baseline, resolver)


def _plain(path):
    for entry in [path, *path.parents]:
        if entry.is_symlink() or getattr(entry, 'is_junction', lambda: False)():
            raise ValueError('redirected publication path')


def _immutable(package, root):
    target = root/package['dataset_id']/(package['revision'] + '.json')
    if target.exists():
        if load_package(target) != package:
            raise ValueError('existing immutable revision differs')
        return target
    return publish_package(package, root)


def _replace(target, raw):
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(dir=target.parent, suffix='.pending', delete=False) as stream:
            temporary = Path(stream.name)
            stream.write(raw)
            stream.flush()
            os.fsync(stream.fileno())
        if temporary.read_bytes() != raw:
            raise OSError('temporary readback mismatch')
        os.replace(temporary, target)
        if target.read_bytes() != raw:
            raise OSError('canonical readback mismatch')
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


def _refresh(package, baseline, manifest, root, resolver, *, review_sha256=None):
    current = load_package(manifest)
    if current not in (baseline, package):
        raise ValueError('canonical baseline changed')
    _evidence(baseline, resolver)
    _evidence(package, resolver)
    _relation(package, baseline, resolver, review_sha256=review_sha256)
    _immutable(baseline, root)
    revision = _immutable(package, root)
    if load_package(revision) != package:
        raise OSError('revision readback mismatch')
    if load_package(manifest) != current:
        raise ValueError('canonical baseline changed before replacement')
    _relation(package, baseline, resolver, review_sha256=review_sha256)
    _replace(manifest, canonical_bytes(package))
    _replace(manifest.with_name('responses.csv'), response_csv(package).encode('utf-8'))
    return revision


def publish_matrix(package, baseline, manifest_path, owner_root, resolver, *, review_sha256=None):
    """Publish or resume one explicit revision; CSV is a non-atomic derived view.

    The outer lock serializes participating integration writers only. Source
    evidence must remain quiescent during publication. Nonparticipating writers
    are not excluded by filesystem locks. No stale lock is removed automatically.
    """
    package, baseline = copy.deepcopy(package), copy.deepcopy(baseline)
    _relation(package, baseline, resolver, review_sha256=review_sha256)
    root, manifest = Path(owner_root).absolute(), Path(manifest_path).absolute()
    _plain(root)
    _plain(manifest)
    if not manifest.is_file():
        raise ValueError('existing canonical manifest required')
    directory = root/safe_id(package['dataset_id'])
    _plain(directory)
    previous = directory/(safe_id(baseline['revision']) + '.json')
    if not previous.is_file() or load_package(previous) != baseline:
        raise ValueError('existing owner baseline revision required')
    lock = directory/'.integration.lock'
    owner = canonical_bytes({'host': socket.gethostname(), 'pid': os.getpid(),
        'started_utc': datetime.now(timezone.utc).isoformat(),
        'baseline_package_hash': baseline['package_hash']})
    descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    try:
        with os.fdopen(descriptor, 'wb') as stream:
            stream.write(owner)
            stream.flush()
            os.fsync(stream.fileno())
        return _refresh(package, baseline, manifest, root, resolver, review_sha256=review_sha256)
    finally:
        try:
            if lock.read_bytes() == owner:
                lock.unlink()
        except OSError:
            pass  # Preserve the primary failure; retain uncertain lock ownership.
