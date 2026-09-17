"""Create an execution-only source binding without regenerating frozen evidence.

Preparation invokes read-only Git metadata commands, never a checker or solver.
Original matrix inputs remain unchanged. Runtime approval is a separate gate.
"""
import os
from pathlib import Path, PurePosixPath
import re
import subprocess

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from digitalmodel.ansys.cylinder_canary import ORDER, runtime_sources
from digitalmodel.ansys.cylinder_reference_provenance import CAPTURE_FILES

ORIGINAL = 'examples/ansys/cylinder-benchmark'
SUCCESSORS = 'examples/ansys/cylinder-runtime'
RESERVED = {'con', 'prn', 'aux', 'nul', *(f'com{i}' for i in range(1, 10)),
            *(f'lpt{i}' for i in range(1, 10))}


def _owning_root():
    root = Path(__file__).resolve().parents[3]
    if not (root / '.git').exists():
        raise ValueError('Loaded source requires its owning Git checkout')
    return root


def _component(value):
    return (isinstance(value, str) and re.fullmatch(r'[A-Za-z0-9_-][A-Za-z0-9_.-]{0,119}', value)
            and not value.endswith('.') and value.split('.')[0].lower() not in RESERVED)


def _owned(root, name):
    if root.is_symlink() or root.is_junction() or root.resolve() != root.absolute():
        raise ValueError('Redirected artifact root')
    if not isinstance(name, str) or '\\' in name:
        raise ValueError('Invalid relative artifact path')
    parts = PurePosixPath(name)
    if parts.is_absolute() or str(parts) != name or not all(_component(p) for p in parts.parts):
        raise ValueError('Invalid relative artifact path')
    target = root
    for part in parts.parts:
        target = target / part
        if target.is_symlink() or target.is_junction():
            raise ValueError('Redirected artifact path')
    if not target.resolve().is_relative_to(root.resolve()):
        raise ValueError('Artifact escapes owning root')
    return target


def _pin(value, length):
    if not isinstance(value, str) or not re.fullmatch('[0-9a-f]{' + str(length) + '}', value):
        raise ValueError('Invalid immutable digest or revision pin')


def _git_blob(root, revision, name):
    kind = subprocess.run(['git', '-C', str(root), 'cat-file', '-t', revision],
                          capture_output=True, check=False)
    if kind.returncode or kind.stdout.strip() != b'commit':
        raise ValueError('Source revision must identify a Git commit')
    result = subprocess.run(['git', '-C', str(root), 'show', revision + ':' + name],
                            capture_output=True, check=False)
    if result.returncode:
        raise ValueError('Pinned Git source is unavailable')
    return result.stdout


def _runtime(root, inventory_pin, revision):
    inventory = runtime_sources()
    if digest_bytes(canonical_bytes(inventory)) != inventory_pin:
        raise ValueError('Runtime inventory differs from reviewed pin')
    seen = set()
    for row in inventory:
        name = row['path']
        if name in seen:
            raise ValueError('Duplicate runtime source')
        seen.add(name)
        raw = _owned(root, name).read_bytes()
        if digest_bytes(raw) != row['sha256'] or raw != _git_blob(root, revision, name):
            raise ValueError('Working runtime differs from pinned Git source')
    if not seen:
        raise ValueError('Missing runtime source inventory')
    return inventory


def _required(manifest):
    if manifest.get('schema') != 'cylinder-b1-1' or manifest.get('case_order') != list(ORDER):
        raise ValueError('Original benchmark schema/order differs')
    if [c['case_id'] for c in manifest['cases']] != list(ORDER):
        raise ValueError('Original benchmark cases differ')
    if manifest.get('reference') != 'reference.json' or 'runtime_lineage' in manifest:
        raise ValueError('Source must be the original reference bundle')
    required = {*CAPTURE_FILES, 'reference_comparison.json', 'reference.json',
                'prepared/basis-criteria.json'}
    for case in manifest['cases']:
        if case['deck'] != f"prepared/{case['case_id']}.inp" or case['metadata'] != f"prepared/{case['case_id']}.json":
            raise ValueError('Original benchmark case locator differs')
        required.update((case['deck'], case['metadata']))
    return required


def _original(root, manifest_pin):
    source = _owned(root, ORIGINAL)
    raw = _owned(root, ORIGINAL + '/manifest.json').read_bytes()
    if digest_bytes(raw) != manifest_pin:
        raise ValueError('Original manifest differs from immutable pin')
    manifest = parse_json(raw)
    required = _required(manifest)
    entries = manifest['artifacts']
    if not isinstance(entries, list) or len(entries) != 21:
        raise ValueError('Original bundle requires exactly 21 artifacts')
    snapshots = {}
    for entry in entries:
        name = entry['path']
        if name in snapshots:
            raise ValueError('Duplicate original artifact')
        path = _owned(source, name)
        data = path.read_bytes()
        if (type(entry['bytes']) is not int or entry['bytes'] != len(data)
                or digest_bytes(data) != entry['sha256']):
            raise ValueError('Original artifact digest or byte count differs')
        snapshots[name] = data
    if not required.issubset(snapshots):
        raise ValueError('Missing mandatory provenance siblings')
    return raw, manifest, snapshots


def _write(path, raw):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('xb') as stream:
        stream.write(raw)
        stream.flush()
        os.fsync(stream.fileno())
    if path.read_bytes() != raw:
        raise OSError('Copied artifact readback differs')


def _failure(output, completed, error):
    record = {'status': 'partial_runtime_preparation', 'completed_files': completed,
              'exception_type': type(error).__name__, 'reason': str(error),
              'native_execution': False, 'checker_invocation': False,
              'disposition': 'Preserve partial directory; label consumed; no automatic retry.'}
    try:
        _write(_owned(output, 'preparation-failure.json'), canonical_bytes(record))
    except (OSError, ValueError):
        pass  # Preserve the original exception and any partial bytes.


def _publish_manifest(output, pending):
    final = _owned(output, 'manifest.json')
    # Link creation refuses an existing destination on Windows and POSIX alike.
    os.link(pending, final)
    try:
        pending.unlink()
    except OSError:
        return True  # Published bytes are complete; retain the temporary alias.
    return False


def _copy(root, output, pins, inventory, original):
    raw, manifest, snapshots = original
    completed = []
    try:
        for name, data in snapshots.items():
            _write(_owned(output, name), data)
            completed.append({'path': name, 'sha256': digest_bytes(data)})
        if _original(root, pins['original_manifest_sha256']) != original:
            raise ValueError('Original evidence changed during copying')
        if _runtime(root, pins['runtime_inventory_sha256'], pins['source_revision']) != inventory:
            raise ValueError('Runtime changed during copying')
        successor = {**manifest, 'runtime_sources': inventory, 'runtime_lineage': pins}
        encoded = canonical_bytes(successor)
        pending = output / 'manifest.pending.json'
        _write(pending, encoded)
        for name, data in snapshots.items():
            if _owned(output, name).read_bytes() != data:
                raise ValueError('Copied artifact changed before publication')
        pending_retained = _publish_manifest(output, pending)
    except (OSError, ValueError, KeyError, TypeError) as error:
        _failure(output, completed, error)
        raise
    return {'status': 'prepared_not_executed', 'bundle_path': str(output.relative_to(root)).replace('\\', '/'),
            'manifest_sha256': digest_bytes(encoded), 'runtime_lineage': pins,
            'native_execution': False, 'checker_invocation': False,
            'pending_manifest_alias_retained': pending_retained}


def prepare_runtime_bundle(revision_label, *, original_manifest_sha256,
                           runtime_inventory_sha256, source_revision):
    """Create one immutable execution bundle; all three external pins are required.

    Failed created directories remain consumed. No source, matrix, checker or deck
    is regenerated. Exact code/artifact review and B2 execution gates stay external.
    """
    if (not isinstance(revision_label, str)
            or not re.fullmatch(r'[a-z0-9][a-z0-9_-]{0,63}', revision_label)
            or revision_label in RESERVED):
        raise ValueError('Revision label requires one safe path component')
    _pin(original_manifest_sha256, 64)
    _pin(runtime_inventory_sha256, 64)
    _pin(source_revision, 40)
    root = _owning_root()
    if root.is_symlink() or root.is_junction():
        raise ValueError('Redirected owning root')
    original = _original(root, original_manifest_sha256)
    inventory = _runtime(root, runtime_inventory_sha256, source_revision)
    output = _owned(root, SUCCESSORS + '/' + revision_label)
    output.mkdir(parents=True, exist_ok=False)
    pins = {'original_manifest_sha256': original_manifest_sha256,
            'runtime_inventory_sha256': runtime_inventory_sha256, 'source_revision': source_revision}
    return _copy(root, output, pins, inventory, original)
