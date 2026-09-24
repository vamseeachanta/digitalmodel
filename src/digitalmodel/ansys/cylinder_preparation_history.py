"""Read-only, omission-resistant accounting for retained intermediate preparations."""
from pathlib import Path
import re

from .analysis_records import canonical_bytes, digest_bytes, parse_json
from .cylinder_pressure_resources import _checked_path, _tree_files
from .cylinder_pressure_scope import pressure_step
from .cylinder_preparation_streams import expected_stream_paths

PREFIX = 'ansys-2121-pressure-intermediate-'
STREAM_PREFIX = 'SOLVERS-intermediate-driver-'
# Exact retained, reviewed pre-binding format from issue 2121; no general legacy exemption.
LEGACY_PREPARATIONS = {
    '6398472c3aca2809878b9f8d6db337a087bdde1c978a13ba6eef84b5e7f7d952':
        '51f7083b9451075cea7d998efe96813866e0356ca0471144ee97b0699dbd6c26',
}


def _entries(directory):
    root = _checked_path(directory, True)
    try:
        entries = list(root.iterdir())
    except OSError as exc:
        raise ValueError('Preparation namespace enumeration unavailable') from exc
    if len(entries) > 65536:
        raise ValueError('Preparation namespace exceeds bounded inventory')
    return entries


def _pin(record):
    if not isinstance(record, dict) or set(record) != {'path', 'sha256', 'bytes'}:
        raise ValueError('Exact preparation file pin required')
    if (type(record['bytes']) is not int or record['bytes'] < 0
            or not isinstance(record['sha256'], str)
            or not re.fullmatch('[0-9a-f]{64}', record['sha256'])):
        raise ValueError('Invalid preparation byte count or digest')
    path = _checked_path(record['path'], False)
    info = path.stat()
    if record['bytes'] > 16*1024**2 or info.st_size > 16*1024**2:
        raise ValueError('Preparation metadata exceeds bounded read size')
    if info.st_nlink != 1 or not info.st_ino:
        raise ValueError('Preparation file identity is shared or unavailable')
    raw = path.read_bytes()
    if len(raw) != record['bytes'] or digest_bytes(raw) != record['sha256']:
        raise ValueError('Retained preparation file bytes changed')
    return path, raw


def _files(records):
    if not isinstance(records, list):
        raise ValueError('Explicit preparation file inventory required')
    result = {}
    for record in records:
        path, raw = _pin(record)
        if path in result:
            raise ValueError('Duplicate preparation file declaration')
        result[path] = raw
    return result


def _refusal(raw):
    result = parse_json(raw)
    expected = dict(case_id='ocv-t60-p10-n8', consumed_count=2, native_launch_count=0,
        launch_adapter_calls=0, terminal_reason='PRECLAIM_REFUSAL', reservation_released=True,
        no_owned_processes_established=True, engineering_qualified=False)
    if not isinstance(result, dict):
        raise ValueError('Preparation refusal object required')
    if canonical_bytes({k: result.get(k) for k in expected}) != canonical_bytes(expected):
        raise ValueError('Exact settled zero-launch preparation refusal required')
    reasons = result.get('terminal_reasons', ['PRECLAIM_REFUSAL'])
    if reasons != ['PRECLAIM_REFUSAL'] or any(k.endswith('_error') for k in result):
        raise ValueError('Preparation contains unresolved secondary terminal failures')
    return result


def _refusal_identity(result, raw, entry, config, root, original):
    expected = dict(config_sha256=entry['config']['sha256'], ordinal=3, output=str(root),
                    streams=expected_stream_paths(original),
                    parent_claim_sha256=config['lineage']['parent_claim']['sha256'])
    if 'preparation_binding' not in result:
        if LEGACY_PREPARATIONS.get(entry['config']['sha256']) != digest_bytes(raw):
            raise ValueError('Unrecognized legacy preparation refusal')
    elif canonical_bytes(result['preparation_binding']) != canonical_bytes(expected):
        raise ValueError('Preparation refusal binding differs')


def _preparation(entry, config, parent, stream_directory):
    if not isinstance(entry, dict) or set(entry) != {'root', 'config', 'files', 'streams'}:
        raise ValueError('Exact abandoned preparation declaration required')
    root = _checked_path(entry['root'], True)
    suffix = root.name.removeprefix(PREFIX)
    if root.parent != parent or not re.fullmatch(r'\d{8}-r[1-9]\d*', suffix):
        raise ValueError('Preparation root outside fixed campaign namespace')
    _, raw = _pin(entry['config'])
    original = parse_json(raw)
    if (pressure_step(original) != (3, 'ocv-t60-p10-n8')
            or Path(original['operational']['output_directory']) != root
            or canonical_bytes(original['lineage']['parent_claim'])
            != canonical_bytes(config['lineage']['parent_claim'])):
        raise ValueError('Preparation configuration identity differs')
    files = _files(entry['files'])
    if set(files) != set(_tree_files(root)):
        raise ValueError('Retained preparation membership differs')
    refusal_path = root/'preparation-refusal.json'
    if refusal_path not in files:
        raise ValueError('Retained preparation refusal missing')
    refusal = _refusal(files[refusal_path])
    _refusal_identity(refusal, files[refusal_path], entry, config, root, original)
    streams = _files(entry['streams'])
    stdout = stream_directory/(STREAM_PREFIX+suffix+'.stdout.json')
    stderr = stream_directory/(STREAM_PREFIX+suffix+'.stderr.txt')
    if set(streams) != {stdout, stderr}:
        raise ValueError('Exact original driver stream pair required')
    if streams[stderr] != b'':
        raise ValueError('Preparation driver standard error is not empty')
    if canonical_bytes(parse_json(streams[stdout])) != canonical_bytes(refusal):
        raise ValueError('Original driver output differs from refusal')
    return root, files, streams


def _namespace(config, output):
    parent = _checked_path(output.parent, True)
    original = Path(config['lineage']['original_root'])
    if not original.is_absolute() or parent != _checked_path(original.parent, True):
        raise ValueError('Output namespace differs from bound original capture namespace')
    observed = {p for p in _entries(parent) if p.name.casefold().startswith(PREFIX) and p != output}
    descriptor = config.get('preparation_history')
    if descriptor is None:
        if observed:
            raise ValueError('Existing preparations require explicit history')
        return parent, observed, None
    if (not isinstance(descriptor, dict)
            or set(descriptor) != {'schema', 'stream_directory', 'preparations'}
            or descriptor['schema'] != 'cylinder-preparation-history-1'
            or not isinstance(descriptor['preparations'], list)):
        raise ValueError('Exact preparation history descriptor required')
    return parent, observed, descriptor


def _ledger(config):
    parent = _checked_path(config['lineage']['parent_claim']['path'], False)
    for suffix in ('.ordinal-3.json', '.ordinal-3.invocation.json', '.ordinal-3.terminal.json'):
        path = parent.with_name(parent.stem+suffix)
        if path.exists() or path.is_symlink() or path.is_junction():
            raise ValueError('Next-attempt ledger already exists')


def _streams(config, output):
    # This Windows benchmark retains streams beside its lineage-verified capture parent.
    workspace = Path(config['lineage']['original_root']).parent.parent
    directory = _checked_path(workspace/'_coordination', True)
    current = (STREAM_PREFIX+output.name.removeprefix(PREFIX)).casefold()
    observed = {p for p in _entries(directory)
        if p.name.casefold().startswith(STREAM_PREFIX.casefold())
        and p.name.casefold().endswith(('.stdout.json', '.stderr.txt'))
        and p.name.casefold() not in (current+'.stdout.json', current+'.stderr.txt')}
    return directory, observed


def validate_preparation_history(config, *, require_unclaimed=True):
    """Bind actual retained membership; metadata/configuration files are not capture replicas."""
    empty = dict(prior_capture_roots=[], supplemental_files=[], retained_bytes=0)
    if type(require_unclaimed) is not bool:
        raise ValueError('Explicit unclaimed-check mode required')
    if pressure_step(config)[0] != 3:
        return empty
    try:
        output = Path(config['operational']['output_directory'])
        if not output.is_absolute():
            raise ValueError('Absolute reviewed output directory required')
        parent, observed, descriptor = _namespace(config, output)
        if require_unclaimed:
            _ledger(config)
        stream_directory, observed_streams = _streams(config, output)
        if descriptor is None:
            if observed_streams:
                raise ValueError('Existing original streams require explicit history')
            return empty
        if _checked_path(descriptor['stream_directory'], True) != stream_directory:
            raise ValueError('Declared stream location differs from bound original workspace')
        roots, files, streams = [], {}, {}
        for entry in descriptor['preparations']:
            root, capture, pair = _preparation(entry, config, parent, stream_directory)
            if root in roots or set(pair) & set(streams):
                raise ValueError('Duplicate preparation or original stream')
            roots.append(root); files.update(capture); streams.update(pair)
        if set(roots) != observed:
            raise ValueError('Preparation namespace membership differs')
        if set(streams) != observed_streams:
            raise ValueError('Original stream namespace membership differs')
        return dict(prior_capture_roots=[str(p) for p in roots],
                    supplemental_files=[str(p) for p in streams],
                    retained_bytes=sum(len(raw) for raw in [*files.values(), *streams.values()]))
    except (KeyError, TypeError, OSError) as exc:
        raise ValueError('Preparation history unavailable or malformed') from exc
