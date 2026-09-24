"""Externally pinned, single-claim diagnostic admission; no engineering authority.

The caller supplies trusted config/review pins. A differently repinned campaign
is outside this guarantee. The production driver binds its actual entrypoint
and performs live preflight; this module never launches or claims a solver.
"""
import json
from pathlib import Path, PurePosixPath
import re

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys import cylinder_canary

ENVIRONMENT = {'ANSYS261_PRODUCT': 'ansys', 'ANS_CONSEC': 'YES'}
ENTRYPOINT = 'scripts/ansys/run_zero_control_diagnostic.py'


def _path(value, directory=False):
    path = Path(value)
    if not path.is_absolute() or path.resolve() != path.absolute():
        raise ValueError('bound path is relative or redirected')
    if any(p.is_symlink() or p.is_junction() for p in (path, *path.parents)):
        raise ValueError('bound path is redirected')
    if not (path.is_dir() if directory else path.is_file()):
        raise ValueError('bound path is unavailable')
    return path


def _sha(value):
    if not isinstance(value, str) or not re.fullmatch('[0-9a-f]{64}', value):
        raise ValueError('invalid SHA256 pin')
    return value


def _text(value):
    if not isinstance(value, str) or not value.strip() or len(value) > 512:
        raise ValueError('missing or invalid identity string')
    return value


def _json(raw):
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError('duplicate JSON field')
            result[key] = value
        return result
    value = json.loads(raw, object_pairs_hook=unique)
    if not isinstance(value, dict):
        raise ValueError('JSON object required')
    return value


def _read(path, expected=None):
    raw = _path(path).read_bytes()
    if expected is not None and digest_bytes(raw) != _sha(expected):
        raise ValueError('bound file digest differs')
    return raw


def _relative(root, name):
    if not isinstance(name, str) or '\\' in name:
        raise ValueError('invalid source locator')
    parts = PurePosixPath(name)
    if parts.is_absolute() or not parts.parts or '..' in parts.parts or str(parts) != name:
        raise ValueError('invalid source locator')
    target = _path(root / name)
    if not target.is_relative_to(root):
        raise ValueError('source locator escapes owner')
    return target


def required_source_inventory(source_root):
    """Derive runtime ownership internally; not a caller-supplied inventory seam."""
    root = _path(source_root, directory=True)
    if root != Path(__file__).resolve().parents[3]:
        raise ValueError('source root differs from loaded package owner')
    inventory = cylinder_canary.runtime_sources()
    required = {item['path']: item['sha256'] for item in inventory}
    directory = Path(__file__).resolve().parent
    paths = [*directory.glob('*.py'), directory.parent / '__init__.py', root / ENTRYPOINT]
    for path in paths:
        required[path.relative_to(root).as_posix()] = digest_bytes(_read(path))
    return [dict(path=name, sha256=required[name]) for name in sorted(required)]


def _source_files(config, root):
    sources = config['source_files']
    if not isinstance(sources, list) or not sources:
        raise ValueError('complete source inventory required')
    observed = {}
    for item in sources:
        if not isinstance(item, dict) or set(item) != {'path', 'sha256'}:
            raise ValueError('invalid source record')
        path = _relative(root, item['path'])
        if item['path'] in observed:
            raise ValueError('duplicate source record')
        _read(path, item['sha256'])
        observed[item['path']] = item['sha256']
    for item in required_source_inventory(root):
        if observed.get(item['path']) != item['sha256']:
            raise ValueError('required runtime source missing or changed')
    return {_relative(root, name): sha for name, sha in observed.items()}


def _execution(binding):
    keys = {'manifest_sha256', 'executable_sha256', 'execution_host', 'profile',
            'runtime_profile', 'capture_allowance_bytes', 'reserve_bytes', 'launch_environment'}
    if not isinstance(binding, dict) or set(binding) != keys:
        raise ValueError('exact execution binding required')
    for key in ('manifest_sha256', 'executable_sha256'):
        _sha(binding[key])
    _text(binding['execution_host'])
    if canonical_bytes(binding['profile']) != canonical_bytes(cylinder_canary.PROFILE):
        raise ValueError('unapproved typed profile')
    if canonical_bytes(binding['launch_environment']) != canonical_bytes(ENVIRONMENT):
        raise ValueError('unapproved launch environment')
    for key in ('capture_allowance_bytes', 'reserve_bytes'):
        if type(binding[key]) is not int or binding[key] <= 0:
            raise ValueError('positive integer storage allowance required')
    profile = binding['runtime_profile']
    if not isinstance(profile, dict) or set(profile) != {'release', 'build', 'update', 'platform'}:
        raise ValueError('exact runtime profile required')
    for value in profile.values():
        _text(value)


def _config(config):
    if config['schema'] != 'cylinder-zero-diagnostic-admission-1':
        raise ValueError('unknown admission schema')
    expected_scope = dict(case_ids=['ocv-zero-t60-n16'], max_attempts=1,
                          qualification='diagnostic_only')
    if canonical_bytes(config['scope']) != canonical_bytes(expected_scope):
        raise ValueError('diagnostic scope differs')
    _text(config['campaign_id'])
    _text(config['operator_id'])
    _execution(config['execution_binding'])
    ledger = _path(config['ledger_directory'], directory=True)
    claim = ledger / (digest_bytes(config['campaign_id'].encode()) + '.json')
    if claim.exists() or claim.is_symlink() or claim.is_junction():
        raise ValueError('diagnostic scope exhausted: durable claim consumed')


def _review_files(bundle, receipt, required):
    if not isinstance(bundle.get('context'), str) or not bundle['context'].strip():
        raise ValueError('review context unavailable')
    files, records = bundle.get('files'), receipt.get('files')
    if not isinstance(files, list) or not files or not isinstance(records, list):
        raise ValueError('reviewed file inventory unavailable')
    actual = {}
    for item in files:
        if not isinstance(item, dict) or set(item) != {'path', 'sha256', 'content'}:
            raise ValueError('invalid reviewed file record')
        path = _path(item['path'])
        if path in actual:
            raise ValueError('duplicate reviewed file')
        raw = _read(path, item['sha256'])
        if raw.decode('utf-8-sig') != item['content']:
            raise ValueError('review content differs from current raw bytes')
        actual[path] = item['sha256']
    recorded = {}
    for item in records:
        if not isinstance(item, dict) or set(item) != {'path', 'sha256'}:
            raise ValueError('invalid receipt file record')
        path = _path(item['path'])
        if path in recorded:
            raise ValueError('duplicate receipt file')
        recorded[path] = _sha(item['sha256'])
    if actual != recorded or any(actual.get(path) != sha for path, sha in required.items()):
        raise ValueError('review file bindings differ or omit required sources/config')


def _review(paths, receipt_sha, required):
    receipt = _json(_read(paths['receipt'], receipt_sha))
    if receipt.get('status') != 'REVIEW_RECEIVED' or type(receipt.get('exit_code')) is not int:
        raise ValueError('valid review receipt required')
    if receipt['exit_code'] != 0:
        raise ValueError('provider execution failed')
    transport = _json(_read(paths['stdout'], receipt['stdout_sha256']))
    if transport.get('is_error') is not False:
        raise ValueError('provider transport failed')
    session = _text(transport.get('session_id'))
    review = receipt.get('review')
    if not isinstance(review, dict) or canonical_bytes(review) != canonical_bytes(transport.get('structured_output')):
        raise ValueError('provider structured output differs')
    if review.get('verdict') not in ('APPROVE', 'MINOR'):
        raise ValueError('blocking or invalid review verdict')
    if not isinstance(review.get('findings'), list):
        raise ValueError('review findings required')
    for finding in review['findings']:
        if not isinstance(finding, str) or not finding.strip():
            raise ValueError('invalid helper review finding')
        if re.search(r'\[(?:MAJOR|CRITICAL|BLOCKER)\b', finding, re.IGNORECASE):
            raise ValueError('review finding contradicts nonblocking verdict')
    bundle_raw = _read(paths['bundle'], receipt['bundle_sha256'])
    if review.get('bundle_sha256') != receipt['bundle_sha256']:
        raise ValueError('review bundle digest differs')
    _review_files(_json(bundle_raw), receipt, required)
    return session


def _validate(paths, root, config_sha, review_sha):
    config = _json(_read(paths['config'], config_sha))
    _config(config)
    required = _source_files(config, _path(root, directory=True))
    required[_path(paths['config'])] = config_sha
    session = _review(paths, review_sha, required)
    if session == config['operator_id']:
        raise ValueError('operator and independent reviewer must differ')
    expected = dict(config['execution_binding'], approval_id=config['campaign_id'],
                    operator_id=config['operator_id'], checker_id=session,
                    ledger_directory=config['ledger_directory'], config_sha256=config_sha,
                    review_receipt_sha256=review_sha)
    return expected


def make_diagnostic_admission(config_path, review_receipt_path, review_stdout_path,
                              review_bundle_path, *, expected_config_sha256,
                              expected_review_sha256, source_root):
    """Bind trusted external pins; recheck every retained byte before admission.

    Scope is consumed by the unchanged runner's durable claim, including any
    subsequent failure. This adapter neither writes that claim nor resets it.
    The driver must bind its running entrypoint before invoking this factory.
    """
    paths = dict(config=Path(config_path), receipt=Path(review_receipt_path),
                 stdout=Path(review_stdout_path), bundle=Path(review_bundle_path))
    root = Path(source_root)
    config_sha, review_sha = _sha(expected_config_sha256), _sha(expected_review_sha256)
    def validated():
        try:
            return _validate(paths, root, config_sha, review_sha)
        except (OSError, KeyError, TypeError, UnicodeError) as error:
            raise ValueError('diagnostic admission evidence unavailable or invalid') from error
    initial = validated()
    def verify_authority(approval):
        expected = validated()
        if canonical_bytes(approval) != canonical_bytes(expected):
            raise ValueError('approval differs from externally pinned execution authority')
        return True
    def adjudicate(*args, **kwargs):
        raise ValueError('diagnostic scope cannot adjudicate engineering qualification')
    return dict(checker_id=initial['checker_id'], verify_authority=verify_authority,
                adjudicate=adjudicate)
