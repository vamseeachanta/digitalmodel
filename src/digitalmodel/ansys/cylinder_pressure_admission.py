"""Offline externally pinned authority for a selected pressure capture; never claims or launches."""
from copy import deepcopy
from pathlib import Path
import re

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys import cylinder_canary
from digitalmodel.ansys import cylinder_diagnostic_admission as shared
from digitalmodel.ansys.cylinder_pressure_lineage import validate_lineage
from digitalmodel.ansys.cylinder_runtime_bundle import _git_blob
from digitalmodel.ansys.cylinder_pressure_scope import COARSE_SCOPE, pressure_step
from digitalmodel.ansys.cylinder_intermediate_lineage import validate_coarse_predecessor
from digitalmodel.ansys.cylinder_preparation_history import validate_preparation_history

ENTRYPOINT = 'scripts/ansys/run_pressure_diagnostic.py'
# Legacy public alias for coarse admission only; intermediate callers use INTERMEDIATE_SCOPE.
SCOPE = COARSE_SCOPE


def required_source_inventory(source_root):
    """Inventory the loaded source owner and fixed pressure entrypoint."""
    root = shared._path(source_root,directory=True)
    module = Path(__file__).resolve()
    if root != module.parents[3]:
        raise ValueError('source root differs from loaded package owner')
    records = {r['path']:r['sha256'] for r in cylinder_canary.runtime_sources()}
    for path in [*module.parent.glob('*.py'),
                 module.parent.parent/'__init__.py',
                 module.parent.parent/'_compat.py', root/ENTRYPOINT]:
        records[path.relative_to(root).as_posix()] = digest_bytes(shared._read(path))
    return [dict(path=name,sha256=records[name]) for name in sorted(records)]


def verify_source_commit(config,source_root):
    """Require every reviewed source's exact working bytes at one immutable commit.

    This read-only subcheck grants no authority. The factory and verify_authority
    both invoke it; orchestration must use their full review/lineage gate.
    """
    root = shared._path(source_root,directory=True)
    revision = config.get('source_revision')
    if not isinstance(revision,str) or not re.fullmatch('[0-9a-f]{40}',revision):
        raise ValueError('source revision requires a full immutable Git commit')
    rows = config['source_files']
    if not isinstance(rows,list) or not rows:
        raise ValueError('complete pressure source inventory required')
    observed = {}
    for row in rows:
        if not isinstance(row,dict) or set(row) != {'path','sha256'} or row['path'] in observed:
            raise ValueError('invalid or duplicate source entry')
        path = shared._relative(root,row['path'])
        raw = shared._read(path,row['sha256'])
        if _git_blob(root,revision,row['path']) != raw:
            raise ValueError('reviewed working source differs from committed Git blob')
        observed[row['path']] = row['sha256']
    for row in required_source_inventory(root):
        if observed.get(row['path']) != row['sha256']:
            raise ValueError('required pressure source inventory missing or changed')
    return {shared._relative(root,k):v for k,v in observed.items()}


def _config(config):
    ordinal, _ = pressure_step(config)
    expected = 'cylinder-pressure-admission-1' if ordinal == 2 else 'cylinder-pressure-admission-2'
    if config['schema'] != expected:
        raise ValueError('pressure admission schema differs from selected scope')
    shared._text(config['campaign_id']); shared._text(config['operator_id'])
    shared._execution(config['execution_binding'])
    binding = config['execution_binding']
    if (binding['capture_allowance_bytes'] != 1073741824 or binding['reserve_bytes'] != 2147483648
            or binding['runtime_profile'] != dict(release='2026 R1.01',build='26.1',
                update='20260202',platform='WINDOWS x64')):
        raise ValueError('fixed pressure execution profile/storage differs')


def _blocking_severity(finding):
    """Recognize labels, not ordinary prose; unknown explicit labels refuse."""
    blocking = r'(?:MAJOR|CRITICAL|BLOCKER|BLOCKING)'
    prefix = r'^\s*(?:[-*]\s*)?(?:F\d+\s*[:.)-]?\s*)?'
    patterns = [prefix + blocking + r'\b',
                r'\[\s*' + blocking + r'\s*\]',
                r'\b' + blocking + r'\s*:']
    if any(re.search(pattern,finding,re.IGNORECASE) for pattern in patterns):
        return True
    # Unknown labels must occupy a label position, not a prose [API] reference.
    labels = []
    for pattern in [prefix + r'\[\s*([A-Za-z][A-Za-z_-]*)\s*\]',
                    prefix + r'([A-Za-z][A-Za-z_-]*)\s*[:|\-]',
                    r'^\s*(?:[-*]\s*)?F\d+\s*[:.)-]?\s+([A-Za-z][A-Za-z_-]*)',
                    r'(?i)\bseverity\s*:\s*([A-Za-z][A-Za-z_-]*)']:
        match = re.search(pattern,finding)
        if match:
            labels.append(match.group(1))
    allowed = {'MINOR','INFO','INFORMATIONAL','NOTE','NONBLOCKING'}
    return not labels or any(label.upper() not in allowed for label in labels)


def _review(paths,sha,required):
    session = shared._review(paths,sha,required)
    receipt = shared._json(shared._read(paths['receipt'],sha))
    for finding in receipt['review']['findings']:
        if _blocking_severity(finding):
            raise ValueError('blocking finding contradicts nonblocking verdict')
    return session


def _validate(paths,root,config_sha,review_sha):
    config = shared._json(shared._read(paths['config'],config_sha))
    _config(config)
    required = verify_source_commit(config,root)
    required[shared._path(paths['config'])] = config_sha
    session = _review(paths,review_sha,required)
    if session == config['operator_id']:
        raise ValueError('operator and independent reviewer must differ')
    if pressure_step(config)[0] == 3:
        validate_coarse_predecessor(config)
        validate_preparation_history(config)
    successor = validate_lineage(config)
    inventory = cylinder_canary.runtime_sources()
    if canonical_bytes(successor.get('runtime_sources')) != canonical_bytes(inventory):
        raise ValueError('successor runtime inventory differs from loaded sources')
    lineage = successor.get('runtime_lineage', {})
    if (lineage.get('original_manifest_sha256') != config['lineage']['base_manifest']['sha256']
            or lineage.get('runtime_inventory_sha256') != digest_bytes(canonical_bytes(inventory))):
        raise ValueError('successor runtime lineage differs')
    approval = dict(deepcopy(config['execution_binding']),approval_id=config['campaign_id'],
        operator_id=config['operator_id'],checker_id=session,ledger_directory=config['ledger_directory'],
        config_sha256=config_sha,review_receipt_sha256=review_sha,
        source_revision=config['source_revision'],
        scope=deepcopy(config['scope']),lineage=deepcopy(config['lineage']))
    if pressure_step(config)[0] == 3:
        approval['predecessor'] = deepcopy(config['predecessor'])
    return approval


def make_pressure_admission(config_path,review_receipt_path,review_stdout_path,review_bundle_path,
                            *,expected_config_sha256,expected_review_sha256,source_root):
    """Return checker, approval and a read-only every-call authority verifier.

    The caller owns ordinal/deadline claims and live resource observations. Existing
    parent claim presence is required evidence, not permission to retry that claim.
    Factory validation rejects blocking review text before lineage validation. The
    caller must obtain this result and call verify_authority(approval) immediately
    before prefix replay; neither this factory nor its subchecks execute replay.
    """
    paths = dict(config=Path(config_path),receipt=Path(review_receipt_path),
                 stdout=Path(review_stdout_path),bundle=Path(review_bundle_path))
    config_sha,review_sha = shared._sha(expected_config_sha256),shared._sha(expected_review_sha256)
    def validated():
        try:
            return _validate(paths,Path(source_root),config_sha,review_sha)
        except (OSError,KeyError,TypeError,UnicodeError,IndexError) as exc:
            raise ValueError('pressure admission evidence unavailable or invalid') from exc
    initial = validated()
    def verify_authority(approval):
        expected = validated()
        if canonical_bytes(approval) != canonical_bytes(expected):
            raise ValueError('approval differs from externally pinned pressure authority')
        return True
    return dict(checker_id=initial['checker_id'],approval=deepcopy(initial),verify_authority=verify_authority)
