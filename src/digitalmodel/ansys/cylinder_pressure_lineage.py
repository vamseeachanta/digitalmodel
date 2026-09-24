"""Read-only original lineage checks for the single pressure continuation."""
from pathlib import Path

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.analysis_replay import _baseline as _validate_baseline
from digitalmodel.ansys.cylinder_canary import ORDER
from digitalmodel.ansys.cylinder_diagnostic_admission import _path, _read, _json, _relative
from digitalmodel.ansys.cylinder_runtime_bundle import _required as _required_artifacts

ORIGINAL_BYTES = 10174192
FIXED = dict(
    approval='c82af1641466d637ac6f5cf42bb54531dbcea52b84777e83bb3b7225124c5119',
    base_manifest='ea69920aad1d84a17ce37aacabcfc2308ac33eb99c4155d6b7b6421e9192f8c1',
    executed_manifest='922a3fd53ccfecf6d16fbfab680df917e3bcc2c04a04f0422813e12d1accc0f9',
    baseline_file='1d10cb47ab2f77c6ae3904c6b37bf72368f21607d7f8b01e3d699885af2e1be3',
    baseline_package='42b27301fd22ed4fe8126d813ef50e60f45ca4306552dd7c85ef34d4e688b4cc',
    baseline_row='2d6073f861c407e0673431641ef86e9e660ba72270ff437ba4f242292e1b3d25',
    inventory='b25cc44c34433f77a2d79aafed71ac267b7f1dda39f968ec36bed52fa294d09f')


def _bound(pin, role):
    if not isinstance(pin, dict) or set(pin) != {'path','sha256'}:
        raise ValueError('invalid lineage pin '+role)
    if role in FIXED and pin['sha256'] != FIXED[role]:
        raise ValueError('fixed lineage pin differs: '+role)
    return _json(_read(pin['path'], pin['sha256']))


def _baseline(pin):
    if not isinstance(pin,dict) or set(pin) != {'path','file_sha256','package_hash','row_hash'}:
        raise ValueError('exact baseline pins required')
    for field, fixed in [('file_sha256','baseline_file'),('package_hash','baseline_package'),
                         ('row_hash','baseline_row')]:
        if pin[field] != FIXED[fixed]:
            raise ValueError('historical baseline external pin differs')
    data = _json(_read(pin['path'], pin['file_sha256']))
    _validate_baseline(data)
    if data['package_hash'] != pin['package_hash'] or data['cases'][8]['row_hash'] != pin['row_hash']:
        raise ValueError('historical baseline package or row differs')


def _original_inventory(binding):
    root = _path(binding['original_root'], directory=True)
    inventory = _bound(binding['inventory'], 'inventory')
    if _path(inventory['root'], directory=True) != root:
        raise ValueError('original inventory root differs')
    rows = inventory['files']
    if not isinstance(rows,list) or len(rows) != 23:
        raise ValueError('original inventory requires 23 files')
    expected, identities, total = {}, set(), 0
    for row in rows:
        if set(row) != {'path','bytes','sha256'} or type(row['bytes']) is not int or row['bytes'] < 0:
            raise ValueError('invalid original inventory entry')
        path = _relative(root, row['path']); stat = path.stat()
        identity = (stat.st_dev, stat.st_ino)
        if row['path'] in expected or identity in identities:
            raise ValueError('duplicate original inventory identity')
        raw = _read(path,row['sha256'])
        if len(raw) != row['bytes']:
            raise ValueError('original byte count differs')
        expected[row['path']] = row; identities.add(identity); total += len(raw)
    actual = set()
    for path in root.rglob('*'):
        _path(path,directory=path.is_dir())
        if path.is_file(): actual.add(path.relative_to(root).as_posix())
    if actual != set(expected) or total != ORIGINAL_BYTES:
        raise ValueError('original inventory enumeration or total differs')
    attempt = binding['attempt']
    if (_path(attempt['path']) != root/'attempt-1.json'
            or attempt['sha256'] != expected['attempt-1.json']['sha256']):
        raise ValueError('original attempt binding differs')
    return root


def _parent(binding, ledger, original):
    approval = _bound(binding['approval'],'approval')
    config = _bound(binding['config'],'config')
    claim = _bound(binding['parent_claim'],'parent_claim')
    attempt = _bound(binding['attempt'],'attempt')
    campaign = approval['approval_id']
    expected = ledger/(digest_bytes(campaign.encode('utf-8'))+'.json')
    if _path(binding['parent_claim']['path']) != expected:
        raise ValueError('parent claim path or stem differs')
    if (claim != dict(approval_id=campaign, approval_sha256=FIXED['approval'],output=str(original))
            or config['campaign_id'] != campaign
            or approval['config_sha256'] != binding['config']['sha256']
            or approval['manifest_sha256'] != FIXED['executed_manifest']
            or config['execution_binding']['manifest_sha256'] != FIXED['executed_manifest']):
        raise ValueError('parent approval/config/claim relationship differs')
    if (attempt.get('case_id') != 'ocv-zero-t60-n16' or type(attempt.get('ordinal')) is not int
            or attempt['ordinal'] != 1 or attempt.get('state') != 'attempt_consumed'):
        raise ValueError('original ordinal-1 attempt differs')


def _artifacts(root, manifest):
    entries = manifest['artifacts']
    if not isinstance(entries,list) or len(entries) != 21:
        raise ValueError('exact 21 frozen artifacts required')
    rows = {}
    for item in entries:
        if (set(item) != {'path','sha256','bytes'} or type(item['bytes']) is not int
                or item['path'] in rows):
            raise ValueError('invalid or duplicate frozen artifact')
        raw = _read(_relative(root,item['path']),item['sha256'])
        if len(raw) != item['bytes']:
            raise ValueError('frozen artifact size differs')
        rows[item['path']] = item
    return rows


def _manifests(binding, successor_root, successor_sha):
    base = _bound(binding['base_manifest'],'base_manifest')
    executed = _bound(binding['executed_manifest'],'executed_manifest')
    successor = _json(_read(successor_root/'manifest.json',successor_sha))
    required = _required_artifacts(base)
    base_rows = _artifacts(Path(binding['base_manifest']['path']).parent,base)
    if not required.issubset(base_rows):
        raise ValueError('mandatory frozen artifact missing')
    for manifest,root in [(executed,Path(binding['executed_manifest']['path']).parent),
                          (successor,successor_root)]:
        if _artifacts(root,manifest) != base_rows:
            raise ValueError('frozen artifact identity changed')
        clean = lambda d: {k:v for k,v in d.items() if k not in ('runtime_sources','runtime_lineage')}
        if canonical_bytes(clean(manifest)) != canonical_bytes(clean(base)):
            raise ValueError('frozen benchmark case/reference metadata changed')
    return successor


def validate_lineage(config):
    """Verify pinned history without creating, clearing or consuming any record."""
    binding = config['lineage']
    keys = {'original_root','inventory','parent_claim','attempt','approval','config',
            'executed_manifest','base_manifest','baseline'}
    if not isinstance(binding,dict) or set(binding) != keys:
        raise ValueError('exact pressure lineage required')
    _baseline(binding['baseline'])
    original = _original_inventory(binding)
    _parent(binding,_path(config['ledger_directory'],directory=True),original)
    root = _path(config['operational']['bundle'],directory=True)
    return _manifests(binding,root,config['execution_binding']['manifest_sha256'])
