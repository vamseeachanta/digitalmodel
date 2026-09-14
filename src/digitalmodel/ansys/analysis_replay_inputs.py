"""Bounded replay inventories; read-only sources, no receipt-driven imports."""
from pathlib import Path
import re

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json, verify_reference
from digitalmodel.ansys.cylinder_criteria import EXPECTED_KEYS

CASE_ID = 'ocv-zero-t60-n16'
REFERENCE_HASH = '385ff3d42e4219faefd8cc1a72c20525f4b2731cf2fcb31aa51ba5b1de63c3b4'
RUNTIME_HASH = '922a3fd53ccfecf6d16fbfab680df917e3bcc2c04a04f0422813e12d1accc0f9'
RUNTIME_REVISION = '16894a32dfc92c217c7398720897ce3e54cdb1ee'
RUNTIME_INVENTORY = '878076ee3fef48cfdb365e1ab794fdea33b6bd6ecb046ffcfb545517a51ba638'
RAW_NAMES = ('attempt-1.json', 'operator-outcome.json', 'outcome.json', 'preflight-1.json',
    *[CASE_ID + '/' + n for n in ('execution.json', 'file.db', 'file.DSP', 'file.err',
       'file.esav', 'file.full', 'file.log', 'file.mntr', 'file.rst', 'file.stat', 'model.cdb',
       CASE_ID + '.inp', CASE_ID + '.out', 'precision_witness.txt', 'state_values.txt',
       'station_values.txt', 'stderr.bin', 'stdout.bin', 'support_reactions.txt')])
DOCUMENT_ROLES = {'reference', 'mapping', 'runtime_manifest', 'approval', 'config',
                  'review', 'review_transport', 'review_bundle'}


def source_inventory():
    from digitalmodel.ansys.cylinder_canary import runtime_sources
    inventory = {item['path']: item['sha256'] for item in runtime_sources()}
    parent = Path(__file__).resolve().parent.parent
    for name in ('__init__.py', '_compat.py'):
        path = parent / name
        if path.is_symlink() or not path.is_file():
            raise ValueError('missing or redirected parent package source')
        inventory['src/digitalmodel/' + name] = digest_bytes(path.read_bytes())
    return inventory


def opaque_reference(ref):
    if not isinstance(ref, dict) or set(ref) != {'id', 'sha256', 'required'}:
        raise ValueError('required opaque reference shape differs')
    if (ref['required'] is not True or not isinstance(ref['id'], str)
            or not re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9_-]{0,99}', ref['id'])
            or not isinstance(ref['sha256'], str)
            or not re.fullmatch(r'[0-9a-f]{64}', ref['sha256'])):
        raise ValueError('required opaque reference invalid')


def validate_review(review, inventory):
    result = review.get('review', {})
    if (review.get('status') != 'REVIEW_RECEIVED' or result.get('verdict') not in ('APPROVE', 'MINOR')
            or not review.get('bundle_sha256') or result.get('bundle_sha256') != review['bundle_sha256']):
        raise ValueError('independent review unavailable, blocking or inconsistent')
    covered = {}
    for row in review.get('files', []):
        path = row['path'].replace('\\', '/')
        if '/src/digitalmodel/' in path:
            path = 'src/digitalmodel/' + path.split('/src/digitalmodel/')[-1]
        if path in covered:
            raise ValueError('duplicate review source')
        covered[path] = row['sha256']
    if any(covered.get(path) != digest for path, digest in inventory.items()):
        raise ValueError('independent review missing exact current source coverage')


def validate_mapping(mapping, responses):
    if (not isinstance(mapping, dict) or set(mapping) != {'schema', 'responses'}
            or mapping['schema'] != 'zero-replay-mapping-1' or len(mapping['responses']) != 64
            or len(responses) != 64):
        raise ValueError('expected complete semantic mapping')
    originals = {r['name']: r for r in responses}
    found, names = {}, set()
    for row in mapping['responses']:
        if set(row) != {'key', 'name', 'definition', 'location', 'unit'}:
            raise ValueError('mapping fields differ')
        key = tuple(row['key']) if isinstance(row['key'], list) else row['key']
        if key not in EXPECTED_KEYS | {'RFY'} or key in found or row['name'] in names:
            raise ValueError('mapping keys are unknown or duplicate')
        expected_name = 'support.RFY' if key == 'RFY' else '.'.join(key)
        unit = 'N' if key == 'RFY' else ('mm' if key[1].startswith('u_') else 'MPa')
        old = originals.get(row['name'], {})
        if row['name'] != expected_name or row['unit'] != unit or any(
                row[k] != old.get(k) for k in ('name', 'definition', 'location', 'unit')):
            raise ValueError('mapping semantic metadata differs')
        found[key], names = row, names | {row['name']}
    if set(found) != EXPECTED_KEYS | {'RFY'}:
        raise ValueError('incomplete semantic mapping')
    return found


def _resolve_inventory(receipt, resolver):
    if set(receipt['raw']) != set(RAW_NAMES) or set(receipt['code']) != set(source_inventory()):
        raise ValueError('fixed raw or code inventory differs')
    if set(receipt['documents']) != DOCUMENT_ROLES:
        raise ValueError('document roles differ')
    resolved, evidence, ids = {}, [], set()
    for group in ('raw', 'code', 'documents'):
        resolved[group] = {}
        for role, ref in sorted(receipt[group].items()):
            opaque_reference(ref)
            if ref['id'] in ids:
                raise ValueError('duplicate evidence id')
            ids.add(ref['id'])
            resolved[group][role] = verify_reference(ref, resolver)
            evidence.append(dict(ref, role=group))
    for path, expected in source_inventory().items():
        if digest_bytes(resolved['code'][path]) != expected:
            raise ValueError('current source differs from replay inventory')
    return resolved, evidence


def _review_binding(raw, inventory, expected):
    if digest_bytes(raw['review']) != expected:
        raise ValueError('externally pinned review digest differs')
    review, transport = parse_json(raw['review']), parse_json(raw['review_transport'])
    bundle = parse_json(raw['review_bundle'])
    validate_review(review, inventory)
    if (digest_bytes(raw['review_bundle']) != review['bundle_sha256']
            or digest_bytes(raw['review_transport']) != review.get('stdout_sha256')
            or transport.get('is_error') is not False or not transport.get('session_id')
            or transport.get('structured_output') != review['review']):
        raise ValueError('review transport or bundle differs')
    entries = bundle.get('files', [])
    for item in entries:
        if digest_bytes(item['content'].encode('utf-8')) != item['sha256']:
            raise ValueError('review bundled source bytes differ')
    validate_review(dict(review, files=entries), inventory)


def load_replay_inputs(reference, resolver, previous_case_hash, review_sha256):
    opaque_reference(reference)
    receipt = parse_json(verify_reference(reference, resolver))
    if (set(receipt) != {'schema', 'case_id', 'previous_case_hash', 'raw', 'code', 'documents'}
            or receipt['schema'] != 'zero-numeric-replay-1' or receipt['case_id'] != CASE_ID
            or receipt['previous_case_hash'] != previous_case_hash):
        raise ValueError('replay receipt identity differs')
    resolved, evidence = _resolve_inventory(receipt, resolver)
    docs = resolved['documents']
    _review_binding(docs, source_inventory(), review_sha256)
    if digest_bytes(docs['reference']) != REFERENCE_HASH or digest_bytes(docs['runtime_manifest']) != RUNTIME_HASH:
        raise ValueError('frozen reference or executed runtime differs')
    runtime, approval, config = [parse_json(docs[n]) for n in ('runtime_manifest', 'approval', 'config')]
    lineage = runtime.get('runtime_lineage', {})
    if (lineage.get('source_revision') != RUNTIME_REVISION
            or lineage.get('runtime_inventory_sha256') != RUNTIME_INVENTORY
            or approval.get('manifest_sha256') != RUNTIME_HASH
            or approval.get('config_sha256') != digest_bytes(docs['config'])
            or config.get('execution_binding', {}).get('manifest_sha256') != RUNTIME_HASH):
        raise ValueError('executed configuration lineage differs')
    outcome = parse_json(resolved['raw']['operator-outcome.json'])
    if outcome.get('execution_approval_sha256') != digest_bytes(docs['approval']):
        raise ValueError('original outcome approval binding differs')
    return receipt, resolved, evidence
