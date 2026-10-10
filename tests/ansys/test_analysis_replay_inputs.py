"""Synthetic digest inventories and transport refusals; no provider invocation."""
from copy import deepcopy
import pytest
from digitalmodel.ansys import analysis_replay_inputs as inputs
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes


def inventory_fixture(tmp_path):
    refs, resolver = {}, {}
    inventory = inputs.source_inventory()
    for group, names in [('raw', inputs.RAW_NAMES), ('code', inventory),
                         ('documents', inputs.DOCUMENT_ROLES)]:
        refs[group] = {}
        for number, name in enumerate(names):
            identity = group + '-' + str(number)
            raw = b'{}'
            if group == 'code':
                from pathlib import Path
                raw = (Path(inputs.__file__).resolve().parents[3] / name).read_bytes()
            path = tmp_path / identity
            path.write_bytes(raw)
            resolver[identity] = path
            refs[group][name] = dict(id=identity, sha256=digest_bytes(raw), required=True)
    return refs, resolver


def test_complete_fixed_inventory_resolves_original_bytes(tmp_path):
    receipt, resolver = inventory_fixture(tmp_path)
    raw, evidence = inputs._resolve_inventory(receipt, resolver)
    assert len(raw['raw']) == 23
    assert len(evidence) == sum(len(rows) for rows in receipt.values())


@pytest.mark.parametrize('fault', ['raw_missing', 'code_missing', 'changed_bytes', 'required', 'path', 'duplicate'])
def test_inventory_rejects_missing_changed_or_private_identifiers(tmp_path, fault):
    receipt, resolver = inventory_fixture(tmp_path)
    ref = next(iter(receipt['raw'].values()))
    if fault == 'raw_missing':
        receipt['raw'].pop(next(iter(receipt['raw'])))
    elif fault == 'code_missing':
        receipt['code'].pop(next(iter(receipt['code'])))
    elif fault == 'changed_bytes':
        resolver[ref['id']].write_bytes(b'changed')
    elif fault == 'required':
        ref['required'] = False
    elif fault == 'path':
        ref['id'] = 'private/path'
    else:
        receipt['raw'][list(receipt['raw'])[1]] = deepcopy(ref)
    with pytest.raises(ValueError):
        inputs._resolve_inventory(receipt, resolver)


def transport_fixture():
    content = 'synthetic reviewed source\n'
    inventory = {'src/digitalmodel/ansys/example.py': digest_bytes(content.encode())}
    files = [dict(path=next(iter(inventory)), sha256=next(iter(inventory.values())), content=content)]
    bundle = canonical_bytes(dict(files=files))
    review = dict(bundle_sha256=digest_bytes(bundle), verdict='MINOR', findings=[])
    transport = canonical_bytes(dict(is_error=False, session_id='synthetic-provider-session', structured_output=review))
    receipt = canonical_bytes(dict(status='REVIEW_RECEIVED', bundle_sha256=digest_bytes(bundle),
        stdout_sha256=digest_bytes(transport), files=files, review=review))
    return dict(review=receipt, review_transport=transport, review_bundle=bundle), inventory


def test_pinned_transport_and_bundle_agree():
    raw, inventory = transport_fixture()
    inputs._review_binding(raw, inventory, digest_bytes(raw['review']))


def repin_transport(raw, transport):
    import json
    receipt = json.loads(raw['review'])
    raw['review_transport'] = transport
    receipt['stdout_sha256'] = digest_bytes(transport)
    raw['review'] = canonical_bytes(receipt)
    return digest_bytes(raw['review'])


def test_provider_cost_float_is_metadata_without_changing_engineering_parser():
    from digitalmodel.ansys.analysis_records import parse_json
    raw, inventory = transport_fixture()
    transport = raw['review_transport'][:-1] + b',"total_cost_usd":0.123456,"metrics":{"rate":1e-7}}'
    pin = repin_transport(raw, transport)
    inputs._review_binding(raw, inventory, pin)
    with pytest.raises(ValueError, match='float'):
        parse_json(b'{"engineering_value":0.123456}')


@pytest.mark.parametrize('extra', [b'"rate":NaN', b'"rate":Infinity',
    b'"rate":-Infinity', b'"rate":1e9999', b'"rate":1,"rate":2', b'"is_error":false'])
def test_provider_metadata_nonfinite_and_duplicates_refuse(extra):
    raw, inventory = transport_fixture()
    pin = repin_transport(raw, raw['review_transport'][:-1] + b',' + extra + b'}')
    with pytest.raises(ValueError):
        inputs._review_binding(raw, inventory, pin)


def test_provider_structured_payload_retains_float_refusal():
    with pytest.raises(ValueError, match='float'):
        inputs._provider_transport(b'{"structured_output":{"engineering_value":0.125}}')


@pytest.mark.parametrize('entry', [None, [], {}, {'path': 1, 'content': 2, 'sha256': 3}])
def test_malformed_review_bundle_row_refuses_with_value_error(entry):
    import json
    raw, inventory = transport_fixture()
    raw['review_bundle'] = canonical_bytes({'files': [entry]})
    receipt = json.loads(raw['review'])
    receipt['bundle_sha256'] = digest_bytes(raw['review_bundle'])
    receipt['review']['bundle_sha256'] = receipt['bundle_sha256']
    transport = json.loads(raw['review_transport'])
    transport['structured_output'] = receipt['review']
    raw['review'] = canonical_bytes(receipt)
    pin = repin_transport(raw, canonical_bytes(transport))
    with pytest.raises(ValueError):
        inputs._review_binding(raw, inventory, pin)


@pytest.mark.parametrize('fault', ['external_pin', 'bundle', 'transport'])
def test_review_chain_tamper_refuses(fault):
    raw, inventory = transport_fixture()
    pin = digest_bytes(raw['review'])
    if fault == 'external_pin':
        pin = 'a' * 64
    else:
        raw['review_' + fault] += b' '
    with pytest.raises(ValueError):
        inputs._review_binding(raw, inventory, pin)


def test_code_inventory_covers_fixed_deck_export_dependency():
    assert 'src/digitalmodel/ansys/cylinder_deck_exports.py' in inputs.source_inventory()


def test_original_binding_uses_observation_receipt_not_ambiguous_role(tmp_path):
    from digitalmodel.ansys.analysis_replay import _original
    from tests.ansys.test_analysis_replay_original_artifacts import fixture
    raw, original, _ = fixture(tmp_path)
    sources, resolver = {}, {}
    for role, data in [('outcome', raw['outcome.json']), ('execution', raw[inputs.CASE_ID+'/execution.json']), ('review', b'old review')]:
        path = tmp_path/role
        path.write_bytes(data)
        resolver[role] = path
        sources[role] = dict(id=role, sha256=digest_bytes(data))
    encoded = canonical_bytes(dict(sources=sources))
    path = tmp_path/'observation'
    path.write_bytes(encoded)
    resolver['observation'] = path
    old = dict(observation_reference=dict(id='observation', sha256=digest_bytes(encoded)),
               evidence=[dict(role='outcome', id='unrelated', sha256='a'*64)])
    assert _original(raw, old, resolver, {'review': b'new independent replay review'}) == original
    path.write_bytes(b'changed')
    with pytest.raises(ValueError):
        _original(raw, old, resolver, {})
