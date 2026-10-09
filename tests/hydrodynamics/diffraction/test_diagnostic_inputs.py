"""Synthetic rights/inspection records exercise protocol, not source clearance."""
import hashlib
import json

import pytest

from digitalmodel.hydrodynamics.diffraction.diagnostic_inputs import (
    build_bundle, divergence_ledger, native_yaml,
)
from tests.hydrodynamics.diffraction.test_diagnostic_mesh import synthetic_mesh


def sources():
    return {'mesh': synthetic_mesh(), 'spec': json.dumps({
        'vessel': {'geometry': {'symmetry': 'xz', 'reference_point': [0, 0, 0]},
                   'inertia': {'mass': 1025, 'centre_of_gravity': [0, 0, -0.5],
                               'radii_of_gyration': [0.4, 0.4, 0.4]}},
        'environment': {'water_density': 1025, 'gravity': 9.80665},
        'frequencies': {'input_type': 'frequency', 'values': [0.3, 0.6, 0.9]},
        'metadata': {'synthetic': True},
    }).encode()}


def rights(raw):
    payload = json.dumps({'sources': {k: hashlib.sha256(v).hexdigest() for k, v in raw.items()},
        'operations': ['transform', 'private-retention'],
        'destination': 'vamseeachanta/digitalmodel-data', 'audience': 'authorized-private',
        'decision': 'permitted', 'basis': 'SYNTHETIC TEST ONLY'}).encode()
    return {'id': 'synthetic-rights', 'sha256': hashlib.sha256(payload).hexdigest()}, lambda _: payload


def expected_sources(raw):
    return {key: hashlib.sha256(value).hexdigest() for key, value in raw.items()}


def test_complete_ledger_unknown_units_and_owner_status():
    raw = sources()
    reference, resolver = rights(raw)
    result = build_bundle(raw, reference, resolver, {'revision': 'a' * 40, 'dirty': True},
                          expected_source_sha256=expected_sources(raw))
    assert result['manifest']['owner_repo'] == 'vamseeachanta/digitalmodel-data'
    assert result['manifest']['intake_status'] == 'not_established_pending_exact_payload_review'
    assert result['manifest']['engineering_qualified'] is False
    assert result['manifest']['source_tree']['dirty'] is True
    ledger = result['ledger']
    for field in ['vessel.inertia.mass', 'environment.water_density', 'environment.gravity',
                  'vessel.inertia.radii_of_gyration', 'vessel.geometry.reference_point',
                  'vessel.geometry.symmetry', 'vessel.inertia.centre_of_gravity',
                  'frequencies.values', 'metadata.synthetic', 'gdf.grav', 'gdf.ulen',
                  'gdf.symmetry', 'gdf.title', 'gdf.panel_count']:
        assert field in ledger
    assert ledger['frequencies.values']['source_units'] == 'unknown'
    assert ledger['vessel.inertia.mass']['source_units'] == 'unknown'
    assert ledger['frequencies.values']['intent']['unit'] == 'rad/s'
    assert ledger['gdf.grav']['effect'] == 'unknown_pending_native_readback'
    assert ledger['intent.interior_surface_panels']['intent'] == 'none'
    assert ledger['intent.angular_frequency']['intent']['unit'] == 'rad/s'
    assert result['native_yaml'] is None


@pytest.mark.parametrize('fault', ['missing', 'digest', 'source', 'denied', 'destination', 'dirty'])
def test_rights_and_provenance_refuse(fault):
    raw = sources()
    reference, resolver = rights(raw)
    tree = {'revision': 'a' * 40, 'dirty': False}
    if fault == 'missing':
        reference = None
    elif fault == 'digest':
        reference['sha256'] = '0' * 64
    elif fault == 'source':
        raw['spec'] += b' '
    elif fault == 'dirty':
        tree.pop('dirty')
    else:
        payload = json.loads(resolver('synthetic-rights'))
        payload['decision' if fault == 'denied' else 'destination'] = 'refused'
        encoded = json.dumps(payload).encode()
        reference['sha256'] = hashlib.sha256(encoded).hexdigest()
        resolver = lambda _: encoded
    with pytest.raises(ValueError):
        build_bundle(raw, reference, resolver, tree, expected_source_sha256=expected_sources(raw))


def test_no_native_yaml_even_with_claimed_mapping():
    with pytest.raises(ValueError):
        native_yaml({'BodyOriginType': 'guessed'}, mapping={'verified': True})


def test_duplicate_source_keys_and_comparison_claim_refuse():
    with pytest.raises(ValueError):
        divergence_ledger(b'{"mass":1,"mass":2}', {})
    raw = sources()
    reference, resolver = rights(raw)
    with pytest.raises(ValueError):
        build_bundle(raw, reference, resolver, {'revision': 'a' * 40, 'dirty': False},
                     comparison_to_source=True, expected_source_sha256=expected_sources(raw))


@pytest.mark.parametrize('raw', [b'mass: .nan', b'mass: .inf', b'date: 2026-09-15',
                               b'loop: &a [*a]', b'bad.key: 1'])
def test_unsafe_source_scalar_or_recursive_structure_refuses(raw):
    with pytest.raises(ValueError):
        divergence_ledger(raw, {})


@pytest.mark.parametrize('namespace', ['gdf', 'intent'])
def test_divergence_namespace_collision_refuses(namespace):
    raw = json.dumps({namespace: {'title': 'source', 'symmetry': 'source'}}).encode()
    with pytest.raises(ValueError):
        divergence_ledger(raw, {'title': 'header'})


def test_consistently_mutated_sources_and_rights_still_refuse():
    raw = sources()
    expected = expected_sources(raw)
    raw['mesh'] = raw['mesh'].replace(b'SYNTHETIC TEST ONLY', b'SYNTHETIC CHANGED')
    reference, resolver = rights(raw)
    with pytest.raises(ValueError):
        build_bundle(raw, reference, resolver, {'revision': 'a' * 40, 'dirty': False},
                     expected_source_sha256=expected)


def test_mapped_missing_basis_key_refuses(monkeypatch):
    import digitalmodel.hydrodynamics.diffraction.diagnostic_inputs as module
    monkeypatch.setitem(module.FIELD_MAP, 'mass', 'absent_basis_key')
    with pytest.raises(ValueError):
        divergence_ledger(b'mass: 1025', {})


@pytest.mark.parametrize('spec',[{'': {'gdf': {'title': 'source'}}},
    {'': {'a': 1}, 'a': 2}, {'nested': {'': 1}}])
def test_empty_keys_cannot_collapse_ledger_paths(spec):
    with pytest.raises(ValueError,match='key'):
        divergence_ledger(json.dumps(spec).encode(), {'title':'header'})


@pytest.mark.parametrize('leaves',[[('gdf.title','source')], [('a',1),('a',2)],
    [('intent.mass','source')]])
def test_all_ledger_insertions_refuse_duplicate_paths(monkeypatch,leaves):
    import digitalmodel.hydrodynamics.diffraction.diagnostic_inputs as module
    monkeypatch.setattr(module,'_leaves',lambda *args,**kwargs:iter(leaves))
    with pytest.raises(ValueError,match='collision|duplicate'):
        divergence_ledger(b'{}',{'title':'header'})


def test_unmapped_source_relationship_is_not_prescribed():
    ledger=divergence_ledger(sources()['spec'],{})
    assert ledger['metadata.synthetic']['relationship']=='source_field_not_carried'
    assert ledger['frequencies.input_type']['relationship']=='source_field_not_carried'
    assert ledger['vessel.inertia.mass']['relationship']=='prescribed_not_recovered'


@pytest.mark.parametrize('extra,value',[('revoked',True),('expires','2000-01-01'),
    ('conditions',['do not redistribute']),('scope_excludes',['solver results'])])
def test_unknown_rights_conditions_refuse(extra,value):
    raw=sources();reference,resolver=rights(raw)
    record=json.loads(resolver(reference['id']));record[extra]=value
    payload=json.dumps(record).encode();reference['sha256']=hashlib.sha256(payload).hexdigest()
    with pytest.raises(ValueError,match='rights|Rights'):
        build_bundle(raw,reference,lambda _:payload,{'revision':'a'*40,'dirty':False},
                     expected_source_sha256=expected_sources(raw))


def test_manifest_binds_derived_bytes_intent_and_both_ledgers():
    raw=sources();reference,resolver=rights(raw)
    result=build_bundle(raw,reference,resolver,{'revision':'a'*40,'dirty':False},
                        expected_source_sha256=expected_sources(raw))
    manifest=result['manifest']
    assert manifest['derived_sha256']==hashlib.sha256(result['mesh']['derived_gdf']).hexdigest()
    assert manifest['intent_sha256']==hashlib.sha256(result['intent_bytes']).hexdigest()
    ledgers={'divergence':result['ledger'],'mesh_transformation':result['mesh']['panels']}
    raw_ledgers=json.dumps(ledgers,sort_keys=True,separators=(',',':'),ensure_ascii=False,allow_nan=False).encode('utf-8')
    assert manifest['ledger_sha256']==hashlib.sha256(raw_ledgers).hexdigest()
    assert manifest['ledger_digest_scope']=='canonical-json:divergence-and-mesh-transformation'


def test_shared_budget_refuses_alias_breadth_before_depth_limit():
    rows=['a0: &a0 [1]']
    for i in range(1,15):rows.append(f'a{i}: &a{i} [*a{i-1}, *a{i-1}]')
    with pytest.raises(ValueError,match='budget'):
        divergence_ledger(('\n'.join(rows)+'\n').encode(),{})
