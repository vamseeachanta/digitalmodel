"""Inventory provenance labels do not relax retention or imply qualification."""
from collections import Counter
from digitalmodel.ansys import analysis_replay_inputs as inputs
from tests.ansys.test_analysis_replay_inputs import inventory_fixture


def test_raw_inventory_has_explicit_provenance_without_changing_references(tmp_path):
    receipt, resolver = inventory_fixture(tmp_path)
    resolved, evidence = inputs._resolve_inventory(receipt, resolver)
    by_id = {ref['id']: ref for ref in evidence}
    expected = {inputs.CASE_ID+'/'+name: 'execution_bound_output'
                for name in inputs.ORIGINAL_ARTIFACT_NAMES}
    expected.update({'outcome.json': 'observation_bound_record',
        inputs.CASE_ID+'/execution.json': 'observation_bound_record',
        'operator-outcome.json': 'approval_linked_record'})
    for name, original in receipt['raw'].items():
        row = by_id[original['id']]
        assert row['role'] == expected.get(name, 'unbound_auxiliary')
        assert {k: v for k, v in row.items() if k != 'role'} == original
        assert row['required'] is True
        assert resolved['raw'][name] == resolver[original['id']].read_bytes()
    assert Counter(by_id[ref['id']]['role'] for ref in receipt['raw'].values()) == {
        'execution_bound_output': 13, 'observation_bound_record': 2,
        'approval_linked_record': 1, 'unbound_auxiliary': 7}
    for group in ('code', 'documents'):
        for ref in receipt[group].values():
            assert by_id[ref['id']] == dict(ref, role=group)


def test_unbound_auxiliary_remains_required_and_digest_verified(tmp_path):
    import pytest
    receipt, resolver = inventory_fixture(tmp_path)
    ref = receipt['raw']['preflight-1.json']
    resolver[ref['id']].write_bytes(b'changed auxiliary')
    with pytest.raises(ValueError):
        inputs._resolve_inventory(receipt, resolver)
