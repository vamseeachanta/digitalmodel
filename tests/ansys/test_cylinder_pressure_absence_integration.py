"""Synthetic absence-only admission, never a native or licence probe."""
from copy import deepcopy
import pytest
from digitalmodel.ansys import cylinder_diagnostic_preflight as preflight
from digitalmodel.ansys import cylinder_pressure_resources as resources
from digitalmodel.ansys.cylinder_pressure_scope import INTERMEDIATE_SCOPE
from . import test_cylinder_diagnostic_preflight as preflight_fixtures

prepared = preflight_fixtures.prepared
_set_cfd_binding = preflight_fixtures._set_cfd_binding

@pytest.fixture
def absence(prepared, monkeypatch):
    config, approval, reservation, _, snapshot = prepared
    approval['scope'] = deepcopy(INTERMEDIATE_SCOPE)
    _set_cfd_binding(config, dict(schema='cfd-resource-absence-1', host='TEST-HOST',
        selector='ansys-mpi-lineage-v1', required_selected_state='empty'))
    snapshot['initial_inventory'] = []
    snapshot['coverage'].update(enumerated_count=0, selected_count=0, excluded_count=0)
    calls = []
    def collect(binding):
        assert binding is None
        calls.append(len(calls))
        return deepcopy(snapshot)
    monkeypatch.setattr(preflight, '_absence_snapshot', lambda: collect(None), raising=False)
    def wrong_route(binding):
        raise AssertionError('absence must use the reviewed protected-name collector')
    monkeypatch.setattr(preflight, 'collect_process_snapshot', wrong_route)
    monkeypatch.setattr(resources, '_now', lambda: '1006')
    return preflight.ProductionPreflight(config, approval, reservation), approval, snapshot, calls


def test_three_fresh_empty_observations(absence):
    collector, approval, _, calls = absence
    assert collector(approval)['classification']['status'] == 'CLEAR'
    collector.before_launch()
    result = resources.production_phase2(collector, lambda _: True, approval)
    assert result['status'] == 'PASS'
    records = result['process_absence_checks']
    assert [row['stage'] for row in records] == ['initial', 'before_launch', 'phase2']
    assert all(row['snapshot']['rows'] == [] and row['status'] == 'CLEAR' for row in records)
    assert len(calls) == 3
    assert collector.last_evidence['process_absence']['scope'] == 'ansys-mpi-lineage-v1 plus interFoam.exe names'


def test_preclaim_probe_has_distinct_stage_and_preserves_observation_times(absence):
    collector, approval, _, calls = absence
    collector(approval)
    collector.before_launch()
    ready, license_record = collector._ready_at, deepcopy(collector.last_evidence['license_observation'])
    result = resources.production_phase2(collector, lambda _: True, approval, stage='preclaim-probe')
    assert result['process_absence_checks'][-1]['stage'] == 'preclaim-probe'
    assert collector._ready_at == ready
    assert collector.last_evidence['license_observation'] == license_record
    final = resources.production_phase2(collector, lambda _: True, approval)
    assert final['process_absence_checks'][-1]['stage'] == 'phase2'
    assert len(calls) == 4


@pytest.mark.parametrize('name', ['ANSYS261.EXE', 'mpiexec.exe', 'interFoam.exe'])
def test_independent_verifier_rejects_competitor_only_in_history(absence, name):
    collector, approval, snapshot, _ = absence
    row = dict(pid=44,parent_pid=1,name=name,creation_time='1000',executable_path='C:/bin/'+name)
    snapshot['observed_inventories'] = [[row], [], []]
    with pytest.raises(ValueError, match='historical'):
        collector(approval)

@pytest.mark.parametrize('phase', ['initial', 'before_launch', 'phase2'])
def test_orphan_fluid_solver_refuses_at_each_observation(absence, phase):
    collector, approval, snapshot, calls = absence
    if phase != 'initial': collector(approval)
    if phase == 'phase2': collector.before_launch()
    snapshot['initial_inventory'] = [dict(pid=99,parent_pid=1,name='INTERFOAM.EXE',
        creation_time='1000',executable_path=None)]
    snapshot['coverage'].update(enumerated_count=1, excluded_count=1)
    with pytest.raises(ValueError):
        if phase == 'initial': collector(approval)
        elif phase == 'before_launch': collector.before_launch()
        else: resources.production_phase2(collector, lambda _: True, approval)


def test_absence_does_not_accept_owner_or_wrapper_exemption(absence):
    collector, approval, _, _ = absence
    collector.config['cfd_owner_evidence'] = {'unreviewed': True}
    with pytest.raises(ValueError): collector(approval)


@pytest.mark.parametrize('stage', ['initial', 'before_launch', 'phase2'])
def test_collection_refusal_retains_original_query_evidence(absence, monkeypatch, stage):
    collector, approval, _, _ = absence
    if stage != 'initial': collector(approval)
    if stage == 'phase2': collector.before_launch()
    class QueryRefusal(ValueError):
        evidence = {'stdout_base64': 'cGFydGlhbA==', 'return_code': None}
    def refuse(): raise QueryRefusal('synthetic bounded query timeout')
    monkeypatch.setattr(preflight, '_absence_snapshot', refuse)
    with pytest.raises(QueryRefusal):
        if stage == 'initial': collector(approval)
        elif stage == 'before_launch': collector.before_launch()
        else: resources.production_phase2(collector, lambda _: True, approval)
    record = collector.last_evidence['process_collection_error']
    assert record['stage'] == stage and record['evidence'] == QueryRefusal.evidence
