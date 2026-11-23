"""Synthetic observations only; no live processes or licensed execution."""
from copy import deepcopy

import pytest

from tests.ansys.test_cylinder_diagnostic_snapshot_v2 import fixture
from digitalmodel.ansys.cylinder_absence_population import (
    PopulationError, reconcile_observed_inventories,
)


def observations(tmp_path, monkeypatch, transform):
    current, seed, _ = fixture(tmp_path, monkeypatch)
    initial = current._enumerate()
    stages = [deepcopy(initial) for _ in range(3)]
    transform(stages)
    iterator = iter(stages)
    monkeypatch.setattr(current, '_enumerate', lambda: next(iterator))
    return current, seed, stages


def extra(pid=100, parent=0, name='unrelated.exe', created='3'):
    return dict(pid=pid, parent_pid=parent, name=name,
                creation_time=created, executable_path='C:/synthetic/tool.exe')


@pytest.mark.parametrize('kind', ['added', 'removed', 'transient', 'parent'])
def test_complete_unrelated_churn_retains_evidence(tmp_path, monkeypatch, kind):
    def transform(stages):
        if kind == 'added':
            stages[2].append(extra())
        elif kind == 'removed':
            stages[0].append(extra())
        elif kind == 'transient':
            stages[1].append(extra())
        else:
            for index, stage in enumerate(stages):
                stage.append(extra(parent=99 if index else 0))
    current, seed, stages = observations(tmp_path, monkeypatch, transform)
    result = current.collect_v2(discovery_seed=seed)
    assert result['observed_inventories'] == stages
    assert result['initial_inventory'] == stages[0]
    assert result['coverage']['enumerated_count'] == len(stages[0])
    assert result['population_check']['events']
    assert result['population_check']['parent_map_basis'].startswith('row-derived')
    assert all(e['disposition'] == 'RETAINED_UNRELATED'
               for e in result['population_check']['events'])


@pytest.mark.parametrize('name', ['ANSYS261.exe', 'mpiexec.exe',
                                  'smpd.exe', 'interFoam.exe'])
def test_relevant_addition_refuses_with_evidence(tmp_path, monkeypatch, name):
    current, seed, _ = observations(
        tmp_path, monkeypatch, lambda s: s[2].append(extra(name=name)))
    with pytest.raises(PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['events'][-1]['disposition'] == 'REFUSED_RELEVANT'
    assert len(caught.value.evidence['completed_inventories']) == 3


@pytest.mark.parametrize('fault', ['child', 'lost', 'parent', 'reuse', 'incomplete'])
def test_selected_or_unknown_change_refuses(tmp_path, monkeypatch, fault):
    def transform(stages):
        if fault == 'child':
            stages[2].append(extra(parent=2))
        elif fault == 'lost':
            stages[2].pop()
        elif fault == 'parent':
            stages[2][1]['parent_pid'] = 999
        elif fault == 'reuse':
            stages[0].append(extra())
            stages[2].append(extra(created='4'))
        else:
            stages[2].append(dict(extra(), executable_path=None))
    current, seed, _ = observations(tmp_path, monkeypatch, transform)
    with pytest.raises(PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['failed_stage']
    assert caught.value.evidence['completed_inventories']


@pytest.mark.parametrize('failed', [0, 1, 2])
def test_stage_failure_retains_completed_observations(tmp_path, monkeypatch, failed):
    current, seed, _ = fixture(tmp_path, monkeypatch)
    initial = current._enumerate()
    calls = []
    def enumerate_stage():
        calls.append(1)
        if len(calls) == failed + 1:
            raise ValueError('observation unavailable')
        return deepcopy(initial)
    monkeypatch.setattr(current, '_enumerate', enumerate_stage)
    with pytest.raises(PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['failed_stage'] == ['A', 'B', 'C'][failed]
    assert len(caught.value.evidence['completed_inventories']) == failed


@pytest.mark.parametrize('required', [[True], [0], [-1], [2, 1], [1, 1], '1'])
def test_required_identifiers_are_strict(required):
    with pytest.raises(ValueError):
        reconcile_observed_inventories([[], [], []], [{}, {}, {}],
                                       required_relevant_pids=required)


def test_missing_required_identifier_retains_relevance():
    stages = [[extra()], [], []]
    maps = [{100: 0}, {}, {}]
    with pytest.raises(PopulationError) as caught:
        reconcile_observed_inventories(stages, maps, required_relevant_pids=[100])
    assert caught.value.evidence['events'][-1]['disposition'] == 'REFUSED_RELEVANT'


@pytest.mark.parametrize('stage', [0, 1, 2])
def test_blank_name_is_explicit_refusal(tmp_path, monkeypatch, stage):
    from digitalmodel.ansys import cylinder_absence_collection as resolver
    def refuse_query(pids):
        raise resolver.CollectionError('synthetic unresolved name', {'performed': True})
    monkeypatch.setattr(resolver, '_query_blank_names', refuse_query)
    current, seed, _ = observations(
        tmp_path, monkeypatch, lambda s: s[stage].append(extra(name=' ')))
    with pytest.raises(PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['failed_stage'] == ['A', 'B', 'C'][stage]
    assert caught.value.evidence['events'][0]['change'] == 'blank_process_name'
    assert len(caught.value.evidence['completed_inventories']) == stage
    assert caught.value.evidence['rejected_inventory'][-1]['name'] == ' '


def test_parent_change_across_middle_gap_is_retained():
    rows = [[extra()], [], [extra(parent=99)]]
    result = reconcile_observed_inventories(rows, [{100: 0}, {}, {100: 99}])
    assert result['events'] == [
        dict(from_stage='A', to_stage='C', pid=100, change='parent_changed',
             disposition='RETAINED_UNRELATED', basis='endpoint_gap'),
        dict(from_stage='A', to_stage='B', pid=100, change='removed',
             disposition='RETAINED_UNRELATED'),
        dict(from_stage='B', to_stage='C', pid=100, change='added',
             disposition='RETAINED_UNRELATED')]


def test_stable_blank_name_refuses_without_normalization(tmp_path, monkeypatch):
    from digitalmodel.ansys import cylinder_absence_collection as resolver
    def refuse_query(pids):
        raise resolver.CollectionError('synthetic unresolved name', {'performed': True})
    monkeypatch.setattr(resolver, '_query_blank_names', refuse_query)
    def transform(stages):
        for stage in stages:
            stage.append(extra(name=' '))
    current, seed, _ = observations(tmp_path, monkeypatch, transform)
    with pytest.raises(PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['completed_inventories'] == []
    assert 'Process observation or name-resolution validation failed' in caught.value.evidence['limitation']


@pytest.mark.parametrize('function,stage', [('_details', 'detail_reads'),
                                           ('_forwarders', 'forwarders')])
def test_detail_phase_failure_label(tmp_path, monkeypatch, function, stage):
    current, seed, _ = fixture(tmp_path, monkeypatch)
    def fail(*args):
        raise ValueError('synthetic detail phase failure')
    monkeypatch.setattr(current, function, fail)
    with pytest.raises(PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['failed_stage'] == stage
