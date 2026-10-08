"""Synthetic rejected parent-table evidence; no process or solver execution."""
import json
import pickle
from copy import deepcopy

import pytest

from . import test_cylinder_parent_map as maps_fixture
from . import test_cylinder_diagnostic_snapshot_v2 as snapshot_fixture


def rejected(monkeypatch, initial, final, skipped=()):
    current = maps_fixture.tool()
    tables = iter([initial, final])
    monkeypatch.setattr(current, 'read_windows_parent_map', lambda: next(tables))
    def process(pid):
        if pid in skipped:
            raise current.psutil.NoSuchProcess(pid)
        return maps_fixture.fake_process(pid)
    monkeypatch.setattr(current.psutil, 'Process', process)
    with pytest.raises(ValueError, match='initial parent') as caught:
        current.enumerate_windows_v2()
    return caught.value


@pytest.mark.parametrize('final,change,before,after', [
    ({1: 0, 2: 0}, 'added', None, 0),
    ({}, 'disappeared', 0, None),
    ({1: 2}, 'parent_changed', 0, 2),
])
def test_rejection_preserves_observed_differences(monkeypatch, final, change,
                                                before, after):
    error = rejected(monkeypatch, {1: 0}, final)
    assert type(error).__module__ == maps_fixture.tool().__name__
    evidence = error.evidence['rejected_parent_observation']
    changed_pid = 2 if change == 'added' else 1
    assert evidence['initial_parent_table'] == [[1, 0]]
    assert evidence['final_parent_table'] == [[pid, final[pid]] for pid in sorted(final)]
    assert evidence['changes'] == [dict(pid=changed_pid, change=change,
        initial_parent_pid=before, final_parent_pid=after)]
    assert evidence['trigger_pids'] == [changed_pid]
    assert evidence['initial_observed_pids'] == [1]
    assert evidence['status'] == 'REFUSED'
    assert evidence['resource_relevance'] == 'NOT_EVALUATED'
    assert error.evidence['events'] == [dict(change='parent_table_changed',
        disposition='REFUSED_PARENT_TABLE_CHANGED', trigger_pids=[changed_pid])]
    assert json.loads(json.dumps(error.evidence)) == error.evidence


def test_observed_difference_is_not_always_a_refusal_trigger(monkeypatch):
    error = rejected(monkeypatch, {2: 1, 1: 0}, {3: 0, 1: 0}, skipped=(2,))
    evidence = error.evidence['rejected_parent_observation']
    assert [row['pid'] for row in evidence['changes']] == [2, 3]
    assert evidence['trigger_pids'] == [3]
    assert evidence['initial_observed_pids'] == [1]
    assert evidence['unobserved_initial_pids'] == [2]


def test_rejected_tables_are_copied(monkeypatch):
    initial, final = {1: 0}, {1: 2}
    error = rejected(monkeypatch, initial, final)
    preserved = json.loads(json.dumps(error.evidence))
    initial[1] = 88
    final.clear()
    assert error.evidence == preserved


def test_skipped_but_still_listed_process_is_not_mislabeled(monkeypatch):
    error = rejected(monkeypatch, {2: 1, 1: 0}, {3: 0, 2: 1, 1: 0}, skipped=(2,))
    evidence = error.evidence['rejected_parent_observation']
    assert evidence['unobserved_initial_pids'] == [2]
    assert [row['pid'] for row in evidence['changes']] == [3]
    assert evidence['trigger_pids'] == [3]


def test_unbounded_process_text_is_excluded(monkeypatch):
    original = maps_fixture.fake_process
    def huge_process(pid):
        process = original(pid)
        process.name = lambda: 'x' * 100000
        process.exe = lambda: 'y' * 100000
        return process
    monkeypatch.setattr(maps_fixture, 'fake_process', huge_process)
    error = rejected(monkeypatch, {1: 0}, {1: 2})
    numeric = error.evidence['rejected_parent_observation']
    assert len(json.dumps(numeric)) < 2048
    assert 'executable_path' not in json.dumps(numeric)
    from digitalmodel.ansys.analysis_records import canonical_bytes
    assert len(canonical_bytes(error.evidence['rejected_parent_identity'])) <= 32768
    assert 'x' * 100000 not in json.dumps(error.evidence)
    assert 'y' * 100000 not in json.dumps(error.evidence)


@pytest.mark.parametrize('copy_method', [deepcopy, lambda error: pickle.loads(pickle.dumps(error))])
def test_exception_copy_preserves_independent_evidence(monkeypatch, copy_method):
    error = rejected(monkeypatch, {1: 0}, {1: 2})
    copied = copy_method(error)
    assert copied.evidence == error.evidence and str(copied) == str(error)
    copied.evidence['rejected_parent_observation']['changes'].clear()
    assert error.evidence['rejected_parent_observation']['changes']


def test_disappearance_uses_validated_nonempty_parent_tables(monkeypatch):
    current = maps_fixture.tool()
    tables = iter([{1: 0, 2: 1}, {1: 0}])
    monkeypatch.setattr(current.backend, 'ppid_map', lambda: next(tables))
    monkeypatch.setattr(current.psutil, 'Process', maps_fixture.fake_process)
    with pytest.raises(ValueError, match='initial parent') as caught:
        current.enumerate_windows_v2()
    evidence = caught.value.evidence['rejected_parent_observation']
    assert evidence['final_parent_table'] == [[1, 0]]
    assert evidence['changes'] == [dict(pid=2, change='disappeared',
        initial_parent_pid=1, final_parent_pid=None)]
    assert evidence['trigger_pids'] == [2]


@pytest.mark.parametrize('failed_phase', [0, 1, 2])
def test_rejected_observation_survives_collector_without_completion(
        tmp_path, monkeypatch, failed_phase):
    current, seed, _ = snapshot_fixture.fixture(tmp_path, monkeypatch)
    initial = current._enumerate()
    parent = maps_fixture.tool()
    tables = iter([{1: 0, 2: 1}, {1: 0, 2: 99}])
    monkeypatch.setattr(parent, 'read_windows_parent_map', lambda: next(tables))
    calls, producer_evidence = [], []
    def enumerate_rows():
        calls.append(1)
        if len(calls) != failed_phase + 1:
            return initial
        try:
            return parent.enumerate_windows_v2()
        except ValueError as error:
            producer_evidence.append(error.evidence)
            raise
    monkeypatch.setattr(current, '_enumerate', enumerate_rows)
    with pytest.raises(ValueError, match='initial parent') as caught:
        current.collect_v2(discovery_seed=seed)
    evidence = caught.value.evidence
    assert evidence['failed_stage'] == ('A', 'B', 'C')[failed_phase]
    assert len(evidence['completed_inventories']) == failed_phase
    assert len(evidence['completed_parent_maps']) == failed_phase
    assert evidence['rejected_parent_observation']['trigger_pids'] == [2]
    assert evidence['rejected_parent_observation']['resource_relevance'] == 'NOT_EVALUATED'
    assert all(evidence[key] == value for key, value in producer_evidence[0].items())
