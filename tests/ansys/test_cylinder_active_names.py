"""Synthetic active-name joins; queries are injected, never live."""
from copy import deepcopy
import importlib

import pytest

from . import test_cylinder_absence_collection as fixtures


def adapter():
    return importlib.import_module('digitalmodel.ansys.cylinder_active_names').ActiveNameResolution()


def install(monkeypatch, returned=None):
    calls = []
    result = fixtures.resolution(returned if returned is not None else [
        fixtures.cim_row(820, 4, 'Secure System')])
    def query(pids):
        calls.append(pids)
        return deepcopy(result)
    monkeypatch.setattr(fixtures.module(), '_query_blank_names', query)
    return calls, result


def original():
    return [fixtures.initial(820, 4, '', '', '0')]


def test_one_bound_query_retains_originals_across_three_observations(monkeypatch):
    calls, query = install(monkeypatch)
    names = adapter()
    raw = original()
    for _ in range(3):
        normalized = names.observe(raw)
        assert normalized == [fixtures.initial(820, 4, 'Secure System', None, '0')]
    assert calls == [[820]] and raw == original()
    evidence = names.evidence()
    assert evidence['schema'] == 'active-name-resolution-1'
    assert evidence['original_inventories'] == [raw, raw, raw]
    assert evidence['query'] == query and evidence['completed_stages'] == 3
    evidence['query']['raw_rows'].clear()
    assert names.evidence()['query']['raw_rows']


def test_complete_names_do_not_query(monkeypatch):
    calls, _ = install(monkeypatch)
    names = adapter()
    raw = [fixtures.initial(820, 4, 'ordinary.exe', '', '0')]
    for _ in range(3):
        assert names.observe(raw)[0]['executable_path'] is None
    assert not calls and names.evidence()['query']['performed'] is False


@pytest.mark.parametrize('fault', ['parent', 'birth', 'missing', 'extra', 'duplicate', 'blank', 'time'])
def test_initial_join_faults_refuse_and_prevent_retry(monkeypatch, fault):
    rows = [fixtures.cim_row(820, 4, 'Secure System')]
    if fault == 'parent': rows[0]['ParentProcessId'] = 5
    elif fault == 'birth': rows[0]['CreationDate'] = '19700101000000.000002+000'
    elif fault == 'missing': rows = []
    elif fault == 'extra': rows.append(fixtures.cim_row(821, 4, 'extra'))
    elif fault == 'duplicate': rows.append(deepcopy(rows[0]))
    elif fault == 'blank': rows[0]['Name'] = ''
    else: rows[0]['CreationDate'] = '*'
    calls, query = install(monkeypatch, rows)
    names = adapter()
    with pytest.raises(ValueError): names.observe(original())
    assert names.evidence()['query'] == query
    assert names.evidence()['completed_stages'] == 0
    with pytest.raises(ValueError): names.observe(original())
    assert calls == [[820]]


@pytest.mark.parametrize('stage', [1, 2])
@pytest.mark.parametrize('fault', ['parent', 'birth', 'new_blank'])
def test_later_blank_identity_must_match_initial_resolution(monkeypatch, stage, fault):
    calls, _ = install(monkeypatch)
    names = adapter()
    for _ in range(stage): names.observe(original())
    changed = original()
    if fault == 'parent': changed[0]['parent_pid'] = 5
    elif fault == 'birth': changed[0]['creation_time'] = '2'
    else: changed.append(fixtures.initial(821, 4, '', None, '0'))
    with pytest.raises(ValueError): names.observe(changed)
    assert names.evidence()['completed_stages'] == stage
    assert names.evidence()['original_inventories'][-1] == changed
    assert calls == [[820]]


def test_no_delayed_query_when_initial_names_were_complete(monkeypatch):
    calls, _ = install(monkeypatch)
    names = adapter()
    names.observe([fixtures.initial(820, 4, 'ordinary.exe', None, '0')])
    with pytest.raises(ValueError): names.observe(original())
    assert not calls


def test_query_failure_retains_transport_and_never_retries(monkeypatch):
    codec = fixtures.module()
    transport = fixtures.resolution([])
    transport.update(return_code=None, stderr_base64='ZmFpbGVk')
    calls = []
    def fail(pids):
        calls.append(pids)
        raise codec.CollectionError('synthetic timeout', deepcopy(transport))
    monkeypatch.setattr(codec, '_query_blank_names', fail)
    names = adapter()
    with pytest.raises(ValueError): names.observe(original())
    assert names.evidence()['query'] == transport
    with pytest.raises(ValueError): names.observe(original())
    assert len(calls) == 1


def test_resolved_solver_name_remains_a_selection_seed(monkeypatch):
    install(monkeypatch, [fixtures.cim_row(820, 4, 'ANSYS261.exe')])
    from digitalmodel.ansys.cylinder_diagnostic_snapshot_v2 import _selection
    rows = adapter().observe(original())
    assert [r['pid'] for r in _selection(rows, {})] == [820]


def test_more_than_64_blanks_refuses_before_interpreter_lookup(monkeypatch):
    monkeypatch.setattr(fixtures.module(), '_powershell_identity',
                        lambda: pytest.fail('over-limit query attempted interpreter lookup'))
    names = adapter()
    with pytest.raises(ValueError, match='bounded PID'):
        names.observe([fixtures.initial(pid, 0, '', None, '0') for pid in range(65)])
    assert names.evidence()['query']['performed'] is False
    assert names.evidence()['completed_stages'] == 0


def test_interpreter_identity_failure_preserves_no_query_record(monkeypatch):
    def fail():
        raise ValueError('synthetic interpreter identity failure')
    monkeypatch.setattr(fixtures.module(), '_powershell_identity', fail)
    names = adapter()
    with pytest.raises(ValueError, match='interpreter identity'): names.observe(original())
    assert names.evidence()['schema'] == 'active-name-resolution-1'
    assert names.evidence()['query']['performed'] is False
    assert names.evidence()['original_inventories'] == [original()]


@pytest.mark.parametrize('timestamp,accepted', [('0.000000999999', True), ('0.000001', True), ('0.000001000001', False)])
@pytest.mark.parametrize('phase', [0, 1])
def test_exact_creation_tolerance_boundary(monkeypatch, timestamp, accepted, phase):
    _, query = install(monkeypatch)
    names = adapter()
    if phase:
        names.observe(original())
    raw = original()
    raw[0]['creation_time'] = timestamp
    if accepted:
        assert names.observe(raw)[0]['name'] == 'Secure System'
    else:
        with pytest.raises(ValueError, match='creation identity differs'):
            names.observe(raw)
        assert names.evidence()['original_inventories'][-1] == raw
        assert names.evidence()['query'] == query
