"""Synthetic active-collector name evidence and existing admission boundaries."""
from copy import deepcopy

import pytest

from . import test_cylinder_active_population as population
from . import test_cylinder_absence_collection as names_fixture


def setup(tmp_path, monkeypatch, transform=None, resolved_name='Secure System'):
    def observed(stages):
        for stage in stages:
            row = population.extra(pid=820, parent=4, name='', created='0')
            row['executable_path'] = None
            stage.append(row)
        if transform:
            transform(stages)
    current, seed, stages = population.observations(tmp_path, monkeypatch, observed)
    calls = []
    receipt = names_fixture.resolution([names_fixture.cim_row(820, 4, resolved_name)])
    def query(pids):
        calls.append(pids)
        return deepcopy(receipt)
    monkeypatch.setattr(names_fixture.module(), '_query_blank_names', query)
    return current, seed, stages, calls, receipt


def test_active_collector_retains_raw_and_normalized_names(tmp_path, monkeypatch):
    current, seed, stages, calls, receipt = setup(tmp_path, monkeypatch)
    result = current.collect_v2(discovery_seed=seed)
    assert calls == [[820]]
    evidence = result['population_check']['name_resolution']
    assert evidence['original_inventories'] == stages
    assert evidence['query'] == receipt and evidence['completed_stages'] == 3
    assert all(rows[-1]['name'] == 'Secure System' for rows in result['observed_inventories'])
    assert result['initial_inventory'][-1]['executable_path'] is None
    assert result['coverage']['enumerated_count'] == 3
    assert result['coverage']['selected_count'] == 2


@pytest.mark.parametrize('phase', [1, 2])
def test_new_blank_identity_refuses_with_original_query(tmp_path, monkeypatch, phase):
    current, seed, stages, calls, receipt = setup(tmp_path, monkeypatch,
        lambda stages: stages[phase].append(population.extra(pid=821, name='')))
    with pytest.raises(population.PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    evidence = caught.value.evidence
    assert evidence['failed_stage'] == ('A', 'B', 'C')[phase]
    assert len(evidence['completed_inventories']) == phase
    assert evidence['rejected_inventory'] == stages[phase]
    assert evidence['name_resolution']['query'] == receipt
    assert evidence['name_resolution']['completed_stages'] == phase
    assert calls == [[820]]


def test_nonblank_name_change_still_refuses(tmp_path, monkeypatch):
    current, seed, _, _, _ = setup(tmp_path, monkeypatch,
        lambda stages: stages[2][-1].update(name='different.exe'))
    with pytest.raises(population.PopulationError, match='identity changed'):
        current.collect_v2(discovery_seed=seed)


def test_resolved_solver_is_sent_to_existing_detailed_checks(tmp_path, monkeypatch):
    current, seed, _, _, _ = setup(tmp_path, monkeypatch, resolved_name='ANSYS261.exe')
    selected = []
    def detailed(rows, *args):
        selected.extend(row['pid'] for row in rows)
        raise ValueError('synthetic inaccessible selected process')
    monkeypatch.setattr(current, '_details', detailed)
    with pytest.raises(population.PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert 820 in selected and caught.value.evidence['failed_stage'] == 'detail_reads'


def test_query_evidence_survives_later_parent_refusal(tmp_path, monkeypatch):
    from digitalmodel.ansys.cylinder_parent_map import ParentMapChangeError
    current, seed, stages, calls, receipt = setup(tmp_path, monkeypatch)
    observations = []
    def enumerate_rows():
        observations.append(1)
        if len(observations) == 2:
            raise ParentMapChangeError({'rejected_parent_observation': {'status': 'REFUSED'}})
        return stages[0]
    monkeypatch.setattr(current, '_enumerate', enumerate_rows)
    with pytest.raises(population.PopulationError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['name_resolution']['query'] == receipt
    assert caught.value.evidence['rejected_parent_observation']['status'] == 'REFUSED'
    assert caught.value.evidence['failed_stage'] == 'B' and calls == [[820]]


def test_added_incomplete_query_helper_receives_no_exemption(tmp_path, monkeypatch):
    def added(stages):
        for stage in stages[1:]:
            row = population.extra(pid=821, name='WmiPrvSE.exe')
            row['executable_path'] = None
            stage.append(row)
    current, seed, _, _, receipt = setup(tmp_path, monkeypatch, added)
    with pytest.raises(population.PopulationError, match='not complete') as caught:
        current.collect_v2(discovery_seed=seed)
    evidence = caught.value.evidence
    assert evidence['events'][-1]['disposition'] == 'REFUSED_INCOMPLETE'
    assert evidence['name_resolution']['query'] == receipt
    assert evidence['name_resolution']['completed_stages'] == len(evidence['completed_inventories']) == 3


@pytest.mark.parametrize('refused', [False, True])
def test_collected_evidence_is_canonicalizable(tmp_path, monkeypatch, refused):
    from digitalmodel.ansys.analysis_records import canonical_bytes
    transform = (lambda stages: stages[1].append(population.extra(pid=821, name=''))) if refused else None
    current, seed, _, _, _ = setup(tmp_path, monkeypatch, transform)
    if refused:
        with pytest.raises(population.PopulationError) as caught:
            current.collect_v2(discovery_seed=seed)
        record = caught.value.evidence
    else:
        record = current.collect_v2(discovery_seed=seed)
    assert canonical_bytes(record)


def test_collect_to_conditional_classifier(monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_snapshot_v2 as current
    from digitalmodel.ansys.analysis_records import canonical_bytes
    from . import test_cylinder_process_inventory_v2 as inventory
    snapshot, binding = inventory.evidence.__wrapped__()
    fields = ('pid', 'parent_pid', 'name', 'creation_time', 'executable_path')
    raw = [{k: row[k] for k in fields} for row in snapshot['rows']]
    raw.append(dict(pid=820, parent_pid=0, name='', creation_time='0', executable_path=None))
    receipt = names_fixture.resolution([names_fixture.cim_row(820, 0, 'Secure System')])
    monkeypatch.setattr(names_fixture.module(), '_query_blank_names', lambda _: receipt)
    monkeypatch.setattr(current, '_enumerate', lambda: deepcopy(raw))
    monkeypatch.setattr(current.time, 'time', lambda: 100)
    monkeypatch.setattr(current.socket, 'gethostname', lambda: 'synthetic')
    monkeypatch.setattr(current, '_details', lambda *args: (
        {r['pid']: deepcopy(r) for r in snapshot['rows']}, {}, {}))
    monkeypatch.setattr(current, '_forwarders', lambda *args: deepcopy(snapshot['forwarders']))
    monkeypatch.setattr(current, '_stability', lambda *args: None)
    result = current.collect_v2(binding=binding)
    assert canonical_bytes(result)
    classified = inventory.classify(result, binding)
    assert classified['status'] == 'CLEAR'
    assert classified['evidence_scope'] == 'conditional_structural_classification'


def test_selected_missing_image_never_grants_clearance(tmp_path, monkeypatch):
    image = ''
    current, seed, stages, _, _ = setup(tmp_path, monkeypatch)
    for rows in stages:
        rows[1]['executable_path'] = image
    monkeypatch.setattr(current.psutil.Process(2), 'exe', lambda: image)
    result = current.collect_v2(discovery_seed=seed)
    assert result['coverage']['selected_details_complete'] is False
    assert any(row['pid'] == 2 for row in result['errors'])
    assert not any(row['pid'] == 2 for row in result['rows'])
