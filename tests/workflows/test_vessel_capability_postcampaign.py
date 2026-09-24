import json
from hashlib import sha256

import pytest

from digitalmodel.workflows import vessel_capability_postcampaign as workflow


def fixture_config(tmp_path):
    matrix = tmp_path / 'matrix.json'
    matrix.write_text('{"cases": []}')
    digest = sha256(matrix.read_bytes()).hexdigest()
    source = tmp_path / 'prior.json'
    source.write_text(json.dumps(dict(matrix_sha256=digest, created_utc='prior',
                                      design_basis={'dry_mass_t': 5.97})))
    campaign = tmp_path / 'campaign.json'
    campaign.write_text(json.dumps(dict(status='selected_cases_complete',
                                        matrix_sha256=digest, cases=[])))
    return dict(source_report=str(source), source_report_sha256=sha256(source.read_bytes()).hexdigest(),
                campaign=str(campaign), matrix=str(matrix), output=str(tmp_path / 'new.html'),
                pid=100, create_time=20.0, poll_seconds=1, wait_timeout=5)


def fake_report(campaign, matrix, output, basis):
    result = dict(counts={'MISSING': 1}, design_basis=basis,
                  campaign_sha256=sha256(campaign.read_bytes()).hexdigest())
    output.write_text('report')
    output.with_suffix('.json').write_text(json.dumps(result))
    return result


def test_completed_wait_preserves_basis_and_records_partial(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    monkeypatch.setattr(workflow, 'generate_report', fake_report)
    receipt = workflow.run(config)
    assert receipt['status'] == 'PARTIAL'
    assert receipt['source_snapshot_utc'] == 'prior'
    assert json.loads((tmp_path / 'new.json').read_text())['design_basis']['dry_mass_t'] == 5.97
    assert (tmp_path / 'new.campaign-snapshot.json').exists()


@pytest.mark.parametrize('change,match', [('lock', 'lock'), ('running', 'terminal'),
                                        ('row', 'RUNNING'), ('pid', 'identity'),
                                        ('source', 'digest')])
def test_guards_fail_without_report(monkeypatch, tmp_path, change, match):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    if change == 'lock': (tmp_path / 'campaign.lock').touch()
    if change in ('running', 'row'):
        path = tmp_path / 'campaign.json'
        data = json.loads(path.read_text())
        if change == 'running': data['status'] = 'running'
        else: data['cases'] = [{'status': 'RUNNING'}]
        path.write_text(json.dumps(data))
    if change == 'pid': monkeypatch.setattr(workflow, '_process_identity', lambda pid: 21)
    if change == 'source': (tmp_path / 'prior.json').write_text('{}')
    with pytest.raises(ValueError, match=match): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()
    assert json.loads((tmp_path / 'new.completion.json').read_text())['status'] == 'FAILED'


def test_timeout(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: 20)
    ticks = iter([0, 6])
    monkeypatch.setattr(workflow.time, 'monotonic', lambda: next(ticks))
    with pytest.raises(TimeoutError): workflow.run(config)


def test_changed_campaign_during_generation_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(*args):
        result = fake_report(*args)
        (tmp_path / 'campaign.json').write_text('{}')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError, match='changed'): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


@pytest.mark.parametrize('key,value', [('poll_seconds', 31), ('wait_timeout', float('inf')),
                                      ('pid', 0), ('create_time', -1)])
def test_invalid_config(tmp_path, key, value):
    config = fixture_config(tmp_path)
    config[key] = value
    with pytest.raises(ValueError): workflow.run(config)


def test_real_generator_keeps_missing_grid_rows(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    matrix = tmp_path / 'matrix.json'
    matrix.write_text(json.dumps({'cases': [dict(hs_m=1, tp_s=8, seed=1)]}))
    digest = sha256(matrix.read_bytes()).hexdigest()
    for name in ('prior.json', 'campaign.json'):
        path = tmp_path / name
        content = json.loads(path.read_text())
        content['matrix_sha256'] = digest
        path.write_text(json.dumps(content))
    config['source_report_sha256'] = sha256((tmp_path / 'prior.json').read_bytes()).hexdigest()
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    receipt = workflow.run(config)
    assert receipt['status'] == 'PARTIAL'
    assert receipt['counts'] == {'MISSING': 1}
    assert json.loads((tmp_path / 'new.json').read_text())['cases'][0]['status'] == 'MISSING'


def test_wait_checks_identity_until_exit(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    identities = iter([20, 20, None])
    sleeps = []
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: next(identities))
    monkeypatch.setattr(workflow.time, 'sleep', sleeps.append)
    workflow._wait(config)
    assert sleeps == [1, 1]


@pytest.mark.parametrize('target', ['prior.json', 'matrix.json', 'campaign.lock'])
def test_postgeneration_input_guard(monkeypatch, tmp_path, target):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(*args):
        result = fake_report(*args)
        (tmp_path / target).write_text('{}')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


def test_existing_receipt_refuses_without_overwrite(tmp_path):
    config = fixture_config(tmp_path)
    receipt = tmp_path / 'new.completion.json'
    receipt.write_text('preserve')
    with pytest.raises(FileExistsError): workflow.run(config)
    assert receipt.read_text() == 'preserve'


def test_mutated_snapshot_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(campaign, *args):
        result = fake_report(campaign, *args)
        campaign.write_text('{}')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


def test_report_campaign_hash_mismatch_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def corrupt(*args):
        result = fake_report(*args)
        result['campaign_sha256'] = 'incorrect'
        return result
    monkeypatch.setattr(workflow, 'generate_report', corrupt)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


def test_code_mutation_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    source = tmp_path / 'dependency.py'
    source.write_text('original')
    monkeypatch.setattr(workflow, '_code_paths', lambda: [source])
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(*args):
        result = fake_report(*args)
        source.write_text('changed')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


@pytest.mark.parametrize('counts,planned,expected', [({'VERIFIED': 0},0,False),({'VERIFIED': 1},2,False),({'VERIFIED': 2,'FAILED':0},2,True)])
def test_execution_complete_requires_full_nonempty_grid(counts, planned, expected):
    assert workflow._execution_complete(counts, planned) is expected


def test_case_evidence_hash_detects_trace_mutation(tmp_path):
    run = tmp_path / 'run'
    traces = run / 'installation_traces'
    traces.mkdir(parents=True)
    trace = traces / 'traces.npz'
    trace.write_bytes(b'original')
    snapshot = {'cases': [{'status': 'COMPLETED', 'run_dir': str(run),
                          'generation_file': str(tmp_path / 'generation.json')}]}
    before = workflow._evidence_hashes(snapshot)
    trace.write_bytes(b'changed')
    assert workflow._evidence_hashes(snapshot) != before
