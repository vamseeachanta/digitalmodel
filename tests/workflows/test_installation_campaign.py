"""Serial campaign evidence/resume contracts without a solver."""
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.workflows import installation_campaign as campaign


@pytest.fixture
def study(tmp_path, monkeypatch):
    root = tmp_path / 'inputs'
    root.mkdir()
    (root / 'master.yml').write_text('master')
    master_hash = campaign.compute_hash(root / 'master.yml')
    cases = []
    for i in range(156):
        change = root / f'change_{i}.yml'
        change.write_text(f'case: {i}')
        cases.append({'hs_m': (i // 13 + 1)/4, 'tp_s': 4+i%13, 'seed': 20260915,
                      'change_file': change.name, 'change_sha256': campaign.compute_hash(change)})
    (root / 'matrix.json').write_text(json.dumps({'master_file': 'master.yml', 'master_sha256': master_hash, 'cases': cases}))
    calls = []
    def materialize(api, study_dir, index, output, **kwargs):
        output.mkdir(parents=True)
        (output / 'model.yml').write_text(f'model {index}')
        cfg = {'model': 'model.yml', 'model_sha256': campaign.compute_hash(output/'model.yml'), 'extraction': kwargs['extraction']}
        (output / 'request.yml').write_text(yaml.safe_dump(cfg))
        generation = {'model_sha256': cfg['model_sha256'], 'dependencies': {
            'master_sha256': master_hash, 'change_sha256': cases[index]['change_sha256']}}
        generation['settings'] = {k: cases[index][k] for k in ('hs_m','tp_s','seed')}
        (output / 'generation.json').write_text(json.dumps(generation))
    def reproduce(request, output):
        calls.append(output.name)
        output.mkdir(parents=True)
        cfg = yaml.safe_load(request.read_text())
        (output / 'source').mkdir()
        (output / 'source/model.yml').write_bytes((request.parent/'model.yml').read_bytes())
        (output / 'request.yml').write_bytes(request.read_bytes())
        (output / 'batch_runs/sims').mkdir(parents=True)
        sim = output / 'batch_runs/sims/model.sim'
        sim.write_text('simulation')
        receipt = {'status': 'completed', 'model_sha256': cfg['model_sha256'],
                   'simulation_sha256': campaign.compute_hash(sim), 'request_sha256': campaign.compute_hash(request),
                   'solver_version': '11.6c', 'extraction': {'status': 'complete'}}
        (output / 'run.json').write_text(json.dumps(receipt))
        return receipt
    monkeypatch.setattr(campaign, 'materialize_case', materialize)
    monkeypatch.setattr(campaign, 'reproduce', reproduce)
    def extract_traces(run):
        traces = run / 'installation_traces'
        traces.mkdir()
        (traces/'traces.npz').write_bytes(b'trace data')
        receipt = json.loads((run/'run.json').read_text())
        (traces/'metadata.json').write_text(json.dumps({'simulation_sha256': receipt['simulation_sha256'],
            'trace_sha256': campaign.compute_hash(traces/'traces.npz'), 'channels': {'wave': {}}}))
    monkeypatch.setattr(campaign, '_extract_traces', extract_traces)
    return root, tmp_path / 'campaign', calls


def test_only_explicit_cases_run_and_completed_resume_skips(study):
    root, output, calls = study
    first = campaign.run_campaign(root, output, [0, 95], extraction={'period': [0,600]}, api=object())
    assert len(first['cases']) == 156
    assert len(calls) == 2
    assert sum(r['status'] == 'MISSING' for r in first['cases']) == 154
    second = campaign.run_campaign(root, output, [0,95], extraction={'period':[0,600]}, api=object())
    assert len(calls) == 2
    assert second['cases'][95]['status'] == 'COMPLETED'
    assert second['engineering_acceptance'] == 'NOT EVALUATED'


def test_tampered_simulation_stops_and_preserves_run(study):
    root, output, calls = study
    campaign.run_campaign(root, output, [0], extraction={}, api=object())
    run = output / 'runs/case_000'
    before = (run / 'run.json').read_bytes()
    (run / 'batch_runs/sims/model.sim').write_text('tampered')
    result = campaign.run_campaign(root, output, [0,1], extraction={}, api=object())
    assert result['status'] == 'stopped'
    assert len(calls) == 1
    assert result['cases'][1]['status'] == 'MISSING'
    assert (run / 'run.json').read_bytes() == before


def test_solver_failure_stops_later_cases(study, monkeypatch):
    root, output, _ = study
    monkeypatch.setattr(campaign, 'reproduce', lambda *a: (_ for _ in ()).throw(RuntimeError('solver failed')))
    result = campaign.run_campaign(root, output, [0,1], extraction={}, api=object())
    assert result['cases'][0]['status'] == 'FAILED'
    assert result['cases'][1]['status'] == 'MISSING'


@pytest.mark.parametrize('indices', [[], [156], [-1], [0,0], [True]])
def test_invalid_selection_rejected_before_write(study, indices):
    root, output, _ = study
    with pytest.raises(ValueError):
        campaign.run_campaign(root, output, indices, extraction={}, api=object())
    assert not output.exists()


def test_verified_pilot_reuses_external_run_across_resume(study):
    root, output, calls = study
    campaign.run_campaign(root, output, [95], extraction={}, api=object())
    other = output.parent / 'with-pilot'
    args = {'pilot_run': output/'runs/case_095', 'pilot_generation': output/'prepared/case_095/generation.json'}
    result = campaign.run_campaign(root, other, [95], extraction={}, api=object(), **args)
    assert result['cases'][95]['disposition'] == 'verified_pilot'
    campaign.run_campaign(root, other, [95], extraction={}, api=object())
    assert len(calls) == 1


def test_master_tamper_blocks_without_changing_summary(study):
    root, output, _ = study
    campaign.run_campaign(root, output, [0], extraction={}, api=object())
    before = (output/'campaign.json').read_bytes()
    (root/'master.yml').write_text('tampered')
    with pytest.raises(ValueError):
        campaign.run_campaign(root, output, [0], extraction={}, api=object())
    assert (output/'campaign.json').read_bytes() == before


def test_supplemental_trace_hash_verified_before_skip(study):
    root, output, calls = study
    campaign.run_campaign(root, output, [0], extraction={}, api=object())
    trace = output/'runs/case_000/installation_traces/traces.npz'
    assert trace.exists()
    trace.write_bytes(b'tampered')
    result = campaign.run_campaign(root, output, [0,1], extraction={}, api=object())
    assert result['status'] == 'stopped'
    assert len(calls) == 1
    assert trace.read_bytes() == b'tampered'


@pytest.mark.parametrize('timeout', [0, -1, float('nan'), float('inf'), True, None, 'bad'])
def test_invalid_timeout_rejected_before_campaign_write(study, timeout):
    root, output, calls = study
    with pytest.raises(ValueError, match='timeout_seconds'):
        campaign.run_campaign(root, output, [0], extraction={}, api=object(), timeout_seconds=timeout)
    assert not output.exists()
    assert not calls


def test_extended_timeout_reaches_materializer_and_preserves_completed_request(study, monkeypatch):
    root, output, calls = study
    original = campaign.materialize_case
    values = []
    def recording(*args, **kwargs):
        values.append(kwargs['timeout_seconds'])
        return original(*args, **kwargs)
    monkeypatch.setattr(campaign, 'materialize_case', recording)
    campaign.run_campaign(root, output, [0], extraction={}, api=object(), timeout_seconds=14400)
    request = output / 'prepared/case_000/request.yml'
    before = request.read_bytes()
    campaign.run_campaign(root, output, [0], extraction={}, api=object(), timeout_seconds=28800)
    assert values == [14400]
    assert request.read_bytes() == before
    assert len(calls) == 1
