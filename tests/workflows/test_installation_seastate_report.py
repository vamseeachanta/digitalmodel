"""Sea-state report evidence and missing-criteria regression fixtures."""
import hashlib
import json

import numpy as np
import pytest

from digitalmodel.workflows.installation_seastate_report import generate_report


def _json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value), encoding='utf-8')


def _digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _fixture(tmp_path):
    cases = [{'hs_m': h / 4, 'tp_s': tp, 'seed': 7, 'change_sha256': 'b' * 64}
             for h in range(1, 13) for tp in range(4, 17)]
    matrix = tmp_path / 'matrix.json'
    _json(matrix, {'cases': cases, 'master_sha256': 'a' * 64})
    run, prep = tmp_path / 'run', tmp_path / 'prepared'
    prep.mkdir()
    (prep / 'model.yml').write_text('synthetic model fixture')
    _json(prep / 'generation.json', {'model_sha256': _digest(prep / 'model.yml'),
          'settings': {'hs_m': .25, 'tp_s': 4, 'seed': 7, 'duration_s': 2, 'buildup_s': 80},
          'wave_reference': {'WaveDirection': 180},
          'dependencies': {'master_sha256': 'a' * 64, 'change_sha256': 'b' * 64}})
    sim = run / 'batch_runs/sims/model.sim'
    sim.parent.mkdir(parents=True)
    sim.write_bytes(b'synthetic simulation fixture')
    _json(run / 'run.json', {'status': 'completed', 'model_sha256': _digest(prep / 'model.yml'),
                           'simulation_sha256': _digest(sim), 'simulation_stop': 2})
    traces = run / 'installation_traces'
    traces.mkdir()
    np.savez(traces / 'traces.npz', time=[0., 1., 2.], sling=[2., -2., 2.])
    _json(traces / 'metadata.json', {'simulation_sha256': _digest(sim),
          'trace_sha256': _digest(traces / 'traces.npz'), 'channels': {
              'sling': {'variable': 'Effective tension', 'units': 'kN', 'minimum': -2.,
                        'maximum': 2., 'events': {'low_tension': {'total_duration_s': 1.}}}}})
    return matrix, {0: {'run_dir': str(run), 'generation_file': str(prep / 'generation.json')}}


def test_complete_grid_preserves_missing_and_not_evaluated(tmp_path):
    matrix, mapping = _fixture(tmp_path)
    result = generate_report(matrix, mapping, tmp_path / 'report.html')
    assert len(result['cases']) == 156
    assert result['cases'][0]['status'] == 'NOT EVALUATED'
    assert result['cases'][0]['minimum_signed_tension_kN'] == -2
    assert result['cases'][0]['maximum_low_tension_duration_s'] == 1
    assert result['cases'][1]['status'] == 'MISSING'
    html = (tmp_path / 'report.html').read_text(encoding='utf-8')
    assert '156' in html and 'NOT EVALUATED' in html and 'snap' in html
    assert '180' in html and 'seed 7' in html
    assert json.loads((tmp_path / 'report.json').read_text()) == result


@pytest.mark.parametrize('fault', ['sim', 'trace', 'coordinate', 'dependency'])
def test_unverified_evidence_is_failed_not_accepted(tmp_path, fault):
    matrix, mapping = _fixture(tmp_path)
    if fault == 'sim':
        (tmp_path / 'run/batch_runs/sims/model.sim').write_bytes(b'tampered')
    elif fault == 'trace':
        (tmp_path / 'run/installation_traces/traces.npz').write_bytes(b'tampered')
    else:
        path = tmp_path / 'prepared/generation.json'
        data = json.loads(path.read_text())
        if fault == 'coordinate': data['settings']['tp_s'] = 5
        else: data['dependencies']['master_sha256'] = 'c' * 64
        _json(path, data)
    result = generate_report(matrix, mapping, tmp_path / 'report.html')
    assert result['cases'][0]['status'] == 'FAILED'
    assert result['cases'][0]['reason']


def test_failed_run_and_completed_without_traces_distinct(tmp_path):
    matrix, mapping = _fixture(tmp_path)
    receipt = tmp_path / 'run/run.json'
    data = json.loads(receipt.read_text())
    data['status'] = 'failed'
    _json(receipt, data)
    result = generate_report(matrix, mapping, tmp_path / 'failed.html')
    assert result['cases'][0]['status'] == 'FAILED'


def test_no_overwrite_existing_output(tmp_path):
    matrix, mapping = _fixture(tmp_path)
    output = tmp_path / 'report.html'
    output.write_text('preserve')
    with pytest.raises(FileExistsError):
        generate_report(matrix, mapping, output)
    assert output.read_text() == 'preserve'


def test_verified_solve_without_trace_metadata_is_not_evaluated(tmp_path):
    matrix, mapping = _fixture(tmp_path)
    metadata = tmp_path / 'run/installation_traces/metadata.json'
    metadata.rename(metadata.with_suffix('.retained.json'))
    result = generate_report(matrix, mapping, tmp_path / 'report.html')
    assert result['cases'][0]['status'] == 'NOT EVALUATED'
    assert 'extraction missing' in result['cases'][0]['reason']
    assert 'peak_tension_kN' not in result['cases'][0]


@pytest.mark.parametrize('status', ['started', 'preflight', 'solving', 'postprocessing'])
def test_active_run_is_running_without_claimed_metrics(tmp_path, status):
    matrix, mapping = _fixture(tmp_path)
    receipt = tmp_path / 'run/run.json'
    data = json.loads(receipt.read_text())
    data['status'] = status
    data.pop('simulation_sha256')
    _json(receipt, data)
    result = generate_report(matrix, mapping, tmp_path / 'report.html')
    assert result['cases'][0]['status'] == 'RUNNING'
    assert status in result['cases'][0]['reason']
    assert 'peak_tension_kN' not in result['cases'][0]
    assert result['grid']['rows'][0]['incomplete']
