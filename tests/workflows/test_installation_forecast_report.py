"""Verified trace input and honest forecast-report output contracts."""
import hashlib
import json

import numpy as np
import pytest

from digitalmodel.workflows.installation_forecast_report import generate_report


def fixture_run(tmp_path, duration=620):
    directory = tmp_path / 'installation_traces'
    directory.mkdir()
    times = np.arange(0., duration + .5, .5)
    path = directory / 'traces.npz'
    np.savez(path, time=times, wave_elevation=np.sin(times / 7),
             **{'Sling#5_end_B': 100 + np.sin(times / 7),
                'JumperLine_midpoint_bend': np.cos(times / 9),
                'JumperLine_Bend moment': np.cos(times / 8)})
    metadata = {'trace_sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
                'simulation_sha256': 'a' * 64, 'channels': {
                    'wave_elevation': {'units': 'm'}, 'Sling#5_end_B': {'units': 'kN'},
                    'JumperLine_midpoint_bend': {'units': 'kN.m', 'selection': 'fixed geometric midpoint'},
                    'JumperLine_Bend moment': {'units': 'kN.m',
                        'selection': 'whole-record governing arc; diagnostic only'}}}
    (directory / 'metadata.json').write_text(json.dumps(metadata))
    return directory


def test_report_labels_selection_origins_and_all_baselines(tmp_path):
    fixture_run(tmp_path)
    output = tmp_path / 'forecast.html'
    generate_report(tmp_path, output)
    html = output.read_text(encoding='utf-8')
    for token in ['SIMULATED', 'NOT EVALUATED', 'whole-record selection',
                  'persistence', 'history_mean', '360', '<svg', 'No advantage']:
        assert token in html
    assert 'No independent forecast evidence' in html
    assert html.count('JumperLine_Bend moment') == 1
    assert html.count('JumperLine_midpoint_bend') > 1
    assert 'cdn.plot.ly' not in html


def test_digest_mismatch_fails_before_output(tmp_path):
    directory = fixture_run(tmp_path)
    with (directory / 'traces.npz').open('ab') as stream:
        stream.write(b'changed')
    with pytest.raises(ValueError, match='digest'):
        generate_report(tmp_path, tmp_path / 'forecast.html')
    assert not (tmp_path / 'forecast.html').exists()


def test_short_run_reports_no_eligible_origins(tmp_path):
    fixture_run(tmp_path, duration=200)
    output = tmp_path / 'forecast.html'
    generate_report(tmp_path, output)
    assert 'No eligible fixed origins' in output.read_text(encoding='utf-8')


def test_nonuniform_trace_is_rejected(tmp_path):
    directory = fixture_run(tmp_path)
    path = directory / 'traces.npz'
    with np.load(path) as data:
        arrays = {key: data[key] for key in data.files}
    arrays['time'][3] += .1
    np.savez(path, **arrays)
    metadata = json.loads((directory / 'metadata.json').read_text())
    metadata['trace_sha256'] = hashlib.sha256(path.read_bytes()).hexdigest()
    (directory / 'metadata.json').write_text(json.dumps(metadata))
    with pytest.raises(ValueError, match='uniform'):
        generate_report(tmp_path, tmp_path / 'forecast.html')
