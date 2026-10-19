"""Expected-profile contract from issue 2138, using synthetic evidence only."""

from hashlib import sha256
import json

import numpy as np
import pytest
import yaml

from digitalmodel.workflows import installation_partial_report as report


def _digest(path):
    return sha256(path.read_bytes()).hexdigest()


def _write_json(path, value):
    path.write_text(json.dumps(value), encoding='utf-8')


@pytest.fixture
def evidence(tmp_path):
    """Hand calculation: [2, -2, 2] at [0, 1, 2] s crosses zero at
    0.5 and 1.5 s, giving 1 s nonpositive duration, min -2 and max 2 kN.
    All coordinates and samples are synthetic inputs, not acceptance limits.
    Profile schema version 1 follows installation_trace_extract._validate_profile.
    """
    profile = dict(schema_version=1, geometry_lines=[], channels=[
        dict(object='wire', variable='Effective tension', units='kN', position='End A'),
        dict(object='body', variable='X', units='m'),
    ])
    run = tmp_path / 'run'
    traces = run / 'installation_traces'
    traces.mkdir(parents=True)
    request = run / 'request.yml'
    request.write_text(yaml.safe_dump(dict(extraction=dict(supplemental_profile=profile))))
    model = tmp_path / 'model.yml'
    model.write_text('synthetic model')
    sim = run / 'batch_runs/sims/model.sim'
    sim.parent.mkdir(parents=True)
    sim.write_bytes(b'synthetic simulation; never loaded')
    case = dict(hs_m=1, tp_s=8, seed=1, change_sha256='change')
    matrix = dict(cases=[case], master_sha256='master')
    generation = tmp_path / 'generation.json'
    _write_json(generation, dict(model_sha256=_digest(model),
        settings=dict(hs_m=1, tp_s=8, seed=1, duration_s=2),
        wave_reference=dict(WaveDirection=0),
        dependencies=dict(master_sha256='master', change_sha256='change')))
    receipt = dict(status='completed', model_sha256=_digest(model),
                   simulation_sha256=_digest(sim), simulation_stop=2,
                   actual_logging_interval=1, request_sha256=_digest(request))
    _write_json(run / 'run.json', receipt)
    arrays = dict(time=[0., 1., 2.], wave_elevation=[0., 0., 0.],
                  profile_000=[2., -2., 2.], profile_001=[0., 1., 2.])
    channels = {f'profile_{i:03d}': dict(row, minimum=min(arrays[f'profile_{i:03d}']),
                maximum=max(arrays[f'profile_{i:03d}'])) for i, row in enumerate(profile['channels'])}
    channels['profile_000']['events'] = dict(low_tension=dict(total_duration_s=1.))
    channels['wave_elevation'] = dict(object='Environment', variable='Elevation',
                                       units='m', minimum=0., maximum=0.)
    metadata = dict(simulation_sha256=_digest(sim), channels=channels,
                    supplemental_profile=profile, request_sha256=_digest(request),
                    supplemental_profile_sha256=sha256(json.dumps(
                        profile, sort_keys=True, separators=(',', ':')).encode()).hexdigest())
    snapshot = dict(cases=[dict(index=0, status='COMPLETED', run_dir=str(run),
                                generation_file=str(generation), **case)])
    return run, snapshot, matrix, arrays, metadata


def _save(evidence):
    run, _, _, arrays, metadata = evidence
    path = run / 'installation_traces/traces.npz'
    np.savez(path, **arrays)
    metadata['trace_sha256'] = _digest(path)
    _write_json(path.with_name('metadata.json'), metadata)


@pytest.mark.parametrize('workers', [1, 3])
def test_valid_expected_profile_is_verified(evidence, workers):
    """Expected extrema and duration follow the fixture's hand calculation."""
    _save(evidence)
    _, snapshot, matrix, _, metadata = evidence
    row, = report.collect_cases(snapshot, matrix, workers=workers)
    assert row['status'] == 'VERIFIED'
    assert set(row['channels']) == set(metadata['channels'])
    assert row['minimum_signed_tension_kN'] == -2
    assert row['peak_tension_kN'] == 2
    assert row['maximum_low_tension_duration_s'] == 1


@pytest.mark.parametrize('defect,reason', [
    ('missing_channel', 'channel coverage'),
    ('missing_wave', 'wave channel'),
    ('profile_mismatch', 'differs from request'),
    ('missing_profile', 'differs from request'),
    ('profile_digest', 'provenance mismatch'),
    ('request_digest', 'provenance mismatch'),
    ('request_bytes', 'Request identity'),
    ('missing_request', 'request.yml'),
    ('missing_array', 'profile_001'),
    ('extra_array', 'array coverage'),
    ('trace_digest', 'Trace digest'),
    ('nonfinite', 'Invalid trace'),
    ('logging_interval', 'sample coverage'),
])
@pytest.mark.parametrize('workers', [1, 3])
def test_invalid_expected_profile_remains_failed(evidence, defect, reason, workers):
    """Expected rejection follows issue 2138 and the saved profile contract."""
    run, snapshot, matrix, arrays, metadata = evidence
    if defect in ('missing_channel', 'missing_wave'):
        key = 'profile_001' if defect == 'missing_channel' else 'wave_elevation'
        del metadata['channels'][key]
        del arrays[key]
    elif defect == 'profile_mismatch':
        # Separate saved profile from request bytes; preserve otherwise valid metadata.
        metadata['supplemental_profile']['channels'][1]['object'] = 'other'
    elif defect == 'missing_profile':
        del metadata['supplemental_profile']
    elif defect in ('profile_digest', 'request_digest'):
        key = 'supplemental_profile_sha256' if defect == 'profile_digest' else 'request_sha256'
        metadata[key] = 'altered'
    elif defect == 'request_bytes':
        (run / 'request.yml').write_text('altered request')
    elif defect == 'missing_request':
        (run / 'request.yml').rename(run / 'retained-request.yml')
    elif defect == 'missing_array':
        del arrays['profile_001']
    elif defect == 'extra_array':
        arrays['unexpected'] = [0., 0., 0.]
    elif defect == 'nonfinite':
        arrays['profile_001'][1] = float('nan')
    elif defect == 'logging_interval':
        path = run / 'run.json'
        receipt = json.loads(path.read_text())
        receipt['actual_logging_interval'] = 0.5  # Samples remain 1 s apart.
        _write_json(path, receipt)
    _save(evidence)
    if defect == 'trace_digest':
        path = run / 'installation_traces/traces.npz'
        path.write_bytes(path.read_bytes() + b'altered')
    row, = report.collect_cases(snapshot, matrix, workers=workers)
    assert row['status'] == 'FAILED'
    assert reason in row['reason']
    assert 'channels' not in row
    assert report.component_envelopes([row]) == []


def test_report_retains_failure_and_reads_back_sidecar(evidence, tmp_path):
    """One invalid case yields one failure, no channels and no demand envelope."""
    _, snapshot, matrix, arrays, metadata = evidence
    del arrays['profile_001']
    del metadata['channels']['profile_001']
    _save(evidence)
    matrix_path = tmp_path / 'matrix.json'
    _write_json(matrix_path, matrix)
    snapshot['matrix_sha256'] = _digest(matrix_path)
    campaign = tmp_path / 'campaign.json'
    _write_json(campaign, snapshot)
    output = tmp_path / 'report.html'
    result = report.generate_report(campaign, matrix_path, output)
    assert result['counts'] == {'FAILED': 1}
    assert result['envelopes'] == []
    assert 'channels' not in result['cases'][0]
    assert result['engineering_acceptance'] == 'NOT EVALUATED'
    assert json.loads(output.with_suffix('.json').read_text()) == result
    assert 'Evidence failures' in output.read_text()


@pytest.mark.parametrize('missing_geometry', [False, True])
def test_expected_geometry_coverage(evidence, missing_geometry):
    """All three named geometry fields are required by the profile contract."""
    run, snapshot, matrix, arrays, metadata = evidence
    profile = metadata['supplemental_profile']
    profile['geometry_lines'] = ['wire']
    request = run / 'request.yml'
    request.write_text(yaml.safe_dump(dict(extraction=dict(supplemental_profile=profile))))
    receipt = json.loads((run / 'run.json').read_text())
    receipt['request_sha256'] = metadata['request_sha256'] = _digest(request)
    _write_json(run / 'run.json', receipt)
    metadata['supplemental_profile_sha256'] = sha256(json.dumps(
        profile, sort_keys=True, separators=(',', ':')).encode()).hexdigest()
    fields = [('span_m', 'm'), ('unstretched_length_minus_span_m', 'm'),
              ('span_rate_m_per_s', 'm/s')]
    for field, units in fields:
        key = f'wire_{field}'
        arrays[key] = [0., 0., 0.]
        metadata['channels'][key] = dict(object='wire', variable=field,
                                         units=units, minimum=0., maximum=0.)
    if missing_geometry:
        del arrays['wire_span_m']
        del metadata['channels']['wire_span_m']
    _save(evidence)
    row, = report.collect_cases(snapshot, matrix)
    assert row['status'] == ('FAILED' if missing_geometry else 'VERIFIED')
    if missing_geometry:
        assert 'geometry coverage' in row['reason']


def test_trace_change_during_profile_check_is_rejected(evidence, monkeypatch):
    """A later coverage check cannot accept bytes with a different digest."""
    _save(evidence)
    _, snapshot, matrix, _, _ = evidence
    verify = report.verify_profile_arrays

    def change_after_check(path, receipt, metadata):
        verify(path, receipt, metadata)
        path.write_bytes(path.read_bytes() + b'changed')

    monkeypatch.setattr(report, 'verify_profile_arrays', change_after_check)
    row, = report.collect_cases(snapshot, matrix)
    assert row['status'] == 'FAILED'
    assert 'changed during profile verification' in row['reason']
    assert 'peak_tension_kN' not in row


def test_legacy_request_without_profile_remains_supported(evidence):
    """No supplemental contract is implied by an explicitly profile-free request."""
    run, snapshot, matrix, _, metadata = evidence
    (run / 'request.yml').write_text('extraction: {}')
    receipt = json.loads((run / 'run.json').read_text())
    receipt['request_sha256'] = _digest(run / 'request.yml')
    _write_json(run / 'run.json', receipt)
    for key in ('supplemental_profile', 'supplemental_profile_sha256', 'request_sha256'):
        del metadata[key]
    _save(evidence)
    assert report.collect_cases(snapshot, matrix)[0]['status'] == 'VERIFIED'
