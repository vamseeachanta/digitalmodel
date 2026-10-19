"""Profile coverage regressions with synthetic, independently specified evidence."""

from copy import deepcopy
from hashlib import sha256
import json

import numpy as np
import pytest

from digitalmodel.workflows import installation_partial_report as report


def _write_json(path, value):
    path.write_text(json.dumps(value), encoding='utf-8')


def _digest(path):
    return sha256(path.read_bytes()).hexdigest()


@pytest.fixture
def evidence(tmp_path):
    """Hand calculation: tension [2, -2, 2] at times [0, 1, 2].

    Linear zero crossings occur at 0.5 and 1.5 s, so nonpositive duration
    is 1 s; extrema are -2 and 2 kN. Other channels are constant zero,
    hence both extrema are zero. All coordinates, schema versions and sample
    intervals below are synthetic test inputs, not engineering criteria.
    Profile selectors follow installation_trace_extract._validate_profile.
    Digests are independently calculated from the fixture's serialized bytes.
    """
    run = tmp_path / 'run'
    traces = run / 'installation_traces'
    traces.mkdir(parents=True)
    prep = tmp_path / 'prepared'
    prep.mkdir()
    model = prep / 'model.yml'
    model.write_text('synthetic model')
    sim = run / 'batch_runs/sims/model.sim'
    sim.parent.mkdir(parents=True)
    sim.write_bytes(b'synthetic evidence; no solver execution')
    profile = dict(schema_version=1, channels=[
        dict(object='rig', variable='Effective tension', units='kN', position='End A'),
        dict(object='body', variable='Z', units='m')], geometry_lines=['rig'])
    request = run / 'request.yml'
    _write_json(request, {'extraction': {'supplemental_profile': profile}})
    receipt = dict(status='completed', model_sha256=_digest(model),
                   simulation_sha256=_digest(sim), simulation_stop=2,
                   actual_logging_interval=1, request_sha256=_digest(request))
    _write_json(run / 'run.json', receipt)
    case = dict(hs_m=1, tp_s=8, seed=1, change_sha256='synthetic-change')
    matrix = dict(master_sha256='synthetic-master', cases=[case])
    generation = prep / 'generation.json'
    _write_json(generation, dict(model_sha256=_digest(model),
        settings=dict(hs_m=1, tp_s=8, seed=1, duration_s=2),
        wave_reference={'WaveDirection': 0}, dependencies=dict(
            master_sha256=matrix['master_sha256'], change_sha256=case['change_sha256'])))
    channels = dict(
        wave_elevation=dict(object='Environment', variable='Elevation', units='m',
                            minimum=0, maximum=0),
        profile_000=dict(**profile['channels'][0], minimum=-2, maximum=2,
                         events={'low_tension': {'total_duration_s': 1}}),
        profile_001=dict(**profile['channels'][1], minimum=0, maximum=0))
    for field, units in [('span_m', 'm'), ('unstretched_length_minus_span_m', 'm'),
                         ('span_rate_m_per_s', 'm/s')]:
        channels['rig_' + field] = dict(object='rig', variable=field, units=units,
                                        minimum=0, maximum=0)
    arrays = {key: np.zeros(3) for key in channels}
    arrays.update(time=np.array([0., 1., 2.]), profile_000=np.array([2., -2., 2.]))
    np.savez(traces / 'traces.npz', **arrays)
    # Canonical JSON contract: installation_trace_extract.profile_digest.
    canonical = json.dumps(profile, sort_keys=True, separators=(',', ':')).encode()
    metadata = dict(channels=channels, simulation_sha256=_digest(sim),
                    trace_sha256=_digest(traces / 'traces.npz'),
                    supplemental_profile=deepcopy(profile),
                    supplemental_profile_sha256=sha256(canonical).hexdigest(),
                    request_sha256=receipt['request_sha256'])
    _write_json(traces / 'metadata.json', metadata)
    snapshot = {'cases': [dict(index=0, status='COMPLETED', run_dir=str(run),
                              generation_file=str(generation), **case)]}
    return snapshot, matrix, run, metadata, arrays


def _save_traces(run, metadata, arrays):
    traces = run / 'installation_traces'
    np.savez(traces / 'traces.npz', **arrays)
    metadata['trace_sha256'] = _digest(traces / 'traces.npz')
    _write_json(traces / 'metadata.json', metadata)


@pytest.mark.parametrize('workers', [1, 2])
def test_complete_profile_is_verified(evidence, workers):
    """Expected extrema and duration are calculated in the evidence fixture."""
    snapshot, matrix, _, metadata, _ = evidence
    row, = report.collect_cases(snapshot, matrix, workers=workers)
    assert row['status'] == 'VERIFIED'
    assert set(row['channels']) == set(metadata['channels'])
    assert row['minimum_signed_tension_kN'] == -2
    assert row['peak_tension_kN'] == 2
    assert row['maximum_low_tension_duration_s'] == 1


@pytest.mark.parametrize('defect,reason', [
    ('missing_metadata_channel', 'channel coverage'),
    ('missing_geometry', 'geometry coverage'),
    ('missing_wave', 'wave channel'),
    ('missing_profile', 'differs from request'),
    ('mismatched_profile', 'differs from request'),
    ('profile_digest', 'provenance mismatch'),
    ('metadata_request_digest', 'provenance mismatch'),
    ('request_bytes', 'Request identity mismatch'),
])
def test_profile_metadata_failure_is_retained_without_demands(evidence, defect, reason):
    snapshot, matrix, run, metadata, arrays = evidence
    if defect == 'missing_metadata_channel':
        metadata['channels'].pop('profile_001')
        arrays.pop('profile_001')
    elif defect == 'missing_geometry':
        metadata['channels'].pop('rig_span_m')
        arrays.pop('rig_span_m')
    elif defect == 'missing_wave':
        metadata['channels'].pop('wave_elevation')
        arrays.pop('wave_elevation')
    elif defect == 'missing_profile':
        metadata.pop('supplemental_profile')
    elif defect == 'mismatched_profile':
        metadata['supplemental_profile']['channels'][1]['object'] = 'different-body'
    elif defect == 'profile_digest':
        metadata['supplemental_profile_sha256'] = 'altered'
    elif defect == 'metadata_request_digest':
        metadata['request_sha256'] = 'altered'
    else:
        (run / 'request.yml').write_text('{}')
    _save_traces(run, metadata, arrays)
    row, = report.collect_cases(snapshot, matrix)
    assert row['status'] == 'FAILED'
    assert reason in row['reason']
    assert row['index'] == snapshot['cases'][0]['index']
    assert 'channels' not in row and 'peak_tension_kN' not in row
    assert report.component_envelopes([row]) == []


@pytest.mark.parametrize('defect,reason', [
    ('extra_array', 'array coverage'),
    ('sample_gap', 'uniformly spaced times'),
    ('nonmonotonic_time', 'increasing'),
    ('logging_interval', 'sample coverage'),
    ('missing_array', 'profile_001'),
    ('nonfinite_array', 'Invalid trace'),
    ('wrong_shape', 'Invalid trace'),
    ('trace_digest', 'Trace digest mismatch'),
])
def test_profile_array_failure_is_retained(evidence, defect, reason):
    """Defects violate exact keys, alignment, finiteness or the input 1 s interval."""
    snapshot, matrix, run, metadata, arrays = evidence
    if defect == 'extra_array':
        arrays['unrecorded'] = np.zeros_like(arrays['time'])
    elif defect == 'sample_gap':
        arrays['time'] = np.array([0., .5, 2.])
    elif defect == 'nonmonotonic_time':
        arrays['time'] = np.array([0., 2., 1., 2.])
        for key in metadata['channels']:
            arrays[key] = np.array([0., -2., 0., 2.]) if key == 'profile_000' else np.zeros(4)
    elif defect == 'missing_array':
        arrays.pop('profile_001')
    elif defect == 'nonfinite_array':
        arrays['profile_001'][1] = np.nan
    elif defect == 'wrong_shape':
        arrays['profile_001'] = np.zeros((3, 1))
    elif defect == 'logging_interval':
        receipt = json.loads((run / 'run.json').read_text())
        receipt['actual_logging_interval'] = .5
        _write_json(run / 'run.json', receipt)
    _save_traces(run, metadata, arrays)
    if defect == 'trace_digest':
        metadata['trace_sha256'] = 'altered'
        _write_json(run / 'installation_traces/metadata.json', metadata)
    row, = report.collect_cases(snapshot, matrix)
    assert row['status'] == 'FAILED'
    assert reason in row['reason']
    assert 'channels' not in row
    assert report.component_envelopes([row]) == []


def test_legacy_request_without_profile_remains_supported(evidence):
    snapshot, matrix, run, metadata, arrays = evidence
    request = run / 'request.yml'
    request.write_text('{}')
    receipt = json.loads((run / 'run.json').read_text())
    receipt['request_sha256'] = _digest(request)
    _write_json(run / 'run.json', receipt)
    for key in ('supplemental_profile', 'supplemental_profile_sha256', 'request_sha256'):
        metadata.pop(key)
    _save_traces(run, metadata, arrays)
    row, = report.collect_cases(snapshot, matrix)
    assert row['status'] == 'VERIFIED'


@pytest.mark.parametrize('field,value', [
    ('object', 'different-body'), ('variable', 'X'), ('units', 'mm'), ('position', 'End B'),
])
def test_metadata_selector_must_match_requested_selector(evidence, field, value):
    snapshot, matrix, run, metadata, arrays = evidence
    metadata['channels']['profile_001'][field] = value
    _save_traces(run, metadata, arrays)
    row, = report.collect_cases(snapshot, matrix)
    assert row['status'] == 'FAILED'
    assert 'channel coverage' in row['reason']


@pytest.mark.parametrize('workers', [1, 2])
def test_report_retains_failed_case_and_excludes_its_envelopes(evidence, tmp_path, workers):
    """One failed case has zero envelopes; its status survives JSON readback."""
    snapshot, matrix, run, metadata, arrays = evidence
    metadata['channels'].pop('profile_001')
    arrays.pop('profile_001')
    _save_traces(run, metadata, arrays)
    matrix_path = tmp_path / 'matrix.json'
    campaign = tmp_path / 'campaign.json'
    _write_json(matrix_path, matrix)
    snapshot['matrix_sha256'] = _digest(matrix_path)
    _write_json(campaign, snapshot)
    captured = campaign.read_bytes()
    output = tmp_path / 'report.html'
    summary = report.generate_report(campaign, matrix_path, output, workers=workers)
    assert campaign.read_bytes() == captured
    assert json.loads(output.with_suffix('.json').read_text()) == summary
    assert summary['counts'] == {'FAILED': 1}
    assert summary['envelopes'] == []
    row, = summary['cases']
    assert row['status'] == 'FAILED' and 'channel coverage' in row['reason']
    assert 'channels' not in row
    html = output.read_text()
    assert 'title="FAILED"' in html
    assert 'Engineering acceptance: NOT EVALUATED' in html
