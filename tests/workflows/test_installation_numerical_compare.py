"""Numerical comparisons isolate sampling, integration and mesh changes."""
import numpy as np
import pytest

from digitalmodel.workflows.installation_numerical_compare import (
    compare_arrays, validate_models,
)


def signals(step):
    time = np.arange(0, 4 + step / 2, step)
    return {'time': time, 'wave_elevation': time * 2,
            'load': np.ones(len(time)), 'chord': time / 10}


CHANNELS = {
    'wave_elevation': {'object': 'Environment', 'variable': 'Elevation', 'units': 'm'},
    'load': {'object': 'Rope', 'variable': 'Effective tension', 'position': 'End B', 'units': 'kN'},
    'chord': {'object': 'Rope', 'variable': 'unstretched_length_minus_span_m', 'units': 'm'},
}


def test_logging_common_identity_keeps_new_native_peak():
    parent, candidate = signals(.1), signals(.05)
    candidate['load'][3] = 9
    result = compare_arrays(parent, candidate, CHANNELS, kind='logging')
    assert result['status'] == 'VERIFIED_DIAGNOSTIC'
    assert result['channels']['load']['candidate_native']['maximum'] == 9
    assert result['channels']['load']['candidate_common']['maximum'] == 1


def test_logging_mismatch_blocks_but_time_difference_is_reported():
    parent, candidate = signals(.1), signals(.05)
    candidate['load'][4] = 2
    assert compare_arrays(parent, candidate, CHANNELS, kind='logging')['status'] == 'BLOCKED'
    assert compare_arrays(parent, candidate, CHANNELS, kind='time')['status'] == 'VERIFIED_DIAGNOSTIC'
    assert not compare_arrays(parent, candidate, CHANNELS, kind='time')['dependent_stage_release']


def test_float32_gate_uses_stored_precision():
    parent, candidate = signals(.1), signals(.05)
    candidate['load'] += 1e-10
    assert compare_arrays(parent, candidate, CHANNELS, kind='logging')['logging_load_identity']


@pytest.mark.parametrize('defect', ['offset', 'non_nested', 'nan', 'coverage', 'wave'])
def test_incompatible_arrays_fail_closed(defect):
    parent, candidate = signals(.1), signals(.05)
    if defect == 'offset':
        candidate['time'] += .01
    elif defect == 'non_nested':
        candidate = signals(.04)
    elif defect == 'nan':
        candidate['load'][0] = np.nan
    elif defect == 'coverage':
        candidate.pop('chord')
    else:
        candidate['wave_elevation'][2] += .1
    with pytest.raises(ValueError):
        compare_arrays(parent, candidate, CHANNELS, kind='time')


def test_zero_plateau_and_censored_events_remain_distinct():
    parent, candidate = signals(.1), signals(.05)
    parent['load'][3:5] = 0
    candidate['load'][6:10] = 0
    candidate['load'][-2:] = -1
    result = compare_arrays(parent, candidate, CHANNELS, kind='time')
    events = result['channels']['load']['candidate_native']['events']
    assert events['unfiltered']['governing']['classification'] == 'nonpositive_without_negative_samples'
    assert events['censored']['right'] == 1
    assert events['fixed_duration']['qualifying_count'] == 1


def test_unfiltered_retains_truncated_peak_with_censor_flag():
    a, b = signals(.1), signals(.05)
    b['load'][-10:-6] = 0
    result = compare_arrays(a, b, CHANNELS, kind='time')
    events = result['channels']['load']['candidate_native']['events']
    assert events['unfiltered']['governing']['retension_window_censored']
    assert events['fixed_duration']['governing'] is None


def model():
    return {'General': {'TargetLogSampleInterval': .1, 'ImplicitConstantTimeStep': .05,
                        'ImplicitUseVariableTimeStep': False, 'LogPrecision': 'Single'},
            'Lines': [{'Name': 'Rope', 'LineType, Length, TargetSegmentLength': [['wire', 20, 1.2]]}],
            'Environment': {'WaveHs': 1}}


def test_exact_allowed_model_changes_only():
    a, b = model(), model()
    b['General']['TargetLogSampleInterval'] = .05
    validate_models(a, b, kind='logging', expected_changes={'General.TargetLogSampleInterval': .05})
    b['Environment']['WaveHs'] = 2
    with pytest.raises(ValueError):
        validate_models(a, b, kind='logging', expected_changes={'General.TargetLogSampleInterval': .05})


def test_wrong_declared_value_or_physics_override_rejected():
    a, b = model(), model()
    with pytest.raises(ValueError):
        validate_models(a, b, kind='logging', expected_changes={'General.TargetLogSampleInterval': .05})
    with pytest.raises(ValueError):
        validate_models(a, b, kind='time', expected_changes={'Environment.WaveHs': 1})


def test_mesh_length_preservation():
    a, b = model(), model()
    b['Lines'][0]['LineType, Length, TargetSegmentLength'][0][2] = .6
    validate_models(a, b, kind='mesh', expected_changes={'Lines.Rope.TargetSegmentLength': [.6]})
    b['Lines'][0]['LineType, Length, TargetSegmentLength'][0][1] = 21
    with pytest.raises(ValueError):
        validate_models(a, b, kind='mesh', expected_changes={'Lines.Rope.TargetSegmentLength': [.6]})


def test_mesh_reordered_packed_header_supported():
    a, b = model(), model()
    b['Lines'][0].pop('LineType, Length, TargetSegmentLength')
    b['Lines'][0]['TargetSegmentLength, LineType, Length'] = [[.6, 'wire', 20]]
    validate_models(a, b, kind='mesh', expected_changes={'Lines.Rope.TargetSegmentLength': [.6]})


def test_event_filter_reports_exclusions_and_ranking_criterion():
    a, b = signals(.1), signals(.05)
    b['load'][6:10] = 0
    b['load'][-4:] = -1
    result = compare_arrays(a, b, CHANNELS, kind='time')
    entry = result['channels']['load']['candidate_native']['events']['fixed_duration']
    assert entry['excluded_censored_count'] == 1
    assert entry['ranking_criterion'] == 'maximum_retension_peak_kN'


@pytest.mark.parametrize('defect', [None, 'plan_hash', 'run_model', 'prepared_model', 'case_id', 'mapping'])
def test_plan_bound_pair(tmp_path, defect):
    import json
    from hashlib import sha256
    from digitalmodel.workflows.installation_numerical_compare_plan import compare_planned_pair
    a, b = tmp_path/'parent', tmp_path/'candidate'
    ar, am = retained_run(a, .1)
    br, bm = retained_run(b, .05)
    h = lambda p: sha256(p.read_bytes()).hexdigest()
    generation = {'status': 'native_verified_not_run', 'case_id': 'anchor',
                  'model_sha256': h(a/'source/model.yml'), 'request_sha256': h(a/'request.yml')}
    (a/'source/generation.json').write_text(json.dumps(generation))
    (a/'source/request.yml').write_bytes((a/'request.yml').read_bytes())
    row = {'id': 'logging', 'source_case': 'anchor', 'compare_to': 'anchor',
           'changes': {'General.TargetLogSampleInterval': .05}}
    anchor = {'model_path': str(a/'source/model.yml'), 'model_sha256': generation['model_sha256'],
              'generation_sha256': h(a/'source/generation.json')}
    plan = {'source_anchors': {'anchor': anchor}, 'cases': [row]}
    pp = tmp_path/'plan.json'
    pp.write_text(json.dumps(plan))
    (b/'source/request.yml').write_bytes((b/'request.yml').read_bytes())
    preparation = {'status': 'native_verified_not_run', 'plan_sha256': h(pp),
                   'source_anchors': plan['source_anchors'], 'cases': [dict(row,
                   model_path=str(b/'source/model.yml'), model_sha256=h(b/'source/model.yml'),
                   request_sha256=h(b/'request.yml'), status='native_verified_not_run')]}
    if defect == 'mapping':
        preparation['cases'][0]['compare_to'] = 'wrong'
    mp = tmp_path/'preparation.json'
    mp.write_text(json.dumps(preparation))
    cfg = dict(plan_path=str(pp), plan_sha256=h(pp), preparation_manifest=str(mp), preparation_sha256=h(mp),
               case_id='logging', parent=str(a), candidate=str(b), parent_receipt_sha256=ar,
               candidate_receipt_sha256=br, parent_metadata_sha256=am, candidate_metadata_sha256=bm)
    if defect == 'plan_hash':
        cfg['plan_sha256'] = '0'*64
    elif defect == 'case_id':
        cfg['case_id'] = 'wrong'
    elif defect == 'run_model':
        r = json.loads((b/'run.json').read_bytes()); r['model_sha256'] = 'f'*64
        (b/'run.json').write_text(json.dumps(r)); cfg['candidate_receipt_sha256'] = h(b/'run.json')
    elif defect == 'prepared_model':
        (b/'source/model.yml').write_text('changed')
    if defect:
        with pytest.raises(ValueError):
            compare_planned_pair(cfg)
    else:
        assert compare_planned_pair(cfg)['planned_case_id'] == 'logging'


def test_cumulative_plan_derives_immediate_time_change():
    from digitalmodel.workflows.installation_numerical_compare_plan import _selected
    plan = {'source_anchors': {'anchor': {}}}
    rows = {'log': {'id': 'log', 'source_case': 'anchor', 'compare_to': 'anchor',
                    'changes': {'General.TargetLogSampleInterval': .05}},
            'time': {'id': 'time', 'source_case': 'anchor', 'compare_to': 'log',
                     'changes': {'General.TargetLogSampleInterval': .05, 'General.ImplicitConstantTimeStep': .025}}}
    _, kind, changes = _selected(plan, rows, 'time')
    assert kind == 'time'
    assert changes == {'General.ImplicitConstantTimeStep': .025}
    rows['log']['compare_to'] = 'time'
    with pytest.raises(ValueError, match='Cyclic'):
        _selected(plan, rows, 'time')


def test_cli_requires_external_config_hash(tmp_path, monkeypatch):
    import sys
    from digitalmodel.workflows.installation_numerical_compare import main
    config = tmp_path/'config.json'
    config.write_text('{}')
    output = tmp_path/'output.json'
    monkeypatch.setattr(sys, 'argv', ['compare', '--config', str(config), '--config-sha256', '0'*64,
                                    '--output', str(output)])
    with pytest.raises(ValueError, match='digest'):
        main()
    assert not output.exists()


def test_geometry_identity_also_blocks_logging():
    a, b = signals(.1), signals(.05)
    b['chord'][4] += .01
    result = compare_arrays(a, b, CHANNELS, kind='logging')
    assert result['logging_load_identity']
    assert not result['logging_response_identity']
    assert result['status'] == 'BLOCKED'


def test_sampled_gradient_is_reported_without_false_solver_gate():
    a, b = signals(.1), signals(.05)
    channels = dict(CHANNELS, rate={'object': 'Rope', 'variable': 'span_rate_m_per_s', 'units': 'm/s'})
    for arrays in (a, b):
        arrays['rate'] = np.gradient(arrays['time'] ** 3, arrays['time'])
    result = compare_arrays(a, b, channels, kind='logging')
    assert result['status'] == 'VERIFIED_DIAGNOSTIC'
    assert not result['logging_response_identity']
    assert result['logging_gate_identity']
    assert result['identity_exclusions'][0]['channel'] == 'rate'


@pytest.mark.parametrize('defect', ['solver', 'threads', 'interval', 'warnings', 'bad_warning'])
def test_rehashed_runtime_inconsistency_rejected(tmp_path, defect):
    import json
    from hashlib import sha256
    from digitalmodel.workflows.installation_numerical_compare import compare_pair
    a, b = tmp_path/'parent', tmp_path/'candidate'
    ar, am = retained_run(a, .1)
    _, bm = retained_run(b, .05)
    p = b/'run.json'
    receipt = json.loads(p.read_bytes())
    if defect == 'solver':
        receipt['solver']['resolved_version'] = 'other'
    elif defect == 'threads':
        receipt['batch']['native_thread_budget'][0]['observed'] = 2
    elif defect == 'interval':
        receipt['actual_logging_interval'] = .1
    elif defect == 'warnings':
        receipt.pop('warnings')
    else:
        receipt['warnings'] = [None]
    p.write_text(json.dumps(receipt))
    with pytest.raises(ValueError):
        compare_pair(a, b, parent_receipt_sha256=ar, candidate_receipt_sha256=sha256(p.read_bytes()).hexdigest(),
                     parent_metadata_sha256=am, candidate_metadata_sha256=bm, kind='logging',
                     expected_changes={'General.TargetLogSampleInterval': .05})


def retained_run(root, step):
    import json
    from hashlib import sha256
    import yaml
    from digitalmodel.workflows.installation_response_metrics import tension_event_metrics
    from digitalmodel.workflows.installation_trace_extract import profile_digest
    arrays = signals(step)
    arrays.pop('chord')
    arrays['profile_000'] = arrays.pop('load')
    profile = {'schema_version': 1, 'channels': [CHANNELS['load']], 'geometry_lines': []}
    root.mkdir()
    (root / 'source').mkdir()
    traces = root / 'installation_traces'
    traces.mkdir()
    m = model()
    m['General']['TargetLogSampleInterval'] = step
    (root / 'source/model.yml').write_text(yaml.safe_dump(m))
    digest = lambda p: sha256(p.read_bytes()).hexdigest()
    request = {'model': 'model.yml', 'model_sha256': digest(root/'source/model.yml'), 'solver_version': 'test',
               'extraction': {'period': [0., 4.], 'supplemental_profile': profile}}
    (root/'request.yml').write_text(yaml.safe_dump(request))
    receipt = {'status': 'completed', 'model_sha256': request['model_sha256'],
               'request_sha256': digest(root/'request.yml'), 'simulation_sha256': 'abc',
               'simulation_start': 0., 'simulation_stop': 4., 'actual_logging_interval': step,
               'solver_version': 'test', 'solver': {'resolved_version': 'test'}, 'warnings': [],
               'batch': {'native_thread_budget': [{'requested': 1, 'observed': 1}]}}
    (root/'run.json').write_text(json.dumps(receipt))
    np.savez(traces/'traces.npz', **arrays)
    channel = dict(CHANNELS['load'], events=tension_event_metrics(arrays['time'], arrays['profile_000'], units='kN'))
    metadata = {'solver': receipt['solver'],
                'channels': {'wave_elevation': CHANNELS['wave_elevation'], 'profile_000': channel},
                'trace_sha256': digest(traces/'traces.npz'), 'simulation_sha256': 'abc',
                'supplemental_profile': profile, 'supplemental_profile_sha256': profile_digest(profile),
                'request_sha256': receipt['request_sha256']}
    (traces/'metadata.json').write_text(json.dumps(metadata))
    return digest(root/'run.json'), digest(traces/'metadata.json')


@pytest.mark.parametrize('defect', [None, 'metadata', 'receipt', 'trace', 'model', 'request'])
def test_production_hash_chain(tmp_path, defect):
    from digitalmodel.workflows.installation_numerical_compare import compare_pair
    a, b = tmp_path/'parent', tmp_path/'candidate'
    ar, am = retained_run(a, .1)
    br, bm = retained_run(b, .05)
    paths = {'metadata': 'installation_traces/metadata.json', 'receipt': 'run.json',
             'trace': 'installation_traces/traces.npz', 'model': 'source/model.yml', 'request': 'request.yml'}
    if defect:
        p = b/paths[defect]
        p.write_bytes(p.read_bytes() + b' ')
    kwargs = dict(parent_receipt_sha256=ar, candidate_receipt_sha256=br,
                  parent_metadata_sha256=am, candidate_metadata_sha256=bm, kind='logging',
                  expected_changes={'General.TargetLogSampleInterval': .05})
    if defect:
        with pytest.raises(ValueError):
            compare_pair(a, b, **kwargs)
    else:
        assert compare_pair(a, b, **kwargs)['status'] == 'VERIFIED_DIAGNOSTIC'
