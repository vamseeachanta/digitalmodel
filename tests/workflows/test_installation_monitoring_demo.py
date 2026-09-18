"""A simulated replay must preserve the forecast information boundary."""
import numpy as np
import pytest

from digitalmodel.workflows.installation_monitoring_demo import (
    add_wave_preview, build_frames, resolve_channels, validate_metadata,
)


def traces():
    time = np.arange(1201) * .5
    return {'time': time, 'wave': np.sin(time), 'load': 10 + np.sin(time / 2)}


def channels():
    return [{'id': 'wave', 'label': 'Wave', 'units': 'm', 'assumed_limit': None},
            {'id': 'load', 'label': 'Load', 'units': 'kN', 'assumed_limit': 30}]


def test_future_changes_truth_and_error_but_never_prediction():
    before = traces()
    after = {k: v.copy() for k, v in before.items()}
    after['load'][721:] += 100
    a = build_frames(before, channels(), origins=(360,))[0]
    b = build_frames(after, channels(), origins=(360,))[0]
    assert a['channels'][1]['forecast'] == b['channels'][1]['forecast']
    assert a['channels'][1]['truth'] != b['channels'][1]['truth']
    assert a['channels'][1]['metrics'] != b['channels'][1]['metrics']
    assert max(a['channels'][1]['history']['times']) == 360
    assert min(a['channels'][1]['forecast']['times']) > 360
    assert max(a['channels'][1]['forecast']['times']) == 480


def test_missing_or_invalid_demo_evidence_rejected():
    with pytest.raises(ValueError):
        build_frames(traces(), channels(), origins=(500,))
    invalid = channels()
    invalid[1]['id'] = 'missing'
    with pytest.raises(ValueError):
        build_frames(traces(), invalid)


def test_no_learned_confidence_or_live_validation_claim():
    frame = build_frames(traces(), channels(), origins=(360,))[0]
    assert frame['forecast_horizon_s'] == 120
    assert frame['operational_validation'] == 'NOT ESTABLISHED'
    assert 'confidence' not in frame['channels'][0]
    assert set(frame['channels'][0]['metrics']) == {'autoregression', 'persistence', 'history_mean'}


def test_plot_limit_is_derived_from_same_criterion_as_envelope():
    criteria = {'force_conversion': {'kN_per_Te': 10}, 'checks': [
        {'id': 'rope', 'kind': 'maximum_tension', 'channels': ['load'],
         'limit': 3, 'value_units': 'Te'}]}
    config = [{'id': 'load', 'units': 'kN', 'criterion_id': 'rope'}]
    assert resolve_channels(criteria, config)[0]['assumed_limit'] == 30
    config[0]['assumed_limit'] = 25
    with pytest.raises(ValueError, match='limit'):
        resolve_channels(criteria, config)
    config[0].pop('assumed_limit')
    config[0]['id'] = 'wrong_end'
    with pytest.raises(ValueError, match='mapping'):
        resolve_channels(criteria, config)


def test_metadata_digest_and_fixed_channel_identity_are_verified():
    import hashlib
    import json
    metadata = {'channels': {'load': {'object': 'rope', 'variable': 'Effective tension',
                                    'position': 'End B', 'units': 'kN'}}}
    raw = json.dumps(metadata).encode()
    case = {'metadata_sha256': hashlib.sha256(raw).hexdigest(),
            'channels': metadata['channels']}
    validate_metadata(case, raw, metadata)
    with pytest.raises(ValueError, match='digest'):
        validate_metadata(case, raw + b' ', metadata)
    changed = json.loads(raw)
    changed['channels']['load']['position'] = 'End A'
    changed_raw = json.dumps(changed).encode()
    case['metadata_sha256'] = hashlib.sha256(changed_raw).hexdigest()
    with pytest.raises(ValueError, match='identity'):
        validate_metadata(case, changed_raw, changed)


def test_random_wave_preview_is_labelled_input_and_future_loads_do_not_fit():
    from copy import deepcopy
    a = traces()
    a['wave_elevation'] = a.pop('wave')
    spec = channels()
    spec[0]['id'] = 'wave_elevation'
    frames = build_frames(a, spec, origins=(360,))
    first = add_wave_preview(deepcopy(frames), a)
    changed = {k: v.copy() for k, v in a.items()}
    changed['load'][721:] += 100
    second = add_wave_preview(deepcopy(frames), changed)
    assert first[0]['channels'][1]['wave_preview'] == second[0]['channels'][1]['wave_preview']
    wave = first[0]['channels'][0]
    assert wave['wave_preview']['values'] == a['wave_elevation'][721:961].tolist()
    assert wave['wave_preview_metrics'] is None
    assert wave['preview_fit_status'] == 'supplied_simulated_wave_input'
    assert first[0]['channels'][1]['wave_preview_metrics'] != second[0]['channels'][1]['wave_preview_metrics']
