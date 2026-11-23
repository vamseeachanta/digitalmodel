"""Synthetic oracle-wave benchmarks; future loads cannot enter fitting."""
import json

import numpy as np
import pytest

from digitalmodel.workflows.installation_wave_assisted import benchmark_wave_assisted


def _signals():
    wave = np.random.default_rng(14).normal(size=1201)
    load = 100 + 3 * wave + .7 * np.roll(wave, 2)
    return np.arange(len(wave)) * .5, wave, load


def test_future_load_perturbation_does_not_change_any_prediction():
    time, wave, load = _signals()
    changed = load.copy()
    changed[481:] += 1500
    before = benchmark_wave_assisted(time, wave, load, [480])
    after = benchmark_wave_assisted(time, wave, changed, [480])
    assert before['traces'][0]['predictions'] == after['traces'][0]['predictions']
    assert before['metrics'] != after['metrics']


def test_future_wave_perturbation_changes_oracle_only():
    time, wave, load = _signals()
    changed = wave.copy()
    changed[481:] += 2
    before = benchmark_wave_assisted(time, wave, load, [480])
    after = benchmark_wave_assisted(time, changed, load, [480])
    a, b = before['traces'][0]['predictions'], after['traces'][0]['predictions']
    assert a['oracle_wave_fir'] != b['oracle_wave_fir']
    for key in ('autoregression', 'history_mean', 'persistence'):
        assert a[key] == b[key]
    assert before['traces'][0]['wave_assisted_fit'] == after['traces'][0]['wave_assisted_fit']


def test_linear_fixture_is_recovered_with_explicit_oracle_label():
    time, wave, load = _signals()
    result = benchmark_wave_assisted(time, wave, load, [480, 720, 960], load_units='kN')
    assert result['metrics']['120']['oracle_wave_fir']['rmse'] < .001
    assert result['metrics']['120']['history_mean']['rmse'] > 1
    assert result['forecast_basis'] == 'PRESCRIBED FUTURE WAVES / ORACLE INPUT'
    assert result['operational_validation'] == 'NOT ESTABLISHED'
    assert result['load_units'] == 'kN'
    assert result['runtime_seconds'] >= 0
    assert list(result['origin_metrics']) == ['240', '360', '480']
    json.dumps(result, allow_nan=False)


def test_default_origins_are_fixed_seconds():
    time, wave, load = _signals()
    result = benchmark_wave_assisted(time, wave, load)
    assert [t['origin_time'] for t in result['traces']] == [240, 360, 480]


def test_constant_wave_history_has_explicit_mean_fallback():
    time, wave, load = _signals()
    wave[:481] = 1
    result = benchmark_wave_assisted(time, wave, load, [480])
    trace = result['traces'][0]
    assert trace['wave_assisted_fit']['status'] == 'constant_wave_history_mean_fallback'
    assert trace['predictions']['oracle_wave_fir'] == trace['predictions']['history_mean']


@pytest.mark.parametrize('fault', ['wave_nan', 'wave_length', 'irregular_time', 'short_history', 'future_coverage'])
def test_invalid_input_fails(fault):
    time, wave, load = _signals()
    origins = [480]
    if fault == 'wave_nan': wave[10] = np.nan
    if fault == 'wave_length': wave = wave[:-1]
    if fault == 'irregular_time': time[10] += .1
    if fault == 'short_history': origins = [10]
    if fault == 'future_coverage': origins = [1000]
    with pytest.raises(ValueError):
        benchmark_wave_assisted(time, wave, load, origins)
