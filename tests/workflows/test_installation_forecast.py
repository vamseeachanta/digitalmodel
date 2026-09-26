"""Causality and honest baseline tests for scalar installation traces."""
import numpy as np
import pytest

from digitalmodel.workflows.installation_forecast import benchmark, forecast


def test_future_perturbation_cannot_change_predictions():
    times = np.arange(1000) * 0.5
    values = np.sin(times / 8)
    changed = values.copy()
    changed[601:] += 1000
    a = benchmark(times, values, [600])
    b = benchmark(times, changed, [600])
    assert a['traces'][0]['predictions'] == b['traces'][0]['predictions']
    assert a['metrics'] != b['metrics']


def test_constant_history_has_true_zero_error_and_undefined_correlation():
    result = benchmark(np.arange(600) * 0.5, np.full(600, 7.), [300])
    for lead in result['metrics'].values():
        for metrics in lead.values():
            assert metrics['rmse'] == metrics['bias'] == 0
            assert metrics['correlation'] is None


def test_fixed_origin_targets_are_future_only():
    values = np.sin(np.arange(800) / 10.)
    result = benchmark(np.arange(800) * .5, values, [400])
    trace = result['traces'][0]
    assert trace['times'][0] == 200.5
    assert trace['actual'] == values[401:641].tolist()
    assert trace['predictions']['persistence'] == [values[400]] * 240
    assert len(result['metrics']['30']['autoregression']) == 4


@pytest.mark.parametrize('fault', ['nonfinite', 'irregular', 'descending', 'length', 'future', 'fractional'])
def test_invalid_sampling_or_origins_fail(fault):
    times = np.arange(800, dtype=float) * .5
    values = np.ones(800)
    origins = [400]
    if fault == 'nonfinite': values[0] = np.nan
    if fault == 'irregular': times[5] += .1
    if fault == 'descending': times = times[::-1]
    if fault == 'length': values = values[:-1]
    if fault == 'future': origins = [790]
    if fault == 'fractional': origins = [400.5]
    with pytest.raises(ValueError):
        benchmark(times, values, origins)


def test_insufficient_history_fails_without_fabricated_fit():
    with pytest.raises(ValueError):
        forecast([1., 2., 3.], 10)


def test_no_superiority_claim_when_persistence_wins():
    values = np.sin(np.arange(800) / 10.)
    values[401:] = values[400]
    result = benchmark(np.arange(800) * .5, values, [400])
    assert result['metrics']['120']['persistence']['rmse'] == 0.
    assert result['metrics']['120']['autoregression']['rmse'] > 0.


def test_explosive_history_is_flagged_or_falls_back():
    prediction = forecast(1.03 ** np.arange(150), 1000, order=10)
    assert prediction['status'] == 'fallback_persistence_unstable'
    assert np.isfinite(prediction['values']).all()
