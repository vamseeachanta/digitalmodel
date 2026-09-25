"""Causal probability-of-exceedance forecast calibrated only on data before NOW."""
import numpy as np
import pytest

from digitalmodel.workflows.installation_forecast import exceedance_forecast


def trace(amplitude=10.0, seconds=600, dt=0.5, seed=3):
    times = np.arange(0, seconds + dt / 2, dt)
    rng = np.random.default_rng(seed)
    values = 100 + amplitude * np.sin(2 * np.pi * times / 10) + rng.normal(0, 1, len(times))
    return times, values


def origin(times, now):
    return int(np.flatnonzero(np.isclose(times, now))[0])


def test_future_samples_never_change_the_result():
    times, values = trace()
    index = origin(times, 360)
    first = exceedance_forecast(times, values, index, limit=108.0)
    changed = values.copy()
    changed[index + 1:] += 500
    second = exceedance_forecast(times, changed, index, limit=108.0)
    assert first == second
    assert first['calibration_last_index'] <= index


def test_probability_tracks_the_limit():
    times, values = trace()
    index = origin(times, 360)
    near = exceedance_forecast(times, values, index, limit=105.0)
    far = exceedance_forecast(times, values, index, limit=200.0)
    assert near['status'] == far['status'] == 'calibrated'
    assert near['window_probability'] > 0.9 and far['window_probability'] == 0.0
    assert near['alert'] is True and far['alert'] is False
    assert len(near['upper_band']['values']) == len(near['upper_band']['times']) == 240
    assert all(t > 360 for t in near['upper_band']['times'])


def test_short_history_is_reported_not_guessed():
    times, values = trace()
    result = exceedance_forecast(times, values, origin(times, 90), limit=105.0)
    assert result['status'] == 'insufficient_calibration'
    assert result['window_probability'] is None and result['alert'] is None


def test_scoring_uses_withheld_truth_only_after_prediction():
    times, values = trace()
    index = origin(times, 360)
    result = exceedance_forecast(times, values, index, limit=105.0, score=True)
    assert result['observed_exceedance'] is True
    assert result['outcome'] == 'hit'
    none = exceedance_forecast(times, values, index, limit=105.0)
    assert 'observed_exceedance' not in none


@pytest.mark.parametrize('kwargs', [dict(limit=float('nan')), dict(limit=105.0, quantile=1.5),
                                    dict(limit=105.0, alert_probability=0.0),
                                    dict(limit=105.0, stride=0)])
def test_invalid_settings_rejected(kwargs):
    times, values = trace()
    with pytest.raises(ValueError):
        exceedance_forecast(times, values, origin(times, 360), **kwargs)
