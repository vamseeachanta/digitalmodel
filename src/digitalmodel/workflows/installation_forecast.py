"""Causal scalar-trace benchmark; solver predictions are not field validation.

Origins are integer sample indexes. All history through the origin is training;
each lead reports pooled error over samples from the next sample to that horizon.
Overlapping origins are descriptive comparisons, not independent experiments.
"""
from __future__ import annotations

import numpy as np


def _vector(values):
    array = np.asarray(values, dtype=float)
    if array.ndim != 1 or not len(array) or not np.isfinite(array).all():
        raise ValueError('A nonempty finite scalar time trace is required')
    return array


def _positive_integer(value, name):
    if isinstance(value, (bool, np.bool_)) or not isinstance(value, (int, np.integer)) or value < 1:
        raise ValueError(f'{name} must be a positive integer')
    return int(value)


def forecast(history, steps, order=40, ridge=1e-3):
    """Fit fixed ridge AR using history only; never accept future observations.

    Recursive standardized magnitudes above 20 trigger a labelled persistence
    fallback for the entire horizon. This is a numerical guard, not a load limit.
    """
    history = _vector(history)
    steps, order = _positive_integer(steps, 'steps'), _positive_integer(order, 'order')
    if not np.isfinite(ridge) or ridge <= 0 or len(history) < 3 * order:
        raise ValueError('Positive finite ridge and at least three AR orders of history required')
    mean, scale = float(np.mean(history)), float(np.std(history))
    result = {'order': order, 'ridge': float(ridge), 'training_samples': len(history),
              'status': 'fitted', 'stability_guard_standard_deviations': 20.}
    if scale == 0:
        return {**result, 'status': 'constant_history', 'values': [float(history[-1])] * steps}
    normalized = (history - mean) / scale
    windows = np.lib.stride_tricks.sliding_window_view(normalized, order + 1)
    design, target = windows[:, :-1], windows[:, -1]
    coefficients = np.linalg.solve(design.T @ design + ridge * np.eye(order), design.T @ target)
    state, predictions = normalized[-order:].copy(), []
    for _ in range(steps):
        value = float(state @ coefficients)
        if not np.isfinite(value) or abs(value) > 20:
            return {**result, 'status': 'fallback_persistence_unstable',
                    'values': [float(history[-1])] * steps}
        predictions.append(float(value * scale + mean))
        state[:-1], state[-1] = state[1:], value
    return {**result, 'values': predictions}


def _sampling(times, values, leads_seconds):
    times, values = _vector(times), _vector(values)
    if len(times) != len(values) or len(times) < 2:
        raise ValueError('Aligned times and values required')
    intervals = np.diff(times)
    dt = float(intervals[0])
    if dt <= 0 or not np.allclose(intervals, dt, rtol=1e-7, atol=1e-9):
        raise ValueError('Finite strictly increasing uniform sample times required')
    leads = _vector(leads_seconds)
    if np.any(leads <= 0) or len(set(leads)) != len(leads):
        raise ValueError('Unique positive lead durations required')
    steps = np.rint(leads / dt).astype(int)
    if np.any(steps < 1) or not np.allclose(steps * dt, leads, rtol=0, atol=1e-7):
        raise ValueError('Lead durations must be exact multiples of sampling interval')
    return times, values, dt, leads, steps


def _trace(times, values, origin, steps, order, ridge):
    history = values[:origin + 1]
    prediction = forecast(history, steps, order=order, ridge=ridge)
    return {'origin_index': origin, 'origin_time': float(times[origin]),
            'times': times[origin + 1:origin + 1 + steps].tolist(),
            'actual': values[origin + 1:origin + 1 + steps].tolist(),
            'fit': {k: v for k, v in prediction.items() if k != 'values'},
            'predictions': {'autoregression': prediction['values'],
                            'persistence': [float(history[-1])] * steps,
                            'history_mean': [float(history.mean())] * steps}}


def _metrics(traces, steps):
    actual = np.concatenate([trace['actual'][:steps] for trace in traces])
    result = {}
    for method in ('autoregression', 'persistence', 'history_mean'):
        predicted = np.concatenate([trace['predictions'][method][:steps] for trace in traces])
        errors = predicted - actual
        correlation = None
        if np.std(actual) > 0 and np.std(predicted) > 0:
            correlation = float(np.corrcoef(actual, predicted)[0, 1])
        result[method] = {'rmse': float(np.sqrt(np.mean(errors ** 2))),
                          'bias': float(np.mean(errors)), 'correlation': correlation,
                          'sample_count': len(actual)}
    return result


def exceedance_forecast(times, values, origin_index, limit, *, horizon_s=120.0, order=40, ridge=1e-3,
                        stride=10, quantile=0.95, alert_probability=0.2, min_calibration=10,
                        score=False):
    """Probability that ``limit`` is exceeded within the horizon after the origin, causally.

    The history-only AR forecast is back-tested at earlier origins whose complete horizon ends at
    or before the current origin. Each back-test contributes one joint error trajectory; the window
    probability is the fraction of those trajectories that, added to the current forecast, exceed
    the limit anywhere in the horizon. Back-test windows overlap and are not independent trials.
    Withheld samples after the origin are read only when ``score`` is requested.
    """
    times, values, dt, _, counts = _sampling(times, values, (horizon_s,))
    steps = int(counts[0])
    origin = _positive_integer(origin_index, 'origin_index')
    stride = _positive_integer(stride, 'stride')
    if not np.isfinite(limit) or not 0 < quantile < 1 or not 0 < alert_probability <= 1:
        raise ValueError('Finite limit, quantile in (0, 1) and alert probability in (0, 1] required')
    if origin >= len(times):
        raise ValueError('Origin outside the trace')
    history = values[:origin + 1]
    prediction = np.asarray(forecast(history, steps, order=order, ridge=ridge)['values'])
    lead_times = [float(times[origin] + dt * (k + 1)) for k in range(steps)]
    errors, last = [], None
    for start in range(3 * order - 1, origin - steps + 1, stride):
        fitted = np.asarray(forecast(values[:start + 1], steps, order=order, ridge=ridge)['values'])
        errors.append(values[start + 1:start + 1 + steps] - fitted)
        last = start + steps
    result = {'method': 'History-only AR with empirical back-test error trajectories',
              'limit': float(limit), 'horizon_s': float(horizon_s), 'quantile': float(quantile),
              'alert_probability': float(alert_probability), 'calibration_windows': len(errors),
              'calibration_last_index': last, 'origin_time': float(times[origin])}
    if len(errors) < min_calibration:
        result.update(status='insufficient_calibration', window_probability=None, alert=None,
                      upper_band=None, first_band_crossing_s=None)
    else:
        errors = np.vstack(errors)
        trajectories = prediction + errors
        probability = float(np.mean(trajectories.max(axis=1) > limit))
        upper = prediction + np.quantile(errors, quantile, axis=0)
        crossing = np.flatnonzero(upper > limit)
        result.update(status='calibrated', window_probability=probability,
                      alert=bool(probability >= alert_probability),
                      upper_band={'times': lead_times, 'values': upper.tolist()},
                      first_band_crossing_s=float(lead_times[crossing[0]] - times[origin]) if len(crossing) else None)
    if score:
        truth = values[origin + 1:origin + 1 + steps]
        observed = bool(truth.max() > limit) if len(truth) == steps else None
        result['observed_exceedance'] = observed
        if observed is None or result['alert'] is None:
            result['outcome'] = 'not_scored'
        else:
            result['outcome'] = {(True, True): 'hit', (False, True): 'miss', (True, False): 'false_alarm',
                                 (False, False): 'correct_negative'}[(result['alert'], observed)]
    return result


def benchmark(times, values, origins, leads_seconds=(30, 60, 120), order=40, ridge=1e-3):
    """Compare fixed AR and naive baselines on causally withheld future windows.

    Callers select origins and sampling before examining withheld performance.
    Bias is prediction minus observation; metrics retain the input scalar units.
    """
    times, values, dt, leads, counts = _sampling(times, values, leads_seconds)
    origins = list(origins)
    if not origins:
        raise ValueError('At least one fixed origin required')
    origins = [_positive_integer(index, 'origin') for index in origins]
    if len(set(origins)) != len(origins) or any(index + max(counts) >= len(times) for index in origins):
        raise ValueError('Unique origins with full future coverage required')
    traces = [_trace(times, values, origin, int(max(counts)), order, ridge) for origin in origins]
    return {'sample_interval_seconds': dt, 'metrics_basis': 'pooled future samples through each lead',
            'bias_sign': 'prediction minus observation', 'traces': traces,
            'metrics': {f'{lead:g}': _metrics(traces, int(count)) for lead, count in zip(leads, counts)},
            'limitations': ['Offline scalar benchmark; no operational qualification.',
                            'Overlapping origins are not independent trials.',
                            'Fixed hyperparameters; no future-based selection.']}
