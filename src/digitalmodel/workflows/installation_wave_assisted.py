"""Conditional load surrogate driven by prescribed future waves, not a forecast.

The FIR fit sees past waves and loads only. Prediction deliberately uses actual
withheld waves as oracle forcing. This assesses potential value IF an accurate
wave outlook were available; it does not establish that wave outlook capability.
"""
from __future__ import annotations

from time import perf_counter

import numpy as np

from digitalmodel.workflows.installation_forecast import benchmark, _positive_integer, _vector


def _fit_predict(wave_history, load_history, future_wave, order, ridge):
    """Fit standardized, regularized finite impulse response with an intercept."""
    if len(wave_history) < 3 * order:
        raise ValueError('At least three FIR orders of paired history required')
    wave_mean, wave_scale = float(wave_history.mean()), float(wave_history.std())
    load_mean, load_scale = float(load_history.mean()), float(load_history.std())
    fit = {'order': order, 'ridge': ridge, 'training_samples': len(load_history),
           'wave_mean': wave_mean, 'wave_scale': wave_scale,
           'load_mean': load_mean, 'load_scale': load_scale,
           'lag_convention': 'current wave and previous order-1 samples', 'status': 'fitted'}
    if wave_scale == 0 or load_scale == 0:
        fit['status'] = ('constant_wave_history_mean_fallback' if wave_scale == 0
                         else 'constant_load_history')
        return [load_mean] * len(future_wave), fit
    wave = (wave_history - wave_mean) / wave_scale
    design = np.lib.stride_tricks.sliding_window_view(wave, order)
    target = (load_history[order - 1:] - load_mean) / load_scale
    center, offset = design.mean(axis=0), float(target.mean())
    centered = design - center
    coefficient = np.linalg.solve(centered.T @ centered + ridge * np.eye(order),
                                  centered.T @ (target - offset))
    history_tail = wave_history[-(order - 1):] if order > 1 else np.array([])
    forcing = (np.concatenate((history_tail, future_wave)) - wave_mean) / wave_scale
    future_design = np.lib.stride_tricks.sliding_window_view(forcing, order)
    predicted = ((future_design - center) @ coefficient + offset) * load_scale + load_mean
    if not np.isfinite(predicted).all():
        raise ValueError('Nonfinite wave-assisted predictions; no usable result')
    fit['coefficient_oldest_to_current'] = coefficient.tolist()
    fit['training_window_count'] = len(target)
    return predicted.tolist(), fit


def _error(traces, count):
    actual = np.concatenate([trace['actual'][:count] for trace in traces])
    predicted = np.concatenate([trace['predictions']['oracle_wave_fir'][:count] for trace in traces])
    residual = predicted - actual
    correlation = None
    if np.std(actual) > 0 and np.std(predicted) > 0:
        correlation = float(np.corrcoef(actual, predicted)[0, 1])
    return {'rmse': float(np.sqrt(np.mean(residual ** 2))), 'bias': float(np.mean(residual)),
            'correlation': correlation, 'sample_count': len(actual)}


def _fixed_origins(times):
    indexes = []
    for origin in (240., 360., 480.):
        found = np.flatnonzero(np.isclose(times, origin, rtol=0, atol=1e-7))
        if len(found) != 1:
            raise ValueError('Default origins require samples at 240, 360 and 480 s')
        indexes.append(int(found[0]))
    return indexes


def _augment(traces, waves, loads, order, ridge):
    for trace in traces:
        origin, count = trace['origin_index'], len(trace['actual'])
        future_wave = waves[origin + 1:origin + 1 + count]
        predicted, fit = _fit_predict(waves[:origin + 1], loads[:origin + 1],
                                     future_wave, order, ridge)
        trace['predictions']['oracle_wave_fir'] = predicted
        trace['wave_assisted_fit'] = fit
        trace['oracle_future_wave'] = future_wave.tolist()


def benchmark_wave_assisted(times, waves, loads, origins=None, *,
                            leads_seconds=(30, 60, 120), order=40, ridge=1e-3,
                            ar_order=40, load_units=None, wave_units='m'):
    """Compare oracle-wave FIR with history-only AR, mean and persistence.

    Explicit origins are sample indexes, matching installation_forecast.
    Omitted origins mean fixed simulation times 240/360/480 s. Default 40 lags
    represent 20 s on a 0.5 s grid. No resampling or future-load fitting occurs.
    Hyperparameters/origins must be fixed before inspecting withheld errors.
    """
    started = perf_counter()
    times, waves, loads = _vector(times), _vector(waves), _vector(loads)
    if len(waves) != len(loads) or len(times) != len(loads):
        raise ValueError('Aligned finite wave, load and time vectors required')
    order = _positive_integer(order, 'order')
    if not np.isfinite(ridge) or ridge <= 0:
        raise ValueError('Positive finite ridge required')
    origins = _fixed_origins(times) if origins is None else list(origins)
    result = benchmark(times, loads, origins, leads_seconds=leads_seconds,
                       order=ar_order, ridge=ridge)
    _augment(result['traces'], waves, loads, order, float(ridge))
    dt = result['sample_interval_seconds']
    for lead, metrics in result['metrics'].items():
        metrics['oracle_wave_fir'] = _error(result['traces'], round(float(lead) / dt))
    result['origin_metrics'] = {}
    for trace in result['traces']:
        metrics = benchmark(times, loads, [trace['origin_index']], leads_seconds=leads_seconds,
                            order=ar_order, ridge=ridge)['metrics']
        for lead, methods in metrics.items():
            methods['oracle_wave_fir'] = _error([trace], round(float(lead) / dt))
        result['origin_metrics'][f"{trace['origin_time']:g}"] = metrics
    result.update(forecast_basis='PRESCRIBED FUTURE WAVES / ORACLE INPUT',
                  operational_validation='NOT ESTABLISHED', load_units=load_units,
                  wave_units=wave_units, runtime_seconds=perf_counter() - started)
    result['limitations'] += [
        'Actual withheld waves are supplied; this is not a causal wave forecast or live validation.',
        'Potential benefit is conditional on an accurate future wave input; no wave forecast is demonstrated.',
        'One-point linear wave-to-load surrogate does not resolve nonlinear slack/snap response.',
        'Channel location and hyperparameters must not be chosen using withheld performance.']
    return result
