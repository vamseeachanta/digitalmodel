"""Supplement a verified reproduction with wave, tension and jumper traces."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import numpy as np

from digitalmodel.infrastructure.persistence.provenance import compute_hash
from digitalmodel.workflows.orcaflex_reproduce import _load_api, validate_samples
from digitalmodel.workflows.installation_response_metrics import tension_event_metrics


def checked_history(obj, api, variable, units, period, extra, count):
    details = obj.varDetails(api.ResultType.TimeHistory, objectExtra=extra)
    matches = [v for v in details if v.VarName == variable]
    if len(matches) != 1 or matches[0].VarUnits != units:
        raise ValueError(f"Unavailable variable or units: {variable} [{units}]")
    values = np.asarray(obj.TimeHistory(variable, period, extra), dtype=float)
    if values.shape != (count,) or not np.isfinite(values).all():
        raise ValueError(f"Invalid history: {variable}")
    return values


def chord_diagnostics(times, end_a, end_b, unstretched_length):
    t = np.asarray(times, dtype=float)
    a, b = np.asarray(end_a, dtype=float), np.asarray(end_b, dtype=float)
    if (a.shape != (len(t), 3) or b.shape != a.shape or len(t) < 2
            or not np.isfinite(a).all() or not np.isfinite(b).all()
            or not np.isfinite(t).all() or np.any(np.diff(t) <= 0)
            or not np.isfinite(unstretched_length) or unstretched_length <= 0):
        raise ValueError('Finite aligned coordinates, times and positive length required')
    span = np.linalg.norm(b - a, axis=1)
    return {'span_m': span.tolist(),
            'unstretched_length_minus_span_m': (unstretched_length - span).tolist(),
            'span_rate_m_per_s': np.gradient(span, t).tolist(),
            'interpretation': 'End-to-end geometric diagnostic, not a measured slack length; '
                              'includes sag and elastic extension effects.'}


def _geometry_channels(model, api, arrays, channels, period):
    for index in range(1, 7):
        name = f'Sling#{index}'
        ends = []
        for end in ('A', 'B'):
            extra = getattr(api, 'oeEnd' + end)
            coords = [checked_history(model[name], api, coordinate, 'm', period,
                                      extra, len(arrays['time'])) for coordinate in ('X', 'Y', 'Z')]
            ends.append(np.column_stack(coords))
        result = chord_diagnostics(arrays['time'], *ends, float(sum(model[name].Length)))
        for field, units in [('span_m', 'm'), ('unstretched_length_minus_span_m', 'm'),
                             ('span_rate_m_per_s', 'm/s')]:
            key = f'{name}_{field}'
            values = np.asarray(result[field])
            arrays[key] = values
            channels[key] = {'object': name, 'variable': field, 'units': units,
                             'minimum': float(values.min()), 'maximum': float(values.max()),
                             'interpretation': result['interpretation']}


def _add_channel(model, api, arrays, channels, key, name, variable, units,
                 period, extra, position):
    values = checked_history(model[name], api, variable, units, period, extra,
                             len(arrays['time']))
    arrays[key] = values
    channels[key] = {'object': name, 'variable': variable, 'units': units,
                     'position': position, 'minimum': float(values.min()),
                     'maximum': float(values.max())}
    if variable == 'Effective tension':
        channels[key]['events'] = tension_event_metrics(
            arrays['time'], values, units=units, near_zero_threshold=0.0)
        channels[key]['static_tension_kN'] = float(
            model[name].StaticResult(variable, extra))


def _line_channels(model, api, arrays, channels, period):
    names = [f'Cranewire#{i}' for i in (1, 2)]
    names += [f'Sling#{i}' for i in range(1, 7)]
    for name in names:
        for end in ('A', 'B'):
            _add_channel(model, api, arrays, channels, f'{name}_end_{end}', name,
                         'Effective tension', 'kN', period,
                         getattr(api, 'oeEnd' + end), 'End ' + end)
    for name in ['JumperLine', 'Connector1', 'Connector2']:
        for variable, units in [('Bend moment', 'kN.m'),
                                ('Max von Mises stress', 'kPa'),
                                ('Seabed clearance', 'm')]:
            graph = model[name].RangeGraph(variable, period)
            maxima = np.asarray(graph.Max, dtype=float)
            minima = np.asarray(graph.Min, dtype=float)
            if not np.isfinite(maxima).all() or not np.isfinite(minima).all():
                raise ValueError(f'Nonfinite range {name}: {variable}')
            index = int(np.argmin(minima) if variable == 'Seabed clearance'
                        else np.argmax(maxima))
            arc = float(graph.X[index])
            key = f'{name}_{variable}'
            _add_channel(model, api, arrays, channels, key, name, variable, units,
                         period, api.oeArcLength(arc), {'arc_length_m': arc})
            channels[key]['spatial_minimum'] = float(minima.min())
            channels[key]['spatial_maximum'] = float(maxima.max())
            channels[key]['selection'] = 'whole-record governing arc; diagnostic only'
    midpoint = float(sum(model['JumperLine'].Length)) / 2
    _add_channel(model, api, arrays, channels, 'JumperLine_midpoint_bend',
                 'JumperLine', 'Bend moment', 'kN.m', period,
                 api.oeArcLength(midpoint), {'arc_length_m': midpoint})
    channels['JumperLine_midpoint_bend']['selection'] = 'fixed geometric midpoint'


def extract(run_dir):
    run_dir = Path(run_dir).resolve()
    receipt = json.loads((run_dir / 'run.json').read_text())
    if receipt['status'] != 'completed':
        raise ValueError('Completed reproduction required')
    sims = list((run_dir / 'batch_runs/sims').glob('*.sim'))
    if len(sims) != 1 or compute_hash(sims[0]) != receipt['simulation_sha256']:
        raise ValueError('Simulation identity mismatch')
    destination = run_dir / 'installation_traces'
    if destination.exists():
        raise FileExistsError(destination)
    api, identity = _load_api({'solver_version': receipt['solver_version']})
    model = api.Model(str(sims[0]))
    period = api.SpecifiedPeriod(0.0, receipt['simulation_stop'])
    times = np.asarray(model.SampleTimes(period))
    validate_samples(times, [0.0, receipt['simulation_stop']],
                     receipt['actual_logging_interval'])
    arrays, channels = {'time': times}, {}
    _add_channel(model, api, arrays, channels, 'wave_elevation', 'Environment',
                 'Elevation', 'm', period, api.oeEnvironment(0, 0, 0),
                 {'x_m': 0, 'y_m': 0, 'z_m': 0})
    _line_channels(model, api, arrays, channels, period)
    _geometry_channels(model, api, arrays, channels, period)
    destination.mkdir(exist_ok=False)
    np.savez_compressed(destination / 'traces.npz', **arrays)
    metadata = {'solver': identity, 'simulation_sha256': receipt['simulation_sha256'],
                'channels': channels, 'engineering_acceptance': 'NOT EVALUATED',
                'limitations': ['Zero-tension events are not geometric slack distance.',
                                'Sampling and snap-load fidelity require convergence.',
                                'Component capacities and project edition unverified.'],
                'trace_sha256': compute_hash(destination / 'traces.npz')}
    (destination / 'metadata.json').write_text(
        json.dumps(metadata, indent=2, allow_nan=False), encoding='utf-8')
    with np.load(destination / 'traces.npz') as check:
        if set(check.files) != set(arrays) or any(
                not np.array_equal(check[key], value) for key, value in arrays.items()):
            raise ValueError('Saved trace readback mismatch')
    if json.loads((destination / 'metadata.json').read_text(encoding='utf-8')) != metadata:
        raise ValueError('Saved metadata readback mismatch')
    return destination


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('run_dir', type=Path)
    print(extract(parser.parse_args().run_dir))
