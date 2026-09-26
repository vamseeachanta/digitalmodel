"""Supplement a verified reproduction with wave, tension and jumper traces."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import numpy as np
import yaml

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


def _geometry_channels(model, api, arrays, channels, period, names=None):
    for name in names if names is not None else [f'Sling#{i}' for i in range(1, 7)]:
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
        if not np.isfinite(channels[key]['static_tension_kN']):
            raise ValueError('Nonfinite static tension')


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


def _validate_profile(profile):
    if (not isinstance(profile, dict) or type(profile.get('schema_version')) is not int
            or profile['schema_version'] != 1):
        raise ValueError('Supplemental profile schema_version 1 required')
    if set(profile) - {'schema_version', 'channels', 'geometry_lines'}:
        raise ValueError('Unknown supplemental profile field')
    rows, names = profile.get('channels'), profile.get('geometry_lines', [])
    if not isinstance(rows, list) or not rows or not isinstance(names, list):
        raise ValueError('Explicit nonempty channels and geometry list required')
    identities = []
    for row in rows:
        if (not isinstance(row, dict) or set(row) - {'object', 'variable', 'units', 'position'}
                or any(not isinstance(row.get(k), str) or not row[k].strip()
                       for k in ('object', 'variable', 'units'))
                or row.get('position') not in (None, 'End A', 'End B')):
            raise ValueError('Invalid supplemental channel selector')
        identities.append((row['object'], row['variable'], row.get('position')))
    if len(set(identities)) != len(identities):
        raise ValueError('Duplicate supplemental channels')
    if any(not isinstance(n, str) or not n.strip() for n in names) or len(set(names)) != len(names):
        raise ValueError('Invalid or duplicate geometry lines')
    return rows, names


def _profile_channels(model, api, arrays, channels, period, profile):
    rows, names = _validate_profile(profile)
    for index, row in enumerate(rows):
        position = row.get('position')
        extra = getattr(api, 'oeEnd' + position[-1]) if position else None
        _add_channel(model, api, arrays, channels, f'profile_{index:03d}',
                     row['object'], row['variable'], row['units'], period, extra, position)
    _geometry_channels(model, api, arrays, channels, period, names=names)


def _read_profile(run_dir, receipt, override):
    request = run_dir / 'request.yml'
    raw = request.read_bytes()
    if hashlib.sha256(raw).hexdigest() != receipt.get('request_sha256'):
        raise ValueError('Request identity mismatch')
    embedded = yaml.safe_load(raw).get('extraction', {}).get('supplemental_profile')
    if override is not None and embedded is not None and override != embedded:
        raise ValueError('Supplemental profile override conflicts with embedded profile')
    profile = embedded if override is None else override
    if profile is not None:
        _validate_profile(profile)
    return profile


def profile_digest(profile):
    return hashlib.sha256(json.dumps(profile, sort_keys=True,
                                    separators=(',', ':')).encode()).hexdigest()


def verify_profile_metadata(run_dir, receipt, metadata):
    expected = _read_profile(run_dir, receipt, None)
    saved = metadata.get('supplemental_profile')
    if expected is not None and saved != expected:
        raise ValueError('Saved supplemental profile differs from request')
    if saved is not None:
        rows, names = _validate_profile(saved)
        if (metadata.get('supplemental_profile_sha256') != profile_digest(saved)
                or metadata.get('request_sha256') != receipt['request_sha256']):
            raise ValueError('Supplemental profile provenance mismatch')
        channels = metadata.get('channels', {})
        if 'wave_elevation' not in channels:
            raise ValueError('Missing supplemental wave channel')
        for index, row in enumerate(rows):
            actual = channels.get(f'profile_{index:03d}', {})
            if any(actual.get(k) != row.get(k) for k in ('object', 'variable', 'units', 'position')):
                raise ValueError('Supplemental channel coverage mismatch')
        for name in names:
            for field in ('span_m', 'unstretched_length_minus_span_m', 'span_rate_m_per_s'):
                if channels.get(f'{name}_{field}', {}).get('object') != name:
                    raise ValueError('Supplemental geometry coverage mismatch')


def verify_profile_arrays(path, receipt, metadata):
    if metadata.get('supplemental_profile') is None:
        return
    with np.load(path, allow_pickle=False) as arrays:
        times = arrays['time']
        validate_samples(times, [0.0, receipt['simulation_stop']], receipt['actual_logging_interval'])
        if set(arrays.files) != {'time', *metadata['channels']}:
            raise ValueError('Supplemental saved array coverage mismatch')
        if any(arrays[key].shape != times.shape or not np.isfinite(arrays[key]).all()
               for key in arrays.files):
            raise ValueError('Invalid supplemental saved arrays')


def extract(run_dir, supplemental_profile=None):
    run_dir = Path(run_dir).resolve()
    receipt = json.loads((run_dir / 'run.json').read_text())
    if receipt['status'] != 'completed':
        raise ValueError('Completed reproduction required')
    profile = _read_profile(run_dir, receipt, supplemental_profile)
    sims = list((run_dir / 'batch_runs/sims').glob('*.sim'))
    if len(sims) != 1 or compute_hash(sims[0]) != receipt['simulation_sha256']:
        raise ValueError('Simulation identity mismatch')
    destination = run_dir / 'installation_traces'
    if destination.exists():
        raise FileExistsError(destination)
    api, identity = _load_api({'solver_version': receipt['solver_version']})
    model = api.Model(str(sims[0]), threadCount=1)
    period = api.SpecifiedPeriod(0.0, receipt['simulation_stop'])
    times = np.asarray(model.SampleTimes(period))
    validate_samples(times, [0.0, receipt['simulation_stop']],
                     receipt['actual_logging_interval'])
    arrays, channels = {'time': times}, {}
    _add_channel(model, api, arrays, channels, 'wave_elevation', 'Environment',
                 'Elevation', 'm', period, api.oeEnvironment(0, 0, 0),
                 {'x_m': 0, 'y_m': 0, 'z_m': 0})
    if profile is None:
        _line_channels(model, api, arrays, channels, period)
        _geometry_channels(model, api, arrays, channels, period)
    else:
        _profile_channels(model, api, arrays, channels, period, profile)
    destination.mkdir(exist_ok=False)
    np.savez_compressed(destination / 'traces.npz', **arrays)
    metadata = {'solver': identity, 'simulation_sha256': receipt['simulation_sha256'],
                'extractor_sha256': compute_hash(Path(__file__)),
                'metrics_sha256': compute_hash(Path(__file__).with_name('installation_response_metrics.py')),
                'channels': channels, 'engineering_acceptance': 'NOT EVALUATED',
                'limitations': ['Zero-tension events are not geometric slack distance.',
                                'Sampling and snap-load fidelity require convergence.',
                                'Component capacities and project edition unverified.'],
                'trace_sha256': compute_hash(destination / 'traces.npz')}
    if profile is not None:
        metadata.update(supplemental_profile=profile,
            supplemental_profile_source='override' if supplemental_profile is not None else 'request',
            supplemental_profile_sha256=profile_digest(profile),
            request_sha256=receipt['request_sha256'])
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
