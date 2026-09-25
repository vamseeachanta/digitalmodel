"""Prepare causal simulated monitoring frames from existing trace benchmarks."""
from __future__ import annotations

import argparse
from datetime import datetime, timezone
from hashlib import sha256
import json
from pathlib import Path

import numpy as np

from digitalmodel.workflows.installation_forecast import benchmark, exceedance_forecast
from digitalmodel.workflows.installation_forecast_report import _load


def _exceedance(arrays, key, index, limit, config):
    """Causal alert from back-tested errors before NOW; truth enters only the post-hoc score."""
    result = exceedance_forecast(arrays['time'], arrays[key], index, limit,
                                 quantile=config.get('quantile', 0.95),
                                 alert_probability=config.get('alert_probability', 0.2), score=True)
    return {**result, 'scoring': 'withheld truth used only for post-hoc scoring'}


def _channel_frames(arrays, spec, origins, exceedance=None):
    key = spec['id']
    if key not in arrays:
        raise ValueError(f'Missing monitoring channel: {key}')
    result = benchmark(arrays['time'], arrays[key], origins)
    frames = []
    for trace in result['traces']:
        index, now = trace['origin_index'], trace['origin_time']
        history = (arrays['time'] >= now - 120) & (arrays['time'] <= now)
        metrics = benchmark(arrays['time'], arrays[key], [index])['metrics']['120']
        extra = {}
        if exceedance is not None and spec.get('assumed_limit') is not None:
            extra['exceedance'] = _exceedance(arrays, key, index, spec['assumed_limit'], exceedance)
        frames.append({**spec, **extra, 'history': {
            'times': arrays['time'][history].tolist(), 'values': arrays[key][history].tolist()},
            'forecast': {'times': trace['times'],
                         'values': trace['predictions']['autoregression']},
            'truth': {'times': trace['times'], 'values': trace['actual']},
            'fit_status': trace['fit']['status'], 'metrics': metrics,
            'method': 'History-only ridge autoregression; fixed order 40, ridge 0.001',
            'training_end_s': now})
    return frames


def build_frames(arrays, channels, origins=(240, 300, 360, 420, 480), exceedance=None):
    """Fix origins before scoring; withheld truth is never a model input."""
    time = np.asarray(arrays['time'])
    indexes = []
    for origin in origins:
        matches = np.flatnonzero(np.isclose(time, origin, rtol=0, atol=1e-7))
        if len(matches) != 1:
            raise ValueError('Every origin must match exactly one sample')
        indexes.append(int(matches[0]))
    if not channels or len({c['id'] for c in channels}) != len(channels):
        raise ValueError('Unique monitoring channels are required')
    predictions = [_channel_frames(arrays, spec, indexes, exceedance) for spec in channels]
    return [{'now_s': float(origin), 'forecast_horizon_s': 120,
             'operational_validation': 'NOT ESTABLISHED',
             'channels': [channel[i] for channel in predictions]}
            for i, origin in enumerate(origins)]


def resolve_channels(criteria, channels):
    """Keep plotted load thresholds tied to the same configured envelope check."""
    checks = {check['id']: check for check in criteria['checks']}
    resolved = []
    for spec in channels:
        if spec['id'] == 'wave_elevation' and spec['units'] == 'm':
            limit = None
        else:
            check = checks.get(spec.get('criterion_id'))
            if (check is None or check['kind'] != 'maximum_tension'
                    or spec['id'] not in check['channels'] or spec['units'] != 'kN'
                    or check['value_units'] != 'Te'):
                raise ValueError('Monitoring criterion/channel mapping is invalid')
            limit = check['limit'] * criteria['force_conversion']['kN_per_Te']
            if not np.isfinite(limit) or limit <= 0:
                raise ValueError('Positive finite monitoring limit required')
        if 'assumed_limit' in spec and spec['assumed_limit'] != limit:
            raise ValueError('Monitoring limit differs from envelope criterion')
        resolved.append({**spec, 'assumed_limit': limit})
    return resolved


def validate_metadata(case, raw, metadata):
    """Pin metadata and identity independently of the unchanged NPZ digest."""
    if sha256(raw).hexdigest() != case.get('metadata_sha256'):
        raise ValueError('Monitoring metadata digest differs from verified evidence')
    if json.loads(raw) != metadata:
        raise ValueError('Monitoring metadata changed during extraction')
    for key, channel in metadata['channels'].items():
        expected = case['channels'].get(key, {})
        if any(channel.get(field) != expected.get(field) for field in
               ('object', 'variable', 'position', 'units', 'selection')):
            raise ValueError('Monitoring channel identity differs from verified evidence')


def add_wave_preview(frames, arrays):
    """Supply simulated future waves explicitly; never describe them as forecast skill."""
    from digitalmodel.workflows.installation_wave_assisted import benchmark_wave_assisted
    time, wave = arrays['time'], arrays['wave_elevation']
    indexes = [int(np.flatnonzero(np.isclose(time, f['now_s']))[0]) for f in frames]
    for channel_index, spec in enumerate(frames[0]['channels']):
        key = spec['id']
        if key == 'wave_elevation':
            for frame in frames:
                ch = frame['channels'][channel_index]
                future = (time > frame['now_s']) & (time <= frame['now_s'] + 120)
                ch.update(wave_preview={'times': time[future].tolist(),
                                        'values': wave[future].tolist()},
                          preview_fit_status='supplied_simulated_wave_input',
                          wave_preview_metrics=None)
            continue
        result = benchmark_wave_assisted(time, wave, arrays[key], indexes,
                                         load_units=spec['units'])
        for frame, trace in zip(frames, result['traces']):
            frame['channels'][channel_index].update(
                wave_preview={'times': trace['times'],
                              'values': trace['predictions']['oracle_wave_fir']},
                preview_fit_status=trace['wave_assisted_fit']['status'],
                wave_preview_metrics=result['origin_metrics'][f"{frame['now_s']:g}"]['120'])
    return frames


def _scenario(summary, selection, channels, *, include_wave_preview=True, exceedance=None):
    cases = [c for c in summary['cases'] if c['hs_m'] == selection['hs_m']
             and c['tp_s'] == selection['tp_s'] and c['status'] == 'VERIFIED']
    if len(cases) != 1:
        raise ValueError('Demo selection must identify one verified case')
    case = cases[0]
    raw = (Path(case['run_dir']) / 'installation_traces' / 'metadata.json').read_bytes()
    arrays, metadata = _load(case['run_dir'])
    validate_metadata(case, raw, metadata)
    if metadata['trace_sha256'] != case['trace_sha256']:
        raise ValueError('Demo trace differs from verified report evidence')
    for spec in channels:
        channel = metadata['channels'][spec['id']]
        if channel['units'] != spec['units'] or channel.get('selection') is not None:
            raise ValueError('Demo requires matching units and fixed channel locations')
    frames = build_frames(arrays, channels, exceedance=exceedance)
    if include_wave_preview:
        frames = add_wave_preview(frames, arrays)
    return {'case_index': case['index'], **selection,
            'source_label': 'SIMULATED replay; prescribed stationary sea state, no live sensors',
            'trace_sha256': metadata['trace_sha256'],
            'frames': frames}


def prepare_payload(summary, criteria, demo_config):
    from digitalmodel.workflows.installation_assumed_envelope import build_envelope
    include_preview = demo_config.get('include_wave_preview', True)
    if type(include_preview) is not bool:
        raise ValueError('include_wave_preview must be boolean')
    if not include_preview and demo_config['default_mode'] != 'history_only':
        raise ValueError('Disabled wave preview requires history_only default mode')
    envelope = build_envelope(summary, criteria)
    channels = resolve_channels(criteria, demo_config['channels'])
    mapping = {'PASS': 'WITHIN_ASSUMPTIONS', 'FAIL': 'EXCEEDS_ASSUMPTIONS',
               'NOT_EVALUATED': 'NOT_EVALUATED'}
    cells = [{**cell, 'status': mapping[cell['status']],
              'reason': cell.get('governing_check') or 'Criteria not evaluated',
              'metrics': {'max_utilization': cell.get('max_utilization')}}
             for cell in envelope['cells']]
    return {'title': demo_config.get('title', 'Jumper installation - assumed-project envelope and monitoring example'),
            'created_utc': datetime.now(timezone.utc).isoformat(),
            'criteria': [{'id': c['id'], 'label': c['id'], 'limit': c['limit'],
                          'units': c['value_units'], 'status': 'project_assumption'}
                         for c in criteria['checks']],
            'cases': cells, 'boundaries': envelope['boundaries'],
            'envelope': envelope, 'demo': {'default_mode': demo_config['default_mode'], 'scenarios': [
                _scenario(summary, s, channels, include_wave_preview=include_preview,
                          **({'exceedance': demo_config['exceedance']} if 'exceedance' in demo_config else {}))
                for s in demo_config['scenarios']]},
            'limitations': demo_config['limitations'], 'provenance': {},
            'snapshot': demo_config['snapshot'],
            'engineering_acceptance': 'NOT EVALUATED', 'criteria_basis': criteria}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('summary', 'criteria', 'demo-config', 'output'):
        parser.add_argument('--' + name, required=True, type=Path)
    args = parser.parse_args()
    paths = {'summary': args.summary, 'criteria': args.criteria, 'demo_config': args.demo_config}
    raw = {name: path.read_bytes() for name, path in paths.items()}
    payload = prepare_payload(*(json.loads(raw[k]) for k in paths))
    payload['provenance'] = {k: {'filename': paths[k].name, 'sha256': sha256(v).hexdigest()}
                             for k, v in raw.items()}
    if args.output.exists():
        raise FileExistsError('Preserve prior review payload')
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(payload, indent=2, allow_nan=False), encoding='utf-8')
    assert json.loads(args.output.read_text(encoding='utf-8')) == payload


if __name__ == '__main__':
    main()
