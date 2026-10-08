"""Pinned numerical diagnostics; neither convergence nor operating acceptance."""
from __future__ import annotations

import argparse
import copy
from hashlib import sha256
import io
import json
from pathlib import Path

import numpy as np
import yaml

from digitalmodel.workflows.installation_response_metrics import tension_event_metrics, _signals
from digitalmodel.workflows.installation_trace_extract import verify_profile_metadata, verify_profile_arrays
from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader
from digitalmodel.workflows.installation_numerical_prepare import _line, _value


def _alignment(parent, candidate):
    a, _ = _signals(parent, parent)
    b, _ = _signals(candidate, candidate)
    if not np.allclose([a[0], a[-1]], [b[0], b[-1]], rtol=0, atol=1e-9):
        raise ValueError('Identical extraction endpoints required')
    coarse, fine = (a, b) if len(a) <= len(b) else (b, a)
    indices = np.rint((coarse - fine[0]) / (fine[1] - fine[0])).astype(int)
    if (np.any(indices < 0) or np.any(indices >= len(fine))
            or not np.allclose(fine[indices], coarse, rtol=0, atol=1e-9)):
        raise ValueError('Nested common-time grids required; interpolation forbidden')
    return (np.arange(len(a)), indices) if len(a) <= len(b) else (indices, np.arange(len(b)))


def _event_sets(times, values):
    dt = float(times[1] - times[0])
    raw = tension_event_metrics(times, values, units='kN')
    events = raw['low_tension']['events']
    result = {'total_nonpositive_s': raw['low_tension']['total_duration_s'],
              'longest_nonpositive_s': raw['low_tension']['maximum_duration_s'],
              'event_count': len(events), 'strict_negative_event_count': raw['compression']['event_count'],
              'censored': {k: sum(bool(e[v]) for e in events) for k, v in
                           [('left', 'left_censored'), ('right', 'right_censored'),
                            ('window', 'retension_window_censored')]}}
    for name, minimum in [('unfiltered', 0.), ('fixed_duration', .1), ('resolution', 2 * dt)]:
        selected = [e for e in events if e['retension_peak'] is not None and e['duration_s'] >= minimum and
                    (name == 'unfiltered' or not any(e[k] for k in
                     ('left_censored', 'right_censored', 'retension_window_censored')))]
        event = copy.deepcopy(max(selected, key=lambda e: e['retension_peak'], default=None))
        if event is not None:
            samples = values[(times >= event['start_s']) & (times <= event['end_s'])]
            event['classification'] = ('nonpositive_with_negative_samples' if np.any(samples < 0)
                                       else 'nonpositive_without_negative_samples')
            event['duration_resolution_limited'] = event['duration_s'] <= minimum + dt
            event['peak_floored'] = event['retension_peak'] == 0 and event['retension_peak_time_s'] is None
        result[name] = dict(minimum_duration_s=minimum, qualifying_count=len(selected), governing=event,
            excludes_censored=name != 'unfiltered', ranking_criterion='maximum_retension_peak_kN',
            excluded_duration_count=sum(e['duration_s'] < minimum for e in events),
            excluded_censored_count=sum(name != 'unfiltered' and any(e[k] for k in
                ('left_censored', 'right_censored', 'retension_window_censored')) for e in events),
            unavailable_peak_count=sum(e['retension_peak'] is None for e in events),
            exclusion_counts_overlap=True)
    return result


def _summary(times, values, tension):
    result = {'minimum': float(np.min(values)), 'maximum': float(np.max(values)),
              'minimum_time_s': float(times[np.argmin(values)]),
              'maximum_time_s': float(times[np.argmax(values)])}
    if tension:
        result['events'] = _event_sets(times, values)
    return result


def _validate_arrays(arrays, channels):
    if set(arrays) != {'time', *channels} or not channels or 'wave_elevation' not in channels:
        raise ValueError('Exact profile/array coverage including wave required')
    times = np.asarray(arrays['time'], dtype=float)
    for key in arrays:
        _signals(times, arrays[key])
    if not any(c['variable'] == 'Effective tension' for c in channels.values()):
        raise ValueError('At least one tension channel required')
    for channel in channels.values():
        if channel['variable'] == 'Effective tension' and channel['units'] != 'kN':
            raise ValueError('Tension units must be kN')
    return times


def compare_arrays(parent_arrays, candidate_arrays, channels, *, kind):
    """Compare nested grids without interpolation or hidden acceptance tolerance."""
    if kind not in ('logging', 'time', 'mesh'):
        raise ValueError('Explicit logging/time/mesh comparison kind required')
    a, b = _validate_arrays(parent_arrays, channels), _validate_arrays(candidate_arrays, channels)
    ia, ib = _alignment(a, b)
    if not np.array_equal(parent_arrays['wave_elevation'][ia], candidate_arrays['wave_elevation'][ib]):
        raise ValueError('Common-time wave identity failed')
    result = dict(kind=kind, status='VERIFIED_DIAGNOSTIC', engineering_acceptance='NOT EVALUATED',
                  common_samples=len(ia), parent_interval_s=float(a[1]-a[0]),
                  candidate_interval_s=float(b[1]-b[0]), channels={}, logging_load_identity=True,
                  logging_response_identity=True, logging_gate_identity=True, identity_exclusions=[])
    for key, info in channels.items():
        pa, cb = np.asarray(parent_arrays[key]), np.asarray(candidate_arrays[key])
        tension = info['variable'] == 'Effective tension'
        identity = np.array_equal(pa[ia].astype(np.float32), cb[ib].astype(np.float32))
        if tension:
            result['logging_load_identity'] &= bool(identity)
        if key != 'wave_elevation':
            result['logging_response_identity'] &= bool(identity)
            if info['variable'] == 'span_rate_m_per_s':
                result['identity_exclusions'].append(dict(channel=key,
                    reason='Discrete gradient depends on logging grid; discrepancy retained as diagnostic'))
            else:
                result['logging_gate_identity'] &= bool(identity)
        result['channels'][key] = dict(identity=info, common_float32_identity=bool(identity),
            common_maximum_absolute_difference=float(np.max(np.abs(cb[ib]-pa[ia]))),
            parent_native=_summary(a, pa, tension), candidate_native=_summary(b, cb, tension),
            parent_common=_summary(a[ia], pa[ia], tension), candidate_common=_summary(b[ib], cb[ib], tension))
    if kind == 'logging' and not result['logging_gate_identity']:
        result['status'] = 'BLOCKED'
    result['dependent_stage_release'] = False
    result['requires_review'] = True
    result['limitations'] = ['Same-path comparisons are smoke checks, not independent repeatability.',
        'Post-exit peaks and chord deficits do not establish physical snap loads or slack allowance.',
        'One refinement and one seed do not establish convergence or qualified operating limits.']
    return result


def _apply_change(model, key, value, kind):
    allowed = {'logging': 'TargetLogSampleInterval', 'time': 'ImplicitConstantTimeStep'}
    if kind in allowed and key == 'General.' + allowed[kind]:
        if type(value) not in (float, int) or not np.isfinite(value) or value <= 0:
            raise ValueError('Positive finite numerical setting required')
        if model['General'][allowed[kind]] == value:
            raise ValueError('Declared numerical change is unchanged')
        model['General'][allowed[kind]] = value
        return
    if kind == 'mesh' and key.startswith('Lines.') and key.endswith('.TargetSegmentLength'):
        name = key[len('Lines.'):-len('.TargetSegmentLength')]
        line, table, column = _line(model, name)
        rows = line[table]
        if (not isinstance(value, list) or len(value) != len(rows) or
                any(type(v) not in (int, float) or not np.isfinite(v) or v <= 0 for v in value)):
            raise ValueError('Explicit positive target for every section required')
        if _value(model, key) == value:
            raise ValueError('Mesh change is unchanged')
        _value(model, key, value, True)
        return
    raise ValueError('Undeclared or nonnumerical override')


def validate_models(parent, candidate, *, kind, expected_changes):
    """The comparator is the immediate predecessor, not the original ancestor."""
    if not isinstance(expected_changes, dict) or len(expected_changes) != 1:
        raise ValueError('Exactly one numerical control must differ per pair')
    for model in (parent, candidate):
        if any(k in model for k in ('BaseFile', 'IncludeFile')):
            raise ValueError('Standalone model required')
        general = model['General']
        if general['ImplicitUseVariableTimeStep'] is not False or general['LogPrecision'] != 'Single':
            raise ValueError('Fixed integration and Single logging required')
    expected = copy.deepcopy(parent)
    for key, value in expected_changes.items():
        _apply_change(expected, key, value, kind)
    if _canonical_tables(expected) != _canonical_tables(candidate):
        raise ValueError('Native model differs outside declared numerical control')


def _canonical_tables(model):
    result = copy.deepcopy(model)
    for item in result.get('Lines', []):
        if not any('TargetSegmentLength' in key for key in item):
            continue
        line, table, _ = _line(result, item['Name'])
        fields = [f.strip() for f in table.split(',')]
        if len(set(fields)) != len(fields) or any(len(row) != len(fields) for row in line[table]):
            raise ValueError('Invalid packed line table')
        order = sorted(range(len(fields)), key=lambda i: fields[i])
        rows = line.pop(table)
        line[', '.join(fields[i] for i in order)] = [[row[i] for i in order] for row in rows]
    return result


def _read_pinned(path, digest, consumed):
    raw = Path(path).read_bytes()
    if not isinstance(digest, str) or sha256(raw).hexdigest() != digest:
        raise ValueError(f'Artifact digest mismatch: {path}')
    consumed[Path(path)] = digest
    return raw


def _load_run(directory, receipt_hash, metadata_hash, consumed):
    root = Path(directory).resolve()
    receipt = json.loads(_read_pinned(root/'run.json', receipt_hash, consumed))
    if receipt.get('status') != 'completed':
        raise ValueError('Completed solver receipt required')
    request = yaml.safe_load(_read_pinned(root/'request.yml', receipt['request_sha256'], consumed))
    model = yaml.load(_read_pinned(root/'source/model.yml', receipt['model_sha256'], consumed), Loader=OrcaFlexLoader)
    metadata = json.loads(_read_pinned(root/'installation_traces/metadata.json', metadata_hash, consumed))
    trace = _read_pinned(root/'installation_traces/traces.npz', metadata['trace_sha256'], consumed)
    if (metadata['simulation_sha256'] != receipt['simulation_sha256'] or
            request['model_sha256'] != receipt['model_sha256'] or not metadata.get('supplemental_profile')):
        raise ValueError('Request, simulation or supplemental profile binding mismatch')
    verify_profile_metadata(root, receipt, metadata)
    verify_profile_arrays(root/'installation_traces/traces.npz', receipt, metadata)
    with np.load(io.BytesIO(trace), allow_pickle=False) as arrays:
        signals = {key: arrays[key].copy() for key in arrays.files}
    _runtime(receipt, request, metadata, model, signals)
    for key, channel in metadata['channels'].items():
        if channel['variable'] == 'Effective tension':
            actual = tension_event_metrics(signals['time'], signals[key], units='kN')
            if actual != channel['events']:
                raise ValueError('Stored tension event dictionary mismatch')
    return dict(receipt=receipt, request=request, model=model, metadata=metadata, arrays=signals)


def _runtime(receipt, request, metadata, model, arrays):
    warnings = receipt.get('warnings')
    if not isinstance(warnings, list) or any(not isinstance(w, str) for w in warnings):
        raise ValueError('Explicit solver warning list required')
    for key in ('solver', 'solver_version', 'simulation_start', 'simulation_stop', 'actual_logging_interval'):
        if receipt.get(key) is None:
            raise ValueError(f'Missing runtime identity: {key}')
    if (not receipt['solver'] or metadata.get('solver') != receipt['solver'] or
            request.get('solver_version') != receipt['solver_version']):
        raise ValueError('Solver identity binding mismatch')
    budgets = receipt.get('batch', {}).get('native_thread_budget', [])
    if len(budgets) != 1 or budgets[0].get('requested') != 1 or budgets[0].get('observed') != 1:
        raise ValueError('Verified one-thread runtime required')
    interval = receipt['actual_logging_interval']
    if interval != model['General']['TargetLogSampleInterval']:
        raise ValueError('Actual logging interval differs from native model')
    if not np.allclose(np.diff(arrays['time']), interval, rtol=0, atol=1e-9):
        raise ValueError('Trace sampling differs from actual logging interval')
    if request['extraction'].get('period') != [float(arrays['time'][0]), float(arrays['time'][-1])]:
        raise ValueError('Trace period differs from request')


def _channel_identities(metadata):
    return {key: {field: row.get(field) for field in ('object', 'variable', 'position', 'units')}
            for key, row in metadata['channels'].items()}


def compare_pair(parent, candidate, *, parent_receipt_sha256, candidate_receipt_sha256,
                 parent_metadata_sha256, candidate_metadata_sha256, kind, expected_changes):
    consumed = {}
    a = _load_run(parent, parent_receipt_sha256, parent_metadata_sha256, consumed)
    b = _load_run(candidate, candidate_receipt_sha256, candidate_metadata_sha256, consumed)
    validate_models(a['model'], b['model'], kind=kind, expected_changes=expected_changes)
    if a['request']['extraction'] != b['request']['extraction']:
        raise ValueError('Extraction configuration changed')
    for key in ('solver', 'solver_version', 'simulation_start', 'simulation_stop'):
        if a['receipt'].get(key) != b['receipt'].get(key):
            raise ValueError(f'Execution identity differs: {key}')
    channels = _channel_identities(a['metadata'])
    if channels != _channel_identities(b['metadata']):
        raise ValueError('Channel semantic identity differs')
    result = compare_arrays(a['arrays'], b['arrays'], channels, kind=kind)
    result['source_paths_equal'] = Path(parent).resolve() == Path(candidate).resolve()
    result['expected_changes'] = expected_changes
    result['comparator_sha256'] = sha256(Path(__file__).read_bytes()).hexdigest()
    result['warnings'] = {name: data['receipt']['warnings'] for name, data in [('parent', a), ('candidate', b)]}
    result['warning_counts'] = {name: len(values) for name, values in result['warnings'].items()}
    result['warnings_changed'] = result['warnings']['parent'] != result['warnings']['candidate']
    result['sources'] = [{'path': str(path), 'sha256': digest} for path, digest in consumed.items()]
    for path, digest in consumed.items():
        if sha256(path.read_bytes()).hexdigest() != digest:
            raise ValueError('Input changed during comparison')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, required=True)
    parser.add_argument('--config-sha256', required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    if args.output.exists():
        raise FileExistsError(args.output)
    from digitalmodel.workflows.installation_numerical_compare_plan import compare_planned_pair
    raw = _read_pinned(args.config, args.config_sha256, {})
    result = compare_planned_pair(json.loads(raw))
    if args.config.read_bytes() != raw:
        raise ValueError('Comparison config changed')
    result['config_sha256'] = args.config_sha256
    payload = json.dumps(result, indent=2, allow_nan=False) + '\n'
    with args.output.open('x', encoding='utf-8') as stream:
        stream.write(payload)
    if args.output.read_text(encoding='utf-8') != payload:
        raise ValueError('Comparison output readback mismatch')
    print(result['status'])
    if result['status'] == 'BLOCKED':
        raise SystemExit(1)


if __name__ == '__main__':
    main()
