"""Strict single-case adapter for existing OrcaFlex extraction/report tools."""
from __future__ import annotations

import csv
import json
from pathlib import Path
from types import SimpleNamespace

import numpy as np


def _extraction_dependencies():
    from digitalmodel.solvers.orcaflex.orcaflex_api import lazy_api
    from digitalmodel.solvers.orcaflex.orcaflex_objects import OrcaFlexObjects
    from digitalmodel.solvers.orcaflex.opp_time_series import OPPTimeSeries
    from digitalmodel.solvers.orcaflex.opp_range_graph import OPPRangeGraph
    return SimpleNamespace(api=lazy_api(), objects=OrcaFlexObjects(),
                           ts=OPPTimeSeries(), rg=OPPRangeGraph())


def _vector(values, label):
    array = np.asarray(values, dtype=float)
    if array.ndim != 1 or not len(array) or not np.isfinite(array).all():
        raise ValueError(f'{label}: nonempty finite one-dimensional data required')
    return array


def _unit_key(unit):
    return str(unit).replace(' ', '').replace('·', '.').casefold()


def _verified_channel(model, channel, deps, kind):
    for key in ('object', 'variable', 'units'):
        if not isinstance(channel.get(key), str) or not channel[key].strip():
            raise ValueError(f'Channel requires {key}')
    obj = model[channel['object']]
    position = channel.get('position')
    if kind == 'TimeHistory' and position not in ('End A', 'End B'):
        raise ValueError('Time history position must be End A or End B')
    extra = getattr(deps.api, 'oeEndA' if position == 'End A' else 'oeEndB') if position else None
    details = obj.varDetails(getattr(deps.api.ResultType, kind), objectExtra=extra)
    matches = [d for d in details if d.VarName.casefold() == channel['variable'].casefold()]
    if len(matches) != 1:
        raise ValueError(f"Unavailable/ambiguous channel: {channel['object']} / {channel['variable']}")
    actual = matches[0].VarUnits
    if _unit_key(actual) != _unit_key(channel['units']):
        raise ValueError(f"Unit mismatch: requested {channel['units']}, solver returned {actual}")
    return obj, matches[0].VarName, actual


def _contract(config, metadata):
    for key in ('time_histories', 'range_graphs'):
        if not isinstance(config.get(key), list) or not config[key]:
            raise ValueError(f'Explicit nonempty {key} contract required')
        identities = [(c.get('object'), c.get('variable', '').casefold(), c.get('position')) for c in config[key]]
        if len(set(identities)) != len(identities):
            raise ValueError(f'Duplicate {key} channels')
    period = _vector(config.get('period'), 'period')
    if len(period) != 2 or period[1] <= period[0]:
        raise ValueError('Increasing two-value extraction period required')
    start, stop = float(metadata['simulation_start']), float(metadata['simulation_stop'])
    if not np.isfinite([start, stop]).all() or start > period[0] or stop < period[1]:
        raise ValueError('Requested period is outside saved simulation coverage')
    for key in ('run_id', 'model_sha256', 'solver_version', 'limitations'):
        if key not in metadata:
            raise ValueError(f'Run metadata requires {key}')
    return period.tolist()


def _histories(model, config, metadata, deps, period):
    from digitalmodel.orcaflex.postprocessor import compute_time_series_stats
    output = []
    model_dict = {'model': model, 'start_time': metadata['simulation_start'],
                  'current_time': metadata['simulation_stop'], 'stop_time': metadata['simulation_stop']}
    for channel in config['time_histories']:
        _, variable, units = _verified_channel(model, channel, deps, 'TimeHistory')
        cfg = {'ObjectName': channel['object'], 'Variable': variable,
               'SimulationPeriod': period, 'objectExtra': [channel['position']], 'ArcLength': []}
        values, times = deps.ts.get_time_series_from_orcaflex_run(model_dict, cfg)
        values, times = _vector(values, variable), _vector(times, 'time')
        if len(values) != len(times) or len(times) < 2 or not np.all(np.diff(times) > 0):
            raise ValueError(f'{variable}: misaligned or non-increasing time history')
        if not np.allclose([times[0], times[-1]], period, rtol=0, atol=1e-7):
            raise ValueError(f'{variable}: incomplete requested time coverage')
        if output and not np.array_equal(times, output[0]['time']):
            raise ValueError('Channels have different time grids')
        label = f"{channel['object']} / {variable} / {channel['position']}"
        stats = compute_time_series_stats(values, label).model_dump()
        # Constant histories have undefined skewness/kurtosis; never emit NaN JSON.
        stats = {k: (v if not isinstance(v, float) or np.isfinite(v) else None) for k, v in stats.items()}
        output.append({'label': label, 'units': units, 'time': times.tolist(),
                       'values': values.tolist(), 'statistics': stats})
    return output


def _ranges(model, config, deps, period):
    output = []
    for channel in config['range_graphs']:
        obj, variable, units = _verified_channel(model, channel, deps, 'RangeGraph')
        graph = deps.rg.get_RangeGraph(obj, deps.objects.get_TimePeriodObject(period), variable, None, None)
        arrays = {key: _vector(getattr(graph, key), f'{variable}.{key}') for key in ('X', 'Min', 'Max', 'Mean')}
        if len({len(v) for v in arrays.values()}) != 1 or np.any(np.diff(arrays['X']) <= 0):
            raise ValueError(f'{variable}: misaligned/non-increasing arc lengths')
        if np.any(arrays['Min'] > arrays['Max']) or np.any(arrays['X'] < 0):
            raise ValueError(f'{variable}: invalid envelope bounds')
        tolerance = 1e-9 * np.maximum(1., np.abs(arrays['Max']))
        if np.any(arrays['Mean'] < arrays['Min'] - tolerance) or np.any(arrays['Mean'] > arrays['Max'] + tolerance):
            raise ValueError(f'{variable}: mean outside minimum/maximum envelope')
        output.append({'label': f"{channel['object']} / {variable}", 'units': units,
                       **{key: val.tolist() for key, val in arrays.items()}})
    return output


def _write_csv(path, headers, columns):
    with path.open('w', newline='', encoding='utf-8') as stream:
        writer = csv.writer(stream)
        writer.writerow(headers)
        writer.writerows(zip(*columns, strict=True))
    with path.open(newline='', encoding='utf-8') as stream:
        rows = list(csv.reader(stream))
    if len(rows) != len(columns[0]) + 1 or rows[0] != headers:
        raise ValueError(f'CSV readback failed: {path.name}')


def _report(config, metadata, histories, ranges, output_dir):
    from digitalmodel.solvers.orcaflex.reporting import OrcaFlexAnalysisReport, generate_orcaflex_report
    from digitalmodel.solvers.orcaflex.reporting.models.results import DynamicResultsData, TimeSeriesData, EnvelopeData
    dynamic = DynamicResultsData(
        ramp_end_time_s=0.0,
        time_series=[TimeSeriesData(id=f'ts_{i}', label=h['label'], t=h['time'],
                                   values=h['values'], units=h['units']) for i, h in enumerate(histories)],
        envelopes=[EnvelopeData(id=f'rg_{i}', label=g['label'], arc_length=g['X'],
                               min_values=g['Min'], max_values=g['Max'], units=g['units']) for i, g in enumerate(ranges)],
        statistical_summary=[{'channel': h['label'], 'units': h['units'], **h['statistics']} for h in histories],
    )
    notes = ['Demand-only diagnostic. Acceptance against allowable loads: Not evaluated.',
             f"Model SHA-256: {metadata['model_sha256']}", *metadata['limitations']]
    warnings = metadata.get('warnings')
    notes.append('Solver warnings: ' + ('; '.join(warnings) if warnings else
                 ('None reported by solver.' if warnings == [] else 'Not established.')))
    report = OrcaFlexAnalysisReport(project_name=config.get('title', 'Installation diagnostic'),
        structure_id=metadata['run_id'], structure_type='installation',
        orcaflex_version=metadata['solver_version'], dynamic_results=dynamic,
        summary_notes=' '.join(notes), recommendations=metadata['limitations'])
    path = generate_orcaflex_report(report, output_dir / 'report.html', include_plotlyjs='inline')
    html = path.read_text(encoding='utf-8')
    if ('Demand-only diagnostic' not in html or 'Error rendering section' in html
            or '<script src="https://cdn.plot.ly/' in html):
        raise ValueError('Report readback/offline validation failed')
    return path


def extract_results(model, config: dict, output_dir: Path, run_metadata: dict) -> dict:
    """Extract all required channels or raise; render only validated demands.

    Output directories must not already contain a success report. Solver units
    are checked through varDetails, and metre arc-length coordinates are required.
    """
    period = _contract(config, run_metadata)
    output_dir = Path(output_dir)
    if (output_dir / 'report.html').exists():
        raise ValueError('Refusing to overwrite an existing report')
    deps = _extraction_dependencies()
    # Existing report schema labels arc lengths in metres; require SI model input.
    general = getattr(model, 'general', None)
    if general is None or getattr(general, 'UnitsSystem', None) != 'SI':
        raise ValueError('SI model units required by existing report schema')
    histories = _histories(model, config, run_metadata, deps, period)
    ranges = _ranges(model, config, deps, period)
    output_dir.mkdir(parents=True, exist_ok=True)
    for i, h in enumerate(histories):
        _write_csv(output_dir / f'time_{i:03d}.csv', ['time_s', f"{h['label']} [{h['units']}]"], [h['time'], h['values']])
    for i, g in enumerate(ranges):
        _write_csv(output_dir / f'range_{i:03d}.csv', ['arc_length_m', *[f"{k} [{g['units']}]" for k in ('Min', 'Max', 'Mean')]],
                   [g[k] for k in ('X', 'Min', 'Max', 'Mean')])
    report = _report(config, run_metadata, histories, ranges, output_dir)
    receipt = {'status': 'complete', 'time_history_count': len(histories), 'range_graph_count': len(ranges),
               'report': str(report), 'period': period, 'run_metadata': run_metadata,
               'time_histories': histories, 'range_graphs': ranges}
    result_path = output_dir / 'extraction.json'
    result_path.write_text(json.dumps(receipt, indent=2, allow_nan=False), encoding='utf-8')
    if json.loads(result_path.read_text(encoding='utf-8')) != receipt:
        raise ValueError('Extraction JSON readback failed')
    return receipt
