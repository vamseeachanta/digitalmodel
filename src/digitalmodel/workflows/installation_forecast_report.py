"""Render an offline scalar forecast benchmark from verified installation traces."""
from __future__ import annotations

import argparse
import hashlib
from html import escape
import io
import json
from pathlib import Path

import numpy as np

from digitalmodel.workflows.installation_forecast import benchmark


def _load(run_dir):
    directory = Path(run_dir) / 'installation_traces'
    metadata = json.loads((directory / 'metadata.json').read_text(encoding='utf-8'))
    raw = (directory / 'traces.npz').read_bytes()
    if hashlib.sha256(raw).hexdigest() != metadata.get('trace_sha256'):
        raise ValueError('Trace digest mismatch')
    with np.load(io.BytesIO(raw), allow_pickle=False) as archive:
        arrays = {key: archive[key].copy() for key in archive.files}
    time = arrays['time']
    if time.ndim != 1 or len(time) < 2 or not np.isfinite(time).all():
        raise ValueError('Finite time vector required')
    intervals = np.diff(time)
    if intervals[0] <= 0 or not np.allclose(intervals, intervals[0], rtol=1e-7, atol=1e-9):
        raise ValueError('Strictly increasing uniform time required')
    stride = int(round(.5 / intervals[0]))
    if stride < 1 or not np.isclose(stride * intervals[0], .5):
        raise ValueError('Trace sampling must divide the fixed 0.5 s benchmark grid')
    for key, array in arrays.items():
        if array.shape != time.shape or not np.isfinite(array).all():
            raise ValueError(f'Invalid aligned trace: {key}')
    return {key: array[::stride] for key, array in arrays.items()}, metadata


def _evaluate(arrays, metadata):
    time = arrays['time']
    origins = []
    for candidate in (240., 360., 480.):
        indexes = np.flatnonzero(np.isclose(time, candidate, rtol=0, atol=1e-7))
        if len(indexes) == 1 and indexes[0] >= 119 and candidate + 120 <= time[-1] + 1e-7:
            origins.append(int(indexes[0]))
    channels = ['wave_elevation', 'Sling#5_end_B']
    if 'JumperLine_midpoint_bend' in arrays:
        channels.append('JumperLine_midpoint_bend')
    channels += sorted(key for key in arrays if key.endswith(('_end_A', '_end_B')) and key not in channels)
    results = {}
    for key in channels:
        if key not in arrays or not metadata.get('channels', {}).get(key, {}).get('units'):
            raise ValueError(f'Missing required trace or units: {key}')
        if metadata['channels'][key].get('selection') not in (None, 'fixed geometric midpoint'):
            raise ValueError(f'Benchmark channel was selected using future information: {key}')
        if origins:
            results[key] = benchmark(time, arrays[key], origins)
            results[key]['origin_metrics'] = {
                f'{time[index]:g} s': benchmark(time, arrays[key], [index])['metrics']
                for index in origins}
    return results


def _number(value):
    return 'undefined' if value is None else f'{value:.5g}'


def _metrics_table(results, metadata):
    rows = []
    for channel, result in results.items():
        unit = escape(metadata['channels'][channel]['units'])
        groups = {'All origins pooled': result['metrics'], **result['origin_metrics']}
        for origin, group in groups.items():
            for lead, methods in group.items():
                ar = methods['autoregression']['rmse']
                baseline = min(methods['persistence']['rmse'], methods['history_mean']['rmse'])
                verdict = 'Lower observed RMSE' if ar < baseline else 'No advantage over best naive baseline'
                for method, metrics in methods.items():
                    cells = [escape(channel), origin, lead, method, unit, _number(metrics['rmse']),
                             _number(metrics['bias']), _number(metrics['correlation']),
                             str(metrics['sample_count']), verdict if method == 'autoregression' else '']
                    rows.append('<tr>' + ''.join(f'<td>{cell}</td>' for cell in cells) + '</tr>')
    headers = ['Channel', 'Origin', 'Horizon (s)', 'Method', 'Units', 'RMSE', 'Bias', 'Correlation', 'Samples', 'Comparison']
    return '<table><tr>' + ''.join(f'<th>{h}</th>' for h in headers) + '</tr>' + ''.join(rows) + '</table>'


def _plot(arrays, results, metadata):
    try:
        from matplotlib.figure import Figure
    except ImportError:
        return '<p>Plot unavailable: optional matplotlib dependency is not installed.</p>'
    reference = results['wave_elevation']['traces']
    selected = next((t['origin_index'] for t in reference if t['origin_time'] == 360.), reference[0]['origin_index'])
    keys = ['wave_elevation', 'Sling#5_end_B']
    if 'JumperLine_midpoint_bend' in results:
        keys.append('JumperLine_midpoint_bend')
    figure = Figure(figsize=(12, 3.5 * len(keys)), constrained_layout=True)
    axes = figure.subplots(len(keys), 1, sharex=True)
    for axis, key in zip(axes, keys):
        trace = next(t for t in results[key]['traces'] if t['origin_index'] == selected)
        start = max(0, selected - 240)
        stop = selected + 1 + len(trace['times'])
        axis.plot(arrays['time'][start:stop], arrays[key][start:stop], color='black', label='Simulated actual')
        for method, style in [('autoregression', '-'), ('persistence', '--'), ('history_mean', ':')]:
            axis.plot(trace['times'], trace['predictions'][method], style, label=method)
        axis.axvline(trace['origin_time'], color='red', linestyle='--', label='NOW (fixed origin)')
        axis.axvspan(trace['origin_time'], trace['times'][-1], alpha=.08, color='blue')
        axis.set_ylabel(metadata['channels'][key]['units'])
        axis.set_title(f"{key}: {trace['fit']['status']}")
        axis.grid(alpha=.2)
        axis.legend(loc='best', fontsize=8)
    axes[-1].set_xlabel('Simulation time (s)')
    buffer = io.StringIO()
    figure.savefig(buffer, format='svg')
    svg = buffer.getvalue()
    return svg[svg.index('<svg'):]


def _origin_details(results):
    rows = []
    for key, result in results.items():
        for trace in result['traces']:
            rows.append(f"<li>{escape(key)}: origin {trace['origin_time']:g} s; "
                        f"{trace['fit']['training_samples']} training samples; "
                        f"{escape(trace['fit']['status'])}</li>")
    return '<h2>All fixed origins and fit status</h2><ul>' + ''.join(rows) + '</ul>'


def _document(arrays, metadata, results):
    diagnostics = [key for key in metadata['channels']
                   if metadata['channels'][key].get('selection') not in (None, 'fixed geometric midpoint')]
    diagnostic_text = ', '.join(escape(key) for key in diagnostics) or 'none supplied'
    content = '<p>No eligible fixed origins: 240, 360 or 480 s require 120 s of future coverage.</p>'
    if results:
        content = _plot(arrays, results, metadata) + _metrics_table(results, metadata) + _origin_details(results)
    return f'''<!doctype html><html lang="en"><meta charset="utf-8">
<title>Simulated installation forecast benchmark</title>
<style>body{{font:16px sans-serif;max-width:1250px;margin:2rem auto;padding:1rem;color:#173042}}
table{{border-collapse:collapse;width:100%;font-size:13px}}td,th{{border:1px solid #ccd;padding:6px;text-align:left}}
th{{background:#eef3f7}}svg{{width:100%;height:auto}}.notice{{padding:1rem;background:#fff1d6}}</style>
<h1>SIMULATED installation forecast benchmark</h1>
<p class="notice">Offline simulated traces, not live monitoring or an approved operating limit.
Engineering acceptance: NOT EVALUATED. Component capacities remain unverified.</p>
<h2>Method and evidence</h2><p>Fixed origins: 240, 360 and 480 s where full future coverage exists.
Training ends at NOW; the next sample starts the withheld future. Plot origin: 360 s if eligible,
otherwise the first eligible fixed origin. No best-origin selection is performed.</p>
<p>Sampling: every 0.5 s (direct decimation; no anti-alias filter). AR order 40, ridge 0.001;
history-only standardization and fitting. Hyperparameters are fixed, not selected on future errors.
Metrics pool samples from NOW to each 30/60/120 s horizon across all eligible origins;
overlapping windows are not independent trials. Bias is prediction minus observation.
Correlation is undefined for constant series. No advantage is claimed unless observed RMSE
is below both naive baselines; lower error does not establish operational validity.</p>
<p>Diagnostic whole-record selection channels: {diagnostic_text}.
No independent forecast evidence is attributed to these diagnostic governing arcs; they are excluded.</p>
<p>Trace SHA-256: {escape(metadata['trace_sha256'])}<br>
Simulation SHA-256 (metadata reference): {escape(str(metadata.get('simulation_sha256', 'not supplied')))}</p>
{content}<h2>Limitations</h2><p>No field measurements, latency test, forecast uncertainty,
snap-load convergence or component-capacity acceptance is demonstrated. No engineering thresholds
are plotted. Forecast errors retain the source channel units.</p></html>'''


def generate_report(run_dir, output):
    """Verify NPZ bytes against metadata and write a standalone HTML benchmark."""
    arrays, metadata = _load(run_dir)
    results = _evaluate(arrays, metadata)
    html = _document(arrays, metadata, results)
    output = Path(output)
    if output.exists():
        raise FileExistsError(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(html, encoding='utf-8')
    if output.read_text(encoding='utf-8') != html:
        raise ValueError('Report readback mismatch')
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    print(generate_report(args.run_dir, args.output))


if __name__ == '__main__':
    main()
