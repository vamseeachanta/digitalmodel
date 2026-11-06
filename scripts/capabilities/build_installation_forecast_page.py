# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Render only reviewed forecast series; no solver imports or recomputation.

The SVG scaling follows installation_envelope_dashboard.drawChart. Input paths
are command-line arguments and are never embedded in the published artifact.
"""
import argparse
import hashlib
from html import escape
import json
import math
from pathlib import Path

LABEL = 'SIMULATED DEMO, not connected offshore'
CRITERIA = ('Load limits are provisional demonstration assumptions; '
            'engineering applicability is unresolved.')
FORECAST_SETTINGS = {'order': 40, 'ridge': 0.001, 'sample_interval_s': 0.5,
                     'horizon_s': 120, 'stride_samples': 10, 'min_calibration_windows': 10,
                     'trace_start_s': 0}


def calibration_geometry(settings: dict) -> tuple[int, int, int, str]:
    """Derive the complete-window bounds and displayed rule from the settings."""
    first = 3 * settings['order'] - 1
    steps = round(settings['horizon_s'] / settings['sample_interval_s'])
    last = first + steps
    clock = 'NOW' if settings['trace_start_s'] == 0 else f'(NOW - {settings["trace_start_s"]:g})'
    rule = (f'N = floor(({clock} / {settings["sample_interval_s"]:g} - {last}) / '
            f'{settings["stride_samples"]}) + 1')
    return first, steps, last, rule


NUMERICAL_BASIS: dict = {
    'revision': 3,
    'status': 'provisional; no basis recorded',
    'kN_per_Te': 9.81,
    'kN_per_Te_source': 'NIST standard acceleration of gravity '
                        '(https://physics.nist.gov/cgi-bin/cuu/Value?gn): '
                        '9.80665 m/s^2, rounded to 9.81 for the retained conversion.',
    'load_limits_Te': {'Master-link load-path proxy: TopRigging End B': 17.7,
                       'Sling2 End B tension': 10.89},
    'load_limit_sources': {
        'Master-link load-path proxy: TopRigging End B':
            'Retained reviewed demonstration input for the master-link proxy; original derivation is unverified.',
        'Sling2 End B tension':
            'Retained reviewed demonstration input for the sling; original derivation is unverified.',
    },
    'forecast_settings': FORECAST_SETTINGS,
    'calibration_count_rule': calibration_geometry(FORECAST_SETTINGS)[3],
    'alert_probability': 0.2,
    'load_limit_rule': 'limit_kN = assumed_limit_Te * kN_per_Te',
    'probability_rule': 'p = count(max(forecast + error_trajectory) > limit) / window_count',
    'alert_rule': 'alert = (p >= alert_probability)',
    'implementation_references': [
        'src/digitalmodel/workflows/installation_monitoring_demo.py:resolve_channels',
        'src/digitalmodel/workflows/installation_forecast.py:exceedance_forecast',
    ],
    'limitations': [
        'The conversion is a demonstration convention, not a certified component capacity or a standards criterion.',
        'The alert cutoff is illustrative; no offshore risk tolerance or operational calibration is established.',
        'Calibration error trajectories are not published; individual recorded probabilities cannot be independently recomputed.',
        'The worked example is hypothetical arithmetic, not the calibration data for any published frame.',
    ],
    'worked_example': {
        'forecast_kN': [100, 110],
        'limit_kN': 110,
        'error_trajectories_kN': [[-1, 0], [10, 0], [0, 1], [11, 0], [12, 0]],
        'window_probability': 0.6,
        'alert': True,
    },
}


def validate_numerical_basis(scenario: dict, config: dict) -> None:
    """Reject inputs that contradict the published demonstration assumptions."""
    basis = NUMERICAL_BASIS
    if config['exceedance']['alert_probability'] != basis['alert_probability']:
        raise ValueError('Alert cutoff differs from the numerical basis')
    for frame in scenario['frames']:
        for channel in frame['channels']:
            limit = channel['assumed_limit']
            if limit is None:
                continue
            assumed = basis['load_limits_Te'].get(channel['label'])
            if (assumed is None or channel['units'] != 'kN'
                    or not math.isclose(limit, assumed * basis['kN_per_Te'], rel_tol=0, abs_tol=1e-9)):
                raise ValueError('Load limit differs from the numerical basis')
            alert = channel['exceedance']
            if (alert['alert_probability'] != basis['alert_probability']
                    or alert['alert'] is not (alert['window_probability'] >= basis['alert_probability'])):
                raise ValueError('Alert result differs from the numerical basis')


def numerical_basis_html(frames: list[dict]) -> str:
    """Expose the assumptions and a hand-worked example independently of the series."""
    basis, rows = NUMERICAL_BASIS, []
    for label, limit in basis['load_limits_Te'].items():
        rows.append(f'<tr><td>{escape(label)}</td><td>{limit:.3f}</td>'
                    f'<td>{limit * basis["kN_per_Te"]:.3f}</td>'
                    f'<td>{escape(basis["load_limit_sources"][label])}</td></tr>')
    settings = basis['forecast_settings']
    first, steps, last, count_rule = calibration_geometry(settings)
    counts = [max(0, (round((frame['now_s'] - settings['trace_start_s']) /
                           settings['sample_interval_s']) - last) // settings['stride_samples'] + 1)
              for frame in frames]
    counts_text = ', '.join(map(str, counts[:-1])) + (' and ' if len(counts) > 1 else '')
    counts_text += str(counts[-1]) if counts else '0'
    example = basis['worked_example']
    example_rows = ''.join(
        f'<tr><td>{errors[0]:g}, {errors[1]:g}</td>'
        f'<td>{max(f + e for f, e in zip(example["forecast_kN"], errors)):g}</td></tr>'
        for errors in example['error_trajectories_kN'])
    limitations = ''.join(f'<li>{escape(item)}</li>' for item in basis['limitations'])
    references = ''.join(f'<li><code>{escape(item)}</code></li>' for item in basis['implementation_references'])
    return f'''<section><h2>Numerical basis and assumptions</h2>
<p>The load-limit basis status is "{basis['status']}". These are provisional demonstration
settings; no component capacity, workbook applicability or operational acceptance is established.</p>
<p>Load conversion: limit (kN) = assumed limit (Te) × {basis['kN_per_Te']:.3f} kN/Te.
Te denotes the tonne-force equivalent used by this demonstration.</p>
<p>Conversion source: {escape(basis['kN_per_Te_source'])}</p>
<table><thead><tr><th>Load channel</th><th>Assumed limit (Te)</th><th>Converted limit (kN)</th><th>Source</th></tr></thead>
<tbody>{''.join(rows)}</tbody></table><p>Table 2. Assumed loads and their conversion.</p>
<p>History-only ridge autoregression; fixed order {settings['order']}, ridge {settings['ridge']:.3f}.
Sampling interval: {settings['sample_interval_s']:g} s; horizon: {settings['horizon_s']:g} s;
calibration stride: {settings['stride_samples']} samples;
minimum calibration: {settings['min_calibration_windows']} windows. The trace starts at {settings['trace_start_s']} s.</p>
<p>The first back-test origin is sample 3 * {settings['order']} - 1 = {first}; its {steps}-sample horizon
ends at sample {last}. For the displayed origins, {count_rule}.
This gives {counts_text} complete windows. Displayed exceeding-window counts
are reconstructed as k = round(p * N) from the retained unrounded probabilities;
the unpublished error trajectories are required to reproduce which windows exceed.</p>
<p>The alert cutoff is {basis['alert_probability']:.1%}, also written as {basis['alert_probability']:.3f}.
For each complete calibration window, its joint error trajectory is added to the current forecast.
The window exceeds the load limit only if its maximum is strictly greater than the limit.
The empirical probability is the number of exceeding windows divided by the window count;
an alert occurs when that probability is greater than or equal to the cutoff.</p>
<h3>Independently worked arithmetic example</h3>
<p>A hypothetical two-sample forecast is 100 and 110 kN, with a 110 kN load limit.
Adding the following five error trajectories gives the maxima shown.</p>
<table><thead><tr><th>Error trajectory (kN)</th><th>Resulting maximum (kN)</th></tr></thead>
<tbody>{example_rows}</tbody></table><p>Table 3. Hypothetical trajectory arithmetic.</p>
<p>Two maxima equal the limit and do not exceed it. Three maxima exceed it:
3 / 5 = 60.0 % exceeds the 20.0 % cutoff, so the example alert is true.
Exactly 1 / 5 = 20.0 % would also trigger an alert under the inclusive cutoff rule.</p>
<ul>{limitations}</ul><p>Implementation references establish the arithmetic rules,
not the engineering suitability of the assumptions:</p><ul>{references}</ul></section>'''


def digest(raw: bytes) -> str:
    """Return the raw-byte SHA-256 digest."""
    return hashlib.sha256(raw).hexdigest()


def chart(channel: dict, now: float) -> str:
    """Render the reviewed history and forecast with shared axis scaling."""
    history, forecast = channel['history'], channel['forecast']
    assert history['times'][-1] == now
    assert forecast['times'][0] > now
    values = history['values'] + forecast['values']
    limit = channel.get('assumed_limit')
    if limit is not None:
        values += [limit]
    low, high = min(values), max(values)
    pad = max((high - low) * .12, .01)
    low, high = low - pad, high + pad
    start, end = history['times'][0], forecast['times'][-1]

    def x(t: float) -> float:
        return float(65 + (t - start) / (end - start) * 915)

    def y(v: float) -> float:
        return float(245 - (v - low) / (high - low) * 215)

    elements = []
    for series, color in ((history, '#173e56'), (forecast, '#147fac')):
        assert len(series['times']) == len(series['values'])
        points = ' '.join(f'{x(t):.3f},{y(v):.3f}' for t, v in zip(series['times'], series['values']))
        elements.append(f'<polyline points="{points}" fill="none" stroke="{color}" stroke-width="2"/>')
    if limit is not None:
        elements.append(f'<path d="M65 {y(limit):.3f} H980" stroke="#c07716" stroke-dasharray="8 4"/>')
    elements.append(f'<path d="M{x(now):.3f} 20 V245" stroke="#a62942" stroke-dasharray="6 4"/>')
    for t in (start, now, end):
        elements.append(f'<text x="{x(t):.3f}" y="267" text-anchor="middle">{t:g} s</text>')
    for i in range(5):
        value = low + (high - low) * i / 4
        elements.append(f'<text x="57" y="{y(value):.3f}" text-anchor="end">{value:.2f}</text>')
    label = escape(channel['label'])
    return (f'<h3>{label} ({escape(channel["units"])})</h3>'
            f'<svg viewBox="0 0 1000 290" role="img" aria-label="{label}: history and forecast">'
            + ''.join(elements) + '</svg>')


def render(payload: dict, config: dict) -> str:
    """Render the selected scenario without recomputing its forecast values."""
    # The reviewed alert demonstration is the scenario with the largest Hs.
    selected = max(config['scenarios'], key=lambda row: row['hs_m'])
    scenario, = [s for s in payload['demo']['scenarios']
                 if all(s[k] == selected[k] for k in ('hs_m', 'tp_s'))]
    validate_numerical_basis(scenario, config)
    frames, sections = [], []
    for frame in scenario['frames']:
        channels, figures = [], []
        for source in frame['channels']:
            channel = {key: source[key] for key in
                       ('label', 'units', 'history', 'forecast', 'assumed_limit')}
            figures.append(chart(channel, frame['now_s']))
            if channel['assumed_limit'] is not None:
                figures.append(f'<p>Provisional load limit: {channel["assumed_limit"]:.3f} '
                               f'{escape(channel["units"])}.</p>')
            if 'exceedance' in source:
                e = source['exceedance']
                channel['exceedance'] = {k: e[k] for k in ('window_probability', 'alert_probability',
                                                         'alert', 'status', 'calibration_windows', 'horizon_s')}
                assert e['alert_probability'] == config['exceedance']['alert_probability']
                assert e['horizon_s'] == channel['forecast']['times'][-1] - frame['now_s']
                figures.append('<p>Provisional exceedance probability: '
                               f'{e["window_probability"]:.3f}; alert threshold: {e["alert_probability"]:.3f}; '
                               f'alert: {str(e["alert"]).lower()}. '
                               f'{round(e["window_probability"] * e["calibration_windows"])} of '
                               f'{e["calibration_windows"]} windows exceed the limit. '
                               f'Calibration windows: {e["calibration_windows"]}.</p>')
                peak = max(channel['forecast']['values'])
                limit = channel['assumed_limit']
                if e['alert'] is True and limit is not None and peak < limit:
                    figures.append(f'<p>At NOW {frame["now_s"]:g} s the plotted forecast peak of {peak:.1f} kN is '
                                   f'{limit - peak:.1f} kN below the {limit:.1f} kN limit; '
                                   'the alert comes from the back-test error spread.</p>')
            channels.append(channel)
        frames.append({'now_s': frame['now_s'], 'channels': channels})
        sections.append(f'<section><h2>NOW {frame["now_s"]:g} s</h2>' + ''.join(figures) + '</section>')
    retained = {'hs_m': scenario['hs_m'], 'tp_s': scenario['tp_s'], 'frames': frames}
    encoded = json.dumps(retained, separators=(',', ':'), allow_nan=False).replace('<', '\\u003c')
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>{LABEL}</title><style>
body{{font:16px/1.5 system-ui,sans-serif;background:#eef3f7;color:#203549;margin:0}}
main{{max-width:1200px;margin:auto;padding:24px}}header,section{{background:white;padding:24px;margin:20px 0;border-radius:12px}}
header{{border-top:8px solid #c07716}}h1{{font-size:28px}}svg{{width:100%;height:auto}}svg text{{font:12px sans-serif}}
</style></head><body><main><header><h1>{LABEL}</h1>
<p>{CRITERIA}</p><p>Engineering acceptance: NOT EVALUATED.</p>
<h2>Two-minute installation forecast: Hs {scenario['hs_m']:g} m / Tp {scenario['tp_s']:g} s</h2>
<p>History-only autoregression. Dark: simulated history; blue: forecast; red dashed: NOW;
orange dashed: provisional load limit. All recorded forecast origins are shown below.</p>
<p>This case was selected after inspection to demonstrate provisional exceedances;
its scores are illustrative, not an unbiased skill estimate. Probabilities use back-tested error
trajectories ending before NOW. Overlapping calibration windows are not independent trials.
The probabilities are empirical estimates, not qualified offshore risk predictions.</p>
<p>Workbook applicability to this deep-submerged arrangement remains unresolved;
the workbook is not established as the source of these numerical limits.
Load-path proxies and hardware mapping are unconfirmed.
Crane, geometric slack, re-tension, interference and whole-line criteria remain unresolved.</p>
<p>Forecast sampling can miss short peaks between samples. No offshore forecast skill
or operational risk calibration is established by this demonstration.</p>
</header><section><h2>Revision history</h2>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>
<tr><td>0</td><td>2026-10-20</td><td>Selected simulated forecast traces and provisional alerts are published for demonstration.</td></tr>
<tr><td>1</td><td>2026-10-20</td><td>Numerical assumptions and a worked example are added to make the arithmetic inspectable.</td></tr>
<tr><td>2</td><td>2026-10-20</td><td>Limit sources, forecast settings and empirical window counts are clarified without changing the retained series.</td></tr>
<tr><td>3</td><td>2026-10-21</td><td>Unsupported basis claims are removed, the conversion source is cited, and calibration arithmetic and alert explanations are derived from their inputs.</td></tr>
</tbody></table><p>Table 1. Page revisions.</p></section>
{numerical_basis_html(frames)}{''.join(sections)}
<footer>Only the selected history-only forecast series are retained. No live sensor connection,
wave-preview forecast, withheld truth or full envelope dataset is published.</footer>
<script type="application/json" id="forecast-data">{encoded}</script></main></body></html>
'''


def build(payload_path: Path, config_path: Path, output: Path, scan_path: Path) -> None:
    """Validate reviewed inputs and write the scanned page with its manifest."""
    raw, config_raw = payload_path.read_bytes(), config_path.read_bytes()
    payload, config = json.loads(raw), json.loads(config_raw)
    if digest(config_raw) != payload['provenance']['demo_config']['sha256']:
        raise ValueError('Config provenance digest mismatch')
    html = render(payload, config)
    terms = json.loads(scan_path.read_text(encoding='utf-8'))
    if not terms or any(term.casefold() in html.casefold() for term in terms):
        raise ValueError('Sanitization scan failed or empty')
    encoded = html.encode('utf-8')
    manifest = {'sha256': digest(encoded), 'size_bytes': len(encoded),
                'source_sha256': {'reviewed_payload': digest(raw), 'reviewed_config': digest(config_raw)},
                'numerical_basis': NUMERICAL_BASIS,
                'sanitization': {'findings': 0, 'terms_checked': len(terms),
                                 'substring_fingerprints': [
                                     {'length': len(term.casefold()),
                                      'sha256': digest(term.casefold().encode('utf-8'))}
                                     for term in terms]},
                'reduction': 'Selected history-only scenario; chart arrays and alert values only.'}
    manifest_text = json.dumps(manifest, indent=2) + '\n'
    if any(term.casefold() in manifest_text.casefold() for term in terms):
        raise ValueError('Manifest sanitization scan failed')
    output.write_bytes(encoded)
    output.with_suffix('.manifest.json').write_bytes(manifest_text.encode('utf-8'))
    assert output.read_bytes() == encoded


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('payload', 'config', 'output', 'scan_terms'):
        parser.add_argument('--' + name.replace('_', '-'), type=Path, required=True)
    args = parser.parse_args()
    build(args.payload, args.config, args.output, args.scan_terms)
