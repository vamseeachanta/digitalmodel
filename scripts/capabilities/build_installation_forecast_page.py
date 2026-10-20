"""Render only reviewed forecast series; no solver imports or recomputation.

The SVG scaling follows installation_envelope_dashboard.drawChart. Input paths
are command-line arguments and are never embedded in the published artifact.
"""
import argparse
import hashlib
from html import escape
import json
from pathlib import Path

LABEL = 'SIMULATED DEMO, not connected offshore'
CRITERIA = ('Acceptance criteria come from a workbook whose applicability is '
            'unresolved; alert levels are provisional.')


def digest(raw):
    return hashlib.sha256(raw).hexdigest()


def chart(channel, now):
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
    x = lambda t: 65 + (t - start) / (end - start) * 915
    y = lambda v: 245 - (v - low) / (high - low) * 215
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


def render(payload, config):
    # The reviewed alert demonstration is the scenario with the largest Hs.
    selected = max(config['scenarios'], key=lambda row: row['hs_m'])
    scenario, = [s for s in payload['demo']['scenarios']
                 if all(s[k] == selected[k] for k in ('hs_m', 'tp_s'))]
    frames, sections = [], []
    for frame in scenario['frames']:
        channels, figures = [], []
        for source in frame['channels']:
            channel = {key: source[key] for key in
                       ('label', 'units', 'history', 'forecast', 'assumed_limit')}
            figures.append(chart(channel, frame['now_s']))
            if channel['assumed_limit'] is not None:
                figures.append(f'<p>Provisional load limit: {channel["assumed_limit"]} '
                               f'{escape(channel["units"])}.</p>')
            if 'exceedance' in source:
                e = source['exceedance']
                channel['exceedance'] = {k: e[k] for k in ('window_probability', 'alert_probability',
                                                         'alert', 'status', 'calibration_windows', 'horizon_s')}
                assert e['alert_probability'] == config['exceedance']['alert_probability']
                assert e['horizon_s'] == channel['forecast']['times'][-1] - frame['now_s']
                figures.append('<p>Provisional exceedance probability: '
                               f'{e["window_probability"]}; alert threshold: {e["alert_probability"]}; '
                               f'alert: {str(e["alert"]).lower()}. Calibration windows: {e["calibration_windows"]}.</p>')
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
<p>Source criteria originate from a splash-zone workbook; applicability to this deep-submerged
arrangement remains unresolved. Load-path proxies and hardware mapping are unconfirmed.
Crane, geometric slack, re-tension, interference and whole-line criteria remain unresolved.</p>
<p>Forecast sampling can miss short peaks between samples. No offshore forecast skill
or operational risk calibration is established by this demonstration.</p>
</header>{''.join(sections)}
<footer>Only the selected history-only forecast series are retained. No live sensor connection,
wave-preview forecast, withheld truth or full envelope dataset is published.</footer>
<script type="application/json" id="forecast-data">{encoded}</script></main></body></html>
'''


def build(payload_path, config_path, output, scan_path):
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
