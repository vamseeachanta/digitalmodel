"""Immutable, evidence-checked partial installation study report."""
from __future__ import annotations

import argparse
from collections import Counter
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from hashlib import sha256
from html import escape
import json
import os
from pathlib import Path

from digitalmodel.workflows.installation_seastate_report import _case, _read
from digitalmodel.workflows.installation_report_sections import front_sections, pending_sections, marketing_section


def collect_cases(snapshot, matrix, *, workers=1):
    """Inspect completed cases only; preserve the captured campaign state."""
    if isinstance(workers, bool) or not isinstance(workers, int) or workers < 1:
        raise ValueError('workers must be a positive integer')
    captured = {row['index']: row for row in snapshot['cases']}
    if len(captured) != len(snapshot['cases']) or any(i < 0 or i >= len(matrix['cases']) for i in captured):
        raise ValueError('Duplicate or out-of-range campaign index')
    for index, row in captured.items():
        if any(row.get(key) != matrix['cases'][index][key] for key in ('hs_m', 'tp_s', 'seed')):
            raise ValueError('Campaign coordinates differ from matrix')
    jobs = [(i, case, captured.get(i, {'status': 'MISSING'}), matrix)
            for i, case in enumerate(matrix['cases'])]
    if workers == 1:
        return [_collect_case(*job) for job in jobs]
    with ThreadPoolExecutor(max_workers=workers) as executor:
        return list(executor.map(lambda job: _collect_case(*job), jobs))


def _collect_case(index, case, current, matrix):
    row = dict(index=index, **{k: case[k] for k in ('hs_m', 'tp_s', 'seed')})
    row.update(status=current['status'], reason='Campaign snapshot; acceptance not evaluated')
    if current['status'] == 'COMPLETED':
        metadata_path = Path(current.get('run_dir', '.')) / 'installation_traces/metadata.json'
        before = metadata_path.read_bytes() if metadata_path.exists() else None
        row = _case(index, case, current, matrix)
        if 'trace_sha256' in row and row['status'] != 'FAILED':
            metadata_path = Path(current['run_dir']) / 'installation_traces/metadata.json'
            raw = metadata_path.read_bytes()
            if raw != before:
                raise ValueError('Metadata changed during verification')
            metadata = json.loads(raw)
            if metadata['trace_sha256'] != row['trace_sha256']:
                raise ValueError('Metadata changed during verification')
            row.update(status='VERIFIED', channels=_compact_channels(metadata['channels']),
                       metadata_sha256=sha256(raw).hexdigest(),
                       limitations=metadata.get('limitations', []))
        elif row['status'] != 'FAILED':
            row['status'] = 'INCOMPLETE'
    return row


def _compact_channels(channels):
    for channel in channels.values():
        for event in channel.get('events', {}).values():
            if isinstance(event, dict):
                event.pop('events', None)
    return channels


def _point(row, value):
    return dict(index=row['index'], hs_m=row['hs_m'], tp_s=row['tp_s'], value=value)


def component_envelopes(rows):
    """Retain each component/channel and governing coordinates separately."""
    groups = {}
    for row in rows:
        for key, channel in row.get('channels', {}).items():
            group = groups.setdefault(key, dict(channel=key, object=channel['object'],
                variable=channel['variable'], units=channel['units'],
                position=channel.get('position', 'Selected arc; see case details')))
            if any(group[field] != channel[field] for field in ('object', 'variable', 'units')):
                raise ValueError('Inconsistent channel identity or units')
            for metric, operation in (('minimum', min), ('maximum', max)):
                point = _point(row, channel[metric])
                point['position'] = channel.get('position', 'Not recorded')
                group[metric] = operation([group.get(metric, point), point], key=lambda p: p['value'])
            if 'events' in channel:
                point = _point(row, channel['events']['low_tension']['total_duration_s'])
                group['low_duration'] = max([group.get('low_duration', point), point],
                                            key=lambda p: p['value'])
    return list(groups.values())


def _location(point):
    return f"Hs {point['hs_m']:g} / Tp {point['tp_s']:g}; case {point['index']:03d}"


def _value(point):
    position = escape(str(point.get('position', '')))
    return f"<strong>{point['value']:.3f}</strong><small>{_location(point)}<br>{position}</small>"


def _table(headers, rows):
    return ('<div class="scroll"><table><thead><tr>' +
            ''.join(f'<th>{escape(h)}</th>' for h in headers) + '</tr></thead><tbody>' +
            ''.join('<tr>' + ''.join(f'<td>{cell}</td>' for cell in row) + '</tr>' for row in rows) +
            '</tbody></table></div>')


def _grid(cases):
    periods = sorted({r['tp_s'] for r in cases})
    heights = sorted({r['hs_m'] for r in cases})
    lookup = {(r['hs_m'], r['tp_s']): r for r in cases}
    rows = []
    for height in heights:
        cells = [f'{height:g}']
        for period in periods:
            row = lookup[height, period]
            status = row['status']
            label = {'VERIFIED': '●', 'RUNNING': '◐', 'MISSING': '—'}.get(status, '!')
            cells.append(f'<span class="{escape(status.lower())}" title="{escape(status)}">{label}</span>')
        rows.append(cells)
    return _table(['Hs (m) / Tp (s)'] + [f'{p:g}' for p in periods], rows)


def _envelope_table(envelopes, tensions):
    rows = []
    for channel in envelopes:
        if (channel['variable'] == 'Effective tension') != tensions:
            continue
        if not tensions and channel['object'] == 'Environment':
            continue
        if not tensions and channel['variable'] in ('span_m', 'span_rate_m_per_s',
                                                     'unstretched_length_minus_span_m'):
            continue
        label = f"{channel['object']} · {channel['position']}"
        row = [escape(label), escape(channel['variable']), escape(channel['units']),
               _value(channel['minimum']), _value(channel['maximum'])]
        if tensions:
            row.append(_value(channel['low_duration']))
        rows.append(row)
    headers = ['Component / location', 'Quantity', 'Unit', 'Minimum / governing case', 'Maximum / governing case']
    return _table(headers + (['Maximum total ≤0 duration (s) / case'] if tensions else []), rows)


def _sling_rows(cases):
    rows = []
    for case in cases:
        channels = case.get('channels', {})
        channel = channels.get('Sling#5_end_B')
        if channel is None:
            continue
        event = channel['events']['low_tension']
        chord = channels.get('Sling#5_unstretched_length_minus_span_m', {}).get('maximum')
        rows.append([f"{case['index']:03d}", f"{case['hs_m']:g}", f"{case['tp_s']:g}",
                     f"{channel['minimum']:.3f}", f"{channel['maximum']:.3f}",
                     f"{event['total_duration_s']:.3f}", str(event['event_count']),
                     f"{event['maximum_duration_s']:.3f}",
                     'Not extracted' if chord is None else f'{chord:.4f}'])
    return _table(['Case', 'Hs (m)', 'Tp (s)', 'Min (kN)', 'Peak (kN)', 'Total ≤0 (s)',
                   'Events', 'Longest event (s)', 'Max chord deficit (m)'], rows)


def _scatter(cases):
    points = []
    for case in cases:
        channel = case.get('channels', {}).get('Sling#5_end_B')
        if channel:
            points.append((case, channel['events']['low_tension']['total_duration_s']))
    dots = ''.join(f'<circle cx="{55 + row["tp_s"] * 29:.2f}" cy="{245 - value * .34:.2f}" '
                   f'r="4" fill="hsl({210 - row["hs_m"] * 45:.0f} 65% 43%)">'
                   f'<title>Hs {row["hs_m"]:g} m; Tp {row["tp_s"]:g} s; {value:.3f} s</title></circle>'
                   for row, value in points)
    ticks = ''.join(f'<text x="{55 + p * 29}" y="268">{p}</text>' for p in range(4, 17, 2))
    yticks = ''.join(f'<text x="5" y="{249 - y * .34}">{y}</text>' for y in (0, 200, 400, 600))
    return (f'<svg viewBox="0 0 600 300" role="img" aria-label="Sling 5 End B low-tension duration by Tp">'
            '<path d="M55 30V245H545" fill="none" stroke="#637383"/>' + dots + ticks + yticks +
            '<text x="245" y="293">Peak period Tp (s)</text>'
            '<text x="60" y="20">Total tension ≤0 duration (s), per 600 s record</text></svg>')


def _case_details(cases, base):
    parts = []
    for case in cases:
        if case['status'] != 'VERIFIED':
            continue
        report = Path(case['run_dir']) / 'extracted/report.html'
        link = escape(os.path.relpath(report, base).replace('\\', '/'), quote=True)
        rows = []
        for key, channel in case['channels'].items():
            rows.append([escape(key), escape(str(channel.get('position', ''))), escape(channel['units']),
                         f"{channel['minimum']:.4g}", f"{channel['maximum']:.4g}"])
        parts.append(f'<details><summary>Case {case["index"]:03d} · Hs {case["hs_m"]:g} m · '
                     f'Tp {case["tp_s"]:g} s · {len(rows)} channels</summary>'
                     f'<p><a href="{link}">Original extraction report</a> — its legacy executive '
                     'summary can incorrectly state no data; supplemental channels appear below.</p>' +
                     _table(['Channel', 'Position / selected arc', 'Unit', 'Min', 'Max'], rows) +
                     f'<p class="hash">Simulation SHA-256: {escape(str(case["simulation_sha256"]))}<br>'
                     f'Traces SHA-256: {escape(str(case["trace_sha256"]))}</p></details>')
    return ''.join(parts)


STYLE = '''body{font:15px/1.5 system-ui,sans-serif;color:#203549;background:#eef3f7;margin:0}
main{max-width:1200px;margin:auto;padding:32px}header{background:#102f48;color:white;padding:28px;border-radius:12px}
h1{font-size:32px;line-height:1.2;margin:10px 0}h2{margin:0 0 16px;font-size:23px}
section{background:white;border:1px solid #d8e2eb;padding:24px;margin:22px 0;border-radius:10px}
.tag{font-size:12px;letter-spacing:2px;text-transform:uppercase}.cards{display:flex;gap:14px;flex-wrap:wrap;margin:20px 0}
.card{background:white;border:1px solid #d8e2eb;border-radius:10px;padding:16px 24px;flex:1;min-width:120px}
.card b{font-size:32px;display:block}.notice{border-left:5px solid #d68d22;padding:12px 18px;background:#fff7e8}
.scroll{overflow:auto}table{border-collapse:collapse;width:100%;font-size:13px}td,th{text-align:left;padding:10px;border-bottom:1px solid #dce5eb;vertical-align:top}
th{background:#eef3f7}small{display:block;color:#64788c;font-size:11px;white-space:nowrap}
.verified{color:#157d90;font-size:22px}.running{color:#be6a00;font-size:22px}.missing{color:#a5b2bc}
.failed{color:#ad2828}a{color:#006c99}details{border-top:1px solid #dce5eb;padding:13px 0}summary{cursor:pointer;font-weight:650}
.hash{font:11px/1.7 monospace;overflow-wrap:anywhere}svg{max-width:680px;width:100%;display:block}svg text{font:12px sans-serif;fill:#435568}
.caption{font-size:12px;color:#61758a}footer{font-size:12px;color:#61758a}@media print{body{background:white}section{break-inside:avoid}main{padding:0}}'''


def render_html(summary, links, base=Path('.')):
    counts, cases = summary['counts'], summary['cases']
    issue = ('Complete run-demand snapshot; engineering acceptance pending'
             if cases and all(case['status'] == 'VERIFIED' for case in cases) else 'Partial issue')
    if any(':' in url.split('/')[0] or url.startswith('//') for url in links.values()):
        raise ValueError('Report links must be relative paths')
    cards = ''.join(f'<div class="card"><b>{counts.get(status, 0)}</b>{label}</div>' for status, label in
                    [('VERIFIED', 'Verified completed'), ('RUNNING', 'Running at snapshot'),
                     ('MISSING', 'Queued / not run'), ('FAILED', 'Evidence failures'),
                     ('INCOMPLETE', 'Extraction incomplete')])
    references = ''.join(f'<li><a href="{escape(url, quote=True)}">{escape(name)}</a></li>' for name, url in links.items())
    return f'''<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>Jumper installation · engineering analysis</title>
<style>{STYLE}</style></head><body><main><header><div class="tag">Installation engineering / review issue</div>
<h1>Jumper installation<br>Engineering analysis report</h1><p>{issue} · simulated irregular waves · immutable snapshot {summary['created_utc']}</p>
</header><div class="cards">{cards}</div><div class="notice"><strong>Engineering acceptance: NOT EVALUATED.</strong>
No operating window has been established. Completed cells represent verified simulation evidence, not approved operating conditions.</div>
{front_sections(summary)}
<section id="results"><h2>5 · Results</h2><h3>5.1 Hs–Tp coverage</h3>{_grid(cases)}<p class="caption">Table 1. ● verified completed; ◐ running;
— queued / missing; ! evidence problem or extraction incomplete. Colours indicate execution state only.</p></section>
<section><h3>5.2 Component demand envelopes</h3>{_envelope_table(summary['envelopes'], True)}
<p class="caption">Table 2. Each end remains separate. Minimum, maximum and longest accumulated low-tension duration can govern
in different cases; each result includes its coordinates. Units are kN and seconds. Effective tension is not abbreviated as Te.</p></section>
<section><h3>5.3 Jumper and connector response</h3>{_envelope_table(summary['envelopes'], False)}
<p class="caption">Table 3. Native quantities and units are retained. Governing arcs are selected from each full simulated record
for diagnostic envelopes; they are not causal forecast locations. Minima are at the selected arc, not necessarily global spatial minima.
These are demand results without an approved utilisation calculation.</p></section>
<section><h3>5.4 Intentional slack: Sling 5</h3><p>Signed tension at or below zero identifies a model-response diagnostic.
It is not measured geometric slack or an allowable-compression criterion. Intended slack requires separate re-tension,
snap-load and interference assessment before an Hs–Tp operating boundary can be assigned.</p>{_scatter(cases)}
<p class="caption">Figure 1. Sling 5 End B low-tension duration by Tp; marker colour varies with Hs. Hover for case values.</p>
{_sling_rows(cases)}<p class="caption">Table 4. Chord deficit = unstretched line length minus endpoint span; sag and extension
contribute, so this is not physical slack length. Event durations are interpolated at each case's recorded sampling interval.</p></section>
{pending_sections(summary)}
<section><h2>6 · Criteria and two-minute forecasting</h2><p>The linked criteria records retain candidate capacities,
source editions and unresolved mappings. No candidate has been silently promoted to an approved limit.
The linked pilot demonstration separates history from a 120 s prediction and compares history-only predictions with persistence and mean baselines.
Forecast performance is not calculated by this report; linked results remain specific to that simulated pilot and do not validate offshore live forecasting.</p><ul>{references}</ul>
<p>Operating-window qualification will require applicable capacity and DNV criteria, resolution of the pipe strength discrepancy,
RAO coverage assessment, slack/snap and interference checks, and duration, time-step, seed and heading sensitivities.</p></section>
{marketing_section()}
<section id="appendix-a"><h2>Appendix A · Detailed results by case</h2>{_case_details(cases, base)}
<h3>A.1 Pending detailed results</h3><p>Detailed records for queued, running, failed or incomplete cases will populate here
after successful evidence verification. Their current states remain visible in Section 5.1.</p></section>
<section><h2>Appendix B · Provenance, references and limitations</h2><p>All status counts refer to the captured campaign manifest; later progress is
excluded. The JSON sidecar retains the captured manifest, channel metrics, source hashes and governing cases.
Failed evidence remains visible and is excluded from demand envelopes.</p><p class="hash">Matrix SHA-256: {summary['matrix_sha256']}<br>
Campaign snapshot SHA-256: {summary['campaign_sha256']}</p></section>
<footer>Reusable engineering results owner: private digitalmodel-data. Local retention does not establish remote backup.</footer></main></body></html>'''


def generate_report(campaign_path, matrix_path, output, links=None, *, workers=1):
    output, campaign_path, matrix_path = map(Path, (output, campaign_path, matrix_path))
    sidecar = output.with_suffix('.json')
    if output.exists() or sidecar.exists() or sidecar == output:
        raise FileExistsError('Report destination must be new')
    captured_utc = datetime.now(timezone.utc).isoformat()
    raw, matrix_raw = campaign_path.read_bytes(), matrix_path.read_bytes()
    snapshot, matrix = json.loads(raw), json.loads(matrix_raw)
    digest = sha256(matrix_raw).hexdigest()
    if snapshot['matrix_sha256'] != digest:
        raise ValueError('Campaign does not match matrix')
    cases = collect_cases(snapshot, matrix, workers=workers)
    summary = dict(created_utc=captured_utc,
                   campaign_sha256=sha256(raw).hexdigest(), matrix_sha256=digest,
                   campaign_snapshot=snapshot, engineering_acceptance='NOT EVALUATED',
                   counts=dict(Counter(row['status'] for row in cases)), cases=cases,
                   verification_workers=workers,
                   envelopes=component_envelopes(cases))
    html = render_html(summary, links or {}, output.parent)
    payload = json.dumps(summary, indent=2, allow_nan=False)
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open('x', encoding='utf-8') as stream:
        stream.write(html)
    with sidecar.open('x', encoding='utf-8') as stream:
        stream.write(payload)
    if output.read_text(encoding='utf-8') != html or _read(sidecar) != summary:
        raise ValueError('Report readback mismatch')
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--campaign', required=True, type=Path)
    parser.add_argument('--matrix', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--workers', type=int, default=1)
    args = parser.parse_args()
    result = generate_report(args.campaign, args.matrix, args.output, workers=args.workers)
    print(json.dumps(result['counts']))


if __name__ == '__main__':
    main()
