"""Render saved failed replay evidence without solving or changing its verdict."""
from hashlib import sha256
from html import escape
import json
import math
import os
from pathlib import Path

from reportlab.lib import colors
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.platypus import SimpleDocTemplate, Paragraph, Table, TableStyle, PageBreak, Spacer


def _load(paths):
    records, sources = [], []
    for path in paths:
        raw = path.read_bytes()
        records.append(json.loads(raw))
        sources.append(dict(name=path.name, path=path, sha256=sha256(raw).hexdigest()))
    return records, sources


def _validate(lineage, summary, comparison):
    if lineage.get('status') != 'failed' or comparison.get('passed') is not False:
        raise ValueError('Saved failed lineage and comparison required')
    rows = comparison.get('metrics', [])
    if not rows or not any(r.get('passed') is False for r in rows):
        raise ValueError('Nonempty failed metrics required')
    cases = summary.get('cases', [])
    if len(cases) != 1 or cases[0]['index'] != lineage['manifest']['case_index']:
        raise ValueError('Selected case identity differs')
    if comparison.get('settings') != lineage['manifest'].get('comparison'):
        raise ValueError('Saved tolerance settings differ')
    for row in rows:
        if type(row.get('passed')) is not bool or row['channel'] not in cases[0]['channels']:
            raise ValueError('Invalid metric verdict or channel identity')
        for key in ('actual', 'reference', 'absolute_difference', 'tolerance'):
            if type(row[key]) not in (int, float) or not math.isfinite(row[key]):
                raise ValueError('Finite numeric comparison required')
        if row['absolute_difference'] < 0 or row['tolerance'] < 0:
            raise ValueError('Negative difference or tolerance')
    if summary.get('engineering_acceptance') != 'NOT EVALUATED':
        raise ValueError('Diagnostic requires unqualified engineering acceptance')
    return cases[0]


def _units(row, case):
    metric = row['metric']
    if metric.endswith('_s'): return 's'
    if metric.endswith('_m'): return 'm'
    if metric.endswith('_kN'): return 'kN'
    if metric.endswith('event_count'): return 'count'
    if metric in {'minimum', 'maximum', 'spatial_minimum', 'spatial_maximum'} or metric.endswith(('threshold', 'signed_tension')):
        return case['channels'][row['channel']].get('units', 'not recorded')
    return 'not established'


def _context(lineage, case, comparison):
    manifest = lineage['manifest']
    reference = manifest['inputs']['reference_summary']
    pilot = str(case.get('run_dir', 'Not recorded')).replace('\\', '/').rstrip('/').split('/')
    result = [('Pilot identifier', pilot[-2] if len(pilot) > 1 else pilot[0]),
              ('Reference summary (pinned)', reference['path']), ('Reference SHA-256', reference['sha256']),
              ('Case index (fresh and reference)', case['index']), ('Hs (m)', case['hs_m']), ('Tp (s)', case['tp_s']),
              ('Random seed', case.get('seed', 'Not recorded')), ('Heading (deg)', case.get('heading_degrees', 'Not recorded')),
              ('Solver version', lineage.get('solver', {}).get('resolved_version', manifest.get('runtime', {}).get('solver_version', 'Not recorded'))),
              ('Code revision', manifest.get('code', {}).get('git_revision', 'Not recorded')),
              ('Started UTC', lineage.get('started_utc', 'Not recorded')), ('Finished UTC', lineage.get('finished_utc', 'Not recorded')),
              ('Simulation disposition (saved)', lineage.get('simulation_disposition', 'Not recorded'))]
    labels = {'gamma': 'gamma (dimensionless)', 'components': 'Wave components (count)'}
    result.extend((labels.get(key, key), case['settings'][key]) for key in sorted(case.get('settings', {})) if key not in {'hs_m', 'tp_s', 'seed'})
    return result


def _failure_rows(comparison):
    return [row for row in comparison['metrics'] if row['passed'] is False]


def _message(lineage, comparison):
    count, total = len(_failure_rows(comparison)), len(comparison['metrics'])
    return (f'{count} of {total} saved metric comparisons exceed their recorded tolerances. '
            'Numerical reproduction FAILED. Engineering acceptance is NOT EVALUATED. '
            'The saved error reports a failure before report completion: ' + str(lineage.get('error', 'Not recorded')) + '. '
            'Matching input settings do not establish identical numerical results. '
            'The saved lineage records retention of any created simulation for diagnosis; this renderer does not inspect or delete it.')


def _row_values(row, case):
    return [row['channel'], row['metric'], _units(row, case)] + [f'{row[k]:.9g}' for k in ('reference', 'actual', 'absolute_difference', 'tolerance')]


def _html(lineage, case, comparison, sources, output):
    headers = ['Channel', 'Metric', 'Unit', 'Reference', 'Fresh', 'Absolute difference', 'Saved tolerance']
    rows = ''.join('<tr>' + ''.join(f'<td>{escape(v)}</td>' for v in _row_values(r, case)) + '</tr>' for r in _failure_rows(comparison))
    context = ''.join(f'<tr><th>{escape(str(k))}</th><td>{escape(str(v))}</td></tr>' for k, v in _context(lineage, case, comparison))
    evidence = ''.join(f'<li><a href="{escape(s["link"], quote=True)}">{escape(s["name"])}</a> — SHA-256 <code>{s["sha256"]}</code></li>' for s in sources)
    settings = comparison['settings']
    return f'''<!doctype html><html><head><meta charset="utf-8"><title>Failed replay diagnostic</title>
<style>body{{font:15px Arial;line-height:1.5;margin:35px;max-width:1400px}}h1{{color:#9a2a13}}table{{border-collapse:collapse;width:100%}}td,th{{padding:7px;border:1px solid #ccd3d8;text-align:left;overflow-wrap:anywhere}}code{{overflow-wrap:anywhere}}th{{background:#edf2f5}}.notice{{border:2px solid #ad421e;padding:15px}}</style></head><body>
<h1>FAILED numerical reproduction — diagnostic only</h1><p class="notice">{escape(_message(lineage, comparison))}</p>
<h2>1. Saved case and execution context</h2><table>{context}</table><p>Table 1. Saved lineage and selected case; units are stated in labels or field suffixes. Reference identity is transcribed from the pinned manifest, not independently re-read here.</p><p>Saved case status: {escape(str(case.get('status', 'Not recorded')))}. VERIFIED denotes source-evidence verification, not a passed numerical reproduction.</p>
<h2>2. Comparison basis</h2><p>Saved absolute tolerance: {settings['absolute_tolerance']}; saved relative tolerance: {settings['relative_tolerance']}. Recorded rule: absolute tolerance + relative tolerance × |reference|. Absolute tolerance has each metric's unit; relative tolerance is dimensionless. Each row below shows the saved combined tolerance in its stated unit. No values, tolerances or verdicts are recomputed.</p>
<p>Saved failure: {escape(str(lineage.get('error', 'Not recorded')))}</p>
<h2>3. All failed comparisons</h2><details open><summary>{len(_failure_rows(comparison))} failed metrics</summary><table><thead><tr>{''.join(f'<th>{h}</th>' for h in headers)}</tr></thead><tbody>{rows}</tbody></table></details><p>Table 2. Complete failed-metric list in saved comparison order.</p>
<h2>4. Evidence and disposition</h2><ul>{evidence}</ul><p>This render is valid only with a COMPLETED render-receipt.json and no .incomplete marker in its directory. Cause and remedy are not established by this diagnostic. The original failed pilot and historical evidence remain unchanged. This diagnostic is not an approved engineering report or an operating-limit assessment.</p></body></html>'''


def _paragraph(text, style):
    return Paragraph(escape(str(text)), style)


def _table(rows, widths, style):
    table = Table([[_paragraph(value, style) for value in row] for row in rows], colWidths=widths, repeatRows=1)
    table.setStyle(TableStyle([('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#edf2f5')),
                              ('GRID', (0, 0), (-1, -1), .3, colors.HexColor('#bcc8cf')),
                              ('VALIGN', (0, 0), (-1, -1), 'TOP'), ('BOTTOMPADDING', (0, 0), (-1, -1), 5)]))
    return table


def _pdf(lineage, case, comparison, sources, path):
    styles = getSampleStyleSheet()
    body, small = styles['BodyText'], styles['BodyText'].clone('Small')
    small.fontSize, small.leading = 7, 9
    flow = [_paragraph('FAILED numerical reproduction', styles['Title']),
            _paragraph('Diagnostic only — not an approved engineering report', styles['Heading2']),
            _paragraph(_message(lineage, comparison), body), Spacer(1, 12),
            _table([['Saved field', 'Value']] + _context(lineage, case, comparison), [175, 340], small),
            _paragraph('Table 1. Saved case and pinned reference identity, transcribed without re-reading the reference. VERIFIED case status denotes source-evidence verification, not numerical reproduction.', small), PageBreak()]
    failed = _failure_rows(comparison)
    selected = sorted(failed, key=lambda row: row['absolute_difference'] / row['tolerance'] if row['tolerance'] else float('inf'), reverse=True)[:12]
    flow += [_paragraph('Largest recorded tolerance exceedances', styles['Heading1']),
             _paragraph(f'{len(failed)} of {len(comparison["metrics"])} comparisons failed. Up to 12 rows are shown; the HTML diagnostic lists every failed row.', body)]
    flow.append(_table([['Channel / metric', 'Unit', 'Reference', 'Fresh', 'Difference', 'Tolerance']] +
                       [[r['channel'] + ' / ' + r['metric'], _units(r, case)] + _row_values(r, case)[3:] for r in selected],
                       [175, 50, 72, 72, 73, 73], small))
    flow += [_paragraph('Table 2. Ranked by recorded absolute difference / recorded tolerance; unlike units are not compared as raw differences.', small),
             _paragraph(f'Saved absolute tolerance: {comparison["settings"]["absolute_tolerance"]}; relative tolerance: {comparison["settings"]["relative_tolerance"]}. Recorded rule: absolute tolerance + relative tolerance × |reference|. Absolute tolerance carries each metric unit; relative tolerance is dimensionless. Row tolerances are copied unchanged.', body), PageBreak(),
             _paragraph('Evidence and diagnostic disposition', styles['Heading1'])]
    for source in sources:
        flow.extend([_paragraph(source['name'], styles['Heading3']), _paragraph('Source: ' + source['link'], small), _paragraph('SHA-256: ' + source['sha256'], small)])
    flow.extend([_paragraph('The full comparison is retained as comparison.json at the recorded source location. The companion HTML includes every failed row.', body),
                 _paragraph('Saved failure: ' + str(lineage.get('error', 'Not recorded')), body),
                 _paragraph('Engineering acceptance: NOT EVALUATED. Cause and remedy are not established. No tolerance was widened, no comparison recomputed, and no solver executed by this renderer. Any created simulation remains retained for diagnosis according to the saved lineage.', body),
                 _paragraph('Render validity requires a COMPLETED render-receipt.json and no .incomplete marker in the output directory.', body)])
    SimpleDocTemplate(str(path), pagesize=A4, leftMargin=40, rightMargin=40, topMargin=36, bottomMargin=36).build(flow)


def render_diagnostic(lineage_path, summary_path, comparison_path, output_dir):
    """Read saved evidence and create a separate, new diagnostic directory."""
    paths = [Path(p).resolve(strict=True) for p in (lineage_path, summary_path, comparison_path)]
    output = Path(output_dir).resolve()
    if output.exists(): raise FileExistsError(output)
    if any(output.is_relative_to(p.parent) for p in paths):
        raise ValueError('Diagnostic output must be outside the pilot/evidence tree')
    (lineage, summary, comparison), sources = _load(paths)
    for source in sources:
        try: source['link'] = Path(os.path.relpath(source['path'], output)).as_posix()
        except ValueError: source['link'] = source['path'].as_uri()
    case = _validate(lineage, summary, comparison)
    html = _html(lineage, case, comparison, sources, output)
    output.mkdir(parents=True, exist_ok=False)
    sentinel = output / '.incomplete'
    sentinel.write_text('INCOMPLETE: render not verified; artifacts must not be used.', encoding='utf-8')
    html_path, pdf_path = output / 'failed-replay-diagnostic.html', output / 'failed-replay-diagnostic.pdf'
    html_path.write_text(html, encoding='utf-8')
    _pdf(lineage, case, comparison, sources, pdf_path)
    if any(sha256(s['path'].read_bytes()).hexdigest() != s['sha256'] for s in sources):
        raise ValueError('Source evidence changed during rendering')
    receipt = dict(status='COMPLETED', sources=[dict(name=s['name'], sha256=s['sha256']) for s in sources],
                   outputs=[dict(name=p.name, sha256=sha256(p.read_bytes()).hexdigest()) for p in (html_path, pdf_path)])
    (output / 'render-receipt.json').write_text(json.dumps(receipt, indent=2), encoding='utf-8')
    sentinel.write_text('COMPLETED: see render-receipt.json.', encoding='utf-8')
    sentinel.rename(output / 'render-state.txt')
    return dict(html=html_path, pdf=pdf_path)
