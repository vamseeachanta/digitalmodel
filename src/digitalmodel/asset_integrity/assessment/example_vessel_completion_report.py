"""Render conditional vessel-example evidence without inventing solver outcomes."""
from __future__ import annotations

from html import escape
import json
import math
from urllib.parse import urlsplit

CRITERIA = ('membrane', 'membrane_plus_bending', 'local_failure')
SOURCE = 'https://www.asme.org/codes-standards/find-codes-standards/fitness-for-service-example-problem-manual'


def _text(value):
    return escape(str(value), quote=True)


def _number(value):
    if isinstance(value, (int, float)) and not isinstance(value, bool) and math.isfinite(value):
        return f'{value:.3f}'
    return 'NOT EVALUATED'


def _url(value):
    value = str(value)
    if urlsplit(value).scheme.lower() not in ('', 'https', 'http'):
        return '#'
    return _text(value)


def _table(headers, rows, caption):
    body = ''.join('<tr>' + ''.join(f'<td>{_text(v)}</td>' for v in row) + '</tr>' for row in rows)
    head = ''.join(f'<th>{_text(h)}</th>' for h in headers)
    return f'<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table><p class="caption">{_text(caption)}</p>'


def _diagram(labels, title):
    cells = []
    for index, label in enumerate(labels):
        x = 15 + index * 205
        cells.append(f'<rect x="{x}" y="15" width="185" height="60" rx="8"/>')
        cells.append(f'<text x="{x+92}" y="49" text-anchor="middle">{_text(label)}</text>')
        if index < len(labels)-1:
            cells.append(f'<path d="M{x+185} 45 h20 m-6 -5 l6 5 -6 5"/>')
    return (f'<svg viewBox="0 0 {len(labels)*205+10} 90" role="img" aria-label="{_text(title)}">'
            f'<title>{_text(title)}</title>{"".join(cells)}</svg>')


def _method_flow():
    labels = ('Area A: L1', 'Area B: L1 → L2', 'Area C: L1 → L2 → elastic L3 / rerate',
              'Area D: L1 → L2 → elastic L3 → repair')
    parts = ['<svg class="flow" viewBox="0 0 1000 310" role="img" aria-label="Four-area assessment branches">',
             '<title>Four independent assessment branches; outcomes require the result tables</title>',
             '<rect x="10" y="110" width="170" height="70" rx="8"/>',
             '<text x="95" y="150" text-anchor="middle">Four thickness grids</text>']
    for index, label in enumerate(labels):
        y = 15 + index * 75
        parts += [f'<path d="M180 145 H220 V{y+25} H250"/>',
                  f'<rect x="250" y="{y}" width="725" height="50" rx="8"/>',
                  f'<text x="270" y="{y+30}">{label}</text>']
    return ''.join(parts) + '</svg>'


def _grids(payload):
    from .example_vessel_report import _colour
    content = '<h2>Four wall-thickness grids</h2>'
    for grid in payload.get('grid_previews', []):
        x0, x1, s0, s1 = grid['bounds']
        scale = min(550/(x1-x0), 300/(s1-s0))
        content += f'<h3>Area {_text(grid["area_id"])} thickness grid</h3>'
        content += '<svg class="grid" viewBox="0 0 820 390" role="img">'
        for left, right, bottom, top, wall in grid['cells']:
            content += (f'<rect x="{70+(left-x0)*scale:.3f}" y="{45+(s1-top)*scale:.3f}" '
                        f'width="{(right-left)*scale:.3f}" height="{(top-bottom)*scale:.3f}" '
                        f'fill="{_colour(wall, grid["nominal_mm"])}"><title>{wall:.3f} mm</title></rect>')
        content += (f'<text x="70" y="22">Arc coordinate {s0:.1f} to {s1:.1f} mm ↑; equal axial/arc scale</text>'
                    f'<text x="70" y="375">Axial coordinate {x0:.1f} to {x1:.1f} mm →</text>')
        for i in range(16):
            wall = grid['nominal_mm'] * (15-i)/15
            content += f'<rect x="675" y="{45+i*17}" width="20" height="17" fill="{_colour(wall, grid["nominal_mm"])}"/>'
        content += f'<text x="705" y="58">{grid["nominal_mm"]:.3f} mm</text><text x="705" y="315">0.000 mm</text></svg>'
        content += ('<p class="caption">Assessed remaining wall, including uncertainty and future loss. '
                    'Each display bin retains its minimum sampled thickness; this preview is not an acceptance map. '
                    f'<a href="{_url(grid["csv_href"])}">Full numeric grid</a>.</p>')
    return content


def _decisions(payload):
    rows = [(r.get('area', '?'), r.get('route', '?'), _number(r.get('pressure_mpa')),
             r.get('conclusion', 'NOT EVALUATED')) for r in payload.get('decisions', [])]
    return '<h2>Area decision matrix</h2>' + _table(
        ('Area', 'Assessment route', 'Evaluated pressure (MPa)', 'Conclusion and qualification'),
        rows or [('—', '—', '—', 'NOT EVALUATED')],
        'Decision matrix. A route identifies the method used; it does not imply every Level 3 failure mode was evaluated.')


def _location(payload):
    parts = ['<h2>Vessel location schematic</h2><svg class="flow" viewBox="0 0 900 280" role="img">',
             '<title>Symbolic vessel elevation; circumferential locations are labelled</title>',
             '<rect x="55" y="60" width="790" height="140" rx="60"/>',
             '<path d="M125 60 V200 M775 60 V200"/>',
             '<text x="320" y="240">6,000 mm tangent length; 2,000 mm bore</text>']
    for area in payload.get('basis', {}).get('areas', []):
        x = 125 + float(area['centre_x_mm'])/6000*650
        y = 90 if float(area['theta_deg']) < 180 else 150
        parts += [f'<rect x="{x-22:.2f}" y="{y-13}" width="44" height="26"/>',
                  f'<text x="{x:.2f}" y="{y+4}" text-anchor="middle">{_text(area["area_id"])}</text>',
                  f'<text x="{x:.2f}" y="{y-23}" text-anchor="middle">θ={float(area["theta_deg"]):g}°</text>']
    return ''.join(parts) + ('</svg><p class="caption">Schematic only; patch sizes and circumferential '
                             'projection are not to scale. Four areas are assumed noninteracting; '
                             'the numerical models assess the selected local damage cases separately.</p>')


def _insert_geometry():
    return ('<h3>Insert geometry and load path</h3><svg class="flow" viewBox="0 0 900 380" role="img">'
            '<title>Developed-surface insert layout and conceptual pressure membrane load path</title>'
            '<rect x="220" y="45" width="330" height="300" rx="30"/>'
            '<rect x="265" y="90" width="240" height="210" rx="8" style="fill:#f5d9cb;stroke-dasharray:7 5"/>'
            '<text x="320" y="28">Insert: 1100 × 1000 mm</text>'
            '<text x="278" y="180">Removed D footprint</text><text x="300" y="202">800 × 700 mm</text>'
            '<text x="575" y="90">Corner radius: 100 mm</text>'
            '<text x="575" y="120">Continuous full-penetration</text><text x="575" y="140">butt weld at insert perimeter</text>'
            '<path d="M90 215 H215 m-12 -8 l12 8 -12 8 M555 215 H680 m-12 -8 l12 8 -12 8"/>'
            '<text x="35" y="250">Membrane load into</text><text x="35" y="270">sound shell / weld</text>'
            '<text x="575" y="250">Load across restored boundary</text>'
            '<text x="575" y="290">Nominal wall: 16.000 mm</text><text x="575" y="312">Assessed wall: 15.300 mm</text>'
            '</svg><p class="caption">Proposed developed-surface layout and conceptual load path; '
            'arrows are not computed force vectors. The numerical repair idealizes parent-equivalent '
            'continuous material. Weld qualification and fabrication are separate requirements.</p>')


def _basis(payload):
    rows = []
    for key, value in payload.get('basis', {}).items():
        rendered = json.dumps(value, ensure_ascii=False, sort_keys=True) if isinstance(value, (dict, list)) else str(value)
        rows.append((key, rendered))
    return ('<h2>Assumed vessel and material basis</h2><p>All input dimensions, loading and '
            'assumed material properties describe a synthetic example. They do not establish '
            'a material-table value or the condition of an actual vessel. Four areas are assumed '
            'noninteracting for this example; actual equipment requires an interaction assessment.</p>'
            + _table(('Parameter', 'Supplied value'), rows or [('Basis', 'NOT EVALUATED')],
                     'Table 1. Supplied assumptions; units follow the parameter names.'))


def _level12(payload):
    rows = []
    for result in payload.get('level12', []):
        for level in ('level1', 'level2'):
            stage = result.get(level, {})
            rows.append((result.get('area_id', '?'), level, _number(stage.get('rsf')),
                         _number(stage.get('rsfa')), _number(stage.get('target_pressure_mpa')),
                         _number(stage.get('mawp_reduced_mpa')), stage.get('status', 'NOT EVALUATED'),
                         '; '.join(stage.get('non_pass_reasons', []))))
    return ('<h2>Levels 1 and 2: supplied code-screening results</h2>'
            + _table(('Area', 'Level', 'RSF (1)', 'RSFa (1)', 'Pressure (MPa)',
                      'Longitudinal reduced-pressure diagnostic (MPa)', 'Disposition', 'Reasons'),
                     rows or [('—', '—', '—', '—', '—', '—', 'NOT EVALUATED', 'No result supplied')],
                     'Table 2. Conditional example results; the pressure diagnostic alone is not an accepted rating.'))


def _circumferential(payload):
    rows = []
    for result in payload.get('level12', []):
        for level in ('level1', 'level2'):
            stage = result.get(level, {})
            required = (_number(stage.get('required_circumferential_rt'))
                        if stage.get('circumferential_applicable') else 'Outside method domain')
            rows.append((result.get('area_id'), level,
                         _number(result.get('remaining_thickness_ratio')),
                         _number(stage.get('circumferential_lambda')), required,
                         'Meets curve' if stage.get('circumferential_pass') else 'No acceptance'))
    return _table(('Area', 'Level', 'Remaining ratio Rt (1)', 'Circumferential parameter (1)',
                   'Required Rt (1)', 'Circumferential conclusion'), rows,
                  'Table 2b. Table 5.4 circumferential check; a longitudinal pressure result alone is insufficient.')


def _fe_status(row):
    demands, limits = row.get('demands_mpa', {}), row.get('limits_mpa', {})
    for key in CRITERIA:
        d, limit = demands.get(key), limits.get(key)
        if _number(d) == 'NOT EVALUATED' or _number(limit) == 'NOT EVALUATED' or limit <= 0:
            return 'INCOMPLETE OR INCONSISTENT'
    passed = all(demands[k] <= limits[k] for k in CRITERIA)
    if not isinstance(row.get('acceptance'), bool) or row['acceptance'] != passed:
        return 'INCOMPLETE OR INCONSISTENT'
    return 'NUMERICAL ELASTIC CHECKS PASS' if passed else 'NUMERICAL ELASTIC CHECK NON-PASS'


def _fea(payload):
    rows = []
    for case in payload.get('fea', []):
        for criterion in CRITERIA:
            demand = case.get('demands_mpa', {}).get(criterion)
            limit = case.get('limits_mpa', {}).get(criterion)
            rows.append((case.get('case_id', '?'), case.get('case', '?'),
                         case.get('formulation', '?'), _number(case.get('pressure_mpa')),
                         _number(case.get('local_pitch_mm')), case.get('radial_layers', '—'),
                         criterion.replace('_', ' '), _number(demand), _number(limit), _fe_status(case)))
    return ('<h2>Finite-element numerical comparisons</h2><p>Each row refers to its own computed '
            'load case and pressure. A numerical elastic-check pass is separate from model qualification. '
            'Mesh and boundary qualification, equilibrium, benchmarks and other failure modes are '
            'reported below. No pressure-independent loads may be scaled with pressure.</p>'
            '<p><strong>Linear elastic analysis:</strong> the executed comparison is not an elastic-perfectly-plastic '
            'collapse calculation. Reported elastic non-pass is not a demonstrated collapse pressure.</p>'
            + (_table(('Case ID', 'Area/case', 'Formulation', 'Pressure (MPa)', 'Local pitch (mm)',
                       'Radial layers', 'Criterion', 'Demand (MPa)', 'Limit (MPa)', 'Numerical disposition'),
                      rows, 'Table 3. Actual supplied tensor checks; no missing result is inferred.')
               if rows else '<p>NOT EVALUATED — No finite-element result supplied.</p>'))


def _evidence(payload):
    content = '<h2>Verification and head assessment</h2>'
    record = payload.get('verification', {})
    rows = list(record.items()) if isinstance(record, dict) else record
    content += _table(('Verification check', 'Evidence / criterion / disposition'),
                      rows or [('Verification', 'NOT EVALUATED')], 'Table 4. Model qualification evidence.')
    head = payload.get('head', {})
    content += _table(('Head parameter', 'Result'), [
        ('Assessed thickness (mm)', _number(head.get('assessed_head_thickness_mm'))),
        ('Required thickness at 1.500 MPa (mm)', _number(head.get('required_thickness_mm'))),
        ('Conditional nominal pressure capacity (MPa)', _number(head.get('mawp_mpa'))),
        ('Scope', head.get('scope', 'NOT EVALUATED'))],
        'Table 5. Assumed undamaged 2:1 elliptical heads, full-efficiency joints and assumed allowable stress.')
    for item in payload.get('evidence_links', []):
        content += f'<p><a href="{_url(item["href"])}">{_text(item["label"])}</a></p>'
    for item in payload.get('figures', []):
        content += (f'<figure><img src="{_url(item.get("src", ""))}" alt="{_text(item.get("caption", ""))}">'
                    f'<figcaption>{_text(item.get("caption", ""))}</figcaption></figure>')
    return content


def _repair():
    return ('<h2>Repair route and common operating pressure</h2>'
            + _diagram(('Damaged shell', 'Remove bounded region', 'Flush welded insert', 'Reassess and inspect'),
                       'Idealized insert repair route')
            + _insert_geometry()
            + '<p>The proposed ideal flush butt-welded insert is 1100 × 1000 mm with 100 mm corner '
            'radius, 16.000 mm nominal wall and 15.300 mm assessed wall. Full penetration, weld '
            'efficiency 1.0 and parent-equivalent properties are assumptions. Physical fabrication '
            'has not been performed. Numerical restoration does not qualify welding, heat treatment, '
            'examination, leak testing or actual installation.</p><p>If area C remains in service and '
            'area D is repaired, the common operating pressure is governed by the lowest qualified '
            'limit across all retained areas, heads and other components. A repaired-D check at '
            '1.500 MPa does not establish 1.500 MPa acceptance for the whole vessel. A reduced-pressure C result '
            'applies only to its stated pressure and verified example basis.</p>')


def render_report(payload):
    """Return an escaped HTML report from supplied, separately qualified evidence."""
    style = ('body{max-width:1250px;margin:32px auto;padding:20px;font:15px/1.5 system-ui;color:#183044}'
             'table{border-collapse:collapse;width:100%;font-size:13px}td,th{border:1px solid #bccbd4;padding:6px}'
             'th{background:#e7f0f5}pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f3f6f8;padding:12px}'
             'svg{width:100%;height:auto}svg:not(.grid) rect{fill:#e7f0f5;stroke:#315c76}svg path{fill:none;stroke:#315c76}'
             'svg text{font:12px system-ui;fill:#183044}img{max-width:100%}.caption,figcaption{font-size:13px}'
             '.notice{padding:16px;background:#fff4d8;border-left:5px solid #b27b13}')
    body = ('<h1>Pressure-vessel four-area demonstration</h1><p class="notice">EXAMPLE DATA — '
            'conditional results only. No actual-asset fitness, certified pressure rating or completed '
            'physical repair is established.</p><h2>Assessment method</h2>'
            + _method_flow())
    body += (f'<p><a href="{_url(payload.get("data_href", "../data/pressure-vessel-example/v2/report.html"))}">'
             'Thickness grids and sampling evidence</a></p>')
    for item in payload.get('conclusions', []):
        body += f'<p>{_text(item)}</p>'
    body += _decisions(payload) + _basis(payload) + _location(payload) + _grids(payload)
    body += _level12(payload) + _circumferential(payload)
    body += _fea(payload) + _evidence(payload) + _repair()
    for item in payload.get('repair_method', []):
        body += f'<p>{_text(item)}</p>'
    source = payload.get('source_href', SOURCE)
    body += ('<h2>Source and qualification</h2><p>Assessment edition: API 579-1/ASME FFS-1:2007. '
             'Edition-specific equation verification and material assumptions remain separate. '
             f'<a href="{_url(source)}">Source or example-manual reference</a>. '
             'The publisher catalog alone does not verify calculation details or material properties. '
             'Original licensed documents remain referenced; no licensed original is embedded.</p>')
    return f'<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Vessel example</title><style>{style}</style></head><body>{body}</body></html>'
